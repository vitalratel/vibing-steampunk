package adt

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"sync"
	"sync/atomic"
	"time"

	"github.com/gorilla/websocket"
)

// DebugWebSocketClient manages ABAP debugging via WebSocket (ZADT_VSP debug domain).
// This replaces the REST-based debugger which has CSRF issues for breakpoints.
type DebugWebSocketClient struct {
	baseURL  string
	client   string
	user     string
	password string
	insecure bool

	conn      *websocket.Conn
	sessionID string
	mu        sync.RWMutex

	// Request/response handling
	msgID     atomic.Int64
	pending   map[string]chan *WSResponse
	pendingMu sync.Mutex

	// Welcome signal
	welcomeCh chan struct{}

	// State
	isConnected bool
	isAttached  bool
	debuggeeID  string

	// Event channel for async events (debuggee caught, etc.)
	Events chan *DebugEvent
}

// DebugEvent represents an async event from the debugger.
type DebugEvent struct {
	Kind       string                 `json:"kind"`
	DebuggeeID string                 `json:"debuggee_id,omitempty"`
	Program    string                 `json:"program,omitempty"`
	Include    string                 `json:"include,omitempty"`
	Line       int                    `json:"line,omitempty"`
	Data       map[string]interface{} `json:"data,omitempty"`
}

// DebugDebuggee represents a debuggee that hit a breakpoint.
type DebugDebuggee struct {
	ID         string `json:"id"`
	Host       string `json:"host"`
	User       string `json:"user"`
	Program    string `json:"program"`
	SameServer bool   `json:"sameServer"`
}

// DebugStackFrame represents a stack frame.
type DebugStackFrame struct {
	Index     int    `json:"index"`
	Program   string `json:"program"`
	Include   string `json:"include"`
	Line      int    `json:"line"`
	Procedure string `json:"procedure"`
	Active    bool   `json:"active"`
	System    bool   `json:"system"`
}

// WSDebugVariable represents a variable value from WebSocket debug service.
type WSDebugVariable struct {
	Name  string `json:"name"`
	Value string `json:"value"`
	Scope string `json:"scope"`
}

// NewDebugWebSocketClient creates a new WebSocket-based debug client.
func NewDebugWebSocketClient(baseURL, client, user, password string, insecure bool) *DebugWebSocketClient {
	return &DebugWebSocketClient{
		baseURL:   baseURL,
		client:    client,
		user:      user,
		password:  password,
		insecure:  insecure,
		pending:   make(map[string]chan *WSResponse),
		welcomeCh: make(chan struct{}, 1),
		Events:    make(chan *DebugEvent, 10),
	}
}

// Connect establishes WebSocket connection to ZADT_VSP.
func (c *DebugWebSocketClient) Connect(ctx context.Context) error {
	c.mu.Lock()
	if c.conn != nil {
		c.mu.Unlock()
		return fmt.Errorf("already connected")
	}

	// Build WebSocket URL
	u, err := url.Parse(c.baseURL)
	if err != nil {
		c.mu.Unlock()
		return fmt.Errorf("invalid base URL: %w", err)
	}

	scheme := "ws"
	if u.Scheme == "https" {
		scheme = "wss"
	}

	wsURL := fmt.Sprintf("%s://%s/sap/bc/apc/sap/zadt_vsp?sap-client=%s", scheme, u.Host, c.client)

	// Create dialer with auth and TLS config
	dialer := websocket.Dialer{
		HandshakeTimeout: 30 * time.Second,
		TLSClientConfig: &tls.Config{
			InsecureSkipVerify: c.insecure,
		},
	}

	// Add basic auth header
	header := http.Header{}
	header.Set("Authorization", basicAuth(c.user, c.password))

	conn, _, err := dialer.DialContext(ctx, wsURL, header)
	if err != nil {
		c.mu.Unlock()
		return fmt.Errorf("WebSocket connection failed: %w", err)
	}

	c.conn = conn
	c.isConnected = true
	c.mu.Unlock()

	// Start message reader goroutine
	go c.readMessages()

	// Wait for welcome message
	select {
	case <-c.welcomeCh:
		return nil
	case <-time.After(5 * time.Second):
		c.Close()
		return fmt.Errorf("timeout waiting for welcome message")
	case <-ctx.Done():
		c.Close()
		return ctx.Err()
	}
}

// Close closes the WebSocket connection.
func (c *DebugWebSocketClient) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.conn != nil {
		err := c.conn.Close()
		c.conn = nil
		c.isConnected = false
		c.isAttached = false
		c.debuggeeID = ""
		return err
	}
	return nil
}

// IsConnected returns whether the client is connected.
func (c *DebugWebSocketClient) IsConnected() bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.isConnected
}

// readMessages reads messages from WebSocket and routes them.
func (c *DebugWebSocketClient) readMessages() {
	for {
		c.mu.RLock()
		conn := c.conn
		c.mu.RUnlock()

		if conn == nil {
			return
		}

		_, message, err := conn.ReadMessage()
		if err != nil {
			c.mu.Lock()
			c.conn = nil
			c.isConnected = false
			c.isAttached = false
			c.mu.Unlock()
			return
		}

		var resp WSResponse
		if err := json.Unmarshal(message, &resp); err != nil {
			continue
		}

		// Check if this is a response to a pending request
		c.pendingMu.Lock()
		if ch, ok := c.pending[resp.ID]; ok {
			ch <- &resp
			delete(c.pending, resp.ID)
			c.pendingMu.Unlock()
			continue
		}
		c.pendingMu.Unlock()

		// Otherwise it's an async event
		if resp.ID == "welcome" {
			var welcomeData struct {
				Session string   `json:"session"`
				Version string   `json:"version"`
				Domains []string `json:"domains"`
			}
			if err := json.Unmarshal(resp.Data, &welcomeData); err == nil {
				c.mu.Lock()
				c.sessionID = welcomeData.Session
				c.mu.Unlock()
			}
			select {
			case c.welcomeCh <- struct{}{}:
			default:
			}
		}
	}
}

// sendRequest sends a request and waits for response.
func (c *DebugWebSocketClient) sendRequest(ctx context.Context, action string, params map[string]interface{}) (*WSResponse, error) {
	c.mu.RLock()
	conn := c.conn
	c.mu.RUnlock()

	if conn == nil {
		return nil, fmt.Errorf("not connected")
	}

	id := fmt.Sprintf("debug_%d", c.msgID.Add(1))

	msg := WSMessage{
		ID:      id,
		Domain:  "debug",
		Action:  action,
		Params:  params,
		Timeout: 60000,
	}

	respCh := make(chan *WSResponse, 1)
	c.pendingMu.Lock()
	c.pending[id] = respCh
	c.pendingMu.Unlock()

	data, err := json.Marshal(msg)
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, err
	}

	c.mu.Lock()
	err = c.conn.WriteMessage(websocket.TextMessage, data)
	c.mu.Unlock()
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, err
	}

	select {
	case resp := <-respCh:
		return resp, nil
	case <-ctx.Done():
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, ctx.Err()
	case <-time.After(65 * time.Second):
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, fmt.Errorf("request timeout")
	}
}

// setBreakpointInternal sends a breakpoint request and parses the response.
func (c *DebugWebSocketClient) setBreakpointInternal(ctx context.Context, params map[string]interface{}) (string, error) {
	resp, err := c.sendRequest(ctx, "setBreakpoint", params)
	if err != nil {
		return "", err
	}

	if !resp.Success {
		if resp.Error != nil {
			return "", fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return "", fmt.Errorf("setBreakpoint failed")
	}

	var result struct {
		BreakpointID string `json:"breakpointId"`
		Registered   bool   `json:"registered"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return "", err
	}

	return result.BreakpointID, nil
}

// SetLineBreakpoint sets a breakpoint at a specific line in a program.
// For classes, program should be in class pool format: ZCL_TEST================CP
// The line number is pool-absolute (the line in the consolidated class source).
func (c *DebugWebSocketClient) SetLineBreakpoint(ctx context.Context, program string, line int) (string, error) {
	params := map[string]interface{}{
		"kind":    "line",
		"program": program,
		"line":    line,
	}
	return c.setBreakpointInternal(ctx, params)
}

// SetMethodBreakpoint sets a breakpoint at a specific line within a method.
// For class methods, this uses include-relative line numbers (line 1 = first line of method).
// The method name is used to resolve the correct include for the breakpoint.
// Example: SetMethodBreakpoint(ctx, "ZCL_TEST================CP", "MY_METHOD", 5)
func (c *DebugWebSocketClient) SetMethodBreakpoint(ctx context.Context, program, method string, line int) (string, error) {
	params := map[string]interface{}{
		"kind":    "line",
		"program": program,
		"method":  method,
		"line":    line,
	}
	return c.setBreakpointInternal(ctx, params)
}

// SetStatementBreakpoint sets a breakpoint on a specific ABAP statement.
// Example statements: "CALL FUNCTION", "SELECT", "LOOP", "CALL METHOD"
func (c *DebugWebSocketClient) SetStatementBreakpoint(ctx context.Context, statement string) (string, error) {
	params := map[string]interface{}{
		"kind":      "statement",
		"statement": statement,
	}
	return c.setBreakpointInternal(ctx, params)
}

// SetExceptionBreakpoint sets a breakpoint that triggers when an exception is raised.
// Example exceptions: "CX_SY_ZERODIVIDE", "CX_SY_OPEN_SQL_DB"
func (c *DebugWebSocketClient) SetExceptionBreakpoint(ctx context.Context, exception string) (string, error) {
	params := map[string]interface{}{
		"kind":      "exception",
		"exception": exception,
	}
	return c.setBreakpointInternal(ctx, params)
}

// GetBreakpoints returns all active breakpoints.
func (c *DebugWebSocketClient) GetBreakpoints(ctx context.Context) ([]map[string]interface{}, error) {
	resp, err := c.sendRequest(ctx, "getBreakpoints", nil)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("getBreakpoints failed")
	}

	var result struct {
		Breakpoints []map[string]interface{} `json:"breakpoints"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	return result.Breakpoints, nil
}

// DeleteBreakpoint removes a breakpoint by ID.
func (c *DebugWebSocketClient) DeleteBreakpoint(ctx context.Context, breakpointID string) error {
	params := map[string]interface{}{
		"breakpointId": breakpointID,
	}

	resp, err := c.sendRequest(ctx, "deleteBreakpoint", params)
	if err != nil {
		return err
	}

	if !resp.Success {
		if resp.Error != nil {
			return fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return fmt.Errorf("deleteBreakpoint failed")
	}

	return nil
}

// Listen waits for a debuggee to hit a breakpoint.
func (c *DebugWebSocketClient) Listen(ctx context.Context, timeout int) ([]DebugDebuggee, error) {
	if timeout <= 0 {
		timeout = 60
	}
	if timeout > 240 {
		timeout = 240
	}

	params := map[string]interface{}{
		"timeout": timeout,
		"user":    c.user,
	}

	// Create a context with longer timeout for listen
	listenCtx, cancel := context.WithTimeout(ctx, time.Duration(timeout+10)*time.Second)
	defer cancel()

	resp, err := c.sendRequest(listenCtx, "listen", params)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("listen failed")
	}

	var result struct {
		Status    string          `json:"status"`
		Debuggees []DebugDebuggee `json:"debuggees"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	if result.Status == "timeout" {
		return nil, nil // No debuggee caught
	}

	return result.Debuggees, nil
}

// Attach attaches to a debuggee.
func (c *DebugWebSocketClient) Attach(ctx context.Context, debuggeeID string) (*DebugStackFrame, error) {
	params := map[string]interface{}{
		"debuggeeId": debuggeeID,
	}

	resp, err := c.sendRequest(ctx, "attach", params)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("attach failed")
	}

	var result struct {
		Attached   bool   `json:"attached"`
		DebuggeeID string `json:"debuggeeId"`
		Program    string `json:"program"`
		Include    string `json:"include"`
		Line       int    `json:"line"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	c.mu.Lock()
	c.isAttached = true
	c.debuggeeID = debuggeeID
	c.mu.Unlock()

	return &DebugStackFrame{
		Program: result.Program,
		Include: result.Include,
		Line:    result.Line,
		Active:  true,
	}, nil
}

// Detach detaches from the current debuggee.
func (c *DebugWebSocketClient) Detach(ctx context.Context) error {
	resp, err := c.sendRequest(ctx, "detach", nil)
	if err != nil {
		return err
	}

	c.mu.Lock()
	c.isAttached = false
	c.debuggeeID = ""
	c.mu.Unlock()

	if !resp.Success {
		if resp.Error != nil {
			return fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
	}

	return nil
}

// Step performs a step operation.
func (c *DebugWebSocketClient) Step(ctx context.Context, stepType string) (*DebugStackFrame, error) {
	params := map[string]interface{}{
		"type": stepType,
	}

	resp, err := c.sendRequest(ctx, "step", params)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("step failed")
	}

	var result struct {
		Stepped   string `json:"stepped"`
		Program   string `json:"program"`
		Include   string `json:"include"`
		Line      int    `json:"line"`
		Procedure string `json:"procedure"`
		Ended     bool   `json:"ended"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	if result.Ended {
		c.mu.Lock()
		c.isAttached = false
		c.debuggeeID = ""
		c.mu.Unlock()
		return nil, nil // Debuggee ended
	}

	return &DebugStackFrame{
		Program:   result.Program,
		Include:   result.Include,
		Line:      result.Line,
		Procedure: result.Procedure,
		Active:    true,
	}, nil
}

// GetStack returns the current call stack.
func (c *DebugWebSocketClient) GetStack(ctx context.Context) ([]DebugStackFrame, error) {
	resp, err := c.sendRequest(ctx, "getStack", nil)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("getStack failed")
	}

	var result struct {
		Stack []DebugStackFrame `json:"stack"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	return result.Stack, nil
}

// GetVariables returns variable values.
func (c *DebugWebSocketClient) GetVariables(ctx context.Context, scope string) ([]WSDebugVariable, error) {
	if scope == "" {
		scope = "system"
	}

	params := map[string]interface{}{
		"scope": scope,
	}

	resp, err := c.sendRequest(ctx, "getVariables", params)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("getVariables failed")
	}

	var result struct {
		Variables []WSDebugVariable `json:"variables"`
		Scope     string            `json:"scope"`
	}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	return result.Variables, nil
}

// GetStatus returns the current debug session status.
func (c *DebugWebSocketClient) GetStatus(ctx context.Context) (map[string]interface{}, error) {
	resp, err := c.sendRequest(ctx, "getStatus", nil)
	if err != nil {
		return nil, err
	}

	if !resp.Success {
		if resp.Error != nil {
			return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
		}
		return nil, fmt.Errorf("getStatus failed")
	}

	var result map[string]interface{}
	if err := json.Unmarshal(resp.Data, &result); err != nil {
		return nil, err
	}

	return result, nil
}

// RFCResult contains the result of an RFC call.
type RFCResult struct {
	Subrc   int                    `json:"subrc"`
	Exports map[string]interface{} `json:"exports"`
	Tables  map[string]interface{} `json:"tables"`
}

// CallRFC calls a function module via WebSocket.
func (c *DebugWebSocketClient) CallRFC(ctx context.Context, function string, params map[string]string) (*RFCResult, error) {
	c.mu.RLock()
	conn := c.conn
	c.mu.RUnlock()

	if conn == nil {
		return nil, fmt.Errorf("not connected")
	}

	id := fmt.Sprintf("rfc_%d", c.msgID.Add(1))

	// Build params as proper object (not JSON string)
	paramsObj := map[string]interface{}{
		"function": function,
	}
	for k, v := range params {
		paramsObj[k] = v
	}

	// Build message with params object
	rawMsg := map[string]interface{}{
		"id":      id,
		"domain":  "rfc",
		"action":  "call",
		"params":  paramsObj,
		"timeout": 120000,
	}

	respCh := make(chan *WSResponse, 1)
	c.pendingMu.Lock()
	c.pending[id] = respCh
	c.pendingMu.Unlock()

	data, err := json.Marshal(rawMsg)
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, err
	}

	c.mu.Lock()
	err = c.conn.WriteMessage(websocket.TextMessage, data)
	c.mu.Unlock()
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, err
	}

	select {
	case resp := <-respCh:
		if !resp.Success {
			if resp.Error != nil {
				return nil, fmt.Errorf("%s: %s", resp.Error.Code, resp.Error.Message)
			}
			return nil, fmt.Errorf("RFC call failed")
		}

		var result RFCResult
		if err := json.Unmarshal(resp.Data, &result); err != nil {
			return nil, err
		}
		return &result, nil

	case <-ctx.Done():
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, ctx.Err()
	case <-time.After(125 * time.Second):
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, fmt.Errorf("RFC call timeout")
	}
}

// RunReport executes a report via background job (RFC domain, runReport action).
// This schedules the report as a background job, which runs in a separate work process
// and CAN hit external breakpoints.
func (c *DebugWebSocketClient) RunReport(ctx context.Context, report string, variant string) error {
	c.mu.RLock()
	conn := c.conn
	c.mu.RUnlock()

	if conn == nil {
		return fmt.Errorf("not connected")
	}

	id := fmt.Sprintf("rfc_run_%d", c.msgID.Add(1))

	// Build params for RFC domain runReport action
	paramsObj := map[string]interface{}{
		"report": report,
	}
	if variant != "" {
		paramsObj["variant"] = variant
	}

	// Build message with RFC domain (runReport schedules as background job)
	rawMsg := map[string]interface{}{
		"id":      id,
		"domain":  "rfc",
		"action":  "runReport",
		"params":  paramsObj,
		"timeout": 30000,
	}

	respCh := make(chan *WSResponse, 1)
	c.pendingMu.Lock()
	c.pending[id] = respCh
	c.pendingMu.Unlock()

	data, err := json.Marshal(rawMsg)
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return err
	}

	c.mu.Lock()
	err = c.conn.WriteMessage(websocket.TextMessage, data)
	c.mu.Unlock()
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return err
	}

	// Don't wait for response - the report might be blocked on breakpoint
	// The listener will catch the debuggee
	go func() {
		select {
		case <-respCh:
			// Report finished (no breakpoint hit or continued past)
		case <-time.After(60 * time.Second):
			c.pendingMu.Lock()
			delete(c.pending, id)
			c.pendingMu.Unlock()
		}
	}()

	return nil
}

// RunReportSync executes a report via background job and waits for the response.
func (c *DebugWebSocketClient) RunReportSync(ctx context.Context, report string, variant string) (*WSResponse, error) {
	c.mu.RLock()
	conn := c.conn
	c.mu.RUnlock()

	if conn == nil {
		return nil, fmt.Errorf("not connected")
	}

	id := fmt.Sprintf("rfc_run_%d", c.msgID.Add(1))

	paramsObj := map[string]interface{}{
		"report": report,
	}
	if variant != "" {
		paramsObj["variant"] = variant
	}

	rawMsg := map[string]interface{}{
		"id":      id,
		"domain":  "rfc",
		"action":  "runReport",
		"params":  paramsObj,
		"timeout": 30000,
	}

	respCh := make(chan *WSResponse, 1)
	c.pendingMu.Lock()
	c.pending[id] = respCh
	c.pendingMu.Unlock()

	data, err := json.Marshal(rawMsg)
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, err
	}

	c.mu.Lock()
	err = c.conn.WriteMessage(websocket.TextMessage, data)
	c.mu.Unlock()
	if err != nil {
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, err
	}

	// Wait for response
	select {
	case resp := <-respCh:
		return resp, nil
	case <-ctx.Done():
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, ctx.Err()
	case <-time.After(60 * time.Second):
		c.pendingMu.Lock()
		delete(c.pending, id)
		c.pendingMu.Unlock()
		return nil, fmt.Errorf("report timeout")
	}
}
