// Package mcp provides the MCP server implementation for ABAP ADT tools.
// handlers_report.go contains handlers for report execution and text elements.
package mcp

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// --- Report Execution Handlers ---

func (s *Server) handleRunReport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	if errResult := s.ensureWSConnected(ctx, "RunReport"); errResult != nil {
		return errResult, nil
	}

	// Parse parameters
	report, _ := request.Params.Arguments["report"].(string)
	if report == "" {
		return newToolResultError("report parameter is required"), nil
	}

	params := adt.RunReportParams{
		Report: report,
	}

	if variant, ok := request.Params.Arguments["variant"].(string); ok {
		params.Variant = variant
	}

	if paramsStr, ok := request.Params.Arguments["params"].(string); ok && paramsStr != "" {
		var p map[string]string
		if err := json.Unmarshal([]byte(paramsStr), &p); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid params JSON: %v", err)), nil
		}
		params.Params = p
	}

	if captureALV, ok := request.Params.Arguments["capture_alv"].(bool); ok {
		params.CaptureALV = captureALV
	}

	if maxRows, ok := request.Params.Arguments["max_rows"].(float64); ok {
		params.MaxRows = int(maxRows)
	}

	result, err := s.amdpWSClient.RunReport(ctx, params)
	if err != nil {
		return newToolResultError(fmt.Sprintf("RunReport failed: %v", err)), nil
	}

	// Format output
	var sb strings.Builder
	fmt.Fprintf(&sb, "Report: %s\n", result.Report)
	fmt.Fprintf(&sb, "Status: %s\n", result.Status)
	fmt.Fprintf(&sb, "Runtime: %dms\n\n", result.RuntimeMs)

	if result.ALVCaptured {
		fmt.Fprintf(&sb, "ALV Data Captured: %d rows", result.TotalRows)
		if result.Truncated {
			sb.WriteString(" (truncated)")
		}
		sb.WriteString("\n\n")

		// Show columns
		sb.WriteString("Columns:\n")
		for _, col := range result.Columns {
			fmt.Fprintf(&sb, "  %s (%s)\n", col.Name, col.Type)
		}
		sb.WriteString("\n")

		// Show data as JSON
		if len(result.Rows) > 0 {
			rowsJSON, _ := json.MarshalIndent(result.Rows, "", "  ")
			sb.WriteString("Data:\n")
			sb.Write(rowsJSON)
			sb.WriteString("\n")
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleRunReportAsync(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Parse parameters
	report, _ := request.Params.Arguments["report"].(string)
	if report == "" {
		return newToolResultError("report parameter is required"), nil
	}

	params := adt.RunReportParams{
		Report: report,
	}

	if variant, ok := request.Params.Arguments["variant"].(string); ok {
		params.Variant = variant
	}

	if paramsStr, ok := request.Params.Arguments["params"].(string); ok && paramsStr != "" {
		var p map[string]string
		if err := json.Unmarshal([]byte(paramsStr), &p); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid params JSON: %v", err)), nil
		}
		params.Params = p
	}

	if captureALV, ok := request.Params.Arguments["capture_alv"].(bool); ok {
		params.CaptureALV = captureALV
	}

	if maxRows, ok := request.Params.Arguments["max_rows"].(float64); ok {
		params.MaxRows = int(maxRows)
	}

	// Generate task ID
	s.asyncTasksMu.Lock()
	s.asyncTaskID++
	taskID := fmt.Sprintf("report_%d_%d", time.Now().Unix(), s.asyncTaskID)
	task := &AsyncTask{
		ID:        taskID,
		Type:      "report",
		Status:    "running",
		StartedAt: time.Now(),
	}
	s.asyncTasks[taskID] = task
	s.asyncTasksMu.Unlock()

	// Run report in background goroutine
	go func() {
		bgCtx := context.Background()

		// Create WebSocket connection for this goroutine
		wsClient := adt.NewAMDPWebSocketClient(
			s.config.BaseURL, s.config.Client, s.config.Username, s.config.Password, s.config.InsecureSkipVerify,
		)
		if err := wsClient.Connect(bgCtx); err != nil {
			s.asyncTasksMu.Lock()
			now := time.Now()
			task.Status = "error"
			task.Error = fmt.Sprintf("WebSocket connect failed: %v", err)
			task.EndedAt = &now
			s.asyncTasksMu.Unlock()
			return
		}
		defer wsClient.Close()

		result, err := wsClient.RunReport(bgCtx, params)

		s.asyncTasksMu.Lock()
		now := time.Now()
		task.EndedAt = &now
		if err != nil {
			task.Status = "error"
			task.Error = err.Error()
		} else {
			task.Status = "completed"
			task.Result = result
		}
		s.asyncTasksMu.Unlock()
	}()

	// Return task ID immediately
	output := map[string]string{
		"task_id": taskID,
		"status":  "started",
		"message": "Report execution started in background. Use GetAsyncResult to check status.",
	}
	outputJSON, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(outputJSON)), nil
}

func (s *Server) handleGetAsyncResult(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	taskID, _ := request.Params.Arguments["task_id"].(string)
	if taskID == "" {
		return newToolResultError("task_id parameter is required"), nil
	}

	wait, _ := request.Params.Arguments["wait"].(bool)

	if wait {
		// Block until complete or timeout
		timeout := time.After(60 * time.Second)
		ticker := time.NewTicker(500 * time.Millisecond)
		defer ticker.Stop()

		for {
			s.asyncTasksMu.RLock()
			task, exists := s.asyncTasks[taskID]
			if !exists {
				s.asyncTasksMu.RUnlock()
				return newToolResultError(fmt.Sprintf("Task not found: %s", taskID)), nil
			}
			status := task.Status
			s.asyncTasksMu.RUnlock()

			if status != "running" {
				break
			}

			select {
			case <-timeout:
				return newToolResultError("Timeout waiting for task completion"), nil
			case <-ticker.C:
				continue
			case <-ctx.Done():
				return newToolResultError("Request cancelled"), nil
			}
		}
	}

	// Get task status
	s.asyncTasksMu.RLock()
	task, exists := s.asyncTasks[taskID]
	if !exists {
		s.asyncTasksMu.RUnlock()
		return newToolResultError(fmt.Sprintf("Task not found: %s", taskID)), nil
	}
	// Make a copy for safe access
	taskCopy := *task
	s.asyncTasksMu.RUnlock()

	// Format output
	output, _ := json.MarshalIndent(taskCopy, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleGetVariants(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	if errResult := s.ensureWSConnected(ctx, "GetVariants"); errResult != nil {
		return errResult, nil
	}

	report, _ := request.Params.Arguments["report"].(string)
	if report == "" {
		return newToolResultError("report parameter is required"), nil
	}

	result, err := s.amdpWSClient.GetVariants(ctx, report)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetVariants failed: %v", err)), nil
	}

	var sb strings.Builder
	fmt.Fprintf(&sb, "Variants for %s:\n\n", result.Report)

	if len(result.Variants) == 0 {
		sb.WriteString("No variants found.\n")
	} else {
		for _, v := range result.Variants {
			if v.Protected {
				fmt.Fprintf(&sb, "  %s (protected)\n", v.Name)
			} else {
				fmt.Fprintf(&sb, "  %s\n", v.Name)
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleGetTextElements(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	if errResult := s.ensureWSConnected(ctx, "GetTextElements"); errResult != nil {
		return errResult, nil
	}

	program, _ := request.Params.Arguments["program"].(string)
	if program == "" {
		return newToolResultError("program parameter is required"), nil
	}

	language, _ := request.Params.Arguments["language"].(string)

	result, err := s.amdpWSClient.GetTextElements(ctx, program, language)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetTextElements failed: %v", err)), nil
	}

	var sb strings.Builder
	fmt.Fprintf(&sb, "Text Elements for %s (Language: %s)\n\n", result.Program, result.Language)

	sb.WriteString("Selection Texts:\n")
	if len(result.SelectionTexts) == 0 {
		sb.WriteString("  (none)\n")
	} else {
		for key, text := range result.SelectionTexts {
			fmt.Fprintf(&sb, "  %s: %s\n", key, text)
		}
	}
	sb.WriteString("\n")

	sb.WriteString("Text Symbols:\n")
	if len(result.TextSymbols) == 0 {
		sb.WriteString("  (none)\n")
	} else {
		for key, text := range result.TextSymbols {
			fmt.Fprintf(&sb, "  TEXT-%s: %s\n", key, text)
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleSetTextElements(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	if errResult := s.ensureWSConnected(ctx, "SetTextElements"); errResult != nil {
		return errResult, nil
	}

	program, _ := request.Params.Arguments["program"].(string)
	if program == "" {
		return newToolResultError("program parameter is required"), nil
	}

	params := adt.SetTextElementsParams{
		Program: program,
	}

	if language, ok := request.Params.Arguments["language"].(string); ok {
		params.Language = language
	}

	if selTextsStr, ok := request.Params.Arguments["selection_texts"].(string); ok && selTextsStr != "" {
		var selTexts map[string]string
		if err := json.Unmarshal([]byte(selTextsStr), &selTexts); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid selection_texts JSON: %v", err)), nil
		}
		params.SelectionTexts = selTexts
	}

	if textSymsStr, ok := request.Params.Arguments["text_symbols"].(string); ok && textSymsStr != "" {
		var textSyms map[string]string
		if err := json.Unmarshal([]byte(textSymsStr), &textSyms); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid text_symbols JSON: %v", err)), nil
		}
		params.TextSymbols = textSyms
	}

	if headTextsStr, ok := request.Params.Arguments["heading_texts"].(string); ok && headTextsStr != "" {
		var headTexts map[string]string
		if err := json.Unmarshal([]byte(headTextsStr), &headTexts); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid heading_texts JSON: %v", err)), nil
		}
		params.HeadingTexts = headTexts
	}

	if params.SelectionTexts == nil && params.TextSymbols == nil && params.HeadingTexts == nil {
		return newToolResultError("At least one of selection_texts, text_symbols, or heading_texts is required"), nil
	}

	result, err := s.amdpWSClient.SetTextElements(ctx, params)
	if err != nil {
		return newToolResultError(fmt.Sprintf("SetTextElements failed: %v", err)), nil
	}

	var sb strings.Builder
	fmt.Fprintf(&sb, "Text Elements Updated for %s (Language: %s)\n\n", result.Program, result.Language)
	fmt.Fprintf(&sb, "Status: %s\n", result.Status)
	fmt.Fprintf(&sb, "Selection Texts Set: %d\n", result.SelectionTextsSet)
	fmt.Fprintf(&sb, "Text Symbols Set: %d\n", result.TextSymbolsSet)
	fmt.Fprintf(&sb, "Heading Texts Set: %d\n", result.HeadingTextsSet)

	return mcp.NewToolResultText(sb.String()), nil
}
