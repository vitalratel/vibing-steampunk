package adt

import (
	"context"
	"encoding/xml"
	"fmt"
	"net/http"
	"net/url"
)

// --- SQL Trace (ST05) Operations ---

// SQLTraceState represents the current state of SQL tracing.
type SQLTraceState struct {
	Active     bool   `json:"active"`
	User       string `json:"user,omitempty"`
	TraceType  string `json:"traceType,omitempty"`
	StartTime  string `json:"startTime,omitempty"`
	MaxRecords int    `json:"maxRecords,omitempty"`
	TraceFile  string `json:"traceFile,omitempty"`
}

// SQLTraceEntry represents a trace file in the directory.
type SQLTraceEntry struct {
	ID          string `json:"id"`
	User        string `json:"user"`
	StartTime   string `json:"startTime"`
	EndTime     string `json:"endTime,omitempty"`
	TraceType   string `json:"traceType"`
	RecordCount int    `json:"recordCount"`
	Size        int64  `json:"size,omitempty"`
	URI         string `json:"uri"`
}

// GetSQLTraceState checks if SQL trace is currently active.
func (c *Client) GetSQLTraceState(ctx context.Context) (*SQLTraceState, error) {
	resp, err := c.transport.Request(ctx, "/sap/bc/adt/st05/trace/state", &RequestOptions{
		Method: http.MethodGet,
		Accept: "application/vnd.sap.adt.perf.trace.state.v1+xml",
	})
	if err != nil {
		return nil, fmt.Errorf("getting SQL trace state: %w", err)
	}

	return parseSQLTraceState(resp.Body)
}

// ListSQLTraces retrieves a list of SQL trace files.
func (c *Client) ListSQLTraces(ctx context.Context, user string, maxResults int) ([]SQLTraceEntry, error) {
	params := url.Values{}
	if user != "" {
		params.Set("user", user)
	}
	if maxResults > 0 {
		params.Set("$top", fmt.Sprintf("%d", maxResults))
	}

	endpoint := "/sap/bc/adt/st05/trace/directory"
	if len(params) > 0 {
		endpoint = endpoint + "?" + params.Encode()
	}

	resp, err := c.transport.Request(ctx, endpoint, &RequestOptions{
		Method: http.MethodGet,
		Accept: "application/vnd.sap.adt.perf.trace.directory.v1+xml",
	})
	if err != nil {
		return nil, fmt.Errorf("listing SQL traces: %w", err)
	}

	return parseSQLTraceDirectory(resp.Body)
}

// parseSQLTraceState parses the SQL trace state XML.
// v1 format uses root element <traceStateInstanceTable> with nested <traceStateInstance>.
func parseSQLTraceState(data []byte) (*SQLTraceState, error) {
	xmlStr := StripXMLNamespaces(string(data), "st05:")

	type traceStateInstance struct {
		Active     string `xml:"active,attr"`
		User       string `xml:"user,attr"`
		TraceType  string `xml:"traceType,attr"`
		StartTime  string `xml:"startTime,attr"`
		MaxRecords string `xml:"maxRecords,attr"`
		TraceFile  string `xml:"traceFile,attr"`
	}
	type stateTableXML struct {
		Instances []traceStateInstance `xml:"traceStateInstance"`
	}

	var table stateTableXML
	if err := xml.Unmarshal([]byte(xmlStr), &table); err != nil {
		return nil, fmt.Errorf("parsing SQL trace state: %w", err)
	}

	// Return first active instance or empty state
	state := &SQLTraceState{}
	if len(table.Instances) > 0 {
		inst := table.Instances[0]
		var maxRecords int
		if inst.MaxRecords != "" {
			_, _ = fmt.Sscanf(inst.MaxRecords, "%d", &maxRecords)
		}
		state.Active = inst.Active == "true" || inst.Active == "X"
		state.User = inst.User
		state.TraceType = inst.TraceType
		state.StartTime = inst.StartTime
		state.MaxRecords = maxRecords
		state.TraceFile = inst.TraceFile
	}

	return state, nil
}

// parseSQLTraceDirectory parses the SQL trace directory.
// v1 format uses root element <traceDirectory> with nested <traceDirectoryEntry>.
func parseSQLTraceDirectory(data []byte) ([]SQLTraceEntry, error) {
	xmlStr := StripXMLNamespaces(string(data), "st05:", "atom:", "adtcore:")

	type traceEntryXML struct {
		ID          string `xml:"id,attr"`
		User        string `xml:"user,attr"`
		TraceType   string `xml:"traceType,attr"`
		StartTime   string `xml:"startTime,attr"`
		EndTime     string `xml:"endTime,attr"`
		RecordCount string `xml:"recordCount,attr"`
		Size        string `xml:"size,attr"`
		Link        struct {
			Href string `xml:"href,attr"`
		} `xml:"link"`
	}
	type directoryXML struct {
		Entries []traceEntryXML `xml:"traceDirectoryEntry"`
	}

	var dir directoryXML
	if err := xml.Unmarshal([]byte(xmlStr), &dir); err != nil {
		return nil, fmt.Errorf("parsing SQL trace directory: %w", err)
	}

	result := make([]SQLTraceEntry, 0, len(dir.Entries))
	for _, entry := range dir.Entries {
		var recordCount int
		var size int64
		if entry.RecordCount != "" {
			_, _ = fmt.Sscanf(entry.RecordCount, "%d", &recordCount)
		}
		if entry.Size != "" {
			_, _ = fmt.Sscanf(entry.Size, "%d", &size)
		}

		result = append(result, SQLTraceEntry{
			ID:          entry.ID,
			User:        entry.User,
			StartTime:   entry.StartTime,
			EndTime:     entry.EndTime,
			TraceType:   entry.TraceType,
			RecordCount: recordCount,
			Size:        size,
			URI:         entry.Link.Href,
		})
	}

	return result, nil
}
