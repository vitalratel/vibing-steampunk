// Package mcp provides the MCP server implementation for ABAP ADT tools.
// handlers_search.go contains handlers for object search operations.
package mcp

import (
	"context"

	"github.com/mark3labs/mcp-go/mcp"
)

// --- Search Routing ---
// Routes for this module:
//   search: OBJECT

// routeSearchAction routes search operations.
// Returns (result, true) if handled, (nil, false) if not handled by this module.
func (s *Server) routeSearchAction(ctx context.Context, action, objectType, objectName string, params map[string]any) (*mcp.CallToolResult, bool, error) {
	if action != "search" {
		return nil, false, nil
	}

	switch objectType {
	case "OBJECT", "":
		query := objectName
		if query == "" {
			query, _ = params["query"].(string)
		}
		if query == "" {
			return newToolResultError("query is required"), true, nil
		}
		args := map[string]any{"query": query}
		if maxResults, ok := params["maxResults"].(float64); ok {
			args["maxResults"] = maxResults
		}
		result, err := s.handleSearchObject(ctx, newRequest(args))
		return result, true, err
	}

	return nil, false, nil
}

// --- Search Handlers ---

func (s *Server) handleSearchObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	query, ok := request.Params.Arguments["query"].(string)
	if !ok || query == "" {
		return newToolResultError("query is required"), nil
	}

	maxResults := 100
	if mr, ok := request.Params.Arguments["maxResults"].(float64); ok && mr > 0 {
		maxResults = int(mr)
	}

	results, err := s.adtClient.SearchObject(ctx, query, maxResults)
	if err != nil {
		return wrapErr("SearchObject", err), nil
	}

	return newToolResultJSON(results), nil
}
