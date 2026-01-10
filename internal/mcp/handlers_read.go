// Package mcp provides the MCP server implementation for ABAP ADT tools.
// handlers_read.go contains handlers for read operations (GetProgram, GetClass, GetTable, etc.)
package mcp

import (
	"context"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// --- Read Routing ---
// Routes for this module:
//   read: PROG, CLAS, INTF, FUNC, FUGR, INCL, TABL, STRU, DEVC, MSAG, TRAN, TYPE_INFO
//   query: TABL_CONTENTS, SQL

// routeReadAction routes basic read operations.
// Returns (result, true) if handled, (nil, false) if not handled by this module.
func (s *Server) routeReadAction(ctx context.Context, action, objectType, objectName string, params map[string]any) (*mcp.CallToolResult, bool, error) {
	switch action {
	case "read":
		switch objectType {
		case "PROG":
			if objectName == "" {
				return newToolResultError("program_name is required"), true, nil
			}
			result, err := s.handleGetProgram(ctx, newRequest(map[string]any{"program_name": objectName}))
			return result, true, err

		case "CLAS":
			if objectName == "" {
				return newToolResultError("class_name is required"), true, nil
			}
			result, err := s.handleGetClass(ctx, newRequest(map[string]any{"class_name": objectName}))
			return result, true, err

		case "INTF":
			if objectName == "" {
				return newToolResultError("interface_name is required"), true, nil
			}
			result, err := s.handleGetInterface(ctx, newRequest(map[string]any{"interface_name": objectName}))
			return result, true, err

		case "FUNC":
			if objectName == "" {
				return newToolResultError("function_name is required"), true, nil
			}
			functionGroup, _ := params["function_group"].(string)
			if functionGroup == "" {
				return newToolResultError("function_group is required in params"), true, nil
			}
			result, err := s.handleGetFunction(ctx, newRequest(map[string]any{
				"function_name":  objectName,
				"function_group": functionGroup,
			}))
			return result, true, err

		case "FUGR":
			if objectName == "" {
				return newToolResultError("function_group is required"), true, nil
			}
			result, err := s.handleGetFunctionGroup(ctx, newRequest(map[string]any{"function_group": objectName}))
			return result, true, err

		case "INCL":
			if objectName == "" {
				return newToolResultError("include_name is required"), true, nil
			}
			result, err := s.handleGetInclude(ctx, newRequest(map[string]any{"include_name": objectName}))
			return result, true, err

		case "TABL":
			if objectName == "" {
				return newToolResultError("table_name is required"), true, nil
			}
			result, err := s.handleGetTable(ctx, newRequest(map[string]any{"table_name": objectName}))
			return result, true, err

		case "STRU":
			if objectName == "" {
				return newToolResultError("structure_name is required"), true, nil
			}
			result, err := s.handleGetStructure(ctx, newRequest(map[string]any{"structure_name": objectName}))
			return result, true, err

		case "DEVC":
			if objectName == "" {
				return newToolResultError("package_name is required"), true, nil
			}
			result, err := s.handleGetPackage(ctx, newRequest(map[string]any{"package_name": objectName}))
			return result, true, err

		case "MSAG":
			if objectName == "" {
				return newToolResultError("message_class is required"), true, nil
			}
			result, err := s.handleGetMessages(ctx, newRequest(map[string]any{"message_class": objectName}))
			return result, true, err

		case "TRAN":
			if objectName == "" {
				return newToolResultError("transaction_name is required"), true, nil
			}
			result, err := s.handleGetTransaction(ctx, newRequest(map[string]any{"transaction_name": objectName}))
			return result, true, err

		case "TYPE_INFO":
			if objectName == "" {
				return newToolResultError("type_name is required"), true, nil
			}
			result, err := s.handleGetTypeInfo(ctx, newRequest(map[string]any{"type_name": objectName}))
			return result, true, err
		}

	case "query":
		switch objectType {
		case "TABL_CONTENTS":
			tableName := objectName
			if tableName == "" {
				tableName, _ = params["table_name"].(string)
			}
			if tableName == "" {
				return newToolResultError("table_name is required"), true, nil
			}
			args := map[string]any{"table_name": tableName}
			if maxRows, ok := params["max_rows"].(float64); ok {
				args["max_rows"] = maxRows
			}
			if sqlQuery, ok := params["sql_query"].(string); ok {
				args["sql_query"] = sqlQuery
			}
			result, err := s.handleGetTableContents(ctx, newRequest(args))
			return result, true, err

		case "SQL":
			sqlQuery, _ := params["sql_query"].(string)
			if sqlQuery == "" {
				return newToolResultError("sql_query is required in params"), true, nil
			}
			args := map[string]any{"sql_query": sqlQuery}
			if maxRows, ok := params["max_rows"].(float64); ok {
				args["max_rows"] = maxRows
			}
			result, err := s.handleRunQuery(ctx, newRequest(args))
			return result, true, err
		}

	case "analyze":
		analysisType, _ := params["type"].(string)
		if analysisType == "cds_dependencies" {
			ddlsName := objectName
			if ddlsName == "" {
				ddlsName, _ = params["ddls_name"].(string)
			}
			if ddlsName == "" {
				return newToolResultError("ddls_name is required"), true, nil
			}
			args := map[string]any{"ddls_name": ddlsName}
			if level, ok := params["dependency_level"].(string); ok {
				args["dependency_level"] = level
			}
			if assoc, ok := params["with_associations"].(bool); ok {
				args["with_associations"] = assoc
			}
			if pkg, ok := params["context_package"].(string); ok {
				args["context_package"] = pkg
			}
			result, err := s.handleGetCDSDependencies(ctx, newRequest(args))
			return result, true, err
		}
	}

	return nil, false, nil
}

// --- Read Handlers ---

func (s *Server) handleGetProgram(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	programName, ok := request.Params.Arguments["program_name"].(string)
	if !ok || programName == "" {
		return newToolResultError("program_name is required"), nil
	}

	source, err := s.adtClient.GetProgram(ctx, programName)
	if err != nil {
		return wrapErr("GetProgram", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetClass(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, ok := request.Params.Arguments["class_name"].(string)
	if !ok || className == "" {
		return newToolResultError("class_name is required"), nil
	}

	source, err := s.adtClient.GetClassSource(ctx, className)
	if err != nil {
		return wrapErr("GetClass", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetInterface(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	interfaceName, ok := request.Params.Arguments["interface_name"].(string)
	if !ok || interfaceName == "" {
		return newToolResultError("interface_name is required"), nil
	}

	source, err := s.adtClient.GetInterface(ctx, interfaceName)
	if err != nil {
		return wrapErr("GetInterface", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetFunction(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	functionName, ok := request.Params.Arguments["function_name"].(string)
	if !ok || functionName == "" {
		return newToolResultError("function_name is required"), nil
	}

	functionGroup, ok := request.Params.Arguments["function_group"].(string)
	if !ok || functionGroup == "" {
		return newToolResultError("function_group is required"), nil
	}

	source, err := s.adtClient.GetFunction(ctx, functionName, functionGroup)
	if err != nil {
		return wrapErr("GetFunction", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetFunctionGroup(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	groupName, ok := request.Params.Arguments["function_group"].(string)
	if !ok || groupName == "" {
		return newToolResultError("function_group is required"), nil
	}

	fg, err := s.adtClient.GetFunctionGroup(ctx, groupName)
	if err != nil {
		return wrapErr("GetFunctionGroup", err), nil
	}
	return newToolResultJSON(fg), nil
}

func (s *Server) handleGetInclude(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	includeName, ok := request.Params.Arguments["include_name"].(string)
	if !ok || includeName == "" {
		return newToolResultError("include_name is required"), nil
	}

	source, err := s.adtClient.GetInclude(ctx, includeName)
	if err != nil {
		return wrapErr("GetInclude", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetTable(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	tableName, ok := request.Params.Arguments["table_name"].(string)
	if !ok || tableName == "" {
		return newToolResultError("table_name is required"), nil
	}

	source, err := s.adtClient.GetTable(ctx, tableName)
	if err != nil {
		return wrapErr("GetTable", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetTableContents(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	tableName, ok := request.Params.Arguments["table_name"].(string)
	if !ok || tableName == "" {
		return newToolResultError("table_name is required"), nil
	}

	maxRows := 100
	if mr, ok := request.Params.Arguments["max_rows"].(float64); ok && mr > 0 {
		maxRows = int(mr)
	}

	sqlQuery := ""
	if sq, ok := request.Params.Arguments["sql_query"].(string); ok {
		sqlQuery = sq
	}

	contents, err := s.adtClient.GetTableContents(ctx, tableName, maxRows, sqlQuery)
	if err != nil {
		return wrapErr("GetTableContents", err), nil
	}
	return newToolResultJSON(contents), nil
}

func (s *Server) handleRunQuery(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	sqlQuery, ok := request.Params.Arguments["sql_query"].(string)
	if !ok || sqlQuery == "" {
		return newToolResultError("sql_query is required"), nil
	}

	maxRows := 100
	if mr, ok := request.Params.Arguments["max_rows"].(float64); ok && mr > 0 {
		maxRows = int(mr)
	}

	contents, err := s.adtClient.RunQuery(ctx, sqlQuery, maxRows)
	if err != nil {
		return wrapErr("RunQuery", err), nil
	}
	return newToolResultJSON(contents), nil
}

func (s *Server) handleGetCDSDependencies(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	ddlsName, ok := request.Params.Arguments["ddls_name"].(string)
	if !ok || ddlsName == "" {
		return newToolResultError("ddls_name is required"), nil
	}

	opts := adt.CDSDependencyOptions{
		DependencyLevel:  "hierarchy",
		WithAssociations: false,
	}

	if level, ok := request.Params.Arguments["dependency_level"].(string); ok && level != "" {
		opts.DependencyLevel = level
	}

	if assoc, ok := request.Params.Arguments["with_associations"].(bool); ok {
		opts.WithAssociations = assoc
	}

	if pkg, ok := request.Params.Arguments["context_package"].(string); ok && pkg != "" {
		opts.ContextPackage = pkg
	}

	dependencyTree, err := s.adtClient.GetCDSDependencies(ctx, ddlsName, opts)
	if err != nil {
		return wrapErr("GetCDSDependencies", err), nil
	}

	// Add metadata summary
	return newToolResultJSON(map[string]any{
		"ddls_name":       ddlsName,
		"dependency_tree": dependencyTree,
		"statistics": map[string]any{
			"total_dependencies":    len(dependencyTree.FlattenDependencies()) - 1, // -1 to exclude root
			"dependency_depth":      dependencyTree.GetDependencyDepth(),
			"by_type":               dependencyTree.CountDependenciesByType(),
			"table_dependencies":    len(dependencyTree.GetTableDependencies()),
			"inactive_dependencies": len(dependencyTree.GetInactiveDependencies()),
			"cycles":                dependencyTree.FindCycles(),
		},
	}), nil
}

func (s *Server) handleGetStructure(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	structName, ok := request.Params.Arguments["structure_name"].(string)
	if !ok || structName == "" {
		return newToolResultError("structure_name is required"), nil
	}

	source, err := s.adtClient.GetStructure(ctx, structName)
	if err != nil {
		return wrapErr("GetStructure", err), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleGetPackage(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	pkg, err := s.adtClient.GetPackage(ctx, packageName)
	if err != nil {
		return wrapErr("GetPackage", err), nil
	}
	return newToolResultJSON(pkg), nil
}

func (s *Server) handleGetMessages(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	msgClass, ok := request.Params.Arguments["message_class"].(string)
	if !ok || msgClass == "" {
		return newToolResultError("message_class is required"), nil
	}

	mc, err := s.adtClient.GetMessageClass(ctx, msgClass)
	if err != nil {
		return wrapErr("GetMessageClass", err), nil
	}
	return newToolResultJSON(mc), nil
}

func (s *Server) handleGetTransaction(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	tcode, ok := request.Params.Arguments["transaction_name"].(string)
	if !ok || tcode == "" {
		return newToolResultError("transaction_name is required"), nil
	}

	tran, err := s.adtClient.GetTransaction(ctx, tcode)
	if err != nil {
		return wrapErr("GetTransaction", err), nil
	}
	return newToolResultJSON(tran), nil
}

func (s *Server) handleGetTypeInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	typeName, ok := request.Params.Arguments["type_name"].(string)
	if !ok || typeName == "" {
		return newToolResultError("type_name is required"), nil
	}

	typeInfo, err := s.adtClient.GetTypeInfo(ctx, typeName)
	if err != nil {
		return wrapErr("GetTypeInfo", err), nil
	}
	return newToolResultJSON(typeInfo), nil
}
