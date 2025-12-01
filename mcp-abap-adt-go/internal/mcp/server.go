// Package mcp provides the MCP server implementation for ABAP ADT tools.
package mcp

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/mark3labs/mcp-go/server"
	"github.com/vibingsteamer/mcp-abap-adt-go/pkg/adt"
)

// Server wraps the MCP server with ADT client.
type Server struct {
	mcpServer *server.MCPServer
	adtClient *adt.Client
}

// Config holds MCP server configuration.
type Config struct {
	// SAP connection settings
	BaseURL            string
	Username           string
	Password           string
	Client             string
	Language           string
	InsecureSkipVerify bool
}

// NewServer creates a new MCP server for ABAP ADT tools.
func NewServer(cfg *Config) *Server {
	// Create ADT client
	opts := []adt.Option{
		adt.WithClient(cfg.Client),
		adt.WithLanguage(cfg.Language),
	}
	if cfg.InsecureSkipVerify {
		opts = append(opts, adt.WithInsecureSkipVerify())
	}

	adtClient := adt.NewClient(cfg.BaseURL, cfg.Username, cfg.Password, opts...)

	// Create MCP server
	mcpServer := server.NewMCPServer(
		"mcp-abap-adt-go",
		"1.0.0",
		server.WithResourceCapabilities(true, true),
		server.WithLogging(),
	)

	s := &Server{
		mcpServer: mcpServer,
		adtClient: adtClient,
	}

	// Register all tools
	s.registerTools()

	return s
}

// ServeStdio starts the MCP server on stdin/stdout.
func (s *Server) ServeStdio() error {
	return server.ServeStdio(s.mcpServer)
}

// registerTools registers all ADT tools with the MCP server.
func (s *Server) registerTools() {
	// GetProgram
	s.mcpServer.AddTool(mcp.NewTool("GetProgram",
		mcp.WithDescription("Retrieve ABAP program source code"),
		mcp.WithString("program_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP program"),
		),
	), s.handleGetProgram)

	// GetClass
	s.mcpServer.AddTool(mcp.NewTool("GetClass",
		mcp.WithDescription("Retrieve ABAP class source code"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
	), s.handleGetClass)

	// GetInterface
	s.mcpServer.AddTool(mcp.NewTool("GetInterface",
		mcp.WithDescription("Retrieve ABAP interface source code"),
		mcp.WithString("interface_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP interface"),
		),
	), s.handleGetInterface)

	// GetFunction
	s.mcpServer.AddTool(mcp.NewTool("GetFunction",
		mcp.WithDescription("Retrieve ABAP Function Module source code"),
		mcp.WithString("function_name",
			mcp.Required(),
			mcp.Description("Name of the function module"),
		),
		mcp.WithString("function_group",
			mcp.Required(),
			mcp.Description("Name of the function group"),
		),
	), s.handleGetFunction)

	// GetFunctionGroup
	s.mcpServer.AddTool(mcp.NewTool("GetFunctionGroup",
		mcp.WithDescription("Retrieve ABAP Function Group source code"),
		mcp.WithString("function_group",
			mcp.Required(),
			mcp.Description("Name of the function group"),
		),
	), s.handleGetFunctionGroup)

	// GetInclude
	s.mcpServer.AddTool(mcp.NewTool("GetInclude",
		mcp.WithDescription("Retrieve ABAP Include Source Code"),
		mcp.WithString("include_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP Include"),
		),
	), s.handleGetInclude)

	// GetTable
	s.mcpServer.AddTool(mcp.NewTool("GetTable",
		mcp.WithDescription("Retrieve ABAP table structure"),
		mcp.WithString("table_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP table"),
		),
	), s.handleGetTable)

	// GetTableContents
	s.mcpServer.AddTool(mcp.NewTool("GetTableContents",
		mcp.WithDescription("Retrieve contents of an ABAP table"),
		mcp.WithString("table_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP table"),
		),
		mcp.WithNumber("max_rows",
			mcp.Description("Maximum number of rows to retrieve (default 100)"),
		),
	), s.handleGetTableContents)

	// GetStructure
	s.mcpServer.AddTool(mcp.NewTool("GetStructure",
		mcp.WithDescription("Retrieve ABAP Structure"),
		mcp.WithString("structure_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP Structure"),
		),
	), s.handleGetStructure)

	// GetPackage
	s.mcpServer.AddTool(mcp.NewTool("GetPackage",
		mcp.WithDescription("Retrieve ABAP package details"),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP package"),
		),
	), s.handleGetPackage)

	// GetTransaction
	s.mcpServer.AddTool(mcp.NewTool("GetTransaction",
		mcp.WithDescription("Retrieve ABAP transaction details"),
		mcp.WithString("transaction_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP transaction"),
		),
	), s.handleGetTransaction)

	// GetTypeInfo
	s.mcpServer.AddTool(mcp.NewTool("GetTypeInfo",
		mcp.WithDescription("Retrieve ABAP type information"),
		mcp.WithString("type_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP type"),
		),
	), s.handleGetTypeInfo)

	// SearchObject
	s.mcpServer.AddTool(mcp.NewTool("SearchObject",
		mcp.WithDescription("Search for ABAP objects using quick search"),
		mcp.WithString("query",
			mcp.Required(),
			mcp.Description("Search query string (use * wildcard for partial match)"),
		),
		mcp.WithNumber("maxResults",
			mcp.Description("Maximum number of results to return (default 100)"),
		),
	), s.handleSearchObject)
}

// newToolResultError creates an error result for tool execution failures.
func newToolResultError(message string) *mcp.CallToolResult {
	result := mcp.NewToolResultText(message)
	result.IsError = true
	return result
}

// Tool handlers

func (s *Server) handleGetProgram(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	programName, ok := request.Params.Arguments["program_name"].(string)
	if !ok || programName == "" {
		return newToolResultError("program_name is required"), nil
	}

	source, err := s.adtClient.GetProgram(ctx, programName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get program: %v", err)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get class: %v", err)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get interface: %v", err)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get function: %v", err)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get function group: %v", err)), nil
	}

	result, _ := json.MarshalIndent(fg, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetInclude(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	includeName, ok := request.Params.Arguments["include_name"].(string)
	if !ok || includeName == "" {
		return newToolResultError("include_name is required"), nil
	}

	source, err := s.adtClient.GetInclude(ctx, includeName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get include: %v", err)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get table: %v", err)), nil
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

	contents, err := s.adtClient.GetTableContents(ctx, tableName, maxRows)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get table contents: %v", err)), nil
	}

	result, _ := json.MarshalIndent(contents, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetStructure(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	structName, ok := request.Params.Arguments["structure_name"].(string)
	if !ok || structName == "" {
		return newToolResultError("structure_name is required"), nil
	}

	source, err := s.adtClient.GetStructure(ctx, structName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get structure: %v", err)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get package: %v", err)), nil
	}

	result, _ := json.MarshalIndent(pkg, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetTransaction(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	tcode, ok := request.Params.Arguments["transaction_name"].(string)
	if !ok || tcode == "" {
		return newToolResultError("transaction_name is required"), nil
	}

	tran, err := s.adtClient.GetTransaction(ctx, tcode)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get transaction: %v", err)), nil
	}

	result, _ := json.MarshalIndent(tran, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetTypeInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	typeName, ok := request.Params.Arguments["type_name"].(string)
	if !ok || typeName == "" {
		return newToolResultError("type_name is required"), nil
	}

	typeInfo, err := s.adtClient.GetTypeInfo(ctx, typeName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get type info: %v", err)), nil
	}

	result, _ := json.MarshalIndent(typeInfo, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

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
		return newToolResultError(fmt.Sprintf("Failed to search: %v", err)), nil
	}

	output, _ := json.MarshalIndent(results, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}
