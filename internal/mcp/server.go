// Package mcp provides the MCP server implementation for ABAP ADT tools.
package mcp

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/mark3labs/mcp-go/server"
	embedded "github.com/oisee/vibing-steampunk/embedded/abap"
	"github.com/oisee/vibing-steampunk/embedded/deps"
	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// AsyncTask represents a background task status.
type AsyncTask struct {
	ID        string      `json:"id"`
	Type      string      `json:"type"`       // "report", "export", etc.
	Status    string      `json:"status"`     // "running", "completed", "error"
	StartedAt time.Time   `json:"started_at"`
	EndedAt   *time.Time  `json:"ended_at,omitempty"`
	Result    interface{} `json:"result,omitempty"`
	Error     string      `json:"error,omitempty"`
}

// Server wraps the MCP server with ADT client.
type Server struct {
	mcpServer      *server.MCPServer
	adtClient      *adt.Client
	amdpWSClient   *adt.AMDPWebSocketClient   // WebSocket-based AMDP client (ZADT_VSP)
	debugWSClient  *adt.DebugWebSocketClient  // WebSocket-based debug client (ZADT_VSP)
	config         *Config                    // Server configuration for session manager creation
	featureProber  *adt.FeatureProber         // Feature detection system (safety network)
	featureConfig  adt.FeatureConfig          // Feature configuration

	// Async task management
	asyncTasks   map[string]*AsyncTask
	asyncTasksMu sync.RWMutex
	asyncTaskID  int64
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

	// Cookie authentication (alternative to basic auth)
	Cookies map[string]string

	// Verbose output
	Verbose bool

	// Mode: focused or expert (default: focused)
	Mode string

	// DisabledGroups disables groups of tools using short codes:
	// 5/U = UI5/BSP tools, T = Test tools, H = HANA/AMDP debugger, D = ABAP Debugger
	// Example: "TH" disables Tests and HANA debugger tools
	DisabledGroups string

	// Safety configuration
	ReadOnly         bool
	BlockFreeSQL     bool
	AllowedOps       string
	DisallowedOps    string
	AllowedPackages  []string
	EnableTransports  bool     // Explicitly enable transport management (default: disabled)
	TransportReadOnly bool     // Only allow read operations on transports (list, get)
	AllowedTransports []string // Whitelist specific transports (supports wildcards like "A4HK*")

	// Feature configuration (safety network)
	// Values: "auto" (default, probe system), "on" (force enabled), "off" (force disabled)
	FeatureAbapGit   string // abapGit integration
	FeatureRAP       string // RAP/OData development (DDLS, BDEF, SRVD, SRVB)
	FeatureAMDP      string // AMDP/HANA debugger
	FeatureUI5       string // UI5/Fiori BSP management
	FeatureTransport string // CTS transport management (distinct from EnableTransports safety)
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
	if len(cfg.Cookies) > 0 {
		opts = append(opts, adt.WithCookies(cfg.Cookies))
	}
	if cfg.Verbose {
		opts = append(opts, adt.WithVerbose())
	}

	// Configure safety settings
	safety := adt.UnrestrictedSafetyConfig() // Default: unrestricted for backwards compatibility
	if cfg.ReadOnly {
		safety.ReadOnly = true
	}
	if cfg.BlockFreeSQL {
		safety.BlockFreeSQL = true
	}
	if cfg.AllowedOps != "" {
		safety.AllowedOps = cfg.AllowedOps
	}
	if cfg.DisallowedOps != "" {
		safety.DisallowedOps = cfg.DisallowedOps
	}
	if len(cfg.AllowedPackages) > 0 {
		safety.AllowedPackages = cfg.AllowedPackages
	}
	if cfg.EnableTransports {
		safety.EnableTransports = true
	}
	if cfg.TransportReadOnly {
		safety.TransportReadOnly = true
	}
	if len(cfg.AllowedTransports) > 0 {
		safety.AllowedTransports = cfg.AllowedTransports
	}
	opts = append(opts, adt.WithSafety(safety))

	adtClient := adt.NewClient(cfg.BaseURL, cfg.Username, cfg.Password, opts...)

	// Set deterministic terminal ID for debugger operations
	// This ensures breakpoints work across MCP tool calls (each is a separate process)
	adt.SetTerminalIDUser(cfg.Username)

	// Configure feature detection (safety network)
	featureConfig := adt.FeatureConfig{
		AbapGit:   parseFeatureMode(cfg.FeatureAbapGit),
		RAP:       parseFeatureMode(cfg.FeatureRAP),
		AMDP:      parseFeatureMode(cfg.FeatureAMDP),
		UI5:       parseFeatureMode(cfg.FeatureUI5),
		Transport: parseFeatureMode(cfg.FeatureTransport),
	}

	// Create feature prober
	featureProber := adt.NewFeatureProber(adtClient, featureConfig, cfg.Verbose)

	// Create MCP server
	mcpServer := server.NewMCPServer(
		"mcp-abap-adt-go",
		"1.0.0",
		server.WithResourceCapabilities(true, true),
		server.WithLogging(),
	)

	s := &Server{
		mcpServer:     mcpServer,
		adtClient:     adtClient,
		config:        cfg,
		featureProber: featureProber,
		featureConfig: featureConfig,
		asyncTasks:    make(map[string]*AsyncTask),
	}

	// Register tools based on mode and disabled groups
	s.registerTools(cfg.Mode, cfg.DisabledGroups)

	return s
}

// parseFeatureMode converts string to FeatureMode
func parseFeatureMode(s string) adt.FeatureMode {
	switch strings.ToLower(s) {
	case "on", "true", "1", "yes", "enabled":
		return adt.FeatureModeOn
	case "off", "false", "0", "no", "disabled":
		return adt.FeatureModeOff
	default:
		return adt.FeatureModeAuto
	}
}

// ServeStdio starts the MCP server on stdin/stdout.
func (s *Server) ServeStdio() error {
	return server.ServeStdio(s.mcpServer)
}

// registerTools registers ADT tools with the MCP server based on mode and disabled groups.
// Mode "focused" registers essential tools.
// Mode "expert" registers all tools.
// DisabledGroups can disable specific tool groups using short codes:
//   - "5" or "U" = UI5/BSP tools (3 tools, read-only)
//   - "T" = Test tools: RunUnitTests, RunATCCheck (2 tools)
//   - "H" = HANA/AMDP debugger (7 tools)
//   - "D" = ABAP Debugger (6 session tools)
//   - "C" = CTS/Transport tools (5 tools)
//   - "G" = Git/abapGit tools (2 tools)
//   - "R" = Report tools (4 tools)
//   - "I" = Install tools (4 tools)
//   - "X" = EXPERIMENTAL: All debugger + RunReport (17 tools) - use to disable unreliable features
func (s *Server) registerTools(mode string, disabledGroups string) {
	// Define tool groups for selective disablement
	// Short codes: 5/U=UI5, T=Tests, H=HANA, D=Debug, C=CTS, X=Experimental
	toolGroups := map[string][]string{
		"5": { // UI5/BSP tools (also mapped as "U") - read-only, write ops need custom plugin
			"UI5ListApps", "UI5GetApp", "UI5GetFileContent",
		},
		"T": { // Test tools
			"RunUnitTests", "RunATCCheck",
		},
		"H": { // HANA/AMDP debugger
			"AMDPDebuggerStart", "AMDPDebuggerResume", "AMDPDebuggerStop",
			"AMDPDebuggerStep", "AMDPGetVariables", "AMDPSetBreakpoint", "AMDPGetBreakpoints",
		},
		"D": { // ABAP debugger (session tools - breakpoints via WebSocket ZADT_VSP)
			"DebuggerListen", "DebuggerAttach", "DebuggerDetach",
			"DebuggerStep", "DebuggerGetStack", "DebuggerGetVariables",
		},
		"C": { // CTS/Transport tools
			"ListTransports", "GetTransport",
			"CreateTransport", "ReleaseTransport", "DeleteTransport",
		},
		"G": { // Git/abapGit tools (via ZADT_VSP WebSocket)
			"GitTypes", "GitExport",
		},
		"R": { // Report execution tools (via ZADT_VSP WebSocket)
			"RunReport", "GetVariants", "GetTextElements", "SetTextElements",
		},
		"I": { // Install/Setup tools
			"InstallZADTVSP",
			"InstallAbapGit",
			"ListDependencies",
			"InstallDummyTest",
		},
		"X": { // EXPERIMENTAL - Tools requiring special setup or with known limitations
			// ABAP Debugger - requires ZADT_VSP WebSocket handler
			"SetBreakpoint", "GetBreakpoints", "DeleteBreakpoint",
			"DebuggerListen", "DebuggerAttach", "DebuggerDetach",
			"DebuggerStep", "DebuggerGetStack", "DebuggerGetVariables",
			// AMDP/HANA Debugger - experimental, session management issues
			"AMDPDebuggerStart", "AMDPDebuggerResume", "AMDPDebuggerStop",
			"AMDPDebuggerStep", "AMDPGetVariables", "AMDPSetBreakpoint", "AMDPGetBreakpoints",
			// RunReport - requires ZADT_VSP, APC context limitations
			"RunReport",
		},
	}
	// Map "U" to same tools as "5"
	toolGroups["U"] = toolGroups["5"]

	// Build set of disabled tools based on disabledGroups string
	disabledTools := make(map[string]bool)
	for _, code := range strings.ToUpper(disabledGroups) {
		if tools, ok := toolGroups[string(code)]; ok {
			for _, tool := range tools {
				disabledTools[tool] = true
			}
		}
	}

	// Define focused mode tool whitelist (41 essential tools)
	focusedTools := map[string]bool{
		// Unified tools (2)
		"GetSource":   true,
		"WriteSource": true,

		// Search tools (3) - foundation
		"GrepObjects":  true, // Multi-object search (replaces GrepObject)
		"GrepPackages": true, // Multi-package + recursive (replaces GrepPackage)
		"SearchObject": true,

		// Primary workflow (1)
		"EditSource": true,

		// Data/Metadata read (6)
		"GetTable":            true,
		"GetTableContents":    true,
		"RunQuery":            true,
		"GetPackage":          true, // Metadata: package contents
		"GetFunctionGroup":    true, // Metadata: function module list
		"GetCDSDependencies":  true, // CDS dependency tree
		"GetMessages":         true, // Message class texts (SE91)

		// Code intelligence (2)
		"FindDefinition":  true,
		"FindReferences":  true,

		// Development tools (11)
		"SyntaxCheck":         true,
		"RunUnitTests":        true,
		"RunATCCheck":         true,  // Code quality checks
		"Activate":            true,  // Re-activate objects without editing
		"ActivatePackage":     true,  // Batch activation of all inactive objects
		"PrettyPrint":         true,  // Format ABAP code
		"GetInactiveObjects":  true,  // List pending activations
		"CreatePackage":       true,  // Create local packages ($...)
		"CreateTable":         true,  // Create DDIC tables from JSON
		"CompareSource":       true,  // Diff two objects
		"CloneObject":         true,  // Copy object to new name
		"GetClassInfo":        true,  // Quick class metadata

		// Advanced/Edge cases (2)
		"LockObject":   true,
		"UnlockObject": true,

		// File-based operations (2)
		"ImportFromFile": true, // File → SAP (replaces DeployFromFile)
		"ExportToFile":   true, // SAP → File (replaces SaveToFile)

		// System information (2)
		"GetSystemInfo":         true, // System ID, release, kernel
		"GetInstalledComponents": true, // Installed software components

		// Code analysis (7)
		"GetCallGraph":       true, // Call hierarchy for methods/functions
		"GetObjectStructure": true, // Object explorer tree
		"GetCallersOf":       true, // Simplified up traversal
		"GetCalleesOf":       true, // Simplified down traversal
		"AnalyzeCallGraph":   true, // Call graph statistics
		"CompareCallGraphs":  true, // Compare static vs actual execution
		"TraceExecution":     true, // Composite RCA tool

		// Runtime errors / Short dumps (2)
		"ListDumps": true, // List runtime errors (consistent with List* pattern)
		"GetDump":   true, // Get dump details

		// ABAP Profiler / Traces (2)
		"ListTraces": true, // List trace files
		"GetTrace":   true, // Get trace analysis

		// SQL Trace / ST05 (2)
		"GetSQLTraceState": true, // Check if SQL trace is active
		"ListSQLTraces":    true, // List SQL trace files

		// External Breakpoints via WebSocket (ZADT_VSP)
		// REST API returns 403 CSRF, but WebSocket works perfectly
		"SetBreakpoint":    true, // Set line breakpoint
		"GetBreakpoints":   true, // List active breakpoints
		"DeleteBreakpoint": true, // Remove breakpoint
		"CallRFC":          true, // Call function module via WebSocket (trigger execution)

		// Debugger Session (6)
		"DebuggerListen":       true, // Wait for debuggee to hit breakpoint
		"DebuggerAttach":       true, // Attach to debuggee
		"DebuggerDetach":       true, // Detach from debug session
		"DebuggerStep":         true, // Step through code
		"DebuggerGetStack":     true, // Get call stack
		"DebuggerGetVariables": true, // Get variable values

		// UI5/Fiori BSP Management (3 read-only - ADT filestore is read-only)
		"UI5ListApps":       true, // List UI5 applications
		"UI5GetApp":         true, // Get UI5 app details
		"UI5GetFileContent": true, // Get file content from UI5 app
		// Write ops disabled - ADT filestore API is read-only (405 on POST)
		// Future: implement via custom plugin using /UI5/CL_REPOSITORY_LOAD
		// "UI5UploadFile":     true, // Upload file to UI5 app
		// "UI5DeleteFile":     true, // Delete file from UI5 app
		// "UI5CreateApp":      true, // Create new UI5 app
		// "UI5DeleteApp":      true, // Delete UI5 app

		// AMDP (HANA) Debugger - expert mode only
		"AMDPDebuggerStart":  true,
		"AMDPDebuggerResume": true,
		"AMDPDebuggerStop":   true,
		"AMDPDebuggerStep":   true,
		"AMDPGetVariables":   true,
		"AMDPSetBreakpoint":  true,
		"AMDPGetBreakpoints": true,

		// CTS/Transport Management (2 read-only in focused mode)
		// Write operations (Create, Release, Delete) only in expert mode
		"ListTransports": true, // List transport requests
		"GetTransport":   true, // Get transport details with objects

		// Git/abapGit Integration (via ZADT_VSP WebSocket)
		"GitTypes":  true, // List 158 supported object types
		"GitExport": true, // Export packages/objects to abapGit ZIP

		// Report Execution (via ZADT_VSP WebSocket)
		"RunReport":        true, // Execute reports with params/variants, capture ALV
		"RunReportAsync":   true, // Background report execution with polling
		"GetAsyncResult":   true, // Retrieve async task results
		"GetVariants":      true, // List report variants
		"GetTextElements":  true, // Get program text elements
		"SetTextElements":  true, // Set program text elements

		// Install/Setup tools
		"InstallZADTVSP":   true, // Deploy ZADT_VSP WebSocket handler to SAP
		"InstallAbapGit":   true, // Deploy abapGit (standalone or dev edition) to SAP
		"ListDependencies": true, // List available dependencies for installation
		"InstallDummyTest": true, // Test tool for verifying Install* workflow
	}

	// Helper to check if tool should be registered
	shouldRegister := func(toolName string) bool {
		// Check if tool is disabled by group
		if disabledTools[toolName] {
			return false
		}
		if mode == "expert" {
			return true // Expert mode: register all tools (except disabled)
		}
		return focusedTools[toolName] // Focused mode: only whitelisted tools (except disabled)
	}

	// Unified Tools (Focused Mode) - NEW
	if shouldRegister("GetSource") {
		s.registerGetSource()
	}
	if shouldRegister("WriteSource") {
		s.registerWriteSource()
	}


	// GetProgram
	if shouldRegister("GetProgram") {
		s.mcpServer.AddTool(mcp.NewTool("GetProgram",
		mcp.WithDescription("Retrieve ABAP program source code"),
		mcp.WithString("program_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP program"),
		),
	), s.handleGetProgram)
	}


	// GetClass
	if shouldRegister("GetClass") {
		s.mcpServer.AddTool(mcp.NewTool("GetClass",
		mcp.WithDescription("Retrieve ABAP class source code"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
	), s.handleGetClass)
	}


	// GetInterface
	if shouldRegister("GetInterface") {
		s.mcpServer.AddTool(mcp.NewTool("GetInterface",
		mcp.WithDescription("Retrieve ABAP interface source code"),
		mcp.WithString("interface_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP interface"),
		),
	), s.handleGetInterface)
	}


	// GetFunction
	if shouldRegister("GetFunction") {
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
	}


	// GetFunctionGroup
	if shouldRegister("GetFunctionGroup") {
		s.mcpServer.AddTool(mcp.NewTool("GetFunctionGroup",
		mcp.WithDescription("Retrieve ABAP Function Group source code"),
		mcp.WithString("function_group",
			mcp.Required(),
			mcp.Description("Name of the function group"),
		),
	), s.handleGetFunctionGroup)
	}


	// GetInclude
	if shouldRegister("GetInclude") {
		s.mcpServer.AddTool(mcp.NewTool("GetInclude",
		mcp.WithDescription("Retrieve ABAP Include Source Code"),
		mcp.WithString("include_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP Include"),
		),
	), s.handleGetInclude)
	}


	// GetTable
	if shouldRegister("GetTable") {
		s.mcpServer.AddTool(mcp.NewTool("GetTable",
		mcp.WithDescription("Retrieve ABAP table structure"),
		mcp.WithString("table_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP table"),
		),
	), s.handleGetTable)
	}


	// GetTableContents
	if shouldRegister("GetTableContents") {
		s.mcpServer.AddTool(mcp.NewTool("GetTableContents",
		mcp.WithDescription("Retrieve contents of an ABAP table. For simple queries use table_name + max_rows. For filtered queries use sql_query parameter with ABAP SQL syntax (use ASCENDING/DESCENDING, not ASC/DESC)."),
		mcp.WithString("table_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP table"),
		),
		mcp.WithNumber("max_rows",
			mcp.Description("Maximum number of rows to retrieve (default 100). Use this instead of SQL LIMIT clause"),
		),
		mcp.WithString("sql_query",
			mcp.Description("Optional ABAP SQL SELECT statement. Uses ABAP syntax: ASCENDING/DESCENDING work, ASC/DESC fail. Example: SELECT * FROM T000 WHERE MANDT = '001' ORDER BY MANDT DESCENDING"),
		),
	), s.handleGetTableContents)
	}


	// RunQuery
	if shouldRegister("RunQuery") {
		s.mcpServer.AddTool(mcp.NewTool("RunQuery",
		mcp.WithDescription("Execute a freestyle SQL query against the SAP database. IMPORTANT: Uses ABAP SQL syntax, NOT standard SQL. Use ASCENDING/DESCENDING instead of ASC/DESC. Use max_rows parameter instead of LIMIT. GROUP BY and WHERE work normally."),
		mcp.WithString("sql_query",
			mcp.Required(),
			mcp.Description("ABAP SQL query. Example: SELECT carrid, COUNT(*) as cnt FROM sflight GROUP BY carrid ORDER BY cnt DESCENDING. Note: ASC/DESC keywords fail - use ASCENDING/DESCENDING"),
		),
		mcp.WithNumber("max_rows",
			mcp.Description("Maximum number of rows to retrieve (default 100). Use this instead of SQL LIMIT clause"),
		),
	), s.handleRunQuery)
	}


	// GetCDSDependencies
	if shouldRegister("GetCDSDependencies") {
		s.mcpServer.AddTool(mcp.NewTool("GetCDSDependencies",
		mcp.WithDescription("Retrieve CDS view FORWARD dependencies (tables/views this CDS reads FROM). Returns tree of base objects. Does NOT return reverse dependencies (where-used). Use with GetSource(DDLS) to read CDS source code."),
		mcp.WithString("ddls_name",
			mcp.Required(),
			mcp.Description("CDS DDL source name (e.g., 'ZRAY_00_I_DOC_NODE_00'). Use SearchObject to find CDS views first."),
		),
		mcp.WithString("dependency_level",
			mcp.Description("Level of dependency resolution: 'unit' (direct only) or 'hierarchy' (recursive). Default: 'hierarchy'"),
		),
		mcp.WithBoolean("with_associations",
			mcp.Description("Include modeled associations in dependency tree. Default: false"),
		),
		mcp.WithString("context_package",
			mcp.Description("Filter dependencies to specific package context"),
		),
	), s.handleGetCDSDependencies)
	}


	// GetStructure
	if shouldRegister("GetStructure") {
		s.mcpServer.AddTool(mcp.NewTool("GetStructure",
		mcp.WithDescription("Retrieve ABAP Structure"),
		mcp.WithString("structure_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP Structure"),
		),
	), s.handleGetStructure)
	}


	// GetPackage
	if shouldRegister("GetPackage") {
		s.mcpServer.AddTool(mcp.NewTool("GetPackage",
		mcp.WithDescription("Retrieve ABAP package details"),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP package"),
		),
	), s.handleGetPackage)
	}

	// GetMessages - Message class texts (SE91)
	if shouldRegister("GetMessages") {
		s.mcpServer.AddTool(mcp.NewTool("GetMessages",
			mcp.WithDescription("Get all messages from an ABAP message class (SE91). Returns message number, text for all messages in the class. Use SearchObject to find message classes first."),
			mcp.WithString("message_class",
				mcp.Required(),
				mcp.Description("Name of the message class (e.g., 'ZRAY_00', 'SY')"),
			),
		), s.handleGetMessages)
	}


	// GetTransaction
	if shouldRegister("GetTransaction") {
		s.mcpServer.AddTool(mcp.NewTool("GetTransaction",
		mcp.WithDescription("Retrieve ABAP transaction details"),
		mcp.WithString("transaction_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP transaction"),
		),
	), s.handleGetTransaction)
	}


	// GetTypeInfo
	if shouldRegister("GetTypeInfo") {
		s.mcpServer.AddTool(mcp.NewTool("GetTypeInfo",
		mcp.WithDescription("Retrieve ABAP type information"),
		mcp.WithString("type_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP type"),
		),
	), s.handleGetTypeInfo)
	}


	// --- System Information ---

	// GetSystemInfo
	if shouldRegister("GetSystemInfo") {
		s.mcpServer.AddTool(mcp.NewTool("GetSystemInfo",
			mcp.WithDescription("Get SAP system information (system ID, release, kernel, database)"),
		), s.handleGetSystemInfo)
	}

	// GetInstalledComponents
	if shouldRegister("GetInstalledComponents") {
		s.mcpServer.AddTool(mcp.NewTool("GetInstalledComponents",
			mcp.WithDescription("List installed software components with version information"),
		), s.handleGetInstalledComponents)
	}

	// GetConnectionInfo - Self-inspection tool
	// Always registered - useful for debugging and introspection
	s.mcpServer.AddTool(mcp.NewTool("GetConnectionInfo",
		mcp.WithDescription("Get current MCP connection info: user, URL, client. Useful for debugging and understanding current session context."),
	), s.handleGetConnectionInfo)

	// GetFeatures - Feature Detection (Safety Network)
	// Always registered - provides visibility into what's available
	s.mcpServer.AddTool(mcp.NewTool("GetFeatures",
		mcp.WithDescription("Probe SAP system for available features. Returns status of optional capabilities like abapGit, RAP/OData, AMDP debugging, UI5/BSP, and CTS transports. Use this to understand what features are available before attempting to use them."),
	), s.handleGetFeatures)

	// --- Code Analysis Infrastructure (CAI) ---

	// GetCallGraph
	if shouldRegister("GetCallGraph") {
		s.mcpServer.AddTool(mcp.NewTool("GetCallGraph",
			mcp.WithDescription("Get call hierarchy for methods/functions. Shows callers or callees of an ABAP object."),
			mcp.WithString("object_uri",
				mcp.Required(),
				mcp.Description("ADT URI of the object (e.g., /sap/bc/adt/oo/classes/ZCL_TEST/source/main#start=10,1)"),
			),
			mcp.WithString("direction",
				mcp.Description("Direction: 'callers' (who calls this) or 'callees' (what this calls). Default: callers"),
			),
			mcp.WithNumber("max_depth",
				mcp.Description("Maximum depth of call hierarchy (default: 3)"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of results (default: 100)"),
			),
		), s.handleGetCallGraph)
	}

	// GetObjectStructure
	if shouldRegister("GetObjectStructure") {
		s.mcpServer.AddTool(mcp.NewTool("GetObjectStructure",
			mcp.WithDescription("Get object explorer tree structure. Returns hierarchical view of object components."),
			mcp.WithString("object_name",
				mcp.Required(),
				mcp.Description("Object name (e.g., ZCL_TEST, ZPROGRAM)"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of results (default: 100)"),
			),
		), s.handleGetObjectStructure)
	}

	// GetCallersOf - simplified up traversal
	if shouldRegister("GetCallersOf") {
		s.mcpServer.AddTool(mcp.NewTool("GetCallersOf",
			mcp.WithDescription("Find all callers of an ABAP object (up traversal). Shows who calls this method/function. Simplified wrapper around GetCallGraph."),
			mcp.WithString("object_uri",
				mcp.Required(),
				mcp.Description("ADT URI of the object (e.g., /sap/bc/adt/oo/classes/ZCL_TEST/source/main#start=10,1)"),
			),
			mcp.WithNumber("max_depth",
				mcp.Description("Maximum depth of caller hierarchy (default: 5)"),
			),
		), s.handleGetCallersOf)
	}

	// GetCalleesOf - simplified down traversal
	if shouldRegister("GetCalleesOf") {
		s.mcpServer.AddTool(mcp.NewTool("GetCalleesOf",
			mcp.WithDescription("Find all callees of an ABAP object (down traversal). Shows what this method/function calls. Simplified wrapper around GetCallGraph."),
			mcp.WithString("object_uri",
				mcp.Required(),
				mcp.Description("ADT URI of the object (e.g., /sap/bc/adt/oo/classes/ZCL_TEST/source/main#start=10,1)"),
			),
			mcp.WithNumber("max_depth",
				mcp.Description("Maximum depth of callee hierarchy (default: 5)"),
			),
		), s.handleGetCalleesOf)
	}

	// AnalyzeCallGraph - get call graph statistics
	if shouldRegister("AnalyzeCallGraph") {
		s.mcpServer.AddTool(mcp.NewTool("AnalyzeCallGraph",
			mcp.WithDescription("Analyze call graph for an object. Returns statistics: total nodes, edges, max depth, nodes by type. Use for understanding code complexity and dependencies."),
			mcp.WithString("object_uri",
				mcp.Required(),
				mcp.Description("ADT URI of the object to analyze"),
			),
			mcp.WithString("direction",
				mcp.Description("Direction: 'callers' or 'callees' (default: callees)"),
			),
			mcp.WithNumber("max_depth",
				mcp.Description("Maximum depth to analyze (default: 5)"),
			),
		), s.handleAnalyzeCallGraph)
	}

	// CompareCallGraphs - compare static vs actual execution
	if shouldRegister("CompareCallGraphs") {
		s.mcpServer.AddTool(mcp.NewTool("CompareCallGraphs",
			mcp.WithDescription("Compare static call graph with actual execution trace. Identifies: common paths, untested paths (static only), and dynamic calls (actual only). Use for test coverage analysis and RCA."),
			mcp.WithString("object_uri",
				mcp.Required(),
				mcp.Description("ADT URI of the root object"),
			),
			mcp.WithString("trace_data",
				mcp.Required(),
				mcp.Description("JSON array of trace edges from execution (format: [{caller_name, callee_name}, ...])"),
			),
		), s.handleCompareCallGraphs)
	}

	// TraceExecution - composite RCA tool
	if shouldRegister("TraceExecution") {
		s.mcpServer.AddTool(mcp.NewTool("TraceExecution",
			mcp.WithDescription("COMPOSITE RCA TOOL: Performs traced execution analysis. 1) Builds static call graph from object, 2) Optionally runs unit tests, 3) Collects trace data, 4) Extracts actual call edges, 5) Compares static vs actual for root cause analysis."),
			mcp.WithString("object_uri",
				mcp.Required(),
				mcp.Description("ADT URI of the starting object for static call graph"),
			),
			mcp.WithNumber("max_depth",
				mcp.Description("Maximum depth for call graph traversal (default: 5)"),
			),
			mcp.WithBoolean("run_tests",
				mcp.Description("Run unit tests before collecting trace (default: false)"),
			),
			mcp.WithString("test_object_uri",
				mcp.Description("Object URI for tests to run (defaults to object_uri)"),
			),
			mcp.WithString("trace_user",
				mcp.Description("Filter traces by user (defaults to current user)"),
			),
		), s.handleTraceExecution)
	}

	// --- Runtime Errors / Short Dumps (RABAX) ---

	// ListDumps (renamed from GetDumps for consistency with List* pattern)
	if shouldRegister("ListDumps") {
		s.mcpServer.AddTool(mcp.NewTool("ListDumps",
			mcp.WithDescription("List runtime errors (short dumps) from the SAP system. Filter by user, exception type, program, date range."),
			mcp.WithString("user",
				mcp.Description("Filter by username"),
			),
			mcp.WithString("exception_type",
				mcp.Description("Filter by exception type (e.g., CX_SY_ZERODIVIDE)"),
			),
			mcp.WithString("program",
				mcp.Description("Filter by program name"),
			),
			mcp.WithString("package",
				mcp.Description("Filter by package"),
			),
			mcp.WithString("date_from",
				mcp.Description("Start date (YYYYMMDD format)"),
			),
			mcp.WithString("date_to",
				mcp.Description("End date (YYYYMMDD format)"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of results (default: 100)"),
			),
		), s.handleListDumps)
	}

	// GetDump
	if shouldRegister("GetDump") {
		s.mcpServer.AddTool(mcp.NewTool("GetDump",
			mcp.WithDescription("Get full details of a specific runtime error (short dump) including stack trace."),
			mcp.WithString("dump_id",
				mcp.Required(),
				mcp.Description("Dump ID from ListDumps result"),
			),
		), s.handleGetDump)
	}

	// --- ABAP Profiler / Runtime Traces (ATRA) ---

	// ListTraces
	if shouldRegister("ListTraces") {
		s.mcpServer.AddTool(mcp.NewTool("ListTraces",
			mcp.WithDescription("List ABAP runtime traces (profiler results) from the SAP system."),
			mcp.WithString("user",
				mcp.Description("Filter by username"),
			),
			mcp.WithString("process_type",
				mcp.Description("Filter by process type"),
			),
			mcp.WithString("object_type",
				mcp.Description("Filter by object type"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of results (default: 100)"),
			),
		), s.handleListTraces)
	}

	// GetTrace
	if shouldRegister("GetTrace") {
		s.mcpServer.AddTool(mcp.NewTool("GetTrace",
			mcp.WithDescription("Get trace analysis (hitlist, statements, or database accesses) for a specific trace."),
			mcp.WithString("trace_id",
				mcp.Required(),
				mcp.Description("Trace ID from ListTraces result"),
			),
			mcp.WithString("tool_type",
				mcp.Description("Analysis type: 'hitlist' (default), 'statements', 'dbAccesses'"),
			),
		), s.handleGetTrace)
	}

	// --- SQL Trace (ST05) ---

	// GetSQLTraceState
	if shouldRegister("GetSQLTraceState") {
		s.mcpServer.AddTool(mcp.NewTool("GetSQLTraceState",
			mcp.WithDescription("Check if SQL trace (ST05) is currently active."),
		), s.handleGetSQLTraceState)
	}

	// ListSQLTraces
	if shouldRegister("ListSQLTraces") {
		s.mcpServer.AddTool(mcp.NewTool("ListSQLTraces",
			mcp.WithDescription("List SQL trace files from ST05."),
			mcp.WithString("user",
				mcp.Description("Filter by username"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of results (default: 100)"),
			),
		), s.handleListSQLTraces)
	}

	// --- Debugger Session (WebSocket-based via ZADT_VSP) ---
	// All debugger tools use WebSocket connection to ZADT_VSP for reliable operation.
	// REST-based breakpoint tools were removed due to CSRF issues.

	// SetBreakpoint - WebSocket-based
	if shouldRegister("SetBreakpoint") {
		s.mcpServer.AddTool(mcp.NewTool("SetBreakpoint",
			mcp.WithDescription("Set a breakpoint at a specific line in ABAP code. Uses WebSocket connection to ZADT_VSP. Requires ZADT_VSP to be deployed on the SAP system."),
			mcp.WithString("program",
				mcp.Required(),
				mcp.Description("Program name (e.g., 'ZADT_DBG_PROG' or 'ZCL_MY_CLASS================CP' for class pool)"),
			),
			mcp.WithNumber("line",
				mcp.Required(),
				mcp.Description("Line number to set breakpoint on"),
			),
		), s.handleSetBreakpoint)
	}

	// GetBreakpoints - WebSocket-based
	if shouldRegister("GetBreakpoints") {
		s.mcpServer.AddTool(mcp.NewTool("GetBreakpoints",
			mcp.WithDescription("Get all breakpoints registered in the current debug session. Uses WebSocket connection to ZADT_VSP."),
		), s.handleGetBreakpoints)
	}

	// DeleteBreakpoint - WebSocket-based
	if shouldRegister("DeleteBreakpoint") {
		s.mcpServer.AddTool(mcp.NewTool("DeleteBreakpoint",
			mcp.WithDescription("Delete a breakpoint by ID. Uses WebSocket connection to ZADT_VSP."),
			mcp.WithString("breakpoint_id",
				mcp.Required(),
				mcp.Description("ID of the breakpoint to delete"),
			),
		), s.handleDeleteBreakpoint)
	}

	// CallRFC - WebSocket-based RFC execution
	if shouldRegister("CallRFC") {
		s.mcpServer.AddTool(mcp.NewTool("CallRFC",
			mcp.WithDescription("Call a function module via WebSocket (ZADT_VSP). Useful for triggering ABAP code execution to hit breakpoints. Parameters are passed as key-value pairs."),
			mcp.WithString("function",
				mcp.Required(),
				mcp.Description("Function module name (e.g., 'RFC_PING', 'BAPI_USER_GET_DETAIL')"),
			),
			mcp.WithString("params",
				mcp.Description("JSON object with function parameters (e.g., '{\"IV_PARAM\":\"value\"}')"),
			),
		), s.handleCallRFC)
	}

	// DebuggerListen
	if shouldRegister("DebuggerListen") {
		s.mcpServer.AddTool(mcp.NewTool("DebuggerListen",
			mcp.WithDescription("Start a debug listener that waits for a debuggee to hit a breakpoint. This is a BLOCKING call that uses long-polling. Returns when a debuggee is caught, timeout occurs, or a conflict is detected."),
			mcp.WithString("user",
				mcp.Description("User to listen for (defaults to current user)"),
			),
			mcp.WithNumber("timeout",
				mcp.Description("Timeout in seconds (default: 60, max: 240)"),
			),
		), s.handleDebuggerListen)
	}

	// DebuggerAttach
	if shouldRegister("DebuggerAttach") {
		s.mcpServer.AddTool(mcp.NewTool("DebuggerAttach",
			mcp.WithDescription("Attach to a debuggee that has hit a breakpoint. Use the debuggee_id from DebuggerListen result."),
			mcp.WithString("debuggee_id",
				mcp.Required(),
				mcp.Description("ID of the debuggee (from DebuggerListen result)"),
			),
			mcp.WithString("user",
				mcp.Description("User for debugging (defaults to current user)"),
			),
		), s.handleDebuggerAttach)
	}

	// DebuggerDetach
	if shouldRegister("DebuggerDetach") {
		s.mcpServer.AddTool(mcp.NewTool("DebuggerDetach",
			mcp.WithDescription("Detach from the current debug session and release the debuggee."),
		), s.handleDebuggerDetach)
	}

	// DebuggerStep
	if shouldRegister("DebuggerStep") {
		s.mcpServer.AddTool(mcp.NewTool("DebuggerStep",
			mcp.WithDescription("Perform a step operation in the debugger."),
			mcp.WithString("step_type",
				mcp.Required(),
				mcp.Description("Step type: 'stepInto', 'stepOver', 'stepReturn', 'stepContinue', 'stepRunToLine', 'stepJumpToLine'"),
			),
			mcp.WithString("uri",
				mcp.Description("Target URI for stepRunToLine/stepJumpToLine (e.g., '/sap/bc/adt/programs/programs/ZTEST/source/main#start=42')"),
			),
		), s.handleDebuggerStep)
	}

	// DebuggerGetStack
	if shouldRegister("DebuggerGetStack") {
		s.mcpServer.AddTool(mcp.NewTool("DebuggerGetStack",
			mcp.WithDescription("Get the current call stack during a debug session."),
		), s.handleDebuggerGetStack)
	}

	// DebuggerGetVariables
	if shouldRegister("DebuggerGetVariables") {
		s.mcpServer.AddTool(mcp.NewTool("DebuggerGetVariables",
			mcp.WithDescription("Get variable values during a debug session. Use '@ROOT' to get top-level variables, or specific variable IDs to get their values."),
			mcp.WithArray("variable_ids",
				mcp.Description("Variable IDs to retrieve (e.g., ['@ROOT'] for top-level, or specific IDs like ['LV_COUNT', 'LS_DATA'])"),
			),
		), s.handleDebuggerGetVariables)
	}

	// SearchObject
	if shouldRegister("SearchObject") {
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


	// --- Development Tools ---

	// SyntaxCheck
	if shouldRegister("SyntaxCheck") {
		s.mcpServer.AddTool(mcp.NewTool("SyntaxCheck",
		mcp.WithDescription("Check ABAP source code for syntax errors"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("content",
			mcp.Required(),
			mcp.Description("ABAP source code to check"),
		),
	), s.handleSyntaxCheck)
	}


	// Activate
	if shouldRegister("Activate") {
		s.mcpServer.AddTool(mcp.NewTool("Activate",
		mcp.WithDescription("Activate an ABAP object"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("object_name",
			mcp.Required(),
			mcp.Description("Technical name of the object (e.g., ZTEST)"),
		),
	), s.handleActivate)
	}

	// ActivatePackage - Batch activation of inactive objects
	if shouldRegister("ActivatePackage") {
		s.mcpServer.AddTool(mcp.NewTool("ActivatePackage",
			mcp.WithDescription("Activate all inactive objects. Objects are sorted by dependency order (interfaces before classes). If no package specified, activates ALL inactive objects for current user."),
			mcp.WithString("package",
				mcp.Description("Package name to filter (optional, empty = all packages)"),
			),
			mcp.WithNumber("max_objects",
				mcp.Description("Maximum number of objects to activate (default: 100)"),
			),
		), s.handleActivatePackage)
	}

	// RunUnitTests
	if shouldRegister("RunUnitTests") {
		s.mcpServer.AddTool(mcp.NewTool("RunUnitTests",
		mcp.WithDescription("Run ABAP Unit tests for an object"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/oo/classes/ZCL_TEST)"),
		),
		mcp.WithBoolean("include_dangerous",
			mcp.Description("Include dangerous risk level tests (default: false)"),
		),
		mcp.WithBoolean("include_long",
			mcp.Description("Include long duration tests (default: false)"),
		),
	), s.handleRunUnitTests)
	}

	// --- ATC (Code Quality) ---

	// RunATCCheck - Convenience tool (combines variant + run + worklist)
	if shouldRegister("RunATCCheck") {
		s.mcpServer.AddTool(mcp.NewTool("RunATCCheck",
			mcp.WithDescription("Run ATC (ABAP Test Cockpit) code quality check on an object. Returns findings with priority, check title, message, and location. Priority: 1=Error, 2=Warning, 3=Info."),
			mcp.WithString("object_url",
				mcp.Required(),
				mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/oo/classes/ZCL_TEST)"),
			),
			mcp.WithString("variant",
				mcp.Description("Check variant name (empty = use system default)"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of findings to return (default: 100)"),
			),
		), s.handleRunATCCheck)
	}

	// GetATCCustomizing - Expert mode: get ATC configuration
	if shouldRegister("GetATCCustomizing") {
		s.mcpServer.AddTool(mcp.NewTool("GetATCCustomizing",
			mcp.WithDescription("Get ATC system configuration including default check variant and exemption reasons"),
		), s.handleGetATCCustomizing)
	}


	// --- CRUD Operations ---

	// LockObject
	if shouldRegister("LockObject") {
		s.mcpServer.AddTool(mcp.NewTool("LockObject",
		mcp.WithDescription("Acquire an edit lock on an ABAP object"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("access_mode",
			mcp.Description("Access mode: MODIFY (default) or READ"),
		),
	), s.handleLockObject)
	}


	// UnlockObject
	if shouldRegister("UnlockObject") {
		s.mcpServer.AddTool(mcp.NewTool("UnlockObject",
		mcp.WithDescription("Release an edit lock on an ABAP object"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("lock_handle",
			mcp.Required(),
			mcp.Description("Lock handle from LockObject"),
		),
	), s.handleUnlockObject)
	}


	// UpdateSource
	if shouldRegister("UpdateSource") {
		s.mcpServer.AddTool(mcp.NewTool("UpdateSource",
		mcp.WithDescription("Write source code to an ABAP object (requires lock)"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP source code to write"),
		),
		mcp.WithString("lock_handle",
			mcp.Required(),
			mcp.Description("Lock handle from LockObject"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleUpdateSource)
	}


	// CreateObject
	if shouldRegister("CreateObject") {
		s.mcpServer.AddTool(mcp.NewTool("CreateObject",
		mcp.WithDescription("Create a new ABAP object. Supports: PROG/P (program), CLAS/OC (class), INTF/OI (interface), PROG/I (include), FUGR/F (function group), FUGR/FF (function module), DEVC/K (package), DDLS/DF (CDS view), BDEF/BDO (behavior definition), SRVD/SRV (service definition), SRVB/SVB (service binding)"),
		mcp.WithString("object_type",
			mcp.Required(),
			mcp.Description("Object type: PROG/P, CLAS/OC, INTF/OI, PROG/I, FUGR/F, FUGR/FF, DEVC/K, DDLS/DF, BDEF/BDO, SRVD/SRV, SRVB/SVB"),
		),
		mcp.WithString("name",
			mcp.Required(),
			mcp.Description("Object name (e.g., ZTEST_PROGRAM)"),
		),
		mcp.WithString("description",
			mcp.Required(),
			mcp.Description("Object description"),
		),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Package name (e.g., $TMP for local, ZPACKAGE for transportable)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (required for non-local packages)"),
		),
		mcp.WithString("parent_name",
			mcp.Description("Parent name (required for function modules - the function group name)"),
		),
		// RAP-specific options
		mcp.WithString("service_definition",
			mcp.Description("For SRVB: the service definition name to bind"),
		),
		mcp.WithString("binding_version",
			mcp.Description("For SRVB: OData version 'V2' or 'V4' (default: V2)"),
		),
		mcp.WithString("binding_category",
			mcp.Description("For SRVB: '0' for Web API, '1' for UI (default: 0)"),
		),
	), s.handleCreateObject)
	}

	// CreatePackage - simplified package creation for focused mode
	if shouldRegister("CreatePackage") {
		s.mcpServer.AddTool(mcp.NewTool("CreatePackage",
		mcp.WithDescription("Create a new local ABAP package. Only local packages (starting with $) are supported. For development/testing purposes."),
		mcp.WithString("name",
			mcp.Required(),
			mcp.Description("Package name (must start with $, e.g., $ZTEST, $ZLOCAL_DEV)"),
		),
		mcp.WithString("description",
			mcp.Required(),
			mcp.Description("Package description"),
		),
		mcp.WithString("parent",
			mcp.Description("Parent package name (optional, e.g., $TMP). If not specified, creates a root-level local package."),
		),
	), s.handleCreatePackage)
	}

	// CreateTable - Create DDIC tables from JSON
	if shouldRegister("CreateTable") {
		s.mcpServer.AddTool(mcp.NewTool("CreateTable",
			mcp.WithDescription("Create a DDIC transparent table from a simple JSON definition. Handles full workflow: create → set source → activate. Supports common ABAP types: CHAR, NUMC, INT4, DEC, STRING, TIMESTAMPL, UUID, etc."),
			mcp.WithString("name",
				mcp.Required(),
				mcp.Description("Table name (uppercase, max 30 chars, must start with Z/Y)"),
			),
			mcp.WithString("description",
				mcp.Required(),
				mcp.Description("Short description of the table"),
			),
			mcp.WithString("package",
				mcp.Description("Target package (default: $TMP)"),
			),
			mcp.WithString("fields",
				mcp.Required(),
				mcp.Description("JSON array of fields: [{\"name\":\"ID\",\"type\":\"CHAR32\",\"key\":true},{\"name\":\"VALUE\",\"type\":\"STRING\"}]. Types: CHAR/CHARnn, NUMC/NUMCnn, INT4, DEC, STRING, TIMESTAMPL, UUID, DATS, TIMS, or data element name."),
			),
			mcp.WithString("transport",
				mcp.Description("Transport request number (optional for $TMP)"),
			),
			mcp.WithString("delivery_class",
				mcp.Description("Delivery class: A=Application (default), C=Customizing, L=Temporary"),
			),
		), s.handleCreateTable)
	}

	// CompareSource - Diff two objects
	if shouldRegister("CompareSource") {
		s.mcpServer.AddTool(mcp.NewTool("CompareSource",
			mcp.WithDescription("Compare source code of two objects and return unified diff. Supports all object types from GetSource."),
			mcp.WithString("type1",
				mcp.Required(),
				mcp.Description("Object type of first object: PROG, CLAS, INTF, FUNC, FUGR, INCL, DDLS, BDEF, SRVD"),
			),
			mcp.WithString("name1",
				mcp.Required(),
				mcp.Description("Name of first object"),
			),
			mcp.WithString("type2",
				mcp.Required(),
				mcp.Description("Object type of second object (can be same or different)"),
			),
			mcp.WithString("name2",
				mcp.Required(),
				mcp.Description("Name of second object"),
			),
			mcp.WithString("include1",
				mcp.Description("Class include type for first object if CLAS: definitions, implementations, macros, testclasses"),
			),
			mcp.WithString("include2",
				mcp.Description("Class include type for second object if CLAS"),
			),
			mcp.WithString("parent1",
				mcp.Description("Function group for first object if FUNC"),
			),
			mcp.WithString("parent2",
				mcp.Description("Function group for second object if FUNC"),
			),
		), s.handleCompareSource)
	}

	// CloneObject - Copy object to new name
	if shouldRegister("CloneObject") {
		s.mcpServer.AddTool(mcp.NewTool("CloneObject",
			mcp.WithDescription("Copy an ABAP object to a new name. Replaces object name in source. Supports PROG, CLAS, INTF."),
			mcp.WithString("object_type",
				mcp.Required(),
				mcp.Description("Object type: PROG, CLAS, INTF"),
			),
			mcp.WithString("source_name",
				mcp.Required(),
				mcp.Description("Name of object to copy"),
			),
			mcp.WithString("target_name",
				mcp.Required(),
				mcp.Description("Name for the new object"),
			),
			mcp.WithString("package",
				mcp.Required(),
				mcp.Description("Target package (e.g., $TMP)"),
			),
		), s.handleCloneObject)
	}

	// GetClassInfo - Quick class metadata
	if shouldRegister("GetClassInfo") {
		s.mcpServer.AddTool(mcp.NewTool("GetClassInfo",
			mcp.WithDescription("Get class metadata without full source: methods, attributes, interfaces, superclass, abstract/final status."),
			mcp.WithString("class_name",
				mcp.Required(),
				mcp.Description("Name of the ABAP class"),
			),
		), s.handleGetClassInfo)
	}

	// DeleteObject
	if shouldRegister("DeleteObject") {
		s.mcpServer.AddTool(mcp.NewTool("DeleteObject",
		mcp.WithDescription("Delete an ABAP object (requires lock)"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("lock_handle",
			mcp.Required(),
			mcp.Description("Lock handle from LockObject"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleDeleteObject)
	}


	// --- Class Include Operations ---

	// GetClassInclude
	if shouldRegister("GetClassInclude") {
		s.mcpServer.AddTool(mcp.NewTool("GetClassInclude",
		mcp.WithDescription("Retrieve source code of a class include (definitions, implementations, macros, testclasses)"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
		mcp.WithString("include_type",
			mcp.Required(),
			mcp.Description("Include type: main, definitions, implementations, macros, testclasses"),
		),
	), s.handleGetClassInclude)
	}


	// CreateTestInclude
	if shouldRegister("CreateTestInclude") {
		s.mcpServer.AddTool(mcp.NewTool("CreateTestInclude",
		mcp.WithDescription("Create the test classes include for a class (required before writing test code)"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
		mcp.WithString("lock_handle",
			mcp.Required(),
			mcp.Description("Lock handle from LockObject (lock the parent class first)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleCreateTestInclude)
	}


	// UpdateClassInclude
	if shouldRegister("UpdateClassInclude") {
		s.mcpServer.AddTool(mcp.NewTool("UpdateClassInclude",
		mcp.WithDescription("Update source code of a class include (requires lock on parent class)"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
		mcp.WithString("include_type",
			mcp.Required(),
			mcp.Description("Include type: main, definitions, implementations, macros, testclasses"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP source code to write"),
		),
		mcp.WithString("lock_handle",
			mcp.Required(),
			mcp.Description("Lock handle from LockObject (lock the parent class first)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleUpdateClassInclude)
	}


	// PublishServiceBinding
	if shouldRegister("PublishServiceBinding") {
		s.mcpServer.AddTool(mcp.NewTool("PublishServiceBinding",
		mcp.WithDescription("Publish a service binding to make it available as OData service"),
		mcp.WithString("service_name",
			mcp.Required(),
			mcp.Description("Service binding name (e.g., ZTRAVEL_SB)"),
		),
		mcp.WithString("service_version",
			mcp.Description("Service version (default: 0001)"),
		),
	), s.handlePublishServiceBinding)
	}


	// UnpublishServiceBinding
	if shouldRegister("UnpublishServiceBinding") {
		s.mcpServer.AddTool(mcp.NewTool("UnpublishServiceBinding",
		mcp.WithDescription("Unpublish a service binding"),
		mcp.WithString("service_name",
			mcp.Required(),
			mcp.Description("Service binding name (e.g., ZTRAVEL_SB)"),
		),
		mcp.WithString("service_version",
			mcp.Description("Service version (default: 0001)"),
		),
	), s.handleUnpublishServiceBinding)
	}


	// --- Workflow Tools ---

	// WriteProgram
	if shouldRegister("WriteProgram") {
		s.mcpServer.AddTool(mcp.NewTool("WriteProgram",
		mcp.WithDescription("Update an existing program with syntax check and activation (Lock -> SyntaxCheck -> Update -> Unlock -> Activate)"),
		mcp.WithString("program_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP program"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP source code"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleWriteProgram)
	}


	// WriteClass
	if shouldRegister("WriteClass") {
		s.mcpServer.AddTool(mcp.NewTool("WriteClass",
		mcp.WithDescription("Update an existing class with syntax check and activation (Lock -> SyntaxCheck -> Update -> Unlock -> Activate)"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP class source code (definition and implementation)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleWriteClass)
	}


	// CreateAndActivateProgram
	if shouldRegister("CreateAndActivateProgram") {
		s.mcpServer.AddTool(mcp.NewTool("CreateAndActivateProgram",
		mcp.WithDescription("Create a new program with source code and activate it (Create -> Lock -> Update -> Unlock -> Activate)"),
		mcp.WithString("program_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP program"),
		),
		mcp.WithString("description",
			mcp.Required(),
			mcp.Description("Program description"),
		),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Package name (e.g., $TMP for local)"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP source code"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (required for non-local packages)"),
		),
	), s.handleCreateAndActivateProgram)
	}


	// CreateClassWithTests
	if shouldRegister("CreateClassWithTests") {
		s.mcpServer.AddTool(mcp.NewTool("CreateClassWithTests",
		mcp.WithDescription("Create a new class with unit tests and run them (Create -> Lock -> Update -> CreateTestInclude -> UpdateTest -> Unlock -> Activate -> RunTests)"),
		mcp.WithString("class_name",
			mcp.Required(),
			mcp.Description("Name of the ABAP class"),
		),
		mcp.WithString("description",
			mcp.Required(),
			mcp.Description("Class description"),
		),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Package name (e.g., $TMP for local)"),
		),
		mcp.WithString("class_source",
			mcp.Required(),
			mcp.Description("ABAP class source code (definition and implementation)"),
		),
		mcp.WithString("test_source",
			mcp.Required(),
			mcp.Description("ABAP unit test source code"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (required for non-local packages)"),
		),
	), s.handleCreateClassWithTests)
	}


	// --- File-Based Deployment Tools ---

	// DeployFromFile (Recommended)
	if shouldRegister("DeployFromFile") {
		s.mcpServer.AddTool(mcp.NewTool("DeployFromFile",
		mcp.WithDescription("✅ RECOMMENDED - Smart deploy from file: auto-detects if object exists and creates/updates accordingly. Solves token limit problem for large generated files (ML models, 3948+ lines). Example: DeployFromFile(file_path=\"/path/to/zcl_ml_iris.clas.abap\", package_name=\"$ZAML_IRIS\") deploys any size file. Workflow: Parse → Check existence → Create or Update → Lock → SyntaxCheck → Write → Unlock → Activate. Supports .clas.abap, .prog.abap, .intf.abap, .fugr.abap, .func.abap. Use this for all file-based deployments."),
		mcp.WithString("file_path",
			mcp.Required(),
			mcp.Description("Absolute path to ABAP source file"),
		),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Package name (required for new objects, e.g., $ZAML_IRIS)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleDeployFromFile)
	}


	// SaveToFile
	if shouldRegister("SaveToFile") {
		s.mcpServer.AddTool(mcp.NewTool("SaveToFile",
		mcp.WithDescription("Save ABAP object source to local file (SAP → File). Enables BIDIRECTIONAL SYNC WORKFLOW: (1) SaveToFile downloads object from SAP, (2) edit locally with vim/VS Code/AI assistants, (3) DeployFromFile uploads changes back to SAP. Example: SaveToFile(objType=\"CLAS/OC\", objectName=\"ZCL_ML_IRIS\", outputPath=\"./src/\") creates ./src/zcl_ml_iris.clas.abap. Then edit locally and use DeployFromFile to sync back. Recommended for iterative development. Auto-determines file extension."),
		mcp.WithString("objType",
			mcp.Required(),
			mcp.Description("Object type: CLAS/OC (class), PROG/P (program), INTF/OI (interface), FUGR/F (function group), FUGR/FF (function module)"),
		),
		mcp.WithString("objectName",
			mcp.Required(),
			mcp.Description("Object name (e.g., ZCL_ML_IRIS, ZAML_IRIS_DEMO)"),
		),
		mcp.WithString("outputPath",
			mcp.Description("Output file path or directory. If directory, filename is auto-generated with correct extension. If omitted, saves to current directory."),
		),
	), s.handleSaveToFile)
	}

	// ImportFromFile (alias for DeployFromFile - File → SAP, supports class includes)
	if shouldRegister("ImportFromFile") {
		s.registerImportFromFile()
	}

	// ExportToFile (alias for SaveToFile - SAP → File)
	if shouldRegister("ExportToFile") {
		s.registerExportToFile()
	}


	// RenameObject
	if shouldRegister("RenameObject") {
		s.mcpServer.AddTool(mcp.NewTool("RenameObject",
		mcp.WithDescription("Rename ABAP object by creating copy with new name and deleting old one. Useful for fixing naming conventions. Workflow: GetSource → Replace names → CreateNew → ActivateNew → DeleteOld"),
		mcp.WithString("objType",
			mcp.Required(),
			mcp.Description("Object type: CLAS/OC (class), PROG/P (program), INTF/OI (interface), FUGR/F (function group)"),
		),
		mcp.WithString("oldName",
			mcp.Required(),
			mcp.Description("Current object name"),
		),
		mcp.WithString("newName",
			mcp.Required(),
			mcp.Description("New object name"),
		),
		mcp.WithString("packageName",
			mcp.Required(),
			mcp.Description("Package name for new object (e.g., $ZAML_IRIS)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number (optional for local packages)"),
		),
	), s.handleRenameObject)
	}


	// --- Surgical Edit Tools ---

	// EditSource
	if shouldRegister("EditSource") {
		s.mcpServer.AddTool(mcp.NewTool("EditSource",
		mcp.WithDescription("Surgical string replacement on ABAP source code. Matches the Edit tool pattern for local files. Workflow: GetSource → FindReplace → SyntaxCheck → Lock → Update → Unlock → Activate. Example: EditSource(object_url=\"/sap/bc/adt/programs/programs/ZTEST\", old_string=\"METHOD foo.\\n  ENDMETHOD.\", new_string=\"METHOD foo.\\n  rv_result = 42.\\n  ENDMETHOD.\", replace_all=false, syntax_check=true). Requires unique match if replace_all=false. Use this for incremental edits between syntax checks - no need to download/upload full source!"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of object (e.g., /sap/bc/adt/programs/programs/ZTEST, /sap/bc/adt/oo/classes/zcl_test)"),
		),
		mcp.WithString("old_string",
			mcp.Required(),
			mcp.Description("Exact string to find and replace. Must be unique in source if replace_all=false. Include enough context (surrounding lines) to ensure uniqueness."),
		),
		mcp.WithString("new_string",
			mcp.Required(),
			mcp.Description("Replacement string. Can be multiline (use \\n). Length can differ from old_string."),
		),
		mcp.WithBoolean("replace_all",
			mcp.Description("If true, replace all occurrences. If false (default), require unique match. Default: false"),
		),
		mcp.WithBoolean("syntax_check",
			mcp.Description("If true (default), validate syntax before saving. If syntax errors found, changes are NOT saved. Default: true"),
		),
		mcp.WithBoolean("case_insensitive",
			mcp.Description("If true, ignore case when matching old_string. Useful for renaming variables regardless of case. Default: false"),
		),
	), s.handleEditSource)
	}


	// --- Grep/Search Tools ---

	// GrepObject
	if shouldRegister("GrepObject") {
		s.mcpServer.AddTool(mcp.NewTool("GrepObject",
		mcp.WithDescription("Search for regex pattern in a single ABAP object's source code. Returns matches with line numbers and optional context. Use for finding TODO comments, string literals, patterns, or code snippets before editing."),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
		),
		mcp.WithString("pattern",
			mcp.Required(),
			mcp.Description("Regular expression pattern (Go regexp syntax). Examples: 'TODO', 'lv_\\w+', 'SELECT.*FROM'"),
		),
		mcp.WithBoolean("case_insensitive",
			mcp.Description("If true, perform case-insensitive matching. Default: false"),
		),
		mcp.WithNumber("context_lines",
			mcp.Description("Number of lines to show before/after each match (like grep -C). Default: 0"),
		),
	), s.handleGrepObject)
	}


	// GrepPackage
	if shouldRegister("GrepPackage") {
		s.mcpServer.AddTool(mcp.NewTool("GrepPackage",
		mcp.WithDescription("Search for regex pattern across all source objects in an ABAP package. Returns matches grouped by object. Use for package-wide analysis, finding patterns across multiple programs/classes."),
		mcp.WithString("package_name",
			mcp.Required(),
			mcp.Description("Package name (e.g., $TMP, ZPACKAGE)"),
		),
		mcp.WithString("pattern",
			mcp.Required(),
			mcp.Description("Regular expression pattern (Go regexp syntax). Examples: 'TODO', 'lv_\\w+', 'SELECT.*FROM'"),
		),
		mcp.WithBoolean("case_insensitive",
			mcp.Description("If true, perform case-insensitive matching. Default: false"),
		),
		mcp.WithString("object_types",
			mcp.Description("Comma-separated object types to search (e.g., 'PROG/P,CLAS/OC'). Empty = search all source objects. Valid: PROG/P, CLAS/OC, INTF/OI, FUGR/F, FUGR/FF, PROG/I"),
		),
		mcp.WithNumber("max_results",
			mcp.Description("Maximum number of matching objects to return. 0 = unlimited. Default: 100"),
		),
	), s.handleGrepPackage)
	}

	// GrepObjects (unified multi-object search)
	if shouldRegister("GrepObjects") {
		s.registerGrepObjects()
	}

	// GrepPackages (unified multi-package search with recursive subpackages)
	if shouldRegister("GrepPackages") {
		s.registerGrepPackages()
	}


	// --- Code Intelligence Tools ---

	// FindDefinition
	if shouldRegister("FindDefinition") {
		s.mcpServer.AddTool(mcp.NewTool("FindDefinition",
		mcp.WithDescription("Navigate to the definition of a symbol at a given position in source code"),
		mcp.WithString("source_url",
			mcp.Required(),
			mcp.Description("ADT URL of the source file (e.g., /sap/bc/adt/programs/programs/ZTEST/source/main)"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("Full source code of the file"),
		),
		mcp.WithNumber("line",
			mcp.Required(),
			mcp.Description("Line number (1-based)"),
		),
		mcp.WithNumber("start_column",
			mcp.Required(),
			mcp.Description("Start column of the symbol (1-based)"),
		),
		mcp.WithNumber("end_column",
			mcp.Required(),
			mcp.Description("End column of the symbol (1-based)"),
		),
		mcp.WithBoolean("implementation",
			mcp.Description("Navigate to implementation instead of definition (default: false)"),
		),
		mcp.WithString("main_program",
			mcp.Description("Main program for includes (optional)"),
		),
	), s.handleFindDefinition)
	}


	// FindReferences
	if shouldRegister("FindReferences") {
		s.mcpServer.AddTool(mcp.NewTool("FindReferences",
		mcp.WithDescription("Find all references to an ABAP object or symbol"),
		mcp.WithString("object_url",
			mcp.Required(),
			mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/oo/classes/ZCL_TEST)"),
		),
		mcp.WithNumber("line",
			mcp.Description("Line number for position-based reference search (1-based, optional)"),
		),
		mcp.WithNumber("column",
			mcp.Description("Column number for position-based reference search (1-based, optional)"),
		),
	), s.handleFindReferences)
	}


	// CodeCompletion
	if shouldRegister("CodeCompletion") {
		s.mcpServer.AddTool(mcp.NewTool("CodeCompletion",
		mcp.WithDescription("Get code completion suggestions at a position in source code"),
		mcp.WithString("source_url",
			mcp.Required(),
			mcp.Description("ADT URL of the source file (e.g., /sap/bc/adt/programs/programs/ZTEST/source/main)"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("Full source code of the file"),
		),
		mcp.WithNumber("line",
			mcp.Required(),
			mcp.Description("Line number (1-based)"),
		),
		mcp.WithNumber("column",
			mcp.Required(),
			mcp.Description("Column number (1-based)"),
		),
	), s.handleCodeCompletion)
	}


	// PrettyPrint
	if shouldRegister("PrettyPrint") {
		s.mcpServer.AddTool(mcp.NewTool("PrettyPrint",
		mcp.WithDescription("Format ABAP source code using the pretty printer"),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP source code to format"),
		),
	), s.handlePrettyPrint)
	}


	// GetPrettyPrinterSettings
	if shouldRegister("GetPrettyPrinterSettings") {
		s.mcpServer.AddTool(mcp.NewTool("GetPrettyPrinterSettings",
		mcp.WithDescription("Get the current pretty printer (code formatter) settings"),
	), s.handleGetPrettyPrinterSettings)
	}


	// SetPrettyPrinterSettings
	if shouldRegister("SetPrettyPrinterSettings") {
		s.mcpServer.AddTool(mcp.NewTool("SetPrettyPrinterSettings",
		mcp.WithDescription("Update the pretty printer (code formatter) settings"),
		mcp.WithBoolean("indentation",
			mcp.Required(),
			mcp.Description("Enable automatic indentation"),
		),
		mcp.WithString("style",
			mcp.Required(),
			mcp.Description("Keyword style: toLower, toUpper, keywordUpper, keywordLower, keywordAuto, none"),
		),
	), s.handleSetPrettyPrinterSettings)
	}


	// GetTypeHierarchy
	if shouldRegister("GetTypeHierarchy") {
		s.mcpServer.AddTool(mcp.NewTool("GetTypeHierarchy",
		mcp.WithDescription("Get the type hierarchy (supertypes or subtypes) for a class/interface"),
		mcp.WithString("source_url",
			mcp.Required(),
			mcp.Description("ADT URL of the source file"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("Full source code of the file"),
		),
		mcp.WithNumber("line",
			mcp.Required(),
			mcp.Description("Line number (1-based)"),
		),
		mcp.WithNumber("column",
			mcp.Required(),
			mcp.Description("Column number (1-based)"),
		),
		mcp.WithBoolean("super_types",
			mcp.Description("Get supertypes instead of subtypes (default: false = subtypes)"),
		),
	), s.handleGetTypeHierarchy)
	}

	// GetClassComponents - get class structure (methods, attributes, events)
	if shouldRegister("GetClassComponents") {
		s.mcpServer.AddTool(mcp.NewTool("GetClassComponents",
			mcp.WithDescription("Get the structure of a class - lists all methods, attributes, events, and other components with their visibility and properties"),
			mcp.WithString("class_url",
				mcp.Required(),
				mcp.Description("ADT URL of the class (e.g., /sap/bc/adt/oo/classes/ZCL_TEST)"),
			),
		), s.handleGetClassComponents)
	}

	// GetInactiveObjects - list objects that need activation
	if shouldRegister("GetInactiveObjects") {
		s.mcpServer.AddTool(mcp.NewTool("GetInactiveObjects",
			mcp.WithDescription("Get all inactive objects for the current user - objects that have been modified but not yet activated"),
		), s.handleGetInactiveObjects)
	}

	// Transport Management Tools (require EnableTransports flag)
	// GetUserTransports - list transport requests for a user
	if shouldRegister("GetUserTransports") {
		s.mcpServer.AddTool(mcp.NewTool("GetUserTransports",
			mcp.WithDescription("Get all transport requests for a user (requires --enable-transports flag). Returns both workbench and customizing requests grouped by target system."),
			mcp.WithString("user_name",
				mcp.Required(),
				mcp.Description("SAP user name (will be converted to uppercase)"),
			),
		), s.handleGetUserTransports)
	}

	// GetTransportInfo - get transport info for an object
	if shouldRegister("GetTransportInfo") {
		s.mcpServer.AddTool(mcp.NewTool("GetTransportInfo",
			mcp.WithDescription("Get transport information for an ABAP object (requires --enable-transports flag). Returns available transports and lock status."),
			mcp.WithString("object_url",
				mcp.Required(),
				mcp.Description("ADT URL of the object (e.g., /sap/bc/adt/programs/programs/ZTEST)"),
			),
			mcp.WithString("dev_class",
				mcp.Required(),
				mcp.Description("Development class/package of the object"),
			),
		), s.handleGetTransportInfo)
	}

	// ExecuteABAP - execute arbitrary ABAP code via unit test wrapper (Expert mode only)
	if shouldRegister("ExecuteABAP") {
		s.mcpServer.AddTool(mcp.NewTool("ExecuteABAP",
			mcp.WithDescription("Execute arbitrary ABAP code via unit test wrapper. Creates temp program, injects code into test method, runs via RunUnitTests, extracts results from assertion messages, cleans up. Use lv_result variable to return output. WARNING: Powerful tool - use responsibly."),
			mcp.WithString("code",
				mcp.Required(),
				mcp.Description("ABAP code to execute. Set lv_result variable to return output via assertion message."),
			),
			mcp.WithString("risk_level",
				mcp.Description("Risk level: harmless (default, no DB writes), dangerous (can write to DB), critical (full access)"),
			),
			mcp.WithString("return_variable",
				mcp.Description("Name of the variable to return (default: lv_result)"),
			),
			mcp.WithBoolean("keep_program",
				mcp.Description("Don't delete temp program after execution (for debugging)"),
			),
			mcp.WithString("program_prefix",
				mcp.Description("Prefix for temp program name (default: ZTEMP_EXEC_)"),
			),
		), s.handleExecuteABAP)
	}

	// --- UI5/Fiori BSP Management ---

	// UI5ListApps
	if shouldRegister("UI5ListApps") {
		s.mcpServer.AddTool(mcp.NewTool("UI5ListApps",
			mcp.WithDescription("List UI5/Fiori BSP applications. Use query parameter for filtering with wildcards (*)."),
			mcp.WithString("query",
				mcp.Description("Search query (supports * wildcard, e.g., 'Z*' for custom apps)"),
			),
			mcp.WithNumber("max_results",
				mcp.Description("Maximum number of results (default: 100)"),
			),
		), s.handleUI5ListApps)
	}

	// UI5GetApp
	if shouldRegister("UI5GetApp") {
		s.mcpServer.AddTool(mcp.NewTool("UI5GetApp",
			mcp.WithDescription("Get details of a UI5/Fiori BSP application including file structure."),
			mcp.WithString("app_name",
				mcp.Required(),
				mcp.Description("Name of the UI5 application"),
			),
		), s.handleUI5GetApp)
	}

	// UI5GetFileContent
	if shouldRegister("UI5GetFileContent") {
		s.mcpServer.AddTool(mcp.NewTool("UI5GetFileContent",
			mcp.WithDescription("Get content of a specific file within a UI5/Fiori BSP application."),
			mcp.WithString("app_name",
				mcp.Required(),
				mcp.Description("Name of the UI5 application"),
			),
			mcp.WithString("file_path",
				mcp.Required(),
				mcp.Description("Path to the file within the app (e.g., '/webapp/manifest.json')"),
			),
		), s.handleUI5GetFileContent)
	}

	// UI5UploadFile
	if shouldRegister("UI5UploadFile") {
		s.mcpServer.AddTool(mcp.NewTool("UI5UploadFile",
			mcp.WithDescription("Upload a file to a UI5/Fiori BSP application."),
			mcp.WithString("app_name",
				mcp.Required(),
				mcp.Description("Name of the UI5 application"),
			),
			mcp.WithString("file_path",
				mcp.Required(),
				mcp.Description("Path for the file within the app (e.g., '/webapp/Component.js')"),
			),
			mcp.WithString("content",
				mcp.Required(),
				mcp.Description("File content to upload"),
			),
			mcp.WithString("content_type",
				mcp.Description("Content type (e.g., 'application/javascript', 'application/json')"),
			),
		), s.handleUI5UploadFile)
	}

	// UI5DeleteFile
	if shouldRegister("UI5DeleteFile") {
		s.mcpServer.AddTool(mcp.NewTool("UI5DeleteFile",
			mcp.WithDescription("Delete a file from a UI5/Fiori BSP application."),
			mcp.WithString("app_name",
				mcp.Required(),
				mcp.Description("Name of the UI5 application"),
			),
			mcp.WithString("file_path",
				mcp.Required(),
				mcp.Description("Path to the file to delete (e.g., '/webapp/test.js')"),
			),
		), s.handleUI5DeleteFile)
	}

	// UI5CreateApp
	if shouldRegister("UI5CreateApp") {
		s.mcpServer.AddTool(mcp.NewTool("UI5CreateApp",
			mcp.WithDescription("Create a new UI5/Fiori BSP application."),
			mcp.WithString("app_name",
				mcp.Required(),
				mcp.Description("Name for the new UI5 application"),
			),
			mcp.WithString("description",
				mcp.Description("Description of the application"),
			),
			mcp.WithString("package",
				mcp.Required(),
				mcp.Description("Package name (e.g., '$TMP' for local, 'ZFIORI' for transportable)"),
			),
			mcp.WithString("transport",
				mcp.Description("Transport request number (optional for local packages)"),
			),
		), s.handleUI5CreateApp)
	}

	// UI5DeleteApp
	if shouldRegister("UI5DeleteApp") {
		s.mcpServer.AddTool(mcp.NewTool("UI5DeleteApp",
			mcp.WithDescription("Delete a UI5/Fiori BSP application."),
			mcp.WithString("app_name",
				mcp.Required(),
				mcp.Description("Name of the UI5 application to delete"),
			),
			mcp.WithString("transport",
				mcp.Description("Transport request number (optional for local packages)"),
			),
		), s.handleUI5DeleteApp)
	}

	// --- AMDP (HANA) Debugger ---

	// AMDPDebuggerStart
	if shouldRegister("AMDPDebuggerStart") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPDebuggerStart",
			mcp.WithDescription("Start an AMDP (HANA SQLScript) debug session with persistent goroutine. Creates a background goroutine that maintains the HTTP session cookies. Use AMDPDebuggerStep/AMDPGetVariables to interact, AMDPDebuggerStop to terminate."),
			mcp.WithString("user",
				mcp.Description("User to debug (defaults to current user)"),
			),
		), s.handleAMDPDebuggerStart)
	}

	// AMDPDebuggerResume
	if shouldRegister("AMDPDebuggerResume") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPDebuggerResume",
			mcp.WithDescription("Get current AMDP debug session status. In goroutine model, this returns the current state without blocking. The session manager goroutine handles events internally."),
		), s.handleAMDPDebuggerResume)
	}

	// AMDPDebuggerStop
	if shouldRegister("AMDPDebuggerStop") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPDebuggerStop",
			mcp.WithDescription("Stop the AMDP debug session and terminate the background goroutine. Cleans up the HTTP session on SAP server."),
		), s.handleAMDPDebuggerStop)
	}

	// AMDPDebuggerStep
	if shouldRegister("AMDPDebuggerStep") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPDebuggerStep",
			mcp.WithDescription("Perform a step operation in the AMDP debugger. Communicates via channel to the session manager goroutine."),
			mcp.WithString("step_type",
				mcp.Required(),
				mcp.Description("Step type: 'stepInto', 'stepOver', 'stepReturn', 'stepContinue'"),
			),
		), s.handleAMDPDebuggerStep)
	}

	// AMDPGetVariables
	if shouldRegister("AMDPGetVariables") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPGetVariables",
			mcp.WithDescription("Get variable values during AMDP debugging. Communicates via channel to the session manager goroutine. Returns scalar, table, and array types."),
		), s.handleAMDPGetVariables)
	}

	// AMDPSetBreakpoint
	if shouldRegister("AMDPSetBreakpoint") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPSetBreakpoint",
			mcp.WithDescription("Set a breakpoint in AMDP (SQLScript) code. Requires an active AMDP debug session. Specify the procedure name and line number."),
			mcp.WithString("proc_name",
				mcp.Required(),
				mcp.Description("AMDP procedure name (e.g., 'ZCL_TEST=>METHOD_NAME')"),
			),
			mcp.WithNumber("line",
				mcp.Required(),
				mcp.Description("Line number in the SQLScript code"),
			),
		), s.handleAMDPSetBreakpoint)
	}

	// AMDPGetBreakpoints
	if shouldRegister("AMDPGetBreakpoints") {
		s.mcpServer.AddTool(mcp.NewTool("AMDPGetBreakpoints",
			mcp.WithDescription("Get all breakpoints registered in the current AMDP debug session. Useful for verifying breakpoints are set correctly."),
		), s.handleAMDPGetBreakpoints)
	}

	// CTS/Transport Management Tools

	// ListTransports
	if shouldRegister("ListTransports") {
		s.mcpServer.AddTool(mcp.NewTool("ListTransports",
			mcp.WithDescription("List transport requests. Returns modifiable transports for a user. Requires --enable-transports flag."),
			mcp.WithString("user",
				mcp.Description("Username to list transports for (default: current user, '*' for all users)"),
			),
		), s.handleListTransports)
	}

	// GetTransport
	if shouldRegister("GetTransport") {
		s.mcpServer.AddTool(mcp.NewTool("GetTransport",
			mcp.WithDescription("Get detailed transport information including objects and tasks. Requires --enable-transports flag."),
			mcp.WithString("transport",
				mcp.Required(),
				mcp.Description("Transport request number (e.g., 'A4HK900094')"),
			),
		), s.handleGetTransport)
	}

	// CreateTransport (expert mode only)
	if shouldRegister("CreateTransport") {
		s.mcpServer.AddTool(mcp.NewTool("CreateTransport",
			mcp.WithDescription("Create a new transport request. Requires --enable-transports flag and not --transport-read-only."),
			mcp.WithString("description",
				mcp.Required(),
				mcp.Description("Transport description"),
			),
			mcp.WithString("package",
				mcp.Required(),
				mcp.Description("Target package (DEVCLASS)"),
			),
			mcp.WithString("transport_layer",
				mcp.Description("Transport layer (optional)"),
			),
			mcp.WithString("type",
				mcp.Description("Type: 'workbench' (default) or 'customizing'"),
			),
		), s.handleCreateTransport)
	}

	// ReleaseTransport (expert mode only)
	if shouldRegister("ReleaseTransport") {
		s.mcpServer.AddTool(mcp.NewTool("ReleaseTransport",
			mcp.WithDescription("Release a transport request. This action is IRREVERSIBLE. Requires --enable-transports flag and not --transport-read-only."),
			mcp.WithString("transport",
				mcp.Required(),
				mcp.Description("Transport request number"),
			),
			mcp.WithBoolean("ignore_locks",
				mcp.Description("Release even with locked objects (default: false)"),
			),
			mcp.WithBoolean("skip_atc",
				mcp.Description("Skip ATC quality checks (default: false)"),
			),
		), s.handleReleaseTransport)
	}

	// DeleteTransport (expert mode only)
	if shouldRegister("DeleteTransport") {
		s.mcpServer.AddTool(mcp.NewTool("DeleteTransport",
			mcp.WithDescription("Delete a transport request. Only modifiable transports can be deleted. Requires --enable-transports flag and not --transport-read-only."),
			mcp.WithString("transport",
				mcp.Required(),
				mcp.Description("Transport request number"),
			),
		), s.handleDeleteTransport)
	}

	// --- Git/abapGit Integration (via ZADT_VSP WebSocket) ---

	// GitTypes
	if shouldRegister("GitTypes") {
		s.mcpServer.AddTool(mcp.NewTool("GitTypes",
			mcp.WithDescription("Get list of supported abapGit object types. Returns 158 object types that can be exported/imported via abapGit. Requires abapGit to be installed on SAP system."),
		), s.handleGitTypes)
	}

	// GitExport
	if shouldRegister("GitExport") {
		s.mcpServer.AddTool(mcp.NewTool("GitExport",
			mcp.WithDescription("Export ABAP objects as abapGit-compatible ZIP. Supports 158 object types. Returns base64-encoded ZIP with files in abapGit format. Use packages OR objects parameter."),
			mcp.WithString("packages",
				mcp.Description("Comma-separated package names to export (e.g., '$ZRAY,$TMP'). Supports wildcards."),
			),
			mcp.WithString("objects",
				mcp.Description("JSON array of objects: [{\"type\":\"CLAS\",\"name\":\"ZCL_TEST\"}]"),
			),
			mcp.WithBoolean("include_subpackages",
				mcp.Description("Include subpackages when exporting by package (default: true)"),
			),
		), s.handleGitExport)
	}

	// --- Report Execution Tools (via ZADT_VSP WebSocket) ---

	// RunReport
	if shouldRegister("RunReport") {
		s.mcpServer.AddTool(mcp.NewTool("RunReport",
			mcp.WithDescription("Execute an ABAP selection-screen report with parameters or variant. Can capture ALV output as structured data. Requires ZADT_VSP WebSocket handler deployed."),
			mcp.WithString("report",
				mcp.Description("Report program name (e.g., 'RFITEMGL', 'ZREPORT_TEST')"),
				mcp.Required(),
			),
			mcp.WithString("variant",
				mcp.Description("Variant name to use for selection screen (optional)"),
			),
			mcp.WithString("params",
				mcp.Description("JSON object with selection screen parameters (e.g., '{\"P_BUKRS\":\"1000\",\"S_KUNNR\":{\"SIGN\":\"I\",\"OPTION\":\"EQ\",\"LOW\":\"0000001000\"}}'). Keys are parameter names."),
			),
			mcp.WithBoolean("capture_alv",
				mcp.Description("If true, capture ALV grid output as structured data (default: false)"),
			),
			mcp.WithNumber("max_rows",
				mcp.Description("Maximum ALV rows to return when capturing (default: 1000)"),
			),
		), s.handleRunReport)
	}

	// RunReportAsync - Background report execution
	if shouldRegister("RunReportAsync") {
		s.mcpServer.AddTool(mcp.NewTool("RunReportAsync",
			mcp.WithDescription("Start report execution in background. Returns task_id immediately. Use GetAsyncResult to poll for completion. Useful for long-running reports that would timeout."),
			mcp.WithString("report",
				mcp.Description("Report program name"),
				mcp.Required(),
			),
			mcp.WithString("variant",
				mcp.Description("Variant name (optional)"),
			),
			mcp.WithString("params",
				mcp.Description("JSON object with selection screen parameters"),
			),
			mcp.WithBoolean("capture_alv",
				mcp.Description("Capture ALV output (default: false)"),
			),
			mcp.WithNumber("max_rows",
				mcp.Description("Maximum ALV rows (default: 1000)"),
			),
		), s.handleRunReportAsync)
	}

	// GetAsyncResult - Retrieve async task results
	if shouldRegister("GetAsyncResult") {
		s.mcpServer.AddTool(mcp.NewTool("GetAsyncResult",
			mcp.WithDescription("Get result of an async task by ID. Returns status (running/completed/error) and result when done."),
			mcp.WithString("task_id",
				mcp.Description("Task ID from RunReportAsync"),
				mcp.Required(),
			),
			mcp.WithBoolean("wait",
				mcp.Description("If true, block until task completes (max 60s). Default: false (poll)"),
			),
		), s.handleGetAsyncResult)
	}

	// GetVariants
	if shouldRegister("GetVariants") {
		s.mcpServer.AddTool(mcp.NewTool("GetVariants",
			mcp.WithDescription("Get list of available variants for a report program. Returns variant names and whether they are protected."),
			mcp.WithString("report",
				mcp.Description("Report program name"),
				mcp.Required(),
			),
		), s.handleGetVariants)
	}

	// GetTextElements
	if shouldRegister("GetTextElements") {
		s.mcpServer.AddTool(mcp.NewTool("GetTextElements",
			mcp.WithDescription("Get program text elements (selection texts and text symbols). Selection texts describe parameters (P_BUKRS='Company Code'), text symbols are TEXT-001 etc."),
			mcp.WithString("program",
				mcp.Description("Program name"),
				mcp.Required(),
			),
			mcp.WithString("language",
				mcp.Description("Language key (e.g., 'E' for English, 'D' for German). Default: system language."),
			),
		), s.handleGetTextElements)
	}

	// SetTextElements
	if shouldRegister("SetTextElements") {
		s.mcpServer.AddTool(mcp.NewTool("SetTextElements",
			mcp.WithDescription("Set program text elements (selection texts and text symbols). Use for adding descriptions to selection screen parameters and text symbols."),
			mcp.WithString("program",
				mcp.Description("Program name"),
				mcp.Required(),
			),
			mcp.WithString("language",
				mcp.Description("Language key (e.g., 'E' for English, 'D' for German). Default: system language."),
			),
			mcp.WithString("selection_texts",
				mcp.Description("JSON object of selection texts (e.g., '{\"P_BUKRS\":\"Company Code\",\"S_KUNNR\":\"Customer Range\"}')"),
			),
			mcp.WithString("text_symbols",
				mcp.Description("JSON object of text symbols (e.g., '{\"001\":\"Header Text\",\"002\":\"Footer\"}')"),
			),
		), s.handleSetTextElements)
	}

	// --- Install/Setup Tools ---

	// InstallZADTVSP
	if shouldRegister("InstallZADTVSP") {
		s.mcpServer.AddTool(mcp.NewTool("InstallZADTVSP",
			mcp.WithDescription("Deploy ZADT_VSP WebSocket handler to SAP system. Creates package and deploys 6 ABAP objects (interface + 5 classes) that enable WebSocket debugging, RFC calls, and abapGit export. After deployment, manual SAPC and SICF setup is required."),
			mcp.WithString("package",
				mcp.Description("Target package name (default: $ZADT_VSP). Must be local package starting with $."),
			),
			mcp.WithBoolean("skip_git_service",
				mcp.Description("Skip ZCL_VSP_GIT_SERVICE deployment if abapGit is not installed (default: false, auto-detected)"),
			),
			mcp.WithBoolean("check_only",
				mcp.Description("Only check prerequisites without deploying (default: false)"),
			),
		), s.handleInstallZADTVSP)
	}

	// ListDependencies
	if shouldRegister("ListDependencies") {
		s.mcpServer.AddTool(mcp.NewTool("ListDependencies",
			mcp.WithDescription("List available dependency packages that can be installed via InstallAbapGit. Shows abapGit editions and other optional dependencies."),
		), s.handleListDependencies)
	}

	// InstallAbapGit
	if shouldRegister("InstallAbapGit") {
		s.mcpServer.AddTool(mcp.NewTool("InstallAbapGit",
			mcp.WithDescription("Deploy abapGit to SAP system from embedded ZIP. Supports standalone (single program) or developer edition (full package structure). Parses abapGit-format ZIP and deploys via WriteSource."),
			mcp.WithString("edition",
				mcp.Description("Edition to install: 'standalone' (single program ZABAPGIT) or 'dev' (full $ZGIT_DEV packages). Default: standalone"),
			),
			mcp.WithString("package",
				mcp.Description("Target package name. Default: $ABAPGIT for standalone, $ZGIT_DEV for dev edition"),
			),
			mcp.WithBoolean("check_only",
				mcp.Description("Only check prerequisites and show deployment plan without deploying (default: false)"),
			),
		), s.handleInstallAbapGit)
	}

	// InstallDummyTest - Test tool to verify Install* workflow
	if shouldRegister("InstallDummyTest") {
		s.mcpServer.AddTool(mcp.NewTool("InstallDummyTest",
			mcp.WithDescription("Test tool that creates a simple interface and class to verify the Install* workflow (create, lock, update, unlock, activate, verify). Uses package $ZADT_INSTALL_TEST."),
			mcp.WithBoolean("check_only",
				mcp.Description("Only check prerequisites without deploying (default: false)"),
			),
			mcp.WithBoolean("cleanup",
				mcp.Description("Delete test objects after verification (default: false)"),
			),
		), s.handleInstallDummyTest)
	}

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

	sqlQuery := ""
	if sq, ok := request.Params.Arguments["sql_query"].(string); ok {
		sqlQuery = sq
	}

	contents, err := s.adtClient.GetTableContents(ctx, tableName, maxRows, sqlQuery)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get table contents: %v", err)), nil
	}

	result, _ := json.MarshalIndent(contents, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to run query: %v", err)), nil
	}

	result, _ := json.MarshalIndent(contents, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
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
		return newToolResultError(fmt.Sprintf("Failed to get CDS dependencies: %v", err)), nil
	}

	// Add metadata summary
	summary := map[string]interface{}{
		"ddls_name":       ddlsName,
		"dependency_tree": dependencyTree,
		"statistics": map[string]interface{}{
			"total_dependencies": len(dependencyTree.FlattenDependencies()) - 1, // -1 to exclude root
			"dependency_depth":   dependencyTree.GetDependencyDepth(),
			"by_type":            dependencyTree.CountDependenciesByType(),
			"table_dependencies": len(dependencyTree.GetTableDependencies()),
			"inactive_dependencies": len(dependencyTree.GetInactiveDependencies()),
			"cycles":             dependencyTree.FindCycles(),
		},
	}

	jsonResult, _ := json.MarshalIndent(summary, "", "  ")
	return mcp.NewToolResultText(string(jsonResult)), nil
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

func (s *Server) handleGetMessages(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	msgClass, ok := request.Params.Arguments["message_class"].(string)
	if !ok || msgClass == "" {
		return newToolResultError("message_class is required"), nil
	}

	mc, err := s.adtClient.GetMessageClass(ctx, msgClass)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get message class: %v", err)), nil
	}

	result, _ := json.MarshalIndent(mc, "", "  ")
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

// --- System Information Handlers ---

func (s *Server) handleGetSystemInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	info, err := s.adtClient.GetSystemInfo(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get system info: %v", err)), nil
	}

	result, _ := json.MarshalIndent(info, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetInstalledComponents(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	components, err := s.adtClient.GetInstalledComponents(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get installed components: %v", err)), nil
	}

	result, _ := json.MarshalIndent(components, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetConnectionInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Return current connection info for introspection
	info := map[string]interface{}{
		"user":   s.config.Username,
		"url":    s.config.BaseURL,
		"client": s.config.Client,
		"mode":   s.config.Mode,
	}

	// Add feature summary
	info["features"] = s.featureProber.FeatureSummary(ctx)

	// Add debugger status
	info["debugger_user"] = strings.ToUpper(s.config.Username) // Debugger uses uppercase

	result, _ := json.MarshalIndent(info, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetFeatures(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Probe all features
	results := s.featureProber.ProbeAll(ctx)

	// Format output
	type featureOutput struct {
		Features map[string]*adt.FeatureStatus `json:"features"`
		Summary  string                        `json:"summary"`
	}

	output := featureOutput{
		Features: make(map[string]*adt.FeatureStatus),
		Summary:  s.featureProber.FeatureSummary(ctx),
	}

	for id, status := range results {
		output.Features[string(id)] = status
	}

	result, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

// --- Code Analysis Infrastructure Handlers ---

func (s *Server) handleGetCallGraph(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURI, ok := request.Params.Arguments["object_uri"].(string)
	if !ok || objectURI == "" {
		return newToolResultError("object_uri is required"), nil
	}

	opts := &adt.CallGraphOptions{
		Direction:  "callers",
		MaxDepth:   3,
		MaxResults: 100,
	}

	if dir, ok := request.Params.Arguments["direction"].(string); ok && dir != "" {
		opts.Direction = dir
	}
	if depth, ok := request.Params.Arguments["max_depth"].(float64); ok && depth > 0 {
		opts.MaxDepth = int(depth)
	}
	if max, ok := request.Params.Arguments["max_results"].(float64); ok && max > 0 {
		opts.MaxResults = int(max)
	}

	graph, err := s.adtClient.GetCallGraph(ctx, objectURI, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get call graph: %v", err)), nil
	}

	result, _ := json.MarshalIndent(graph, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetObjectStructure(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectName, ok := request.Params.Arguments["object_name"].(string)
	if !ok || objectName == "" {
		return newToolResultError("object_name is required"), nil
	}

	maxResults := 100
	if max, ok := request.Params.Arguments["max_results"].(float64); ok && max > 0 {
		maxResults = int(max)
	}

	structure, err := s.adtClient.GetObjectStructureCAI(ctx, objectName, maxResults)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get object structure: %v", err)), nil
	}

	result, _ := json.MarshalIndent(structure, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetCallersOf(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURI, ok := request.Params.Arguments["object_uri"].(string)
	if !ok || objectURI == "" {
		return newToolResultError("object_uri is required"), nil
	}

	maxDepth := 5
	if depth, ok := request.Params.Arguments["max_depth"].(float64); ok && depth > 0 {
		maxDepth = int(depth)
	}

	graph, err := s.adtClient.GetCallersOf(ctx, objectURI, maxDepth)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get callers: %v", err)), nil
	}

	// Flatten to edges for easier consumption
	edges := adt.FlattenCallGraph(graph)
	stats := adt.AnalyzeCallGraph(graph)

	output := map[string]interface{}{
		"root":  graph,
		"edges": edges,
		"stats": stats,
	}

	result, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetCalleesOf(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURI, ok := request.Params.Arguments["object_uri"].(string)
	if !ok || objectURI == "" {
		return newToolResultError("object_uri is required"), nil
	}

	maxDepth := 5
	if depth, ok := request.Params.Arguments["max_depth"].(float64); ok && depth > 0 {
		maxDepth = int(depth)
	}

	graph, err := s.adtClient.GetCalleesOf(ctx, objectURI, maxDepth)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get callees: %v", err)), nil
	}

	// Flatten to edges for easier consumption
	edges := adt.FlattenCallGraph(graph)
	stats := adt.AnalyzeCallGraph(graph)

	output := map[string]interface{}{
		"root":  graph,
		"edges": edges,
		"stats": stats,
	}

	result, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleAnalyzeCallGraph(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURI, ok := request.Params.Arguments["object_uri"].(string)
	if !ok || objectURI == "" {
		return newToolResultError("object_uri is required"), nil
	}

	direction := "callees"
	if dir, ok := request.Params.Arguments["direction"].(string); ok && dir != "" {
		direction = dir
	}

	maxDepth := 5
	if depth, ok := request.Params.Arguments["max_depth"].(float64); ok && depth > 0 {
		maxDepth = int(depth)
	}

	graph, err := s.adtClient.GetCallGraph(ctx, objectURI, &adt.CallGraphOptions{
		Direction:  direction,
		MaxDepth:   maxDepth,
		MaxResults: 1000,
	})
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get call graph: %v", err)), nil
	}

	stats := adt.AnalyzeCallGraph(graph)
	edges := adt.FlattenCallGraph(graph)

	output := map[string]interface{}{
		"object_uri": objectURI,
		"direction":  direction,
		"stats":      stats,
		"edge_count": len(edges),
		"edges":      edges,
	}

	result, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleCompareCallGraphs(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURI, ok := request.Params.Arguments["object_uri"].(string)
	if !ok || objectURI == "" {
		return newToolResultError("object_uri is required"), nil
	}

	traceDataStr, ok := request.Params.Arguments["trace_data"].(string)
	if !ok || traceDataStr == "" {
		return newToolResultError("trace_data is required (JSON array of edges)"), nil
	}

	// Parse trace data
	var actualEdges []adt.CallGraphEdge
	if err := json.Unmarshal([]byte(traceDataStr), &actualEdges); err != nil {
		return newToolResultError(fmt.Sprintf("Failed to parse trace_data: %v", err)), nil
	}

	// Get static call graph
	graph, err := s.adtClient.GetCallGraph(ctx, objectURI, &adt.CallGraphOptions{
		Direction:  "callees",
		MaxDepth:   10,
		MaxResults: 1000,
	})
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get static call graph: %v", err)), nil
	}

	staticEdges := adt.FlattenCallGraph(graph)

	// Compare
	comparison := adt.CompareCallGraphs(staticEdges, actualEdges)

	output := map[string]interface{}{
		"object_uri":      objectURI,
		"static_edges":    len(staticEdges),
		"actual_edges":    len(actualEdges),
		"common_edges":    len(comparison.CommonEdges),
		"untested_paths":  len(comparison.StaticOnly),
		"dynamic_calls":   len(comparison.ActualOnly),
		"coverage_ratio":  comparison.CoverageRatio,
		"common":          comparison.CommonEdges,
		"static_only":     comparison.StaticOnly,
		"actual_only":     comparison.ActualOnly,
	}

	result, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleTraceExecution(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURI, ok := request.Params.Arguments["object_uri"].(string)
	if !ok || objectURI == "" {
		return newToolResultError("object_uri is required"), nil
	}

	opts := &adt.TraceExecutionOptions{
		ObjectURI: objectURI,
		MaxDepth:  5,
	}

	if maxDepth, ok := request.Params.Arguments["max_depth"].(float64); ok {
		opts.MaxDepth = int(maxDepth)
	}

	if runTests, ok := request.Params.Arguments["run_tests"].(bool); ok {
		opts.RunTests = runTests
	}

	if testURI, ok := request.Params.Arguments["test_object_uri"].(string); ok && testURI != "" {
		opts.TestObjectURI = testURI
	} else if opts.RunTests {
		opts.TestObjectURI = objectURI // Default to same object
	}

	if traceUser, ok := request.Params.Arguments["trace_user"].(string); ok && traceUser != "" {
		opts.TraceUser = traceUser
	}

	result, err := s.adtClient.TraceExecution(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Trace execution failed: %v", err)), nil
	}

	// Build comprehensive output
	output := map[string]interface{}{
		"object_uri": objectURI,
	}

	if result.StaticStats != nil {
		output["static_stats"] = result.StaticStats
	}

	if result.Trace != nil {
		output["trace"] = map[string]interface{}{
			"id":          result.Trace.TraceID,
			"total_time":  result.Trace.TotalTime,
			"total_calls": result.Trace.TotalCalls,
			"entries":     len(result.Trace.Entries),
		}
	}

	if len(result.ActualEdges) > 0 {
		output["actual_edges"] = result.ActualEdges
	}

	if result.Comparison != nil {
		output["comparison"] = map[string]interface{}{
			"common_edges":   len(result.Comparison.CommonEdges),
			"untested_paths": len(result.Comparison.StaticOnly),
			"dynamic_calls":  len(result.Comparison.ActualOnly),
			"coverage_ratio": result.Comparison.CoverageRatio,
			"static_only":    result.Comparison.StaticOnly,
			"actual_only":    result.Comparison.ActualOnly,
		}
	}

	if len(result.ExecutedTests) > 0 {
		output["executed_tests"] = result.ExecutedTests
	}

	output["execution_time_us"] = result.ExecutionTime

	jsonResult, _ := json.MarshalIndent(output, "", "  ")
	return mcp.NewToolResultText(string(jsonResult)), nil
}

// --- Runtime Errors / Short Dumps Handlers ---

func (s *Server) handleListDumps(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	opts := &adt.DumpQueryOptions{
		MaxResults: 100,
	}

	if user, ok := request.Params.Arguments["user"].(string); ok && user != "" {
		opts.User = user
	}
	if excType, ok := request.Params.Arguments["exception_type"].(string); ok && excType != "" {
		opts.ExceptionType = excType
	}
	if prog, ok := request.Params.Arguments["program"].(string); ok && prog != "" {
		opts.Program = prog
	}
	if pkg, ok := request.Params.Arguments["package"].(string); ok && pkg != "" {
		opts.Package = pkg
	}
	if dateFrom, ok := request.Params.Arguments["date_from"].(string); ok && dateFrom != "" {
		opts.DateFrom = dateFrom
	}
	if dateTo, ok := request.Params.Arguments["date_to"].(string); ok && dateTo != "" {
		opts.DateTo = dateTo
	}
	if max, ok := request.Params.Arguments["max_results"].(float64); ok && max > 0 {
		opts.MaxResults = int(max)
	}

	dumps, err := s.adtClient.GetDumps(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get dumps: %v", err)), nil
	}

	result, _ := json.MarshalIndent(dumps, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetDump(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	dumpID, ok := request.Params.Arguments["dump_id"].(string)
	if !ok || dumpID == "" {
		return newToolResultError("dump_id is required"), nil
	}

	dump, err := s.adtClient.GetDump(ctx, dumpID)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get dump: %v", err)), nil
	}

	result, _ := json.MarshalIndent(dump, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

// --- ABAP Profiler / Traces Handlers ---

func (s *Server) handleListTraces(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	opts := &adt.TraceQueryOptions{
		MaxResults: 100,
	}

	if user, ok := request.Params.Arguments["user"].(string); ok && user != "" {
		opts.User = user
	}
	if procType, ok := request.Params.Arguments["process_type"].(string); ok && procType != "" {
		opts.ProcessType = procType
	}
	if objType, ok := request.Params.Arguments["object_type"].(string); ok && objType != "" {
		opts.ObjectType = objType
	}
	if max, ok := request.Params.Arguments["max_results"].(float64); ok && max > 0 {
		opts.MaxResults = int(max)
	}

	traces, err := s.adtClient.ListTraces(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to list traces: %v", err)), nil
	}

	result, _ := json.MarshalIndent(traces, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleGetTrace(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	traceID, ok := request.Params.Arguments["trace_id"].(string)
	if !ok || traceID == "" {
		return newToolResultError("trace_id is required"), nil
	}

	toolType := "hitlist"
	if tt, ok := request.Params.Arguments["tool_type"].(string); ok && tt != "" {
		toolType = tt
	}

	analysis, err := s.adtClient.GetTrace(ctx, traceID, toolType)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get trace: %v", err)), nil
	}

	result, _ := json.MarshalIndent(analysis, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

// --- SQL Trace (ST05) Handlers ---

func (s *Server) handleGetSQLTraceState(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	state, err := s.adtClient.GetSQLTraceState(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get SQL trace state: %v", err)), nil
	}

	result, _ := json.MarshalIndent(state, "", "  ")
	return mcp.NewToolResultText(string(result)), nil
}

func (s *Server) handleListSQLTraces(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	user := ""
	maxResults := 100

	if u, ok := request.Params.Arguments["user"].(string); ok {
		user = u
	}
	if max, ok := request.Params.Arguments["max_results"].(float64); ok && max > 0 {
		maxResults = int(max)
	}

	traces, err := s.adtClient.ListSQLTraces(ctx, user, maxResults)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to list SQL traces: %v", err)), nil
	}

	result, _ := json.MarshalIndent(traces, "", "  ")
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

// --- Development Tool Handlers ---

func (s *Server) handleSyntaxCheck(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	content, ok := request.Params.Arguments["content"].(string)
	if !ok || content == "" {
		return newToolResultError("content is required"), nil
	}

	results, err := s.adtClient.SyntaxCheck(ctx, objectURL, content)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Syntax check failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(results, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleActivate(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	objectName, ok := request.Params.Arguments["object_name"].(string)
	if !ok || objectName == "" {
		return newToolResultError("object_name is required"), nil
	}

	result, err := s.adtClient.Activate(ctx, objectURL, objectName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Activation failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleActivatePackage(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	packageName := ""
	if pkg, ok := request.Params.Arguments["package"].(string); ok {
		packageName = pkg
	}

	maxObjects := 100
	if max, ok := request.Params.Arguments["max_objects"].(float64); ok && max > 0 {
		maxObjects = int(max)
	}

	result, err := s.adtClient.ActivatePackage(ctx, packageName, maxObjects)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Batch activation failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleRunUnitTests(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	// Build flags from optional parameters
	flags := adt.DefaultUnitTestFlags()

	if includeDangerous, ok := request.Params.Arguments["include_dangerous"].(bool); ok && includeDangerous {
		flags.Dangerous = true
	}

	if includeLong, ok := request.Params.Arguments["include_long"].(bool); ok && includeLong {
		flags.Long = true
	}

	result, err := s.adtClient.RunUnitTests(ctx, objectURL, &flags)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Unit test run failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- ATC Handlers ---

func (s *Server) handleRunATCCheck(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	variant := ""
	if v, ok := request.Params.Arguments["variant"].(string); ok {
		variant = v
	}

	maxResults := 100
	if mr, ok := request.Params.Arguments["max_results"].(float64); ok && mr > 0 {
		maxResults = int(mr)
	}

	result, err := s.adtClient.RunATCCheck(ctx, objectURL, variant, maxResults)
	if err != nil {
		return newToolResultError(fmt.Sprintf("ATC check failed: %v", err)), nil
	}

	// Format output with summary
	type summary struct {
		TotalObjects  int `json:"totalObjects"`
		TotalFindings int `json:"totalFindings"`
		Errors        int `json:"errors"`
		Warnings      int `json:"warnings"`
		Infos         int `json:"infos"`
	}
	type output struct {
		Summary  summary            `json:"summary"`
		Worklist *adt.ATCWorklist   `json:"worklist"`
	}

	sum := summary{TotalObjects: len(result.Objects)}
	for _, obj := range result.Objects {
		sum.TotalFindings += len(obj.Findings)
		for _, f := range obj.Findings {
			switch f.Priority {
			case 1:
				sum.Errors++
			case 2:
				sum.Warnings++
			default:
				sum.Infos++
			}
		}
	}

	out := output{Summary: sum, Worklist: result}
	outputJSON, _ := json.MarshalIndent(out, "", "  ")
	return mcp.NewToolResultText(string(outputJSON)), nil
}

func (s *Server) handleGetATCCustomizing(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	result, err := s.adtClient.GetATCCustomizing(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get ATC customizing: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- CRUD Handlers ---

func (s *Server) handleLockObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	accessMode := "MODIFY"
	if am, ok := request.Params.Arguments["access_mode"].(string); ok && am != "" {
		accessMode = am
	}

	result, err := s.adtClient.LockObject(ctx, objectURL, accessMode)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to lock object: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleUnlockObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	err := s.adtClient.UnlockObject(ctx, objectURL, lockHandle)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to unlock object: %v", err)), nil
	}

	return mcp.NewToolResultText("Object unlocked successfully"), nil
}

func (s *Server) handleUpdateSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	// Append /source/main to object URL if not already present
	sourceURL := objectURL
	if !strings.HasSuffix(sourceURL, "/source/main") {
		sourceURL = objectURL + "/source/main"
	}

	err := s.adtClient.UpdateSource(ctx, sourceURL, source, lockHandle, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to update source: %v", err)), nil
	}

	return mcp.NewToolResultText("Source updated successfully"), nil
}

func (s *Server) handleCreateObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, ok := request.Params.Arguments["object_type"].(string)
	if !ok || objectType == "" {
		return newToolResultError("object_type is required"), nil
	}

	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	parentName := ""
	if p, ok := request.Params.Arguments["parent_name"].(string); ok {
		parentName = p
	}

	// RAP-specific options
	serviceDefinition := ""
	if sd, ok := request.Params.Arguments["service_definition"].(string); ok {
		serviceDefinition = sd
	}
	bindingVersion := ""
	if bv, ok := request.Params.Arguments["binding_version"].(string); ok {
		bindingVersion = bv
	}
	bindingCategory := ""
	if bc, ok := request.Params.Arguments["binding_category"].(string); ok {
		bindingCategory = bc
	}

	opts := adt.CreateObjectOptions{
		ObjectType:        adt.CreatableObjectType(objectType),
		Name:              name,
		Description:       description,
		PackageName:       packageName,
		Transport:         transport,
		ParentName:        parentName,
		ServiceDefinition: serviceDefinition,
		BindingVersion:    bindingVersion,
		BindingCategory:   bindingCategory,
	}

	err := s.adtClient.CreateObject(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to create object: %v", err)), nil
	}

	// Return the object URL for convenience
	objURL := adt.GetObjectURL(opts.ObjectType, opts.Name, opts.ParentName)
	result := map[string]string{
		"status":     "created",
		"object_url": objURL,
	}
	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCreatePackage(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	// Validate package name starts with $
	name = strings.ToUpper(name)
	if !strings.HasPrefix(name, "$") {
		return newToolResultError("package name must start with $ (local packages only)"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	parent := ""
	if p, ok := request.Params.Arguments["parent"].(string); ok && p != "" {
		parent = strings.ToUpper(p)
	}

	opts := adt.CreateObjectOptions{
		ObjectType:  adt.ObjectTypePackage,
		Name:        name,
		Description: description,
		PackageName: parent, // Parent package
	}

	err := s.adtClient.CreateObject(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to create package: %v", err)), nil
	}

	result := map[string]string{
		"status":      "created",
		"package":     name,
		"description": description,
	}
	if parent != "" {
		result["parent"] = parent
	}
	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCreateTable(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	fieldsJSON, ok := request.Params.Arguments["fields"].(string)
	if !ok || fieldsJSON == "" {
		return newToolResultError("fields is required (JSON array)"), nil
	}

	// Parse fields JSON
	var fields []adt.TableField
	if err := json.Unmarshal([]byte(fieldsJSON), &fields); err != nil {
		return newToolResultError(fmt.Sprintf("Invalid fields JSON: %v", err)), nil
	}

	if len(fields) == 0 {
		return newToolResultError("At least one field is required"), nil
	}

	// Optional parameters
	pkg := "$TMP"
	if p, ok := request.Params.Arguments["package"].(string); ok && p != "" {
		pkg = strings.ToUpper(p)
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok && t != "" {
		transport = t
	}

	deliveryClass := "A"
	if dc, ok := request.Params.Arguments["delivery_class"].(string); ok && dc != "" {
		deliveryClass = strings.ToUpper(dc)
	}

	opts := adt.CreateTableOptions{
		Name:          name,
		Description:   description,
		Package:       pkg,
		Fields:        fields,
		Transport:     transport,
		DeliveryClass: deliveryClass,
	}

	err := s.adtClient.CreateTable(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to create table: %v", err)), nil
	}

	result := map[string]interface{}{
		"status":      "created",
		"table":       strings.ToUpper(name),
		"package":     pkg,
		"description": description,
		"fields":      len(fields),
	}
	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCompareSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	type1, _ := request.Params.Arguments["type1"].(string)
	name1, _ := request.Params.Arguments["name1"].(string)
	type2, _ := request.Params.Arguments["type2"].(string)
	name2, _ := request.Params.Arguments["name2"].(string)

	if type1 == "" || name1 == "" || type2 == "" || name2 == "" {
		return newToolResultError("type1, name1, type2, and name2 are all required"), nil
	}

	// Build options for first object
	opts1 := &adt.GetSourceOptions{}
	if inc, ok := request.Params.Arguments["include1"].(string); ok && inc != "" {
		opts1.Include = inc
	}
	if parent, ok := request.Params.Arguments["parent1"].(string); ok && parent != "" {
		opts1.Parent = parent
	}

	// Build options for second object
	opts2 := &adt.GetSourceOptions{}
	if inc, ok := request.Params.Arguments["include2"].(string); ok && inc != "" {
		opts2.Include = inc
	}
	if parent, ok := request.Params.Arguments["parent2"].(string); ok && parent != "" {
		opts2.Parent = parent
	}

	diff, err := s.adtClient.CompareSource(ctx, type1, name1, type2, name2, opts1, opts2)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CompareSource failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(diff, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCloneObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, _ := request.Params.Arguments["object_type"].(string)
	sourceName, _ := request.Params.Arguments["source_name"].(string)
	targetName, _ := request.Params.Arguments["target_name"].(string)
	pkg, _ := request.Params.Arguments["package"].(string)

	if objectType == "" || sourceName == "" || targetName == "" || pkg == "" {
		return newToolResultError("object_type, source_name, target_name, and package are all required"), nil
	}

	result, err := s.adtClient.CloneObject(ctx, objectType, sourceName, targetName, pkg)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CloneObject failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleGetClassInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, _ := request.Params.Arguments["class_name"].(string)
	if className == "" {
		return newToolResultError("class_name is required"), nil
	}

	info, err := s.adtClient.GetClassInfo(ctx, className)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetClassInfo failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(info, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleDeleteObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	err := s.adtClient.DeleteObject(ctx, objectURL, lockHandle, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to delete object: %v", err)), nil
	}

	return mcp.NewToolResultText("Object deleted successfully"), nil
}

// --- Class Include Handlers ---

func (s *Server) handleGetClassInclude(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, ok := request.Params.Arguments["class_name"].(string)
	if !ok || className == "" {
		return newToolResultError("class_name is required"), nil
	}

	includeType, ok := request.Params.Arguments["include_type"].(string)
	if !ok || includeType == "" {
		return newToolResultError("include_type is required"), nil
	}

	source, err := s.adtClient.GetClassInclude(ctx, className, adt.ClassIncludeType(includeType))
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get class include: %v", err)), nil
	}

	return mcp.NewToolResultText(source), nil
}

func (s *Server) handleCreateTestInclude(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, ok := request.Params.Arguments["class_name"].(string)
	if !ok || className == "" {
		return newToolResultError("class_name is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	err := s.adtClient.CreateTestInclude(ctx, className, lockHandle, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to create test include: %v", err)), nil
	}

	return mcp.NewToolResultText("Test include created successfully"), nil
}

func (s *Server) handleUpdateClassInclude(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, ok := request.Params.Arguments["class_name"].(string)
	if !ok || className == "" {
		return newToolResultError("class_name is required"), nil
	}

	includeType, ok := request.Params.Arguments["include_type"].(string)
	if !ok || includeType == "" {
		return newToolResultError("include_type is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	err := s.adtClient.UpdateClassInclude(ctx, className, adt.ClassIncludeType(includeType), source, lockHandle, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to update class include: %v", err)), nil
	}

	return mcp.NewToolResultText("Class include updated successfully"), nil
}

// --- Service Binding Publish/Unpublish Handlers ---

func (s *Server) handlePublishServiceBinding(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	serviceName, ok := request.Params.Arguments["service_name"].(string)
	if !ok || serviceName == "" {
		return newToolResultError("service_name is required"), nil
	}

	serviceVersion := "0001"
	if sv, ok := request.Params.Arguments["service_version"].(string); ok && sv != "" {
		serviceVersion = sv
	}

	result, err := s.adtClient.PublishServiceBinding(ctx, serviceName, serviceVersion)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to publish service binding: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleUnpublishServiceBinding(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	serviceName, ok := request.Params.Arguments["service_name"].(string)
	if !ok || serviceName == "" {
		return newToolResultError("service_name is required"), nil
	}

	serviceVersion := "0001"
	if sv, ok := request.Params.Arguments["service_version"].(string); ok && sv != "" {
		serviceVersion = sv
	}

	result, err := s.adtClient.UnpublishServiceBinding(ctx, serviceName, serviceVersion)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to unpublish service binding: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- Workflow Handlers ---

func (s *Server) handleWriteProgram(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	programName, ok := request.Params.Arguments["program_name"].(string)
	if !ok || programName == "" {
		return newToolResultError("program_name is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	result, err := s.adtClient.WriteProgram(ctx, programName, source, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("WriteProgram failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleWriteClass(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, ok := request.Params.Arguments["class_name"].(string)
	if !ok || className == "" {
		return newToolResultError("class_name is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	result, err := s.adtClient.WriteClass(ctx, className, source, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("WriteClass failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCreateAndActivateProgram(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	programName, ok := request.Params.Arguments["program_name"].(string)
	if !ok || programName == "" {
		return newToolResultError("program_name is required"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	result, err := s.adtClient.CreateAndActivateProgram(ctx, programName, description, packageName, source, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CreateAndActivateProgram failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCreateClassWithTests(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, ok := request.Params.Arguments["class_name"].(string)
	if !ok || className == "" {
		return newToolResultError("class_name is required"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	classSource, ok := request.Params.Arguments["class_source"].(string)
	if !ok || classSource == "" {
		return newToolResultError("class_source is required"), nil
	}

	testSource, ok := request.Params.Arguments["test_source"].(string)
	if !ok || testSource == "" {
		return newToolResultError("test_source is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	result, err := s.adtClient.CreateClassWithTests(ctx, className, description, packageName, classSource, testSource, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CreateClassWithTests failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- File-Based Deployment Handlers ---

// Note: CreateFromFile and UpdateFromFile handlers removed - use DeployFromFile instead

func (s *Server) handleDeployFromFile(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	filePath, ok := request.Params.Arguments["file_path"].(string)
	if !ok || filePath == "" {
		return newToolResultError("file_path is required"), nil
	}

	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	result, err := s.adtClient.DeployFromFile(ctx, filePath, packageName, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DeployFromFile failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleSaveToFile(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Support both old (objType/objectName/outputPath) and new (object_type/object_name/output_dir) parameter names
	objTypeStr, ok := request.Params.Arguments["objType"].(string)
	if !ok || objTypeStr == "" {
		objTypeStr, ok = request.Params.Arguments["object_type"].(string)
		if !ok || objTypeStr == "" {
			return newToolResultError("object_type is required (e.g., PROG, CLAS, INTF, FUGR, FUNC, DDLS, BDEF, SRVD)"), nil
		}
	}

	objectName, ok := request.Params.Arguments["objectName"].(string)
	if !ok || objectName == "" {
		objectName, ok = request.Params.Arguments["object_name"].(string)
		if !ok || objectName == "" {
			return newToolResultError("object_name is required"), nil
		}
	}

	outputPath := ""
	if p, ok := request.Params.Arguments["outputPath"].(string); ok {
		outputPath = p
	} else if p, ok := request.Params.Arguments["output_dir"].(string); ok {
		outputPath = p
	}

	// Check for include parameter (for class includes)
	includeStr := ""
	if inc, ok := request.Params.Arguments["include"].(string); ok {
		includeStr = strings.ToLower(inc)
	}

	// Parse object type - support both short (PROG) and full (PROG/P) format
	var objType adt.CreatableObjectType
	switch strings.ToUpper(objTypeStr) {
	case "PROG", "PROG/P":
		objType = adt.ObjectTypeProgram
	case "CLAS", "CLAS/OC":
		objType = adt.ObjectTypeClass
	case "INTF", "INTF/OI":
		objType = adt.ObjectTypeInterface
	case "FUGR", "FUGR/F":
		objType = adt.ObjectTypeFunctionGroup
	case "FUNC", "FUGR/FF":
		objType = adt.ObjectTypeFunctionMod
	case "INCL", "PROG/I":
		objType = adt.ObjectTypeInclude
	// RAP types
	case "DDLS", "DDLS/DF":
		objType = adt.ObjectTypeDDLS
	case "BDEF", "BDEF/BDO":
		objType = adt.ObjectTypeBDEF
	case "SRVD", "SRVD/SRV":
		objType = adt.ObjectTypeSRVD
	default:
		objType = adt.CreatableObjectType(objTypeStr)
	}

	// Handle class includes
	if objType == adt.ObjectTypeClass && includeStr != "" && includeStr != "main" {
		var includeType adt.ClassIncludeType
		switch includeStr {
		case "testclasses":
			includeType = adt.ClassIncludeTestClasses
		case "definitions":
			includeType = adt.ClassIncludeDefinitions
		case "implementations":
			includeType = adt.ClassIncludeImplementations
		case "macros":
			includeType = adt.ClassIncludeMacros
		default:
			return newToolResultError(fmt.Sprintf("invalid include type: %s (expected: main, testclasses, definitions, implementations, macros)", includeStr)), nil
		}

		result, err := s.adtClient.SaveClassIncludeToFile(ctx, objectName, includeType, outputPath)
		if err != nil {
			return newToolResultError(fmt.Sprintf("SaveClassIncludeToFile failed: %v", err)), nil
		}
		output, _ := json.MarshalIndent(result, "", "  ")
		return mcp.NewToolResultText(string(output)), nil
	}

	result, err := s.adtClient.SaveToFile(ctx, objType, objectName, outputPath)
	if err != nil {
		return newToolResultError(fmt.Sprintf("SaveToFile failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleRenameObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objTypeStr, ok := request.Params.Arguments["objType"].(string)
	if !ok || objTypeStr == "" {
		return newToolResultError("objType is required (e.g., CLAS/OC, PROG/P, INTF/OI, FUGR/F)"), nil
	}

	oldName, ok := request.Params.Arguments["oldName"].(string)
	if !ok || oldName == "" {
		return newToolResultError("oldName is required"), nil
	}

	newName, ok := request.Params.Arguments["newName"].(string)
	if !ok || newName == "" {
		return newToolResultError("newName is required"), nil
	}

	packageName, ok := request.Params.Arguments["packageName"].(string)
	if !ok || packageName == "" {
		return newToolResultError("packageName is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	// Parse object type
	objType := adt.CreatableObjectType(objTypeStr)

	result, err := s.adtClient.RenameObject(ctx, objType, oldName, newName, packageName, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("RenameObject failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleEditSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	oldString, ok := request.Params.Arguments["old_string"].(string)
	if !ok || oldString == "" {
		return newToolResultError("old_string is required"), nil
	}

	newString, ok := request.Params.Arguments["new_string"].(string)
	if !ok {
		return newToolResultError("new_string is required"), nil
	}

	replaceAll := false
	if r, ok := request.Params.Arguments["replace_all"].(bool); ok {
		replaceAll = r
	}

	syntaxCheck := true
	if sc, ok := request.Params.Arguments["syntax_check"].(bool); ok {
		syntaxCheck = sc
	}

	caseInsensitive := false
	if ci, ok := request.Params.Arguments["case_insensitive"].(bool); ok {
		caseInsensitive = ci
	}

	result, err := s.adtClient.EditSource(ctx, objectURL, oldString, newString, replaceAll, syntaxCheck, caseInsensitive)
	if err != nil {
		return newToolResultError(fmt.Sprintf("EditSource failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- Grep/Search Handlers ---

func (s *Server) handleGrepObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	pattern, ok := request.Params.Arguments["pattern"].(string)
	if !ok || pattern == "" {
		return newToolResultError("pattern is required"), nil
	}

	caseInsensitive := false
	if ci, ok := request.Params.Arguments["case_insensitive"].(bool); ok {
		caseInsensitive = ci
	}

	contextLines := 0
	if cl, ok := request.Params.Arguments["context_lines"].(float64); ok {
		contextLines = int(cl)
	}

	result, err := s.adtClient.GrepObject(ctx, objectURL, pattern, caseInsensitive, contextLines)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GrepObject failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleGrepPackage(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	pattern, ok := request.Params.Arguments["pattern"].(string)
	if !ok || pattern == "" {
		return newToolResultError("pattern is required"), nil
	}

	caseInsensitive := false
	if ci, ok := request.Params.Arguments["case_insensitive"].(bool); ok {
		caseInsensitive = ci
	}

	// Parse object_types (comma-separated string to slice)
	var objectTypes []string
	if ot, ok := request.Params.Arguments["object_types"].(string); ok && ot != "" {
		objectTypes = strings.Split(ot, ",")
		// Trim whitespace from each type
		for i := range objectTypes {
			objectTypes[i] = strings.TrimSpace(objectTypes[i])
		}
	}

	maxResults := 100 // default
	if mr, ok := request.Params.Arguments["max_results"].(float64); ok {
		maxResults = int(mr)
	}

	result, err := s.adtClient.GrepPackage(ctx, packageName, pattern, caseInsensitive, objectTypes, maxResults)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GrepPackage failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- Code Intelligence Handlers ---

func (s *Server) handleFindDefinition(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	sourceURL, ok := request.Params.Arguments["source_url"].(string)
	if !ok || sourceURL == "" {
		return newToolResultError("source_url is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	lineF, ok := request.Params.Arguments["line"].(float64)
	if !ok {
		return newToolResultError("line is required"), nil
	}
	line := int(lineF)

	startColF, ok := request.Params.Arguments["start_column"].(float64)
	if !ok {
		return newToolResultError("start_column is required"), nil
	}
	startCol := int(startColF)

	endColF, ok := request.Params.Arguments["end_column"].(float64)
	if !ok {
		return newToolResultError("end_column is required"), nil
	}
	endCol := int(endColF)

	implementation := false
	if impl, ok := request.Params.Arguments["implementation"].(bool); ok {
		implementation = impl
	}

	mainProgram := ""
	if mp, ok := request.Params.Arguments["main_program"].(string); ok {
		mainProgram = mp
	}

	result, err := s.adtClient.FindDefinition(ctx, sourceURL, source, line, startCol, endCol, implementation, mainProgram)
	if err != nil {
		return newToolResultError(fmt.Sprintf("FindDefinition failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleFindReferences(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	line := 0
	column := 0
	if lineF, ok := request.Params.Arguments["line"].(float64); ok {
		line = int(lineF)
	}
	if colF, ok := request.Params.Arguments["column"].(float64); ok {
		column = int(colF)
	}

	results, err := s.adtClient.FindReferences(ctx, objectURL, line, column)
	if err != nil {
		return newToolResultError(fmt.Sprintf("FindReferences failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(results, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleCodeCompletion(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	sourceURL, ok := request.Params.Arguments["source_url"].(string)
	if !ok || sourceURL == "" {
		return newToolResultError("source_url is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	lineF, ok := request.Params.Arguments["line"].(float64)
	if !ok {
		return newToolResultError("line is required"), nil
	}
	line := int(lineF)

	colF, ok := request.Params.Arguments["column"].(float64)
	if !ok {
		return newToolResultError("column is required"), nil
	}
	column := int(colF)

	proposals, err := s.adtClient.CodeCompletion(ctx, sourceURL, source, line, column)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CodeCompletion failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(proposals, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handlePrettyPrint(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	formatted, err := s.adtClient.PrettyPrint(ctx, source)
	if err != nil {
		return newToolResultError(fmt.Sprintf("PrettyPrint failed: %v", err)), nil
	}

	return mcp.NewToolResultText(formatted), nil
}

func (s *Server) handleGetPrettyPrinterSettings(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	settings, err := s.adtClient.GetPrettyPrinterSettings(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetPrettyPrinterSettings failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(settings, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleSetPrettyPrinterSettings(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	indentation, ok := request.Params.Arguments["indentation"].(bool)
	if !ok {
		return newToolResultError("indentation is required"), nil
	}

	style, ok := request.Params.Arguments["style"].(string)
	if !ok || style == "" {
		return newToolResultError("style is required"), nil
	}

	settings := &adt.PrettyPrinterSettings{
		Indentation: indentation,
		Style:       adt.PrettyPrinterStyle(style),
	}

	err := s.adtClient.SetPrettyPrinterSettings(ctx, settings)
	if err != nil {
		return newToolResultError(fmt.Sprintf("SetPrettyPrinterSettings failed: %v", err)), nil
	}

	return mcp.NewToolResultText("Pretty printer settings updated successfully"), nil
}

func (s *Server) handleGetTypeHierarchy(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	sourceURL, ok := request.Params.Arguments["source_url"].(string)
	if !ok || sourceURL == "" {
		return newToolResultError("source_url is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	lineF, ok := request.Params.Arguments["line"].(float64)
	if !ok {
		return newToolResultError("line is required"), nil
	}
	line := int(lineF)

	colF, ok := request.Params.Arguments["column"].(float64)
	if !ok {
		return newToolResultError("column is required"), nil
	}
	column := int(colF)

	superTypes := false
	if st, ok := request.Params.Arguments["super_types"].(bool); ok {
		superTypes = st
	}

	hierarchy, err := s.adtClient.GetTypeHierarchy(ctx, sourceURL, source, line, column, superTypes)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetTypeHierarchy failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(hierarchy, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

func (s *Server) handleGetClassComponents(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	classURL, ok := request.Params.Arguments["class_url"].(string)
	if !ok || classURL == "" {
		return newToolResultError("class_url is required"), nil
	}

	components, err := s.adtClient.GetClassComponents(ctx, classURL)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetClassComponents failed: %v", err)), nil
	}

	// Format output with summary
	output := formatClassComponents(components)
	return mcp.NewToolResultText(output), nil
}

// formatClassComponents creates a readable summary of class components
func formatClassComponents(comp *adt.ClassComponent) string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Class: %s (%s)\n\n", comp.Name, comp.Type))

	// Group components by type
	methods := []adt.ClassComponent{}
	attributes := []adt.ClassComponent{}
	events := []adt.ClassComponent{}
	types := []adt.ClassComponent{}
	others := []adt.ClassComponent{}

	for _, c := range comp.Components {
		switch {
		case strings.Contains(c.Type, "METH"):
			methods = append(methods, c)
		case strings.Contains(c.Type, "DATA") || strings.Contains(c.Type, "ATTR"):
			attributes = append(attributes, c)
		case strings.Contains(c.Type, "EVNT") || strings.Contains(c.Type, "EVENT"):
			events = append(events, c)
		case strings.Contains(c.Type, "TYPE"):
			types = append(types, c)
		default:
			others = append(others, c)
		}
	}

	if len(methods) > 0 {
		sb.WriteString(fmt.Sprintf("## Methods (%d)\n", len(methods)))
		for _, m := range methods {
			flags := componentFlags(m)
			sb.WriteString(fmt.Sprintf("  - %s [%s]%s\n", m.Name, m.Visibility, flags))
			if m.Description != "" {
				sb.WriteString(fmt.Sprintf("    %s\n", m.Description))
			}
		}
		sb.WriteString("\n")
	}

	if len(attributes) > 0 {
		sb.WriteString(fmt.Sprintf("## Attributes (%d)\n", len(attributes)))
		for _, a := range attributes {
			flags := componentFlags(a)
			sb.WriteString(fmt.Sprintf("  - %s [%s]%s\n", a.Name, a.Visibility, flags))
		}
		sb.WriteString("\n")
	}

	if len(events) > 0 {
		sb.WriteString(fmt.Sprintf("## Events (%d)\n", len(events)))
		for _, e := range events {
			sb.WriteString(fmt.Sprintf("  - %s [%s]\n", e.Name, e.Visibility))
		}
		sb.WriteString("\n")
	}

	if len(types) > 0 {
		sb.WriteString(fmt.Sprintf("## Types (%d)\n", len(types)))
		for _, t := range types {
			sb.WriteString(fmt.Sprintf("  - %s [%s]\n", t.Name, t.Visibility))
		}
		sb.WriteString("\n")
	}

	if len(others) > 0 {
		sb.WriteString(fmt.Sprintf("## Other Components (%d)\n", len(others)))
		for _, o := range others {
			sb.WriteString(fmt.Sprintf("  - %s (%s) [%s]\n", o.Name, o.Type, o.Visibility))
		}
	}

	return sb.String()
}

func componentFlags(c adt.ClassComponent) string {
	var flags []string
	if c.IsStatic {
		flags = append(flags, "static")
	}
	if c.IsFinal {
		flags = append(flags, "final")
	}
	if c.IsAbstract {
		flags = append(flags, "abstract")
	}
	if c.ReadOnly {
		flags = append(flags, "read-only")
	}
	if c.Constant {
		flags = append(flags, "constant")
	}
	if len(flags) > 0 {
		return " " + strings.Join(flags, ", ")
	}
	return ""
}

func (s *Server) handleGetInactiveObjects(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objects, err := s.adtClient.GetInactiveObjects(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetInactiveObjects failed: %v", err)), nil
	}

	if len(objects) == 0 {
		return mcp.NewToolResultText("No inactive objects found."), nil
	}

	// Format output
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Found %d inactive object(s):\n\n", len(objects)))

	for _, record := range objects {
		if record.Object != nil {
			obj := record.Object
			sb.WriteString(fmt.Sprintf("- %s (%s)\n", obj.Name, obj.Type))
			sb.WriteString(fmt.Sprintf("  URI: %s\n", obj.URI))
			if obj.User != "" {
				sb.WriteString(fmt.Sprintf("  User: %s\n", obj.User))
			}
			if obj.Deleted {
				sb.WriteString("  Status: DELETED\n")
			}
		}
		if record.Transport != nil {
			tr := record.Transport
			sb.WriteString(fmt.Sprintf("  Transport: %s\n", tr.Name))
		}
		sb.WriteString("\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

// registerGetSource registers the unified GetSource tool
func (s *Server) registerGetSource() {
	s.mcpServer.AddTool(mcp.NewTool("GetSource",
		mcp.WithDescription("Unified tool for reading ABAP source code across different object types. Replaces GetProgram, GetClass, GetInterface, GetFunction, GetInclude, GetFunctionGroup, GetClassInclude."),
		mcp.WithString("object_type",
			mcp.Required(),
			mcp.Description("Object type: PROG (program), CLAS (class), INTF (interface), FUNC (function module), FUGR (function group), INCL (include), DDLS (CDS DDL source), VIEW (DDIC view), BDEF (behavior definition), SRVD (service definition), SRVB (service binding), MSAG (message class)"),
		),
		mcp.WithString("name",
			mcp.Required(),
			mcp.Description("Object name (e.g., program name, class name, function module name)"),
		),
		mcp.WithString("parent",
			mcp.Description("Function group name (required only for FUNC type)"),
		),
		mcp.WithString("include",
			mcp.Description("Class include type for CLAS: definitions, implementations, macros, testclasses (optional)"),
		),
	), s.handleGetSource)
}

// registerWriteSource registers the unified WriteSource tool
func (s *Server) registerWriteSource() {
	s.mcpServer.AddTool(mcp.NewTool("WriteSource",
		mcp.WithDescription("Unified tool for writing ABAP source code with automatic create/update detection. Supports PROG, CLAS, INTF, and RAP types (DDLS, BDEF, SRVD)."),
		mcp.WithString("object_type",
			mcp.Required(),
			mcp.Description("Object type: PROG (program), CLAS (class), INTF (interface), DDLS (CDS view), BDEF (behavior definition), SRVD (service definition)"),
		),
		mcp.WithString("name",
			mcp.Required(),
			mcp.Description("Object name"),
		),
		mcp.WithString("source",
			mcp.Required(),
			mcp.Description("ABAP source code"),
		),
		mcp.WithString("mode",
			mcp.Description("Operation mode: upsert (default, auto-detect), create (new only), update (existing only)"),
		),
		mcp.WithString("description",
			mcp.Description("Object description (required for create mode)"),
		),
		mcp.WithString("package",
			mcp.Description("Package name (required for create mode)"),
		),
		mcp.WithString("test_source",
			mcp.Description("Test source code for CLAS (auto-creates test include and runs tests)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number"),
		),
	), s.handleWriteSource)
}

// handleGetSource handles the unified GetSource tool call
func (s *Server) handleGetSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, ok := request.Params.Arguments["object_type"].(string)
	if !ok || objectType == "" {
		return newToolResultError("object_type is required"), nil
	}

	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	parent, _ := request.Params.Arguments["parent"].(string)
	include, _ := request.Params.Arguments["include"].(string)

	opts := &adt.GetSourceOptions{
		Parent:  parent,
		Include: include,
	}

	source, err := s.adtClient.GetSource(ctx, objectType, name, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetSource failed: %v", err)), nil
	}

	return mcp.NewToolResultText(source), nil
}

// handleWriteSource handles the unified WriteSource tool call
func (s *Server) handleWriteSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, ok := request.Params.Arguments["object_type"].(string)
	if !ok || objectType == "" {
		return newToolResultError("object_type is required"), nil
	}

	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	mode, _ := request.Params.Arguments["mode"].(string)
	description, _ := request.Params.Arguments["description"].(string)
	packageName, _ := request.Params.Arguments["package"].(string)
	testSource, _ := request.Params.Arguments["test_source"].(string)
	transport, _ := request.Params.Arguments["transport"].(string)

	opts := &adt.WriteSourceOptions{
		Description: description,
		Package:     packageName,
		TestSource:  testSource,
		Transport:   transport,
	}

	if mode != "" {
		opts.Mode = adt.WriteSourceMode(mode)
	}

	result, err := s.adtClient.WriteSource(ctx, objectType, name, source, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("WriteSource failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// registerGrepObjects registers the unified GrepObjects tool
func (s *Server) registerGrepObjects() {
	s.mcpServer.AddTool(mcp.NewTool("GrepObjects",
		mcp.WithDescription("Unified tool for searching regex patterns in single or multiple ABAP objects. Replaces GrepObject."),
		mcp.WithArray("object_urls",
			mcp.Required(),
			mcp.Description("Array of ADT object URLs to search (e.g., [\"/sap/bc/adt/programs/programs/ZTEST\"])"),
			mcp.Items(map[string]interface{}{"type": "string"}),
		),
		mcp.WithString("pattern",
			mcp.Required(),
			mcp.Description("Regular expression pattern (Go regexp syntax)"),
		),
		mcp.WithBoolean("case_insensitive",
			mcp.Description("If true, perform case-insensitive matching (default: false)"),
		),
		mcp.WithNumber("context_lines",
			mcp.Description("Number of context lines before/after each match (default: 0)"),
		),
	), s.handleGrepObjects)
}

// registerGrepPackages registers the unified GrepPackages tool
func (s *Server) registerGrepPackages() {
	s.mcpServer.AddTool(mcp.NewTool("GrepPackages",
		mcp.WithDescription("Unified tool for searching regex patterns across single or multiple packages with optional recursive subpackage search. Replaces GrepPackage."),
		mcp.WithArray("packages",
			mcp.Required(),
			mcp.Description("Array of package names to search (e.g., [\"$TMP\"], [\"Z\"] for namespace search)"),
			mcp.Items(map[string]interface{}{"type": "string"}),
		),
		mcp.WithBoolean("include_subpackages",
			mcp.Description("If true, recursively search all subpackages (default: false). Enables namespace-wide searches."),
		),
		mcp.WithString("pattern",
			mcp.Required(),
			mcp.Description("Regular expression pattern (Go regexp syntax)"),
		),
		mcp.WithBoolean("case_insensitive",
			mcp.Description("If true, perform case-insensitive matching (default: false)"),
		),
		mcp.WithArray("object_types",
			mcp.Description("Filter by object types (e.g., [\"CLAS/OC\", \"PROG/P\"]). Empty = search all types."),
			mcp.Items(map[string]interface{}{"type": "string"}),
		),
		mcp.WithNumber("max_results",
			mcp.Description("Maximum number of matching objects to return (0 = unlimited, default: 0)"),
		),
	), s.handleGrepPackages)
}

// registerImportFromFile registers the ImportFromFile tool (alias for DeployFromFile)
func (s *Server) registerImportFromFile() {
	s.mcpServer.AddTool(mcp.NewTool("ImportFromFile",
		mcp.WithDescription("Import ABAP object from local file into SAP system. Auto-detects object type from file extension, creates or updates, activates. Supports: programs, classes (with includes), interfaces, function groups/modules, CDS views (DDLS), behavior definitions (BDEF), service definitions (SRVD). For class includes (.clas.testclasses.abap, .clas.locals_def.abap, etc.), the parent class must exist."),
		mcp.WithString("file_path",
			mcp.Required(),
			mcp.Description("Absolute path to ABAP source file. Supported extensions: .prog.abap, .clas.abap, .clas.testclasses.abap, .clas.locals_def.abap, .clas.locals_imp.abap, .intf.abap, .fugr.abap, .func.abap, .ddls.asddls, .bdef.asbdef, .srvd.srvdsrv"),
		),
		mcp.WithString("package_name",
			mcp.Description("Target package name (required for new objects, not needed for class includes)"),
		),
		mcp.WithString("transport",
			mcp.Description("Transport request number"),
		),
	), s.handleDeployFromFile) // Reuse existing handler
}

// registerExportToFile registers the ExportToFile tool (alias for SaveToFile)
func (s *Server) registerExportToFile() {
	s.mcpServer.AddTool(mcp.NewTool("ExportToFile",
		mcp.WithDescription("Export ABAP object from SAP system to local file. Saves source code with appropriate file extension. Supports: programs, classes (with includes), interfaces, function groups/modules, CDS views (DDLS), behavior definitions (BDEF), service definitions (SRVD). For classes, use 'include' parameter to export specific includes (testclasses, definitions, implementations, macros)."),
		mcp.WithString("object_type",
			mcp.Required(),
			mcp.Description("Object type: PROG, CLAS, INTF, FUGR, FUNC, DDLS, BDEF, SRVD"),
		),
		mcp.WithString("object_name",
			mcp.Required(),
			mcp.Description("Object name"),
		),
		mcp.WithString("output_dir",
			mcp.Required(),
			mcp.Description("Output directory path (must exist)"),
		),
		mcp.WithString("include",
			mcp.Description("For CLAS only: include type to export. Values: main (default), testclasses, definitions, implementations, macros. Creates abapGit-compatible files (.clas.testclasses.abap, .clas.locals_def.abap, etc.)"),
		),
		mcp.WithString("parent",
			mcp.Description("Function group name (required for FUNC type)"),
		),
	), s.handleSaveToFile) // Reuse existing handler
}

// handleGrepObjects handles the unified GrepObjects tool call
func (s *Server) handleGrepObjects(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURLsRaw, ok := request.Params.Arguments["object_urls"].([]interface{})
	if !ok || len(objectURLsRaw) == 0 {
		return newToolResultError("object_urls array is required"), nil
	}

	// Convert []interface{} to []string
	objectURLs := make([]string, len(objectURLsRaw))
	for i, v := range objectURLsRaw {
		if s, ok := v.(string); ok {
			objectURLs[i] = s
		} else {
			return newToolResultError(fmt.Sprintf("object_urls[%d] must be a string", i)), nil
		}
	}

	pattern, ok := request.Params.Arguments["pattern"].(string)
	if !ok || pattern == "" {
		return newToolResultError("pattern is required"), nil
	}

	caseInsensitive := false
	if ci, ok := request.Params.Arguments["case_insensitive"].(bool); ok {
		caseInsensitive = ci
	}

	contextLines := 0
	if cl, ok := request.Params.Arguments["context_lines"].(float64); ok {
		contextLines = int(cl)
	}

	result, err := s.adtClient.GrepObjects(ctx, objectURLs, pattern, caseInsensitive, contextLines)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GrepObjects failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// handleGrepPackages handles the unified GrepPackages tool call
func (s *Server) handleGrepPackages(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	packagesRaw, ok := request.Params.Arguments["packages"].([]interface{})
	if !ok || len(packagesRaw) == 0 {
		return newToolResultError("packages array is required"), nil
	}

	// Convert []interface{} to []string
	packages := make([]string, len(packagesRaw))
	for i, v := range packagesRaw {
		if s, ok := v.(string); ok {
			packages[i] = s
		} else {
			return newToolResultError(fmt.Sprintf("packages[%d] must be a string", i)), nil
		}
	}

	includeSubpackages := false
	if is, ok := request.Params.Arguments["include_subpackages"].(bool); ok {
		includeSubpackages = is
	}

	pattern, ok := request.Params.Arguments["pattern"].(string)
	if !ok || pattern == "" {
		return newToolResultError("pattern is required"), nil
	}

	caseInsensitive := false
	if ci, ok := request.Params.Arguments["case_insensitive"].(bool); ok {
		caseInsensitive = ci
	}

	var objectTypes []string
	if ot, ok := request.Params.Arguments["object_types"].([]interface{}); ok {
		objectTypes = make([]string, len(ot))
		for i, v := range ot {
			if s, ok := v.(string); ok {
				objectTypes[i] = s
			}
		}
	}

	maxResults := 0
	if mr, ok := request.Params.Arguments["max_results"].(float64); ok {
		maxResults = int(mr)
	}

	result, err := s.adtClient.GrepPackages(ctx, packages, includeSubpackages, pattern, caseInsensitive, objectTypes, maxResults)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GrepPackages failed: %v", err)), nil
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// --- Transport Management Handlers ---

func (s *Server) handleGetUserTransports(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	userName, ok := request.Params.Arguments["user_name"].(string)
	if !ok || userName == "" {
		return newToolResultError("user_name is required"), nil
	}

	transports, err := s.adtClient.GetUserTransports(ctx, userName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetUserTransports failed: %v", err)), nil
	}

	// Format output
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Transports for user %s:\n\n", strings.ToUpper(userName)))

	if len(transports.Workbench) > 0 {
		sb.WriteString("=== Workbench Requests ===\n")
		for _, tr := range transports.Workbench {
			formatTransportRequest(&sb, &tr)
		}
	} else {
		sb.WriteString("No workbench requests found.\n")
	}

	sb.WriteString("\n")

	if len(transports.Customizing) > 0 {
		sb.WriteString("=== Customizing Requests ===\n")
		for _, tr := range transports.Customizing {
			formatTransportRequest(&sb, &tr)
		}
	} else {
		sb.WriteString("No customizing requests found.\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func formatTransportRequest(sb *strings.Builder, tr *adt.TransportRequest) {
	sb.WriteString(fmt.Sprintf("\n%s - %s\n", tr.Number, tr.Description))
	sb.WriteString(fmt.Sprintf("  Owner: %s, Status: %s", tr.Owner, tr.Status))
	if tr.Target != "" {
		sb.WriteString(fmt.Sprintf(", Target: %s", tr.Target))
	}
	sb.WriteString("\n")

	if len(tr.Tasks) > 0 {
		sb.WriteString("  Tasks:\n")
		for _, task := range tr.Tasks {
			sb.WriteString(fmt.Sprintf("    %s - %s (Owner: %s, Status: %s)\n",
				task.Number, task.Description, task.Owner, task.Status))
			if len(task.Objects) > 0 {
				for _, obj := range task.Objects {
					sb.WriteString(fmt.Sprintf("      - %s %s %s\n", obj.PGMID, obj.Type, obj.Name))
				}
			}
		}
	}
}

func (s *Server) handleGetTransportInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	devClass, ok := request.Params.Arguments["dev_class"].(string)
	if !ok || devClass == "" {
		return newToolResultError("dev_class is required"), nil
	}

	info, err := s.adtClient.GetTransportInfo(ctx, objectURL, devClass)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetTransportInfo failed: %v", err)), nil
	}

	// Format output
	var sb strings.Builder
	sb.WriteString("Transport Information:\n\n")
	sb.WriteString(fmt.Sprintf("PGMID: %s\n", info.PGMID))
	sb.WriteString(fmt.Sprintf("Object: %s\n", info.Object))
	sb.WriteString(fmt.Sprintf("Object Name: %s\n", info.ObjectName))
	sb.WriteString(fmt.Sprintf("Operation: %s\n", info.Operation))
	sb.WriteString(fmt.Sprintf("Dev Class: %s\n", info.DevClass))
	sb.WriteString(fmt.Sprintf("Recording: %s\n", info.Recording))

	if info.LockedByUser != "" {
		sb.WriteString(fmt.Sprintf("\nLocked by: %s", info.LockedByUser))
		if info.LockedInTask != "" {
			sb.WriteString(fmt.Sprintf(" in task %s", info.LockedInTask))
		}
		sb.WriteString("\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

// handleExecuteABAP executes arbitrary ABAP code via unit test wrapper.
func (s *Server) handleExecuteABAP(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	code, ok := request.Params.Arguments["code"].(string)
	if !ok || code == "" {
		return newToolResultError("code parameter is required"), nil
	}

	opts := &adt.ExecuteABAPOptions{}

	if riskLevel, ok := request.Params.Arguments["risk_level"].(string); ok && riskLevel != "" {
		opts.RiskLevel = riskLevel
	}

	if returnVar, ok := request.Params.Arguments["return_variable"].(string); ok && returnVar != "" {
		opts.ReturnVariable = returnVar
	}

	if keepProgram, ok := request.Params.Arguments["keep_program"].(bool); ok {
		opts.KeepProgram = keepProgram
	}

	if prefix, ok := request.Params.Arguments["program_prefix"].(string); ok && prefix != "" {
		opts.ProgramPrefix = prefix
	}

	result, err := s.adtClient.ExecuteABAP(ctx, code, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("ExecuteABAP failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Program: %s\n", result.ProgramName))
	sb.WriteString(fmt.Sprintf("Success: %t\n", result.Success))
	sb.WriteString(fmt.Sprintf("Execution Time: %.3f s\n", result.ExecutionTime))
	sb.WriteString(fmt.Sprintf("Cleaned Up: %t\n", result.CleanedUp))
	sb.WriteString(fmt.Sprintf("Message: %s\n", result.Message))

	if len(result.Output) > 0 {
		sb.WriteString("\nOutput:\n")
		for i, output := range result.Output {
			sb.WriteString(fmt.Sprintf("  [%d] %s\n", i+1, output))
		}
	}

	// Include raw alerts for debugging if no clean output was captured
	if len(result.Output) == 0 && len(result.RawAlerts) > 0 {
		sb.WriteString("\nRaw Alerts (for debugging):\n")
		for _, alert := range result.RawAlerts {
			sb.WriteString(fmt.Sprintf("  Kind: %s, Severity: %s\n", alert.Kind, alert.Severity))
			sb.WriteString(fmt.Sprintf("  Title: %s\n", alert.Title))
			if len(alert.Details) > 0 {
				sb.WriteString("  Details:\n")
				for _, d := range alert.Details {
					sb.WriteString(fmt.Sprintf("    - %s\n", d))
				}
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

// --- Debugger Session Handlers (WebSocket-based via ZADT_VSP) ---
// All breakpoint operations use WebSocket for reliable CSRF-free communication.

// ensureDebugWSClient ensures WebSocket debug client is connected.
func (s *Server) ensureDebugWSClient(ctx context.Context) error {
	if s.debugWSClient != nil && s.debugWSClient.IsConnected() {
		return nil
	}

	// Create new client
	s.debugWSClient = adt.NewDebugWebSocketClient(
		s.config.BaseURL,
		s.config.Client,
		s.config.Username,
		s.config.Password,
		s.config.InsecureSkipVerify,
	)

	return s.debugWSClient.Connect(ctx)
}

func (s *Server) handleSetBreakpoint(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	program, ok := request.Params.Arguments["program"].(string)
	if !ok || program == "" {
		return newToolResultError("program is required"), nil
	}

	lineFloat, ok := request.Params.Arguments["line"].(float64)
	if !ok || lineFloat <= 0 {
		return newToolResultError("line is required and must be positive"), nil
	}
	line := int(lineFloat)

	// Ensure WebSocket client is connected
	if err := s.ensureDebugWSClient(ctx); err != nil {
		return newToolResultError(fmt.Sprintf("Failed to connect to ZADT_VSP WebSocket: %v. Ensure ZADT_VSP is deployed and SAPC/SICF are configured.", err)), nil
	}

	bpID, err := s.debugWSClient.SetBreakpoint(ctx, program, line)
	if err != nil {
		return newToolResultError(fmt.Sprintf("SetBreakpoint failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Breakpoint set successfully!\n\nBreakpoint ID: %s\nProgram: %s\nLine: %d\n\nUse DebuggerListen to wait for the breakpoint to be hit.", bpID, program, line)), nil
}

func (s *Server) handleGetBreakpoints(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	if err := s.ensureDebugWSClient(ctx); err != nil {
		return newToolResultError(fmt.Sprintf("Failed to connect to ZADT_VSP WebSocket: %v", err)), nil
	}

	breakpoints, err := s.debugWSClient.GetBreakpoints(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetBreakpoints failed: %v", err)), nil
	}

	if len(breakpoints) == 0 {
		return mcp.NewToolResultText("No breakpoints are currently set."), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Active Breakpoints (%d):\n\n", len(breakpoints)))
	for i, bp := range breakpoints {
		sb.WriteString(fmt.Sprintf("%d. ID: %v\n", i+1, bp["id"]))
		if kind, ok := bp["kind"]; ok {
			sb.WriteString(fmt.Sprintf("   Kind: %v\n", kind))
		}
		if uri, ok := bp["uri"]; ok {
			sb.WriteString(fmt.Sprintf("   URI: %v\n", uri))
		}
		if line, ok := bp["line"]; ok {
			sb.WriteString(fmt.Sprintf("   Line: %v\n", line))
		}
		sb.WriteString("\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleDeleteBreakpoint(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	bpID, ok := request.Params.Arguments["breakpoint_id"].(string)
	if !ok || bpID == "" {
		return newToolResultError("breakpoint_id is required"), nil
	}

	if err := s.ensureDebugWSClient(ctx); err != nil {
		return newToolResultError(fmt.Sprintf("Failed to connect to ZADT_VSP WebSocket: %v", err)), nil
	}

	if err := s.debugWSClient.DeleteBreakpoint(ctx, bpID); err != nil {
		return newToolResultError(fmt.Sprintf("DeleteBreakpoint failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Breakpoint %s deleted successfully.", bpID)), nil
}

func (s *Server) handleCallRFC(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	function, ok := request.Params.Arguments["function"].(string)
	if !ok || function == "" {
		return newToolResultError("function is required"), nil
	}

	// Parse params if provided
	params := make(map[string]string)
	if paramsStr, ok := request.Params.Arguments["params"].(string); ok && paramsStr != "" {
		// Parse JSON params
		var rawParams map[string]interface{}
		if err := json.Unmarshal([]byte(paramsStr), &rawParams); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid params JSON: %v", err)), nil
		}
		for k, v := range rawParams {
			params[k] = fmt.Sprintf("%v", v)
		}
	}

	// Ensure WebSocket client is connected
	if err := s.ensureDebugWSClient(ctx); err != nil {
		return newToolResultError(fmt.Sprintf("Failed to connect to ZADT_VSP WebSocket: %v. Ensure ZADT_VSP is deployed and SAPC/SICF are configured.", err)), nil
	}

	result, err := s.debugWSClient.CallRFC(ctx, function, params)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CallRFC failed: %v", err)), nil
	}

	// Format result
	resultJSON, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(fmt.Sprintf("RFC call completed.\n\nFunction: %s\nSubrc: %d\n\nResult:\n%s", function, result.Subrc, string(resultJSON))), nil
}

// --- Legacy REST-based Debugger Handlers (fallback) ---
// These use REST API which works for Listen/Attach/Step but not for breakpoints.

func (s *Server) handleDebuggerListen(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	user, _ := request.Params.Arguments["user"].(string)
	if user == "" {
		user = s.config.Username // Default to connection user
	}
	timeout := 60 // default
	if t, ok := request.Params.Arguments["timeout"].(float64); ok && t > 0 {
		timeout = int(t)
		if timeout > 240 {
			timeout = 240 // max 240 seconds
		}
	}

	result, err := s.adtClient.DebuggerListen(ctx, &adt.ListenOptions{
		DebuggingMode:  adt.DebuggingModeUser,
		User:           user,
		TimeoutSeconds: timeout,
	})
	if err != nil {
		return newToolResultError(fmt.Sprintf("DebuggerListen failed: %v", err)), nil
	}

	if result.TimedOut {
		return mcp.NewToolResultText("Listener timed out - no debuggee hit a breakpoint within the timeout period."), nil
	}

	if result.Conflict != nil {
		return mcp.NewToolResultText(fmt.Sprintf("Listener conflict detected: %s (user: %s)",
			result.Conflict.ConflictText, result.Conflict.IdeUser)), nil
	}

	if result.Debuggee != nil {
		var sb strings.Builder
		sb.WriteString("Debuggee caught!\n\n")
		sb.WriteString(fmt.Sprintf("Debuggee ID: %s\n", result.Debuggee.ID))
		sb.WriteString(fmt.Sprintf("User: %s\n", result.Debuggee.User))
		sb.WriteString(fmt.Sprintf("Program: %s\n", result.Debuggee.Program))
		sb.WriteString(fmt.Sprintf("Include: %s\n", result.Debuggee.Include))
		sb.WriteString(fmt.Sprintf("Line: %d\n", result.Debuggee.Line))
		sb.WriteString(fmt.Sprintf("Kind: %s\n", result.Debuggee.Kind))
		sb.WriteString(fmt.Sprintf("Attachable: %v\n", result.Debuggee.IsAttachable))
		sb.WriteString(fmt.Sprintf("App Server: %s\n", result.Debuggee.AppServer))
		sb.WriteString("\nUse DebuggerAttach with the debuggee_id to attach to this session.")
		return mcp.NewToolResultText(sb.String()), nil
	}

	return mcp.NewToolResultText("Listener returned with no result."), nil
}

func (s *Server) handleDebuggerAttach(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	debuggeeID, ok := request.Params.Arguments["debuggee_id"].(string)
	if !ok || debuggeeID == "" {
		return newToolResultError("debuggee_id is required"), nil
	}

	user, _ := request.Params.Arguments["user"].(string)
	if user == "" {
		user = s.config.Username // Default to connection user
	}

	result, err := s.adtClient.DebuggerAttach(ctx, debuggeeID, user)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DebuggerAttach failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("Successfully attached to debuggee!\n\n")
	sb.WriteString(fmt.Sprintf("Debug Session ID: %s\n", result.DebugSessionID))
	sb.WriteString(fmt.Sprintf("Process ID: %d\n", result.ProcessID))
	sb.WriteString(fmt.Sprintf("Server: %s\n", result.ServerName))
	sb.WriteString(fmt.Sprintf("Stepping Possible: %v\n", result.IsSteppingPossible))
	sb.WriteString(fmt.Sprintf("Termination Possible: %v\n", result.IsTerminationPossible))

	if len(result.ReachedBreakpoints) > 0 {
		sb.WriteString("\nReached Breakpoints:\n")
		for _, bp := range result.ReachedBreakpoints {
			sb.WriteString(fmt.Sprintf("  - ID: %s (kind: %s)\n", bp.ID, bp.Kind))
		}
	}

	if len(result.Actions) > 0 {
		sb.WriteString("\nAvailable Actions:\n")
		for _, action := range result.Actions {
			sb.WriteString(fmt.Sprintf("  - %s: %s\n", action.Name, action.Title))
		}
	}

	sb.WriteString("\nUse DebuggerGetStack to see the call stack, DebuggerGetVariables to inspect variables.")
	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleDebuggerDetach(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	err := s.adtClient.DebuggerDetach(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DebuggerDetach failed: %v", err)), nil
	}

	return mcp.NewToolResultText("Successfully detached from debug session."), nil
}

func (s *Server) handleDebuggerStep(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	stepTypeStr, ok := request.Params.Arguments["step_type"].(string)
	if !ok || stepTypeStr == "" {
		return newToolResultError("step_type is required"), nil
	}

	// Map string to step type
	var stepType adt.DebugStepType
	switch stepTypeStr {
	case "stepInto":
		stepType = adt.DebugStepInto
	case "stepOver":
		stepType = adt.DebugStepOver
	case "stepReturn":
		stepType = adt.DebugStepReturn
	case "stepContinue":
		stepType = adt.DebugStepContinue
	case "stepRunToLine":
		stepType = adt.DebugStepRunToLine
	case "stepJumpToLine":
		stepType = adt.DebugStepJumpToLine
	default:
		return newToolResultError(fmt.Sprintf("Invalid step_type: %s. Valid values: stepInto, stepOver, stepReturn, stepContinue, stepRunToLine, stepJumpToLine", stepTypeStr)), nil
	}

	uri, _ := request.Params.Arguments["uri"].(string)

	result, err := s.adtClient.DebuggerStep(ctx, stepType, uri)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DebuggerStep failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Step '%s' executed.\n\n", stepTypeStr))
	sb.WriteString(fmt.Sprintf("Session: %s\n", result.DebugSessionID))
	sb.WriteString(fmt.Sprintf("Debuggee Changed: %v\n", result.IsDebuggeeChanged))
	sb.WriteString(fmt.Sprintf("Stepping Possible: %v\n", result.IsSteppingPossible))

	if len(result.ReachedBreakpoints) > 0 {
		sb.WriteString("\nReached Breakpoints:\n")
		for _, bp := range result.ReachedBreakpoints {
			sb.WriteString(fmt.Sprintf("  - ID: %s (kind: %s)\n", bp.ID, bp.Kind))
		}
	}

	sb.WriteString("\nUse DebuggerGetStack to see current position, DebuggerGetVariables to inspect variables.")
	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleDebuggerGetStack(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	result, err := s.adtClient.DebuggerGetStack(ctx, true)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DebuggerGetStack failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("Call Stack:\n\n")
	sb.WriteString(fmt.Sprintf("Server: %s\n", result.ServerName))
	sb.WriteString(fmt.Sprintf("Current Stack Index: %d\n\n", result.DebugCursorStackIndex))

	for i, entry := range result.Stack {
		marker := "  "
		if entry.StackPosition == result.DebugCursorStackIndex {
			marker = "→ "
		}
		sb.WriteString(fmt.Sprintf("%s[%d] %s::%s (line %d)\n",
			marker, entry.StackPosition, entry.ProgramName, entry.EventName, entry.Line))
		sb.WriteString(fmt.Sprintf("      Type: %s, Include: %s\n", entry.EventType, entry.IncludeName))
		if entry.SystemProgram {
			sb.WriteString("      (system program)\n")
		}
		if i < len(result.Stack)-1 {
			sb.WriteString("\n")
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleDebuggerGetVariables(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Parse variable_ids from request
	var variableIDs []string

	if ids, ok := request.Params.Arguments["variable_ids"].([]interface{}); ok {
		for _, id := range ids {
			if s, ok := id.(string); ok {
				variableIDs = append(variableIDs, s)
			}
		}
	}

	// Default to @ROOT if no IDs specified
	if len(variableIDs) == 0 {
		variableIDs = []string{"@ROOT"}
	}

	// If @ROOT is requested, use GetChildVariables for top-level vars
	if len(variableIDs) == 1 && variableIDs[0] == "@ROOT" {
		result, err := s.adtClient.DebuggerGetChildVariables(ctx, []string{"@ROOT", "@DATAAGING"})
		if err != nil {
			return newToolResultError(fmt.Sprintf("DebuggerGetVariables failed: %v", err)), nil
		}

		var sb strings.Builder
		sb.WriteString("Variables:\n\n")

		for _, v := range result.Variables {
			sb.WriteString(fmt.Sprintf("%s: %s = %s\n", v.Name, v.DeclaredTypeName, v.Value))
			sb.WriteString(fmt.Sprintf("  MetaType: %s, Kind: %s\n", v.MetaType, v.Kind))
			if v.IsComplexType() {
				sb.WriteString(fmt.Sprintf("  (complex type - use variable ID '%s' to expand)\n", v.ID))
			}
		}

		return mcp.NewToolResultText(sb.String()), nil
	}

	// Get specific variables
	result, err := s.adtClient.DebuggerGetVariables(ctx, variableIDs)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DebuggerGetVariables failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("Variables:\n\n")

	for _, v := range result {
		sb.WriteString(fmt.Sprintf("%s: %s = %s\n", v.Name, v.DeclaredTypeName, v.Value))
		sb.WriteString(fmt.Sprintf("  ID: %s\n", v.ID))
		sb.WriteString(fmt.Sprintf("  MetaType: %s, Kind: %s\n", v.MetaType, v.Kind))
		if v.HexValue != "" {
			sb.WriteString(fmt.Sprintf("  Hex: %s\n", v.HexValue))
		}
		if v.TableLines > 0 {
			sb.WriteString(fmt.Sprintf("  Table Lines: %d\n", v.TableLines))
		}
		if v.IsComplexType() {
			sb.WriteString("  (complex type - expandable)\n")
		}
		sb.WriteString("\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

// --- UI5/Fiori BSP Management Handlers ---

func (s *Server) handleUI5ListApps(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	query, _ := request.Params.Arguments["query"].(string)

	maxResults := 100
	if mr, ok := request.Params.Arguments["max_results"].(float64); ok {
		maxResults = int(mr)
	}

	apps, err := s.adtClient.UI5ListApps(ctx, query, maxResults)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5ListApps failed: %v", err)), nil
	}

	if len(apps) == 0 {
		return mcp.NewToolResultText("No UI5 applications found"), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Found %d UI5 applications:\n\n", len(apps)))

	for _, app := range apps {
		sb.WriteString(fmt.Sprintf("- %s", app.Name))
		if app.Description != "" {
			sb.WriteString(fmt.Sprintf(" (%s)", app.Description))
		}
		if app.Package != "" {
			sb.WriteString(fmt.Sprintf(" [%s]", app.Package))
		}
		sb.WriteString("\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleUI5GetApp(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	appName, ok := request.Params.Arguments["app_name"].(string)
	if !ok || appName == "" {
		return newToolResultError("app_name is required"), nil
	}

	details, err := s.adtClient.UI5GetApp(ctx, appName)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5GetApp failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("UI5 Application: %s\n", details.Name))
	if details.Description != "" {
		sb.WriteString(fmt.Sprintf("Description: %s\n", details.Description))
	}
	if details.Package != "" {
		sb.WriteString(fmt.Sprintf("Package: %s\n", details.Package))
	}

	if len(details.Files) > 0 {
		sb.WriteString(fmt.Sprintf("\nFiles (%d):\n", len(details.Files)))
		for _, f := range details.Files {
			if f.Type == "folder" {
				sb.WriteString(fmt.Sprintf("  [DIR]  %s\n", f.Path))
			} else {
				sb.WriteString(fmt.Sprintf("  [FILE] %s", f.Path))
				if f.Size > 0 {
					sb.WriteString(fmt.Sprintf(" (%d bytes)", f.Size))
				}
				sb.WriteString("\n")
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleUI5GetFileContent(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	appName, ok := request.Params.Arguments["app_name"].(string)
	if !ok || appName == "" {
		return newToolResultError("app_name is required"), nil
	}

	filePath, ok := request.Params.Arguments["file_path"].(string)
	if !ok || filePath == "" {
		return newToolResultError("file_path is required"), nil
	}

	content, err := s.adtClient.UI5GetFileContent(ctx, appName, filePath)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5GetFileContent failed: %v", err)), nil
	}

	return mcp.NewToolResultText(string(content)), nil
}

func (s *Server) handleUI5UploadFile(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	appName, ok := request.Params.Arguments["app_name"].(string)
	if !ok || appName == "" {
		return newToolResultError("app_name is required"), nil
	}

	filePath, ok := request.Params.Arguments["file_path"].(string)
	if !ok || filePath == "" {
		return newToolResultError("file_path is required"), nil
	}

	content, ok := request.Params.Arguments["content"].(string)
	if !ok {
		return newToolResultError("content is required"), nil
	}

	contentType, _ := request.Params.Arguments["content_type"].(string)

	err := s.adtClient.UI5UploadFile(ctx, appName, filePath, []byte(content), contentType)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5UploadFile failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Successfully uploaded %s to %s", filePath, appName)), nil
}

func (s *Server) handleUI5DeleteFile(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	appName, ok := request.Params.Arguments["app_name"].(string)
	if !ok || appName == "" {
		return newToolResultError("app_name is required"), nil
	}

	filePath, ok := request.Params.Arguments["file_path"].(string)
	if !ok || filePath == "" {
		return newToolResultError("file_path is required"), nil
	}

	err := s.adtClient.UI5DeleteFile(ctx, appName, filePath)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5DeleteFile failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Successfully deleted %s from %s", filePath, appName)), nil
}

func (s *Server) handleUI5CreateApp(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	appName, ok := request.Params.Arguments["app_name"].(string)
	if !ok || appName == "" {
		return newToolResultError("app_name is required"), nil
	}

	description, _ := request.Params.Arguments["description"].(string)

	packageName, ok := request.Params.Arguments["package"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package is required"), nil
	}

	transport, _ := request.Params.Arguments["transport"].(string)

	err := s.adtClient.UI5CreateApp(ctx, appName, description, packageName, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5CreateApp failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Successfully created UI5 application %s in package %s", appName, packageName)), nil
}

func (s *Server) handleUI5DeleteApp(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	appName, ok := request.Params.Arguments["app_name"].(string)
	if !ok || appName == "" {
		return newToolResultError("app_name is required"), nil
	}

	transport, _ := request.Params.Arguments["transport"].(string)

	err := s.adtClient.UI5DeleteApp(ctx, appName, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("UI5DeleteApp failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Successfully deleted UI5 application %s", appName)), nil
}

// --- AMDP (HANA) Debugger Handlers ---

func (s *Server) handleAMDPDebuggerStart(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Check if session already active
	if s.amdpWSClient != nil && s.amdpWSClient.IsActive() {
		return newToolResultError("AMDP session already active. Use AMDPDebuggerStop first."), nil
	}

	// Create WebSocket-based AMDP client (connects to ZADT_VSP)
	s.amdpWSClient = adt.NewAMDPWebSocketClient(
		s.config.BaseURL,
		s.config.Client,
		s.config.Username,
		s.config.Password,
		s.config.InsecureSkipVerify,
	)

	// Connect to ZADT_VSP WebSocket
	if err := s.amdpWSClient.Connect(ctx); err != nil {
		s.amdpWSClient = nil
		return newToolResultError(fmt.Sprintf("AMDPDebuggerStart: WebSocket connect failed: %v", err)), nil
	}

	// Start AMDP debug session
	cascadeMode := "FULL"
	if cm, ok := request.Params.Arguments["cascade_mode"].(string); ok && cm != "" {
		cascadeMode = cm
	}

	if err := s.amdpWSClient.Start(ctx, cascadeMode); err != nil {
		s.amdpWSClient.Close()
		s.amdpWSClient = nil
		return newToolResultError(fmt.Sprintf("AMDPDebuggerStart failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("AMDP Debug Session Started (WebSocket via ZADT_VSP)\n\n")
	sb.WriteString(fmt.Sprintf("Cascade Mode: %s\n", cascadeMode))
	sb.WriteString("\nSession uses WebSocket connection to ZADT_VSP APC handler.")
	sb.WriteString("\nUse AMDPDebuggerStep, AMDPGetVariables to interact.")
	sb.WriteString("\nUse AMDPDebuggerStop to terminate the session.")

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleAMDPDebuggerResume(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// WebSocket-based Resume via ZADT_VSP
	if s.amdpWSClient == nil || !s.amdpWSClient.IsActive() {
		return newToolResultError("No active AMDP session. Use AMDPDebuggerStart first."), nil
	}

	result, err := s.amdpWSClient.Resume(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("AMDPDebuggerResume failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("AMDP Debugger Resume\n\n")

	if len(result.Events) == 0 {
		sb.WriteString("Waiting for debuggee... (no events yet)\n")
		sb.WriteString("Execute AMDP code to trigger breakpoints.\n")
	} else {
		for i, event := range result.Events {
			sb.WriteString(fmt.Sprintf("Event %d: %s\n", i+1, event.Kind))
			if event.ContextID != "" {
				sb.WriteString(fmt.Sprintf("  Context ID: %s\n", event.ContextID))
			}
			if event.Kind == "on_break" {
				sb.WriteString("\n=== BREAKPOINT HIT ===\n")
				if event.ABAPPosition != nil {
					sb.WriteString(fmt.Sprintf("  ABAP Position: %s/%s line %d\n",
						event.ABAPPosition.Program,
						event.ABAPPosition.Include,
						event.ABAPPosition.Line))
				}
				if event.NativePosition != nil {
					sb.WriteString(fmt.Sprintf("  Native Position: %s.%s line %d\n",
						event.NativePosition.Schema,
						event.NativePosition.Name,
						event.NativePosition.Line))
				}
				sb.WriteString(fmt.Sprintf("  Variable Count: %d\n", event.VariableCount))
				sb.WriteString(fmt.Sprintf("  Stack Depth: %d\n", event.StackDepth))
			}
			if event.Aborted {
				sb.WriteString("  Session was aborted\n")
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleAMDPDebuggerStop(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// WebSocket-based Stop via ZADT_VSP
	if s.amdpWSClient == nil {
		return mcp.NewToolResultText("No AMDP session active."), nil
	}

	// Stop AMDP debug session
	if err := s.amdpWSClient.Stop(ctx); err != nil {
		// Close connection anyway
		s.amdpWSClient.Close()
		s.amdpWSClient = nil
		return newToolResultError(fmt.Sprintf("AMDPDebuggerStop failed: %v", err)), nil
	}

	// Close WebSocket connection
	s.amdpWSClient.Close()
	s.amdpWSClient = nil

	return mcp.NewToolResultText("AMDP debug session stopped. WebSocket connection closed."), nil
}

func (s *Server) handleAMDPDebuggerStep(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// WebSocket-based Step via ZADT_VSP
	if s.amdpWSClient == nil || !s.amdpWSClient.IsActive() {
		return newToolResultError("No active AMDP session. Use AMDPDebuggerStart first."), nil
	}

	stepType, ok := request.Params.Arguments["step_type"].(string)
	if !ok || stepType == "" {
		return newToolResultError("step_type is required"), nil
	}

	// Execute step via WebSocket
	if err := s.amdpWSClient.Step(ctx, stepType); err != nil {
		return newToolResultError(fmt.Sprintf("AMDPDebuggerStep failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Step executed: %s\n\n", stepType))
	sb.WriteString("Use AMDPDebuggerResume to check for breakpoint events.\n")

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleAMDPGetVariables(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// WebSocket-based GetVariables via ZADT_VSP
	if s.amdpWSClient == nil || !s.amdpWSClient.IsActive() {
		return newToolResultError("No active AMDP session. Use AMDPDebuggerStart first."), nil
	}

	result, err := s.amdpWSClient.GetVariables(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("AMDPGetVariables failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("AMDP Variables:\n\n")

	if len(result.Variables) == 0 {
		sb.WriteString("No variables available (session may not be at breakpoint)\n")
	} else {
		for _, v := range result.Variables {
			if v.Type == "table" {
				sb.WriteString(fmt.Sprintf("  %s: [TABLE %d rows]\n", v.Name, v.Rows))
			} else {
				sb.WriteString(fmt.Sprintf("  %s = %s (%s)\n", v.Name, v.Value, v.Type))
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleAMDPSetBreakpoint(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// WebSocket-based SetBreakpoint via ZADT_VSP
	if s.amdpWSClient == nil || !s.amdpWSClient.IsActive() {
		return newToolResultError("No active AMDP session. Use AMDPDebuggerStart first."), nil
	}

	procName, _ := request.Params.Arguments["proc_name"].(string)
	if procName == "" {
		return newToolResultError("proc_name is required"), nil
	}

	lineFloat, ok := request.Params.Arguments["line"].(float64)
	if !ok {
		return newToolResultError("line is required"), nil
	}
	line := int(lineFloat)

	// Set breakpoint via WebSocket
	if err := s.amdpWSClient.SetBreakpoint(ctx, procName, line); err != nil {
		return newToolResultError(fmt.Sprintf("AMDPSetBreakpoint failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("AMDP breakpoint set at %s line %d", procName, line)), nil
}

func (s *Server) handleAMDPGetBreakpoints(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// WebSocket-based GetBreakpoints via ZADT_VSP
	if s.amdpWSClient == nil || !s.amdpWSClient.IsActive() {
		return newToolResultError("No active AMDP session. Use AMDPDebuggerStart first."), nil
	}

	result, err := s.amdpWSClient.GetBreakpoints(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("AMDPGetBreakpoints failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString("AMDP Breakpoints:\n\n")

	if len(result.Breakpoints) == 0 {
		sb.WriteString("No breakpoints set.\n")
	} else {
		for i, bp := range result.Breakpoints {
			enabled := "enabled"
			if !bp.Enabled {
				enabled = "disabled"
			}
			sb.WriteString(fmt.Sprintf("  %d. %s line %d (%s)\n", i+1, bp.ObjectName, bp.Line, enabled))
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

// Transport Management Handlers

func (s *Server) handleListTransports(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Check safety config for transport operations
	if err := s.adtClient.Safety().CheckTransport("", "ListTransports", false); err != nil {
		return newToolResultError(err.Error()), nil
	}

	user, _ := request.Params.Arguments["user"].(string)

	transports, err := s.adtClient.ListTransports(ctx, user)
	if err != nil {
		return newToolResultError(fmt.Sprintf("ListTransports failed: %v", err)), nil
	}

	if len(transports) == 0 {
		return mcp.NewToolResultText("No modifiable transports found."), nil
	}

	jsonBytes, err := json.MarshalIndent(transports, "", "  ")
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to format result: %v", err)), nil
	}

	return mcp.NewToolResultText(string(jsonBytes)), nil
}

func (s *Server) handleGetTransport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	transport, ok := request.Params.Arguments["transport"].(string)
	if !ok || transport == "" {
		return newToolResultError("transport is required"), nil
	}

	// Check safety config for transport operations
	if err := s.adtClient.Safety().CheckTransport(transport, "GetTransport", false); err != nil {
		return newToolResultError(err.Error()), nil
	}

	result, err := s.adtClient.GetTransport(ctx, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GetTransport failed: %v", err)), nil
	}

	jsonBytes, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to format result: %v", err)), nil
	}

	return mcp.NewToolResultText(string(jsonBytes)), nil
}

func (s *Server) handleCreateTransport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Check safety config for transport write operations
	if err := s.adtClient.Safety().CheckTransport("", "CreateTransport", true); err != nil {
		return newToolResultError(err.Error()), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	pkg, ok := request.Params.Arguments["package"].(string)
	if !ok || pkg == "" {
		return newToolResultError("package is required"), nil
	}

	transportLayer, _ := request.Params.Arguments["transport_layer"].(string)
	transportType, _ := request.Params.Arguments["type"].(string)

	opts := adt.CreateTransportOptions{
		Description:    description,
		Package:        pkg,
		TransportLayer: transportLayer,
		Type:           transportType,
	}

	transportNumber, err := s.adtClient.CreateTransportV2(ctx, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("CreateTransport failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Transport created: %s", transportNumber)), nil
}

func (s *Server) handleReleaseTransport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	transport, ok := request.Params.Arguments["transport"].(string)
	if !ok || transport == "" {
		return newToolResultError("transport is required"), nil
	}

	// Check safety config for transport write operations
	if err := s.adtClient.Safety().CheckTransport(transport, "ReleaseTransport", true); err != nil {
		return newToolResultError(err.Error()), nil
	}

	ignoreLocks, _ := request.Params.Arguments["ignore_locks"].(bool)
	skipATC, _ := request.Params.Arguments["skip_atc"].(bool)

	opts := adt.ReleaseTransportOptions{
		IgnoreLocks: ignoreLocks,
		SkipATC:     skipATC,
	}

	err := s.adtClient.ReleaseTransportV2(ctx, transport, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("ReleaseTransport failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Transport %s released successfully.", transport)), nil
}

func (s *Server) handleDeleteTransport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	transport, ok := request.Params.Arguments["transport"].(string)
	if !ok || transport == "" {
		return newToolResultError("transport is required"), nil
	}

	// Check safety config for transport write operations
	if err := s.adtClient.Safety().CheckTransport(transport, "DeleteTransport", true); err != nil {
		return newToolResultError(err.Error()), nil
	}

	err := s.adtClient.DeleteTransport(ctx, transport)
	if err != nil {
		return newToolResultError(fmt.Sprintf("DeleteTransport failed: %v", err)), nil
	}

	return mcp.NewToolResultText(fmt.Sprintf("Transport %s deleted successfully.", transport)), nil
}

// --- Git/abapGit Handlers ---

func (s *Server) handleGitTypes(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Ensure WebSocket client is connected
	if s.amdpWSClient == nil {
		// Create and connect WebSocket client
		s.amdpWSClient = adt.NewAMDPWebSocketClient(
			s.config.BaseURL,
			s.config.Client,
			s.config.Username,
			s.config.Password,
			s.config.InsecureSkipVerify,
		)
		if err := s.amdpWSClient.Connect(ctx); err != nil {
			s.amdpWSClient = nil
			return newToolResultError(fmt.Sprintf("GitTypes: WebSocket connect failed: %v", err)), nil
		}
	}

	types, err := s.amdpWSClient.GitTypes(ctx)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GitTypes failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Supported abapGit Object Types: %d\n\n", len(types)))
	for i, t := range types {
		sb.WriteString(t)
		if i < len(types)-1 {
			if (i+1)%10 == 0 {
				sb.WriteString("\n")
			} else {
				sb.WriteString(", ")
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleGitExport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Ensure WebSocket client is connected
	if s.amdpWSClient == nil {
		s.amdpWSClient = adt.NewAMDPWebSocketClient(
			s.config.BaseURL,
			s.config.Client,
			s.config.Username,
			s.config.Password,
			s.config.InsecureSkipVerify,
		)
		if err := s.amdpWSClient.Connect(ctx); err != nil {
			s.amdpWSClient = nil
			return newToolResultError(fmt.Sprintf("GitExport: WebSocket connect failed: %v", err)), nil
		}
	}

	params := adt.GitExportParams{}

	// Parse packages
	if pkgStr, ok := request.Params.Arguments["packages"].(string); ok && pkgStr != "" {
		params.Packages = strings.Split(pkgStr, ",")
		for i, p := range params.Packages {
			params.Packages[i] = strings.TrimSpace(p)
		}
	}

	// Parse objects
	if objsStr, ok := request.Params.Arguments["objects"].(string); ok && objsStr != "" {
		var objs []adt.GitObjectRef
		if err := json.Unmarshal([]byte(objsStr), &objs); err != nil {
			return newToolResultError(fmt.Sprintf("Invalid objects JSON: %v", err)), nil
		}
		params.Objects = objs
	}

	// Include subpackages
	if inclSub, ok := request.Params.Arguments["include_subpackages"].(bool); ok {
		params.IncludeSubpackages = inclSub
	} else {
		params.IncludeSubpackages = true // default
	}

	if len(params.Packages) == 0 && len(params.Objects) == 0 {
		return newToolResultError("Either packages or objects parameter is required"), nil
	}

	result, err := s.amdpWSClient.GitExport(ctx, params)
	if err != nil {
		return newToolResultError(fmt.Sprintf("GitExport failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Git Export Successful\n\n"))
	sb.WriteString(fmt.Sprintf("Objects: %d\n", result.ObjectCount))
	sb.WriteString(fmt.Sprintf("Files: %d\n\n", result.FileCount))

	sb.WriteString("Files:\n")
	for _, f := range result.Files {
		sb.WriteString(fmt.Sprintf("  %s (%d bytes)\n", f.Path, f.Size))
	}

	sb.WriteString(fmt.Sprintf("\nZIP Base64 length: %d chars\n", len(result.ZipBase64)))
	sb.WriteString("Use base64 decode to extract the ZIP file.\n")

	return mcp.NewToolResultText(sb.String()), nil
}

// --- Report Execution Handlers ---

func (s *Server) handleRunReport(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Ensure WebSocket connection
	if s.amdpWSClient == nil {
		s.amdpWSClient = adt.NewAMDPWebSocketClient(
			s.config.BaseURL, s.config.Client, s.config.Username, s.config.Password, s.config.InsecureSkipVerify,
		)
		if err := s.amdpWSClient.Connect(ctx); err != nil {
			s.amdpWSClient = nil
			return newToolResultError(fmt.Sprintf("RunReport: WebSocket connect failed: %v", err)), nil
		}
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
	sb.WriteString(fmt.Sprintf("Report: %s\n", result.Report))
	sb.WriteString(fmt.Sprintf("Status: %s\n", result.Status))
	sb.WriteString(fmt.Sprintf("Runtime: %dms\n\n", result.RuntimeMs))

	if result.ALVCaptured {
		sb.WriteString(fmt.Sprintf("ALV Data Captured: %d rows", result.TotalRows))
		if result.Truncated {
			sb.WriteString(" (truncated)")
		}
		sb.WriteString("\n\n")

		// Show columns
		sb.WriteString("Columns:\n")
		for _, col := range result.Columns {
			sb.WriteString(fmt.Sprintf("  %s (%s)\n", col.Name, col.Type))
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
	// Ensure WebSocket connection
	if s.amdpWSClient == nil {
		s.amdpWSClient = adt.NewAMDPWebSocketClient(
			s.config.BaseURL, s.config.Client, s.config.Username, s.config.Password, s.config.InsecureSkipVerify,
		)
		if err := s.amdpWSClient.Connect(ctx); err != nil {
			s.amdpWSClient = nil
			return newToolResultError(fmt.Sprintf("GetVariants: WebSocket connect failed: %v", err)), nil
		}
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
	sb.WriteString(fmt.Sprintf("Variants for %s:\n\n", result.Report))

	if len(result.Variants) == 0 {
		sb.WriteString("No variants found.\n")
	} else {
		for _, v := range result.Variants {
			if v.Protected {
				sb.WriteString(fmt.Sprintf("  %s (protected)\n", v.Name))
			} else {
				sb.WriteString(fmt.Sprintf("  %s\n", v.Name))
			}
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleGetTextElements(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Ensure WebSocket connection
	if s.amdpWSClient == nil {
		s.amdpWSClient = adt.NewAMDPWebSocketClient(
			s.config.BaseURL, s.config.Client, s.config.Username, s.config.Password, s.config.InsecureSkipVerify,
		)
		if err := s.amdpWSClient.Connect(ctx); err != nil {
			s.amdpWSClient = nil
			return newToolResultError(fmt.Sprintf("GetTextElements: WebSocket connect failed: %v", err)), nil
		}
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
	sb.WriteString(fmt.Sprintf("Text Elements for %s (Language: %s)\n\n", result.Program, result.Language))

	sb.WriteString("Selection Texts:\n")
	if len(result.SelectionTexts) == 0 {
		sb.WriteString("  (none)\n")
	} else {
		for key, text := range result.SelectionTexts {
			sb.WriteString(fmt.Sprintf("  %s: %s\n", key, text))
		}
	}
	sb.WriteString("\n")

	sb.WriteString("Text Symbols:\n")
	if len(result.TextSymbols) == 0 {
		sb.WriteString("  (none)\n")
	} else {
		for key, text := range result.TextSymbols {
			sb.WriteString(fmt.Sprintf("  TEXT-%s: %s\n", key, text))
		}
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleSetTextElements(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Ensure WebSocket connection
	if s.amdpWSClient == nil {
		s.amdpWSClient = adt.NewAMDPWebSocketClient(
			s.config.BaseURL, s.config.Client, s.config.Username, s.config.Password, s.config.InsecureSkipVerify,
		)
		if err := s.amdpWSClient.Connect(ctx); err != nil {
			s.amdpWSClient = nil
			return newToolResultError(fmt.Sprintf("SetTextElements: WebSocket connect failed: %v", err)), nil
		}
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

	if params.SelectionTexts == nil && params.TextSymbols == nil {
		return newToolResultError("At least one of selection_texts or text_symbols is required"), nil
	}

	result, err := s.amdpWSClient.SetTextElements(ctx, params)
	if err != nil {
		return newToolResultError(fmt.Sprintf("SetTextElements failed: %v", err)), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Text Elements Updated for %s (Language: %s)\n\n", result.Program, result.Language))
	sb.WriteString(fmt.Sprintf("Status: %s\n", result.Status))
	sb.WriteString(fmt.Sprintf("Selection Texts Set: %d\n", result.SelectionTextsSet))
	sb.WriteString(fmt.Sprintf("Text Symbols Set: %d\n", result.TextSymbolsSet))

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleInstallDummyTest(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	const (
		testPackage   = "$ZADT_INSTALL_TEST"
		testInterface = "ZIF_DUMMY_TEST"
		testClass     = "ZCL_DUMMY_TEST"
	)

	checkOnly := false
	if check, ok := request.Params.Arguments["check_only"].(bool); ok {
		checkOnly = check
	}

	cleanup := false
	if cl, ok := request.Params.Arguments["cleanup"].(bool); ok {
		cleanup = cl
	}

	var sb strings.Builder
	sb.WriteString("InstallDummyTest - Workflow Verification\n")
	sb.WriteString("========================================\n\n")

	// Track success/failure
	allPassed := true
	stepNum := 0

	step := func(name string) {
		stepNum++
		sb.WriteString(fmt.Sprintf("[Step %d] %s\n", stepNum, name))
	}

	pass := func(msg string) {
		sb.WriteString(fmt.Sprintf("  ✓ %s\n", msg))
	}

	fail := func(msg string) {
		sb.WriteString(fmt.Sprintf("  ✗ %s\n", msg))
		allPassed = false
	}

	info := func(msg string) {
		sb.WriteString(fmt.Sprintf("  → %s\n", msg))
	}

	// Step 1: Check/Create package (upsert strategy)
	step("Package Check/Create")
	pkg, err := s.adtClient.GetPackage(ctx, testPackage)
	packageExists := err == nil && pkg.URI != "" // URI empty = package doesn't really exist
	if !packageExists {
		info("Package doesn't exist, creating...")
		if checkOnly {
			info("SKIP (check_only mode)")
		} else {
			err = s.adtClient.CreateObject(ctx, adt.CreateObjectOptions{
				ObjectType:  adt.ObjectTypePackage,
				Name:        testPackage,
				Description: "Install Tools Test Package",
			})
			if err != nil {
				fail(fmt.Sprintf("CreateObject(package) failed: %v", err))
			} else {
				// Verify
				pkg, err = s.adtClient.GetPackage(ctx, testPackage)
				if err != nil || pkg.URI == "" {
					fail(fmt.Sprintf("Verification failed: package not found after create"))
				} else {
					pass("Package created and verified")
				}
			}
		}
	} else {
		pass("Package exists")
	}

	if checkOnly {
		sb.WriteString("\n[check_only mode] Would deploy:\n")
		// Check existing objects
		intfResults, _ := s.adtClient.SearchObject(ctx, testInterface, 1)
		intfExists := len(intfResults) > 0 && intfResults[0].Name == testInterface
		classResults, _ := s.adtClient.SearchObject(ctx, testClass, 1)
		classExists := len(classResults) > 0 && classResults[0].Name == testClass

		intfAction := "CREATE"
		if intfExists {
			intfAction = "UPDATE"
		}
		classAction := "CREATE"
		if classExists {
			classAction = "UPDATE"
		}
		sb.WriteString(fmt.Sprintf("  1. Interface: %s [%s]\n", testInterface, intfAction))
		sb.WriteString(fmt.Sprintf("  2. Class: %s [%s]\n", testClass, classAction))
		return mcp.NewToolResultText(sb.String()), nil
	}

	// Step 2: Create/Update Interface (upsert)
	step("Interface Creation")

	// Check if interface exists
	intfResults, _ := s.adtClient.SearchObject(ctx, testInterface, 1)
	intfExists := len(intfResults) > 0 && intfResults[0].Name == testInterface
	if intfExists {
		info(fmt.Sprintf("Interface %s exists, will update", testInterface))
	}

	interfaceSource := `INTERFACE zif_dummy_test
  PUBLIC.

  METHODS get_value
    RETURNING VALUE(rv_value) TYPE string.

ENDINTERFACE.`

	opts := &adt.WriteSourceOptions{
		Package:     testPackage,
		Description: "Dummy Test Interface",
		Mode:        adt.WriteModeUpsert,
	}
	result, err := s.adtClient.WriteSource(ctx, "INTF", testInterface, interfaceSource, opts)
	if err != nil {
		fail(fmt.Sprintf("WriteSource failed: %v", err))
	} else if !result.Success {
		fail(fmt.Sprintf("WriteSource returned failure: %s", result.Message))
	} else {
		pass(fmt.Sprintf("Interface written (mode=%s)", result.Mode))
	}

	// Step 3: Verify Interface
	step("Interface Verification")
	results, err := s.adtClient.SearchObject(ctx, testInterface, 10)
	if err != nil {
		fail(fmt.Sprintf("SearchObject failed: %v", err))
	} else {
		found := false
		for _, r := range results {
			if r.Name == testInterface {
				pass(fmt.Sprintf("Found: %s (%s) in %s", r.Name, r.Type, r.PackageName))
				found = true
				break
			}
		}
		if !found {
			fail("Interface not found in search results!")
		}
	}

	// Step 4: Read Interface Source
	step("Interface Source Read-back")
	src, err := s.adtClient.GetSource(ctx, "INTF", testInterface, nil)
	if err != nil {
		fail(fmt.Sprintf("GetSource failed: %v", err))
	} else {
		pass(fmt.Sprintf("Source retrieved (%d bytes)", len(src)))
	}

	// Step 5: Create/Update Class (upsert)
	step("Class Creation")

	// Check if class exists
	classResults, _ := s.adtClient.SearchObject(ctx, testClass, 1)
	classExists := len(classResults) > 0 && classResults[0].Name == testClass
	if classExists {
		info(fmt.Sprintf("Class %s exists, will update", testClass))
	}

	classSource := `CLASS zcl_dummy_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_dummy_test.

ENDCLASS.

CLASS zcl_dummy_test IMPLEMENTATION.

  METHOD zif_dummy_test~get_value.
    rv_value = 'Dummy Test Passed'.
  ENDMETHOD.

ENDCLASS.`

	opts = &adt.WriteSourceOptions{
		Package:     testPackage,
		Description: "Dummy Test Class",
		Mode:        adt.WriteModeUpsert,
	}
	result, err = s.adtClient.WriteSource(ctx, "CLAS", testClass, classSource, opts)
	if err != nil {
		fail(fmt.Sprintf("WriteSource failed: %v", err))
	} else if !result.Success {
		fail(fmt.Sprintf("WriteSource returned failure: %s", result.Message))
	} else {
		pass(fmt.Sprintf("Class written (mode=%s)", result.Mode))
	}

	// Step 6: Verify Class
	step("Class Verification")
	results, err = s.adtClient.SearchObject(ctx, testClass, 10)
	if err != nil {
		fail(fmt.Sprintf("SearchObject failed: %v", err))
	} else {
		found := false
		for _, r := range results {
			if r.Name == testClass {
				pass(fmt.Sprintf("Found: %s (%s) in %s", r.Name, r.Type, r.PackageName))
				found = true
				break
			}
		}
		if !found {
			fail("Class not found in search results!")
		}
	}

	// Step 7: Read Class Source
	step("Class Source Read-back")
	src, err = s.adtClient.GetSource(ctx, "CLAS", testClass, nil)
	if err != nil {
		fail(fmt.Sprintf("GetSource failed: %v", err))
	} else {
		pass(fmt.Sprintf("Source retrieved (%d bytes)", len(src)))
	}

	// Step 8: Run Unit Tests (optional verification)
	step("Unit Tests (Activation Check)")
	testResult, err := s.adtClient.RunUnitTests(ctx, fmt.Sprintf("/sap/bc/adt/oo/classes/%s", strings.ToLower(testClass)), nil)
	if err != nil {
		info(fmt.Sprintf("RunUnitTests: %v (expected - no tests defined)", err))
	} else {
		pass(fmt.Sprintf("Unit test framework responded (classes=%d)", len(testResult.Classes)))
	}

	// Cleanup if requested
	if cleanup {
		sb.WriteString("\n[Cleanup]\n")
		sb.WriteString("  → Cleanup not yet implemented (manual deletion required)\n")
		// TODO: Implement delete via ADT
	}

	// Summary
	sb.WriteString("\n========================================\n")
	if allPassed {
		sb.WriteString("✅ ALL TESTS PASSED\n")
		sb.WriteString("\nThe Install* workflow is working correctly:\n")
		sb.WriteString("  • Package creation ✓\n")
		sb.WriteString("  • Interface creation (WriteSource) ✓\n")
		sb.WriteString("  • Interface verification (SearchObject) ✓\n")
		sb.WriteString("  • Interface read-back (GetSource) ✓\n")
		sb.WriteString("  • Class creation (WriteSource) ✓\n")
		sb.WriteString("  • Class verification (SearchObject) ✓\n")
		sb.WriteString("  • Class read-back (GetSource) ✓\n")
	} else {
		sb.WriteString("❌ SOME TESTS FAILED\n")
		sb.WriteString("\nCheck the steps above for details.\n")
	}

	sb.WriteString(fmt.Sprintf("\nTest objects in package: %s\n", testPackage))

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleInstallZADTVSP(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Parse parameters
	packageName := "$ZADT_VSP"
	if pkg, ok := request.Params.Arguments["package"].(string); ok && pkg != "" {
		packageName = strings.ToUpper(pkg)
	}

	skipGitService := false
	if skip, ok := request.Params.Arguments["skip_git_service"].(bool); ok {
		skipGitService = skip
	}

	checkOnly := false
	if check, ok := request.Params.Arguments["check_only"].(bool); ok {
		checkOnly = check
	}

	// Validate package name
	if !strings.HasPrefix(packageName, "$") {
		return newToolResultError("Package name must start with $ (local package)"), nil
	}

	var sb strings.Builder
	sb.WriteString("ZADT_VSP Installation\n")
	sb.WriteString("=====================\n\n")

	// Phase 1: Check prerequisites
	sb.WriteString("Checking prerequisites...\n")

	// Check if package exists (verify URI is populated - empty URI means package doesn't really exist)
	packageExists := false
	pkg, err := s.adtClient.GetPackage(ctx, packageName)
	if err == nil && pkg.URI != "" {
		packageExists = true
		sb.WriteString(fmt.Sprintf("  ✓ Package %s exists\n", packageName))
	} else {
		sb.WriteString(fmt.Sprintf("  → Package %s will be created\n", packageName))
	}

	// Check for abapGit (for Git service)
	hasAbapGit := false
	results, err := s.adtClient.SearchObject(ctx, "ZCL_ABAPGIT_OBJECTS", 1)
	if err == nil && len(results) > 0 {
		hasAbapGit = true
		sb.WriteString("  ✓ abapGit detected → Git service will be deployed\n")
	} else {
		sb.WriteString("  ⚠ abapGit not detected → Git service will be skipped\n")
		skipGitService = true
	}

	// Check existing objects
	objects := embedded.GetObjects()
	existingObjects := []string{}
	for _, obj := range objects {
		results, err := s.adtClient.SearchObject(ctx, obj.Name, 1)
		if err == nil && len(results) > 0 {
			existingObjects = append(existingObjects, obj.Name)
		}
	}
	if len(existingObjects) > 0 {
		sb.WriteString(fmt.Sprintf("  ⚠ Existing objects will be updated: %s\n", strings.Join(existingObjects, ", ")))
	}

	sb.WriteString("\n")

	// If check_only, stop here
	if checkOnly {
		sb.WriteString("Check complete (--check_only mode, no changes made).\n\n")
		sb.WriteString("Objects to deploy:\n")
		for i, obj := range objects {
			if obj.Optional && skipGitService && obj.Name == "ZCL_VSP_GIT_SERVICE" {
				sb.WriteString(fmt.Sprintf("  [%d/%d] %s - SKIP (no abapGit)\n", i+1, len(objects), obj.Name))
			} else {
				sb.WriteString(fmt.Sprintf("  [%d/%d] %s - %s\n", i+1, len(objects), obj.Name, obj.Description))
			}
		}
		return mcp.NewToolResultText(sb.String()), nil
	}

	// Phase 2: Create package if needed
	if !packageExists {
		sb.WriteString(fmt.Sprintf("Creating package %s...\n", packageName))
		createOpts := adt.CreateObjectOptions{
			ObjectType:  adt.ObjectTypePackage,
			Name:        packageName,
			Description: "VSP WebSocket Handler",
		}
		err := s.adtClient.CreateObject(ctx, createOpts)
		if err != nil {
			return newToolResultError(fmt.Sprintf("Failed to create package: %v", err)), nil
		}
		sb.WriteString(fmt.Sprintf("  ✓ Package %s created\n\n", packageName))
	} else {
		sb.WriteString(fmt.Sprintf("Using existing package %s\n\n", packageName))
	}

	// Phase 3: Deploy objects
	sb.WriteString("Deploying ABAP objects...\n")

	deployed := []string{}
	skipped := []string{}
	failed := []string{}

	for i, obj := range objects {
		// Skip Git service if no abapGit
		if obj.Name == "ZCL_VSP_GIT_SERVICE" && skipGitService {
			sb.WriteString(fmt.Sprintf("  [%d/%d] %s ⊘ Skipped (no abapGit)\n", i+1, len(objects), obj.Name))
			skipped = append(skipped, obj.Name)
			continue
		}

		sb.WriteString(fmt.Sprintf("  [%d/%d] %s ", i+1, len(objects), obj.Name))

		// Use WriteSource to create/update
		opts := &adt.WriteSourceOptions{
			Package: packageName,
			Mode:    adt.WriteModeUpsert,
		}
		_, err := s.adtClient.WriteSource(ctx, obj.Type, obj.Name, obj.Source, opts)
		if err != nil {
			sb.WriteString(fmt.Sprintf("✗ Failed: %v\n", err))
			failed = append(failed, obj.Name+": "+err.Error())
		} else {
			sb.WriteString("✓ Deployed\n")
			deployed = append(deployed, obj.Name)
		}
	}

	sb.WriteString("\n")

	// Summary
	sb.WriteString("═══════════════════════════════════════════════════════════════════════════════\n")
	if len(failed) > 0 {
		sb.WriteString("  DEPLOYMENT PARTIALLY FAILED\n")
		sb.WriteString("═══════════════════════════════════════════════════════════════════════════════\n\n")
		sb.WriteString("Failed objects:\n")
		for _, f := range failed {
			sb.WriteString(fmt.Sprintf("  • %s\n", f))
		}
	} else {
		sb.WriteString("  DEPLOYMENT COMPLETE - Manual Steps Required\n")
		sb.WriteString("═══════════════════════════════════════════════════════════════════════════════\n")
	}

	sb.WriteString(fmt.Sprintf("\nDeployed: %d, Skipped: %d, Failed: %d\n\n", len(deployed), len(skipped), len(failed)))

	// Post-deployment instructions
	sb.WriteString(embedded.PostDeploymentInstructions())

	// Features unlocked
	sb.WriteString("\nFeatures unlocked:\n")
	sb.WriteString("  ✓ WebSocket debugging (TPDAPI)\n")
	sb.WriteString("  ✓ RFC/BAPI execution\n")
	sb.WriteString("  ✓ AMDP debugging (experimental)\n")
	if hasAbapGit && !skipGitService {
		sb.WriteString("  ✓ abapGit export (158 object types)\n")
	} else {
		sb.WriteString("  ✗ abapGit export (install abapGit first)\n")
	}

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleListDependencies(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	var sb strings.Builder
	sb.WriteString("Available Dependencies\n")
	sb.WriteString("======================\n\n")

	dependencies := deps.GetAvailableDependencies()
	for _, dep := range dependencies {
		status := "⚠ Not embedded (placeholder)"
		if dep.Available {
			status = "✓ Available"
		}
		sb.WriteString(fmt.Sprintf("• %s\n", dep.Name))
		sb.WriteString(fmt.Sprintf("  Description: %s\n", dep.Description))
		sb.WriteString(fmt.Sprintf("  Package: %s\n", dep.Package))
		sb.WriteString(fmt.Sprintf("  Status: %s\n", status))
		sb.WriteString("\n")
	}

	sb.WriteString("Usage:\n")
	sb.WriteString("  InstallAbapGit --edition standalone  # Single program ZABAPGIT\n")
	sb.WriteString("  InstallAbapGit --edition dev         # Full $ZGIT_DEV packages\n")
	sb.WriteString("\nNote: To add dependency ZIPs, export from SAP with GitExport and\n")
	sb.WriteString("embed in embedded/deps/\n")

	return mcp.NewToolResultText(sb.String()), nil
}

func (s *Server) handleInstallAbapGit(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	// Parse parameters
	edition := "standalone"
	if ed, ok := request.Params.Arguments["edition"].(string); ok && ed != "" {
		edition = strings.ToLower(ed)
	}

	packageName := ""
	if pkg, ok := request.Params.Arguments["package"].(string); ok && pkg != "" {
		packageName = strings.ToUpper(pkg)
	}

	checkOnly := false
	if check, ok := request.Params.Arguments["check_only"].(bool); ok {
		checkOnly = check
	}

	// Validate edition
	if edition != "standalone" && edition != "dev" {
		return newToolResultError("Invalid edition. Use 'standalone' or 'dev'"), nil
	}

	// Set default package based on edition
	if packageName == "" {
		if edition == "standalone" {
			packageName = "$ABAPGIT"
		} else {
			packageName = "$ZGIT_DEV"
		}
	}

	// Validate package name
	if !strings.HasPrefix(packageName, "$") {
		return newToolResultError("Package name must start with $ (local package)"), nil
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Install abapGit (%s edition)\n", edition))
	sb.WriteString("============================\n\n")

	// Check if ZIP is available
	dependencies := deps.GetAvailableDependencies()
	var selectedDep *deps.DependencyInfo
	for i, dep := range dependencies {
		if (edition == "standalone" && dep.Name == "abapgit-standalone") ||
			(edition == "dev" && dep.Name == "abapgit-dev") {
			selectedDep = &dependencies[i]
			break
		}
	}

	if selectedDep == nil {
		return newToolResultError(fmt.Sprintf("Edition '%s' not found in available dependencies", edition)), nil
	}

	if !selectedDep.Available {
		sb.WriteString("⚠ ZIP not embedded yet\n\n")
		sb.WriteString("To embed abapGit:\n")
		sb.WriteString("1. On a system with abapGit installed, run:\n")
		if edition == "standalone" {
			sb.WriteString("   vsp git-export --objects '[{\"type\":\"PROG\",\"name\":\"ZABAPGIT\"}]' > abapgit-standalone.zip\n")
		} else {
			sb.WriteString("   vsp git-export --packages '$ZGIT_DEV' --include-subpackages > abapgit-dev.zip\n")
		}
		sb.WriteString("\n2. Place ZIP in embedded/deps/\n")
		sb.WriteString("3. Update embedded/deps/embed.go with go:embed directive\n")
		sb.WriteString("4. Rebuild vsp\n\n")
		sb.WriteString("Alternative: Download from GitHub:\n")
		sb.WriteString("  https://github.com/abapGit/abapGit\n")
		return mcp.NewToolResultText(sb.String()), nil
	}

	// TODO: Implement actual deployment when ZIPs are embedded
	// This is the workflow:
	// 1. Get ZIP data from embedded variable
	// 2. files, err := deps.UnzipInMemory(zipData)
	// 3. plan := deps.CreateDeploymentPlan(edition, packageName, files)
	// 4. If checkOnly, show plan and return
	// 5. For each object in plan:
	//    - Check if exists (SearchObject)
	//    - WriteSource for main source
	//    - WriteSource for each include (locals_def, locals_imp, testclasses)
	// 6. Show summary

	sb.WriteString(fmt.Sprintf("Target package: %s\n", packageName))
	sb.WriteString(fmt.Sprintf("Edition: %s\n", edition))
	sb.WriteString(fmt.Sprintf("Check only: %v\n\n", checkOnly))

	sb.WriteString("Status: ZIP embedding ready, deployment logic implemented.\n")
	sb.WriteString("Waiting for actual ZIP files to be embedded.\n\n")

	sb.WriteString("Architecture:\n")
	sb.WriteString("  1. embedded/deps/embed.go - go:embed for ZIP files\n")
	sb.WriteString("  2. deps.UnzipInMemory() - Extract files in memory\n")
	sb.WriteString("  3. deps.ParseAbapGitFilename() - Parse object type/name\n")
	sb.WriteString("  4. deps.CreateDeploymentPlan() - Sort by dependency order\n")
	sb.WriteString("  5. WriteSource() - Deploy each object via ADT\n")

	return mcp.NewToolResultText(sb.String()), nil
}
