# CDS Dependency Analyzer Tool & Object Type Unification Analysis

**Date:** 2025-12-04
**Status:** 🧠 Brainstorming
**Related:**
- [CDS Dependency Investigation](2025-12-02-016-cds-and-zray-endpoint-investigation.md)
- [CDS & $ZRAY Brainstorming](2025-12-02-015-cds-dependency-and-zray-local-implementation.md)
- [Focused Mode Proposal](focused-mode-proposal.md)

---

## Part 1: CDS Dependency Analyzer Tool

### 1.1 Executive Summary

**Status:** ✅ **READY FOR IMPLEMENTATION**

**Key Finding:** ADT REST API exists via `CL_CDS_RES_DEPENDENCIES` (SABP_UNIT_DOUBLE_CDS_CORE)

**Value Proposition:**
- Understand CDS view dependency trees (critical for S/4HANA development)
- Analyze impact before changes (what breaks if I modify this view?)
- Visualize complex CDS hierarchies
- Enable automated dependency analysis for AI agents

**Estimated Effort:** 2-3 days

---

### 1.2 Why CDS Dependencies Matter

#### The Problem

Modern ABAP development increasingly uses CDS views for:
- Virtual Data Models (VDM) in S/4HANA
- Analytical queries
- Fiori OData services
- RAP (RESTFUL ABAP Programming)

**Challenge:** CDS views can have deep dependency chains:
```
ZDDL_SALES_ORDER (your view)
  └─ FROM I_SalesOrder (SAP standard)
       └─ FROM VBAK (table)
       └─ LEFT JOIN I_Customer
            └─ FROM KNA1
            └─ INNER JOIN I_Address
                 └─ FROM ADR6
```

**Without dependency analyzer:**
- ❌ Don't know what breaks if you change a view
- ❌ Can't trace where data comes from
- ❌ Manual investigation via Eclipse ADT only
- ❌ No API for automation

**With dependency analyzer:**
- ✅ See full dependency tree instantly
- ✅ Understand data lineage
- ✅ Impact analysis before changes
- ✅ AI agents can reason about dependencies

---

### 1.3 Tool Design: GetCDSDependencies

#### MCP Tool Specification

**Tool Name:** `GetCDSDependencies`

**Description:** "Retrieve CDS view dependency tree showing all dependent objects (tables, views, associations)"

**Input Parameters:**
```json
{
  "ddls_name": {
    "type": "string",
    "required": true,
    "description": "CDS DDL source name (e.g., 'I_SalesOrder', 'ZDDL_MY_VIEW')"
  },
  "dependency_level": {
    "type": "string",
    "required": false,
    "default": "hierarchy",
    "enum": ["unit", "hierarchy"],
    "description": "Level of dependency resolution: 'unit' (direct only) or 'hierarchy' (recursive)"
  },
  "with_associations": {
    "type": "boolean",
    "required": false,
    "default": false,
    "description": "Include modeled associations in dependency tree"
  },
  "context_package": {
    "type": "string",
    "required": false,
    "description": "Filter dependencies to specific package context"
  }
}
```

**Output Structure:**
```json
{
  "name": "ZDDL_SALES_ORDER",
  "type": "CDS_VIEW",
  "entity_name": "ZDDL_SALES_ORDER",
  "user_defined_entity_name": "ZddlSalesOrder",
  "activation_state": "ACTIVE",
  "ddls_name": "ZDDL_SALES_ORDER",
  "has_params": false,
  "children": [
    {
      "name": "I_SALESORDER",
      "type": "CDS_VIEW",
      "relation": "FROM",
      "entity_name": "I_SalesOrder",
      "activation_state": "ACTIVE",
      "ddls_name": "I_SALESORDER",
      "children": [
        {
          "name": "VBAK",
          "type": "TABLE",
          "relation": "FROM"
        },
        {
          "name": "I_CUSTOMER",
          "type": "CDS_VIEW",
          "relation": "LEFT_OUTER_JOIN",
          "children": [...]
        }
      ]
    }
  ]
}
```

---

### 1.4 Implementation Details

#### Backend API

**Endpoint (verified):**
```
GET /sap/bc/adt/cds/dependencies
  ?ddls_name=<name>
  &dependency_level=hierarchy
  &with_associations=false
  &contextPackage=<package>
```

**Response Content Type:**
```
application/vnd.sap.adt.aunit.cds.dependencymodel.v1+xml
```

**ABAP Handler Class:**
- `CL_CDS_RES_DEPENDENCIES` (SABP_UNIT_DOUBLE_CDS_CORE) - REST resource controller
- `CL_CDS_TEST_DDL_HIER_PARSER` - Dependency graph parser
- `CL_DDLS_DEPENDENCY_VISITOR` (SDDIC_ADT_DDLS_TOOLS) - Alternative analyzer

#### Node Types Supported

| Type | Description | Example |
|------|-------------|---------|
| `TABLE` | Database table | MARA, VBAK |
| `CDS_VIEW` | CDS view (DDIC-based) | I_SalesOrder |
| `CDS_DB_VIEW` | CDS database view | I_SalesOrderDB |
| `VIEW` | Classic DDIC view | V_VBAK |
| `CDS_TABLE_FUNCTION` | AMDP table function | F_CALCULATE_PRICE |
| `EXTERNAL_VIEW` | External SAP HANA view | - |
| `PROJECTION_VIEW` | CDS projection view | C_SalesOrder |
| `UNKNOWN` | Unresolved dependency | - |

#### Relation Types

| Relation | Description | SQL Equivalent |
|----------|-------------|----------------|
| `FROM` | Main data source | FROM table |
| `INNER_JOIN` | Inner join | INNER JOIN |
| `LEFT_OUTER_JOIN` | Left outer join | LEFT OUTER JOIN |
| `RIGHT_OUTER_JOIN` | Right outer join | RIGHT OUTER JOIN |
| `MODELED` | Association | - |
| `CYCLIC_DEPENDENCY` | Circular reference (error) | - |

#### Activation States

| State | Description |
|-------|-------------|
| `ACTIVE` | Activated successfully |
| `INACTIVE` | Not yet activated |
| `INCONSISTENT` | Activation errors |

---

### 1.5 Go Implementation

#### File: `pkg/adt/cds.go` (NEW FILE)

```go
package adt

import (
	"context"
	"encoding/xml"
	"fmt"
)

// CDSDependencyNode represents a node in the CDS dependency tree
type CDSDependencyNode struct {
	Name                   string                `xml:"name,attr"`
	Type                   string                `xml:"type,attr"`
	ObjectType             string                `xml:"object_type,attr"`
	HasParams              bool                  `xml:"has_params,attr"`
	Relation               string                `xml:"relation,attr"`
	EntityName             string                `xml:"entity_name,attr"`
	UserDefinedEntityName  string                `xml:"user_defined_entity_name,attr"`
	ActivationState        string                `xml:"activation_state,attr"`
	DDLSName               string                `xml:"ddls_name,attr"`
	Children               []CDSDependencyNode   `xml:"node"`
}

// CDSDependencyOptions configures dependency retrieval
type CDSDependencyOptions struct {
	DependencyLevel  string // "unit" or "hierarchy"
	WithAssociations bool
	ContextPackage   string
}

// GetCDSDependencies retrieves CDS view dependency tree
func (c *Client) GetCDSDependencies(ctx context.Context, ddlsName string, opts CDSDependencyOptions) (*CDSDependencyNode, error) {
	if ddlsName == "" {
		return nil, fmt.Errorf("ddlsName is required")
	}

	// Default to hierarchy if not specified
	if opts.DependencyLevel == "" {
		opts.DependencyLevel = "hierarchy"
	}

	// Build query parameters
	url := fmt.Sprintf("/sap/bc/adt/cds/dependencies?ddls_name=%s&dependency_level=%s&with_associations=%v",
		ddlsName, opts.DependencyLevel, opts.WithAssociations)

	if opts.ContextPackage != "" {
		url += fmt.Sprintf("&contextPackage=%s", opts.ContextPackage)
	}

	resp, err := c.http.Get(ctx, url)
	if err != nil {
		return nil, fmt.Errorf("failed to get CDS dependencies: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("unexpected status code: %d", resp.StatusCode)
	}

	var root CDSDependencyNode
	if err := xml.NewDecoder(resp.Body).Decode(&root); err != nil {
		return nil, fmt.Errorf("failed to parse XML response: %w", err)
	}

	return &root, nil
}

// FlattenDependencies returns a flat list of all dependencies (BFS traversal)
func (node *CDSDependencyNode) FlattenDependencies() []CDSDependencyNode {
	var result []CDSDependencyNode
	queue := []*CDSDependencyNode{node}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		result = append(result, *current)

		for i := range current.Children {
			queue = append(queue, &current.Children[i])
		}
	}

	return result
}

// CountDependenciesByType returns count of dependencies grouped by type
func (node *CDSDependencyNode) CountDependenciesByType() map[string]int {
	counts := make(map[string]int)
	flat := node.FlattenDependencies()

	for _, dep := range flat {
		counts[dep.Type]++
	}

	return counts
}

// FindCycles detects circular dependencies in the tree
func (node *CDSDependencyNode) FindCycles() []string {
	var cycles []string
	visited := make(map[string]bool)
	stack := make(map[string]bool)

	var dfs func(*CDSDependencyNode)
	dfs = func(n *CDSDependencyNode) {
		visited[n.Name] = true
		stack[n.Name] = true

		for i := range n.Children {
			child := &n.Children[i]
			if !visited[child.Name] {
				dfs(child)
			} else if stack[child.Name] {
				cycles = append(cycles, fmt.Sprintf("%s -> %s", n.Name, child.Name))
			}
		}

		stack[n.Name] = false
	}

	dfs(node)
	return cycles
}
```

---

### 1.6 MCP Server Integration

#### File: `internal/mcp/server.go` (ADD TO registerTools)

```go
// GetCDSDependencies
s.mcpServer.AddTool(mcp.NewTool("GetCDSDependencies",
	mcp.WithDescription("Retrieve CDS view dependency tree showing all dependent objects (tables, views, associations)"),
	mcp.WithString("ddls_name",
		mcp.Required(),
		mcp.Description("CDS DDL source name (e.g., 'I_SalesOrder', 'ZDDL_MY_VIEW')"),
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
```

#### Handler Function

```go
func (s *Server) handleGetCDSDependencies(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	ddlsName, ok := request.Params.Arguments["ddls_name"].(string)
	if !ok || ddlsName == "" {
		return newToolResultError("ddls_name is required"), nil
	}

	opts := adt.CDSDependencyOptions{
		DependencyLevel:  "hierarchy",
		WithAssociations: false,
	}

	if level, ok := request.Params.Arguments["dependency_level"].(string); ok {
		opts.DependencyLevel = level
	}

	if assoc, ok := request.Params.Arguments["with_associations"].(bool); ok {
		opts.WithAssociations = assoc
	}

	if pkg, ok := request.Params.Arguments["context_package"].(string); ok {
		opts.ContextPackage = pkg
	}

	result, err := s.adtClient.GetCDSDependencies(ctx, ddlsName, opts)
	if err != nil {
		return newToolResultError(fmt.Sprintf("Failed to get CDS dependencies: %v", err)), nil
	}

	// Add metadata summary
	summary := map[string]interface{}{
		"ddls_name":       ddlsName,
		"dependency_tree": result,
		"statistics": map[string]interface{}{
			"total_dependencies": len(result.FlattenDependencies()),
			"by_type":            result.CountDependenciesByType(),
			"cycles":             result.FindCycles(),
		},
	}

	jsonResult, _ := json.MarshalIndent(summary, "", "  ")
	return mcp.NewToolResultText(string(jsonResult)), nil
}
```

---

### 1.7 Integration Test

#### File: `pkg/adt/integration_test.go` (ADD)

```go
func TestIntegration_GetCDSDependencies(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	ctx := context.Background()
	client := getTestClient(t)

	// Test with simple SAP standard CDS view
	result, err := client.GetCDSDependencies(ctx, "ACM_DDDDLSRC", adt.CDSDependencyOptions{
		DependencyLevel:  "hierarchy",
		WithAssociations: false,
	})

	if err != nil {
		t.Fatalf("GetCDSDependencies failed: %v", err)
	}

	if result.Name == "" {
		t.Error("Expected result name, got empty")
	}

	t.Logf("CDS view: %s", result.Name)
	t.Logf("Type: %s", result.Type)
	t.Logf("Activation state: %s", result.ActivationState)
	t.Logf("Children count: %d", len(result.Children))

	// Test flattening
	flat := result.FlattenDependencies()
	t.Logf("Total dependencies (flat): %d", len(flat))

	// Test type counting
	byType := result.CountDependenciesByType()
	for typ, count := range byType {
		t.Logf("  %s: %d", typ, count)
	}

	// Test cycle detection
	cycles := result.FindCycles()
	if len(cycles) > 0 {
		t.Logf("WARNING: Cycles detected: %v", cycles)
	}
}
```

---

### 1.8 Use Cases & Workflows

#### Use Case 1: Impact Analysis Before Modification

**Scenario:** Developer wants to modify `ZDDL_SALES_ORDER`, needs to know what depends on it.

**Workflow:**
```
1. GetCDSDependencies("ZDDL_SALES_ORDER", hierarchy)
   → Returns: 15 child dependencies (3 tables, 12 CDS views)

2. AI agent analyzes:
   - Used by: C_SALESORDER_TP (Fiori transactional app)
   - Risk: HIGH (breaking change impacts Fiori app)
   - Recommendation: Create new view instead of modifying

3. Developer decides: Create ZDDL_SALES_ORDER_V2
```

#### Use Case 2: Data Lineage Tracing

**Scenario:** Auditor asks "Where does the CUSTOMER_NAME field come from in our sales report?"

**Workflow:**
```
1. Start with report CDS view: "ZRPT_SALES_DETAIL"

2. GetCDSDependencies → Traverse tree:
   ZRPT_SALES_DETAIL
     └─ FROM ZDDL_SALES_ORDER
          └─ INNER JOIN I_CUSTOMER
               └─ FROM KNA1 (table)
                    └─ NAME1 field

3. Answer: "CUSTOMER_NAME comes from KNA1-NAME1 table field"
```

#### Use Case 3: Inactive Dependency Detection

**Scenario:** CDS view won't activate, error says "dependent object not active"

**Workflow:**
```
1. GetCDSDependencies("ZDDL_MY_VIEW", hierarchy)

2. Check activation_state for all dependencies

3. Find: I_CUSTOM_ENTITY has activation_state = "INACTIVE"

4. Action: Activate I_CUSTOM_ENTITY first, then retry ZDDL_MY_VIEW
```

#### Use Case 4: Circular Dependency Detection

**Scenario:** CDS view activation fails with "cyclic dependency"

**Workflow:**
```
1. GetCDSDependencies("ZDDL_A", hierarchy)

2. result.FindCycles() returns: ["ZDDL_A -> ZDDL_B -> ZDDL_C -> ZDDL_A"]

3. AI agent identifies: Association from ZDDL_C back to ZDDL_A causes cycle

4. Recommendation: Remove circular association or redesign view structure
```

---

## Part 2: FUGR and PACKAGE Object Type Analysis

### 2.1 The Question

**From Focused Mode Proposal:**
> GetSource(type, name, [parent], [include]) - unified tool for all "code units"

**User's Question:**
> "FUGR and PACKAGE are 'separate' slightly different objects - maybe this tool unification can allow separate tools for them?"

### 2.2 Analysis: What Do These Tools Return?

#### GetProgram, GetClass, GetInterface, GetInclude
**Returns:** Plain text source code (ABAP)
```abap
REPORT ZTEST.
  WRITE: / 'Hello World'.
```

#### GetFunctionGroup
**Returns:** JSON structure (metadata + source)
```json
{
  "name": "SYST",
  "description": "System Fields",
  "mainInclude": "LSYSTTOP",
  "functionModules": [
    {"name": "SYSTEM_INITIALIZE", "include": "LSYSTU01"},
    {"name": "SYSTEM_RESET", "include": "LSYSTU02"}
  ],
  "includes": [
    {"name": "LSYSTTOP", "type": "TOP"},
    {"name": "LSYSTUXX", "type": "UXX"}
  ]
}
```

**Key Insight:** GetFunctionGroup returns **metadata** about the function group structure, not just source code.

#### GetPackage
**Returns:** JSON structure (package metadata + object list)
```json
{
  "name": "$TMP",
  "description": "Local objects",
  "responsible": "SAP",
  "superPackage": "",
  "packageType": "development",
  "contents": [
    {"type": "PROG", "name": "ZTEST", "description": "Test program"},
    {"type": "CLAS", "name": "ZCL_TEST", "description": "Test class"}
  ]
}
```

**Key Insight:** GetPackage returns **container metadata**, not source code at all.

---

### 2.3 Recommendation: KEEP SEPARATE

#### Rationale

| Tool | Returns | Purpose | Unify into GetSource? |
|------|---------|---------|----------------------|
| GetProgram | Source code | Read ABAP | ✅ YES |
| GetClass | Source code | Read ABAP | ✅ YES |
| GetInterface | Source code | Read ABAP | ✅ YES |
| GetInclude | Source code | Read ABAP | ✅ YES |
| GetFunction | Source code | Read ABAP | ✅ YES |
| **GetFunctionGroup** | JSON metadata | Structure navigation | ⚠️ **MAYBE** |
| **GetPackage** | JSON metadata | Container discovery | ❌ **NO** |

#### Updated Recommendation

**Option 1: Strict Separation (RECOMMENDED)**
```
GetSource(type, name, [parent], [include]) - unified for source code only
  type: PROG | CLAS | INTF | FUNC | INCL

GetFunctionGroup(name) - separate (returns metadata structure)
GetPackage(name) - separate (returns container metadata)
```

**Pros:**
- Clear distinction: source vs metadata
- GetSource always returns plain text
- GetFunctionGroup/GetPackage always return JSON
- No conditional return types

**Option 2: Hybrid with "include_metadata" flag**
```
GetSource(type, name, [parent], [include], [include_metadata])
  type: PROG | CLAS | INTF | FUNC | INCL | FUGR | DEVC
  include_metadata: false → returns source only
                    true → returns JSON with metadata

GetPackage(name) - still separate (too different)
```

**Pros:**
- Single tool for all object types
- Optional metadata for FUGR

**Cons:**
- Conditional return type (text or JSON depending on flags)
- More complex implementation
- Confusing for AI agents (what type is returned?)

---

### 2.4 Final Decision for Focused Mode

**Update Focused Mode Toolset:**

```diff
📖 READ (unified)
-  • GetSource(type, name, [parent], [include])
-    └─ type: PROG|CLAS|INTF|FUNC|FUGR|INCL
+  • GetSource(type, name, [parent], [include])
+    └─ type: PROG|CLAS|INTF|FUNC|INCL
+  • GetFunctionGroup(name) - returns metadata + FM list
   • GetTable(name) - separate (different structure)
+  • GetPackage(name) - returns package contents
   • QueryData(sql | table_name) - merged
```

**Total:** 4 read tools (was 3)

**Justification:**
1. **GetSource** - Unified for "pure source code" objects
2. **GetFunctionGroup** - Separate (returns metadata, not just source)
3. **GetPackage** - Separate (container discovery, not code)
4. **GetTable** - Separate (returns structure, not code)
5. **QueryData** - Separate (returns data, not code)

---

### 2.5 Alternative: "Get" vs "Describe" Distinction

**Another way to think about it:**

```
"Get" tools - Return source code (text)
  • GetSource(type, name, ...)
    - GetProgram → GetSource(PROG, name)
    - GetClass → GetSource(CLAS, name)
    - GetInterface → GetSource(INTF, name)
    - GetFunction → GetSource(FUNC, name, parent)
    - GetInclude → GetSource(INCL, name)

"Describe" tools - Return metadata (JSON)
  • DescribeFunctionGroup(name) - Returns FM list + includes
  • DescribePackage(name) - Returns object list
  • DescribeTable(name) - Returns field structure
```

**Pros:**
- Very clear naming convention
- Predictable return types
- Easy for AI agents to understand

**Cons:**
- Breaking change (all tools renamed)
- More verbose

---

## Part 3: Updated Focused Mode Toolset

### 3.1 Final Recommendation

**13 tools → 14 tools (add GetCDSDependencies)**

```
┌─────────────────────────────────────────────┐
│         FOCUSED MODE (14 tools)             │
├─────────────────────────────────────────────┤
│                                             │
│  🔍 SEARCH (mandatory foundation)          │
│    • GrepObject                             │
│    • GrepPackage                            │
│    • SearchObject                           │
│                                             │
│  📖 READ (unified + specialized)           │
│    • GetSource(type, name, [parent], [include])│
│      └─ type: PROG|CLAS|INTF|FUNC|INCL    │
│    • GetFunctionGroup(name) ⭐ KEEP        │
│    • GetPackage(name) ⭐ KEEP              │
│    • GetTable(name)                         │
│    • GetCDSDependencies(ddls) 🆕 NEW       │
│    • QueryData(sql | table_name)           │
│                                             │
│  ✏️ EDIT (surgical, primary)               │
│    • EditSource ⭐ PRIMARY                  │
│                                             │
│  📝 WRITE (full replacement, fallback)     │
│    • WriteSource(type, name, source, mode) │
│                                             │
│  🧭 NAVIGATE                                │
│    • FindDefinition                         │
│    • FindReferences                         │
│                                             │
│  ⚡ EXECUTE                                 │
│    • RunUnitTests                           │
│    • SyntaxCheck                            │
│                                             │
│  🔒 ADVANCED (edge cases)                  │
│    • LockObject                             │
│    • UnlockObject                           │
│                                             │
└─────────────────────────────────────────────┘

Total: 14 tools (was 13)
```

### 3.2 Tool Categories Summary

| Category | Tools | Rationale |
|----------|-------|-----------|
| **Search** | 3 | Pattern discovery foundation |
| **Read** | 6 | Mix of unified (GetSource) + specialized (metadata tools) |
| **Edit** | 1 | Surgical editing (primary workflow) |
| **Write** | 1 | Full replacement (fallback) |
| **Navigate** | 2 | Code intelligence |
| **Execute** | 2 | Testing & validation |
| **Advanced** | 2 | Manual lock control |

---

## Part 4: Implementation Roadmap

### 4.1 Phase 1: CDS Dependencies (Week 1)

**Tasks:**
1. ✅ Verify endpoint with `/IWFND/GW_CLIENT` (1 day)
2. ✅ Implement `pkg/adt/cds.go` (1 day)
3. ✅ Add MCP tool + handler (4 hours)
4. ✅ Integration test (4 hours)
5. ✅ Documentation (2 hours)

**Deliverable:** `GetCDSDependencies` tool in v1.6.0

---

### 4.2 Phase 2: Focused Mode Refinement (Week 1-2)

**Tasks:**
1. Update focused mode proposal with FUGR/PACKAGE analysis
2. Keep GetFunctionGroup and GetPackage as separate tools
3. Update tool count: 13 → 14 (added GetCDSDependencies)
4. Document rationale for separation

**Deliverable:** Updated focused mode specification

---

### 4.3 Phase 3: Tool Unification (Week 2-3)

**Tasks:**
1. Implement `GetSource(type, name, [parent], [include])`
2. Keep GetFunctionGroup, GetPackage separate
3. Implement `WriteSource(type, name, source, mode, opts)`
4. Implement `QueryData(sql | table_name)`
5. Add mode filtering

**Deliverable:** Unified tools + focused mode implementation

---

## Part 5: Open Questions

### 5.1 CDS Dependency Tool

1. ❓ Should we add graph visualization (Mermaid diagram generation)?
2. ❓ Should we cache dependency trees for performance?
3. ❓ Should we support reverse dependencies (where is this view used)?

### 5.2 Object Type Unification

1. ❓ Should we rename to Get/Describe pattern for clarity?
2. ❓ Should GetFunction be separate or unified into GetSource?
   - Current plan: Unified (GetSource(FUNC, name, parent))
3. ❓ Should we add GetFunctionGroupSource(name, fm_name) for specific FM source?

---

## Conclusion

### CDS Dependencies: ✅ READY TO IMPLEMENT

- REST API exists and verified
- Clear use cases and value proposition
- 2-3 days implementation time
- High value for S/4HANA development

### FUGR/PACKAGE Separation: ✅ KEEP SEPARATE

**Key Insight:** They return metadata/structure, not source code.

**Decision:**
- ✅ GetSource - Unified for pure source code (PROG, CLAS, INTF, FUNC, INCL)
- ✅ GetFunctionGroup - Separate (returns FM list + structure)
- ✅ GetPackage - Separate (returns object list + metadata)
- ✅ GetTable - Separate (returns field structure)

**Rationale:** Clear distinction between source code vs metadata tools.

### Updated Focused Mode: 14 Tools

- Added: GetCDSDependencies (CDS dependency analyzer)
- Kept separate: GetFunctionGroup, GetPackage (metadata, not source)
- Total: 14 tools (still 67% reduction from 42)

**Next Steps:**
1. Implement GetCDSDependencies (Week 1)
2. Update focused mode proposal with FUGR/PACKAGE analysis
3. Begin tool unification (GetSource, WriteSource) in v1.6.0

---

**Document Version:** 1.0
**Date:** 2025-12-04
**Status:** Brainstorming Complete - Ready for Review
**Related:** Focused Mode v1.6.0 implementation
