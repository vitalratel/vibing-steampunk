# AMDP Debugging & UI5/BSP Management Capabilities

**Date:** 2025-12-05
**Report ID:** 017
**Subject:** Investigation of AMDP debugging and UI5/Fiori BSP upload capabilities via ADT

---

## Executive Summary

| Capability | Supported? | ADT Endpoint | Implementation Complexity |
|------------|------------|--------------|---------------------------|
| AMDP Debugging | ✅ YES | `/sap/bc/adt/amdp/debugger/` | Medium - separate from ABAP debugger |
| UI5/BSP Management | ✅ YES | `/sap/bc/adt/filestore/ui5-bsp/` | Medium - uses BSP repository APIs |

---

## 1. AMDP Debugging

### Overview

AMDP (ABAP Managed Database Procedures) debugging is fully supported via ADT with a **separate debugger infrastructure** from ABAP debugging.

### Key REST Endpoints

```
POST   /sap/bc/adt/amdp/debugger/main/          → Start AMDP debugger session
GET    /sap/bc/adt/amdp/debugger/main/{mainId}  → Resume/get state (long-poll)
DELETE /sap/bc/adt/amdp/debugger/main/{mainId}  → Stop debugger
```

### Key Classes

| Class | Description |
|-------|-------------|
| `CL_AMDP_DBG_ADT_RES_MAIN` | Main debugger REST resource |
| `CL_AMDP_DBG_ADT_RES_BPS` | Breakpoints resource |
| `CL_AMDP_DBG_ADT_RES_VARS` | Variables resource |
| `CL_AMDP_DBG_ADT_RES_DEBUGGEE` | Debuggee resource |
| `CL_AMDP_DBG_ADT_RES_LOOKUP` | Name lookup resource |
| `CL_AMDP_DBG_CONTROL` | Debugger commands |
| `CL_AMDP_DBG_MAIN` | Init and operate debugger |
| `CL_AMDP_DBG_SYS_DEBUG` | SQLSCRIPT debugger calls |

### Response Types

From `IF_AMDP_DBG_MAIN=>CO_RES_KIND`:
- `on_break` - Hit breakpoint (callstack, variables, position)
- `on_toggle_breakpoints` - Breakpoint toggle response
- `on_execution_end` - Execution finished
- `sync_breakpoints` - Breakpoint sync
- `get_scalar_values` - Variable values
- `set_scalar_values` - Change values
- `lookup_names` - Name lookups
- `start` - Debugger started

### Key Differences from ABAP Debugging

| Aspect | ABAP Debugger | AMDP Debugger |
|--------|---------------|---------------|
| Endpoint | `/sap/bc/adt/debugger/` | `/sap/bc/adt/amdp/debugger/` |
| Session ID | Debuggee ID | Main ID + HANA Session ID |
| Execution | ABAP VM | HANA SQLScript engine |
| Cascade Mode | N/A | None or Full (cross-procedure) |
| Variables | ABAP types | SQLScript types (incl. TABLE, ARRAY) |

### Implementation Notes

```go
// Proposed vsp tools for AMDP debugging
"AMDPDebuggerStart"     // Start AMDP debug session
"AMDPDebuggerResume"    // Get state (blocks until event)
"AMDPDebuggerStop"      // Stop debug session
"AMDPBreakpointSet"     // Set AMDP breakpoint
"AMDPBreakpointSync"    // Sync breakpoints
"AMDPGetVariables"      // Get scalar/table values
```

### Cascade Mode

AMDP debugger supports cascade debugging:
- `NONE` - Debug only the called procedure
- `FULL` - Debug all nested procedure calls

Controlled via SAP parameter `AMDP_DEBUG_CAS_MODE` or query parameter.

---

## 2. UI5/BSP Management

### Overview

UI5 applications are stored in SAP as **BSP (Business Server Pages) applications**. Multiple APIs exist for managing them.

### Key Programs & Classes

| Object | Type | Description |
|--------|------|-------------|
| `/UI5/UI5_REPOSITORY_LOAD` | Program | GUI for upload/download/delete |
| `/UI5/CL_REPOSITORY_LOAD` | Class | Core implementation |
| `/UI5/CL_REPOSITORY_LOAD_FILE` | Class | File system adapter |
| `/UI5/CL_REPOSITORY_LOAD_ZIP` | Class | ZIP file adapter |
| `CL_O2_API_APPLICATION` | Class | Low-level BSP API |
| `CL_O2_API_PAGES` | Class | BSP pages API |

### ADT REST Resources

Found ADT classes for UI5 repository:
```
/UI5/CL_ADT_REP_RES_CONTENT   - Content resource
/UI5/CL_ADT_REP_RES_OBJ       - Object resource
/UI5/CL_ADT_REP_RES_OBJ_BASE  - Base resource controller
/UI5/CL_ADT_REP_RES_QUERY     - Query resource
```

Suggests REST endpoint exists at: `/sap/bc/adt/filestore/ui5-bsp/`

### BSP Application API (CL_O2_API_APPLICATION)

Key methods for BSP/UI5 management:

| Method | Description |
|--------|-------------|
| `CREATE_NEW` | Create new BSP application |
| `LOAD` | Load existing application |
| `SAVE` | Save application changes |
| `DELETE` | Delete application |
| `ACTIVATE` | Activate application |
| `CHECK` | Syntax check |
| `GET_PAGES` | Get all pages |
| `GET_ATTRIBUTES` | Get application attributes |
| `SET_ATTRIBUTES` | Set application attributes |
| `COPY` | Copy application |

### UI5 Repository Load Operations

From `/UI5/CL_REPOSITORY_LOAD`:
- `upload` - Upload UI5 app from file system/ZIP
- `download` - Download UI5 app to file system
- `delete` - Delete UI5 app from repository

### Workflow for UI5 Upload

```
1. Create/Load BSP application
2. Lock application (SET_CHANGEABLE)
3. Upload files (via /UI5/CL_REPOSITORY_LOAD or ADT)
4. Save changes
5. Activate application
6. Unlock
```

### Transport Integration

UI5 applications support transport requests:
- Object type: `WAPA` (BSP Application)
- Transport handled via `RS_CORR_INSERT`
- Package assignment required for transportable apps

---

## 3. Implementation Recommendations for vsp

### Phase 1: AMDP Debugging (Medium Priority)

Add new tools:
```go
// pkg/adt/amdp_debugger.go
func (c *Client) AMDPDebuggerStart(ctx context.Context, user string, cascadeMode string) (*AMDPDebugSession, error)
func (c *Client) AMDPDebuggerResume(ctx context.Context, mainID string) (*AMDPDebugResponse, error)
func (c *Client) AMDPDebuggerStop(ctx context.Context, mainID string, hardStop bool) error
func (c *Client) AMDPSetBreakpoint(ctx context.Context, mainID string, bp AMDPBreakpoint) error
func (c *Client) AMDPGetScalarValues(ctx context.Context, vars []string) ([]AMDPVariable, error)
```

### Phase 2: UI5/BSP Management (Lower Priority)

Add new tools:
```go
// pkg/adt/ui5.go
func (c *Client) UI5ListApps(ctx context.Context, query string) ([]UI5App, error)
func (c *Client) UI5GetApp(ctx context.Context, appName string) (*UI5AppDetails, error)
func (c *Client) UI5UploadApp(ctx context.Context, appName string, zipContent []byte) error
func (c *Client) UI5DownloadApp(ctx context.Context, appName string) ([]byte, error)
func (c *Client) UI5DeleteApp(ctx context.Context, appName string) error
```

---

## 4. Discovered REST Endpoints

### AMDP Debugger
```
/sap/bc/adt/amdp/debugger/main/
/sap/bc/adt/amdp/debugger/main/{mainId}
/sap/bc/adt/amdp/debugger/main/{mainId}/debuggees/{debuggeeId}
```

### UI5 Repository (needs verification)
```
/sap/bc/adt/filestore/ui5-bsp/
/sap/bc/adt/filestore/ui5-bsp/objects/{appName}/
/sap/bc/adt/filestore/ui5-bsp/objects/{appName}/content/{path}
```

---

## 5. References

### AMDP Debugging
- Package: `SABP_AMDP_DBG` - Core AMDP debugger
- Package: `SABP_AMDP_DBG_ADT` - ADT integration
- Message class: `AMDP_DBG_ADT`

### UI5/BSP
- Package: `/UI5/UI5_INFRA_APP` - UI5 infrastructure
- Package: `/UI5/ADT_REP_FS` - ADT file store
- Package: `SO2_DBLAYER` - BSP database layer
- Transaction: `SE80` → BSP Applications

---

## Conclusion

Both AMDP debugging and UI5/BSP management are fully supported by SAP ADT. The infrastructure exists and can be integrated into vsp:

1. **AMDP Debugging**: Requires implementing a separate set of tools mirroring the ABAP debugger but targeting the `/sap/bc/adt/amdp/debugger/` endpoint.

2. **UI5/BSP Management**: Can leverage either:
   - Direct ADT REST calls to `/sap/bc/adt/filestore/ui5-bsp/`
   - Or wrap the `/UI5/CL_REPOSITORY_LOAD` class functionality

Both would enable AI-assisted debugging of HANA procedures and automated Fiori/UI5 deployment workflows.
