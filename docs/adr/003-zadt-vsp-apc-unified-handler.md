# ADR-003: ZADT-VSP Unified APC Handler

**Date:** 2025-12-18
**Status:** PROPOSAL / DESIGN DRAFT
**Context:** Unified WebSocket handler for stateful SAP operations

---

> **NOT VANILLA ADT - REQUIRES CUSTOM SAP DEVELOPMENT**
>
> This ADR proposes deploying custom ABAP objects (Z*) to the SAP system.
> This is an **optional extension** for users who need capabilities beyond
> standard ADT REST APIs.

---

## Summary

A single WebSocket/APC handler (`ZADT_VSP_APC`) that provides a unified entry point for all stateful operations that cannot be achieved via standard ADT REST APIs:

- **Debugging** - Stateful debug sessions (attach, step, inspect)
- **RCA/ANST** - SAP Note search and fix assessment
- **RFC Gateway** - Call arbitrary RFCs/BAPIs
- **Long-running ops** - Background jobs, mass operations
- **Event streaming** - Real-time notifications

## Motivation

### The Vanilla ADT Gap

Standard ADT REST APIs are stateless. Each HTTP request is independent. This works for most operations but fails for:

| Operation | Why Stateless Fails |
|-----------|---------------------|
| Debugging | Session must persist between step/inspect calls |
| ANST search | Long-running, requires SAP Support Portal auth |
| RFC calls | Not exposed via ADT at all |
| Batch operations | Need progress tracking, cancellation |

### One Handler, Many Capabilities

Instead of building multiple custom endpoints, one unified APC handler can:
- Share authentication/session management
- Provide consistent message protocol
- Enable future extensibility
- Reduce SAP-side footprint

## Architecture

```
┌──────────────────────────────────────────────────────────────────────────┐
│                            vsp MCP Server                                 │
│  ┌────────────────────────────────────────────────────────────────────┐  │
│  │                    WebSocket Client Manager                         │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐   │  │
│  │  │   Debug     │ │    RCA      │ │    RFC      │ │   Events    │   │  │
│  │  │  Handler    │ │  Handler    │ │  Handler    │ │  Handler    │   │  │
│  │  └──────┬──────┘ └──────┬──────┘ └──────┬──────┘ └──────┬──────┘   │  │
│  │         │               │               │               │          │  │
│  │         └───────────────┴───────────────┴───────────────┘          │  │
│  │                                │                                    │  │
│  │                    ┌───────────▼───────────┐                       │  │
│  │                    │  Message Multiplexer  │                       │  │
│  │                    └───────────┬───────────┘                       │  │
│  └────────────────────────────────│────────────────────────────────────┘  │
└───────────────────────────────────│───────────────────────────────────────┘
                                    │ WebSocket (wss://)
                                    │
┌───────────────────────────────────│───────────────────────────────────────┐
│                          SAP System                                        │
│  ┌────────────────────────────────▼────────────────────────────────────┐  │
│  │                      ZADT_VSP_APC                                    │  │
│  │                   (ICF WebSocket Service)                            │  │
│  │  ┌──────────────────────────────────────────────────────────────┐   │  │
│  │  │              ZCL_VSP_APC_HANDLER                              │   │  │
│  │  │              (IF_APC_WS_EXTENSION)                            │   │  │
│  │  │  ┌────────────────────────────────────────────────────────┐  │   │  │
│  │  │  │              Message Router                             │  │   │  │
│  │  │  │  domain: "debug" → ZCL_VSP_DEBUG_SERVICE                │  │   │  │
│  │  │  │  domain: "rca"   → ZCL_VSP_RCA_SERVICE                  │  │   │  │
│  │  │  │  domain: "rfc"   → ZCL_VSP_RFC_SERVICE                  │  │   │  │
│  │  │  │  domain: "event" → ZCL_VSP_EVENT_SERVICE                │  │   │  │
│  │  │  └────────────────────────────────────────────────────────┘  │   │  │
│  │  └──────────────────────────────────────────────────────────────┘   │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                                                            │
│  ┌────────────────┐ ┌────────────────┐ ┌────────────────┐                 │
│  │ ADT Debugger   │ │ ANST/SNOTE     │ │ RFC/BAPI       │                 │
│  │ APIs           │ │ Functions      │ │ Gateway        │                 │
│  └────────────────┘ └────────────────┘ └────────────────┘                 │
└────────────────────────────────────────────────────────────────────────────┘
```

## Message Protocol

### Envelope Format

All messages share a common envelope:

```json
{
  "id": "uuid-v4",           // Correlation ID
  "domain": "debug|rca|rfc|event",
  "action": "string",        // Domain-specific action
  "params": {},              // Action parameters
  "timeout": 30000           // Optional timeout in ms
}
```

### Response Format

```json
{
  "id": "uuid-v4",           // Correlation ID (matches request)
  "success": true|false,
  "data": {},                // Action-specific response
  "error": {                 // Only if success=false
    "code": "ERROR_CODE",
    "message": "Human readable",
    "details": {}
  }
}
```

### Push Events (Server → Client)

```json
{
  "event": "string",         // Event type
  "domain": "debug|rca|event",
  "data": {}                 // Event-specific data
}
```

## Domain: Debug

### Actions

| Action | Description | Params |
|--------|-------------|--------|
| `listen` | Start listening for debuggee | `user`, `timeout` |
| `attach` | Attach to caught debuggee | `debuggeeId` |
| `step` | Execute step operation | `type`: into/over/return/continue |
| `getStack` | Get call stack | - |
| `getVariables` | Get variables | `scope`, `ids[]` |
| `setBreakpoint` | Set breakpoint | `uri`, `line`, `condition` |
| `detach` | Release debuggee | - |

### Events

| Event | Description |
|-------|-------------|
| `debuggeeCaught` | Debuggee hit breakpoint |
| `breakpointHit` | Stopped at breakpoint |
| `sessionEnded` | Debug session terminated |

### Example Flow

```
Client                              Server
   │                                   │
   │─── {domain:"debug",              │
   │     action:"listen",             │
   │     params:{user:"DEV"}} ───────▶│
   │                                   │ (long-poll internally)
   │◀── {event:"debuggeeCaught",      │
   │     data:{debuggeeId:"123"}} ────│
   │                                   │
   │─── {action:"attach",             │
   │     params:{debuggeeId:"123"}} ──▶│
   │◀── {success:true, data:{...}} ───│
   │                                   │
   │─── {action:"step",               │
   │     params:{type:"over"}} ───────▶│
   │◀── {success:true, data:{         │
   │      line:42, vars:{...}}} ──────│
```

## Domain: RCA (Root Cause Analysis)

### Actions

| Action | Description | Params |
|--------|-------------|--------|
| `analyzeDump` | Parse dump, extract symptoms | `dumpId` |
| `searchNotes` | Query ANST | `symptoms[]`, `component` |
| `getNoteDetails` | Get full note content | `noteNumber` |
| `checkApplicability` | Can note be applied? | `noteNumber` |
| `getPrerequisites` | What's needed first? | `noteNumber` |

### Events

| Event | Description |
|-------|-------------|
| `searchProgress` | ANST search progress |
| `noteFound` | Relevant note discovered |

## Domain: RFC

### Actions

| Action | Description | Params |
|--------|-------------|--------|
| `call` | Call RFC/BAPI | `function`, `imports`, `tables` |
| `getMetadata` | Get function signature | `function` |
| `search` | Find functions | `pattern` |

### Example: Call RFC

```json
// Request
{
  "id": "abc123",
  "domain": "rfc",
  "action": "call",
  "params": {
    "function": "BAPI_USER_GET_DETAIL",
    "imports": {
      "USERNAME": "DEVELOPER"
    }
  }
}

// Response
{
  "id": "abc123",
  "success": true,
  "data": {
    "exports": {
      "ADDRESS": {...},
      "LOGONDATA": {...}
    },
    "tables": {
      "ACTIVITYGROUPS": [...]
    }
  }
}
```

## Domain: Events

### Actions

| Action | Description | Params |
|--------|-------------|--------|
| `subscribe` | Subscribe to events | `types[]` |
| `unsubscribe` | Unsubscribe | `types[]` |

### Event Types

| Type | Description |
|------|-------------|
| `dump.new` | New short dump occurred |
| `transport.released` | Transport released |
| `object.changed` | Object modified |
| `job.completed` | Background job finished |

## SAP Objects

### Package Structure

```
$ZADT_VSP                        " Root package
├── $ZADT_VSP_CORE              " Core handler
│   ├── ZCL_VSP_APC_HANDLER     " IF_APC_WS_EXTENSION
│   ├── ZCL_VSP_MESSAGE_ROUTER  " Domain routing
│   ├── ZCL_VSP_SESSION         " Session management
│   └── ZIF_VSP_SERVICE         " Service interface
│
├── $ZADT_VSP_DEBUG             " Debug domain
│   ├── ZCL_VSP_DEBUG_SERVICE   " ZIF_VSP_SERVICE impl
│   └── ZCL_VSP_DEBUG_SESSION   " Debug state
│
├── $ZADT_VSP_RCA               " RCA domain
│   ├── ZCL_VSP_RCA_SERVICE     " ZIF_VSP_SERVICE impl
│   ├── ZCL_VSP_ANST_WRAPPER    " ANST function calls
│   └── ZCL_VSP_DUMP_PARSER     " Structured parsing
│
├── $ZADT_VSP_RFC               " RFC domain
│   ├── ZCL_VSP_RFC_SERVICE     " ZIF_VSP_SERVICE impl
│   └── ZCL_VSP_RFC_EXECUTOR    " Dynamic RFC calls
│
└── $ZADT_VSP_EVENT             " Event domain
    ├── ZCL_VSP_EVENT_SERVICE   " ZIF_VSP_SERVICE impl
    └── ZCL_VSP_EVENT_BROKER    " Event distribution
```

### Service Interface

```abap
INTERFACE zif_vsp_service PUBLIC.
  METHODS:
    get_domain
      RETURNING VALUE(rv_domain) TYPE string,

    handle_message
      IMPORTING is_message TYPE zst_vsp_message
      RETURNING VALUE(rs_response) TYPE zst_vsp_response,

    handle_disconnect
      IMPORTING iv_session_id TYPE string.
ENDINTERFACE.
```

### APC Handler

```abap
CLASS zcl_vsp_apc_handler DEFINITION PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apc_wsp_ext_stateful.

  PRIVATE SECTION.
    DATA: mo_router  TYPE REF TO zcl_vsp_message_router,
          mo_session TYPE REF TO zcl_vsp_session.

ENDCLASS.

CLASS zcl_vsp_apc_handler IMPLEMENTATION.

  METHOD if_apc_wsp_ext_stateful~on_start.
    " Initialize session
    mo_session = NEW zcl_vsp_session( io_context = i_context ).
    mo_router = NEW zcl_vsp_message_router( ).

    " Register domain services
    mo_router->register( NEW zcl_vsp_debug_service( mo_session ) ).
    mo_router->register( NEW zcl_vsp_rca_service( mo_session ) ).
    mo_router->register( NEW zcl_vsp_rfc_service( mo_session ) ).
    mo_router->register( NEW zcl_vsp_event_service( mo_session ) ).
  ENDMETHOD.

  METHOD if_apc_wsp_ext_stateful~on_message.
    DATA(ls_message) = parse_message( i_message ).
    DATA(ls_response) = mo_router->route( ls_message ).
    send_response( ls_response ).
  ENDMETHOD.

  METHOD if_apc_wsp_ext_stateful~on_close.
    mo_router->disconnect( mo_session->get_id( ) ).
  ENDMETHOD.

ENDCLASS.
```

## vsp Integration

### New Configuration

```bash
# Enable WebSocket connection to ZADT_VSP_APC
SAP_VSP_WS_ENABLED=true
SAP_VSP_WS_PATH=/sap/bc/apc/zadt_vsp

# Or via CLI
./vsp --vsp-websocket --vsp-ws-path /sap/bc/apc/zadt_vsp
```

### Feature Detection

```go
// In GetFeatures, detect APC availability
func (c *Client) GetFeatures(ctx context.Context) (*Features, error) {
    // ... existing checks ...

    // Check for ZADT_VSP_APC
    features.VSPWebSocket = c.checkVSPAPC(ctx)

    return features, nil
}
```

### Graceful Degradation

```go
func (s *Server) handleDebugStep(ctx context.Context, ...) {
    if s.vspWebSocket != nil && s.vspWebSocket.Connected() {
        // Use WebSocket - full functionality
        return s.vspWebSocket.DebugStep(ctx, params)
    }
    // Fall back to HTTP - limited functionality
    return s.httpDebugStep(ctx, params)
}
```

## Security Considerations

1. **Authentication**
   - WebSocket inherits ICF authentication
   - Session bound to authenticated user
   - All actions logged with user context

2. **Authorization**
   - Each domain checks authorizations
   - RFC domain validates S_RFC
   - Debug domain validates S_DEBUG
   - RCA domain validates ANST access

3. **Input Validation**
   - All messages validated against schema
   - RFC parameters sanitized
   - No SQL injection possible (parameterized)

4. **Rate Limiting**
   - Configurable limits per session
   - Prevents resource exhaustion

## Deployment

### Prerequisites

1. SAP NetWeaver 7.40+ (APC support)
2. SICF service activation
3. Authorization setup

### Installation Steps

1. Import transport with ZADT_VSP objects
2. Activate ICF service `/sap/bc/apc/zadt_vsp`
3. Configure authorizations
4. Test with vsp `--vsp-websocket`

### Rollback

Simply deactivate ICF service - vsp falls back to HTTP automatically.

## Alternatives Considered

### Multiple APC Handlers

- **Pro:** Smaller, focused handlers
- **Con:** Multiple connections, auth overhead, harder to manage

### OData for Stateful Ops

- **Pro:** More standard
- **Con:** OData not designed for real-time bidirectional

### gRPC

- **Pro:** Efficient binary protocol
- **Con:** Not native to SAP, requires additional infrastructure

## Decision

Implement a single unified APC handler that can be incrementally extended with new domains. Start with Debug domain, add RCA and RFC as needed.

## Status

**PROPOSAL** - Awaiting decision on whether to pursue non-vanilla approach.

## Related Documents

- ADR-001: WebSocket/APC for Stateful Debugging
- ADR-002: RCA Tooling Architecture
- Report: 2025-12-18-001 AI-Assisted RCA & ANST Integration
