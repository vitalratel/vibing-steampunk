# WebSocket Debugging Deep Dive

**Date:** 2025-12-19
**Report ID:** 001
**Subject:** ZADT-VSP WebSocket Debugging - Architecture, Integration, and RCA Workflows
**Version:** 2.0.0

---

## Executive Summary

The ZADT-VSP WebSocket handler provides **stateful ABAP debugging capabilities** that are impossible to achieve through standard ADT REST APIs. By integrating with SAP's TPDAPI (Test and Performance Development API), we enable AI-driven debugging workflows including:

- **Real-time code stepping** (step-into, step-over, step-return, continue)
- **Call stack inspection** during execution
- **Variable inspection** at breakpoints
- **Integration with runtime error analysis** (dumps/RABAX)
- **Unit test-triggered debugging** for reproducible scenarios

This creates a powerful **Root Cause Analysis (RCA)** toolkit where an AI agent can:
1. Analyze runtime errors from dumps
2. Set breakpoints at failure points
3. Trigger reproduction via unit tests
4. Step through code to understand the issue
5. Inspect variables at the point of failure

---

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        vsp MCP Server                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
│  │ ADT REST    │  │ WebSocket   │  │ Analysis Tools          │ │
│  │ Tools       │  │ Client      │  │ (GetDumps, RunUnitTests)│ │
│  └──────┬──────┘  └──────┬──────┘  └───────────┬─────────────┘ │
└─────────┼────────────────┼─────────────────────┼───────────────┘
          │                │                     │
          ▼                ▼                     ▼
┌─────────────────────────────────────────────────────────────────┐
│                      SAP ABAP System                            │
│  ┌─────────────┐  ┌─────────────────────────────────────────┐  │
│  │ ADT REST    │  │ ZADT_VSP APC Handler                    │  │
│  │ Endpoints   │  │  ┌─────────────┐  ┌──────────────────┐  │  │
│  │             │  │  │ RFC Domain  │  │ Debug Domain     │  │  │
│  │ /sap/bc/adt │  │  │ (BAPI/FM)   │  │ (TPDAPI)         │  │  │
│  └─────────────┘  │  └─────────────┘  └──────────────────┘  │  │
│                   └─────────────────────────────────────────┘  │
│                              │                                  │
│                              ▼                                  │
│                   ┌─────────────────────┐                      │
│                   │ CL_TPDAPI_SERVICE   │                      │
│                   │ CL_TPDAPI_SESSION   │                      │
│                   │ (SAP Debugger API)  │                      │
│                   └─────────────────────┘                      │
└─────────────────────────────────────────────────────────────────┘
```

### Why WebSocket?

| Aspect | HTTP REST | WebSocket |
|--------|-----------|-----------|
| **State** | Stateless | Stateful session |
| **Debug Session** | Cannot maintain | Persistent across messages |
| **TPDAPI Access** | Not possible | Full access via session |
| **Blocking Operations** | Timeout issues | Native support |
| **Real-time Updates** | Polling required | Push notifications |

The SAP debugger (TPDAPI) requires a **stateful session** to:
- Maintain debugger context between step operations
- Keep the debuggee process attached
- Preserve breakpoint state
- Allow sequential step-into/step-over operations

HTTP REST is fundamentally stateless - each request creates a new session. WebSocket maintains a persistent connection, allowing the ABAP handler to keep TPDAPI session objects alive.

---

## Debug Domain API Reference

### Connection

```
WebSocket URL: ws://<host>:<port>/sap/bc/apc/sap/zadt_vsp?sap-client=<client>
Authorization: Basic <base64(user:password)>
```

### Message Format

**Request:**
```json
{
  "id": "unique-request-id",
  "domain": "debug",
  "action": "actionName",
  "params": { ... }
}
```

**Response:**
```json
{
  "id": "unique-request-id",
  "success": true,
  "data": { ... }
}
```

### Actions

#### 1. getStatus
Get current debug session status.

```json
// Request
{"id":"1","domain":"debug","action":"getStatus"}

// Response
{
  "id": "1",
  "success": true,
  "data": {
    "sessionId": "0242AC1100111FE0B79EE82B2934B8A2",
    "user": "DEVELOPER",
    "breakpointCount": 0,
    "attached": false,
    "attachedDebuggee": "",
    "listenerActive": false,
    "debuggingAvailable": true
  }
}
```

#### 2. listen
Start debug listener and wait for a debuggee to hit a breakpoint.

**Parameters:**
- `timeout` (int): Seconds to wait (default: 60, max: 240)
- `user` (string): User to debug (default: current user)

```json
// Request
{"id":"2","domain":"debug","action":"listen","params":{"timeout":120}}

// Response (timeout)
{
  "id": "2",
  "success": true,
  "data": {
    "status": "timeout",
    "debuggees": []
  }
}

// Response (caught debuggee)
{
  "id": "2",
  "success": true,
  "data": {
    "status": "caught",
    "debuggees": [
      {
        "id": "ABC123DEF456...",
        "host": "sapserver_APP_00",
        "user": "DEVELOPER",
        "program": "ZCL_MY_CLASS================CP",
        "sameServer": true
      }
    ]
  }
}
```

#### 3. getDebuggees
Get list of waiting debuggees without blocking.

```json
// Request
{"id":"3","domain":"debug","action":"getDebuggees"}

// Response
{
  "id": "3",
  "success": true,
  "data": {
    "debuggees": [
      {
        "id": "ABC123DEF456...",
        "host": "sapserver_APP_00",
        "user": "DEVELOPER",
        "program": "ZTEST_PROGRAM"
      }
    ]
  }
}
```

#### 4. attach
Attach to a waiting debuggee.

**Parameters:**
- `debuggeeId` (string): ID from listen/getDebuggees response

```json
// Request
{"id":"4","domain":"debug","action":"attach","params":{"debuggeeId":"ABC123DEF456..."}}

// Response
{
  "id": "4",
  "success": true,
  "data": {
    "attached": true,
    "debuggeeId": "ABC123DEF456...",
    "program": "ZCL_MY_CLASS================CP",
    "include": "ZCL_MY_CLASS================CCIMP",
    "line": 42
  }
}
```

#### 5. step
Execute a step operation.

**Parameters:**
- `type` (string): `into`, `over`, `return`, or `continue`

```json
// Request
{"id":"5","domain":"debug","action":"step","params":{"type":"over"}}

// Response
{
  "id": "5",
  "success": true,
  "data": {
    "stepped": "over",
    "program": "ZCL_MY_CLASS================CP",
    "include": "ZCL_MY_CLASS================CCIMP",
    "line": 43,
    "procedure": "IF_MY_INTERFACE~PROCESS"
  }
}

// Response (program ended)
{
  "id": "5",
  "success": true,
  "data": {
    "stepped": "continue",
    "ended": true,
    "message": "Debuggee ended"
  }
}
```

#### 6. getStack
Get current call stack.

```json
// Request
{"id":"6","domain":"debug","action":"getStack"}

// Response
{
  "id": "6",
  "success": true,
  "data": {
    "stack": [
      {
        "index": 0,
        "program": "ZCL_MY_CLASS================CP",
        "include": "ZCL_MY_CLASS================CCIMP",
        "line": 43,
        "procedure": "IF_MY_INTERFACE~PROCESS",
        "active": true,
        "system": false
      },
      {
        "index": 1,
        "program": "ZCL_CALLER================CP",
        "include": "ZCL_CALLER================CCIMP",
        "line": 15,
        "procedure": "EXECUTE",
        "active": false,
        "system": false
      },
      {
        "index": 2,
        "program": "SAPLUNIT_ASSERT",
        "include": "LUNIT_ASSERTU01",
        "line": 100,
        "procedure": "RUN_TEST",
        "active": false,
        "system": true
      }
    ]
  }
}
```

#### 7. getVariables
Get variable values at current execution point.

**Parameters:**
- `scope` (string): `system`, `locals`, or `all` (default: `system`)

```json
// Request
{"id":"7","domain":"debug","action":"getVariables","params":{"scope":"system"}}

// Response
{
  "id": "7",
  "success": true,
  "data": {
    "variables": [
      {"name": "SY-SUBRC", "value": "4", "scope": "system"},
      {"name": "SY-TABIX", "value": "0", "scope": "system"},
      {"name": "SY-INDEX", "value": "5", "scope": "system"},
      {"name": "SY-DBCNT", "value": "0", "scope": "system"},
      {"name": "SY-UNAME", "value": "DEVELOPER", "scope": "system"},
      {"name": "SY-DATUM", "value": "20251219", "scope": "system"},
      {"name": "SY-UZEIT", "value": "143052", "scope": "system"},
      {"name": "SY-CPROG", "value": "ZCL_MY_CLASS================CP", "scope": "system"}
    ],
    "scope": "system"
  }
}
```

#### 8. detach
Detach from current debuggee.

```json
// Request
{"id":"8","domain":"debug","action":"detach"}

// Response
{
  "id": "8",
  "success": true,
  "data": {
    "detached": true,
    "debuggeeId": "ABC123DEF456..."
  }
}
```

---

## Integration with Dump Analysis (RCA Workflow)

### The Power of Combined Tools

The vsp MCP server provides **both** dump analysis (via ADT REST) and debugging (via WebSocket). This creates a powerful Root Cause Analysis workflow:

```
┌──────────────────────────────────────────────────────────────┐
│                   RCA Workflow                                │
│                                                               │
│  1. GetDumps ──► Find runtime error                          │
│         │                                                     │
│         ▼                                                     │
│  2. GetDump ───► Analyze stack trace, identify failure point │
│         │                                                     │
│         ▼                                                     │
│  3. GetSource ─► Read code at failure location               │
│         │                                                     │
│         ▼                                                     │
│  4. SetExternalBreakpoint ──► Set breakpoint before failure  │
│         │                                                     │
│         ▼                                                     │
│  5. RunUnitTests ──► Trigger test that reproduces the error  │
│         │                                                     │
│         ▼                                                     │
│  6. WebSocket Debug ──► Step through, inspect variables      │
│         │                                                     │
│         ▼                                                     │
│  7. Identify Root Cause ──► Fix the code                     │
└──────────────────────────────────────────────────────────────┘
```

### Example: Analyzing a Division by Zero

**Step 1: Find the dump**
```bash
# MCP Tool: GetDumps
vsp GetDumps --user DEVELOPER --exception_type CX_SY_ZERODIVIDE
```

Response:
```json
{
  "dumps": [
    {
      "id": "/sap/bc/adt/vit/runtime/dumps/20251219/143052/DEVELOPER",
      "date": "2025-12-19",
      "time": "14:30:52",
      "user": "DEVELOPER",
      "exception": "CX_SY_ZERODIVIDE",
      "program": "ZCL_INVOICE_CALC================CP"
    }
  ]
}
```

**Step 2: Get dump details**
```bash
# MCP Tool: GetDump
vsp GetDump --dump_id "20251219/143052/DEVELOPER"
```

Response shows:
- Exception: `CX_SY_ZERODIVIDE`
- Program: `ZCL_INVOICE_CALC`
- Method: `CALCULATE_DISCOUNT`
- Line: 127
- Stack trace pointing to the exact failure location

**Step 3: Read the source code**
```bash
# MCP Tool: GetSource
vsp GetSource --object_type CLAS --name ZCL_INVOICE_CALC
```

Looking at line 127:
```abap
METHOD calculate_discount.
  " ... code ...
  DATA(lv_discount_rate) = get_discount_rate( iv_customer ).
  rv_discount = iv_amount / lv_discount_rate.  " <-- Line 127: Division!
ENDMETHOD.
```

**Step 4: Set breakpoint before the division**
```bash
# MCP Tool: SetExternalBreakpoint
vsp SetExternalBreakpoint --kind line \
  --object_uri "/sap/bc/adt/oo/classes/zcl_invoice_calc/source/main" \
  --line 126 --user DEVELOPER
```

**Step 5: Create/Run unit test to reproduce**
```abap
CLASS ltcl_invoice_calc DEFINITION FOR TESTING.
  PRIVATE SECTION.
    METHODS test_discount_zero_rate FOR TESTING.
ENDCLASS.

CLASS ltcl_invoice_calc IMPLEMENTATION.
  METHOD test_discount_zero_rate.
    DATA(lo_calc) = NEW zcl_invoice_calc( ).
    " This customer has 0% discount rate - triggers the bug!
    DATA(lv_result) = lo_calc->calculate_discount(
      iv_customer = 'NEWCUST001'
      iv_amount   = 1000
    ).
  ENDMETHOD.
ENDCLASS.
```

```bash
# MCP Tool: RunUnitTests
vsp RunUnitTests --object_url "/sap/bc/adt/oo/classes/zcl_invoice_calc"
```

**Step 6: Debug via WebSocket**

```javascript
// Connect to WebSocket
ws.connect("ws://saphost:50000/sap/bc/apc/sap/zadt_vsp?sap-client=001")

// Start listening for the debuggee
ws.send({"id":"1","domain":"debug","action":"listen","params":{"timeout":120}})

// Meanwhile, unit test triggers breakpoint...
// Response: {"status":"caught","debuggees":[{"id":"XYZ...","program":"ZCL_INVOICE_CALC"}]}

// Attach
ws.send({"id":"2","domain":"debug","action":"attach","params":{"debuggeeId":"XYZ..."}})
// Response: {"attached":true,"line":126}

// Step to the division line
ws.send({"id":"3","domain":"debug","action":"step","params":{"type":"over"}})
// Response: {"stepped":"over","line":127}

// Check the variable value
ws.send({"id":"4","domain":"debug","action":"getVariables","params":{"scope":"system"}})
// Response shows SY-SUBRC after get_discount_rate call

// We can see lv_discount_rate = 0, causing division by zero!
```

**Step 7: Fix identified**
The `get_discount_rate` method returns 0 for new customers without a discount configuration. Solution: Add validation before division.

---

## Unit Test Integration for Debugging

### Why Unit Tests for Debugging?

Unit tests provide **reproducible, isolated execution** that's perfect for debugging:

| Benefit | Description |
|---------|-------------|
| **Reproducible** | Same input, same execution path every time |
| **Isolated** | No external dependencies or user interaction |
| **Targeted** | Test specific methods/scenarios |
| **Automated** | Can be triggered programmatically via ADT |
| **Fast** | Quick iteration for debugging |

### Debugging Pattern with Unit Tests

```
┌─────────────────────────────────────────────────────────────────┐
│                  Unit Test Debug Pattern                        │
│                                                                 │
│   ┌──────────────┐                                             │
│   │ AI Agent     │                                             │
│   └──────┬───────┘                                             │
│          │                                                      │
│          ▼                                                      │
│   ┌──────────────┐     ┌─────────────────────────────────┐    │
│   │ 1. Analyze   │────►│ Read source, understand flow    │    │
│   └──────┬───────┘     └─────────────────────────────────┘    │
│          │                                                      │
│          ▼                                                      │
│   ┌──────────────┐     ┌─────────────────────────────────┐    │
│   │ 2. Create    │────►│ Write test class targeting      │    │
│   │    Test      │     │ the specific scenario           │    │
│   └──────┬───────┘     └─────────────────────────────────┘    │
│          │                                                      │
│          ▼                                                      │
│   ┌──────────────┐     ┌─────────────────────────────────┐    │
│   │ 3. Set       │────►│ SetExternalBreakpoint at        │    │
│   │    Breakpoint│     │ method entry or suspect line    │    │
│   └──────┬───────┘     └─────────────────────────────────┘    │
│          │                                                      │
│          ▼                                                      │
│   ┌──────────────┐     ┌─────────────────────────────────┐    │
│   │ 4. Start     │────►│ WebSocket: listen action        │    │
│   │    Listener  │     │ (blocking, waits for debuggee)  │    │
│   └──────┬───────┘     └─────────────────────────────────┘    │
│          │                                                      │
│          │ (parallel)                                           │
│          ▼                                                      │
│   ┌──────────────┐     ┌─────────────────────────────────┐    │
│   │ 5. Run Tests │────►│ RunUnitTests triggers the       │    │
│   │              │     │ breakpoint, debuggee caught     │    │
│   └──────┬───────┘     └─────────────────────────────────┘    │
│          │                                                      │
│          ▼                                                      │
│   ┌──────────────┐     ┌─────────────────────────────────┐    │
│   │ 6. Debug     │────►│ attach, step, getStack,         │    │
│   │              │     │ getVariables, analyze           │    │
│   └──────────────┘     └─────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
```

### Example: Debugging OData Service Implementation

**Scenario:** An OData service returns incorrect data for certain queries.

**Step 1: Identify the RAP behavior implementation**
```bash
vsp GetSource --object_type CLAS --name ZCL_BP_I_SALESORDER
```

**Step 2: Create targeted test class**
```abap
CLASS ltcl_odata_debug DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: mo_environment TYPE REF TO if_cds_test_environment.

    METHODS setup.
    METHODS test_read_with_filter FOR TESTING.
ENDCLASS.

CLASS ltcl_odata_debug IMPLEMENTATION.
  METHOD setup.
    " Setup test doubles for CDS entities
    mo_environment = cl_cds_test_environment=>create(
      i_for_entity = 'ZI_SALESORDER' ).

    " Insert test data
    mo_environment->insert_test_data(
      i_data = VALUE zt_salesorder_t(
        ( order_id = '1000001' customer = 'CUST001' status = 'NEW' )
        ( order_id = '1000002' customer = 'CUST001' status = 'COMPLETED' )
        ( order_id = '1000003' customer = 'CUST002' status = 'NEW' )
      )
    ).
  ENDMETHOD.

  METHOD test_read_with_filter.
    " This simulates the OData query:
    " GET /SalesOrder?$filter=customer eq 'CUST001' and status eq 'NEW'

    DATA lt_result TYPE STANDARD TABLE OF zi_salesorder.

    SELECT * FROM zi_salesorder
      WHERE customer = 'CUST001'
        AND status = 'NEW'
      INTO TABLE @lt_result.

    " Expected: 1 record (order 1000001)
    " Bug: Returns 2 records (incorrect filter logic)
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lt_result )
      msg = 'Filter should return exactly one order'
    ).
  ENDMETHOD.
ENDCLASS.
```

**Step 3: Set breakpoint in behavior implementation**
```bash
vsp SetExternalBreakpoint --kind line \
  --object_uri "/sap/bc/adt/oo/classes/zcl_bp_i_salesorder/source/main" \
  --line 45 --user DEVELOPER
```

**Step 4: Debug session**
```javascript
// Terminal 1: Start listener
ws.send({"id":"1","domain":"debug","action":"listen","params":{"timeout":180}})

// Terminal 2: Run unit tests (triggers breakpoint)
// vsp RunUnitTests --object_url "/sap/bc/adt/oo/classes/zcl_bp_i_salesorder"

// Listener catches debuggee
// {"status":"caught","debuggees":[{"id":"...","program":"ZCL_BP_I_SALESORDER"}]}

// Attach and debug
ws.send({"id":"2","domain":"debug","action":"attach","params":{"debuggeeId":"..."}})
ws.send({"id":"3","domain":"debug","action":"step","params":{"type":"over"}})
ws.send({"id":"4","domain":"debug","action":"getStack"})
ws.send({"id":"5","domain":"debug","action":"getVariables","params":{"scope":"system"}})
```

---

## OData/RAP Debugging Workflow

### Complete Example: Debugging RAP CRUD Operations

**Architecture of RAP debugging:**

```
┌─────────────────────────────────────────────────────────────────┐
│                    OData Request Flow                           │
│                                                                 │
│  HTTP Request                                                   │
│  GET /sap/opu/odata4/sap/zapi_salesorder/SalesOrder('1000')    │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────┐                                           │
│  │ OData Framework │                                           │
│  └────────┬────────┘                                           │
│           │                                                     │
│           ▼                                                     │
│  ┌─────────────────┐    ┌──────────────────────────────────┐  │
│  │ Behavior Pool   │───►│ ZCL_BP_I_SALESORDER              │  │
│  │ (BDEF)          │    │   read_entity()      ← BREAKPOINT│  │
│  └─────────────────┘    │   create()                       │  │
│                         │   update()                       │  │
│                         │   delete()                       │  │
│                         └──────────────────────────────────┘  │
│                                    │                           │
│                                    ▼                           │
│                         ┌──────────────────────────────────┐  │
│                         │ CDS View: ZI_SALESORDER          │  │
│                         │ (Data Model)                     │  │
│                         └──────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

**Debugging RAP Read Operation:**

```javascript
// 1. Set breakpoint in behavior implementation read method
vsp.SetExternalBreakpoint({
  kind: "line",
  object_uri: "/sap/bc/adt/oo/classes/zcl_bp_i_salesorder/source/main",
  line: 25,  // METHOD read_entity
  user: "DEVELOPER"
})

// 2. Start debug listener via WebSocket
ws.send({
  id: "listen1",
  domain: "debug",
  action: "listen",
  params: { timeout: 120 }
})

// 3. Trigger OData call (via test or curl)
// curl -X GET "http://saphost:50000/sap/opu/odata4/sap/zapi_salesorder/SalesOrder('1000')"

// 4. Debuggee caught - attach and inspect
ws.send({id: "attach1", domain: "debug", action: "attach", params: {debuggeeId: "..."}})

// 5. Step through the read implementation
ws.send({id: "step1", domain: "debug", action: "step", params: {type: "into"}})

// 6. Inspect the keys and result
ws.send({id: "stack1", domain: "debug", action: "getStack"})
ws.send({id: "vars1", domain: "debug", action: "getVariables", params: {scope: "all"}})
```

---

## AI Agent Debugging Scenarios

### Scenario 1: Automated Bug Investigation

```
User: "There's a bug in ZCL_PAYMENT_PROCESSOR - payments are being duplicated"

AI Agent Workflow:
1. GetSource(CLAS, ZCL_PAYMENT_PROCESSOR) - Read the code
2. GetDumps(program=ZCL_PAYMENT_PROCESSOR) - Check for related errors
3. Analyze code, identify process_payment method
4. WriteSource(test class) - Create unit test for payment scenario
5. SetExternalBreakpoint(line 87) - Set BP at payment insert
6. WebSocket: listen → attach → step through
7. Discover: Missing duplicate check before INSERT
8. EditSource - Add duplicate check
9. RunUnitTests - Verify fix
10. Report findings to user
```

### Scenario 2: Performance Investigation

```
User: "The GET_CUSTOMER_ORDERS method is slow"

AI Agent Workflow:
1. GetSource - Read the method
2. SetExternalBreakpoint at method entry
3. Create performance test:
   - Call method with large dataset
   - Trigger breakpoint
4. WebSocket debug:
   - Step through each database operation
   - Check SY-DBCNT after each SELECT
   - Identify N+1 query pattern
5. Report: "Found nested SELECT in loop, causing 1000+ DB calls"
6. Suggest fix: Use JOIN or FOR ALL ENTRIES
```

### Scenario 3: Integration Test Debugging

```
User: "Our integration test fails intermittently"

AI Agent Workflow:
1. RunUnitTests - Run the test, capture result
2. GetDumps - Check if it causes dumps
3. Analyze test code
4. SetExternalBreakpoint at assertion line
5. Run test multiple times with debugging:
   - Capture variable values each run
   - Compare successful vs failed runs
6. Discover: Race condition in async processing
7. Report root cause with evidence from debug sessions
```

---

## Best Practices

### 1. Breakpoint Strategy

```
Location Priority:
1. Method entry points (catch all calls)
2. Before database operations (check data)
3. Inside loops (watch iterations)
4. Before conditional branches (verify logic)
5. Error handling blocks (catch exceptions)
```

### 2. Debug Session Management

```javascript
// Always clean up sessions
try {
  // Debug operations...
} finally {
  ws.send({id: "cleanup", domain: "debug", action: "detach"})
}
```

### 3. Timeout Handling

```javascript
// Use appropriate timeouts based on scenario
const timeouts = {
  quickTest: 30,      // Fast unit test
  integration: 120,   // Longer running test
  manual: 240         // Waiting for manual trigger
}
```

### 4. Variable Inspection

```javascript
// Start with system variables for quick orientation
ws.send({action: "getVariables", params: {scope: "system"}})

// SY-SUBRC tells you if last operation succeeded
// SY-TABIX shows current loop iteration
// SY-DBCNT shows rows affected by last DB operation
```

---

## Comparison with Traditional Debugging

| Aspect | SAP GUI Debugger | Eclipse ADT | vsp WebSocket |
|--------|------------------|-------------|---------------|
| **Interface** | GUI | IDE | API/CLI |
| **Automation** | Limited macros | Some scripting | Full automation |
| **AI Integration** | Not possible | Not possible | Native support |
| **Remote Access** | VPN + GUI | VPN + IDE | HTTP/WebSocket |
| **Headless Operation** | No | No | Yes |
| **Session Recording** | Manual | Manual | Automatic JSON |
| **Reproducibility** | Manual steps | Manual steps | Scripted |

---

## Security Considerations

1. **Authentication**: WebSocket uses same SAP credentials
2. **Authorization**: TPDAPI respects S_DEBUG authorizations
3. **Session Isolation**: Each WebSocket gets isolated debug session
4. **Cleanup**: on_disconnect properly releases debugger resources
5. **Timeout Protection**: Max 240 seconds prevents hung sessions

---

## Conclusion

The ZADT-VSP WebSocket debugging capability transforms ABAP debugging from a manual, interactive process into an **automated, AI-drivable workflow**. Combined with dump analysis and unit test execution, it creates a comprehensive Root Cause Analysis toolkit that enables:

- **Faster bug resolution** through automated investigation
- **Better test coverage** with debug-driven test creation
- **Knowledge capture** via recorded debug sessions
- **CI/CD integration** for automated regression debugging

This is a significant step toward **AI-native ABAP development** where intelligent agents can not only write code but also debug, test, and fix issues autonomously.

---

## Appendix: Quick Reference

### WebSocket Actions Cheat Sheet

| Action | Purpose | Key Params |
|--------|---------|------------|
| `getStatus` | Check session state | - |
| `listen` | Wait for debuggee | `timeout` |
| `getDebuggees` | List waiting processes | `user` |
| `attach` | Connect to debuggee | `debuggeeId` |
| `step` | Execute step | `type`: into/over/return/continue |
| `getStack` | Get call stack | - |
| `getVariables` | Get variable values | `scope`: system/locals/all |
| `detach` | End debug session | - |

### MCP Tools for RCA

| Tool | Purpose |
|------|---------|
| `GetDumps` | List runtime errors |
| `GetDump` | Get dump details |
| `GetSource` | Read ABAP source |
| `SetExternalBreakpoint` | Set persistent breakpoint |
| `RunUnitTests` | Execute unit tests |
| `SyntaxCheck` | Validate code changes |
| `Activate` | Activate fixed code |
