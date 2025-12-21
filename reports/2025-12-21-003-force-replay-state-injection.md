# Force Replay: Live State Injection for Debugging

**Date:** 2025-12-21
**Report ID:** 003
**Subject:** Injecting Recorded State into Live Debug Sessions
**Status:** Design Document

---

## The Insight

Two complementary approaches to "replay":

| Approach | Description | Use Case |
|----------|-------------|----------|
| **Isolated Replay** | Run code independently with mocks | Fast patch testing |
| **Force Replay** | Inject state into LIVE session | Production-like debugging |

**Force Replay** is the game-changer: Debug to a breakpoint, then **overwrite** the variables with recorded values from a previous execution.

---

## The Concept

```
┌─────────────────────────────────────────────────────────────────┐
│  TRADITIONAL DEBUGGING                                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Production Bug:          Your Debug Session:                    │
│  ────────────────         ────────────────────                   │
│  LV_AMOUNT = 1000000      LV_AMOUNT = 100                       │
│  LV_CUSTOMER = 'BIGCORP'  LV_CUSTOMER = 'TEST01'                │
│  → CRASH!                 → Works fine :(                       │
│                                                                  │
│  "I can't reproduce it because I don't have the same data!"     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│  FORCE REPLAY                                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. Record production crash (via short dump or trace)            │
│     → Variables captured: LV_AMOUNT=1000000, LV_CUSTOMER=...    │
│                                                                  │
│  2. Debug locally, reach same breakpoint                         │
│     → Your local values: LV_AMOUNT=100, LV_CUSTOMER='TEST01'    │
│                                                                  │
│  3. FORCE REPLAY: Inject production values!                      │
│     → setVariable("LV_AMOUNT", 1000000)                         │
│     → setVariable("LV_CUSTOMER", "BIGCORP")                     │
│                                                                  │
│  4. Continue execution                                           │
│     → CRASH! Same as production!                                │
│     → Now you can debug the ACTUAL scenario                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Use Cases

### 1. Production Bug Reproduction

```lua
-- Load state from production dump
local prod_state = loadCheckpoint("dump_TICKET-1234")

-- Debug to the relevant point
setBreakpoint("ZCL_PRICING", "CALCULATE", 42)
local event = waitForBreakpoint(60)
attach(event.debuggee_id)

-- FORCE REPLAY: Inject production values
for name, var in pairs(prod_state.variables) do
    setVariable(name, var.value)
    print("Injected: " .. name .. " = " .. tostring(var.value))
end

-- Continue and watch it crash
stepOver()  -- BOOM! Now you see the bug!
```

### 2. Edge Case Testing

```lua
-- Reach the calculation point
setBreakpoint("ZCL_PRICING", "CALCULATE", 50)
waitAndAttach()

-- Test edge cases by injecting values
local test_cases = {
    { LV_AMOUNT = 0, expected = "zero handling" },
    { LV_AMOUNT = -1, expected = "negative handling" },
    { LV_AMOUNT = 999999999, expected = "overflow" },
    { LV_AMOUNT = nil, expected = "null handling" },
}

for _, tc in ipairs(test_cases) do
    -- Save current state
    saveCheckpoint("before_test")

    -- Inject test value
    setVariable("LV_AMOUNT", tc.LV_AMOUNT)

    -- Step and observe
    local result = stepOver()

    print(tc.expected .. ": " .. (result.exception or "OK"))

    -- Restore and try next
    restoreCheckpoint("before_test")
end
```

### 3. Regression Testing

```lua
-- Compare behavior before and after patch
local old_recording = loadRecording("before_patch.json")
local checkpoint = old_recording:getCheckpointAt(42)

-- Run new code with old inputs
setBreakpoint("ZCL_PRICING", "CALCULATE", 42)
waitAndAttach()

-- Inject old state
injectCheckpoint(checkpoint)

-- Run to completion
local new_result = runToEnd()

-- Compare
if new_result.output ~= checkpoint.expected_output then
    print("REGRESSION: Output changed!")
    print("  Old: " .. checkpoint.expected_output)
    print("  New: " .. new_result.output)
end
```

### 4. What-If Analysis

```lua
-- "What if the discount was 50% instead of 10%?"

setBreakpoint("ZCL_PRICING", "APPLY_DISCOUNT", 1)
waitAndAttach()

-- Save baseline
saveCheckpoint("baseline")
local baseline_result = runToEnd()

-- Restore and modify
restoreCheckpoint("baseline")
setVariable("LV_DISCOUNT_PCT", 50)
local whatif_result = runToEnd()

print("Baseline total: " .. baseline_result.LV_TOTAL)
print("What-if total:  " .. whatif_result.LV_TOTAL)
print("Difference:     " .. (whatif_result.LV_TOTAL - baseline_result.LV_TOTAL))
```

---

## Technical Implementation

### ADT Variable Modification API

SAP ADT debugger supports variable modification:

```
POST /sap/bc/adt/debugger/sessions/{id}/variables
Content-Type: application/xml

<variable name="LV_AMOUNT" value="1000000"/>
```

### Go Implementation

```go
// pkg/adt/debugger.go

// SetVariable modifies a variable in the current debug session.
func (s *DebugSession) SetVariable(name string, value interface{}) error {
    // Convert value to ABAP-compatible format
    abapValue, err := toABAPValue(value)
    if err != nil {
        return fmt.Errorf("converting value: %w", err)
    }

    body := fmt.Sprintf(`<variable name="%s" value="%s"/>`, name, abapValue)

    _, err = s.client.transport.Request(ctx,
        fmt.Sprintf("/sap/bc/adt/debugger/sessions/%s/variables", s.sessionID),
        &RequestOptions{
            Method:      http.MethodPost,
            ContentType: "application/xml",
            Body:        strings.NewReader(body),
        })

    return err
}

// InjectCheckpoint restores all variables from a checkpoint.
func (s *DebugSession) InjectCheckpoint(cp *Checkpoint) error {
    var errs []error

    for name, variable := range cp.Variables {
        if err := s.SetVariable(name, variable.Value); err != nil {
            errs = append(errs, fmt.Errorf("%s: %w", name, err))
        }
    }

    if len(errs) > 0 {
        return fmt.Errorf("failed to inject %d variables: %v", len(errs), errs)
    }

    return nil
}

// ForceReplay injects state from a recording at a specific step.
func (s *DebugSession) ForceReplay(recording *ExecutionRecording, stepNum int) error {
    frame := recording.GetFrame(stepNum)
    if frame == nil {
        return fmt.Errorf("no frame at step %d", stepNum)
    }

    // Verify we're at the same location
    currentLoc, _ := s.GetCurrentLocation()
    if currentLoc.Program != frame.Location.Program ||
       currentLoc.Line != frame.Location.Line {
        return fmt.Errorf("location mismatch: at %s:%d, expected %s:%d",
            currentLoc.Program, currentLoc.Line,
            frame.Location.Program, frame.Location.Line)
    }

    // Inject all variables
    for name, variable := range frame.Variables {
        if err := s.SetVariable(name, variable.Value); err != nil {
            log.Printf("Warning: couldn't set %s: %v", name, err)
        }
    }

    return nil
}
```

### MCP Tools

```go
// SetVariable - Modify a variable in debug session
case "SetVariable":
    name, _ := getString(args, "name")
    value := args["value"]

    err := s.debugSession.SetVariable(name, value)
    // ...

// InjectCheckpoint - Restore all variables from checkpoint
case "InjectCheckpoint":
    checkpointID, _ := getString(args, "checkpoint_id")

    cp, err := s.checkpointStore.Get(checkpointID)
    err = s.debugSession.InjectCheckpoint(cp)
    // ...

// ForceReplay - Inject state from recording
case "ForceReplay":
    recordingID, _ := getString(args, "recording_id")
    stepNum, _ := getInt(args, "step")

    recording, err := s.recordingStore.Load(recordingID)
    err = s.debugSession.ForceReplay(recording, stepNum)
    // ...
```

### Lua API

```lua
-- Low-level: Set individual variable
setVariable("LV_AMOUNT", 1000000)
setVariable("LV_CUSTOMER", "BIGCORP")
setVariable("LT_ITEMS", {
    { MATNR = "WIDGET-01", QUANTITY = 100 },
    { MATNR = "WIDGET-02", QUANTITY = 50 },
})

-- Mid-level: Inject entire checkpoint
injectCheckpoint("production_crash_001")

-- High-level: Force replay from recording
forceReplay("recording_20251221", 42)  -- Inject state from step 42

-- Batch injection with filtering
injectVariables({
    pattern = "LV_*",        -- Only local variables
    from = "checkpoint_001",
    exclude = { "LV_DEBUG" } -- Skip some
})
```

---

## Workflow: Force Replay Debugging

### Step-by-Step

```
┌─────────────────────────────────────────────────────────────────┐
│  FORCE REPLAY WORKFLOW                                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. CAPTURE (Production/QA)                                      │
│     ┌──────────────────────────────────────────────────────────┐│
│     │ Short dump occurs → Variables captured automatically     ││
│     │ OR: Enable trace → Record execution with variables       ││
│     │ OR: Manual checkpoint → Save state at specific point     ││
│     └──────────────────────────────────────────────────────────┘│
│                           ▼                                      │
│  2. TRANSFER                                                     │
│     ┌──────────────────────────────────────────────────────────┐│
│     │ $ vsp get-dump DUMP123 --save-checkpoint                 ││
│     │ Saved to: checkpoint_dump123.json                        ││
│     │                                                          ││
│     │ Contains:                                                ││
│     │   - All local variables at crash point                   ││
│     │   - Call stack with locations                            ││
│     │   - Relevant global/system variables                     ││
│     └──────────────────────────────────────────────────────────┘│
│                           ▼                                      │
│  3. REPRODUCE (Dev/Sandbox)                                      │
│     ┌──────────────────────────────────────────────────────────┐│
│     │ $ vsp lua                                                ││
│     │ lua> bp = setBreakpoint("ZCL_PRICING", 42)               ││
│     │ lua> event = waitForBreakpoint(60)                       ││
│     │ lua> attach(event.debuggee_id)                           ││
│     │ lua> forceReplay("checkpoint_dump123")                   ││
│     │ Injected 15 variables.                                   ││
│     │ lua> stepOver()                                          ││
│     │ EXCEPTION: CX_SY_ZERODIVIDE at line 45                   ││
│     │                                                          ││
│     │ # NOW you can debug the actual production scenario!      ││
│     └──────────────────────────────────────────────────────────┘│
│                           ▼                                      │
│  4. FIX & VERIFY                                                 │
│     ┌──────────────────────────────────────────────────────────┐│
│     │ # Fix the code                                           ││
│     │ lua> editSource("ZCL_PRICING", old, new)                 ││
│     │                                                          ││
│     │ # Replay again to verify fix                             ││
│     │ lua> restoreCheckpoint("before_injection")               ││
│     │ lua> forceReplay("checkpoint_dump123")                   ││
│     │ lua> result = runToEnd()                                 ││
│     │ lua> print(result.exception)                             ││
│     │ nil  -- No more crash!                                   ││
│     └──────────────────────────────────────────────────────────┘│
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Advanced: Hybrid Replay

Combine **Force Replay** with **Isolated Playground**:

```lua
-- Scenario: Test a patch with production-like data
--           but without touching real database

-- 1. Load production checkpoint
local prod_cp = loadCheckpoint("production_crash_001")

-- 2. Start playground with mocks
local playground = createPlayground({
    mocks = extractMocksFrom(prod_cp.recording),
    code = getSource("CLAS", "ZCL_PRICING")
})

-- 3. Apply patch
playground:applyPatch([[
    IF lv_amount < 0.
      lv_amount = 0.  " Fix: handle negative
    ENDIF.
]])

-- 4. Inject production state
playground:injectVariables(prod_cp.variables)

-- 5. Run and verify
local result = playground:run()
assert(result.success, "Patch should fix the issue")
assert(result.output.LV_AMOUNT >= 0, "Amount should not be negative")

-- 6. If good, apply to real system
if result.success then
    editSource("ZCL_PRICING", original, patched)
    activate("ZCL_PRICING")
end
```

---

## Data Types and Limitations

### Supported Types for Injection

| ABAP Type | Injection Support | Notes |
|-----------|-------------------|-------|
| Elementary (I, F, STRING, etc.) | ✅ Full | Direct value injection |
| Structures | ✅ Full | Inject as JSON object |
| Internal Tables | ✅ Full | Inject as JSON array |
| References | ⚠️ Partial | Can inject data, not pointer |
| Objects | ⚠️ Partial | Public attributes only |
| Field Symbols | ❌ No | Points to other memory |
| Data References | ❌ No | Memory address |

### Limitations

1. **Read-Only Variables**: Some system variables can't be modified
2. **Object State**: Can modify attributes, not recreate objects
3. **Database State**: Variables in memory only, not DB commits
4. **Transaction Scope**: Within current LUW only

### Workarounds

```lua
-- For complex objects, inject constructor parameters instead
-- Before: obj->attribute = value (might not work)
-- After:  Create object with test factory

-- For database-dependent values:
-- Use mocks for DB reads, inject results
```

---

## Integration with Existing Tools

### From Short Dump

```lua
-- Automatic: Extract variables from dump
local dump = getDump("DUMP123")
local checkpoint = extractCheckpointFromDump(dump)

-- checkpoint.variables contains all visible variables at crash
-- checkpoint.location contains crash location
-- checkpoint.stack contains call stack
```

### From Trace

```lua
-- Load ATRA trace, get variables at any point
local trace = getTrace("TRACE456", "variables")
local checkpoint = extractCheckpointFromTrace(trace, lineNumber)
```

### From Recording

```lua
-- Use recorded execution
local recording = loadRecording("rec_abc123")
local checkpoint = recording:getFrameAt(42)
```

---

## Safety Considerations

### Injection Validation

```go
// Validate before injection
func (s *DebugSession) ValidateInjection(name string, value interface{}) error {
    // Get current variable info
    info, err := s.GetVariableInfo(name)
    if err != nil {
        return fmt.Errorf("variable not found: %s", name)
    }

    // Check type compatibility
    if !isCompatible(info.Type, value) {
        return fmt.Errorf("type mismatch: %s expects %s, got %T",
            name, info.Type, value)
    }

    // Check if writable
    if info.ReadOnly {
        return fmt.Errorf("variable is read-only: %s", name)
    }

    return nil
}
```

### Audit Trail

```lua
-- All injections are logged
local injection = injectCheckpoint("prod_crash_001")
print(injection.log)
-- [2025-12-21 14:32:15] Injected LV_AMOUNT: 100 -> 1000000
-- [2025-12-21 14:32:15] Injected LV_CUSTOMER: 'TEST01' -> 'BIGCORP'
-- [2025-12-21 14:32:15] Skipped SY-UNAME (read-only)
```

---

## Conclusion

**Force Replay** transforms debugging from:
- "I can't reproduce it" → "I can inject the exact production state"
- "Let me set up test data" → "Let me load the checkpoint"
- "What if this value was different?" → "Let me inject and see"

Combined with **Isolated Replay** (playground), you get:
- **Force Replay**: Debug with real SAP session + injected state
- **Isolated Replay**: Debug with mocked environment + recorded inputs

Both approaches use the same recording format, enabling seamless workflows.

---

## Next Steps

1. Verify ADT variable modification API availability
2. Add `SetVariable` to debug session
3. Implement checkpoint injection
4. Add MCP tools
5. Create Lua bindings
6. Document and test

---

*"Why recreate the bug when you can replay it?"*
