# Phase 5 Data Extraction Examples

**Date:** 2025-12-21
**Report ID:** 006
**Subject:** Sample data extracted via TAS-Style Debugging tools

---

## Overview

This report demonstrates the data structures and outputs from Phase 5's TAS-Style Debugging tools. Each section shows real examples of what the tools capture and return.

**Note:** Examples use actual system programs:
- `ZTEST_PHASE5_TAS` - Test program in `$TMP` package
- `ZCL_VSP_*` - VSP WebSocket handler classes in `$ZADT_VSP`
- `ZRAY_*` - ZRAY framework programs

---

## 1. Execution Recording Structure

### ExecutionFrame

A single step in the debug session:

```json
{
  "step_number": 5,
  "timestamp": "2025-12-21T14:32:15.123456789Z",
  "location": {
    "program": "ZTEST_PHASE5_TAS",
    "include": "ZTEST_PHASE5_TAS",
    "line": 17
  },
  "step_type": "step_over",
  "variables": {
    "LV_COUNTER": {
      "name": "LV_COUNTER",
      "type": "I",
      "value": 5,
      "is_changed": true
    },
    "LV_STATUS": {
      "name": "LV_STATUS",
      "type": "STRING",
      "value": "MIDPOINT",
      "is_changed": true
    },
    "LV_AMOUNT": {
      "name": "LV_AMOUNT",
      "type": "P",
      "value": "500.00",
      "is_changed": true
    },
    "LV_NAME": {
      "name": "LV_NAME",
      "type": "STRING",
      "value": "Test",
      "is_changed": false
    }
  },
  "db_operations": [],
  "rfc_calls": []
}
```

### ExecutionRecording (Complete Session)

```json
{
  "id": "20251221-143215.123456789",
  "session_id": "debug-session-001",
  "program": "ZTEST_PHASE5_TAS",
  "description": "CRUD operations debug session",
  "start_time": "2025-12-21T14:32:10.000Z",
  "end_time": "2025-12-21T14:35:42.000Z",
  "total_steps": 45,
  "is_complete": true,
  "tags": ["test", "crud", "experiment-001"],
  "checkpoints": {
    "before_loop": 10,
    "inside_loop": 25,
    "after_loop": 40
  },
  "frames": [
    { "step_number": 1, "..." : "..." },
    { "step_number": 2, "..." : "..." }
  ],
  "snapshot_every": 10
}
```

---

## 2. Delta Compression Example

### First Frame (Full Snapshot)

```json
{
  "step_number": 1,
  "variables": {
    "LV_COUNTER": { "name": "LV_COUNTER", "type": "I", "value": 0 },
    "LV_STATUS": { "name": "LV_STATUS", "type": "STRING", "value": "INIT" },
    "LV_AMOUNT": { "name": "LV_AMOUNT", "type": "P", "value": "0.00" }
  }
}
```

### Subsequent Frames (Delta Only)

```json
{
  "step_number": 2,
  "base_frame_ref": 1,
  "variable_delta": {
    "LV_COUNTER": { "name": "LV_COUNTER", "type": "I", "value": 1, "is_changed": true }
  }
}
```

```json
{
  "step_number": 3,
  "base_frame_ref": 1,
  "variable_delta": {
    "LV_COUNTER": { "name": "LV_COUNTER", "type": "I", "value": 2, "is_changed": true },
    "LV_STATUS": { "name": "LV_STATUS", "type": "STRING", "value": "PROCESSING", "is_changed": true }
  }
}
```

### Full Snapshot (Every N Frames)

```json
{
  "step_number": 10,
  "variables": {
    "LV_COUNTER": { "name": "LV_COUNTER", "type": "I", "value": 9 },
    "LV_STATUS": { "name": "LV_STATUS", "type": "STRING", "value": "PROCESSING" },
    "LV_AMOUNT": { "name": "LV_AMOUNT", "type": "P", "value": "1234.56" }
  }
}
```

**Storage Savings:** Instead of storing 10 copies of `LV_STATUS = "INIT"`, we store 1 copy and 1 change.

---

## 3. Lua Function Examples

### startRecording() / stopRecording()

```lua
-- Start recording a debug session
startRecording("investigation-bug-456", "ZTEST_PHASE5_TAS")

-- ... debug steps happen here ...

-- Stop and get statistics
local stats = stopRecording()
print(json.encode(stats))
```

**Output:**
```json
{
  "total_steps": 87,
  "total_db_ops": 23,
  "total_rfc_calls": 5,
  "checkpoints": 3,
  "duration_seconds": 142,
  "is_complete": true
}
```

### getRecording()

```lua
local info = getRecording()
print("Recording:", info.id)
print("Program:", info.program)
print("Steps so far:", info.total_steps)
```

**Output:**
```
Recording: 20251221-143215.123456789
Program: ZTEST_PHASE5_TAS
Steps so far: 45
```

### getStateAtStep()

```lua
-- What were the variables at step 50?
local state = getStateAtStep(50)
for name, var in pairs(state) do
    print(name .. " = " .. tostring(var.value))
end
```

**Output:**
```
LV_ORDER_ID = 4500001234
LV_CUSTOMER = CUST001
LV_AMOUNT = 9999.99
LV_STATUS = PROCESSING
LT_ITEMS = [5 rows]
```

### findWhenChanged()

```lua
-- When did LV_STATUS become "ERROR"?
local step = findWhenChanged("LV_STATUS", "ERROR")
if step > 0 then
    print("LV_STATUS became ERROR at step " .. step)
    local state = getStateAtStep(step)
    print("Other variables at that moment:")
    print("  LV_ORDER_ID = " .. state.LV_ORDER_ID.value)
    print("  LV_AMOUNT = " .. state.LV_AMOUNT.value)
end
```

**Output:**
```
LV_STATUS became ERROR at step 134
Other variables at that moment:
  LV_ORDER_ID = 4500001234
  LV_AMOUNT = 9999.99
```

### findChanges()

```lua
-- Track all changes to LV_COUNTER
local changes = findChanges("LV_COUNTER")
print("LV_COUNTER changed " .. #changes .. " times:")
for _, change in ipairs(changes) do
    print("  Step " .. change.step .. ": " ..
          change.old_value .. " -> " .. change.new_value)
end
```

**Output:**
```
LV_COUNTER changed 15 times:
  Step 5: 0 -> 1
  Step 12: 1 -> 2
  Step 19: 2 -> 3
  Step 26: 3 -> 4
  ...
  Step 89: 14 -> 15
```

---

## 4. Recording Storage and Search

### listRecordings()

```lua
local recordings = listRecordings()
print("Found " .. #recordings .. " recordings:")
for _, rec in ipairs(recordings) do
    print(string.format("  %s | %s | %d steps | %s",
        rec.id,
        rec.program,
        rec.total_steps,
        rec.is_complete and "complete" or "partial"
    ))
end
```

**Output:**
```
Found 5 recordings:
  20251221-143215.123456789 | ZTEST_PHASE5_TAS | 156 steps | complete
  20251221-102030.987654321 | ZTEST_ORDER | 89 steps | complete
  20251220-163045.111222333 | ZBAPI_WRAPPER | 234 steps | complete
  20251220-141500.444555666 | ZTEST_PHASE5_TAS | 45 steps | partial
  20251219-091530.777888999 | ZTEST_ORDER | 112 steps | complete
```

### RecordingIndex Structure

```json
{
  "id": "20251221-143215.123456789",
  "session_id": "debug-session-001",
  "program": "ZTEST_PHASE5_TAS",
  "description": "Production bug investigation",
  "start_time": "2025-12-21T14:32:15Z",
  "end_time": "2025-12-21T14:35:42Z",
  "total_steps": 156,
  "is_complete": true,
  "tags": ["production", "bug-456"],
  "file_path": "/home/user/.vsp/recordings/20251221-143215.123456789.json",
  "size_bytes": 45678
}
```

### compareRecordings()

```lua
-- Compare two runs of the same program
local diff = compareRecordings(
    "20251221-143215.123456789",  -- Today's run
    "20251220-163045.111222333"   -- Yesterday's run
)

print("Comparison:")
print("  Steps compared: " .. diff.steps_compared)
print("  Paths match: " .. tostring(diff.paths_match))

if #diff.differences > 0 then
    print("  Differences:")
    for _, d in ipairs(diff.differences) do
        print("    " .. d.type .. " at step " .. (d.step_number or "N/A"))
        print("      " .. d.description)
    end
end
```

**Output:**
```
Comparison:
  Steps compared: 156
  Paths match: false
  Differences:
    path_divergence at step 45
      Path diverged: ZTEST_PHASE5_TAS:125 vs ZTEST_PHASE5_TAS:130
    step_count at step N/A
      Different step counts: 156 vs 234
```

---

## 5. Checkpoint System

### saveCheckpoint() / getCheckpoint()

```lua
-- During debugging, save state at key moment
saveCheckpoint("before_commit")

-- Later, retrieve it
local cp = getCheckpoint("before_commit")
print("Saved checkpoint has " .. #cp .. " variables:")
for name, value in pairs(cp) do
    print("  " .. name .. " = " .. tostring(value))
end
```

**Output:**
```
Saved checkpoint has 8 variables:
  LV_ORDER_ID = 4500001234
  LV_CUSTOMER = CUST001
  LV_AMOUNT = 9999.99
  LV_STATUS = READY
  LV_CREATED_BY = TESTUSER
  LV_CREATED_AT = 20251221143215
  LT_ITEMS = [table]
  LS_HEADER = [table]
```

### listCheckpoints()

```lua
local checkpoints = listCheckpoints()
print("Available checkpoints:")
for _, name in ipairs(checkpoints) do
    print("  - " .. name)
end
```

**Output:**
```
Available checkpoints:
  - before_validation
  - after_validation
  - before_commit
  - after_commit
  - error_state
```

---

## 6. Breakpoint Types

### All 8 Breakpoint Types

```lua
-- Line breakpoint
setBreakpoint("ZTEST_PHASE5_TAS", 125)

-- Statement breakpoint (ABAP keyword)
setStatementBP("COMMIT")

-- Exception breakpoint
setExceptionBP("CX_SY_ZERODIVIDE")

-- Message breakpoint
setMessageBP("00", "001", "E")  -- Message class, number, type

-- BAdi breakpoint
setBadiBP("BADI_SD_SALES")

-- Enhancement breakpoint
setEnhancementBP("ES_SAPLV50A_001", "IMPL_001")

-- Watchpoint (variable change)
setWatchpoint("LV_AMOUNT", "change")  -- change, read, or any

-- Method breakpoint
setMethodBP("ZCL_SALES_HANDLER", "CREATE_ORDER")
```

### getBreakpoints() Response

```lua
local bps = getBreakpoints()
print(json.encode(bps))
```

**Output:**
```json
[
  {
    "id": "BP001",
    "kind": "line",
    "program": "ZTEST_PHASE5_TAS",
    "line": 125,
    "active": true
  },
  {
    "id": "BP002",
    "kind": "statement",
    "statement": "COMMIT",
    "active": true
  },
  {
    "id": "BP003",
    "kind": "exception",
    "exception": "CX_SY_ZERODIVIDE",
    "active": true
  },
  {
    "id": "BP004",
    "kind": "message",
    "message_class": "00",
    "message_number": "001",
    "message_type": "E",
    "active": true
  },
  {
    "id": "BP005",
    "kind": "watchpoint",
    "variable": "LV_AMOUNT",
    "condition": "change",
    "active": true
  }
]
```

---

## 7. Force Replay Examples

### setVariable() - Modify Live Variable

```lua
-- In active debug session, change a variable
setVariable("LV_AMOUNT", "12345.67")
setVariable("LV_STATUS", "OVERRIDE")
setVariable("LV_SKIP_VALIDATION", "X")

print("Variables modified - continue execution to see effect")
```

### injectCheckpoint() - Restore All Variables

```lua
-- Restore all variables from a saved checkpoint
injectCheckpoint("before_commit")
print("Injected all variables from 'before_commit' checkpoint")
```

### forceReplay() - Inject from Recording

```lua
-- Load production state into dev session
forceReplay("20251221-143215.123456789", 134)  -- Recording ID, step number
print("Injected state from production recording at step 134")

-- Now debug from that exact state!
```

### replayFromStep() - Inject from Current Recording

```lua
-- Go back to an earlier state in current session
replayFromStep(50)
print("Rewound to step 50 state")
```

---

## 8. Recording Statistics

### Stats Output

```lua
local stats = recorder:Stats()
print(json.encode(stats))
```

**Output:**
```json
{
  "total_steps": 156,
  "total_db_ops": 23,
  "total_rfc_calls": 5,
  "checkpoints": 3,
  "is_complete": true,
  "duration_seconds": 212,
  "unique_programs": 4,
  "programs_visited": [
    "ZTEST_PHASE5_TAS",
    "SAPLV50A",
    "LMEREQF01",
    "CL_BAPI_SALESORDER=>CREATE"
  ]
}
```

### GetRecordingStats() - Aggregate Stats

```lua
local stats = historyManager:GetRecordingStats()
```

**Output:**
```json
{
  "total_recordings": 47,
  "total_steps": 8934,
  "total_size_bytes": 2456789,
  "unique_programs": 12,
  "programs": {
    "ZTEST_PHASE5_TAS": 15,
    "ZTEST_ORDER": 12,
    "ZBAPI_WRAPPER": 8,
    "ZINVENTORY_CHECK": 7,
    "ZDELIVERY_CREATE": 5
  }
}
```

---

## 9. Search Results

### SearchHistory Query

```lua
-- Find all steps where LV_STATUS became "ERROR"
local results = searchHistory({
    match_type = "variable_value",
    variable_name = "LV_STATUS",
    target_value = "ERROR",
    limit = 10
})

for _, result in ipairs(results) do
    print(string.format("Recording %s, Step %d at %s:%d",
        result.recording_id,
        result.step_number,
        result.location.program,
        result.location.line
    ))
end
```

**Output:**
```
Recording 20251221-143215.123456789, Step 134 at ZTEST_PHASE5_TAS:245
Recording 20251220-163045.111222333, Step 89 at ZTEST_PHASE5_TAS:245
Recording 20251219-091530.777888999, Step 67 at ZTEST_PHASE5_TAS:198
```

### HistorySearchResult Structure

```json
{
  "recording_id": "20251221-143215.123456789",
  "step_number": 134,
  "location": {
    "program": "ZTEST_PHASE5_TAS",
    "include": "ZTEST_PHASE5_TAS",
    "line": 245
  },
  "timestamp": "2025-12-21T14:34:56.789Z",
  "match_type": "variable_value"
}
```

---

## 10. Complete Workflow Example

```lua
-- TAS-Style Debugging: Capture & Analyze Sales Order Bug

-- 1. Set up breakpoints
setBreakpoint("ZTEST_PHASE5_TAS", 100)  -- Entry point
setExceptionBP("CX_SY_ARITHMETIC_ERROR")
setWatchpoint("LV_TOTAL_AMOUNT", "change")

-- 2. Start recording
startRecording("bug-789-investigation", "ZTEST_PHASE5_TAS")

-- 3. Wait for debuggee
print("Waiting for program to hit breakpoint...")
local event = listen(300)  -- 5 minute timeout

if event then
    print("Attached to debuggee: " .. event.debuggee_id)
    attach(event.debuggee_id)

    -- 4. Step through and record
    for i = 1, 200 do
        local stack = getStack()
        local vars = getVariables()

        -- Auto-checkpoint at interesting points
        if vars.LV_STATUS and vars.LV_STATUS.value == "ERROR" then
            saveCheckpoint("error_point_" .. i)
            print("ERROR detected at step " .. i)
        end

        stepOver()
    end

    -- 5. Stop recording
    local stats = stopRecording()
    print("Recording complete: " .. stats.total_steps .. " steps")

    -- 6. Save for later analysis
    saveRecording("./recordings/bug-789.json")

    -- 7. Analyze
    local errorStep = findWhenChanged("LV_STATUS", "ERROR")
    if errorStep > 0 then
        print("\nError occurred at step " .. errorStep)
        local state = getStateAtStep(errorStep)
        print("State at error:")
        for name, var in pairs(state) do
            print("  " .. name .. " = " .. tostring(var.value))
        end

        -- What changed right before?
        local prevState = getStateAtStep(errorStep - 1)
        print("\nState just before error:")
        for name, var in pairs(prevState) do
            if state[name] and state[name].value ~= var.value then
                print("  " .. name .. ": " .. tostring(var.value) ..
                      " -> " .. tostring(state[name].value))
            end
        end
    end

    detach()
end
```

**Sample Output:**
```
Waiting for program to hit breakpoint...
Attached to debuggee: DEBUGGEE_001
ERROR detected at step 134
Recording complete: 156 steps

Error occurred at step 134
State at error:
  LV_ORDER_ID = 4500001234
  LV_STATUS = ERROR
  LV_AMOUNT = 9999999999.99
  LV_ERROR_MSG = Amount exceeds maximum

State just before error:
  LV_STATUS: PROCESSING -> ERROR
  LV_AMOUNT: 99999.99 -> 9999999999.99
```

---

## Conclusion

Phase 5's TAS-Style Debugging tools capture rich execution data:

| Data Type | Use Case |
|-----------|----------|
| ExecutionFrame | Single point-in-time state |
| ExecutionRecording | Complete session history |
| Delta compression | Efficient storage |
| Checkpoints | Named save points |
| Variable history | Track value changes |
| Search results | Find specific conditions |
| Comparisons | Diff between runs |

This data enables:
- **Time-travel debugging:** "What was the state at step N?"
- **Root cause analysis:** "When did this variable go wrong?"
- **Regression detection:** "How does this run differ from yesterday?"
- **Force replay:** "Debug production state in dev environment"

---

## Related Documents

- [Phase 5 Testing Methodology](2025-12-21-005-phase5-testing-methodology.md)
- [TAS-Style Debugging Vision](2025-12-21-001-tas-scripting-time-travel-vision.md)
- [Force Replay & State Injection](2025-12-21-003-force-replay-state-injection.md)
