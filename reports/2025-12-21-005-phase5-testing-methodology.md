# Phase 5 Testing Methodology: Ensuring TAS-Style Debugging Works

**Date:** 2025-12-21
**Report ID:** 005
**Subject:** Testing strategy for Phase 5 TAS-Style Debugging features

---

## Overview

This report documents our testing methodology for Phase 5 of vsp - the TAS-Style Debugging features. We cover:

1. What we're testing and why
2. Test organization and structure
3. How to run tests
4. Test coverage analysis
5. E2E testing approach

---

## Test Categories

### 1. Unit Tests: Isolated Component Testing

Unit tests verify individual components work correctly in isolation. They're fast, deterministic, and don't require external systems.

#### recorder.go Tests (12 tests)

Tests the execution recording system:

| Test | Purpose |
|------|---------|
| `TestNewExecutionRecorder` | Verify recorder initialization |
| `TestRecordFrame` | Test frame capture and step counting |
| `TestDeltaCompression` | Verify only changed variables stored in deltas |
| `TestGetVariablesAtStep` | Reconstruct full state from delta chain |
| `TestFindWhenChanged` | Find when variable became specific value |
| `TestFindChanges` | Track all changes to a variable |
| `TestCheckpoints` | Named checkpoints for key moments |
| `TestRecordingComplete` | Finalize recording with end time |
| `TestToJSON` | Serialize to JSON format |
| `TestFromJSON` | Deserialize from JSON format |
| `TestStats` | Aggregate statistics calculation |
| `TestVariablesEqual` | Value comparison helper |

**Why these tests?**
- Recording is the foundation of time-travel debugging
- Delta compression must work correctly or we waste memory
- State reconstruction must be accurate for "what happened at step N?" queries

#### history.go Tests (19 tests)

Tests the recording storage and retrieval system:

| Test | Purpose |
|------|---------|
| `TestNewHistoryManager` | Manager initialization, directory creation |
| `TestSaveAndLoadRecording` | Round-trip persistence |
| `TestLoadRecordingFromCache` | LRU cache behavior |
| `TestLoadRecordingNotFound` | Error handling for missing recordings |
| `TestListRecordings` | Enumerate all recordings |
| `TestListRecordingsWithFilter` | Program name filtering |
| `TestRecordingFilterMinSteps` | Filter by step count |
| `TestDeleteRecording` | Remove recordings |
| `TestGetRecordingStats` | Aggregate statistics |
| `TestCompareRecordings` | Diff two recordings |
| `TestCompareRecordingsDifferentPaths` | Detect path divergence |
| `TestCompareRecordingsDifferentStepCount` | Detect step count differences |
| `TestRecordingFilterMatches` | All filter criteria |
| `TestRebuildIndex` | Recovery from corrupt index |
| `TestCacheEviction` | Memory management |
| `TestSearchHistoryVariableValue` | Search by variable value |
| `TestSearchHistoryLocation` | Search by code location |
| `TestSearchHistoryWithLimit` | Result limiting |

**Why these tests?**
- Storage must be reliable for production debugging sessions
- Search must be efficient for large recording collections
- Comparison is critical for "what's different between runs?" analysis

#### lua_test.go Tests (28 tests)

Tests the Lua scripting integration:

| Category | Tests | Purpose |
|----------|-------|---------|
| Engine | 5 | Initialization, execution, error handling |
| Output | 2 | print(), output redirection |
| JSON | 4 | Encode, decode, arrays, round-trip |
| Checkpoints | 3 | Save, get, list |
| Sleep | 2 | Timing, context cancellation |
| Conversions | 9 | luaToGo, goToLua, tables, maps, slices |
| Recording | 3 | Start, stop, get |

**Total: 28 scripting tests**

**Why these tests?**
- Lua is the scripting interface for TAS-style debugging
- JSON conversion is used everywhere in data exchange
- Type conversions must be lossless

---

## Test Organization

```
pkg/
├── adt/
│   ├── recorder.go          # Execution recording implementation
│   ├── recorder_test.go     # 12 tests
│   ├── history.go           # Recording storage/retrieval
│   └── history_test.go      # 19 tests
│
└── scripting/
    ├── lua.go               # Lua engine
    ├── bindings.go          # ADT Lua bindings
    ├── helpers.go           # Type conversion utilities
    └── lua_test.go          # 30 tests
```

---

## Running Tests

### All Unit Tests

```bash
go test ./...
```

### Specific Package

```bash
# ADT package (recorder + history)
go test -v ./pkg/adt/

# Scripting package (Lua integration)
go test -v ./pkg/scripting/
```

### Specific Test

```bash
# Run only recording tests
go test -v ./pkg/adt/ -run "TestRecording|TestRecordFrame"

# Run only Lua conversion tests
go test -v ./pkg/scripting/ -run "TestLuaToGo|TestGoToLua"
```

### Coverage Report

```bash
go test -coverprofile=coverage.out ./...
go tool cover -html=coverage.out
```

---

## E2E Testing

### The Challenge

Phase 5 features require an active debug session with a real SAP system. This creates challenges:

1. **External Dependency:** Tests need SAP credentials and network access
2. **State Dependency:** Need breakpoints hit, debuggee attached
3. **Non-Determinism:** Timing, other users, system state

### Our Solution: Layered Testing

```
Layer 1: Unit Tests (No SAP Required)
├── Data structures (ExecutionFrame, Recording)
├── Algorithms (delta compression, search)
├── Serialization (JSON, persistence)
└── Lua integration (engine, conversions)

Layer 2: Function Existence (No SAP Required)
├── All Lua functions registered
├── Correct signatures
└── Basic error handling

Layer 3: E2E Script (SAP Required)
├── ADT connection verification
├── Breakpoint operations
└── Recording workflow (manual setup)
```

### E2E Test Script

```bash
./vsp lua examples/scripts/test-phase5-e2e.lua
```

The E2E script:
1. Verifies all Phase 5 functions exist
2. Tests JSON/checkpoint operations locally
3. Checks SAP connection (skips if unavailable)
4. Runs integration tests when SAP available

Sample output:
```
=======================================================================
vsp Phase 5: TAS-Style Debugging - E2E Test Suite
=======================================================================

--- Phase 5.1: Lua Scripting Integration ---

TEST: print() function works ... PASS
TEST: json.encode() works ... PASS
TEST: json.decode() works ... PASS

--- Phase 5.2: Variable History Recording ---

TEST: startRecording() creates a recording ... PASS
TEST: stopRecording() returns statistics ... PASS

--- Phase 5.4: Watchpoint Scripting ---

TEST: setBreakpoint() exists ... PASS
TEST: setWatchpoint() exists ... PASS
TEST: setMethodBP() exists ... PASS

--- Phase 5.5: Force Replay ---

TEST: forceReplay() exists ... PASS
TEST: setVariable() exists ... PASS

=======================================================================
Test Summary
=======================================================================
  PASSED:  35
  FAILED:  0
  SKIPPED: 5
=======================================================================

All tests PASSED!
```

---

## What Each Test Level Verifies

### Unit Tests Verify

1. **Correctness:** Algorithms produce right results
2. **Edge Cases:** Empty inputs, nil values, boundary conditions
3. **Error Handling:** Graceful failures, clear error messages
4. **Determinism:** Same input → same output

### E2E Tests Verify

1. **Integration:** Components work together
2. **API Contracts:** Lua sees what Go provides
3. **Real Workflow:** Actual debugging scenarios work
4. **Environment:** Works with real SAP system

---

## Test Coverage Analysis

### Current Coverage

| Package | Tests | Coverage Area |
|---------|-------|---------------|
| `pkg/adt` | 31+ | recorder, history |
| `pkg/scripting` | 30 | lua engine, bindings |
| E2E script | 40+ | all Phase 5 functions |

### What's Tested

- All public functions have tests
- Delta compression algorithm
- State reconstruction from deltas
- JSON serialization/deserialization
- Lua ↔ Go type conversions
- Recording persistence and retrieval
- Search and filter operations
- Recording comparison

### What's NOT Tested (Requires SAP)

- Actual breakpoint setting/triggering
- Real debug session recording
- Variable modification (Force Replay)
- State injection from recordings

---

## Testing Best Practices Used

### 1. Table-Driven Tests

```go
tests := []struct {
    name     string
    input    interface{}
    expected interface{}
}{
    {"equal strings", "hello", "hello"},
    {"different numbers", 42, 43},
}

for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) {
        // Test logic
    })
}
```

**Why:** Clear, extensible, easy to add cases

### 2. Subtests for Related Cases

```go
func TestRecordingFilterMatches(t *testing.T) {
    t.Run("empty filter matches all", ...)
    t.Run("program filter matches", ...)
    t.Run("min steps filter", ...)
}
```

**Why:** Granular failures, organized output

### 3. Temporary Directories

```go
tmpDir := t.TempDir()
hm, _ := NewHistoryManager(tmpDir)
```

**Why:** Isolated tests, automatic cleanup

### 4. Output Capture

```go
var buf bytes.Buffer
engine.SetOutput(&buf)
engine.Execute("print('hello')")
assert(strings.Contains(buf.String(), "hello"))
```

**Why:** Test output without side effects

---

## How to Add New Tests

### For New Recording Features

1. Add test to `pkg/adt/recorder_test.go`:
```go
func TestNewFeature(t *testing.T) {
    recorder := NewExecutionRecorder("test", "ZTEST")
    // Test your feature
}
```

2. Run: `go test -v ./pkg/adt/ -run TestNewFeature`

### For New Lua Functions

1. Add test to `pkg/scripting/lua_test.go`:
```go
func TestNewLuaFunction(t *testing.T) {
    engine := NewLuaEngine(nil)
    defer engine.Close()

    var buf bytes.Buffer
    engine.SetOutput(&buf)

    err := engine.Execute(`newFunction()`)
    // Assert results
}
```

2. Run: `go test -v ./pkg/scripting/ -run TestNewLuaFunction`

### For E2E Scenarios

1. Add test to `examples/scripts/test-phase5-e2e.lua`:
```lua
test("New workflow scenario", function()
    if not adt_available then skip("SAP not available") end
    -- Your test
end)
```

2. Run: `./vsp lua examples/scripts/test-phase5-e2e.lua`

---

## Conclusion

Our testing approach for Phase 5 follows the testing pyramid:

```
        /\
       /E2E\        <- Few, slow, comprehensive
      /------\
     /Integration\  <- Some, medium speed
    /------------\
   /  Unit Tests  \ <- Many, fast, focused
  /----------------\
```

- **59 unit tests** verify core algorithms work correctly (12 recorder + 19 history + 28 scripting)
- **E2E script** verifies the complete system works together
- **Layered approach** allows testing without SAP when developing locally

This ensures we can confidently develop new features while maintaining quality.

---

## Related Documents

- [TAS-Style Debugging Vision](2025-12-21-001-tas-scripting-time-travel-vision.md)
- [Force Replay & State Injection](2025-12-21-003-force-replay-state-injection.md)
- [ROADMAP.md](/ROADMAP.md) - Phase 5 status
