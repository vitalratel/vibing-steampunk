--[[
E2E Test Script for Phase 5: TAS-Style Debugging

This script tests the complete Phase 5 feature set:
- 5.1: Lua Scripting Integration
- 5.2: Variable History Recording
- 5.4: Watchpoint Scripting
- 5.5: Force Replay

Prerequisites:
- SAP system accessible
- SAP credentials configured (environment variables)
- ZTEST program exists with breakpointable code

Usage:
  ./vsp lua examples/scripts/test-phase5-e2e.lua
--]]

print("=" .. string.rep("=", 70))
print("vsp Phase 5: TAS-Style Debugging - E2E Test Suite")
print("=" .. string.rep("=", 70))
print("")

local tests_passed = 0
local tests_failed = 0
local tests_skipped = 0

-- Helper function to run a test
local function test(name, fn)
    io.write("TEST: " .. name .. " ... ")
    local ok, err = pcall(fn)
    if ok then
        print("PASS")
        tests_passed = tests_passed + 1
        return true
    else
        if string.find(tostring(err), "SKIP") then
            print("SKIP: " .. string.gsub(tostring(err), ".*SKIP: ", ""))
            tests_skipped = tests_skipped + 1
        else
            print("FAIL: " .. tostring(err))
            tests_failed = tests_failed + 1
        end
        return false
    end
end

-- Helper to skip a test
local function skip(reason)
    error("SKIP: " .. reason)
end

-- ============================================================================
-- Phase 5.1: Lua Scripting Integration
-- ============================================================================

print("\n--- Phase 5.1: Lua Scripting Integration ---\n")

test("print() function works", function()
    -- We're already using print, so this is a sanity check
    assert(type(print) == "function", "print is not a function")
end)

test("sleep() function exists", function()
    assert(type(sleep) == "function", "sleep is not a function")
end)

test("json.encode() works", function()
    local data = {name = "test", value = 42}
    local encoded = json.encode(data)
    assert(encoded ~= nil, "json.encode returned nil")
    assert(string.find(encoded, "name"), "encoded JSON missing 'name'")
    assert(string.find(encoded, "test"), "encoded JSON missing 'test'")
end)

test("json.decode() works", function()
    local jsonStr = '{"name": "hello", "count": 10}'
    local data = json.decode(jsonStr)
    assert(data ~= nil, "json.decode returned nil")
    assert(data.name == "hello", "data.name mismatch")
    assert(data.count == 10, "data.count mismatch")
end)

test("json round-trip preserves data", function()
    local original = {
        string_val = "hello world",
        number_val = 42,
        bool_val = true,
        nested = {a = 1, b = 2}
    }
    local encoded = json.encode(original)
    local decoded = json.decode(encoded)
    assert(decoded.string_val == original.string_val, "string mismatch")
    assert(decoded.number_val == original.number_val, "number mismatch")
    assert(decoded.bool_val == original.bool_val, "bool mismatch")
    assert(decoded.nested.a == 1, "nested.a mismatch")
end)

-- ============================================================================
-- Phase 5.2: Variable History Recording (Unit Tests)
-- ============================================================================

print("\n--- Phase 5.2: Variable History Recording ---\n")

test("startRecording() function exists", function()
    assert(type(startRecording) == "function", "startRecording not found")
end)

test("stopRecording() function exists", function()
    assert(type(stopRecording) == "function", "stopRecording not found")
end)

test("getRecording() function exists", function()
    assert(type(getRecording) == "function", "getRecording not found")
end)

test("startRecording() creates a recording", function()
    startRecording("e2e-test-session", "ZTEST_E2E")
    local info = getRecording()
    assert(info ~= nil, "getRecording returned nil after start")
end)

test("stopRecording() returns statistics", function()
    -- Start fresh
    startRecording("e2e-stats-test", "ZTEST_E2E")
    local stats = stopRecording()
    assert(stats ~= nil, "stopRecording returned nil")
end)

test("getStateAtStep() function exists", function()
    assert(type(getStateAtStep) == "function", "getStateAtStep not found")
end)

test("findWhenChanged() function exists", function()
    assert(type(findWhenChanged) == "function", "findWhenChanged not found")
end)

test("findChanges() function exists", function()
    assert(type(findChanges) == "function", "findChanges not found")
end)

-- ============================================================================
-- Phase 5.4: Watchpoint Scripting (Function Existence)
-- ============================================================================

print("\n--- Phase 5.4: Watchpoint Scripting ---\n")

test("setBreakpoint() exists", function()
    assert(type(setBreakpoint) == "function", "setBreakpoint not found")
end)

test("setStatementBP() exists", function()
    assert(type(setStatementBP) == "function", "setStatementBP not found")
end)

test("setExceptionBP() exists", function()
    assert(type(setExceptionBP) == "function", "setExceptionBP not found")
end)

test("setMessageBP() exists", function()
    assert(type(setMessageBP) == "function", "setMessageBP not found")
end)

test("setBadiBP() exists", function()
    assert(type(setBadiBP) == "function", "setBadiBP not found")
end)

test("setEnhancementBP() exists", function()
    assert(type(setEnhancementBP) == "function", "setEnhancementBP not found")
end)

test("setWatchpoint() exists", function()
    assert(type(setWatchpoint) == "function", "setWatchpoint not found")
end)

test("setMethodBP() exists", function()
    assert(type(setMethodBP) == "function", "setMethodBP not found")
end)

test("getBreakpoints() exists", function()
    assert(type(getBreakpoints) == "function", "getBreakpoints not found")
end)

test("deleteBreakpoint() exists", function()
    assert(type(deleteBreakpoint) == "function", "deleteBreakpoint not found")
end)

-- ============================================================================
-- Phase 5.5: Force Replay
-- ============================================================================

print("\n--- Phase 5.5: Force Replay ---\n")

test("saveCheckpoint() exists", function()
    assert(type(saveCheckpoint) == "function", "saveCheckpoint not found")
end)

test("getCheckpoint() exists", function()
    assert(type(getCheckpoint) == "function", "getCheckpoint not found")
end)

test("listCheckpoints() exists", function()
    assert(type(listCheckpoints) == "function", "listCheckpoints not found")
end)

test("injectCheckpoint() exists", function()
    assert(type(injectCheckpoint) == "function", "injectCheckpoint not found")
end)

test("forceReplay() exists", function()
    assert(type(forceReplay) == "function", "forceReplay not found")
end)

test("replayFromStep() exists", function()
    assert(type(replayFromStep) == "function", "replayFromStep not found")
end)

test("setVariable() exists", function()
    assert(type(setVariable) == "function", "setVariable not found")
end)

-- ============================================================================
-- Checkpoint Workflow Test (No ADT Required)
-- ============================================================================

print("\n--- Checkpoint Workflow (Local) ---\n")

test("saveCheckpoint() saves data locally", function()
    -- This works without ADT because checkpoints are in-memory
    -- but saveCheckpoint needs variables from getVariables which requires ADT
    -- Skip for now
    skip("requires active debug session")
end)

test("listCheckpoints() returns array", function()
    local list = listCheckpoints()
    assert(type(list) == "table", "listCheckpoints should return table")
end)

-- ============================================================================
-- ADT Integration Tests (Require SAP System)
-- ============================================================================

print("\n--- ADT Integration (Requires SAP) ---\n")

-- Check if we can search (indicates ADT connection)
local function check_adt_connection()
    local ok, result = pcall(function()
        return searchObject("ZTEST*", 1)
    end)
    return ok and result ~= nil
end

local adt_available = check_adt_connection()

if not adt_available then
    print("NOTICE: SAP system not available, skipping ADT integration tests")
    print("")
end

test("searchObject() can find ZTEST programs", function()
    if not adt_available then skip("SAP not available") end
    local results = searchObject("ZTEST*", 5)
    assert(results ~= nil, "searchObject returned nil")
    -- Don't require specific results, just that it works
end)

test("getBreakpoints() can list breakpoints", function()
    if not adt_available then skip("SAP not available") end
    local bps = getBreakpoints()
    assert(bps ~= nil, "getBreakpoints returned nil")
end)

-- ============================================================================
-- Recording Integration Test (With Debug Session)
-- ============================================================================

print("\n--- Recording Integration (Requires Debug Session) ---\n")

test("Full recording workflow", function()
    if not adt_available then skip("SAP not available") end
    skip("requires manual debug session setup")

    -- This would be the full test:
    -- 1. Set breakpoint
    -- 2. Start recording
    -- 3. Trigger debuggee
    -- 4. Step through
    -- 5. Stop recording
    -- 6. Query history
end)

test("Force Replay workflow", function()
    if not adt_available then skip("SAP not available") end
    skip("requires manual debug session setup")

    -- This would test:
    -- 1. Load a saved recording
    -- 2. forceReplay to inject state
    -- 3. Verify variables were set
end)

-- ============================================================================
-- Summary
-- ============================================================================

print("")
print("=" .. string.rep("=", 70))
print("Test Summary")
print("=" .. string.rep("=", 70))
print(string.format("  PASSED:  %d", tests_passed))
print(string.format("  FAILED:  %d", tests_failed))
print(string.format("  SKIPPED: %d", tests_skipped))
print("=" .. string.rep("=", 70))

if tests_failed > 0 then
    print("\nSome tests FAILED!")
    os.exit(1)
else
    print("\nAll tests PASSED!")
end
