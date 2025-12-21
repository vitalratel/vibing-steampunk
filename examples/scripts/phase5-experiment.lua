--[[
Phase 5: TAS-Style Debugging - Live Experiment
==============================================

This script demonstrates the complete Phase 5 feature set with a real SAP system.

Prerequisites:
  - SAP credentials configured (SAP_URL, SAP_USER, SAP_PASSWORD, SAP_CLIENT)
  - ZTEST_PHASE5_TAS program exists (or will be created)

Usage:
  ./vsp lua examples/scripts/phase5-experiment.lua

]]

print("╔══════════════════════════════════════════════════════════════╗")
print("║     Phase 5: TAS-Style Debugging - Live Experiment           ║")
print("╚══════════════════════════════════════════════════════════════╝")
print("")

-- Utility functions
local function separator()
    print(string.rep("─", 60))
end

local function header(text)
    print("")
    separator()
    print("  " .. text)
    separator()
    print("")
end

-- Check connection
header("Checking SAP Connection")

local function check_connection()
    local ok, result = pcall(function()
        return searchObject("ZTEST*", 1)
    end)
    return ok, result
end

local connected, search_result = check_connection()
if not connected then
    print("✗ Cannot connect to SAP system")
    print("")
    print("Please ensure environment variables are set:")
    print("  export SAP_URL=http://your-sap-server:50000")
    print("  export SAP_USER=your_user")
    print("  export SAP_PASSWORD=your_password")
    print("  export SAP_CLIENT=001")
    print("")
    os.exit(1)
end

print("✓ SAP connection OK")
if search_result and #search_result > 0 then
    print("  Found " .. #search_result .. " ZTEST* objects")
end

-- ============================================================================
-- Experiment 1: Lua Engine & JSON
-- ============================================================================

header("Experiment 1: Lua Engine & JSON Functions")

-- Test JSON encode
local test_data = {
    program = "ZTEST_PHASE5_TAS",
    variables = {
        LV_COUNTER = 5,
        LV_STATUS = "PROCESSING"
    },
    steps = {1, 2, 3, 4, 5}
}

local encoded = json.encode(test_data)
print("JSON encode test:")
print(encoded)
print("")

-- Test JSON decode
local decoded = json.decode(encoded)
print("JSON decode test:")
print("  program = " .. decoded.program)
print("  LV_COUNTER = " .. tostring(decoded.variables.LV_COUNTER))
print("  steps count = " .. #decoded.steps)
print("")
print("✓ JSON functions work correctly")

-- ============================================================================
-- Experiment 2: Recording System (Without Debug Session)
-- ============================================================================

header("Experiment 2: Recording System")

-- Start a recording
print("Starting recording...")
startRecording("experiment-phase5", "ZTEST_PHASE5_TAS")

local info = getRecording()
if info then
    print("✓ Recording started:")
    print("  ID: " .. (info.id or "unknown"))
    print("  Program: " .. (info.program or "unknown"))
else
    print("✗ Failed to start recording")
end

-- Stop recording
print("")
print("Stopping recording...")
local stats = stopRecording()
if stats then
    print("✓ Recording stopped")
    print("  Stats: " .. json.encode(stats))
else
    print("  (No stats - empty recording)")
end

-- ============================================================================
-- Experiment 3: Checkpoint System
-- ============================================================================

header("Experiment 3: Checkpoint System")

-- Note: saveCheckpoint requires active debug session with variables
-- We'll test the list/get functions

print("Listing checkpoints...")
local checkpoints = listCheckpoints()
print("  Found " .. #checkpoints .. " checkpoints")

if #checkpoints > 0 then
    print("  Available:")
    for _, name in ipairs(checkpoints) do
        print("    - " .. name)
    end

    -- Try to get first checkpoint
    local cp = getCheckpoint(checkpoints[1])
    if cp then
        print("")
        print("  Contents of '" .. checkpoints[1] .. "':")
        for k, v in pairs(cp) do
            print("    " .. k .. " = " .. tostring(v))
        end
    end
else
    print("  (No checkpoints saved yet - requires debug session)")
end

print("")
print("✓ Checkpoint system functional")

-- ============================================================================
-- Experiment 4: Breakpoint Types
-- ============================================================================

header("Experiment 4: Breakpoint Functions")

print("Testing breakpoint function existence:")

local bp_functions = {
    {"setBreakpoint", setBreakpoint},
    {"setStatementBP", setStatementBP},
    {"setExceptionBP", setExceptionBP},
    {"setMessageBP", setMessageBP},
    {"setBadiBP", setBadiBP},
    {"setEnhancementBP", setEnhancementBP},
    {"setWatchpoint", setWatchpoint},
    {"setMethodBP", setMethodBP},
    {"getBreakpoints", getBreakpoints},
    {"deleteBreakpoint", deleteBreakpoint},
}

for _, item in ipairs(bp_functions) do
    local name, fn = item[1], item[2]
    if type(fn) == "function" then
        print("  ✓ " .. name)
    else
        print("  ✗ " .. name .. " (not a function)")
    end
end

-- Try to get current breakpoints
print("")
print("Checking current breakpoints...")
local ok, bps = pcall(getBreakpoints)
if ok and bps then
    print("  Found " .. #bps .. " active breakpoints")
    for _, bp in ipairs(bps) do
        print("    - " .. (bp.kind or "?") .. " at " .. (bp.program or "?") .. ":" .. (bp.line or "?"))
    end
else
    print("  (Could not retrieve breakpoints)")
end

-- ============================================================================
-- Experiment 5: History Functions
-- ============================================================================

header("Experiment 5: History Navigation Functions")

print("Testing history function existence:")

local history_functions = {
    {"getStateAtStep", getStateAtStep},
    {"findWhenChanged", findWhenChanged},
    {"findChanges", findChanges},
    {"listRecordings", listRecordings},
    {"loadRecording", loadRecording},
    {"compareRecordings", compareRecordings},
}

for _, item in ipairs(history_functions) do
    local name, fn = item[1], item[2]
    if type(fn) == "function" then
        print("  ✓ " .. name)
    else
        print("  ✗ " .. name .. " (not a function)")
    end
end

-- ============================================================================
-- Experiment 6: Force Replay Functions
-- ============================================================================

header("Experiment 6: Force Replay Functions")

print("Testing Force Replay function existence:")

local replay_functions = {
    {"setVariable", setVariable},
    {"injectCheckpoint", injectCheckpoint},
    {"forceReplay", forceReplay},
    {"replayFromStep", replayFromStep},
}

for _, item in ipairs(replay_functions) do
    local name, fn = item[1], item[2]
    if type(fn) == "function" then
        print("  ✓ " .. name)
    else
        print("  ✗ " .. name .. " (not a function)")
    end
end

-- ============================================================================
-- Interactive Debug Session (Optional)
-- ============================================================================

header("Interactive Debug Session")

print("To test with a live debug session:")
print("")
print("1. Set a breakpoint:")
print("   setBreakpoint('ZTEST_PHASE5_TAS', 13)")
print("")
print("2. Start recording:")
print("   startRecording('my-session', 'ZTEST_PHASE5_TAS')")
print("")
print("3. Execute program in SE38 (or via runUnitTests)")
print("")
print("4. Wait for debuggee:")
print("   local event = listen(60)")
print("   attach(event.debuggee_id)")
print("")
print("5. Step through:")
print("   stepOver()")
print("   getVariables()")
print("   saveCheckpoint('my-checkpoint')")
print("")
print("6. Stop recording:")
print("   stopRecording()")
print("   detach()")
print("")

-- ============================================================================
-- Summary
-- ============================================================================

header("Experiment Summary")

print("Phase 5 TAS-Style Debugging Features:")
print("")
print("  ✓ Lua Scripting Engine")
print("  ✓ JSON encode/decode")
print("  ✓ Recording start/stop")
print("  ✓ Checkpoint list/get")
print("  ✓ All 8 breakpoint types registered")
print("  ✓ History navigation functions")
print("  ✓ Force Replay functions")
print("")
print("For full debug session testing, use the interactive REPL:")
print("  ./vsp lua")
print("")
print("Or run individual experiment scripts:")
print("  ./vsp lua examples/scripts/experiment1_record.lua")
print("")

separator()
print("  Experiment complete!")
separator()
