-- record-debug-session.lua
-- Example: Record a debug session with variable history for time-travel debugging
--
-- Usage: vsp lua examples/scripts/record-debug-session.lua PROGRAM_NAME LINE_NUMBER

local PROGRAM = arg and arg[1] or "ZTEST_MCP_CRUD"
local LINE = arg and arg[2] and tonumber(arg[2]) or 10
local TIMEOUT = 120  -- 2 minutes to trigger the program

print("vsp Execution Recording Example")
print("=" .. string.rep("=", 50))
print("")
print("Program:    " .. PROGRAM)
print("Breakpoint: Line " .. LINE)
print("Timeout:    " .. TIMEOUT .. " seconds")
print("")

-- Step 1: Start recording
print("1. Starting recording session...")
local recordingId = startRecording("demo-session", PROGRAM)
print("   Recording ID: " .. recordingId)

-- Step 2: Set a breakpoint
print("")
print("2. Setting breakpoint...")
local bpId, bpErr = setBreakpoint(PROGRAM, LINE)
if not bpId then
    print("   Error: " .. (bpErr or "unknown"))
    stopRecording()
    return
end
print("   Breakpoint ID: " .. bpId)

-- Step 3: Wait for debuggee
print("")
print("3. Waiting for debuggee...")
print("   Please execute " .. PROGRAM .. " in SAP GUI or via unit test")
print("")

local event = listen(TIMEOUT)
if not event then
    print("   Timeout - no debuggee caught")
    deleteBreakpoint(bpId)
    stopRecording()
    return
end

print("   Caught debuggee!")
print("   - Program: " .. (event.program or "unknown"))
print("   - Line: " .. (event.line or "unknown"))

-- Step 4: Attach and record steps
print("")
print("4. Attaching and recording execution...")
local session = attach(event.id)
if not session then
    print("   Error: Could not attach")
    deleteBreakpoint(bpId)
    stopRecording()
    return
end

-- Save initial checkpoint
saveCheckpoint("entry_point")
print("   Saved checkpoint: entry_point")

-- Step through and record
local maxSteps = 20
local step = 0
while step < maxSteps do
    step = step + 1

    -- Get current state
    local stack = getStack()
    if not stack or #stack == 0 then
        print("   Session ended")
        break
    end

    local frame = stack[1]
    print(string.format("   Step %2d: %s:%d", step, frame.program, frame.line))

    -- Get variables at this point (would need variable IDs in real scenario)
    -- local vars = getVariables({"LV_RESULT", "SY-SUBRC"})

    -- Step over
    local result = stepOver()
    if not result or not result.stepping then
        print("   Cannot continue stepping")
        break
    end

    -- Small delay to let state settle
    sleep(0.1)
end

-- Step 5: Final checkpoint and detach
print("")
print("5. Saving final state...")
saveCheckpoint("exit_point")
detach()
deleteBreakpoint(bpId)

-- Step 6: Stop recording and show stats
print("")
print("6. Stopping recording...")
local stats = stopRecording()
if stats then
    print("")
    print("Recording Statistics:")
    print("-" .. string.rep("-", 30))
    print("   Total Steps:      " .. (stats.total_steps or 0))
    print("   Unique Locations: " .. (stats.unique_locations or 0))
    print("   Checkpoints:      " .. (stats.checkpoints or 0))
    print("   Duration:         " .. string.format("%.2f", stats.duration_seconds or 0) .. "s")
end

-- Step 7: Save recording to disk
print("")
print("7. Saving recording to disk...")
local ok, savedId = saveRecording(".vsp-recordings")
if ok then
    print("   Saved as: " .. savedId)
    print("   Location: .vsp-recordings/" .. savedId .. ".json")
else
    print("   Error saving: " .. (savedId or "unknown"))
end

-- Step 8: Demo: Load and query the recording
print("")
print("8. Demo: Query the saved recording...")
local recordings = listRecordings()
if recordings and #recordings > 0 then
    print("   Found " .. #recordings .. " recording(s)")
    local latest = recordings[1]
    print("   Latest: " .. latest.id .. " (" .. latest.total_steps .. " steps)")

    -- Show how to get state at step N
    print("")
    print("   To get state at step 5:")
    print('   local state = getStateAtStep(5)')
    print("")
    print("   To find when a variable changed:")
    print('   local step = findWhenChanged("LV_RESULT", "SUCCESS")')
    print("")
    print("   To find all changes to a variable:")
    print('   local changes = findChanges("LV_RESULT")')
end

print("")
print("=" .. string.rep("=", 50))
print("Recording complete! Use getStateAtStep(N) for time-travel.")
