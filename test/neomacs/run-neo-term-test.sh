#!/usr/bin/env bash
# Test terminal emulation (neo-term) rendering
# Usage: ./test/neomacs/run-neo-term-test.sh
#
# What this tests:
# - Terminal buffer creation and rendering
# - Terminal text output and ANSI color support
# - Terminal cursor display
# - Basic terminal I/O
#
# Interactions: wait 8 seconds for terminal creation and initialization,
# take screenshot, check log.

set -e

cd "$(dirname "$0")/../.."

LOG=/tmp/neo-term-test.log
SCREENSHOT=/tmp/neo-term-screenshot.png
TEST_NAME="Neo Term"

echo "=== $TEST_NAME Test ==="
echo "Starting Emacs..."

RUST_LOG=neomacs_display=debug DISPLAY=:0 ./src/emacs -Q \
    -l test/neomacs/neo-term-test.el 2>"$LOG" &
EMACS_PID=$!

echo "Emacs PID: $EMACS_PID"
echo "Waiting for window to appear..."
sleep 5

# Find emacs window
WIN_ID=$(DISPLAY=:0 xdotool search --name "emacs" 2>/dev/null | head -1)
if [ -z "$WIN_ID" ]; then
    echo "ERROR: Could not find Emacs window"
    kill $EMACS_PID 2>/dev/null || true
    exit 1
fi

echo "Found window: $WIN_ID"
DISPLAY=:0 xdotool getwindowgeometry "$WIN_ID"

# Activate window
echo "Activating window..."
DISPLAY=:0 xdotool windowactivate --sync "$WIN_ID"
sleep 1

# Wait for terminal creation and initialization
echo ""
echo "=== Waiting 8 seconds for terminal creation and initialization ==="
sleep 8

# Take screenshot
echo ""
echo "=== Taking screenshot ==="
if command -v import &>/dev/null; then
    DISPLAY=:0 import -window "$WIN_ID" "$SCREENSHOT" 2>/dev/null || true
    if [ -f "$SCREENSHOT" ]; then
        echo "Screenshot saved: $SCREENSHOT"
    else
        echo "Screenshot capture failed (non-fatal)"
    fi
else
    echo "Skipping screenshot (ImageMagick 'import' not available)"
fi

# Check logs for errors
echo ""
echo "=== Checking log entries ==="
if [ -f "$LOG" ]; then
    PANIC_COUNT=$(grep -ci "panic" "$LOG" 2>/dev/null || echo "0")
    ERROR_COUNT=$(grep -ci "error" "$LOG" 2>/dev/null || echo "0")

    if [ "$PANIC_COUNT" -gt 0 ]; then
        echo "WARNING: $PANIC_COUNT PANIC entries found!"
        grep -i "panic" "$LOG" | tail -5
    else
        echo "No PANIC entries detected."
    fi

    if [ "$ERROR_COUNT" -gt 0 ]; then
        echo "WARNING: $ERROR_COUNT ERROR entries found:"
        grep -i "error" "$LOG" | tail -10
    else
        echo "No ERROR entries detected."
    fi
else
    echo "Log file not found."
fi

# Cleanup
echo ""
echo "Stopping Emacs..."
kill $EMACS_PID 2>/dev/null || true
wait $EMACS_PID 2>/dev/null || true

# Summary
echo ""
echo "=== $TEST_NAME Test Summary ==="
if [ "$PANIC_COUNT" -gt 0 ]; then
    echo "RESULT: PANICS DETECTED - check log"
elif [ "$ERROR_COUNT" -gt 0 ]; then
    echo "RESULT: ERRORS DETECTED - check log for details"
else
    echo "RESULT: No panics or errors detected"
fi
echo "Full log at: $LOG"
[ -f "$SCREENSHOT" ] && echo "Screenshot at: $SCREENSHOT"
