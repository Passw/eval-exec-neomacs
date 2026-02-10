#!/usr/bin/env bash
# Test real mouse clicks on WebKit view using xdotool
# Usage: ./test/neomacs/run-xdotool-click-test.sh

set -e

cd "$(dirname "$0")/../.."

echo "=== WebKit XDotool Click Test ==="
echo "Starting Emacs with WebKit view..."

# Start Emacs and capture stderr to a log file
RUST_LOG=debug DISPLAY=:0 ./src/emacs -Q -l test/neomacs/webkit-xdotool-test.el 2>/tmp/webkit-click-test.log &
EMACS_PID=$!

echo "Emacs PID: $EMACS_PID"
echo "Waiting for window to appear..."
sleep 6

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

# First click - center of webkit view area (around y=200 to account for header text)
echo ""
echo "=== Click 1: Center of webkit view (300, 200) ==="
DISPLAY=:0 xdotool mousemove --window "$WIN_ID" 300 200
sleep 0.3
DISPLAY=:0 xdotool click 1
echo "Click sent!"
sleep 2

# Second click - "About" link position (top-left of webkit view)
# Header text ends around y=100, webkit view starts there
# "About" is at approximately (77, 172) in window coordinates
echo ""
echo "=== Click 2: About link (77, 172) ==="
DISPLAY=:0 xdotool mousemove --window "$WIN_ID" 77 172
sleep 0.3
DISPLAY=:0 xdotool click 1
echo "Click sent!"
sleep 3

# Check logs for click events
echo ""
echo "=== Checking for click events in log ==="
grep -E "(Pointer button|Click at|button press|button release)" /tmp/webkit-click-test.log | tail -20 || echo "No click events found"

echo ""
echo "=== Checking for navigation ==="
grep -E "load_changed|about\.google" /tmp/webkit-click-test.log | tail -10 || echo "No navigation events found"

sleep 30
# Cleanup
echo ""
echo "Stopping Emacs..."
kill $EMACS_PID 2>/dev/null || true
wait $EMACS_PID 2>/dev/null || true

echo ""
echo "=== Test complete ==="
echo "Full log at: /tmp/webkit-click-test.log"
