#!/usr/bin/env bash
# Take a screenshot of the webkit view
cd "$(dirname "$0")/../.."

echo "Starting Emacs with WebKit..."
RUST_LOG=warn DISPLAY=:0 ./src/emacs -Q -l test/neomacs/webkit-xdotool-test.el &
EMACS_PID=$!

sleep 8

WIN_ID=$(DISPLAY=:0 xdotool search --name "emacs" 2>/dev/null | head -1)
echo "Window ID: $WIN_ID"

if [ -n "$WIN_ID" ]; then
    DISPLAY=:0 import -window "$WIN_ID" /tmp/webkit-screenshot.png
    echo "Screenshot saved to /tmp/webkit-screenshot.png"
    ls -la /tmp/webkit-screenshot.png
else
    echo "Could not find window"
fi

kill $EMACS_PID 2>/dev/null || true
