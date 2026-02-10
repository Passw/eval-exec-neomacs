#!/usr/bin/env bash
# Test inline video rendering in Neomacs
#
# This script:
# 1. Launches neomacs with video test
# 2. Captures GPU rendering logs
# 3. Takes a screenshot

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NEOMACS_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
EMACS_BIN="$NEOMACS_ROOT/src/emacs"
TEST_EL="$SCRIPT_DIR/neomacs-video-test.el"
LOG_FILE="/tmp/neomacs-video-test-$$.log"
SCREENSHOT_FILE="/tmp/neomacs-video-test-screenshot-$$.png"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=== Neomacs Video Rendering Test ==="
echo ""

# Check emacs binary
if [[ ! -x "$EMACS_BIN" ]]; then
    echo -e "${RED}ERROR: Emacs binary not found at $EMACS_BIN${NC}"
    echo "Please build neomacs first: make -j8"
    exit 1
fi

# Check test file
if [[ ! -f "$TEST_EL" ]]; then
    echo -e "${RED}ERROR: Test elisp file not found at $TEST_EL${NC}"
    exit 1
fi

# Check for test video
VIDEO_FILE="/home/exec/Videos/4k_f1.mp4"
if [[ ! -f "$VIDEO_FILE" ]]; then
    echo -e "${YELLOW}WARNING: Test video not found at $VIDEO_FILE${NC}"
    echo "Will try with fallback video..."
fi

echo "Emacs binary: $EMACS_BIN"
echo "Test file: $TEST_EL"
echo "Log file: $LOG_FILE"
echo ""

# Enable video debug logs
export RUST_LOG="neomacs_display::backend::wgpu::video_cache=debug,neomacs_display::backend::wgpu::vulkan_dmabuf=debug,info"

echo "Running test with RUST_LOG=$RUST_LOG"
echo ""

# Run emacs and capture logs
timeout 15 "$EMACS_BIN" -Q \
    --eval "(setq inhibit-startup-screen t)" \
    -l "$TEST_EL" \
    2>&1 | tee "$LOG_FILE" &

EMACS_PID=$!

# Wait for window to appear
sleep 3

# Take screenshot
if command -v import &> /dev/null && [[ -n "$DISPLAY" ]]; then
    echo "Taking screenshot..."
    WINDOW_ID=$(xdotool search --name "emacs" 2>/dev/null | head -1 || true)
    if [[ -n "$WINDOW_ID" ]]; then
        import -window "$WINDOW_ID" "$SCREENSHOT_FILE" 2>/dev/null || true
        if [[ -f "$SCREENSHOT_FILE" ]]; then
            echo -e "${GREEN}Screenshot saved: $SCREENSHOT_FILE${NC}"
        fi
    fi
fi

# Wait for emacs to finish
wait $EMACS_PID 2>/dev/null || true

echo ""
echo "=== Analyzing logs ==="
echo ""

# Check for video-related logs
VIDEO_SUCCESS=false

if grep -q "VideoCache" "$LOG_FILE" 2>/dev/null; then
    echo -e "${GREEN}[INFO] VideoCache activity detected${NC}"
    VIDEO_SUCCESS=true
fi

if grep -q "GStreamer" "$LOG_FILE" 2>/dev/null; then
    echo -e "${GREEN}[INFO] GStreamer pipeline created${NC}"
    VIDEO_SUCCESS=true
fi

if grep -q "video.*load\|load.*video" "$LOG_FILE" 2>/dev/null; then
    echo -e "${GREEN}[INFO] Video loading detected${NC}"
    VIDEO_SUCCESS=true
fi

if grep -q "DMA-BUF\|dmabuf\|DmaBuf" "$LOG_FILE" 2>/dev/null; then
    echo -e "${GREEN}[INFO] DMA-BUF activity detected${NC}"
fi

# Check for errors
if grep -qi "error\|failed\|panic" "$LOG_FILE" 2>/dev/null; then
    echo -e "${YELLOW}[WARN] Some errors in log:${NC}"
    grep -i "error\|failed\|panic" "$LOG_FILE" | head -10
fi

echo ""
echo "=== Summary ==="

if [[ "$VIDEO_SUCCESS" == "true" ]]; then
    echo -e "${GREEN}VIDEO TEST: Activity detected${NC}"
else
    echo -e "${YELLOW}VIDEO TEST: No video activity in logs${NC}"
    echo "This might be because:"
    echo "  - Video FFI functions are still stubs"
    echo "  - GStreamer not properly initialized"
    echo "  - Video feature not fully integrated yet"
fi

echo ""
echo "Full log saved to: $LOG_FILE"
if [[ -f "$SCREENSHOT_FILE" ]]; then
    echo "Screenshot saved to: $SCREENSHOT_FILE"
fi

# Show relevant log lines
echo ""
echo "=== Relevant log lines ==="
grep -E "(video|Video|gstreamer|GStreamer|DMA|dmabuf)" "$LOG_FILE" 2>/dev/null | head -20 || echo "(no video-related output)"

exit 0
