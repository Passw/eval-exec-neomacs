#!/usr/bin/env bash
# Run the font family spacing test
# Usage: ./test/neomacs/run-font-family-test.sh
#
# This opens a buffer showing bold/italic text in many font families.
# Check that character spacing is identical for normal, bold, and italic.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EMACS="${EMACS:-./src/emacs}"

exec "$EMACS" -Q -l "$SCRIPT_DIR/font-family-test.el"
