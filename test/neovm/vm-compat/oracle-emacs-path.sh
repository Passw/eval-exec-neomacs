#!/usr/bin/env bash

# Central Oracle Emacs executable path used by the compatibility harness.
hardcoded_oracle_emacs="/nix/store/hql3zwz5b4ywd2qwx8jssp4dyb7nx4cb-emacs-30.2/bin/emacs"
FORCED_ORACLE_EMACS="${NEOVM_FORCE_ORACLE_PATH:-}"

if [[ -n "$FORCED_ORACLE_EMACS" ]]; then
    # Explicit override is intentionally verbose and opt-in for debugging.
    # Keep this unset in normal CI to preserve deterministic compatibility.
    oracle_emacs_path="$FORCED_ORACLE_EMACS"
else
    oracle_emacs_path="$hardcoded_oracle_emacs"
fi
