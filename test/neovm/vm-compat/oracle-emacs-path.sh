#!/usr/bin/env bash

# Central Oracle Emacs executable path used by the compatibility harness.
hardcoded_oracle_emacs="/nix/store/hql3zwz5b4ywd2qwx8jssp4dyb7nx4cb-emacs-30.2/bin/emacs"
FORCED_ORACLE_EMACS="${NEOVM_FORCE_ORACLE_PATH:-}"

if [[ -n "$FORCED_ORACLE_EMACS" ]]; then
    # Explicit override is intentionally verbose and opt-in for debugging.
    # Keep this unset in normal CI to preserve deterministic compatibility.
    oracle_emacs_path="$FORCED_ORACLE_EMACS"
elif [[ -x "$hardcoded_oracle_emacs" ]]; then
    oracle_emacs_path="$hardcoded_oracle_emacs"
else
    # Local-dev fallback when the pinned oracle store path is absent.
    # Prefer packaged builds first, then any GNU Emacs store entry.
    for candidate in /nix/store/*-emacs-git-with-packages-*/bin/emacs \
        /nix/store/*-emacs-git-*/bin/emacs \
        /nix/store/*-emacs-*/bin/emacs; do
        if [[ -x "$candidate" ]]; then
            oracle_emacs_path="$candidate"
            break
        fi
    done
    oracle_emacs_path="${oracle_emacs_path:-$hardcoded_oracle_emacs}"
fi
