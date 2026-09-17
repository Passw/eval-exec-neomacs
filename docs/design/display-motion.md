# Display motion and viewport ownership

Interactive motion and redisplay must interpret the same display rows. A
buffer newline is not necessarily a screen-line boundary: invisibility can
remove it, replacement strings can add several rows at one source position,
and an image can occupy more pixels than the entire window body.

GNU's reference implementations are `Fvertical_motion` in `src/indent.c`,
`move_it_by_lines` in `src/xdisp.c`, and `window_scroll_pixel_based` in
`src/window.c`. GNU deliberately uses a different `compute_motion` engine
in batch mode; this design does not erase that distinction.

## Ownership

```text
Lisp motion / scrolling command                        VM thread
  │
  ├─ xdisp/motion: source motion and pixel-page decisions
  │    └─ WindowLayoutQueryScope::Rows { start, count }
  │         └─ existing layout-query adapter
  │              └─ canonical layout engine → owned row geometry
  │
  └─ WindowScrollUpdate: commit point + start + pixel offset
       └─ ordinary redisplay → immutable presentation → renderer
```

- `neovm-core/emacs_core/display/xdisp/motion` owns motion semantics.
  `measurement.rs` owns safe source backtracking and expanding row coverage;
  `paging.rs` plans graphical scrolling; `policy.rs` captures Lisp scrolling
  preferences. The existing `indent` and `window_cmds` subsystems retain their
  Lisp builtin registrations.
- `neovm-core/window` owns the typed query boundary, freshness witnesses,
  marker-backed viewport update, and geometry returned to core consumers.
- `neomacs-layout-engine` remains the sole interactive row producer. It
  interprets fonts, overlays, replacements, wrapping and image metrics for
  both measurement and presentation. It does not decide scroll policy.
- Runtime wiring installs the existing synchronous query adapter. `Context`
  and any Lisp fontification callbacks stay on the VM thread. Neither the
  renderer nor a platform event thread may evaluate Lisp.

Native GUI and browser use these same core and layout paths. This does not
require a platform-specific scrolling algorithm, feature flag or extra crate.

## Measurement contract

`WindowLayoutQueryScope` distinguishes a viewport query from a row-bounded
measurement. `Rows` takes a one-based source position and a nonzero row count.
It projects its start and budget into local layout parameters without changing
live window markers, publishing a presentation, or updating `window-end`.
Its accessible-region semantics preserve labeled restrictions.

Retained rows can answer a motion only when their canonical freshness witness
matches. Insufficient retained or newly measured coverage means measure more;
it is not evidence that the accessible buffer boundary was reached. The
resolver distinguishes completed motion, a proven accessible boundary and
insufficient rows. Coverage grows geometrically and checks for Lisp quit.
Errors from an installed row producer are errors, not permission to silently
switch to a less capable text scanner. Batch/startup without that adapter
retains the existing source-based path.

Mutable string prefixes are captured by value, not just Lisp object identity.
The shared `LayoutPrefixInputs` projection records effective buffer-local
`line-prefix` and `wrap-prefix` string bytes, multibyteness, identity and string
text-property revision. Redisplay skipping, retained-geometry freshness,
in-flight validation and incremental row reuse all include that projection.
Changing a prefix with `aset` or changing its text properties need not modify
the buffer itself, but can still change every row's source-to-pixel mapping.
Owned bytes are shared on clone; no mutable Lisp string is retained by this
projection. Stretch-space prefixes also capture the first direct operand of
each supported geometry property, using the same `DisplaySpaceKey` enum as
geometry evaluation. In-place plist changes therefore invalidate layout even
when the prefix cons retains its identity. Unknown keys and shadowed duplicate
operands do not affect this projection. Arithmetic pixel expressions, absolute
pixel lengths and scaled lengths are captured as an immutable token stream,
with an iterative, cycle-aware traversal. No live Lisp references are retained.
Image/resource expressions and mutations inside a string's property value remain
audit items, not a claim of complete mutable-Lisp-graph invalidation.

`LayoutInvisibilityInput` captures the effective buffer's ordered invisibility
membership, including each cons entry's category and ellipsis truthiness.
In-place `setcar`/`setcdr` changes therefore participate in both the shared
freshness projection and retained-row key. This follows GNU's identity-based
membership checks; it does not deep-copy category objects or interpret non-nil
ellipsis tails beyond their truthiness.

Source backtracking retreats past display/invisibility spans covering a
newline before measuring forward. Producer-owned row metadata distinguishes
buffer rows, replacement newlines, replacement wraps, and before/after overlay
strings. Several physical rows may share one source anchor; collapsing them
loses the actual motion distance.

Fontification and automatic composition use the measurement's requested row
extent, not the live viewport's height. Their source-coverage estimates remain
part of the layout engine; consumers must not duplicate display interpretation.

## Plan, validate, commit

Graphical pages use measured pixel heights and GNU's default-line-height page
quantization. Partial scrolling within a tall first row is a pixel offset,
not an invented buffer position. A candidate viewport also chooses a visible
point, respecting scroll margins and screen-position preservation policy.
Consecutive scroll commands retain a typed VM-owned pixel goal, so a short
intermediate row does not permanently lose the original column. A clipped
tall point row below ordinary text is promoted to window-start before its
contents are pixel-scrolled.

Both graphical and terminal scroll plans are speculative. Fontification can edit text, move markers or
change layout while a query runs. A fresh individual query is therefore not
enough: the entire plan compares the existing canonical input witness and
captured scroll policy before and after measurement. If either changes, the
plan (including any provisional error) is discarded and retried, with a
bounded convergence limit.

Only then does `WindowScrollUpdate` validate the target identity and accessible
positions and synchronously update point, window-start, old-point when needed,
vscroll and redisplay flags. No Lisp runs inside this commit. Terminal row
scrolling uses the same transaction and marker-update owners. The pixel goal
is committed with the viewport, not during speculative queries. It is reset
when creating/restoring a VM, not serialized as application data. Smooth pixel input retains its
existing separate input policy; it must not independently reinterpret rows.

## Regression boundary

`neomacs-layout-engine/src/engine_display_motion_test.rs` tests Lisp-visible
motion against the real layout-query engine, including offscreen replacement
and overlay rows, hidden/narrowed starts, tall rows, scrolling policy,
fontification edits, and offscreen composition. Keep compatibility expectations
grounded in GNU's interactive engine: setting Lisp `noninteractive` to nil in
GNU `--batch` does not switch its underlying C engine.

These tests establish the shared architecture and covered behavior, not full
equivalence with every GNU display iterator case (for example, arbitrary
positions inside replacement strings or every bidi/continuation combination).
Add those cases at this shared boundary, not in a browser-only workaround.
