# Emacs Frame and Window Layout

This document describes the official Emacs frame and window layout structure.
The neomacs-display renderer must align with these layouts for correct rendering.

Reference: [GNU Emacs Lisp Reference Manual - Frame Layout](https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html)

## Frame Layout

From `doc/lispref/frames.texi`:

```
        <------------ Outer Frame Width ----------->
        ____________________________________________
     ^(0)  ________ External/Outer Border _______   |
     | |  |_____________ Title Bar ______________|  |
     | | (1)_____________ Menu Bar ______________|  | ^
     | | (2)_____________ Tool Bar ______________|  | ^
     | | (3)_____________ Tab Bar _______________|  | ^
     | |  |  _________ Internal Border ________  |  | ^
     | |  | |   ^                              | |  | |
     | |  | |   |                              | |  | |
Outer  |  | | Inner                            | |  | Native
Frame  |  | | Frame                            | |  | Frame
Height |  | | Height                           | |  | Height
     | |  | |   |                              | |  | |
     | |  | |<--+--- Inner Frame Width ------->| |  | |
     | |  | |   |                              | |  | |
     | |  | |___v______________________________| |  | |
     | |  |___________ Internal Border __________|  | |
     | | (4)__________ Bottom Tool Bar __________|  | v
     v |___________ External/Outer Border __________|
           <-------- Native Frame Width -------->
```

### Frame Components

| Component | Description |
|-----------|-------------|
| **Outer Frame** | Rectangle comprising all areas; edges are "outer edges" |
| **External Border** | Window manager decoration for resizing |
| **Outer Border** | Separate border with configurable width (`border-width` parameter) |
| **Title Bar** | Window manager decoration showing frame title |
| **Menu Bar** | Can be internal (Emacs) or external (toolkit) |
| **Tool Bar** | Can be internal or external; position varies by toolkit |
| **Tab Bar** | Always drawn by Emacs |
| **Native Frame** | Rectangle within outer frame, excludes WM decorations |
| **Internal Border** | Border drawn by Emacs around inner frame |
| **Inner Frame** | Rectangle reserved for windows (also called "display area") |
| **Text Area** | Fictitious area for calculating text dimensions |

### Native Position Variants

- **(1)** non-toolkit, Android, Haiku, and terminal frames
- **(2)** Lucid, Motif, and MS-Windows frames
- **(3)** GTK+ and NS frames
- **(4)** When tool bar is at bottom

## Window Layout

From `doc/lispref/windows.texi`:

```
        ____________________________________________
       |________________ Tab Line _______________|RD| ^
       |______________ Header Line ______________|  | |
     ^ |LS|LM|LF|                       |RF|RM|RS|  | |
     | |  |  |  |                       |  |  |  |  | |
Window |  |  |  |                       |  |  |  |  | Window
Body | |  |  |  |      Window Body      |  |  |  |  | Total
Height |  |  |  |                       |  |  |  |  | Height
     | |  |  |  |<- Window Body Width ->|  |  |  |  | |
     v |__|__|__|_______________________|__|__|__|  | |
       |_________ Horizontal Scroll Bar _________|  | |
       |_______________ Mode Line _______________|__| |
       |_____________ Bottom Divider _______________| v
        <---------- Window Total Width ------------>
```

### Window Components Legend

| Symbol | Component | Description |
|--------|-----------|-------------|
| **LS** | Left Scroll Bar | Vertical scroll bar (left side) |
| **RS** | Right Scroll Bar | Vertical scroll bar (right side) |
| **LM** | Left Margin | Display margin on left |
| **RM** | Right Margin | Display margin on right |
| **LF** | Left Fringe | Fringe area on left (continuation/truncation glyphs) |
| **RF** | Right Fringe | Fringe area on right |
| **RD** | Right Divider | Divider between windows |

### Window Areas

| Area | Description |
|------|-------------|
| **Tab Line** | Tab bar within window (top) |
| **Header Line** | Header line (below tab line) |
| **Window Body** | Main text display area |
| **Horizontal Scroll Bar** | Horizontal scrolling (bottom) |
| **Mode Line** | Status line showing buffer info |
| **Bottom Divider** | Divider below mode line |

## Horizontal Window Layout

From `src/window.h`:

```
   LEFT_EDGE_COL         RIGHT_EDGE_COL
   |                                  |
   v                                  v
   <-><-><---><-----------><---><-><->
    ^  ^   ^        ^        ^   ^  ^
    |  |   |        |        |   |  +-- RIGHT_SCROLL_BAR_COLS
    |  |   |        |        |   +----- RIGHT_FRINGE_WIDTH
    |  |   |        |        +--------- RIGHT_MARGIN_COLS
    |  |   |        +------------------ TEXT_AREA_COLS
    |  |   +--------------------------- LEFT_MARGIN_COLS
    |  +------------------------------- LEFT_FRINGE_WIDTH
    +---------------------------------- LEFT_SCROLL_BAR_COLS
```

## Key Implementation Notes

### Window Height Includes Mode Line

`WINDOW_PIXEL_HEIGHT(W)` **includes** the mode line height. This is critical:

```c
/* Return the pixel height of window W.  This includes dividers, scroll
   bars, header and mode lines, if any.  */
#define WINDOW_PIXEL_HEIGHT(W) (W)->pixel_height
```

To get height without mode line:
```c
#define WINDOW_BOX_HEIGHT_NO_MODE_LINE(W)  \
  (WINDOW_PIXEL_HEIGHT (W)                 \
   - WINDOW_BOTTOM_DIVIDER_WIDTH (W)       \
   - WINDOW_SCROLL_BAR_AREA_HEIGHT (W)     \
   - WINDOW_MODE_LINE_HEIGHT (W))
```

### Mode Line Position

The mode line is at the bottom of the window, above the bottom divider:

```c
/* Return the canonical frame line before which window W ends.
   This includes a mode line, if any.  */
#define WINDOW_BOTTOM_EDGE_LINE(W) \
  (WINDOW_TOP_EDGE_LINE (W) + WINDOW_TOTAL_LINES (W))
```

### Glyph Row Areas

Each glyph row has three areas (from `dispextern.h`):

```
   +--------------------+-------------+---------------------+
   |                    |             |                     |
   glyphs[LEFT_MARGIN_AREA]           glyphs[RIGHT_MARGIN_AREA]
                        |                                   |
                        glyphs[TEXT_AREA]                   |
                                              glyphs[LAST_AREA]
```

### Mode Line Row

The mode line is stored as the last row in the glyph matrix:

```c
/* Return a pointer to the row reserved for the mode line in MATRIX.
   Row MATRIX->nrows - 1 is always reserved for the mode line.  */
#define MATRIX_MODE_LINE_ROW(MATRIX) \
  ((MATRIX)->rows + (MATRIX)->nrows - 1)
```

## Implications for neomacs-display Renderer

1. **Window bounds include mode-line**: When `neomacs_display_add_window` is called,
   the window's height includes the mode-line area.

2. **Mode-line is part of window**: Mode-line characters are drawn within the
   window's coordinate space, not as a separate overlay.

3. **Background coverage**: The window's background rectangle should cover the
   entire window including the mode-line area. Mode-line character backgrounds
   are then drawn on top.

4. **Coordinate system**: All positions are relative to the frame's native
   position (after internal border).

5. **Row structure**: Each text row and the mode-line row have the same
   horizontal structure (margins, fringes, text area).
