/* Functions to manipulate menus.
   Copyright (C) 2008-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef MENU_H
#define MENU_H

/* Bit fields used by terminal-specific menu_show_hook.  */

enum {
  MENU_KEYMAPS = 0x1,
  MENU_FOR_CLICK = 0x2,
  MENU_KBD_NAVIGATION = 0x4
};

extern void init_menu_items (void);
extern void finish_menu_items (void);
extern void discard_menu_items (void);
extern void save_menu_items (void);
extern bool parse_single_submenu (Lisp_Object, Lisp_Object, Lisp_Object);
extern void list_of_panes (Lisp_Object);


extern Lisp_Object tty_menu_show (struct frame *, int, int, int,
				  Lisp_Object, const char **);
extern ptrdiff_t menu_item_width (const unsigned char *);
extern Lisp_Object x_popup_menu_1 (Lisp_Object position, Lisp_Object menu);
#endif /* MENU_H */
