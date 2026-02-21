/* Calculate what line insertion or deletion to do, and do it

Copyright (C) 1985-1986, 1990, 1993-1994, 2001-2026 Free Software
Foundation, Inc.

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


#include <config.h>

/* The entire file is defined out under Android, where there is no
   text terminal support of any kind.  */

#ifndef HAVE_ANDROID

#include "lisp.h"
#include "termchar.h"
#include "dispextern.h"
#include "frame.h"
#include "termhooks.h"

/* Calculate the line insertion/deletion
   overhead and multiply factor values */

static void
line_ins_del (struct frame *frame, int ov1, int pf1, int ovn, int pfn,
              int *ov, int *mf)
{
  int i;
  int frame_total_lines = FRAME_TOTAL_LINES (frame);
  int insert_overhead = ov1 * 10;
  int next_insert_cost = ovn * 10;

  for (i = frame_total_lines - 1; i >= 0; i--)
    {
      mf[i] = next_insert_cost / 10;
      next_insert_cost += pfn;
      ov[i] = (insert_overhead + next_insert_cost) / 10;
      insert_overhead += pf1;
    }
}

static void
ins_del_costs (struct frame *frame,
	       const char *one_line_string, const char *multi_string,
	       const char *setup_string, const char *cleanup_string,
	       int *costvec, int *ncostvec,
	       int coefficient)
{
  if (multi_string)
    line_ins_del (frame,
		  string_cost (multi_string) * coefficient,
		  per_line_cost (multi_string) * coefficient,
		  0, 0, costvec, ncostvec);
  else if (one_line_string)
    line_ins_del (frame,
		  string_cost (setup_string) + string_cost (cleanup_string), 0,
		  string_cost (one_line_string),
		  per_line_cost (one_line_string),
		  costvec, ncostvec);
  else
    line_ins_del (frame,
		  9999, 0, 9999, 0,
		  costvec, ncostvec);
}

/* Calculate the insert and delete line costs.
   Note that this is done even when running with a window system
   because we want to know how long scrolling takes (and avoid it).
   This must be redone whenever the frame height changes.

   We keep the ID costs in a precomputed array based on the position
   at which the I or D is performed.  Also, there are two kinds of ID
   costs: the "once-only" and the "repeated".  This is to handle both
   those terminals that are able to insert N lines at a time (once-
   only) and those that must repeatedly insert one line.

   The cost to insert N lines at line L is
	    [tt.t_ILov  + (frame_total_lines + 1 - L) * tt.t_ILpf] +
	N * [tt.t_ILnov + (frame_total_lines + 1 - L) * tt.t_ILnpf]

   ILov represents the basic insert line overhead.  ILpf is the padding
   required to allow the terminal time to move a line: insertion at line
   L changes (frame_total_lines + 1 - L) lines.

   The first bracketed expression above is the overhead; the second is
   the multiply factor.  Both are dependent only on the position at
   which the insert is performed.  We store the overhead in
   FRAME_INSERT_COST (frame) and the multiply factor in
   FRAME_INSERTN_COST (frame).  Note however that any insertion
   must include at least one multiply factor.  Rather than compute this
   as FRAME_INSERT_COST (frame)[line]+FRAME_INSERTN_COST (frame)[line],
   we add FRAME_INSERTN_COST (frame) into FRAME_INSERT_COST (frame).
   This is reasonable because of the particular algorithm used in calcM.

   Deletion is essentially the same as insertion.
 */

void
do_line_insertion_deletion_costs (struct frame *frame,
				  const char *ins_line_string,
				  const char *multi_ins_string,
				  const char *del_line_string,
				  const char *multi_del_string,
				  const char *setup_string,
				  const char *cleanup_string,
				  int coefficient)
{
  int frame_total_lines = FRAME_TOTAL_LINES (frame);
  FRAME_INSERT_COST (frame) =
    xnrealloc (FRAME_INSERT_COST (frame), frame_total_lines, sizeof (int));
  FRAME_DELETEN_COST (frame) =
    xnrealloc (FRAME_DELETEN_COST (frame), frame_total_lines, sizeof (int));
  FRAME_INSERTN_COST (frame) =
    xnrealloc (FRAME_INSERTN_COST (frame), frame_total_lines, sizeof (int));
  FRAME_DELETE_COST (frame) =
    xnrealloc (FRAME_DELETE_COST (frame), frame_total_lines, sizeof (int));

  ins_del_costs (frame,
		 ins_line_string, multi_ins_string,
		 setup_string, cleanup_string,
		 FRAME_INSERT_COST (frame), FRAME_INSERTN_COST (frame),
		 coefficient);
  ins_del_costs (frame,
		 del_line_string, multi_del_string,
		 setup_string, cleanup_string,
		 FRAME_DELETE_COST (frame), FRAME_DELETEN_COST (frame),
		 coefficient);
}

#endif
