/* Functions for image support on window system.

Copyright (C) 1989-2026 Free Software Foundation, Inc.

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

#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <unistd.h>
#include <stdlib.h>

/* Include this before including <setjmp.h> to work around bugs with
   older libpng; see Bug#17429.  */

#include <setjmp.h>

#include <math.h>
#include <stdint.h>
#include <c-ctype.h>
#include <flexmember.h>

#include "lisp.h"
#include "frame.h"
#include "process.h"
#include "window.h"
#include "buffer.h"
#include "dispextern.h"
#include "blockinput.h"
#include "sysstdio.h"
#include "systime.h"
#include <epaths.h>
#include "coding.h"
#include "termhooks.h"
#include "font.h"

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */


#define RGB_TO_ULONG(r, g, b) (((r) << 16) | ((g) << 8) | (b))
#define ARGB_TO_ULONG(a, r, g, b) (((a) << 24) | ((r) << 16) | ((g) << 8) | (b))
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)
#define RED16_FROM_ULONG(color)		(RED_FROM_ULONG (color) * 0x101)
#define GREEN16_FROM_ULONG(color)	(GREEN_FROM_ULONG (color) * 0x101)
#define BLUE16_FROM_ULONG(color)	(BLUE_FROM_ULONG (color) * 0x101)

#ifdef USE_CAIRO
#define GET_PIXEL image_pix_context_get_pixel
#define PUT_PIXEL image_pix_container_put_pixel
#define NO_PIXMAP 0

#define PIX_MASK_RETAIN	0
#define PIX_MASK_DRAW	255

static unsigned long image_alloc_image_color (struct frame *, struct image *,
					      Lisp_Object, unsigned long);
#endif	/* USE_CAIRO */



#define XBM_BIT_SHUFFLE(b) (b)




typedef struct neomacs_bitmap_record Bitmap_Record;
#include "neomacsterm.h"
#include "neomacs_display.h"
#define NLOG_MODULE "image"
#include "neomacs_log.h"




static void image_disable_image (struct frame *, struct image *);
static void image_edge_detection (struct frame *, struct image *, Lisp_Object,
                                  Lisp_Object);

static double image_compute_scale (struct frame *f, Lisp_Object spec,
				   struct image *img);

static void init_color_table (void);
static unsigned long lookup_rgb_color (struct frame *f, int r, int g, int b);
#ifdef COLOR_TABLE_SUPPORT
static void free_color_table (void);
static unsigned long *colors_in_color_table (int *n);
#endif



#ifdef USE_CAIRO

static Emacs_Pix_Container
image_create_pix_container (unsigned int width, unsigned int height,
			    unsigned int depth)
{
  Emacs_Pix_Container pimg;

  pimg = xmalloc (sizeof (*pimg));
  pimg->width = width;
  pimg->height = height;
  pimg->bits_per_pixel = depth == 1 ? 8 : 32;
  pimg->bytes_per_line = cairo_format_stride_for_width ((depth == 1
							 ? CAIRO_FORMAT_A8
							 : CAIRO_FORMAT_RGB24),
							width);
  pimg->data = xmalloc (pimg->bytes_per_line * height);

  return pimg;
}

static void
image_pix_container_put_pixel (Emacs_Pix_Container image,
			       int x, int y, unsigned long pixel)
{
  if (image->bits_per_pixel == 32)
    ((uint32_t *)(image->data + y * image->bytes_per_line))[x] = pixel;
  else
    ((uint8_t *)(image->data + y * image->bytes_per_line))[x] = pixel;
}

static unsigned long
image_pix_context_get_pixel (Emacs_Pix_Context image, int x, int y)
{
  if (image->bits_per_pixel == 32)
    return ((uint32_t *)(image->data + y * image->bytes_per_line))[x];
  else
    return ((uint8_t *)(image->data + y * image->bytes_per_line))[x];
}

static Emacs_Pix_Container
image_pix_container_create_from_bitmap_data (char *data, unsigned int width,
					     unsigned int height,
					     unsigned long fg,
					     unsigned long bg)
{
  Emacs_Pix_Container pimg = image_create_pix_container (width, height, 0);
  int bytes_per_line = (width + (CHAR_BIT - 1)) / CHAR_BIT;

  for (int y = 0; y < height; y++)
    {
      for (int x = 0; x < width; x++)
	PUT_PIXEL (pimg, x, y,
		   (data[x / CHAR_BIT] >> (x % CHAR_BIT)) & 1 ? fg : bg);
      data += bytes_per_line;
    }

  return pimg;
}


#endif	/* USE_CAIRO */


/* Code to deal with bitmaps.  Bitmaps are referenced by their bitmap
   id, which is just an int that this section returns.  Bitmaps are
   reference counted so they can be shared among frames.

   Bitmap indices are guaranteed to be > 0, so a negative number can
   be used to indicate no bitmap.

   If you use image_create_bitmap_from_data, then you must keep track
   of the bitmaps yourself.  That is, creating a bitmap from the same
   data more than once will not be caught.  */

/* Functions to access the contents of a bitmap, given an id.  */




/* Allocate a new bitmap record.  Returns index of new record.  */

static ptrdiff_t
image_allocate_bitmap_record (struct frame *f)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
  ptrdiff_t i;

  if (dpyinfo->bitmaps_last < dpyinfo->bitmaps_size)
    return ++dpyinfo->bitmaps_last;

  for (i = 0; i < dpyinfo->bitmaps_size; ++i)
    if (dpyinfo->bitmaps[i].refcount == 0)
      return i + 1;

  dpyinfo->bitmaps =
    xpalloc (dpyinfo->bitmaps, &dpyinfo->bitmaps_size,
	     10, -1, sizeof *dpyinfo->bitmaps);
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id ID.  */

void
image_reference_bitmap (struct frame *f, ptrdiff_t id)
{
  ++FRAME_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}


/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at BITS.  */

ptrdiff_t
image_create_bitmap_from_data (struct frame *f, char *bits,
                               unsigned int width, unsigned int height)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
  ptrdiff_t id;







  id = image_allocate_bitmap_record (f);



  {
    int bytes_per_row = (width + 7) / 8;
    int nbytes = bytes_per_row * height;
    dpyinfo->bitmaps[id - 1].depth = 1;
    dpyinfo->bitmaps[id - 1].pattern = NULL;
    dpyinfo->bitmaps[id - 1].stipple_bits = xmalloc (nbytes);
    memcpy (dpyinfo->bitmaps[id - 1].stipple_bits, bits, nbytes);
  }


  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  dpyinfo->bitmaps[id - 1].refcount = 1;



  return id;
}

typedef int image_fd;

static char *slurp_file (image_fd, ptrdiff_t *);
static Lisp_Object image_find_image_fd (Lisp_Object, image_fd *);
static bool xbm_read_bitmap_data (struct frame *, char *, char *,
				  int *, int *, char **, bool);

/* Create bitmap from file FILE for frame F.  */

ptrdiff_t
image_create_bitmap_from_file (struct frame *f, Lisp_Object file)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);




  ptrdiff_t id, size;
  int fd, width, height, rc;
  char *contents, *data;

  if (!STRINGP (image_find_image_fd (file, &fd)))
    return -1;

  contents = slurp_file (fd, &size);

  if (!contents)
    return -1;

  rc = xbm_read_bitmap_data (f, contents, contents + size,
			     &width, &height, &data, 0);

  if (!rc)
    {
      xfree (contents);
      return -1;
    }

  id = image_allocate_bitmap_record (f);

  {
    int bytes_per_row = (width + 7) / 8;
    int nbytes = bytes_per_row * height;
    dpyinfo->bitmaps[id - 1].stipple_bits = xmalloc (nbytes);
    memcpy (dpyinfo->bitmaps[id - 1].stipple_bits, data, nbytes);
  }
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file = xlispstrdup (file);
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].pattern = NULL;
  xfree (contents);
  xfree (data);
  return id;



}

/* Free bitmap B.  */

static void
free_bitmap_record (Display_Info *dpyinfo, Bitmap_Record *bm)
{






  if (bm->stipple_bits)
    {
      xfree (bm->stipple_bits);
      bm->stipple_bits = NULL;
    }

  if (bm->file)
    {
      xfree (bm->file);
      bm->file = NULL;
    }
}

/* Remove reference to bitmap with id number ID.  */

void
image_destroy_bitmap (struct frame *f, ptrdiff_t id)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (id > 0)
    {
      Bitmap_Record *bm = &dpyinfo->bitmaps[id - 1];

      if (--bm->refcount == 0)
	{
	  block_input ();
	  free_bitmap_record (dpyinfo, bm);
	  unblock_input ();
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

void
image_destroy_all_bitmaps (Display_Info *dpyinfo)
{
  ptrdiff_t i;
  Bitmap_Record *bm = dpyinfo->bitmaps;

  for (i = 0; i < dpyinfo->bitmaps_last; i++, bm++)
    if (bm->refcount > 0)
      free_bitmap_record (dpyinfo, bm);

  dpyinfo->bitmaps_last = 0;
}

/* Required for the definition of image_create_x_image_and_pixmap_1 below.  */
typedef void Picture;

static bool image_create_x_image_and_pixmap_1 (struct frame *, int, int, int,
                                               Emacs_Pix_Container *,
                                               Emacs_Pixmap *, Picture *);
static void image_destroy_x_image (Emacs_Pix_Container);

static Emacs_Pix_Container image_get_x_image (struct frame *, struct image *,
                                              bool);
static void image_unget_x_image (struct image *, bool, Emacs_Pix_Container);
#define image_get_x_image_or_dc(f, img, mask_p, dummy)	\
  image_get_x_image (f, img, mask_p)
#define image_unget_x_image_or_dc(img, mask_p, ximg, dummy)	\
  image_unget_x_image (img, mask_p, ximg)


/***********************************************************************
			    Image types
 ***********************************************************************/

/* Each image format (JPEG, TIFF, ...) supported is described by
   a structure of the type below.  */

struct image_type
{
  /* Index of a symbol uniquely identifying the image type, e.g., 'jpeg'.  */
  int type;

  /* Check that SPEC is a valid image specification for the given
     image type.  Value is true if SPEC is valid.  */
  bool (*valid_p) (Lisp_Object spec);

  /* Load IMG which is to be used on frame F from information contained
     in IMG->spec.  Value is true if successful.  */
  bool (*load_img) (struct frame *f, struct image *img);

  /* Free such resources of image IMG as are used on frame F.  */
  void (*free_img) (struct frame *f, struct image *img);
};

/* Forward function prototypes.  */

static struct image_type const *lookup_image_type (Lisp_Object);
static void image_laplace (struct frame *, struct image *);
static void image_emboss (struct frame *, struct image *);
static void image_build_heuristic_mask (struct frame *, struct image *,
                                    Lisp_Object);

static void
add_image_type (Lisp_Object type)
{
  Vimage_types = Fcons (type, Vimage_types);
}


/* Value is true if OBJECT is a valid Lisp image specification.  A
   valid image specification is a list whose car is the symbol
   `image', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported image type.  The rest of the property list depends on the
   image type.  */

bool
valid_image_p (Lisp_Object object)
{
  if (IMAGEP (object))
    {
      Lisp_Object tail = XCDR (object);
      FOR_EACH_TAIL_SAFE (tail)
	{
	  if (EQ (XCAR (tail), QCtype))
	    {
	      tail = XCDR (tail);
	      if (CONSP (tail))
		{
		  struct image_type const *type =
		    lookup_image_type (XCAR (tail));
		  if (type)
		    return type->valid_p (object);
		}
	      break;
	    }
	  tail = XCDR (tail);
	  if (! CONSP (tail))
	    return false;
	}
    }

  return false;
}

/* Log error message with format string FORMAT and trailing arguments.
   Signaling an error, e.g. when an image cannot be loaded, is not a
   good idea because this would interrupt redisplay, and the error
   message display would lead to another redisplay.  This function
   therefore simply displays a message.  */

static void
image_error (const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  vadd_to_log (format, ap);
  va_end (ap);
}

static void
image_invalid_data_error (Lisp_Object data)
{
  image_error ("Invalid image data `%s'", data);
}

static void
image_not_found_error (Lisp_Object filename)
{
  image_error ("Cannot find image file `%s'", filename);
}

static void
image_size_error (void)
{
  image_error ("Invalid image size (see `max-image-size')");
}


/***********************************************************************
			 Image specifications
 ***********************************************************************/

enum image_value_type
{
  IMAGE_DONT_CHECK_VALUE_TYPE,
  IMAGE_STRING_VALUE,
  IMAGE_STRING_OR_NIL_VALUE,
  IMAGE_SYMBOL_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE,
  IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR,
  IMAGE_NON_NEGATIVE_INTEGER_VALUE,
  IMAGE_ASCENT_VALUE,
  IMAGE_INTEGER_VALUE,
  IMAGE_FUNCTION_VALUE,
  IMAGE_NUMBER_VALUE,
  IMAGE_BOOL_VALUE
};

/* Structure used when parsing image specifications.  */

struct image_keyword
{
  /* Name of keyword.  */
  const char *name;

  /* The type of value allowed.  */
  enum image_value_type type;

  /* True means key must be present.  */
  bool mandatory_p;

  /* True means key is present.
     Also used to recognize duplicate keywords in a property list.  */
  bool count;

  /* The value that was found.  */
  Lisp_Object value;
};


/* Parse image spec SPEC according to KEYWORDS.  A valid image spec
   has the format (image KEYWORD VALUE ...).  One of the keyword/
   value pairs must be `:type TYPE'.  KEYWORDS is a vector of
   image_keywords structures of size NKEYWORDS describing other
   allowed keyword/value pairs.  Value is true if SPEC is valid.  */

static bool
parse_image_spec (Lisp_Object spec, struct image_keyword *keywords,
		  int nkeywords, Lisp_Object type)
{
  int i;
  Lisp_Object plist;

  if (!IMAGEP (spec))
    return false;

  plist = XCDR (spec);
  FOR_EACH_TAIL_SAFE (plist)
    {
      Lisp_Object key, value;

      /* First element of a pair must be a symbol.  */
      key = XCAR (plist);
      plist = XCDR (plist);
      if (!SYMBOLP (key))
	return false;

      /* There must follow a value.  */
      if (!CONSP (plist))
	return false;
      value = XCAR (plist);

      /* Find key in KEYWORDS.  Error if not found.  */
      for (i = 0; i < nkeywords; ++i)
	if (strcmp (keywords[i].name, SSDATA (SYMBOL_NAME (key))) == 0)
	  break;

      if (i == nkeywords)
	goto maybe_done;

      /* Record that we recognized the keyword.  If a keyword
	 was found more than once, it's an error.  */
      keywords[i].value = value;
      if (keywords[i].count)
	return false;
      keywords[i].count = true;

      /* Check type of value against allowed type.  */
      switch (keywords[i].type)
	{
	case IMAGE_STRING_VALUE:
	  if (!STRINGP (value))
	    return false;
	  break;

	case IMAGE_STRING_OR_NIL_VALUE:
	  if (!STRINGP (value) && !NILP (value))
	    return false;
	  break;

	case IMAGE_SYMBOL_VALUE:
	  if (!SYMBOLP (value))
	    return false;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE:
	  if (! RANGED_FIXNUMP (1, value, INT_MAX))
	    return false;
	  break;

	case IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR:
	  if (RANGED_FIXNUMP (0, value, INT_MAX))
	    break;
	  if (CONSP (value)
	      && RANGED_FIXNUMP (0, XCAR (value), INT_MAX)
	      && RANGED_FIXNUMP (0, XCDR (value), INT_MAX))
	    break;
	  return false;

	case IMAGE_ASCENT_VALUE:
	  if (SYMBOLP (value) && EQ (value, Qcenter))
	    break;
	  else if (RANGED_FIXNUMP (0, value, 100))
	    break;
	  return false;

	case IMAGE_NON_NEGATIVE_INTEGER_VALUE:
	  /* Unlike the other integer-related cases, this one does not
	     verify that VALUE fits in 'int'.  This is because callers
	     want EMACS_INT.  */
	  if (!FIXNATP (value))
	    return false;
	  break;

	case IMAGE_DONT_CHECK_VALUE_TYPE:
	  break;

	case IMAGE_FUNCTION_VALUE:
	  value = indirect_function (value);
	  if (FUNCTIONP (value))
	    break;
	  return false;

	case IMAGE_NUMBER_VALUE:
	  if (! NUMBERP (value))
	    return false;
	  break;

	case IMAGE_INTEGER_VALUE:
	  if (! TYPE_RANGED_FIXNUMP (int, value))
	    return false;
	  break;

	case IMAGE_BOOL_VALUE:
	  if (!NILP (value) && !EQ (value, Qt))
	    return false;
	  break;

	default:
	  emacs_abort ();
	  break;
	}

      if (EQ (key, QCtype)
	  && !EQ (type, value))
	return false;

    maybe_done:
      if (NILP (XCDR (plist)))
	{
	  /* Check that all mandatory fields are present.  */
	  for (i = 0; i < nkeywords; ++i)
	    if (keywords[i].mandatory_p && keywords[i].count == 0)
	      return false;

	  return true;
	}
    }

  return false;
}


/* Return the value of KEY in image specification SPEC.  Value is nil
   if KEY is not present in SPEC.  Set *FOUND depending on whether KEY
   was found in SPEC.  */

Lisp_Object
image_spec_value (Lisp_Object spec, Lisp_Object key, bool *found)
{
  Lisp_Object tail;

  eassert (valid_image_p (spec));

  tail = XCDR (spec);
  FOR_EACH_TAIL_SAFE (tail)
    {
      if (EQ (XCAR (tail), key))
	{
	  if (found)
	    *found = 1;
	  return XCAR (XCDR (tail));
	}
      tail = XCDR (tail);
      if (! CONSP (tail))
	break;
    }

  if (found)
    *found = 0;
  return Qnil;
}


DEFUN ("image-size", Fimage_size, Simage_size, 1, 3, 0,
       doc: /* Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.

FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.

Calling this function will result in the image being stored in the
image cache.  If this is not desirable, call `image-flush' after
calling this function.  */)
  (Lisp_Object spec, Lisp_Object pixels, Lisp_Object frame)
{
  Lisp_Object size;

  size = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = decode_window_system_frame (frame);
      ptrdiff_t id = lookup_image (f, spec, -1);
      struct image *img = IMAGE_FROM_ID (f, id);
      int width = img->width + 2 * img->hmargin;
      int height = img->height + 2 * img->vmargin;

      if (NILP (pixels))
	size = Fcons (make_float ((double) width / FRAME_COLUMN_WIDTH (f)),
		      make_float ((double) height / FRAME_LINE_HEIGHT (f)));
      else
	size = Fcons (make_fixnum (width), make_fixnum (height));
    }
  else
    error ("Invalid image specification");

  return size;
}


DEFUN ("image-mask-p", Fimage_mask_p, Simage_mask_p, 1, 2, 0,
       doc: /* Return t if image SPEC has a mask bitmap.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
  (Lisp_Object spec, Lisp_Object frame)
{
  Lisp_Object mask;

  mask = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = decode_window_system_frame (frame);
      ptrdiff_t id = lookup_image (f, spec, -1);
      struct image *img = IMAGE_FROM_ID (f, id);
      if (img->mask)
	mask = Qt;
    }
  else
    error ("Invalid image specification");

  return mask;
}

DEFUN ("image-metadata", Fimage_metadata, Simage_metadata, 1, 2, 0,
       doc: /* Return metadata for image SPEC.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
  (Lisp_Object spec, Lisp_Object frame)
{
  Lisp_Object ext;

  ext = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = decode_window_system_frame (frame);
      ptrdiff_t id = lookup_image (f, spec, -1);
      struct image *img = IMAGE_FROM_ID (f, id);
      ext = img->lisp_data;
    }

  return ext;
}


/***********************************************************************
		 Image type independent image structures
 ***********************************************************************/

#define MAX_IMAGE_SIZE 10.0
/* Allocate and return a new image structure for image specification
   SPEC.  SPEC has a hash value of HASH.  */

static struct image *
make_image (Lisp_Object spec, EMACS_UINT hash)
{
  struct image *img = xzalloc (sizeof *img);
  Lisp_Object file = image_spec_value (spec, QCfile, NULL);

  eassert (valid_image_p (spec));
  img->dependencies = NILP (file) ? Qnil : list1 (file);
  img->type = lookup_image_type (image_spec_value (spec, QCtype, NULL));
  eassert (img->type != NULL);
  img->spec = spec;
  img->lisp_data = Qnil;
  img->ascent = DEFAULT_IMAGE_ASCENT;
  img->hash = hash;
  img->corners[BOT_CORNER] = -1;  /* Full image */
#ifdef HAVE_NEOMACS
  img->neomacs_gpu_id = 0;
#endif
  return img;
}


/* Free image IMG which was used on frame F, including its resources.  */

static void
free_image (struct frame *f, struct image *img)
{
  if (img)
    {
      struct image_cache *c = FRAME_IMAGE_CACHE (f);

      /* Remove IMG from the hash table of its cache.  */
      if (img->prev)
	img->prev->next = img->next;
      else
	c->buckets[img->hash % IMAGE_CACHE_BUCKETS_SIZE] = img->next;

      if (img->next)
	img->next->prev = img->prev;

      c->images[img->id] = NULL;

	/* Free resources, then free IMG.  */
	img->type->free_img (f, img);

      xfree (img->face_font_family);
      xfree (img);
    }
}

/* Return true if the given widths and heights are valid for display.  */

static bool
check_image_size (struct frame *f, int width, int height)
{
  int w, h;

  if (width <= 0 || height <= 0)
    return 0;

  if (FIXNUMP (Vmax_image_size))
    return (width <= XFIXNUM (Vmax_image_size)
	    && height <= XFIXNUM (Vmax_image_size));
  else if (FLOATP (Vmax_image_size))
    {
      if (f != NULL)
	{
	  w = FRAME_PIXEL_WIDTH (f);
	  h = FRAME_PIXEL_HEIGHT (f);
	}
      else
	w = h = 1024;  /* Arbitrary size for unknown frame. */
      return (width <= XFLOAT_DATA (Vmax_image_size) * w
	      && height <= XFLOAT_DATA (Vmax_image_size) * h);
    }
  else
    return 1;
}

/* Prepare image IMG for display on frame F.  Must be called before
   drawing an image.  */

void
prepare_image_for_display (struct frame *f, struct image *img)
{
  /* We're about to display IMG, so set its timestamp to `now'.  */
  img->timestamp = current_timespec ();

  /* If IMG doesn't have a pixmap yet, load it now, using the image
     type dependent loader function.  */
  if (img->pixmap == NO_PIXMAP && !img->load_failed_p)
    img->load_failed_p = ! img->type->load_img (f, img);

  if (!img->load_failed_p)
    {
      block_input ();
      if (img->cr_data == NULL || (cairo_pattern_get_type (img->cr_data)
				   != CAIRO_PATTERN_TYPE_SURFACE))
	{
	  /* Neomacs renders images via GPU, not Cairo.  Preserve
	     img->pixmap->data so neomacsterm.c can upload it directly.
	     cr_put_image_to_cr_data would transfer ownership to a Cairo
	     pattern and set pixmap->data = NULL, making it inaccessible.  */
	  IMAGE_BACKGROUND (img, f, img->pixmap);
	  IMAGE_BACKGROUND_TRANSPARENT (img, f, img->mask);
	}
      unblock_input ();
    }
}


/* Value is the number of pixels for the ascent of image IMG when
   drawn in face FACE.  */

int
image_ascent (struct image *img, struct face *face, struct glyph_slice *slice)
{
  int height;
  int ascent;

  if (slice->height == img->height)
    height = img->height + img->vmargin;
  else if (slice->y == 0)
    height = slice->height + img->vmargin;
  else
    height = slice->height;

  if (img->ascent == CENTERED_IMAGE_ASCENT)
    {
      if (face->font)
	{
	  /* This expression is arranged so that if the image can't be
	     exactly centered, it will be moved slightly up.  This is
	     because a typical font is `top-heavy' (due to the presence
	     uppercase letters), so the image placement should err towards
	     being top-heavy too.  It also just generally looks better.  */
	  ascent = (height + FONT_BASE (face->font)
                    - FONT_DESCENT (face->font) + 1) / 2;
	}
      else
	ascent = height / 2;
    }
  else
    ascent = height * (img->ascent / 100.0);

  return ascent;
}


/* Image background colors.  */

/* Find the "best" corner color of a bitmap.
   On W32, PIMG is assumed to a device context with the bitmap selected.  */

static RGB_PIXEL_COLOR
four_corners_best (Emacs_Pix_Context pimg, int *corners,
		   unsigned long width, unsigned long height)
{
  RGB_PIXEL_COLOR corner_pixels[4];
  RGB_PIXEL_COLOR best UNINIT;
  int i, best_count;

  if (corners && corners[BOT_CORNER] >= 0)
    {
      /* Get the colors at the corner_pixels of pimg.  */
      corner_pixels[0] = GET_PIXEL (pimg, corners[LEFT_CORNER], corners[TOP_CORNER]);
      corner_pixels[1] = GET_PIXEL (pimg, corners[RIGHT_CORNER] - 1, corners[TOP_CORNER]);
      corner_pixels[2] = GET_PIXEL (pimg, corners[RIGHT_CORNER] - 1, corners[BOT_CORNER] - 1);
      corner_pixels[3] = GET_PIXEL (pimg, corners[LEFT_CORNER], corners[BOT_CORNER] - 1);
    }
  else

    {
      /* Get the colors at the corner_pixels of pimg.  */
      corner_pixels[0] = GET_PIXEL (pimg, 0, 0);
      corner_pixels[1] = GET_PIXEL (pimg, width - 1, 0);
      corner_pixels[2] = GET_PIXEL (pimg, width - 1, height - 1);
      corner_pixels[3] = GET_PIXEL (pimg, 0, height - 1);
    }
  /* Choose the most frequently found color as background.  */
  for (i = best_count = 0; i < 4; ++i)
    {
      int j, n;

      for (j = n = 0; j < 4; ++j)
	if (corner_pixels[i] == corner_pixels[j])
	  ++n;

      if (n > best_count)
	best = corner_pixels[i], best_count = n;
    }

  return best;
}

static Lisp_Object
make_color_name (unsigned int red, unsigned int green, unsigned int blue)
{
  return make_formatted_string ("#%04x%04x%04x", red, green, blue);
}

/* Return the `background' field of IMG.  If IMG doesn't have one yet,
   it is guessed heuristically.  If non-zero, XIMG is an existing
   Emacs_Pix_Context object (device context with the image selected on
   W32) to use for the heuristic.  */

RGB_PIXEL_COLOR
image_background (struct image *img, struct frame *f, Emacs_Pix_Context pimg)
{
  if (! img->background_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      bool free_pimg = !pimg;

      if (free_pimg)
	pimg = image_get_x_image_or_dc (f, img, 0, &prev);

      RGB_PIXEL_COLOR bg
	= four_corners_best (pimg, img->corners, img->width, img->height);
#ifdef USE_CAIRO
      Lisp_Object color_name = make_color_name (RED16_FROM_ULONG (bg),
						GREEN16_FROM_ULONG (bg),
						BLUE16_FROM_ULONG (bg));
      bg = image_alloc_image_color (f, img, color_name, 0);
#endif
      img->background = bg;

      if (free_pimg)
	image_unget_x_image_or_dc (img, 0, pimg, prev);

      img->background_valid = 1;
    }

  return img->background;
}

/* Return the `background_transparent' field of IMG.  If IMG doesn't
   have one yet, it is guessed heuristically.  If non-zero, MASK is an
   existing Emacs_Pix_Context (XImage* on X) object to use for the
   heuristic.  */

int
image_background_transparent (struct image *img, struct frame *f,
                              Emacs_Pix_Context mask)
{
  if (! img->background_transparent_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      if (img->mask)
	{
	  bool free_mask = !mask;

	  if (free_mask)
	    mask = image_get_x_image_or_dc (f, img, 1, &prev);

	  img->background_transparent
	    = (four_corners_best (mask, img->corners, img->width, img->height) == PIX_MASK_RETAIN);

	  if (free_mask)
	    image_unget_x_image_or_dc (img, 1, mask, prev);
	}
      else
	img->background_transparent = 0;

      img->background_transparent_valid = 1;
    }

  return img->background_transparent;
}

/***********************************************************************
		  Helper functions for X image types
 ***********************************************************************/

/* Clear X resources of image IMG on frame F according to FLAGS.
   FLAGS is bitwise-or of the following masks:
   CLEAR_IMAGE_PIXMAP free the pixmap if any.
   CLEAR_IMAGE_MASK means clear the mask pixmap if any.
   CLEAR_IMAGE_COLORS means free colors allocated for the image, if
     any.  */

#define CLEAR_IMAGE_PIXMAP	(1 << 0)
#define CLEAR_IMAGE_MASK	(1 << 1)
#define CLEAR_IMAGE_COLORS	(1 << 2)

static void
image_clear_image_1 (struct frame *f, struct image *img, int flags)
{
  if (flags & CLEAR_IMAGE_PIXMAP)
    {
      if (img->pixmap)
	{
	  FRAME_TERMINAL (f)->free_pixmap (f, img->pixmap);
	  img->pixmap = NO_PIXMAP;
	  img->background_valid = 0;
	}
    }

  if (flags & CLEAR_IMAGE_MASK)
    {
      if (img->mask)
	{
	  FRAME_TERMINAL (f)->free_pixmap (f, img->mask);
	  img->mask = NO_PIXMAP;
	  img->background_transparent_valid = 0;
	}
    }

  if ((flags & CLEAR_IMAGE_COLORS) && img->ncolors)
    {
      xfree (img->colors);
      img->colors = NULL;
      img->ncolors = 0;
    }

  if (img->cr_data)
    {
      cairo_pattern_destroy (img->cr_data);
      img->cr_data = NULL;
    }
}

/* Free X resources of image IMG which is used on frame F.  */

static void
image_clear_image (struct frame *f, struct image *img)
{
  img->lisp_data = Qnil;
  block_input ();
  image_clear_image_1 (f, img,
		       (CLEAR_IMAGE_PIXMAP
			| CLEAR_IMAGE_MASK
			| CLEAR_IMAGE_COLORS));
  unblock_input ();
}


/* Allocate color COLOR_NAME for image IMG on frame F.  If color
   cannot be allocated, use DFLT.  Add a newly allocated color to
   IMG->colors, so that it can be freed again.  Value is the pixel
   color.  */

static unsigned long
image_alloc_image_color (struct frame *f, struct image *img,
                         Lisp_Object color_name, unsigned long dflt)
{
  Emacs_Color color;
  unsigned long result;

  eassert (STRINGP (color_name));

  if (FRAME_TERMINAL (f)->defined_color_hook (f,
                                              SSDATA (color_name),
                                              &color,
                                              true,
                                              false)
      && img->ncolors < min (min (PTRDIFF_MAX, SIZE_MAX) / sizeof *img->colors,
			     INT_MAX))
    {
      /* This isn't called frequently so we get away with simply
	 reallocating the color vector to the needed size, here.  */
      ptrdiff_t ncolors = img->ncolors + 1;
      img->colors = xrealloc (img->colors, ncolors * sizeof *img->colors);
      img->colors[ncolors - 1] = color.pixel;
      img->ncolors = ncolors;
      result = color.pixel;
    }
  else
    result = dflt;

  return result;
}



/***********************************************************************
			     Image Cache
 ***********************************************************************/

static void cache_image (struct frame *f, struct image *img);

/* Return a new, initialized image cache that is allocated from the
   heap.  Call free_image_cache to free an image cache.  */

struct image_cache *
make_image_cache (void)
{
  struct image_cache *c = xmalloc (sizeof *c);

  c->size = 50;
  c->used = c->refcount = 0;
  c->images = xmalloc (c->size * sizeof *c->images);
  c->buckets = xzalloc (IMAGE_CACHE_BUCKETS_SIZE * sizeof *c->buckets);
  /* This value should never be encountered.  */
  c->scaling_col_width = -1;
  return c;
}

/* Find an image matching SPEC in the cache, and return it.  If no
   image is found, return NULL.  */
static struct image *
search_image_cache (struct frame *f, Lisp_Object spec, EMACS_UINT hash,
                    unsigned long foreground, unsigned long background,
                    int font_size, char *font_family, bool ignore_colors)
{
  struct image *img;
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  int i = hash % IMAGE_CACHE_BUCKETS_SIZE;

  if (!c) return NULL;

  /* If the image spec does not specify a background color, the cached
     image must have the same background color as the current frame.
     The foreground color must also match, for the sake of monochrome
     images.

     In fact, we could ignore the foreground color matching condition
     for color images, or if the image spec specifies :foreground;
     similarly we could ignore the background color matching condition
     for formats that don't use transparency (such as jpeg), or if the
     image spec specifies :background.  However, the extra memory
     usage is probably negligible in practice, so we don't bother.  */

  double scale = image_compute_scale (f, spec, NULL);

  for (img = c->buckets[i]; img; img = img->next)
    if (img->hash == hash
	&& !NILP (Fequal (img->spec, spec))
	&& scale == img->scale
	&& (ignore_colors || (img->face_foreground == foreground
                              && img->face_background == background
			      && img->face_font_size == font_size
			      && (font_family
				  &&!strcmp (font_family, img->face_font_family)))))
      break;
  return img;
}


/* Filter out image elements that don't affect display, but will
   disrupt finding the image in the cache.  This should perhaps be
   user-configurable, but for now it's hard-coded (but new elements
   can be added at will).  */
static Lisp_Object
filter_image_spec (Lisp_Object spec)
{
  Lisp_Object out = Qnil;

  /* Skip past the `image' element.  */
  if (CONSP (spec))
    spec = XCDR (spec);

  while (CONSP (spec))
    {
      Lisp_Object key = XCAR (spec);
      spec = XCDR (spec);
      if (CONSP (spec))
	{
	  Lisp_Object value = XCAR (spec);
	  spec = XCDR (spec);

	  /* Some animation-related data doesn't affect display, but
	     breaks the image cache.  Filter those out.  */
	  if (!(EQ (key, QCanimate_buffer)
		|| EQ (key, QCanimate_tardiness)
		|| EQ (key, QCanimate_position)))
	    {
	      out = Fcons (value, out);
	      out = Fcons (key, out);
	    }
	}
    }
  return out;
}

/* Search frame F for an image with spec SPEC, and free it.  */

static void
uncache_image (struct frame *f, Lisp_Object spec)
{
  struct image *img;
  EMACS_UINT hash = sxhash (filter_image_spec (spec));

  /* Because the background colors are based on the current face, we
     can have multiple copies of an image with the same spec. We want
     to remove them all to ensure the user doesn't see an old version
     of the image when the face changes.  */
  while ((img = search_image_cache (f, spec, hash, 0, 0, 0, NULL, true)))
    {
      free_image (f, img);
      /* As display glyphs may still be referring to the image ID, we
	 must garbage the frame (Bug#6426).  */
      SET_FRAME_GARBAGED (f);
    }
}


/* Free image cache of frame F.  Be aware that X frames share images
   caches.  */

void
free_image_cache (struct frame *f)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  ptrdiff_t i;

  /* This function assumes the caller already verified that the frame's
     image cache is non-NULL.  */
  eassert (c);
  /* Cache should not be referenced by any frame when freed.  */
  eassert (c->refcount == 0);

  for (i = 0; i < c->used; ++i)
    free_image (f, c->images[i]);
  xfree (c->images);
  xfree (c->buckets);
  xfree (c);
}

/* Clear image cache of frame F.  FILTER=t means free all images.
   FILTER=nil means clear only images that haven't been
   displayed for some time.
   Else, only free the images which have FILTER in their `dependencies'.
   Should be called from time to time to reduce the number of loaded images.
   If image-cache-eviction-delay is non-nil, this frees images in the cache
   which weren't displayed for at least that many seconds.  */

void
clear_image_cache (struct frame *f, Lisp_Object filter)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);

  if (c && !f->inhibit_clear_image_cache)
    {
      ptrdiff_t i, nfreed = 0;

      /* Block input so that we won't be interrupted by a SIGIO
	 while being in an inconsistent state.  */
      block_input ();

      if (!NILP (filter))
	{
	  /* Filter image cache.  */
	  for (i = 0; i < c->used; ++i)
	    {
	      struct image *img = c->images[i];
	      if (img && (EQ (Qt, filter)
			  || !NILP (Fmember (filter, img->dependencies))))
		{
		  free_image (f, img);
		  ++nfreed;
		}
	    }
	}
      else if (FIXNUMP (Vimage_cache_eviction_delay))
	{
	  /* Free cache based on timestamp.  */
	  struct timespec old, t;
	  double delay;
	  ptrdiff_t nimages = 0;

	  for (i = 0; i < c->used; ++i)
	    if (c->images[i])
	      nimages++;

	  /* If the number of cached images has grown unusually large,
	     decrease the cache eviction delay (Bug#6230).  */
	  delay = XFIXNUM (Vimage_cache_eviction_delay);
	  if (nimages > 40)
	    delay = 1600 * delay / nimages / nimages;
	  delay = max (delay, 1);

	  t = current_timespec ();
	  old = timespec_sub (t, dtotimespec (delay));

	  for (i = 0; i < c->used; ++i)
	    {
	      struct image *img = c->images[i];
	      if (img && timespec_cmp (img->timestamp, old) < 0)
		{
		  free_image (f, img);
		  ++nfreed;
		}
	    }
	}

      /* We may be clearing the image cache because, for example,
	 Emacs was iconified for a longer period of time.  In that
	 case, current matrices may still contain references to
	 images freed above.  So, clear these matrices.  */
      if (nfreed)
	{
	  Lisp_Object tail, frame;

	  FOR_EACH_FRAME (tail, frame)
	    {
	      struct frame *fr = XFRAME (frame);
	      if (FRAME_IMAGE_CACHE (fr) == c)
		clear_current_matrices (fr);
	    }

	  windows_or_buffers_changed = 19;
	}

      unblock_input ();
    }
}

void
clear_image_caches (Lisp_Object filter)
{
  /* FIXME: We want to do
   * struct terminal *t;
   * for (t = terminal_list; t; t = t->next_terminal)
   *   clear_image_cache (t, filter); */
  Lisp_Object tail, frame;
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_WINDOW_P (XFRAME (frame)))
      clear_image_cache (XFRAME (frame), filter);
}

DEFUN ("clear-image-cache", Fclear_image_cache, Sclear_image_cache,
       0, 2, 0,
       doc: /* Clear the image and animation caches.
FILTER nil or a frame means clear all images in the selected frame.
FILTER t means clear the image caches of all frames.
Anything else means clear only those images that refer to FILTER,
which is then usually a filename.

This function also clears the image animation cache.
ANIMATION-FILTER nil means clear all animation cache entries.
Otherwise, clear the image spec `eq' to ANIMATION-FILTER only
from the animation cache, and do not clear any image caches.
This can help reduce memory usage after an animation is stopped
but the image is still displayed.  */)
  (Lisp_Object filter, Lisp_Object animation_filter)
{
  if (!NILP (animation_filter))
    {
      /* IMAGEP?  */
      CHECK_CONS (animation_filter);
      return Qnil;
    }

  if (! (NILP (filter) || FRAMEP (filter)))
    clear_image_caches (filter);
  else
    clear_image_cache (decode_window_system_frame (filter), Qt);

  /* Also clear the animation caches.  */
  image_prune_animation_caches (true);

  return Qnil;
}

static intptr_t
image_size_in_bytes (struct image *img)
{
  intptr_t size = 0;

  Emacs_Pixmap pm = img->pixmap;
  if (pm)
    size += pm->height * pm->bytes_per_line;
  Emacs_Pixmap msk = img->mask;
  if (msk)
    size += msk->height * msk->bytes_per_line;

  return size;
}

static intptr_t
image_frame_cache_size (struct frame *f)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  if (!c)
    return 0;

  intptr_t total = 0;
  for (ptrdiff_t i = 0; i < c->used; ++i)
    {
      struct image *img = c->images[i];
      total += img ? image_size_in_bytes (img) : 0;
    }
  return total;
}

DEFUN ("image-flush", Fimage_flush, Simage_flush,
       1, 2, 0,
       doc: /* Flush the image with specification SPEC on frame FRAME.
This removes the image from the Emacs image cache.  If SPEC specifies
an image file, the next redisplay of this image will read from the
current contents of that file.

FRAME nil or omitted means use the selected frame.
FRAME t means refresh the image on all frames.  */)
  (Lisp_Object spec, Lisp_Object frame)
{
  if (!valid_image_p (spec))
    error ("Invalid image specification");

  if (EQ (frame, Qt))
    {
      Lisp_Object tail;
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  if (FRAME_WINDOW_P (f))
	    uncache_image (f, spec);
	}
    }
  else
    uncache_image (decode_window_system_frame (frame), spec);

  return Qnil;
}


/* Compute masks and transform image IMG on frame F, as specified
   by the image's specification,  */

static void
postprocess_image (struct frame *f, struct image *img)
{
  /* Manipulation of the image's mask.  */
  if (img->pixmap)
    {
      Lisp_Object conversion, spec;
      Lisp_Object mask;

      spec = img->spec;

      /* `:heuristic-mask t'
	 `:mask heuristic'
	 means build a mask heuristically.
	 `:heuristic-mask (R G B)'
	 `:mask (heuristic (R G B))'
	 means build a mask from color (R G B) in the
	 image.
	 `:mask nil'
	 means remove a mask, if any.  */

      mask = image_spec_value (spec, QCheuristic_mask, NULL);
      if (!NILP (mask))
	image_build_heuristic_mask (f, img, mask);
      else
	{
	  bool found_p;

	  mask = image_spec_value (spec, QCmask, &found_p);

	  if (EQ (mask, Qheuristic))
	    image_build_heuristic_mask (f, img, Qt);
	  else if (CONSP (mask)
		   && EQ (XCAR (mask), Qheuristic))
	    {
	      if (CONSP (XCDR (mask)))
		image_build_heuristic_mask (f, img, XCAR (XCDR (mask)));
	      else
		image_build_heuristic_mask (f, img, XCDR (mask));
	    }
	  else if (NILP (mask) && found_p && img->mask)
	    image_clear_image_1 (f, img, CLEAR_IMAGE_MASK);
	}


      /* Should we apply an image transformation algorithm?  */
      conversion = image_spec_value (spec, QCconversion, NULL);
      if (EQ (conversion, Qdisabled))
	image_disable_image (f, img);
      else if (EQ (conversion, Qlaplace))
	image_laplace (f, img);
      else if (EQ (conversion, Qemboss))
	image_emboss (f, img);
      else if (CONSP (conversion)
	       && EQ (XCAR (conversion), Qedge_detection))
	{
	  Lisp_Object tem;
	  tem = XCDR (conversion);
	  if (CONSP (tem))
	    image_edge_detection (f, img,
                                  plist_get (tem, QCmatrix),
                                  plist_get (tem, QCcolor_adjustment));
	}
    }
}


/* Calculate the scale of the image.  IMG may be null as it is only
   required when creating an image, and this function is called from
   image cache related functions that do not have access to the image
   structure.  */
static double
image_compute_scale (struct frame *f, Lisp_Object spec, struct image *img)
{
  double scale = 1;
  Lisp_Object value = image_spec_value (spec, QCscale, NULL);

  if (EQ (value, Qdefault))
    {
      Lisp_Object sval = Vimage_scaling_factor;

      /* Compute the scale from factors specified by the value of
	 Vimage_scaling_factor.  */

    invalid_value:
      if (EQ (sval, Qauto))
	{
	  /* This is a tag with which callers of `clear_image_cache' can
	     refer to this image and its likenesses.  */
	  if (img)
	    img->dependencies = Fcons (Qauto, img->dependencies);

	  scale = (FRAME_COLUMN_WIDTH (f) > 10
		   ? (FRAME_COLUMN_WIDTH (f) / 10.0f) : 1);
	}
      else if (NUMBERP (sval))
	scale = XFLOATINT (sval);
      else
	{
	  image_error ("Invalid `image-scaling-factor': %s",
		       Vimage_scaling_factor);

	  /* If Vimage_scaling_factor is set to an invalid value, treat
	     it as though it were the default.  */
	  sval = Qauto;
	  goto invalid_value;
	}
    }
  else if (NUMBERP (value))
    {
      double dval = XFLOATINT (value);
      if (0 <= dval)
	scale = dval;
    }

  if (img)
    img->scale = scale;

  return scale;
}


/* Return the id of image with Lisp specification SPEC on frame F.
   SPEC must be a valid Lisp image specification (see valid_image_p).  */

ptrdiff_t
lookup_image (struct frame *f, Lisp_Object spec, int face_id)
{
  nlog_debug ("lookup_image: face_id=%d", face_id);
  struct image *img;
  EMACS_UINT hash;

  if (FRAME_FACE_CACHE (f) == NULL)
    init_frame_faces (f);
  if (FRAME_FACE_CACHE (f)->used == 0)
    recompute_basic_faces (f);
  if (face_id < 0 || face_id >= FRAME_FACE_CACHE (f)->used)
    face_id = DEFAULT_FACE_ID;

  struct face *face = FACE_FROM_ID (f, face_id);
  unsigned long foreground = face->foreground;
  unsigned long background = face->background;
  int font_size = face->font->pixel_size;
  char *font_family = SSDATA (face->lface[LFACE_FAMILY_INDEX]);

  /* F must be a window-system frame, and SPEC must be a valid image
     specification.  */
  eassert (FRAME_WINDOW_P (f));
  eassert (valid_image_p (spec));

  /* Look up SPEC in the hash table of the image cache.  */
  hash = sxhash (filter_image_spec (spec));
  img = search_image_cache (f, spec, hash, foreground, background,
			    font_size, font_family, false);
  if (img && img->load_failed_p)
    {
      free_image (f, img);
      img = NULL;
    }

  /* If not found, create a new image and cache it.  */
  if (img == NULL)
    {
      block_input ();
      img = make_image (spec, hash);
      cache_image (f, img);
      img->face_foreground = foreground;
      img->face_background = background;
      img->face_font_size = font_size;
      img->face_font_height = face->font->height;
      img->face_font_width = face->font->average_width;
      size_t len = strlen (font_family) + 1;
      img->face_font_family = xmalloc (len);
      memcpy (img->face_font_family, font_family, len);
      img->load_failed_p = ! img->type->load_img (f, img);

      /* If we can't load the image, and we don't have a width and
	 height, use some arbitrary width and height so that we can
	 draw a rectangle for it.  */
      if (img->load_failed_p)
	{
	  Lisp_Object value;

	  value = image_spec_value (spec, QCwidth, NULL);
	  img->width = (FIXNUMP (value)
			? XFIXNAT (value) : DEFAULT_IMAGE_WIDTH);
	  value = image_spec_value (spec, QCheight, NULL);
	  img->height = (FIXNUMP (value)
			 ? XFIXNAT (value) : DEFAULT_IMAGE_HEIGHT);
	}
      else
	{
	  /* Handle image type independent image attributes
	     `:ascent ASCENT', `:margin MARGIN', `:relief RELIEF',
	     `:background COLOR'.  */
	  Lisp_Object ascent, margin, relief, bg;
	  int relief_bound;

	  ascent = image_spec_value (spec, QCascent, NULL);
	  if (FIXNUMP (ascent))
	    img->ascent = XFIXNUM (ascent);
	  else if (EQ (ascent, Qcenter))
	    img->ascent = CENTERED_IMAGE_ASCENT;

	  margin = image_spec_value (spec, QCmargin, NULL);
	  if (FIXNUMP (margin))
	    img->vmargin = img->hmargin = XFIXNUM (margin);
	  else if (CONSP (margin))
	    {
	      img->hmargin = XFIXNUM (XCAR (margin));
	      img->vmargin = XFIXNUM (XCDR (margin));
	    }

	  relief = image_spec_value (spec, QCrelief, NULL);
	  relief_bound = INT_MAX - max (img->hmargin, img->vmargin);
	  if (RANGED_FIXNUMP (- relief_bound, relief, relief_bound))
	    {
	      img->relief = XFIXNUM (relief);
	      img->hmargin += eabs (img->relief);
	      img->vmargin += eabs (img->relief);
	    }

	  if (! img->background_valid)
	    {
	      bg = image_spec_value (img->spec, QCbackground, NULL);
	      if (!NILP (bg))
		{
		  img->background
		    = image_alloc_image_color (f, img, bg, background);
		  img->background_valid = 1;
		}
	    }

	  /* Do image transformations and compute masks, unless we
	     don't have the image yet.  */
	  if (!EQ (builtin_lisp_symbol (img->type->type), Qpostscript))
	    postprocess_image (f, img);

          /* postprocess_image above may modify the image or the mask,
             relying on the image's real width and height, so
             image_set_transform must be called after it.  */
	}

      unblock_input ();
    }

  /* IMG is now being used, so set its timestamp to the current
     time.  */
  img->timestamp = current_timespec ();

  /* Value is the image id.  */
  return img->id;
}


/* Cache image IMG in the image cache of frame F.  */

static void
cache_image (struct frame *f, struct image *img)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  ptrdiff_t i;

  if (!c)
    {
      c = FRAME_IMAGE_CACHE (f) = share_image_cache (f);
      c->refcount++;
    }

  /* Find a free slot in c->images.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i] == NULL)
      break;

  /* If no free slot found, maybe enlarge c->images.  */
  if (i == c->used && c->used == c->size)
    c->images = xpalloc (c->images, &c->size, 1, -1, sizeof *c->images);

  /* Add IMG to c->images, and assign IMG an id.  */
  c->images[i] = img;
  img->id = i;
  if (i == c->used)
    ++c->used;

  /* Add IMG to the cache's hash table.  */
  i = img->hash % IMAGE_CACHE_BUCKETS_SIZE;
  img->next = c->buckets[i];
  if (img->next)
    img->next->prev = img;
  img->prev = NULL;
  c->buckets[i] = img;
}



/* Mark Lisp objects in image IMG.  */
static void
mark_image (struct image *img)
{
  mark_object (img->spec);
  mark_object (img->dependencies);

  if (!NILP (img->lisp_data))
    mark_object (img->lisp_data);
}

/* Mark every image in image cache C, as well as the global animation
   cache.  */
void
mark_image_cache (struct image_cache *c)
{
  if (c)
    {
      ptrdiff_t i;
      for (i = 0; i < c->used; ++i)
	if (c->images[i])
	  mark_image (c->images[i]);
    }

}



/***********************************************************************
			  X / NS / W32 support code
             Most of this code is shared with Android to make
             it easier to maintain.
 ***********************************************************************/


/* Return true if XIMG's size WIDTH x HEIGHT doesn't break the
   windowing system.
   WIDTH and HEIGHT must both be positive.
   If XIMG is null, assume it is a bitmap.  */

static bool
image_check_image_size (Emacs_Pix_Container ximg, int width, int height)
{
  return 1;
}

/* Create an Emacs_Pix_Container and a pixmap of size WIDTH x
   HEIGHT for use on frame F.  Set *PIMG and *PIXMAP to the
   Emacs_Pix_Container and Emacs_Pixmap created.  Set (*PIMG)->data
   to a raster of WIDTH x HEIGHT pixels allocated via xmalloc.  Print
   error messages via image_error if an error occurs.  Value is true
   if successful.

   On W32, a DEPTH of zero signifies a 24 bit image, otherwise DEPTH
   should indicate the bit depth of the image.  */

static bool
image_create_x_image_and_pixmap_1 (struct frame *f, int width, int height, int depth,
                                   Emacs_Pix_Container *pimg,
                                   Emacs_Pixmap *pixmap, Picture *picture)
{
  eassert (input_blocked_p ());

  /* Allocate a pixmap of the same size.  */
  *pixmap = image_create_pix_container (width, height, depth);
  if (*pixmap == NO_PIXMAP)
    {
      *pimg = NULL;
      image_error ("Unable to create X pixmap");
      return false;
    }

  *pimg = *pixmap;
  return 1;
}


/* Destroy Emacs_Pix_Container PIMG.  Free data associated with PIMG.  */

static void
image_destroy_x_image (Emacs_Pix_Container pimg)
{
  eassert (input_blocked_p ());
  if (pimg)
    {
      /* On Cairo, Emacs_Pix_Containers always point to the same
	 data as pixmaps in `struct image', and therefore must never be
	 freed separately.  */
    }
}


/* Put Emacs_Pix_Container PIMG into pixmap PIXMAP on frame F.
   WIDTH and HEIGHT are width and height of both the image and
   pixmap.  */

static void
gui_put_x_image (struct frame *f, Emacs_Pix_Container pimg,
                 Emacs_Pixmap pixmap, int width, int height)
{
  eassert (pimg == pixmap);
}

/* Thin wrapper for image_create_x_image_and_pixmap_1, so that it matches
   with image_put_x_image.  */

static bool
image_create_x_image_and_pixmap (struct frame *f, struct image *img,
				 int width, int height, int depth,
				 Emacs_Pix_Container *ximg, bool mask_p)
{
  eassert ((!mask_p ? img->pixmap : img->mask) == NO_PIXMAP);

  Picture *picture = NULL;
  return image_create_x_image_and_pixmap_1 (f, width, height, depth, ximg,
                                            !mask_p ? &img->pixmap : &img->mask,
                                            picture);
}

/* Put pixel image PIMG into image IMG on frame F, as a mask if and only
   if MASK_P.  On X, this simply records PIMG on a member of IMG, so
   it can be put into the pixmap afterwards via image_sync_to_pixmaps.
   On the other platforms, it puts PIMG into the pixmap, then frees
   the pixel image and its buffer.  */

static void
image_put_x_image (struct frame *f, struct image *img, Emacs_Pix_Container ximg,
		   bool mask_p)
{
  gui_put_x_image (f, ximg, !mask_p ? img->pixmap : img->mask,
                   img->width, img->height);
  image_destroy_x_image (ximg);
}

/* Get the X image for IMG on frame F.  The resulting X image data
   should be treated as read-only at least on X.  */

static Emacs_Pix_Container
image_get_x_image (struct frame *f, struct image *img, bool mask_p)
{
  return !mask_p ? img->pixmap : img->mask;
}

static void
image_unget_x_image (struct image *img, bool mask_p, Emacs_Pix_Container ximg)
{
}


/***********************************************************************
			      File Handling
 ***********************************************************************/

/* Find image file FILE.  Look in data-directory/images, then
   x-bitmap-file-path.  Value is the full name of the file
   found, or nil if not found.  If PFD is nonnull store into *PFD a
   readable file descriptor for the file, opened in binary mode.  If
   PFD is null, do not open the file.  */

static Lisp_Object
image_find_image_fd (Lisp_Object file, image_fd *pfd)
{
  Lisp_Object file_found, search_path;
  int fd;
  void *platform;

  /* TODO I think this should use something like image-load-path
     instead.  Unfortunately, that can contain non-string elements.  */
  search_path = Fcons (Fexpand_file_name (build_string ("images"),
					  Vdata_directory),
		       Vx_bitmap_file_path);

  /* Try to find FILE in data-directory/images, then x-bitmap-file-path.  */
  platform = NULL;
  fd = openp (search_path, file, Qnil, &file_found,
	      pfd ? Qt : make_fixnum (R_OK), false, false,
	      pfd ? &platform : NULL);
  if (fd == -2)
    {
      /* The file exists locally, but has a file name handler.
	 (This happens, e.g., under Auto Image File Mode.)
	 'openp' didn't open the file, so we should, because the
	 caller expects that.  */
      Lisp_Object encoded_name = ENCODE_FILE (file_found);
      fd = emacs_open (SSDATA (encoded_name), O_RDONLY, 0);
    }
  /* FD is -3 if PLATFORM is set to a valid asset file descriptor on
     Android.  */
  else if (fd < 0 && fd != -3)
    return Qnil;

  if (pfd)
    *pfd = fd;
  return file_found;
}

/* Find image file FILE.  Look in data-directory/images, then
   x-bitmap-file-path.  Value is the full name of the file found, or
   nil if not found.  */

Lisp_Object
image_find_image_file (Lisp_Object file)
{
  return image_find_image_fd (file, 0);
}


/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occurred.  FD is a file descriptor open for reading FILE.  Set
   *SIZE to the size of the file.  */

static char *
slurp_file (image_fd fd, ptrdiff_t *size)
{
  FILE *fp = emacs_fdopen (fd, "rb");

  char *buf = NULL;
  struct stat st;

  if (fp)
    {
      specpdl_ref count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (fclose_unwind, fp);

      if (sys_fstat (fileno (fp), &st) == 0
	  && 0 <= st.st_size && st.st_size < min (PTRDIFF_MAX, SIZE_MAX))
	{
	  /* Report an error if we read past the purported EOF.
	     This can happen if the file grows as we read it.  */
	  ptrdiff_t buflen = st.st_size;
	  buf = xmalloc (buflen + 1);
	  if (fread (buf, 1, buflen + 1, fp) == buflen)
	    *size = buflen;
	  else
	    {
	      xfree (buf);
	      buf = NULL;
	    }
	}

      unbind_to (count, Qnil);
    }

  return buf;
}

/* Like slurp_file above, but with added error handling.  Value is
   null if an error occurred.  Set SIZE to the size of the file.
   IMAGE_TYPE describes the image type (e.g. "PNG").  */

static char *
slurp_image (Lisp_Object filename, ptrdiff_t *size, const char *image_type)
{
  image_fd fd;
  Lisp_Object file = image_find_image_fd (filename, &fd);
  if (!STRINGP (file))
    {
      image_not_found_error (filename);
      return NULL;
    }
  char *result = slurp_file (fd, size);
  if (result == NULL)
    image_error ("Error loading %s image `%s'",
		 build_unibyte_string (image_type),
		 file);
  return result;
}


/***********************************************************************
			      XBM images
 ***********************************************************************/

static bool xbm_file_p (Lisp_Object);


/* Indices of image specification fields in xbm_format, below.  */

enum xbm_keyword_index
{
  XBM_TYPE,
  XBM_FILE,
  XBM_WIDTH,
  XBM_HEIGHT,
  XBM_STRIDE,
  XBM_DATA,
  XBM_FOREGROUND,
  XBM_BACKGROUND,
  XBM_ASCENT,
  XBM_MARGIN,
  XBM_RELIEF,
  XBM_ALGORITHM,
  XBM_HEURISTIC_MASK,
  XBM_MASK,
  XBM_DATA_WIDTH,
  XBM_DATA_HEIGHT,
  XBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XBM image specifications.  */

static const struct image_keyword xbm_format[XBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":width",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":height",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":stride",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":data-width",	IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data-height",	IMAGE_POSITIVE_INTEGER_VALUE,		0}
};

/* Tokens returned from xbm_scan.  */

enum xbm_token
{
  XBM_TK_IDENT = 256,
  XBM_TK_NUMBER,
  XBM_TK_OVERFLOW
};


/* Return true if OBJECT is a valid XBM-type image specification.
   A valid specification is a list starting with the symbol `image'.
   The rest of the list is a property list which must contain an
   entry `:type xbm'.

   If the specification specifies a file to load, it must contain
   an entry `:file FILENAME' where FILENAME is a string.

   If the specification is for a bitmap loaded from memory it must
   contain `:data-width WIDTH', `:data-height HEIGHT', and `:data DATA',
   where WIDTH and HEIGHT are integers > 0.  DATA may be:

   1. a string large enough to hold the bitmap data, i.e. it must
   have a size >= (WIDTH + 7) / 8 * HEIGHT

   2. a bool-vector of size >= WIDTH * HEIGHT

   3. a vector of strings or bool-vectors, one for each line of the
   bitmap.

   4. a string containing an in-memory XBM file.

   Both the file and data forms may contain the additional entries
   `:background COLOR' and `:foreground COLOR'.  If not present,
   the foreground and background of the frame on which the image is
   displayed are used.  */

static bool
xbm_image_p (Lisp_Object object)
{
  struct image_keyword kw[XBM_LAST];

  memcpy (kw, xbm_format, sizeof kw);
  if (!parse_image_spec (object, kw, XBM_LAST, Qxbm))
    return 0;

  eassert (EQ (kw[XBM_TYPE].value, Qxbm));

  if (kw[XBM_FILE].count)
    {
      if (kw[XBM_DATA].count)
	return 0;
    }
  else if (! (kw[XBM_DATA].count && xbm_file_p (kw[XBM_DATA].value)))
    /* Not an in-memory XBM file.  */
    {
      Lisp_Object data;
      int width, height, stride;

      /* Entries for `:data-width', `:data-height', and `:data' must be
	 present.  */
      if (!kw[XBM_DATA_WIDTH].count
	  || !kw[XBM_DATA_HEIGHT].count
	  || !kw[XBM_DATA].count)
	return 0;

      data = kw[XBM_DATA].value;
      width = XFIXNAT (kw[XBM_DATA_WIDTH].value);
      height = XFIXNAT (kw[XBM_DATA_HEIGHT].value);

      if (!kw[XBM_STRIDE].count)
	stride = width;
      else
	stride = XFIXNAT (kw[XBM_STRIDE].value);

      /* Check type of data, and width and height against contents of
	 data.  */
      if (VECTORP (data))
	{
	  EMACS_INT i;

	  /* Number of elements of the vector must be >= height.  */
	  if (ASIZE (data) < height)
	    return 0;

	  /* Each string or bool-vector in data must be large enough
	     for one line of the image.  */
	  for (i = 0; i < height; ++i)
	    {
	      Lisp_Object elt = AREF (data, i);

	      if (STRINGP (elt))
		{
		  if (SCHARS (elt) < stride / CHAR_BIT)
		    return 0;
		}
	      else if (BOOL_VECTOR_P (elt))
		{
		  if (bool_vector_size (elt) < width)
		    return 0;
		}
	      else
		return 0;
	    }
	}
      else if (STRINGP (data))
	{
	  if (SCHARS (data) < stride / CHAR_BIT * height)
	    return 0;
	}
      else if (BOOL_VECTOR_P (data))
	{
	  if (height > 1 && stride != (width + CHAR_BIT - 1)
	      / CHAR_BIT * CHAR_BIT)
	    return 0;

	  if (bool_vector_size (data) / height < stride)
	    return 0;
	}
      else
	return 0;
    }

  return 1;
}


/* Scan a bitmap file.  FP is the stream to read from.  Value is
   either an enumerator from enum xbm_token, or a character for a
   single-character token, or 0 at end of file.  If scanning an
   identifier, store the lexeme of the identifier in SVAL.  If
   scanning a number, store its value in *IVAL.  */

static int
xbm_scan (char **s, char *end, char *sval, int *ival)
{
  unsigned char c UNINIT;
  char *sval_end = sval + BUFSIZ;

 loop:

  /* Skip white space.  */
  while (*s < end && (c = *(*s)++, c_isspace (c)))
    ;

  if (*s >= end)
    c = 0;
  else if (c_isdigit (c))
    {
      int value = 0, digit;
      bool overflow = false;

      if (c == '0' && *s < end)
	{
	  c = *(*s)++;
	  if (c == 'x' || c == 'X')
	    {
	      while (*s < end)
		{
		  c = *(*s)++;
		  digit = char_hexdigit (c);
		  if (digit < 0)
		    break;
		  overflow |= ckd_mul (&value, value, 16);
		  value += digit;
		}
	    }
	  else if ('0' <= c && c <= '7')
	    {
	      value = c - '0';
	      while (*s < end
		     && (c = *(*s)++, '0' <= c && c <= '7'))
		{
		  overflow |= ckd_mul (&value, value, 8);
		  value += c - '0';
		}
	    }
	}
      else
	{
	  value = c - '0';
	  while (*s < end
		 && (c = *(*s)++, c_isdigit (c)))
	    {
	      overflow |= ckd_mul (&value, value, 10);
	      overflow |= ckd_add (&value, value, c - '0');
	    }
	}

      if (*s < end)
	*s = *s - 1;
      *ival = value;
      return overflow ? XBM_TK_OVERFLOW : XBM_TK_NUMBER;
    }
  /* Character literal.  XBM images typically contain hex escape
     sequences and not actual characters, so we only try to handle
     that here.  */
  else if (c == '\'')
    {
      int value = 0, digit;
      bool overflow = false;

      if (*s == end)
	return 0;

      c = *(*s)++;

      if (c != '\\' || *s == end)
	return 0;

      c = *(*s)++;

      if (c == 'x')
	{
	  while (*s < end)
	    {
	      c = *(*s)++;

	      if (c == '\'')
		{
		  *ival = value;
		  return overflow ? XBM_TK_OVERFLOW : XBM_TK_NUMBER;
		}

	      digit = char_hexdigit (c);

	      if (digit < 0)
		return 0;

	      overflow |= ckd_mul (&value, value, 16);
	      value += digit;
	    }
	}

      return 0;
    }
  else if (c_isalpha (c) || c == '_')
    {
      *sval++ = c;
      while (*s < end && sval < sval_end
	     && (c = *(*s)++, (c_isalnum (c) || c == '_')))
	*sval++ = c;
      *sval = 0;
      if (*s < end)
	*s = *s - 1;
      return XBM_TK_IDENT;
    }
  else if (c == '/' && **s == '*')
    {
      /* C-style comment.  */
      ++*s;
      while (**s && (**s != '*' || *(*s + 1) != '/'))
	++*s;
      if (**s)
	{
	  *s += 2;
	  goto loop;
	}
    }

  return c;
}



static void
Create_Pixmap_From_Bitmap_Data (struct frame *f, struct image *img, char *data,
				RGB_PIXEL_COLOR fg, RGB_PIXEL_COLOR bg,
				bool non_default_colors)
{
  if (FRAME_TERMINAL (f)->query_colors)
    {
      Emacs_Color fgbg[] = {{.pixel = fg}, {.pixel = bg}};
      FRAME_TERMINAL (f)->query_colors (f, fgbg, ARRAYELTS (fgbg));
      fg = lookup_rgb_color (f, fgbg[0].red, fgbg[0].green, fgbg[0].blue);
      bg = lookup_rgb_color (f, fgbg[1].red, fgbg[1].green, fgbg[1].blue);
    }
  /* Some terminals may not provide query_colors; in that case treat
     FG and BG as already being RGB pixel values.  */
  img->pixmap
    = image_pix_container_create_from_bitmap_data (data, img->width,
						   img->height, fg, bg);
}



/* Replacement for XReadBitmapFileData which isn't available under old
   X versions.  CONTENTS is a pointer to a buffer to parse; END is the
   buffer's end.  Set *WIDTH and *HEIGHT to the width and height of
   the image.  Return in *DATA the bitmap data allocated with xmalloc.
   Value is true if successful.  DATA null means just test if
   CONTENTS looks like an in-memory XBM file.  If INHIBIT_IMAGE_ERROR,
   inhibit the call to image_error when the image size is invalid (the
   bitmap remains unread).  */

static bool
xbm_read_bitmap_data (struct frame *f, char *contents, char *end,
		      int *width, int *height, char **data,
		      bool inhibit_image_error)
{
  char *s = contents;
  char buffer[BUFSIZ];
  bool padding_p = 0;
  bool v10 = 0;
  int bytes_per_line, i, nbytes;
  char *p;
  int value;
  int LA1;

#define match() \
     LA1 = xbm_scan (&s, end, buffer, &value)

#define expect(TOKEN)		\
  do				\
    {				\
      if (LA1 != (TOKEN)) 	\
	goto failure;		\
      match ();			\
    }				\
  while (0)

#define expect_ident(IDENT)					\
     if (LA1 == XBM_TK_IDENT && strcmp (buffer, IDENT) == 0)	\
       match ();						\
     else							\
       goto failure

  *width = *height = -1;
  if (data)
    *data = NULL;
  LA1 = xbm_scan (&s, end, buffer, &value);

  /* Parse defines for width, height and hot-spots.  */
  while (LA1 == '#')
    {
      match ();
      expect_ident ("define");
      expect (XBM_TK_IDENT);

      if (LA1 == XBM_TK_NUMBER)
	{
	  char *q = strrchr (buffer, '_');
	  q = q ? q + 1 : buffer;
	  if (strcmp (q, "width") == 0)
	    *width = value;
	  else if (strcmp (q, "height") == 0)
	    *height = value;
	}
      expect (XBM_TK_NUMBER);
    }

  if (!check_image_size (f, *width, *height))
    {
      if (!inhibit_image_error)
	image_size_error ();
      goto failure;
    }
  else if (data == NULL)
    goto success;

  /* Parse bits.  Must start with `static'.  */
  expect_ident ("static");
  if (LA1 == XBM_TK_IDENT)
    {
      if (strcmp (buffer, "unsigned") == 0)
	{
	  match ();
	  expect_ident ("char");
	}
      else if (strcmp (buffer, "short") == 0)
	{
	  match ();
	  v10 = 1;
	  if (*width % 16 && *width % 16 < 9)
	    padding_p = 1;
	}
      else if (strcmp (buffer, "char") == 0)
	match ();
      else
	goto failure;
    }
  else
    goto failure;

  expect (XBM_TK_IDENT);
  expect ('[');
  expect (']');
  expect ('=');
  expect ('{');

  if (! image_check_image_size (0, *width, *height))
    {
      if (!inhibit_image_error)
	image_error ("Image too large (%dx%d)",
		     make_fixnum (*width), make_fixnum (*height));
      goto failure;
    }
  bytes_per_line = (*width + 7) / 8 + padding_p;
  nbytes = bytes_per_line * *height;
  p = *data = xmalloc (nbytes);

  if (v10)
    {
      for (i = 0; i < nbytes; i += 2)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);

	  *p++ = XBM_BIT_SHUFFLE (val);
	  if (!padding_p || ((i + 2) % bytes_per_line))
	    *p++ = XBM_BIT_SHUFFLE (value >> 8);

	  if (LA1 == ',' || LA1 == '}')
	    match ();
	  else
	    goto failure;
	}
    }
  else
    {
      for (i = 0; i < nbytes; ++i)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);

	  *p++ = XBM_BIT_SHUFFLE (val);

	  if (LA1 == ',' || LA1 == '}')
	    match ();
	  else
	    goto failure;
	}
    }

 success:
  return 1;

 failure:

  if (data && *data)
    {
      xfree (*data);
      *data = NULL;
    }
  return 0;

#undef match
#undef expect
#undef expect_ident
}


/* Load XBM image IMG which will be displayed on frame F from buffer
   CONTENTS.  END is the end of the buffer.  Value is true if
   successful.  */

static bool
xbm_load_image (struct frame *f, struct image *img, char *contents, char *end)
{
  bool rc;
  char *data;
  bool success_p = 0;

  rc = xbm_read_bitmap_data (f, contents, end, &img->width, &img->height,
			     &data, 0);

  if (rc)
    {
      unsigned long foreground = img->face_foreground;
      unsigned long background = img->face_background;
      bool non_default_colors = 0;
      Lisp_Object value;

      eassert (img->width > 0 && img->height > 0);

      /* Get foreground and background colors, maybe allocate colors.  */
      value = image_spec_value (img->spec, QCforeground, NULL);
      if (!NILP (value))
	{
	  foreground = image_alloc_image_color (f, img, value, foreground);
	  non_default_colors = 1;
	}
      value = image_spec_value (img->spec, QCbackground, NULL);
      if (!NILP (value))
	{
	  background = image_alloc_image_color (f, img, value, background);
	  img->background = background;
	  img->background_valid = 1;
	  non_default_colors = 1;
	}

      if (image_check_image_size (0, img->width, img->height))
	Create_Pixmap_From_Bitmap_Data (f, img, data,
					foreground, background,
					non_default_colors);
      else
	img->pixmap = NO_PIXMAP;
      xfree (data);

      if (img->pixmap == NO_PIXMAP)
	{
	  image_clear_image (f, img);
	  image_error ("Unable to create X pixmap for `%s'", img->spec);
	}
      else
	success_p = 1;
    }
  else
    image_error ("Error loading XBM image `%s'", img->spec);

  return success_p;
}


/* Value is true if DATA looks like an in-memory XBM file.  */

static bool
xbm_file_p (Lisp_Object data)
{
  int w, h;
  return (STRINGP (data)
	  && xbm_read_bitmap_data (NULL, SSDATA (data),
				   SSDATA (data) + SBYTES (data),
				   &w, &h, NULL, 1));
}


/* Fill image IMG which is used on frame F with pixmap data.  Value is
   true if successful.  */

static bool
xbm_load (struct frame *f, struct image *img)
{
  bool success_p = 0;
  Lisp_Object file_name;

  eassert (xbm_image_p (img->spec));

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      ptrdiff_t size;
      char *contents = slurp_image (file_name, &size, "XBM");
      if (contents == NULL)
	return false;
      success_p = xbm_load_image (f, img, contents, contents + size);
      xfree (contents);
    }
  else
    {
      struct image_keyword fmt[XBM_LAST];
      Lisp_Object data;
      unsigned long foreground = img->face_foreground;
      unsigned long background = img->face_background;
      bool non_default_colors = 0;
      char *bits;
      bool parsed_p;
      bool in_memory_file_p = 0;

      /* See if data looks like an in-memory XBM file.  */
      data = image_spec_value (img->spec, QCdata, NULL);
      in_memory_file_p = xbm_file_p (data);

      /* Parse the image specification.  */
      memcpy (fmt, xbm_format, sizeof fmt);
      parsed_p = parse_image_spec (img->spec, fmt, XBM_LAST, Qxbm);
      eassert (parsed_p);

      /* Get specified width, and height.  */
      if (!in_memory_file_p)
	{
	  img->width = XFIXNAT (fmt[XBM_DATA_WIDTH].value);
	  img->height = XFIXNAT (fmt[XBM_DATA_HEIGHT].value);
	  eassert (img->width > 0 && img->height > 0);
	  if (!check_image_size (f, img->width, img->height))
	    {
	      image_size_error ();
	      return 0;
	    }
	}

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[XBM_FOREGROUND].count
	  && STRINGP (fmt[XBM_FOREGROUND].value))
	{
	  foreground = image_alloc_image_color (f,
                                                img,
                                                fmt[XBM_FOREGROUND].value,
                                                foreground);
	  non_default_colors = 1;
	}

      if (fmt[XBM_BACKGROUND].count
	  && STRINGP (fmt[XBM_BACKGROUND].value))
	{
	  background = image_alloc_image_color (f,
                                                img,
                                                fmt[XBM_BACKGROUND].value,
                                                background);
	  non_default_colors = 1;
	}

      if (in_memory_file_p)
	success_p = xbm_load_image (f, img, SSDATA (data),
				    SSDATA (data) + SBYTES (data));
      else
	{
	  USE_SAFE_ALLOCA;

	  if (VECTORP (data))
	    {
	      int i;
	      char *p;
	      int nbytes = (img->width + CHAR_BIT - 1) / CHAR_BIT;

	      SAFE_NALLOCA (bits, nbytes, img->height);
	      p = bits;
	      for (i = 0; i < img->height; ++i, p += nbytes)
		{
		  Lisp_Object line = AREF (data, i);
		  if (STRINGP (line))
		    memcpy (p, SDATA (line), nbytes);
		  else
		    memcpy (p, bool_vector_data (line), nbytes);
		}
	    }
	  else if (STRINGP (data))
	    bits = SSDATA (data);
	  else
	    bits = (char *) bool_vector_data (data);

	  /* Create the pixmap.  */

	  if (image_check_image_size (0, img->width, img->height))
	    Create_Pixmap_From_Bitmap_Data (f, img, bits,
					    foreground, background,
					    non_default_colors);
	  else
	    img->pixmap = NO_PIXMAP;

	  if (img->pixmap)
	    success_p = 1;
	  else
	    {
	      image_error ("Unable to create pixmap for XBM image `%s'",
			   img->spec);
	      image_clear_image (f, img);
	    }

	  SAFE_FREE ();
	}
    }

  return success_p;
}



/***********************************************************************
			      XPM images
 ***********************************************************************/



#ifdef USE_CAIRO

/* Indices of image specification fields in xpm_format, below.  */

enum xpm_keyword_index
{
  XPM_TYPE,
  XPM_FILE,
  XPM_DATA,
  XPM_ASCENT,
  XPM_MARGIN,
  XPM_RELIEF,
  XPM_ALGORITHM,
  XPM_HEURISTIC_MASK,
  XPM_MASK,
  XPM_COLOR_SYMBOLS,
  XPM_BACKGROUND,
  XPM_LAST
};



#ifdef ALLOC_XPM_COLORS

static struct xpm_cached_color *xpm_cache_color (struct frame *, char *,
                                                 XColor *, int);

/* An entry in a hash table used to cache color definitions of named
   colors.  This cache is necessary to speed up XPM image loading in
   case we do color allocations ourselves.  Without it, we would need
   a call to XParseColor per pixel in the image.

   FIXME Now that we're using x_parse_color and its cache, reevaluate
   the need for this caching layer.  */

struct xpm_cached_color
{
  /* Next in collision chain.  */
  struct xpm_cached_color *next;

  /* Color definition (RGB and pixel color).  */
  XColor color;

  /* Color name.  */
  char name[FLEXIBLE_ARRAY_MEMBER];
};

/* The hash table used for the color cache, and its bucket vector
   size (which should be prime).  */

#define XPM_COLOR_CACHE_BUCKETS 1009
static struct xpm_cached_color **xpm_color_cache;

/* Initialize the color cache.  */

static void
xpm_init_color_cache (struct frame *f, XpmAttributes *attrs)
{
  size_t nbytes = XPM_COLOR_CACHE_BUCKETS * sizeof *xpm_color_cache;
  xpm_color_cache = xzalloc (nbytes);
  init_color_table ();

  if (attrs->valuemask & XpmColorSymbols)
    {
      int i;
      XColor color;

      for (i = 0; i < attrs->numsymbols; ++i)
	if (x_parse_color (f, attrs->colorsymbols[i].value, &color))
	  {
	    color.pixel = lookup_rgb_color (f, color.red, color.green,
					    color.blue);
	    xpm_cache_color (f, attrs->colorsymbols[i].name, &color, -1);
	  }
    }
}

/* Free the color cache.  */

static void
xpm_free_color_cache (void)
{
  struct xpm_cached_color *p, *next;
  int i;

  for (i = 0; i < XPM_COLOR_CACHE_BUCKETS; ++i)
    for (p = xpm_color_cache[i]; p; p = next)
      {
	next = p->next;
	xfree (p);
      }

  xfree (xpm_color_cache);
  xpm_color_cache = NULL;
  free_color_table ();
}

/* Return the bucket index for color named COLOR_NAME in the color
   cache.  */

static int
xpm_color_bucket (char *color_name)
{
  EMACS_UINT hash = hash_char_array (color_name, strlen (color_name));
  return hash % XPM_COLOR_CACHE_BUCKETS;
}


/* On frame F, cache values COLOR for color with name COLOR_NAME.
   BUCKET, if >= 0, is a precomputed bucket index.  Value is the cache
   entry added.  */

static struct xpm_cached_color *
xpm_cache_color (struct frame *f, char *color_name, XColor *color, int bucket)
{
  if (bucket < 0)
    bucket = xpm_color_bucket (color_name);

  size_t len = strlen (color_name) + 1;
  size_t nbytes = FLEXSIZEOF (struct xpm_cached_color, name, len);
  struct xpm_cached_color *p = xmalloc (nbytes);
  memcpy (p->name, color_name, len);
  p->color = *color;
  p->next = xpm_color_cache[bucket];
  xpm_color_cache[bucket] = p;
  return p;
}

/* Look up color COLOR_NAME for frame F in the color cache.  If found,
   return the cached definition in *COLOR.  Otherwise, make a new
   entry in the cache and allocate the color.  Value is false if color
   allocation failed.  */

static bool
xpm_lookup_color (struct frame *f, char *color_name, XColor *color)
{
  struct xpm_cached_color *p;
  int h = xpm_color_bucket (color_name);

  for (p = xpm_color_cache[h]; p; p = p->next)
    if (strcmp (p->name, color_name) == 0)
      break;

  if (p != NULL)
    *color = p->color;
  else if (x_parse_color (f, color_name, color))
    {
      color->pixel = lookup_rgb_color (f, color->red, color->green,
				       color->blue);
      p = xpm_cache_color (f, color_name, color, h);
    }
  /* You get `opaque' at least from ImageMagick converting pbm to xpm
     with transparency, and it's useful.  */
  else if (strcmp ("opaque", color_name) == 0)
    {
      memset (color, 0, sizeof (XColor));  /* Is this necessary/correct?  */
      color->pixel = FRAME_FOREGROUND_PIXEL (f);
      p = xpm_cache_color (f, color_name, color, h);
    }

  return p != NULL;
}


/* Callback for allocating color COLOR_NAME.  Called from the XPM lib.
   CLOSURE is a pointer to the frame on which we allocate the
   color.  Return in *COLOR the allocated color.  Value is non-zero
   if successful.  */

static int
xpm_alloc_color (Display *dpy, Colormap cmap, char *color_name, XColor *color,
		 void *closure)
{
  return xpm_lookup_color (closure, color_name, color);
}


/* Callback for freeing NPIXELS colors contained in PIXELS.  CLOSURE
   is a pointer to the frame on which we allocate the color.  Value is
   non-zero if successful.  */

static int
xpm_free_colors (Display *dpy, Colormap cmap, Pixel *pixels, int npixels, void *closure)
{
  return 1;
}

#endif /* ALLOC_XPM_COLORS */


#endif /* USE_CAIRO */


/* Load image IMG which will be displayed on frame F.  Value is
   true if successful.  */





/***********************************************************************
			     Color table
 ***********************************************************************/

#ifdef COLOR_TABLE_SUPPORT

/* An entry in the color table mapping an RGB color to a pixel color.  */

struct ct_color
{
  int r, g, b;
  unsigned long pixel;

  /* Next in color table collision list.  */
  struct ct_color *next;
};

/* The bucket vector size to use.  Must be prime.  */

#define CT_SIZE 101

/* Value is a hash of the RGB color given by R, G, and B.  */

static unsigned
ct_hash_rgb (unsigned r, unsigned g, unsigned b)
{
  return (r << 16) ^ (g << 8) ^ b;
}

/* The color hash table.  */

static struct ct_color **ct_table;

/* Number of entries in the color table.  */

static int ct_colors_allocated;
enum
{
  ct_colors_allocated_max =
    min (INT_MAX,
	 min (PTRDIFF_MAX, SIZE_MAX) / sizeof (unsigned long))
};

/* Initialize the color table.  */

static void
init_color_table (void)
{
  int size = CT_SIZE * sizeof (*ct_table);
  ct_table = xzalloc (size);
  ct_colors_allocated = 0;
}


/* Free memory associated with the color table.  */

static void
free_color_table (void)
{
  int i;
  struct ct_color *p, *next;

  for (i = 0; i < CT_SIZE; ++i)
    for (p = ct_table[i]; p; p = next)
      {
	next = p->next;
	xfree (p);
      }

  xfree (ct_table);
  ct_table = NULL;
}


/* Value is a pixel color for RGB color R, G, B on frame F.  If an
   entry for that color already is in the color table, return the
   pixel color of that entry.  Otherwise, allocate a new color for R,
   G, B, and make an entry in the color table.  */

static unsigned long
lookup_rgb_color (struct frame *f, int r, int g, int b)
{
  unsigned hash = ct_hash_rgb (r, g, b);
  int i = hash % CT_SIZE;
  struct ct_color *p;
  Display_Info *dpyinfo;

  /* Handle TrueColor visuals specially, which improves performance by
     two orders of magnitude.  Freeing colors on TrueColor visuals is
     a nop, and pixel colors specify RGB values directly.  See also
     the Xlib spec, chapter 3.1.  */
  dpyinfo = FRAME_DISPLAY_INFO (f);
  if (dpyinfo->red_bits > 0)
    {
      /* Apply gamma-correction like normal color allocation does.  */
      if (f->gamma)
	{
	  XColor color;
	  color.red = r, color.green = g, color.blue = b;
	  gamma_correct (f, &color);
	  r = color.red, g = color.green, b = color.blue;
	}

      return x_make_truecolor_pixel (dpyinfo, r, g, b);
    }

  for (p = ct_table[i]; p; p = p->next)
    if (p->r == r && p->g == g && p->b == b)
      break;

  if (p == NULL)
    {

      COLORREF color;

      if (ct_colors_allocated_max <= ct_colors_allocated)
	return FRAME_FOREGROUND_PIXEL (f);

      color = RGB_TO_ULONG (r, g, b);
      ++ct_colors_allocated;
      p = xmalloc (sizeof *p);
      p->r = r;
      p->g = g;
      p->b = b;
      p->pixel = color;
      p->next = ct_table[i];
      ct_table[i] = p;

    }

  return p->pixel;
}


/* Look up pixel color PIXEL which is used on frame F in the color
   table.  If not already present, allocate it.  Value is PIXEL.  */

static unsigned long
lookup_pixel_color (struct frame *f, unsigned long pixel)
{
  int i = pixel % CT_SIZE;
  struct ct_color *p;

  for (p = ct_table[i]; p; p = p->next)
    if (p->pixel == pixel)
      break;

  if (p == NULL)
    {
      XColor color;
      Colormap cmap;
      bool rc;

      if (ct_colors_allocated >= ct_colors_allocated_max)
	return FRAME_FOREGROUND_PIXEL (f);

      block_input ();
      cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      color.pixel = pixel;
      XQueryColor (NULL, cmap, &color);
      rc = x_alloc_nearest_color (f, cmap, &color);
      unblock_input ();

      if (rc)
	{
	  ++ct_colors_allocated;

	  p = xmalloc (sizeof *p);
	  p->r = color.red;
	  p->g = color.green;
	  p->b = color.blue;
	  p->pixel = pixel;
	  p->next = ct_table[i];
	  ct_table[i] = p;
	}
      else
	return FRAME_FOREGROUND_PIXEL (f);
    }
  return p->pixel;
}


/* Value is a vector of all pixel colors contained in the color table,
   allocated via xmalloc.  Set *N to the number of colors.  */

static unsigned long *
colors_in_color_table (int *n)
{
  int i, j;
  struct ct_color *p;
  unsigned long *colors;

  if (ct_colors_allocated == 0)
    {
      *n = 0;
      colors = NULL;
    }
  else
    {
      colors = xmalloc (ct_colors_allocated * sizeof *colors);
      *n = ct_colors_allocated;

      for (i = j = 0; i < CT_SIZE; ++i)
	for (p = ct_table[i]; p; p = p->next)
	  colors[j++] = p->pixel;
    }

  return colors;
}

#else /* COLOR_TABLE_SUPPORT */

static unsigned long
lookup_rgb_color (struct frame *f, int r, int g, int b)
{
  xsignal1 (Qfile_error,
	    build_string ("This Emacs mishandles this image file type"));
}

static void
init_color_table (void)
{
}
#endif /* COLOR_TABLE_SUPPORT */


/***********************************************************************
			      Algorithms
 ***********************************************************************/

/* Edge detection matrices for different edge-detection
   strategies.  */

static int emboss_matrix[9] = {
   /* x - 1	x	x + 1  */
        2,     -1,  	  0,		/* y - 1 */
       -1,      0,        1,		/* y     */
        0,      1,       -2		/* y + 1 */
};

static int laplace_matrix[9] = {
   /* x - 1	x	x + 1  */
        1,      0,  	  0,		/* y - 1 */
        0,      0,        0,		/* y     */
        0,      0,       -1		/* y + 1 */
};

/* Value is the intensity of the color whose red/green/blue values
   are R, G, and B.  */

#define COLOR_INTENSITY(R, G, B) ((2 * (R) + 3 * (G) + (B)) / 6)


/* On frame F, return an array of Emacs_Color structures describing image
   IMG->pixmap.  Each Emacs_Color structure has its pixel color set.  RGB_P
   means also fill the red/green/blue members of the Emacs_Color
   structures.  Value is a pointer to the array of Emacs_Color structures,
   allocated with xmalloc; it must be freed by the caller.  */

static Emacs_Color *
image_to_emacs_colors (struct frame *f, struct image *img, bool rgb_p)
{
  int x, y;
  Emacs_Color *colors, *p;
  Emacs_Pix_Context ximg;
  ptrdiff_t nbytes;

  if (ckd_mul (&nbytes, sizeof *colors, img->width)
      || ckd_mul (&nbytes, nbytes, img->height)
      || SIZE_MAX < nbytes)
    memory_full (SIZE_MAX);
  colors = xmalloc (nbytes);

  /* Get the X image or create a memory device context for IMG. */
  ximg = image_get_x_image_or_dc (f, img, 0, &prev);

  /* Fill the `pixel' members of the Emacs_Color array.  I wished there
     were an easy and portable way to circumvent XGetPixel.  */
  p = colors;
  for (y = 0; y < img->height; ++y)
    {
      for (x = 0; x < img->width; ++x, ++p)
	{
	  p->pixel = GET_PIXEL (ximg, x, y);
	  if (rgb_p)
	    {
	      p->red = RED16_FROM_ULONG (p->pixel);
	      p->green = GREEN16_FROM_ULONG (p->pixel);
	      p->blue = BLUE16_FROM_ULONG (p->pixel);
	    }
	}
    }

  image_unget_x_image_or_dc (img, 0, ximg, prev);

  return colors;
}


/* Create IMG->pixmap from an array COLORS of Emacs_Color structures, whose
   RGB members are set.  F is the frame on which this all happens.
   COLORS will be freed; an existing IMG->pixmap will be freed, too.  */

static void
image_from_emacs_colors (struct frame *f, struct image *img, Emacs_Color *colors)
{
  int x, y;
  Emacs_Pix_Container ximage;
  Emacs_Color *p;

  ximage = NULL;

  init_color_table ();

  image_clear_image_1 (f, img, CLEAR_IMAGE_PIXMAP | CLEAR_IMAGE_COLORS);
  image_create_x_image_and_pixmap (f, img, img->width, img->height, 0,
				   &ximage, 0);
  p = colors;
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x, ++p)
      {
	unsigned long pixel;
	pixel = lookup_rgb_color (f, p->red, p->green, p->blue);
	PUT_PIXEL (ximage, x, y, pixel);
      }

  xfree (colors);

  image_put_x_image (f, img, ximage, false);
#ifdef COLOR_TABLE_SUPPORT
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */
}


/* On frame F, perform edge-detection on image IMG.

   MATRIX is a nine-element array specifying the transformation
   matrix.  See emboss_matrix for an example.

   COLOR_ADJUST is a color adjustment added to each pixel of the
   outgoing image.  */

static void
image_detect_edges (struct frame *f, struct image *img,
                    int *matrix, int color_adjust)
{
  Emacs_Color *colors = image_to_emacs_colors (f, img, 1);
  Emacs_Color *new, *p;
  int x, y, i, sum;
  ptrdiff_t nbytes;

  for (i = sum = 0; i < 9; ++i)
    sum += eabs (matrix[i]);

#define COLOR(A, X, Y) ((A) + (Y) * img->width + (X))

  if (ckd_mul (&nbytes, sizeof *new, img->width)
      || ckd_mul (&nbytes, nbytes, img->height))
    memory_full (SIZE_MAX);
  new = xmalloc (nbytes);

  for (y = 0; y < img->height; ++y)
    {
      p = COLOR (new, 0, y);
      p->red = p->green = p->blue = 0xffff/2;
      p = COLOR (new, img->width - 1, y);
      p->red = p->green = p->blue = 0xffff/2;
    }

  for (x = 1; x < img->width - 1; ++x)
    {
      p = COLOR (new, x, 0);
      p->red = p->green = p->blue = 0xffff/2;
      p = COLOR (new, x, img->height - 1);
      p->red = p->green = p->blue = 0xffff/2;
    }

  for (y = 1; y < img->height - 1; ++y)
    {
      p = COLOR (new, 1, y);

      for (x = 1; x < img->width - 1; ++x, ++p)
	{
	  int r, g, b, yy, xx;

	  r = g = b = i = 0;
	  for (yy = y - 1; yy < y + 2; ++yy)
	    for (xx = x - 1; xx < x + 2; ++xx, ++i)
	      if (matrix[i])
	        {
	          Emacs_Color *t = COLOR (colors, xx, yy);
		  r += matrix[i] * t->red;
		  g += matrix[i] * t->green;
		  b += matrix[i] * t->blue;
		}

	  r = (r / sum + color_adjust) & 0xffff;
	  g = (g / sum + color_adjust) & 0xffff;
	  b = (b / sum + color_adjust) & 0xffff;
	  p->red = p->green = p->blue = COLOR_INTENSITY (r, g, b);
	}
    }

  xfree (colors);
  image_from_emacs_colors (f, img, new);

#undef COLOR
}


/* Perform the pre-defined `emboss' edge-detection on image IMG
   on frame F.  */

static void
image_emboss (struct frame *f, struct image *img)
{
  image_detect_edges (f, img, emboss_matrix, 0xffff / 2);
}


/* Transform image IMG which is used on frame F with a Laplace
   edge-detection algorithm.  The result is an image that can be used
   to draw disabled buttons, for example.  */

static void
image_laplace (struct frame *f, struct image *img)
{
  image_detect_edges (f, img, laplace_matrix, 45000);
}


/* Perform edge-detection on image IMG on frame F, with specified
   transformation matrix MATRIX and color-adjustment COLOR_ADJUST.

   MATRIX must be either

   - a list of at least 9 numbers in row-major form
   - a vector of at least 9 numbers

   COLOR_ADJUST nil means use a default; otherwise it must be a
   number.  */

static void
image_edge_detection (struct frame *f, struct image *img,
                      Lisp_Object matrix, Lisp_Object color_adjust)
{
  int i = 0;
  int trans[9];

  if (CONSP (matrix))
    {
      for (i = 0;
	   i < 9 && CONSP (matrix) && NUMBERP (XCAR (matrix));
	   ++i, matrix = XCDR (matrix))
	trans[i] = XFLOATINT (XCAR (matrix));
    }
  else if (VECTORP (matrix) && ASIZE (matrix) >= 9)
    {
      for (i = 0; i < 9 && NUMBERP (AREF (matrix, i)); ++i)
	trans[i] = XFLOATINT (AREF (matrix, i));
    }

  if (NILP (color_adjust))
    color_adjust = make_fixnum (0xffff / 2);

  if (i == 9 && NUMBERP (color_adjust))
    image_detect_edges (f, img, trans, XFLOATINT (color_adjust));
}


static void
image_pixmap_draw_cross (struct frame *f, Emacs_Pixmap pixmap,
			 int x, int y, unsigned int width, unsigned int height,
			 unsigned long color)
{
  cairo_surface_t *surface
    = cairo_image_surface_create_for_data ((unsigned char *) pixmap->data,
					   (pixmap->bits_per_pixel == 32
					    ? CAIRO_FORMAT_RGB24
					    : CAIRO_FORMAT_A8),
					   pixmap->width, pixmap->height,
					   pixmap->bytes_per_line);
  cairo_t *cr = cairo_create (surface);
  cairo_surface_destroy (surface);
  cairo_set_source_rgb (cr, RED_FROM_ULONG (color) / 255.0,
			GREEN_FROM_ULONG (color) / 255.0,
			BLUE_FROM_ULONG (color) / 255.0);
  cairo_move_to (cr, x + 0.5, y + 0.5);
  cairo_rel_line_to (cr, width - 1, height - 1);
  cairo_rel_move_to (cr, 0, - (height - 1));
  cairo_rel_line_to (cr, - (width - 1), height - 1);
  cairo_set_line_width (cr, 1);
  cairo_stroke (cr);
  cairo_destroy (cr);
}

/* Transform image IMG on frame F so that it looks disabled.  */

static void
image_disable_image (struct frame *f, struct image *img)
{
  Display_Info *dpyinfo = FRAME_DISPLAY_INFO (f);
  int n_planes = dpyinfo->n_planes;

  if (n_planes >= 2)
    {
      /* Color (or grayscale).  Convert to gray, and equalize.  Just
	 drawing such images with a stipple can look very odd, so
	 we're using this method instead.  */
      Emacs_Color *colors = image_to_emacs_colors (f, img, 1);
      Emacs_Color *p, *end;
      const int h = 15000;
      const int l = 30000;

      for (p = colors, end = colors + img->width * img->height;
	   p < end;
	   ++p)
	{
	  int i = COLOR_INTENSITY (p->red, p->green, p->blue);
	  int i2 = (0xffff - h - l) * i / 0xffff + l;
	  p->red = p->green = p->blue = i2;
	}

      image_from_emacs_colors (f, img, colors);
    }

  /* Draw a cross over the disabled image, if we must or if we
     should.  */
  if (n_planes < 2 || cross_disabled_images)
    {
#define CrossForeground(f) 0
#define MaskForeground(f)  PIX_MASK_DRAW

      image_pixmap_draw_cross (f, img->pixmap, 0, 0, img->width, img->height,
			       CrossForeground (f));
      if (img->mask)
	image_pixmap_draw_cross (f, img->mask, 0, 0, img->width, img->height,
				 MaskForeground (f));
    }
}


/* Build a mask for image IMG which is used on frame F.  FILE is the
   name of an image file, for error messages.  HOW determines how to
   determine the background color of IMG.  If it is a list '(R G B)',
   with R, G, and B being integers >= 0, take that as the color of the
   background.  Otherwise, determine the background color of IMG
   heuristically.  */

static void
image_build_heuristic_mask (struct frame *f, struct image *img,
                            Lisp_Object how)
{
  Emacs_Pix_Context ximg;
  Emacs_Pix_Container mask_img;
  int x, y;
  bool use_img_background;
  unsigned long bg = 0;

  if (img->mask)
    image_clear_image_1 (f, img, CLEAR_IMAGE_MASK);

  /* Create an image and pixmap serving as mask.  */
  if (! image_create_x_image_and_pixmap (f, img, img->width, img->height, 1,
					 &mask_img, 1))
    return;

  /* Get the X image or create a memory device context for IMG.  */
  ximg = image_get_x_image (f, img, 0);

  /* Determine the background color of ximg.  If HOW is `(R G B)'
     take that as color.  Otherwise, use the image's background color. */
  use_img_background = 1;

  if (CONSP (how))
    {
      int rgb[3], i;

      for (i = 0; i < 3 && CONSP (how) && FIXNATP (XCAR (how)); ++i)
	{
	  rgb[i] = XFIXNAT (XCAR (how)) & 0xffff;
	  how = XCDR (how);
	}

      if (i == 3 && NILP (how))
	{
#ifndef USE_CAIRO
	  Lisp_Object color_name = make_color_name (rgb[0], rgb[1], rgb[2]);
	  bg = image_alloc_image_color (f, img, color_name, 0);
#else  /* USE_CAIRO */
	  bg = lookup_rgb_color (f, rgb[0], rgb[1], rgb[2]);
#endif	/* USE_CAIRO */
	  use_img_background = 0;
	}
    }

  if (use_img_background)
    bg = four_corners_best (ximg, img->corners, img->width, img->height);

  /* Set all bits in mask_img to 1 whose color in ximg is different
     from the background color bg.  */
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x)
      PUT_PIXEL (mask_img, x, y, (GET_PIXEL (ximg, x, y) != bg
				  ? PIX_MASK_DRAW : PIX_MASK_RETAIN));
  /* Fill in the background_transparent field while we have the mask handy. */
  image_background_transparent (img, f, mask_img);

  /* Put mask_img into the image.  */
  image_put_x_image (f, img, mask_img, 1);

  image_unget_x_image_or_dc (img, 0, ximg, prev);
}


/***********************************************************************
		       PBM (mono, gray, color)
 ***********************************************************************/

/* Indices of image specification fields in pbm_format, below.  */

enum pbm_keyword_index
{
  PBM_TYPE,
  PBM_FILE,
  PBM_DATA,
  PBM_ASCENT,
  PBM_MARGIN,
  PBM_RELIEF,
  PBM_ALGORITHM,
  PBM_HEURISTIC_MASK,
  PBM_MASK,
  PBM_FOREGROUND,
  PBM_BACKGROUND,
  PBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword pbm_format[PBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid PBM image specification.  */

static bool
pbm_image_p (Lisp_Object object)
{
  struct image_keyword fmt[PBM_LAST];

  memcpy (fmt, pbm_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, PBM_LAST, Qpbm))
    return 0;

  /* Must specify either :data or :file.  */
  return fmt[PBM_DATA].count + fmt[PBM_FILE].count == 1;
}


/* Get next char skipping comments in Netpbm header.  Returns -1 at
   end of input.  */

static int
pbm_next_char (char **s, char *end)
{
  while (*s < end)
    {
      unsigned char c = *(*s)++;
      if (c != '#')
	return c;
      while (*s < end)
	{
	  c = *(*s)++;
	  if (c == '\n' || c == '\r')
	    break;
	}
    }

  return -1;
}


/* Scan a decimal number from *S and return it.  Advance *S while
   reading the number.  END is the end of the string.  Value is -1 at
   end of input.  */

static int
pbm_scan_number (char **s, char *end)
{
  int c = 0, val = -1;

  /* Skip white-space.  */
  while ((c = pbm_next_char (s, end)) != -1 && c_isspace (c))
    ;

  if (c_isdigit (c))
    {
      /* Read decimal number.  */
      val = c - '0';
      while ((c = pbm_next_char (s, end)) != -1 && c_isdigit (c))
        val = 10 * val + c - '0';
    }

  return val;
}

/* Scan an index from *S and return it.  It is a one-byte unsigned
   index if !TWO_BYTE, and a two-byte big-endian unsigned index if
   TWO_BYTE.  */

static int
pbm_scan_index (char **s, bool two_byte)
{
  char *p = *s;
  unsigned char c0 = *p++;
  int n = c0;
  if (two_byte)
    {
      unsigned char c1 = *p++;
      n = (n << 8) + c1;
    }
  *s = p;
  return n;
}


/* Load PBM image IMG for use on frame F.  */

static bool
pbm_load (struct frame *f, struct image *img)
{
  bool raw_p;
  int x, y;
  int width, height, max_color_idx = 0;
  Lisp_Object specified_file;
  enum {PBM_MONO, PBM_GRAY, PBM_COLOR} type;
  char *contents = NULL;
  char *end, *p;
  Emacs_Pix_Container ximg;

  specified_file = image_spec_value (img->spec, QCfile, NULL);

  if (STRINGP (specified_file))
    {
      ptrdiff_t size;
      contents = slurp_image (specified_file, &size, "PBM");
      if (contents == NULL)
	return false;

      p = contents;
      end = contents + size;
    }
  else
    {
      Lisp_Object data;
      data = image_spec_value (img->spec, QCdata, NULL);
      if (!STRINGP (data))
	{
	  image_invalid_data_error (data);
	  return false;
	}
      p = SSDATA (data);
      end = p + SBYTES (data);
    }

  /* Check magic number.  */
  if (end - p < 2 || *p++ != 'P')
    {
      image_error ("Not a PBM image: `%s'", img->spec);
    error:
      xfree (contents);
      img->pixmap = NO_PIXMAP;
      return 0;
    }

  switch (*p++)
    {
    case '1':
      raw_p = 0, type = PBM_MONO;
      break;

    case '2':
      raw_p = 0, type = PBM_GRAY;
      break;

    case '3':
      raw_p = 0, type = PBM_COLOR;
      break;

    case '4':
      raw_p = 1, type = PBM_MONO;
      break;

    case '5':
      raw_p = 1, type = PBM_GRAY;
      break;

    case '6':
      raw_p = 1, type = PBM_COLOR;
      break;

    default:
      image_error ("Not a PBM image: `%s'", img->spec);
      goto error;
    }

  /* Read width, height, maximum color-component.  Characters
     starting with `#' up to the end of a line are ignored.  */
  width = pbm_scan_number (&p, end);
  height = pbm_scan_number (&p, end);

  if (type != PBM_MONO)
    {
      max_color_idx = pbm_scan_number (&p, end);
      if (max_color_idx > 65535 || max_color_idx < 0)
	{
	  image_error ("Unsupported maximum PBM color value");
	  goto error;
	}
    }

  if (!check_image_size (f, width, height))
    {
      image_size_error ();
      goto error;
    }

  if (!image_create_x_image_and_pixmap (f, img, width, height, 0, &ximg, 0))
    goto error;

  /* Initialize the color hash table.  */
  init_color_table ();

  if (type == PBM_MONO)
    {
      unsigned char c = 0;
      int g;
      struct image_keyword fmt[PBM_LAST];
      unsigned long fg = img->face_foreground;
      unsigned long bg = img->face_background;
      /* Parse the image specification.  */
      memcpy (fmt, pbm_format, sizeof fmt);
      parse_image_spec (img->spec, fmt, PBM_LAST, Qpbm);

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[PBM_FOREGROUND].count
	  && STRINGP (fmt[PBM_FOREGROUND].value))
	fg = image_alloc_image_color (f, img, fmt[PBM_FOREGROUND].value, fg);
      if (fmt[PBM_BACKGROUND].count
	  && STRINGP (fmt[PBM_BACKGROUND].value))
	{
	  bg = image_alloc_image_color (f, img, fmt[PBM_BACKGROUND].value, bg);
	  img->background = bg;
	  img->background_valid = 1;
	}

#ifdef USE_CAIRO
      {
	Emacs_Color fgbg[] = {{.pixel = fg}, {.pixel = bg}};
	FRAME_TERMINAL (f)->query_colors (f, fgbg, ARRAYELTS (fgbg));
	fg = lookup_rgb_color (f, fgbg[0].red, fgbg[0].green, fgbg[0].blue);
	bg = lookup_rgb_color (f, fgbg[1].red, fgbg[1].green, fgbg[1].blue);
      }
#endif
      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    if (raw_p)
	      {
		if ((x & 7) == 0)
		  {
		    if (p >= end)
		      {
			image_destroy_x_image (ximg);
			image_clear_image (f, img);
			image_error ("Invalid image size in image `%s'",
				     img->spec);
			goto error;
		      }
		    c = *p++;
		  }
		g = c & 0x80;
		c <<= 1;
	      }
	    else
	      {
		int c = 0;
		/* Skip white-space and comments.  */
		while ((c = pbm_next_char (&p, end)) != -1 && c_isspace (c))
		  ;

		if (c == '0' || c == '1')
		  g = c - '0';
		else
		  g = 0;
	      }

	    PUT_PIXEL (ximg, x, y, g ? fg : bg);
	  }
    }
  else
    {
      int expected_size = height * width;
      bool two_byte = 255 < max_color_idx;
      if (two_byte)
	expected_size *= 2;
      if (type == PBM_COLOR)
	expected_size *= 3;

      if (raw_p && p + expected_size > end)
	{
	  image_destroy_x_image (ximg);
	  image_clear_image (f, img);
	  image_error ("Invalid image size in image `%s'", img->spec);
	  goto error;
	}

      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    int r, g, b;

	    if (type == PBM_GRAY && raw_p)
	      r = g = b = pbm_scan_index (&p, two_byte);
	    else if (type == PBM_GRAY)
	      r = g = b = pbm_scan_number (&p, end);
	    else if (raw_p)
	      {
		r = pbm_scan_index (&p, two_byte);
		g = pbm_scan_index (&p, two_byte);
		b = pbm_scan_index (&p, two_byte);
	      }
	    else
	      {
		r = pbm_scan_number (&p, end);
		g = pbm_scan_number (&p, end);
		b = pbm_scan_number (&p, end);
	      }

	    if (r < 0 || g < 0 || b < 0)
	      {
		image_destroy_x_image (ximg);
		image_error ("Invalid pixel value in image `%s'", img->spec);
		goto error;
	      }

	    /* RGB values are now in the range 0..max_color_idx.
	       Scale this to the range 0..0xffff supported by X.  */
	    r = (double) r * 65535 / max_color_idx;
	    g = (double) g * 65535 / max_color_idx;
	    b = (double) b * 65535 / max_color_idx;
	    PUT_PIXEL (ximg, x, y, lookup_rgb_color (f, r, g, b));
	  }
    }

#ifdef COLOR_TABLE_SUPPORT
  /* Store in IMG->colors the colors allocated for the image, and
     free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif /* COLOR_TABLE_SUPPORT */

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy.  */

  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Casting avoids a GCC warning.  */
    IMAGE_BACKGROUND (img, f, (Emacs_Pix_Context)ximg);

  /* Put ximg into the image.  */
  image_put_x_image (f, img, ximg, 0);

  /* X and W32 versions did it here, MAC version above.  ++kfs
     img->width = width;
     img->height = height; */

  xfree (contents);
  return 1;
}


/***********************************************************************
			    NATIVE IMAGE HANDLING
 ***********************************************************************/



/***********************************************************************
				 PNG
 ***********************************************************************/






/***********************************************************************
				 JPEG
 ***********************************************************************/

#if defined (HAVE_JPEG)

/* Indices of image specification fields in jpeg_format, below.  */

enum jpeg_keyword_index
{
  JPEG_TYPE,
  JPEG_DATA,
  JPEG_FILE,
  JPEG_ASCENT,
  JPEG_MARGIN,
  JPEG_RELIEF,
  JPEG_ALGORITHM,
  JPEG_HEURISTIC_MASK,
  JPEG_MASK,
  JPEG_BACKGROUND,
  JPEG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword jpeg_format[JPEG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid JPEG image specification.  */

static bool
jpeg_image_p (Lisp_Object object)
{
  struct image_keyword fmt[JPEG_LAST];

  memcpy (fmt, jpeg_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, JPEG_LAST, Qjpeg))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[JPEG_FILE].count + fmt[JPEG_DATA].count == 1;
}

#endif /* HAVE_JPEG */




/***********************************************************************
				 TIFF
 ***********************************************************************/

#if defined (HAVE_TIFF)

/* Indices of image specification fields in tiff_format, below.  */

enum tiff_keyword_index
{
  TIFF_TYPE,
  TIFF_DATA,
  TIFF_FILE,
  TIFF_ASCENT,
  TIFF_MARGIN,
  TIFF_RELIEF,
  TIFF_ALGORITHM,
  TIFF_HEURISTIC_MASK,
  TIFF_MASK,
  TIFF_BACKGROUND,
  TIFF_INDEX,
  TIFF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword tiff_format[TIFF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":index",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0}
};

/* Return true if OBJECT is a valid TIFF image specification.  */

static bool
tiff_image_p (Lisp_Object object)
{
  struct image_keyword fmt[TIFF_LAST];
  memcpy (fmt, tiff_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, TIFF_LAST, Qtiff))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[TIFF_FILE].count + fmt[TIFF_DATA].count == 1;
}

#endif /* HAVE_TIFF */




/***********************************************************************
				 GIF
 ***********************************************************************/

#if defined (HAVE_GIF)

/* Indices of image specification fields in gif_format, below.  */

enum gif_keyword_index
{
  GIF_TYPE,
  GIF_DATA,
  GIF_FILE,
  GIF_ASCENT,
  GIF_MARGIN,
  GIF_RELIEF,
  GIF_ALGORITHM,
  GIF_HEURISTIC_MASK,
  GIF_MASK,
  GIF_IMAGE,
  GIF_BACKGROUND,
  GIF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword gif_format[GIF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":index",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid GIF image specification.  */

static bool
gif_image_p (Lisp_Object object)
{
  struct image_keyword fmt[GIF_LAST];
  memcpy (fmt, gif_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, GIF_LAST, Qgif))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[GIF_FILE].count + fmt[GIF_DATA].count == 1;
}

#endif /* HAVE_GIF */








/***********************************************************************
				 SVG
 ***********************************************************************/





/***********************************************************************
				Ghostscript
 ***********************************************************************/


#ifdef HAVE_GHOSTSCRIPT

/* Indices of image specification fields in gs_format, below.  */

enum gs_keyword_index
{
  GS_TYPE,
  GS_PT_WIDTH,
  GS_PT_HEIGHT,
  GS_FILE,
  GS_LOADER,
  GS_BOUNDING_BOX,
  GS_ASCENT,
  GS_MARGIN,
  GS_RELIEF,
  GS_ALGORITHM,
  GS_HEURISTIC_MASK,
  GS_MASK,
  GS_BACKGROUND,
  GS_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static const struct image_keyword gs_format[GS_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":pt-width",		IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":pt-height",	IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":loader",		IMAGE_FUNCTION_VALUE,			0},
  {":bounding-box",	IMAGE_DONT_CHECK_VALUE_TYPE,		1},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR, 0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Return true if OBJECT is a valid Ghostscript image
   specification.  */

static bool
gs_image_p (Lisp_Object object)
{
  struct image_keyword fmt[GS_LAST];
  Lisp_Object tem;
  int i;

  memcpy (fmt, gs_format, sizeof fmt);

  if (!parse_image_spec (object, fmt, GS_LAST, Qpostscript))
    return 0;

  /* Bounding box must be a list or vector containing 4 integers.  */
  tem = fmt[GS_BOUNDING_BOX].value;
  if (CONSP (tem))
    {
      for (i = 0; i < 4; ++i, tem = XCDR (tem))
	if (!CONSP (tem) || !FIXNUMP (XCAR (tem)))
	  return 0;
      if (!NILP (tem))
	return 0;
    }
  else if (VECTORP (tem))
    {
      if (ASIZE (tem) != 4)
	return 0;
      for (i = 0; i < 4; ++i)
	if (!FIXNUMP (AREF (tem, i)))
	  return 0;
    }
  else
    return 0;

  return 1;
}


/* Load Ghostscript image IMG for use on frame F.  Value is true
   if successful.  */

static bool
gs_load (struct frame *f, struct image *img)
{
  uintmax_t printnum1, printnum2;
  Lisp_Object window_and_pixmap_id = Qnil, loader, pt_height, pt_width;
  Lisp_Object frame;
  double in_width, in_height;
  Lisp_Object pixel_colors = Qnil;

  /* Compute pixel size of pixmap needed from the given size in the
     image specification.  Sizes in the specification are in pt.  1 pt
     = 1/72 in, xdpi and ydpi are stored in the frame's X display
     info.  */
  pt_width = image_spec_value (img->spec, QCpt_width, NULL);
  in_width = FIXNUMP (pt_width) ? XFIXNAT (pt_width) / 72.0 : 0;
  in_width *= FRAME_RES_X (f);
  pt_height = image_spec_value (img->spec, QCpt_height, NULL);
  in_height = FIXNUMP (pt_height) ? XFIXNAT (pt_height) / 72.0 : 0;
  in_height *= FRAME_RES_Y (f);

  if (! (in_width <= INT_MAX && in_height <= INT_MAX
	 && check_image_size (f, in_width, in_height)))
    {
      image_size_error ();
      return 0;
    }
  img->width = in_width;
  img->height = in_height;

  /* Create the pixmap.  */
  eassert (img->pixmap == NO_PIXMAP);

  if (image_check_image_size (0, img->width, img->height))
    {
      /* Only W32 version did BLOCK_INPUT here.  ++kfs */
      block_input ();
      img->pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				   img->width, img->height,
				   FRAME_DISPLAY_INFO (f)->n_planes);
      unblock_input ();
    }

  if (!img->pixmap)
    {
      image_error ("Unable to create pixmap for `%s'" , img->spec);
      return 0;
    }

  /* Call the loader to fill the pixmap.  It returns a process object
     if successful.  We do not record_unwind_protect here because
     other places in redisplay like calling window scroll functions
     don't either.  Let the Lisp loader use `unwind-protect' instead.  */
  printnum1 = FRAME_X_DRAWABLE (f);
  printnum2 = img->pixmap;
  window_and_pixmap_id
    = make_formatted_string ("%"PRIuMAX" %"PRIuMAX,
			     printnum1, printnum2);

  printnum1 = FRAME_FOREGROUND_PIXEL (f);
  printnum2 = FRAME_BACKGROUND_PIXEL (f);
  pixel_colors
    = make_formatted_string ("%"PRIuMAX" %"PRIuMAX,
			     printnum1, printnum2);

  XSETFRAME (frame, f);
  loader = image_spec_value (img->spec, QCloader, NULL);
  if (NILP (loader))
    loader = Qgs_load_image;

  img->lisp_data = calln (loader, frame, img->spec,
			  make_fixnum (img->width),
			  make_fixnum (img->height),
			  window_and_pixmap_id,
			  pixel_colors);
  return PROCESSP (img->lisp_data);
}


/* Kill the Ghostscript process that was started to fill PIXMAP on
   frame F.  Called from XTread_socket when receiving an event
   telling Emacs that Ghostscript has finished drawing.  */

void
x_kill_gs_process (Pixmap pixmap, struct frame *f)
{
  struct image_cache *c = FRAME_IMAGE_CACHE (f);
  ptrdiff_t i;
  struct image *img;

  /* Find the image containing PIXMAP.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i]->pixmap == pixmap)
      break;

  /* Should someone in between have cleared the image cache, for
     instance, give up.  */
  if (i == c->used)
    return;

  /* Kill the GS process.  We should have found PIXMAP in the image
     cache and its image should contain a process object.  */
  img = c->images[i];
  eassert (PROCESSP (img->lisp_data));
  Fkill_process (img->lisp_data, Qnil);
  img->lisp_data = Qnil;


  /* Now that we have the pixmap, compute mask and transform the
     image if requested.  */
  block_input ();
  postprocess_image (f, img);
  unblock_input ();
}

#endif /* HAVE_GHOSTSCRIPT */


/***********************************************************************
				Tests
 ***********************************************************************/
DEFUN ("imagep", Fimagep, Simagep, 1, 1, 0,
       doc: /* Value is non-nil if SPEC is a valid image specification.  */)
  (Lisp_Object spec)
{
  return valid_image_p (spec) ? Qt : Qnil;
}



/***********************************************************************
			    Initialization
 ***********************************************************************/

DEFUN ("image-transforms-p", Fimage_transforms_p, Simage_transforms_p, 0, 1, 0,
       doc: /* Test whether FRAME supports image transformation.
Return list of capabilities if FRAME supports native transforms, nil otherwise.
FRAME defaults to the selected frame.
The list of capabilities can include one or more of the following:

 - the symbol `scale' if FRAME supports image scaling
 - the symbol `rotate90' if FRAME supports image rotation only by angles
    that are integral multiples of 90 degrees.  */)
     (Lisp_Object frame)
{
  struct frame *f = decode_live_frame (frame);
  if (FRAME_WINDOW_P (f))
    {
    }

  return Qnil;
}

DEFUN ("image-cache-size", Fimage_cache_size, Simage_cache_size, 0, 0, 0,
       doc: /* Return the size of the image cache.  */)
  (void)
{
  Lisp_Object tail, frame;
  intmax_t total = 0;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_WINDOW_P (XFRAME (frame)))
      total += image_frame_cache_size (XFRAME (frame));


  return make_int (total);
}


DEFUN ("init-image-library", Finit_image_library, Sinit_image_library, 1, 1, 0,
       doc: /* Initialize image library implementing image type TYPE.
Return t if TYPE is a supported image type.

If image libraries are loaded dynamically (currently the case only on
MS-Windows), load the library for TYPE if it is not yet loaded, using
the library file(s) specified by `dynamic-library-alist'.  */)
  (Lisp_Object type)
{
  return lookup_image_type (type) ? Qt : Qnil;
}

static bool
initialize_image_type (struct image_type const *type)
{
  return true;
}

/* Neomacs image type - images loaded via GPU backend (gdk-pixbuf).
   This allows PNG, JPEG, GIF, WebP, SVG support without native libraries.
   The actual image loading is done by neomacsterm.c via the Rust backend. */

/* Indices of keywords in neomacs_format.  */
enum neomacs_keyword_index
{
  NEOMACS_TYPE,
  NEOMACS_FILE,
  NEOMACS_DATA,
  NEOMACS_ID,
  NEOMACS_WIDTH,
  NEOMACS_HEIGHT,
  NEOMACS_MAX_WIDTH,
  NEOMACS_MAX_HEIGHT,
  NEOMACS_SCALE,
  NEOMACS_ASCENT,
  NEOMACS_MARGIN,
  NEOMACS_RELIEF,
  NEOMACS_CONVERSION,
  NEOMACS_HEURISTIC_MASK,
  NEOMACS_MASK,
  NEOMACS_BACKGROUND,
  NEOMACS_INDEX,
  NEOMACS_FOREGROUND,
  NEOMACS_LAST
};

static const struct image_keyword neomacs_format[NEOMACS_LAST] =
{
  {":type",            IMAGE_SYMBOL_VALUE,                        1},
  {":file",            IMAGE_STRING_VALUE,                        0},
  {":data",            IMAGE_STRING_VALUE,                        0},
  {":neomacs-id",      IMAGE_POSITIVE_INTEGER_VALUE,              0},
  {":width",           IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":height",          IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":max-width",       IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":max-height",      IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":scale",           IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":ascent",          IMAGE_ASCENT_VALUE,                        0},
  {":margin",          IMAGE_NON_NEGATIVE_INTEGER_VALUE_OR_PAIR,  0},
  {":relief",          IMAGE_INTEGER_VALUE,                       0},
  {":conversion",      IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":heuristic-mask",  IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":mask",            IMAGE_DONT_CHECK_VALUE_TYPE,               0},
  {":background",      IMAGE_STRING_OR_NIL_VALUE,                 0},
  {":index",           IMAGE_NON_NEGATIVE_INTEGER_VALUE,          0},
  {":foreground",      IMAGE_STRING_OR_NIL_VALUE,                 0},
};

/* Return true if OBJECT is a valid neomacs image specification.  */
static bool
neomacs_image_p (Lisp_Object object)
{
  struct image_keyword kw[NEOMACS_LAST];
  memcpy (kw, neomacs_format, sizeof kw);

  if (!parse_image_spec (object, kw, NEOMACS_LAST, Qneomacs))
    return 0;

  /* Must have either :file, :data, or :neomacs-id.  */
  if (!kw[NEOMACS_FILE].count && !kw[NEOMACS_DATA].count && !kw[NEOMACS_ID].count)
    return 0;

  return 1;
}

/* Generate neomacs-backed validator for standard image type.
   These reuse the neomacs keyword format but accept a different :type symbol,
   allowing (image-type-available-p 'png) etc. to return t.  */
#define NEOMACS_TYPE_VALIDATOR(typename, typesym)                       \
static bool                                                             \
neomacs_##typename##_image_p (Lisp_Object object)                      \
{                                                                       \
  struct image_keyword kw[NEOMACS_LAST];                               \
  memcpy (kw, neomacs_format, sizeof kw);                              \
  if (!parse_image_spec (object, kw, NEOMACS_LAST, typesym))           \
    return 0;                                                           \
  if (!kw[NEOMACS_FILE].count && !kw[NEOMACS_DATA].count               \
      && !kw[NEOMACS_ID].count)                                        \
    return 0;                                                           \
  return 1;                                                             \
}

NEOMACS_TYPE_VALIDATOR(png, Qpng)
NEOMACS_TYPE_VALIDATOR(jpeg, Qjpeg)
NEOMACS_TYPE_VALIDATOR(gif, Qgif)
NEOMACS_TYPE_VALIDATOR(tiff, Qtiff)
NEOMACS_TYPE_VALIDATOR(webp, Qwebp)
NEOMACS_TYPE_VALIDATOR(svg, Qsvg)
NEOMACS_TYPE_VALIDATOR(xpm, Qxpm)

/* Load a neomacs image.
   Query dimensions from GPU backend for proper scaling.  */
static bool
neomacs_load (struct frame *f, struct image *img)
{
  struct neomacs_display_info *dpyinfo = neomacs_display_list;
  if (!dpyinfo || !dpyinfo->display_handle)
    {
      /* No display - use defaults */
      img->width = 100;
      img->height = 100;
      img->background_valid = 1;
      img->background = 0;
      img->background_transparent_valid = 1;
      img->background_transparent = 1;
      return true;
    }

  /* Get image spec properties */
  Lisp_Object neomacs_id = image_spec_value (img->spec, intern (":neomacs-id"), NULL);
  Lisp_Object file = image_spec_value (img->spec, QCfile, NULL);
  Lisp_Object data = image_spec_value (img->spec, QCdata, NULL);
  Lisp_Object width = image_spec_value (img->spec, QCwidth, NULL);
  Lisp_Object height = image_spec_value (img->spec, QCheight, NULL);
  Lisp_Object max_width = image_spec_value (img->spec, QCmax_width, NULL);
  Lisp_Object max_height = image_spec_value (img->spec, QCmax_height, NULL);
  Lisp_Object scale = image_spec_value (img->spec, QCscale, NULL);

  int tw = FIXNUMP (width) ? XFIXNUM (width) : 0;      /* target width */
  int th = FIXNUMP (height) ? XFIXNUM (height) : 0;    /* target height */
  int mw = FIXNUMP (max_width) ? XFIXNUM (max_width) : 0;
  int mh = FIXNUMP (max_height) ? XFIXNUM (max_height) : 0;
  double sc = NUMBERP (scale) ? XFLOATINT (scale) : 1.0;

  int actual_w = 0, actual_h = 0;

  if (FIXNUMP (neomacs_id))
    {
      /* Pre-loaded by neomacs-insert-image - get dimensions from cache */
      uint32_t gpu_id = (uint32_t) XFIXNUM (neomacs_id);
      neomacs_display_get_image_size (dpyinfo->display_handle, gpu_id,
                                       &actual_w, &actual_h);
    }
  else if (STRINGP (file))
    {
      /* Query image dimensions without loading into cache */
      const char *path = SSDATA (file);
      neomacs_display_query_image_file_size (dpyinfo->display_handle, path,
                                              &actual_w, &actual_h);

      /* Apply max constraints if specified */
      if (actual_w > 0 && actual_h > 0 && (mw > 0 || mh > 0))
        {
          double ratio = (double)actual_w / actual_h;
          if (mw > 0 && actual_w > mw)
            {
              actual_w = mw;
              actual_h = (int)(mw / ratio);
            }
          if (mh > 0 && actual_h > mh)
            {
              actual_h = mh;
              actual_w = (int)(mh * ratio);
            }
        }
    }
  else if (STRINGP (data))
    {
      /* Inline image data — load via Rust and store GPU ID to avoid
         double-loading in neomacs_load_image later.  */
      const unsigned char *bytes = (const unsigned char *) SDATA (data);
      ptrdiff_t len = SBYTES (data);

      /* Query dimensions synchronously (reads header only, no GPU) */
      neomacs_display_query_image_data_size (dpyinfo->display_handle,
                                              bytes, len,
                                              &actual_w, &actual_h);

      /* Apply max constraints if specified */
      if (actual_w > 0 && actual_h > 0 && (mw > 0 || mh > 0))
        {
          double ratio = (double)actual_w / actual_h;
          if (mw > 0 && actual_w > mw)
            {
              actual_w = mw;
              actual_h = (int)(mw / ratio);
            }
          if (mh > 0 && actual_h > mh)
            {
              actual_h = mh;
              actual_w = (int)(mh * ratio);
            }
        }

      /* Load via Rust and store GPU ID on the image struct */
      struct NeomacsImageLoadInfo load_info;
      memset (&load_info, 0, sizeof (load_info));
      load_info.encoded_data = bytes;
      load_info.encoded_data_len = len;
      load_info.max_width = mw;
      load_info.max_height = mh;
      load_info.scale = 1.0;
      load_info.img_width = actual_w;
      load_info.img_height = actual_h;
      struct NeomacsImageLoadResult result = neomacs_rust_load_image (&load_info);
      if (result.gpu_id != 0)
        img->neomacs_gpu_id = result.gpu_id;
    }

  if (actual_w > 0 && actual_h > 0)
    {
      /* Apply :scale if specified */
      if (sc != 1.0 && sc > 0)
        {
          actual_w = (int)(actual_w * sc);
          actual_h = (int)(actual_h * sc);
        }

      /* Compute final dimensions respecting :width/:height with aspect ratio */
      if (tw > 0 && th > 0)
        {
          /* Both specified - use as-is */
          img->width = tw;
          img->height = th;
        }
      else if (tw > 0)
        {
          /* Width specified - compute height preserving aspect ratio */
          img->width = tw;
          img->height = (actual_h > 0) ? (int)((double)tw * actual_h / actual_w) : tw;
        }
      else if (th > 0)
        {
          /* Height specified - compute width preserving aspect ratio */
          img->height = th;
          img->width = (actual_w > 0) ? (int)((double)th * actual_w / actual_h) : th;
        }
      else
        {
          /* Use (possibly scaled) actual dimensions */
          img->width = actual_w;
          img->height = actual_h;
        }
    }
  else
    {
      /* Fallback to explicit dimensions or defaults */
      if (tw > 0 && th > 0)
        {
          img->width = tw;
          img->height = th;
        }
      else
        {
          img->width = 100;
          img->height = 100;
        }
    }

  /* Set pixmap to a sentinel value so prepare_image_for_display won't
     re-call neomacs_load on every redisplay.  The actual GPU loading is
     done lazily by neomacs_get_or_load_image in neomacsterm.c.  */
  img->pixmap = (Emacs_Pixmap) 1;

  /* Mark background as valid */
  img->background_valid = 1;
  img->background = 0;
  img->background_transparent_valid = 1;
  img->background_transparent = 1;

  return true;
}

/* Clear a neomacs GPU-backed image.  neomacs_load sets img->pixmap to a
   sentinel value (1) to prevent prepare_image_for_display from
   re-calling load on every redisplay.  We must reset it before calling
   the generic clear function, which would try to free_pixmap on it.  */
static void
neomacs_clear_image (struct frame *f, struct image *img)
{
  /* Free GPU resource if uploaded */
  if (img->neomacs_gpu_id != 0)
    {
      struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);
      if (dpyinfo && dpyinfo->display_handle)
        neomacs_display_free_image (dpyinfo->display_handle,
                                    img->neomacs_gpu_id);
      img->neomacs_gpu_id = 0;
    }

  if (img->pixmap == (Emacs_Pixmap) 1)
    img->pixmap = NO_PIXMAP;
  image_clear_image (f, img);
}

/* Array of supported image types.  */

static struct image_type const image_types[] =
{
 /* Neomacs GPU-backed image types (unconditional, Rust decoding) */
 { SYMBOL_INDEX (Qpng),  neomacs_png_image_p,  neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qjpeg), neomacs_jpeg_image_p, neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qgif),  neomacs_gif_image_p,  neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qtiff), neomacs_tiff_image_p, neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qwebp), neomacs_webp_image_p, neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qsvg),  neomacs_svg_image_p,  neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qxpm),  neomacs_xpm_image_p,  neomacs_load, neomacs_clear_image },
#ifdef HAVE_GHOSTSCRIPT
 { SYMBOL_INDEX (Qpostscript), gs_image_p, gs_load, image_clear_image },
#endif
#if defined HAVE_GIF
 { SYMBOL_INDEX (Qgif), gif_image_p, gif_load, image_clear_image,
   IMAGE_TYPE_INIT (init_gif_functions) },
#endif
#if defined HAVE_TIFF
 { SYMBOL_INDEX (Qtiff), tiff_image_p, tiff_load, image_clear_image,
   IMAGE_TYPE_INIT (init_tiff_functions) },
#endif
#if defined HAVE_JPEG
 { SYMBOL_INDEX (Qjpeg), jpeg_image_p, jpeg_load, image_clear_image,
   IMAGE_TYPE_INIT (init_jpeg_functions) },
#endif
#if defined HAVE_WEBP
 { SYMBOL_INDEX (Qwebp), webp_image_p, webp_load, image_clear_image,
   IMAGE_TYPE_INIT (init_webp_functions) },
#endif
 { SYMBOL_INDEX (Qneomacs), neomacs_image_p, neomacs_load, neomacs_clear_image },
 { SYMBOL_INDEX (Qxbm), xbm_image_p, xbm_load, image_clear_image },
 { SYMBOL_INDEX (Qpbm), pbm_image_p, pbm_load, image_clear_image },
};


/* Look up image TYPE, and return a pointer to its image_type structure.
   Return a null pointer if TYPE is not a known image type.  */

static struct image_type const *
lookup_image_type (Lisp_Object type)
{
  for (int i = 0; i < ARRAYELTS (image_types); i++)
    {
      struct image_type const *r = &image_types[i];
      if (EQ (type, builtin_lisp_symbol (r->type)))
	return initialize_image_type (r) ? r : NULL;
    }
  return NULL;
}

/* Prune old entries from the animation cache.
   If CLEAR, remove all animation cache entries.  */
void
image_prune_animation_caches (bool clear)
{
  /* FIXME: Consolidate these animation cache implementations.  */
}

void
syms_of_image (void)
{
  /* Must be defined now because we're going to update it below, while
     defining the supported image types.  */
  DEFVAR_LISP ("image-types", Vimage_types,
    doc: /* List of potentially supported image types.
Each element of the list is a symbol for an image type, like `jpeg' or `png'.
To check whether it is really supported, use `image-type-available-p'.  */);
  Vimage_types = Qnil;

  DEFVAR_LISP ("max-image-size", Vmax_image_size,
    doc: /* Maximum size of images.
Emacs will not load an image into memory if its pixel width or
pixel height exceeds this limit.

If the value is an integer, it directly specifies the maximum
image height and width, measured in pixels.  If it is a floating
point number, it specifies the maximum image height and width
as a ratio to the frame height and width.  If the value is
non-numeric, there is no explicit limit on the size of images.  */);
  Vmax_image_size = make_float (MAX_IMAGE_SIZE);

  /* Other symbols.  */
  DEFSYM (Qcount, "count");
  DEFSYM (Qextension_data, "extension-data");
  DEFSYM (Qdelay, "delay");
  DEFSYM (Qauto, "auto");

  /* Keywords.  */
  DEFSYM (QCascent, ":ascent");
  DEFSYM (QCmargin, ":margin");
  DEFSYM (QCrelief, ":relief");
  DEFSYM (QCconversion, ":conversion");
  DEFSYM (QCcolor_symbols, ":color-symbols");
  DEFSYM (QCheuristic_mask, ":heuristic-mask");
  DEFSYM (QCindex, ":index");
  DEFSYM (QCcrop, ":crop");
  DEFSYM (QCrotation, ":rotation");
  DEFSYM (QCmatrix, ":matrix");
  DEFSYM (QCscale, ":scale");
  DEFSYM (QCtransform_smoothing, ":transform-smoothing");
  DEFSYM (QCcolor_adjustment, ":color-adjustment");
  DEFSYM (QCmask, ":mask");
  DEFSYM (QCflip, ":flip");

  /* Other symbols.  */
  DEFSYM (Qlaplace, "laplace");
  DEFSYM (Qemboss, "emboss");
  DEFSYM (Qedge_detection, "edge-detection");
  DEFSYM (Qheuristic, "heuristic");

  DEFSYM (Qpostscript, "postscript");
  DEFSYM (QCmax_width, ":max-width");
  DEFSYM (QCmax_height, ":max-height");

  DEFSYM (Qem, "em");
  DEFSYM (Qch, "ch");
  DEFSYM (Qcw, "cw");


#ifdef HAVE_GHOSTSCRIPT
  add_image_type (Qpostscript);
  DEFSYM (QCloader, ":loader");
  DEFSYM (QCpt_width, ":pt-width");
  DEFSYM (QCpt_height, ":pt-height");
  DEFSYM (Qgs_load_image, "gs-load-image");
#endif /* HAVE_GHOSTSCRIPT */


  DEFSYM (Qpbm, "pbm");
  add_image_type (Qpbm);

  DEFSYM (Qxbm, "xbm");
  add_image_type (Qxbm);

  DEFSYM (Qneomacs, "neomacs");
  add_image_type (Qneomacs);

  /* Neomacs: register all standard image types unconditionally.
     Rust decodes PNG/JPEG/GIF/TIFF/WebP via the `image` crate
     and SVG via `resvg` — no C library dependencies needed.  */
  DEFSYM (Qpng, "png");   add_image_type (Qpng);
  DEFSYM (Qjpeg, "jpeg"); add_image_type (Qjpeg);
  DEFSYM (Qgif, "gif");   add_image_type (Qgif);
  DEFSYM (Qtiff, "tiff"); add_image_type (Qtiff);
  DEFSYM (Qwebp, "webp"); add_image_type (Qwebp);
  DEFSYM (Qsvg, "svg");   add_image_type (Qsvg);
  DEFSYM (Qxpm, "xpm");   add_image_type (Qxpm);






#if defined (HAVE_WEBP)						\
  || (defined (HAVE_NATIVE_IMAGE_API)				\
      && (defined (HAVE_NS) || defined (HAVE_HAIKU)))
  DEFSYM (Qwebp, "webp");
  DEFSYM (Qwebpdemux, "webpdemux");
#endif





  defsubr (&Sinit_image_library);
  defsubr (&Sclear_image_cache);
  defsubr (&Simage_flush);
  defsubr (&Simage_size);
  defsubr (&Simage_mask_p);
  defsubr (&Simage_metadata);
  defsubr (&Simage_cache_size);
  defsubr (&Simagep);


  DEFSYM (QCanimate_buffer, ":animate-buffer");
  DEFSYM (QCanimate_tardiness, ":animate-tardiness");
  DEFSYM (QCanimate_position, ":animate-position");

  defsubr (&Simage_transforms_p);

  DEFVAR_BOOL ("cross-disabled-images", cross_disabled_images,
    doc: /* Non-nil means always draw a cross over disabled images.
Disabled images are those having a `:conversion disabled' property.
A cross is always drawn on black & white displays.  */);
  cross_disabled_images = 0;

  DEFVAR_LISP ("x-bitmap-file-path", Vx_bitmap_file_path,
    doc: /* List of directories to search for window system bitmap files.  */);
  Vx_bitmap_file_path = decode_env_path (0, PATH_BITMAPS, 0);

  DEFVAR_LISP ("image-cache-eviction-delay", Vimage_cache_eviction_delay,
    doc: /* Maximum time after which images are removed from the cache.
When an image has not been displayed this many seconds, Emacs
automatically removes it from the image cache.  If the cache contains
a large number of images, the actual eviction time may be shorter.
The value can also be nil, meaning the cache is never cleared.

The function `clear-image-cache' disregards this variable.  */);
  Vimage_cache_eviction_delay = make_fixnum (300);

  DEFVAR_LISP ("image-scaling-factor", Vimage_scaling_factor,
    doc: /* When displaying images, apply this scaling factor before displaying.
This is not supported for all image types, and is mostly useful
when you have a high-resolution monitor.
The value is either a floating point number (where numbers higher
than 1 means to increase the size and lower means to shrink the
size), or the symbol `auto', which will compute a scaling factor
based on the font pixel size.  */);
  Vimage_scaling_factor = Qauto;

}
