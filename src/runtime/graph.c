/* Stub code for the graphic primitives */
/* This version has been modified for Moscow ML and won't work for 
   Caml Light any more */

#include <math.h>
#include <grx.h>
#include <mousex.h>
#include <dos.h>
#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "fail.h"

#define Unit Atom(0)

int size_x, size_y, max_x, max_y;
int cx, cy;
int cc;

#define Cvt(y) (max_y - (y))

static void graphic_failure(msg)
     char * msg;
{
  raise_with_string(SYS__EXN_GRAPHIC, msg);
}

value gr_open_graph(mode)       /* ML */
     value mode;
{
  char * mode_name;
  int mode_code, width, height;
  
  mode_name = String_val(mode);
  mode_code = GR_default_graphics;
  width = 0;
  height = 0;
  if (strcmp(mode_name, "biggest") == 0) {
    mode_code = GR_biggest_graphics;
  } else if (strcmp(mode_name, "noninterlaced") == 0) {
    mode_code = GR_biggest_noninterlaced_graphics;
  } else if (strcmp(mode_name, "320x200") == 0) {
    mode_code = GR_320_200_graphics;
  } else if (sscanf(mode_name, "%dx%d", &width, &height) == 2) {
    mode_code = GR_width_height_graphics;
  }
  GrSetMode(mode_code, width, height);
  size_x = GrSizeX();
  size_y = GrSizeY();
  max_x = GrMaxX();
  max_y = GrMaxY();
  cx = 0;
  cy = Cvt(0);
  cc = GrWhite();
  return Unit;
}

value gr_close_graph()  /* ML */
{
  GrSetMode(GR_default_text, 0, 0);
  return Unit;
}

value gr_clear_graph()  /* ML */
{
  GrClearScreen(GrBlack());
  GrResetColors();
  return Unit;
}

value gr_size_x()       /* ML */
{
  return Val_long(size_x);
}

value gr_size_y()       /* ML */
{
  return Val_long(size_y);
}

static int rgb_to_color(r, g, b)
     int r, g, b;
{
  int best_c, c, best_dist, dist, dr, dg, db;

  c = GrAllocColor(r, g, b);
  if (c != GrNOCOLOR) return c;
  best_c = 0;
  best_dist = 0x100000;
  for (c = GrNumColors() - 1; c >= 0; c--) {
    GrQueryColor(c, &dr, &dg, &db);
    dr -= r; dg -= g; db -= b;
    dist = dr*dr + dg*dg + db*db;
    if (dist == 0) return c;
    if (dist < best_dist) { best_c = c; best_dist = dist; }
  }
  return best_c;
}

static void color_to_rgb(c, r, g, b)
     int c, * r, * g, * b;
{
  GrQueryColor(c, r, g, b);
}

value gr_set_color(color)       /* ML */
     value color;
{
  int rgb, r, g, b;

  rgb = Long_val(color);
  r = (rgb >> 16) & 0xFF;
  g = (rgb >> 8) & 0xFF;
  b = rgb & 0xFF;
  cc = rgb_to_color(r, g, b);
  return Unit;
}

value gr_plot(x, y)     /* ML */
     value x, y;
{
  GrPlot(Int_val(x), Cvt(Int_val(y)), cc);
  return Unit;
}

value gr_point_color(x, y)      /* ML */
     value x, y;
{
  int c, r, g, b;

  c = GrPixel(Int_val(x), Cvt(Int_val(y)));
  color_to_rgb(c, &r, &g, &b);
  return Val_long((r << 16) + (g << 8) + b);
}

value gr_moveto(x, y)   /* ML */
     value x, y;
{
  cx = Int_val(x);
  cy = Cvt(Int_val(y));
  return Unit;
}

value gr_current_point()        /* ML */
{
  value res;
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(cx);
  Field(res, 1) = Val_int(Cvt(cy));
  return res;
}

value gr_lineto(x, y)   /* ML */
     value x, y;
{
  int xp, yp;
  xp = Int_val(x);
  yp = Cvt(Int_val(y));
  GrLine(cx, cy, xp, yp, cc);
  cx = xp;
  cy = yp;
  return Unit;
}

value gr_draw_arc(argv, argc)   /* ML */
     value * argv;
     int argc;
{
  int a1, a2;
  a1 = 360 - Int_val(argv[5]);
  a2 = 360 - Int_val(argv[4]);
  GrEllipseArc(Int_val(argv[0]), Cvt(Int_val(argv[1])),
	       Int_val(argv[2]), Int_val(argv[3]), a1 * 10, a2 * 10, cc);
  return Unit;
}

static GrTextOption * text_option()

{
  static GrTextOption textopt = {
    NULL,                       /* font */
    1, 1,                       /* X, Y magnification */
    GrNOCOLOR, GrNOCOLOR,       /* foreground, background */
    GR_TEXT_RIGHT,              /* direction */
    GR_ALIGN_LEFT,              /* X alignment */
    GR_ALIGN_BOTTOM,            /* Y alignment */
    GR_BYTE_TEXT
  };
  if (textopt.txo_font == NULL) {
    if ((textopt.txo_font = GrLoadBIOSFont("@:pc8x16.fnt")) == NULL &&
	(textopt.txo_font = GrLoadBIOSFont("@:pc8x14.fnt")) == NULL &&
	(textopt.txo_font = GrLoadBIOSFont("@:pc8x8.fnt")) == NULL)
      graphic_failure("cannot find default font");
  }
  textopt.txo_fgcolor.v = cc;
  return &textopt;
}

value gr_draw_char(c)   /* ML */
     value c;
{
  GrTextOption * topt = text_option();
  GrDrawChar(Int_val(c), cx, cy, topt);
  cx += GrCharWidth(Int_val(c), topt);
  return Unit;
}

value gr_draw_string(s) /* ML */
     value s;
{
  GrTextOption * topt = text_option();
  GrDrawString(String_val(s), string_length(s), cx, cy, topt);
  cx += GrStringWidth(String_val(s), string_length(s), topt);
  return Unit;
}

value gr_text_size(s)   /* ML */
     value s;
{
  GrTextOption * topt = text_option();
  int sx, sy;
  value res;
  
  sx = GrStringWidth(String_val(s), string_length(s), topt);
  sy = GrStringHeight(String_val(s), string_length(s), topt);
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(sx);
  Field(res, 1) = Val_int(sy);
  return res;
}

value gr_fill_rect(vx, vy, vw, vh)      /* ML */
     value vx, vy, vw, vh;
{
  int x, y, w, h;
  x = Int_val(vx);
  y = Int_val(vy);
  w = Int_val(vw);
  h = Int_val(vh);
  y = Cvt(y + h - 1);
  GrFilledBox(x, y, x+w-1, y+h-1, cc);
  return Unit;
}

value gr_fill_arc(argv, argc)   /* ML */
     value * argv;
     int argc;
{
  int a1, a2;
  a1 = - Int_val(argv[5]);
  while (a1 < 0) a1 += 360;
  while (a1 >= 360) a1 -= 360;
  a2 = - Int_val(argv[4]);
  while (a2 <= 0) a2 += 360;
  while (a2 > 360) a2 -= 360;
  GrFilledEllipseArc(Int_val(argv[0]), Cvt(Int_val(argv[1])),
		     Int_val(argv[2]), Int_val(argv[3]), a1 * 10, a2 * 10, cc);
  return Unit;
}

value gr_fill_poly(v)   /* ML */
     value v;
{
  int numpoints = Wosize_val(v);
  int points[numpoints][2];
  int i;

  for (i = 0; i < numpoints; i++) {
    points[i][0] = Int_val(Field(Field(v, i), 0));
    points[i][1] = Cvt(Int_val(Field(Field(v, i), 1)));
  }
  GrFilledPolygon(numpoints, points, cc);
  return Unit;
}

static value new_bitmap(width, height)
     int width, height;
{
  unsigned bsize;
  mlsize_t wsize;

  bsize = GrContextSize(width, height);
  wsize = (bsize + 3) >> 2;
  if (wsize == 0)       /* never allocate an empty object */
    return Atom(Abstract_tag);
  if (wsize <= Max_young_wosize)
    return alloc(wsize, Abstract_tag);
  else
    return alloc_shr(wsize, Abstract_tag);
}

struct image {
  value width;                  /* Width, in pixels */
  value height;                 /* Height, in pixels */
  value data;                   /* Image data */
  value mask;                   /* Image mask (or Val_long(0) if empty) */
};

value gr_draw_image(image, vx, vy)      /* ML */
     struct image * image;
     value vx, vy;
{
  GrContext source;
  int h, w, x, y;
  w = Int_val(image->width);
  h = Int_val(image->height);
  x = Int_val(vx);
  y = Cvt(Int_val(vy) + h - 1);
  if (Is_long(image->mask)) {
    GrCreateContext(w, h, (char *) image->data, &source);
    GrBitBlt(NULL, x, y, &source, 0, 0, w, h, GrWRITE);
  } else {
    GrCreateContext(w, h, (char *) image->mask, &source);
    GrBitBlt(NULL, x, y, &source, 0, 0, w, h, GrAND);
    GrCreateContext(w, h, (char *) image->data, &source);
    GrBitBlt(NULL, x, y, &source, 0, 0, w, h, GrOR);
  }
  return Unit;
}

value gr_create_image(vw, vh)           /* ML */
	value vw, vh;
{
  int w, h;
  struct image * res;
  Push_roots(roots, 1)
#define bitmap roots[0]
  w = Int_val(vw);
  h = Int_val(vh);
  bitmap = new_bitmap(w, h);
  res = (struct image *) alloc_tuple(4);
  res->width = Val_int(w);
  res->height = Val_int(h);
  res->data = bitmap;
  res->mask = Val_int(0);
  Pop_roots();
  return (value) res;
#undef bitmap
}

value gr_blit_image(image, vx, vy)      /* ML */
	struct image * image;
	value vx, vy;
{
  GrContext dest;
  int x, y, w, h;
  
  w = Int_val(image->width);
  h = Int_val(image->height);
  x = Int_val(vx);
  y = Cvt(Int_val(vy) + h - 1);
  GrCreateContext(w, h, (char *) image->data, &dest);
  GrBitBlt(&dest, 0, 0, NULL, x, y, x+w, y+h, GrWRITE);
  return Atom(0);
}

value gr_make_image(color_matrix)       /* ML */
     value color_matrix;
{
  int width, height;
  int i, j;
  struct image * res;
  value row;
  int rgb;
  GrContext context;
  int has_transp;
  Push_roots(roots, 3)
#define v roots[0]
#define bm_data roots[1]
#define bm_mask roots[2]

  v = color_matrix;
  height = Wosize_val(v);
  if (height == 0) {
    width = 0;
  } else {
    width = Wosize_val(Field(v, 0));
    for (i = 1; i < height; i++) {
      if (width != Wosize_val(Field(v, i)))
	graphic_failure("make_image: non-rectangular matrix");
    }
  }
  bm_data = new_bitmap(width, height);
  GrCreateContext(width, height, (char *) bm_data, &context);
  GrSetContext(&context);
  has_transp = 0;
  for (j = 0; j < height; j++) {
    row = Field(v, j);
    for(i = 0; i < width; i++) {
      rgb = Long_val(Field(row, i));
      if (rgb == -1) {
	has_transp = 1;
	GrPlot(i, j, 0);
      } else {
	GrPlot(i, j, 
	       rgb_to_color((rgb>>16) & 0xFF, (rgb>>8) & 0xFF, rgb & 0xFF));
      }
    }
  }
  if (has_transp) {
    bm_mask = new_bitmap(width, height);
    GrCreateContext(width, height, (char *) bm_mask, &context);
    GrSetContext(&context);
    GrClearContext(0);
    for (j = 0; j < height; j++) {
      row = Field(v, j);
      for(i = 0; i < width; i++) {
	if (Long_val(Field(row, i)) == -1) GrPlot(i, j, 255);
      }
    }
  } else {
    bm_mask = Val_int(0);
  }
  GrSetContext(NULL);
  res = (struct image *) alloc_tuple(4);
  res->width = Val_int(width);
  res->height = Val_int(height);
  res->data = bm_data;
  res->mask = bm_mask;
  Pop_roots();
  return (value) res;
#undef v
#undef bm_data
#undef bm_mask
}

/* For Moscow ML we make it return an array of arrays instead: */

static value alloc_int_vect(size)
     mlsize_t size;
{
  value res;
  mlsize_t i;
  
  if (size <= Max_young_wosize) {
    res = alloc(size, Reference_tag);
  } else {
    res = alloc_shr(size, Reference_tag);
  }
  for (i = 0; i < size; i++) {
    Field(res, i) = Val_long(0);
  }
  return res;
}

value gr_dump_image(image)      /* ML */
     struct image * image;
{
  int height, width, i, j;
  unsigned char * p;
  int r, g, b;
  GrContext context;
  int c;
  Push_roots(roots, 2);
#define img ((struct image *) roots[0])
#define matrix (roots[1])

  img = image;
  height = Int_val(img->height);
  width  = Int_val(img->width);
  matrix = alloc_int_vect(height);
  for (i = 0; i < height; i++) {
    value row = alloc_int_vect(width);
    modify(&Field(matrix, i), row);
  }
  GrCreateContext(width, height, (char *) img->data, &context);
  GrSetContext(&context);
  for (i = 0; i < height; i++) {
    for (j = 0; j < width; j++) {
      c = GrPixel(j, i);
      color_to_rgb(c, &r, &g, &b);
      Field(Field(matrix, i), j) = Val_long((r << 16) + (g << 8) + b);
    }
  }
  if (img->mask != Val_long(0)) {
    GrCreateContext(width, height, (char *) img->mask, &context);
    GrSetContext(&context);
    for (i = 0; i < height; i++) {
      for (j = 0; j < width; j++) {
	c = GrPixel(j, i);
	if (c == 255) Field(Field(matrix, i), j) = Val_long(-1);
      }
    }
  }
  Pop_roots();
  return matrix;
#undef img
#undef matrix
}

static int event_codes[] = {
  M_BUTTON_DOWN, M_BUTTON_UP, M_KEYPRESS, M_MOTION, M_POLL
};

/* Moscow ML expects gr_wait_events to return fields in this order: 
	0 - button
	1 - key
	2 - keypressed
	3 - mouse_x
	4 - mouse_y
  Caml Light expects 3, 4, 0, 2, 1
*/

value gr_wait_event(events)     /* ML */
     value events;
{
  int event_mask;
  value res;
  MouseEvent e, e2;

  enter_blocking_section();
  for (event_mask = 0; Tag_val(events) == 1; events = Field(events, 1))
    event_mask |= event_codes[Tag_val(Field(events, 0))];
  if (event_mask & M_POLL) {
    MouseGetEvent(M_MOTION|M_BUTTON_DOWN|M_BUTTON_UP|M_POLL|M_NOPAINT, &e);
    e.key = kbhit() ? 0 : -1;
  } else {
    e.key = -1;
    MouseGetEvent(event_mask, &e);
    MouseGetEvent(M_MOTION|M_BUTTON_DOWN|M_BUTTON_UP|M_POLL|M_NOPAINT, &e2);
    if ((e.flags & (M_MOTION | M_BUTTON_DOWN | M_BUTTON_UP)) == 0) {
      e.x = e2.x; e.y = e2.y; e.buttons = e2.buttons;
    }
  }
  leave_blocking_section();
  res = alloc_tuple(5);
  Field(res, 3) = Val_int(e.x);
  Field(res, 4) = Val_int(Cvt(e.y));
  Field(res, 0) = Atom(e.buttons != 0);
  if (e.key == -1) {
    Field(res, 2) = Atom(0);
    Field(res, 1) = Val_int(0);
  } else {
    Field(res, 2) = Atom(1);
    Field(res, 1) = Val_int(e.key);
  }
  return res;
}

static unsigned get_time()
{
  union REGS r;
  
  r.x.ax = 0;
  int86(0x1A, &r, &r);
  return (r.x.cx << 16) + r.x.dx;
}

value gr_sound(freq, duration)  /* ML */
	value freq, duration;
{
  int start, d, t;
	
  enter_blocking_section();
  sound(Long_val(freq));
  start = get_time();
  d = Long_val(duration) / 55;
  do {
    t = get_time() - start;
    if (t < 0) t += 1572997;
  } while (t < d);
  sound(0);
  leave_blocking_section();
  return Atom(0);
}

/* New function for Moscow ML */

value gr_image_size(image)      /* ML */
     struct image * image;
{ value res;
  res = alloc_tuple(2);
  Field(res, 0) = image -> width;
  Field(res, 1) = image -> height;
  return res;
}
