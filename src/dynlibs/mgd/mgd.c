/* File mosml/src/dynlibs/mgd/mgd.c -- interface to Thomas Boutell's 
   gd graphics package.  
   sestoft@dina.kvl.dk 1998-05-07 for gd 1.3, 2000-02-04 for gd 1.7.3
 */

#include <gd.h>			/* For gd                        */
#include <gdfontt.h>		/* For gd font description files */
#include <gdfonts.h>
#include <gdfontmb.h>
#include <gdfontl.h>
#include <gdfontg.h>

#include <stdlib.h>		/* For malloc */
#include <stdio.h>		/* For stdout */

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* Moscow ML specific includes: */

#include <alloc.h>		/* For alloc_tuple, ...      */
#include <mlvalues.h>		/* For Val_unit, Long_val, String_val, ... */

/* Representation of images.  

   An image should be a finalized object: a pair, 

              header with Final_tag
	      0: finalization function image_destroy
	      1: a gdImagePtr pointer to C data outside the ML heap

   The finalization function should apply gdImageDestroy to the second
   component of the pair: 
*/

#define Image_val(x) (gdImagePtr)(Field(x, 1))
 
void image_destroy(value obj)
{ 
  gdImagePtr im = Image_val(obj);
  gdImageDestroy(im);
}

/* When the image becomes unreachable from the ML process, it will be
   garbage-collected, and image_destroy will be called on the
   gdImagePtr to destroy the image, thus freeing the C memory occupied
   by it.  The camlrunm gc then frees the finalized pair representing
   the image. 
*/

value finalize_image(gdImagePtr imgptr)
{ 
  int w = gdImageSX(imgptr);
  int h = gdImageSY(imgptr);
  /* Wrap a finalizer around the image data structure.  

     Also, adjust the speed of the garbage collector so that at most
     10 MB unreachable images are lying around.  In practice this
     works as intended only if the ML program allocates other data
     (lists, arrays, ...) in the ML heap: */
  value res = alloc_final(2, &image_destroy, w * h, 10000000);
  Field(res, 1) = (long)imgptr;
  
  return res;
}

/* SML type: xy -> rgb -> image */

EXTERNML value mgd_image(value xy, value background)
{
  int sx = Long_val(Field(xy, 0));
  int sy = Long_val(Field(xy, 1));
  int r  = Long_val(Field(background, 0));
  int g  = Long_val(Field(background, 1));
  int b  = Long_val(Field(background, 2));
  int bgcolor;
  gdImagePtr imgptr = gdImageCreate(sx, sy);
  if (imgptr == NULL)
    failwith("Could not create image");
  bgcolor = gdImageColorAllocate(imgptr, r, g, b);
  return finalize_image(imgptr);
}

/* SML type: string -> image */

EXTERNML value mgd_frompng(value filename)
{
  char *filenam = String_val(filename);
  gdImagePtr imgptr;
  FILE *in;
  in = fopen(filenam, "rb");
  if (in == NULL)
    failwith("Cannot open PNG file for input");
  imgptr = gdImageCreateFromPng(in);
  fclose(in);
  if (imgptr == NULL)
    failwith("Cannot read PNG image from file");
  return finalize_image(imgptr);
}

/* SML type: image -> string -> unit */

EXTERNML value mgd_topng(value im, value filename)
{
  char *filenam = String_val(filename);
  FILE *out;
  out = fopen(filenam, "wb");
  if (out == NULL)
    failwith("Cannot open PNG file for output");
  gdImagePng(Image_val(im), out);
  fclose(out);
  return Val_unit;
}

/* SML type: image -> unit */

EXTERNML value mgd_tostdoutpng(value im)
{
  fprintf(stdout, "Content-type: image/png\n\n");   
  gdImagePng(Image_val(im), stdout);
#ifdef WIN32
  fflush(stdout);
#else
  flush(stdout);
#endif
  return Val_unit;
}

/* SML type: image -> rgb -> color */

EXTERNML value mgd_color(value im, value rgb)
{
  int r  = Long_val(Field(rgb, 0));
  int g  = Long_val(Field(rgb, 1));
  int b  = Long_val(Field(rgb, 2));
  gdImagePtr imgptr = Image_val(im);
  int color = gdImageColorExact(imgptr, r, g, b);
  if (color != -1)
    return Val_long(color);
  color = gdImageColorAllocate(imgptr, r, g, b);
  if (color != -1)
    return Val_long(color);
  color = gdImageColorClosest(imgptr, r, g, b);
  return Val_long(color);
}

/* SML type: image -> color -> unit */

EXTERNML value mgd_settransparentcolor(value im, value color)
{
  gdImageColorTransparent(Image_val(im), Long_val(color));
  return Val_unit;
}

/* SML type: image -> color option */

EXTERNML value mgd_gettransparentcolor(value im)
{
  int color = gdImageGetTransparent(Image_val(im));
  value res;
  if (color == -1)
    return NONE;
  res = alloc(1, SOMEtag);
  modify(&Field(res, 0), Val_long(color));
  return res;
}


/* SML type: image -> unit */

EXTERNML value mgd_unsettransparentcolor(value im, value color)
{
  gdImageColorTransparent(Image_val(im), -1);
  return Val_unit;
}


/* SML type: unit -> color */

EXTERNML value mgd_gettransparentstyle(value v)
{ return Val_long(gdTransparent); }

/* SML type: unit -> color */

EXTERNML value mgd_getbrushed(value v)
{ return Val_long(gdBrushed); }

/* SML type: unit -> color */

EXTERNML value mgd_getstyled(value v)
{ return Val_long(gdStyled); }

/* SML type: unit -> color */

EXTERNML value mgd_getstyledbrushed(value v)
{ return Val_long(gdStyledBrushed); }

/* SML type: unit -> color */

EXTERNML value mgd_gettiled(value v)
{ return Val_long(gdTiled); }

/* SML type: image -> int vector -> unit */

EXTERNML value mgd_setstyle(value im, value stylevec)
{
  int len = Wosize_val(stylevec);
  int *styles = (int*)malloc(sizeof(int) * len); 
  int i;
  for (i=0; i<len; i++)
    styles[i] = Long_val(Field(stylevec, i));
  gdImageSetStyle(Image_val(im), styles, len);
  free(styles);
  return Val_unit;
}

/* SML type: image -> image -> unit */

EXTERNML value mgd_setbrush(value im, value brush)
{
  gdImageSetBrush(Image_val(im), Image_val(brush));
  return Val_unit;
}

/* SML type: image -> image -> unit */

EXTERNML value mgd_settile(value im, value tile)
{
  gdImageSetTile(Image_val(im), Image_val(tile));
  return Val_unit;
}

/* SML type: image -> xy -> color -> unit */

EXTERNML value mgd_pixel(value im, value xy, value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  gdImageSetPixel(Image_val(im), x, y, Long_val(color));
  return Val_unit;
}

/* SML type: image -> xy -> xy -> color -> unit */

EXTERNML value mgd_drawline(value im, value xy1, value xy2, value color)
{
  int x1 = Long_val(Field(xy1, 0));
  int y1 = Long_val(Field(xy1, 1));
  int x2 = Long_val(Field(xy2, 0));
  int y2 = Long_val(Field(xy2, 1));
  gdImageLine(Image_val(im), x1, y1, x2, y2, Long_val(color));
  return Val_unit;
}

/* SML type: image -> xy -> xy -> color -> unit */

EXTERNML value mgd_drawrect(value im, value xy1, value xy2, value color)
{
  int x1 = Long_val(Field(xy1, 0));
  int y1 = Long_val(Field(xy1, 1));
  int x2 = Long_val(Field(xy2, 0));
  int y2 = Long_val(Field(xy2, 1));
  gdImageRectangle(Image_val(im), x1, y1, x2, y2, Long_val(color));
  return Val_unit;
}

/* SML type: image -> xy -> xy -> color -> unit */

EXTERNML value mgd_fillrect(value im, value xy1, value xy2, value color)
{
  int x1 = Long_val(Field(xy1, 0));
  int y1 = Long_val(Field(xy1, 1));
  int x2 = Long_val(Field(xy2, 0));
  int y2 = Long_val(Field(xy2, 1));
  gdImageFilledRectangle(Image_val(im), x1, y1, x2, y2, Long_val(color));
  return Val_unit;
}

/* SML type: image -> xy vector -> color -> unit */

EXTERNML value mgd_drawpoly(value im, value xys, value color)
{
  int len = Wosize_val(xys);
  gdPointPtr points = malloc(sizeof(gdPoint) * len);
  int i;
  for (i=0; i<len; i++)
    {
      value xy = Field(xys, i);
      points[i].x = Long_val(Field(xy, 0));
      points[i].y = Long_val(Field(xy, 1));
    }
  gdImagePolygon(Image_val(im), points, len, Long_val(color));
  free(points);
  return Val_unit;
}

/* SML type: image -> xy vector -> color -> unit */

EXTERNML value mgd_fillpoly(value im, value xys, value color)
{
  int len = Wosize_val(xys);
  gdPointPtr points = malloc(sizeof(gdPoint) * len);
  int i;
  for (i=0; i<len; i++)
    {
      value xy = Field(xys, i);
      points[i].x = Long_val(Field(xy, 0));
      points[i].y = Long_val(Field(xy, 1));
    }
  gdImageFilledPolygon(Image_val(im), points, len, Long_val(color));
  free(points);
  return Val_unit;
}

/* SML type: image -> xy * xy * int * int -> color -> unit */

EXTERNML value mgd_drawarc(value im, value arc, value color)
{
  value center = Field(arc, 0);
  int cx = Long_val(Field(center, 0));
  int cy = Long_val(Field(center, 1));
  value wh = Field(arc, 1);
  int w  = Long_val(Field(wh, 0));
  int h  = Long_val(Field(wh, 1));
  int s  = Long_val(Field(arc, 2));
  int e  = Long_val(Field(arc, 3));
  gdImageArc(Image_val(im), cx, cy, w, h, s, e, Long_val(color));
  return Val_unit;
}

/* SML type: image -> xy -> color -> unit */

EXTERNML value mgd_fill(value im, value xy, value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  gdImageFill(Image_val(im), x, y, Long_val(color));
  return Val_unit;
}

/* SML type: image -> xy -> color -> color -> unit */

EXTERNML value mgd_fillborder(value im, value xy, value border, value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  gdImageFillToBorder(Image_val(im), x, y, Long_val(border), Long_val(color));
  return Val_unit;
}

/* SML type: image * xy * xy * image * xy -> unit */

EXTERNML value mgd_copy(value args)
{
  gdImagePtr src = Image_val(Field(args, 0));
  value srcxy    = Field(args, 1);
  value srcwh    = Field(args, 2);
  gdImagePtr dst = Image_val(Field(args, 3));
  value dstxy    = Field(args, 4);
  int srcx = Long_val(Field(srcxy, 0));
  int srcy = Long_val(Field(srcxy, 1));
  int srcw = Long_val(Field(srcwh, 0));
  int srch = Long_val(Field(srcwh, 1));
  int dstx = Long_val(Field(dstxy, 0));
  int dsty = Long_val(Field(dstxy, 1));
  gdImageCopy(dst, src, dstx, dsty, srcx, srcy, srcw, srch);
  return Val_unit;
}

/* SML type: image * xy * xy * image * xy * xy -> unit */

EXTERNML value mgd_copyresize(value args)
{
  gdImagePtr src = Image_val(Field(args, 0));
  value srcxy    = Field(args, 1);
  value srcwh    = Field(args, 2);
  gdImagePtr dst = Image_val(Field(args, 3));
  value dstxy    = Field(args, 4);
  value dstwh    = Field(args, 5);
  int srcx = Long_val(Field(srcxy, 0));
  int srcy = Long_val(Field(srcxy, 1));
  int srcw = Long_val(Field(srcwh, 0));
  int srch = Long_val(Field(srcwh, 1));
  int dstx = Long_val(Field(dstxy, 0));
  int dsty = Long_val(Field(dstxy, 1));
  int dstw = Long_val(Field(dstwh, 0));
  int dsth = Long_val(Field(dstwh, 1));
  gdImageCopyResized(dst, src, dstx, dsty, srcx, srcy, dstw, dsth, srcw, srch);
  return Val_unit;
}

gdFontPtr getfont(value fontcode)
{
  int fontnumber = Long_val(fontcode);
  switch (fontnumber) {
  case 0: return gdFontTiny;
  case 1: return gdFontSmall;
  case 2: return gdFontMediumBold;
  case 3: return gdFontLarge;
  case 4: return gdFontGiant;
  default: return gdFontMediumBold;
  }
}

/* SML type: image -> fontcode -> xy -> char -> color -> unit */

EXTERNML value mgd_char(value im, value fontcode, value xy, value ch, value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  char c = (char)Long_val(ch);
  gdImageChar(Image_val(im), getfont(fontcode), x, y, c, Long_val(color));
  return Val_unit;
}

/* SML type: image -> fontcode -> xy -> char -> color -> unit */

EXTERNML value mgd_charup(value im, value fontcode, value xy, value ch, 
		 value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  char c = (char)Long_val(ch);
  gdImageCharUp(Image_val(im), getfont(fontcode), x, y, c, Long_val(color));
  return Val_unit;
}

/* SML type: image -> fontcode -> xy -> string -> color -> unit */

EXTERNML value mgd_string(value im, value fontcode, value xy, value str, 
		 value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  unsigned char *s = String_val(str);
  gdImageString(Image_val(im), getfont(fontcode), x, y, s, Long_val(color));
  return Val_unit;
}

/* SML type: image -> fontcode -> xy -> string -> color -> unit */

EXTERNML value mgd_stringup(value im, value fontcode, value xy, value str, 
		   value color)
{
  int x = Long_val(Field(xy, 0));
  int y = Long_val(Field(xy, 1));
  unsigned char *s = String_val(str);
  gdImageStringUp(Image_val(im), getfont(fontcode), x, y, s, Long_val(color));
  return Val_unit;
}

/* SML type: image -> color -> rgb */

EXTERNML value mgd_rgb(value im, value color)
{
  int co = Long_val(color);
  gdImagePtr imgptr = Image_val(im);
  int r = gdImageRed(imgptr, co);
  int g = gdImageGreen(imgptr, co);
  int b = gdImageBlue(imgptr, co);
  value res = alloc_tuple(3);
  Field(res, 0) = Val_long(r);
  Field(res, 1) = Val_long(g);
  Field(res, 2) = Val_long(b);
  return res;
}

/* SML type: image -> xy */

EXTERNML value mgd_size(value im)
{
  gdImagePtr imgptr = Image_val(im);
  int w = gdImageSX(imgptr);
  int h = gdImageSY(imgptr);
  value res = alloc_tuple(2);
  Field(res, 0) = Val_long(w);
  Field(res, 1) = Val_long(h);
  return res;
}

/* SML type: font -> xy */

EXTERNML value mgd_charsize(value fontcode)
{
  gdFontPtr font = getfont(fontcode);
  int w = font->w;
  int h = font->h;
  value res = alloc_tuple(2);
  Field(res, 0) = Val_long(w);
  Field(res, 1) = Val_long(h);
  return res;
}

