(* mosml/src/dynlibs/mgd/Gdimage.sml.  Version 0.1 of 1998-05-08 *)

open Dynlib;

(* Obtain a handle pointing to the library defining the C functions: *)

val dlh = dlopen { lib = "libmgd.so",
		   flag = RTLD_LAZY, 
		   global = false }

prim_type image (* really, a gdImagePointer, pointing outside the ML heap *)

type color = int

datatype style = 
    ColorS of color
  | TransparentS

datatype mode = 
    Color of color
  | Transparent
  | Brushed of image
  | Styled of style vector
  | StyledBrushed of bool vector * image
  | Tiled of image

type rgb = int * int * int
type xy = int * int

val image : xy -> rgb -> image = 
    app2 (dlsym dlh "mgd_image")

val fromPng : string -> image = 
    app1 (dlsym dlh "mgd_frompng")

val toPng : image -> string -> unit = 
    app2 (dlsym dlh "mgd_topng")

val stdoutPng : image -> unit = 
    app1 (dlsym dlh "mgd_tostdoutpng")

val color : image -> rgb -> color =
    app2 (dlsym dlh "mgd_color")

fun htmlcolors im = 
    let fun c rgb = color im rgb
    in
	{ black   = c (0, 0, 0), 
	  white   = c (255, 255, 255),
	  silver  = c (192, 192, 192),
	  gray    = c (128, 128, 128),
	  maroon  = c (128, 0, 0),
	  red     = c (255, 0, 0),
	  purple  = c (128, 0, 128),
	  fuchsia = c (255, 0, 255),
	  green   = c (0, 128, 0),
	  lime    = c (0, 255, 0),
	  olive   = c (128, 128, 0),
	  yellow  = c (255, 255, 0),
	  navy    = c (0, 0, 128),
	  blue    = c (0, 0, 255),
	  teal    = c (0, 128, 128),
	  aqua    = c (0, 255, 255) }
    end

val setTransparent : image -> color -> unit =
    app2 (dlsym dlh "mgd_settransparentcolor")

val getTransparent : image -> color option =
    app1 (dlsym dlh "mgd_gettransparentcolor")

val noTransparent : image -> unit =
    app1 (dlsym dlh "mgd_unsettransparentcolor")

val gdTransparent   : color = app1 (dlsym dlh "mgd_gettransparentstyle") ();
val gdBrushed       : color = app1 (dlsym dlh "mgd_getbrushed") ();
val gdStyled        : color = app1 (dlsym dlh "mgd_getstyled") ();
val gdStyledBrushed : color = app1 (dlsym dlh "mgd_getstyledbrushed") ();
val gdTiled         : color = app1 (dlsym dlh "mgd_gettiled") ();

val setstyles : image -> int vector -> unit =
    app2 (dlsym dlh "mgd_setstyle");

fun convertstyle (ColorS c)   = c
  | convertstyle TransparentS = gdTransparent 

fun convertvisible false = 0
  | convertvisible true  = 1

fun setmode (im : image) (mo : mode) : color = 
    case mo of 
	Color c       => c
      | Transparent   => gdTransparent
      | Brushed brush => (app2 (dlsym dlh "mgd_setbrush") im brush;
			  gdBrushed)
      | Styled styles => (setstyles im (Vector.map convertstyle styles);
			  gdStyled)
      | StyledBrushed(visible, brush) =>
	                 (setstyles im (Vector.map convertvisible visible);
			  app2 (dlsym dlh "mgd_setbrush") im brush;
			  gdStyledBrushed)
      | Tiled tile    => (app2 (dlsym dlh "mgd_settile") im tile;
			  gdTiled)

fun drawPixel (im : image) (mo : mode) (xy : xy) : unit =
    app3 (dlsym dlh "mgd_pixel") im xy (setmode im mo)

fun drawLine (im : image) (mo : mode) (xy1 : xy, xy2 : xy) : unit =
    app4 (dlsym dlh "mgd_drawline") im xy1 xy2 (setmode im mo)

fun drawRect (im : image) (mo : mode) (xy1 : xy, xy2 : xy) : unit =
    app4 (dlsym dlh "mgd_drawrect") im xy1 xy2 (setmode im mo)

fun fillRect (im : image) (mo : mode) (xy1 : xy, xy2 : xy) : unit =
    app4 (dlsym dlh "mgd_fillrect") im xy1 xy2 (setmode im mo)

fun drawPolygon (im : image) (mo : mode) (xys : xy vector) : unit =
    app3 (dlsym dlh "mgd_drawpoly") im xys (setmode im mo)

fun fillPolygon (im : image) (mo : mode) (xys : xy vector) : unit =
    app3 (dlsym dlh "mgd_fillpoly") im xys (setmode im mo)

fun drawArc (im:image) (mo : mode) { c : xy, wh : xy, from : int, to : int } =
    app3 (dlsym dlh "mgd_drawarc") im (c, wh, from, to) (setmode im mo)

fun fill (im : image) (mo : mode) (xy : xy) : unit =        
    app3 (dlsym dlh "mgd_fill") im xy (setmode im mo)

fun fillBorder (im : image) (mo : mode) (xy : xy) (co : color) =        
    app4 (dlsym dlh "mgd_fillborder") im xy co (setmode im mo)
 
fun copy { src : image, srcxy : xy, srcwh : xy,
	   dst : image, dstxy : xy } : unit =
    app1 (dlsym dlh "mgd_copy") (src, srcxy, srcwh, dst, dstxy)


fun copyResize { src : image, srcxy : xy, srcwh : xy,
		 dst : image, dstxy : xy, dstwh : xy } : unit =
    app1 (dlsym dlh "mgd_copyresize") (src, srcxy, srcwh, dst, dstxy, dstwh)

datatype font = 
    Tiny 
  | Small
  | MediumBold
  | Large 
  | Giant

fun fontcode Tiny       = 0
  | fontcode Small      = 1
  | fontcode MediumBold = 2
  | fontcode Large      = 3
  | fontcode Giant      = 4

fun char (im : image) (co : color) (font : font) (xy : xy) (c : char) : unit =
    app5 (dlsym dlh "mgd_char") im (fontcode font) xy c co 

fun charUp (im:image) (co : color) (font : font) (xy : xy) (c : char) : unit =
    app5 (dlsym dlh "mgd_charup") im (fontcode font) xy c co

fun string (im:image) (co:color) (font : font) (xy : xy) (s : string) : unit =
    app5 (dlsym dlh "mgd_string") im (fontcode font) xy s co

fun stringUp (im:image) (co:color) (font:font) (xy : xy) (s : string) : unit =
    app5 (dlsym dlh "mgd_stringup") im (fontcode font) xy s co

fun rgb  (im : image) (co : color) : rgb =
    app2 (dlsym dlh "mgd_rgb") im co

fun size  (im : image) : xy =
    app1 (dlsym dlh "mgd_size") im

fun charsize (f : font) : xy = 
    app1 (dlsym dlh "mgd_charsize") (fontcode f)
