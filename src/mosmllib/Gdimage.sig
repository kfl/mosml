(* Gdimage -- creating PNG images -- requires Dynlib *)

type image

type color

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

datatype font = 
    Tiny 
  | Small
  | MediumBold
  | Large 
  | Giant

type rgb = int * int * int           (* RGB color components, 0..255   *)
type xy  = int * int                 (* points (x, y) and sizes (w, h) *)

val image     : xy -> rgb -> image
val fromPng   : string -> image
val toPng     : image -> string -> unit
val stdoutPng : image -> unit 
val size      : image -> xy

val color          : image -> rgb -> color
val rgb            : image -> color -> rgb
val htmlcolors     : image -> { aqua : color, black : color, blue : color,
                                fuchsia : color, gray : color, 
                                green : color, lime : color, maroon : color,
                                navy : color, olive : color, purple : color,
                                red : color, silver : color, teal : color,
                                white : color, yellow : color }
val getTransparent : image -> color option 
val setTransparent : image -> color -> unit
val noTransparent  : image -> unit 

val drawPixel   : image -> mode -> xy -> unit
val drawLine    : image -> mode -> xy * xy -> unit
val drawRect    : image -> mode -> xy * xy -> unit
val fillRect    : image -> mode -> xy * xy -> unit
val drawPolygon : image -> mode -> xy vector -> unit
val fillPolygon : image -> mode -> xy vector -> unit
val drawArc     : image -> mode -> { c : xy, wh : xy, from : int, to : int }
                  -> unit
val fill        : image -> mode -> xy -> unit
val fillBorder  : image -> mode -> xy -> color -> unit
 
val copy        : { src : image, srcxy : xy, srcwh : xy,
                    dst : image, dstxy : xy } -> unit
val copyResize  : { src : image, srcxy : xy, srcwh : xy,
                    dst : image, dstxy : xy, dstwh : xy } -> unit

val char        : image -> color -> font -> xy -> char -> unit
val charUp      : image -> color -> font -> xy -> char -> unit
val string      : image -> color -> font -> xy -> string -> unit
val stringUp    : image -> color -> font -> xy -> string -> unit
val charsize    : font -> xy

(* 
   This is an interface to version 1.7.3 of Thomas Boutell's gd image
   package for creating PNG images.

   [image] is the type of images being drawn.  They can be created
   from scratch, imported from PNG files, and exported to PNG files.

   All functions correctly clip to the actual size of the image.
   
   [color] is the type of colors.  Currently there can be at most 256
   different colors in an image.

   [style] is the type of drawing styles.  A style is either a color,
   or transparent.

   [mode] is the type of drawing modes for line drawing and filling.  
   It may be one of
        Color c         where c is a color
        Transparent
        Brushed img     for line drawing using the given image as brush
        Styled stys     for line drawing, cyclically using the styles 
                        in the given vector to create a dashed line
        StyledBrushed (vis, img) 
                        for line drawing, using the given image as a brush,
                        cyclically switching it on and off according to the 
                        given bool vector
        Tiled img       for filling, using the given image as a tile

   [font] is the type of fonts: Tiny, Small, MediumBold, Large, Giant

   [rgb] is the type of (r, g, b) triples, where the components
   indicate color intensity as an integer value in the range 0..255.

   [xy] is the type of pairs, used for (x, y) coordinates and to
   indicate dimensions (width, height).  The origin (0, 0) is the
   upper left-hand corner of the image.  The x coordinates increase to
   the right; the y coordinates increase downwards.

   [image (w, h) rgb] creates a new empty image with size (w, h) and
   the background color rgb.  Raises Fail if the image cannot be
   created.

   [fromPng filename] reads an image from the given PNG file.  Raises
   Fail if the file does not exist or does not contain a PNG image.

   [size img] returns (w, h) where w is the width and h the height of
   img.

   [toPng img filename] write the image to the given file in PNG
   format.

   [stdoutPng img] writes the image to standard output in PNG format,
   preceded by the HTTP header "Content-type: image/png\n\n".  Useful
   in CGI scripts.

   [color img rgb] returns the color code corresponding to rgb in the
   color table of img.  Reuses the color code if it has already been
   allocated; otherwise allocates the color if possible; otherwise
   returns an approximation to the color rgb.

   [htmlcolors im] returns a record containing the 16 standard HTML
   colors: aqua, black, blue, fuchsia, gray, green, lime, maroon,
   navy, olive, purple, red, silver, teal, white, yellow.  This call
   will allocate all these colors in the color table of the image,
   even if you do not use all of them.

   [rgb img color] returns (r, g, b) where r, g, b are the component
   intensities of the given color in the color table of img.

   [getTransparent img] returns SOME c where c is the `transparent'
   color of the image, if any; otherwise returns NONE.

   [setTransparent img col] makes the given color transparent in the
   image.

   [noTransparent img] makes all colors non-transparent in the image.
   This is useful for images that are to be used as tiles for filling.
   Such images are not allowed to have a transparent color.

   [drawPixel img mode xy] draws the pixel in img at xy using the
   given mode.

   [drawLine img mode (xy1, xy2)] draws a line in img from xy1 to xy2
   using the given mode.

   [drawRect img mode (xy1, xy2)] draws a rectangle in img with
   opposing corners xy1 and xy2 using the given mode.

   [fillRect img mode (xy1, xy2)] draws a filled rectangle in img with
   opposing corners xy1 and xy2 using the given mode.

   [drawPolygon img mode xys] draws a polygon in img with corners as
   given by the vector xys of coordinates using the given mode.

   [fillPolygon img mode xys] draws a filled polygon in img with
   corners as given by the vector xys of coordinates using the given
   mode.

   [drawArc img mode { c, wh, from, to }] draw part of an ellipsis arc
   in img, with center c, width and height wh, using the given `from'
   and `to' angles, given in degrees (0..360).

   [fill img mode xy] fills the region in img around xy which has the
   same color as the point at img, using the given mode.

   [fillBorder img mode xy col] fills the region in img around xy
   which is delimited by the color col, using the given mode.
 
   [copy { src, srcxy, srcwh, dst, dstxy }] copies part of the image
   src into the image dst, without rescaling.  More precisely, copies
   the subimage of src whose upper left-hand corner is srcxy and whose
   size is srcwh, into the subimage of dst whose upper left-hand
   corner is dstxy.  The images src and dst may be the same, but if
   the subimages overlap, then the result is unpredictable.  

   [copyResize { src, srcxy, srcwh, dst, dstxy, dstwh }] copies part
   of the image src into the image dst, rescaling to the given size
   dstwh of the destination subimage.  Otherwise works as copy.

   [char img col font xy ch] draws the character ch left-right (to be
   read from south) in img at xy using the given color.

   [charUp img col font xy ch] draws the character ch bottom-up (to be
   read from east) in img at xy using the given color.

   [string img col font xy s] draws the string s left-right (to be
   read from south) in img at xy using the given color.

   [stringUp img col font xy s] draws the string s bottom-up (to be
   read from east) in img at xy using the given color.

   [charsize font] returns (w, h) where w is the width and h the
   height, in pixels, of each character in the given font.
*)
