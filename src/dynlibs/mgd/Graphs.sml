(* Graphs.sml --- drawing bar charts, pie charts, cumulative graphs, ... 
   using Gdimage. For examples of use, see file testgdimage.sml.
   sestoft@dina.kvl.dk 1998-05-13
 *)

open Gdimage;

(* Creating a histogram of a given size, from a list of values *)

(* So far no axes or tick marks *)

fun histogram (w, h) []  = raise Fail "histogram: no data"
  | histogram (w, h) obs =
    let val max = foldl Int.max 0 obs
	val yscale = real h / real (max + 2)
	val im = image (w, h) (255, 255, 255)
	val {blue, ...} = htmlcolors im
	val n = length obs
	val colwidth = w div n
	val colspace = 1 + colwidth div 8
	fun loop i []        = ()
	  | loop i (x :: xr) =
	    let val x1 = i * colwidth
		val y1 = h - round (yscale * real x)
		val x2 = x1+colwidth-colspace
	    in
		fillRect im (Color blue) ((x1, y1), (x2, h));
		loop (i+1) xr
	    end
    in loop 0 obs; im end

(* Allocate colors and return a coloration function *)

fun makecolors im =
    let val {black, white, silver, gray, maroon, red, purple, fuchsia,
	     green, lime, olive, yellow, navy, blue, teal, aqua} =
	     htmlcolors im
	val colors = Array.fromList [red, navy, olive, aqua, black, purple, 
				     lime, blue, maroon, silver, green, 
				     yellow, teal]
	val firstcolor = 
	    Random.range (0, Array.length colors) (Random.newgen())
	fun color i = 
	    Color (Array.sub(colors, (i + firstcolor) mod Array.length colors))
    in color end

fun addlegend im itemnames black color =
    let val (w, h) = size im
	val longestitem = foldl Int.max 0 (map String.size itemnames)
	val font = Small
	val (fontw, fonth) = charsize font
	val bh = fonth
	val bw = 2 * bh
	val sepw = bh
	val marginsep = 10
	val legendw = bw + sepw + fontw * longestitem + marginsep
	val itemcount = length itemnames
	val itemh = (h - 2 * marginsep) div itemcount
	val seph = Int.min(bh, itemh - bh)
	val x1 = w - legendw
	val y0 = (h - itemcount * bh - (itemcount - 1) * seph) div 2
	fun legend _ [] = ()
	  | legend i (item1 :: itemr) =
	    let val y1 = i * (bh + seph) + y0
	    in
		fillRect im (color i) ((x1, y1), (x1+bw, y1+bh));
		string im black font (x1+bw+sepw, y1) item1;
		legend (i+1) itemr
	    end
    in legend 0 itemnames; legendw end

fun piechart _ [] _ = raise Fail "piechart: no data"
  | piechart (wh as (w, h)) (obs as obs1 :: obsr) itemnames =
    let (* The image and its color table *)
	val im = image (w, h) (255, 255, 255)
	val black = color im (0, 0, 0)
	val color = makecolors im

	val legendw = addlegend im itemnames black color

	(* The pie-chart itself, to the left *)
	val sum = if List.all (fn x => x > 0.0) obs then foldl op+ 0.0 obs
		  else raise Fail "piechart: all slices must be positive"
	val radscale = 2.0 * Math.pi / sum 
	val piew = w - legendw
	val pieh = h
	val pied = Int.min(piew, pieh) - 20
	val center = (piew div 2, h div 2)
	val angleoffset = (Math.pi + obs1 * radscale) / 2.0
	fun polar r a = (#1 center + round (r * Math.cos (a - angleoffset)), 
			 #2 center + round (r * Math.sin (a - angleoffset)))
	fun drawradius angle = 
	    drawLine im (Color black) (center, polar (real pied / 2.0) angle)
	fun fillslice i angle =
	    fill im (color i) (polar (real pied / 4.0) angle)
	fun loop i lastsum []        = ()
	  | loop i lastsum (x :: xr) = 
	    (drawradius ((lastsum + x) * radscale);
	     fillslice i ((lastsum + x / 2.0) * radscale); 
	     loop (i+1) (lastsum + x) xr)

    in 
	drawArc im (Color black) { c = center, wh = (pied, pied), 
				   from = 0, to = 360 };
	(case obsr of 
	     [] => fillslice 0 0.0
	   | _  => (drawradius (obs1 * radscale);
		    loop 1 obs1 obsr;
		    fillslice 0 (obs1 / 2.0 * radscale)));
	im
    end

(* Graphing several series of data, stacking the series on top of each
   other.  Time is implicit.  Linear interpolation.  Automatically
   scale X and Y axis.

   The input is a list of data series (rows).  Each data series (row)
   is a list of reals, and will be plotted with linear interpolation
   between the data points.
 *)

fun accugraph (w, h) [] _ = raise Fail "accugraph: no data series"
  | accugraph (w, h) (obss as obs1 :: obsr) itemnames = 
    let val noobs = length obs1
	val _ = if noobs >= 2 then ()
		else raise Fail "accugraph: series must have at least two obs"
	val _ = if List.all (fn xs => length xs = noobs) obsr then ()
		else raise Fail "accugraph: data series of unequal length"

	(* The image and its color table *)
	val im = image (w, h) (255, 255, 255)
	val black = color im (0, 0, 0)
	val color = makecolors im

	(* Compute accumulated observations *)
	fun loop []          sum = [sum]
	  | loop (xs :: xss) sum = 
	    sum :: loop xss (ListPair.map op+ (xs, sum))
	val accumrows = loop obsr obs1 

	val legendw = addlegend im itemnames black color + 20

	val maxsum  = List.foldr Real.max 0.0 (List.last accumrows)
	val lastsum = List.last (List.last accumrows)
	val xoffset = 10		(* from left *)
	val grfw = w - legendw - 2 * xoffset
	(* The height will be at most 2/3 of the width: *)
	val grfh = Int.min(h - 2 * xoffset, grfw * 2 div 3)
	val yoffset = (h + grfh) div 2	(* from top  *)
	val xscale = real grfw / real (noobs - 1)
	val yscale = real grfh / (maxsum + 2.0) 

	fun pos i y = (round (real i * xscale) + xoffset, 
		       yoffset - round (y * yscale)) 

	fun drawobs i last [] = ()
	  | drawobs i last (obs1 :: obsr) =
	    (drawLine im (Color black) (pos i last, pos (i+1) obs1);
	     drawobs (i+1) obs1 obsr)

	fun drawseries (obs1 :: obsr) = drawobs 0 obs1 obsr;

	fun fillsection ser last [] = ()
	  | fillsection ser last ((_ :: obs12 :: _) :: obsr) =
	    (fill im (color ser) (pos 1 ((last+obs12) / 2.0));
	     fillsection (ser-1) obs12 obsr)

	fun uparrowhead (xy as (x, y)) = 
	    (drawLine im (Color black) (xy, (x-3, y+4));
	     drawLine im (Color black) (xy, (x+3, y+4)))
	fun rightarrowhead (xy as (x, y)) = 
	    (drawLine im (Color black) (xy, (x-4, y-3));
	     drawLine im (Color black) (xy, (x-4, y+3)))
    in
	(* Y axis, left *)
	drawLine im (Color black) ((xoffset, yoffset-grfh-5), 
				   (xoffset, yoffset+5));
	uparrowhead (xoffset, yoffset-grfh-5);
	(* Y axis, left *)
	drawLine im (Color black) ((xoffset+grfw, 
				    yoffset - round (lastsum * yscale)), 
				   (xoffset+grfw, yoffset));
        (* X axis *)
	drawLine im (Color black) ((xoffset-5, yoffset), 
				   (xoffset+grfw+10, yoffset));
	rightarrowhead (xoffset+grfw+10, yoffset);
 
	List.app drawseries accumrows;
	fillsection (length obss - 1) 0.0 accumrows;
	im
    end

