(* Example drawing using Gdimage -- not an exhaustive test suite.
   File mosml/src/dynlibs/gd/testgdimage.sml * sestoft@dina.kvl.dk 1998-05-13
 *)

app load ["Gdimage", "Graphs"];


val _ = let 

open Gdimage

val im = image (200, 200) (0, 0, 0);

val {black, white, silver, gray, maroon, red, purple, fuchsia, green,
     lime, olive, yellow, navy, blue, teal, aqua} = htmlcolors im;

val triangle1 = #[(10, 100), (30, 100), (20, 110)];

val triangle2 = #[(10, 120), (30, 120), (20, 110)];

fun drawCircle im mo center radius = 
    drawArc im mo { c = center, wh = (2*radius, 2*radius), 
		   from = 0, to = 360 };

fun fillCircle im color center radius = 
    (drawCircle im (Color color) center radius;
     fillBorder im (Color color) center color);

val redwhitedash = 
    Vector.map ColorS #[red, red, red, white, white, white];

val usualdash = 
    #[TransparentS, TransparentS, TransparentS, 
      ColorS white, ColorS white, ColorS white];

val crayon = image (2, 6) (0, 200, 200);

val tile = image (8, 8) (0, 0, 0);

val _ = fillRect tile (Color (color tile (255, 0, 0))) ((0, 0), (3, 3));
val _ = fillRect tile (Color (color tile (255, 255, 255))) ((4, 4), (7, 7));


in 
drawLine im (Color white) ((10, 0), (40, 50));

drawRect im (Color aqua) ((40, 50), (100, 150));

fillRect im (Color yellow) ((100, 50), (130, 80));

drawPolygon im (Color red) triangle1;

fillPolygon im (Color red) triangle2;

drawArc im (Color lime) { c = (150, 50), wh = (60, 40), from = 270, to = 90 };

drawCircle im (Color green) (160, 160) 40;

fillCircle im lime (100, 160) 40;

copy { src = tile, srcxy = (0,0), srcwh = size tile,
       dst = im, dstxy = (20, 20) };

fillRect im (Tiled tile) ((40, 0), (100, 24));

string im fuchsia Tiny (100, 100) "זרו hen";

string im fuchsia Small (100, 115) "זרו hen";

string im fuchsia MediumBold (100, 130) "זרו hen";

string im fuchsia Large (100, 145) "זרו hen";

string im fuchsia Giant (100, 160) "זרו hen";

stringUp im blue Large (100, 100) "Lodret";

drawRect im (Color aqua) ((10, 180), (20, 190));

fill im (Color aqua) (15, 185);

drawLine im (Styled redwhitedash) ((190, 0), (0, 190));

drawLine im (Styled usualdash) ((10, 0), (190, 200));

drawCircle im (Brushed crayon) (170, 90) 20;

drawLine im (Styled usualdash) ((10, 0), (190, 200));

toPng im "try.png"

end;

val _ = let 

fun dice n = 
    let val gen = Random.newgen()
	val freq = Array.array(6, 0)
	fun loop 0 = ()
	  | loop i = let val x = Random.range (0, 6) gen;
		     in 
			 Array.update(freq, x, Array.sub(freq, x) + 1);
			 loop (i-1)
		     end
    in 
	loop n;
	List.map real (Array.foldr op:: [] freq)
    end

fun graphs () = 
    let val gen = Random.newgen ()
	val n   = Random.range (20, 30) gen
	fun mkseries trend = 
	    List.tabulate(n, fn i => trend i * (1.0 - 0.3 * Random.random gen))
	fun trend1 i = real i / 2.0 + 2.0;
	fun trend2 i = real (n-i) / 3.0 + 2.0;
	fun trend3 i = Math.ln (real (i+1)) * 4.0 + 2.0
	fun trend4 i = real (abs (n div 2 - i)) + 2.0
	val trendlist = [trend1, trend2, trend3, trend4]

	val data = map mkseries trendlist

	val tekster = ["Praise", "Annoyances", "Salary", "Boredom"]
    in 
	Gdimage.toPng (Graphs.accugraph (550, 220) data tekster) "graph.png"
    end

in
    Gdimage.toPng (Graphs.histogram (500, 100) 
	                   (Random.rangelist (0, 100) (100, Random.newgen()))) 
    "hist.png";

    Gdimage.toPng (Graphs.piechart (500, 300) (dice 100) 
	   ["One", "Two", "Three", "Four", "Five", "Six"])
    "pie.png";

    graphs ();

    print "Now try\n\
              \       xv try.png hist.png pie.png graph.png\n\n";

    quit()
end

