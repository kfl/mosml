(* Math -- SML Basis Library *)

type real = real

val pi    : real
val e     : real

val sqrt  : real -> real
val sin   : real -> real
val cos   : real -> real
val tan   : real -> real
val atan  : real -> real
val asin  : real -> real
val acos  : real -> real
val atan2 : real * real -> real
val exp   : real -> real
val pow   : real * real -> real
val ln    : real -> real
val log10 : real -> real
val sinh  : real -> real
val cosh  : real -> real
val tanh  : real -> real

(*  
   [pi] is the circumference of the circle with diameter 1, that is,
   3.14159265358979323846.

   [e] is the base of the natural logarithm: 2.7182818284590452354.

   [sqrt x] is the square root of x.  Raises Domain if x < 0.0.

   [sin r] is the sine of r, where r is in radians.

   [cos r] is the cosine of r, where r is in radians.

   [tan r] is the tangent of r, where r is in radians.  Raises Domain if 
   r is a multiple of pi/2.

   [atan t] is the arc tangent of t, in the open interval ] ~pi/2, pi/2 [.

   [asin t] is the arc sine of t, in the closed interval [ ~pi/2, pi/2 ].  
   Raises Domain if abs x > 1.

   [acos t] is the arc cosine of t, in the closed interval [ 0, pi ].
   Raises Domain if abs x > 1.

   [atan2(y, x)] is the arc tangent of y/x, in the interval ] ~pi, pi ],
   except that atan2(y, 0) = sign y * pi/2.  The quadrant of the result
   is the same as the quadrant of the point (x, y).
   Hence sign(cos(atan2(y, x))) = sign x 
   and   sign(sin(atan2(y, x))) = sign y. 

   [exp x] is e to the x'th power.

   [pow (x, y)] is x it the y'th power, defined when 
      y >= 0 and (y integral or x >= 0)
   or y < 0 and ((y integral and x <> 0.0) or x > 0).

   We define pow(0, 0) = 1.

   [ln x] is the natural logarithm of x (that is, with base e).
   Raises Domain if x <= 0.0.

   [log10 x] is the base-10 logarithm of x.  Raises Domain if x <= 0.0.

   [sinh x] returns the hyperbolic sine of x, mathematically defined as
   (exp x - exp (~x)) / 2.  Raises Overflow if x is too large.

   [cosh x] returns the hyperbolic cosine of x, mathematically defined as
   (exp x + exp (~x)) / 2.  Raises Overflow if x is too large.

   [tanh x] returns the hyperbolic tangent of x, mathematically defined 
   as (sinh x) / (cosh x).  Raises Domain if x is too large.
*)
