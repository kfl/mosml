(* Conversion to Roman numerals.			sestoft@dina.kvl.dk 

   This problem can be solved the same way as paying an amount of money
   using as large bills as possible.  The legal letter combinations (`bills') 
   are 
       M, CM, D, CD, C, XC, L, XL, X, IX, V, IV, I
   with values
       1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1
*)

local 
    val romannum =
	[(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), 
	 (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), 
	 (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

    fun choose (n : int, [])               = []
      | choose (n, romans as ((s, name) :: romanr)) = 
	if n >= s then name :: choose(n - s, romans) else choose(n, romanr)
in
    fun roman n = concat (choose (n, romannum))
end
