(* Compute the 17th Fibonacci number, inefficiently *)

letrec 
	fib = \n.if n = 0 | n = 1 then 1 else fib (n-1) + fib (n-2)
in fib 17
