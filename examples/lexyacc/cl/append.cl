(* File "cl/append.cl" -- 
   do indirection chains in the second argument build up? 
   No. *)

letrec 
  from  = \n. pack{2, n, from (n+1)};
  append = \xs.\ys. case xs of
	              <1>      -> ys;
                      <2> x xr -> pack{2, x, append xr ys}
                    end
   
in append (from 1) (from 1)

