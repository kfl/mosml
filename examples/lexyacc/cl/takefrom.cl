(* Stolen and edited from Klaus Elmquist Nielsen, Copyright (c) 1993 *)

letrec
   take = \n.\xs.case xs of
                <1>      -> pack {1} ;
                <2> x xr -> if n=0 then pack {1}
                            else pack {2, x, take (n-1) xr}
                end;

   from = \n. pack{2, n, from (n+1)}
in take 1000 (from 117)

