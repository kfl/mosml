(* Stolen and edited from Klaus Elmquist Nielsen, Copyright (c) 1993 *)

letrec
   sieve = \xs.case xs of
               <1>      -> pack {1} ;
               <2> x xs -> pack {2, x, sieve (filter (\n.(n % x) ~= 0) xs)}
               end ;

   take = \n.\xs.case xs of
                 <1>      -> pack {1} ;
                 <2> x xr -> if n=0 then pack {1}
                             else pack {2, x, take (n-1) xr}
                 end;

   filter = \p.\xs.case xs of
                   <1>      -> pack {1} ;
                   <2> x xs -> if p x then pack {2, x, filter p xs}
                               else filter p xs
                   end ;

   from = \n.pack {2, n, from (n+1)}

in take 300 (sieve (from 2))

