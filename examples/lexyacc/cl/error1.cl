(* The infinite list of natural numbers --- with lexical error

letrec map = \f.\xs. case xs of
                           <1>      -> pack{1};
                           <2> x xr -> pack{2, f x, map f xr}
                     end; 
       add1 = \y.y+1; 
       nats = pack{2, 0, map add1 nats}
in nats
