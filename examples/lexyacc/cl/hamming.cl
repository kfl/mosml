(* From Gofer demos/examples.gs: Hamming's exercise

hamming :: [Int]
hamming = 1 : (map (2* ) hamming || map (3* ) hamming || map (5* ) hamming)
               where (x:xs) || (y:ys)  | x==y  =  x : (xs || ys)
                                       | x<y   =  x : (xs || (y:ys))
                                       | y<x   =  y : (ys || (x:xs))
*)

letrec 
  merge = \xs.\ys.
          case xs of
            <1>      -> ys;
	    <2> x xr -> case ys of
                          <1>      -> xs;
                          <2> y yr -> if x=y then 
	                                   pack{2, x, merge xr yr}
	                              else if x<y then 
                                           pack{2, x, merge xr ys}
                                      else
                                           pack{2, y, merge xs yr}
                        end
          end;
  map = \f.letrec h = \xs. case xs of
                           <1>      -> pack{1};
                           <2> x xr -> pack{2, f x, h xr}
                           end
           in h;
  hamming = pack{2, 1, merge (map (\n.2*n) hamming)
	               	     (merge (map (\n.3*n) hamming)
        	                    (map (\n.5*n) hamming))};
  take = \n.\xs.case xs of
                  <1>      -> pack {1} ;
                  <2> x xr -> if n=0 then pack {1}
                              else pack {2, x, take (n-1) xr}
                end
in hamming
