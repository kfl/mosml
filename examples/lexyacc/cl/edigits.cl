(* Translated from Gofer demos/examples.gs: Digits of e

eFactBase ::  [Int]
eFactBase  =  map head (iterate scale (2:repeat 1))

scale      =  renorm . map (10* ) . tail
renorm ds  =  foldr step [0] (zip ds [2..])

step (d,n) bs | (d `mod` n + 9) < n  = (d/n) : b : tail bs
              | otherwise            = c     : b : tail bs
              where b' = head bs
                    b  = (d+b') `mod` n
                    c  = (d+b') `div` n
*)

letrec 
  eFactBase = map head (iterate scale pack{2, 2, repeat 1});
  scale = compose (compose renorm (map (\n.10*n))) tail;
  renorm = \ds. foldr step pack{2, 0, pack{1}} (zip ds (from 2));
  step = \dn.\bs. 
         case dn of
           <1> d n -> letrec 
                        bp = head bs;
                        b  = (d+bp) % n;
                        c  = (d+bp) / n
                      in pack{2, if (d % n + 9) < n then d/n else c,
                                 pack{2, b, tail bs}}
         end;
  map = \f.\xs. case xs of
                  <1>      -> pack{1};
                  <2> x xr -> pack{2, f x, map f xr}
                end; 
  take = \n.\xs.case xs of
                  <1>      -> pack {1} ;
                  <2> x xr -> if n=0 then pack {1}
                              else pack {2, x, take (n-1) xr}
                end;
  compose = \f.\g.\x. f(g x);
  head = \xs. case xs of
                <1>      -> 0;
                <2> x xr -> x
              end;
  tail = \xs. case xs of
                <1>      -> pack{1};
                <2> x xr -> xr
              end;
  zip = \xs.\ys.
        case xs of
          <1>      -> pack{1};
          <2> x xr -> case ys of
                        <1>      -> pack{1};
                        <2> y yr -> pack{2, pack{1, x, y}, zip xr yr}
	              end
        end;
  foldr = \f.\z.\xs.
          case xs of
            <1>      -> z;
            <2> x xr -> f x (foldr f z xr)
          end; 
  iterate = \f.\x. pack{2, x, iterate f (f x)};
  repeat = \x.letrec xs = pack{2, x, xs} in xs;
  from = \n. pack{2, n, from (n+1)}
in take 100 eFactBase

