(* Compute the infinite list of Fibonacci numbers efficiently  *)

letrec
  zipwith = \f.\xs.\ys.
            case xs of
              <1>      -> pack{1};
              <2> x xr -> case ys of
                            <1>      -> pack{1};
                            <2> y yr -> pack{2, f x y, zipwith f xr yr}
	                  end
            end;
  head = \xs. case xs of
                <1>      -> 0;
                <2> x xr -> x
              end;
  tail = \xs. case xs of
                <1>      -> pack{1};
                <2> x xr -> xr
              end;
  add = \x.\y.x+y;
  fibs = pack{2, 1, pack{2, 1, zipwith add fibs (tail fibs)}};
  take = \n.\xs.case xs of
                  <1>      -> pack {1} ;
                  <2> x xr -> if n=0 then pack {1}
                              else pack {2, x, take (n-1) xr}
                end
in fibs
