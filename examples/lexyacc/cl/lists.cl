(* An unsystematic collection of list utility functions *)

   ones = pack{2, 1, ones};

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

  map = \f.\xs. case xs of
                  <1>      -> pack{1};
                  <2> x xr -> pack{2, f x, map f xr}
                end; 

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

  append = \xs.\ys. case xs of
	              <1>      -> ys;
                      <2> x xr -> pack{2, x, append xr ys}
                    end;
 
  (* The following version of scanr works only for finite lists *)

  scanr = \f.\q0.letrec h = \xs. case xs of
                                   <1>      -> pack{2, q0, pack{1}};
                                   <2> x xr -> case h xr of
                                                 <1>      -> pack{1};
                                                 <2> q qs -> pack{2, f x q, qs}
                                               end
                                 end
                 in h

  (* This alternative version works for infinite lists but leaks space
     because of Wadler/Sparud's problem *)

  scanr = \f.\q0.letrec h = \xs. case xs of
                                   <1>      -> pack{2, q0, pack{1}};
                                   <2> x xr -> letrec qqs = h xr 
                                                      q   = head qqs
                                                      qs  = tail qqs
					       in pack{2, f x q, qs}
                                 end
                                 end
                 in h
