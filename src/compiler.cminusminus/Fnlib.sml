(* Fnlib.sml. Library functions *)

exception Impossible of string;

fun fatalError s = raise(Impossible s);

fun getOption NONE = fatalError "getOption"
  | getOption (SOME a) = a
;

fun fst (x, y) = x;
fun snd (x, y) = y;

fun incr r = r := !r + 1;
fun decr r = r := !r - 1;

fun mapFrom f n [] = []
  | mapFrom f n (x :: xs) = f n x :: mapFrom f (n+1) xs
;

fun map2 f [] [] = []
  | map2 f (x :: xs) (y :: ys) = f x y :: map2 f xs ys
  | map2 f _ _ = fatalError "map2: lists of different length"
;

fun appFrom f n [] = ()
  | appFrom f n (x :: xs) = (f n x : unit; appFrom f (n+1) xs)
;

fun app2 f [] [] = ()
  | app2 f (x::xs) (y::ys) = (f x y : unit; app2 f xs ys)
  | app2 f _ _ = fatalError "app2: lists of different length"
;

fun revApp f [] = ()
  | revApp f (h::t) = (revApp f t;f  h)
;

fun foldL f a [] = a
  | foldL f a (x::xs) = foldL f (f x a) xs
;

fun foldL_zip f a [] [] = a
  | foldL_zip f a (x::xs) (y::ys) = foldL_zip f (f x y a) xs ys
  | foldL_zip f a _ _ = fatalError "foldL_zip: lists of different length"
;

fun foldL_map f g a [] = a
  | foldL_map f g a (x::xs) = foldL_map f g (f (g x) a) xs
;

fun foldR f a [] = a
  | foldR f a (x::xs) = f x (foldR f a xs)
;

fun foldR1 f [] = fatalError "foldR1: an empty argument"
  | foldR1 f [x]   = x
  | foldR1 f (x::xs) = f x (foldR1 f xs)
;

fun foldR_map f g e [] = e
  | foldR_map f g e (x::xs) = f (g x) (foldR_map f g e xs)
;

fun map_fields f [] = []
  | map_fields f ((lab, t) :: xs) = (lab, f t) :: map_fields f xs
;

fun all_fields f [] = true
  | all_fields f ((_, t) :: xs) =
      (f t) andalso all_fields f xs
;

fun exists_field f [] = false
  | exists_field f ((_, t) :: xs) =
      (f t) orelse exists_field f xs
;

fun app_field f [] = ()
  | app_field f ((_, t) :: xs) = (f t : unit; app_field f xs)
;

fun member k [] = false
  | member k (x :: xs) =
    if k = x then true else member k xs
;

fun exists p [] = false
  | exists p (a::xs) = p a orelse exists p xs
;

fun remove k [] = []
  | remove k (x :: xs) =
    if k = x then xs else x :: (remove k xs)
;

fun drop p [] = []
  | drop p (x :: xs) =
    if p x then (drop p xs) else x :: (drop p xs)
;

fun lookup k [] = raise Subscript
  | lookup k ((a, v) :: xs) =
    if k = a then v else lookup k xs
;


(* The vector vec must be sorted *)

fun binlookup (name : string) (vec : (string * 'a) Vector.vector) =
    let fun search a b = 
	    if a > b then raise Subscript
	    else
		let val i = (a+b) div 2
		    val (key, v) = Vector.sub(vec, i)
		in 
		    if name < key      then search a (i-1)
		    else if key < name then search (i+1) b
		    else v
		end
    in search 0 (Vector.length vec - 1) end;


fun choose p [] = raise Subscript
  | choose p (a::xs) =
    if p a then a else choose p xs
;

fun find p [] = raise Subscript
  | find p ((a, v) :: xs) =
    if p a then v else find p xs
;

fun foldInt f a n = if n <= 0 then a else f n (foldInt f a (n-1))
;

fun duplicates [] = false
  | duplicates (x :: xs) = member x xs orelse duplicates xs
;
fun stringToLower s =
    CharVector.tabulate(size s, fn i => Char.toLower(CharVector.sub(s, i)));

fun for f i j =
  if i > j then () else (f i : unit; for f (i+1) j)
;

fun zip2 [] [] = []
  | zip2 [] (y :: ys) = fatalError "zip2"
  | zip2 (x :: xs) [] = fatalError "zip2"
  | zip2 (x :: xs) (y :: ys) = (x, y) :: zip2 xs ys
;







