(* Fnlib.sig *)

exception Impossible of string;
val fatalError : string -> 'a;

val getOption : 'a option -> 'a;

val fst : 'a * 'b -> 'a;
val snd : 'a * 'b -> 'b;

val incr : int ref -> unit;
val decr : int ref -> unit;

val mapFrom:  (int -> 'a -> 'b) -> int -> 'a list -> 'b list;
val map2:     ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list;
val appFrom:  (int -> 'a -> unit) -> int -> 'a list -> unit;
val app2:     ('a -> 'b -> unit) -> 'a list -> 'b list -> unit;
val revApp:     ('a -> unit) -> 'a list -> unit;
val foldL:    ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b;
val foldL_zip : ('a -> 'b -> 'c -> 'c) -> 'c -> 'a list -> 'b list -> 'c;
val foldL_map : ('a -> 'b -> 'b) -> ('c -> 'a) -> 'b -> 'c list -> 'b;
val foldR:    ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b;
val foldR1:   ('a -> 'a -> 'a) -> 'a list -> 'a;
val foldR_map: ('a -> 'b -> 'b) -> ('c -> 'a) -> 'b -> 'c list -> 'b;
val map_fields : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list;
val all_fields : ('a -> bool) -> ('b * 'a) list -> bool;
val exists_field : ('a -> bool) -> ('b * 'a) list -> bool;
val app_field : ('a -> unit) -> ('b * 'a) list -> unit;
val member : ''a -> ''a list -> bool
val remove : ''a -> ''a list -> ''a list
val drop : ('a -> bool) -> 'a list -> 'a list 
val lookup : ''a -> (''a * 'b) list -> 'b   (* May raise Subscript *)
val binlookup  : string -> (string * 'b) Vector.vector -> 'b   (* Subscript *) 
val exists: ('a -> bool) -> 'a list -> bool 
val choose: ('a -> bool) -> 'a list -> 'a   (* May raise Subscript *)
val find : ('a -> bool) -> ('a * 'b) list -> 'b   (* May raise Subscript *)
val foldInt : (int -> 'b -> 'b) -> 'b -> int -> 'b
val duplicates : ''a list -> bool;
val stringToLower : string -> string;

val for : (int -> unit) -> int -> int -> unit;

val zip2 : 'a list -> 'b list -> ('a * 'b) list;

