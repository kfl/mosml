val write_symbols : bool ref;
val no_header : bool ref;
val stand_alone   : bool ref; (* cvr: 144 merge *)
val autolink  : bool ref
val verbose   : bool ref
val link : string list -> string -> unit;
