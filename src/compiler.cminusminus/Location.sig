datatype Location =
    Loc of int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
;

val getCurrentLocation : unit -> Location
and mkLoc : 'a -> Location * 'a
and xLR : Location * 'a -> Location
and xL : Location * 'a -> int
and xR : Location * 'a -> int
and xxLR : Location * 'a -> Location * 'b -> Location
and xxRL : Location * 'a -> Location * 'b -> Location
and nilLocation : Location
and errLocation : Location -> unit
and errInputName : unit -> unit
and input_name : string ref
and input_stream : BasicIO.instream ref
and input_lexbuf : Lexing.lexbuf ref
and errorMsg : Location -> string -> 'a
;
