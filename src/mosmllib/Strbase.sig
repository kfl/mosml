(* Strbase -- internal auxiliaries for String and Substring *)

local 
    type substring = string * int * int
in 
    val maxlen    : int
    val dropl     : (char -> bool) -> substring -> substring
    val dropr     : (char -> bool) -> substring -> substring
    val takel     : (char -> bool) -> substring -> substring
    val taker     : (char -> bool) -> substring -> substring
    val splitl    : (char -> bool) -> substring -> substring * substring
    val splitr    : (char -> bool) -> substring -> substring * substring
	
    val translate : (char -> string) -> substring -> string
	
    val tokens    : (char -> bool) -> substring -> substring list
    val fields    : (char -> bool) -> substring -> substring list

    val foldl     : (char * 'a -> 'a) -> 'a -> substring -> 'a
    val fromMLescape : (int -> (char * int) option) -> 
	               (int -> (char * int) option)
    val toMLescape   : char -> string
    val fromCescape  : (int -> (char * int) option) -> 
	               (int -> (char * int) option)
    val toCescape    : char -> string
    val fromCString  : string -> string option 
end
