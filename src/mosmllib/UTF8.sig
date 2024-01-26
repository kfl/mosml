signature UTF8 = sig

exception BadUTF8 of string

type ('a, 'b) reader = 'b -> ('a * 'b) option

val scanUTF8 : (char, 'a) reader -> (string, 'a) reader
val scanUCS : (char, 'a) reader -> (word, 'a) reader
val UCStoUTF8String : word -> string
val UCSfromUTF8String : string -> word option
val size : string -> int
val size_ : string -> int
val padLeft : char -> int -> string -> string
val padRight : char -> int -> string -> string
val UTF8fromUTF8string : string -> string option

val scanUTF8Transition : (char, 'a) reader -> (string, 'a) reader

end
