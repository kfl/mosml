signature Parser =
sig
    type 'a stream

    val makeStream : ('src -> ('a * 'src) option) -> 'src -> 'a stream
    val split      : 'a stream -> ('a * 'a stream) option

    type ('elm, 'res) parser = 'elm stream -> ('res * 'elm stream) option

    val $$ : char   -> (char, char) parser
    val $  : string -> (char, string) parser
    val getChars  : (char -> bool) -> (char, string) parser
    val getChars1 : (char -> bool) -> (char, string) parser
    val getChar   : (char -> bool) -> (char, char) parser

    val -- : ('a, 'b) parser * ('a, 'c) parser -> ('a, 'b * 'c) parser
    val || : ('a, 'b) parser * ('a, 'b) parser -> ('a, 'b) parser
    val >> : ('a, 'b) parser * ('b -> 'c) -> ('a, 'c) parser

    val bind : ('a, 'b) parser * ('b -> ('a, 'c) parser) -> ('a, 'c) parser

    val #-- : ('a, 'b) parser * ('a, 'c) parser -> ('a, 'c) parser
    val --# : ('a, 'b) parser * ('a, 'c) parser -> ('a, 'b) parser

    val repeat    : ('a, 'b) parser -> ('a, 'b list) parser
    val optional  : ('a, 'b) parser -> ('a, 'b option) parser

    val empty : 'res -> ('a, 'res) parser
    val eof   : 'res -> ('a, 'res) parser
end
