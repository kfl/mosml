(* CharVector *)

type vector = string
type elem = Char.char

local 
    prim_val magic : 'a -> 'b = 1 "identity";
in
    val maxLen = Word8Vector.maxLen
	
    val fromList : elem list -> vector 		= magic Word8Vector.fromList
    val tabulate : int * (int -> elem) -> vector = magic Word8Vector.tabulate

    val length   : vector -> int  		= magic Word8Vector.length
    val sub      : vector * int -> elem         = magic Word8Vector.sub
    val extract  : vector * int * int option -> vector 
						= magic Word8Vector.extract
    val concat   : vector list -> vector        = magic Word8Vector.concat


    val app      : (elem -> unit) -> vector -> unit
						= magic Word8Vector.app
    val map      : (elem -> elem) -> vector -> vector
						= magic Word8Vector.map
    fun foldl (f : elem * 'b -> 'b) (e : 'b) v : 'b
	= Word8Vector.foldl (magic f) e (magic v)

    fun foldr (f : elem * 'b -> 'b) (e : 'b) (v : vector) : 'b
	= Word8Vector.foldr (magic f) e (magic v)
	
    fun appi (f : int * elem -> unit) (v : vector * int * int option) : unit
	= Word8Vector.appi (magic f) (magic v)

    fun mapi (f : int * elem -> elem) (v : vector * int * int option) : vector
	= magic(Word8Vector.mapi (magic f) (magic v))

    fun foldli (f : int * elem * 'b -> 'b) (e : 'b) 
	       (v : vector*int*int option) : 'b
        = Word8Vector.foldli (magic f) e (magic v)

    fun foldri (f : int * elem * 'b -> 'b) (e : 'b) 
	       (v : vector*int*int option) : 'b
	= Word8Vector.foldri (magic f) e (magic v)

(*
    val foldl    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
						= magic Word8Vector.foldl
    val foldr    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
						= magic Word8Vector.foldr
	
    val appi     : (int * elem -> unit) -> vector * int * int option -> unit
						= magic Word8Vector.appi
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> vector*int*int option -> 'b
						= magic Word8Vector.foldli
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> vector*int*int option -> 'b
						= magic Word8Vector.foldri
*)
end
