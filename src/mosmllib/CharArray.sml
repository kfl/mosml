(* CharArray -- as of 1995-06-14 *)

prim_eqtype array
type elem   = Char.char
type vector = CharVector.vector

local 
    prim_val magic : 'a -> 'b = 1 "identity";
in
    val maxLen = Word8Array.maxLen
    val array    : int * elem -> array          = magic Word8Array.array
    val tabulate : int * (int -> elem) -> array = magic Word8Array.tabulate
    val fromList : elem list -> array           = magic Word8Array.fromList
    val length   : array -> int                 = magic Word8Array.length
    val sub      : array * int -> elem          = magic Word8Array.sub
    val update   : array * int * elem -> unit   = magic Word8Array.update
    val extract  : array * int * int option -> vector  
						= magic Word8Array.extract
    val copy     : {src: array, si: int, len : int option, 
		    dst: array, di: int} -> unit = magic Word8Array.copy
    val copyVec  : {src: vector, si: int, len: int option, 
		    dst: array, di: int} -> unit = magic Word8Array.copyVec
    val app      : (elem -> unit) -> array -> unit
						= magic Word8Array.app

    fun foldl (f : elem * 'b -> 'b) (e : 'b) (a : array) : 'b
	= Word8Array.foldl (magic f) e (magic a)

    fun foldr (f : elem * 'b -> 'b) (e : 'b) (a : array) : 'b
	= Word8Array.foldr (magic f) e (magic a)

    fun modify (f : elem -> elem) (a : array) : unit
	= Word8Array.modify (magic f) (magic a)

    fun appi (f : int * elem -> unit) (a : array*int*int option) : unit
	= Word8Array.appi (magic f) (magic a)

    fun foldli (f : int * elem * 'b -> 'b) (e : 'b) 
               (a : array*int*int option) : 'b
	= Word8Array.foldli (magic f) e (magic a)

    fun foldri (f : int * elem * 'b -> 'b) (e : 'b) 
	       (a : array*int*int option) : 'b 
	= Word8Array.foldri (magic f) e (magic a)

    fun modifyi (f : int * elem -> elem) (a : array*int*int option) : unit
	= Word8Array.modifyi (magic f) (magic a)

(*
    val foldl    : (elem * 'b -> 'b) -> 'b -> array -> 'b
						= magic Word8Array.foldl
    val foldr    : (elem * 'b -> 'b) -> 'b -> array -> 'b
						= magic Word8Array.foldr
    val modify   : (elem -> elem) -> array -> unit
						= magic Word8Array.modify
    val appi     : (int * elem -> unit) -> array*int*int option -> unit
						= magic Word8Array.appi
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> array*int*int option -> 'b
						= magic Word8Array.foldli
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> array*int*int option -> 'b
						= magic Word8Array.foldri
    val modifyi  : (int * elem -> elem) -> array*int*int option -> unit
						= magic Word8Array.modifyi
*)
end

