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
    val vector   : array -> vector		= magic Word8Array.vector
    val copy     : {src: array,  dst: array, di: int} -> unit 
						= magic Word8Array.copy
    val copyVec  : {src: vector, dst: array, di: int} -> unit 
						= magic Word8Array.copyVec
    val app      : (elem -> unit) -> array -> unit
						= magic Word8Array.app

    val find     : (elem -> bool) -> array -> elem option
						= magic Word8Array.find
    val exists   : (elem -> bool) -> array -> bool
						= magic Word8Array.exists
    val all      : (elem -> bool) -> array -> bool
						= magic Word8Array.all

    fun foldl (f : elem * 'b -> 'b) (e : 'b) (a : array) : 'b
	= Word8Array.foldl (magic f) e (magic a)

    fun foldr (f : elem * 'b -> 'b) (e : 'b) (a : array) : 'b
	= Word8Array.foldr (magic f) e (magic a)

    fun modify (f : elem -> elem) (a : array) : unit
	= Word8Array.modify (magic f) (magic a)

    val findi : (int * elem -> bool) -> array -> (int * elem) option
	                                        = magic Word8Array.findi

    fun appi (f : int * elem -> unit) (a : array) : unit
	= Word8Array.appi (magic f) (magic a)

    fun foldli (f : int * elem * 'b -> 'b) (e : 'b) (a : array) : 'b
	= Word8Array.foldli (magic f) e (magic a)

    fun foldri (f : int * elem * 'b -> 'b) (e : 'b) (a : array) : 'b 
	= Word8Array.foldri (magic f) e (magic a)

    fun modifyi (f : int * elem -> elem) (a : array) : unit
	= Word8Array.modifyi (magic f) (magic a)

    val collate  : (elem * elem -> order) -> array * array -> order
						= magic Word8Array.collate
end

