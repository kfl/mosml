(* CharArraySlice -- SML Basis Library *)

type elem = char
type array = CharArray.array
type vector = CharVector.vector
type vector_slice = CharVectorSlice.slice

local 
    prim_val magic : 'a -> 'b = 1 "identity";
in
    type slice = array * int * int

    val length   : slice -> int		= magic Word8ArraySlice.length;
    val sub      : slice * int -> elem  = magic Word8ArraySlice.sub;
    val update   : slice * int * elem -> unit 
					= magic Word8ArraySlice.update;
    val slice    : array * int * int option -> slice  
					= magic Word8ArraySlice.slice;
    val full     : array -> slice	= magic Word8ArraySlice.full;
    val subslice : slice * int * int option -> slice  
					= magic Word8ArraySlice.subslice;
    val base     : slice -> array * int * int 
					= magic Word8ArraySlice.base;
    val vector   : slice -> vector	= magic Word8ArraySlice.vector;
    val copy     : {src: slice, dst: array, di: int} -> unit 
					= magic Word8ArraySlice.copy;
    val copyVec  : {src: vector_slice, dst: array, di: int} -> unit  
					= magic Word8ArraySlice.copyVec;
    val isEmpty  : slice -> bool	= magic Word8ArraySlice.isEmpty;
    val getItem  : slice -> (elem * slice) option 
					= magic Word8ArraySlice.getItem;
    val find     : (elem -> bool) -> slice -> elem option 
					= magic Word8ArraySlice.find;
    val exists   : (elem -> bool) -> slice -> bool 
					= magic Word8ArraySlice.exists;
    val all      : (elem -> bool) -> slice -> bool 
					= magic Word8ArraySlice.all;
    val app      : (elem -> unit) -> slice -> unit 
					= magic Word8ArraySlice.app;
    val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b 
					= magic Word8ArraySlice.foldl;
    val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b 
					= magic Word8ArraySlice.foldr;
    val modify   : (elem -> elem) -> slice -> unit 
					= magic Word8ArraySlice.modify;
    val findi    : (int * elem -> bool) -> slice -> (int * elem) option 
					= magic Word8ArraySlice.findi;
    val appi     : (int * elem -> unit) -> slice -> unit 
					= magic Word8ArraySlice.appi;
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b 
					= magic Word8ArraySlice.foldli;
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b 
					= magic Word8ArraySlice.foldri;
    val modifyi  : (int * elem -> elem) -> slice -> unit 
					= magic Word8ArraySlice.modifyi;
    val collate  : (elem * elem -> order) -> slice * slice -> order 
					= magic Word8ArraySlice.collate;
end
