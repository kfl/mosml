(* CharVectorSlice -- SML Basis Library *)

type elem = Char.char
type vector = CharVector.vector

local 
    prim_val magic : 'a -> 'b = 1 "identity";
in
    type slice = Substring.substring

    val length   : slice -> int		= magic Word8VectorSlice.length;    
    val sub      : slice * int -> elem	= magic Word8VectorSlice.sub;    
    val slice    : vector * int * int option -> slice
					= magic Word8VectorSlice.slice;    
    val full     : vector -> slice	= magic Word8VectorSlice.full;    
    val subslice : slice * int * int option -> slice
					= magic Word8VectorSlice.subslice;    
    val base     : slice -> vector * int * int
					= magic Word8VectorSlice.base;    
    val vector   : slice -> vector	= magic Word8VectorSlice.vector;    
    val concat   : slice list -> vector = magic Word8VectorSlice.concat;    
    val isEmpty  : slice -> bool	= magic Word8VectorSlice.isEmpty;    
    val getItem  : slice -> (elem * slice) option
					= magic Word8VectorSlice.getItem;    
    val find     : (elem -> bool) -> slice -> elem option
					= magic Word8VectorSlice.find;    
    val exists   : (elem -> bool) -> slice -> bool
					= magic Word8VectorSlice.exists;    
    val all      : (elem -> bool) -> slice -> bool
					= magic Word8VectorSlice.all;    
    val app      : (elem -> unit) -> slice -> unit
					= magic Word8VectorSlice.app;    
    val map      : (elem -> elem) -> slice -> vector
					= magic Word8VectorSlice.map;    
    val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
					= magic Word8VectorSlice.foldl;    
    val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
					= magic Word8VectorSlice.foldr;    
    val findi    : (int * elem -> bool) -> slice -> (int * elem) option
					= magic Word8VectorSlice.findi;    
    val appi     : (int * elem -> unit) -> slice -> unit
					= magic Word8VectorSlice.appi;    
    val mapi     : (int * elem -> elem) -> slice -> vector
					= magic Word8VectorSlice.mapi;    
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
					= magic Word8VectorSlice.foldli;    
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
					= magic Word8VectorSlice.foldri;    
    val collate  : (elem * elem -> order) -> slice * slice -> order
					= magic Word8VectorSlice.collate;
end
