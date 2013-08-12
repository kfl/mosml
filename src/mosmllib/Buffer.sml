(* String buffers heavily inspired by the Buffer module in OCaml *)
(* Ken Friis Larsen <ken@friislarsen.net> 2001-07-31                      *)
structure Buffer :> Buffer =
struct

datatype buf = BUF of {elts    : string ref,
		       size    : int ref,
                       initial : int}

local 
    prim_val create_ : int -> string                 = 1 "create_string";
    prim_val update_ : string -> int -> char -> unit = 3 "set_nth_char";
    prim_val blit_   : string -> int -> string -> int -> int -> unit 
                                                     = 5 "blit_string";
in 
fun new bufSize =
    if bufSize > 0 andalso bufSize <= String.maxSize
    then BUF{elts    = ref (create_ bufSize),
	     size    = ref 0,
             initial = bufSize}
    else raise Size

fun clear (BUF{size, ...}) = size := 0

fun size (BUF{size as ref s, ...}) = s

fun reset (BUF{initial, elts, size, ...}) =
    ( elts := create_ initial
    ; size := 0 )

fun contents (BUF{elts as ref arr, size as ref s, ...}) =
    let val newstr = create_ s 
    in blit_ arr 0 newstr 0 s; newstr end

fun resize (BUF{elts as ref arr, size as ref s, ...}) more =
    let val newSize = s + more
        val ()      = if newSize > String.maxSize then raise Size else ()
        fun roundUp new = if new < newSize then roundUp(2*new) 
                          else if new > String.maxSize then String.maxSize
                          else new
        val len     = String.size arr
        val newLen  = roundUp(2*len)
                      handle Overflow => String.maxSize
        val arr'    = create_ newLen
    in  blit_ arr 0 arr' 0 s
      ; elts := arr' end


fun addChar (buf as BUF{elts as ref arr, size as ref s, ...}) c =
    ( if s = String.size arr then resize buf 1 else ()
    ; update_ (!elts) s c
    ; size := s+1
    )

fun addSubString (buf as BUF{elts as ref arr, size as ref s, ...}) x =
    let val (ss, off, len) = Substring.base x
        val newSize = s+len
    in  if newSize >= String.size arr then resize buf len else ()
      ; blit_ ss off (!elts) s len
      ; size := newSize end

fun addString (buf as BUF{elts as ref arr, size as ref s, ...}) x = 
    let val len = String.size x
        val newSize = s+len  
    in  if newSize >= String.size arr then resize buf len else ()
      ; blit_ x 0 (!elts) s len
      ; size := newSize end

end (*end local*)
end


(* Implementation in pure SML (using slices) *)
(*
structure Buffer :> Buffer =
struct
   structure CA  = CharArray
   structure CAS = CharArraySlice
   structure CVS = CharVectorSlice

   datatype buf = BUF of {elts    : CA.array ref,
		          size    : int ref,
                          initial : int}

   fun contents (BUF{elts as ref arr, size as ref s, ...}) = 
       CAS.vector(CAS.slice(arr, 0, SOME s))

   fun clear (BUF{size, ...}) = size := 0

   fun size (BUF{size as ref s, ...}) = s

   fun new bufSize =
       if bufSize > 0 then BUF{elts = ref (CA.array (bufSize, #"\000")),
	                       size = ref 0,
                               initial = bufSize}
       else raise Size

   fun reset (BUF{initial, elts, size, ...}) =
       ( elts := CA.array (initial, #"\000")
       ; size := 0 )


   fun resize (BUF{elts as ref arr, size as ref s, ...}) more =
       let val newSize = s + more
           val ()      = if newSize > CA.maxLen then raise Size else ()
           fun roundUp new = if new < newSize then roundUp(2*new) else new
           val len     = CA.length arr
           val newLen  = roundUp(2*len) 
                         handle Overflow => CA.maxLen
           val arr'    = CA.array (newLen, #"\000")
                         handle Size => CA.array (CA.maxLen, #"\000")
       in  CAS.copy{src=CAS.slice(arr, 0, SOME s), dst=arr', di=0}
         ; elts := arr' end


   fun addChar (buf as BUF{elts as ref arr, size as ref s, ...}) x =
       ( if s = CA.length arr then resize buf 1 else ()
       ; CA.update (!elts, s, x)
       ; size := s+1)

   fun addSubString (buf as BUF{elts as ref arr, size as ref s, ...}) x =
       let val len = CVS.length x
           val newSize = s+len  
       in  if newSize >= CA.length arr then resize buf len else ()
         ; CAS.copyVec{src = x, dst = !elts, di = s}
         ; size := newSize end

   fun addString (buf as BUF{elts as ref arr, size as ref s, ...}) x = 
       let val len = String.size x
           val newSize = s+len  
       in  if newSize >= CA.length arr then resize buf len else ()
         ; CA.copyVec{src = x, dst = !elts, di = s}
         ; size := newSize end
end
*)
