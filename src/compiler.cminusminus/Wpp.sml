(* A Pretty Printer, based on Philip Wadler's "A prettier printer".
   But heavily modified to be efficient in a strict language. 
   http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html

   Copyright 1997, 1998, 1999, 2000, 2001 Ken Friis Larsen <ken@friislarsen.net>

   This code is released under GNU LGPL version 2 or any later after
   your choice, the licence can be obtained at
   http://www.gnu.org/copyleft/lgpl.html
*)
structure Wpp :> Wpp =
struct
    infixr 6 ^^

    datatype doc = 
	NIL
      | APPEND of doc * doc
      | NEST   of int * doc
      | TEXT   of string
      | BREAK  of int * int
      | NEWLINE
      | GROUP  of doc

    fun op^^ p     = case p of
                       (NIL,NIL) => NIL
                     | (NIL, y)  => y
                     | (x, NIL)  => x
                     | _         => APPEND p

    val empty    = NIL
    fun nest i x = NEST(i,x)
    val text     = TEXT 
    fun break sp off = BREAK(sp, off)
    val line     = BREAK (1,0)
    val newline  = NEWLINE
    fun group x	 = GROUP x	

    (*** Derived functions ***)
    val concat     = List.foldr op^^ empty
    fun seq sep ppr xs =
        let fun iter nil acc     = acc
              | iter [x] acc     = acc ^^ ppr x
              | iter (x::xs) acc = iter xs (acc ^^ ppr x ^^ sep)
        in  iter xs empty
        end

    fun fromConv conv x = text(conv x)
    val int   = fromConv Int.toString
    val char  = fromConv Char.toString
    val word  = fromConv Word.toString
    val word8 = fromConv Word8.toString
    val real  = fromConv Real.toString
    fun bool b = if b then text "true" else text "false"



    (*** Formating of docs ***)

    val nlsize = String.size "\n" 
    fun spaces outs s i  = outs s (StringCvt.padLeft #" " i "")
    fun nlspace outs s i = outs s (StringCvt.padRight #" " (i+nlsize) "\n")

    local
	datatype mode = Flat | Break

        fun fitting [] left                       = true
          | fitting ((i, mode, doc) :: rest) left = 
            if left >= 0 
            then case doc of 
                     NIL         => fitting rest left
                   | APPEND(x,y) => fitting ((i,mode,x)::(i,mode,y)::rest) left
                   | NEST(j,x)   => fitting ((i+j, mode, x) :: rest) left
                   | TEXT s      => fitting rest (left - String.size s)
                   | BREAK(sp,_) => (case mode of 
                                         Flat  => fitting rest (left - sp)
                                       | Break => true)
                   | NEWLINE     => true
                   | GROUP x     => fitting ((i,mode,x) :: rest) left
            else false
    in
    (* w    : linewidth
       outs : function to output a string
       s    : state for outs
       k    : number of chars already used on current line
       i    : indent after linebreaks
    *)
    fun best w outs s x = 
        let fun be s k [] = s
              | be s k ((i, mode, doc) :: rest) =
                case doc of
                    NIL           => be s k rest
                  | APPEND(x,y)   => be s k ((i,mode,x)::(i,mode,y)::rest)
                  | NEST(j,x)     => be s k ((i+j, mode, x):: rest)
                  | TEXT str      => let val s = outs s str
                                     in  be s (k+String.size str) rest end
                  | NEWLINE       => let val s = nlspace outs s i
                                     in  be s i rest end
                  | BREAK(sp,off) => 
                    (case mode of
                         Flat     => let val s = spaces outs s sp
                                     in  be s (k+sp) rest end
                       | Break    => let val s = nlspace outs s (i+off)
                                     in  be s (i+off) rest end)
                  | GROUP x       =>
                    (case mode of
                         Flat     => be s k ((i,Flat,x) :: rest)
                       | Break    => 
                         if fitting  ((i,Flat,x)::rest) (w - k)
                         then be s k ((i,Flat,x)::rest)
                         else be s k ((i,Break,x)::rest))
        in be s 0 [(0,Break,x)]
        end
    end

    fun toOutStream w outstream doc =
      let fun outs () s = TextIO.output(outstream, s)
      in  best w outs () doc
        ; outs () "\n"
        ; TextIO.flushOut outstream
      end

    fun toFile w filename doc =
        let val dev = TextIO.openOut filename
        in  (toOutStream w dev doc) handle ? => (TextIO.closeOut dev; raise ?)
          ; TextIO.closeOut dev
        end

    fun toString w doc =
      let fun outs strs s = s :: strs
          val strs = best w outs [] doc
      in  String.concat (List.rev ("\n" :: strs))
      end

    val toConsumer = best
end


