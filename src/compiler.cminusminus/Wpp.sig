(* A Pretty Printer, based on Philip Wadler's "A prettier printer".
   But heavily modified to be efficient in a strict language. 
   http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html

   Copyright 1997, 1998, 1999, 2000, 2001 Ken Friis Larsen <kfl@it.edu>

   This code is released under GNU LGPL version 2 or any later after
   your choice, the licence can be obtained at
   http://www.gnu.org/copyleft/lgpl.html
*)
signature  Wpp =
  sig
    type doc
    val empty   : doc
    val break   : int -> int -> doc
    val line    : doc
    val newline : doc  (* force a new line *)
   
    val group  : doc -> doc
    val nest   : int -> doc -> doc
    val text   : string -> doc
    val ^^     : doc * doc -> doc

    (* derived functions *)
    val concat : doc list -> doc 
    val seq    : doc -> ('a -> doc) -> 'a list -> doc

    (* Simple converters*)
    val fromConv : ('a -> string) -> 'a -> doc
    val int      : int   -> doc          (* an integer *)
    val char     : char  -> doc          (* an ML character *)
    val word     : word  -> doc          (* an ML word constant *)
    val word8    : word8 -> doc          (* an ML word constant *)
    val real     : real  -> doc          (* an ML real constant *)
    val bool     : bool  -> doc          (* a boolean *)


    val toString    : int -> doc -> string 
    val toOutStream : int -> TextIO.outstream -> doc -> unit
    val toFile      : int -> string -> doc -> unit
    val toConsumer  : int -> ('a -> string -> 'a) -> 'a -> doc -> 'a
  end

