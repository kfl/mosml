(* BinIO -- SML Basis Library *)

type elem   = Word8.word
type vector = Word8Vector.vector

(* The only way BinIO.instream and BinIO.outstream differ from
 * TextIO.instream and TextIO.outstream is in the way they were
 * opened.  Hence we call on the TextIO functions to implement most of
 * the BinIO functions too (except openIn, openOut, openAppend, of
 * course.  Some `conversion' functions:
 *)

prim_val fromString : string -> vector = 1 "identity"
prim_val toString   : vector -> string = 1 "identity"

prim_val fromChar   : char -> elem = 1 "identity"
prim_val toChar     : elem -> char = 1 "identity"

prim_type in_channel and out_channel;

(* Opening and closing files in binary mode: *)

local 
    datatype open_flag =
	O_APPEND                       (* `open' for appending *)
      | O_BINARY                       (* `open' in binary mode *)    
      | O_CREAT                        (* create the file if nonexistent *)
      | O_EXCL                         (* fails if the file exists *)
      | O_RDONLY                       (* `open' read-only *)
      | O_RDWR                         (* `open' for reading and writing *)
      | O_TEXT                         (* `open' in text mode *)
      | O_TRUNC                        (* truncate the file to 0 if exists *)
      | O_WRONLY                       (* `open' write-only *)

    type file_perm = int;	
    prim_val sys_open : string -> open_flag list -> file_perm -> int 
						= 3 "sys_open"

    prim_val open_descriptor_in  : int -> in_channel  = 1 "open_descriptor";
    prim_val open_descriptor_out : int -> out_channel = 1 "open_descriptor";

    fun caml_open_in_gen  mode rights filename =
	open_descriptor_in  (sys_open filename mode rights);
    fun caml_open_out_gen mode rights filename =
	open_descriptor_out (sys_open filename mode rights);

    prim_val s_irall : file_perm = 0 "s_irall";
    prim_val s_iwall : file_perm = 0 "s_iwall";
in
    val caml_open_in_bin = 
	caml_open_in_gen [O_RDONLY, O_BINARY] 0;
	
    val caml_open_out_bin =
	caml_open_out_gen [O_WRONLY, O_TRUNC,  O_CREAT, O_BINARY] 
	                  (s_irall + s_iwall);

    val caml_open_out_bin_append =
	caml_open_out_gen [O_WRONLY, O_APPEND, O_CREAT, O_BINARY]
	                  (s_irall + s_iwall)
end

(* Binary input: *)

type instream = { closed: bool, ic: in_channel, name : string } ref;

prim_val inToText : instream -> TextIO.instream = 1 "identity"

fun raiseIo fcn nam exn = 
    raise Io {function = fcn, name = nam, cause = exn};

fun openIn (s : string) : instream =
    ref {closed=false, ic=caml_open_in_bin s, name=s}
    handle exn as SysErr _ => raiseIo "openIn" s exn;

fun closeIn (is : instream) : unit = 
    TextIO.closeIn (inToText is)

fun input (is : instream) : vector = 
    fromString (TextIO.input (inToText is))

fun inputAll (is : instream) : vector =
    fromString (TextIO.inputAll (inToText is))

fun inputNoBlock (is : instream) : vector option =
    raise Fail "not implemented";

fun input1 (is : instream) : elem option =
    case TextIO.input1 (inToText is) of
	NONE   => NONE
      | SOME c => SOME (fromChar c);

fun inputN (is : instream, n : int) : vector =
    fromString (TextIO.inputN (inToText is, n));

fun endOfStream (is : instream) : bool =
    TextIO.endOfStream (inToText is);

fun lookahead (is : instream) : elem option =
    case TextIO.lookahead (inToText is) of
	NONE   => NONE
      | SOME c => SOME (fromChar c);
    

(* Binary output: *)

type outstream = { closed: bool, oc: out_channel, name : string } ref;

prim_val outToText : outstream -> TextIO.outstream = 1 "identity"

fun openOut (s : string) : outstream =
    ref {closed=false, oc=caml_open_out_bin s, name=s}
    handle exn as SysErr _ => raiseIo "openOut" s exn;

fun openAppend s : outstream =
    ref { closed=false, oc=caml_open_out_bin_append s, name=s }
    handle exn as SysErr _ => raiseIo "openAppend" s exn;

fun closeOut (os : outstream) : unit =
    TextIO.closeOut (outToText os);

fun output(os : outstream, vec : vector) : unit =
    TextIO.output(outToText os, toString vec);

fun output1(os : outstream, w : elem) : unit =
    TextIO.output1(outToText os, toChar w);

fun flushOut(os : outstream) : unit =
    TextIO.flushOut(outToText os);
