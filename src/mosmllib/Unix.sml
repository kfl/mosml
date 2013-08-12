(* Unix -- SML Basis Library *)

(* sestoft@dina.kvl.dk 1999-11-07 version 0.2 *)

(* ken@friislarsen.net 2008-05-03 version 0.3 *)

(* Type safety depends on the representation of TextIO.instream and
   TextIO.outstream, BinIO.instream and BinIO.outstream and of
   Process.status
 *)

structure Unix :> Unix =
struct

type signal = Signal.signal

(* Caml Light "channels" *)

prim_type in_channel and out_channel;
prim_val open_descriptor_in  : int -> in_channel  = 1 "open_descriptor";
prim_val open_descriptor_out : int -> out_channel = 1 "open_descriptor";

(* In Moscow ML TextIO and BinIO streams are represented the same way *)
type instream = { closed: bool, ic: in_channel, name : string } ref;
type outstream = { closed: bool, oc: out_channel, name : string } ref;

type ('a, 'b) proc = { syspid : int, ins : instream, outs : outstream }

local 
    open Dynlib
    val hdl  = dlopen {lib = "libmunix.so",
		       flag = RTLD_LAZY, global = false}
    val symb = Dynlib.dlsym hdl
    fun app1 name = Dynlib.app1 (symb ("unix_"^name))
    fun app2 name = Dynlib.app2 (symb ("unix_"^name))
    fun app3 name = Dynlib.app3 (symb ("unix_"^name))
    fun app4 name = Dynlib.app4 (symb ("unix_"^name))
    fun app5 name = Dynlib.app5 (symb ("unix_"^name))


fun raiseIo fcn nam exn = 
    raise Io {function = "Unix." ^ fcn, name = nam, cause = exn};

(* From Caml Light "channels" to SML instreams and outstreams: *)

fun openInPipe fcn infd =
    ref {closed=false, 
	 ic=open_descriptor_in infd, 
	 name = "<inpipe>"}
    handle exn as SysErr _ => raiseIo fcn "<inpipe>" exn;

fun openOutPipe fcn outfd =
    ref {closed=false, 
	 oc=open_descriptor_out outfd, 
	 name="<outpipe>"}
    handle exn as SysErr _ => raiseIo fcn "<outpipe>" exn;

in 

val kill_ : int -> int -> unit = app2 "kill"

fun killpid (s : signal) (syspid : int) : unit = 
    kill_ syspid (Word.toInt (Signal.toWord s))

fun kill ({ syspid, ... } : ('a,'b) proc, s : signal) : unit = 
    killpid s syspid
    handle Fail s => raise Fail ("Unix.kill: " ^ s)

val execute_ : string -> string vector -> string vector option
               -> int * int * int = app3 "execute"

fun executewrap fcn cmd args envOpt : ('a, 'b) proc =
    let val argvec = Vector.fromList (cmd :: args)
	val envvec = Option.map Vector.fromList envOpt
	val (syspid, infd, outfd) = execute_ cmd argvec envvec
	val ins  = openInPipe fcn infd
	val outs = openOutPipe fcn outfd
    in
	{ syspid = syspid, ins = ins, outs = outs } 
    end
    handle Fail s => raise Fail ("Unix." ^ fcn ^ ": " ^ s)

fun executeInEnv (cmd, args, env) : ('a, 'b) proc = 
    executewrap "executeInEnv" cmd args (SOME env)

fun execute (cmd, args) : ('a, 'b) proc =
    executewrap "execute" cmd args NONE

fun streamsOf ({syspid, ins, outs} : (TextIO.instream, TextIO.outstream) proc) 
    : TextIO.instream * TextIO.outstream = Obj.magic (ins, outs)

fun textInstreamOf ({syspid, ins, outs} : (TextIO.instream, 'a) proc) 
    : TextIO.instream = Obj.magic ins
fun binInstreamOf ({syspid, ins, outs} : (BinIO.instream, 'a) proc)
    : BinIO.instream =  Obj.magic ins
fun textOutstreamOf ({syspid, ins, outs} : ('a, TextIO.outstream) proc)
    : TextIO.outstream = Obj.magic outs
fun binOutstreamOf ({syspid, ins, outs} : ('a, BinIO.outstream) proc)
    : BinIO.outstream = Obj.magic outs


val waitpid_ : int -> int = app1 "waitpid"

fun reap ({syspid, ins, outs} : ('a,'b) proc) : OS.Process.status = 
    let val status_ = waitpid_ syspid : int
    in 
	TextIO.closeIn (Obj.magic ins); 
	TextIO.closeOut (Obj.magic outs); 
	Obj.magic status_ 
    end
    handle Fail s => raise Fail ("Unix.reap: " ^ s)
end
end
