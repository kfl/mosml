(* Unix -- SML Basis Library *)

(* sestoft@dina.kvl.dk 1999-11-07 version 0.2 *)

(* Type safety depends on the representation of TextIO.instream and
   TextIO.outstream, and of Process.status
 *)

structure Unix :> Unix =
struct

type signal = Signal.signal

type proc = { syspid : int, ins : TextIO.instream, outs : TextIO.outstream }

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

(* Caml Light "channels" *)

prim_type in_channel and out_channel;
prim_val open_descriptor_in  : int -> in_channel  = 1 "open_descriptor";
prim_val open_descriptor_out : int -> out_channel = 1 "open_descriptor";

fun raiseIo fcn nam exn = 
    raise Io {function = "Unix." ^ fcn, name = nam, cause = exn};

(* From Caml Light "channels" to SML instreams and outstreams: *)

fun openInPipe fcn infd : TextIO.instream =
    Obj.magic (ref {closed=false, 
		    ic=open_descriptor_in infd, 
		    name = "<inpipe>"})
    handle exn as SysErr _ => raiseIo fcn "<inpipe>" exn;

fun openOutPipe fcn outfd : TextIO.outstream =
    Obj.magic (ref {closed=false, 
		    oc=open_descriptor_out outfd, 
		    name="<outpipe>"})
    handle exn as SysErr _ => raiseIo fcn "<outpipe>" exn;

in 

val kill_ : int -> int -> unit = app2 "kill"

fun killpid (s : signal) (syspid : int) : unit = 
    kill_ syspid (Word.toInt (Signal.toWord s))

fun kill ({ syspid, ... } : proc, s : signal) : unit = 
    killpid s syspid
    handle Fail s => raise Fail ("Unix.kill: " ^ s)

val execute_ : string -> string vector -> string vector option
               -> int * int * int = app3 "execute"

fun executewrap fcn cmd args envOpt : proc =
    let val argvec = Vector.fromList (cmd :: args)
	val envvec = Option.map Vector.fromList envOpt
	val (syspid, infd, outfd) = execute_ cmd argvec envvec
	val ins  = openInPipe fcn infd
	val outs = openOutPipe fcn outfd
    in
	{ syspid = syspid, ins = ins, outs = outs } 
    end
    handle Fail s => raise Fail ("Unix." ^ fcn ^ ": " ^ s)

fun executeInEnv (cmd, args, env) : proc = 
    executewrap "executeInEnv" cmd args (SOME env)

fun execute (cmd, args) : proc =
    executewrap "execute" cmd args NONE

fun streamsOf ({ syspid, ins, outs } : proc) 
    : TextIO.instream * TextIO.outstream = (ins, outs)

val waitpid_ : int -> int = app1 "waitpid"

fun reap ({ syspid, ins, outs } : proc) : Process.status = 
    let val status_ = waitpid_ syspid : int
    in 
	TextIO.closeIn ins; 
	TextIO.closeOut outs; 
	Obj.magic status_ 
    end
    handle Fail s => raise Fail ("Unix.reap: " ^ s)
end
end
