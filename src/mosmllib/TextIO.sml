(* TextIO -- 1995-11-22, 1996-07-07, 2000-03-15; no positions etc. yet *)

type elem = Char.char
type vector = string
type pos = int

prim_val create_string_ : int -> string                 = 1 "create_string";
prim_val nth_char_      : string -> int -> char         = 2 "get_nth_char";
prim_val set_nth_char_  : string -> int -> char -> unit = 3 "set_nth_char";
prim_val blit_string_   : string -> int -> string -> int -> int -> unit 
                                                        = 5 "blit_string";

fun sub_string_ s start len =
  let val res = create_string_ len
  in blit_string_ s start res 0 len; res end
;

(* Caml Light "channels" *)

prim_type in_channel and out_channel;

prim_val open_descriptor_in : int -> in_channel = 1 "open_descriptor";
        (* [open_descriptor_in fd] returns a buffered input channel
           reading from the file descriptor [fd]. The file descriptor [fd]
           must have been previously opened for reading, else the behavior is
	   undefined. *)

prim_val open_descriptor_out : int -> out_channel = 1 "open_descriptor";
        (* [open_descriptor_out fd] returns a buffered output channel
           writing to the file descriptor [fd]. The file descriptor [fd]
           must have been previously opened for writing, else the behavior is
	   undefined. *)

prim_val input_char_ : in_channel -> char = 1 "input_char"
        (* Read one character from the given input channel.
           Raise [Size] if there are no more characters to read. *)

prim_val output_char_ : out_channel -> char -> unit = 2 "output_char";

prim_val caml_seek_in : in_channel -> int -> unit = 2 "seek_in"
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. *)

prim_val caml_pos_in : in_channel -> int = 1 "pos_in";
        (* Return the current reading position for the given channel. *)

prim_val caml_close_in : in_channel -> unit = 1 "close_in"
        (* Close the given channel. Anything can happen if any of the
           above functions is called on a closed channel. *)

type file_perm = int;

datatype open_flag =
    O_APPEND                       (* `open' for appending *)
  | O_BINARY                       (* `open' in binary mode *)    
  | O_CREAT                        (* create the file if nonexistent *)
  | O_EXCL                         (* fails if the file exists *)
  | O_RDONLY                       (* `open' read-only *)
  | O_RDWR                         (* `open' for reading and writing *)
  | O_TEXT                         (* `open' in text mode *)
  | O_TRUNC                        (* truncate the file to 0 if it exists *)
  | O_WRONLY                       (* `open' write-only *)
;

prim_val sys_open :
  string -> open_flag list -> file_perm -> int = 3 "sys_open"
        (* Open a file. The second argument is the opening mode.
           The third argument is the permissions to use if the file
           must be created. The result is a file descriptor opened on the
           file. *)
prim_val sys_close :
  int -> unit = 1 "sys_close"
        (* Close a file descriptor. *)

val caml_std_in  = open_descriptor_in 0
and caml_std_out = open_descriptor_out 1
and caml_std_err = open_descriptor_out 2
;

prim_val fast_input :
  in_channel -> string -> int -> int -> int = 4 "input";
prim_val fast_input_nonblocking :
  in_channel -> string -> int -> int -> int option = 4 "input_nonblocking";
prim_val fast_output :
  out_channel -> string -> int -> int -> unit = 4 "output";

fun caml_open_in_gen mode rights filename =
  open_descriptor_in (sys_open filename mode rights)
;

val caml_open_in = caml_open_in_gen [O_RDONLY, O_TEXT] 0
and caml_open_in_bin = caml_open_in_gen [O_RDONLY, O_BINARY] 0
;

fun open_out_gen mode rights filename =
  open_descriptor_out(sys_open filename mode rights)
;

prim_val s_irall : file_perm = 0 "s_irall";
prim_val s_iwall : file_perm = 0 "s_iwall";

val caml_open_out =
  open_out_gen [O_WRONLY, O_TRUNC, O_CREAT, O_TEXT] (s_irall + s_iwall)
and caml_open_out_bin =
  open_out_gen [O_WRONLY, O_TRUNC, O_CREAT, O_BINARY] (s_irall + s_iwall)
;

prim_val caml_flush : out_channel -> unit = 1 "flush"
        (* Flush the buffer associated with the given output channel,
           performing all pending writes on that channel.
           Interactive programs must be careful about flushing [std_out]
           at the right times. *)

fun caml_output_string channel s =
    fast_output channel s 0 (size s);

prim_val caml_close_out : out_channel -> unit = 1 "close_out"
        (* Close the given channel, flushing all buffered write operations.
	   The behavior is unspecified if any of the above functions is
	   called on a closed channel. *)

fun try_input_char_ ic =
  SOME (input_char_ ic)
  handle Size => NONE;

(* Moscow ML imperative Text I/O *)

type instream  = { closed: bool, ic: in_channel,  name : string} ref;
type outstream = { closed: bool, oc: out_channel, name : string} ref;

val stdIn  : instream  = 
    ref { closed=false, ic=caml_std_in,  name = "<stdIn>"  }
and stdOut : outstream = 
    ref { closed=false, oc=caml_std_out, name = "<stdOut>" }
and stdErr : outstream = 
    ref { closed=false, oc=caml_std_err, name = "<stdErr>" };

fun raiseIo fcn nam exn = 
    raise Io {function = fcn, name = nam, cause = exn};

fun raiseClosed fcn nam = 
    raiseIo fcn nam (Fail "Stream is closed");

fun openIn s =
  ref {closed=false, ic=caml_open_in s, name = s}
  handle exn as SysErr _ => raiseIo "openIn" s exn;

fun closeIn (is as ref {closed, ic, name}) =
  if closed then () 
  else (caml_close_in ic;
	is := { closed=true, ic=ic, name=name });

fun input1 (is as ref {closed, ic, name}) =
    if closed then NONE 
    else 
	SOME (input_char_ ic)
	handle Size => NONE;

fun input (is as ref {closed, ic, name}) =
    if closed then "" 
    else let val buff = create_string_ 60 
	 in case fast_input ic buff 0 60 of
	     0 => ""
	   | m => sub_string_ buff 0 m
	 end;

fun inputNoBlock (is as ref {closed, ic, name}) =
    if closed then SOME "" 
    else let val buff = create_string_ 60 
	 in case fast_input_nonblocking ic buff 0 60 of
	     NONE   => NONE 
	   | SOME 0 => SOME ""
	   | SOME m => SOME (sub_string_ buff 0 m)
	 end;

fun inputN (is as ref {closed, ic, name}, n) =
    if n < 0 orelse n > String.maxSize then raise Size
    else if closed then "" 
    else let val buff = create_string_ n
	     fun loop k =
		 if k = n then buff
		 else
		     case fast_input ic buff k (n-k) of
			 0 => sub_string_ buff 0 k
		       | m => loop (k+m)
	 in loop 0 end;

fun inputAll (is as ref {closed, ic, name}) =
  if closed then "" else
  let val max = ref 127
      val tmp = ref (create_string_ (!max))
      fun realloc () =
	  let val newmax = if !max = String.maxSize then raise Size
			   else if 2 * !max >= String.maxSize then String.maxSize
			   else 2 * !max
	      val newtmp = create_string_ newmax
	  in 
	      blit_string_ (!tmp) 0 newtmp 0 (!max);
	      max := newmax;
	      tmp := newtmp
	  end
      (* Don't read more than runtime/io.IO_BUFFER_SIZE characters:  *)
      fun chunksize sz = if sz > 4096 then 4096 else sz
      fun h len =
	  case fast_input ic (!tmp) len (chunksize (!max - len)) of
	      0 => sub_string_ (!tmp) 0 len
	    | m => (if !max - (len + m) < 127 then realloc () else ();
		    h (len + m));
  in h 0 end;

type cs = int (* character source state *)

fun scanStream scan (instrm as ref {closed, ic, name}) =
    let prim_eqtype array_
	prim_val array_  : int -> array_                 = 1 "create_string";
	prim_val sub_    : array_ -> int -> char         = 2 "get_nth_char";
	prim_val update_ : array_ -> int -> char -> unit = 3 "set_nth_char";

        val buf  = array_ 512		(* characters recently read     *)
	val read = ref 0		(* number of characters read    *)
	fun getc charno =
	    if charno < !read then		(* already read         *)
		if charno >= !read - 512 then	(* still in buffer      *)
		    SOME(sub_ buf (charno mod 512), charno+1)
		else				(* no longer in buffer  *)
		    raise Fail "scanStream: backtracking too far"
	    else	        (* charno = !read; read a new character *)
		if closed then NONE
		else (let val c = input_char_ ic
		      in (update_ buf (charno mod 512) c;
			  read := !read + 1;
			  SOME(c, charno+1))
		      end handle Size => NONE)
    in case scan getc 0 of
        NONE         => NONE
      | SOME(res, _) => SOME res
    end;

(* fun inputNoBlock (is : instream) : vector option =
    raise Fail "not implemented";
*)

fun lookahead (is as ref {closed, ic, name}) =
  if closed then NONE else
  let val pos = caml_pos_in ic in
      case try_input_char_ ic of
	  NONE            => NONE
	| res as (SOME c) => (caml_seek_in ic pos; res)
  end;

fun endOfStream is = (lookahead is = NONE);

fun openAppend (s : string) : outstream =
  ref { closed=false,
        oc=open_out_gen [O_WRONLY, O_APPEND, O_CREAT, O_TEXT]
                        (s_irall + s_iwall) s,
	name=s}
  handle exn as SysErr _ => raiseIo "openAppend" s exn;

fun output (os as ref {closed, oc, name}, s) =
    if closed then
	raiseClosed "output" name
    else
	(caml_output_string oc s;
	 if os = stdErr then caml_flush oc else ());

fun outputSubstr (os as ref {closed, oc, name}, sus : substring) =
    let prim_val substrToTrip : substring -> string * int * int = 1 "identity"
        val (s, i, n) = substrToTrip sus
    in if closed then
	  raiseClosed "outputSubstr" name
       else
	   (fast_output oc s i n;
	    if os = stdErr then caml_flush oc else ())
    end;

fun output1 (os as ref {closed, oc, name}, c) =
    if closed then
	raiseClosed "output1" name
    else
	(output_char_ oc c;
	 if os = stdErr then caml_flush oc else ());

fun inputLine (is as ref {closed, ic, name}) =
  if closed then "" else
  let val max = ref 127
      val tmp = ref (create_string_ (!max))
      fun realloc () =
	  let val newmax = if !max = String.maxSize then raise Size
			   else if 2 * !max >= String.maxSize then String.maxSize
			   else 2 * !max
	      val newtmp = create_string_ newmax
	  in 
	      blit_string_ (!tmp) 0 newtmp 0 (!max);
	      max := newmax;
	      tmp := newtmp
	  end
      fun h len =
	  (if len >= !max then realloc () else ();
	   case try_input_char_ ic of
	       NONE   => (set_nth_char_ (!tmp) len #"\n"; 
			  sub_string_ (!tmp) 0 (len+1))
	     | SOME c => (set_nth_char_ (!tmp) len c;
			  if c = #"\n" then sub_string_ (!tmp) 0 (len+1) 
			               else h (len+1)))
  in if endOfStream is then "" else h 0 end;

fun openOut (s : string) : outstream =
    ref {closed=false, oc=caml_open_out s, name=s}
    handle exn as SysErr _ => raiseIo "openOut" s exn;

fun closeOut (os as ref {closed, oc, name}) =
    if closed then () 
    else (caml_close_out oc; 
	  os := {closed = true, oc=oc, name=name}; 
	  ());

fun flushOut (os as ref {closed, oc, name}) =
  if closed then
      raiseClosed "flushOut" name
  else
      caml_flush oc;

fun print s = (output(stdOut, s); flushOut stdOut);
