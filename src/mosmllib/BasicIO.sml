(* BasicIO.sml *)

prim_val create_string_ : int -> string = 1 "create_string";
prim_val nth_char_ : string -> int -> char = 2 "get_nth_char";
prim_val set_nth_char_ : string -> int -> char -> unit = 3 "set_nth_char";
prim_val blit_string_ :
  string -> int -> string -> int -> int -> unit = 5 "blit_string";

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

(* Moscow ML streams *)

type instream  = { closed: bool, ic: in_channel } ref;
type outstream = { closed: bool, oc: out_channel } ref;

val std_in  : instream  = ref { closed=false, ic=caml_std_in }
and std_out : outstream = ref { closed=false, oc=caml_std_out }
and std_err : outstream = ref { closed=false, oc=caml_std_err }
;

prim_val fast_input :
  in_channel -> string -> int -> int -> int = 4 "input";
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
  fast_output channel s 0 (size s)
;

prim_val caml_close_out : out_channel -> unit = 1 "close_out"
        (* Close the given channel, flushing all buffered write operations.
	   The behavior is unspecified if any of the above functions is
	   called on a closed channel. *)

fun raiseIo fcn nam exn = 
    raise SysErr ("BasicIO." ^ fcn ^ " on " ^ nam, NONE);

fun raiseClosed fcn nam = 
    raiseIo fcn nam (Fail "Stream is closed");

fun open_in s =
  ref {closed=false, ic=caml_open_in s}
  handle exn as SysErr _ => raiseIo "open_in" s exn;

fun open_in_bin s =
  ref {closed=false, ic=caml_open_in_bin s}
  handle exn as SysErr _ => raiseIo "open_in_bin" s exn;

fun try_input_char_ ic =
  SOME (input_char_ ic)
  handle Size => NONE;

fun inputc (is as ref {closed, ic}) n =
  if closed orelse n<=0 then "" else
  let
    val buff = create_string_ n
    fun loop k =
      if k = n then buff
      else
        case fast_input ic buff k (n-k) of
            0 => sub_string_ buff 0 k
          | m => loop (k+m)
  in loop 0 end;

fun input (is, n) = inputc is n;

fun lookahead (is as ref {closed, ic}) =
  if closed then "" else
  let val pos = caml_pos_in ic in
    case try_input_char_ ic of
        NONE   => ""
      | SOME c =>
          let val () = caml_seek_in ic pos
              val s = create_string_ 1
          in set_nth_char_ s 0 c; s end
  end;

fun close_in (is as ref {closed, ic}) =
  if closed then () else
    (caml_close_in ic;
     is := { closed=true, ic=ic };
     ());

fun end_of_stream is = (lookahead is = "");

fun open_out s =
  ref {closed=false, oc=caml_open_out s}
  handle exn as SysErr _ => raiseIo "open_out" s exn;

fun open_out_bin s =
  ref {closed=false, oc=caml_open_out_bin s}
  handle exn as SysErr _ => raiseIo "open_out_bin" s exn;

fun outputc (os as ref {closed, oc}) s =
  if closed then
    raiseClosed "outputc" "" 
  else
    (caml_output_string oc s;
     if os = std_err then caml_flush oc else ());

fun output (os, s) = outputc os s;

fun close_out (os as ref {closed, oc}) =
  if closed then () else
    (caml_close_out oc; os := {closed = true, oc=oc}; ());

fun flush_out (os as ref {closed, oc}) =
  if closed then
    raiseClosed "flush_out" ""
  else
    caml_flush oc;

fun input_line (is as ref {closed, ic}) =
  if closed then "" else
  let val max = ref 127
      val tmp = ref (create_string_ (!max))
      fun realloc () =
	  let val newmax = 2 * !max
	      val newtmp = create_string_ newmax
	  in 
	      blit_string_ (!tmp) 0 newtmp 0 (!max);
	      max := newmax;
	      tmp := newtmp
	  end
      fun h len =
	  case try_input_char_ ic of
	      NONE   => sub_string_ (!tmp) 0 len
	    | SOME c => (if len >= !max then realloc () else ();
			 set_nth_char_ (!tmp) len c;
			 if c = #"\n" then sub_string_ (!tmp) 0 (len+1) 
			              else h (len+1))
  in h 0 end;

fun can_inputc (is as ref {closed, ic}) n =
  if n<0 then false else
  if closed then n=0 else
  let
    val pos = caml_pos_in ic
    val buff = create_string_ n
    val n' = fast_input ic buff 0 n
  in caml_seek_in ic pos; n' = n end;

fun can_input (is, n) = can_inputc is n;

fun open_append s =
  ref { closed=false,
        oc=open_out_gen [O_WRONLY, O_APPEND, O_CREAT, O_TEXT]
                        (s_irall + s_iwall) s }
  handle exn as SysErr _ => raiseIo "open_append" s exn;

prim_val sys_exit : int -> 'a = 1 "sys_exit";

fun exit n =
  (flush_out std_out; flush_out std_err; sys_exit n)
;

fun print s = (outputc std_out s; flush_out std_out);
