(* Nonstdio.sml *)

(*
  This unit extends BasicIO. Since Moscow ML doesn't provide
  legal means for creating "derived" units, we have to use some
  "magic", to get into abstract Basicio.instream and
  Basicio.outstream values.

  The same problem arises with CharArray.array values.
*)

open BasicIO;

(* Caml Light "channels" *)

(* We define in_channel and out_channel as in Basicio *)
(* for internal use only. *)

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

prim_val input_char_ : in_channel -> char = 1 "input_char";
        (* Read one character from the given input channel.
           Raise [Size] if there are no more characters to read. *)

prim_val input_binary_int_ : in_channel -> int = 1 "input_int";
        (* Read an integer encoded in binary format from the given input
           channel. See [output_binary_int].
           Raise [Size] if an end of file was reached while reading the
	   integer. *)

prim_val input_value_ : in_channel -> 'a = 1 "intern_val";
        (* Read the representation of a structured value, as produced
           by [output_value], and return the corresponding value. *)

prim_val seek_in_ : in_channel -> int -> unit = 2 "seek_in"
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. *)

prim_val pos_in_ : in_channel -> int = 1 "pos_in";
        (* Return the current reading position for the given channel. *)

prim_val in_channel_length_ : in_channel -> int = 1 "channel_size";
        (* Return the total length (number of characters) of the
           given channel. This works only for regular files. *)

prim_val fast_input :
  in_channel -> string -> int -> int -> int = 4 "input";

prim_val fast_output :
  out_channel -> string -> int -> int -> unit = 4 "output";

prim_val output_char_ : out_channel -> char -> unit = 2 "output_char"
        (* Write one character on the given output channel. *)

prim_val output_byte_ : out_channel -> int -> unit = 2 "output_char"
        (* Write one 8-bit integer (as the single character with that code)
           on the given output channel. The given integer is taken modulo
           256. *)

prim_val output_binary_int_ : out_channel -> int -> unit = 2 "output_int";
        (* Write one integer in binary format on the given output channel. *)

prim_val output_value_ : out_channel -> 'a -> unit = 2 "extern_val";
        (* Write the representation of a structured value of any type
           to a channel. *)

prim_val seek_out_ : out_channel -> int -> unit = 2 "seek_out"
        (* [seek_out chan pos] sets the current writing position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds (such as terminals, pipes and sockets,)
	   the behavior is unspecified. *)

prim_val pos_out_ : out_channel -> int = 1 "pos_out";
        (* Return the current writing position for the given channel. *)

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


(* Moscow ML streams *)

type buffer = string;

(* Since instream and outstream are declared in Basicio *)
(* as abstract types, we need a dirty trick to get access *)
(* to their representation.  :-< *)

type instream_  = { closed: bool, ic: in_channel } ref;
type outstream_ = { closed: bool, oc: out_channel } ref;

prim_val fromI : instream -> instream_   = 1 "identity";
prim_val fromO : outstream -> outstream_ = 1 "identity";
prim_val mkI   : instream_ -> instream   = 1 "identity";
prim_val mkO   : outstream_ -> outstream = 1 "identity";

(* The same trick to access the internals of CharArray.array. *)

prim_val fromCA : CharArray.array -> string ref = 1 "identity";

fun open_in_gen_ mode rights filename =
  open_descriptor_in (sys_open filename mode rights)
;

val open_in_bin_ = open_in_gen_ [O_RDONLY, O_BINARY] 0;

fun open_out_gen mode rights filename =
  open_descriptor_out(sys_open filename mode rights)
;

prim_val s_irall : file_perm = 0 "s_irall";
prim_val s_iwall : file_perm = 0 "s_iwall";
prim_val s_ixall : file_perm = 0 "s_ixall";

val open_out_bin_ =
  open_out_gen [O_WRONLY, O_TRUNC, O_CREAT, O_BINARY]
               (s_irall + s_iwall);

val open_out_exe_ =
  open_out_gen [O_WRONLY, O_TRUNC, O_CREAT, O_BINARY]
               (s_irall + s_iwall + s_ixall);

fun raiseIo fcn nam exn = 
    raise SysErr ("Nonstdio." ^ fcn ^ " on " ^ nam, NONE);

fun raiseClosed fcn nam = 
    raiseIo fcn nam (Fail "Stream is closed");

fun open_in_bin s =
  mkI (ref {closed=false, ic=open_in_bin_ s})
  handle exn as SysErr _ => raiseIo "open_in_bin" s exn;

fun fast_really_input is (buff : string) offs len =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "fast_really_input" ""
    else if len <= 0 then () else
      case fast_input ic buff offs len of
        0 => raise Fail "fast_really_input: unexpected end of file"
      | r => fast_really_input is buff (offs+r) (len-r)
  end;

fun buff_input is (buff : CharArray.array) offs len =
  let val ref {closed, ic} = fromI is in
    if closed then
      0
    else
      let val ref sbuff = fromCA buff in
        if len < 0 orelse offs < 0 orelse offs+len > size sbuff then
          raise Fail "buff_input"
        else
          fast_input ic sbuff offs len
      end
  end;

fun input_char is =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "input_char" ""
    else
      input_char_ ic
  end;

fun input_binary_int is =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "input_binary_int" ""
    else
      input_binary_int_ ic
  end;

fun input_value is =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "input_value" ""
    else
      input_value_ ic
  end;

fun seek_in is =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "seek_in" ""
    else
      seek_in_ ic
  end;

fun pos_in is =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "pos_in" ""
    else
      pos_in_ ic
  end;

fun in_stream_length is =
  let val ref {closed, ic} = fromI is in
    if closed then
      raiseClosed "in_stream_length" ""
    else
      in_channel_length_ ic
  end;

fun open_out_bin s =
  mkO(ref {closed=false, oc=open_out_bin_ s})
  handle exn as SysErr _ => raiseIo "open_out_bin" s exn;

fun open_out_exe s =
  mkO(ref {closed=false, oc=open_out_exe_ s})
  handle exn as SysErr _ => raiseIo "open_out_exe" s exn;

fun buff_output os (buff : CharArray.array) offs len =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "buff_output" ""
    else
      let val ref sbuff = fromCA buff in
        if len < 0 orelse offs < 0 orelse offs+len > size sbuff then
          raise Fail "buff_output"
        else
          fast_output oc sbuff offs len
      end
  end;

fun output_char os (c : char) =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "output_char" ""
    else
      output_char_ oc c
  end;

fun output_byte os (c : int) =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "output_byte" ""
    else
      output_byte_ oc c
  end;

fun output_binary_int os i =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "output_binary_int" ""
    else
      output_binary_int_ oc i
  end;

fun output_value os v =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "output_value" ""
    else
      output_value_ oc v
  end;

fun seek_out os pos =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "seek_out" ""
    else
      seek_out_ oc pos
  end;

fun pos_out os =
  let val ref {closed, oc} = fromO os in
    if closed then
      raiseClosed "pos_out" ""
    else
      pos_out_ oc
  end;

fun file_exists filename =
  (sys_close(sys_open filename [O_RDONLY] 0); true)
     handle SysErr _ => false;
