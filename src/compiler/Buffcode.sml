(* To buffer bytecode during emission *)

local
  open Obj Fnlib Mixture Config Opcodes;

  prim_val andb_      : int -> int -> int = 2 "and";
  prim_val rshiftsig_ : int -> int -> int = 2 "shift_right_signed";
  prim_val rshiftuns_ : int -> int -> int = 2 "shift_right_unsigned";

  fun make_buffer n = CharArray.array(n, #"\000");

in

val out_buffer = ref (make_buffer 64);
val out_position = ref 0;

fun realloc_out_buffer () =
  let val len = CharArray.length (!out_buffer)
      val new_buffer = make_buffer (2 * len)
  in
    CharArray.copy { src = !out_buffer, si = 0, len = NONE,
                     dst = new_buffer, di = 0 };
    out_buffer := new_buffer
  end;

fun init_out_code () = (out_position := 0);

fun out (b : int) =
(
  if !out_position < CharArray.length (!out_buffer) then () else
    realloc_out_buffer();
  CharArray.update(!out_buffer, !out_position, Char.chr(andb_ b 255));
  incr out_position
);

fun out_short s =
  (out s; out (rshiftuns_ s 8))
;

fun out_long l =
(
  out l;
  out (rshiftuns_ l 8);
  out (rshiftuns_ l 16);
  out (rshiftsig_ l 24)
);

end;
