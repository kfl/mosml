(* Miscsys -- not part of the new unified basis, 12-27-94 *)

(* This module provides a simple interface to the operating system. *)

prim_val command_line : string Vector.vector = 0 "command_line";
prim_val interactive: bool = 0 "interactive";
prim_val getenv : string -> string = 1 "sys_getenv";

prim_val catch_interrupt : bool -> unit = 1 "sys_catch_break"

prim_val rename_ : string -> string -> unit = 2 "sys_rename";
prim_val remove  : string -> unit           = 1 "sys_remove";
prim_val chdir   : string -> unit           = 1 "sys_chdir";

fun rename {old, new} = rename_ old new;
