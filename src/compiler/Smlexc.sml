(* Predefined SML exceptions *)

local
    open Obj Const;
in

(* SML exceptions *)
(* cvr:
val exnTag = obj_tag(repr (let exception DUMMY in DUMMY end));
val exnTagName   = ({qual="General", id="(Exception)"}, 0);
val bindTagName  = ({qual="General", id="Bind"}, 0);
val matchTagName = ({qual="General", id="Match"}, 0);

val predefExceptions = [
  ("Bind",     (({qual="general", id="Bind"},      2), 0)),
  ("Chr",      (({qual="general", id="Chr"},       3), 0)),
  ("Div",      (({qual="general", id="Div"},       4), 0)),
  ("Domain",   (({qual="general", id="Domain"},    5), 0)),
  ("Match",    (({qual="general", id="Match"},     6), 0)),
  ("Ord",      (({qual="general", id="Ord"},       7), 0)),
  ("Overflow", (({qual="general", id="Overflow"},  8), 0)),
  ("Out_of_memory",
            (({qual="exc",     id="Out_of_memory"}, 1), 0)),
  ("Invalid_argument",
            (({qual="exc",     id="Invalid_argument"}, 2), 1)),
  ("Fail",  (({qual="exc",     id="Failure"},     3), 1)),
  ("Subscript",
            (({qual="exc",     id="Not_found"},   4), 0)),
  ("Size",  (({qual="io",      id="End_of_file"}, 1), 0)),
  ("SysErr",(({qual="sys",     id="Sys_error"},   1), 1)),
  ("Interrupt",
            (({qual="sys",    id="Break"},     2), 0)),
  ("Graphic_failure",
            (({qual="graphics", id="Graphic_failure"}, 1), 1))
];

local 
    open Config Mixture Types

    fun decode_string (v : obj) = (magic_obj v : string);
in
    val type_of_syserror_exn = (* Must match actual type of OS.SysErr *)
	type_pair type_string (type_option type_syserror);

    val type_of_io_exn =       (* Must match actual type of IO.Io     *)
	type_rigid_record 
	[(STRINGlab "cause",    type_exn), 
	 (STRINGlab "function", type_string),
	 (STRINGlab "name",     type_string)];

    fun exnArgType {qual="General", id="SysErr"} = SOME type_of_syserror_exn
      | exnArgType {qual="OS",      id="SysErr"} = SOME type_of_syserror_exn
      | exnArgType {qual="General", id="Io"}     = SOME type_of_io_exn
      | exnArgType _                             = NONE

end
end;
*)

val exnTag = obj_tag(repr (let exception DUMMY in DUMMY end));
val exnTagName   = ({qual="General", id=["(Exception)"]}, 0);
val bindTagName  = ({qual="General", id=["Bind"]}, 0);
val matchTagName = ({qual="General", id=["Match"]}, 0);

val predefExceptions = [
  ("Bind",     (({qual="general", id=["Bind"]},      2), 0)),
  ("Chr",      (({qual="general", id=["Chr"]},       3), 0)),
  ("Div",      (({qual="general", id=["Div"]},       4), 0)),
  ("Domain",   (({qual="general", id=["Domain"]},    5), 0)),
  ("Match",    (({qual="general", id=["Match"]},     6), 0)),
  ("Ord",      (({qual="general", id=["Ord"]},       7), 0)),
  ("Overflow", (({qual="general", id=["Overflow"]},  8), 0)),
  ("Out_of_memory",
            (({qual="exc",     id=["Out_of_memory"]}, 1), 0)),
  ("Invalid_argument",
            (({qual="exc",     id=["Invalid_argument"]}, 2), 1)),
  ("Fail",  (({qual="exc",     id=["Failure"]},     3), 1)),
  ("Subscript",
            (({qual="exc",     id=["Not_found"]},   4), 0)),
  ("Size",  (({qual="io",      id=["End_of_file"]}, 1), 0)),
  ("SysErr",(({qual="sys",     id=["Sys_error"]},   1), 1)),
  ("Interrupt",
            (({qual="sys",    id=["Break"]},     2), 0)),
  ("Graphic_failure",
            (({qual="graphics", id=["Graphic_failure"]}, 1), 1))
];

local 
    open Config Mixture Types

    fun decode_string (v : obj) = (magic_obj v : string);
in
    val type_of_syserror_exn = (* Must match actual type of OS.SysErr *)
	type_pair type_string (type_option type_syserror);

    val type_of_io_exn =       (* Must match actual type of IO.Io     *)
	type_rigid_record 
	[(STRINGlab "cause",    type_exn), 
	 (STRINGlab "function", type_string),
	 (STRINGlab "name",     type_string)];

    fun exnArgType {qual="General", id=["SysErr"]} = SOME type_of_syserror_exn
      | exnArgType {qual="OS",      id=["SysErr"]} = SOME type_of_syserror_exn
      | exnArgType {qual="General", id=["Io"]}     = SOME type_of_io_exn
      | exnArgType _                             = NONE

end
end;


