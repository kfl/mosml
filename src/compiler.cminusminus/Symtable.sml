(* Symtable.sml : to assign numbers to global variables and so on *)

local
  open Misc Nonstdio Obj Fnlib Config Mixture Const;
in

(* Hashtables for numbering objects *)

type 'a numtable =
{
  num_cnt: int ref,              (* The current number *)
  num_tbl: ('a, int) Hasht.t     (* The table *)
};

fun new_numtable size =
  { num_cnt = ref 0, num_tbl = Hasht.new size }
;

fun find_in_numtable (nt : ''a numtable) =
  Hasht.find (#num_tbl nt)
;

fun enter_in_numtable (nt : ''_a numtable) key =
  let val c = !(#num_cnt nt) in
    #num_cnt nt := !(#num_cnt nt) + 1;
    Hasht.insert (#num_tbl nt) key c;
    c
  end;

fun remove_from_numtable (nt : ''a numtable) key =
  Hasht.remove (#num_tbl nt) key
;

(* Global variables *)

val global_table =
  ref (new_numtable 1 : (QualifiedIdent * int) numtable)

and literal_table =
  ref ([] : (int * StructConstant) list)
;

fun get_slot_for_variable (uid as (qualid, stamp)) =
  find_in_numtable (!global_table) uid
  handle Subscript =>
    (let val {qual,id} = qualid
	 val mid = longIdentAsIdent id "get_slot_for_variable"
	 val (desc,s) = 
	     case unmangle mid of
		 ValId s => 
		     ("Value ",s)
	       | ModId s =>
		     ("Structure ",s)
	       | FunId s =>
		     ("Functor ",s)
     in
	 msgIBlock 0;
	 msgEOL();
	 errPrompt desc;
	 msgString qual; msgString "."; msgString s;
	 if stamp <> 0 then (msgString "/"; msgInt stamp) else ();
	 msgString " hasn't been defined yet"; msgEOL();
	 msgEBlock();
	 raise Toplevel
     end)
;

fun get_slot_for_defined_variable (uid as (qualid, stamp)) =
  enter_in_numtable (!global_table) uid
;

fun get_slot_for_literal cst =
  let val c = !(#num_cnt (!global_table)) in
    #num_cnt(!global_table) := !(#num_cnt (!global_table)) + 1;
    literal_table := (c, cst) :: !literal_table;
    c
  end;

fun number_of_globals () =
  !(#num_cnt (!global_table))
;

fun defineGlobalValueAlias uid uid' =
  let val slot = get_slot_for_variable uid' in
    Hasht.insert (#num_tbl (!global_table)) uid slot
  end;

(* The exception tags *)

(* ps: val unknown_exn_name = ({qual="?", id=["?"]}, 0);
val exn_tag_table = ref(new_numtable 1 : (QualifiedIdent * int) numtable);
val tag_exn_table = ref(Array.fromList [] : (QualifiedIdent * int) Array.array );

*)

(* cvr: 
fun fromShortTagExnTable a = Array.tabulate 
     ((Array.length a),
      (fn i => case Array.sub (a, i) of
               ({qual=qual,id = id},i) => ({qual=qual,id = [id]},i)));

fun toShortTagExnTable a = Array.tabulate 
     ((Array.length a),
      (fn i => case Array.sub (a, i) of
               ({qual=qual,id = [id]},i) => ({qual=qual,id = id},i)));

fun get_num_of_exn (name, stamp) =
  Hasht.find (#num_tbl (!exn_tag_table)) (name, stamp)
  handle Subscript =>
    let val c = enter_in_numtable (!exn_tag_table) (name, stamp)
        val len = Array.length (!tag_exn_table)
    in
      if c < len then () else
        let val new_len = 2 * len
            val new_tag_exn_table = Array.array(new_len, unknown_exn_name)
        in
          Array.copy {src = !tag_exn_table, si = 0, len = NONE,
                      dst = new_tag_exn_table, di = 0};
          tag_exn_table := new_tag_exn_table
        end;
      Array.update(!tag_exn_table, c, (name, stamp));
      c
    end;

fun get_exn_of_num tag =
  if tag >= Array.length (!tag_exn_table)
  then unknown_exn_name
  else Array.sub(!tag_exn_table, tag)
;

fun defineGlobalExceptionAlias (q, (q', stamp')) =
  let val tag = get_num_of_exn (q', stamp') in
    Hasht.insert (#num_tbl (!exn_tag_table)) (q, 0) tag
  end;
*)

fun intOfTag (CONtag(n,_)) = n
(*  | intOfTag (EXNtag(id, stamp)) = fatalError "intOfTag" (* ps: get_num_of_exn(id, stamp) *) *)
;

(* The C primitives *)

val c_prim_table = ref (new_numtable 0 : string numtable);

(* cvr: 144 merge 
fun set_c_primitives prim_vect =
  (c_prim_table := new_numtable 31;
   for (fn i => ignore
          (enter_in_numtable (!c_prim_table) (Vector.sub(prim_vect, i))))
       0 (Vector.length prim_vect - 1))
;
*)
fun set_c_primitives prim_vect =
  (c_prim_table := new_numtable 67;
   Vector.app (ignore o enter_in_numtable (!c_prim_table)) prim_vect)
;

fun get_num_of_prim name =
  find_in_numtable (!c_prim_table) name
  handle Subscript =>
    (msgIBlock 0;
     errPrompt "Unavailable C primitive: ";
     msgString name; msgEOL();
     msgEBlock();
     raise Toplevel)
;

fun exportPublicNames uname excRenList valRenList =
  ((* ps: List.app defineGlobalExceptionAlias excRenList; *)
   List.app
     (fn (id, stamp) =>
         defineGlobalValueAlias
           ({ qual=uname, id=[id] }, 0)
           ({ qual=uname, id=[id] }, stamp))
     valRenList)
;

(* Initialization *)

(* ps:

val normalizeExnName = fn
    {qual="sys", id=["Break"]}     => {qual="General", id=["Interrupt"]}
  | {qual="sys", id=["Sys_error"]} => {qual="General", id=["SysErr"]}
  | {qual="exc", id=["Not_found"]} => {qual="General", id=["Subscript"]}
  | {qual="io",  id=["End_of_file"]} => {qual="General", id=["Size"]}
  | {qual="exc", id=["Out_of_memory"]} => {qual="General", id=["Out_of_memory"]}
  | {qual="exc", id=["Invalid_argument"]} =>
       {qual="General", id=["Invalid_argument"]}
  | {qual="exc", id=["Failure"]}   => {qual="General", id=["Fail"]}
  | {qual="graphics", id=["Graphic_failure"]} =>
                          {qual="General", id=["Graphic_failure"]}
  | {qual="general", id=["Exception"]} => {qual="General", id=["(Exception)"]}
  | {qual="general", id=[id]}      => {qual="General", id=[id]}
  | qualid => qualid
;

*)

fun reset_linker_tables () =
(
  global_table := new_numtable 263;
  literal_table := [];
  List.app
    (fn {qual, id} =>
       ignore( get_slot_for_defined_variable
	      (* ps: ({qual="(global)", id=[id]}, 0) *)
	      ({qual=qual, id=[id]}, 0) ))
    Predef.predef_variables;
(* ps: exn_tag_table := new_numtable 31;
  tag_exn_table := Array.array(50, unknown_exn_name);
  List.app
    (fn ({qual,id}, stamp) => 
        ignore(get_num_of_exn (normalizeExnName {qual=qual,id = [id]}, 0)))
    Predef.predef_exn;
*)
  set_c_primitives Prim_c.primitives_table
);

fun save_linker_tables outstream =
(
  output_binary_int outstream (! (#num_cnt(!global_table)))
(* cvr: removed
  output_value outstream (!exn_tag_table);
  output_value outstream (!tag_exn_table)
*)
(* cvr: added: *)
(* ps:  ; output_value outstream (toShortExnTagTable(!exn_tag_table));
  output_value outstream (toShortTagExnTable(!tag_exn_table))
(* cvr: *) *)
);

(* To read linker tables from the executable file *)

fun load_linker_tables () =
  ( let
      val is = open_in_bin (Vector.sub(Miscsys.command_line, 0))
      (* The code, data, symb, and debug indexes are located 20 bytes 
         before the end of the bytecode file. *)
      val () = seek_in is (in_stream_length is - 20)
      val size_code = input_binary_int is
      val size_data = input_binary_int is
      val size_symb = input_binary_int is
      val size_debug = input_binary_int is
    in
      seek_in is (in_stream_length is - 20 - size_debug - size_symb);
      (* We don't need information about the internals *)
      (* of Moscow ML system! *)
      global_table := new_numtable 263;
      #num_cnt (!global_table) := input_binary_int is
    end
  ) handle _ => fatalError "Unable to read linker tables from bytecode"
;

(* Initialization *)

prim_val available_primitives : unit -> string Vector.vector
                                  = 1 "available_primitives";

fun init_linker_tables () =
(
  load_linker_tables();
  (* Hasht.clear (#num_tbl (!global_table)); *)
  appFrom
    (fn slot => fn {qual,id} =>
       Hasht.insert (#num_tbl (!global_table)) ({qual=qual,id=[id]}, 0) slot)
    0 Predef.predef_variables;
  literal_table := [];
  set_c_primitives (available_primitives())
);

(* added -- 07Sep95 e *)

fun protect_linker_tables fct =
  let val saved_global_table     = !global_table
      and saved_literal_table    = !literal_table
      and saved_c_prim_table     = !c_prim_table
  in
    (fct();
     global_table  := saved_global_table;
     literal_table := saved_literal_table;
     c_prim_table  := saved_c_prim_table
     )
    handle x =>
      (global_table  := saved_global_table;
       literal_table := saved_literal_table;
       c_prim_table  := saved_c_prim_table;
       raise x)
  end

end;
