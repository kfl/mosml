(* Production of a bytecode executable file *)

open Misc BasicIO Nonstdio Miscsys Obj Fnlib Const Mixture Config;
open Code_dec Symtable Patch Tr_const;

val autolink = ref true
val verbose  = ref false

(* First pass: check the consistency of files *)

fun read_file name = 
    let val truename = find_in_path name
	val is = open_in_bin truename
	val tables =
	    let val n = input_binary_int is in
		seek_in is n;
		input_value is : compiled_unit_tables
	    end
	    handle x =>
		(close_in is;
		 msgIBlock 0;
		 errPrompt "Error on file ";
		 msgString truename; msgEOL();
		 msgEBlock();
		 raise x)
	val _ = close_in is
    in (truename, tables) end

exception WrongStamp and NotYet

fun check_file name stampOpt pending processed = 
  let val simplename = Filename.chop_suffix name ".uo"
      val uname = normalizedUnitName(Filename.basename simplename)
      val () =
	  if member uname pending then
	      raise Fail ("Unit " ^ name ^ " depends on itself")
	  else ()
      val () =
	  if member uname reservedUnitNames then
	      raise Fail ("Unit "^uname^" is built-in, and cannot be linked")
	  else ()

      val already = (SOME (Hasht.find (!watchDog) uname)) 
	            handle Subscript => NONE 

      fun needs subuname substamp processed =
	  (check_file (subuname ^ ".uo") (SOME substamp) 
	              (uname :: pending) processed)
	  handle WrongStamp => 
	              raise Fail ("Compiled body of unit " ^ uname 
				    ^ " is incompatible with unit "^ subuname)
	       | NotYet => 
		      raise Fail ("Unit " ^ subuname ^ " is mentioned by "
				  ^ uname ^ " but not yet linked")
  in 
      case already of
	  SOME stamp' =>
	      (case stampOpt of
		   SOME stamp => 
		       if stamp <> stamp' then raise WrongStamp 
		       else processed
		 | NONE => 
                       (msgIBlock 0;
			errPrompt "Warning: unit ";
			msgString uname; 
			msgString " is needed by a unit preceding it";
			msgEOL();
			msgEBlock();
			processed))
	| NONE => let val implicit = case stampOpt of NONE => false | _ => true
		      val _ = if not(!autolink) andalso implicit 
			      then raise NotYet else ()
		      val (truename, tables) = read_file name
		      val precedingUnits = 
			  Hasht.fold needs processed (#cu_mentions tables)
		   in 
		      Hasht.insert (!watchDog) uname (#cu_sig_stamp tables);
		      (uname, truename, tables) :: precedingUnits
		  end
  end

val check_file = fn name => fn processed => check_file name NONE [] processed


(* Second pass: determine which phrases are required *)

val missing_globals =
    ref (Hasht.new 1 : (QualifiedIdent * int, unit) Hasht.t)
;

fun is_in_missing g =
  (Hasht.find (!missing_globals) g; true)
  handle Subscript => false
;

fun remove_from_missing g =
  Hasht.remove (!missing_globals) g
;

fun add_to_missing g =
  Hasht.insert (!missing_globals) g ()
;

fun is_required (Reloc_setglobal g, _) = is_in_missing g
  | is_required _ = false
;

fun remove_required (Reloc_setglobal g, _) = remove_from_missing g
  | remove_required _ = ()
;

fun add_required (Reloc_getglobal g, _) = add_to_missing g
  | add_required _ = ()
;

fun scan_val uname (id, stamp) tolink =
  let val q = {qual=uname, id=[id]} in
    if is_in_missing (q, 0) then
      (remove_from_missing (q, 0);
       add_to_missing (q, stamp);
       (id, stamp) :: tolink)
    else
      tolink
  end;

fun scan_phrase (phr : compiled_phrase) tolink =
    let val (_, otherlist) = #cph_reloc phr
    in 
	if not (#cph_pure phr) orelse List.exists is_required otherlist then
	    (List.app remove_required otherlist;
	     List.app add_required otherlist;
	     phr :: tolink)
	else
	    tolink
    end;

fun scan_file (uname, truename, (tables : compiled_unit_tables)) tolink =
  let val exportedE = #cu_exc_ren_list tables
      val valRenList = #cu_val_ren_list tables
      val exportedV = foldL (scan_val uname) [] valRenList
      val phraseIndex = #cu_phrase_index tables
      val required = foldL scan_phrase [] phraseIndex
  in
    (uname, truename, required, exportedE, exportedV) :: tolink
  end;

(* Third pass : link in the required phrases. *)

fun link_object os (uname, truename, required, exportedE, exportedV) =
  let val is = open_in_bin truename in
    (List.app
       (fn (phr : compiled_phrase) =>
         let val () = seek_in is (#cph_pos phr)
             val buff = input(is, #cph_len phr)
             val () = if size buff < #cph_len phr
                      then raise Size else ()
         in
           patch_object buff 0 (#cph_reloc phr);
           output(os, buff)
         end)
       required;
     exportPublicNames uname exportedE exportedV;
     close_in is)
    handle x =>
      (close_in is;
       msgIBlock 0;
       errPrompt "Error while linking file ";
       msgString truename; msgEOL();
       msgEBlock();
       raise x)
  end;

(* To build the initial table of globals *)

local
  prim_val vector_ : int -> '_a -> '_a vector       = 2 "make_vect"
  prim_val sub_    : 'a vector -> int -> 'a         = 2 "get_vect_item"
  prim_val update_ : 'a vector -> int -> 'a -> unit = 3 "set_vect_item"
in

  fun emit_data os =
    let val len = number_of_globals()
        val globals = vector_ len (repr 0)
    in
      List.app
        (fn (n,sc) => update_ globals n (translStructuredConst sc))
        (!literal_table);
      output_value os globals
    end;

end;

(* To build a bytecode executable file *)

val write_symbols = ref false;
val no_header     = ref false;
val stand_alone   = ref false; (* cvr: 144 merge *)

fun reportLinked toscan =
    let fun reportUnit (uname, _, _) = (msgString uname; msgString ".uo ")
    in 
	msgIBlock 0; 
	msgString "Linking: ";
	List.app reportUnit (rev toscan); 
	msgEOL(); msgEBlock()
    end

fun link unit_list exec_name =
  let val _ = missing_globals :=  (* 04Sep95 e *)
               (Hasht.new 263 : (QualifiedIdent * int, unit) Hasht.t)
      val toscan = foldL check_file [] unit_list
      val _ = if !verbose then reportLinked toscan else ()
      val tolink = foldL scan_file [] toscan
      val os = if !no_header andalso not (!stand_alone) then 
	           open_out_bin exec_name
	       else 
		   open_out_exe exec_name
      fun copy name =
	  let val buff = CharArray.array(4096, #"\000")
	      val is = open_in_bin (Filename.concat (!path_library) name)
	      fun loop () = 
		  case buff_input is buff 0 4096 of
		      0 => ()
		    | n => (buff_output os buff 0 n; loop ())
	  in 
	      (loop (); close_in is) handle x => (close_in is; raise x)
	  end
  in
     ((* Prepend the runtime system? *)
      if !stand_alone then copy "camlrunm" else ();
      (* Prepend the header? *)
      if !no_header orelse !stand_alone then () else copy "header";
      missing_globals := (* for gc -- 04Sep95 e *)
       (Hasht.new 1 : (QualifiedIdent * int, unit) Hasht.t);
      (* The bytecode *)
      let val pos1 = pos_out os
          val () = List.app (link_object os) tolink
          val () = output_byte os Opcodes.STOP;
          (* The table of global data *)
          val pos2 = pos_out os
          val () = emit_data os
          (* Linker tables *)
          val pos3 = pos_out os
          val () =
            if !write_symbols then save_linker_tables os
            else ();
          (* Debugging info (none, presently) *)
          val pos4 = pos_out os
      in
        (* The trailer *)
        output_binary_int os (pos2 - pos1);
        output_binary_int os (pos3 - pos2);
        output_binary_int os (pos4 - pos3);
        output_binary_int os 0;
        output(os, "ML08");
        close_out os
      end
    ) handle x =>
       (close_out os;
        remove_file exec_name;
        raise x)
  end;



