(* Rtvals.sml *)

open List Misc Obj BasicIO Nonstdio Miscsys Memory Fnlib Config Mixture;
open Const Smlexc Globals Units Types Symtable;

(* --- Run-time values --- *)

(* Encoding and decoding *)

fun decode_int (v : obj) = (magic_obj v : int);

fun decode_word (v : obj) = (magic_obj v : word);

fun decode_char (v : obj) = (magic_obj v : char);

fun decode_real (v : obj) = (magic_obj v : real);

fun decode_string (v : obj) = (magic_obj v : string);

(* Exceptions *)

fun decode_exn (v : obj) (c0 : QualifiedIdent -> unit) 
                         (c1 : QualifiedIdent -> obj -> Type option -> unit) =
    let val strref = getExnStrref v 
	val arg = obj_field v 1
	fun prExn exnPrName NONE         = c0 exnPrName
	  | prExn exnPrName (SOME argTy) = c1 exnPrName arg (SOME argTy)
    in prExn { qual = "", id = [!strref] } (Smlexc.exnArgType strref arg) end

(* Run-time environments *)

fun getGlobalVal (slot : int) =
  Vector.sub(global_data, slot)
;

fun setGlobalVal (slot : int) (v : obj) =
  let prim_val update_ : 'a Vector.vector -> int -> 'a -> unit
                           = 3 "set_vect_item"
  in update_ global_data slot v end
;

(* Block values *)

fun decode_block (v : obj) =
  if not(is_block v) then
    fatalError "block expected"
  else
    let val len = obj_size v
        fun makeArgs i =
              if i>= len then [] else obj_field v i :: makeArgs (i+1)
    in (obj_tag v, makeArgs 0) end
;

fun decode_unit (v : obj) = ();

fun decode_pair (v : obj) = (magic_obj v : obj * obj);

fun decode_boolean (v : obj) = (magic_obj v : bool);

fun decode_list (v : obj) = (magic_obj v : obj list);

fun decode_vector (v : obj) = (magic_obj v : obj Vector.vector);

(* --- Value printing --- *)

fun prSeq lbr rbr printer sep ts vs =
  let fun loop [] [] = ()
        | loop [t] [v] = printer t v
        | loop (t :: ts) (v :: vs) =
            (printer t v; msgString sep; msgBreak(1, 1); loop ts vs)
        | loop _ _ = fatalError "prSeq: length mismatch"
  in
    msgIBlock 0; msgString lbr;
    loop ts vs;
    msgString rbr; msgEBlock()
  end
;

fun prInt (v: obj) =
  let val n = decode_int v
  in msgString (sml_string_of_int n) end
;

fun prWord (v: obj) =
  let val n = decode_word v
  in msgString (sml_hexstring_of_word n) end
;

fun prChar (v : obj) =
  let val c = decode_char v
  in msgString (sml_makestring_of_char c) end
;

fun prReal (v : obj) =
  let val r = decode_real v
  in msgString (sml_string_of_float r) end
;

fun prString (v : obj) =
  let val s = decode_string v
  in msgString (sml_makestring_of_string s) end
;

fun prLiteralConst (depth: int) (v: obj) =
  if not(is_block v) then
    prInt v
  else if depth <= 0 then
    msgString "#"
  else
    let val tag = obj_tag v
        val len = obj_size v
    in
      if tag = realTag then
        prReal v
      else if tag = stringTag then
        prString v
      else
        (msgString "(BLOCK "; msgInt tag;
         for (fn i => (msgString " ";
                       prLiteralConst (depth-1) (obj_field v i)))
             0 (len-1);
         msgString ")")
    end
;

fun printLiteralConst (v: obj) =
  prLiteralConst 10 v
;

fun prGeneric (v : obj) =
  if not(is_block v) then
    msgString "<poly>"
  else
    let val tag = obj_tag v in
      if tag = realTag then prReal v
      else if tag = stringTag then prString v
      else msgString "<poly>"
    end
;

val installedPrinters = ref([] : (TyName * (ppstream -> obj -> unit)) list);

fun findInstalledPrinter tyname =
  let fun loop [] = NONE
        | loop ((tyname', p) :: rest) =
            if isEqTN tyname tyname' then (SOME p) else (loop rest)
  in loop (!installedPrinters) end
;

val printDepth = ref 20;
val printLength = ref 200;

fun prVal (depth: int) (prior: int) (tau: Type) (v: obj) =
  let fun prP s = if prior > 0 then msgString s else ()
      fun prD f = if depth <= 0 then msgString "#" else f()
      and prExn (e : obj) =			(* e : exn *)
	  decode_exn (repr e)
	             (fn q => (prP " "; printVQ q))
	             (fn q => fn va => fn tyOpt =>
		              (prP "(";
			       printVQ q; msgString " ";
			       (case tyOpt of 
				    NONE    => prGeneric va
				  | SOME ty => prVal (depth-1) 1 ty va); 
			       prP ")" ))
      fun prettyprint printer pp_out v =
	  printer pp_out v
	  handle e => (msgString "<installed prettyprinter failed: "; 
		       prExn (repr e); msgString ">")
      val tau = normType tau
  in
    case tau of
      VARt _ => (prP " "; prGeneric v)
    | ARROWt _ => (prP " "; msgString "fn")
    | RECt rt =>
        let val {fields=fs, ...} = !rt
            val (_, vs) = decode_block v
        in
          if isTupleRow fs then
            (prD (fn() =>
               prSeq "(" ")" (prTupleField (depth-1)) "," fs vs))
          else
            (prD (fn() =>
               prSeq "{" "}" (prField (depth-1)) "," fs vs))
        end
    | CONt(ts, tyapp) => 
        (case conEnvOfTyApp tyapp of
           NONE => 
            (case tyapp of
		 NAMEtyapp tyname =>
		     (case findInstalledPrinter tyname of
			  SOME printer => prettyprint printer pp_out v 
			| NONE =>
				if (isEqTN tyname tyname_int) then (prP " "; prInt v)
				else if (isEqTN tyname tyname_word)   
					 then (prP " "; prWord v)
                                else if (isEqTN tyname tyname_word8)  
					 then (prP " "; prWord v)
				else if (isEqTN tyname tyname_char)  
					 then (prP " "; prChar v)
				else if (isEqTN tyname tyname_real)
					 then (prP " "; prReal v)
				else if (isEqTN tyname tyname_string)
					 then (prP " "; prString v)
   			        else if (isEqTN tyname tyname_exn) then prExn v
				else if (isEqTN tyname tyname_ref) then
				    let val t = hd ts
					val x = obj_field v 0
				    in
					prD (fn() => 
					     (prP "(";printVQ (#qualid tyname);
					      prVal (depth-1) 1 t x; prP ")"))
				    end
				else if (isEqTN tyname tyname_vector) then
				    let val vs = decode_vector v in
					prD (fn() =>
					     (prP " ";
					      prVector (depth-1)
					               (!printLength) 
						       (hd ts) 
						       vs))
				    end
				else
				    (msgString "<";
				     msgString (hd (#id (#qualid tyname)));
				     msgString ">"))
	       | APPtyapp _ =>(msgString "<"; 
			       prTyApp 0 tyapp; 
			       msgString ">"))
         | SOME (ConEnv CE) =>
             ( if (case tyapp of 
		       NAMEtyapp tyname =>
			 (case findInstalledPrinter tyname of
			       SOME printer => (prettyprint printer pp_out v;true) 
			     | NONE => false)
		      | _ => false)
		then ()
		else
                    if null CE then
                      (msgString "<"; prTyApp 0 tyapp;
                       msgString ">")
                    else if #conSpan(! (#info (hd CE))) = 1 andalso
                            #conArity(! (#info (hd CE))) = 1
                    then
                      let val ci = hd CE
                          val {qualid, info} = ci
                          val {conArity, conIsGreedy, conType, ...} = !info
                      in
                        case specialization conType of
                            ARROWt(a_t, r_t) =>
                              (unify tau r_t;
                               (prD (fn() =>
                                  (prP "("; printVQ qualid;
                                   prVal (depth-1) 1 a_t v;
                                   prP ")"))))
                          | _ => fatalError "prVal"
                      end
                    else
                      let val i = obj_tag v
                          val ci = nth(CE, i)
                          val {qualid, info} = ci
                          val {conArity, conIsGreedy, conType, ...} = !info
                      in
                       if case tyapp of 
			   NAMEtyapp tyname =>
			       if (isEqTN tyname tyname_list) then
				   (prD (fn() =>
					 (prP " ";
					  prList (depth-1) (!printLength)
					  (hd ts) (decode_list v)));
				    true)
			       else false
			  | _ => false
			then ()
                        else if conArity = 0 then
                          (prD (fn() => (prP " "; printVQ qualid)))
                        else
                          case specialization conType of
                              ARROWt(a_t, r_t) =>
                                (unify tau r_t;
                                 (prD (fn() =>
                                    (prP "("; printVQ qualid;
                                     if conIsGreedy
                                       then prVal (depth-1) 1 a_t v
                                       else prVal (depth-1) 1 a_t (obj_field v 0);
                                     prP ")"))))
                            | _ => fatalError "prVal"
                      end)
	 | _ => fatalError "prVal 1")
 | PACKt (EXISTSexmod(T,STRmod S)) =>  (prP " "; msgString "[structure ...]")
 | PACKt (EXISTSexmod(T,FUNmod F)) =>  (prP " "; msgString "[functor ...]")
end

and prField (depth: int) (lab, t) v =
  (msgIBlock 0; printLab lab; msgString " ="; msgBreak(1, 2);
   prVal depth 0 t v; msgEBlock())

and prTupleField (depth: int) (lab, t) v =
  prVal depth 0 t v

and prList (depth: int) (len: int) tau v =
  case v of
      [] => msgString "[]"
    | x :: xs =>
        if len <= 0 then
          msgString "[...]"
        else
          (msgIBlock 0; msgString "["; prVal depth 0 tau x;
           prListTail depth (len-1) tau xs)

and prListTail (depth: int) (len: int) tau = fn
    [] => (msgString "]"; msgEBlock())
  | x :: xs =>
      (msgString ","; msgBreak(1, 1);
       if len <= 0 then
         (msgString "...]"; msgEBlock())
       else
         (prVal depth 0 tau x; prListTail depth (len-1) tau xs))

and prVector (depth: int) (maxlen: int) tau v =
  let val len = Vector.length v
      fun loop count i =
        if i = len then msgString "]"
        else if count <= 0 then
          (msgString ","; msgBreak(1, 2); msgString "...]")
        else
          (msgString ","; msgBreak(1, 2);
           prVal depth 0 tau (Vector.sub(v, i));
           loop (count-1) (i+1))
  in
    msgIBlock 0;
    if len = 0 then msgString "#[]"
    else if maxlen <= 0 then msgString "#[...]" else
      (msgString "#["; prVal depth 0 tau (Vector.sub(v, 0));
       loop (maxlen-1) 1);
    msgEBlock()
  end
;

fun printVal (scheme: TypeScheme) (v: obj) =
  prVal (!printDepth) 0 (specialization scheme) v
;

fun evalPrint (sc : obj) (v : obj) =
  (printVal (magic_obj sc : TypeScheme) v; msgFlush(); v)
;

fun evalInstallPP (sc : obj) (p : ppstream -> 'a -> unit) =
  case normType(specialization (magic_obj sc : TypeScheme)) of
      CONt([], NAMEtyapp tyname) =>
	  installedPrinters :=
	  (tyname, magic p : ppstream -> obj -> unit)
	  :: !installedPrinters
(*
      CONt([], NAMEtyapp tyname) =>
        (case #tnStr(! (#info tyname)) of
             DATATYPEts _ =>
               installedPrinters :=
                 (tyname, magic p : ppstream -> obj -> unit)
                 :: !installedPrinters
	   | NILts =>
               installedPrinters :=
                 (tyname, magic p : ppstream -> obj -> unit)
                 :: !installedPrinters
           | _ =>
              raise Fail "installPP: pp's argument is not a nullary type constructor")
*)
    |  CONt(_ :: _, tyname) =>
        raise Fail "installPP: pp's argument type is not a nullary type constructor"
    | _ =>
        raise Fail "installPP: pp's argument type is not a type constructor"
;

(* === End of Primitives === *)

(* --- Handling global dynamic environment --- *)

fun loadGlobalDynEnv uname env =
(
  app (fn(id,_) =>
             ignore (get_slot_for_defined_variable ({qual=uname, id=[id]}, 0)))
    env;
  if number_of_globals() >= Vector.length global_data then
      realloc_global_data(number_of_globals())
  else ();
  app (fn(id,v) =>
            let val slot = get_slot_for_variable ({qual=uname, id=[id]}, 0)
            in setGlobalVal slot v end)
          env
);

fun resetGlobalDynEnv() = init_linker_tables();
