(* Initialization of built-in units *)

open List Fnlib Const Smlexc Prim Smlprim Globals Units Types;

(* --- Global infix basis --- *)

val std_infix_basis =
[
   ("before", INFIXst 0),
   ("o",   INFIXst 3),    (":=",  INFIXst 3),
   ("=",   INFIXst 4),    ("<>",  INFIXst 4),
   ("<",   INFIXst 4),    (">",   INFIXst 4),
   ("<=",  INFIXst 4),    (">=",  INFIXst 4),
   ("@",   INFIXRst 5),   ("::",  INFIXRst 5),
   ("+",   INFIXst 6),    ("-",   INFIXst 6),
   ("^",   INFIXst 6),
   ("div", INFIXst 7),    ("mod", INFIXst 7),
   ("*",   INFIXst 7),    ("/",   INFIXst 7)
];

val () =
  app
    (fn(id, status) =>
      Hasht.insert pervasiveInfixTable id status)
    std_infix_basis
;

(* --- Initial constructor basis --- *)

val deConEnv = fn (ConEnv CE) => CE | _ => fatalError "deConEnv"

val infoFalse = hd(deConEnv initial_bool_CE)
and infoTrue  = hd(tl (deConEnv initial_bool_CE))
and infoNil   = hd(deConEnv initial_list_CE)
and infoCons  = hd(tl(deConEnv initial_list_CE))
and infoNONE  = hd(deConEnv initial_option_CE)
and infoSOME  = hd(tl (deConEnv initial_option_CE))
and infoEQUAL = hd(deConEnv initial_order_CE)
and infoGREATER = hd(tl(deConEnv initial_order_CE))
and infoLESS  = hd(tl(tl(deConEnv initial_order_CE)))
and infoANTIQUOTE  = hd(deConEnv initial_frag_CE)
and infoQUOTE      = hd(tl (deConEnv initial_frag_CE))

(* ps 2000-04-27
and infoLESS  = hd(deConEnv initial_order_CE)
and infoEQUAL = hd(tl(deConEnv initial_order_CE))
and infoGREATER = hd(tl (tl (deConEnv initial_order_CE)))
and infoQUOTE      = hd(deConEnv initial_frag_CE)
and infoANTIQUOTE  = hd(tl (deConEnv initial_frag_CE)) *)
;

(* *** Initial static environments *** *)

(* Typing variable environment *)

val sc_bool =
  trivial_scheme type_bool
and sc_ii_i = trivial_scheme
  (type_arrow (type_pair type_int type_int) type_int)
and sc_r_r = trivial_scheme
  (type_arrow type_real type_real)
and sc_s_i = trivial_scheme
  (type_arrow type_string type_int)
and sc_ss_s = trivial_scheme
  (type_arrow (type_pair type_string type_string) type_string)
and sc_s_exn = trivial_scheme
  (type_arrow type_exn type_string)
and sc_exn =
  trivial_scheme type_exn
;

(* cvr: TODO dummy schemes for overloaded val ids *) 
(* TODO: perhaps make
   these more meaningful than sc_bogus: this is not essential since
   they'll never be reported to the user anyway...
*) 
val sc_OVL1NNo = sc_bogus; 
val sc_OVL1NSo = sc_bogus; 
val sc_OVL2NNBo = sc_bogus; 
val sc_OVL2NNNo = sc_bogus;

fun VEofCE (ConEnv CE) =
    map (fn ci => 
	 let val coninfo =  #info(ci) 
	 in
	     (hd (#id(#qualid ci)), ((#conType(! coninfo)),CONname coninfo))
	 end)
    CE
  | VEofCE _ = fatalError "VEofCE";

(* cvr: added *)
val initial_eq_VE =
[
  ("=", ((scheme_1u_eq (fn a =>
          type_arrow (type_pair a a) type_bool)),
        VARname OVL2EEBo)),
  ("<>", ((scheme_1u_eq (fn a =>
           type_arrow (type_pair a a) type_bool)), 
          VARname OVL2EEBo))
];


val initial_OVL1NNo_VE = [
   ("~",(sc_OVL1NNo,VARname OVL1NNo)),
   ("abs",(sc_OVL1NNo,VARname OVL1NNo))
];

val initial_OVL2NNNo_VE = [
   ("+",(sc_OVL2NNNo,VARname OVL2NNNo)),
   ("-",(sc_OVL2NNNo,VARname OVL2NNNo)),
   ("*",(sc_OVL2NNNo,VARname OVL2NNNo)),
   ("div",(sc_OVL2NNNo,VARname OVL2NNNo)),
   ("mod",(sc_OVL2NNNo,VARname OVL2NNNo))
];

val initial_OVL2NNBo_VE = [
   ("<",(sc_OVL2NNBo,VARname OVL2NNBo)),
   (">",(sc_OVL2NNBo,VARname OVL2NNBo)),
   ("<=",(sc_OVL2NNBo,VARname OVL2NNBo)),
   (">=",(sc_OVL2NNBo,VARname OVL2NNBo))
];

val initial_OVL1NSo_VE = [
   ("makestring",(sc_OVL1NSo,VARname OVL1NSo))
];

val initial_int_VE =
[
];

val initial_real_VE =
[
  ("/",      (trivial_scheme
               (type_arrow (type_pair type_real type_real) type_real),
              PRIMname (mkPrimInfo 1  MLPdiv_real))),
  ("floor",  (trivial_scheme (type_arrow type_real type_int),
              PRIMname (mkPrimInfo 1  (MLPccall(1, "sml_floor"))))),
  ("ceil",   (trivial_scheme (type_arrow type_real type_int),
              PRIMname (mkPrimInfo 1  (MLPccall(1, "sml_ceil"))))),
  ("trunc",  (trivial_scheme (type_arrow type_real type_int),
              PRIMname (mkPrimInfo 1  (MLPccall(1, "sml_trunc"))))),
  ("round",  (trivial_scheme (type_arrow type_real type_int),
              PRIMname (mkPrimInfo 1  (MLPccall(1, "sml_round"))))),
  ("real",   (trivial_scheme (type_arrow type_int type_real),
              PRIMname (mkPrimInfo 1 
                          (MLPprim(1, Pfloatprim Pfloatofint)))))
];

val initial_string_VE =
[
  ("^",          (sc_ss_s, 
		  PRIMname (mkPrimInfo 1 (MLPconcat)))),
  ("size",       (sc_s_i,
		  PRIMname (mkPrimInfo 1 (MLPprim(1, Pstringlength))))),
  ("exnName",    (sc_s_exn,
		  PRIMname (mkPrimInfo 1  (MLPccall(1, "sml_exnname"))))),
  ("exnMessage", (sc_s_exn,
		  PRIMname (mkPrimInfo 1  (MLPccall(1, "sml_exnmessage")))))
];

val initial_ref_VE =
[
  ("ref", (scheme_1u_imp (fn a =>
             type_arrow a (type_ref a)), 
           REFname)),
  ("!", (scheme_1u (fn a =>
           type_arrow (type_ref a) a),
         PRIMname (mkPrimInfo 1 (MLPprim(1, Pfield 0))))),
  (":=", (scheme_1u (fn a =>
            type_arrow (type_pair (type_ref a) a) type_unit),
          PRIMname (mkPrimInfo 1 (MLPsetref))))
];

val sml_initial_VE = concat
[
  VEofCE initial_bool_CE,
  initial_eq_VE, (* cvr: cf. the original if this doesn't work *)
  initial_int_VE,
  initial_real_VE, 
  initial_string_VE,
  VEofCE initial_list_CE,
  VEofCE initial_option_CE,
  VEofCE initial_order_CE,
  VEofCE initial_frag_CE,
  initial_ref_VE,
  [("not", (trivial_scheme(type_arrow type_bool type_bool),
            PRIMname (mkPrimInfo 1 (MLPprim(1, Pnot)))))],
  [("ignore", (scheme_1u (fn a => type_arrow a type_unit),
               PRIMname (mkPrimInfo 1  (MLPprim(1, Patom 0)))))],
  (* cvr: added overloaded bindings to VE *)
  initial_OVL1NNo_VE,
  initial_OVL2NNNo_VE,
  initial_OVL2NNBo_VE,
  initial_OVL1NSo_VE 
];


val sml_initial_TE =
[
   ("unit",     (APPtyfun (NAMEtyapp tyname_unit), ConEnv [])),
   ("bool",      (APPtyfun (NAMEtyapp tyname_bool), initial_bool_CE)),
   ("int",       (APPtyfun (NAMEtyapp tyname_int), ConEnv [])), 
   ("syserror",  (APPtyfun (NAMEtyapp tyname_syserror), ConEnv [])),
   ("word",      (APPtyfun (NAMEtyapp tyname_word), ConEnv [])),
   ("word8",     (APPtyfun (NAMEtyapp tyname_word8), ConEnv [])),
   ("char",      (APPtyfun (NAMEtyapp tyname_char), ConEnv [])),
   ("real",      (APPtyfun (NAMEtyapp tyname_real), ConEnv [])),
   ("string",    (APPtyfun (NAMEtyapp tyname_string), ConEnv [])),
   ("substring", (APPtyfun (NAMEtyapp tyname_substring), ConEnv [])),
   ("list",      (APPtyfun (NAMEtyapp tyname_list), initial_list_CE)),
   ("vector",    (APPtyfun (NAMEtyapp tyname_vector), ConEnv [])),
   ("option",    (APPtyfun (NAMEtyapp tyname_option), initial_option_CE)),
   ("order",     (APPtyfun (NAMEtyapp tyname_order), initial_order_CE)),
   ("frag",      (APPtyfun (NAMEtyapp tyname_frag), initial_frag_CE)),
   ("ref",       (APPtyfun (NAMEtyapp tyname_ref), ConEnv [])),
   ("exn",       (APPtyfun (NAMEtyapp tyname_exn), ConEnv [])),
   ("ppstream",  (APPtyfun (NAMEtyapp tyname_ppstream), ConEnv []))
];

val sml_initial_T = 
    map (fn (_,(APPtyfun (NAMEtyapp tn),_)) =>  tn 
         | _ => fatalError "sml_initial_T")
        sml_initial_TE;

val () =
  app (fn (id, scis) =>
         Hasht.insert (#uVarEnv unit_General) id 
           { qualid={qual="General", id=[id]}, info=scis })
      sml_initial_VE
;

val () =
  app (fn (id, tn) =>
         Hasht.insert (#uTyEnv unit_General) id tn)
      sml_initial_TE
;

val () = (#uTyNameSet unit_General) := sml_initial_T;

fun mkEi arity =
  let val ei = mkExConInfo() in
    setExConArity ei arity;
    ei
  end;

val sc_str_exn = trivial_scheme (type_arrow type_string type_exn);

(* The exn names for these are defined as globals by the runtime system *)

val predefExceptions = [
  ("Out_of_memory",    ("exn_memory",    0, sc_exn)),
  ("Invalid_argument", ("exn_argument",  1, sc_str_exn)),
  ("Graphic",          ("exn_graphic",   1, sc_str_exn)),
  ("SysErr",           ("exn_syserr",    1, 
                        trivial_scheme (type_arrow type_of_syserror_exn 
                                        type_exn))),
  ("Io",               ("exn_io",        1, 
			trivial_scheme(type_arrow type_of_io_exn type_exn))),
  ("Fail",             ("exn_fail",      1, sc_str_exn)),
  ("Size",             ("exn_size",      0, sc_exn)),
  ("Interrupt",        ("exn_interrupt", 0, sc_exn)),
  ("Subscript",        ("exn_subscript", 0, sc_exn)),
  ("Chr",              ("exn_chr",       0, sc_exn)),
  ("Div",              ("exn_div",       0, sc_exn)),
  ("Domain",           ("exn_domain",    0, sc_exn)),
  ("Ord",              ("exn_ord",       0, sc_exn)),
  ("Overflow",         ("exn_overflow",  0, sc_exn)),
  ("Bind",             ("exn_bind",      0, sc_exn)),
  ("Match",            ("exn_match",     0, sc_exn))
];

val () =
  app (fn (smlid, (globid, arity, sc)) => 
          let val sc = { qualid={qual="General", id=[globid]}, 
			 info=(sc, EXNname(mkEi arity)) }
	  in Hasht.insert (#uVarEnv unit_General) smlid sc end)
       predefExceptions

val () =
  Hasht.insert pervSigTable "General" unit_General;
