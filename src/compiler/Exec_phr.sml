(* Exec_phr.sml *)
open Const
open List BasicIO Nonstdio Miscsys Fnlib Mixture Globals Units Types Asynt;
open Infixst Ovlres Infixres Elab Pr_zam Tr_env Front Back Compiler;
open Symtable Rtvals Load_phr;

(* Will successful evaluation results be reported in the top-level system: *)

val quietdec = ref false ;

(* Executing a top-level declaration. *)

(* cvr: TODO
fun report_results iBas cBas static_VE static_TE =
  let val firstLine = ref true in
    app
      (fn x =>
         (msgIBlock 0;
          if !firstLine then (firstLine := false; msgPrompt "")
                        else msgContPrompt "";
          reportFixityResult x;
          msgEOL();
          msgEBlock()))
      (cleanEnv iBas);
    app
      (fn (id, tn) =>
         (msgIBlock 0;
          if !firstLine then (firstLine := false; msgPrompt "")
                        else msgContPrompt "";
          reportTypeResult tn;
          msgEOL();
          msgEBlock()))
      (cleanEnv static_TE);
    app
      (fn (id,sch) =>
         let val status = lookup_new_cBas cBas id
             val {qualid, info} = status
         in
           msgIBlock 0;
           msgCBlock 0;
           (if !firstLine then (firstLine := false; msgPrompt)
                         else msgContPrompt)
             (case info of
                 VARname  _ => "val "
               | PRIMname _ => "val "
               | CONname  _ => "con "
               | EXNname  _ => "exn "
               | REFname    => "con ");
           msgString id;
           msgString " ="; msgBreak(1, 4);
           (case info of
               VARname _  =>
                 let val slot = get_slot_for_variable (lookupRenEnv qualid)
                     val v = getGlobalVal slot
                 in printVal sch v end
             | PRIMname pi =>
                 if #primArity pi  = 0 then
                   msgString "-"
                 else
                   msgString "fn"
             | CONname ci =>
                 if #conArity(!ci) = 0 then
                   printVQ qualid
                 else
                   msgString "fn"
             | EXNname ei  =>
                 if #exconArity(!ei) = 0 then
                   printVQ qualid
                 else
                   msgString "fn"
             | REFname =>
                 msgString "fn");
           msgBreak(1, 4); msgString ": "; printScheme sch;
           msgEBlock();
           msgEOL();
           msgEBlock()
         end)
      (cleanEnv static_VE)
  end
;
*)

(* cvr: hack ! *)
(*
fun report_results iBas cBas static_VE static_TE =
    (msgString "toplevel report_results disabled";
    msgFlush())
;
*)
(* cvr: modified *)

(*
fun report_results iBas cBas static_VE static_TE =
  let val firstLine = ref true in
   app
      (fn x =>
         (msgIBlock 0;
          if !firstLine then (firstLine := false; msgPrompt "")
                        else msgContPrompt "";
          reportFixityResult x;
          msgEOL();
          msgEBlock()))
      (Compiler.cleanEnv iBas);
(* cvr: TODO temporarilly disable
    app
      (fn (id, tn) =>
         (msgIBlock 0;
          if !firstLine then (firstLine := false; msgPrompt "")
                        else msgContPrompt "";
          reportTypeResult tn;
          msgEOL();
          msgEBlock()))
      (cleanEnv static_TE);
*)
    app
      (fn (id,{qualid,info = (sch,status)}) =>
         let (* val status = lookup_new_cBas cBas id
                val {qualid, info} = status
              *)
             val qualid = if #qual qualid = "" 
                          then {qual = currentUnitName(), id = #id qualid} (* cvr: rectify on the fly *)
                          else qualid
         in
           msgIBlock 0;
           msgCBlock 0;
           (if !firstLine then (firstLine := false; msgPrompt)
                         else msgContPrompt)
             (case status of
                 VARname  _ => "val "
               | PRIMname _ => "val "
               | CONname  _ => "con "
               | EXNname  _ => "exn "
               | REFname    => "con ");
           msgString id;
           msgString " ="; msgBreak(1, 4);
           (case status of
               VARname _  =>
                 let val slot = get_slot_for_variable (lookupRenEnv ValId qualid)
                     val v = getGlobalVal slot
                 in printVal sch v end
             | PRIMname pi =>
                 if #primArity pi  = 0 then
                   msgString "-"
                 else
                   msgString "fn"
             | CONname ci =>
                 if #conArity(!ci) = 0 then
                   printVQ qualid
                 else
                   msgString "fn"
             | EXNname ei  =>
                 if #exconArity(!ei) = 0 then
                   printVQ qualid
                 else
                   msgString "fn"
             | REFname =>
                 msgString "fn");
           msgBreak(1, 4); msgString ": "; collectSchemeExplicitVars sch; printNextScheme sch; 
                  (* cvr: TODO revise to allow for free variables at top level etc.. *)
           msgEBlock();
           msgEOL();
           msgEBlock()
         end)
      (Compiler.cleanEnv static_VE)
  end
;
*)
local
    fun prTopEnv prInfo env firstLine =
	foldEnv (fn k => fn v => fn firstLine => 
		(msgIBlock 0;
		 (if firstLine then msgPrompt ""
		  else msgContPrompt "");
		 prInfo k v;	
                 msgEOL();
                 msgEBlock();       
		 false)) firstLine env;
   fun prVal {qualid,info=(sch,status)} =
       let val qualid = if #qual qualid = "" 
			    then {qual = currentUnitName(), id = #id qualid} (* cvr: rectify on the fly *)
			else qualid
       in
          msgString " ="; msgBreak(1, 4);
	  (case status of
	       VARname REGULARo  =>
		 let val slot = get_slot_for_variable (lookupRenEnv ValId qualid)
		     val v = getGlobalVal slot
		 in 
		     printVal sch v
		 end
	     | VARname _ => msgString "(overloaded)"
	     | PRIMname pi =>
		 if #primArity pi  = 0 then
		     msgString "-"
		 else msgString "fn"
	     | CONname ci =>
		 if #conArity(!ci) = 0 then
		     printVQ qualid
		 else msgString "fn"
	     | EXNname ei  =>
		 if #exconArity(!ei) = 0 then
		     printVQ qualid
		 else msgString "fn"
	     | REFname => msgString "fn");
         msgBreak(0, 4)
       end
in
fun report_results iBas (Env as EXISTS(T,(ME,FE,GE,VE,TE))) =
  let
     val _ = checkClosedExEnvironment Env; 
     val _ = collectTopVars Env;
     val firstLine = 
	 case T of 
	     [] => true 
	   |   _ =>  (msgIBlock 0;
		      msgPrompt "New type names: ";
		      prTyNameSet T ",";
		      msgEOL();
		      msgEBlock();
		      false)
     val firstLine = 
	 prTopEnv (fn id => fn status => reportFixityResult (id,status)) iBas firstLine;
     val firstLine = 
	 prTopEnv prModInfo ME firstLine;
     val firstLine = 
	 prTopEnv prFunInfo FE firstLine;
     val firstLine = 
	 prTopEnv prSigInfo GE firstLine;
     val firstLine = 
	 prTopEnv prTyInfo TE firstLine;
     val firstLine =
         prTopEnv (prVarInfo prVal) VE firstLine 
  in
      ()
  end;
(*
fun report_results iBas (Env as EXISTS(T,(ME,FE,GE,VE,TE))) =
  let 
     val firstLine =
      foldEnv
         (fn id => fn status => fn firstLine =>
	  (msgIBlock 0;
	   (if firstLine then msgPrompt ""
	    else msgContPrompt "");
	   reportFixityResult (id,status);
	   msgEOL();
	   msgEBlock();
	   false))
	   true (Mixture.cleanEnv iBas);
      val firstLine = 
	foldEnv 
	(fn id => fn (tyfun,conenv) => fn firstLine => 
	 (msgIBlock 0;
          msgCBlock 0;
	  (if firstLine then  msgPrompt ""
	    else msgContPrompt "");
	  msgString id;
	  msgString " =";
	  msgBreak(1, 2);
	  prTyFun  tyfun;
          msgEBlock();
          msgEOL();
          msgEBlock();
	  false)) firstLine TE;
      val firstLine =
	foldEnv 
	(fn id => fn {qualid,info = M} => fn firstLine => 
	 (msgIBlock 0;
          msgCBlock 0;
	  (if firstLine then msgPrompt ""
	    else msgContPrompt "");
	  msgString "module ";
	  msgString id;
	  msgBreak(1, 4);
          msgString ": ";
	  prMod  M;
          msgEBlock();
          msgEOL();
          msgEBlock();
	  false)) firstLine ME;
      val firstLine =
	foldEnv 
	(fn id => fn {qualid,info = F} => fn firstLine => 
	 (msgIBlock 0;
          msgCBlock 0;
	  (if firstLine then msgPrompt ""
	    else msgContPrompt "");
	  msgString "functor ";
	  msgString id;
	  msgBreak(1, 4);
          msgString ": ";
 	  prGenFun F; 
          msgEBlock();
          msgEOL();
          msgEBlock();
	  false)) firstLine FE;
      val firstLine =
	foldEnv 
	(fn id => fn {qualid,info = G} => fn firstLine => 
	 (msgIBlock 0;
          msgCBlock 0;
	  (if firstLine then msgPrompt ""
	    else msgContPrompt "");
	  msgString "signature ";
	  msgString id;
	  msgBreak(1, 4);
          msgString "= ";
	  prSig G;
          msgEBlock();
          msgEOL();
          msgEBlock();
	  false)) firstLine GE;
      val firstLine =
	foldEnv
	  (fn id => fn {qualid,info = (sch as TypeScheme{tscParameters,tscBody},status)} => fn firstLine =>
	   under_binder (fn () =>
	   let val qualid = if #qual qualid = "" 
				then {qual = currentUnitName(), id = #id qualid} (* cvr: rectify on the fly *)
			    else qualid
	   in
	       msgIBlock 0;
	       msgCBlock 0;
	       (if firstLine then msgPrompt
		else msgContPrompt)
	         (case status of
		     VARname  _ => "val "
		   | PRIMname _ => "val "
		   | CONname  _ => "con "
		   | EXNname  _ => "exn "
		   | REFname    => "con ");
	       msgString id;
	       msgString " ="; msgBreak(1, 4);
	       (case status of
		    VARname REGULARo  =>
			let val slot = get_slot_for_variable (lookupRenEnv ValId qualid)
			    val v = getGlobalVal slot
			in printVal sch v end
                  | VARname _ => msgString "(overloaded)"
		  | PRIMname pi =>
		        if #primArity pi  = 0 then
			    msgString "-"
			else
			    msgString "fn"
		  | CONname ci =>
			if #conArity(!ci) = 0 then
			    printVQ qualid
			else
			    msgString "fn"
		  | EXNname ei  =>
			if #exconArity(!ei) = 0 then
			    printVQ qualid
			else
			    msgString "fn"
		  | REFname =>
		       	msgString "fn");
	        msgBreak(1, 4); msgString ": "; 
	(*	collectSchemeExplicitVars sch; printNextScheme sch; *)
               (case status of 
		    VARname ovltype => 
			(case ovltype of
			     REGULARo => prType tscBody
			   | OVL1NNo => msgString "num -> num "
			   | OVL1NSo => msgString "numtext -> string "
			   | OVL2NNBo => msgString "num * num -> bool "
			   | OVL2NNNo => msgString "num * num -> num "
			   | OVL1TXXo => msgString "'a -> 'a "
			   | OVL1TPUo => msgString "(ppstream -> 'a -> unit) -> unit "
			   | OVL2EEBo => msgString  "''a * ''a -> bool ")
		| _ => prType tscBody);
		(* cvr: TODO revise to allow for free variables at top level etc.. *)
		msgEBlock();
		msgEOL();
		msgEBlock();
		false
	   end) ())
	   firstLine VE
  in
      ()
  end
*)
end
;



(* This is written in tail-recursive form to ensure *)
(* that the intermediate results will be discarded. *)

fun updateCurrentState ((iBas, cBas, (Env as EXISTS(T,(ME,FE,GE,VE, TE)))), RE) =
(
  catch_interrupt false;
  updateCurrentInfixBasis iBas;
  updateCurrentStaticT T;
  updateCurrentStaticME ME;
  updateCurrentStaticFE FE;
  updateCurrentStaticGE GE;
  updateCurrentStaticTE TE;
  updateCurrentStaticVE VE;
  updateCurrentRenEnv RE;
  catch_interrupt true;
  if not (!quietdec) then 
      ((* report_results iBas cBas VE TE; *)
       report_results iBas Env;
       msgFlush())
  else ()
);

fun execLamPhrase state (RE, tlams) =
(
  app
    (fn (is_pure, lam) =>
      ( (*   msgIBlock 0; Pr_lam.printLam lam; msgEOL(); msgEBlock();  *)
        (* msgIBlock 0; Pr_lam.printLam lam; msgEOL(); msgEBlock();msgFlush(); *) (* cvr: TODO remove *)
       ignore (loadZamPhrase
         let val zam = compileLambda is_pure lam in
             (* printZamPhrase zam; msgFlush(); *)
             (*   printZamPhrase zam; msgFlush();       (* cvr: TODO remove *) *)
	       zam
         end)
      ))
    tlams;
    updateCurrentState (state, RE)
);

fun execResolvedDecPhrase (iBas, cBas, dec) =

  let (* val _ = Asyntfn.printDec dec (* cvr: *) *)
      val ExEnv = 
	  let val ExEnv = elabToplevelDec dec
          in
              resolveOvlDec dec;
              commit_free_typevar_names ();
              ExEnv
          end
	  handle e => (rollback_free_typevar_names ();
		       raise e) 
  in 
    (* msgString "cvr: elaborated\n";msgFlush(); (* cvr: *) *)
    (* msgString "cvr: resolved\n";msgFlush(); (* cvr: *) *)
    execLamPhrase (iBas, cBas, ExEnv) (translateToplevelDec dec)
  end 
;

(* cvr:
fun execToplevelPhrase dec =
  execResolvedDecPhrase (resolveToplevelDec dec)
;
*)
fun execToplevelPhrase dec =
  let val _ = checkpoint_free_typevar_names ();
      val (iBas,resdec) = resolveToplevelDec dec in
    (* msgString "cvr: resolvedTopLevelDec";msgFlush(); (*cvr: TODO  REMOVE*) *)
    execResolvedDecPhrase (iBas,NILenv,resdec)
  end
;















