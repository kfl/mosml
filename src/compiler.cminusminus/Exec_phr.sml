(* Exec_phr.sml *)
open Const
open List BasicIO Nonstdio Miscsys Fnlib Mixture Globals Units Types Asynt;
open Infixst Ovlres Infixres Elab Pr_zam Tr_env Front Back Compiler;
open Symtable Rtvals Load_phr;

(* Will successful evaluation results be reported in the top-level system: *)

val quietdec = ref false ;

(* Executing a top-level declaration. *)

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
end
;



(* This is written in tail-recursive form to ensure *)
(* that the intermediate results will be discarded. *)

fun updateCurrentState ((iBas, (Env as EXISTS(T,(ME,FE,GE,VE, TE)))), RE) =
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
      (report_results iBas Env;
       msgFlush())
  else ()
);

fun execLamPhrase state (RE, tlams) =
(
  app
    (fn (is_pure, lam) =>
      ( (* msgIBlock 0; Pr_lam.printLam lam; msgEOL(); msgEBlock();   *)
        (* msgIBlock 0; Pr_lam.printLam lam; msgEOL(); msgEBlock();msgFlush(); *) (* cvr: TODO remove *)
       ignore (loadZamPhrase
         let val zam = compileLambda is_pure lam in
             (* printZamPhrase zam; msgFlush(); *)
	       zam
         end)
      ))
    tlams;
    updateCurrentState (state, RE)
);

fun execResolvedDecPhrase (iBas, dec) =

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
    execLamPhrase (iBas, ExEnv) (translateToplevelDec dec)
  end 
;

fun execToplevelPhrase dec =
  let val _ = checkpoint_free_typevar_names ();
      val (iBas,resdec) = resolveToplevelDec dec in
    execResolvedDecPhrase (iBas,resdec)
  end
;















