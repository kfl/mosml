(*  cvr: checks now in Elab.sml 
open List Fnlib Mixture Const Globals Location Units Asynt Asyntfn

(* --- Syntactic restrictions --- *)

fun inIds (ii : IdInfo) (iis : IdInfo list) =
  exists (fn ii' => #id(#qualid ii) = #id(#qualid ii')) iis
;


fun checkDuplIds (iis : IdInfo list) msg =
  case iis of
      [] => ()
    | ii :: iis' =>
        if inIds ii iis' then
          errorMsg (#idLoc (#info ii)) msg
        else checkDuplIds iis' msg
;

fun checkAllIdsIn xs ys msg =
  app (fn ii => if inIds ii ys then () else errorMsg (#idLoc (#info ii)) msg)
      xs
;
(* cvr: *)
fun inTyCons (ii : TyCon) (iis : TyCon list) =
  exists (fn ii' => #2(ii) = #2(ii')) iis
;

(* cvr: *)
fun checkDuplTyCons (iis : TyCon list) msg =
  case iis of
      [] => ()
    | ii :: iis' =>
        if inTyCons ii iis' then
          errorMsg (#1 ii) msg
        else checkDuplTyCons iis' msg
;

fun checkTy (loc, ty') =
  case ty' of
    TYVARty _ => ()
  | RECty fs =>
      (app_field checkTy fs;
       if duplicates (map fst fs) then
         errorMsg loc "The same label is bound twice in a record type"
       else ())
  | CONty(tys, _) =>
      app checkTy tys
  | FNty(ty, ty') =>
      (checkTy ty; checkTy ty')
;

fun checkAsPatSource (loc, pat') =
  case pat' of
    VARpat _ => ()
  | TYPEDpat((_, VARpat _), _) => ()
  | _ => errorMsg loc "Ill-formed source of a layered pattern"
;

fun checkPat (loc, pat') =
  case pat' of
    SCONpat _ => ()
  | VARpat _ => ()
  | WILDCARDpat => ()
  | NILpat _ => ()
  | CONSpat(_, p) => checkPat p
  | EXNILpat _ => ()
  | EXCONSpat(_, p) => checkPat p
  | EXNAMEpat _ => fatalError "checkPat"
  | REFpat p => checkPat p
  | RECpat(ref (RECrp(fs, _))) =>
      (app_field checkPat fs;
       if duplicates (map fst fs) then
         errorMsg loc "The same label is bound twice in a record pattern"
       else ())
  | RECpat(ref (TUPLErp _)) => fatalError "checkPat"
  | VECpat ps => app checkPat ps
  | PARpat p => checkPat p
  | INFIXpat _ => fatalError "checkPat"
  | TYPEDpat(pat, ty) => (checkPat pat; checkTy ty)
  | LAYEREDpat(pat1, pat2) =>
      (checkAsPatSource pat1;
       checkPat pat1; checkPat pat2)
;

fun isFnExp (_, exp') =
  case exp' of
    PARexp exp => isFnExp exp
  | TYPEDexp(exp, ty) => isFnExp exp
  | FNexp _ => true
  | _ => false
;

fun checkFnExp exp =
  if isFnExp exp then () else
  errorMsg (xLR exp) "Non-functional rhs expression in val rec declaration"
;

fun tyconsOfTBs tbs = map (fn(_, tycon, _) => tycon) tbs;
fun tyconsOfTDs tds = map (fn(_, tycon) => tycon) tds;
fun tyconsOfDBs dbs = map (fn(_, tycon, _) => tycon) dbs;
fun consOfDBs dbs =
  concat( map (fn(_, _, cbs) => map (fn ConBind(ii,_) => ii) cbs) dbs );

fun consOfEBs ebs =
  map (fn EXDECexbind(ii,_) => ii
        | EXEQUALexbind(ii,_) => ii)
      ebs
;

fun appOpt f u (SOME x) = f x
  | appOpt f u NONE     = u
;

fun checkTypBind (tyvars, tycon, ty) =
(
  checkTy ty;
  checkDuplIds tyvars
    "Duplicate parameter in a type binding";
  checkAllIdsIn (varsOfTy ty) tyvars
    "Unbound parameter in the rhs of a type binding"
);

fun checkTypDesc (tyvars, tycon) =
  checkDuplIds tyvars
    "Duplicate parameter in a prim_type binding"
;

(* true, false, it, nil, ::, and ref may not be rebound or respecified
   as constructors : *)

fun illegalCon id =
    id = "true" orelse id = "false" orelse id = "it"
    orelse id = "nil" orelse id = "::" orelse id = "ref"

fun initialUpper id =
    let val char1 = CharVector.sub(id, 0)
	        handle Subscript => fatalError "initialUpper" 
    in
	if #"A" <= char1 andalso char1 <= #"Z" then () (* OK *)
	else () (* warning *)
    end;

fun checkConName ({qualid={id = [id], ...}, info = {idLoc, ...}} : IdInfo) =
    if illegalCon id then
	errorMsg idLoc "Illegal rebinding or respecification"
    else
	();

fun checkDatBind (tyvars, tycon, cbs) =
(
  app (fn ConBind(ii, SOME ty) =>
                (checkConName ii;
		 checkTy ty;
                 checkAllIdsIn (varsOfTy ty) tyvars
                   "Unbound parameter in the rhs of a datatype binding")
        | ConBind(ii, NONE) => checkConName ii)
          cbs;
  checkDuplIds tyvars
    "Duplicate parameter in a datatype binding"
);

fun checkExBind (EXDECexbind(ii, ty_opt)) = 
    (checkConName ii; appOpt checkTy () ty_opt)
  | checkExBind (EXEQUALexbind(ii, _)) = checkConName ii;
;

fun checkInfixIds loc ids =
  if duplicates ids then
    errorMsg loc "An identifier appears twice in a fixity declaration"
  else ()
;

fun patOfValBind (ValBind(ref pat, _)) = pat;

fun checkExp (loc, exp') =
  case exp' of
    SCONexp _ => ()
  | VIDPATHexp _ => ()
  | FNexp mrules =>
      app checkMRule mrules
  | APPexp(func, arg) =>
      (checkExp func; checkExp arg)
  | LETexp(dec, scope) =>
      (checkDec false dec; checkExp scope)
  | RECexp(ref (RECre fs)) =>
      (app_field checkExp fs;
       if duplicates (map fst fs) then
         errorMsg loc "The same label is bound twice in a record expression"
       else ())
  | RECexp(ref (TUPLEre _)) => fatalError "checkExp"
  | VECexp es =>
      app checkExp es
  | PARexp e => checkExp e
  | INFIXexp _ => fatalError "checkExp"
  | TYPEDexp(e, ty) =>
      (checkExp e; checkTy ty)
  | ANDALSOexp(e1, e2) =>
      (checkExp e1; checkExp e2)
  | ORELSEexp(e1, e2) =>
      (checkExp e1; checkExp e2)
  | HANDLEexp(e, mrules) =>
      (checkExp e;
       app checkMRule mrules)
  | RAISEexp e =>
      checkExp e
  | IFexp(e0, e1, e2) =>
      (checkExp e0; checkExp e1; checkExp e2)
  | WHILEexp(e1, e2) =>
      (checkExp e1; checkExp e2)
  | SEQexp(e1, e2) =>
      (checkExp e1; checkExp e2)
(* cvr: TODO complete cases *)
and checkMRule (MRule(ref pats, exp)) =
(
  app checkPat pats; checkExp exp;
  checkDuplIds (foldR varsOfPatAcc [] pats)
    "The same pattern variable is bound twice"
)

and checkValBind (ValBind(ref pat, exp)) =
(
  checkPat pat; checkExp exp;
  checkDuplIds (varsOfPatAcc pat [])
    "The same variable is bound twice in a pattern"
)

and checkPrimValBind (_, ty, _, _) =
  checkTy ty

and checkDec onTop (loc, dec') =
  case dec' of
    VALdec (tyvars, (pvbs, rvbs)) =>
      (app checkValBind pvbs;
       app checkValBind rvbs;
       checkDuplIds tyvars "Duplicate explicit type variable";
       app (fn ValBind(_, exp) => checkFnExp exp) rvbs;
       let val pat_vars =
         foldR_map varsOfPatAcc patOfValBind
           (foldR_map varsOfPatAcc patOfValBind [] rvbs) pvbs
       in
         checkDuplIds(pat_vars)
            "The same variable is bound twice in a valbind"
       end)
  | PRIM_VALdec pbs =>
      let val ()  = app checkPrimValBind pbs
          val iis = map (fn (ii,_,_,_) => ii) pbs
      in
        checkDuplIds iis
          "The same variable is bound twice in a prim_valbind"
      end
  | FUNdec _ => fatalError "checkDec"
  | TYPEdec tbs =>
      (app checkTypBind tbs;
       let val tycons = tyconsOfTBs tbs in
         checkDuplTyCons tycons
           "The same tycon is bound twice in a type declaration"
       end)
  | PRIM_TYPEdec(_, tds) =>
      (app checkTypDesc tds;
       let val tycons = tyconsOfTDs tds in
         checkDuplTyCons tycons
           "The same tycon is bound twice in a prim_type declaration"
       end)
  | DATATYPEdec(dbs, tbs_opt) =>
      (app checkDatBind dbs;
       appOpt (app checkTypBind) () tbs_opt;
       let val tycons = tyconsOfDBs dbs @ appOpt tyconsOfTBs [] tbs_opt
           val cons = consOfDBs dbs
       in
         checkDuplTyCons tycons
           "The same tycon is bound twice in a datatype declaration";
         checkDuplIds cons
           "The same con is bound twice in a datatype declaration"
       end)
  | ABSTYPEdec(dbs, tbs_opt, dec2) =>
      (app checkDatBind dbs;
       appOpt (app checkTypBind) () tbs_opt;
       let val tycons = tyconsOfDBs dbs @ appOpt tyconsOfTBs [] tbs_opt
           val cons = consOfDBs dbs
       in
         checkDuplTyCons tycons
           "The same tycon is bound twice in an abstype declaration";
         checkDuplIds cons
           "The same con is bound twice in an abstype declaration";
         checkDec onTop dec2
       end)
  | EXCEPTIONdec ebs =>
      (app checkExBind ebs;
       checkDuplIds (consOfEBs ebs)
         "The same excon is bound twice in an exception declaration")
  | LOCALdec (dec1, dec2) =>
      (checkDec false dec1; checkDec onTop dec2)
  | OPENdec _ =>
      (*
      if not(!hasSpecifiedSignature) andalso
         onTop & currentUnitName() <> "Top"
      then
        (msgIBlock 0;
         errLocation loc;
         errPrompt "`open' is not permitted at the top level,";
         msgEOL();
         errPrompt "unless the unit has explicitly specified signature";
         msgEOL();
         msgEBlock();
         raise Toplevel); *)
      ()
  | EMPTYdec => ()
  | SEQdec (dec1, dec2) =>
      (checkDec onTop dec1; checkDec onTop dec2)
  | FIXITYdec(_, ids) =>
      checkInfixIds loc ids
(* cvr: TODO complete cases *)
;

(* --- Signatures --- *)

fun checkExDesc (ii, ty_opt) = 
    (checkConName ii; appOpt checkTy () ty_opt);

fun consOfEDs eds =
  map (fn (ii,_) => ii) eds
;

fun checkSpec onTop (loc, spec') =
  case spec' of
    VALspec (_,vds) =>
      (if not onTop then errorMsg loc
         "Value specifications are permitted only at the top level"
       else ();
       app (fn(_, ty) => checkTy ty) vds;
       let val iis = map (fn(ii,_) => ii) vds in
         checkDuplIds iis
           "The same variable is bound twice in a value description"
       end)
  | PRIM_VALspec pbs =>
      (if not onTop then errorMsg loc
         "Primitive value specifications are permitted only at the top level"
       else ();
       app checkPrimValBind pbs;
       let val iis = map (fn(ii,_,_,_) => ii) pbs in
         checkDuplIds iis
           "The same variable is bound twice in a prim_valbind"
       end)
  | TYPEDESCspec(_, tds) =>
      (if not onTop then errorMsg loc
         "Abstract type specifications are permitted only at the top level"
       else ();
       app checkTypDesc tds;
       let val tycons = tyconsOfTDs tds in
         checkDuplTyCons tycons
           "The same tycon is bound twice in a type description"
       end)
  | TYPEspec tbs =>
      (app checkTypBind tbs;
       let val tycons = tyconsOfTBs tbs in
         checkDuplTyCons tycons
           "The same tycon is bound twice in a manifest type description"
       end)
  | DATATYPEspec(dbs, tbs_opt) =>
      (if not onTop then errorMsg loc
         "Variant type specifications are permitted only at the top level"
       else ();
       app checkDatBind dbs;
       appOpt (app checkTypBind) () tbs_opt;
       let val tycons = tyconsOfDBs dbs @ appOpt tyconsOfTBs [] tbs_opt
           val cons = consOfDBs dbs
       in
         checkDuplTyCons tycons
           "The same tycon is bound twice in a datatype description";
         checkDuplIds cons
           "The same con is bound twice in a datatype description"
       end)
  | EXCEPTIONspec eds =>
      (if not onTop then errorMsg loc
         "Exception specifications are permitted only at the top level"
       else ();
       app checkExDesc eds;
       checkDuplIds (consOfEDs eds)
         "The same excon is bound twice in an exception description")
  | LOCALspec (spec1, spec2) =>
      (checkSpec false spec1; checkSpec onTop spec2)
  | OPENspec _ =>
      if onTop then errorMsg loc
        "`open' is not permitted at the top level"
      else ()
  | EMPTYspec => ()
  | SEQspec (spec1, spec2) =>
      (checkSpec onTop spec1; checkSpec onTop spec2)
(* cvr: TODO complete cases *)
;









*)


