(* tr_env.ml: handling of the translation environment. *)

open List Fnlib Mixture Const Prim Lambda Globals Units Types Asynt Asyntfn;

type RenEnv = (string * int) list;

datatype AccessPath =
    Path_rec of int 
  | Path_local of int
  | Path_global of (QualifiedIdent * int)
  | Path_son of int * AccessPath
  | Path_virtual_son of int * AccessPath
;

type TranslEnv = (Const.Id, AccessPath) Env * int;


fun lookupRenEnv asId q =
  let val {qual, id = lid} = q 
      val id = (longIdentAsIdent lid "lookupRenEnv")
      val mangled_id = mangle (asId id)
  in (* cvr: TODO treat lond ids *)
    if qual = "" then fatalError ("lookupRenEnv: empty qualifier for "^id)
    else ();
    if qual = currentUnitName() then
      (mkUniqueGlobalName (mangled_id, Hasht.find (!currentRenEnv) (mangled_id))
       handle Subscript => fatalError
         ("lookupRenEnv: unknown variable: " ^ showQualId q))
    else
      ({qual=qual,id = [mangled_id]}, 0)
  end
;

fun updateCurrentRenEnv RE =
  app (fn (x,y) => Hasht.insert (!currentRenEnv) x y)
      (rev RE)
;

fun renameId id = (id, newValStamp());

(* Generating lambda expressions from access pahts *)
local  (* cvr: TODO copied from Front.sml ---- code should be shared *)
    fun mkDynexn exnname =
	Lprim(Pmakeblock (CONtag(0,1)),
	      [exnname, Lconst constUnit])
    val bindExn  = 
	mkDynexn (Lprim(Pget_global ({qual="General", id=["exn_bind"]}, 0), []));
    val bindRaiser  = Lprim(Praise, [bindExn])
in
fun translatePath depth = fn
    Path_rec i => 
	Llet([Lprim(Pfield 0,[Lvar (depth-(i+1))])],
	     Lswitch(2,Lvar 0,
		     [(CONtag(0,2),bindRaiser),
		      (CONtag(1,2),Lprim(Pfield 0,[Lvar 0]))]))
  | Path_local i => Lvar (depth-(i+1))
  | Path_global uid => Lprim(Pget_global uid, [])
  | Path_virtual_son(arity, p) =>
      translatePath depth p
  | Path_son(n, p)
      => Lprim(Pfield n, [translatePath depth p])
end

fun translateTopOfPath depth = fn
    Path_virtual_son(arity, Path_local i) =>
      let val lvar = Lvar (depth-(i+1)) in
        Lprim(Pmakeblock(CONtag(0,1)),
              tabulate(arity, (fn n => Lprim(Pfield n, [lvar]))))
      end
  | Path_virtual_son(arity, p) =>
      Llet([translatePath depth p],
        Lprim(Pmakeblock(CONtag(0,1)),
              tabulate(arity, (fn n => Lprim(Pfield n, [Lvar 0])))))
  | p => translatePath depth p
;

fun translateLocalAccess asId (rho, depth) id =
  translateTopOfPath depth (lookupEnv rho (asId id))
  handle Subscript =>
    fatalError ("translateLocalAccess"^id)
;

fun lookupInLocalEnv asId env q =
  let val {qual,id} = q in (* cvr: TODO handle long ids *)
    if qual = "" orelse qual = currentUnitName() then
      lookupEnv env (asId (longIdentAsIdent id "lookupInLocalEnv"))
    else
      raise Subscript
  end
;

fun translateAccess asId (rho, depth) q =
  translateTopOfPath depth (lookupInLocalEnv asId rho q)
  handle Subscript =>
    Lprim(Pget_global (lookupRenEnv asId q), [])
;

(* cvr: added *)

fun translateLongAccess asId env (ii:IdInfo) = 
    let val {info={idKind,idFields,...}, ...} = ii
        val {qualid, ...} = !idKind
    in 
    case qualid of 
       {qual,id=[]} => Lstruct []
    |  {qual,id=[_]} => translateAccess asId env qualid
    |  {qual,id} => 
         let fun trLongAccess [id] _ = 
                    translateAccess ModId env {qual = qual,id = [id]}
               | trLongAccess (id::ids) (field::fields) = 
                    Lprim(Pfield field, [trLongAccess ids fields])
               | trLongAccess _ _ = fatalError "trLongAccess"
         in trLongAccess id (!idFields)
         end
    end;


fun translateExName env (ii : IdInfo) =
  let val {qualid, info} = ii in
    case #info(!(#idKind info)) of
        EXCONik _ =>
          translateLongAccess ValId env ii
      | _ => fatalError "translateExName"
  end;

fun pair x y = (x, y);

fun pathsOfPatAcc path ((loc, pat') : Pat) acc =
  case pat' of
    SCONpat _ => acc
  | VARpat ii =>
      bindInEnv acc (ValId (hd(#id(#qualid ii)))) path
  | WILDCARDpat => acc
  | NILpat _ => acc
  | CONSpat(ii, p) =>
      let val ci = getConInfo ii in
        if #conSpan(!ci) = 1 then
          pathsOfPatAcc path p acc
        else if #conIsGreedy(!ci) then
          (if #conTag(!ci) = 0 then
             pathsOfPatAcc path p acc
           else
             pathsOfPatAcc (Path_virtual_son(#conArity(!ci), path)) p acc)
        else
          pathsOfPatAcc (Path_son(0, path)) p acc
      end
  | EXNILpat _       => acc
  | EXCONSpat(ii, p) => pathsOfPatAcc (Path_son(1, path)) p acc
  | EXNAMEpat _      => fatalError "pathsOfPatAcc"
  | REFpat p         => pathsOfPatAcc (Path_son(0, path)) p acc
  | RECpat(ref (TUPLErp ps)) =>
      foldR (fn(i,p) => fn acc => pathsOfPatAcc (Path_son(i,path)) p acc)
            acc (mapFrom pair 0 ps)
  | RECpat(ref (RECrp _)) =>
      fatalError "pathsOfPatAcc: unresolved record pattern"
  | VECpat ps =>
      foldR (fn(i,p) => fn acc => pathsOfPatAcc (Path_son(i,path)) p acc)
            acc (mapFrom pair 0 ps)
  | INFIXpat _ => fatalError "pathsOfPatAcc"
  | PARpat p         => pathsOfPatAcc path p acc
  | TYPEDpat(p, _)   => pathsOfPatAcc path p acc
  | LAYEREDpat(p1, p2) =>
      pathsOfPatAcc path p1 (pathsOfPatAcc path p2 acc)
;

fun pathsOfPat path pat = pathsOfPatAcc path pat NILenv;

fun mutableVarsOfPatAcc ((loc, pat') : Pat) acc =
  case pat' of
    SCONpat _ => acc
  | VARpat _ => acc
  | WILDCARDpat => acc
  | NILpat _ => acc
  | CONSpat(_, p) =>
      mutableVarsOfPatAcc p acc
  | EXNILpat _ => acc
  | EXCONSpat(ii, p) =>
      mutableVarsOfPatAcc p acc
  | EXNAMEpat _ => fatalError "mutableVarsOfPatAcc"
  | REFpat p =>
      domPatAcc p acc
  | RECpat(ref (TUPLErp ps)) =>
      foldR mutableVarsOfPatAcc acc ps
  | RECpat(ref (RECrp _)) =>
      fatalError "mutableVarsOfPatAcc: unresolved record pattern"
  | VECpat ps =>
      foldR mutableVarsOfPatAcc acc ps
  | INFIXpat _ => fatalError "mutableVarsOfPatAcc"
  | PARpat p =>
      mutableVarsOfPatAcc p acc
  | TYPEDpat(p, _) =>
      mutableVarsOfPatAcc p acc
  | LAYEREDpat(p1, p2) =>
      mutableVarsOfPatAcc p1 (mutableVarsOfPatAcc p2 acc)
;

(* Since the program is supposed to be well-typed, *)
(* the patterns in a `val rec' can't contain mutable variables. *)
(* Thus there's no danger in accessing variable values via *)
(* their access paths... *)

fun mkEnvOfRecPats depth pats =
  foldL (fn pat => fn (rho, depth) =>
           (pathsOfPatAcc (Path_local depth) pat rho, depth+1))
        (NILenv, depth) pats
;

(* If a `val' declaration isn't recursive, the mutable variables *)
(* appearing in its patterns must be taken special care of... *)

fun mutableVarsOfPat pat = mutableVarsOfPatAcc pat [];

fun addLetsToEnv varlist (env as (rho, depth)) =
  case varlist of
    [] => env
  | var::rest =>
      addLetsToEnv rest (bindInEnv rho (ValId var) (Path_local depth), depth+1)
;

fun addLetsToExp varlist (rho, depth) exp =
  case varlist of
      [] => exp
    | _ =>
       Llet(mapFrom (fn i => fn var => translateLocalAccess ValId (rho, i) var)
                    depth varlist,
           exp)
;

fun mkEnvOfPats depth pats =
  let val env' = mkEnvOfRecPats depth pats
      val mut_vars = foldR mutableVarsOfPatAcc [] pats
  in (addLetsToEnv mut_vars env', addLetsToExp mut_vars env') end
;
