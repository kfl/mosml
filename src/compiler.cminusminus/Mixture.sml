open BasicIO Nonstdio Fnlib Config PP;

exception Toplevel;
exception EndOfFile;
exception LexicalError of string * int * int;

val toplevel = ref false;

val pp_out = mk_ppstream
{
  linewidth = 79,
  flush     = fn() => flush_out std_out,
  consumer  = outputc std_out
};

fun msgCBlock offset = begin_block pp_out CONSISTENT offset;
fun msgIBlock offset = begin_block pp_out INCONSISTENT offset;
fun msgEBlock() = end_block pp_out;
fun msgBreak size_offset = add_break pp_out size_offset;
fun msgClear() = clear_ppstream pp_out;
fun msgFlush() = flush_ppstream pp_out;

val msgString = add_string pp_out;

fun msgChar (i : char) = msgString (String.str i);
local 
    prim_val sml_string_of_int    : int  -> string = 1 "sml_string_of_int";
    prim_val sml_string_of_float  : real -> string = 1 "sml_string_of_float";
    prim_val sml_hexstring_of_word : word -> string = 1 "sml_hexstring_of_word";
in
    fun msgInt  (i : int)  = msgString (sml_string_of_int i);
    fun msgReal (r : real) = msgString (sml_string_of_float r);
    fun msgWord (w : word) = msgString (sml_hexstring_of_word w);
end

fun msgEOL() = add_newline pp_out;

fun msgPrompt s =
  (if !toplevel then msgString toplevel_output_prompt
                else msgString batch_output_prompt;
  msgString s)
;

fun msgContPrompt s =
  (if !toplevel then msgString toplevel_output_cont_prompt
                else msgString batch_output_cont_prompt;
  msgString s)
;

fun errPrompt s =
  (if !toplevel then msgString toplevel_error_prompt
                else msgString batch_error_prompt;
  msgString s)
;

val msgStyle = ref "default";

(* Handling files and directories *)

val path_library = ref "";
val load_path = ref ([] : string list);

(* This MUST be ref false; the default (value polymorphism/imperative types) 
 * is set in files Mainc.sml and Maint.sml instead: 
 *)
val value_polymorphism = ref false;

fun cannot_find filename =
  raise (Fail ("Cannot find file "^filename))
;

fun find_in_path filename =
  if file_exists filename then
    filename
  else if Filename.is_absolute filename then
    cannot_find filename
  else
    let fun h [] =
              cannot_find filename
          | h (a::rest) =
              let val b = Filename.concat a filename in
                if file_exists b then b else h rest
              end
    in h (!load_path) end
;

fun remove_file f =
  Miscsys.remove f
    handle SysErr _ => ()
;

(* ---------- *)

datatype Lab =
    INTlab of int
  | STRINGlab of string
;

type 'a Row = (Lab * 'a) list;

fun printLab (STRINGlab s) = msgString s
  | printLab (INTlab i) = msgInt i
;

val labOne = INTlab 1
and labTwo = INTlab 2
;

fun isPairRow [(INTlab 1, _), (INTlab 2, _)] = true
  | isPairRow [(INTlab 2, _), (INTlab 1, _)] = true
  | isPairRow _ = false
;

fun isTupleRow' n [] = true
  | isTupleRow' n (((INTlab i), _) :: fs) =
      if n = i then isTupleRow' (n+1) fs else false
  | isTupleRow' n _ = false

fun isTupleRow fs =
  (List.length fs <> 1) andalso (isTupleRow' 1 fs)
;

fun mkPairRow x1 x2 = [(labOne, x1), (labTwo, x2)];

fun mkTupleRow' n [] = []
  | mkTupleRow' n (x :: xs) =
      (INTlab n, x) :: mkTupleRow' (n+1) xs
;

fun mkTupleRow xs = mkTupleRow' 1 xs;

fun lt_lab (STRINGlab s1) (STRINGlab s2)  = s1 < s2
  | lt_lab (STRINGlab _)  (INTlab _)      = true
  | lt_lab (INTlab _)     (STRINGlab _)   = false
  | lt_lab (INTlab i1)    (INTlab i2)     = i1 < i2
;

fun insertField (lab, x) fields =
  case fields of
      [] => [(lab, x)]
    | (lab', x') :: rest =>
        if lt_lab lab lab' then
          (lab, x) :: fields
        else if lt_lab lab' lab then
          (lab', x') :: insertField (lab, x) rest
        else
          fatalError "insertField"
;

fun sortRow row = foldL insertField [] row;

(* --- Local environments --- *)

datatype ('a, 'b) Env
  = NILenv
  | BNDenv of 'a * 'b * ('a, 'b) Env
  | TOPenv of ('a, 'b) Hasht.t * ('a, 'b) Env
  | COMPenv of ('a, 'b) Env * ('a, 'b) Env
;

fun plusEnv NILenv env2   = env2
  | plusEnv env1   NILenv = env1
  | plusEnv env1   (BNDenv(k, v, NILenv)) = BNDenv(k, v, env1)
  | plusEnv env1   env2   = COMPenv(env2, env1)
;

fun lookupEnv env key =
  let val rec search = fn
       NILenv => raise Subscript
     | BNDenv(k, v, env) =>
	   if key = k then v else search env
     | TOPenv(x, env) =>
	   (Hasht.find x key handle Subscript => search env)
     | COMPenv(env1, env2) =>
	   (search env1 handle Subscript => search env2)
  in search env end
;

fun bindInEnv env k v = BNDenv(k, v, env);
fun bindTopInEnv env x = TOPenv(x, env);

fun mk1Env k v = BNDenv(k, v, NILenv);
fun mk1TopEnv x = TOPenv(x, NILenv);

fun revEnvAcc NILenv acc = acc
  | revEnvAcc (BNDenv(k, v, env)) acc =
      revEnvAcc env (BNDenv(k, v, acc))
  | revEnvAcc (TOPenv(x, env)) acc =
      revEnvAcc env (TOPenv(x, acc))
  | revEnvAcc (COMPenv(env1, env2)) acc =
      revEnvAcc env2 (revEnvAcc env1 acc)
;

fun revEnv env = revEnvAcc env NILenv;

fun traverseEnv action env =
  let fun traverse NILenv = ()
        | traverse (BNDenv(k, v, env)) =
            (action k v; traverse env)
        | traverse (TOPenv(x, env)) =
            (Hasht.apply action x; traverse env)
        | traverse (COMPenv(env1, env2)) =
            (traverse env1; traverse env2)
  in traverse env end
;

fun mapEnv f env0 =
  case env0 of
      NILenv => NILenv
    | BNDenv(k, v, env) =>
        BNDenv(k, f k v, mapEnv f env)
    | TOPenv(x, env) =>
	(* This can be improved by simply making a copy of the hash table *)
	let val newx = Hasht.new 17
	    fun ins k v = Hasht.insert newx k (f k v)
	in 
            Hasht.apply ins x; 
	    TOPenv(newx, mapEnv f env) 
	end
    | COMPenv(env1, env2) =>
        COMPenv(mapEnv f env1, mapEnv f env2)
;

fun foldEnv f u env0 =
  case env0 of
      NILenv => u
    | BNDenv(k, v, env) =>
        f k v (foldEnv f u env)
    | TOPenv(x, env) =>
	Hasht.fold f (foldEnv f u env) x
    | COMPenv(env1, env2) =>
        foldEnv f (foldEnv f u env2) env1
;

fun mkHashEnv n env =
    if n < 7 then env
    else
	let val hashenv = Hasht.new n
	in 
	    traverseEnv (Hasht.insert hashenv) (revEnv env);
	    mk1TopEnv hashenv
	end

fun cleanEnv env =
    foldEnv (fn k => fn v => fn cont =>
	       fn acc => 
	       if (member k acc) 
		   then  cont acc
	       else  bindInEnv (cont (k::acc)) k v)
            (fn acc => NILenv) env [];

local
    fun insertInEnv (k:string) v env =
	case env of
	    NILenv => BNDenv(k,v,NILenv)
	  | BNDenv(k', v', env') =>
		if k = k' then BNDenv(k,v,env')
		else if k < k' then BNDenv(k',v',insertInEnv k v env')
		     else BNDenv(k,v,env)
	  | _ => fatalError "insertInEnv"
in
    fun sortEnv env = foldEnv insertInEnv NILenv env
end;

fun lookupEnvWithPos size env key  =
  (* cvr: assumes env is clean (no shadowed bindings) *)
  foldEnv (fn k => fn v => fn cont =>
             fn pos => if key = k then (pos,v) else cont (pos+size(v)))
          (fn pos => raise Subscript) env;

(* --- Stamps of compiled signatures --- *)

type SigStamp = string;

val char_star = Char.chr 42;
val dummySigStamp = CharVector.tabulate(22, fn _ => char_star);

(* This table is used by `load' to prevent mismatching *)
(* versions of compiled units from being loaded, and also *)
(* to prevent the same unit from being loaded twice. *)

val watchDog = ref (Hasht.new 17 : (string, SigStamp) Hasht.t);

(* The list of automatically preloaded units. *)
(* Some of them are also preopened. *)

val preloadedUnits = ref ([] : string list);
val preopenedPreloadedUnits = ref ([] : string list);

(* compilation mode *)

datatype Mode = STRmode | TOPDECmode
;

val currentMode = ref STRmode
;

(* vanilla SML compliance levels *)

datatype Compliance = 
    Orthodox (* SML only, reject extensions *)
  | Conservative (* warn of any extensions *)
  | Liberal (* anything goes *);

val currentCompliance = ref Liberal; 


   







