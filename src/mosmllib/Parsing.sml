(* Internal interface to the parsing engine *)

open Vector Obj Lexing;

type parseTables =
    (* actions *)    (unit -> obj) vector  *
    (* transl *)     int vector *
    (* lhs *)        string *
    (* len *)        string *
    (* defred *)     string *
    (* dgoto *)      string *
    (* sindex *)     string *
    (* rindex *)     string *
    (* gindex *)     string *
    (* tablesize *)  int *
    (* table *)      string *
    (* check *)      string
;

prim_val getActions   : parseTables -> (unit -> obj) vector = 1 "field0";
prim_val getTransl    : parseTables -> int vector           = 1 "field1";

exception yyexit of obj;
exception ParseError of (obj -> bool);

type parserEnv =
  (* sStack *)          int vector *  (* States *)
  (* vStack  *)         obj vector *  (* Semantic attributes *)
  (* symbStartStack  *) int vector *  (* Start positions *)
  (* symbEndStack *)    int vector *  (* End positions *)
  (* stackSize  *)      int *         (* Size of the stacks *)
  (* currChar  *)       int *         (* Last token read *)
  (* LVal  *)           obj *         (* Its semantic attribute *)
  (* symbStart  *)      int *         (* Start pos. of the current symbol*)
  (* symbEnd  *)        int *         (* End pos. of the current symbol *)
  (* SP  *)             int *         (* The stack pointer *)
  (* ruleLen  *)        int *         (* Number of rhs items in the rule *)
  (* ruleNumber *)      int           (* Rule number to reduce by *)
;

prim_val getSStack         : parserEnv -> int vector = 1 "field0";
prim_val getVStack         : parserEnv -> obj vector = 1 "field1";
prim_val getSymbStartStack : parserEnv -> int vector = 1 "field2";
prim_val getSymbEndStack   : parserEnv -> int vector = 1 "field3";
prim_val getStackSize      : parserEnv -> int        = 1 "field4";
prim_val getCurrChar       : parserEnv -> int        = 1 "field5";
prim_val getLVal           : parserEnv -> obj        = 1 "field6";
prim_val getSymbStart      : parserEnv -> int        = 1 "field7";
prim_val getSymbEnd        : parserEnv -> int        = 1 "field8";
prim_val getSP             : parserEnv -> int        = 1 "field9";
prim_val getRuleLen        : parserEnv -> int        = 1 "field10";
prim_val getRuleNumber     : parserEnv -> int        = 1 "field11";

prim_val setSStack         : parserEnv -> int vector -> unit = 2 "setfield0";
prim_val setVStack         : parserEnv -> obj vector -> unit = 2 "setfield1";
prim_val setSymbStartStack : parserEnv -> int vector -> unit = 2 "setfield2";
prim_val setSymbEndStack   : parserEnv -> int vector -> unit = 2 "setfield3";
prim_val setStackSize      : parserEnv -> int       -> unit = 2 "setfield4";
prim_val setCurrChar       : parserEnv -> int       -> unit = 2 "setfield5";
prim_val setLVal           : parserEnv -> obj       -> unit = 2 "setfield6";
prim_val setSymbStart      : parserEnv -> int       -> unit = 2 "setfield7";
prim_val setSymbEnd        : parserEnv -> int       -> unit = 2 "setfield8";
prim_val setSP             : parserEnv -> int       -> unit = 2 "setfield9";
prim_val setRuleLen        : parserEnv -> int       -> unit = 2 "setfield10";
prim_val setRuleNumber     : parserEnv -> int       -> unit = 2 "setfield11";

datatype parserInput =
    Semantic_action_computed
  | Stacks_grown_1
  | Stacks_grown_2
  | Start
  | Token_read

and parserOutput =
    Compute_semantic_action    
  | Grow_stacks_1
  | Grow_stacks_2
  | Raise_parse_error
  | Read_token
;

prim_val parseEngine :
    parseTables -> parserEnv -> parserInput -> obj -> parserOutput
    = 4 "parse_engine"
;

prim_val vector_ : int -> '_a -> '_a vector       = 2 "make_vect";
prim_val sub_    : 'a vector -> int -> 'a         = 2 "get_vect_item";
prim_val update_ : 'a vector -> int -> 'a -> unit = 3 "set_vect_item";

(* The parsing engine *)

val env : parserEnv =
  ( vector_ 100 0,
    vector_ 100 (repr ()),
    vector_ 100 0,
    vector_ 100 0,
    100, 0, repr (), 0, 0, 0, 0, 0 )
;

fun copyStack oldS newS oldsize =
  let fun h i =
        if i < 0 then ()
        else (update_ newS i (sub_ oldS i); h (i-1))
  in h (oldsize - 1) end
;

fun clearStack stack size v =
  let fun h i =
        if i < 0 then ()
        else (update_ stack i v; h (i-1))
  in h (size - 1) end
;

fun growStacks() =
  let
    val oldsize   = getStackSize env
    val newsize   = oldsize * 2
    val new_s     = vector_ newsize 0
    val new_v     = vector_ newsize (repr ())
    val new_start = vector_ newsize 0
    val new_end   = vector_ newsize 0
  in
    copyStack (getSStack env) new_s oldsize;
    setSStack env new_s;
    copyStack (getVStack env) new_v oldsize;
    setVStack env new_v;
    copyStack (getSymbStartStack env) new_start oldsize;
    setSymbStartStack env new_start;
    copyStack (getSymbEndStack env) new_end oldsize;
    setSymbEndStack env new_end;
    setStackSize env newsize
  end
;

fun clearParser() =
  (clearStack (getVStack env) (getStackSize env) (repr ());
   setLVal env (repr ()))
;

fun yyparse (tables : parseTables) start lexer lexbuf =
  let
    fun loop cmd arg =
    case (parseEngine tables env cmd arg) of
      Read_token =>
        let val t = repr(lexer lexbuf) in
          setSymbStart env (getLexAbsPos lexbuf + getLexStartPos lexbuf);
          setSymbEnd env  (getLexAbsPos lexbuf + getLexCurrPos lexbuf);
          loop Token_read t
        end
    | Raise_parse_error =>
        let val c = getCurrChar env in
          raise ParseError
            (fn tok => sub_ (getTransl tables) (obj_tag tok) = c)
        end
    | Compute_semantic_action =>
        loop Semantic_action_computed
          (sub_ (getActions tables) (getRuleNumber env) ())
    | Grow_stacks_1 =>
        (growStacks(); loop Stacks_grown_1 (repr ()))
    | Grow_stacks_2 =>
        (growStacks(); loop Stacks_grown_2 (repr ()))
  in
    setCurrChar env start;
    setSP env 0;
    (loop Start (repr ()) handle yyexit v => magic_obj v)
  end
;

fun peekVal n =
  magic_obj (sub_ (getVStack env) (getSP env - n))
;

fun symbolStart() =
  sub_ (getSymbStartStack env) (getSP env - getRuleLen env + 1)
;

fun symbolEnd() =
  sub_ (getSymbEndStack env) (getSP env)
;

fun itemStart n =
  sub_ (getSymbStartStack env) (getSP env - (getRuleLen env - n))
;

fun itemEnd n =
  sub_ (getSymbEndStack env) (getSP env - (getRuleLen env - n))
;
