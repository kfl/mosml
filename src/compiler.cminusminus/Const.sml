local
  open Obj Fnlib Config Mixture
in

(* Qualified identifiers *)

type QualifiedIdent =
{
  id: string list,
  qual: string 
};

(* cvr: REVISE uses of this function should eventually be removed once we introduce a proper
   distinction between short and long qualified idents *)

fun longIdentAsIdent [id] _ = id
  | longIdentAsIdent _ msg = fatalError msg
;

(* Constants *)

datatype SCon =
    INTscon of int
  | WORDscon of word
  | CHARscon of char
  | REALscon of real
  | STRINGscon of string
;

datatype BlockTag =
    CONtag of int * int             (* tag number & span *)
;

datatype StructConstant =
    ATOMsc of SCon
  | BLOCKsc of BlockTag * StructConstant list
  | QUOTEsc of obj ref
;

val constUnit =
    BLOCKsc(CONtag(0,1), [])
;

fun intOfAtom (INTscon i) = i
  | intOfAtom (WORDscon w) = (magic w) : int
  | intOfAtom (CHARscon c) = Char.ord c
  | intOfAtom _ = fatalError "intOfAtom"
;

fun intOfAbsoluteTag (CONtag(i,_)) = i
;

(* Id is used distinguish between name spaces 
   for compiled var, structure and functor values *)

datatype Id = ModId of string | ValId of string | FunId of string

(* (un)mangle valid's, modids and funids to disjoint subsets of string *)

val  mangle = fn
      ValId s => s
    | ModId s => "_"^s
    | FunId s => "__"^s;

val unmangle = fn s =>
    case Misc.explode s of
      ((#"_")::(#"_")::fid) => FunId (Misc.implode fid)
    | ((#"_")::mid) => ModId (Misc.implode mid)
    | vid => ValId (Misc.implode vid);




(* Printing structured constants for debugging purposes *)

fun printSeq printEl sep =
  let fun loop [] = ()
        | loop [x] = printEl x
        | loop (x :: xs) = (printEl x; msgString sep; loop xs)
  in loop end
;

local fun show_id [] = ""
        | show_id [i] =  i
        | show_id (modid::modids) = 
             (show_id modids) ^ "." ^ modid
in
fun showQualId {qual="", id=id} = show_id id
  | showQualId {qual=qual,  id=id} = qual ^ "." ^ show_id id
end;

local fun print_id [] = ()
        | print_id [i] = msgString i
        | print_id (i::id) = 
            (print_id id;  msgString "." ; msgString i)
in
fun printQualId {qual="", id=id} =
      print_id id
  | printQualId {qual=qual, id=id} =
      (msgString qual;  msgString "." ; print_id id)
end;

prim_val sml_makestring_of_char : char -> string
                              = 1 "sml_makestring_of_char";
prim_val sml_makestring_of_string : string -> string
                              = 1 "sml_makestring_of_string";

fun printSCon (INTscon i) =
      msgInt i
  | printSCon (WORDscon w) =
      msgWord w
  | printSCon (CHARscon c) =
      msgString (sml_makestring_of_char c)
  | printSCon (REALscon r) =
      msgReal r
  | printSCon (STRINGscon s) =
      msgString (sml_makestring_of_string s)
;

fun printCTag (CONtag(tag, span)) =
    (msgInt tag; msgString ":"; msgInt span)
;

fun printStrConst (ATOMsc scon) =
      printSCon scon
  | printStrConst (BLOCKsc(ct, consts)) =
      (msgString "(BLOCK "; printCTag ct; msgString " ";
       printSeq printStrConst " " consts; msgString ")")
  | printStrConst (QUOTEsc rv) =
      msgString "<const>"
;

end;

