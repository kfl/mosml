(* First, a number of bogus declarations *)

type info = unit;
type scon = unit
type longid = unit
type tycon = unit
type longtycon = unit
type longstrid = unit
type id = unit
type funid = unit
type strid = unit
type sigid = unit

datatype 'a op_opt = OP_OPT of 'a * bool
datatype 'a WithInfo = WITH_INFO of info * 'a

  datatype atexp =
	SCONatexp of info * scon |         
	IDENTatexp of info * longid op_opt |
	RECORDatexp of info * exprow option |
	LETatexp of info * dec * exp |
	PARatexp of info * exp

  and opid = OPID of longid * bool

  and exprow =
	EXPROW of info * lab * exp * exprow option

  and exp =
	ATEXPexp of info * atexp |
	APPexp of info * exp * atexp |
	TYPEDexp of info * exp * ty |
	HANDLEexp of info * exp * match |
        RAISEexp of info * exp |
	FNexp of info * match |
	UNRES_INFIXexp of info * atexp list

  and match =
        MATCH of info * mrule * match option

  and mrule =
        MRULE of info * pat * exp

  and dec = 
	VALdec of info * tyvar list * valbind |
	UNRES_FUNdec of info * tyvar list * FValBind |
	TYPEdec of info * typbind |
	DATATYPEdec of info * datbind |
	DATATYPE_REPLICATIONdec of info * tycon * longtycon |
	ABSTYPEdec of info * datbind * dec |
	EXCEPTIONdec of info * exbind |
	LOCALdec of info * dec * dec |
	OPENdec of info * longstrid WithInfo list |
	SEQdec of info * dec * dec |
	INFIXdec of info * int option * id list |
	INFIXRdec of info * int option * id list |
	NONFIXdec of info * id list |
	EMPTYdec of info

  and valbind =
	PLAINvalbind of info * pat * exp * valbind option |
	RECvalbind of info * valbind

  and FValBind = FVALBIND of info * FClause * FValBind option
  and FClause = FCLAUSE of info * atpat list * ty option * exp * FClause option

  and typbind =
        TYPBIND of info * tyvar list * tycon * ty * typbind option

  and datbind =
        DATBIND of info * tyvar list * tycon * conbind * datbind option

  and conbind =
        CONBIND of info * id op_opt * ty option * conbind option

  and exbind =
        EXBIND of info * id op_opt * ty option * exbind option |
        EXEQUAL of info * id op_opt * longid op_opt * exbind option

  and atpat =
        WILDCARDatpat of info |
	SCONatpat of info * scon |
	LONGIDatpat of info * longid op_opt |
	RECORDatpat of info * patrow option |
	PARatpat of info * pat

  and patrow =
        DOTDOTDOT of info |
        PATROW of info * lab * pat * patrow option

  and pat =
        ATPATpat of info * atpat |
        CONSpat of info * longid op_opt * atpat |
        TYPEDpat of info * pat * ty |
        LAYEREDpat of info * id op_opt * ty option * pat |
	UNRES_INFIXpat of info * atpat list

  and ty =
        TYVARty of info * tyvar |
        RECORDty of info * tyrow option |
        CONty of info * ty list * longtycon |
        FNty of info * ty * ty |
        PARty of info * ty

  and tyrow =
        TYROW of info * lab * ty * tyrow option

    datatype strexp =
      STRUCTstrexp of info * strdec |
      LONGSTRIDstrexp of info * longstrid |
      TRANSPARENT_CONSTRAINTstrexp of info * strexp * sigexp |
      OPAQUE_CONSTRAINTstrexp of info * strexp * sigexp |
      APPstrexp of info * funid * strexp |
      LETstrexp of info * strdec * strexp

    and strdec =
      DECstrdec of info * dec |
      STRUCTUREstrdec of info * strbind |
      LOCALstrdec of info * strdec * strdec |
      EMPTYstrdec of info |
      SEQstrdec of info * strdec * strdec

    and strbind =
      STRBIND of info * strid * strexp * strbind option

    and sigexp =
      SIGsigexp of info * spec |
      SIGIDsigexp of info * sigid |
      WHERE_TYPEsigexp of info * sigexp * tyvar list * longtycon * ty


    and sigdec =
      SIGNATUREsigdec of info * sigbind

    and sigbind =
      SIGBIND of info * sigid * sigexp * sigbind option

			    (* Figure 7 *)

    and spec =
      VALspec of info * valdesc |
      TYPEspec of info * typdesc |
      EQTYPEspec of info * typdesc |
      DATATYPEspec of info * datdesc |
      DATATYPE_REPLICATIONspec of info * tycon * longtycon |
      EXCEPTIONspec of info * exdesc |
      STRUCTUREspec of info * strdesc |
      INCLUDEspec of info * sigexp |
      SHARING_TYPEspec of info * spec * longtycon WithInfo list |
      SHARINGspec of info * spec * longstrid WithInfo list |
      EMPTYspec of info |
      SEQspec of info * spec * spec

    and valdesc =
      VALDESC of info * id * ty * valdesc option

    and typdesc =
      TYPDESC of info * tyvar list * tycon * typdesc option

    and datdesc =
      DATDESC of info * tyvar list * tycon * condesc * datdesc option

    and condesc =
      CONDESC of info * id * ty option * condesc option

    and exdesc =
      EXDESC of info * id * ty option * exdesc option

    and strdesc =
      STRDESC of info * strid * sigexp * strdesc option

			    (* Figure 8 *)

    and fundec =
      FUNCTORfundec of info * funbind

    and funbind =
      FUNBIND of info * funid * strid * sigexp * strexp * funbind option

    and topdec =
      STRtopdec of info * strdec * topdec option |
      SIGtopdec of info * sigdec * topdec option |
      FUNtopdec of info * fundec * topdec option 

fun mk_IdentLab _ = let fun loop () = loop () in loop () end;
fun mk_IntegerLab _ = let fun loop () = loop () in loop () end;
fun PP _ = let fun loop () = loop () in loop () end;
fun topdecOfExp _ = let fun loop () = loop () in loop () end;
fun rightmost _ = let fun loop () = loop () in loop () end;
fun mk_FunId _ = let fun loop () = loop () in loop () end;


type pos = int;

structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | TYVAR of unit ->  (string) | ID of unit ->  (string)
 | STRING of unit ->  (string) | REAL of unit ->  (string option)
 | WORD of unit ->  (int option) | DIGIT of unit ->  (int)
 | HEXINTEGER of unit ->  (int option)
 | DECNEGINTEGER of unit ->  (int option)
 | DECPOSINTEGER of unit ->  (int option)
 | QUAL_STAR of unit ->  (string list)
 | QUAL_ID of unit ->  (string list) | Char of unit ->  (int)
 | Integer of unit ->  (int) | DecPosInteger of unit ->  (int)
 | Label of unit ->  (lab)
 | LongOpEqIdent of unit ->  (string list op_opt)
 | LongOpIdent of unit ->  (string list op_opt)
 | LongIdent of unit ->  (string list)
 | LongTypeIdent of unit ->  (string list)
 | TypeIdent of unit ->  (string) | EqIdent of unit ->  (string)
 | OpIdent of unit ->  (string op_opt) | Ident of unit ->  (string)
 | OneDec_sans_LOCAL of unit ->  (dec) | OneDec of unit ->  (dec)
 | DIGIT_opt of unit ->  (int option)
 | LongTypeIdentEq_seq2 of unit ->  (string list WithInfo list)
 | LongIdentEq_seq2 of unit ->  (string list WithInfo list)
 | LongIdent_seq1 of unit ->  (string list WithInfo list)
 | EqIdent_seq1 of unit ->  (string list)
 | Ident_seq2 of unit ->  (string WithInfo list)
 | NonEmptyDec of unit ->  (dec)
 | OneDec_or_SEMICOLON of unit ->  (dec option)
 | TyComma_seq2 of unit ->  (ty list)
 | TyVarComma_seq1 of unit ->  (tyvar list)
 | TyVarSeq1 of unit ->  (tyvar list)
 | ExpSemicolon_seq2 of unit ->  (exp list)
 | AtPat_seq1 of unit ->  (atpat list)
 | AtPat_seq2 of unit ->  (atpat list)
 | AtExp_seq1 of unit ->  (atexp list)
 | PatComma_seq2 of unit ->  (pat list)
 | PatComma_seq1 of unit ->  (pat list)
 | PatComma_seq0 of unit ->  (pat list)
 | ExpComma_seq2 of unit ->  (exp list)
 | ExpComma_seq1 of unit ->  (exp list)
 | ExpComma_seq0 of unit ->  (exp list)
 | OfTy_opt of unit ->  (ty option)
 | CommaTyRow_opt of unit ->  (tyrow option)
 | TyRow_opt of unit ->  (tyrow option)
 | ColonTy_seq1 of unit ->  ( ( ty * pos )  list)
 | ColonTy_opt of unit ->  (ty option)
 | CommaPatRow_opt of unit ->  (patrow option)
 | AsPat_opt of unit ->  (pat option)
 | PatRow_opt of unit ->  (patrow option)
 | ExpRow_opt of unit ->  (exprow option)
 | AndExBind_opt of unit ->  (exbind option)
 | BarMatch_opt of unit ->  (match option)
 | BarConBind_opt of unit ->  (conbind option)
 | AndDatBind_opt of unit ->  (datbind option)
 | AndTypBind_opt of unit ->  (typbind option)
 | BarFClause_opt of unit ->  (FClause option)
 | AndFValBind_opt of unit ->  (FValBind option)
 | AndFnValBind_opt of unit ->  (valbind option)
 | AndValBind_opt of unit ->  (valbind option)
 | CommaExpRow_opt of unit ->  (exprow option)
 | FClause of unit ->  (FClause) | AtExp of unit ->  (atexp)
 | ExpRow of unit ->  (exprow) | Match_ of unit ->  (match)
 | Exp_ of unit ->  (exp) | MRule of unit ->  (mrule)
 | Dec of unit ->  (dec) | FValBind of unit ->  (FValBind)
 | FnValBind of unit ->  (valbind) | ValBind of unit ->  (valbind)
 | TypBind of unit ->  (typbind)
 | DatBind_nonzero_arity of unit ->  (datbind)
 | DatBind_zero_arity of unit ->  (datbind)
 | DatBind of unit ->  (datbind) | ConBind of unit ->  (conbind)
 | ExBind of unit ->  (exbind) | AtPat of unit ->  (atpat)
 | PatRow of unit ->  (patrow) | Pat of unit ->  (pat)
 | SCon of unit ->  (scon) | TyVarSeq of unit ->  (tyvar list)
 | AtomicTy of unit ->  (ty) | Ty_sans_STAR of unit ->  (ty)
 | TupleTy of unit ->  (ty list) | Ty of unit ->  (ty)
 | TyRow of unit ->  (tyrow)
 | AndWhereDesc_opt of unit ->  ( ( pos * sigexp -> sigexp )  option)
 | WhereDesc of unit ->  (pos*sigexp -> sigexp)
 | NonEmptyStrDec of unit ->  (strdec)
 | OneStrDec_or_SEMICOLON of unit ->  (strdec option)
 | NonEmptySpec' of unit ->  (spec)
 | Spec_sans_SHARING of unit ->  (spec)
 | NonEmptySpec of unit ->  (spec)
 | OneSpec_or_SEMICOLON of unit ->  (spec option)
 | SEMICOLON_opt of unit ->  (unit)
 | AndValDesc_opt of unit ->  (valdesc option)
 | TypAbbreviationAND_seq1 of unit ->  ( ( tyvar list * tycon * ty * info * info )  list)
 | AndTypDesc_opt of unit ->  (typdesc option)
 | AndDatDesc_opt of unit ->  (datdesc option)
 | BarConDesc_opt of unit ->  (condesc option)
 | AndExDesc_opt of unit ->  (exdesc option)
 | AndStrDesc_opt of unit ->  (strdesc option)
 | AndSigBind_opt of unit ->  (sigbind option)
 | AndStrBind_opt of unit ->  (strbind option)
 | AndFunBind_opt of unit ->  (funbind option)
 | SigExp_constraint_maybe of unit ->  (pos -> strexp -> strexp)
 | SigExp_constraint of unit ->  (pos -> strexp -> strexp)
 | StrExp of unit ->  (strexp) | StrDec of unit ->  (strdec)
 | StrBind of unit ->  (strbind) | SigExp of unit ->  (sigexp)
 | SigBind of unit ->  (sigbind) | OneSpec of unit ->  (spec)
 | Spec of unit ->  (spec) | ValDesc of unit ->  (valdesc)
 | TypAbbreviation of unit ->  (tyvar list*tycon*ty*info*info)
 | TypDesc of unit ->  (typdesc)
 | DatDesc_nonzero_arity of unit ->  (datdesc)
 | DatDesc_zero_arity of unit ->  (datdesc)
 | DatDesc of unit ->  (datdesc) | ConDesc of unit ->  (condesc)
 | ExDesc of unit ->  (exdesc) | StrDesc of unit ->  (strdesc)
 | FunBind of unit ->  (funbind) | OneStrDec of unit ->  (strdec)
 | FunDec of unit ->  (fundec)
 | StrDec_sans_SEMICOLON of unit ->  (strdec)
 | SigDec of unit ->  (sigdec)
 | TopDec_opt of unit ->  (topdec option)
 | TopDec_ of unit ->  (topdec) | TopDec of unit ->  (topdec)
end


structure LrTable = struct
   datatype t = NT of int
end

structure Actions =
struct 
exception mlyAction of int


(* cvr: begin *)
local in
val actions = 
fn (i392,defaultPos,stack,
    (())) =>
case (i392,stack)
of (0,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.Ident(fn _ => let val ID as ID1=ID1 ()
 in ( ID ) end
)
 in (LrTable.NT 113,(result,ID1left,ID1right),rest671) end
| (1,(_,(_,STAR1left,STAR1right))::rest671) => let val result=
MlyValue.Ident(fn _ => ( "*" ))
 in (LrTable.NT 113,(result,STAR1left,STAR1right),rest671) end
| (2,(_,(MlyValue.Ident Ident1,Ident1left,Ident1right))::rest671) => 
let val result=MlyValue.OpIdent(fn _ => let val Ident as Ident1=Ident1
 ()
 in ( OP_OPT(Ident, false) ) end
)
 in (LrTable.NT 114,(result,Ident1left,Ident1right),rest671) end
| (3,(_,(MlyValue.Ident Ident1,_,Ident1right))::(_,(_,OP1left,_))::
rest671) => let val result=MlyValue.OpIdent(fn _ => let val Ident as 
Ident1=Ident1 ()
 in ( OP_OPT(Ident, true) ) end
)
 in (LrTable.NT 114,(result,OP1left,Ident1right),rest671) end
| (4,(_,(MlyValue.Ident Ident1,Ident1left,Ident1right))::rest671) => 
let val result=MlyValue.EqIdent(fn _ => let val Ident as Ident1=Ident1
 ()
 in ( Ident ) end
)
 in (LrTable.NT 115,(result,Ident1left,Ident1right),rest671) end
| (5,(_,(_,EQUALS1left,EQUALS1right))::rest671) => let val result=
MlyValue.EqIdent(fn _ => ( "=" ))
 in (LrTable.NT 115,(result,EQUALS1left,EQUALS1right),rest671) end
| (6,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.TypeIdent(fn _ => let val ID as ID1=ID1 ()
 in ( ID ) end
)
 in (LrTable.NT 116,(result,ID1left,ID1right),rest671) end
| (7,(_,(MlyValue.TypeIdent TypeIdent1,TypeIdent1left,TypeIdent1right)
)::rest671) => let val result=MlyValue.LongTypeIdent(fn _ => let val 
TypeIdent as TypeIdent1=TypeIdent1 ()
 in ( [TypeIdent] ) end
)
 in (LrTable.NT 117,(result,TypeIdent1left,TypeIdent1right),rest671)
 end
| (8,(_,(MlyValue.QUAL_ID QUAL_ID1,QUAL_ID1left,QUAL_ID1right))::
rest671) => let val result=MlyValue.LongTypeIdent(fn _ => let val 
QUAL_ID as QUAL_ID1=QUAL_ID1 ()
 in ( QUAL_ID ) end
)
 in (LrTable.NT 117,(result,QUAL_ID1left,QUAL_ID1right),rest671) end
| (9,(_,(MlyValue.Ident Ident1,Ident1left,Ident1right))::rest671) => 
let val result=MlyValue.LongIdent(fn _ => let val Ident as Ident1=
Ident1 ()
 in ( [Ident] ) end
)
 in (LrTable.NT 118,(result,Ident1left,Ident1right),rest671) end
| (10,(_,(MlyValue.QUAL_ID QUAL_ID1,QUAL_ID1left,QUAL_ID1right))::
rest671) => let val result=MlyValue.LongIdent(fn _ => let val QUAL_ID
 as QUAL_ID1=QUAL_ID1 ()
 in ( QUAL_ID ) end
)
 in (LrTable.NT 118,(result,QUAL_ID1left,QUAL_ID1right),rest671) end
| (11,(_,(MlyValue.QUAL_STAR QUAL_STAR1,QUAL_STAR1left,QUAL_STAR1right
))::rest671) => let val result=MlyValue.LongIdent(fn _ => let val 
QUAL_STAR as QUAL_STAR1=QUAL_STAR1 ()
 in ( QUAL_STAR ) end
)
 in (LrTable.NT 118,(result,QUAL_STAR1left,QUAL_STAR1right),rest671)
 end
| (12,(_,(MlyValue.LongIdent LongIdent1,LongIdent1left,LongIdent1right
))::rest671) => let val result=MlyValue.LongOpIdent(fn _ => let val 
LongIdent as LongIdent1=LongIdent1 ()
 in ( OP_OPT(LongIdent, false) ) end
)
 in (LrTable.NT 119,(result,LongIdent1left,LongIdent1right),rest671)
 end
| (13,(_,(MlyValue.LongIdent LongIdent1,_,LongIdent1right))::(_,(_,
OP1left,_))::rest671) => let val result=MlyValue.LongOpIdent(fn _ => 
let val LongIdent as LongIdent1=LongIdent1 ()
 in ( OP_OPT(LongIdent, true) ) end
)
 in (LrTable.NT 119,(result,OP1left,LongIdent1right),rest671) end
| (14,(_,(MlyValue.LongOpIdent LongOpIdent1,LongOpIdent1left,
LongOpIdent1right))::rest671) => let val result=MlyValue.LongOpEqIdent
(fn _ => let val LongOpIdent as LongOpIdent1=LongOpIdent1 ()
 in ( LongOpIdent ) end
)
 in (LrTable.NT 120,(result,LongOpIdent1left,LongOpIdent1right),
rest671) end
| (15,(_,(_,EQUALS1left,EQUALS1right))::rest671) => let val result=
MlyValue.LongOpEqIdent(fn _ => ( OP_OPT(["="], false) ))
 in (LrTable.NT 120,(result,EQUALS1left,EQUALS1right),rest671) end
| (16,(_,(_,_,EQUALS1right))::(_,(_,OP1left,_))::rest671) => let val 
result=MlyValue.LongOpEqIdent(fn _ => ( OP_OPT(["="], true) ))
 in (LrTable.NT 120,(result,OP1left,EQUALS1right),rest671) end
| (17,(_,(MlyValue.DIGIT DIGIT1,DIGIT1left,DIGIT1right))::rest671) => 
let val result=MlyValue.DIGIT_opt(fn _ => let val DIGIT as DIGIT1=
DIGIT1 ()
 in ( SOME DIGIT ) end
)
 in (LrTable.NT 110,(result,DIGIT1left,DIGIT1right),rest671) end
| (18,rest671) => let val result=MlyValue.DIGIT_opt(fn _ => ( NONE ))
 in (LrTable.NT 110,(result,defaultPos,defaultPos),rest671) end
| (19,(_,(MlyValue.DECPOSINTEGER DECPOSINTEGER1,DECPOSINTEGERleft as 
DECPOSINTEGER1left,DECPOSINTEGER1right))::rest671) => let val result=
MlyValue.DecPosInteger(fn _ => let val DECPOSINTEGER as DECPOSINTEGER1
=DECPOSINTEGER1 ()
 in (
 (* raise_lexical_error_if_none *)
			    DECPOSINTEGERleft; DECPOSINTEGER; raise Fail "bogus")
 end
)
 in (LrTable.NT 122,(result,DECPOSINTEGER1left,DECPOSINTEGER1right),
rest671) end
| (20,(_,(MlyValue.DIGIT DIGIT1,DIGIT1left,DIGIT1right))::rest671) => 
let val result=MlyValue.DecPosInteger(fn _ => let val DIGIT as DIGIT1=
DIGIT1 ()
 in ( DIGIT ) end
)
 in (LrTable.NT 122,(result,DIGIT1left,DIGIT1right),rest671) end
| (21,(_,(MlyValue.Ident Ident1,Ident1left,Ident1right))::rest671) => 
let val result=MlyValue.Label(fn _ => let val Ident as Ident1=Ident1 
()
 in ( mk_IdentLab Ident ) end
)
 in (LrTable.NT 121,(result,Ident1left,Ident1right),rest671) end
| (22,(_,(MlyValue.DecPosInteger DecPosInteger1,DecPosInteger1left,
DecPosInteger1right))::rest671) => let val result=MlyValue.Label(fn _
 => let val DecPosInteger as DecPosInteger1=DecPosInteger1 ()
 in ( mk_IntegerLab DecPosInteger ) end
)
 in (LrTable.NT 121,(result,DecPosInteger1left,DecPosInteger1right),
rest671) end
| (23,(_,(MlyValue.Ident_seq2 Ident_seq21,_,Ident_seq21right))::(_,(
MlyValue.Ident Ident1,Identleft as Ident1left,Identright))::rest671)
 => let val result=MlyValue.Ident_seq2(fn _ => let val Ident as Ident1
=Ident1 ()
val Ident_seq2 as Ident_seq21=Ident_seq21 ()
 in (
 WITH_INFO(PP Identleft Identright, Ident)
                          :: Ident_seq2
                        
) end
)
 in (LrTable.NT 105,(result,Ident1left,Ident_seq21right),rest671) end
| (24,(_,(MlyValue.Ident Ident2,Ident2left,Ident2right))::(_,(
MlyValue.Ident Ident1,Ident1left,Ident1right))::rest671) => let val 
result=MlyValue.Ident_seq2(fn _ => let val Ident1=Ident1 ()
val Ident2=Ident2 ()
 in (
 [WITH_INFO(PP Ident1left Ident1right, Ident1),
			   WITH_INFO(PP Ident2left Ident2right, Ident2)] 
) end
)
 in (LrTable.NT 105,(result,Ident1left,Ident2right),rest671) end
| (25,(_,(MlyValue.EqIdent_seq1 EqIdent_seq11,_,EqIdent_seq11right))::
(_,(MlyValue.EqIdent EqIdent1,EqIdent1left,_))::rest671) => let val 
result=MlyValue.EqIdent_seq1(fn _ => let val EqIdent as EqIdent1=
EqIdent1 ()
val EqIdent_seq1 as EqIdent_seq11=EqIdent_seq11 ()
 in ( EqIdent :: EqIdent_seq1 ) end
)
 in (LrTable.NT 106,(result,EqIdent1left,EqIdent_seq11right),rest671)
 end
| (26,(_,(MlyValue.EqIdent EqIdent1,EqIdent1left,EqIdent1right))::
rest671) => let val result=MlyValue.EqIdent_seq1(fn _ => let val 
EqIdent as EqIdent1=EqIdent1 ()
 in ( [EqIdent] ) end
)
 in (LrTable.NT 106,(result,EqIdent1left,EqIdent1right),rest671) end
| (27,(_,(MlyValue.LongIdent_seq1 LongIdent_seq11,_,
LongIdent_seq11right))::(_,(MlyValue.LongIdent LongIdent1,
LongIdentleft as LongIdent1left,LongIdentright))::rest671) => let val 
result=MlyValue.LongIdent_seq1(fn _ => let val LongIdent as LongIdent1
=LongIdent1 ()
val LongIdent_seq1 as LongIdent_seq11=LongIdent_seq11 ()
 in (
 WITH_INFO(PP LongIdentleft LongIdentright,
                                    LongIdent
                                   ) :: LongIdent_seq1
                        
) end
)
 in (LrTable.NT 107,(result,LongIdent1left,LongIdent_seq11right),
rest671) end
| (28,(_,(MlyValue.LongIdent LongIdent1,LongIdentleft as 
LongIdent1left,LongIdentright as LongIdent1right))::rest671) => let 
val result=MlyValue.LongIdent_seq1(fn _ => let val LongIdent as 
LongIdent1=LongIdent1 ()
 in (
 [WITH_INFO(PP LongIdentleft LongIdentright,
                                     LongIdent
                                    )
                          ]
                        
) end
)
 in (LrTable.NT 107,(result,LongIdent1left,LongIdent1right),rest671)
 end
| (29,(_,(MlyValue.LongIdentEq_seq2 LongIdentEq_seq21,_,
LongIdentEq_seq21right))::_::(_,(MlyValue.LongIdent LongIdent1,
LongIdentleft as LongIdent1left,LongIdentright))::rest671) => let val 
result=MlyValue.LongIdentEq_seq2(fn _ => let val LongIdent as 
LongIdent1=LongIdent1 ()
val LongIdentEq_seq2 as LongIdentEq_seq21=LongIdentEq_seq21 ()
 in (
 WITH_INFO(PP LongIdentleft LongIdentright, LongIdent)
                          :: LongIdentEq_seq2
                        
) end
)
 in (LrTable.NT 108,(result,LongIdent1left,LongIdentEq_seq21right),
rest671) end
| (30,(_,(MlyValue.LongIdent LongIdent2,LongIdent2left,LongIdent2right
))::_::(_,(MlyValue.LongIdent LongIdent1,LongIdent1left,
LongIdent1right))::rest671) => let val result=
MlyValue.LongIdentEq_seq2(fn _ => let val LongIdent1=LongIdent1 ()
val LongIdent2=LongIdent2 ()
 in (
 [WITH_INFO(PP LongIdent1left LongIdent1right,
                                     LongIdent1
                                    ),
                           WITH_INFO(PP LongIdent2left LongIdent2right,
                                     LongIdent2
                                    )
                          ]
                        
) end
)
 in (LrTable.NT 108,(result,LongIdent1left,LongIdent2right),rest671)
 end
| (31,(_,(MlyValue.LongTypeIdentEq_seq2 LongTypeIdentEq_seq21,_,
LongTypeIdentEq_seq21right))::_::(_,(MlyValue.LongTypeIdent 
LongTypeIdent1,LongTypeIdentleft as LongTypeIdent1left,
LongTypeIdentright))::rest671) => let val result=
MlyValue.LongTypeIdentEq_seq2(fn _ => let val LongTypeIdent as 
LongTypeIdent1=LongTypeIdent1 ()
val LongTypeIdentEq_seq2 as LongTypeIdentEq_seq21=
LongTypeIdentEq_seq21 ()
 in (
 WITH_INFO(PP LongTypeIdentleft LongTypeIdentright,
                                    LongTypeIdent
                                   )
                          :: LongTypeIdentEq_seq2
                        
) end
)
 in (LrTable.NT 109,(result,LongTypeIdent1left,
LongTypeIdentEq_seq21right),rest671) end
| (32,(_,(MlyValue.LongTypeIdent LongTypeIdent2,LongTypeIdent2left,
LongTypeIdent2right))::_::(_,(MlyValue.LongTypeIdent LongTypeIdent1,
LongTypeIdent1left,LongTypeIdent1right))::rest671) => let val result=
MlyValue.LongTypeIdentEq_seq2(fn _ => let val LongTypeIdent1=
LongTypeIdent1 ()
val LongTypeIdent2=LongTypeIdent2 ()
 in (
 [WITH_INFO(PP LongTypeIdent1left LongTypeIdent1right,
                                     LongTypeIdent1
                                    ),
                           WITH_INFO(PP LongTypeIdent2left LongTypeIdent2right,
                                     LongTypeIdent2
                                    )
                          ]
                        
) end
)
 in (LrTable.NT 109,(result,LongTypeIdent1left,LongTypeIdent2right),
rest671) end
| (33,(_,(MlyValue.TopDec_ TopDec_1,TopDec_1left,TopDec_1right))::
rest671) => let val result=MlyValue.TopDec(fn _ => let val TopDec_ as 
TopDec_1=TopDec_1 ()
 in ( TopDec_ ) end
)
 in (LrTable.NT 0,(result,TopDec_1left,TopDec_1right),rest671) end
| (34,(_,(MlyValue.Exp_ Exp_1,Exp_1left,Exp_1right))::rest671) => let 
val result=MlyValue.TopDec(fn _ => let val Exp_ as Exp_1=Exp_1 ()
 in ( topdecOfExp Exp_ ) end
)
 in (LrTable.NT 0,(result,Exp_1left,Exp_1right),rest671) end
| (35,rest671) => let val result=MlyValue.TopDec(fn _ => (
 STRtopdec(PP defaultPos defaultPos,
				    EMPTYstrdec(PP defaultPos defaultPos), NONE) 
))
 in (LrTable.NT 0,(result,defaultPos,defaultPos),rest671) end
| (36,(_,(MlyValue.TopDec_opt TopDec_opt1,_,TopDec_opt1right))::(_,(
MlyValue.OneStrDec OneStrDec1,OneStrDecleft as OneStrDec1left,_))::
rest671) => let val result=MlyValue.TopDec_(fn _ => let val OneStrDec
 as OneStrDec1=OneStrDec1 ()
val TopDec_opt as TopDec_opt1=TopDec_opt1 ()
 in (
 STRtopdec (PP OneStrDecleft
				       (rightmost  OneStrDec
				                   TopDec_opt),
				     OneStrDec, TopDec_opt) 
) end
)
 in (LrTable.NT 1,(result,OneStrDec1left,TopDec_opt1right),rest671)
 end
| (37,(_,(MlyValue.TopDec_opt TopDec_opt1,_,TopDec_opt1right))::(_,(
MlyValue.SigDec SigDec1,SigDecleft as SigDec1left,_))::rest671) => 
let val result=MlyValue.TopDec_(fn _ => let val SigDec as SigDec1=
SigDec1 ()
val TopDec_opt as TopDec_opt1=TopDec_opt1 ()
 in (
 SIGtopdec (PP SigDecleft
				       (rightmost  SigDec
				                   TopDec_opt),
				     SigDec, TopDec_opt) 
) end
)
 in (LrTable.NT 1,(result,SigDec1left,TopDec_opt1right),rest671) end
| (38,(_,(MlyValue.TopDec_opt TopDec_opt1,_,TopDec_opt1right))::(_,(
MlyValue.FunDec FunDec1,FunDecleft as FunDec1left,_))::rest671) => 
let val result=MlyValue.TopDec_(fn _ => let val FunDec as FunDec1=
FunDec1 ()
val TopDec_opt as TopDec_opt1=TopDec_opt1 ()
 in (
 FUNtopdec (PP FunDecleft
				       (rightmost  FunDec
				                   TopDec_opt),
				     FunDec, TopDec_opt) 
) end
)
 in (LrTable.NT 1,(result,FunDec1left,TopDec_opt1right),rest671) end
| (39,(_,(MlyValue.TopDec_ TopDec_1,TopDec_1left,TopDec_1right))::
rest671) => let val result=MlyValue.TopDec_opt(fn _ => let val TopDec_
 as TopDec_1=TopDec_1 ()
 in ( SOME TopDec_ ) end
)
 in (LrTable.NT 2,(result,TopDec_1left,TopDec_1right),rest671) end
| (40,rest671) => let val result=MlyValue.TopDec_opt(fn _ => ( NONE ))
 in (LrTable.NT 2,(result,defaultPos,defaultPos),rest671) end
| (41,(_,(MlyValue.AndFunBind_opt AndFunBind_opt1,_,
AndFunBind_opt1right))::(_,(MlyValue.StrExp StrExp1,_,StrExpright))::_
::(_,(MlyValue.SigExp_constraint_maybe SigExp_constraint_maybe1,
SigExp_constraint_maybeleft,_))::_::(_,(MlyValue.SigExp SigExp1,_,_))
::_::(_,(MlyValue.Ident Ident2,_,_))::_::(_,(MlyValue.Ident Ident1,
Ident1left,_))::rest671) => let val result=MlyValue.FunBind(fn _ => 
let val Ident1=Ident1 ()
val Ident2=Ident2 ()
val SigExp as SigExp1=SigExp1 ()
val SigExp_constraint_maybe as SigExp_constraint_maybe1=
SigExp_constraint_maybe1 ()
val StrExp as StrExp1=StrExp1 ()
val AndFunBind_opt as AndFunBind_opt1=AndFunBind_opt1 ()
 in (
 let val i_body = PP SigExp_constraint_maybeleft StrExpright
                              val empty_strdec = EMPTYstrdec (PP defaultPos defaultPos)
                          in 
                             FUNBIND (PP Ident1left
				     (rightmost  StrExp
				                 AndFunBind_opt),
                                   mk_FunId Ident1, mk_StrId Ident2, SigExp,
                                   LETstrexp(i_body, empty_strdec,
                                             SigExp_constraint_maybe SigExp_constraint_maybeleft StrExp), 
                                   AndFunBind_opt)
                          end 
) end
)
 in (LrTable.NT 7,(result,Ident1left,AndFunBind_opt1right),rest671)
 end
| (42,(_,(MlyValue.AndFunBind_opt AndFunBind_opt1,_,
AndFunBind_opt1right))::(_,(MlyValue.StrExp StrExp1,_,StrExpright))::_
::(_,(MlyValue.SigExp_constraint_maybe SigExp_constraint_maybe1,
SigExp_constraint_maybeleft,_))::_::(_,(MlyValue.Spec Spec1,_,_))::_::
(_,(MlyValue.Ident Ident1,Ident1left,_))::rest671) => let val result=
MlyValue.FunBind(fn _ => let val Ident as Ident1=Ident1 ()
val Spec as Spec1=Spec1 ()
val SigExp_constraint_maybe as SigExp_constraint_maybe1=
SigExp_constraint_maybe1 ()
val StrExp as StrExp1=StrExp1 ()
val AndFunBind_opt as AndFunBind_opt1=AndFunBind_opt1 ()
 in (
 let val strid_nu = inventStrId ()
			      val i_body = PP SigExp_constraint_maybeleft StrExpright
			      val i_spec =  Spec
                          in FUNBIND
			       (PP Ident1left
				  (rightmost  StrExp
				              AndFunBind_opt),
				mk_FunId Ident, strid_nu,
				SIGsigexp (i_spec, Spec),
				LETstrexp
				  (i_body,
				   DECstrdec
				     (i_spec,
				      OPENdec (i_spec,
					       [WITH_INFO (i_spec, longStrIdOfStrId strid_nu)])),
				   SigExp_constraint_maybe SigExp_constraint_maybeleft StrExp),
				AndFunBind_opt)
                          end 
) end
)
 in (LrTable.NT 7,(result,Ident1left,AndFunBind_opt1right),rest671)
 end
| (43,(_,(MlyValue.SigExp_constraint SigExp_constraint1,
SigExp_constraint1left,SigExp_constraint1right))::rest671) => let val 
result=MlyValue.SigExp_constraint_maybe(fn _ => let val 
SigExp_constraint as SigExp_constraint1=SigExp_constraint1 ()
 in ( SigExp_constraint ) end
)
 in (LrTable.NT 25,(result,SigExp_constraint1left,
SigExp_constraint1right),rest671) end
| (44,rest671) => let val result=MlyValue.SigExp_constraint_maybe(fn _
 => ( (fn _ => fn strexp => strexp) ))
 in (LrTable.NT 25,(result,defaultPos,defaultPos),rest671) end
| (45,(_,(MlyValue.SigExp SigExp1,_,SigExpright as SigExp1right))::(_,
(_,COLON1left,_))::rest671) => let val result=
MlyValue.SigExp_constraint(fn _ => let val SigExp as SigExp1=SigExp1 
()
 in (
 (fn leftpos => fn strexp => 
			   TRANSPARENT_CONSTRAINTstrexp
			     (PP leftpos SigExpright, strexp, SigExp)) 
) end
)
 in (LrTable.NT 24,(result,COLON1left,SigExp1right),rest671) end
| (46,(_,(MlyValue.SigExp SigExp1,_,SigExpright as SigExp1right))::(_,
(_,COLONGREATER1left,_))::rest671) => let val result=
MlyValue.SigExp_constraint(fn _ => let val SigExp as SigExp1=SigExp1 
()
 in (
 (fn leftpos => fn strexp => 
			   OPAQUE_CONSTRAINTstrexp
			     (PP leftpos SigExpright, strexp, SigExp)) 
) end
)
 in (LrTable.NT 24,(result,COLONGREATER1left,SigExp1right),rest671)
 end
| (47,(_,(MlyValue.FunBind FunBind1,_,FunBind1right))::(_,(_,AND1left,
_))::rest671) => let val result=MlyValue.AndFunBind_opt(fn _ => let 
val FunBind as FunBind1=FunBind1 ()
 in ( SOME FunBind ) end
)
 in (LrTable.NT 26,(result,AND1left,FunBind1right),rest671) end
| (48,rest671) => let val result=MlyValue.AndFunBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 26,(result,defaultPos,defaultPos),rest671) end
| (49,(_,(MlyValue.FunBind FunBind1,_,FunBind1right))::(_,(_,
FUNCTORleft as FUNCTOR1left,_))::rest671) => let val result=
MlyValue.FunDec(fn _ => let val FunBind as FunBind1=FunBind1 ()
 in (
 FUNCTORfundec (PP FUNCTORleft (right (info_on_funbind FunBind)),
					 FunBind) 
) end
)
 in (LrTable.NT 5,(result,FUNCTOR1left,FunBind1right),rest671) end
| (50,(_,(MlyValue.AndStrDesc_opt AndStrDesc_opt1,_,
AndStrDesc_opt1right))::(_,(MlyValue.SigExp SigExp1,_,_))::_::(_,(
MlyValue.Ident Ident1,Identleft as Ident1left,_))::rest671) => let 
val result=MlyValue.StrDesc(fn _ => let val Ident as Ident1=Ident1 ()
val SigExp as SigExp1=SigExp1 ()
val AndStrDesc_opt as AndStrDesc_opt1=AndStrDesc_opt1 ()
 in (
 STRDESC (PP Identleft
				     (rightmost info_on_sigexp SigExp
				                info_on_strdesc AndStrDesc_opt),
				   mk_StrId Ident, SigExp, AndStrDesc_opt) 
) end
)
 in (LrTable.NT 8,(result,Ident1left,AndStrDesc_opt1right),rest671)
 end
| (51,(_,(MlyValue.StrDesc StrDesc1,_,StrDesc1right))::(_,(_,AND1left,
_))::rest671) => let val result=MlyValue.AndStrDesc_opt(fn _ => let 
val StrDesc as StrDesc1=StrDesc1 ()
 in ( SOME StrDesc ) end
)
 in (LrTable.NT 29,(result,AND1left,StrDesc1right),rest671) end
| (52,rest671) => let val result=MlyValue.AndStrDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 29,(result,defaultPos,defaultPos),rest671) end
| (53,(_,(MlyValue.AndExDesc_opt AndExDesc_opt1,_,AndExDesc_opt1right)
)::(_,(MlyValue.OfTy_opt OfTy_opt1,_,_))::(_,(MlyValue.Ident Ident1,
Identleft as Ident1left,Identright))::rest671) => let val result=
MlyValue.ExDesc(fn _ => let val Ident as Ident1=Ident1 ()
val OfTy_opt as OfTy_opt1=OfTy_opt1 ()
val AndExDesc_opt as AndExDesc_opt1=AndExDesc_opt1 ()
 in (
 EXDESC (PP Identleft
				    (rightmost_of_three  Identright
				                get_info_ty OfTy_opt
						info_on_exdesc AndExDesc_opt),
				  mk_Id Ident, OfTy_opt, AndExDesc_opt) 
) end
)
 in (LrTable.NT 9,(result,Ident1left,AndExDesc_opt1right),rest671) end
| (54,(_,(MlyValue.ExDesc ExDesc1,_,ExDesc1right))::(_,(_,AND1left,_))
::rest671) => let val result=MlyValue.AndExDesc_opt(fn _ => let val 
ExDesc as ExDesc1=ExDesc1 ()
 in ( SOME ExDesc ) end
)
 in (LrTable.NT 30,(result,AND1left,ExDesc1right),rest671) end
| (55,rest671) => let val result=MlyValue.AndExDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 30,(result,defaultPos,defaultPos),rest671) end
| (56,(_,(MlyValue.BarConDesc_opt BarConDesc_opt1,_,
BarConDesc_opt1right))::(_,(MlyValue.OfTy_opt OfTy_opt1,_,_))::(_,(
MlyValue.OpIdent OpIdent1,OpIdentleft as OpIdent1left,OpIdentright))::
rest671) => let val result=MlyValue.ConDesc(fn _ => let val OpIdent
 as OpIdent1=OpIdent1 ()
val OfTy_opt as OfTy_opt1=OfTy_opt1 ()
val BarConDesc_opt as BarConDesc_opt1=BarConDesc_opt1 ()
 in (
 let val OP_OPT (id, _) = OpIdent
                          in CONDESC
			       (PP OpIdentleft
				  (rightmost_of_three OpIdentright
				              get_info_ty OfTy_opt
					      info_on_condesc BarConDesc_opt), 
				mk_Id id, OfTy_opt, BarConDesc_opt)
                          end 
) end
)
 in (LrTable.NT 10,(result,OpIdent1left,BarConDesc_opt1right),rest671)
 end
| (57,(_,(MlyValue.ConDesc ConDesc1,_,ConDesc1right))::(_,(_,BAR1left,
_))::rest671) => let val result=MlyValue.BarConDesc_opt(fn _ => let 
val ConDesc as ConDesc1=ConDesc1 ()
 in ( SOME ConDesc ) end
)
 in (LrTable.NT 31,(result,BAR1left,ConDesc1right),rest671) end
| (58,rest671) => let val result=MlyValue.BarConDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 31,(result,defaultPos,defaultPos),rest671) end
| (59,(_,(MlyValue.AndDatDesc_opt AndDatDesc_opt1,_,
AndDatDesc_opt1right))::(_,(MlyValue.ConDesc ConDesc1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,_,_))::(_,(MlyValue.TyVarSeq TyVarSeq1,
TyVarSeqleft as TyVarSeq1left,_))::rest671) => let val result=
MlyValue.DatDesc(fn _ => let val TyVarSeq as TyVarSeq1=TyVarSeq1 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val ConDesc as ConDesc1=ConDesc1 ()
val AndDatDesc_opt as AndDatDesc_opt1=AndDatDesc_opt1 ()
 in (
 DATDESC (PP TyVarSeqleft
				     (rightmost info_on_condesc ConDesc
				                info_on_datdesc AndDatDesc_opt),
				   TyVarSeq, mk_TyCon TypeIdent,
				   ConDesc, AndDatDesc_opt) 
) end
)
 in (LrTable.NT 11,(result,TyVarSeq1left,AndDatDesc_opt1right),rest671
) end
| (60,(_,(MlyValue.AndDatDesc_opt AndDatDesc_opt1,_,
AndDatDesc_opt1right))::(_,(MlyValue.ConDesc ConDesc1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,TypeIdentleft as TypeIdent1left,_))::
rest671) => let val result=MlyValue.DatDesc_zero_arity(fn _ => let 
val TypeIdent as TypeIdent1=TypeIdent1 ()
val ConDesc as ConDesc1=ConDesc1 ()
val AndDatDesc_opt as AndDatDesc_opt1=AndDatDesc_opt1 ()
 in (
 DATDESC (PP TypeIdentleft
				     (rightmost info_on_condesc ConDesc
				                info_on_datdesc AndDatDesc_opt),
				   [], mk_TyCon TypeIdent,
				   ConDesc, AndDatDesc_opt) 
) end
)
 in (LrTable.NT 12,(result,TypeIdent1left,AndDatDesc_opt1right),
rest671) end
| (61,(_,(MlyValue.AndDatDesc_opt AndDatDesc_opt1,_,
AndDatDesc_opt1right))::(_,(MlyValue.ConDesc ConDesc1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,_,_))::(_,(MlyValue.TyVarSeq1 TyVarSeq11
,TyVarSeq1left as TyVarSeq11left,_))::rest671) => let val result=
MlyValue.DatDesc_nonzero_arity(fn _ => let val TyVarSeq1 as TyVarSeq11
=TyVarSeq11 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val ConDesc as ConDesc1=ConDesc1 ()
val AndDatDesc_opt as AndDatDesc_opt1=AndDatDesc_opt1 ()
 in (
 DATDESC (PP TyVarSeq1left
				     (rightmost info_on_condesc ConDesc
				                info_on_datdesc AndDatDesc_opt),
				   TyVarSeq1, mk_TyCon TypeIdent,
				   ConDesc, AndDatDesc_opt) 
) end
)
 in (LrTable.NT 13,(result,TyVarSeq11left,AndDatDesc_opt1right),
rest671) end
| (62,(_,(MlyValue.DatDesc DatDesc1,_,DatDesc1right))::(_,(_,AND1left,
_))::rest671) => let val result=MlyValue.AndDatDesc_opt(fn _ => let 
val DatDesc as DatDesc1=DatDesc1 ()
 in ( SOME DatDesc ) end
)
 in (LrTable.NT 32,(result,AND1left,DatDesc1right),rest671) end
| (63,rest671) => let val result=MlyValue.AndDatDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 32,(result,defaultPos,defaultPos),rest671) end
| (64,(_,(MlyValue.AndTypDesc_opt AndTypDesc_opt1,_,
AndTypDesc_opt1right))::(_,(MlyValue.TypeIdent TypeIdent1,_,
TypeIdentright))::(_,(MlyValue.TyVarSeq TyVarSeq1,TyVarSeqleft as 
TyVarSeq1left,_))::rest671) => let val result=MlyValue.TypDesc(fn _
 => let val TyVarSeq as TyVarSeq1=TyVarSeq1 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val AndTypDesc_opt as AndTypDesc_opt1=AndTypDesc_opt1 ()
 in (
 TYPDESC (PP TyVarSeqleft
				     (rightmost' TypeIdentright info_on_typdesc AndTypDesc_opt),
				   TyVarSeq, mk_TyCon TypeIdent, AndTypDesc_opt) 
) end
)
 in (LrTable.NT 14,(result,TyVarSeq1left,AndTypDesc_opt1right),rest671
) end
| (65,(_,(MlyValue.TypDesc TypDesc1,_,TypDesc1right))::(_,(_,AND1left,
_))::rest671) => let val result=MlyValue.AndTypDesc_opt(fn _ => let 
val TypDesc as TypDesc1=TypDesc1 ()
 in ( SOME TypDesc ) end
)
 in (LrTable.NT 33,(result,AND1left,TypDesc1right),rest671) end
| (66,rest671) => let val result=MlyValue.AndTypDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 33,(result,defaultPos,defaultPos),rest671) end
| (67,(_,(MlyValue.TypAbbreviationAND_seq1 TypAbbreviationAND_seq11,_,
TypAbbreviationAND_seq11right))::_::(_,(MlyValue.TypAbbreviation 
TypAbbreviation1,TypAbbreviation1left,_))::rest671) => let val result=
MlyValue.TypAbbreviationAND_seq1(fn _ => let val TypAbbreviation as 
TypAbbreviation1=TypAbbreviation1 ()
val TypAbbreviationAND_seq1 as TypAbbreviationAND_seq11=
TypAbbreviationAND_seq11 ()
 in ( TypAbbreviation :: TypAbbreviationAND_seq1 ) end
)
 in (LrTable.NT 34,(result,TypAbbreviation1left,
TypAbbreviationAND_seq11right),rest671) end
| (68,(_,(MlyValue.TypAbbreviation TypAbbreviation1,
TypAbbreviation1left,TypAbbreviation1right))::rest671) => let val 
result=MlyValue.TypAbbreviationAND_seq1(fn _ => let val 
TypAbbreviation as TypAbbreviation1=TypAbbreviation1 ()
 in ( [TypAbbreviation] ) end
)
 in (LrTable.NT 34,(result,TypAbbreviation1left,TypAbbreviation1right)
,rest671) end
| (69,(_,(MlyValue.Ty Ty1,_,Tyright as Ty1right))::_::(_,(
MlyValue.TypeIdent TypeIdent1,_,TypeIdentright))::(_,(
MlyValue.TyVarSeq TyVarSeq1,TyVarSeqleft as TyVarSeq1left,_))::rest671
) => let val result=MlyValue.TypAbbreviation(fn _ => let val TyVarSeq
 as TyVarSeq1=TyVarSeq1 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val Ty as Ty1=Ty1 ()
 in (
 (TyVarSeq, mk_TyCon TypeIdent, Ty,
			   PP TyVarSeqleft Tyright, PP TyVarSeqleft TypeIdentright) 
) end
)
 in (LrTable.NT 15,(result,TyVarSeq1left,Ty1right),rest671) end
| (70,(_,(MlyValue.AndValDesc_opt AndValDesc_opt1,_,
AndValDesc_opt1right))::(_,(MlyValue.Ty Ty1,_,Tyright))::_::(_,(
MlyValue.EqIdent EqIdent1,EqIdentleft as EqIdent1left,_))::rest671)
 => let val result=MlyValue.ValDesc(fn _ => let val EqIdent as 
EqIdent1=EqIdent1 ()
val Ty as Ty1=Ty1 ()
val AndValDesc_opt as AndValDesc_opt1=AndValDesc_opt1 ()
 in (
 VALDESC (PP EqIdentleft
				     (rightmost' Tyright info_on_valdesc AndValDesc_opt),
				   mk_Id EqIdent, Ty, AndValDesc_opt) 
) end
)
 in (LrTable.NT 16,(result,EqIdent1left,AndValDesc_opt1right),rest671)
 end
| (71,(_,(MlyValue.ValDesc ValDesc1,_,ValDesc1right))::(_,(_,AND1left,
_))::rest671) => let val result=MlyValue.AndValDesc_opt(fn _ => let 
val ValDesc as ValDesc1=ValDesc1 ()
 in ( SOME ValDesc ) end
)
 in (LrTable.NT 35,(result,AND1left,ValDesc1right),rest671) end
| (72,rest671) => let val result=MlyValue.AndValDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 35,(result,defaultPos,defaultPos),rest671) end
| (73,(_,(MlyValue.ValDesc ValDesc1,_,ValDesc1right))::(_,(_,VALleft
 as VAL1left,_))::rest671) => let val result=MlyValue.OneSpec(fn _ => 
let val ValDesc as ValDesc1=ValDesc1 ()
 in (
 VALspec (PP VALleft (right (info_on_valdesc ValDesc)),
				   ValDesc) 
) end
)
 in (LrTable.NT 18,(result,VAL1left,ValDesc1right),rest671) end
| (74,(_,(MlyValue.TypDesc TypDesc1,_,TypDesc1right))::(_,(_,TYPEleft
 as TYPE1left,_))::rest671) => let val result=MlyValue.OneSpec(fn _
 => let val TypDesc as TypDesc1=TypDesc1 ()
 in (
 TYPEspec (PP TYPEleft (right (info_on_typdesc TypDesc)),
				    TypDesc) 
) end
)
 in (LrTable.NT 18,(result,TYPE1left,TypDesc1right),rest671) end
| (75,(_,(MlyValue.TypDesc TypDesc1,_,TypDesc1right))::(_,(_,
EQTYPEleft as EQTYPE1left,_))::rest671) => let val result=
MlyValue.OneSpec(fn _ => let val TypDesc as TypDesc1=TypDesc1 ()
 in (
 EQTYPEspec (PP EQTYPEleft (right (info_on_typdesc TypDesc)),
				      TypDesc) 
) end
)
 in (LrTable.NT 18,(result,EQTYPE1left,TypDesc1right),rest671) end
| (76,(_,(MlyValue.DatDesc_zero_arity DatDesc_zero_arity1,_,
DatDesc_zero_arity1right))::(_,(_,DATATYPEleft as DATATYPE1left,_))::
rest671) => let val result=MlyValue.OneSpec(fn _ => let val 
DatDesc_zero_arity as DatDesc_zero_arity1=DatDesc_zero_arity1 ()
 in (
 DATATYPEspec (PP DATATYPEleft
					  (right (info_on_datdesc DatDesc_zero_arity)),
				        DatDesc_zero_arity) 
) end
)
 in (LrTable.NT 18,(result,DATATYPE1left,DatDesc_zero_arity1right),
rest671) end
| (77,(_,(MlyValue.DatDesc_nonzero_arity DatDesc_nonzero_arity1,_,
DatDesc_nonzero_arity1right))::(_,(_,DATATYPEleft as DATATYPE1left,_))
::rest671) => let val result=MlyValue.OneSpec(fn _ => let val 
DatDesc_nonzero_arity as DatDesc_nonzero_arity1=DatDesc_nonzero_arity1
 ()
 in (
 DATATYPEspec (PP DATATYPEleft
					  (right (info_on_datdesc DatDesc_nonzero_arity)),
					DatDesc_nonzero_arity) 
) end
)
 in (LrTable.NT 18,(result,DATATYPE1left,DatDesc_nonzero_arity1right),
rest671) end
| (78,(_,(MlyValue.LongTypeIdent LongTypeIdent1,_,LongTypeIdentright
 as LongTypeIdent1right))::_::_::(_,(MlyValue.TypeIdent TypeIdent1,_,_
))::(_,(_,DATATYPE1left,_))::rest671) => let val result=
MlyValue.OneSpec(fn _ => let val TypeIdent as TypeIdent1=TypeIdent1 ()
val LongTypeIdent as LongTypeIdent1=LongTypeIdent1 ()
 in (
 DATATYPE_REPLICATIONspec
			    (PP DATATYPE1left LongTypeIdentright,
			     mk_TyCon TypeIdent, mk_LongTyCon LongTypeIdent) 
) end
)
 in (LrTable.NT 18,(result,DATATYPE1left,LongTypeIdent1right),rest671)
 end
| (79,(_,(MlyValue.ExDesc ExDesc1,_,ExDesc1right))::(_,(_,
EXCEPTIONleft as EXCEPTION1left,_))::rest671) => let val result=
MlyValue.OneSpec(fn _ => let val ExDesc as ExDesc1=ExDesc1 ()
 in (
 EXCEPTIONspec (PP EXCEPTIONleft (right (info_on_exdesc ExDesc)),
					 ExDesc) 
) end
)
 in (LrTable.NT 18,(result,EXCEPTION1left,ExDesc1right),rest671) end
| (80,(_,(MlyValue.StrDesc StrDesc1,_,StrDesc1right))::(_,(_,
STRUCTUREleft as STRUCTURE1left,_))::rest671) => let val result=
MlyValue.OneSpec(fn _ => let val StrDesc as StrDesc1=StrDesc1 ()
 in (
 STRUCTUREspec (PP STRUCTUREleft (right (info_on_strdesc StrDesc)),
					 StrDesc) 
) end
)
 in (LrTable.NT 18,(result,STRUCTURE1left,StrDesc1right),rest671) end
| (81,(_,(MlyValue.SigExp SigExp1,_,SigExpright as SigExp1right))::(_,
(_,INCLUDEleft as INCLUDE1left,_))::rest671) => let val result=
MlyValue.OneSpec(fn _ => let val SigExp as SigExp1=SigExp1 ()
 in ( INCLUDEspec (PP INCLUDEleft SigExpright, SigExp) ) end
)
 in (LrTable.NT 18,(result,INCLUDE1left,SigExp1right),rest671) end
| (82,(_,(MlyValue.TypAbbreviationAND_seq1 TypAbbreviationAND_seq11,_,
TypAbbreviationAND_seq11right))::(_,(_,TYPE1left,_))::rest671) => let 
val result=MlyValue.OneSpec(fn _ => let val TypAbbreviationAND_seq1
 as TypAbbreviationAND_seq11=TypAbbreviationAND_seq11 ()
 in (
 fold_specs_to_spec
			    (map rewrite_type_abbreviation_spec TypAbbreviationAND_seq1) 
) end
)
 in (LrTable.NT 18,(result,TYPE1left,TypAbbreviationAND_seq11right),
rest671) end
| (83,(_,(MlyValue.Ident_seq2 Ident_seq21,_,Ident_seq21right))::(_,(_,
INCLUDE1left,_))::rest671) => let val result=MlyValue.OneSpec(fn _ => 
let val Ident_seq2 as Ident_seq21=Ident_seq21 ()
 in (
 fold_specs_to_spec
			    (map (fn WITH_INFO (i, Ident) =>
				      (i, INCLUDEspec (i, SIGIDsigexp (i, mk_SigId Ident))))
			       Ident_seq2) 
) end
)
 in (LrTable.NT 18,(result,INCLUDE1left,Ident_seq21right),rest671) end
| (84,(_,(MlyValue.OneSpec OneSpec1,_,OneSpec1right))::(_,(
MlyValue.Spec Spec1,Specleft as Spec1left,_))::rest671) => let val 
result=MlyValue.Spec(fn _ => let val Spec as Spec1=Spec1 ()
val OneSpec as OneSpec1=OneSpec1 ()
 in (
 composeSpec (PP Specleft (right (info_on_spec OneSpec)),
				       Spec, OneSpec) 
) end
)
 in (LrTable.NT 17,(result,Spec1left,OneSpec1right),rest671) end
| (85,(_,(MlyValue.LongTypeIdentEq_seq2 LongTypeIdentEq_seq21,_,
LongTypeIdentEq_seq2right as LongTypeIdentEq_seq21right))::_::_::(_,(
MlyValue.Spec Spec1,Specleft as Spec1left,_))::rest671) => let val 
result=MlyValue.Spec(fn _ => let val Spec as Spec1=Spec1 ()
val LongTypeIdentEq_seq2 as LongTypeIdentEq_seq21=
LongTypeIdentEq_seq21 ()
 in (
 SHARING_TYPEspec (PP Specleft LongTypeIdentEq_seq2right,
					    Spec,
					    wi_Convert mk_LongTyCon LongTypeIdentEq_seq2) 
) end
)
 in (LrTable.NT 17,(result,Spec1left,LongTypeIdentEq_seq21right),
rest671) end
| (86,(_,(MlyValue.LongIdentEq_seq2 LongIdentEq_seq21,_,
LongIdentEq_seq2right as LongIdentEq_seq21right))::_::(_,(
MlyValue.Spec Spec1,Specleft as Spec1left,_))::rest671) => let val 
result=MlyValue.Spec(fn _ => let val Spec as Spec1=Spec1 ()
val LongIdentEq_seq2 as LongIdentEq_seq21=LongIdentEq_seq21 ()
 in (
 SHARINGspec (PP Specleft LongIdentEq_seq2right,
				       Spec, wi_Convert mk_LongStrId LongIdentEq_seq2) 
) end
)
 in (LrTable.NT 17,(result,Spec1left,LongIdentEq_seq21right),rest671)
 end
| (87,(_,(_,_,SEMICOLON1right))::(_,(MlyValue.Spec Spec1,Spec1left,_))
::rest671) => let val result=MlyValue.Spec(fn _ => let val Spec as 
Spec1=Spec1 ()
 in ( Spec ) end
)
 in (LrTable.NT 17,(result,Spec1left,SEMICOLON1right),rest671) end
| (88,rest671) => let val result=MlyValue.Spec(fn _ => (
 EMPTYspec (PP defaultPos defaultPos) ))
 in (LrTable.NT 17,(result,defaultPos,defaultPos),rest671) end
| (89,(_,(_,_,ENDright as END1right))::(_,(MlyValue.StrDec StrDec1,_,_
))::(_,(_,STRUCTleft as STRUCT1left,_))::rest671) => let val result=
MlyValue.StrExp(fn _ => let val StrDec as StrDec1=StrDec1 ()
 in ( STRUCTstrexp (PP STRUCTleft ENDright, StrDec) ) end
)
 in (LrTable.NT 23,(result,STRUCT1left,END1right),rest671) end
| (90,(_,(MlyValue.LongIdent LongIdent1,LongIdentleft as 
LongIdent1left,LongIdentright as LongIdent1right))::rest671) => let 
val result=MlyValue.StrExp(fn _ => let val LongIdent as LongIdent1=
LongIdent1 ()
 in (
 LONGSTRIDstrexp (PP LongIdentleft LongIdentright,
					   mk_LongStrId LongIdent) 
) end
)
 in (LrTable.NT 23,(result,LongIdent1left,LongIdent1right),rest671)
 end
| (91,(_,(MlyValue.SigExp_constraint SigExp_constraint1,_,
SigExp_constraint1right))::(_,(MlyValue.StrExp StrExp1,StrExpleft as 
StrExp1left,_))::rest671) => let val result=MlyValue.StrExp(fn _ => 
let val StrExp as StrExp1=StrExp1 ()
val SigExp_constraint as SigExp_constraint1=SigExp_constraint1 ()
 in ( SigExp_constraint StrExpleft StrExp ) end
)
 in (LrTable.NT 23,(result,StrExp1left,SigExp_constraint1right),
rest671) end
| (92,(_,(_,_,RPARENright as RPAREN1right))::(_,(MlyValue.StrExp 
StrExp1,_,_))::_::(_,(MlyValue.Ident Ident1,Identleft as Ident1left,_)
)::rest671) => let val result=MlyValue.StrExp(fn _ => let val Ident
 as Ident1=Ident1 ()
val StrExp as StrExp1=StrExp1 ()
 in (
 APPstrexp (PP Identleft RPARENright,
				     mk_FunId Ident, StrExp) 
) end
)
 in (LrTable.NT 23,(result,Ident1left,RPAREN1right),rest671) end
| (93,(_,(_,_,ENDright as END1right))::(_,(MlyValue.StrExp StrExp1,_,_
))::_::(_,(MlyValue.StrDec StrDec1,_,_))::(_,(_,LETleft as LET1left,_)
)::rest671) => let val result=MlyValue.StrExp(fn _ => let val StrDec
 as StrDec1=StrDec1 ()
val StrExp as StrExp1=StrExp1 ()
 in ( LETstrexp (PP LETleft ENDright, StrDec, StrExp) ) end
)
 in (LrTable.NT 23,(result,LET1left,END1right),rest671) end
| (94,(_,(_,_,RPARENright as RPAREN1right))::(_,(MlyValue.StrDec 
StrDec1,StrDecleft,_))::_::(_,(MlyValue.Ident Ident1,Identleft as 
Ident1left,_))::rest671) => let val result=MlyValue.StrExp(fn _ => 
let val Ident as Ident1=Ident1 ()
val StrDec as StrDec1=StrDec1 ()
 in (
 APPstrexp (PP Identleft RPARENright, mk_FunId Ident,
				     STRUCTstrexp
				       (PP StrDecleft (right (info_on_strdec StrDec)),
					StrDec)) 
) end
)
 in (LrTable.NT 23,(result,Ident1left,RPAREN1right),rest671) end
| (95,(_,(MlyValue.OneStrDec_or_SEMICOLON OneStrDec_or_SEMICOLON1,_,
OneStrDec_or_SEMICOLON1right))::(_,(MlyValue.NonEmptyStrDec 
NonEmptyStrDec1,NonEmptyStrDecleft as NonEmptyStrDec1left,_))::rest671
) => let val result=MlyValue.NonEmptyStrDec(fn _ => let val 
NonEmptyStrDec as NonEmptyStrDec1=NonEmptyStrDec1 ()
val OneStrDec_or_SEMICOLON as OneStrDec_or_SEMICOLON1=
OneStrDec_or_SEMICOLON1 ()
 in (
 (case OneStrDec_or_SEMICOLON of
			     SOME strdec =>
			       composeStrDec (PP NonEmptyStrDecleft
					        (right (info_on_strdec strdec)),
					      NonEmptyStrDec, strdec)
			   | NONE =>
			       NonEmptyStrDec) 
) end
)
 in (LrTable.NT 42,(result,NonEmptyStrDec1left,
OneStrDec_or_SEMICOLON1right),rest671) end
| (96,(_,(MlyValue.OneStrDec_or_SEMICOLON OneStrDec_or_SEMICOLON1,
OneStrDec_or_SEMICOLON1left,OneStrDec_or_SEMICOLON1right))::rest671)
 => let val result=MlyValue.NonEmptyStrDec(fn _ => let val 
OneStrDec_or_SEMICOLON as OneStrDec_or_SEMICOLON1=
OneStrDec_or_SEMICOLON1 ()
 in (
 (case OneStrDec_or_SEMICOLON of
			     SOME strdec => strdec
			   | NONE => EMPTYstrdec (PP defaultPos defaultPos)) 
) end
)
 in (LrTable.NT 42,(result,OneStrDec_or_SEMICOLON1left,
OneStrDec_or_SEMICOLON1right),rest671) end
| (97,(_,(MlyValue.NonEmptyStrDec NonEmptyStrDec1,NonEmptyStrDec1left,
NonEmptyStrDec1right))::rest671) => let val result=MlyValue.StrDec(fn 
_ => let val NonEmptyStrDec as NonEmptyStrDec1=NonEmptyStrDec1 ()
 in ( NonEmptyStrDec ) end
)
 in (LrTable.NT 22,(result,NonEmptyStrDec1left,NonEmptyStrDec1right),
rest671) end
| (98,rest671) => let val result=MlyValue.StrDec(fn _ => (
 EMPTYstrdec (PP defaultPos defaultPos) ))
 in (LrTable.NT 22,(result,defaultPos,defaultPos),rest671) end
| (99,(_,(MlyValue.OneStrDec OneStrDec1,OneStrDec1left,OneStrDec1right
))::rest671) => let val result=MlyValue.OneStrDec_or_SEMICOLON(fn _
 => let val OneStrDec as OneStrDec1=OneStrDec1 ()
 in ( SOME OneStrDec ) end
)
 in (LrTable.NT 41,(result,OneStrDec1left,OneStrDec1right),rest671)
 end
| (100,(_,(_,SEMICOLON1left,SEMICOLON1right))::rest671) => let val 
result=MlyValue.OneStrDec_or_SEMICOLON(fn _ => ( NONE ))
 in (LrTable.NT 41,(result,SEMICOLON1left,SEMICOLON1right),rest671)
 end
| (101,(_,(MlyValue.OneDec_sans_LOCAL OneDec_sans_LOCAL1,
OneDec_sans_LOCALleft as OneDec_sans_LOCAL1left,
OneDec_sans_LOCAL1right))::rest671) => let val result=
MlyValue.OneStrDec(fn _ => let val OneDec_sans_LOCAL as 
OneDec_sans_LOCAL1=OneDec_sans_LOCAL1 ()
 in (
 DECstrdec (PP OneDec_sans_LOCALleft
                                        (right (get_info_dec OneDec_sans_LOCAL)),
                                     OneDec_sans_LOCAL) 
) end
)
 in (LrTable.NT 6,(result,OneDec_sans_LOCAL1left,
OneDec_sans_LOCAL1right),rest671) end
| (102,(_,(MlyValue.StrBind StrBind1,_,StrBind1right))::(_,(_,
STRUCTUREleft as STRUCTURE1left,_))::rest671) => let val result=
MlyValue.OneStrDec(fn _ => let val StrBind as StrBind1=StrBind1 ()
 in (
 STRUCTUREstrdec (PP STRUCTUREleft
					     (right (info_on_strbind StrBind)),
					   StrBind) 
) end
)
 in (LrTable.NT 6,(result,STRUCTURE1left,StrBind1right),rest671) end
| (103,(_,(_,_,ENDright as END1right))::(_,(MlyValue.StrDec StrDec2,_,
_))::_::(_,(MlyValue.StrDec StrDec1,_,_))::(_,(_,LOCALleft as 
LOCAL1left,_))::rest671) => let val result=MlyValue.OneStrDec(fn _ => 
let val StrDec1=StrDec1 ()
val StrDec2=StrDec2 ()
 in ( LOCALstrdec (PP LOCALleft ENDright, StrDec1, StrDec2) ) end
)
 in (LrTable.NT 6,(result,LOCAL1left,END1right),rest671) end
| (104,(_,(MlyValue.AndStrBind_opt AndStrBind_opt1,_,
AndStrBind_opt1right))::(_,(MlyValue.StrExp StrExp1,_,_))::_::(_,(
MlyValue.SigExp_constraint_maybe SigExp_constraint_maybe1,
SigExp_constraint_maybeleft,_))::(_,(MlyValue.Ident Ident1,Identleft
 as Ident1left,_))::rest671) => let val result=MlyValue.StrBind(fn _
 => let val Ident as Ident1=Ident1 ()
val SigExp_constraint_maybe as SigExp_constraint_maybe1=
SigExp_constraint_maybe1 ()
val StrExp as StrExp1=StrExp1 ()
val AndStrBind_opt as AndStrBind_opt1=AndStrBind_opt1 ()
 in (
 STRBIND (PP Identleft
				     (rightmost info_on_strexp StrExp
				                info_on_strbind AndStrBind_opt), 
				   mk_StrId Ident, SigExp_constraint_maybe SigExp_constraint_maybeleft StrExp, 
				   AndStrBind_opt) 
) end
)
 in (LrTable.NT 21,(result,Ident1left,AndStrBind_opt1right),rest671)
 end
| (105,(_,(MlyValue.StrBind StrBind1,_,StrBind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AndStrBind_opt(fn _ => let 
val StrBind as StrBind1=StrBind1 ()
 in ( SOME StrBind ) end
)
 in (LrTable.NT 27,(result,AND1left,StrBind1right),rest671) end
| (106,rest671) => let val result=MlyValue.AndStrBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 27,(result,defaultPos,defaultPos),rest671) end
| (107,(_,(_,_,ENDright as END1right))::(_,(MlyValue.Spec Spec1,_,_))
::(_,(_,SIGleft as SIG1left,_))::rest671) => let val result=
MlyValue.SigExp(fn _ => let val Spec as Spec1=Spec1 ()
 in ( SIGsigexp (PP SIGleft ENDright, Spec) ) end
)
 in (LrTable.NT 20,(result,SIG1left,END1right),rest671) end
| (108,(_,(MlyValue.Ident Ident1,Identleft as Ident1left,Identright
 as Ident1right))::rest671) => let val result=MlyValue.SigExp(fn _ => 
let val Ident as Ident1=Ident1 ()
 in ( SIGIDsigexp (PP Identleft Identright, mk_SigId Ident) ) end
)
 in (LrTable.NT 20,(result,Ident1left,Ident1right),rest671) end
| (109,(_,(MlyValue.WhereDesc WhereDesc1,_,WhereDesc1right))::_::(_,(
MlyValue.SigExp SigExp1,SigExpleft as SigExp1left,_))::rest671) => 
let val result=MlyValue.SigExp(fn _ => let val SigExp as SigExp1=
SigExp1 ()
val WhereDesc as WhereDesc1=WhereDesc1 ()
 in ( WhereDesc(SigExpleft,SigExp) ) end
)
 in (LrTable.NT 20,(result,SigExp1left,WhereDesc1right),rest671) end
| (110,(_,(MlyValue.AndWhereDesc_opt AndWhereDesc_opt1,_,
AndWhereDesc_opt1right))::(_,(MlyValue.Ty Ty1,_,Tyright))::_::(_,(
MlyValue.LongTypeIdent LongTypeIdent1,_,_))::(_,(MlyValue.TyVarSeq 
TyVarSeq1,_,_))::(_,(_,TYPE1left,_))::rest671) => let val result=
MlyValue.WhereDesc(fn _ => let val TyVarSeq as TyVarSeq1=TyVarSeq1 ()
val LongTypeIdent as LongTypeIdent1=LongTypeIdent1 ()
val Ty as Ty1=Ty1 ()
val AndWhereDesc_opt as AndWhereDesc_opt1=AndWhereDesc_opt1 ()
 in (
 fn (sigexpleft,sigexp) =>
                             case AndWhereDesc_opt
                               of SOME f => f (sigexpleft, WHERE_TYPEsigexp (PP sigexpleft Tyright,
					                                     sigexp, TyVarSeq,
					                                     mk_LongTyCon LongTypeIdent, Ty))
                                | NONE => WHERE_TYPEsigexp (PP sigexpleft Tyright,
					                    sigexp, TyVarSeq,
					                    mk_LongTyCon LongTypeIdent, Ty) 
) end
)
 in (LrTable.NT 43,(result,TYPE1left,AndWhereDesc_opt1right),rest671)
 end
| (111,(_,(MlyValue.WhereDesc WhereDesc1,_,WhereDesc1right))::(_,(_,
AND1left,_))::rest671) => let val result=MlyValue.AndWhereDesc_opt(fn 
_ => let val WhereDesc as WhereDesc1=WhereDesc1 ()
 in ( SOME WhereDesc ) end
)
 in (LrTable.NT 44,(result,AND1left,WhereDesc1right),rest671) end
| (112,rest671) => let val result=MlyValue.AndWhereDesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 44,(result,defaultPos,defaultPos),rest671) end
| (113,(_,(MlyValue.SigBind SigBind1,_,SigBind1right))::(_,(_,
SIGNATUREleft as SIGNATURE1left,_))::rest671) => let val result=
MlyValue.SigDec(fn _ => let val SigBind as SigBind1=SigBind1 ()
 in (
 SIGNATUREsigdec
			    (PP SIGNATUREleft (right (info_on_sigbind SigBind)),
			     SigBind) 
) end
)
 in (LrTable.NT 3,(result,SIGNATURE1left,SigBind1right),rest671) end
| (114,(_,(MlyValue.AndSigBind_opt AndSigBind_opt1,_,
AndSigBind_opt1right))::(_,(MlyValue.SigExp SigExp1,_,_))::_::(_,(
MlyValue.Ident Ident1,Identleft as Ident1left,_))::rest671) => let 
val result=MlyValue.SigBind(fn _ => let val Ident as Ident1=Ident1 ()
val SigExp as SigExp1=SigExp1 ()
val AndSigBind_opt as AndSigBind_opt1=AndSigBind_opt1 ()
 in (
 SIGBIND (PP Identleft
				     (rightmost info_on_sigexp SigExp
				                info_on_sigbind AndSigBind_opt),
                                   mk_SigId Ident, SigExp, AndSigBind_opt) 
) end
)
 in (LrTable.NT 19,(result,Ident1left,AndSigBind_opt1right),rest671)
 end
| (115,(_,(MlyValue.SigBind SigBind1,_,SigBind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AndSigBind_opt(fn _ => let 
val SigBind as SigBind1=SigBind1 ()
 in ( SOME SigBind ) end
)
 in (LrTable.NT 28,(result,AND1left,SigBind1right),rest671) end
| (116,rest671) => let val result=MlyValue.AndSigBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 28,(result,defaultPos,defaultPos),rest671) end
| (117,(_,(MlyValue.SCon SCon1,SConleft as SCon1left,SConright as 
SCon1right))::rest671) => let val result=MlyValue.AtExp(fn _ => let 
val SCon as SCon1=SCon1 ()
 in ( SCONatexp (PP SConleft SConright, SCon) ) end
)
 in (LrTable.NT 69,(result,SCon1left,SCon1right),rest671) end
| (118,(_,(MlyValue.LongOpEqIdent LongOpEqIdent1,LongOpEqIdentleft as 
LongOpEqIdent1left,LongOpEqIdentright as LongOpEqIdent1right))::
rest671) => let val result=MlyValue.AtExp(fn _ => let val 
LongOpEqIdent as LongOpEqIdent1=LongOpEqIdent1 ()
 in (
 let val OP_OPT (id, withOp) = LongOpEqIdent
                          in
			    IDENTatexp (PP LongOpEqIdentleft LongOpEqIdentright,
					OP_OPT (mk_LongId id, withOp))
                          end 
) end
)
 in (LrTable.NT 69,(result,LongOpEqIdent1left,LongOpEqIdent1right),
rest671) end
| (119,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.ExpRow_opt 
ExpRow_opt1,_,_))::(_,(_,LBRACEleft as LBRACE1left,_))::rest671) => 
let val result=MlyValue.AtExp(fn _ => let val ExpRow_opt as 
ExpRow_opt1=ExpRow_opt1 ()
 in ( RECORDatexp (PP LBRACEleft RBRACEright, ExpRow_opt) ) end
)
 in (LrTable.NT 69,(result,LBRACE1left,RBRACE1right),rest671) end
| (120,(_,(_,_,ENDright as END1right))::(_,(MlyValue.Exp_ Exp_1,_,_))
::_::(_,(MlyValue.Dec Dec1,_,_))::(_,(_,LETleft as LET1left,_))::
rest671) => let val result=MlyValue.AtExp(fn _ => let val Dec as Dec1=
Dec1 ()
val Exp_ as Exp_1=Exp_1 ()
 in ( LETatexp (PP LETleft ENDright, Dec, Exp_) ) end
)
 in (LrTable.NT 69,(result,LET1left,END1right),rest671) end
| (121,(_,(_,_,RPARENright as RPAREN1right))::(_,(MlyValue.Exp_ Exp_1,
_,_))::(_,(_,LPARENleft as LPAREN1left,_))::rest671) => let val result
=MlyValue.AtExp(fn _ => let val Exp_ as Exp_1=Exp_1 ()
 in ( PARatexp (PP LPARENleft RPARENright, Exp_) ) end
)
 in (LrTable.NT 69,(result,LPAREN1left,RPAREN1right),rest671) end
| (122,(_,(_,_,RPARENright as RPAREN1right))::(_,(_,LPARENleft as 
LPAREN1left,_))::rest671) => let val result=MlyValue.AtExp(fn _ => (
 RECORDatexp (PP LPARENleft RPARENright, NONE) ))
 in (LrTable.NT 69,(result,LPAREN1left,RPAREN1right),rest671) end
| (123,(_,(_,_,RPARENright as RPAREN1right))::(_,(
MlyValue.ExpComma_seq2 ExpComma_seq21,_,_))::(_,(_,LPARENleft as 
LPAREN1left,_))::rest671) => let val result=MlyValue.AtExp(fn _ => 
let val ExpComma_seq2 as ExpComma_seq21=ExpComma_seq21 ()
 in ( tuple_atexp_with_info (PP LPARENleft RPARENright) ExpComma_seq2 
) end
)
 in (LrTable.NT 69,(result,LPAREN1left,RPAREN1right),rest671) end
| (124,(_,(MlyValue.Label Label1,_,Labelright as Label1right))::(_,(_,
HASHleft as HASH1left,_))::rest671) => let val result=MlyValue.AtExp(
fn _ => let val Label as Label1=Label1 ()
 in ( hash (PP HASHleft Labelright) Label ) end
)
 in (LrTable.NT 69,(result,HASH1left,Label1right),rest671) end
| (125,(_,(_,_,RPARENright as RPAREN1right))::(_,(
MlyValue.ExpSemicolon_seq2 ExpSemicolon_seq21,_,_))::(_,(_,LPARENleft
 as LPAREN1left,_))::rest671) => let val result=MlyValue.AtExp(fn _
 => let val ExpSemicolon_seq2 as ExpSemicolon_seq21=ExpSemicolon_seq21
 ()
 in (
 PARatexp (PP LPARENleft RPARENright,
                                   sequenceExp ExpSemicolon_seq2) 
) end
)
 in (LrTable.NT 69,(result,LPAREN1left,RPAREN1right),rest671) end
| (126,(_,(_,_,ENDright as END1right))::(_,(MlyValue.ExpSemicolon_seq2
 ExpSemicolon_seq21,_,_))::_::(_,(MlyValue.Dec Dec1,_,_))::(_,(_,
LETleft as LET1left,_))::rest671) => let val result=MlyValue.AtExp(fn 
_ => let val Dec as Dec1=Dec1 ()
val ExpSemicolon_seq2 as ExpSemicolon_seq21=ExpSemicolon_seq21 ()
 in (
 LETatexp (PP LETleft ENDright,
				    Dec, sequenceExp ExpSemicolon_seq2) 
) end
)
 in (LrTable.NT 69,(result,LET1left,END1right),rest671) end
| (127,(_,(_,_,RBRACKETright as RBRACKET1right))::(_,(
MlyValue.ExpComma_seq0 ExpComma_seq01,_,_))::(_,(_,LBRACKETleft as 
LBRACKET1left,_))::rest671) => let val result=MlyValue.AtExp(fn _ => 
let val ExpComma_seq0 as ExpComma_seq01=ExpComma_seq01 ()
 in ( list_atexp (PP LBRACKETleft RBRACKETright) ExpComma_seq0 ) end
)
 in (LrTable.NT 69,(result,LBRACKET1left,RBRACKET1right),rest671) end
| (128,(_,(MlyValue.AtExp_seq1 AtExp_seq11,_,AtExp_seq11right))::(_,(
MlyValue.AtExp AtExp1,AtExp1left,_))::rest671) => let val result=
MlyValue.AtExp_seq1(fn _ => let val AtExp as AtExp1=AtExp1 ()
val AtExp_seq1 as AtExp_seq11=AtExp_seq11 ()
 in ( AtExp :: AtExp_seq1 ) end
)
 in (LrTable.NT 96,(result,AtExp1left,AtExp_seq11right),rest671) end
| (129,(_,(MlyValue.AtExp AtExp1,AtExp1left,AtExp1right))::rest671)
 => let val result=MlyValue.AtExp_seq1(fn _ => let val AtExp as AtExp1
=AtExp1 ()
 in ( [AtExp] ) end
)
 in (LrTable.NT 96,(result,AtExp1left,AtExp1right),rest671) end
| (130,(_,(MlyValue.CommaExpRow_opt CommaExpRow_opt1,_,
CommaExpRow_opt1right))::(_,(MlyValue.Exp_ Exp_1,_,_))::_::(_,(
MlyValue.Label Label1,Labelleft as Label1left,_))::rest671) => let 
val result=MlyValue.ExpRow(fn _ => let val Label as Label1=Label1 ()
val Exp_ as Exp_1=Exp_1 ()
val CommaExpRow_opt as CommaExpRow_opt1=CommaExpRow_opt1 ()
 in (
 EXPROW (PP Labelleft
				    (rightmost get_info_exp Exp_
				               get_info_exprow CommaExpRow_opt),
				  Label, Exp_, CommaExpRow_opt) 
) end
)
 in (LrTable.NT 68,(result,Label1left,CommaExpRow_opt1right),rest671)
 end
| (131,(_,(MlyValue.ExpRow ExpRow1,ExpRow1left,ExpRow1right))::rest671
) => let val result=MlyValue.ExpRow_opt(fn _ => let val ExpRow as 
ExpRow1=ExpRow1 ()
 in ( SOME ExpRow ) end
)
 in (LrTable.NT 81,(result,ExpRow1left,ExpRow1right),rest671) end
| (132,rest671) => let val result=MlyValue.ExpRow_opt(fn _ => ( NONE )
)
 in (LrTable.NT 81,(result,defaultPos,defaultPos),rest671) end
| (133,(_,(MlyValue.ExpRow ExpRow1,_,ExpRow1right))::(_,(_,COMMA1left,
_))::rest671) => let val result=MlyValue.CommaExpRow_opt(fn _ => let 
val ExpRow as ExpRow1=ExpRow1 ()
 in ( SOME ExpRow ) end
)
 in (LrTable.NT 71,(result,COMMA1left,ExpRow1right),rest671) end
| (134,rest671) => let val result=MlyValue.CommaExpRow_opt(fn _ => (
 NONE ))
 in (LrTable.NT 71,(result,defaultPos,defaultPos),rest671) end
| (135,(_,(MlyValue.ExpComma_seq1 ExpComma_seq11,ExpComma_seq11left,
ExpComma_seq11right))::rest671) => let val result=
MlyValue.ExpComma_seq0(fn _ => let val ExpComma_seq1 as ExpComma_seq11
=ExpComma_seq11 ()
 in ( ExpComma_seq1 ) end
)
 in (LrTable.NT 90,(result,ExpComma_seq11left,ExpComma_seq11right),
rest671) end
| (136,rest671) => let val result=MlyValue.ExpComma_seq0(fn _ => (
 nil ))
 in (LrTable.NT 90,(result,defaultPos,defaultPos),rest671) end
| (137,(_,(MlyValue.ExpComma_seq1 ExpComma_seq11,_,ExpComma_seq11right
))::_::(_,(MlyValue.Exp_ Exp_1,Exp_1left,_))::rest671) => let val 
result=MlyValue.ExpComma_seq1(fn _ => let val Exp_ as Exp_1=Exp_1 ()
val ExpComma_seq1 as ExpComma_seq11=ExpComma_seq11 ()
 in ( Exp_ :: ExpComma_seq1 ) end
)
 in (LrTable.NT 91,(result,Exp_1left,ExpComma_seq11right),rest671) end
| (138,(_,(MlyValue.Exp_ Exp_1,Exp_1left,Exp_1right))::rest671) => 
let val result=MlyValue.ExpComma_seq1(fn _ => let val Exp_ as Exp_1=
Exp_1 ()
 in ( [Exp_] ) end
)
 in (LrTable.NT 91,(result,Exp_1left,Exp_1right),rest671) end
| (139,(_,(MlyValue.ExpComma_seq1 ExpComma_seq11,_,ExpComma_seq11right
))::_::(_,(MlyValue.Exp_ Exp_1,Exp_1left,_))::rest671) => let val 
result=MlyValue.ExpComma_seq2(fn _ => let val Exp_ as Exp_1=Exp_1 ()
val ExpComma_seq1 as ExpComma_seq11=ExpComma_seq11 ()
 in ( Exp_ :: ExpComma_seq1 ) end
)
 in (LrTable.NT 92,(result,Exp_1left,ExpComma_seq11right),rest671) end
| (140,(_,(MlyValue.ExpSemicolon_seq2 ExpSemicolon_seq21,_,
ExpSemicolon_seq21right))::_::(_,(MlyValue.Exp_ Exp_1,Exp_1left,_))::
rest671) => let val result=MlyValue.ExpSemicolon_seq2(fn _ => let val 
Exp_ as Exp_1=Exp_1 ()
val ExpSemicolon_seq2 as ExpSemicolon_seq21=ExpSemicolon_seq21 ()
 in ( Exp_ :: ExpSemicolon_seq2) end
)
 in (LrTable.NT 99,(result,Exp_1left,ExpSemicolon_seq21right),rest671)
 end
| (141,(_,(MlyValue.Exp_ Exp_2,_,Exp_2right))::_::(_,(MlyValue.Exp_ 
Exp_1,Exp_1left,_))::rest671) => let val result=
MlyValue.ExpSemicolon_seq2(fn _ => let val Exp_1=Exp_1 ()
val Exp_2=Exp_2 ()
 in ( [Exp_1, Exp_2] ) end
)
 in (LrTable.NT 99,(result,Exp_1left,Exp_2right),rest671) end
| (142,(_,(MlyValue.AtExp_seq1 AtExp_seq11,AtExp_seq1left as 
AtExp_seq11left,AtExp_seq1right as AtExp_seq11right))::rest671) => 
let val result=MlyValue.Exp_(fn _ => let val AtExp_seq1 as AtExp_seq11
=AtExp_seq11 ()
 in ( UNRES_INFIXexp (PP AtExp_seq1left AtExp_seq1right, AtExp_seq1) )
 end
)
 in (LrTable.NT 66,(result,AtExp_seq11left,AtExp_seq11right),rest671)
 end
| (143,(_,(MlyValue.Ty Ty1,_,Tyright as Ty1right))::_::(_,(
MlyValue.Exp_ Exp_1,Exp_left as Exp_1left,_))::rest671) => let val 
result=MlyValue.Exp_(fn _ => let val Exp_ as Exp_1=Exp_1 ()
val Ty as Ty1=Ty1 ()
 in ( TYPEDexp (PP Exp_left Tyright, Exp_, Ty) ) end
)
 in (LrTable.NT 66,(result,Exp_1left,Ty1right),rest671) end
| (144,(_,(MlyValue.Match_ Match_1,_,Match_1right))::_::(_,(
MlyValue.Exp_ Exp_1,Exp_left as Exp_1left,_))::rest671) => let val 
result=MlyValue.Exp_(fn _ => let val Exp_ as Exp_1=Exp_1 ()
val Match_ as Match_1=Match_1 ()
 in (
 HANDLEexp (PP Exp_left (right (get_info_match Match_)),
				     Exp_, Match_) 
) end
)
 in (LrTable.NT 66,(result,Exp_1left,Match_1right),rest671) end
| (145,(_,(MlyValue.Exp_ Exp_1,_,Exp_1right))::(_,(_,RAISEleft as 
RAISE1left,_))::rest671) => let val result=MlyValue.Exp_(fn _ => let 
val Exp_ as Exp_1=Exp_1 ()
 in ( RAISEexp (PP RAISEleft (right (get_info_exp Exp_)), Exp_) ) end
)
 in (LrTable.NT 66,(result,RAISE1left,Exp_1right),rest671) end
| (146,(_,(MlyValue.Match_ Match_1,_,Match_1right))::(_,(_,FNleft as 
FN1left,_))::rest671) => let val result=MlyValue.Exp_(fn _ => let val 
Match_ as Match_1=Match_1 ()
 in ( FNexp (PP FNleft (right (get_info_match Match_)), Match_) ) end
)
 in (LrTable.NT 66,(result,FN1left,Match_1right),rest671) end
| (147,(_,(MlyValue.Match_ Match_1,_,Match_1right))::_::(_,(
MlyValue.Exp_ Exp_1,_,_))::(_,(_,CASEleft as CASE1left,_))::rest671)
 => let val result=MlyValue.Exp_(fn _ => let val Exp_ as Exp_1=Exp_1 
()
val Match_ as Match_1=Match_1 ()
 in (
 case_exp (PP CASEleft (right (get_info_match Match_)))
			    (Exp_, Match_) 
) end
)
 in (LrTable.NT 66,(result,CASE1left,Match_1right),rest671) end
| (148,(_,(MlyValue.Exp_ Exp_3,_,Exp_3right))::_::(_,(MlyValue.Exp_ 
Exp_2,_,_))::_::(_,(MlyValue.Exp_ Exp_1,_,_))::(_,(_,IFleft as IF1left
,_))::rest671) => let val result=MlyValue.Exp_(fn _ => let val Exp_1=
Exp_1 ()
val Exp_2=Exp_2 ()
val Exp_3=Exp_3 ()
 in (
 if_then_else_exp (PP IFleft (right (get_info_exp Exp_3)))
			    (Exp_1, Exp_2, Exp_3) 
) end
)
 in (LrTable.NT 66,(result,IF1left,Exp_3right),rest671) end
| (149,(_,(MlyValue.Exp_ Exp_2,_,Exp_2right))::_::(_,(MlyValue.Exp_ 
Exp_1,Exp_1left,_))::rest671) => let val result=MlyValue.Exp_(fn _ => 
let val Exp_1=Exp_1 ()
val Exp_2=Exp_2 ()
 in (
 let val info = PP Exp_1left (right (get_info_exp Exp_2))
			  in
			    if_then_else_exp info (Exp_1, exp_true info, Exp_2)
			  end 
) end
)
 in (LrTable.NT 66,(result,Exp_1left,Exp_2right),rest671) end
| (150,(_,(MlyValue.Exp_ Exp_2,_,Exp_2right))::_::(_,(MlyValue.Exp_ 
Exp_1,Exp_1left,_))::rest671) => let val result=MlyValue.Exp_(fn _ => 
let val Exp_1=Exp_1 ()
val Exp_2=Exp_2 ()
 in (
 let val info = PP Exp_1left (right (get_info_exp Exp_2))
			  in
			    if_then_else_exp info (Exp_1, Exp_2, exp_false info)
			  end 
) end
)
 in (LrTable.NT 66,(result,Exp_1left,Exp_2right),rest671) end
| (151,(_,(MlyValue.Exp_ Exp_2,_,Exp_2right))::_::(_,(MlyValue.Exp_ 
Exp_1,_,_))::(_,(_,WHILEleft as WHILE1left,_))::rest671) => let val 
result=MlyValue.Exp_(fn _ => let val Exp_1=Exp_1 ()
val Exp_2=Exp_2 ()
 in (
 while_exp (PP WHILEleft (right (get_info_exp Exp_2)))
			    (Exp_1, Exp_2) 
) end
)
 in (LrTable.NT 66,(result,WHILE1left,Exp_2right),rest671) end
| (152,(_,(MlyValue.BarMatch_opt BarMatch_opt1,_,BarMatch_opt1right))
::(_,(MlyValue.MRule MRule1,MRuleleft as MRule1left,_))::rest671) => 
let val result=MlyValue.Match_(fn _ => let val MRule as MRule1=MRule1 
()
val BarMatch_opt as BarMatch_opt1=BarMatch_opt1 ()
 in (
 MATCH (PP MRuleleft
				   (rightmost get_info_mrule MRule
				              get_info_match BarMatch_opt), 
				 MRule, BarMatch_opt) 
) end
)
 in (LrTable.NT 67,(result,MRule1left,BarMatch_opt1right),rest671) end
| (153,(_,(MlyValue.Match_ Match_1,_,Match_1right))::(_,(_,BAR1left,_)
)::rest671) => let val result=MlyValue.BarMatch_opt(fn _ => let val 
Match_ as Match_1=Match_1 ()
 in ( SOME Match_ ) end
)
 in (LrTable.NT 79,(result,BAR1left,Match_1right),rest671) end
| (154,rest671) => let val result=MlyValue.BarMatch_opt(fn _ => (
 NONE ))
 in (LrTable.NT 79,(result,defaultPos,defaultPos),rest671) end
| (155,(_,(MlyValue.Exp_ Exp_1,_,Exp_1right))::_::(_,(MlyValue.Pat 
Pat1,Patleft as Pat1left,_))::rest671) => let val result=
MlyValue.MRule(fn _ => let val Pat as Pat1=Pat1 ()
val Exp_ as Exp_1=Exp_1 ()
 in ( MRULE (PP Patleft (right (get_info_exp Exp_)), Pat, Exp_) ) end
)
 in (LrTable.NT 65,(result,Pat1left,Exp_1right),rest671) end
| (156,(_,(MlyValue.ValBind ValBind1,_,ValBind1right))::(_,(_,VALleft
 as VAL1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val ValBind as ValBind1=
ValBind1 ()
 in (
 VALdec (PP VALleft (right (get_info_valbind ValBind)),
				  [], ValBind) 
) end
)
 in (LrTable.NT 112,(result,VAL1left,ValBind1right),rest671) end
| (157,(_,(MlyValue.ValBind ValBind1,_,ValBind1right))::(_,(
MlyValue.TyVarSeq1 TyVarSeq11,_,_))::(_,(_,VALleft as VAL1left,_))::
rest671) => let val result=MlyValue.OneDec_sans_LOCAL(fn _ => let val 
TyVarSeq1 as TyVarSeq11=TyVarSeq11 ()
val ValBind as ValBind1=ValBind1 ()
 in (
 VALdec (PP VALleft (right (get_info_valbind ValBind)),
				  TyVarSeq1, ValBind) 
) end
)
 in (LrTable.NT 112,(result,VAL1left,ValBind1right),rest671) end
| (158,(_,(MlyValue.TypBind TypBind1,_,TypBind1right))::(_,(_,TYPEleft
 as TYPE1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val TypBind as TypBind1=
TypBind1 ()
 in (
 TYPEdec (PP TYPEleft (right (get_info_typbind TypBind)),
				   TypBind) 
) end
)
 in (LrTable.NT 112,(result,TYPE1left,TypBind1right),rest671) end
| (159,(_,(MlyValue.DatBind_zero_arity DatBind_zero_arity1,_,
DatBind_zero_arity1right))::(_,(_,DATATYPEleft as DATATYPE1left,_))::
rest671) => let val result=MlyValue.OneDec_sans_LOCAL(fn _ => let val 
DatBind_zero_arity as DatBind_zero_arity1=DatBind_zero_arity1 ()
 in (
 DATATYPEdec (PP DATATYPEleft
				         (right (get_info_datbind DatBind_zero_arity)),
				       DatBind_zero_arity) 
) end
)
 in (LrTable.NT 112,(result,DATATYPE1left,DatBind_zero_arity1right),
rest671) end
| (160,(_,(MlyValue.DatBind_nonzero_arity DatBind_nonzero_arity1,_,
DatBind_nonzero_arity1right))::(_,(_,DATATYPEleft as DATATYPE1left,_))
::rest671) => let val result=MlyValue.OneDec_sans_LOCAL(fn _ => let 
val DatBind_nonzero_arity as DatBind_nonzero_arity1=
DatBind_nonzero_arity1 ()
 in (
 DATATYPEdec(PP DATATYPEleft
				        (right (get_info_datbind DatBind_nonzero_arity)),
				      DatBind_nonzero_arity) 
) end
)
 in (LrTable.NT 112,(result,DATATYPE1left,DatBind_nonzero_arity1right)
,rest671) end
| (161,(_,(MlyValue.LongTypeIdent LongTypeIdent1,_,LongTypeIdentright
 as LongTypeIdent1right))::_::_::(_,(MlyValue.TypeIdent TypeIdent1,_,_
))::(_,(_,DATATYPE1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val TypeIdent as TypeIdent1=
TypeIdent1 ()
val LongTypeIdent as LongTypeIdent1=LongTypeIdent1 ()
 in (
 DATATYPE_REPLICATIONdec
			    (PP DATATYPE1left LongTypeIdentright,
			     mk_TyCon TypeIdent, mk_LongTyCon LongTypeIdent) 
) end
)
 in (LrTable.NT 112,(result,DATATYPE1left,LongTypeIdent1right),rest671
) end
| (162,(_,(_,_,ENDright as END1right))::(_,(MlyValue.Dec Dec1,_,_))::_
::(_,(MlyValue.DatBind DatBind1,_,_))::(_,(_,ABSTYPEleft as 
ABSTYPE1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val DatBind as DatBind1=
DatBind1 ()
val Dec as Dec1=Dec1 ()
 in ( ABSTYPEdec (PP ABSTYPEleft ENDright, DatBind, Dec) ) end
)
 in (LrTable.NT 112,(result,ABSTYPE1left,END1right),rest671) end
| (163,(_,(MlyValue.ExBind ExBind1,_,ExBind1right))::(_,(_,
EXCEPTIONleft as EXCEPTION1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val ExBind as ExBind1=ExBind1 
()
 in (
 EXCEPTIONdec (PP EXCEPTIONleft
					  (right (get_info_exbind ExBind)),
					ExBind) 
) end
)
 in (LrTable.NT 112,(result,EXCEPTION1left,ExBind1right),rest671) end
| (164,(_,(MlyValue.LongIdent_seq1 LongIdent_seq11,_,
LongIdent_seq1right as LongIdent_seq11right))::(_,(_,OPENleft as 
OPEN1left,_))::rest671) => let val result=MlyValue.OneDec_sans_LOCAL(
fn _ => let val LongIdent_seq1 as LongIdent_seq11=LongIdent_seq11 ()
 in (
 OPENdec (PP OPENleft LongIdent_seq1right,
				   wi_Convert mk_LongStrId LongIdent_seq1) 
) end
)
 in (LrTable.NT 112,(result,OPEN1left,LongIdent_seq11right),rest671)
 end
| (165,(_,(MlyValue.EqIdent_seq1 EqIdent_seq11,_,EqIdent_seq1right as 
EqIdent_seq11right))::(_,(MlyValue.DIGIT_opt DIGIT_opt1,_,_))::(_,(_,
INFIXleft as INFIX1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val DIGIT_opt as DIGIT_opt1=
DIGIT_opt1 ()
val EqIdent_seq1 as EqIdent_seq11=EqIdent_seq11 ()
 in (
 INFIXdec (PP INFIXleft EqIdent_seq1right,
				    DIGIT_opt, map mk_Id EqIdent_seq1) 
) end
)
 in (LrTable.NT 112,(result,INFIX1left,EqIdent_seq11right),rest671)
 end
| (166,(_,(MlyValue.EqIdent_seq1 EqIdent_seq11,_,EqIdent_seq1right as 
EqIdent_seq11right))::(_,(MlyValue.DIGIT_opt DIGIT_opt1,_,_))::(_,(_,
INFIXRleft as INFIXR1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val DIGIT_opt as DIGIT_opt1=
DIGIT_opt1 ()
val EqIdent_seq1 as EqIdent_seq11=EqIdent_seq11 ()
 in (
 INFIXRdec (PP INFIXRleft EqIdent_seq1right,
				     DIGIT_opt, map mk_Id EqIdent_seq1) 
) end
)
 in (LrTable.NT 112,(result,INFIXR1left,EqIdent_seq11right),rest671)
 end
| (167,(_,(MlyValue.EqIdent_seq1 EqIdent_seq11,_,EqIdent_seq1right as 
EqIdent_seq11right))::(_,(_,NONFIXleft as NONFIX1left,_))::rest671)
 => let val result=MlyValue.OneDec_sans_LOCAL(fn _ => let val 
EqIdent_seq1 as EqIdent_seq11=EqIdent_seq11 ()
 in (
 NONFIXdec (PP NONFIXleft EqIdent_seq1right,
				     map mk_Id EqIdent_seq1) 
) end
)
 in (LrTable.NT 112,(result,NONFIX1left,EqIdent_seq11right),rest671)
 end
| (168,(_,(MlyValue.TypBind TypBind1,_,TypBind1right))::_::(_,(
MlyValue.DatBind_zero_arity DatBind_zero_arity1,_,_))::(_,(_,
DATATYPEleft as DATATYPE1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val DatBind_zero_arity as 
DatBind_zero_arity1=DatBind_zero_arity1 ()
val TypBind as TypBind1=TypBind1 ()
 in (
 let val db = rewriteDatBind (DatBind_zero_arity, TypBind)
                          in
                            SEQdec (PP DATATYPEleft (right (get_info_typbind TypBind)),
				    DATATYPEdec
				      (get_info_datbind DatBind_zero_arity, db),
				    TYPEdec (get_info_typbind TypBind, TypBind))
                          end 
) end
)
 in (LrTable.NT 112,(result,DATATYPE1left,TypBind1right),rest671) end
| (169,(_,(MlyValue.TypBind TypBind1,_,TypBind1right))::_::(_,(
MlyValue.DatBind_nonzero_arity DatBind_nonzero_arity1,_,_))::(_,(_,
DATATYPEleft as DATATYPE1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val DatBind_nonzero_arity as 
DatBind_nonzero_arity1=DatBind_nonzero_arity1 ()
val TypBind as TypBind1=TypBind1 ()
 in (
 let val db = rewriteDatBind (DatBind_nonzero_arity, TypBind)
                          in
                            SEQdec (PP DATATYPEleft (right (get_info_typbind TypBind)),
				    DATATYPEdec
				      (get_info_datbind DatBind_nonzero_arity, db),
				    TYPEdec (get_info_typbind TypBind, TypBind))
                          end 
) end
)
 in (LrTable.NT 112,(result,DATATYPE1left,TypBind1right),rest671) end
| (170,(_,(_,_,ENDright as END1right))::(_,(MlyValue.Dec Dec1,_,_))::_
::(_,(MlyValue.TypBind TypBind1,TypBindleft,_))::_::(_,(
MlyValue.DatBind DatBind1,_,_))::(_,(_,ABSTYPEleft as ABSTYPE1left,_))
::rest671) => let val result=MlyValue.OneDec_sans_LOCAL(fn _ => let 
val DatBind as DatBind1=DatBind1 ()
val TypBind as TypBind1=TypBind1 ()
val Dec as Dec1=Dec1 ()
 in (
 let val db = rewriteDatBind (DatBind, TypBind)
                          in
                            ABSTYPEdec
			      (PP ABSTYPEleft ENDright, db,
			       SEQdec (PP TypBindleft (right (get_info_dec Dec)),
				       TYPEdec (get_info_typbind TypBind, TypBind),
				       Dec))
                          end 
) end
)
 in (LrTable.NT 112,(result,ABSTYPE1left,END1right),rest671) end
| (171,(_,(MlyValue.FValBind FValBind1,_,FValBind1right))::(_,(_,
FUNleft as FUN1left,_))::rest671) => let val result=
MlyValue.OneDec_sans_LOCAL(fn _ => let val FValBind as FValBind1=
FValBind1 ()
 in (
 UNRES_FUNdec (PP FUNleft (right (get_info_FValBind FValBind)),
					[], FValBind) 
) end
)
 in (LrTable.NT 112,(result,FUN1left,FValBind1right),rest671) end
| (172,(_,(MlyValue.FValBind FValBind1,_,FValBind1right))::(_,(
MlyValue.TyVarSeq1 TyVarSeq11,_,_))::(_,(_,FUNleft as FUN1left,_))::
rest671) => let val result=MlyValue.OneDec_sans_LOCAL(fn _ => let val 
TyVarSeq1 as TyVarSeq11=TyVarSeq11 ()
val FValBind as FValBind1=FValBind1 ()
 in (
 UNRES_FUNdec (PP FUNleft (right (get_info_FValBind FValBind)),
					TyVarSeq1, FValBind) 
) end
)
 in (LrTable.NT 112,(result,FUN1left,FValBind1right),rest671) end
| (173,(_,(_,_,ENDright as END1right))::(_,(MlyValue.Dec Dec2,_,_))::_
::(_,(MlyValue.Dec Dec1,_,_))::(_,(_,LOCALleft as LOCAL1left,_))::
rest671) => let val result=MlyValue.OneDec(fn _ => let val Dec1=Dec1 
()
val Dec2=Dec2 ()
 in ( LOCALdec (PP LOCALleft ENDright, Dec1, Dec2) ) end
)
 in (LrTable.NT 111,(result,LOCAL1left,END1right),rest671) end
| (174,(_,(MlyValue.OneDec_sans_LOCAL OneDec_sans_LOCAL1,
OneDec_sans_LOCAL1left,OneDec_sans_LOCAL1right))::rest671) => let val 
result=MlyValue.OneDec(fn _ => let val OneDec_sans_LOCAL as 
OneDec_sans_LOCAL1=OneDec_sans_LOCAL1 ()
 in ( OneDec_sans_LOCAL ) end
)
 in (LrTable.NT 111,(result,OneDec_sans_LOCAL1left,
OneDec_sans_LOCAL1right),rest671) end
| (175,(_,(MlyValue.OneDec OneDec1,OneDec1left,OneDec1right))::rest671
) => let val result=MlyValue.OneDec_or_SEMICOLON(fn _ => let val 
OneDec as OneDec1=OneDec1 ()
 in ( SOME OneDec ) end
)
 in (LrTable.NT 103,(result,OneDec1left,OneDec1right),rest671) end
| (176,(_,(_,SEMICOLON1left,SEMICOLON1right))::rest671) => let val 
result=MlyValue.OneDec_or_SEMICOLON(fn _ => ( NONE ))
 in (LrTable.NT 103,(result,SEMICOLON1left,SEMICOLON1right),rest671)
 end
| (177,(_,(MlyValue.OneDec_or_SEMICOLON OneDec_or_SEMICOLON1,_,
OneDec_or_SEMICOLON1right))::(_,(MlyValue.NonEmptyDec NonEmptyDec1,
NonEmptyDecleft as NonEmptyDec1left,_))::rest671) => let val result=
MlyValue.NonEmptyDec(fn _ => let val NonEmptyDec as NonEmptyDec1=
NonEmptyDec1 ()
val OneDec_or_SEMICOLON as OneDec_or_SEMICOLON1=OneDec_or_SEMICOLON1 
()
 in (
 (case OneDec_or_SEMICOLON of
			     SOME dec =>
			       composeDec (PP NonEmptyDecleft
					     (right (get_info_dec dec)),
					   NonEmptyDec, dec)
			   | NONE => NonEmptyDec) 
) end
)
 in (LrTable.NT 104,(result,NonEmptyDec1left,OneDec_or_SEMICOLON1right
),rest671) end
| (178,(_,(MlyValue.OneDec_or_SEMICOLON OneDec_or_SEMICOLON1,
OneDec_or_SEMICOLON1left,OneDec_or_SEMICOLON1right))::rest671) => let 
val result=MlyValue.NonEmptyDec(fn _ => let val OneDec_or_SEMICOLON
 as OneDec_or_SEMICOLON1=OneDec_or_SEMICOLON1 ()
 in (
 (case OneDec_or_SEMICOLON of
			     SOME dec => dec
			   | NONE => EMPTYdec (PP defaultPos defaultPos)) 
) end
)
 in (LrTable.NT 104,(result,OneDec_or_SEMICOLON1left,
OneDec_or_SEMICOLON1right),rest671) end
| (179,(_,(MlyValue.NonEmptyDec NonEmptyDec1,NonEmptyDec1left,
NonEmptyDec1right))::rest671) => let val result=MlyValue.Dec(fn _ => 
let val NonEmptyDec as NonEmptyDec1=NonEmptyDec1 ()
 in ( NonEmptyDec ) end
)
 in (LrTable.NT 64,(result,NonEmptyDec1left,NonEmptyDec1right),rest671
) end
| (180,rest671) => let val result=MlyValue.Dec(fn _ => (
 EMPTYdec (PP defaultPos defaultPos) ))
 in (LrTable.NT 64,(result,defaultPos,defaultPos),rest671) end
| (181,(_,(MlyValue.AndValBind_opt AndValBind_opt1,_,
AndValBind_opt1right))::(_,(MlyValue.Exp_ Exp_1,_,_))::_::(_,(
MlyValue.Pat Pat1,Patleft as Pat1left,_))::rest671) => let val result=
MlyValue.ValBind(fn _ => let val Pat as Pat1=Pat1 ()
val Exp_ as Exp_1=Exp_1 ()
val AndValBind_opt as AndValBind_opt1=AndValBind_opt1 ()
 in (
 PLAINvalbind (PP Patleft
					  (rightmost get_info_exp Exp_
					             get_info_valbind AndValBind_opt), 
					Pat, Exp_, AndValBind_opt) 
) end
)
 in (LrTable.NT 61,(result,Pat1left,AndValBind_opt1right),rest671) end
| (182,(_,(MlyValue.FnValBind FnValBind1,_,FnValBind1right))::(_,(_,
RECleft as REC1left,_))::rest671) => let val result=MlyValue.ValBind(
fn _ => let val FnValBind as FnValBind1=FnValBind1 ()
 in (
 RECvalbind (PP RECleft
				        (right (get_info_valbind FnValBind)),
				      FnValBind) 
) end
)
 in (LrTable.NT 61,(result,REC1left,FnValBind1right),rest671) end
| (183,(_,(MlyValue.ValBind ValBind1,_,ValBind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AndValBind_opt(fn _ => let 
val ValBind as ValBind1=ValBind1 ()
 in ( SOME ValBind ) end
)
 in (LrTable.NT 72,(result,AND1left,ValBind1right),rest671) end
| (184,rest671) => let val result=MlyValue.AndValBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 72,(result,defaultPos,defaultPos),rest671) end
| (185,(_,(MlyValue.AndFnValBind_opt AndFnValBind_opt1,_,
AndFnValBind_opt1right))::(_,(MlyValue.Match_ Match_1,_,_))::(_,(_,
FNleft,_))::_::(_,(MlyValue.Pat Pat1,Patleft as Pat1left,_))::rest671)
 => let val result=MlyValue.FnValBind(fn _ => let val Pat as Pat1=Pat1
 ()
val Match_ as Match_1=Match_1 ()
val AndFnValBind_opt as AndFnValBind_opt1=AndFnValBind_opt1 ()
 in (
 PLAINvalbind
			    (PP Patleft
			       (rightmost get_info_match Match_
				          get_info_valbind AndFnValBind_opt),
			       Pat,
			       FNexp (PP FNleft (right (get_info_match Match_)), Match_),
			       AndFnValBind_opt) 
) end
)
 in (LrTable.NT 62,(result,Pat1left,AndFnValBind_opt1right),rest671)
 end
| (186,(_,(MlyValue.AndFnValBind_opt AndFnValBind_opt1,_,
AndFnValBind_opt1right))::(_,(MlyValue.ColonTy_seq1 ColonTy_seq11,_,
ColonTy_seq1right))::(_,(_,_,RPARENright))::(_,(MlyValue.Match_ 
Match_1,_,_))::(_,(_,FNleft,_))::(_,(_,LPARENleft,_))::_::(_,(
MlyValue.Pat Pat1,Patleft as Pat1left,_))::rest671) => let val result=
MlyValue.FnValBind(fn _ => let val Pat as Pat1=Pat1 ()
val Match_ as Match_1=Match_1 ()
val ColonTy_seq1 as ColonTy_seq11=ColonTy_seq11 ()
val AndFnValBind_opt as AndFnValBind_opt1=AndFnValBind_opt1 ()
 in (
 let
                            val fnExp =
			          FNexp (PP FNleft (right (get_info_match Match_)), Match_)
                            val parAtexp =
                                  PARatexp (PP LPARENleft RPARENright, fnExp)
                            val atexpExp =
                                  ATEXPexp (PP LPARENleft RPARENright, parAtexp)

                            fun attachTypes (e, nil) = e
                              | attachTypes (e, (ty, p) :: rest) =
                                  attachTypes (TYPEDexp (PP FNleft p, e, ty), rest)
                          in
                            PLAINvalbind (PP Patleft
					    (rightmost' ColonTy_seq1right
					                get_info_valbind AndFnValBind_opt),
					  Pat,
					  attachTypes (atexpExp, ColonTy_seq1),
					  AndFnValBind_opt)
                          end 
) end
)
 in (LrTable.NT 62,(result,Pat1left,AndFnValBind_opt1right),rest671)
 end
| (187,(_,(MlyValue.FnValBind FnValBind1,_,FnValBind1right))::(_,(_,
RECleft as REC1left,_))::rest671) => let val result=MlyValue.FnValBind
(fn _ => let val FnValBind as FnValBind1=FnValBind1 ()
 in (
 RECvalbind (PP RECleft
				        (right (get_info_valbind FnValBind)),
				      FnValBind) 
) end
)
 in (LrTable.NT 62,(result,REC1left,FnValBind1right),rest671) end
| (188,(_,(MlyValue.ColonTy_seq1 ColonTy_seq11,_,ColonTy_seq11right))
::(_,(MlyValue.Ty Ty1,_,Tyright))::(_,(_,COLON1left,_))::rest671) => 
let val result=MlyValue.ColonTy_seq1(fn _ => let val Ty as Ty1=Ty1 ()
val ColonTy_seq1 as ColonTy_seq11=ColonTy_seq11 ()
 in ( (Ty, Tyright) :: ColonTy_seq1 ) end
)
 in (LrTable.NT 86,(result,COLON1left,ColonTy_seq11right),rest671) end
| (189,(_,(MlyValue.Ty Ty1,_,Tyright as Ty1right))::(_,(_,COLON1left,_
))::rest671) => let val result=MlyValue.ColonTy_seq1(fn _ => let val 
Ty as Ty1=Ty1 ()
 in ( [(Ty, Tyright)] ) end
)
 in (LrTable.NT 86,(result,COLON1left,Ty1right),rest671) end
| (190,(_,(MlyValue.FnValBind FnValBind1,_,FnValBind1right))::(_,(_,
AND1left,_))::rest671) => let val result=MlyValue.AndFnValBind_opt(fn 
_ => let val FnValBind as FnValBind1=FnValBind1 ()
 in ( SOME FnValBind ) end
)
 in (LrTable.NT 73,(result,AND1left,FnValBind1right),rest671) end
| (191,rest671) => let val result=MlyValue.AndFnValBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 73,(result,defaultPos,defaultPos),rest671) end
| (192,(_,(MlyValue.AndTypBind_opt AndTypBind_opt1,_,
AndTypBind_opt1right))::(_,(MlyValue.Ty Ty1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,_,_))::(_,(MlyValue.TyVarSeq TyVarSeq1,
TyVarSeqleft as TyVarSeq1left,_))::rest671) => let val result=
MlyValue.TypBind(fn _ => let val TyVarSeq as TyVarSeq1=TyVarSeq1 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val Ty as Ty1=Ty1 ()
val AndTypBind_opt as AndTypBind_opt1=AndTypBind_opt1 ()
 in (
 TYPBIND (PP TyVarSeqleft
				     (rightmost get_info_ty Ty
				                get_info_typbind AndTypBind_opt),
				   TyVarSeq, mk_TyCon TypeIdent, Ty, AndTypBind_opt) 
) end
)
 in (LrTable.NT 60,(result,TyVarSeq1left,AndTypBind_opt1right),rest671
) end
| (193,(_,(MlyValue.TypBind TypBind1,_,TypBind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AndTypBind_opt(fn _ => let 
val TypBind as TypBind1=TypBind1 ()
 in ( SOME TypBind ) end
)
 in (LrTable.NT 76,(result,AND1left,TypBind1right),rest671) end
| (194,rest671) => let val result=MlyValue.AndTypBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 76,(result,defaultPos,defaultPos),rest671) end
| (195,(_,(MlyValue.AndDatBind_opt AndDatBind_opt1,_,
AndDatBind_opt1right))::(_,(MlyValue.ConBind ConBind1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,_,_))::(_,(MlyValue.TyVarSeq TyVarSeq1,
TyVarSeqleft as TyVarSeq1left,_))::rest671) => let val result=
MlyValue.DatBind(fn _ => let val TyVarSeq as TyVarSeq1=TyVarSeq1 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val ConBind as ConBind1=ConBind1 ()
val AndDatBind_opt as AndDatBind_opt1=AndDatBind_opt1 ()
 in (
 DATBIND (PP TyVarSeqleft
				     (rightmost get_info_conbind ConBind
				                get_info_datbind AndDatBind_opt),
				   TyVarSeq, mk_TyCon TypeIdent,
				   ConBind, AndDatBind_opt) 
) end
)
 in (LrTable.NT 57,(result,TyVarSeq1left,AndDatBind_opt1right),rest671
) end
| (196,(_,(MlyValue.AndDatBind_opt AndDatBind_opt1,_,
AndDatBind_opt1right))::(_,(MlyValue.ConBind ConBind1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,TypeIdentleft as TypeIdent1left,_))::
rest671) => let val result=MlyValue.DatBind_zero_arity(fn _ => let 
val TypeIdent as TypeIdent1=TypeIdent1 ()
val ConBind as ConBind1=ConBind1 ()
val AndDatBind_opt as AndDatBind_opt1=AndDatBind_opt1 ()
 in (
 DATBIND (PP TypeIdentleft
				     (rightmost get_info_conbind ConBind
				                get_info_datbind AndDatBind_opt),
				   [], mk_TyCon TypeIdent,
				   ConBind, AndDatBind_opt) 
) end
)
 in (LrTable.NT 58,(result,TypeIdent1left,AndDatBind_opt1right),
rest671) end
| (197,(_,(MlyValue.AndDatBind_opt AndDatBind_opt1,_,
AndDatBind_opt1right))::(_,(MlyValue.ConBind ConBind1,_,_))::_::(_,(
MlyValue.TypeIdent TypeIdent1,_,_))::(_,(MlyValue.TyVarSeq1 TyVarSeq11
,TyVarSeq1left as TyVarSeq11left,_))::rest671) => let val result=
MlyValue.DatBind_nonzero_arity(fn _ => let val TyVarSeq1 as TyVarSeq11
=TyVarSeq11 ()
val TypeIdent as TypeIdent1=TypeIdent1 ()
val ConBind as ConBind1=ConBind1 ()
val AndDatBind_opt as AndDatBind_opt1=AndDatBind_opt1 ()
 in (
 DATBIND (PP TyVarSeq1left
				     (rightmost get_info_conbind ConBind
				                get_info_datbind AndDatBind_opt),
				   TyVarSeq1, mk_TyCon TypeIdent,
				   ConBind, AndDatBind_opt) 
) end
)
 in (LrTable.NT 59,(result,TyVarSeq11left,AndDatBind_opt1right),
rest671) end
| (198,(_,(MlyValue.DatBind DatBind1,_,DatBind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AndDatBind_opt(fn _ => let 
val DatBind as DatBind1=DatBind1 ()
 in ( SOME DatBind ) end
)
 in (LrTable.NT 77,(result,AND1left,DatBind1right),rest671) end
| (199,rest671) => let val result=MlyValue.AndDatBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 77,(result,defaultPos,defaultPos),rest671) end
| (200,(_,(MlyValue.BarConBind_opt BarConBind_opt1,_,
BarConBind_opt1right))::(_,(MlyValue.OfTy_opt OfTy_opt1,_,_))::(_,(
MlyValue.OpIdent OpIdent1,OpIdentleft as OpIdent1left,OpIdentright))::
rest671) => let val result=MlyValue.ConBind(fn _ => let val OpIdent
 as OpIdent1=OpIdent1 ()
val OfTy_opt as OfTy_opt1=OfTy_opt1 ()
val BarConBind_opt as BarConBind_opt1=BarConBind_opt1 ()
 in (
 let val OP_OPT (id, withOp) = OpIdent
                          in
                            CONBIND (PP OpIdentleft
				       (rightmost_of_three OpIdentright
					  get_info_ty OfTy_opt
					  get_info_conbind BarConBind_opt),
				     OP_OPT (mk_Id id, withOp),
				     OfTy_opt, BarConBind_opt)
                          end 
) end
)
 in (LrTable.NT 56,(result,OpIdent1left,BarConBind_opt1right),rest671)
 end
| (201,(_,(MlyValue.ConBind ConBind1,_,ConBind1right))::(_,(_,BAR1left
,_))::rest671) => let val result=MlyValue.BarConBind_opt(fn _ => let 
val ConBind as ConBind1=ConBind1 ()
 in ( SOME ConBind ) end
)
 in (LrTable.NT 78,(result,BAR1left,ConBind1right),rest671) end
| (202,rest671) => let val result=MlyValue.BarConBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 78,(result,defaultPos,defaultPos),rest671) end
| (203,(_,(MlyValue.AndExBind_opt AndExBind_opt1,_,AndExBind_opt1right
))::(_,(MlyValue.OfTy_opt OfTy_opt1,_,_))::(_,(MlyValue.OpIdent 
OpIdent1,OpIdentleft as OpIdent1left,OpIdentright))::rest671) => let 
val result=MlyValue.ExBind(fn _ => let val OpIdent as OpIdent1=
OpIdent1 ()
val OfTy_opt as OfTy_opt1=OfTy_opt1 ()
val AndExBind_opt as AndExBind_opt1=AndExBind_opt1 ()
 in (
 let val OP_OPT (id, withOp) = OpIdent
                          in
                            EXBIND (PP OpIdentleft
				      (rightmost_of_three OpIdentright
					  get_info_ty OfTy_opt
					  get_info_exbind AndExBind_opt),
				    OP_OPT (mk_Id id, withOp),
				    OfTy_opt, AndExBind_opt)
                          end 
) end
)
 in (LrTable.NT 55,(result,OpIdent1left,AndExBind_opt1right),rest671)
 end
| (204,(_,(MlyValue.AndExBind_opt AndExBind_opt1,_,AndExBind_opt1right
))::(_,(MlyValue.LongOpEqIdent LongOpEqIdent1,_,LongOpEqIdentright))::
_::(_,(MlyValue.OpIdent OpIdent1,OpIdentleft as OpIdent1left,_))::
rest671) => let val result=MlyValue.ExBind(fn _ => let val OpIdent as 
OpIdent1=OpIdent1 ()
val LongOpEqIdent as LongOpEqIdent1=LongOpEqIdent1 ()
val AndExBind_opt as AndExBind_opt1=AndExBind_opt1 ()
 in (
 let val OP_OPT (id1, withOp1) = OpIdent
			      val OP_OPT (id2, withOp2) = LongOpEqIdent
                          in
                            EXEQUAL (PP OpIdentleft
				       (rightmost' LongOpEqIdentright
					  get_info_exbind AndExBind_opt), 
				     OP_OPT (mk_Id id1, withOp1),
				     OP_OPT (mk_LongId id2, withOp2),
				     AndExBind_opt)
                          end 
) end
)
 in (LrTable.NT 55,(result,OpIdent1left,AndExBind_opt1right),rest671)
 end
| (205,(_,(MlyValue.ExBind ExBind1,_,ExBind1right))::(_,(_,AND1left,_)
)::rest671) => let val result=MlyValue.AndExBind_opt(fn _ => let val 
ExBind as ExBind1=ExBind1 ()
 in ( SOME ExBind ) end
)
 in (LrTable.NT 80,(result,AND1left,ExBind1right),rest671) end
| (206,rest671) => let val result=MlyValue.AndExBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 80,(result,defaultPos,defaultPos),rest671) end
| (207,(_,(MlyValue.AndFValBind_opt AndFValBind_opt1,_,
AndFValBind_opt1right))::(_,(MlyValue.FClause FClause1,FClauseleft as 
FClause1left,_))::rest671) => let val result=MlyValue.FValBind(fn _
 => let val FClause as FClause1=FClause1 ()
val AndFValBind_opt as AndFValBind_opt1=AndFValBind_opt1 ()
 in (
 FVALBIND (PP FClauseleft
				      (rightmost get_info_FClause FClause
				                 get_info_FValBind AndFValBind_opt),
				    FClause, AndFValBind_opt) 
) end
)
 in (LrTable.NT 63,(result,FClause1left,AndFValBind_opt1right),rest671
) end
| (208,(_,(MlyValue.FValBind FValBind1,_,FValBind1right))::(_,(_,
AND1left,_))::rest671) => let val result=MlyValue.AndFValBind_opt(fn _
 => let val FValBind as FValBind1=FValBind1 ()
 in ( SOME FValBind ) end
)
 in (LrTable.NT 74,(result,AND1left,FValBind1right),rest671) end
| (209,rest671) => let val result=MlyValue.AndFValBind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 74,(result,defaultPos,defaultPos),rest671) end
| (210,(_,(MlyValue.BarFClause_opt BarFClause_opt1,_,
BarFClause_opt1right))::(_,(MlyValue.Exp_ Exp_1,_,_))::_::(_,(
MlyValue.ColonTy_opt ColonTy_opt1,_,_))::(_,(MlyValue.AtPat_seq1 
AtPat_seq11,AtPat_seq1left as AtPat_seq11left,_))::rest671) => let 
val result=MlyValue.FClause(fn _ => let val AtPat_seq1 as AtPat_seq11=
AtPat_seq11 ()
val ColonTy_opt as ColonTy_opt1=ColonTy_opt1 ()
val Exp_ as Exp_1=Exp_1 ()
val BarFClause_opt as BarFClause_opt1=BarFClause_opt1 ()
 in (
 FCLAUSE (PP AtPat_seq1left
				     (rightmost get_info_exp Exp_
				                get_info_FClause BarFClause_opt),
				   AtPat_seq1, ColonTy_opt,
				   Exp_, BarFClause_opt) 
) end
)
 in (LrTable.NT 70,(result,AtPat_seq11left,BarFClause_opt1right),
rest671) end
| (211,(_,(MlyValue.FClause FClause1,_,FClause1right))::(_,(_,BAR1left
,_))::rest671) => let val result=MlyValue.BarFClause_opt(fn _ => let 
val FClause as FClause1=FClause1 ()
 in ( SOME FClause ) end
)
 in (LrTable.NT 75,(result,BAR1left,FClause1right),rest671) end
| (212,rest671) => let val result=MlyValue.BarFClause_opt(fn _ => (
 NONE ))
 in (LrTable.NT 75,(result,defaultPos,defaultPos),rest671) end
| (213,(_,(MlyValue.AtPat_seq1 AtPat_seq11,_,AtPat_seq11right))::(_,(
MlyValue.AtPat AtPat1,AtPat1left,_))::rest671) => let val result=
MlyValue.AtPat_seq2(fn _ => let val AtPat as AtPat1=AtPat1 ()
val AtPat_seq1 as AtPat_seq11=AtPat_seq11 ()
 in ( AtPat :: AtPat_seq1 ) end
)
 in (LrTable.NT 97,(result,AtPat1left,AtPat_seq11right),rest671) end
| (214,(_,(MlyValue.AtPat_seq1 AtPat_seq11,_,AtPat_seq11right))::(_,(
MlyValue.AtPat AtPat1,AtPat1left,_))::rest671) => let val result=
MlyValue.AtPat_seq1(fn _ => let val AtPat as AtPat1=AtPat1 ()
val AtPat_seq1 as AtPat_seq11=AtPat_seq11 ()
 in ( AtPat :: AtPat_seq1 ) end
)
 in (LrTable.NT 98,(result,AtPat1left,AtPat_seq11right),rest671) end
| (215,(_,(MlyValue.AtPat AtPat1,AtPat1left,AtPat1right))::rest671)
 => let val result=MlyValue.AtPat_seq1(fn _ => let val AtPat as AtPat1
=AtPat1 ()
 in ( [AtPat] ) end
)
 in (LrTable.NT 98,(result,AtPat1left,AtPat1right),rest671) end
| (216,(_,(MlyValue.Ty Ty1,_,Ty1right))::(_,(_,COLON1left,_))::rest671
) => let val result=MlyValue.ColonTy_opt(fn _ => let val Ty as Ty1=Ty1
 ()
 in ( SOME Ty ) end
)
 in (LrTable.NT 85,(result,COLON1left,Ty1right),rest671) end
| (217,rest671) => let val result=MlyValue.ColonTy_opt(fn _ => ( NONE 
))
 in (LrTable.NT 85,(result,defaultPos,defaultPos),rest671) end
| (218,(_,(MlyValue.Ty Ty1,_,Ty1right))::(_,(_,OF1left,_))::rest671)
 => let val result=MlyValue.OfTy_opt(fn _ => let val Ty as Ty1=Ty1 ()
 in ( SOME Ty ) end
)
 in (LrTable.NT 89,(result,OF1left,Ty1right),rest671) end
| (219,rest671) => let val result=MlyValue.OfTy_opt(fn _ => ( NONE ))
 in (LrTable.NT 89,(result,defaultPos,defaultPos),rest671) end
| (220,(_,(_,UNDERBARleft as UNDERBAR1left,UNDERBARright as 
UNDERBAR1right))::rest671) => let val result=MlyValue.AtPat(fn _ => (
 WILDCARDatpat (PP UNDERBARleft UNDERBARright) ))
 in (LrTable.NT 54,(result,UNDERBAR1left,UNDERBAR1right),rest671) end
| (221,(_,(MlyValue.SCon SCon1,SConleft as SCon1left,SConright as 
SCon1right))::rest671) => let val result=MlyValue.AtPat(fn _ => let 
val SCon as SCon1=SCon1 ()
 in ( SCONatpat (PP SConleft SConright, SCon) ) end
)
 in (LrTable.NT 54,(result,SCon1left,SCon1right),rest671) end
| (222,(_,(MlyValue.LongOpIdent LongOpIdent1,LongOpIdentleft as 
LongOpIdent1left,LongOpIdentright as LongOpIdent1right))::rest671) => 
let val result=MlyValue.AtPat(fn _ => let val LongOpIdent as 
LongOpIdent1=LongOpIdent1 ()
 in (
 let val OP_OPT (id, withOp) = LongOpIdent
                          in
                            LONGIDatpat (PP LongOpIdentleft LongOpIdentright,
					 OP_OPT (mk_LongId id, withOp))
                          end 
) end
)
 in (LrTable.NT 54,(result,LongOpIdent1left,LongOpIdent1right),rest671
) end
| (223,(_,(_,_,EQUALSright as EQUALS1right))::(_,(_,OPleft as OP1left,
_))::rest671) => let val result=MlyValue.AtPat(fn _ => (
 LONGIDatpat (PP OPleft EQUALSright,
				       OP_OPT (mk_LongId ["="], true)) 
))
 in (LrTable.NT 54,(result,OP1left,EQUALS1right),rest671) end
| (224,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.PatRow_opt 
PatRow_opt1,_,_))::(_,(_,LBRACEleft as LBRACE1left,_))::rest671) => 
let val result=MlyValue.AtPat(fn _ => let val PatRow_opt as 
PatRow_opt1=PatRow_opt1 ()
 in ( RECORDatpat (PP LBRACEleft RBRACEright, PatRow_opt) ) end
)
 in (LrTable.NT 54,(result,LBRACE1left,RBRACE1right),rest671) end
| (225,(_,(_,_,RPARENright as RPAREN1right))::(_,(MlyValue.Pat Pat1,_,
_))::(_,(_,LPARENleft as LPAREN1left,_))::rest671) => let val result=
MlyValue.AtPat(fn _ => let val Pat as Pat1=Pat1 ()
 in ( PARatpat (PP LPARENleft RPARENright, Pat) ) end
)
 in (LrTable.NT 54,(result,LPAREN1left,RPAREN1right),rest671) end
| (226,(_,(_,_,RPARENright as RPAREN1right))::(_,(_,LPARENleft as 
LPAREN1left,_))::rest671) => let val result=MlyValue.AtPat(fn _ => (
 RECORDatpat (PP LPARENleft RPARENright, NONE) ))
 in (LrTable.NT 54,(result,LPAREN1left,RPAREN1right),rest671) end
| (227,(_,(_,_,RPARENright as RPAREN1right))::(_,(
MlyValue.PatComma_seq2 PatComma_seq21,_,_))::(_,(_,LPARENleft as 
LPAREN1left,_))::rest671) => let val result=MlyValue.AtPat(fn _ => 
let val PatComma_seq2 as PatComma_seq21=PatComma_seq21 ()
 in ( tuple_atpat_with_info (PP LPARENleft RPARENright) PatComma_seq2 
) end
)
 in (LrTable.NT 54,(result,LPAREN1left,RPAREN1right),rest671) end
| (228,(_,(_,_,RBRACKETright as RBRACKET1right))::(_,(
MlyValue.PatComma_seq0 PatComma_seq01,_,_))::(_,(_,LBRACKETleft as 
LBRACKET1left,_))::rest671) => let val result=MlyValue.AtPat(fn _ => 
let val PatComma_seq0 as PatComma_seq01=PatComma_seq01 ()
 in ( list_atpat (PP LBRACKETleft RBRACKETright) PatComma_seq0 ) end
)
 in (LrTable.NT 54,(result,LBRACKET1left,RBRACKET1right),rest671) end
| (229,(_,(MlyValue.PatRow PatRow1,PatRow1left,PatRow1right))::rest671
) => let val result=MlyValue.PatRow_opt(fn _ => let val PatRow as 
PatRow1=PatRow1 ()
 in ( SOME PatRow ) end
)
 in (LrTable.NT 82,(result,PatRow1left,PatRow1right),rest671) end
| (230,rest671) => let val result=MlyValue.PatRow_opt(fn _ => ( NONE )
)
 in (LrTable.NT 82,(result,defaultPos,defaultPos),rest671) end
| (231,(_,(_,DOTDOTDOTleft as DOTDOTDOT1left,DOTDOTDOTright as 
DOTDOTDOT1right))::rest671) => let val result=MlyValue.PatRow(fn _ => 
( DecGrammar.DOTDOTDOT (PP DOTDOTDOTleft DOTDOTDOTright) ))
 in (LrTable.NT 53,(result,DOTDOTDOT1left,DOTDOTDOT1right),rest671)
 end
| (232,(_,(MlyValue.CommaPatRow_opt CommaPatRow_opt1,_,
CommaPatRow_opt1right))::(_,(MlyValue.Pat Pat1,_,_))::_::(_,(
MlyValue.Label Label1,Labelleft as Label1left,_))::rest671) => let 
val result=MlyValue.PatRow(fn _ => let val Label as Label1=Label1 ()
val Pat as Pat1=Pat1 ()
val CommaPatRow_opt as CommaPatRow_opt1=CommaPatRow_opt1 ()
 in (
 PATROW (PP Labelleft
				    (rightmost get_info_pat Pat
				               get_info_patrow CommaPatRow_opt),
				  Label, Pat, CommaPatRow_opt) 
) end
)
 in (LrTable.NT 53,(result,Label1left,CommaPatRow_opt1right),rest671)
 end
| (233,(_,(MlyValue.CommaPatRow_opt CommaPatRow_opt1,_,
CommaPatRow_opt1right))::(_,(MlyValue.AsPat_opt AsPat_opt1,_,_))::(_,(
MlyValue.ColonTy_opt ColonTy_opt1,_,_))::(_,(MlyValue.Ident Ident1,
Identleft as Ident1left,Identright))::rest671) => let val result=
MlyValue.PatRow(fn _ => let val Ident as Ident1=Ident1 ()
val ColonTy_opt as ColonTy_opt1=ColonTy_opt1 ()
val AsPat_opt as AsPat_opt1=AsPat_opt1 ()
val CommaPatRow_opt as CommaPatRow_opt1=CommaPatRow_opt1 ()
 in (
 PATROW
			    (PP Identleft
			       (rightmost_of_four Identright
				  get_info_ty ColonTy_opt
				  get_info_pat AsPat_opt
				  get_info_patrow CommaPatRow_opt),
			     mk_IdentLab Ident,
			     let
			       val info_ident = PP Identleft Identright
			       val idPat =
				     ATPATpat
				       (info_ident,
					LONGIDatpat
					  (info_ident, OP_OPT (mk_LongId [Ident], false)))
			     in
			       case (ColonTy_opt, AsPat_opt) of
				 (_, SOME pat) =>
				   LAYEREDpat (PP Identleft (right (get_info_pat pat)),
					       OP_OPT (mk_Id Ident, false),
					       ColonTy_opt, pat)
			       | (SOME ty, NONE) =>
				   TYPEDpat (PP Identleft (right (get_info_ty ty)),
					     idPat, ty)
			       | (NONE, NONE) => idPat
			     end,
			     CommaPatRow_opt) 
) end
)
 in (LrTable.NT 53,(result,Ident1left,CommaPatRow_opt1right),rest671)
 end
| (234,(_,(MlyValue.Pat Pat1,_,Pat1right))::(_,(_,AS1left,_))::rest671
) => let val result=MlyValue.AsPat_opt(fn _ => let val Pat as Pat1=
Pat1 ()
 in ( SOME Pat ) end
)
 in (LrTable.NT 83,(result,AS1left,Pat1right),rest671) end
| (235,rest671) => let val result=MlyValue.AsPat_opt(fn _ => ( NONE ))
 in (LrTable.NT 83,(result,defaultPos,defaultPos),rest671) end
| (236,(_,(MlyValue.PatRow PatRow1,_,PatRow1right))::(_,(_,COMMA1left,
_))::rest671) => let val result=MlyValue.CommaPatRow_opt(fn _ => let 
val PatRow as PatRow1=PatRow1 ()
 in ( SOME PatRow ) end
)
 in (LrTable.NT 84,(result,COMMA1left,PatRow1right),rest671) end
| (237,rest671) => let val result=MlyValue.CommaPatRow_opt(fn _ => (
 NONE ))
 in (LrTable.NT 84,(result,defaultPos,defaultPos),rest671) end
| (238,(_,(MlyValue.PatComma_seq1 PatComma_seq11,PatComma_seq11left,
PatComma_seq11right))::rest671) => let val result=
MlyValue.PatComma_seq0(fn _ => let val PatComma_seq1 as PatComma_seq11
=PatComma_seq11 ()
 in ( PatComma_seq1 ) end
)
 in (LrTable.NT 93,(result,PatComma_seq11left,PatComma_seq11right),
rest671) end
| (239,rest671) => let val result=MlyValue.PatComma_seq0(fn _ => (
 nil ))
 in (LrTable.NT 93,(result,defaultPos,defaultPos),rest671) end
| (240,(_,(MlyValue.PatComma_seq1 PatComma_seq11,_,PatComma_seq11right
))::_::(_,(MlyValue.Pat Pat1,Pat1left,_))::rest671) => let val result=
MlyValue.PatComma_seq1(fn _ => let val Pat as Pat1=Pat1 ()
val PatComma_seq1 as PatComma_seq11=PatComma_seq11 ()
 in ( Pat :: PatComma_seq1 ) end
)
 in (LrTable.NT 94,(result,Pat1left,PatComma_seq11right),rest671) end
| (241,(_,(MlyValue.Pat Pat1,Pat1left,Pat1right))::rest671) => let 
val result=MlyValue.PatComma_seq1(fn _ => let val Pat as Pat1=Pat1 ()
 in ( [Pat] ) end
)
 in (LrTable.NT 94,(result,Pat1left,Pat1right),rest671) end
| (242,(_,(MlyValue.PatComma_seq1 PatComma_seq11,_,PatComma_seq11right
))::_::(_,(MlyValue.Pat Pat1,Pat1left,_))::rest671) => let val result=
MlyValue.PatComma_seq2(fn _ => let val Pat as Pat1=Pat1 ()
val PatComma_seq1 as PatComma_seq11=PatComma_seq11 ()
 in ( Pat :: PatComma_seq1 ) end
)
 in (LrTable.NT 95,(result,Pat1left,PatComma_seq11right),rest671) end
| (243,(_,(MlyValue.AtPat AtPat1,AtPat1left,AtPat1right))::rest671)
 => let val result=MlyValue.Pat(fn _ => let val AtPat as AtPat1=AtPat1
 ()
 in ( ATPATpat (get_info_atpat AtPat, AtPat) ) end
)
 in (LrTable.NT 52,(result,AtPat1left,AtPat1right),rest671) end
| (244,(_,(MlyValue.AtPat_seq2 AtPat_seq21,AtPat_seq2left as 
AtPat_seq21left,AtPat_seq2right as AtPat_seq21right))::rest671) => 
let val result=MlyValue.Pat(fn _ => let val AtPat_seq2 as AtPat_seq21=
AtPat_seq21 ()
 in (
 UNRES_INFIXpat (PP AtPat_seq2left AtPat_seq2right,
					  AtPat_seq2) 
) end
)
 in (LrTable.NT 52,(result,AtPat_seq21left,AtPat_seq21right),rest671)
 end
| (245,(_,(MlyValue.Ty Ty1,_,Tyright as Ty1right))::_::(_,(
MlyValue.Pat Pat1,Patleft as Pat1left,_))::rest671) => let val result=
MlyValue.Pat(fn _ => let val Pat as Pat1=Pat1 ()
val Ty as Ty1=Ty1 ()
 in ( TYPEDpat (PP Patleft Tyright, Pat, Ty) ) end
)
 in (LrTable.NT 52,(result,Pat1left,Ty1right),rest671) end
| (246,(_,(MlyValue.Pat Pat2,_,Pat2right))::_::(_,(MlyValue.Pat Pat1,
Pat1left,_))::rest671) => let val result=MlyValue.Pat(fn _ => let val 
Pat1=Pat1 ()
val Pat2=Pat2 ()
 in ( layeredPat (PP Pat1left Pat2right, Pat1, Pat2) ) end
)
 in (LrTable.NT 52,(result,Pat1left,Pat2right),rest671) end
| (247,(_,(MlyValue.Ty Ty1,_,Tyright as Ty1right))::_::(_,(
MlyValue.TupleTy TupleTy1,TupleTyleft as TupleTy1left,_))::rest671)
 => let val result=MlyValue.Ty(fn _ => let val TupleTy as TupleTy1=
TupleTy1 ()
val Ty as Ty1=Ty1 ()
 in (
 FNty (PP TupleTyleft Tyright,
				(case TupleTy of
				   [t] => t
				 | ts => tuple_type (PP TupleTyleft Tyright) ts),
				Ty) 
) end
)
 in (LrTable.NT 46,(result,TupleTy1left,Ty1right),rest671) end
| (248,(_,(MlyValue.TupleTy TupleTy1,TupleTyleft as TupleTy1left,
TupleTyright as TupleTy1right))::rest671) => let val result=
MlyValue.Ty(fn _ => let val TupleTy as TupleTy1=TupleTy1 ()
 in (
 (case TupleTy of
			     [t] => t
			   | ts => tuple_type (PP TupleTyleft TupleTyright) ts) 
) end
)
 in (LrTable.NT 46,(result,TupleTy1left,TupleTy1right),rest671) end
| (249,(_,(MlyValue.Ty_sans_STAR Ty_sans_STAR1,Ty_sans_STAR1left,
Ty_sans_STAR1right))::rest671) => let val result=MlyValue.TupleTy(fn _
 => let val Ty_sans_STAR as Ty_sans_STAR1=Ty_sans_STAR1 ()
 in ( [Ty_sans_STAR] ) end
)
 in (LrTable.NT 47,(result,Ty_sans_STAR1left,Ty_sans_STAR1right),
rest671) end
| (250,(_,(MlyValue.TupleTy TupleTy1,_,TupleTy1right))::_::(_,(
MlyValue.Ty_sans_STAR Ty_sans_STAR1,Ty_sans_STAR1left,_))::rest671)
 => let val result=MlyValue.TupleTy(fn _ => let val Ty_sans_STAR as 
Ty_sans_STAR1=Ty_sans_STAR1 ()
val TupleTy as TupleTy1=TupleTy1 ()
 in ( Ty_sans_STAR :: TupleTy ) end
)
 in (LrTable.NT 47,(result,Ty_sans_STAR1left,TupleTy1right),rest671)
 end
| (251,(_,(MlyValue.LongTypeIdent LongTypeIdent1,_,LongTypeIdentright
 as LongTypeIdent1right))::_::(_,(MlyValue.TyComma_seq2 TyComma_seq21,
_,_))::(_,(_,LPARENleft as LPAREN1left,_))::rest671) => let val result
=MlyValue.Ty_sans_STAR(fn _ => let val TyComma_seq2 as TyComma_seq21=
TyComma_seq21 ()
val LongTypeIdent as LongTypeIdent1=LongTypeIdent1 ()
 in (
 CONty (PP LPARENleft LongTypeIdentright,
				 TyComma_seq2, mk_LongTyCon LongTypeIdent) 
) end
)
 in (LrTable.NT 48,(result,LPAREN1left,LongTypeIdent1right),rest671)
 end
| (252,(_,(MlyValue.LongTypeIdent LongTypeIdent1,_,LongTypeIdentright
 as LongTypeIdent1right))::(_,(MlyValue.Ty_sans_STAR Ty_sans_STAR1,
Ty_sans_STARleft as Ty_sans_STAR1left,_))::rest671) => let val result=
MlyValue.Ty_sans_STAR(fn _ => let val Ty_sans_STAR as Ty_sans_STAR1=
Ty_sans_STAR1 ()
val LongTypeIdent as LongTypeIdent1=LongTypeIdent1 ()
 in (
 CONty (PP Ty_sans_STARleft LongTypeIdentright,
				 [Ty_sans_STAR], mk_LongTyCon LongTypeIdent) 
) end
)
 in (LrTable.NT 48,(result,Ty_sans_STAR1left,LongTypeIdent1right),
rest671) end
| (253,(_,(MlyValue.AtomicTy AtomicTy1,AtomicTy1left,AtomicTy1right))
::rest671) => let val result=MlyValue.Ty_sans_STAR(fn _ => let val 
AtomicTy as AtomicTy1=AtomicTy1 ()
 in ( AtomicTy ) end
)
 in (LrTable.NT 48,(result,AtomicTy1left,AtomicTy1right),rest671) end
| (254,(_,(MlyValue.TyComma_seq2 TyComma_seq21,_,TyComma_seq21right))
::_::(_,(MlyValue.Ty Ty1,Ty1left,_))::rest671) => let val result=
MlyValue.TyComma_seq2(fn _ => let val Ty as Ty1=Ty1 ()
val TyComma_seq2 as TyComma_seq21=TyComma_seq21 ()
 in ( Ty :: TyComma_seq2 ) end
)
 in (LrTable.NT 102,(result,Ty1left,TyComma_seq21right),rest671) end
| (255,(_,(MlyValue.Ty Ty2,_,Ty2right))::_::(_,(MlyValue.Ty Ty1,
Ty1left,_))::rest671) => let val result=MlyValue.TyComma_seq2(fn _ => 
let val Ty1=Ty1 ()
val Ty2=Ty2 ()
 in ( [Ty1, Ty2] ) end
)
 in (LrTable.NT 102,(result,Ty1left,Ty2right),rest671) end
| (256,(_,(MlyValue.LongTypeIdent LongTypeIdent1,LongTypeIdentleft as 
LongTypeIdent1left,LongTypeIdentright as LongTypeIdent1right))::
rest671) => let val result=MlyValue.AtomicTy(fn _ => let val 
LongTypeIdent as LongTypeIdent1=LongTypeIdent1 ()
 in (
 CONty (PP LongTypeIdentleft LongTypeIdentright,
				 [], mk_LongTyCon LongTypeIdent) 
) end
)
 in (LrTable.NT 49,(result,LongTypeIdent1left,LongTypeIdent1right),
rest671) end
| (257,(_,(MlyValue.TYVAR TYVAR1,TYVARleft as TYVAR1left,TYVARright
 as TYVAR1right))::rest671) => let val result=MlyValue.AtomicTy(fn _
 => let val TYVAR as TYVAR1=TYVAR1 ()
 in ( TYVARty(PP TYVARleft TYVARright, mk_TyVar TYVAR) ) end
)
 in (LrTable.NT 49,(result,TYVAR1left,TYVAR1right),rest671) end
| (258,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.TyRow_opt 
TyRow_opt1,_,_))::(_,(_,LBRACEleft as LBRACE1left,_))::rest671) => 
let val result=MlyValue.AtomicTy(fn _ => let val TyRow_opt as 
TyRow_opt1=TyRow_opt1 ()
 in ( RECORDty (PP LBRACEleft RBRACEright, TyRow_opt) ) end
)
 in (LrTable.NT 49,(result,LBRACE1left,RBRACE1right),rest671) end
| (259,(_,(_,_,RPARENright as RPAREN1right))::(_,(MlyValue.Ty Ty1,_,_)
)::(_,(_,LPARENleft as LPAREN1left,_))::rest671) => let val result=
MlyValue.AtomicTy(fn _ => let val Ty as Ty1=Ty1 ()
 in ( PARty (PP LPARENleft RPARENright, Ty) ) end
)
 in (LrTable.NT 49,(result,LPAREN1left,RPAREN1right),rest671) end
| (260,(_,(MlyValue.TyRow TyRow1,TyRow1left,TyRow1right))::rest671)
 => let val result=MlyValue.TyRow_opt(fn _ => let val TyRow as TyRow1=
TyRow1 ()
 in ( SOME TyRow ) end
)
 in (LrTable.NT 87,(result,TyRow1left,TyRow1right),rest671) end
| (261,rest671) => let val result=MlyValue.TyRow_opt(fn _ => ( NONE ))
 in (LrTable.NT 87,(result,defaultPos,defaultPos),rest671) end
| (262,(_,(MlyValue.CommaTyRow_opt CommaTyRow_opt1,_,
CommaTyRow_opt1right))::(_,(MlyValue.Ty Ty1,_,_))::_::(_,(
MlyValue.Label Label1,Labelleft as Label1left,_))::rest671) => let 
val result=MlyValue.TyRow(fn _ => let val Label as Label1=Label1 ()
val Ty as Ty1=Ty1 ()
val CommaTyRow_opt as CommaTyRow_opt1=CommaTyRow_opt1 ()
 in (
 TYROW (PP Labelleft
				   (rightmost get_info_ty Ty
				              get_info_tyrow CommaTyRow_opt),
				 Label, Ty, CommaTyRow_opt) 
) end
)
 in (LrTable.NT 45,(result,Label1left,CommaTyRow_opt1right),rest671)
 end
| (263,(_,(MlyValue.TyRow TyRow1,_,TyRow1right))::(_,(_,COMMA1left,_))
::rest671) => let val result=MlyValue.CommaTyRow_opt(fn _ => let val 
TyRow as TyRow1=TyRow1 ()
 in ( SOME TyRow ) end
)
 in (LrTable.NT 88,(result,COMMA1left,TyRow1right),rest671) end
| (264,rest671) => let val result=MlyValue.CommaTyRow_opt(fn _ => (
 NONE ))
 in (LrTable.NT 88,(result,defaultPos,defaultPos),rest671) end
| (265,(_,(MlyValue.DECPOSINTEGER DECPOSINTEGER1,DECPOSINTEGERleft as 
DECPOSINTEGER1left,DECPOSINTEGER1right))::rest671) => let val result=
MlyValue.Integer(fn _ => let val DECPOSINTEGER as DECPOSINTEGER1=
DECPOSINTEGER1 ()
 in (
 raise_lexical_error_if_none
			    DECPOSINTEGERleft DECPOSINTEGER )
 end
)
 in (LrTable.NT 123,(result,DECPOSINTEGER1left,DECPOSINTEGER1right),
rest671) end
| (266,(_,(MlyValue.DECNEGINTEGER DECNEGINTEGER1,DECNEGINTEGERleft as 
DECNEGINTEGER1left,DECNEGINTEGER1right))::rest671) => let val result=
MlyValue.Integer(fn _ => let val DECNEGINTEGER as DECNEGINTEGER1=
DECNEGINTEGER1 ()
 in (
 raise_lexical_error_if_none
			    DECNEGINTEGERleft DECNEGINTEGER )
 end
)
 in (LrTable.NT 123,(result,DECNEGINTEGER1left,DECNEGINTEGER1right),
rest671) end
| (267,(_,(MlyValue.HEXINTEGER HEXINTEGER1,HEXINTEGERleft as 
HEXINTEGER1left,HEXINTEGER1right))::rest671) => let val result=
MlyValue.Integer(fn _ => let val HEXINTEGER as HEXINTEGER1=HEXINTEGER1
 ()
 in ( raise_lexical_error_if_none
			    HEXINTEGERleft HEXINTEGER )
 end
)
 in (LrTable.NT 123,(result,HEXINTEGER1left,HEXINTEGER1right),rest671)
 end
| (268,(_,(MlyValue.DIGIT DIGIT1,DIGIT1left,DIGIT1right))::rest671)
 => let val result=MlyValue.Integer(fn _ => let val DIGIT as DIGIT1=
DIGIT1 ()
 in ( DIGIT ) end
)
 in (LrTable.NT 123,(result,DIGIT1left,DIGIT1right),rest671) end
| (269,(_,(MlyValue.STRING STRING1,STRINGleft,STRING1right))::(_,(_,
HASH1left,_))::rest671) => let val result=MlyValue.Char(fn _ => let 
val STRING as STRING1=STRING1 ()
 in (
 case explode STRING
                            of [c] => ord c
                             | _ => raise LEXICAL_ERROR (STRINGleft, "string must have length 1") 
) end
)
 in (LrTable.NT 124,(result,HASH1left,STRING1right),rest671) end
| (270,(_,(MlyValue.Integer Integer1,Integer1left,Integer1right))::
rest671) => let val result=MlyValue.SCon(fn _ => let val Integer as 
Integer1=Integer1 ()
 in ( mk_IntSCon Integer ) end
)
 in (LrTable.NT 51,(result,Integer1left,Integer1right),rest671) end
| (271,(_,(MlyValue.WORD WORD1,WORDleft as WORD1left,WORD1right))::
rest671) => let val result=MlyValue.SCon(fn _ => let val WORD as WORD1
=WORD1 ()
 in ( mk_WordSCon (raise_lexical_error_if_none WORDleft WORD) ) end
)
 in (LrTable.NT 51,(result,WORD1left,WORD1right),rest671) end
| (272,(_,(MlyValue.STRING STRING1,STRING1left,STRING1right))::rest671
) => let val result=MlyValue.SCon(fn _ => let val STRING as STRING1=
STRING1 ()
 in ( mk_StringSCon STRING ) end
)
 in (LrTable.NT 51,(result,STRING1left,STRING1right),rest671) end
| (273,(_,(MlyValue.Char Char1,Char1left,Char1right))::rest671) => 
let val result=MlyValue.SCon(fn _ => let val Char as Char1=Char1 ()
 in ( mk_CharSCon Char ) end
)
 in (LrTable.NT 51,(result,Char1left,Char1right),rest671) end
| (274,(_,(MlyValue.REAL REAL1,REALleft as REAL1left,REAL1right))::
rest671) => let val result=MlyValue.SCon(fn _ => let val REAL as REAL1
=REAL1 ()
 in ( mk_RealSCon (raise_lexical_error_if_none REALleft REAL) ) end
)
 in (LrTable.NT 51,(result,REAL1left,REAL1right),rest671) end
| (275,(_,(MlyValue.TyVarSeq1 TyVarSeq11,TyVarSeq11left,
TyVarSeq11right))::rest671) => let val result=MlyValue.TyVarSeq(fn _
 => let val TyVarSeq1 as TyVarSeq11=TyVarSeq11 ()
 in ( TyVarSeq1 ) end
)
 in (LrTable.NT 50,(result,TyVarSeq11left,TyVarSeq11right),rest671)
 end
| (276,rest671) => let val result=MlyValue.TyVarSeq(fn _ => ( [] ))
 in (LrTable.NT 50,(result,defaultPos,defaultPos),rest671) end
| (277,(_,(MlyValue.TYVAR TYVAR1,TYVAR1left,TYVAR1right))::rest671)
 => let val result=MlyValue.TyVarSeq1(fn _ => let val TYVAR as TYVAR1=
TYVAR1 ()
 in ( [mk_TyVar TYVAR] ) end
)
 in (LrTable.NT 100,(result,TYVAR1left,TYVAR1right),rest671) end
| (278,(_,(_,_,RPAREN1right))::(_,(MlyValue.TyVarComma_seq1 
TyVarComma_seq11,_,_))::(_,(_,LPAREN1left,_))::rest671) => let val 
result=MlyValue.TyVarSeq1(fn _ => let val TyVarComma_seq1 as 
TyVarComma_seq11=TyVarComma_seq11 ()
 in ( TyVarComma_seq1 ) end
)
 in (LrTable.NT 100,(result,LPAREN1left,RPAREN1right),rest671) end
| (279,(_,(MlyValue.TyVarComma_seq1 TyVarComma_seq11,_,
TyVarComma_seq11right))::_::(_,(MlyValue.TYVAR TYVAR1,TYVAR1left,_))::
rest671) => let val result=MlyValue.TyVarComma_seq1(fn _ => let val 
TYVAR as TYVAR1=TYVAR1 ()
val TyVarComma_seq1 as TyVarComma_seq11=TyVarComma_seq11 ()
 in ( mk_TyVar TYVAR :: TyVarComma_seq1 ) end
)
 in (LrTable.NT 101,(result,TYVAR1left,TyVarComma_seq11right),rest671)
 end
| (280,(_,(MlyValue.TYVAR TYVAR1,TYVAR1left,TYVAR1right))::rest671)
 => let val result=MlyValue.TyVarComma_seq1(fn _ => let val TYVAR as 
TYVAR1=TYVAR1 ()
 in ( [mk_TyVar TYVAR] ) end
)
 in (LrTable.NT 101,(result,TYVAR1left,TYVAR1right),rest671) end
| _ => raise (mlyAction i392)
end
(* cvr: end *)
end
