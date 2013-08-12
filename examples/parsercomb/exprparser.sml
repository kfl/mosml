(* Expression parser using Parser combinators                              *)
(* By Ken Friis Larsen <ken@friislarsen.net>                                        *)
(* (Re)Created: 20010907                                                   *)
structure ExprParser =
struct

local
    structure C  = Char
    structure PC = Parsercomb
    infix 6 $-- --$ #-- --#
    infix 5 --
    infix 3 >> >>*
    infix 2 >>=
    infix 0 ||

    val (op$--, op--$, op#--, op--#, op--, op>>, op>>*, op>>=, op||) =
        (PC.$--, PC.--$, PC.#--, PC.--#, PC.--, PC.>>, PC.>>*, PC.>>=, PC.||)
    (* Instead of this line-noise I'd prefer to write
       open PC.Symbols
    *)

    infix 3 ##>
    fun e ##> res = PC.getLit e >> (fn _ => res) 

    fun compose(par1, par2) strm = 
        let val par1stream = PC.stream par1 strm
        in par2 par1stream end

    infix 3 >>>
    val op >>> = compose
            
    fun toReader par get src =
        let val input  = PC.stream get src
        in  Option.map (fn(res,_) => (res, src)) (par input) end

    val % = PC.getLit

in

   datatype operator = PLUS | MULT | MINUS | DIV

   (* The datatype of abstact syntax trees for expressions *)
   datatype expr  = VAR of string
                  | INT of int
                  | BIN of expr * operator * expr

   local
       datatype token = VAR_T of string
                      | INT_T of int
                      | PLUS_T | MULT_T | MINUS_T | DIV_T
                      | LPAR_T | RPAR_T
                      | EOF_T

       (* First a simple lexer *)
       local
           val ident = 
               PC.getChars1 C.isAlpha -- PC.getChars0 C.isAlphaNum >> op^
           
           val number = PC.scan (Int.scan StringCvt.DEC) 
       in             
           val token = PC.skipWS
               (  ident    >> VAR_T
               || #"+"    ##> PLUS_T  
               || number   >> INT_T
               || #"*"    ##> MULT_T  
               || #"-"    ##> MINUS_T  
               || #"/"    ##> DIV_T  
               || #"("    ##> LPAR_T  
               || #")"    ##> RPAR_T  
               || PC.eof      EOF_T
               )
       
       val lexer = PC.stream token
       end

       fun toList stream =
	   case PC.getItem stream of
	       SOME(EOF_T,  _) => [EOF_T]
	     | SOME(x, stream) => x :: toList stream
	     | NONE            => []
	    

      (* Then the parser *)

      (* The grammar we want to parse:
         start ::= expr EOF_T  
          
         expr  ::= sum

         sum   ::= sum sumOpr prod
                |  prod

         prod  ::= prod prodOpr term
                |  term

         term  ::= VAR_T
                |  INT_T
                |  LPAR_T expr RPAR_T
      
         sumOpr  ::= PLUS_T | MINUS_T
         prodOrd ::= MULT_T | DIV_T 
    
         But first we need to eliminate left recursion in sum and prod.
         Hence, they are changed to:

         sum   ::= prod sum'

         sum'  ::= sumOpr prod sum'
                |  empty

         prod  ::= term prod'

         prod' ::= prodOpr term prod'
                |  empty
       *)      

       (* helper functions *)
       fun buildBin (x, oprRest) =
           case oprRest of
               SOME(opr, y) => BIN(x, opr, y)
             | NONE         => x

       fun mkBin opr (x, y) = BIN(x, opr, y)

       fun getOpt f = PC.getItem >>* f
                      
       (* simple productions *)
       val getVar = getOpt(fn (VAR_T s) => SOME s | _ => NONE)
       val getInt = getOpt(fn (INT_T i) => SOME i | _ => NONE)

       val sumOpr =  PLUS_T  ##> mkBin PLUS
                  || MINUS_T ##> mkBin MINUS

       val prodOpr =  MULT_T  ##> mkBin MULT
                   || DIV_T   ##> mkBin DIV

       fun leftrecur left recur opr right = 
           (PC.optional((opr -- right >> (fn (opr, right) => opr(left, right)))
                        >>= recur))
            >> (fn NONE => left | SOME e => e)


       (* recursive productions *)
       fun start toks = (expr --# % EOF_T) toks

       and expr toks = sum toks

       and sum toks = (prod >>= sum') toks 
       and sum' e = leftrecur e sum' sumOpr prod
(*           (PC.optional((sumOpr -- prod >> (fn (opr, y) => BIN(e, opr, y))) 
                        >>= sum'))
            >> (fn NONE => e | SOME e => e)
*)
       and prod toks = (term >>= prod') toks
       and prod' e  = leftrecur e prod' prodOpr term 

       and term toks = 
           (  getVar                     >> VAR
           || getInt                     >> INT
           || % LPAR_T #-- expr --# % RPAR_T
           ) toks
   in

   val parse = fn x => toReader(token >>> start) x

   val parseString = StringCvt.scanString parse 

   fun parseFile filename = 
       let val dev = TextIO.openIn filename
       in  #1((TextIO.scanStream parse dev)
              handle ? => (TextIO.closeIn dev; raise ?)
             , TextIO.closeIn dev)
       end
   end 
end
end

(* Test
   val e1 = ExprParser.parseString "x+3   * pi";
   val e2 = ExprParser.parseString "(x +   3 )   * pi  ";
*)
