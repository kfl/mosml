(* cvr: devise comprehensive tests *)

(* simple tests *)
signature S = sig type t type v sharing type v = t end;
structure X = struct type t = int type v = int end :> S;
structure Y as S = [structure struct type t = int type v = int end as S];
structure Z as S = 
	 if true then [structure Y as S] else
	 [structure struct type t = bool type v = bool end as S];
functor F(X:S)=X:>S;
functor G(X:S)=F(F(F(X:S)));
functor H(X:S)=X:S:>S:>S:>S;

(* elaborate tests *)

signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
    end
;
signature TOKEN =
    sig
	structure LrTable : LR_TABLE
    end
;
signature LR_PARSER =
    sig
	structure LrTable : LR_TABLE
	structure Token : TOKEN
	sharing LrTable = Token.LrTable
    end
;
signature PARSER_DATA =
   sig
	structure LrTable : LR_TABLE
	structure Token : TOKEN
	sharing Token.LrTable = LrTable
    end
;
signature PARSER =
    sig
        structure Token : TOKEN
     end
;

functor Join(structure ParserData: PARSER_DATA
	     structure LrParser : LR_PARSER
	     sharing ParserData.LrTable = LrParser.LrTable
	     sharing ParserData.Token = LrParser.Token
	    )
		 : PARSER =
struct
    structure Token = ParserData.Token
end
;
structure LrTable : LR_TABLE = 
    struct

	datatype ('a,'b) pairlist = EMPTY
				  | PAIR of 'a * 'b * ('a,'b) pairlist
end;

structure LrParser :> LR_PARSER =
   struct
      structure LrTable = LrTable
      structure Token : TOKEN =
	struct
	    structure LrTable = LrTable
        end
 end;

signature Fol_LRVALS=
    sig
	structure ParserData:PARSER_DATA
    end
;
functor FolLrValsFun (structure Token : TOKEN
			  ) : Fol_LRVALS = 
    struct
	structure ParserData=
	    struct

		structure LrTable = Token.LrTable
		structure Token = Token
	    end
    end
;
structure FolLrVals : Fol_LRVALS =
   FolLrValsFun(structure Token = LrParser.Token);


structure FolParser : PARSER =
   Join(structure ParserData = FolLrVals.ParserData
	structure LrParser = LrParser)
; 


