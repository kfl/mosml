structure PMBasic =
struct
    type filename = string

    datatype body =
	     SRC of filename * body
           | STRSRC of filename * body
           | LOCAL of body * body * body
           | NULL
    datatype pmfile = PM of {imports : filename list,
			     body    : body}   

    fun eq x y = x=y

    local
	open Parsercomb
        infix 6 $-- --$ #-- --#
        infix 5 --
        infix 3 >> >>*
        infix 2 >>=
        infix 0 ||

	fun fileChar c = 
	    Char.isAlphaNum c 
	    orelse c = #"_" orelse c = #"/" orelse c = #"-" orelse c = #"."

	val ident = getChars1 fileChar 
    
				   
	datatype token =
		 LOCAL_T | IN_T | END_T | IMPORT_T | STRUCTURE_T | TOPLEVEL_T
	       | FILENAME_T of filename
	       | EOF_T



	fun toList stream =
	    case getItem stream of
		SOME(EOF_T,  _) => [EOF_T]
	      | SOME(x, stream) => x :: toList stream
	      | NONE            => []
				   

			       
	infix 7 |>
	fun (pf |> res) = pf >> (fn _ => res)
			
	fun keyword "local"     = LOCAL_T
	  | keyword "in"        = IN_T
	  | keyword "end"       = END_T
	  | keyword "import"    = IMPORT_T
	  | keyword "structure" = STRUCTURE_T
	  | keyword "toplevel"  = TOPLEVEL_T
	  | keyword file     = FILENAME_T file
  
	fun token stream =
	    (   ident  >>  keyword
	     || $ "(*" #-- comment 1
	     || eof        EOF_T
            ) (StringCvt.skipWS getItem stream) 

	(* FIXME: write the third clause without recursion *)
	and comment depth stream =
	    (   $"*)" #-- (if depth-1 = 0 then token
			   else comment (depth-1))
             || $"(*" #-- comment (depth+1)
             || getChar (fn _ => true) #-- comment depth
             || eof () >> (fn _ => raise Fail "Comment not closed")
            ) stream

	val lexer = stream token
		    
	fun getIf pred stream = 
	    case getItem stream of
		res as SOME(x, src) => if pred x then res
				       else NONE
	      | _  => NONE
		      
	infix 6 &-- --&
	fun (t &-- pf) = getLit t #-- pf
	fun (pf --& t) = pf --# getLit t
(*			 
	val getFile = getIf (fn (FILENAME_T _) => true | _ => false) >>
			    (fn (FILENAME_T s) => s)   
*)			    
	fun getOpt f = getItem >>* f

	val getFile = getOpt(fn (FILENAME_T s) => SOME s | _ => NONE)

	val imports = repeat0 getFile

	val optTOPLEVEL = optional(getLit TOPLEVEL_T)

	fun body stream = 
	    (   optTOPLEVEL #-- getFile -- body >> SRC
             || STRUCTURE_T &-- getFile -- body >> STRSRC
	     || LOCAL_T &-- body -- IN_T &-- body -- END_T &-- body
	                                >> (fn((b1,b2),b3) => LOCAL(b1,b2,b3))
             || success                                    NULL
            ) stream

	val pm = 
	      IMPORT_T &-- imports -- IN_T &-- body --& END_T 
		       >> (fn (im,b) => PM{imports=im ,body=b})
	  || body      >> (fn b      => PM{imports=[], body=b})

	val pmfile = pm --& EOF_T
    in
    fun parse get src =
	let val stream = stream get src
	    val lexer  = lexer stream
	in  case pmfile lexer of
		SOME(res, _) => SOME(res, src) (*FIXME: src is wrong *)
	      | NONE         => NONE
	end

    fun parseFile filename =
	let open TextIO
	    val dev = openIn filename
	    val res = scanStream parse dev
	in  closeIn dev
          ; Option.valOf res
	end

    fun tokList filename =
	let open TextIO
	    val dev = openIn filename
	    val ss  = Substring.all(inputAll dev) before closeIn dev
	in  toList (lexer(stream Substring.getc ss))
	end

    fun parse get src =
	let val stream = stream get src
	    val lexer  = lexer stream
	in  case pmfile lexer of
		SOME(res, _) => SOME res
	      | NONE         => NONE
	end

    fun parseFile filename =
	let open TextIO
	    val dev = openIn filename
	    val ss  = Substring.all(inputAll dev) before closeIn dev
	    val res = parse Substring.getc ss
	in  Option.valOf res
	end


    end

end
