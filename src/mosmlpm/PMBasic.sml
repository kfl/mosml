structure PMBasic =
struct
    type filename = string

    datatype body =
	     SRC of filename * body
           | LOCAL of body * body * body
           | NULL
    datatype pmfile = PM of {imports : filename list,
			     body    : body}   

    fun eq x y = x=y

    local
	open Parser
	infix 6 $-- --$ #-- --#
	infix 5 --
	infix 3 >>
	infix 0 ||

	fun fileChar c = Char.isAlphaNum c 
			 orelse c = #"_" orelse c = #"/" orelse c = #"-" orelse c = #"."

	val fileName = getChars1 fileChar 
    
				   
	datatype token =
		 LOCAL_T | IN_T | END_T | IMPORT_T 
	       | FILENAME_T of filename
	       | EOF_T



	fun toList stream =
	    case split stream of
		SOME(EOF_T,  _) => [EOF_T]
	      | SOME(x, stream) => x :: toList stream
	      | NONE            => []
				   

			       
	infix 7 |>
	fun (pf |> res) = pf >> (fn _ => res)
			  
	fun token stream =
	    (   $ "local"  |> LOCAL_T 
             || $ "in"     |> IN_T
	     || $ "end"    |> END_T
	     || $ "import" |> IMPORT_T
	     || fileName   >> FILENAME_T
	     || $ "(*"     #-- comment 1
	     || eof           EOF_T
            ) (StringCvt.skipWS split stream) 

	(* FIXME: write the third clause without recursion *)
	and comment depth stream =
	    (   $"*)" #-- (if depth-1 = 0 then token
			   else comment (depth-1))
             || $"(*" #-- comment (depth+1)
             || getChar (fn _ => true) #-- comment depth
             || eof () >> (fn _ => raise Fail "Comment not closed")
            ) stream

	val lexer = makeStream token
		    
	fun getIf pred stream = 
	    case split stream of
		res as SOME(x, src) => if pred x then res
				       else NONE
	      | _  => NONE
		      
	infix 6 &-- --&
	fun (t &-- pf) = getIf(eq t) #-- pf
	fun (pf --& t) = pf --# getIf(eq t)
			 
	val getFile = getIf (fn (FILENAME_T _) => true | _ => false) >>
			    (fn (FILENAME_T s) => s)   
			    
	val imports = repeat getFile
		      
	fun body stream = 
	    (   getFile -- body >> SRC
	     || LOCAL_T &-- body -- IN_T &-- body -- END_T &-- body
	                                >> (fn((b1,b2),b3) => LOCAL(b1,b2,b3))
             || empty                      NULL
            ) stream

	val pm = 
	      IMPORT_T &-- imports -- IN_T &-- body --& END_T 
		       >> (fn (im,b) => PM{imports=im ,body=b})
	  || body      >> (fn b      => PM{imports=[], body=b})

	val pmfile = pm --& EOF_T
    in
    fun parse get src =
	let val stream = makeStream get src
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
	in  toList (lexer(makeStream Substring.getc ss))
	end

    fun parse get src =
	let val stream = makeStream get src
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
