structure Parser :> Parser =
struct

    datatype 'a stream =
	S of unit -> ('a * 'a stream) option

    fun makeStream get src =
	let fun next src () = 
	        case get src of
		    SOME(x, src) => SOME(x, S(next src))
		  | NONE         => NONE
	in S(next src)
	end

    fun split (S stream) = stream()
    fun head  (S stream) = Option.map #1 (stream())
    fun tail  (S stream) = Option.map #2 (stream())
    fun isNil (S stream) = not(Option.isSome(stream()))
    fun cons e stream    = S(fn()=>SOME(e,stream))
    val sNil           	 = S(fn()=>NONE)   

    type ('elm, 'res) parser = 'elm stream -> ('res * 'elm stream) option

    infix 6 $-- --$ #-- --#
    infix 5 --
    infix 3 >>
    infix 0 ||

    fun $$ c stream =
	case split stream of
	    res as SOME(c', stream) => if c = c' then res 
				       else NONE
	  | NONE => NONE
		    
		    
    fun $ s stream =
	let val len = size s
	    val sub = String.sub
	    infix sub
	    fun loop n stream =
		if n = len then SOME(s, stream)
		else (case split stream of
			  SOME(c, stream) => if c = (s sub n) then loop (n+1) stream
					  else NONE
			| NONE => NONE)
	in  loop 0 stream
	end
	    
    fun getChars pred stream = SOME(StringCvt.splitl pred split stream)
			       
    fun getChars1 pred stream =
	case StringCvt.splitl pred split stream of
	    ("", _) => NONE
	  | res     => SOME res
		       
    fun getChar pred stream =
	case split stream of
	    res as SOME(c, stream) => if pred c then res 
				   else NONE
	  | NONE => NONE
		    
    fun (pf1 -- pf2) stream =
	case pf1 stream of
	    SOME(x, stream) => (case pf2 stream of
				    SOME(y, stream) => SOME((x,y), stream)
				  | NONE            => NONE)
	  | NONE => NONE
		    
    fun (pf1 || pf2) stream =
	case pf1 stream of
	    NONE => pf2 stream
	  | res  => res 
		    
    fun (pf >> f) stream =
	case pf stream of
	    SOME(x, stream) => SOME(f x, stream)
	  | _               => NONE
			    
    fun pf1 #-- pf2 = pf1 -- pf2 >> #2
    fun pf1 --# pf2 = pf1 -- pf2 >> #1

			    
    fun optional pf stream =
	case pf stream of
	    SOME(x, stream) => SOME(SOME x,stream)
	  | NONE            => SOME(NONE, stream)

    fun repeat pf stream =
	let fun loop stream res =
		case pf stream of
		    SOME(x, stream) => loop stream (x::res)
		  | NONE            => SOME(List.rev res, stream)
	in  loop stream []
	end

    fun empty w stream = SOME(w, stream)

    fun eof w stream = if isNil stream then SOME(w,stream) else NONE

end
