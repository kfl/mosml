(* mosml/src/dynlibs/mregex/Regex.sml
   sestoft@dina.kvl.dk -- 1998-12-25, 1999-01-02
 *)

open List Dynlib;

prim_type regex;	     (* An abstract finalized object, see mregex.c *)

exception Regex of string;

fun error fcn msg = raise Regex (fcn ^ ": " ^ msg)

(* Obtain a handle pointing to the library defining the C functions: *)

val dlh = dlopen { lib = "libmregex.so",
		   flag = RTLD_NOW, 
		   global = false }

val (REG_EXTENDED, REG_ICASE, REG_NEWLINE, REG_NOTBOL, REG_NOTEOL) 
    : word * word * word * word * word
    = app1 (dlsym dlh "mregex_getflags") ()

datatype cflag = Extended | Icase | Newline

fun cflagval Extended = REG_EXTENDED
  | cflagval Icase    = REG_ICASE
  | cflagval Newline  = REG_NEWLINE

val cflagsval = foldl (fn (f, res) => Word.orb(cflagval f, res)) 0w0

datatype eflag = Notbol | Noteol

fun eflagval Notbol = REG_NOTBOL
  | eflagval Noteol = REG_NOTEOL 

val eflagsval = foldl (fn (f, res) => Word.orb(eflagval f, res)) 0w0

val regcomp_ : string -> word -> regex 
    = app2 (dlsym dlh "mregex_regcomp")

fun regcomp pat cflags = 
    (regcomp_ pat (cflagsval cflags))
    handle Fail msg => error "regcomp" msg

val regexec_sus_ : regex -> word -> substring -> substring vector option 
    = app3 (dlsym dlh "mregex_regexec_sus")

fun regexec regex eflags tgt = 
    (regexec_sus_ regex (eflagsval eflags) (Substring.all tgt))
    handle Fail msg => error "regexec" msg

fun regnexec regex eflags sus = 
    (regexec_sus_ regex (eflagsval eflags) sus)
    handle Fail msg => error "regnexec" msg

val regexec_bool_ : regex -> word -> substring -> bool 
    = app3 (dlsym dlh "mregex_regexec_bool")

fun regexecBool regex eflags tgt = 
    (regexec_bool_ regex (eflagsval eflags) (Substring.all tgt))
    handle Fail msg => error "regexecBool" msg

fun regnexecBool regex eflags sus = 
    (regexec_bool_ regex (eflagsval eflags) sus)
    handle Fail msg => error "regnexecBool" msg
 
val regmatch_sus_ : string -> word -> word -> substring 
                    -> substring vector option
    = app4 (dlsym dlh "mregex_regmatch_sus")

fun regmatch { pat : string, tgt : string } cflags eflags =
    (regmatch_sus_ pat (cflagsval cflags) (eflagsval eflags) 
                   (Substring.all tgt))
    handle Fail msg => error "regmatch" msg

val regmatch_bool_ : string -> word -> word -> substring -> bool
    = app4 (dlsym dlh "mregex_regmatch_bool")

fun regmatchBool { pat : string, tgt : string } cflags eflags =
    (regmatch_bool_ pat (cflagsval cflags) (eflagsval eflags)  
                    (Substring.all tgt))
    handle Fail msg => error "regmatchBool" msg


(* Return the substring to the left of the given substring *)

(* Precondition: s2 is a substring of s1, that is, 
   s1 = s2 and i1<=i2 and i2+n2<=i1+n1
*)

fun leftsus (sus1 : substring) (sus2 : Substring.substring) = 
    let val (s1, i1, n1) = Substring.base sus1
	val (s2, i2, n2) = Substring.base sus2
    in
	if s1 = s2 andalso i1<=i2 andalso i2+n2<=i1+n1 then 
	    Substring.substring(s1, i1, i2-i1)
	else 
	    raise Subscript
    end

(* Faster, non-checking version *)

fun leftsus_ (sus1 : substring) (sus2 : Substring.substring) = 
    let val (s1, i1, _) = Substring.base sus1
	val (_,  i2, _) = Substring.base sus2
    in
	Substring.substring(s1, i1, i2-i1)
    end

(* Return the substring to the right of the given substring *)

(* Precondition: sus is a substring of s, that is, s = s' and sus is valid *)

fun right (s : string) (sus : Substring.substring) = 
    let val (s', i, n) = Substring.base sus
    in
	if s = s' then Substring.extract(s', i+n, NONE)
	else raise Subscript
    end

datatype replacer =
    Str of string
  | Sus of int
  | Tr  of (string -> string) * int
  | Trs of substring vector -> string

fun applyreplacer suss replacer res = 
    let open Substring
	fun h []                  res = res
	  | h (Str s :: rest)     res = h rest (all s :: res)
	  | h (Sus i :: rest)     res = h rest (Vector.sub(suss, i) :: res)
	  | h (Tr (f,i)  :: rest) res = 
	    h rest (all (f (string (Vector.sub(suss, i)))) :: res)
	  | h (Trs f :: rest) res     = h rest (all (f suss) :: res)
    in h replacer res end

fun replace1 regex replacer s = 
    let open Substring
	val sus = all s
    in
	case regexec_sus_ regex 0w0 sus of
	    NONE      => s
	  | SOME suss => 
		let val match  = Vector.sub(suss, 0) 
		    val left'  = leftsus sus match 
		    val right' = right s match
		    val repl'  = applyreplacer suss replacer []
		in Substring.concat(left' :: rev (right' :: repl')) end
    end

fun replace_aux regex fcn replacer s = 
    let open Substring
	fun h sus revres = 
	    case regexec_sus_ regex 0w0 sus of
		NONE      => Substring.concat (List.rev (sus :: revres))
	      | SOME suss => 
		    let val match   = Vector.sub(suss, 0)
			val field1  = leftsus sus match
			val revres1 = applyreplacer suss replacer
			              (field1 :: revres)
		    in 
			(* Check that we make progress *)
			if isEmpty field1 andalso isEmpty match then 
			    error fcn "no progress"
			else 
			    h (right s match) revres1
		    end
    in h (all s) [] end  

fun replace regex replacer s = 
    replace_aux regex "replace" replacer s

fun substitute1 regex tr s = replace1 regex [Tr (tr, 0)] s

fun substitute regex tr s = 
    replace_aux regex "substitute" [Tr (tr, 0)] s

fun split regex fcn add s = 
    let open Substring
	val eflags = Word.orb(REG_NOTBOL, REG_NOTEOL)
	fun h sus revres = 
	    case regexec_sus_ regex eflags sus of
		NONE      => List.rev (add sus revres)
	      | SOME suss => 
		    let val match   = Vector.sub(suss, 0)
			val field1  = leftsus sus match
			val revres1 = add field1 revres
		    in 
			(* Check that we make progress *)
			if isEmpty field1 andalso isEmpty match then 
			    error fcn "no progress"
			else 
			    h (right s match) revres1
		    end
    in h (all s) [] end

fun addfield sus res = 
    sus :: res

fun fields regex s = split regex "fields" addfield s

fun addtoken sus res = 
    if Substring.isEmpty sus then res else sus :: res

fun tokens regex s = split regex "tokens" addtoken s

fun fold regex (fa, fb) e s = 
    let open Substring
	fun h sus res = 
	    case regexec_sus_ regex 0w0 sus of
		NONE      => fa(sus, res)
	      | SOME suss => 
		    let val match   = Vector.sub(suss, 0)
			val field1  = leftsus sus match
			val res1    = fb (suss, fa(field1, res))
		    in 
			(* Check that we make progress *)
			if isEmpty field1 andalso isEmpty match then 
			    error "fold" "no progress"
			else 
			    h (right s match) res1
		    end
    in h (all s) e end  

fun map regex f s = 
    let open Substring
	fun h sus revres = 
	    case regexec_sus_ regex 0w0 sus of
		NONE      => List.rev revres
	      | SOME suss => 
		    let val match   = Vector.sub(suss, 0)
			val field1  = leftsus sus match
			val revres1 = f suss :: revres
		    in 
			(* Check that we make progress *)
			if isEmpty field1 andalso isEmpty match then 
			    error "map" "no progress"
			else 
			    h (right s match) revres1
		    end
    in h (all s) [] end  

fun app regex f s = 
    let open Substring
	fun h sus = 
	    case regexec_sus_ regex 0w0 sus of
		NONE      => ()
	      | SOME suss => 
		    let val match   = Vector.sub(suss, 0)
			val field1  = leftsus sus match
			val revres1 = f suss
		    in 
			(* Check that we make progress *)
			if isEmpty field1 andalso isEmpty match then 
			    error "app" "no progress"
			else 
			    h (right s match)
		    end
    in h (all s) end  


