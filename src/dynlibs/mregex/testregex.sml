(* Testing the POSIX regex interface -- sestoft@dina.kvl.dk 1999-01-03 *) 

app load ["Int", "Real", "Mosml", "Substring", "Regex"];

use "../../mosmllib/test/auxil.sml";

fun fapp f (x, r) = f x :: r
fun get i vec = Substring.string(Vector.sub(vec, i))

open Regex;

val test1a = (regcomp "(" [Extended]; "WRONG") 
             handle Regex _ => "OK" | _ => "WRONG"

val test1b = (regcomp "\\(" []; "WRONG") 
             handle Regex _ => "OK" | _ => "WRONG" 

local

(* To be compiled with the Extended flag: *)

val neg = [("ab+c", ["", "7g.ab+c,9jd", "K ab bbbbcL"]),
	   ("ab+c|[de]*f", ["", "7g.ab+c,9jd", "K ab bbbbcL"]),
	   ("ab+c|([de]*)f", ["", "7g.ab+c,9jd", "K ab bbbbcL"]),
	   ("ab+c", ["Abc", "7g.Abc,9jd", "K abBbbbcl"]),
	   ("ab+c|[de]*f", ["F", "7gF.ab+c,9jd", "K ab ddeedFL"]),
	   ("ab+c|([de]*)f", ["F", "7gF.ab+c,9jd", "K ab ddeedFL"])
	   ]
val pos = [("", ["", "abc"]),
	   ("()()()", ["", "abc"]),
	   ("ab+c", ["abc", "7g.abc,9jd", "K abbbbbcL"]),
	   ("ab+c|[de]*f", ["f", "7gf.ab+c,9jd", "K ab ddeedfL"]),
	   ("ab+c|([de]*)f", ["f", "7gf.ab+c,9jd", "K ab ddeedfL"])
	   ];

in

(* regexecBool *)

val test2a = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     let val regex = regcomp pat [Extended]
		     in not (List.exists (regexecBool regex []) ss) end)
	            neg)

val test2b = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     let val regex = regcomp pat [Extended]
		     in List.all (regexecBool regex []) ss end)
	            pos)

(* regexec *)

val test3a = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     let val regex = regcomp pat [Extended]
		     in 
			 not (List.exists (Option.isSome o regexec regex []) 
			                   ss) 
		     end)
	            neg)

val test3b = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     let val regex = regcomp pat [Extended]
		     in List.all (Option.isSome o regexec regex []) ss end)
	            pos)

(* regmatchBool *)

val test4a = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     not (List.exists (fn s => regmatchBool {pat=pat, tgt=s} 
				                            [Extended] []) ss))
	            neg)

val test4b = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     List.all (fn s => regmatchBool {pat=pat, tgt=s} 
				                    [Extended] []) ss)
	            pos)

(* regmatch *)

val test5a = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     not (List.exists (fn s => 
				       Option.isSome 
				       (regmatch {pat=pat, tgt=s} 
					         [Extended] [])) ss))
		     neg)

val test5b = 
    check'(fn _ => 
	   List.all (fn (pat, ss) => 
		     List.all (fn s => 
			       Option.isSome 
			       (regmatch {pat=pat, tgt=s} 
				         [Extended] [])) ss)
	            pos)

end (* definitions of neg and pos *)

local 

val repl1 = []
val repl2 = [Str "<<", 
	     Tr(String.translate (String.str o Char.toUpper), 0),
	     Str ">>"]
fun ftoc f = Int.toString(trunc((valOf(Real.fromString f) - 32.0) / 1.8))
val repl3 = [Str "[", Tr(ftoc, 1), Str ",", Sus 2, Str "/", Sus 2, Str "]"]
fun get i suss = Substring.string(Vector.sub(suss, i))
fun app f i suss = f(Substring.string(Vector.sub(suss, i)))
val repl4 = [Str "[", Trs(app ftoc 1), Str ",", Trs(get 2), Str "/", 
	     Trs (get 2), Str "]"]

fun checkreplace replace replacer (pat, xys) =
    let val regex = regcomp pat [Extended]
	fun check1 (x, y) = y = replace regex replacer x
    in List.all check1 xys end

val regex1 = regcomp "E(ab)E|F(cd)F" [Extended]

in 

(* replace *)

val test6a = check'(fn _ => List.all (checkreplace replace repl1)
		    [("ab+c", [("ababcab", "abab"),
			       ("ababcabgabbbch", "ababgh"),
			       ("abaBcabgabbbch", "abaBcabgh")])
		     ])

val test6b = check'(fn _ => List.all (checkreplace replace repl2)
		    [("ab+c", [("ababcab", "ab<<ABC>>ab"),
			       ("ababcabgabbbch", "ab<<ABC>>abg<<ABBBC>>h"),
			       ("abaBcabgabbbch", "abaBcabg<<ABBBC>>h")])
		     ])

val test6c = check'(fn _ => List.all (checkreplace replace repl3)
		    [("\\[([0-9]+),([0-9]+)\\]", 
		      [("hkj[32,123] [92,234] [212,6a6]ab",
			"hkj[0,123/123] [33,234/234] [212,6a6]ab"),
		       ("ababcabgabbbch", "ababcabgabbbch")])
		     ])

val test6d = check'(fn _ => List.all (checkreplace replace repl4)
		    [("\\[([0-9]+),([0-9]+)\\]", 
		      [("hkj[32,123] [92,234] [212,6a6]ab",
			"hkj[0,123/123] [33,234/234] [212,6a6]ab"),
		       ("ababcabgabbbch", "ababcabgabbbch")])
		     ])

val test6e = check'(fn _ => List.all (checkreplace replace repl1)
		    [("<[^>]*>", 
		      [("blah1 <P WIDTH=100%> blah2 blah3 </HTML>",
			"blah1  blah2 blah3 ")])
		     ])

val test6f = (replace (regcomp "" []) repl1 ""; "WRONG")
             handle Regex _ => "OK" | _ => "WRONG"

val test6g = 
    check'(fn _ =>
	   replace regex1 [] "xEabEyFcdFz" = "xyz"
	   andalso replace regex1 [Sus 0] "xEabEyFcdFz" = "xEabEyFcdFz"
	   andalso replace regex1 [Sus 1] "xEabEyFcdFz" = "xabyz"
	   andalso replace regex1 [Sus 2] "xEabEyFcdFz" = "xycdz"
	   andalso replace regex1 [Sus 1, Sus 2] "xEabEyFcdFz" = "xabycdz")

(* replace1 *)

val test7a = check'(fn _ => List.all (checkreplace replace1 repl1)
		    [("ab+c", [("ababcab", "abab"),
			       ("ababcabgabbbch", "ababgabbbch"),
			       ("abaBcabgabbbch", "abaBcabgh")])
		     ])

val test7b = check'(fn _ => List.all (checkreplace replace1 repl2)
		    [("ab+c", [("ababcab", "ab<<ABC>>ab"),
			       ("ababcabgabbbch", "ab<<ABC>>abgabbbch"),
			       ("abaBcabgabbbch", "abaBcabg<<ABBBC>>h")])
		     ])

val test7c = check'(fn _ => List.all (checkreplace replace1 repl3)
		    [("\\[([0-9]+),([0-9]+)\\]", 
		      [("hkj[32,123] [92,234] [212,6a6]ab",
			"hkj[0,123/123] [92,234] [212,6a6]ab"),
		       ("ababcabgabbbch", "ababcabgabbbch")])
		     ])

val test7d = check'(fn _ => List.all (checkreplace replace1 repl1)
		    [("<[^>]*>", 
		      [("blah1 <P WIDTH=100%> blah2 blah3 </HTML>",
			"blah1  blah2 blah3 </HTML>")])
		     ])

val test7e = check'(fn _ => 
		    replace1 (regcomp "" []) repl1 "" = "")

end (* definitions of repl1, repl2, repl3, ftoc, checkreplace *)


local
    fun checksubstitute substitute f (pat, xys) =
	let val regex = regcomp pat [Extended]
	    fun check1 (x, y) = y = substitute regex f x
	in List.all check1 xys end
    fun substitute' regex f s = 
	Substring.concat(List.rev 
			 (fold regex (op ::, fapp (Substring.all o f o get 0))
			       [] s))
in
   
(* substitute *)
val test8a = check'(fn _ =>
		    checksubstitute substitute (fn s => Int.toString (size s))
		    ("ab+c", [("abc", "3"), 
			      ("abcabbcabc", "343"),
			      ("gabbchacjabc", "g4hacj3"),
			      ("gaBbchacjabC", "gaBbchacjabC")]))

val test8b = (substitute (regcomp "" []) (fn x=>x) ""; "WRONG")
	     handle Regex _ => "OK" | _ => "WRONG"

val test8c = check'(fn _ =>
		    checksubstitute substitute' (fn s => Int.toString (size s))
		    ("ab+c", [("abc", "3"), 
			      ("abcabbcabc", "343"),
			      ("gabbchacjabc", "g4hacj3"),
			      ("gaBbchacjabC", "gaBbchacjabC")]))

val test8d = (substitute' (regcomp "" []) (fn x=>x) ""; "WRONG")
	     handle Regex _ => "OK" | _ => "WRONG"

(* substitute1 *)

val test9a = check'(fn _ =>
		    checksubstitute substitute1 (fn s => Int.toString (size s))
		    ("ab+c", [("abc", "3"), 
			      ("abcabbcabc", "3abbcabc"),
			      ("gabbchacjabc", "g4hacjabc"),
			      ("gaBbchacjabC", "gaBbchacjabC")]))

val test9b = check'(fn _ =>
		    substitute1 (regcomp "" []) (fn x=>x) "" = "")

end (* definition of checksubstitute  *)


local 
    val delim1 = regcomp " *; *" []
	
    fun checkfields fields delim xys =
	let fun chk (x, y) = 
	        y = List.map Substring.string (fields delim x)
	in List.all chk xys end

    fun fields' regex s = List.rev(fold regex (op ::, #2) [] s)
    fun tokens' regex s = 
	List.filter (not o Substring.isEmpty) (fields' regex s)
in

(* tokens *)

val test10a = check'(fn _ => checkfields tokens delim1 
		     [("", []),
		      ("56", ["56"]),
		      (";;", []),
		      ("56; 23 ; 22;; 89; 99", ["56", "23", "22", "89", "99"])
		      ])

val test10b = (tokens (regcomp "" []) ""; "WRONG")
              handle Regex _ => "OK" | _ => "WRONG"

val test10c = check'(fn _ => checkfields tokens' delim1 
		     [("", []),
		      ("56", ["56"]),
		      (";;", []),
		      ("56; 23 ; 22;; 89; 99", ["56", "23", "22", "89", "99"])
		      ])

val test10d = (tokens' (regcomp "" []) ""; "WRONG")
              handle Regex _ => "OK" | _ => "WRONG"


(* efficiency of tokens on large arguments *)

val _ = let 
    fun dup 0 s = s
      | dup n s = dup (n-1) (s^s)
    fun run n = 
	let val s = dup n "56; 23 ; 22;; 89; 99;  "
	    val res = Mosml.time (tokens delim1) s
	in 
	    print "size(s) = "; 
	    print (Int.toString (size s));
	    print "; length(res) = ";
	    print (Int.toString (length res));
	    print "\n"
	end
in
    List.app run (List.tabulate(14, fn x=>x))
end
		      
(* fields *)

val test11a = check'(fn _ => checkfields fields delim1 
		     [("", [""]),
		      ("56", ["56"]),
		      (";;", ["", "", ""]),
		      ("56; 23 ; 22;; 89; 99", ["56", "23", "22", "", 
						"89", "99"])
		      ])

val test11b = (fields (regcomp "" []) ""; "WRONG")
              handle Regex _ => "OK" | _ => "WRONG"

val test11c = check'(fn _ => checkfields fields' delim1 
		     [("", [""]),
		      ("56", ["56"]),
		      (";;", ["", "", ""]),
		      ("56; 23 ; 22;; 89; 99", ["56", "23", "22", "", 
						"89", "99"])
		      ])

val test11d = (fields' (regcomp "" []) ""; "WRONG")
              handle Regex _ => "OK" | _ => "WRONG"

end (* definitions of checkfields, delim1, delim2 *)

local
    val regex1 = regcomp "ab+c" [Extended]
    val regex2 = regcomp "a(b+)c" [Extended]
    val s1 = "abcabbcabbbc"
    val sus1 = Substring.extract(s1, 0, NONE)
    val sus2 = Substring.extract(s1, 1, NONE)
    val sus3 = Substring.extract(s1, 4, NONE)
    val sus4 = Substring.extract(s1, 8, NONE)
    val sus5 = Substring.extract(s1, 12, NONE)
    val susbad = Substring.extract(s1, 4, SOME 2)	(* non-suffix of s1 *)
    fun vec (SOME v) i = Substring.base(Vector.sub(v, i))
      | vec NONE     i = raise Fail "WRONG"
in

(* regnexec *)

val test12a = 
    check'(fn _ =>
	   vec (regnexec regex1 [] sus1) 0 = (s1, 0, 3)
	   andalso vec (regnexec regex1 [] sus2) 0 = (s1, 3, 4) 
	   andalso vec (regnexec regex1 [] sus3) 0 = (s1, 7, 5)
	   andalso not (isSome (regnexec regex1 [] sus4))
	   andalso not (isSome (regnexec regex1 [] sus5)))

val test12b = 
    check'(fn _ =>
	   vec (regnexec regex2 [] sus1) 0 = (s1, 0, 3)
	   andalso vec (regnexec regex2 [] sus2) 0 = (s1, 3, 4) 
	   andalso vec (regnexec regex2 [] sus3) 0 = (s1, 7, 5)
	   andalso not (isSome (regnexec regex2 [] sus4))
	   andalso not (isSome (regnexec regex2 [] sus5))
	   andalso vec (regnexec regex2 [] sus1) 1 = (s1, 1, 1)
	   andalso vec (regnexec regex2 [] sus2) 1 = (s1, 4, 2) 
	   andalso vec (regnexec regex2 [] sus3) 1 = (s1, 8, 3))

(* regnexecBool  *)

val test13a = 
    check'(fn _ =>
	   regnexecBool regex1 [] sus1
	   andalso regnexecBool regex1 [] sus2
	   andalso regnexecBool regex1 [] sus3
	   andalso not (regnexecBool regex1 [] sus4)
	   andalso not (regnexecBool regex1 [] sus5))

val test13b = 
    check'(fn _ =>
	   regnexecBool regex2 [] sus1
	   andalso regnexecBool regex2 [] sus2
	   andalso regnexecBool regex2 [] sus3
	   andalso not (regnexecBool regex2 [] sus4)
	   andalso not (regnexecBool regex2 [] sus5))

end (* regex1-2, s1, sus1-5, susbad, vec *)

local
    val smlint = regcomp "~?[0-9]+" [Extended] 
    val smlalphaid = regcomp  "[a-zA-Z0-9][a-zA-Z0-9'_]*" [Extended]
    val smlreal = 
	regcomp "[+~]?[0-9]+(\\.[0-9]+|(\\.[0-9]+)?[eE][+~]?[0-9]+)" 
	        [Extended]
    val htmlstarttag = regcomp "<([[:alnum:]]+)[^>]*>" [Extended]

    fun map' regex f s = List.rev (fold regex (#2, fapp f) [] s)
    fun app' regex f s = fold regex (ignore, f o #1) () s

    fun checkf f xys = List.all (fn (x, y) => f x = y) xys

    val intres = [("", []),
		  ("jkjsdh", []),
		  ("1", [1]),
		  ("ghdf 237hfu87.2 233~12", [237, 87, 2, 233, ~12])]

    val realres = [("", []),
		   ("jkjsdh", []),
		   ("1.0", [1.0]),
		   ("ghdf ~237E0hfu87.2 2.33e+004", [~237.0, 87.2, 23300.0])]

    val htmlres = [("", [""]),
		   ("blah", ["blah"]),
		   ("red green<BR>blue<br>white<p color=black> yellow<h2>pink\
		    \</h2>brown",
		    List.rev ["red green", "BR", "blue", "br", "white", 
			      "p", " yellow", "h2", "pink</h2>brown"])]

in
    
(* map *)

val test14a = 
    check'(fn _ => 
	   checkf (map smlint (valOf o Int.fromString o get 0)) intres
	   andalso checkf (map' smlint (valOf o Int.fromString o get 0)) 
	                  intres )

val test14b = 
    check'(fn _ => 
	   checkf (map smlreal (valOf o Real.fromString o get 0)) realres
	   andalso checkf (map' smlreal (valOf o Real.fromString o get 0)) 
	                  realres)

(* app *)

val test15a = 
    check'(fn _ => 
	   checkf(fn x => 
		  let val res = ref []
		      fun f v = res := !res@[valOf(Int.fromString(get 0 v))]
		  in app smlint f x; !res end) intres
	   andalso 
	   checkf(fn x => 
		  let val res = ref []
		      fun f v = res := !res@[valOf(Int.fromString(get 0 v))]
		  in app' smlint f x; !res end) intres)

(* fold *)

val test16a = 
    check'(fn _ =>
	   checkf (fold htmlstarttag (fapp Substring.string, fapp (get 1)) [])
	          htmlres)

val test16b = 
    (fold (regcomp "" []) (op::, #2) [] ""; "WRONG")
    handle Regex _ => "OK" | _ => "WRONG"
end;

val _ = quit();
