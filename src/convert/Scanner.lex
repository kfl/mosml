{

open Parser

val pos = ref 0

fun savePos lexbuf = pos := getLexemeEnd lexbuf

fun getPosStart lexbuf = 
    let val endpos = getLexemeStart lexbuf
    in  (!pos, endpos) before pos := endpos
    end

fun getPosEnd lexbuf = 
    let val endpos = getLexemeEnd lexbuf
    in  (!pos, endpos) before pos := endpos
    end

(* For nesting comments *)

val comment_depth = ref 0

fun incr r = r := !r + 1
fun decr r = r := !r - 1

datatype mode = COM | NORM | ENDOFFILE
val mode = ref NORM

fun resetLexer () =
    (pos := 0; mode := NORM)

fun notTerminated msg lexbuf =
(
  resetLexer();
  raise Fail (msg ^ " not terminated")
)



}

rule Token = parse
    [^ `\000`-`\255`]
      { raise Fail "this will be never called!" }
  | ""
      { case !mode of
            NORM => MLStuff lexbuf
          | COM  => Comment lexbuf
	  | ENDOFFILE => (resetLexer(); EOF)
      } 

and MLStuff = parse
    "(*" { comment_depth := 1; mode := COM; MLSTUFF(getPosStart lexbuf) }
  | "\"" { String lexbuf }
  | "`"  { Quotation lexbuf }
  | "structure" { savePos lexbuf; STRUCTURE }
  | "signature" { savePos lexbuf; SIGNATURE }
  | (eof | `\^Z`) { mode := ENDOFFILE; 
		    MLSTUFF(getPosStart lexbuf) }
  | _    { MLStuff lexbuf }

and Comment = parse
    "(*"
      { (incr comment_depth; Comment lexbuf) }
  | "*)"
      { (decr comment_depth;
         if !comment_depth > 0 then Comment lexbuf 
	 else (mode := NORM; COMMENT(getPosEnd lexbuf))) }
  | (eof | `\^Z`)
      { notTerminated "comment" lexbuf }
  | _ { Comment lexbuf }

and String = parse
    `"` {  MLStuff lexbuf }
  | `\\` [`\\` `"` `n` `t`]
      { String lexbuf }
  | `\\` [` ` `\t` `\n` `\r`]+ `\\`
      { String lexbuf }
  | `\\` `^` [`@`-`_`]
      { String lexbuf }
  | `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`]
      { String lexbuf }
  | `\\`
      { SkipString lexbuf }
  | (eof | `\^Z`) { notTerminated "string" lexbuf }
  | [`\^A`-`\^Z` `\127` `\255`]
      { SkipString lexbuf }
  | _
      { String lexbuf }

and SkipString = parse
    `"` {  MLStuff lexbuf }
  | `\\` [`\\` `"` `n` `t`]
      { SkipString lexbuf }
  | `\\` [` ` `\t` `\n` `\r`]+ `\\`
      { SkipString lexbuf }
  | (eof | `\^Z`){ notTerminated "string" lexbuf }
  | _
      { SkipString lexbuf }

and Quotation = parse
    "`" { MLStuff lexbuf }
  | (eof | `\^Z`) { notTerminated "quotation" lexbuf }
  | _ { Quotation lexbuf }
;
