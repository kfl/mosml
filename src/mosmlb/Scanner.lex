(* Lexer for scanning .mlb files. *)
{

open Parser

(* Functions and variables to keep track of the position. *)
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

(* For nesting comments - unfortunately mosmllex does not allow 
 * parameters for rules. *)

(* mode encodes comment depth *)
datatype mode = Comment of int | Normal

val mode = ref Normal

fun beginComment () =
    case !mode of
      Normal => mode := Comment 1
    | Comment depth => mode := Comment (depth + 1)

fun endComment () =
    case !mode of
      Comment 0 => raise Fail "Invalid comment depth = 0."
    | Comment 1 => mode := Normal
    | Comment depth => mode := Comment (depth - 1)
    | _ => raise Fail "trying to end comment while not in comment."

fun inComment () =
    case !mode of
      Comment _ => true
    | _ => false

fun resetLexer () =
    (pos := 0; mode := Normal)

fun notTerminated msg lexbuf =
(
  resetLexer();
  raise Fail (msg ^ " not terminated")
)

fun printDebug token lexbuf =
    print (token ^ " \"" ^ (getLexeme lexbuf) ^ "\"\n")

}

let alpha = [`A`-`Z``a`-`z`]
let digit = [`0`-`9`]
let alphaDigit = (alpha|digit)

let id = alpha(alphaDigit|`'`|`_`)*
let quotedString = `"`(alphaDigit|`'`|`_`|`-`|` `|`\t`)*`"`

(* Path declarations, carefully translated from MLton distribution (mlb.lex) *)
let firstFileNameChar = (alphaDigit|`.`|`_`)
let fileNameChar = firstFileNameChar|`-` 
let pathVar = "$("(alpha|`_`)(alphaDigit|`_`)*")"
let fileName = (pathVar|firstFileNameChar)(pathVar|fileNameChar)*
let pathArc = (pathVar|fileName|"."|"..")
let relativePath = (pathArc"/")*
let absolutePath = "/"relativePath
let directoryPath = absolutePath|relativePath
let filePath = (directoryPath)fileName

rule Lexer = parse
    [^ `\000`-`\255`] { raise Fail "unreachable point in lexer." }
  | "structure" { savePos lexbuf; STRUCTURE }
  | "signature" { savePos lexbuf; SIGNATURE }
  | "functor" { savePos lexbuf; FUNCTOR }
  | "ann" { savePos lexbuf; ANN }
  | "and" { savePos lexbuf; AND }
  | "basis" { savePos lexbuf; BASIS }
  | "bas" { savePos lexbuf; BAS }
  | "in" { savePos lexbuf; IN }
  | "end" { savePos lexbuf; END }
  | "=" { savePos lexbuf; EQUAL }
  | "let" { savePos lexbuf; LET }
  | "local" { savePos lexbuf; LOCAL }
  | "open" { savePos lexbuf; OPEN }
  | ";" { savePos lexbuf; SEMICOLON }
  | (eof | `\^Z`) { resetLexer(); EOF }
  | id { savePos lexbuf; ID (getLexeme lexbuf)}
  | filePath".mlb" { savePos lexbuf; PATH (Mlb.MLBFile, (getLexeme lexbuf)) }
  | filePath".sml" { savePos lexbuf; PATH (Mlb.SMLFile, (getLexeme lexbuf)) }
  | filePath".sig" { savePos lexbuf; PATH (Mlb.SIGFile, (getLexeme lexbuf)) }
  | filePath".fun" { savePos lexbuf; PATH (Mlb.FUNFile, (getLexeme lexbuf)) }
  | filePath { savePos lexbuf; PATH (Mlb.UnknownFile, (getLexeme lexbuf)) }
  | quotedString { savePos lexbuf; STRING (getLexeme lexbuf)}
  | "(*" { beginComment (); Comment lexbuf }
  | "*)" { raise Fail "unexpected comment end." }
  | _ { Lexer lexbuf }

and Comment = parse
    "(*" { beginComment (); Comment lexbuf }
  | "*)" 
    { 
      endComment (); 
      if (inComment ()) then
        Comment lexbuf
      else
        Lexer lexbuf
    }
  | (eof | `\^Z`) { notTerminated "comment" lexbuf }
  | _ { Comment lexbuf }
;
