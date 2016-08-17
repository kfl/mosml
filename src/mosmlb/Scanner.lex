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

(* Utility functions for expanding Path variables *)
(*
 * Search for character ch in string str and return two
 * substrings - before and after first occurence of this
 * char (ch is not included).
 *)
fun findAndSplit ch str =
    let
        fun iter head (c::tail) =
            if c = ch then
                (rev head, tail)
            else
                iter (c::head) tail
          | iter head [] = (rev head, [])
        val (head, tail) = iter [] (String.explode str)
    in
        (String.implode head, String.implode tail)
    end

(*
 * Search for path variable - substring of form
 * $(VARIABLE) - spaces and tabulation are NOT trimmed.
 * Returns VARIABLE and substrings before and after variable.
 *)
fun searchForPathVar path =
    let
        val (prefix, suffix) = findAndSplit #"$" path
        val (inVar, suffix) = findAndSplit #")" suffix
    in
        case String.explode inVar of
          #"("::tail =>
            SOME (String.implode tail, prefix, suffix)
        | _ => NONE
    end

(* Searches for path variables in path and replaces them with their
 * actual value from Mlb *)
fun substitutePathVars path =
    let
        fun iter prefix suffix =
            case searchForPathVar suffix  of
              NONE => prefix ^ suffix
            | SOME (variable, pre, suffix) =>
                iter (prefix ^ pre ^ (Mlb.pathVariable variable)) suffix
    in
        iter "" path
    end

(* Decoding escaped sequences. *)
fun processEscaped str =
    case String.fromCString str of
      SOME s => s
    | NONE => raise Fail "Invalid escape sequence"

(* We do not check for the presence of quotes! *)
fun unquote str = String.substring (str, 1, ((String.size str) - 2))

(* Quoted paths contain escaped sequences as well and can contain path variables! *)
fun unquotePath str = processEscaped (substitutePathVars (unquote str))

}

let alpha = [`A`-`Z``a`-`z`]
let digit = [`0`-`9`]
let alphaDigit = (alpha|digit)
let escaped = (`\\`alpha|`\\``\\`|`\\``'`|`\\``"`|`\\``?`|`\\`digit+|`\\``x`digit+)

let id = alpha(alphaDigit|`'`|`_`)*
let punctuation = (`.`|`,`|`;`|`:`)
let math = (`+`|`-`|`/`|`*`)
let inquotedChar = (alphaDigit|`'`|`_`|`-`|` `|`\t`|`$`|`(`|`)`|punctuation|math)

(* quotedStringPrefix is used in quoted paths *)
let quotedStringPrefix = `"`(inquotedChar|escaped)*
let quotedString = quotedStringPrefix`"`

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

let quotedFunPath = quotedStringPrefix".fun"`"`
let quotedMlbPath = quotedStringPrefix".mlb"`"`
let quotedSigPath = quotedStringPrefix".sig"`"`
let quotedSmlPath = quotedStringPrefix".sml"`"`

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

  | filePath".fun" { savePos lexbuf; PATH (Mlb.FUNFile, (getLexeme lexbuf)) }
  | quotedFunPath { savePos lexbuf; PATH (Mlb.FUNFile, (unquotePath (getLexeme lexbuf))) }

  | filePath".mlb" { savePos lexbuf; PATH (Mlb.MLBFile, (getLexeme lexbuf)) }
  | quotedMlbPath { savePos lexbuf; PATH (Mlb.MLBFile, (unquotePath (getLexeme lexbuf))) }

  | filePath".sig" { savePos lexbuf; PATH (Mlb.SIGFile, (getLexeme lexbuf)) }
  | quotedSigPath { savePos lexbuf; PATH (Mlb.SIGFile, (unquotePath (getLexeme lexbuf))) }

  | filePath".sml" { savePos lexbuf; PATH (Mlb.SMLFile, (getLexeme lexbuf)) }
  | quotedSmlPath { savePos lexbuf; PATH (Mlb.SMLFile, (unquotePath (getLexeme lexbuf))) }

  (* Perhaps quoted paths will conflict with quoted strings, so, we do not include general quoted path *)
  | filePath { savePos lexbuf; PATH (Mlb.UnknownFile, (getLexeme lexbuf)) }

  | quotedString
    { savePos lexbuf; STRING (processEscaped (getLexeme lexbuf)) }

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
