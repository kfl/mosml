(* Lexer for scanning .mlb files. *)
{

open Parser

(* For calculating position ala ocamllex - filename, line number and
   current position in stream of the start of the current line. *)
val fileName = ref ""
val lineNumber = ref 1
val lineStartPos = ref 0

fun newLinePosUpdate lexbuf =
(
    lineNumber := !lineNumber + 1;
    lineStartPos := Lexing.getLexemeEnd lexbuf
)

(* we have to update the position and lineNumber after reading
   quoted string, since it can contain new line chars. *)
fun quotedStringPosUpdate lexbuf =
(
    let
        val str = getLexeme lexbuf
        val (nlNumber, shift, _) =
            foldl
                (fn (ch, (nlNumber, shift, counter)) =>
                    if ch = #"\n" then
                        (nlNumber + 1, counter, counter + 1)
                    else
                        (nlNumber, shift, counter + 1)
                )
                (0, 0, 0) (String.explode str)
    in
        lineNumber := !lineNumber + nlNumber;
        lineStartPos := (Lexing.getLexemeStart lexbuf) + shift
    end
)

(* position of current lexeme (start or start-end).
   we do not process lexemes occupying several lines. *)
fun currentPosition onlyStart lexbuf =
    let
        (* seek - position from the beginning of stream *)
        fun pos seek = (!lineNumber, seek - !lineStartPos)
    in
        if onlyStart then
            (!fileName, (pos (Lexing.getLexemeStart lexbuf)), NONE)
        else
            (!fileName, (pos (Lexing.getLexemeStart lexbuf)), 
                              SOME (pos (Lexing.getLexemeEnd lexbuf)))
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
    (mode := Normal)

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
  | "structure" { STRUCTURE }
  | "signature" { SIGNATURE }
  | "functor" { FUNCTOR }
  | "ann" { ANN }
  | "and" { AND }
  | "basis" { BASIS }
  | "bas" { BAS }
  | "in" { IN }
  | "end" { END }
  | "=" { EQUAL }
  | "let" { LET }
  | "local" { LOCAL }
  | "open" { OPEN }
  | ";" { SEMICOLON }
  | (eof | `\^Z`) { resetLexer(); EOF }
  | id { ID (getLexeme lexbuf)}

  | filePath".fun" { PATH (Mlb.FUNFile, (getLexeme lexbuf)) }
  | quotedFunPath { PATH (Mlb.FUNFile, (unquotePath (getLexeme lexbuf))) }

  | filePath".mlb" { PATH (Mlb.MLBFile, (getLexeme lexbuf)) }
  | quotedMlbPath { PATH (Mlb.MLBFile, (unquotePath (getLexeme lexbuf))) }

  | filePath".sig" { PATH (Mlb.SIGFile, (getLexeme lexbuf)) }
  | quotedSigPath { PATH (Mlb.SIGFile, (unquotePath (getLexeme lexbuf))) }

  | filePath".sml" { PATH (Mlb.SMLFile, (getLexeme lexbuf)) }
  | quotedSmlPath { PATH (Mlb.SMLFile, (unquotePath (getLexeme lexbuf))) }

  (* Perhaps quoted paths will conflict with quoted strings, so, we do not include general quoted path *)
  | filePath { PATH (Mlb.UnknownFile, (getLexeme lexbuf)) }

  | quotedString
    { quotedStringPosUpdate lexbuf; STRING (processEscaped (getLexeme lexbuf)) }

  | "(*" { beginComment (); Comment lexbuf }
  | "*)" { Log.error (Log.UnexpectedCommentEnd (currentPosition true lexbuf)); Lexer lexbuf }
  | `\n` { newLinePosUpdate lexbuf; Lexer lexbuf }
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
  | `\n` { newLinePosUpdate lexbuf; Comment lexbuf }
  | _ { Comment lexbuf }
;
