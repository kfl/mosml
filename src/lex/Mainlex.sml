(* The lexer generator. Command-line parsing and expansion of abbreviations *)

local

open Lexing Parsing Miscsys;
open Syntax Scanner Grammar Lexgen Output;
open Fnlib
in
(* Lexer of stream *)

fun createLexerStream (is : BasicIO.instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
;

fun expandRe defining name ls =
    let fun expRe (Name s) =
	    (lookup s ls
	     handle Subscript => 
		 (output(std_err, 
			 if defining andalso s = name then 
			     "Illegal recursive let-definition: "
			 else "Undefined let-name: ");
		  output(std_err, s); output(std_err,"\n\n");
		  flush_out std_err;
		  BasicIO.exit 2))
	  | expRe (Sequence(r1,r2))     = Sequence (expRe r1, expRe r2)
	  | expRe (Alternative (r1,r2)) = Alternative (expRe r1, expRe r2)
	  | expRe (Repetition r)        = Repetition (expRe r)
	  | expRe x                     = x
    in expRe end

fun expandLet (name, re) ls = 
    (name, expandRe true name ls re) :: ls 

fun expandRule ls (name,rl) =
    (name, List.map (fn (r, loc) => (expandRe false name ls r, loc)) rl)

fun main () =
  let val () =
        if Vector.length command_line <> 2 then
          (output(std_err, "Usage: mosmllex <input file>\n");
           flush_out std_err;
           BasicIO.exit 2)
        else ()
      val source_name = Vector.sub(command_line, 1)
      val dest_name =
        if Filename.check_suffix source_name ".lex" then
          Filename.chop_suffix source_name ".lex" ^ ".sml"
        else
          source_name ^ ".sml" 
      val () = (is := open_in_bin source_name)
      val () = (os := open_out dest_name)
      val lexbuf =
        createLexerStream (!is)
      val Lexdef(header,lets,rules) =
        lexer_definition Scanner.main lexbuf
        handle
          ParseError x =>
            (output(std_err, "Syntax error around char ");
             output(std_err, makestring (getLexemeStart lexbuf));
             output(std_err, ".\n"); flush_out std_err;
             BasicIO.exit 2)
        | Scan_aux.Lexical_error s =>
            (output(std_err, "Lexical error around char ");
             output(std_err, makestring (getLexemeStart lexbuf));
             output(std_err, ": ");
             output(std_err, s);
             output(std_err, ".\n"); flush_out std_err;
             BasicIO.exit 2)
      val nlets = foldL expandLet [] lets
      val nrules = List.map (expandRule nlets) rules
      val dfa as (init, states, acts) = make_dfa nrules 
  in
    output_lexdef header dfa;
    close_in (!is);
    close_out (!os);
    BasicIO.exit 0
  end
;

val () = Printexc.f main ();
end
