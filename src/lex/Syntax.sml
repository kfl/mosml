(* The shallow abstract syntax *)

datatype location =
    Location of int * int
;

datatype regular_expression =
    Epsilon
  | Characters  of char list
  | Sequence    of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition  of regular_expression
  | Name        of string
;

datatype lexer_definition =
    Lexdef of location *
              (string * regular_expression) list *
              (string * (regular_expression * location) list) list
;

(* Representation of automata *)

datatype automata =
    Perform of int
  | Shift of automata_trans * automata_move Array.array
and automata_trans =
    No_remember
  | Remember of int
and automata_move =
    Backtrack
  | Goto of int
;
