(* Generating a DFA as a set of mutually recursive functions *)

open List BasicIO Nonstdio Fnlib Syntax;

val is = ref std_in;
val os = ref std_out;

(* 1- Generating the actions *)

val copy_buffer = CharArray.array(1024, #" ");

fun copy_chunk (Location(start,stop)) =
  let fun copy s =
    if s <= 0 then () else
      let val n = if s < 1024 then s else 1024
          val m = buff_input (!is) copy_buffer 0 n
      in
        buff_output (!os) copy_buffer 0 m;
        copy (s - m)
      end
  in
    seek_in (!is) start;
    copy (stop - start)
  end
;

fun output_actname (i : int) =
    output(!os, "action_" ^ makestring i);

fun output_action (i : int, act) =
    (output_actname i; 
     output(!os, " lexbuf = (\n");
     copy_chunk act;
     output(!os, ")\nand ")
     );

fun output_actcheck []  = ()
  | output_actcheck [_] = ()
  | output_actcheck ((a1, _) :: ar) =
    (output(!os, "val _ = fn _ => [");
     output_actname a1;
     app (fn (a, _) => (output(!os, ", "); output_actname a)) ar;
     output(!os, "];\n")
     );

(* 2- Generating the states *)

val states = ref (Array.fromList [] : automata Array.array);

fun enumerate_vect v =
  let open Array infix 9 sub
      fun enum env pos =
        if pos >= length v then env else
          let val pl = lookup (v sub pos) env in
            pl := pos :: !pl; enum env (pos+1)
          end
          handle Subscript =>
            enum ((v sub pos, ref [pos]) :: env) (pos+1) 
  in
    Sort.sort
      (fn (e1, ref pl1) => fn (e2, ref pl2) => 
         List.length pl1 >= List.length pl2)
      (enum [] 0)
  end
;

fun output_move Backtrack =
      output(!os, "backtrack lexbuf")
  | output_move (Goto dest) =
      case Array.sub(!states, dest) of
        Perform act_num =>
          output(!os, "action_" ^ makestring act_num ^ " lexbuf")
      | _ =>
          output(!os, "state_" ^ makestring dest ^ " lexbuf")
;

fun output_char_const os (i : int) =
  if i <= 127 then
    output(os, makestring(Char.chr i))
  else
    (output(os, "#\"\\");
     output(os, makestring i);
     output(os, "\""))
;

fun addToInterv c acc =
  case acc of
      [] => [(c, c)]
    | (c1, c2) :: rest =>
        if c+1 = c1 then
          (c, c2) :: rest
        else
          (c, c) :: acc
;

fun mkIntervals cs =
  foldL addToInterv [] cs
;

fun addInterv dest (c1, c2) (intervs, singls) =
  if c1 > c2 then
    (intervs, singls)
  else if c2 - c1 >= 5 then
    ((dest, (c1, c2)) :: intervs, singls)
  else
    addInterv dest (c1+1, c2) (intervs, (dest, c1) :: singls)
;

fun unzipInterv trans =
  foldR
    (fn (dest, chars) => fn acc =>
       foldR (addInterv dest) acc (mkIntervals (!chars)))
    ([], [])
    trans
;

fun output_one_trans_i (dest, (c1, c2)) =
(
  output(!os, " if currChar >= ");
  output_char_const (!os) c1;
  output(!os, " andalso currChar <= ");
  output_char_const (!os) c2;
  output(!os, " then  ");
  output_move dest;
  output(!os, "\n else")
);

fun output_one_trans_s (dest, c) =
(
  output_char_const (!os) c;
  output(!os, " => ");
  output_move dest;
  output(!os, "\n |  ")
);

fun output_all_trans_i trans =
  app output_one_trans_i trans
;

fun output_all_trans_s trans =
(
  output(!os, " case currChar of\n    ");
  app output_one_trans_s trans;
  output(!os, "_ => ")
);

fun output_all_trans trans =
(
  case enumerate_vect trans of
      [] =>
        raise Fail "output_all_trans"
    | (default, _) :: rest =>
        (output(!os, " let val currChar = getNextChar lexbuf in\n");
         case unzipInterv rest of
             ([], trans_s) =>
               (output_all_trans_s trans_s;
                output_move default)
           | (trans_i, []) =>
               (output_all_trans_i trans_i;
                output(!os, " ");
                output_move default)
           | (trans_i, trans_s) =>
               (output_all_trans_i trans_i;
                output_all_trans_s trans_s;
                output_move default));
  output(!os, "\n end)\nand ")
);

fun output_state (state_num : int) = fn
    Perform i =>
      ()
  | Shift(what_to_do, moves) =>
      (output(!os,
         "state_"  ^ makestring state_num ^ " lexbuf = (\n");
       (case what_to_do of
            No_remember => ()
          | Remember i =>
              (output(!os,
                 " setLexLastPos lexbuf (getLexCurrPos lexbuf);\n");
               output(!os,
                 (" setLexLastAction lexbuf (magic action_" ^
                                  makestring i ^ ");\n"))));
       output_all_trans moves)
;

(* 3- Generating the entry points *)

fun output_entries [] = 
      raise Fail "output_entries"
  | output_entries ((name, state_num : int) :: rest) =
      (output(!os, name ^ " lexbuf =\n");
       (* sestoft@dina.kvl.dk 1999-08-09: reset lastaction for every token *)
       output(!os,
         "  (setLexLastAction lexbuf (magic dummyAction);\n");
       output(!os,
         "   setLexStartPos lexbuf (getLexCurrPos lexbuf);\n");
       output(!os,
         "   state_" ^ makestring state_num ^ " lexbuf)\n");
       case rest of
         [] => output(!os, "\n")
       | _  => (output(!os, "\nand "); output_entries rest))
;

(* All together *)

fun output_lexdef header (initial_st, st, actions) =
(
  output(std_out, makestring (Array.length st)); 
  output(std_out, " states, ");
  output(std_out, makestring
	 (List.foldl (op+) 0 (List.map List.length actions)));
  output(std_out, " actions.\n"); flush_out std_out;
  output(!os, "local open Obj Lexing in\n\n");
  copy_chunk header;
  output(!os, "\nfun ");
  states := st;
  app (app output_action) actions;
  for (fn i => output_state i (Array.sub(st, i)))
      0 (Array.length st - 1);
  output_entries initial_st;
  output(!os, "(* The following checks type consistency of actions *)\n");
  app output_actcheck actions;
  output(!os, "\nend\n")
);

