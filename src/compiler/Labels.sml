(* Handlings of local labels and backpatching *)

local 
  open Fnlib Instruct Buffcode
in

datatype label_definition =
    Label_defined of int
  | Label_undefined of (int * int) list
;

val label_table  = ref (Array.fromList [] : label_definition Array.array)
;

fun reset_label_table () =
  label_table := Array.array(16, Label_undefined [])
;

fun extend_label_table needed =
  let val old = Array.length (!label_table)
      val new_table = 
        Array.array((needed div old + 1) * old, Label_undefined [])
  in
    Array.copy { src= !label_table, si=0, len = NONE, dst= new_table, di=0 };
    label_table := new_table
  end;

fun define_label lbl =
(
  if lbl < Array.length (!label_table) then () else 
    extend_label_table lbl;
  case Array.sub(!label_table, lbl) of
      Label_defined _ =>
        fatalError "define_label : already defined"
    | Label_undefined L =>
        let val currpos = !out_position in
          Array.update(!label_table, lbl, Label_defined currpos);
          case L of
              [] => ()
            |  _ => (* Backpatching the list L of pending labels: *)
              (List.app (fn (pos,orig) => 
                           (out_position := pos;
                            out_long (currpos - orig)))
                        L;
               out_position := currpos)
        end
);

fun out_label_with_orig orig lbl =
(
  if lbl = Nolabel then 
    fatalError "out_label: undefined label"
  else if lbl >= Array.length (!label_table) then
    extend_label_table lbl
  else ();
  case Array.sub(!label_table, lbl) of
      Label_defined def =>
        out_long (def - orig)
    | Label_undefined L =>
        (Array.update(!label_table, lbl,
           Label_undefined ((!out_position, orig) :: L));
         out_long 0)
);

fun out_label l = out_label_with_orig (!out_position) l;

end;
