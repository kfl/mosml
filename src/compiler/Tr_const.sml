local
  open Obj Const Symtable;
in

(* To translate a structured constant into an object. *)

fun translStructuredConst (ATOMsc(INTscon i)) = repr i
  | translStructuredConst (ATOMsc(WORDscon w)) = repr w
  | translStructuredConst (ATOMsc(CHARscon c)) = repr c
  | translStructuredConst (ATOMsc(REALscon f)) = repr f
  | translStructuredConst (ATOMsc(STRINGscon s)) = repr s
  | translStructuredConst (BLOCKsc(tag, comps)) =
      let val res = obj_block (intOfTag tag) (List.length comps) in
        fillStructuredConst 0 res comps;
        res
      end
  | translStructuredConst (QUOTEsc (ref v)) = v

and fillStructuredConst n obj = fn
    [] => ()
  | cst::rest =>
      (set_obj_field obj n (translStructuredConst cst);
       fillStructuredConst (n+1) obj rest)
;

end;
