(* This test contains---intentionally--- a syntax error *)

let
  val x = 25
in
 (x, 10, x; x)
end;
