signature S = sig val x : ('a -> 'a) ref end;
structure S :> S = struct fun id x = x; val x = ref id end;
val _ = S.x := (fn _ => 0);

