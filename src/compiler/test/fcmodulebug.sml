(* this is a bug because a functor can't eliminate a package type in its body *)
(* WrongGen is rejected, but WrongApp is not! *)
functor WrongApp X:sig end = struct structure X as sig type t end = [structure struct  type t = int end as sig type t end] end;

functor WrongGen (A : sig end) = struct structure X as sig type t end = [structure struct  type t = int end as sig type t end] end;

