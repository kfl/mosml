signature S = 
    sig type t val x : t val f : t -> unit end;

signature UNIT = sig end 
structure Unit = struct end

functor F1 (X:UNIT) :> S =
    struct
        type t = int
        val x = 12
        fun f x = ()
    end

functor F2 (X:UNIT) :> S =
    struct
        type t = unit -> unit
        fun x () = ()
        fun f g = g ()
    end

signature ARG = functor (X:UNIT) -> S 

functor Apply F:ARG = F(Unit)

structure Res1 = Apply(F1)
structure Res2 = Apply(F2)

val breakit = Res2.f(Res1.x)  (* core dump ensues *)

