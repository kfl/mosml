(* Using higher-order modules to represents numbers a la Church *)

(* 1995-03-29 for SML/NJ, 2000-01-21 for Moscow ML 2.00 *)

signature VAL = sig val x : int end
signature ITER = functor (Val : VAL) -> VAL;
signature NAT = functor (F : ITER) -> functor (Val : VAL) -> VAL;

structure Val = struct val x = 0 end;
functor Inc(Val : VAL) : VAL = 
    struct
	val x = Val.x + 1
    end

functor N0(F : ITER) (Val : VAL) : VAL =
    struct
	val x = Val.x
    end;

functor N2 (F : ITER) (Val : VAL) : VAL =
    struct
	structure Val = F(Val)
	structure Val = F(Val)
	val x = Val.x
    end;

functor N3 (F : ITER) (Val : VAL) : VAL =
    struct
	structure Val = F(Val)
	structure Val = F(Val)
	structure Val = F(Val)
	val x = Val.x
    end;

structure R3 = N3 (Inc) (Val);

functor Plus(F1 : NAT) (F2 : NAT)
            (F : ITER) (Val : VAL) : VAL =
    struct
	structure Val = F1(F) (Val)
	structure Val = F2(F) (Val)
	val x = Val.x
    end;

functor NR5(F : ITER) (Val : VAL) : VAL =
    struct
	structure V = Plus(N2) (N3) (F) (Val);
	val x = V.x
    end;

structure R5 = NR5 (Inc) (Val);

functor Times(F1 : NAT) (F2 : NAT) (F : ITER) (Val : VAL) : VAL =
    struct
	functor F2F(Val : VAL) : VAL =
	    struct
		structure V = F2 (F) (Val);
		val x = V.x
	    end;
	structure V = F1(F2F) (Val)
	val x = V.x
    end;

functor NR6(F : ITER) (Val : VAL) : VAL =
    struct
	structure V = Times(N2) (N3) (F) (Val);
	val x = V.x
    end;

structure R6 = NR6 (Inc) (Val);

functor NR15(F : ITER) (Val : VAL) : VAL =
    struct
	structure V = Times(NR5) (N3) (F) (Val);
	val x = V.x
    end;

structure R15 = NR15(Inc) (Val);

functor NR75(F : ITER) (Val : VAL) : VAL =
    struct
	structure V = Times(NR5) (NR15) (F) (Val);
	val x = V.x
    end;

structure R75 = NR75(Inc) (Val);

functor NR5625(F : ITER) (Val : VAL) : VAL =
    struct
	structure V = Times(NR75) (NR75) (F) (Val);
	val x = V.x
    end;

structure R5625 = NR5625(Inc) (Val);

fun mkn file n =
    let val os = TextIO.openOut file
	fun say s = TextIO.output(os, s);
	fun rep f 0 = ()
	  | rep f n = (f (); rep f (n-1))
    in
	say ("functor N" ^ makestring n ^ " (F : ITER) \
            \ (Val : VAL) : VAL =\nstruct\n");
	rep (fn _ => say "    structure Val = F(Val)\n") n;
	say "    val x = Val.x\nend;\n";
	TextIO.closeOut os
    end;
