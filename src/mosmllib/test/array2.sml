(* File "test/array2.sml" 1995-09-12, 1997-03-12, 1998-04-07 *)

val _ = load "Array2";			(* MOSML *)

use "auxil.sml";

local 
    open Array2
in

val a0 = tabulate RowMajor (0, 0, fn (i, j) => 1 div 0);
fun fill (i, j) = 10 * i + j

local 
    val sequence = ref ([] : (int * int) list)
in
val a = tabulate RowMajor 
        (3, 4, fn args => (sequence := args :: !sequence; fill args));

val test0a = check(!sequence = [(2,3), (2,2), (2,1), (2,0), 
				(1,3), (1,2), (1,1), (1,0), 
				(0,3), (0,2), (0,1), (0,0)]);
end

local 
    val sequence = ref ([] : (int * int) list)
    val a' = 
	tabulate ColMajor 
        (3, 4, fn args => (sequence := args :: !sequence; fill args));
in
    val test0b = check(!sequence = [(2,3), (1,3), (0,3),
				    (2,2), (1,2), (0,2),
				    (2,1), (1,1), (0,1), 
				    (2,0), (1,0), (0,0)]
		       andalso foldi RowMajor
		                     (fn (i, j, aij, same) =>
				      same andalso sub(a, i, j) = aij)
		                     true {base=a', row=0, col=0, 
					   nrows=NONE, ncols=NONE})
end

val test1a = 
    check'(fn _ => 
	   sub(a, 1, 2) = 12
	   andalso sub(a, 0, 0) = 0
	   andalso sub(a, 0, 3) = 3
	   andalso sub(a, 2, 0) = 20
	   andalso sub(a, 2, 3) = 23);

val test1b = (sub(a, 3, 0) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test1c = (sub(a, 0, 4) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test1d = (sub(a, ~1, 0) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test1e = (sub(a, 0, ~1) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test1f = (sub(a, ~1, ~1) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val _ = update(a, 1, 2, 112);

val test2 =
    check'(fn _ => 
	   sub(a, 1, 2) = 112
	   andalso sub(a, 1, 1) = 11
	   andalso sub(a, 1, 3) = 13
	   andalso sub(a, 0, 2) =  2
	   andalso sub(a, 2, 2) = 22);

val test3a = check'(fn _ => dimensions a = (3, 4));
val test3b = check'(fn _ => nRows a = 3);
val test3c = check'(fn _ => nCols a = 4);

val test4a = 
    check'(fn _ =>
	   row(a, 0) = Vector.tabulate(4, fn j => fill(0, j)));
val test4b = 
    check'(fn _ =>
	   row(a, 2) = Vector.tabulate(4, fn j => fill(2, j)));
val test4c = (row(a, 4) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test4d = (row(a, ~1) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val test5a = 
    check'(fn _ =>
	   column(a, 0) = Vector.tabulate(3, fn i => fill(i, 0)));
val test5b = 
    check'(fn _ =>
	   column(a, 3) = Vector.tabulate(3, fn i => fill(i, 3)));
val test5c = (column(a,  4) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test5d = (column(a, ~1) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val a1 = tabulate RowMajor (3, 4, fill); (* Supposed to be constant below *)

fun testcopy { row, col, nrows, ncols } dst_row dst_col reschk = 
    check'(fn _ => 
	   let val a2 = tabulate RowMajor (3, 4, fill);
	       val src = { base=a2, row=row, col=col, nrows=nrows, ncols=ncols}
	       val _ = copy { src=src, dst=a2, 
			      dst_col=dst_col, dst_row=dst_row } 
	   in reschk a2 end);

fun same a2 = List.all (fn i => row(a2, i) = row(a1, i)) [0, 1, 2]

fun elts matrix a2 = [row(a2, 0), row(a2, 1), row(a2, 2)] = matrix

val test6a = 
    testcopy {row=0, col=0, nrows=NONE, ncols=NONE} 0 0 same
val test6b = 
    testcopy {row=3,  col=0,  nrows=NONE, ncols=NONE } 0 0 same
val test6c = 
    testcopy {row=0,  col=4,  nrows=NONE, ncols=NONE } 0 0 same
val test6d = 
    testcopy {row=3,  col=4,  nrows=NONE, ncols=NONE } 0 0 same
val test6e = 
    testcopy {row=3,  col=4,  nrows=SOME 0, ncols=SOME 0 } 0 0 same
val test6f = 
    testcopy {row=1,  col=1,  nrows=SOME 0, ncols=SOME 2 } 0 0 same
val test6g = 
    testcopy {row=1,  col=1,  nrows=SOME 2, ncols=SOME 0 } 0 0 same
val test6h = 
    testcopy {row=0,  col=0,  nrows=NONE, ncols=SOME 3 } 0 1
    (elts [ #[0, 0, 1, 2], #[10, 10, 11, 12], #[20, 20, 21, 22]])
val test6i = 
    testcopy {row=0,  col=0,  nrows=SOME 2, ncols=NONE } 1 0
    (elts [ #[0, 1, 2, 3], #[0, 1, 2, 3], #[10, 11, 12, 13]])
val test6j = 
    testcopy {row=0,  col=0,  nrows=SOME 2, ncols=SOME 3 } 1 1
    (elts [ #[0, 1, 2, 3], #[10, 0, 1, 2], #[20, 10, 11, 12]])
val test6k = 
    testcopy {row=1,  col=1,  nrows=SOME 2, ncols=SOME 3 } 0 0
    (elts [ #[11, 12, 13, 3], #[21, 22, 23, 13], #[20, 21, 22, 23]])
val test6l = 
    testcopy {row=0,  col=1,  nrows=SOME 2, ncols=SOME 3 } 1 0
    (elts [ #[0, 1, 2, 3], #[1, 2, 3, 13], #[11, 12, 13, 23]])
val test6m = 
    testcopy {row=0,  col=1,  nrows=SOME 2, ncols=SOME 3 } 1 1
    (elts [ #[0, 1, 2, 3], #[10, 1, 2, 3], #[20, 11, 12, 13]])
val test6n = 
    testcopy {row=0,  col=1,  nrows=NONE, ncols=SOME 1 } 0 3
    (elts [ #[0, 1, 2, 1], #[10, 11, 12, 11], #[20, 21, 22, 21]])
val test6o = 
    testcopy {row=1,  col=0,  nrows=SOME 1, ncols=NONE } 2 0
    (elts [ #[0, 1, 2, 3], #[10, 11, 12, 13], #[10, 11, 12, 13]])

fun failcopy { row, col, nrows, ncols } dst_row dst_col = 
    (copy { src={ base=a1, row=row, col=col, nrows=nrows, ncols=ncols}, 
	    dst=a1, dst_col=dst_col, dst_row=dst_row } seq "WRONG")
    handle Subscript => "OK" | _ => "WRONG"

val test7a = failcopy {row=0,   col=0,   nrows=NONE,   ncols=NONE   } 0 1
val test7b = failcopy {row=0,   col=0,   nrows=NONE,   ncols=NONE   } 1 0
val test7c = failcopy {row=0,   col=0,   nrows=NONE,   ncols=SOME 4 } 0 1
val test7d = failcopy {row=0,   col=0,   nrows=SOME 3, ncols=NONE   } 1 0
val test7e = failcopy {row=0,   col=0,   nrows=NONE,   ncols=SOME 5 } 0 0
val test7f = failcopy {row=0,   col=0,   nrows=SOME 4, ncols=NONE   } 0 0
val test7g = failcopy {row= ~1, col=0,   nrows=NONE,   ncols=NONE   } 0 0
val test7h = failcopy {row=0,   col= ~1, nrows=NONE,   ncols=NONE   } 0 0
val test7i = failcopy {row=3,   col=0,   nrows=SOME 1, ncols=NONE   } 0 0
val test7j = failcopy {row=0,   col=4,   nrows=NONE,   ncols=SOME 1 } 0 0
val test7k = failcopy {row=1,   col=1,   nrows=NONE,   ncols=NONE   } 0 2
val test7l = failcopy {row=1,   col=1,   nrows=NONE,   ncols=NONE   } 2 0
val test7m = failcopy {row=1,   col=1,   nrows=NONE,   ncols=SOME 3 } 0 2
val test7n = failcopy {row=1,   col=1,   nrows=SOME 2, ncols=NONE   } 2 0

val sequence = ref ([] : int list);

fun collect a = sequence := a :: !sequence;

fun collecti (i, j, a) = 
    if fill(i, j) = a then 
	sequence := a :: !sequence
    else
	raise Fail ("collecti: Error in " ^ Int.toString a)

val a3 = tabulate RowMajor (3, 4, fill);

val test8aa = check'(fn _ => 
    (sequence := []; 
     app RowMajor collect a3;
     !sequence = [23, 22, 21, 20, 
		  13, 12, 11, 10, 
		  3,   2,  1,  0]));
val test8ab = check'(fn _ => 
    (sequence := []; 
     app ColMajor collect a3;
     !sequence = [23, 13, 3, 
		  22, 12, 2, 
		  21, 11, 1, 
		  20, 10, 0]));
val test8ba = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=0, col=0, nrows=NONE, ncols=NONE };
     !sequence = [23, 22, 21, 20, 13, 12, 11, 10, 3, 2, 1, 0]));
val test8bb = check'(fn _ => 
    (sequence := []; 
     appi ColMajor collecti { base=a3, row=0, col=0, nrows=NONE, ncols=NONE };
     !sequence = [23, 13, 3, 22, 12, 2, 21, 11, 1, 20, 10, 0]));
val test8c = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=0, col=1, nrows=NONE, ncols=NONE };
     !sequence = [23, 22, 21, 13, 12, 11, 3, 2, 1]));
val test8d = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=1, col=0, nrows=NONE, ncols=NONE };
     !sequence = [23, 22, 21, 20, 13, 12, 11, 10]));
val test8e = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=1, col=1, nrows=NONE, ncols=NONE };
     !sequence = [23, 22, 21, 13, 12, 11]));
val test8f = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=3, col=0, nrows=NONE, ncols=NONE };
     !sequence = []));
val test8g = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=0, col=4, nrows=NONE, ncols=NONE };
     !sequence = []));
val test8h = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=1, col=1, nrows=SOME 0, ncols=NONE};
     !sequence = []));
val test8i = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=1, col=1, nrows=NONE, ncols=SOME 0};
     !sequence = []));
val test8j = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=1, col=1, nrows=SOME 1, ncols=NONE};
     !sequence = [13, 12, 11]));
val test8k = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti { base=a3, row=1, col=1, nrows=NONE, ncols=SOME 1};
     !sequence = [21, 11]));
val test8l = check'(fn _ => 
    (sequence := []; 
     appi RowMajor collecti {base=a3, row=0, col=1, nrows=SOME 2, ncols=SOME 2};
     !sequence = [12, 11, 2, 1]));

fun chkmodify resseq reschk =
    check'(fn _ =>
	   let val a3 = tabulate RowMajor (3, 4, fill)
	   in 
	       sequence := [];
	       modify RowMajor (fn a => (collect a; a*10)) a3;
	       !sequence = resseq andalso reschk a3
	   end)

fun chkmodifyi { row, col, nrows, ncols } resseq reschk =
    check'(fn _ =>
	   let val a3 = tabulate RowMajor (3, 4, fill)
	   in 
	       sequence := [];
	       modifyi RowMajor (fn (args as (i, j, a)) => (collecti args; 
							    a*10))
	              {base=a3, row=row, col=col, nrows=nrows, ncols=ncols};
	       !sequence = resseq andalso reschk a3
	   end)

val test9a = 
    chkmodify 
      [23, 22, 21, 20, 13, 12, 11, 10, 3, 2, 1, 0]
      (elts [#[0, 10, 20, 30], #[100, 110, 120, 130], #[200, 210, 220, 230]]);
val test9b = 
    chkmodifyi { row=0, col=0, nrows=NONE, ncols=NONE }
      [23, 22, 21, 20, 13, 12, 11, 10, 3, 2, 1, 0]
      (elts [#[0, 10, 20, 30], #[100, 110, 120, 130], #[200, 210, 220, 230]]);
val test9c = 
    chkmodifyi { row=0, col=1, nrows=NONE, ncols=NONE }
      [23, 22, 21, 13, 12, 11, 3, 2, 1]
      (elts [#[0, 10, 20, 30], #[10, 110, 120, 130], #[20, 210, 220, 230]]);
val test9d = 
    chkmodifyi { row=1, col=0, nrows=NONE, ncols=NONE }
      [23, 22, 21, 20, 13, 12, 11, 10]
      (elts [#[0, 1, 2, 3], #[100, 110, 120, 130], #[200, 210, 220, 230]]);
val test9e = 
    chkmodifyi { row=1, col=1, nrows=NONE, ncols=NONE }
      [23, 22, 21, 13, 12, 11]
      (elts [#[0, 1, 2, 3], #[10, 110, 120, 130], #[20, 210, 220, 230]]);
val test9f =  
    chkmodifyi { row=3, col=0, nrows=NONE, ncols=NONE }
      []
      (elts [#[0, 1, 2, 3], #[10, 11, 12, 13], #[20, 21, 22, 23]]);
val test9g =  
    chkmodifyi { row=0, col=4, nrows=NONE, ncols=NONE }
      []
      (elts [#[0, 1, 2, 3], #[10, 11, 12, 13], #[20, 21, 22, 23]]);
val test9h =  
    chkmodifyi { row=1, col=1, nrows=SOME 0, ncols=NONE }
      []
      (elts [#[0, 1, 2, 3], #[10, 11, 12, 13], #[20, 21, 22, 23]]);
val test9i =  
    chkmodifyi { row=1, col=1, nrows=NONE, ncols=SOME 0 }
      []
      (elts [#[0, 1, 2, 3], #[10, 11, 12, 13], #[20, 21, 22, 23]]);
val test9j =  
    chkmodifyi { row=1, col=1, nrows=SOME 1, ncols=NONE }
      [13, 12, 11]
      (elts [#[0, 1, 2, 3], #[10, 110, 120, 130], #[20, 21, 22, 23]]);
val test9k =  
    chkmodifyi { row=1, col=1, nrows=NONE, ncols=SOME 1 }
      [21, 11]
      (elts [#[0, 1, 2, 3], #[10, 110, 12, 13], #[20, 210, 22, 23]]);
val test9l =  
    chkmodifyi { row=0, col=1, nrows=SOME 2, ncols=SOME 2 }
      [12, 11, 2, 1]
      (elts [#[0, 10, 20, 3], #[10, 110, 120, 13], #[20, 21, 22, 23]]);

fun chkfold traversal resseq =
    check'(fn _ =>
	   let val a3 = tabulate RowMajor (3, 4, fill)
	       val sequence = 
		   fold traversal (fn (a, res) => a :: res) [] a3
	   in sequence = resseq end)

fun chkfoldi traversal { row, col, nrows, ncols } resseq =
    check'(fn _ =>
	   let val a3 = tabulate RowMajor (3, 4, fill)
	       fun chkidx i j a = 
		   if fill(i, j) = a then () 
		   else raise Fail ("chkfoldi: Error in " ^ Int.toString a)
	       val sequence = 
		   foldi traversal 
		   (fn (i, j, a, res) => (chkidx i j a; a :: res)) [] 
		   { base=a3, row=row, col=col, nrows=nrows, ncols=ncols}
	   in sequence = resseq end)

val test10a1 = 
    chkfold RowMajor
      [23, 22, 21, 20, 13, 12, 11, 10, 3, 2, 1, 0];
val test10a2 = 
    chkfold ColMajor
      [23, 13, 3, 22, 12, 2, 21, 11, 1, 20, 10, 0];
val test10b1 = 
    chkfoldi RowMajor { row=0, col=0, nrows=NONE, ncols=NONE }
      [23, 22, 21, 20, 13, 12, 11, 10, 3, 2, 1, 0];
val test10b2 = 
    chkfoldi ColMajor { row=0, col=0, nrows=NONE, ncols=NONE }
      [23, 13, 3, 22, 12, 2, 21, 11, 1, 20, 10, 0];
val test10c1 = 
    chkfoldi RowMajor { row=0, col=1, nrows=NONE, ncols=NONE }
      [23, 22, 21, 13, 12, 11, 3, 2, 1];
val test10c2 = 
    chkfoldi ColMajor { row=0, col=1, nrows=NONE, ncols=NONE }
      [23, 13, 3, 22, 12, 2, 21, 11, 1];
val test10d1 = 
    chkfoldi RowMajor { row=1, col=0, nrows=NONE, ncols=NONE }
      [23, 22, 21, 20, 13, 12, 11, 10];
val test10d2 = 
    chkfoldi ColMajor { row=1, col=0, nrows=NONE, ncols=NONE }
      [23, 13, 22, 12, 21, 11, 20, 10];
val test10e1 = 
    chkfoldi RowMajor { row=1, col=1, nrows=NONE, ncols=NONE }
      [23, 22, 21, 13, 12, 11];
val test10e2 = 
    chkfoldi ColMajor { row=1, col=1, nrows=NONE, ncols=NONE }
      [23, 13, 22, 12, 21, 11];
val test10f1 =  
    chkfoldi RowMajor { row=3, col=0, nrows=NONE, ncols=NONE }
      [];
val test10f2 =  
    chkfoldi ColMajor { row=3, col=0, nrows=NONE, ncols=NONE }
      [];
val test10g1 =  
    chkfoldi RowMajor { row=0, col=4, nrows=NONE, ncols=NONE }
      [];
val test10g2 =  
    chkfoldi ColMajor { row=0, col=4, nrows=NONE, ncols=NONE }
      [];
val test10h1 =  
    chkfoldi RowMajor { row=1, col=1, nrows=SOME 0, ncols=NONE }
      [];
val test10h2 =  
    chkfoldi ColMajor { row=1, col=1, nrows=SOME 0, ncols=NONE }
      [];
val test10i1 =  
    chkfoldi RowMajor { row=1, col=1, nrows=NONE, ncols=SOME 0 }
      [];
val test10i2 =  
    chkfoldi ColMajor { row=1, col=1, nrows=NONE, ncols=SOME 0 }
      [];
val test10j1 =  
    chkfoldi RowMajor { row=1, col=1, nrows=SOME 1, ncols=NONE }
      [13, 12, 11];
val test10j2 =  
    chkfoldi ColMajor { row=1, col=1, nrows=SOME 1, ncols=NONE }
      [13, 12, 11];
val test10k1 =  
    chkfoldi RowMajor { row=1, col=1, nrows=NONE, ncols=SOME 1 }
      [21, 11];
val test10k2 =  
    chkfoldi ColMajor { row=1, col=1, nrows=NONE, ncols=SOME 1 }
      [21, 11];
val test10l1 =  
    chkfoldi RowMajor { row=0, col=1, nrows=SOME 2, ncols=SOME 2 }
      [12, 11, 2, 1];
val test10l2 =  
    chkfoldi ColMajor { row=0, col=1, nrows=SOME 2, ncols=SOME 2 }
      [12, 2, 11, 1];

fun faili { row, col, nrows, ncols } = 
    let val reg = {base=a1, row=row, col=col, nrows=nrows, ncols=ncols}
    in
	(appi RowMajor ignore reg seq "WRONG") 
	handle Subscript => 
	(appi ColMajor ignore reg seq "WRONG") 
	handle Subscript => 
	    (modifyi RowMajor (fn (_, _, a) => a) reg seq "WRONG")
	    handle Subscript => 
	    (modifyi ColMajor (fn (_, _, a) => a) reg seq "WRONG") 
	    handle Subscript => 
		(foldi RowMajor (fn _ => 1) 0 reg seq "WRONG") 
		handle Subscript => 
		    (foldi ColMajor (fn _ => 1) 0 reg seq "WRONG") 
		    handle Subscript => "OK"
			 | _ => "WRONG"
    end

val test11a = faili {row=0,   col=0,   nrows=NONE,   ncols=SOME 5 }
val test11b = faili {row=0,   col=0,   nrows=SOME 4, ncols=NONE   }
val test11c = faili {row= ~1, col=0,   nrows=NONE,   ncols=NONE   }
val test11d = faili {row=0,   col= ~1, nrows=NONE,   ncols=NONE   }
val test11e = faili {row=3,   col=0,   nrows=SOME 1, ncols=NONE   }
val test11f = faili {row=0,   col=4,   nrows=NONE,   ncols=SOME 1 }
val test11g = faili {row=0,   col=1,   nrows=NONE,   ncols=SOME 4 }
val test11h = faili {row=1,   col=0,   nrows=SOME 3, ncols=NONE   }

fun chkfromlist xss =
    check'(fn _ =>
	   let val a        = fromList xss
	       val elements = 
		   List.tabulate(nRows a, 
				 fn i => Vector.foldr (op::) [] (row(a, i)))
	   in elements = xss end)

val test12a = chkfromlist []
val test12b = chkfromlist [[]]
val test12c = chkfromlist [[], [], []]
val test12d = chkfromlist [[0]]
val test12e = chkfromlist [[0, 1]]
val test12f = chkfromlist [[0], [10]]
val test12g = chkfromlist [[0, 1, 2], [10, 11, 12]]
val test12h = chkfromlist [[0, 1, 2], [10, 11, 12], [20, 21, 22]]
val test12i = (fromList [[], [2]] seq "WRONG")
              handle Size => "OK" | _ => "WRONG"
val test12j = (fromList [[2], []] seq "WRONG")
              handle Size => "OK" | _ => "WRONG"
val test12k = (fromList [[1, 2, 3], [4, 5, 6], [7, 8]] seq "WRONG")
              handle Size => "OK" | _ => "WRONG"
val test12l = (fromList [[1, 2, 3], [7, 8], [4, 5, 6]] seq "WRONG")
              handle Size => "OK" | _ => "WRONG"

end
