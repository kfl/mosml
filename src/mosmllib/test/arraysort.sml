(* File "test/arraysort.sml"  *)

load "Arraysort";
load "Random";
load "Real";

val a = Array.fromList (Random.randomlist (10000, Random.newgen ()));

val test1 = 
    not (Arraysort.sorted Real.compare a
	 orelse Arraysort.sorted Real.compare (Array.fromList [2.1, 1.0]));

val _ = Arraysort.sort Real.compare a;

val test2 = Arraysort.sorted Real.compare a
    andalso Arraysort.sorted Real.compare (Array.fromList [])
    andalso Arraysort.sorted Real.compare (Array.fromList [1.0, 2.1]);
