(* File "test/listsort.sml"  *)

load "Listsort";
load "Random";
load "Real";

local
    val a_unsort = Random.randomlist (10000, Random.newgen ());
    val a_sort = Listsort.sort Real.compare a_unsort;
    val sortedsort = 
	Listsort.sorted Real.compare o Listsort.sort Real.compare
in
    val test1 = 
	not (Listsort.sorted Real.compare a_unsort
	     orelse Listsort.sorted Real.compare [2.1, 1.0]);
	
    val test2 = Listsort.sorted Real.compare a_sort
	andalso Listsort.sorted Real.compare []
	andalso Listsort.sorted Real.compare [1.0, 2.1];

    val test3 = sortedsort []
	andalso sortedsort [1.0]
	andalso sortedsort [1.0, 1.0]
	andalso sortedsort [1.0, 2.0]
	andalso sortedsort [2.0, 1.0]
	andalso sortedsort [3.0, 2.0, 1.0]
	andalso sortedsort [2.0, 3.0, 1.0]
	andalso sortedsort [2.0, 1.0, 3.0]
end
