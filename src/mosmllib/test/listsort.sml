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

    fun inteqccheck ordr arg res = 
	List.map (Listsort.sort Int.compare) (Listsort.eqclasses ordr arg)
	= List.map (Listsort.sort Int.compare) res;

    fun f (arg, res) = 
	inteqccheck (fn (x, y) => Int.compare(Int.abs x, Int.abs y)) arg res;

    val test4 = 
	f([], [])
	andalso f([1], [[1]])
	andalso f([1, ~1], [[1,~1]])
	andalso f([1, 2, ~1], [[1,~1],[2]])
	andalso f([5, 1, 2, ~1, ~1, 3, ~0, 7, 1, 5, 5, ~2,0],
		  [[~0,0],[1,~1,~1,1],[2,~2],[3],[5,5,5],[7]])
end
