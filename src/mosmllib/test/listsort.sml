(* File "test/listsort.sml" * 1995, 2001-10-21  *)

app load ["Listsort", "Random", "Real"];
use "auxil.sml";

local
    open Listsort
    val r_unsort = Random.randomlist (10000, Random.newgen ());
    val r_sort = Listsort.sort Real.compare r_unsort;
    val r_sortedsort = 
	Listsort.sorted Real.compare o Listsort.sort Real.compare
    val i_unsort = Random.rangelist (0, 10000) (10000, Random.newgen ());
    val i_sort = Listsort.sort Int.compare i_unsort;
    val i_sortedsort = 
	Listsort.sorted Int.compare o Listsort.sort Int.compare
in
    val test1r = 
	check'(fn _ =>
	       not (Listsort.sorted Real.compare r_unsort
		    orelse Listsort.sorted Real.compare [2.1, 1.0]));

    val test1i = 
	check'(fn _ =>
	       not (Listsort.sorted Int.compare i_unsort
		    orelse Listsort.sorted Int.compare [2, 1]));
	
    val test2r = 
	check'(fn _ =>
	       Listsort.sorted Real.compare r_sort
	       andalso Listsort.sorted Real.compare []
	       andalso Listsort.sorted Real.compare [1.0, 2.1]);

    val test2i = 
	check'(fn _ =>
	       Listsort.sorted Int.compare i_sort
	       andalso Listsort.sorted Int.compare []
	       andalso Listsort.sorted Int.compare [1, 2]);

    val test3r = 
	check'(fn _ =>
	       r_sortedsort []
	       andalso r_sortedsort [1.0]
	       andalso r_sortedsort [1.0, 1.0]
	       andalso r_sortedsort [1.0, 2.0]
	       andalso r_sortedsort [2.0, 1.0]
	       andalso r_sortedsort [3.0, 2.0, 1.0]
	       andalso r_sortedsort [2.0, 3.0, 1.0]
	       andalso r_sortedsort [2.0, 1.0, 3.0]);

    val test3i = 
	check'(fn _ =>
	       i_sortedsort []
	       andalso i_sortedsort [1]
	       andalso i_sortedsort [1, 1]
	       andalso i_sortedsort [1, 2]
	       andalso i_sortedsort [2, 1]
	       andalso i_sortedsort [3, 2, 1]
	       andalso i_sortedsort [2, 3, 1]
	       andalso i_sortedsort [2, 1, 3]);

    (* merge *)

    fun double []       = []
      | double (x1::xr) = x1 :: x1 :: double xr

    val test4 = 
	check'(fn _ =>
	       merge Int.compare (i_sort, i_sort) = double i_sort 
	       andalso merge Int.compare ([], i_sort) = i_sort
	       andalso merge Int.compare (i_sort, []) = i_sort
	       andalso merge Int.compare ([1,3,5], [2,4,6]) = [1,2,3,4,5,6]
	       andalso merge Int.compare ([2,4,6], [1,3,5]) = [1,2,3,4,5,6]
	       andalso merge Int.compare ([1,4,5], [2,4,6]) = [1,2,4,4,5,6]
	       andalso merge Int.compare ([2,3,6], [1,3,5]) = [1,2,3,3,5,6]
	       andalso merge Int.compare ([4,6], [1,3]) = [1,3,4,6]
	       andalso merge Int.compare ([1,3], [4,6]) = [1,3,4,6]
	       andalso merge Int.compare ([4,6], [1,3,4]) = [1,3,4,4,6]
	       andalso merge Int.compare ([1,3,4], [4,6]) = [1,3,4,4,6])

    (* mergeUniq *)

    val test5 = 
	check'(fn _ =>
	       mergeUniq Int.compare (i_sort, i_sort) = i_sort
	       andalso mergeUniq Int.compare ([], i_sort) = i_sort
	       andalso mergeUniq Int.compare (i_sort, []) = i_sort
	       andalso mergeUniq Int.compare ([1,3,5], [2,4,6]) = [1,2,3,4,5,6]
	       andalso mergeUniq Int.compare ([2,4,6], [1,3,5]) = [1,2,3,4,5,6]
	       andalso mergeUniq Int.compare ([1,4,5], [2,4,6]) = [1,2,4,5,6]
	       andalso mergeUniq Int.compare ([2,3,6], [1,3,5]) = [1,2,3,5,6]
	       andalso mergeUniq Int.compare ([4,6], [1,3]) = [1,3,4,6]
	       andalso mergeUniq Int.compare ([1,3], [4,6]) = [1,3,4,6]
	       andalso mergeUniq Int.compare ([4,6], [1,3,4]) = [1,3,4,6]
	       andalso mergeUniq Int.compare ([1,3,4], [4,6]) = [1,3,4,6])

    (* eqclasses *)

    fun inteqccheck ordr arg res = 
	List.map (Listsort.sort Int.compare) (Listsort.eqclasses ordr arg)
	= List.map (Listsort.sort Int.compare) res;

    fun f (arg, res) = 
	inteqccheck (fn (x, y) => Int.compare(Int.abs x, Int.abs y)) arg res;

    val test6 = 
	check'(fn _ =>
	       f([], [])
	       andalso f([1], [[1]])
	       andalso f([1, ~1], [[1,~1]])
	       andalso f([1, 2, ~1], [[1,~1],[2]])
	       andalso f([5, 1, 2, ~1, ~1, 3, ~0, 7, 1, 5, 5, ~2,0],
			 [[~0,0],[1,~1,~1,1],[2,~2],[3],[5,5,5],[7]]));
end
