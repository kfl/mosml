(* File mosml/examples/weak: hash consing with weak pointers *)

app load ["Int", "Weak", "Polyhash"];

datatype 'a hashpair =
    Nil
  | Cons of int * 'a hashpair * 'a hashpair

local 
    fun hashNode Nil             = 0
      | hashNode (Cons(h, _, _)) = h

    fun hashVal (a, b) = hashNode a + hashNode b

    val table = Polyhash.mkTable (hashVal, op=) (37, Fail "Not found");

    val count = ref 1

    fun report msg node = 
	print (msg ^ " node " ^ Int.toString (hashNode node) ^ "\n")

    fun newpair a b = 
	let val newnode  = Cons(!count, a, b)
	    val newentry = Weak.weak newnode
	in
	    Polyhash.insert table ((a, b), newentry);
	    report "Creating new" newnode;
	    count := !count + 1;	    
	    newnode
	end
in

fun cons a b : string hashpair = 
    case Polyhash.peek table (a, b) of
	SOME w => (let val node = Weak.get w
		   in report "Reusing" node; node end
		   handle Fail _ => (print "Old node had died. "; newpair a b))
      | NONE   => newpair a b
end;

(* One scenario: *)

fun try () = 
    let (* Function f just exercises the garbage collector: *)
	fun f 0 = [] | f n = n :: f (n-1)
	fun pairthem c = (cons c c ; ())
	val v1 = Nil
	val _ = print "\n[Build new node]\n";
	val v2 = cons v1 v1
	val _ = print "[Reuse that node]\n";
	val v3 = cons v1 v1
	val _ = print "[Build new node]\n";
	val v4 = cons v2 v1
	val _ = print "[Build new node]\n";
	val v5 = cons v1 v2
	val _ = print "[First application builds a new node; the rest reuse it]\n";
	val a = List.tabulate(5, fn i => cons v4 v3)
	val _ = print "[Build a new (v5, v5) node]\n";
	val _  = pairthem v5 
	val _ = print "[Cause the garbage collector to run]\n";
	val _ = f 100000
	val _ = print "[The old (v5, v5) node may have been deallocated]\n";
	val v6 = cons v5 v5
	val _ = print "\n";
    in v6 end

val _ = try ();
