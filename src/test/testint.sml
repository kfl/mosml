
fun showint (i:int) = print (makestring i);
fun showintnl i = (showint i; print "\n");

fun f n =
  let
    fun loop () = (showint n; loop ())
  in
    case n
      of 0 => 0
       | _ => (loop() handle Interrupt => f (n-1))
  end;

print "Enter `f 5;', and then press ^C !\n";
