(* Signal -- Standard ML Basis Library *)

type signal = int

val hup  =  1
val int  =  2 
val quit =  3
val ill  =  4
val abrt =  6
val bus  =  7
val fpe  =  8
val kill =  9
val usr1 = 10
val segv = 11
val usr2 = 12
val pipe = 13
val alrm = 14
val term = 15
val chld = 17
val cont = 18
val stop = 19
val tstp = 20
val ttin = 21
val ttou = 22

val toWord   = Word.fromInt
val fromWord = Word.toInt
