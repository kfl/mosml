(* test/callback.sml
   sestoft@dina.kvl.dk 1999-08-09
*)

use "auxil.sml";

load "Callback";

local
    open Callback
	
in

val test1a = check'
    (fn _ => (register "mkstr" makestring; isRegistered "mkstr"));

val test1b = (register "mkstr" abs; "WRONG")
             handle Fail _ => "OK" | _ => "WRONG";

val test1c = check'
    (fn _ => (unregister "mkstr"; not (isRegistered "mkstr")));

val test1d = check'
    (fn _ => (unregister "mkstr"; register "mkstr" abs; isRegistered "mkstr"));

val test2a = check'
    (fn _ => isRegistered "Callback.register"
             andalso isRegistered "Callback.unregister")

val test3a = (getcptr "foo"; "WRONG")
             handle Fail _ => "OK" | _ => "WRONG";

end
