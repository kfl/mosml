(* mosml/src/dynlibs/crypt/crypt.sml -- foreign function interface
   The SML side of things: how to load and call the C function.  *)

(* 1. Load the Dynlib and FileSys structures from the Moscow ML library: *)

app load ["Dynlib", "FileSys"];

(* 2. Load the C dynamic library libcrypt.so, and get a handle to it.
      Use the absolute path because libcrypt.so is not a system DLL:     *)

val dlh = Dynlib.dlopen { lib = Path.concat(FileSys.getDir (), "libcrypt.so"),
			  flag = Dynlib.RTLD_LAZY, 
			  global = false }

(* 3. Get a handle to the C function ml_crypt defined in libcrypt.so:    *)

val crypthandle = Dynlib.dlsym dlh "ml_crypt";

(* 4. Define an SML function using this handle and a fixed salt.  
      The type ascription is necessary for SML type safety:              *)

fun crypt (key : string) : string = 
    Dynlib.app2 crypthandle key "AB";

val _ = print "\nNow invoke the SML crypt function: crypt \"blah\" ...\n\n";
