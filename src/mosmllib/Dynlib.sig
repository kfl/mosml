(* Dynlib -- dynamic linking with foreign functions *)

type dlHandle
type symHandle
    
exception Closed

datatype flag = RTLD_LAZY | RTLD_NOW
val dlopen  : { lib : string, flag : flag, global : bool } -> dlHandle
val dlsym   : dlHandle -> string -> symHandle
val dlclose : dlHandle -> unit

val var  : symHandle -> 'b                            
val app1 : symHandle -> 'a1 -> 'b                     
val app2 : symHandle -> 'a1 -> 'a2 -> 'b              
val app3 : symHandle -> 'a1 -> 'a2 -> 'a3 -> 'b       
val app4 : symHandle -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b
val app5 : symHandle -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b

(* 
   Structure Dynlib provides dynamic loading and calling of C
   functions, using the dlfcn interface.  A dynamic library is a
   collection of symbols (C variables and functions).  

   An ML value passed to or returned from a symbol has type `value' as
   defined in src/runtime/mlvalues.h.  The C functions should use the
   macroes defined there to access and produce ML values.  When
   writing a C function, remember that the garbage collector may be
   activated whenever you allocate an ML value.  Also, remember that
   the garbage collector may move values from the young heap to the
   old one, so that a C pointer pointing into the ML heap may need to
   be updated. Use the Push_roots and Pop_roots macroes to achieve
   this.

   [dlHandle] is the type of dynamic library handles.  A dynamic
   library handle is created by opening a dynamic library using
   dlopen.  This will load the library into the runtime system.  The
   dynamic library handle is used for accessing symbols in that
   library.  The library may be closed and removed from the runtime
   system using dlclose.

   The same library may be opened more than once, resulting in
   different library handles.  The physical library will be loaded
   only once, though, and will remain in the runtime system until all
   handles to the library have been closed.

   [symHandle] is the type of symbol handles.  A symbol handle is used
   to access a symbol (variable or function) in the dynamic library,
   using the functions var, app1, app2, ..., app5.  Type safety is the
   responsibility of the programmer; the runtime system performs no
   type checking.  Hence you are advised to add explicit types
   whenever you define an ML function in terms of var, app1, ..., app5.

   How to create a dynamically loadable library
   --------------------------------------------
   Assume file "xyz.c" contains your C functions.  

   To compile xyz.c into xyz.o and then create a dynamic library 
   libxyz.so from xyz.o:

     Under Linux and OSF/1 (Digital Unix):
        gcc -c -o xyz.o xyz.c 
        ld -shared -o libxyz.so xyz.o
     Under Solaris (ignore the warnings from ld):
        gcc -c -o xyz.o xyz.c 
        ld -G -B symbolic -z nodefs -o libxyz.so xyz.o
     Under HP-UX:
        gcc -fPIC -c -o xyz.o xyz.c 
        ld -b -B symbolic -E -o libxyz.so xyz.o

   If "xyz.o" depends on another library "libabc.a" you may link the
   required functions into libxyz.so just by adding -labc or libabc.a
   to the above linker command.

   If "xyz.o" depends on another dynamic library "libabc.so" you may
   specify this by adding -labc to the above linker command.  Then
   Dynlib.dlopen will automatically load libabc.so before libxyz.so.


   [dlopen { lib, flag, global }] will load and open the library in
   file `lib', returning a handle to it.  Libraries are usually
   specified just by file name, leaving out the directory path.
   Linux/Unix-specific information: Libraries are searched for in
   those directories mentioned in LD_LIBRARY_PATH, those mentioned in
   /etc/ld.so.cache, in /usr/lib and /lib.  (Note that
   /etc/ld.so.cache is created from /etc/ld.so.conf by running
   ldconfig; you must be superuser to do that).
        If `global' is true, then the library's global symbols are
   made available for other libraries subsequently loaded.

   [flag] is the type of library loading modes: RTLD_LAZY and RTLD_NOW.  

   [RTLD_LAZY] specifies that only symbol relocations will be
   performed when calling dlopen, whereas function relocations will be
   performed later when a function is invoked for the first time (if
   ever).  This is the normal situation.

   [RTLD_NOW] specifies that all function relocations must be
   performed immediately, also for functions that will never be
   called.  This checks that all functions are defined, but may waste
   some time.

   [dlsym dlh nam] returns a symbol handle for the symbol called `nam'
   in the library associated with dlh.  Raises Closed if dlh has been
   closed.

   [dlclose dlh] closes the library handle and deallocates the library
   if there are no more open handles to this library.

   The following functions raise Closed if the associated handle has
   been closed.

   [var sym] returns the value of the C variable associated with sym.

   [app1 sym arg1] applies the C function associated with sym to arg1.

   [app2 sym arg1 arg2] applies the C function associated with sym to
   (arg1, arg2).

   [app3 sym arg1 arg2 arg3] applies the C function associated with
   sym to (arg1, arg2, arg3).

   [app4 sym arg1 arg2 arg3 arg4] applies the C function associated
   with sym to (arg1, arg2, arg3, arg4).

   [app5 sym arg1 arg2 arg3 arg4 arg5] applies the C function
   associated with sym to (arg1, arg2, arg3, arg4, arg5). 
*)
