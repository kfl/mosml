(* Callback -- registering ML values with C, and accessing C values from ML *)

(* Registering ML values for access from C code: *)

val register     : string -> 'a -> unit
val unregister   : string -> unit
val isRegistered : string -> bool


(* Accessing C variables and functions from ML: *)

type cptr

val getcptr : string -> cptr
val var     : cptr -> 'b                            
val app1    : cptr -> 'a1 -> 'b                     
val app2    : cptr -> 'a1 -> 'a2 -> 'b              
val app3    : cptr -> 'a1 -> 'a2 -> 'a3 -> 'b       
val app4    : cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b
val app5    : cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b

(* 
   REGISTERING ML VALUES FOR ACCESS FROM C CODE
   --------------------------------------------

   This example shows how to register the ML function (fn n => 2*n) so
   that it may be called from C code.

   (0) The ML side registers the function:
          Callback.register "myfun" (fn n => 2*n)               

   (1) The C side first obtains an ML value pointer:
          valueptr mvp = get_valueptr("myfun");
   
   (2) The C side then uses the ML value pointer to obtain an ML
       value, and uses it:
          callback(get_value(mvp), Val_long(42));

   Operation (1) involves a callback to ML, and hence may be slow.
   Calling get_valueptr may cause the garbage collector to run; hence
   other live ML values must be registered as GC roots.  The garbage
   collector will never move the ML value pointer; hence it need not
   be registered as a GC root in the C code.

   Operation (2) is very fast.  If the garbage collector is invoked
   between the call of get_value() and the use of the ML value, then 
   the value must be registered as a GC root.  However, the idiom
        callback(get_value(mvp), arg1);
   is safe provided the evaluation of arg1 does not provoke a garbage
   collection (e.g. if arg1 is a variable).

   The C function get_valueptr returns NULL if nam is not registered.

   The C function get_value returns NULL if nam has been unregistered
   (and not reregistered) since mvp was obtained; it raises exception
   Fail if mvp itself is NULL.  Every access to the ML value from C
   code should use the ML valueptr and get_valueptr, otherwise the C
   code will not know when the value has been unregistered and
   possibly deallocated.

   The C functions (in mosml/src/runtime/callback.c)
      void registervalue(char* nam, value mlval)
      void unregistervalue(char* nam)
   can be used just as Callback.register and Callback.unregister.

   The C functions
      value callbackptr (valueptr mvp, value arg1)
      value callbackptr2(valueptr mvp, value arg1, value arg2)
      value callbackptr3(valueptr mvp, value arg1, value arg2, value arg3)
   can be used for callback via an ML value pointer; they will raise
   exception Fail if the ML function indicated by mvp has been unregistered.


   [register nam v] registers the ML value v, so that it can be
   retrieved from C code under the name nam.  If nam has previously
   been registered and then unregistered, it will be reregistered with
   the new value.  The new value immediately becomes visible to the C
   side, both via get_valueptr nam and via any ML value pointer
   previously obtained for nam.  Raises exception Fail if nam has
   been registered and not yet unregistered.

   [unregister nam] deletes the registration.  This prevents C code
   from obtaining an ML value pointer for nam and from using an ML
   value pointer already obtained (but does not prevent C from
   attempting to use a stored ML value previously obtained with the
   help of the ML value pointer, which is unsafe anyway).  Does
   nothing if nam is already unregistered.  Raises exception Fail
   if nam has never been registered.

   [isRegistered nam] returns true if nam has been registered and not
   yet unregistered.


   ACCESSING REGISTERED C VARIABLES AND FUNCTIONS FROM ML
   ------------------------------------------------------

   This example shows how to register the C function 

      value silly_cfun(value v) 
      { return copy_double(42.42 * Double_val(v)); }

   so that it may be called from ML.

   (0) The C side registers the function:
          registercptr("mycfun", sillycfun);

   (1) The ML side obtains a C pointer and defines an ML function
       via that pointer: 
          val sillycfun = app1 (getcptr "mycfun") : real -> real
       The type ascription is needed to ensure any type safety whatsoever.
       Mistakes in the types will lead to crashes, as usual with C.

   (2) To the ML side, the new ML function is indistinguishable from
       other ML functions
          val result = sillyfun(3.4)

   The C function (in mosml/src/runtime/callback.c)
        void registercptr(char* nam, void* cptr);

   is used to register C pointers for access from ML.  Only pointers
   to static C variables, and C functions, should be registered. There
   is no way to unregister a C pointer.

   [cptr] is the type of pointers to C variables and C functions.

   [getcptr nam] returns a pointer to the C variable or function
   registered (by the C side) under the name nam.  Raises exception
   Fail if the name nam has not been registered.

   [var cptr] returns the value of the C variable associated with cptr.

   [app1 cptr arg1] applies the C function associated with cptr to arg1.

   [app2 cptr arg1 arg2] applies the C function associated with cptr to
   (arg1, arg2).

   [app3 cptr arg1 arg2 arg3] applies the C function associated with
   cptr to (arg1, arg2, arg3).

   [app4 cptr arg1 arg2 arg3 arg4] applies the C function associated
   with cptr to (arg1, arg2, arg3, arg4).

   [app5 cptr arg1 arg2 arg3 arg4 arg5] applies the C function
   associated with cptr to (arg1, arg2, arg3, arg4, arg5). 
*)
