(* To load in-core a compiled bytecode phrase, and execute it *)

open List Obj Memory Fnlib Mixture Const Instruct Types;
open Symtable Patch Tr_const Opcodes Buffcode Reloc Emitcode Rtvals;

fun do_code may_free code entrypoint len =
(
  if number_of_globals() >= Vector.length global_data then
    realloc_global_data(number_of_globals())
  else ();
  app
    (fn (n, sc) => setGlobalVal n (translStructuredConst sc))
    (!literal_table);
  literal_table := [];
  let val res =
    interprete may_free code entrypoint len
    handle x =>
      ((case x of
            Interrupt => raise x
          | Toplevel => raise x
          | Impossible _ => raise x
          | Out_of_memory => gc_full_major()
          | _ =>
              ());
       msgIBlock 0;
       errPrompt "Uncaught exception: "; msgEOL(); errPrompt "";
       printVal (trivial_scheme type_exn) (repr x);
       msgEOL();
       msgEBlock();
       raise Toplevel)
  in
    res
  end
);

fun loadZamPhrase (phr : ZamPhrase) =
(
  reloc_reset();
  init_out_code();
  Labels.reset_label_table();
  literal_table := [];
  (* It is essential to emit the initialization code *)
  (* before the function bodies, in order for all Pset_global *)
  (* to appear before all the Pget_global. *)
  let val entrypoint = !out_position
      val () = emit (#kph_inits phr)
      val () = out STOP
      val () = emit (#kph_funcs phr)
      val len = !out_position
      val out_buffer_ = !(magic (!out_buffer) : string ref)
  in
    patch_object out_buffer_ 0 (get_reloc_info());
    do_code (case (#kph_funcs phr) of [] => true | _ => false)
            out_buffer_ entrypoint len
  end
);












