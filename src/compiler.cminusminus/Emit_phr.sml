(* Emitting phrases *)

local
  open CmmAST CmmEmitcode CmmPrint;
in

val abs_out_position = ref 0;

val compiled_phrase_index = ref ([] : Code_dec.compiled_phrase list);

fun start_emit_phrase os = ()

fun emit_phrase os (phr : Instruct.ZamPhrase) = 
    let val funcs = #kph_funcs phr
        fun makeFun instr =
            let val _ = resetTemp()
                val s = List.map emit_zam instr
                val name = getLetName instr
            in  Function{conv = Cmm,
                         name = name,
                         formals = [(bits32,"sp"), (bits32,"accu"),
                                    (bits32,"env"), (bits32,"extra_args")],
                         stmts = s} 
            end
        val cfuns = List.map makeFun funcs
        val pp = ppProgram cfuns
    in  Wpp.toOutStream 74 TextIO.stdOut pp
    end

fun end_emit_phrase
  excRenList valRenList sigStamp mentions os = ()
(*fun start_emit_phrase os =
(
  output_binary_int os 0;
  abs_out_position := 4;
  compiled_phrase_index := []
);

fun emit_phrase os (phr : ZamPhrase) =
(
  reloc_reset();
  init_out_code();
  Labels.reset_label_table();
  if #kph_funcs phr = [] then
    emit (#kph_inits phr)
  else
    (emit (#kph_inits phr);
     emit [Kbranch 0];
     emit (#kph_funcs phr);
     emit [Klabel 0]);
  buff_output os (!out_buffer) 0 (!out_position);
  compiled_phrase_index :=
    { cph_pos   = !abs_out_position,
      cph_len   = !out_position,
      cph_reloc = get_reloc_info(),
      cph_pure  = #kph_is_pure phr}
        :: !compiled_phrase_index;
  abs_out_position := !abs_out_position + !out_position
);

fun end_emit_phrase
  excRenList valRenList sigStamp mentions os =
(
  output_value os
    { cu_phrase_index = !compiled_phrase_index,
      cu_exc_ren_list = excRenList,
      cu_val_ren_list = valRenList,
      cu_sig_stamp = sigStamp,
      cu_mentions = mentions };
  compiled_phrase_index := [];
  seek_out os 0;
  output_binary_int os (!abs_out_position)
);
*)

end;
