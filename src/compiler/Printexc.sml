(* A catch-all exception handler *)

open Obj BasicIO Nonstdio;

type qualid = {qual:string, id:string};

val exnTag = obj_tag (repr (let exception DUMMY in DUMMY end));

fun errString s = output(std_err, s);

fun f fct arg =
(
  (fct arg)
  handle x =>
    (
    flush_out std_out;
    (case x of
         Out_of_memory =>
           errString "Out of memory"
       | Fail s =>
           (errString "Evaluation failed: "; errString s)
       | Invalid_argument s =>
           (errString "Invalid argument: "; errString s)
       | SysErr(msg, _) =>
           (errString "I/O failure: "; errString msg)
       | x =>
           let val tag = obj_tag (repr x) in
             errString "Uncaught exception ";
             errString (makestring tag); flush_out std_err;
             let val is = open_in_bin (Vector.sub(Miscsys.command_line, 0))
                 val pos_hdr = in_stream_length is - 20
                 val () = seek_in is pos_hdr
                 val size_code = input_binary_int is
                 val size_data = input_binary_int is
                 val size_symb = input_binary_int is
                 val size_debug = input_binary_int is
                 val () = seek_in is (pos_hdr - size_debug - size_symb)
                 val _ = input_value is
                 val _ = input_value is
                 val tag_exn_table =
                       (input_value is : (qualid * int) Vector.vector)
             in
               if tag >= Vector.length tag_exn_table then
                 errString " (never compiled)"
               else
                 let val (q,s) = Vector.sub(tag_exn_table, tag)
                     val {qual, id} = q
                 in
                   errString " ("; errString qual; errString ".";
                   errString id; errString ")"
                 end;
               close_in is
             end
               handle _ => ()
           end
    );
    errString "\n"; flush_out std_err;
    BasicIO.exit 2
    )
);
















