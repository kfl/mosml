(* File test/test.sml, main file for Standard Library test cases *)
(* Moscow ML Unix version *)

(* open OS; *)                          (* MOSML *)

app use
["array.sml",
 "array2.sml",
 "arraysort.sml",                       (* MOSML *)
 "bytecmac.sml",   (* "bytechar.sml" *)
 "date.sml",
 "filesmac.sml",   (* "filesys.sml"  *)
 "generalmac.sml", (* "general.sml"  *)
 "int.sml",
 "list.sml",
 "listpair.sml",
 "listsort.sml",                        (* MOSML *)
 "math.sml",
 "mosml.sml",                           (* MOSML *)
 "real.sml",
 "stringmac.sml",  (* "string.sml" *)
 "stringcvt.sml",
 "substring.sml",
 "textio.sml",
 "time.sml",
 "timer.sml",
 "macpath.sml",    (* "unixpath.sml", *)
 "vector.sml",
 "weak.sml",                            (* MOSML *)
 "word.sml",
 "word8.sml",
 "word8array.sml",
 "word8vector.sml"];

(* ignore(Process.exit Process.success); *)

