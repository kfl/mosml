(* To read a file word per word, and return the list of the strings read *)

local

  open BasicIO Nonstdio;

fun from_stream is =
  let val buff = CharArray.array(1024, #" ")
      fun readchars i =
        case input_char is of
            #" " => i
          | #"\n" => i
          | #"\r" => i (* was #"\^M" *)
          | #"\t" => i
          | c =>
             (if i < CharArray.length buff then CharArray.update(buff, i, c)
              else ();
              readchars (i+1))
      fun readword() =
        case input_char is of
            #" "    => readword()
          | #"\n"   => readword()
          | #"\r"   => readword() (* was #"\^M" *)
          | #"\t"   => readword()
          | c =>
              (CharArray.update(buff, 0, c);
               CharArray.extract(buff, 0, SOME (readchars 1)))
      fun readwords l =
        (readwords(readword() :: l))
           handle Size => List.rev l
  in
    readwords []
  end;

in

fun from_file filename =
  let val is = open_in filename
      val res = from_stream is
  in
    close_in is;
    res
  end;

end;
