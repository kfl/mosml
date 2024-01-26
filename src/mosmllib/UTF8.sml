structure UTF8 :> UTF8 = struct

type ('a, 'b) reader = 'b -> ('a * 'b) option;

exception BadUTF8 of string;

(* A lookup table: The n'th entry gives the number of octets that
   follow in a well-formed UTF8 encoding beginning with the octet n +
   0x7F, or 4 if the value is a continuation octet, or 0 if the value
   is not a valid octet of a UTF representation. These conditions are
   from Table 9.3 of the Third edition of the UCS standard ISO/IEC
   10646:2009 *)

fun countf n' = 
   let val n = n' + 0x80
       val r =      if n > 0xF4 then 0
               else if n > 0xEF then 3
               else if n > 0xDF then 2
               else if n > 0xC1 then 1
               else if n > 0xBF then 0
               else 4
   in Word8.fromInt r
   end;

(* A lookup table: The n'th entry gives the 4 high bits of each of the
   lower and upper bounds of a valid UTF8 continuation octet (i.e. the
   2nd octet of a multi-octet UTF8 representation) beginning with the
   octet n + 0x7F. These values are taken from Table 9.3 of the Third
   edition of the UCS standard ISO/IEC 10646:2009 *)

fun validf n' =
   let val n = n' + 0x80
       val r =
           if n = 0xE0 then 0xAB
      else if n = 0xED then 0x89
      else if n = 0xF0 then 0x9F
      else if n = 0xF4 then 0x88
      else 0x8B
    in Word8.fromInt r
    end;

val count = Word8Vector.tabulate (0x80,countf);
val valid = Word8Vector.tabulate (0x80,validf);

fun size s =
   let val len = String.size s
       fun errmsg n cn = ("Invalid UTF8 sequence at octet "^(Int.toString n)^
                          " (code 0x"^(Int.fmt StringCvt.HEX cn)^")")
       fun iter sz n = 
              if n < len
                 then let val cn = Char.ord (CharVector.sub (s,n))
                      in if cn < 0x80
                            then iter (sz+1) (n+1)
                            else case Word8.toInt (Word8Vector.sub (count, cn - 0x80))
                                   of 1  => iter (sz+1) (n+2)
                                    | 2  => iter (sz+1) (n+3)
                                    | 3  => iter (sz+1) (n+4)
                                    | _ => raise BadUTF8 (errmsg n cn)
                      end
                 else sz
   in iter 0 0
   end

fun size_ s = size s handle BadUTF8 _ => String.size s 

local
   fun pad c n s =
      let val len = size s
      in CharVector.tabulate (Int.max(n - len, 0), fn _ => c)
      end
in
   fun padLeft c n s = (pad c n s)^s
   fun padRight c n s = s^(pad c n s)
end

type state = {stateno : int, seqno : int, fcno : int, chars : string, ucsno : Word.word};

(* A state machine for decoding UTF8 octet sequences. *)

datatype transition = Process of Char.char -> transition
                    | Value of string * Word.word
                    | Error of string;

fun transition state =
   let fun continue (state : state) c n = 
        let val ucsno' = Word.orb(Word.<<(#ucsno state,0w6), Word.andb(Word.fromInt n,0wx3F))
            val chars' = (#chars state)^(String.str c)
        in if #seqno state = 1
              then Value (chars', ucsno')
              else Process (transition {stateno = 3, seqno = (#seqno state) - 1,
                                        fcno = n, chars = chars', ucsno = ucsno'})
        end
       fun newstate c =
           let val n = Char.ord c
           in case #stateno state
                of 1 => if n < 0x80
                           then Value (String.str c, Word.fromInt n)
                           else let val sl = Word8.toInt (Word8Vector.sub (count, n - 0x80))
                                in if sl < 1 orelse sl > 3
                                      then Error "Invalid UTF8 initial octet"
                                      else Process (transition 
                                                      {stateno = 2, seqno = sl, fcno = n,
                                                       chars = String.str c,
                                                       ucsno = Word.andb(Word.fromInt n,
                                                                    Word.<<(0w1,0w6-Word.fromInt sl)-0w1)})
                                end
                 | 2 => let val r = (Word8Vector.sub (valid, #fcno state - 0x80))
                            val l = Word8.toInt (Word8.andb(r, 0wxF0))
                            val h = Word8.toInt (Word8.orb(Word8.<<(Word8.andb(r, 0wx0F), 0w4),0wx0F))
                        in if n < l orelse n > h
                              then Error "Invalid UTF8 continuation octet" 
                              else continue state c n 
                        end
                 | 3 => if n < 0x80 orelse n > 0xBF
                           then Error "Invalid UTF8 trailing octet"
                           else continue state c n 
                 | _ => raise Fail "Internal error: UTF8.transition: invalid state"
           end
   in newstate
   end;

fun scanUTF8UCS rvf (getc : (char, 'a) reader) =
   let val start = Process (transition {stateno = 1, seqno = 0, fcno = 0, chars = "", ucsno = 0wx000000});
       fun recur st (css : 'a) =
             (case st
                of Process f => 
                       (case getc css
                          of NONE => NONE
                           | SOME(c,css) => recur (f c) css)
                     | Value rv => SOME (rvf rv,css)
                     | Error s => raise (BadUTF8 s))
   in fn css => recur start css
   end;

val scanUCS = scanUTF8UCS (fn (_,w) => w);

val scanUTF8Transition = scanUTF8UCS (fn (s,_) => s);

val UCSfromUTF8String =
   StringCvt.scanString scanUCS;

fun UCStoUTF8String cp =
  let
    fun storeString cp =
       let 
           fun store_bits acc n cp =
               let val mask = Word8.<<(0w1,n)-0w1
                   val topbits = Word8.~>>(0wx80,0w6-n)
                   val c = topbits + Word8.andb(Word8.fromLargeWord cp,mask)
               in ((String.str (Char.chr (Word8.toInt c)))^acc, (Word.>>(cp,n)))
               end
           fun continue n (acc,cp) =
               if n < 0w6
                  then store_bits acc n cp 
                  else continue (n - 0w6) (store_bits acc 0w6 cp)
        in
           if cp > 0wx10FFFF then raise BadUTF8 "Invalid UCS scalar value (too large)"
              else if cp >= 0wxD800 andalso cp <= 0wxDFFF then 
                        raise BadUTF8 "Invalid UCS value (surrogate code point range)"
              else if cp > 0wx00FFFF then continue 0w21 ("",cp)
              else if cp > 0wx0007FF then continue 0w16 ("",cp)
              else if cp > 0wx00007F then continue 0w11 ("",cp)
              else (String.str(Char.chr (Word.toInt cp)),0w0)
        end
     val (cs,_) = storeString cp 
  in
     cs
  end;

(* A UTF8 character scanner suitable for plugging into a String.string
   or TextIO.instream scanning function such as scanString or
   scanStream in the StringCvt and TextIO structures of the Standard
   ML basis library. Alternatively, this function can be applied to
   Substring.getc to produce a UTF8 reader for values of type
   Substring.substring. *)

fun scanUTF8 (getc : (char, 'a) reader) =
       (fn (cs) => 
          let val c = getc cs
          in
             case c of
                NONE => NONE
              | SOME (c1,cs) =>
                let val n = Char.ord c1
                in if n < 0x80 then SOME(String.str c1,cs)
                   else let open Word8Vector
                            val sl = Word8.toInt (sub (count,n - 0x80))
                            val r = sub (valid, n - 0x80)
                        in if sl < 1 orelse sl > 3 then raise BadUTF8 "Invalid UTF8 continuation octet."
                           else let val c = getc cs 
                                in case c
                                     of NONE => NONE
                                      | SOME (c2,cs) =>
                                           let val chrs = (String.str c1)^(String.str c2)
                                               val n = Char.ord c2
                                               val l = Word8.toInt (Word8.andb(r, 0wxF0))
                                               val h = Word8.toInt (Word8.orb(Word8.<<(Word8.andb(r, 0wx0F),
                                                                                       0w4),0wx0F))
                                           in if n < l orelse n > h then 
                                                 raise BadUTF8  "Invalid UTF8 continuation octet."
                                              else if sl = 1 then SOME(chrs, cs)
                                              else let val c = getc cs 
                                                   in case c
                                                        of NONE => NONE
                                                         | SOME (c3,cs) =>
                                                              let val chrs = chrs^(String.str c3)
                                                                  val n = Char.ord c3
                                                              in if n < 0x80 orelse n > 0xBF
                                                                    then raise BadUTF8 
                                                                         "Invalid UTF8 continuation octet."
                                                                    else if sl = 2 then SOME(chrs, cs)
                                                                    else let val c = getc cs 
                                                                         in case c
                                                                              of NONE => NONE
                                                                               | SOME (c4,cs) =>
                                                                                   let val chrs = 
                                                                                             chrs^(String.str c4)
                                                                                       val n = Char.ord c4
                                                                                   in if n < 0x80 
                                                                                           orelse n > 0xBF
                                                                                         then raise BadUTF8
                                                                          "Invalid UTF8 continuation octet."
                                                                                         else SOME(chrs,cs)
                                                                                   end
                                                                         end
                                                              end
                                                   end
                                           end
                                end
                        end
                end
          end);

val UTF8fromUTF8string = StringCvt.scanString scanUTF8;

end (* struct *)
