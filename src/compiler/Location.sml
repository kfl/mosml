(* Printing a location in the source program *)

open BasicIO Nonstdio Lexing Parsing Fnlib Config Mixture;

datatype Location =
    Loc of int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
;

val input_name = ref ""              (* Input file name *)
and input_stream = ref std_in        (* Current input channel *)
and input_lexbuf =                   (* Current lexbuf *)
                 ref(createLexer(fn s => fn n => 0))
;

val nilLocation = Loc(0,0);

fun getCurrentLocation () =
  Loc(symbolStart(), symbolEnd())
;

fun mkLoc x = (getCurrentLocation(), x);

fun xLR (loc, _) = loc
and xL (Loc(l,r), _) = l
and xR (Loc(l,r), _) = r
;

fun xxLR (Loc(l,_), _) (Loc(_,r),_) = Loc(l,r);
fun xxRL (Loc(_,r), _) (Loc(l,_),_) = Loc(r,l);

fun errLines char1 char2 charline1 line1 line2 =
  case !msgStyle of
    "default" =>
      (
        msgString ", line "; msgInt line1;
        if line2 <> line1 then ( msgString "-"; msgInt line2 ) else ();
        msgString ", characters ";
        msgInt (char1-charline1); msgString "-"; msgInt (char2-charline1);
        msgString ":"
      )
  | "msdev" =>
      (
        msgString "("; msgInt line1;
        if line2 <> line1 then ( msgString "-"; msgInt line2 ) else ();
        msgString "): characters ";
        msgInt (char1-charline1); msgString "-"; msgInt (char2-charline1);
        msgString ":"
      )
  | _ =>
      raise Impossible ("errLines: " ^ !msgStyle)
;

fun msgChars n c =
  if n > 0 then (msgChar c; msgChars (n-1) c) else ()
;

fun errLoc input seek line_flag (Loc(pos1, pos2)) =
  let
    fun skipLine () =
      (case input() of #"\^Z" => () | #"\n" => () | _ => skipLine())
      handle Size => ()
    and copyLine () =
      (case input() of
           #"\^Z" => raise Size
         | #"\n" => msgEOL()
         | c => (msgChar c; copyLine()))
      handle Size => (msgString "<EOF>"; msgEOL())
    and tr_line first len ch =
      let
        val c = ref #" "
        val f = ref first
        val l = ref len
        fun loop f l =
          (case input() of
                #"\^Z" => raise Size
              | #"\n" => ()
              | c =>
                  if f > 0 then
                    (msgChar(if c = #"\t" then c else #" "); loop (f-1) l)
                  else if l > 0 then
                    (msgChar(if c = #"\t" then c else ch); loop f (l-1))
                  else loop f l)
          handle Size => msgChars 5 ch
      in loop first len end
    val pos = ref 0
    val line1 = ref 1
    val line1_pos = ref 0
    val line2 = ref 1
    val line2_pos = ref 0
  in
    seek 0;
    (while !pos < pos1 do
       (incr pos;
        case input() of
            #"\^Z" => raise Size
          | #"\n" => (incr line1; line1_pos := !pos)
          | _ => ()))
    handle Size => ();
    line2 := !line1;
    line2_pos := !line1_pos;
    (while !pos < pos2 do
       (incr pos;
        case input() of
            #"\^Z" => raise Size
          | #"\n" => (incr line2; line2_pos := !pos)
          | _ => ()))
    handle Size => ();
    if line_flag then
      errLines pos1 pos2 (!line1_pos) (!line1) (!line2)
    else ();
    msgEOL();
    if !line1 = !line2 then
      (seek (!line1_pos);
       errPrompt ""; copyLine ();
       seek (!line1_pos);
       errPrompt ""; tr_line (pos1 - !line1_pos) (pos2 - pos1) #"^";
       msgEOL())
    else
      (
      seek (!line1_pos);
      errPrompt ""; tr_line 0 (pos1 - !line1_pos) #".";
      seek pos1;
      copyLine();
      if !line2 - !line1 <= 8 then
        (for (fn i => (errPrompt ""; copyLine()))
             (!line1 + 1) (!line2 - 1))
      else
        (for (fn i => (errPrompt ""; copyLine()))
             (!line1 + 1) (!line1 + 3);
         errPrompt ".........."; msgEOL();
         for (fn i => skipLine())
             (!line1 + 4) (!line2 - 4);
         for (fn i => (errPrompt ""; copyLine()))
             (!line2 - 3) (!line2 - 1));
      errPrompt "";
      (for (fn i => msgChar(input()))
           (!line2_pos) (pos2 - 1);
       tr_line 0 100 #".")
      handle Size => msgString "<EOF>";
      msgEOL()
      )
  end;

fun errFileName() =
  case !msgStyle of
    "default" =>
      (
        msgString "File \""; msgString (!input_name); msgString "\""
      )
  | "msdev" =>
      msgString( FileSys.fullPath (!input_name) )
  | _ =>
      raise Impossible ("errFileName: " ^ !msgStyle)
;

fun errLocation loc =
  if size (!input_name) > 0 then
    let val p = pos_in (!input_stream) in
      errFileName();
      errLoc (fn () => input_char (!input_stream)) (seek_in (!input_stream))
             true loc;
      seek_in (!input_stream) p
    end
  else
    let
      val curr_pos = ref 0
      fun input () =
        let val c =
          if !curr_pos >= 2048 then
            raise Size
          else if !curr_pos >= 0 then
            CharVector.sub(getLexBuffer(!input_lexbuf), !curr_pos)
          else
            #"."
        in incr curr_pos; c end
      and seek pos =
        curr_pos := pos - getLexAbsPos(!input_lexbuf)
    in
      errPrompt "Toplevel input:";
      errLoc input seek false loc
    end
;

fun errInputName () =
(
  errFileName();
  msgString ", line 1:"; 
  msgEOL()
);

fun errorMsg loc msg =
(
  msgIBlock 0;
  errLocation loc;
  errPrompt msg; msgEOL();
  msgEBlock();
  raise Toplevel
);
