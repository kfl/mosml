(* Msp.sml -- prelude for ML Server Pages
   sestoft@dina.kvl.dk 2000-02-24 version 0.7
 *)

(* Efficiently concatenable word sequences *)

datatype wseq = 
    Empty                               (* The empty sequence         *)
  | Nl                                  (* Newline                    *)
  | $ of string                         (* A string                   *)
  | $$ of string list                   (* A sequence of strings      *)
  | && of wseq * wseq;                  (* Concatenation of sequences *)

infix &&

fun prmap f []       = Empty 
  | prmap f (x1::xr) = 
    let fun loop y1 []       = f y1
	  | loop y1 (y2::yr) = f y1 && loop y2 yr 
    in loop x1 xr end

fun prsep sep f []       = Empty 
  | prsep sep f (x1::xr) = 
    let fun loop y1 []       = f y1
	  | loop y1 (y2::yr) = f y1 && sep && loop y2 yr 
    in loop x1 xr end

fun flatten Empty acc      = acc
  | flatten Nl    acc      = "\n" :: acc
  | flatten ($ s) acc      = s :: acc
  | flatten ($$ ss) acc    = List.@(ss, acc)
  | flatten (s1 && s2) acc = flatten s1 (flatten s2 acc)
val flatten = fn seq => String.concat(flatten seq []);
    
fun printseq Empty      = ()
  | printseq Nl         = TextIO.print "\n"
  | printseq ($ s)      = TextIO.print s
  | printseq ($$ ss)    = List.app TextIO.print ss
  | printseq (s1 && s2) = (printseq s1; printseq s2);
    
fun vec2list vec = Vector.foldr op:: [] vec

(* CGI parameter access shorthands *)

exception ParamMissing of string
exception NotInt of string * string

fun % fnm = 
    case Mosmlcgi.cgi_field_string fnm of 
	NONE   => raise ParamMissing "fnm"
      | SOME v => v

fun %? fnm = Option.isSome(Mosmlcgi.cgi_field_string fnm)

fun %# fnm =
    case Mosmlcgi.cgi_field_string fnm of 
	NONE   => raise ParamMissing fnm
      | SOME v => (case Int.fromString v of
		       NONE   => raise NotInt(fnm, v)
		     | SOME i => i)
		
fun %%(fnm, dflt)  = Option.getOpt(Mosmlcgi.cgi_field_string fnm, dflt)

fun %%#(fnm, dflt) = 
    Option.getOpt(Option.mapPartial Int.fromString
		  (Mosmlcgi.cgi_field_string fnm), dflt)

(* HTML generic marks *)

fun mark0 tag = $$["<", tag, ">"]
fun mark0a attr tag = $$["<", tag, " ", attr, ">"]
fun mark1 tag seq = $$["<", tag, ">"] && seq && $$["</", tag, ">"]
fun mark1a tag attr seq = 
    $$["<", tag, " ", attr, ">"] && seq && $$["</", tag, ">"]
fun comment seq = $"<!--" && seq && $"-->"

(* HTML documents and headers *)

fun html seq = $"<HTML>" && seq && $"</HTML>"
fun head seq = $"<HEAD>" && seq && $"</HEAD>"
fun title seq = $"<TITLE>" && seq && $"</TITLE>"
fun body seq = $"<BODY>" && seq && $"</BODY>"
fun bodya attr seq = $$["<BODY ", attr, ">"] && seq && $"</BODY>"
fun htmldoc tit bod = html (head (title tit) && body bod)

(* HTML headings and vertical format *)

fun h1 seq = $"<H1>" && seq && $"</H1>"
fun h2 seq = $"<H2>" && seq && $"</H2>"
fun h3 seq = $"<H3>" && seq && $"</H3>"
fun h4 seq = $"<H4>" && seq && $"</H4>"
fun h5 seq = $"<H5>" && seq && $"</H5>"
fun h6 seq = $"<H6>" && seq && $"</H6>"

fun p seq = $"<P>" && seq && $"</P>"
fun pa attr seq = $$["<P ", attr, ">"] && seq && $"</P>"
fun divi seq = $"<DIV>" && seq && $"</DIV>"
fun divia attr seq = $$["<DIV ", attr, ">"] && seq && $"</DIV>"
fun blockquote seq = $"<BLOCKQUOTE>" && seq && $"</BLOCKQUOTE>"
fun blockquotea attr seq = 
    $$["<BLOCKQUOTE ", attr, ">"] && seq && $"</BLOCKQUOTE>"
fun center seq = $"<CENTER>" && seq && $"</CENTER>"
fun address seq = $"<ADDRESS>" && seq && $"</ADDRESS>"
fun pre seq = $"<PRE>" && seq && $"</PRE>"

val br = $"<BR>"
fun bra attr = $$["<BR ", attr, ">"]
val hr = $"<HR>"
fun hra attr = $$["<HR ", attr, ">"]

(* HTML anchors and hyperlinks *)

fun ahref link seq = $$["<A HREF=\"", link, "\">"] && seq && $"</A>"
fun ahrefa link attr seq = 
    $$["<A HREF=\"", link, " ", attr, "\">"] && seq && $"</A>"
fun aname name seq = $$["<A NAME=\"", name, "\">"] && seq && $"</A>"

(* HTML text formats and style *)

fun em seq = $"<EM>" && seq && $"</EM>"
fun strong seq = $"<STRONG>" && seq && $"</STRONG>"
fun tt seq = $"<TT>" && seq && $"</TT>"
fun sub seq = $"<SUB>" && seq && $"</SUB>"
fun sup seq = $"<SUP>" && seq && $"</SUP>"
fun fonta attr seq = $$["<FONT ", attr, ">"] && seq && $"</FONT>"

(* HTML lists *)

fun ul seq = $"<UL>" && seq && $"</UL>"
fun ula attr seq = $$["<UL ", attr, ">"] && seq && $"</UL>"
fun ol seq = $"<OL>" && seq && $"</OL>"
fun ola attr seq = $$["<OL ", attr, ">"] && seq && $"</OL>"
fun li seq = $"<LI>" && seq && $"</LI>"

fun dl seq = $"<DL>" && seq && $"</DL>"
fun dla attr seq = $$["<DL ", attr, ">"] && seq && $"</DL>"
fun dt seq = $"<DT>" && seq && $"</DT>"
fun dd seq = $"<DD>" && seq && $"</DD>"

(* HTML tables *)

fun tr seq = $"<TR>" && seq && $"</TR>"
fun tra attr seq = $$["<TR ", attr, ">"] && seq && $"</TR>"
fun td seq = $"<TD>" && seq && $"</TD>"
fun tda attr seq = $$["<TD ", attr, ">"] && seq && $"</TD>"
fun th seq = $"<TH>" && seq && $"</TH>"
fun tha attr seq = $$["<TH ", attr, ">"] && seq && $"</TH>"
fun table seq = $"<TABLE>" && seq && $"</TABLE>"
fun tablea attr seq = $$["<TABLE ", attr, ">"] && seq && $"</TABLE>"
fun caption seq = $"<CAPTION>" && seq && $"</CAPTION>"
fun captiona attr seq = $$["<CAPTION ", attr, ">"] && seq && $"</CAPTION>"

(* HTML images and image maps *)

fun img src  = $$["<IMG SRC=\"", src, "\">"]
fun imga src attr = $$["<IMG SRC=\"", src, "\" ", attr, ">"]
fun map nam seq = $$["<MAP NAME=\"", nam, "\">"] && seq && $"</MAP>"
fun mapa nam attr seq = 
    $$["<MAP NAME=\"", nam, "\" ", attr, ">"] && seq && $"</MAP>"
fun area { shape, href, coords, alt } = 
    $$["<AREA SHAPE=\"", shape, "\" COORDS=\"", coords, "\" "]
    && (case href of NONE => $"NOHREF" | SOME r => $$["HREF=\"", r, "\" "])
    && (case alt  of NONE => Empty | SOME a => $$["ALT=\"", a, "\""])

(* HTML forms etc *)

fun form action seq = $$["<FORM ACTION=\"", action, "\">"] && seq && $"</FORM>"
fun forma action attr seq = 
    $$["<FORM ACTION=\"", action, "\" ", attr, ">"] && seq && $"</FORM>"
fun input typ = $$["<INPUT TYPE=", typ, ">"]
fun inputa typ attr = $$["<INPUT TYPE=", typ, " ", attr, ">"]
fun intext name attr = $$["<INPUT TYPE=TEXT NAME=\"", name, "\" ", attr, ">"]
fun inpassword name attr = 
       $$["<INPUT TYPE=PASSWORD NAME=\"", name, "\" ", attr, ">"]
fun incheckbox {name, value} attr = 
       $$["<INPUT TYPE=CHECKBOX VALUE=\"", value, "\" NAME=\"", name, 
	  "\" ", attr, ">"]
fun inradio {name, value} attr = 
       $$["<INPUT TYPE=RADIO VALUE=\"", value, "\" NAME=\"", name, 
	  "\" ", attr, ">"]
fun inreset value attr = 
       $$["<INPUT TYPE=RESET VALUE=\"", value, "\" ", attr, ">"]
fun insubmit value attr = 
       $$["<INPUT TYPE=SUBMIT VALUE=\"", value, "\" ", attr, ">"]
fun inhidden {name, value} = 
       $$["<INPUT TYPE=HIDDEN NAME=\"", name, "\" VALUE=\"", value, "\">"]
fun textarea name seq = 
	  $$["<TEXTAREA NAME=\"", name, "\">"] && seq && $"</TEXTAREA>"
fun textareaa name attr seq = 
	  $$["<TEXTAREA NAME=\"", name, "\" ", attr, ">"] 
	  && seq && $"</TEXTAREA>"
fun select name attr seq = 
	  $$["<SELECT NAME=\"", name, "\" ", attr, ">"] && seq && $"</SELECT>"
fun option value = $$["<OPTION VALUE=\"", value, "\">"]

(* HTML frames and framesets *)

fun frameset attr seq = $$["<FRAMESET ", attr, ">"] && seq && $"</FRAMESET>"
fun frame { src, name } = $$["<FRAME SRC=\"", src, "\" NAME=\"", name, "\">"] 
fun framea { src, name } attr = 
	  $$["<FRAME SRC=\"", src, "\" NAME=\"", name, "\" ", attr, ">"] 

(* HTML encoding *)

fun urlencode s : string =
    let fun encode #" " = "+"
	  | encode #"-" = "-"
	  | encode #"_" = "_"
	  | encode #"." = "."
	  | encode c    = 
	    if Char.isAlphaNum c then String.str c 
	    else "%" ^ StringCvt.padLeft #"0" 2 
                       (Int.fmt StringCvt.HEX (Char.ord c))
    in String.translate encode s end

(* Maybe this should create a wseq instead, to avoid creating a long 
   string by concatenation *)

fun htmlencode s : string =
    let fun encode #"<" = "&lt;"
	  | encode #">" = "&gt;"
	  | encode #"&" = "&amp;"
	  | encode c    = String.str c
    in String.translate encode s end
