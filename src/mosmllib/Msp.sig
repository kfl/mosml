(* Msp -- utilities for CGI scripts and ML Server Pages *)

(* Efficiently concatenable word sequences *)

datatype wseq = 
    Empty                               (* The empty sequence         *)
  | Nl                                  (* Newline                    *)
  | $ of string                         (* A string                   *)
  | $$ of string list                   (* A sequence of strings      *)
  | && of wseq * wseq;                  (* Concatenation of sequences *)

(* Manipulating wseqs *)

val prmap    : ('a -> wseq) -> 'a list -> wseq
val prsep    : wseq -> ('a -> wseq) -> 'a list -> wseq
val flatten  : wseq -> string
val printseq : wseq -> unit
val vec2list : 'a vector -> 'a list


(* Shorthands for accessing CGI parameters *)

exception ParamMissing of string
exception NotInt of string * string

val %        : string -> string      
val %?       : string -> bool
val %#       : string -> int
val %%       : string * string -> string
val %%#      : string * int -> int


(* HTML generic marks *)

val mark0    : string -> wseq
val mark0a   : string -> string -> wseq
val mark1    : string -> wseq -> wseq
val mark1a   : string -> string -> wseq -> wseq
val comment  : wseq -> wseq

(* HTML documents and headers *)

val html     : wseq -> wseq
val head     : wseq -> wseq
val title    : wseq -> wseq
val body     : wseq -> wseq
val bodya    : string -> wseq -> wseq
val htmldoc  : wseq -> wseq -> wseq

(* HTML headings and vertical format *)

val h1       : wseq -> wseq
val h2       : wseq -> wseq
val h3       : wseq -> wseq
val h4       : wseq -> wseq
val h5       : wseq -> wseq
val h6       : wseq -> wseq
val p        : wseq -> wseq
val pa       : string -> wseq -> wseq
val br       : wseq
val bra      : string -> wseq
val hr       : wseq
val hra      : string -> wseq

val divi        : wseq -> wseq
val divia       : string -> wseq -> wseq
val blockquote  : wseq -> wseq
val blockquotea : string -> wseq -> wseq
val center      : wseq -> wseq
val address     : wseq -> wseq
val pre         : wseq -> wseq

(* HTML anchors and hyperlinks *)

val ahref    : string -> wseq -> wseq
val ahrefa   : string -> string -> wseq -> wseq
val aname    : string -> wseq -> wseq

(* HTML text formats and style *)

val em       : wseq -> wseq
val strong   : wseq -> wseq
val tt       : wseq -> wseq
val sub      : wseq -> wseq
val sup      : wseq -> wseq
val fonta    : string -> wseq -> wseq

(* HTML lists *)

val ul       : wseq -> wseq
val ula      : string -> wseq -> wseq
val ol       : wseq -> wseq
val ola      : string -> wseq -> wseq
val li       : wseq -> wseq
val dl       : wseq -> wseq
val dla      : string -> wseq -> wseq
val dt       : wseq -> wseq
val dd       : wseq -> wseq

(* HTML tables *)

val table    : wseq -> wseq
val tablea   : string -> wseq -> wseq
val tr       : wseq -> wseq
val tra      : string -> wseq -> wseq
val td       : wseq -> wseq
val tda      : string -> wseq -> wseq
val th       : wseq -> wseq
val tha      : string -> wseq -> wseq
val caption  : wseq -> wseq
val captiona : string -> wseq -> wseq

(* HTML images and image maps *)

val img      : string -> wseq
val imga     : string -> string -> wseq
val map      : string -> wseq -> wseq
val mapa     : string -> string -> wseq -> wseq
val area     : { alt : string option, coords : string, 
                 href : string option, shape : string} -> wseq

(* HTML forms etc *)

val form       : string -> wseq -> wseq
val forma      : string -> string -> wseq -> wseq
val input      : string -> wseq
val inputa     : string -> string -> wseq
val intext     : string -> string -> wseq
val inpassword : string -> string -> wseq
val incheckbox : {name : string, value : string} -> string -> wseq
val inradio    : {name : string, value : string} -> string -> wseq
val inreset    : string -> string -> wseq
val insubmit   : string -> string -> wseq
val inhidden   : {name : string, value : string} -> wseq
val textarea   : string -> wseq -> wseq
val textareaa  : string -> string -> wseq -> wseq
val select     : string -> string -> wseq -> wseq
val option     : string -> wseq

(* HTML frames and framesets *)

val frameset   : string -> wseq -> wseq
val frame      : { src : string, name : string } -> wseq
val framea     : { src : string, name : string } -> string -> wseq

(* HTML encoding  *)

val urlencode  : string -> string
val htmlencode : string -> string


(* 
   This module provides support functions for writing CGI scripts and
   ML Server Page scripts.

   [wseq] is the type of efficiently concatenable word sequences.
   Building an HTML page (functionally) as a wseq is more efficient
   than building it (functionally) as a string, and more convenient
   and modular than building it (imperatively) by calling print.

   [Empty] represents the empty string "".

   [Nl] represents the string "\n" consisting of a single newline character.

   [$ s] represents the string s.

   [$$ ss] represents the string String.concat(ss).

   [&&(ws1, ws2)] represents the concatenation of the strings
   represented by ws1 and ws2.  The function && should be declared
        infix &&

   [prmap f xs] is f x1 && ... && f xn evaluated from left to right, 
   when xs is [x1, ..., xn].

   [prsep sep f xs] is f x1 && sep && ... && sep && f xn, evaluated
   from left to right, when xs is [x1, ..., xn].

   [flatten ws] is the string represented by ws.

   [printseq ws] is equivalent to print(flatten ws), but avoids
   building any new strings.

   [vec2list vec] is a list of the elements of vector vec.  Use it to
   convert e.g. the results of a database query into a list, for
   processing with prmap or prsep.


   Shorthands for accessing CGI parameters:

   [%? fnm] returns true if there is a string associated with CGI
   parameter fnm; otherwise returns false.

   [% fnm] returns a string associated with CGI parameter fnm if there
   is any; raises ParamMissing(fnm) if no strings are associated with
   fnm.  Equivalent to
       case Mosmlcgi.cgi_field_string fnm of 
           NONE   => raise ParamMissing "fnm"
         | SOME v => v
   In general, multiple strings may be associated with a CGI parameter; 
   use Mosmlcgi.cgi_field_strings if you need to access all of them.

   [%# fnm] returns the integer i if there is a string associated with
   CGI parameter fnm, and that string is parsable as ML integer i.
   Raises ParamMissing(fnm) if no string is associated with fnm.
   Raises NotInt(fnm, s) if there is a string but it is not parsable
   as an ML int.

   [%%(fnm, dflt)] returns a string associated with CGI parameter fnm
   if there is any; otherwise returns the string dflt.

   [%%#(fnm, dflt)] returns the integer i if there is a string
   associated with CGI parameter fnm, and that string is parsable as
   an ML int; otherwise returns the string dflt.


   HTML generic marks:

   [mark0 t] generates the HTML tag <t> as a wseq.

   [mark0a attr t] generates the attributed HTML tag <t attr> as a wseq.

   [mark1 t ws] generates  <t>ws</t>  as a wseq.

   [mark1a attr t ws] generates  <t attr>ws</t> as a wseq.

   [comment ws] generates  <!--ws-->  as a wseq.


   HTML documents and headers:

   [html ws] generates <HTML>ws</HTML>.

   [head ws] generates <HEAD>ws</HEAD>.

   [title ws] generates <TITLE>ws</TITLE>.

   [body ws] generates <BODY>ws</BODY>.

   [bodya attr ws] generates <BODY attr>ws</BODY>.

   [htmldoc titl ws] generates 
   <HTML><HEAD><TITLE>titl</TITLE></HEAD><BODY>ws</BODY></HTML>.

   
   HTML headings and vertical format:

   [h1 ws] generates <H1>ws</H1>.

   [p ws] generates <P>ws</P>.

   [pa attr ws] generates <P attr>ws</P>.

   [br] generates <BR>.

   [bra attr] generates <BR attr>.

   [hr] generates <HR>.

   [hra attr] generates <HR attr>.

   [divi ws] generates <DIV>ws</DIV>.

   [divia attr ws] generates <DIV attr>ws</DIV>.

   [blockquote ws] generates <BLOCKQUOTE>ws</BLOCKQUOTE>.

   [blockquotea attr ws] generates <BLOCKQUOTE attr>ws</BLOCKQUOTE> 

   [center ws] generates <CENTER>ws</CENTER>.

   [address ws] generates <ADDRESS>ws</ADDRESS>.

   [pre ws] generates <PRE>ws</PRE>.


   HTML anchors and hyperlinks:

   [ahref link ws] generates <A HREF="link">ws</A>.

   [ahrefa link attr ws] generates <A HREF="link" attr>ws</A>.

   [aname nam ws] generates <A NAME="name">ws</A>.


   HTML text formats and style:

   [em ws] generates <EM>ws</EM>.

   [strong ws] generates <STRONG>ws</STRONG>.

   [tt ws] generates <TT>ws</TT>.

   [sub ws] generates <SUB>ws</SUB>.

   [sup ws] generates <SUP>ws</SUP>.

   [fonta attr ws] generates <FONT attr>ws</FONT>.


   HTML lists:

   [ul ws] generates <UL>ws</UL>.

   [ula attr ws] generates <UL attr>ws</UL>.

   [ol ws] generates <OL>ws</OL>.

   [ola attr ws] generates <OL attr>ws</OL>.

   [li ws] generates <LI>ws</LI>.

   [dl ws] generates <DL>ws</DL>.

   [dla attr ws] generates <DL attr>ws</DL>.

   [dt ws] generates <DT>ws</DT>.

   [dd ws] generates <DD>ws</DD>.


   HTML tables:

   [table ws] generates <TABLE>ws</TABLE>.

   [tablea attr ws] generates <TABLE attr>ws</TABLE>.

   [tr ws] generates <TR>ws</TR>.

   [tra attr ws] generates <TR attr>ws</TR>.

   [td ws] generates <TD>ws</TD>.

   [tda attr ws] generates <TD attr>ws</TD>.

   [th ws] generates <TH>ws</TH>.

   [tha attr ws] generates <TH attr>ws</TH>.

   [caption ws] generates <CAPTION>ws</CAPTION>.

   [captiona attr ws] generates <CAPTION attr>ws</CAPTION>.


   HTML images and image maps:

   [img s] generates <IMG SRC="s">.

   [imga s attr] generates <IMG SRC="s" attr>.

   [map nam ws] generates <MAP NAME="name">ws</MAP>.

   [mapa nam attr ws] generates <MAP NAME="name" attr>ws</MAP>.

   [area { alt, coords, href, shape}] generates
       <AREA SHAPE="shape" COORDS="coords" HREF="link" ALT="desc"> 
   when href is SOME link (where HREF is replaced by NOHREF otherwise)
   and  alt  is SOME desc (where ALT is omitted otherwise).


   HTML forms etc:

   [form act ws] generates <FORM ACTION="act">ws</FORM>.

   [forma act attr ws] generates <FORM ACTION="act" attr>ws</FORM>.

   [input typ] generates <INPUT TYPE=typ>.

   [inputa typ attr] generates <INPUT TYPE=typ attr>.

   [intext name attr] generates <INPUT TYPE=TEXT NAME="name" attr>.

   [inpassword name attr] generates <INPUT TYPE=PASSWORD NAME="name" attr>.

   [incheckbox {name, value} attr] generates 
   <INPUT TYPE=CHECKBOX NAME="name" VALUE="value" attr>.

   [inradio {name, value} attr] generates 
   <INPUT TYPE=RADIO NAME="name" VALUE="value" attr>.

   [inreset value attr] generates <INPUT TYPE=RESET VALUE="value" attr>.

   [insubmit value attr] generates <INPUT TYPE=SUBMIT VALUE="value" attr>.

   [inhidden {name, value}] generates
   <INPUT TYPE=HIDDEN NAME="name" VALUE="value">.

   [textarea name ws] generates <TEXTAREA NAME="name">ws</TEXTAREA>.

   [textareaa name attr ws] generates 
   <TEXTAREA NAME="name" attr>ws</TEXTAREA>.

   [select name attr ws] generates <SELECT NAME="name" attr>ws</SELECT>.

   [option value] generates <OPTION VALUE="value">.


   HTML frames and framesets:

   [frameset attr ws] generates <FRAMESET attr>ws</FRAMESET>.

   [frame { src, name }] generates <FRAME SRC="src" NAME="name">.

   [framea { src, name } attr] generates <FRAME SRC="src" NAME="name" attr>.


   HTML encoding functions:

   [urlencode s] returns the url-encoding of s.  That is, space (ASCII 32) 
   is replaced by `+' and every non-alphanumeric character c except 
   the characters - _ . is replaced by %hh, where hh is the hexadecimal 
   representation of the ASCII code of c.

   [htmlencode s] returns the html-encoding of s.  That is, < and >
   are replaced by &lt; and &gt; respectively, and & is replaced by 
   &amp;
*)
