(* AppleScript -- Apple MacOS scripting *)

type OSAID
type OSAerr = int

exception AppleScriptErr of OSAerr * string

val as_compile    : string -> OSAID
val as_dispose    : OSAID -> unit
val as_run_script : OSAID -> string
val as_run_text   : string -> string

(*
   These Mac specific functions provide the capability to compile 
   and run AppleScript programs.

   The exception AppleScriptErr is raised in the event of an error.

   [as_compile str] compiles AppleScript source code text, returning
   an abstract token of type OSAID. This token may be used to run
   the script. The token may be used repeatedly until it is returned
   with as_dispose or until mosml exits.

   [as_dispose tok] disposes of the resources associated with the 
   OSAID token so that they may be reused by the AppleScript system.
   AppleScriptErr is raised upon any attemp to reuse a disposed token.

   [as_run_script tok] runs the script associated with the token.
   This typically involves AppleEvent communication with other 
   programs running on the Mac, or networked Macs. The AppleScript
   result is returned as a string.

   [as_run_text str] compiles and runs the AppleScript source code text,
   disposing all resources allocated in the process, and returns the
   AppleScript result as a string.


   References:

   Inside Macintosh: Interapplication Communication, Chapter 10
   AppleScript Language Guide English Edition,
   available at http://applescript.apple.com/support.html
*)
