(* AppleScript.sml *)
(* 1997 Jul 04  e  *)

(* This module is a Mac specific implementation of an applescript client
   it provides the capability to
   - compile AppleScript source
   - run the resulting script objects
   - retrieve string results and/or error messages
   - dispose of script objects and results
*)

(* references:
   Inside Macintosh: Interapplication Communication, especially Chapter 10
   AppleScript Language Guide English Edition
   -- available at http://applescript.apple.com/support.html
*)

(* OSA is Apple's Open Scripting Architecture -- the interface to AppleScript *)

type OSAerr = int (* 0 => OK *)
prim_type OSAID_  (* handle to compiled script objects and results *)
type OSAID = { id: OSAID_, ok: bool ref }

exception AppleScriptErr of OSAerr * string;

local

  type OSA_oid = { err : OSAerr,  id : OSAID_ }
  type OSA_str = { err : OSAerr, str : string }

  prim_val as_compile_ : string -> OSA_oid = 1 "mac_OSACompile"
  prim_val as_execute_ : OSAID_ -> OSA_oid = 1 "mac_OSAExecute"
  prim_val as_display_ : OSAID_ -> OSA_str = 1 "mac_OSADisplay"
  prim_val as_dispose_ : OSAID_ -> OSAerr  = 1 "mac_OSADispose"
  prim_val as_error_   :   unit -> OSA_str = 1 "mac_OSAScriptError"

  fun as_run_script_ xid =
    let
      val { err, id = rid } = as_execute_ xid
      val res = if err = 0
                then as_display_ rid
                else as_error_ ()
    in
      as_dispose_ rid;
      res
    end

  fun as_run_text_ text = 
    let
      val (ss as { err, id = xid }) = as_compile_ text
    in 
      let val res = if err = 0
                    then as_run_script_ xid
                    else as_error_ ()
      in
        as_dispose_ xid;
        res
      end
    end

  fun as_check_str_ { err, str } =
    if err = 0
    then str
    else raise (AppleScriptErr (err,str))

  fun as_raise_err _ =
    let val {err,str} = as_error_ ()
    in raise (AppleScriptErr (err,str)) end

  fun as_check_err_ err { id, ok } =
    if err = 0
    then ok := false
    else as_raise_err ()

  fun as_check_oid_ { err = ror, id = rid } =
    if ror = 0
    then { id = rid, ok = ref true }
    else as_raise_err ()

  fun as_check_osaid_ { id = xid, ok } =
    if !ok
    then xid 
    else raise (AppleScriptErr (~1,"Attempt to use disposed OSAID"))

in

(*  as_compile -- compiles a text script for later execution *)

fun as_compile txt = as_check_oid_ (as_compile_ txt);

(*  as_dispose -- disposes the compiled script freeing AS memory *)

fun as_dispose s   = as_check_err_ (as_dispose_ (as_check_osaid_ s)) s;

(*  as_run_script -- runs a precompiled script *)

fun as_run_script s = as_check_str_ (as_run_script_ (as_check_osaid_ s));

(*  as_run_text -- runs a one shot script, like system() but with a result *)

fun as_run_text txt = as_check_str_ (as_run_text_ txt)

end;

(* end of AppleScript.sml *)
