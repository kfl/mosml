(* Mosmlcookie -- getting and setting cookies in CGI scripts *)

exception CookieError of string

val allCookies     : string list
val getCookieValue : string -> string option 
val getCookie      : string -> string option 

type cookiedata = 
    { name   : string, 
      value  : string, 
      expiry : Date.date option, 
      domain : string option, 
      path   : string option, 
      secure : bool }

val setCookie    : cookiedata -> string
val setCookies   : cookiedata list -> string

val deleteCookie : { name : string, path : string option } -> string

(* 
   These functions may be used in CGI scripts to get and set cookies.
   (c) Hans Molin, Computing Science Dept., Uppsala University, 1999.

   [getCookieValue ck] returns SOME(v) where v is the value associated
   with the cookie ck, if any; otherwise returns NONE.

   [getCookie ck] returns SOME(nv) where nv is the ck=value string
   for the cookie ck, if any; otherwise returns NONE.

   [allCookies] is a list [nv1, nv2, ..., nvm] of all the ck=value
   pairs of defined cookies.

   [setCookie { name, value, expiry, domain, path, secure }] returns a
   string which (when transmitted to a browser as part of the HTTP
   response header) sets a cookie with the given name, value, expiry
   date, domain, path, and security.

   [setCookies ckds] returns a string which (when transmitted to a
   browser as part of the HTTP response header) sets the specified cookies.

   [deleteCookie { name, path }] returns a string which (when
   transmitted to a browser as part of the HTTP response header)
   deletes the specified cookie by setting its expiry to some time in
   the past.
*)
