(* Mosmlcgi -- support for writing CGI scripts in Moscow ML *)

(* 1. Accessing the fields or parameters of a CGI call *)

val cgi_fieldnames     : string list
val cgi_field_strings  : string -> string list;
val cgi_field_string   : string -> string option;
val cgi_field_integer  : string * int -> int;

(* 2. Accessing parts in multipart/form-data; form-based file upload *)

val cgi_partnames      : string list

type part
val cgi_part           : string -> part option
val cgi_parts          : string -> part list

val part_fieldnames    : part -> string list
val part_type          : part -> string option
val part_data          : part -> string
val part_field_strings : part -> string -> string list
val part_field_string  : part -> string -> string option
val part_field_integer : part -> string * int -> int

(* 3. Administrative information *)

val cgi_server_software       : string option
val cgi_server_name           : string option
val cgi_gateway_interface     : string option
val cgi_server_protocol       : string option
val cgi_server_port           : string option
val cgi_request_method        : string option
val cgi_http_accept           : string option
val cgi_http_user_agent       : string option
val cgi_http_referer          : string option
val cgi_path_info             : string option
val cgi_path_translated       : string option
val cgi_script_name           : string option
val cgi_query_string          : string option
val cgi_remote_host           : string option
val cgi_remote_addr           : string option
val cgi_remote_user           : string option
val cgi_remote_ident          : string option
val cgi_auth_type             : string option
val cgi_content_type          : string option
val cgi_content_length        : string option
val cgi_annotation_server     : string option

val cgi_http_cookie           : string option
val cgi_http_forwarded        : string option
val cgi_http_host             : string option
val cgi_http_proxy_connection : string option
val cgi_script_filename       : string option
val cgi_document_root         : string option
val cgi_server_admin          : string option
val cgi_api_version           : string option
val cgi_the_request           : string option
val cgi_request_uri           : string option
val cgi_request_filename      : string option
val cgi_is_subreq             : string option

(* 
   The Mosmlcgi library is for writing CGI programs in Moscow ML.  A
   CGI program may be installed on a WWW server and is invoked in
   response to HTTP requests sent to the server from a web browser,
   typically from an HTML FORM element.


   1. Obtaining field values sent from an ordinary HTML form
   ---------------------------------------------------------

   [cgi_fieldnames] is a list of the names of fields present in the
   CGI call message.  If field name fnm is in cgi_fieldnames, then
   cgi_field_string fnm <> NONE.

   [cgi_field_strings fnm] is a (possibly empty) list of the strings
   bound to field fnm.

   [cgi_field_string fnm] returns SOME(s) where s is a string bound to
   field name fnm, if any; otherwise NONE.  Equivalent to 
        case cgi_field_strings fnm of 
             []     => NONE 
           | s :: _ => SOME s

   [cgi_field_integer (fnm, deflt)] attempts to parse an integer from
   field fnm.  Returns i if cgi_field_string(fnm) = SOME(s) and an
   integer i can be parsed from a prefix of s; otherwise returns deflt.


   2. Obtaining field values sent with ENCTYPE="multipart/form-data"  
   -----------------------------------------------------------------

   [cgi_partnames] is a list of the names of the parts of the
   multipart/form-data message.

   The type part is the abstract type of parts of a message.  Each part
   may have several fields.  In this implementation, the field of a
   part cannot be a another part itself.

   [cgi_parts pnm] is a (possibly empty) list of the parts called pnm.

   [cgi_part pnm] is SOME(prt) where prt is a part called pnm, if any;
   otherwise NONE.  Equivalent to
        case cgi_parts pnm of 
             []       => NONE 
           | prt :: _ => SOME prt

   [part_fieldnames prt] is the list of field names in part pnm.

   [part_type prt] is SOME(typ) if the part prt contains a specification
   `Context-Type: typ'; otherwise NONE.

   [part_data prt] is the data contain in part prt; for instance, the
   contents of a file uploaded via form-based file upload.

   [part_field_strings prt fnm] is a (possibly empty) list of the
   strings bound to field fnm in part prt.

   [part_field_string prt fnm] returns SOME(s) where s is a string
   bound to field name fnm in part prt, if any; otherwise NONE.
   Equivalent to 
        case part_field_strings prt fnm of 
             []     => NONE 
           | s :: _ => SOME s

   [part_field_integer prt (fnm, deflt)] attempts to parse an integer
   from field fnm of part prt.  Returns i if part_field_string prt fnm
   = SOME(s) and an integer i can be parsed from a prefix of s;
   otherwise returns deflt.


   3. Administrative and server information
   ----------------------------------------

   Each of the following variables has the value SOME(s) if the
   corresponding CGI environment variable is bound to string s;
   otherwise NONE:

   [cgi_server_software] is the value of SERVER_SOFTWARE

   [cgi_server_name] is the value of SERVER_NAME

   [cgi_gateway_interface] is the value of GATEWAY_INTERFACE

   [cgi_server_protocol] is the value of SERVER_PROTOCOL

   [cgi_server_port] is the value of SERVER_PORT

   [cgi_request_method] is the value of REQUEST_METHOD

   [cgi_http_accept] is the value of HTTP_ACCEPT

   [cgi_http_user_agent] is the value of HTTP_USER_AGENT

   [cgi_http_referer] is the value of HTTP_REFERER

   [cgi_path_info] is the value of PATH_INFO

   [cgi_path_translated] is the value of PATH_TRANSLATED

   [cgi_script_name] is the value of SCRIPT_NAME

   [cgi_query_string] is the value of QUERY_STRING

   [cgi_remote_host] is the value of REMOTE_HOST

   [cgi_remote_addr] is the value of REMOTE_ADDR

   [cgi_remote_user] is the value of REMOTE_USER

   [cgi_remote_ident] is the value of REMOTE_IDENT

   [cgi_auth_type] is the value of AUTH_TYPE

   [cgi_content_type] is the value of CONTENT_TYPE

   [cgi_content_length] is the value of CONTENT_LENGTH, that is, the
   length of the data transmitted in the CGI call.

   [cgi_annotation_server] is the value of ANNOTATION_SERVER

   [cgi_http_cookie] is the value of HTTP_COOKIE

   [cgi_http_forwarded] is the value of HTTP_FORWARDED

   [cgi_http_host] is the value of HTTP_HOST

   [cgi_http_proxy_connection] is the value of HTTP_PROXY_CONNECTION

   [cgi_script_filename] is the value of SCRIPT_FILENAME

   [cgi_document_root] is the value of DOCUMENT_ROOT

   [cgi_server_admin] is the value of SERVER_ADMIN

   [cgi_api_version] is the value of API_VERSION

   [cgi_the_request] is the value of THE_REQUEST

   [cgi_request_uri] is the value of REQUEST_URI

   [cgi_request_filename] is the value of REQUEST_FILENAME

   [cgi_is_subreq] is the value of IS_SUBREQ
*)
