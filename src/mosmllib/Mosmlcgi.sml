(*

 Mosmlcgi.sml

 (c) Jonas Barklund, Computing Science Dept., Uppsala University, 1996.

 Support for form-based file upload via multipart/form-data,
 by Peter Sestoft (sestoft@dina.kvl.dk) December 1996.

 Anyone is granted the right to copy and/or use this code, provided
 that this note is retained, also in modified versions.  The code is
 provided as is with no guarantee about any functionality.  I take no
 responsibility for its proper function.

*)

val cgi_server_software = Process.getEnv("SERVER_SOFTWARE");
val cgi_server_name = Process.getEnv("SERVER_NAME");
val cgi_gateway_interface = Process.getEnv("GATEWAY_INTERFACE");
val cgi_server_protocol = Process.getEnv("SERVER_PROTOCOL");
val cgi_server_port = Process.getEnv("SERVER_PORT");
val cgi_request_method = Process.getEnv("REQUEST_METHOD");
val cgi_http_accept = Process.getEnv("HTTP_ACCEPT");
val cgi_http_user_agent = Process.getEnv("HTTP_USER_AGENT");
val cgi_http_referer = Process.getEnv("HTTP_REFERER");
val cgi_path_info = Process.getEnv("PATH_INFO");
val cgi_path_translated = Process.getEnv("PATH_TRANSLATED");
val cgi_script_name = Process.getEnv("SCRIPT_NAME");
val cgi_query_string = Process.getEnv("QUERY_STRING");
val cgi_remote_host = Process.getEnv("REMOTE_HOST");
val cgi_remote_addr = Process.getEnv("REMOTE_ADDR");
val cgi_remote_user = Process.getEnv("REMOTE_USER");
val cgi_remote_ident = Process.getEnv("REMOTE_IDENT");
val cgi_auth_type = Process.getEnv("AUTH_TYPE");
val cgi_content_type = Process.getEnv("CONTENT_TYPE");
val cgi_content_length = Process.getEnv("CONTENT_LENGTH");
val cgi_annotation_server = Process.getEnv("ANNOTATION_SERVER");

(* Added by Hans Molin, Uppsala, 1999 *)
val cgi_http_cookie = Process.getEnv("HTTP_COOKIE");
val cgi_http_forwarded = Process.getEnv("HTTP_FORWARDED");
val cgi_http_host = Process.getEnv("HTTP_HOST");
val cgi_http_proxy_connection = Process.getEnv("HTTP_PROXY_CONNECTION");

val cgi_script_filename = Process.getEnv("SCRIPT_FILENAME");

val cgi_document_root = Process.getEnv("DOCUMENT_ROOT");
val cgi_server_admin = Process.getEnv("SERVER_ADMIN");

val cgi_api_version = Process.getEnv("API_VERSION");
val cgi_the_request = Process.getEnv("THE_REQUEST");
val cgi_request_uri = Process.getEnv("REQUEST_URI");
val cgi_request_filename = Process.getEnv("REQUEST_FILENAME");
val cgi_is_subreq = Process.getEnv("IS_SUBREQ");

local
    open Option TextIO

    fun intOf NONE     = NONE
      | intOf (SOME s) = Int.fromString s

    val query_string = 
	case cgi_request_method of
	    SOME ("GET")  => getOpt(cgi_query_string,"")
	  | SOME ("POST") => inputN(stdIn, getOpt(intOf cgi_content_length, 0))
	  | _             => getOpt(cgi_query_string,"");     (* Perhaps GET *)

    fun isn't c1 c2 = c1 <> c2
    fun is    c1 c2 = c1 = c2

    (* For debugging, one may log to the httpd error_log: *)

    fun err s = TextIO.output(TextIO.stdErr, s);

    (* val _ = err query_string;
       val _ = err (Int.toString  (getOpt(intOf cgi_content_length, 0)));
     *)

    (* Get the line starting with string s *)

    fun line s sus = 
	let open Substring
	    val (_, fullline) = position s sus
	    val after = triml (String.size s) fullline
	in takel (fn c => c <> #"\r" andalso c <> #"\n") after end

    (* Get the value of boundary *)

    fun getboundary line = 
	let open Substring
	    val (_, bndeqn) = position "boundary=" line
	in 
	    if isEmpty bndeqn then NONE
	    else SOME (string (triml 1 (dropl (isn't #"=") bndeqn)))
	end
        handle Option => NONE

    (* If CGI request type is multipart/form-data, then SOME(boundary):  *)

    val multipart_boundary = 
	let open Substring
	    val content_type = all (valOf cgi_content_type)
	in 
	    if isPrefix "multipart/form-data;" content_type then 
		getboundary content_type
	    else
		NONE
	end
        handle Option => NONE

    val the_fields =
	case multipart_boundary of
	    NONE => Substring.tokens (is #"&") (Substring.all query_string)
	  | _    => []

    val dict_with_codes = List.map (Substring.fields (is #"=")) the_fields;

    (* Decode CGI parameters: *)

    fun decode(sus) =
        let
            val sz = Substring.size(sus);
            exception Dehex;
            fun dehex(ch) =
                if #"0" <= ch andalso ch <= #"9"
                    then Char.ord(ch) - Char.ord(#"0")
                else if #"A" <= ch andalso ch <= #"F"
                         then (Char.ord(ch) - Char.ord(#"A")) + 10
                     else if #"a" <= ch andalso ch <= #"f"
                              then (Char.ord(ch) - Char.ord(#"a")) + 10
                          else raise Dehex;
            fun decode_one(i) =
                Char.chr(16*dehex(Substring.sub(sus,i+1))+
		         dehex(Substring.sub(sus,i+2)));
            fun dec(i) =
                if i>=sz then []
                else case Substring.sub(sus,i)
                       of #"+" => #" "::dec(i+1)
                        | #"%" => decode_one(i)::dec(i+3)
                        | ch => ch::dec(i+1);
        in
            String.implode(dec(0))
        end handle exn => 
	    (err ("decode failed on " ^ Substring.string sus ^ "\n"); "")

    fun addItem ((key, value), dict) =
	Splaymap.insert(dict, key, case Splaymap.peek(dict, key) of
			                SOME vs => value :: vs 
				      | NONE    => [value])

    fun addField ([keysrc, valsrc], dict) =
	addItem ((decode keysrc, decode valsrc), dict)
      | addField (_, dict) = dict

    val cgi_dict =
        List.foldr addField (Splaymap.mkDict String.compare) dict_with_codes;

    fun keys dict = Splaymap.foldr (fn (key, _, res) => key :: res) [] dict

    (* Decode multipart messages: *)

    fun part_fields dict name = 
	case Splaymap.peek (dict, name) of
	    NONE      => []
	  | SOME vals => vals

    fun part_field dict name =
	case Splaymap.peek (dict, name) of
	    SOME (v :: _) => SOME v
	  | _             => NONE

    fun getint NONE       default = default
      | getint (SOME str) default =
	case Int.scan StringCvt.DEC Substring.getc (Substring.all str) of
	    NONE          => default
	  | SOME(i, rest) => if Substring.isEmpty rest then i else default

    val multiparts = 
	let open Substring
	    val boundary = "--" ^ valOf multipart_boundary
	    val skipbnd = dropl (isn't #"\n") 
	    val (_, contents) = position boundary (all query_string)
	    fun loop rest =
		let val (pref, suff) = position boundary rest
		in 
		    if isEmpty pref orelse isEmpty suff then []
		    else pref :: loop (skipbnd suff)
		end
	in loop (skipbnd contents) end
        handle Option => []

    fun decodepart (part : Substring.substring) = 
	let open Char Substring
	    val crlf2 = "\r\n\r\n"
	    val (header, rest) = position crlf2 part
	    val eqnsrc = line "Content-Disposition: form-data;" header
	    val typ = line "Content-Type: " header
	    val equations = List.map (fn f => dropl isSpace (dropr isSpace f))
		                     (fields (is #";") eqnsrc)

	    fun addField (eqn, dict) =
		let val (name, v) = splitl (isn't #"=") eqn
		   (* Drop equals sign and quotes from value *)
		    val value = triml 2 (trimr 1 v)
		in addItem((string name, string value), dict) end

	    val dict = 
		List.foldr addField (Splaymap.mkDict String.compare) equations

	    val partname = 
		case part_field dict "name" of
		    NONE   => "[Anonymous]" (* Is this is good idea? *)
		  | SOME n => n
	in 
	    (partname, 
	     { fieldnames = keys dict,
	       tyOpt = if isEmpty typ then NONE else SOME (string typ),
	       dict  = dict, 
	       (* Strip off CRLFCRLF and CRLF *)
	       data  = string (trimr 2 (triml 4 rest))
	     })
	end

    type part = {fieldnames : string list, 
		 tyOpt : string option, 
		 dict : (string, string list) Splaymap.dict,
		 data : string}
	
    val part_dict : (string, part list) Splaymap.dict =
	List.foldr addItem (Splaymap.mkDict String.compare) 
	              (List.map decodepart multiparts)
in
    type part = part
    val cgi_partnames = keys part_dict
    fun cgi_part  name = part_field  part_dict name
    fun cgi_parts name = part_fields part_dict name

    fun part_fieldnames    (p : part) = #fieldnames p
    fun part_type          (p : part) = #tyOpt p
    fun part_data          (p : part) = #data p
    fun part_field_strings (p : part) name = part_fields (#dict p) name
    fun part_field_string  (p : part) name = part_field  (#dict p) name
    fun part_field_integer (p : part) (name, default) = 
	getint (part_field  (#dict p) name) default

    val cgi_fieldnames = keys cgi_dict
    fun cgi_field_strings name = part_fields cgi_dict name
    fun cgi_field_string name  = part_field cgi_dict name
    fun cgi_field_integer (name, default) = 
	getint (cgi_field_string name) default
end;
