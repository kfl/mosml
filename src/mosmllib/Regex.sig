(* Regex -- regular expressions a la POSIX 1003.2 -- requires Dynlib *)

exception Regex of string

type regex                      (* A compiled regular expression         *)

datatype cflag = 
    Extended                    (* Compile POSIX extended REs            *)
  | Icase                       (* Compile case-insensitive match        *)
  | Newline                     (* Treat \n in target string as new line *)

datatype eflag = 
    Notbol                      (* Do not match ^ at beginning of string *)
  | Noteol                      (* Do not match $ at end of string       *)

val regcomp      : string -> cflag list -> regex

val regexec      : regex -> eflag list -> string -> substring vector option
val regexecBool  : regex -> eflag list -> string -> bool

val regnexec     : regex -> eflag list -> substring 
                   -> substring vector option
val regnexecBool : regex -> eflag list -> substring -> bool

val regmatch     : { pat : string, tgt : string } -> cflag list 
                     -> eflag list -> substring vector option
val regmatchBool : { pat : string, tgt : string } -> cflag list 
                     -> eflag list -> bool

datatype replacer =
    Str of string                       (* A literal string             *)
  | Sus of int                          (* The i'th parenthesized group *)
  | Tr  of (string -> string) * int     (* Transformation of i'th group *)
  | Trs of substring vector -> string   (* Transformation of all groups *)

val replace1     : regex -> replacer list -> string -> string
val replace      : regex -> replacer list -> string -> string

val substitute1  : regex -> (string -> string) -> string -> string
val substitute   : regex -> (string -> string) -> string -> string

val tokens       : regex -> string -> substring list
val fields       : regex -> string -> substring list

val map          : regex -> (substring vector -> 'a) -> string -> 'a list
val app          : regex -> (substring vector -> unit) -> string -> unit
val fold         : regex 
                   -> (substring * 'a -> 'a) * (substring vector * 'a -> 'a) 
                   -> 'a -> string -> 'a

(* 
   This structure provides pattern matching with POSIX 1003.2 regular
   expressions.  

   The form and meaning of Extended and Basic regular expressions are
   described below.  Here R and S denote regular expressions; m and n
   denote natural numbers; L denotes a character list; and d denotes a
   decimal digit:

        Extended    Basic       Meaning
       ---------------------------------------------------------------
        c           c           Match the character c
        .           .           Match any character
        R*          R*          Match R zero or more times
        R+          R\+         Match R one or more times
        R|S         R\|S        Match R or S
        R?          R\?         Match R or the empty string
        R{m}        R\{m\}      Match R exactly m times
        R{m,}       R\{m,\}     Match R at least m times
        R{m,n}      R\{m,n\}    Match R at least m and at most n times
        [L]         [L]         Match any character in L
        [^L]        [^L]        Match any character not in L
        ^           ^           Match at string's beginning
        $           $           Match at string's end
        (R)         \(R\)       Match R as a group; save the match
        \d          \d          Match the same as previous group d
        \\          \\          Match \ --- similarly for *.[]^$
        \+          +           Match + --- similarly for |?{}()

   Some example character lists L:

        [aeiou]         Match vowel: a or e or i or o or u
        [0-9]           Match digit: 0 or 1 or 2 or ... or 9
        [^0-9]          Match non-digit
        [-+*/^]         Match - or + or * or / or ^
        [-a-z]          Match lowercase letter or hyphen (-)
        [0-9a-fA-F]     Match hexadecimal digit
        [[:alnum:]]     Match letter or digit
        [[:alpha:]]     Match letter 
        [[:cntrl:]]     Match ASCII control character
        [[:digit:]]     Match decimal digit; same as [0-9]
        [[:graph:]]     Same as [:print:] but not [:space:]
        [[:lower:]]     Match lowercase letter
        [[:print:]]     Match printable character
        [[:punct:]]     Match punctuation character
        [[:space:]]     Match SML #" ", #"\r", #"\n", #"\t", #"\v", #"\f"
        [[:upper:]]     Match uppercase letter
        [[:xdigit:]]    Match hexadecimal digit; same as [0-9a-fA-F]
        [[:lower:]זרו]  Match lowercase Danish letters (ISO Latin 1)

   Remember that backslash (\) must be escaped as "\\" in SML strings.

   [regcomp pat cflags] returns a compiled representation of the
   regular expression pat.  Raises Regex in case of failure.  

   [cflag] is the type of compilation flags with the following meanings:

   [Extended] : compile as POSIX extended regular expression.
   [Icase]    : compile case-insensitive match.
   [Newline]  : make the newline character \n significant, so ^ matches 
                just after newline (\n), and $ matches just before \n.

   Example: Match SML integer constant:
   regcomp "^~?[0-9]+$" [Extended] 

   Example: Match SML alphanumeric identifier:
   regcomp "^[a-zA-Z0-9][a-zA-Z0-9'_]*$" [Extended]

   Example: Match SML floating-point constant:
   regcomp "^[+~]?[0-9]+(\\.[0-9]+|(\\.[0-9]+)?[eE][+~]?[0-9]+)$" [Extended]

   Example: Match any HTML start tag; make the tag's name into a group:
   regcomp "<([[:alnum:]]+)[^>]*>" [Extended]

   [regexec regex eflags s] returns SOME(vec) if some substring of s
   matches regex, NONE otherwise.  In case of success, vec is the
   match vector, a vector of substrings such that vec[0] is the
   (longest leftmost) substring of s matching regex, and vec[1],
   vec[2], ... are substrings matching the parenthesized groups in pat
   (numbered 1, 2, ... from left to right in the order of their
   opening parentheses).  For a group that does not take part in the
   match, such as (ab) in "(ab)|(cd)" when matched against the string
   "xcdy", the corresponding substring is the empty substring at the
   beginning of the underlying string.  For a group that takes part in
   the match repeatedly, such as the group (b+) in "(a(b+))*" when
   matched against "babbabbb", the corresponding substring is the last
   (rightmost) one matched.  

   [eflag] is the type of end flags with the following meaning:

   [Notbol] : do not match ^ at beginning of string.
   [Noteol] : do not match $ at end of string.

   [regexecBool regex eflags s] returns true if some substring of s
   matches regex, false otherwise.  Equivalent to, but faster than, 
   Option.isSome(regexec regexec eflags s).

   [regnexec regex eflags sus] returns SOME(vec) if some substring of
   sus matches regex, NONE otherwise.  The substrings returned in the
   vector vec will have the same base string as sus.  Useful e.g. for
   splitting a string into fragments separated by substrings matching
   some regular expression.

   [regnexecBool regex eflags sus] returns true if some substring of
   sus matches regex, false otherwise.  Equivalent to, but faster than, 
   Option.isSome(regnexec regexec eflags sus).

   [regmatch { pat, tgt } cflags eflags] is equivalent to 
         regexec (regcomp pat cflags) eflags tgt
   but more efficient when the compiled regex is used only once.

   [regmatchBool { pat, tgt } cflags eflags] is equivalent to 
         regexecBool (regcomp pat cflags) eflags tgt 
   but more efficient when the compiled regex is used only once.

   [replace regex repl s] finds the (disjoint) substrings of s
   matching regex from left to right, and returns the string obtained
   from s by applying the replacer list repl to every such substring
   (see below).  Raises Regex if it fails to make progress in
   decomposing s, that is, if regex matches an empty string at the
   head of s or immediately after a previous regex match.
   Example use: delete all HTML tags from s: 
        replace (regcomp "<[^>]+>" [Extended]) [] s

   [replace1 regex repl s] finds the leftmost substring b1 of s
   matching regex, and returns the string resulting from s by applying
   the replacer list repl to the match vector vec1 (see below).

   Let x0 be a substring matching the entire regex and xi be the
   substring matching the i'th parenthesized group in regex; thus xi =
   vec[i] where vec is the match vector (see regexec above).  Then a
   single replacer evaluates to a string as follows:

   [Str s]      gives the string  s
   [Sus i]      gives the string  xi
   [Tr (f, i)]  gives the string  f(xi)
   [Trs f]      gives the string  f(vec)

   A replacer list repl evaluates to the concatenation of the results
   of the replacers.  The replacers are applied from left to right.

   [substitute regex f s] finds the (disjoint) substrings b1, ..., bn
   of s matching regex from left to right, and returns the string
   obtained from s by replacing every bi by f(bi).  Function f is
   applied to the matching substrings from left to right.  Raises
   Regex if it fails to make progress in decomposing s.  Equivalent to
        replace regex [Tr (f, 0)] s

   [substitute1 regex f s] finds the leftmost substring b of s
   matching regex, and returns the string obtained from s by replacing
   that substring by f(b).  Equivalent to 
        replace1 regex [Tr (f, 0)] s

   [map regex f s] finds the (disjoint) substrings of s matching regex
   from left to right, applies f to the match vectors vec1, ..., vecn,
   and returns the list [f(vec1), ..., f(vecn)].  Raises Regex if it
   fails to make progress in decomposing s.

   [app regex f s] finds the (disjoint) substrings of s matching regex
   from left to right, and applies f to the match vectors vec1, ...,
   vecn.  Raises Regex if the regex fails to make progress in
   decomposing s.

   [fields regex s] returns the list of fields in s, from left to
   right.  A field is a (possibly empty) maximal substring of s not
   containing any delimiter.  A delimiter is a maximal substring that
   matches regex.  The eflags Notbol and Noteol are set.  Raises Regex
   if it fails to make progress in decomposing s.
   Example use: 
        fields (regcomp " *; *" []) "56; 23 ; 22;; 89; 99"

   [tokens regex s] returns the list of tokens in s, from left to
   right.  A token is a non-empty maximal substring of s not
   containing any delimiter.  A delimiter is a maximal substring that
   matches regex.  The eflags Notbol and Noteol are set.  Raises Regex
   if it fails to make progress in decomposing s.  Equivalent to 
        List.filter (not o Substring.isEmpty) (fields regex s)

   Two tokens may be separated by more than one delimiter, whereas two
   fields are separated by exactly one delimiter.  If the only delimiter 
   is the character #"|", then
        "abc||def" contains three fields: "abc" and "" and "def"
        "abc||def" contains two tokens:   "abc" and "def"

   [fold regex (fa, fb) e s] finds the (disjoint) substrings b1, ...,
   bn of s matching regex from left to right, and splits s into the
   substrings 
        a0, b1, a1, b2, a2, ..., bn, an         
   where n >= 0 and where a0 is the (possibly empty) substring of s
   preceding the first match, and ai is the (possibly empty) substring
   between the matches bi and b(i+1).  Then it computes and returns
        fa(an, fb(vecn, ..., fa(a1, fb(vec1, fa(a0, e))) ...))  
   where veci is the match vector corresponding to bi.  Raises Regex
   if it fails to make progress in decomposing s.

   If we define the auxiliary functions
        fun fapp f (x, r) = f x :: r
        fun get i vec = Substring.string(Vector.sub(vec, i))
   then 
        map regex f s  = List.rev (fold regex (#2, fapp f) [] s)
        app regex f s  = fold regex (ignore, f o #1) () s
        fields regex s = List.rev (fold regex (op ::, #2) [] s)
        substitute regex f s = 
           Substring.concat(List.rev 
              (fold regex (op ::, fapp (Substring.all o f o get 0)) [] s))
*)
