(* Meta -- functions available only in interactive Moscow ML sessions *)

val printVal         : 'a -> 'a
val printDepth       : int ref
val printLength      : int ref
val installPP        : (ppstream -> 'a -> unit) -> unit

val liberal          : unit -> unit
val conservative     : unit -> unit
val orthodox         : unit -> unit

val use              : string -> unit
val compile          : string -> unit
val compileToplevel  : string list -> string -> unit
val compileStructure : string list -> string -> unit

val load             : string -> unit
val loadOne          : string -> unit
val loaded           : unit -> string list
val loadPath         : string list ref

val quietdec         : bool ref
val verbose          : bool ref

val quotation        : bool ref
val valuepoly        : bool ref

val quit             : unit -> 'a

(* 
   These values and functions are available in the Moscow ML
   interactive system only.
 
   [printVal e] prints the value of expression e to standard output
   exactly as it would be printed at top-level, and returns the value
   of e.  Output is flushed immediately.  This function is provided as
   a simple debugging aid.  The effect of printVal is similar to that
   of `print' in Edinburgh ML or Umeaa ML.  For string arguments, the 
   effect of SML/NJ print can be achieved by the function 
   TextIO.print : string -> unit.

   [printDepth] determines the depth (in terms of nested constructors,
   records, tuples, lists, and vectors) to which values are printed by
   the top-level value printer and the function printVal.  The components
   of the value whose depth is greater than printDepth are printed as
   `#'.  The initial value of printDepth is 20. This value can be
   changed at any moment, by evaluating, for example,
        printDepth := 17;

   [printLength] determines the way in which list values are printed
   by the top-level value printer and the function printVal.  If the
   length of a list is greater than printLength, then only the first
   printLength elements are printed, and the remaining elements are
   printed as `...'.  The initial value of printLength is 200.  This
   value can be changed at any moment, by evaluating, for example,
        printLength := 500;

   [quit ()] quits Moscow ML immediately.

   [installPP pp] installs the prettyprinter pp : ppstream -> ty -> unit 
   at type ty.  The type ty must be a nullary (parameter-less) type
   constructor representing a datatype, either built-in (such as bool)
   or user-defined.  Whenever a value of type ty is about to be
   printed by the interactive system, or function printVal is invoked
   on an argument of type ty, the pretty-printer pp will be invoked to
   print it.  See library unit PP for more information.

   [use "f"] causes ML declarations to be read from file f as if they
   were entered from the console.  A file loaded by use may, in turn,
   evaluate calls to use.  For best results, use `use' only at top
   level, or at top level within a use'd file.

   [liberal ()] sets liberal mode for the compilation functions:
   accept (without warnings) all extensions to the SML Modules
   language.  The extensions are: higher-order modules (functors
   defined within structures and functors); first-order modules
   (structures can be packed as values, and values can be unpacked as
   structures); and recursively defined modules (signatures and
   structures). The liberal, conservative, and orthodox modes affect
   the functions compile, compileStructure, and compileToplevel.  The
   liberal mode may be set also by the mosml option -liberal.

   [conservative ()] sets conservative mode for the compilation
   functions: accept all extensions to the SML Modules language, but
   issue a warning for each use.  The conservative mode may be set
   also by the mosml option -conservative.  This is the default.

   [orthodox ()] sets orthodox mode for the compilation functions:
   reject all uses of the extensions to the SML Modules language.
   That is, accept only SML Modules syntax.  The orthodox mode may be
   set also by the mosml option -orthodox.

   [compile "U.sig"] will compile and elaborate the specifications in
   file U.sig in structure mode, producing a compiled signature U in
   file U.ui.  This function is backwards compatible with Moscow ML
   1.44 and earlier.  Equivalent to compileStructure [] "U.sig".

   [compile "U.sml"] will elaborate and compile the declarations in
   file U.sml in structure mode, producing a compiled structure U in
   bytecode file U.uo.  If there is an explicit signature file U.sig,
   then file U.ui must exist, and the unit body must match the
   signature.  If there is no U.sig, then an inferred signature file
   U.ui will be produced also.  No evaluation takes place.  This
   function is backwards compatible with Moscow ML 1.44 and earlier.
   Equivalent to compileStructure [] "U.sml".

   The declared identifiers will be reported if verbose is true (see
   below); otherwise compilation will be silent.  In any case,
   compilation warnings are reported, and compilation errors abort the
   compilation and raise the exception Fail with a string argument.

   [compileStructure opnunits "U.sig"] compiles the specifications
   in file U.sig as if they form a signature declaration
        signature U = sig ... contents of U.sig ... end
   The contents of opnunits is added to the compilation context in
   which the specifications in U.sig are compiled.  The result is a
   compiled signature file U.ui.  This
   corresponds to invoking the batch compiler as follows:
        mosmlc -c U1.ui ... Un.ui -structure U.sig
   where opnunits equals ["U1", ..., "Un"]. 

   [compileStructure opnunits "U.sml"] compiles the declarations in
   file U.sml as if they formed a structure declaration 
        structure U = struct ... contents of U.sml ... end
   The contents of opnunits is added to the compilation context in
   which the declarations in U.sml are compiled.  If U.ui exists
   already and represents a signature called U, then the compiled
   declarations are matched against it.  The result is a bytecode file
   U.uo.  If no file U.ui existed, then also a file U.ui is created,
   containing the inferred signature of structure U.  This
   corresponds to invoking the batch compiler as follows:
        mosmlc -c U1.ui ... Un.ui -structure U.sml
   where opnunits equals ["U1", ..., "Un"]. 

   [compileToplevel opnunits "U.sig"] compiles the specifications in
   file U.sig, in a context in which all declarations from opnunits
   are visible, creating a compiled signature file U.ui.  This
   corresponds to invoking the batch compiler as follows:
        mosmlc -c U1.ui ... Un.ui -toplevel U.sig
   where opnunits equals ["U1", ..., "Un"]. 

   [compileToplevel opnunits "U.sml"] compiles the declarations in
   file U.sml, in a context in which all declarations from opnunits
   are visible, creating a bytecode file U.uo.  If U.ui exists
   already, then the compiled declarations are matched against it;
   otherwise the file U.ui is created.  This corresponds to invoking the
   batch compiler as follows
        mosmlc -c U1.ui ... Un.ui -toplevel U.sml
   where opnunits equals ["U1", ..., "Un"]. 

   [load "U"] will load and evaluate the compiled unit body from file
   U.uo.  The resulting values are not reported, but exceptions are
   reported, and cause evaluation and loading to stop.  If U is
   already loaded, then load "U" has no effect.  If any other unit is
   mentioned by U but not yet loaded, then it will be loaded
   automatically before U.

   After loading a unit, it can be opened with `open U'.  Opening it
   at top-level will list the identifiers declared in the unit.

   When loading U, it is checked that the signatures of units
   mentioned by U agree with the signatures used when compiling U, and
   it is checked that the signature of U has not been modified since U
   was compiled; these checks are necessary for type safety.  The
   exception Fail is raised if these signature checks fail, or if the
   file containing U or a unit mentioned by U does not exist.

   [loadOne "U"] is similar to `load "U"', but raises exception Fail
   if U is already loaded or if some unit mentioned by U is not yet
   loaded.  That is, it does not automatically load any units
   mentioned by U.  It performs the same signature checks as `load'.

   [loaded ()] returns a list of the names of all compiled units that
   have been loaded so far.  The names appear in some random order.

   [loadPath] determines the load path: which directories will be
   searched for interface files (.ui files), bytecode files (.uo
   files), and source files (.sml files).  This variable affects the
   load, loadOne, and use functions.  The current directory is always
   searched first, followed by the directories in loadPath, in order.
   By default, only the standard library directory is in the list, but
   if additional directories are specified using option -I, then these
   directories are prepended to loadPath.

   [quietdec] when true, turns off the interactive system's prompt and
   responses, except warnings and error messages.  Useful for writing
   scripts in SML.  The default value is false; can be set to true
   with the -quietdec command line option.

   [verbose] determines whether the signature inferred by a call to
   compile will be printed.  The printed signature follows the syntax
   of Moscow ML signatures, so the output of compile "U.sml" can be
   edited to subsequently create file U.sig.  The default value is
   ref false.

   [quotation] determines whether quotations and antiquotations are
   permitted in declarations entered at top-level and in files
   compiled with compile.  A quotation is a piece of text surrounded
   by backquote characters `a b c` and is used to embed object
   language phrases in ML programs; see the Moscow ML Owner's Manual
   for a brief explanation of quotations.  When quotation is false,
   the backquote character is an ordinary symbol which can be used in
   ML symbolic identifiers.  When quotation is true, the backquote
   character is illegal in symbolic identifiers, and a quotation `a b
   c` will be recognized by the parser and evaluated to an object of
   type 'a General.frag list.  False by default.

   [valuepoly] determines whether value polymorphism is used or not in
   the type checker.  With value polymorphism (the default), there is
   no distinction between imperative ('_a) and applicative ('a) type
   variables, and type variables are generalized only in bindings to
   non-expansive expressions.  Non-generalized type variables are left
   free, to be instantiated when the bound identifier is used.  An
   expression is non-expansive if it is a variable, a special
   constant, a function, a tuple or record of non-expansive
   expressions, a parenthesized or typed non-expansive expression, or
   the application of an exception or value constructor (other than
   ref) to a non-expansive expression.  If valuepoly is false, then
   the type checker will distinguish imperative and applicative type
   variables, generalize all applicative type variables, and
   generalize imperative type variables only in non-expansive
   expressions.  True by default.  
*)
