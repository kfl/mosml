(*
 * Types for mlb file.
 *
 * In contrast to MLton parsing, each .mlb file is scanned and
 * parsed separately. The included files are loaded only after AST
 * is fully created.
 *)

(* The type of referenced file - .mlb, .sig, .sml, .fun. For some
 * .mlb files from MLton distribution we also need Unknown. The type
 * of the file is determined by lexer by extension of the file. *)
datatype includedFileType = UnknownFile | MLBFile | SIGFile | SMLFile | FUNFile

datatype funBind = FunId of string | FunBind of string*string

datatype strBind = StrId of string | StrBind of string*string

datatype sigBind = SigId of string | SigBind of string*string

type basId = string

type annotation = string

datatype basDec = Basis of basBind list | Local of (basDec list)*(basDec list)
    | Open of basId list | Structure of strBind list | Signature of sigBind list 
    | Functor of funBind list | Path of includedFileType*string 
    | Annotation of (string list)*(basDec list)
    and basBind = BasBind of (basId)*(basExp)
    and basExp = Bas of basDec list | BasId of basId | Let of (basDec list)*basExp

(* Returns the value of path variable by its name.
 * Currently works only with hardcoded predefined 
 * path variables. *)
fun pathVariable variable =
    let
        val predefinedPathVariables =
           [("SML_LIB","/sml-lib-location"),
            ("HOME_PATH","/home"), (* for automated testing, remove in future *)
            ("TARGET_ARCH", "bytecode"),
            ("TARGET_OS", "linux"),
            ("DEFAULT_INT", "int32"),
            ("DEFAULT_WORD", "word32"),
            ("DEFAULT_REAL", "real64")]
        val sub = List.find 
            (fn (name, _) => (String.compare (name, variable)) = EQUAL)
            predefinedPathVariables
    in
        case sub of
          SOME (name, value) => value
        | NONE => raise Fail ("Unknown path variable " ^ variable)
    end
