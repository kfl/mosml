exception Toplevel;
exception EndOfFile;
exception LexicalError of string * int * int;

val toplevel : bool ref;

val pp_out : ppstream;
val msgCBlock : int -> unit; (* Begins a CONSISTENT block. *)
val msgIBlock : int -> unit; (* Begins an INCONSISTENT block. *)
val msgEBlock : unit -> unit;
val msgBreak : int * int -> unit;
val msgClear : unit -> unit;
val msgFlush : unit -> unit;

val msgPrompt : string -> unit;
val msgContPrompt : string -> unit;
val errPrompt : string -> unit;
val msgString : string -> unit;
val msgChar : char -> unit;
val msgInt : int -> unit;
val msgReal : real -> unit;
val msgWord : word -> unit;
val msgEOL : unit -> unit;

val msgStyle : string ref;

val path_library : string ref;
val value_polymorphism : bool ref;
val load_path : string list ref;
val file_exists : string -> bool;
val find_in_path : string -> string;
val remove_file : string -> unit;

datatype Lab =
    INTlab of int
  | STRINGlab of string
;

type 'a Row = (Lab * 'a) list;

val printLab : Lab -> unit;
val isPairRow : 'a Row -> bool;
val isTupleRow : 'a Row -> bool;
val mkPairRow : 'a -> 'a -> 'a Row;
val mkTupleRow : 'a list -> 'a Row;
val lt_lab : Lab -> Lab -> bool;
val insertField : Lab * 'a -> 'a Row -> 'a Row;
val sortRow : 'a Row -> 'a Row;

datatype ('a, 'b) Env
  = NILenv
  | BNDenv of 'a * 'b * ('a, 'b) Env
  | TOPenv of ('a, 'b) Hasht.t * ('a, 'b) Env
  | COMPenv of ('a, 'b) Env * ('a, 'b) Env
;

val plusEnv : ('a, 'b) Env -> ('a, 'b) Env -> ('a, 'b) Env;
val lookupEnv : (''a, 'b) Env -> ''a -> 'b;
val bindInEnv : ('a, 'b) Env -> 'a -> 'b -> ('a, 'b) Env;
val bindTopInEnv : ('a, 'b) Env -> ('a, 'b) Hasht.t -> ('a, 'b) Env;
val mkHashEnv : int -> (''a, 'b) Env -> (''a, 'b) Env
val mk1Env : 'a -> 'b -> ('a, 'b) Env;
val mk1TopEnv : ('a, 'b) Hasht.t -> ('a, 'b) Env;
val revEnvAcc : ('a, 'b) Env -> ('a, 'b) Env -> ('a, 'b) Env;
val revEnv : ('a, 'b) Env -> ('a, 'b) Env;
val traverseEnv : (''_a -> 'b -> unit) -> (''_a, 'b) Env -> unit;
val mapEnv : (''_a -> 'b -> 'c) -> (''_a, 'b) Env -> (''_a, 'c) Env;
val foldEnv : (''_a -> 'b -> 'c -> 'c) -> 'c -> (''_a, 'b) Env -> 'c;
val cleanEnv : (''_a, 'b) Env -> (''_a, 'b) Env; 
val sortEnv : (string, 'b) Env -> (string,'b) Env;
val lookupEnvWithPos : ('b -> int) -> 
                       (''a, 'b) Env -> ''a -> int -> (int * 'b);

type SigStamp = string;
val dummySigStamp : SigStamp;
val watchDog : (string, SigStamp) Hasht.t ref;

val preloadedUnits : string list ref;
val preopenedPreloadedUnits : string list ref;

(* current compilation mode for units
   STRmode units are compiled as structures
   TOPDECmode if units are compiled as topdecs
   should be false for boostrapping!
*)

datatype Mode = STRmode | TOPDECmode
;
val currentMode : Mode ref
;

(* vanilla SML compliance levels *)

datatype Compliance = 
    Orthodox (* SML only, reject extensions *)
  | Conservative (* warn of any extensions *)
  | Liberal (* anything goes *);

val currentCompliance : Compliance ref;

