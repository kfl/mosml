local
  open Lambda Location Asynt Tr_env;
in

val translateMatch :
  TranslEnv -> (unit -> Lambda) ->
  Location -> (Pat list * Lambda) list -> Lambda

end;
