local
  open Fnlib Mixture Globals Location Asynt;
in

exception WrongInfix and MixedAssociativity;

type 'Obj InfixStackStr = {
    applyId : IdInfo -> 'Obj -> 'Obj,
    applyObj : 'Obj -> 'Obj -> 'Obj,
    asId : 'Obj -> IdInfo option,
    pair : 'Obj -> 'Obj -> 'Obj
};

val resolveInfix :
  'Obj InfixStackStr -> (string -> InfixStatus) ->
      'Obj list -> 'Obj
;

end;
