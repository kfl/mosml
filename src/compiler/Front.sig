local
  open Lambda Smlprim Globals Asynt Tr_env
in

val trConVar : ConInfo -> Lambda;
val trTopExConVar : ExConInfo -> Lambda;
val trTopDynExConVar : ExConInfo -> Lambda -> Lambda;
val trPrimVar : SMLPrim -> Lambda;
val coerceMod : Lambda -> Mod -> Mod -> Lambda;
val coerceStr : Lambda -> Str -> Str -> Lambda;
val coerceFun : Lambda -> GenFun -> GenFun -> Lambda;

val translateToplevelDec: Dec -> RenEnv * (bool * Lambda) list;

end;
