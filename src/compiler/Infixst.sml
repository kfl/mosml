(* This file has been derived from the ML Kit. *)

open Fnlib Mixture Const Globals Location Asynt;

exception WrongInfix;
exception MixedAssociativity;

type 'Obj InfixStackStr = {
    applyId : IdInfo -> 'Obj -> 'Obj,
    applyObj : 'Obj -> 'Obj -> 'Obj,
    asId : 'Obj -> IdInfo option,
    pair : 'Obj -> 'Obj -> 'Obj
};

datatype StackEntry =
    INFIXentry of IdInfo * int
  | INFIXRentry of IdInfo * int
  | APPentry
;

datatype LastObj = ARG | OPER | VOID;

fun resolveInfix (iStackStr : 'Obj InfixStackStr) statusOfId objs =

  let 
    val { pair, asId, applyId, applyObj } = iStackStr

    fun apply entry (o2 :: o1 :: rest) =
          let val thePair = pair o1 o2 in
            ( case entry of
                  INFIXentry(ii, n) => applyId ii thePair
                | INFIXRentry(ii, n) => applyId ii thePair
                | APPentry => applyObj o1 o2
            ) :: rest
          end
      | apply entry output =
          raise WrongInfix

    and assocLeft APPentry _ = true
      | assocLeft _ APPentry = false
      | assocLeft op1 op2 =
          let fun extract (INFIXentry(_, n))  = (n, true)
                | extract (INFIXRentry(_, n)) = (n, false)
                | extract _ = raise WrongInfix
              val (prec1, left1) = extract op1
              val (prec2, left2) = extract op2 
          in
            if prec1 > prec2 then true
            else if prec1 < prec2 then false
            else if left1 = left2 then left1
            else raise MixedAssociativity
	  end

    and flushHigher entry stack output =
      case stack of
          [] => ([], output)
        | top :: rest =>
            if assocLeft top entry then
              flushHigher entry rest (apply top output)
            else
              (stack, output)

    and flushAll stack output =
      case stack of
          [] => ( case output of
                      [item] => item
                    | _ => raise WrongInfix )
        | top :: rest => flushAll rest (apply top output)

    and process input stack last output =
      case input of
          [] =>
            flushAll stack output
        | this :: rest =>
            ( case asId this of
                    SOME ii =>
                      ( case statusOfId (List.hd (#id(#qualid ii))) of
                            INFIXst n =>
                              operator (INFIXentry(ii,n))
                                        rest stack output
                          | INFIXRst n =>
                              operator (INFIXRentry(ii,n))
                                        rest stack output
                          | NONFIXst =>
                              ( case last of
                                    ARG => operator APPentry
                                                    input stack output
                                  | _ => process rest stack ARG
                                                  (this :: output) ) )
                  | NONE =>
                      ( case last of
                            ARG =>
                              operator APPentry input stack output
                          | _ =>
                              process rest stack ARG (this :: output)
                      ) )

    and operator entry input stack output =
      let val (stack', output') = flushHigher entry stack output 
      in process input (entry :: stack') OPER output' end

  in process objs [] VOID [] end
;
