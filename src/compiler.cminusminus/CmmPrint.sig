(* Pretty print of C--                                                       *)
(* Created by Ken Friis Larsen <ken@friislarsen.net> 2001-02-13                       *)
signature CmmPrint =
sig 
    type 'a printer = 'a -> Wpp.doc

    val ppType    : CmmAST.typ     printer
    val ppExp     : CmmAST.expr    printer
    val ppStmt    : CmmAST.stmt    printer
    val ppProgram : CmmAST.program printer
end
