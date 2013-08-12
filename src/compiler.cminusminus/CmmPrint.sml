(* Pretty print of C--                                                       *)
(* Created by Ken Friis Larsen <ken@friislarsen.net> 2001-02-27                       *)
structure CmmPrint :> CmmPrint =
struct 
   type 'a printer = 'a -> Wpp.doc
   local open CmmAST Wpp in
   infix ^^ ^+^ ^/^ o
   fun f o g = fn x => f(g x)




   (* Some convenience functions for pp *)
   val $     = text
   val ws    = group(break 1 0) (* space or break*)
   val br    = break 0 0
   val sp    = break 1 0
   val nl    = break 100000 0   (* an almost certain forced newline *)
   val comma = $"," ^^ ws
   val semi  = $";"
   val colon = $":" ^^ ws

   fun x ^+^ y = x ^^ text" " ^^ y
   fun x ^/^ y = x ^^ ws ^^ y

               
   fun opr s = ws ^^ $s  ^^ ws
   val plus = opr "+"

   fun cseq ppr = seq comma ppr

   fun block i = nest i o group
   val block1 = block 1

   fun paren doc  = $ "(" ^^ (block1 doc) ^^ $ ")"
   fun cparen doc = $ "{" ^^ (block1 doc) ^^ $ "}"
   fun bracket doc = $ "[" ^^ (block1 doc) ^^ $ "]"
   
   fun opt ppr NONE     = Wpp.empty
     | opt ppr (SOME x) = ppr x

   val isSymbol = Char.contains "+=!<>-/"
   fun isInfix opr = List.all isSymbol (String.explode opr) 


   fun ppComment s = $"/*" ^/^ $s ^/^ $"*/"

   fun ppType (Bits8 ) = $"bits8"  
     | ppType (Bits16) = $"bits16"
     | ppType (Bits32) = $"bits32"
     | ppType (Bits64) = $"bits64"  

   fun ppFormal (t,r) = ppType t ^+^ $r

   val ppFormals = cseq ppFormal

   fun ppConv Cmm  = Wpp.empty
     | ppConv Ccal = $"foreign \"C\" " 

   exception can'tHandle

   fun ppAlign align = $"align" ^^ int align

   fun ppLvalue (Var name)               = $name
     | ppLvalue (Mem(t, exp, alignOpt))  =
       ppType t ^^ opt(cparen o ppAlign) alignOpt ^^ bracket(ppExp exp)

   and ppExp exp =
       case exp of
           LitInt i    => $ i
         | LitFloat f  => $ f
         | LitChar c   => $"'" ^^ $(String.translate Char.toCString c) ^^ $"'"
         | Fetch lv    => ppLvalue lv
         | Prim(name, args) => ppPrim name args
         | EComment(e, s)   => ppExp e ^/^ ppComment s

   and ppPrim opr (args as [e1, e2]) = 
       if isInfix opr 
       then paren(ppExp e1 ^/^ $opr ^/^ ppExp e2)
       else $opr ^^ paren(cseq ppExp args)
     | ppPrim name args              = $name ^^ paren(cseq ppExp args)

(*   fun ppRel rel =
       case rel of 
           EQ s => $"==" ^^ $s 
         | NE s => $"!=" ^^ $s
         | LT s => $"<"  ^^ $s 
         | LE s => $"<=" ^^ $s
         | GT s => $">"  ^^ $s 
         | GE s => $">=" ^^ $s


   fun ppCondExp (e1, rel, e2) =
       (* we don't need the parentheses but I like them *)
       paren(ppExp e1 ^/^ (ppRel rel) ^/^ ppExp e2) 
*)
   fun ppRange (low, high) = bracket(int low ^+^ $".." ^+^ int high)

   fun ppTargets [] = Wpp.empty
     | ppTargets ts = $"targets" ^+^ cseq $ ts

   fun ppFlow flow =
       case flow of
           Aborts     => $"also aborts"
         | Cuts ns    => $"also cuts to "    ^^ nest 5 (cseq $ ns)
         | Unwinds ns => $"also unwinds to " ^^ nest 5 (cseq $ ns)
         | Returns ns => $"also returns to " ^^ nest 5 (cseq $ ns)

   val ppFlows = seq ws ppFlow

   fun ppStmt stmt =
       (case stmt of
            DeclReg f                          => 
            ppFormal f ^^ semi
          | DeclStackData sd                   => raise can'tHandle
          | Assign(lv, exp)                    => 
            ppLvalue lv ^^ $" = " ^^ ppExp exp ^^ semi
          | If(cexp, stmts, stmtsOpt)          => 
            $"if" ^^ ppExp cexp ^^ ppIndentedBlock stmts ^^
            (case stmtsOpt of
                 SOME stmts => 
                 newline ^^ $"else "^^ ppIndentedBlock stmts ^^ semi 
               | NONE => semi) 
          | Switch(exp, rngOpt, swts)          => 
            $"switch" ^+^ opt ppRange rngOpt ^+^ ppExp exp ^+^
             $"{" ^^  
             block 2 (newline ^^ seq newline ppSwt swts)^^newline^^ $"}" ^^semi
          | Label l                            => 
            $l ^^ colon
          | Goto l                             => 
            $"goto " ^^ $l ^^ semi
          | ComputedGoto(exp, ls)              => 
            $"goto " ^^ ppExp exp ^+^ ppTargets ls ^^ semi
          | Jump(conv, exp, args, ts)          => 
            ppConv conv ^^ $"jump" ^+^ ppExp exp ^^ paren(cseq ppExp args) ^+^
                   ppTargets ts ^^ semi
          | Call(rs, conv, exp, args, targets, flow) => 
            (case rs of [] => Wpp.empty
                      | _  => cseq $ rs ^^ $" = ")^^ppConv conv ^^ ppExp exp ^^
                 paren(cseq ppExp args) ^+^ ppTargets targets ^+^ 
                 ppFlows flow ^^ semi
          | Return(conv, exps)                 => 
            ppConv conv ^^ $"return" ^^ paren(cseq ppExp exps) ^^ semi
          | Continuation(cont, rs)             => 
            $"continuation " ^^ $cont ^^ paren(cseq $ rs) ^^ colon
          | Cut(exp, exps, flow)                     =>
            $"cut to " ^^ ppExp exp ^^ paren(cseq ppExp exps) ^+^ 
             ppFlows flow ^^ semi
          | Comment s                          =>
            ppComment s
          | Block stmts                        =>
            seq newline ppStmt stmts 
          | SafePoint                          => Wpp.empty
          | MarkStmt _                         => Wpp.empty) 
       

   and ppIndentedBlock stmts = 
       $"{" ^^ group(nest 2 (sp ^^ seq nl ppStmt stmts) ^^sp) ^^ $"}"
       
   and ppSwt (Swt(xs, stmts))   = 
       $"case " ^^ (cseq int xs) ^+^ colon ^^ ppIndentedBlock stmts
     | ppSwt (SwtDefault stmts) = $"default : " ^^ ppIndentedBlock stmts

   fun ppImp name = $name ^+^ $"as "
   fun ppImport (nameOpt, name) = opt ppImp nameOpt ^^ $name
   fun ppImports (ty, imps) = 
       $"import " ^^ ppType ty ^+^ cseq ppImport imps ^^ semi

   fun ppExpo name = $" as " ^^ $name
   fun ppExport (name, nameOpt) = $name ^^ opt ppExpo nameOpt
   fun ppExports (tyOpt, expos) = 
       $"export "^^ opt ppType tyOpt ^+^ cseq ppExport expos ^^ semi

   fun ppTypedef (old, new) = $"typedef " ^^ $old ^+^ $new ^^ semi

   fun ppTarget (memsize, byteorder) =
       $"target" ^^ opt(fn i => $" memsize "^^ int i) memsize ^+^
       $"byteorder " ^^ (case byteorder of
                             Little => $"little"
                           | Big    => $"big") ^^ semi

   fun ppToplevel toplevel =
       case toplevel of
           Imports imps  => ppImports imps
         | Exports expos => ppExports expos
         | Typedef td    => ppTypedef td
         | Target tg     => ppTarget tg
         | Function{conv, name, formals, stmts} =>
           ppConv conv ^^ $name ^^ paren(ppFormals formals) ^^ 
           ppIndentedBlock stmts
         | _             => raise can'tHandle

   fun ppProgram topdecls = 
       seq (newline ^^ newline) ppToplevel topdecls ^^ newline
                            

   end
end
