fun main () =
    let val dev = TextIO.openIn"c:\\autoexec.bat"
        val stop = "REM Do NOT delete this line.  This line and the following two lines will be deleted unconditional by the Moscow ML uninstaller \n"
        fun readlines acc =
            let val line = TextIO.inputLine dev
            in  if line = "" then acc
                else if line = stop then 
                     ( TextIO.inputLine dev
                     ; TextIO.inputLine dev
                     ; TextIO.inputAll dev :: acc
                     )
                else readlines (line :: acc)
            end
         val lines = List.rev(readlines[])
         val dev   = (TextIO.closeIn dev; TextIO.openOut "c:\\autoexec.bat")
     in  List.app (fn line => TextIO.output(dev, line)) lines
       ; TextIO.closeOut dev
     end

val _ = main() handle ? => ()
