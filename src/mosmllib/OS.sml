(* OS *)

structure OS :> OS = 
struct
    type syserror = syserror
    exception SysErr = SysErr
    prim_val errorMsg : syserror -> string = 1 "sml_errormsg"; 

    structure FileSys = FileSys
    structure Path = Path
    structure Process = Process
end


