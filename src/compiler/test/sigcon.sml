(* test signature constraints *)
signature OK = sig type t end where type  t = int;
signature OK = sig structure X : sig type t end
	       end where type  X.t = int;
signature OK = sig structure Y : sig structure X : sig type t end 
				 end 
	       end where type  Y.X.t = int;

signature OK = sig type t 
		   type u
		   sharing type  t = u 
	       end;
signature OK = sig type u 
		   type t 
		   sharing type  t = u 
	       end;

signature OK = sig structure X : sig 
				     type t 
				     type u 
				 end 
		   type u 
		   sharing type  X.t = X.u = u 
	       end;

signature OK = sig structure Y : sig
				     structure X : sig type t 
						       type u
						   end 
				     type u 
				 end
		   type u 
                   sharing type Y.X.t = Y.X.u = Y.u = u
	       end;

signature OK = sig 
		   structure Y : sig structure X : sig type t type u end type u type a end 
		   structure Z :
		       sig
			   structure Y : sig structure X : sig type t type u end type u type v end 
		       end
		   structure W : sig type u type v end;
		   sharing Y = Z.Y = W
	       end;


(* cvr: TODO need failure tests *)
