(* Printing cyclic structures *)

datatype xxx = P | Q of {key: (xxx ref * xxx ref) list};

val z = ref P;
z := Q{key = [(z,z), (z,z)]};
z;
