datatype 'a arex =
    ADD   of 'a binary
  | SUB   of 'a binary
  | MINUS of 'a unary
  | TIP   of 'a
withtype 'a binary = 'a arex * 'a arex
     and 'a unary = 'a arex
;

val a1 = MINUS(ADD(TIP 1, SUB(TIP 2, TIP 5)));
