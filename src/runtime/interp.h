#include "mlvalues.h"

EXTERN value interprete(int mode, bytecode_t bprog, int code_size, CODE* rprog);
EXTERN value callback(value closure, value arg);
EXTERN value callback2(value closure, value arg1, value arg2);
EXTERN value callback3(value closure, value arg1, value arg2, value arg3);
