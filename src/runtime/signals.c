#include "alloc.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "fail.h"
#include "instruct.h"
#include "interp.h"

Volatile int signal_is_pending = 0;
int in_blocking_section = 0;
Volatile CODE signal_handler;
Volatile int signal_number;

/* This is set by interprete in interp.c on initialization: */

CODE raise_break_exn;

void execute_signal(void)
{
  if (in_blocking_section) {
    value clos;
    clos = alloc(Closure_wosize, Closure_tag);
    Code_val(clos) = signal_handler;
    Env_val(clos) = Atom(0);
    callback(clos, Val_int(signal_number));
  } else {
    signal_is_pending = 1;
    something_to_do = 1;
  }
}

void enter_blocking_section(void)
{
  in_blocking_section = 1;
  if (signal_is_pending) execute_signal();
}

void leave_blocking_section(void)
{
  in_blocking_section = 0;
}
