/* structure of the stacks */

#ifndef _stacks_
#define _stacks_


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

extern value * stack_low;
extern value * stack_high;
extern value * stack_threshold;
extern value * extern_sp;
extern value * trapsp;

extern value global_data;

#define Trap_pc(tp) (((CODE *)(tp))[0])
#define Trap_link(tp) (((value **)(tp))[1])

void reset_roots (void);
void init_stack (void);
void realloc_stack (void);


#endif /* _stacks_ */
