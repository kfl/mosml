/* callback.h */

#ifndef _callback_
#define _callback_

#include "mlvalues.h"		/* for Field, Reference_tag etc */
#include "fail.h"		/* for failwith */
#include "memory.h"		/* for alloc_shr */
#include "alloc.h"		/* for copy_string */
#include "minor_gc.h"		/* for minor_collection */
#include "interp.h"		/* for callback */

typedef value valueptr;		/* An 'a option ref */

extern valueptr get_valueptr(char* nam);
extern value get_value(valueptr mvp);
extern value callbackptr(valueptr closureptr, value arg1);
extern value callbackptr2(valueptr closureptr, value arg1, value arg2);
extern value callbackptr3(valueptr closureptr, value arg1, value arg2, 
			  value arg3);
extern void registervalue(char* nam, value mlval);
extern void unregistervalue(char* nam);

extern void registercptr(char* nam, void* cptr);

#endif /* _callback_ */
