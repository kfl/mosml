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

EXTERN valueptr get_valueptr(char* nam);
EXTERN value get_value(valueptr mvp);
EXTERN value callbackptr(valueptr closureptr, value arg1);
EXTERN value callbackptr2(valueptr closureptr, value arg1, value arg2);
EXTERN value callbackptr3(valueptr closureptr, value arg1, value arg2, 
			  value arg3);
EXTERN void registervalue(char* nam, value mlval);
EXTERN void unregistervalue(char* nam);

EXTERN void registercptr(char* nam, void* cptr);

#endif /* _callback_ */
