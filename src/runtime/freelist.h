/* Free lists of heap blocks. */

#ifndef _freelist_
#define _freelist_


#include "misc.h"
#include "mlvalues.h"

char *fl_allocate (mlsize_t);
void fl_init_merge (void);
char *fl_merge_block (char *);
void fl_add_block (char *);


#endif /* _freelist_ */
