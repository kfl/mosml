#ifndef _sys_
#define _sys_

#include "misc.h"

void sys_error (char *);
void raise_pending_signal (void);
void sys_init (char **);
void sys_exit (value);

#endif /* _sys_ */
