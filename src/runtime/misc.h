/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_

#include "config.h"
#if defined(__STDC__) || defined(WIN32)
#include <stddef.h>
#endif
#if defined(SIXTEEN) || defined (__MWERKS__)
#include <stdlib.h>
#include <string.h>
#endif

#if defined(__STDC__) || defined(WIN32)
typedef size_t asize_t;
#else
typedef int asize_t;
#endif

#ifndef NULL
#define NULL 0
#endif

#ifdef SIXTEEN
typedef char huge * addr;
#else
typedef char * addr;
#endif

#if defined(__STDC__) || defined(WIN32)
#define Volatile volatile
#else
#define Volatile
#endif

#define Noreturn void

extern int verb_gc;
extern int Volatile something_to_do;
extern int Volatile force_minor_flag;

void force_minor_gc(void);
void gc_message(char *, unsigned long);
Noreturn fatal_error(char *);
Noreturn fatal_error_arg(char *, char *);
void memmov(char *, char *, unsigned long);
char * aligned_malloc(asize_t, int);


#endif /* _misc_ */
