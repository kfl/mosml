#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#include "debugger.h"
#include "misc.h"
#include "io.h"
#include "sys.h"
#ifdef HAS_UI
#include "ui.h"
#endif

int verb_gc;
int Volatile something_to_do = 0;
int Volatile force_minor_flag = 0;

void force_minor_gc (void)
{
  force_minor_flag = 1;
  something_to_do = 1;
}

void gc_message (char * msg, unsigned long arg)
{
  if (verb_gc){
#ifdef HAS_UI
    ui_gc_message(msg, arg);
#else
    fprintf (stderr, msg, arg);
    fflush (stderr);
#endif
  }
}

void fatal_error (char * msg)
{
  flush_stdouterr();
#ifdef HAS_UI
  ui_fatal_error("%s", msg);
#else
  fprintf (stderr, "%s", msg);
  sys_exit(Val_int(2));  
#endif
}

void fatal_error_arg (char * fmt, char * arg)
{
  flush_stdouterr();
#ifdef HAS_UI
  ui_fatal_error(fmt, arg);
#else
  fprintf (stderr, fmt, arg);
  sys_exit(Val_int(2)); 
#endif
}

#ifdef USING_MEMMOV

/* This should work on 64-bit machines as well as 32-bit machines.
   It assumes a long is the natural size for memory reads and writes.
*/
void memmov (char * dst, char * src, unsigned long length)
{
  unsigned long i;

  if ((unsigned long) dst <= (unsigned long) src){

      /* Copy in ascending order. */
    if (((unsigned long) src - (unsigned long) dst) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length != 0; length--){
	*dst++ = *src++;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i != 0){
	i = sizeof (long) - i;              /* Number of bytes to copy. */
	if (i > length) i = length;         /* Never copy more than length.*/
	for (; i != 0; i--){
	  *dst++ = *src++; --length;
	}
      }                    Assert ((unsigned long) dst % sizeof (long) == 0);
                           Assert ((unsigned long) src % sizeof (long) == 0);

      /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
	*(long *) dst = *(long *) src;
	dst += sizeof (long); src += sizeof (long);
      }

      /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
	*dst++ = *src++;
      }
    }
  }else{                                       /* Copy in descending order. */
    src += length; dst += length;
    if (((unsigned long) dst - (unsigned long) src) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
	   Copy byte by byte. */
      for (; length > 0; length--){
	*--dst = *--src;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i > length) i = length;           /* Never copy more than length. */
      for (; i > 0; i--){
	*--dst = *--src; --length;
      }

        /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
	dst -= sizeof (long); src -= sizeof (long);
	*(long *) dst = *(long *) src;
      }

        /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
	*--dst = *--src;
      }
    }
  }
}

#endif /* USING_MEMMOV */

char *aligned_malloc (asize_t size, int modulo)
{
  char *raw_mem;
  unsigned long aligned_mem;
/*
#ifndef __MWERKS__
  extern char * malloc();
#endif
*/
                                                 Assert (modulo < Page_size);
  raw_mem = malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  raw_mem += modulo;		/* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
  return (char *) (aligned_mem - modulo);
}
