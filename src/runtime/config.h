#ifndef _config_
#define _config_


#if defined(__MWERKS__) || defined(THINK_C)
#include "m.h"
#include "s.h"
#else
#ifdef macintosh
#include ":::config:m.h"
#include ":::config:s.h"
#else
#if defined(msdos)
#include "../config.dos/m.h"
#include "../config.dos/s.h"
#elif defined(WIN32)
#include "../config.w32/m.h"
#include "../config.w32/s.h"
#else
#include "../config/m.h"
#include "../config/s.h"
#endif
#endif
#endif

#ifdef WIN32

#ifdef CAMLRT
#define EXTERN __declspec(dllexport)
#else
#define EXTERN __declspec(dllimport)
#endif

#else
#define EXTERN extern
#endif

/* Library dependencies */

#ifdef HAS_MEMMOVE
#define bcopy(src,dst,len) memmove((dst), (src), (len))
#else
#ifdef HAS_BCOPY
/* Nothing to do */
#else
#ifdef HAS_MEMCPY
#define bcopy(src,dst,len) memcpy((dst), (src), (len))
#else
#define bcopy(src,dst,len) memmov((dst), (src), (len))
#define USING_MEMMOV
#endif
#endif
#endif

#ifndef HAS__SETJMP
#define _setjmp setjmp
#define _longjmp longjmp
#endif

/* Signed char type */

#if defined(__STDC__) || defined(SIGNED_CHAR_WORKS) || defined(WIN32)
typedef signed char schar;
#else
typedef char schar;
#endif

/* Do not change this definition. */
#define Page_size (1 << Page_log)

/* Memory model parameters */

#if !defined(SMALL) && !defined(SIXTEEN)

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Initial sizes of stacks (bytes). */
#define Stack_size 32768

/* Minimum free size of stacks (bytes); below that, they are reallocated. */
#define Stack_threshold 2048

/* Maximum sizes for the stacks (bytes). */
   
#ifdef MINIMIZE_MEMORY
#define Max_stack_size 262144
#else
#define Max_stack_size 1048576
#endif

/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768


/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))

/* Default size increment when growing the heap. (bytes)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * Page_size / sizeof (value))


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
#define Percent_free_def 30


#else
#ifdef SIXTEEN                 /* Scaled-down parameters for 16-bit machines */

#define Page_log 10
#define Stack_size 32768
#define Stack_threshold 2048

#define Max_stack_size 65532
#define Max_young_wosize 256
#define Minor_heap_min 512
#define Minor_heap_max 0x3F00
#define Minor_heap_def 8192
#define Heap_chunk_min 0x400
#define Heap_chunk_max 0x3C00
#define Heap_chunk_def 0x2000
#define Percent_free_def 15

#else
#ifdef SMALL                   /* Scaled-down parameters for small memory */

#define Page_log 10
#define Stack_size 32768
#define Stack_threshold 2048
#define Max_stack_size 1048576
#define Max_young_wosize 256
#define Minor_heap_min 1024
#define Minor_heap_max (1 << 28)
#define Minor_heap_def 16384
#define Heap_chunk_min (2 * Page_size / sizeof (value))
#define Heap_chunk_max (1 << 28)
#define Heap_chunk_def (126 * Page_size / sizeof (value))
#define Percent_free_def 20

#endif /* SMALL */
#endif /* SIXTEEN */

#endif /* !defined(SMALL) && !defined(SIXTEEN) */


#endif /* _config_ */
