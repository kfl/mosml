/* intinf.c -- partial interface to the GNU GMP multi-precision library.
   sestoft@dina.kvl.dk 1995, 1998-04-20 */

#include <gmp.h>

/* Access to the camlrunm/Moscow ML runtime data representation: */

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include <globals.h>

/* Arbitrary-precision integers: interface to the GNU
   Multiple-Precision Library GMP.

   The type LargeInt.int of arbitrary-precision integers is an
   abstract type; really an MP_INT structure.  This will contain a
   pointer to a limb array.  The limb array cannot be in camlrunm's
   heap space because the gc cannot understand the MP_INT structure
   (it would be confused by the untagged integer fields).

   This raises the question how to deallocate the limb structure when
   it is no longer reachable.  One possibility is to use finalized
   objects, calling the mpz_clear function explicitly whenever an
   MP_INT value is about to be garbage-collected by the camlrunm
   runtime.

   An largeint should be a finalized object: a pair,

              header with Final_tag
	      0: finalization function largeint_clear
	      1: pointer to MP_INT struct

   whose component 0 is a pointer to the finalizing function
   largeint_clear, and whose component 1 is a pointer to an MP_INT
   struct.  The finalization function should apply mpz_clear to the
   second component of the pair:
*/

/* 1999Sep05 <e@flavors.com>

   This code has been modified for quicker garbage collection. The
   motivating example is the fac.sml code which, even though iterative,
   retains all intermediate (malloc) garbage because almost all the
   allocation occurs in gmp. On the Mac, I couldn't execute 'fac 5050;'
   with 32M allocated to mosml.

   The biggest change is to hook the gmp allocation routines here
   (fortunately gmp provides an interface for this). The hook'd malloc
   and realloc provide two benefits: (1) they call the runtime's
   adjust_gc_speed() function to inform the gc about the external
   allocations, and (2) they raise mosml exceptions for out-of-memory
   errors. These changes are marked with a "/* adjust_gc_speed tweak *"
   comment.

   A macro MAX_GMP_ALLOC is defined below. It determines how much gmp
   allocation occurs before a garbage collection is forced. (The
   formula depends on other configurations, see src/runtime/memory.c.)
   It would be nice to determine MAX_GMP_ALLOC based on the memory
   configuration of the application, but I'm not sure how to do this.
   If it becomes an issue perhaps an environment variable can be used.

   The second change is to integrate the MP_INT structure into the
   mosml largeint rather than pointing to it. This provides a modest
   speed improvement by avoiding a malloc()/free() for each Largeint.
   These changes are marked with a "/* inline MPINT tweak *" comment.

   A largeint is now a finalized object: a structure,

              header with Final_tag
	      0: finalization function largeint_clear
	      1: MP_INT struct {
              2: ...
              3: ... }

   [currently an MP_INT is 3 words, but the code below doesn't depend
     on this; also, Final_tag'd structs are not scanned by the gc]
*/

/* inline MPINT tweak */
#define Large_val(x) (MP_INT*)&(Field(x, 1))

void largeint_finalize(value obj)
{
  MP_INT *mpint = Large_val(obj);
  mpz_clear(mpint);
}

/* When the largeint becomes unreachable from the ML process, it will
   be garbage-collected, largeint_finalize() will be called on the
   pointer to the MP_INT struct to deallocate the limbs, and finally
   free() will be called to deallocate the mpint struct itself.  The
   camlrunm gc then frees the pair representing the largeint.

   Creation of a largeint should call malloc() to allocate an MP_INT
   struct pointed to by mpint, make a finalized pair (largeint_finalize,
   mpint) as described above, and call mpz_init (or similar) on mpint:
*/

/* adjust_gc_speed tweak */

#include <memory.h>

#ifdef macintosh
/* this is a nested 'ifdef macintosh' which should remain in place
   even if the outer 'ifdef macintosh' is removed.
*/
extern long FreeMem( void ); // #include <MacMemory.h>

static max_gmp_alloc = 0;

#define MAX_GMP_ALLOC (max_gmp_alloc ? max_gmp_alloc : get_max_gmp_alloc())

/* dbg: #include <stdio.h>*/

static long get_max_gmp_alloc( void )
{
  long freemem = FreeMem();
  max_gmp_alloc = freemem >> 1;
  /* dbg: printf( "\n max_gmp_alloc = %ld\n", max_gmp_alloc );*/
  return max_gmp_alloc;
}

#else
/* choose an arbitrary value */
#define MAX_GMP_ALLOC 50000000
#endif

static int gmp_memfunc_set = 0;

static void *mosml_gmp_allocate( size_t size )
{
  adjust_gc_speed( size, MAX_GMP_ALLOC );
  return stat_alloc( size );
}

static void *mosml_gmp_reallocate( void *oldptr, size_t old_size, 
				   size_t new_size )
{
  #pragma unused (old_size)
  adjust_gc_speed( new_size, MAX_GMP_ALLOC );
  return stat_resize( oldptr, new_size );
}

value largeint_alloc()
{
  value res;
  int n;
  /* adjust_gc_speed tweak */
  if( gmp_memfunc_set == 0 )
  {
    mp_set_memory_functions( &mosml_gmp_allocate, &mosml_gmp_reallocate,
                             NULL ); /* NULL -> use default free() */
    gmp_memfunc_set = 1;
  }
  /* inline MPINT tweak */
  n = 1 + (sizeof(MP_INT) + sizeof(value) - 1) / sizeof(value);
  res = alloc_final(n, &largeint_finalize, n, MAX_GMP_ALLOC);

  return res;
}

value largeint_clear(value obj)			
{ 
  largeint_finalize(obj);
  /* Change the tag from Final_tag to Abstract_tag so that the GC will not
     attempt to deallocate the limbs again: */
  Tag_val(obj) = Abstract_tag;
  return Val_unit;
}

value largeint_make(value null)			
{ 
  value li = largeint_alloc();
  mpz_init(Large_val(li));
  return li;
}

value largeint_make_si(value src)			
{ 
  value li = largeint_alloc();
  mpz_init_set_si(Large_val(li), Long_val(src));
  return li;
}

value largeint_set(value dest, value src)
{ 
  mpz_set(Large_val(dest), Large_val(src)); 
  return Val_unit;
}

value largeint_set_si(value dest, value src)
{ 
  mpz_set_si(Large_val(dest), Long_val(src)); 
  return Val_unit;
}

value largeint_to_si(value src)
{ 
  signed long int tmp = mpz_get_si(Large_val(src)); 
  value res = Val_long(tmp);
  if (Long_val(res) != tmp)
    { raiseprimitive0(SYS__EXN_OVERFLOW); }
  return res;
}

value largeint_neg(value dest, value src)
{ 
  mpz_neg(Large_val(dest), Large_val(src)); 
  return Val_unit;
}

value largeint_add(value dest, value li1, value li2)
{ 
  mpz_add(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_sub(value dest, value li1, value li2)
{ 
  mpz_sub(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_mul(value dest, value li1, value li2)
{ 
  mpz_mul(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

/* Division truncating towards 0: */
value largeint_tdiv(value dest, value li1, value li2)
{ 
  mpz_tdiv_q(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_tmod(value dest, value li1, value li2)		
{ 
  mpz_tdiv_r(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_tdivmod(value quotdest, value remdest, value li1, value li2)
{ 
  mpz_tdiv_qr(Large_val(quotdest), Large_val(remdest), 
	      Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

/* Division rounding towards minus infinity: */
value largeint_fdiv(value dest, value li1, value li2)
{ 
  mpz_fdiv_q(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_fmod(value dest, value li1, value li2)
{ 
  mpz_fdiv_r(Large_val(dest), Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_fdivmod(value quotdest, value remdest, value li1, value li2)
{ 
  mpz_fdiv_qr(Large_val(quotdest), Large_val(remdest), 
	     Large_val(li1), Large_val(li2)); 
  return Val_unit;
}

value largeint_cmp(value li1, value li2)			
{ 
  long res = mpz_cmp(Large_val(li1), Large_val(li2));
  if (res < 0) 
    return Val_long(-1); 
  else if (res > 0) 
    return Val_long(1); 
  else 
    return Val_long(0);
}

value largeint_cmp_si(value li, value si)		
{ 
  long res = mpz_cmp_si(Large_val(li), Long_val(si));
  if (res < 0)      
    return Val_long(-1);
  else if (res > 0) 
    return Val_long(1); 
  else 
    return Val_long(0);
}

value largeint_sizeinbase(value li, value base)		
{ return (Val_long(mpz_sizeinbase(Large_val(li), Long_val(base)))); }

/* The mpz_set_str function below is pretty absurd:
 * "- 123"	-> ~123
 * " -123"   fails
 * "- 12 3"  -> ~123
 * "+123"    fails
 */

value largeint_set_str(value dest, value str, value base)	
{ 
  long changesign = (Byte(str, 0) == '~');
  long res;
  if (changesign) { Byte(str, 0) = '-'; }
  res = mpz_set_str(Large_val(dest), String_val(str), Long_val(base));
  if (changesign) { Byte(str, 0) = '~'; }
  if (0 == res)
    { return Val_unit; }
  else
    { failwith("Ill-formed number string"); }
}

value largeint_get_str(value src, value base)		
{ 
  long len = 3 + mpz_sizeinbase(Large_val(src), Long_val(base));
  char *buffer = (char*)(malloc(len));
  value res;
  mpz_get_str(buffer, Long_val(base), Large_val(src));  
  res = copy_string(buffer);
  free(buffer);

  /* Use the SML sign character: */
  if (Byte(res, 0) == '-')
    { Byte(res, 0) = '~'; }
  
  return res;
}

value largeint_pow_ui(value dest, value li, value ui)		
{ 
  mpz_pow_ui(Large_val(dest), Large_val(li), Long_val(ui)); 
  return Val_unit;
}

