#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

#define Uint64_wosize ((sizeof(uint64_t) / sizeof(value)))

#define UInt64_val(v) (* (uint64_t *) (v))

static void Store_uint64_val(value val, uint64_t raw) {
    * (uint64_t *) (val) = raw;
}

value copy_uint64(uint64_t raw) {
  value res = alloc(Uint64_wosize, Abstract_tag);
  Store_uint64_val(res, raw);
  return res;
}


#define Int64_wosize ((sizeof(int64_t) / sizeof(value)))

#define Int64_val(v) (* (int64_t *) (v))

static void Store_int64_val(value val, int64_t raw) {
    * (int64_t *) (val) = raw;
}

value copy_int64(int64_t raw) {
  value res = alloc(Int64_wosize, Abstract_tag);
  Store_int64_val(res, raw);
  return res;
}


/* --------------------------------------------------------------------- */
/* ------ uint64_t a.k.a word64----------------------------------------- */
/* --------------------------------------------------------------------- */

value boxed_uint64_equal(value v1, value v2) {              /* ML */
  return (UInt64_val(v1) == UInt64_val(v2))
         ? Val_true
         : Val_false;
}

value boxed_uint64_less(value v1, value v2) {              /* ML */
  return (UInt64_val(v1) < UInt64_val(v2))
         ? Val_true
         : Val_false;
}

value boxed_uint64_fromword(value v) {                      /* ML */
  return copy_uint64(Long_val(v));
}

value boxed_uint64_toword(value v) {                      /* ML */
  return Val_long(UInt64_val(v));
}

value boxed_uint64_add(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) + UInt64_val(v2));
}

value boxed_uint64_mul(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) * UInt64_val(v2));
}

value boxed_uint64_sub(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) - UInt64_val(v2));
}

value boxed_uint64_div(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) / UInt64_val(v2));
}

value boxed_uint64_mod(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) % UInt64_val(v2));
}

value boxed_uint64_orb(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) | UInt64_val(v2));
}

value boxed_uint64_andb(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) & UInt64_val(v2));
}

value boxed_uint64_xorb(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) ^ UInt64_val(v2));
}

value boxed_uint64_lshift(value v1, value v2) {               /* ML */
  return copy_uint64(UInt64_val(v1) << UInt64_val(v2));
}

value boxed_uint64_rshift_signed(value v1, value v2) {               /* ML */
  return copy_uint64(((int64_t)UInt64_val(v1)) >> UInt64_val(v2));
}

value boxed_uint64_rshift_unsigned(value v1, value v2) {               /* ML */
  return copy_uint64((UInt64_val(v1)) >> UInt64_val(v2));
}


/* --------------------------------------------------------------------- */
/* ------ int64_t a.k.a int64 ------------------------------------------ */
/* --------------------------------------------------------------------- */

value boxed_int64_equal(value v1, value v2) {              /* ML */
  return (Int64_val(v1) == Int64_val(v2))
         ? Val_true
         : Val_false;
}

value boxed_int64_less(value v1, value v2) {              /* ML */
  return (Int64_val(v1) < Int64_val(v2))
         ? Val_true
         : Val_false;
}

value boxed_int64_fromint(value v) {                      /* ML */
  return copy_int64(Long_val(v));
}

value boxed_int64_toint(value v) {                        /* ML */
  return Val_long(Int64_val(v));
}

value boxed_int64_add(value v1, value v2) {               /* ML */
  int64_t res;
  if ( __builtin_add_overflow(Int64_val(v1), Int64_val(v2), &res)  )
    raise_overflow();

  return copy_int64(res);
}

value boxed_int64_sub(value v1, value v2) {               /* ML */
  int64_t res;
  if ( __builtin_sub_overflow(Int64_val(v1), Int64_val(v2), &res)  )
    raise_overflow();

  return copy_int64(res);
}

value boxed_int64_mul(value v1, value v2) {               /* ML */
  int64_t res;
  if ( __builtin_mul_overflow(Int64_val(v1), Int64_val(v2), &res)  )
    raise_overflow();

  return copy_int64(res);
}

/* div rounding towards minus infinity,
   mod is the remainder for div.
   assumes C99 semantics
*/

value boxed_int64_div(value v1, value v2) {               /* ML */
  int64_t x = Int64_val(v1), y = Int64_val(v2);
  if ( y == -1 && x == INT64_MIN )
    raise_overflow();
  else {
    imaxdiv_t r = imaxdiv(x, y);
    if ( (r.rem != 0) && ( (r.rem < 0) != (y < 0) ) )
      --r.quot;
    return copy_int64(r.quot);
  }
}

value boxed_int64_mod(value v1, value v2) {               /* ML */
  int64_t x = Int64_val(v1), y = Int64_val(v2);
  int64_t r = x % y;
  if ( (r != 0) && ( (r < 0) != (y < 0) ) )
    r += y;
  return copy_int64(r);
}

/* quot and rem assumes C99 semantics */

value boxed_int64_quot(value v1, value v2) {               /* ML */
  int64_t x = Int64_val(v1), y = Int64_val(v2);
  if ( y == -1 && x == INT64_MIN )
    raise_overflow();
  return copy_int64(x / y);
}

value boxed_int64_rem(value v1, value v2) {                 /* ML */
  return copy_int64(Int64_val(v1) % Int64_val(v2));
}
