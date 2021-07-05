#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
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
