/* Operations on strings */

#include <stdlib.h>
#include <string.h>
#include "alloc.h"
#include "debugger.h"
#include "mlvalues.h"
#include "str.h"

mlsize_t string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

value create_string(value len)        /* ML */
{
  return alloc_string(Long_val(len));
}

EXTERN value compare_strings(value s1, value s2)   /* ML */
{
  mlsize_t len1, len2;
  register mlsize_t len;
  register unsigned char * p1, * p2;

  len1 = string_length(s1);
  len2 = string_length(s2);
  for (len = (len1 <= len2 ? len1 : len2),
         p1 = (unsigned char *) String_val(s1),
         p2 = (unsigned char *) String_val(s2);
       len > 0;
       len--, p1++, p2++)
    if (*p1 != *p2)
      return (*p1 < *p2 ? Val_long(-1) : Val_long(1));
  if (len1 == len2)
    return Val_long(0);
  else if (len1 < len2)
    return Val_long(-2);
  else
    return Val_long(2);
}

value blit_string(value s1, value offset1, value s2, value offset2, value len) /* ML */
{
  bcopy(&Byte(s1, Long_val(offset1)),
        &Byte(s2, Long_val(offset2)),
        Int_val(len));
  return Atom(0);
}

value fill_string(value s, value offset, value len, value init) /* ML */
{
  register char * p;
  register mlsize_t n;
  register char c;

  c = Long_val(init);
  for(p = &Byte(s, Long_val(offset)), n = Long_val(len);
      n > 0; n--, p++)
    *p = c;
  return Atom(0);
}

#ifdef unix
static unsigned char printable_chars_ascii[] = /* 0x20-0x7E */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
static unsigned char printable_chars_iso[] = /* 0x20-0x7E 0xA1-0xFF */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\376\377\377\377\377\377\377\377\377\377\377\377";
#endif
#ifdef macintosh
static unsigned char printable_chars[] = /* 0x20-0x7E 0x80-0xC9 0xCB-0xD8 */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\377\377\377\377\377\377\377\377\377\373\377\001\000\000\000\000";
#endif
#ifdef MSDOS
static unsigned char printable_chars[] = /* 0x20-0xFE */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\177";
#endif

value is_printable(value chr) /* ML */
{
  int c;
#ifdef unix
  static int iso_charset = -1;
  unsigned char * printable_chars;

  if (iso_charset == -1) {
    char * lc_ctype = (char *) getenv("LC_CTYPE");
    if (lc_ctype != 0 && strcmp(lc_ctype, "iso_8859_1") == 0)
      iso_charset = 1;
    else
      iso_charset = 0;
  }
  printable_chars = iso_charset ? printable_chars_iso : printable_chars_ascii;
#endif

  c = Int_val(chr);
  return Val_bool(printable_chars[c >> 3] & (1 << (c & 7)));
}
