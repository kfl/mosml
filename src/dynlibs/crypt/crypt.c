/* File mosml/src/dynlibs/crypt/crypt.c -- foreign function interface
   The C side of things: how to define the C function to be invoked from SML
 */

#ifdef WIN32
#include <crypt.h>		/* For crypt       */
#else
#include <unistd.h>		/* For crypt       */
#endif

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* Moscow ML specific includes: */

#include <mlvalues.h>		/* For String_val  */
#include <alloc.h>		/* For copy_string */

/* Type on the SML side: string -> string -> string */

EXTERNML value ml_crypt(value mlkey, value mlsalt)
{
  /* Obtain pointers to the SML strings mlkey and mlsalt in the SML heap: */
  char *key  = String_val(mlkey);
  char *salt = String_val(mlsalt);

  /* Invoke C library function: */
  char *res = (char*)crypt(key, salt);

  /* The result is a pointer to a null-terminated C string.    */
  /* Copy it to a fresh string in the ML heap and return that: */
  return copy_string(res);
}
