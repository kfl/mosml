/* mgdbm.c, version 0.4, sestoft@dina.kvl.dk, 1997, 1998-05-31 */

#include <gdbm.h>
#ifdef WIN32
#include <gdbmerrno.h>
#endif

#include <string.h>
#include <stdlib.h>

/* Moscow ML specific includes from mosml/src/runtime: */

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include <str.h>
#include <gc.h>

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* Interface to GNU DBM; see also Gdbm.{sig,sml}

   The type Gdbm.database_ of gdbm databases is an abstract block 
   whose only field is a GDBM_FILE (a pointer) as defined in <gdbm.h>.  

   The strings returned by calls to gdbm_fetch, gdbm_firstkey and
   gdbm_nextkey are kept in memory malloc'ed by gdbm.  This memory
   must be explicitly free'd.  We do this by creating an ML string
   from every string returned by gdbm, then freeing the string
   malloced by gdbm.  This incurs an overhead for copying of
   potentially large strings.

   An alternative would be to create finalized objects of the returned
   strings, but then they cannot be accessed as normal strings.  Also,
   subtle problems of adjusting the speed of the garbage collector
   would arise. */

/* Make a datum from an ML string.  The garbage collector must not be
   invoked (i.e. nothing must be allocated in the ML heap) before the
   last use of the datum: */

datum datum_string(value v)
{
  datum val;
  val.dptr = String_val(v);
  val.dsize = string_length(v);
  return val;
}

/* Copy the datum to an ML string, then free the datum.  Argument must
   be non-NULL: */

value string_datum(datum val)
{
  value str;
  str = alloc_string(val.dsize);
  bcopy(val.dptr, String_val(str), val.dsize);
  free(val.dptr);
  return str;
} 

#define Gdbm_val(x) (GDBM_FILE)(Field(x, 0))

/* Raise Fail with an argument in case of a fatal error: */

void mgdbm_fatal(char *msg)
{
  char buf[256] = "Gdbm error: ";
  strcat(buf, msg);
  failwith(buf);
}

EXTERNML value mgdbm_open(value nam, value flags, value perm) /* ML */
{
  GDBM_FILE dbf;
  value res;
  /* Block size = 0 means default file system block size: */
  dbf = gdbm_open(String_val(nam), 0, Long_val(flags), Long_val(perm), 
		  &mgdbm_fatal);
  if (!dbf)
    failwith("gdbm_open");
  res = alloc(1, Abstract_tag);
  Field(res, 0) = (value)dbf;
  return res;
}

EXTERNML value mgdbm_close(value db)	/* ML */
{
  gdbm_close(Gdbm_val(db));
  return Val_unit;
}

EXTERNML value mgdbm_store(value db, value k, value v, value flag) /* ML */
{
  return Val_long(gdbm_store(Gdbm_val(db), datum_string(k), 
			     datum_string(v), Long_val(flag)));
}

EXTERNML value mgdbm_fetch(value db, value k) /* ML */
{
  datum val;
  val = gdbm_fetch(Gdbm_val(db), datum_string(k));
  if (NULL == val.dptr)
    failwith("gdbm_fetch");
  return string_datum(val);
}

EXTERNML value mgdbm_exists(value db, value k) /* ML */
{
  return Val_bool(gdbm_exists(Gdbm_val(db), datum_string(k)));
}

EXTERNML value mgdbm_delete(value db, value k) /* ML */
{
  return Val_bool(gdbm_delete(Gdbm_val(db), datum_string(k)) == 0);
}

EXTERNML value mgdbm_numitems(value db)	/* ML */
{
  int count = 0;
  GDBM_FILE dbf = Gdbm_val(db);
  datum key, old;
  key = gdbm_firstkey(dbf);
  while (key.dptr != NULL)
    {
      count++;
      old = key;
      key = gdbm_nextkey(dbf, old);
      free(old.dptr);
    }
  return Val_long(count);
}

EXTERNML value mgdbm_firstkey(value db) /* ML */
{
  datum res;
  res = gdbm_firstkey(Gdbm_val(db));
  if (NULL == res.dptr)
    failwith("gdbm_firstkey");
  return string_datum(res);
}

EXTERNML value mgdbm_nextkey(value db, value k) /* ML */
{
  datum res;
  res = gdbm_nextkey(Gdbm_val(db), datum_string(k));
  if (NULL == res.dptr)
    failwith("gdbm_nextkey");
  return string_datum(res);
}

EXTERNML value mgdbm_reorganize(value db) /* ML */
{
  return Val_bool(gdbm_reorganize(Gdbm_val(db)) == 0);
}

EXTERNML value mgdbm_error(value unit)	/* ML */
{
  return copy_string((char*)gdbm_strerror(gdbm_errno));
}

/* Pass the constants from <gdbm.h> to the ML code: */

EXTERNML value mgdbm_constants(value unit)	/* ML */
{
  value res = alloc_tuple(7);
  Field(res, 0) = Val_long(GDBM_READER);
  Field(res, 1) = Val_long(GDBM_WRITER);
  Field(res, 2) = Val_long(GDBM_WRCREAT);
  Field(res, 3) = Val_long(GDBM_NEWDB);
  Field(res, 4) = Val_long(GDBM_FAST);
  Field(res, 5) = Val_long(GDBM_INSERT);
  Field(res, 6) = Val_long(GDBM_REPLACE);
  return res;
}
