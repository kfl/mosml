/* File mosml/src/mosmllib/test/callback/cside.c -- testing Callback.

   How to access SML values from C, and how to create SML values to be 
   returned to SML. The C side of things.  Updated 2000-06-16.
 */

#include <ctype.h>		/* For toupper */
#include <stdlib.h>		/* For malloc */
#include <string.h>		/* For strcpy */

/* Moscow ML specific includes: */

#include <mlvalues.h>		/* For Val_unit, Long_val, String_val, ... */
#include <alloc.h>		/* For copy_string, alloc_string, ...      */
#include <memory.h>		/* For Modify, Push_roots, Pop_roots       */
#include <str.h>		/* For string_length                       */
#include <callback.h>


#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif


/* SML type: int -> int */

value cfi(value v)
{
  long i = Long_val(v);

  return Val_long(i + 1);
}


/* SML type: unit -> unit */

value cfu(value dummy)
{
  return Val_unit;
}


/* SML type: char -> char */

value cfc(value v)
{
  char c = (char)Long_val(v);

  return Val_long((long)(toupper(c)));
}


/* SML type: real -> real */

value cfr(value v)
{
  double d = Double_val(v);

  return copy_double(2 * d);
}


/* SML type: string -> string */

value cfs(value v)
{
  char *oldp, *newp, *q;
  int i, len;
  oldp = String_val(v);		/* Null-terminated heap-allocated string */
  len = string_length(v);	/* Much faster than strlen */
  
  q = newp = malloc(len+1);	/* malloc a C string */
  strcpy(newp, oldp);		/* and copy the given ML string to it */

  while (*q)			/* Modify the C string */
    { *q = toupper(*q); q++; }
  
  return copy_string(newp);	/* Copy modified string to the ML heap */
}


/* SML type: bool -> bool */

value cfb(value v)
{
  int b = Bool_val(v);

  return Val_bool(!b);
}


/* SML type: int -> char -> real -> string -> bool -> int */

value cfcur(value vi, value vc, value vr, value vs, value vb)
{
  long i   = Long_val(vi);
  char c   = (char)Long_val(vc);
  double d = Double_val(vr);
  char *p  = String_val(vs);
  int b    = Bool_val(vb);

  return Val_long(i + c + (int)d + strlen(p) + b);
}


/* SML type: int * char * real -> int */
/* The components of a tuple v are Field(v, 0), Field(v, 1), ... */

value cftup(value v)
{
  long i   = Long_val(Field(v, 0)); 
  char c   = (char)Long_val(Field(v, 1));
  double d = Double_val(Field(v, 2));

  return Val_long(i + c + (int)d);
}


/* SML type: { age : int, givenname : string, surname : string } -> bool */
/* A record is really a tuple, sorted lexicograhically on labels: */

value cfrec(value v)
{
  long age       = Long_val(Field(v, 0));
  char *givennam = String_val(Field(v, 1)); 
  char *surnam   = String_val(Field(v, 2)); 

  return Val_bool(age > 30 || strcmp(surnam, givennam) >= 0);
}


long treesum(value v);		/* Forwards C declaration */

/* SML type: t -> int where 
   datatype t = Br of int * t * t | Brs of t list | Lf */

value cfdat(value v)
{
  return Val_long(treesum(v));
}


/* Traversal of an SML list data structure (auxiliary function): */

long listsum(value lst)
{
#define isCons(x) (Tag_val(x) != 0)  

  long sum = 0;
  while (isCons(lst))		    /* While non-Nil */
    {
      value elem = Field(lst, 0);   /* The list element = first arg of Cons */
      sum += treesum(elem);
      lst = Field(lst, 1);	    /* The list tail = second arg of Cons */
    }
  return sum;
}


/* Auxiliary function demonstrating traversal of SML tree data structure. */
/* Datatype constructors are sorted alphabetically (based on ASCII) and   */
/* then numbered 0, 1, ...; the C code must use these numbers:            */

long treesum(value v)
{
  long sum = 0;
  int contag = Tag_val(v);	/* 0 = Lf, 1 = Br, 2 = Brs */
  switch (contag) {
  case 2: /* Lf */
    sum = 0; break;
  case 0: /* Br(i, t1, t2) */
    {
      long i   = Long_val(Field(v, 0));
      value t1 = Field(v, 1);	/* Left subtree */
      value t2 = Field(v, 2);	/* Right subtree */
      sum = i + treesum(t1) + treesum(t2);
      break;
    }
  case 1: /* Brs(tlist) */
    {
      value tlist = Field(v, 0); /* The list of subtrees */
      sum = listsum(tlist);
      break;
    }
  }
  return sum;
}


/* SML type: (int -> string) -> int -> string */

value cffun(value vf, value vi)
{
  int count = Val_long(vi);
  int ok = 1;
  value res;
  int i;
  Push_roots(r, 1);
  r[0] = vf;
  for (i=0; i<count; i++)
    if (Double_val(callback(r[0], Val_long(i))) != i+7)
      ok = 0;
  /* NB: After a callback, the garbage collector may have run, so that
     the pointer vf may have been invalidated. */
  if (ok)
    res = copy_string("Just right");
  else
    res = copy_string("Something is wrong");
  Pop_roots();
  return res;
}


/* SML type: int -> int * bool */

value cfrtup(value vi)
{
  long i = Long_val(vi);
  value tup = alloc_tuple(2);	/* Allocate 2-element tuple in ML heap */
  /* Must, in general, use modify to update because the GC is generational: */
  modify(&Field(tup, 0), Val_long(i / 2));
  modify(&Field(tup, 1), Val_bool(i % 2 == 1));
  return tup;
}


/* SML type: int -> { half : int, odd : bool } */
/* A record is a sorted tuple, so this function is identical to that above */

value cfrrec(value vi)
{
  long i = Long_val(vi);
  value tup = alloc_tuple(2);	/* Allocate 2-element tuple in ML heap */
  /* Must, in general, use modify to update because the GC is generational: */
  modify(&Field(tup, 0), Val_long(i / 2));
  modify(&Field(tup, 1), Val_bool(i % 2 == 1));
  return tup;
}


/* When allocating new values in the ML heap, special precautions must
   be made.  When the C function allocates in the ML heap, it may
   cause the garbage collector to run, which may cause it to move
   values from the young generation to the old one.  To make sure that
   the C function's copies of ML heap pointers are updated
   accordingly, register them with Push_roots (and unregister them
   with Pop_roots after use).  This is necessary if the C function's
   argument is any ML value other than int, char, or bool. */

/* SML type: string -> string -> string */
/* String concatenation (without error checking).  The resulting string is
   allocated in the ML heap, so pointers must be registered with Push_roots */

value cfconcat(value s1, value s2)
{
  mlsize_t len1, len2;
  value res;
  /* The Push_roots macro introduces a declaration and thus can be
     preceded only by declarations.  If necessary, put it inside a new
     block { ... }, in which Pop_roots must occur, too.  */
  Push_roots(r, 2);
  r[0] = s1;
  r[1] = s2;
  len1 = string_length(s1);
  len2 = string_length(s2);
  /* Allocating the result in the heap may cause the GC to run, which
     may move the strings given as arguments.  But in this case, the
     registered pointers r[0] and r[1] will be adjusted accordingly
     (while s1 and s2 won't be) so it is safe to use them below: */
  res = alloc_string(len1 + len2);
  /* Byte(v, i) is the proper way to refer to byte i of value v: */
  bcopy(&Byte(r[0],0), &Byte(res,0),    len1);
  bcopy(&Byte(r[1],0), &Byte(res,len1), len2);
  /* Unregister pointers r[0] and r[1] to avoid space leaks: */
  Pop_roots();
  return res;
}

value getting_notreg(value dummy) {
  valueptr mvp = get_valueptr("never registered");
  return Val_bool(mvp == (valueptr)NULL);
}

value getting_unreg(value dummy) {
  valueptr mvp = get_valueptr("unregistered");
  return Val_bool(mvp == (valueptr)NULL);
}

value using_notreg(value dummy) {
  valueptr mvp = get_valueptr("never registered");
  value val = get_value(mvp);
  return Val_bool(mvp == (valueptr)NULL);
}

value using_unreg(value dummy) {
  valueptr temp1_ptr = get_valueptr("temp1");
  value val;
  if (temp1_ptr == (valueptr)NULL)
    return Val_false;
  unregistervalue("temp1");
  val = get_value(temp1_ptr);  
  return Val_bool(val == (value)NULL);
}

value callfunction(value startval) {
  valueptr mlfunptr;
  value d;
  int i, max;
  Push_roots(r, 1);
  r[0] = startval;
  mlfunptr = get_valueptr("extrafun"); 
  max = Long_val(get_value(get_valueptr("steps"))); 
  d = r[0];
  Pop_roots();
  for (i=0; i<max; i++) 
    d = callbackptr(mlfunptr, d); 
  return d;
} 

EXTERNML value initialize_callbacktest(value dummy) {
  registercptr("call function",  callfunction);  

  registercptr("getting_notreg", getting_notreg);
  registercptr("getting_unreg",  getting_unreg);
  registercptr("using_notreg",   using_notreg);
  registercptr("using_unreg",    using_unreg);  

  /* Order of definition, registration, and use should be different: */
  registercptr("regcfdat", cfdat);
  registercptr("regcfu", cfu);
  registercptr("regcfi", cfi);
  registercptr("regcfc", cfc);
  registercptr("regcfr", cfr);
  registercptr("regcfs", cfs);
  registercptr("regcfb", cfb);
  registercptr("regcfcur", cfcur);
  registercptr("regcftup", cftup);
  registercptr("regcfrec", cfrec);
  registercptr("regcffun", cffun);
  registercptr("regcfrtup", cfrtup);
  registercptr("regcfrrec", cfrrec);
  registercptr("regcfconcat", cfconcat);
  return Val_unit;
}
