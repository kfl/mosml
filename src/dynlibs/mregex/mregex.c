/* Moscow ML interface to POSIX regular expressions, C side */
/* sestoft@dina.kvl.dk 1998-12-25, 1999-01-02 */

/* This code uses and builds values of the abstract type
   Substring.substring defined in mosmllib/Substring.sml.  Any change
   in the representation necessitates changes below. */

#include <stdlib.h>
#include <sys/types.h>
#include "regex-0.12/regex.h"

/* Moscow ML specific includes: */

#include <mlvalues.h>		/* For Val_unit, Long_val, String_val, ... */
#include <alloc.h>		/* For copy_string, alloc_string, ...      */
#include <memory.h>		/* For Modify, Push_roots, Pop_roots       */
#include <str.h>		/* For string_length                       */
#include <fail.h>		/* For failwith                            */

/* Special version of regexec, defined at the end of this file */

int mosml_regexec();

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* Representation of substrings (ML type Substring.substring): */

#define base_susval(x) (Field(x, 0))
#define basestr_susval(x) (String_val(base_susval(x)))
#define start_susval(x) (Long_val(Field(x, 1)))
#define susaddr_susval(x) (&((basestr_susval(x))[start_susval(x)]))
#define len_susval(x) (Long_val(Field(x, 2)))

/* Representation of a compiled regular expression (ML type regex):
  
   A finalized 4-tuple:

              header with Final_tag
	      0: finalization function regex_finalize
	      1: pointer to malloc'ed C regex_t struct
	      2: nmatch (C integer)
	      3: pmatch (C array of regmatch_t)

   whose component 0 is a pointer to the finalizing function
   regex_finalize, whose component 1 is a pointer to a regex_t
   value, whose component 2 is a C int value giving an upper bound on
   the number of named substrings, plus 1, and whose component 3 is a
   malloc'ed array of nmatch regmatch_t structs. */

#define regex_val(x) ((regex_t*)(Field(x, 1)))
#define nmatch_val(x) ((int)(Field(x, 2)))
#define pmatch_val(x) ((regmatch_t*)(Field(x, 3)))
 
/* Finalize and deallocate a regex value */

void regex_finalize(value regexval) { 
  regfree(regex_val(regexval));
  free(regex_val(regexval));
  free(pmatch_val(regexval));
}

/* Allocate a regex value */
/* Finalized objects are abstract, so can store nmatch as C int */

value regex_alloc(regex_t* preg, int nmatch) { 
  value res = alloc_final(4, &regex_finalize, 1, 10000);
  Field(res, 1) = (value)preg;
  Field(res, 2) = (value)nmatch;
  Field(res, 3) = (value)(malloc(nmatch * sizeof(regmatch_t)));
  return res;
}

/* Obtain error string after a regex compilation error */

char* regerrorstring(int errcode, regex_t* preg) {
  /* Get the error message size: */
  int n = regerror(errcode, preg, NULL, 0);
  char* errmsg = (char*)(malloc(n));
  /* Get the error message itself: */
  regerror(errcode, preg, errmsg, n);
  return errmsg;
}

/* Find the maximal number of named substrings in the match, plus 1 */

int maxnmatch(value patval) {
  char* pat = String_val(patval);
  int patlen = string_length(patval);
  int nmatch = 1;
  int i;		
  for (i=0; i<patlen; i++)
    if (pat[i] == '(')
      nmatch++;
  return nmatch;
}

/* Return (REG_EXTENDED, REG_ICASE, REG_NEWLINE, REG_NOTBOL, REG_NOTEOL) */

/* ML type: unit -> word * word * word * word * word */

EXTERNML value mregex_getflags(value dummy) {
  value res = alloc_tuple(5);
  Field(res, 0) = Val_long(REG_EXTENDED);
  Field(res, 1) = Val_long(REG_ICASE);
  Field(res, 2) = Val_long(REG_NEWLINE);
  Field(res, 3) = Val_long(REG_NOTBOL);
  Field(res, 4) = Val_long(REG_NOTEOL);
  return res;
}

/* ML type: string -> word -> regex */

EXTERNML value mregex_regcomp(value patval, value cflagsval) {
  regex_t* preg = (regex_t*)(malloc(sizeof(regex_t)));
  char* pat = String_val(patval);
  int cflags = Long_val(cflagsval);
  int comperror = regcomp(preg, pat, cflags);
  if (comperror != 0) {
    char* errmsg = regerrorstring(comperror, preg);
    free(preg);
    failwith(errmsg);		/* Should free errmsg too, but never mind */
  } else 
    return regex_alloc(preg, maxnmatch(patval));
  /* Unreachable */
  return NONE;
}

/* Do the actual regex matching, returning substrings. */
/* The given substring must be a suffix of a null-terminated string. */

value regmatch_sus(regex_t* preg, int nmatch, regmatch_t pmatch[], 
		   int eflags, value susval) {
  char* str = susaddr_susval(susval);
  int len = len_susval(susval);
  int matcherror = mosml_regexec(preg, str, len, nmatch, pmatch, eflags);
  if (matcherror == 0) {
    /* Find actual number of named substrings */
    value res;
    int strlen = len_susval(susval);
    int last = 0;
    int i = 0;
    // printf("pmatch[%d].rm_so = %d\n", i, pmatch[i].rm_so);
    /* In a pattern such as "(ab)|(cb)", at most one of the
       subexpressions takes part in each match.  In these cases, the
       value -1 will be stored in rm_so and rm_eo of the unused
       substrings.  In other cases (e.g. when pmatch has room for
       excess substrings, various kinds of garbage is stored instead.
       Hence we need to explicitly check for wellformedness of
       substrings.  
    */
    while (i < nmatch 
	   && (pmatch[i].rm_so == -1 ||
	       (last <= pmatch[i].rm_so
		&& pmatch[i].rm_so <= pmatch[i].rm_eo
		&& pmatch[i].rm_eo <= strlen))) {
      if (pmatch[i].rm_so != -1) 
	last = pmatch[i].rm_so;
      i++;
      // printf("pmatch[%d].rm_so = %d\n", i, pmatch[i].rm_so);
    }
    nmatch = i;
    /* Create the ML return value: SOME(vector of nmatch substrings) */
    {
      Push_roots(r, 2);
      r[0] = base_susval(susval); 
      /* Allocate and initialize vector in ML heap */
      r[1] = alloc_tuple(nmatch); 
      for (i=0; i<nmatch; i++) 
	modify(&Field(r[1], i), Atom(0));
      /* Fill in substrings in vector */
      for (i=0; i<nmatch; i++) {
	/* Allocate a value of type substring = string * int * int */
	value substr = alloc_tuple(3);
	/* Component 0: the base string pointer = base(susval) */
	modify(&Field(substr, 0), r[0]); 
	if (pmatch[i].rm_so != -1) { /* Substring took part in the match */
	  /* Component 1: the substring start byte offset  */
	  modify(&Field(substr, 1), 
		 Val_long(start_susval(susval) + pmatch[i].rm_so));
	  /* Component 2: the substring length in bytes    */
	  modify(&Field(substr, 2), 
		 Val_long(pmatch[i].rm_eo - pmatch[i].rm_so));
	} else {		/* Substring did not take part in the match */
	  /* Make an empty substring at the beginning of the base string */
	  /* Component 1: the substring start byte offset  */
	  modify(&Field(substr, 1), Val_long(0));
	  /* Component 2: the substring length in bytes    */
	  modify(&Field(substr, 2), Val_long(0));
	}
	/* Store the substring in the vector's position i */
	modify(&Field(r[1], i), substr);
      }
      /* Return SOME(vec) */
      res = alloc(1, SOMEtag); 
      modify(&Field(res, 0), r[1]);
      Pop_roots();
    }
    return res;
  } else /* matcherror == REG_NOMATCH */
    return NONE;
}

/* Do the actual regex matching, returning true or false: */

value regmatch_bool(regex_t* preg, int eflags, value susval) {
  char* str = susaddr_susval(susval);
  int len = len_susval(susval);
  int matcherror = mosml_regexec(preg, str, len, 0, NULL, eflags | REG_NOSUB);
  return Val_bool(matcherror == 0);
}

/* ML type: regex -> word -> substring -> substring vector option */

EXTERNML value mregex_regexec_sus(value regex, value eflagsval, 
				  value susval) {
  regex_t* preg = regex_val(regex);
  int nmatch = nmatch_val(regex);
  regmatch_t* pmatch = pmatch_val(regex);
  int eflags = Long_val(eflagsval);
  return regmatch_sus(preg, nmatch, pmatch, eflags, susval);
}

/* ML type: regex -> word -> substring -> bool */

EXTERNML value mregex_regexec_bool(value regex, value eflagsval, 
				   value susval) {
  regex_t* preg = regex_val(regex);
  int eflags = Long_val(eflagsval);
  return regmatch_bool(preg, eflags, susval);
}

/* Optimization: Compile and run match without allocating regex block: */

/* ML type: string -> word -> word -> substring -> substring vector option */

EXTERNML value mregex_regmatch_sus(value patval, value cflagsval, 
				   value eflagsval, value susval) {
  regex_t patbuf;
  char* pat = String_val(patval);
  int cflags = Long_val(cflagsval);
  int comperror = regcomp(&patbuf, pat, cflags);
  if (comperror != 0) 
    failwith(regerrorstring(comperror, &patbuf));
  else {
    int nmatch = maxnmatch(patval);
    regmatch_t* pmatch = (regmatch_t*)(malloc(nmatch * sizeof(regmatch_t)));
    int eflags = Long_val(eflagsval);
    value res = regmatch_sus(&patbuf, nmatch, pmatch, eflags, susval);
    free(pmatch);
    regfree(&patbuf);
    return res;
  }
  /* Unreachable: */
  return NONE;
}

/* Optimization: Compile and run match without allocating a regex block: */

/* ML type: string -> word -> word -> substring -> bool */

EXTERNML value mregex_regmatch_bool(value patval, value cflagsval, 
				    value eflagsval, value susval) {
  regex_t patbuf;
  char* pat = String_val(patval);
  int cflags = Long_val(cflagsval);
  int comperror = regcomp(&patbuf, pat, cflags);
  if (comperror != 0) 
    failwith(regerrorstring(comperror, &patbuf));
  else {
    int eflags = Long_val(eflagsval);
    value res = regmatch_bool(&patbuf, eflags, susval);
    regfree(&patbuf);
    return res;
  }
  /* Unreachable: */
  return Val_false;
}

/* This is copied from GNU regex-0.12 file regex.c, and renamed from
   regexec to mosml_regexec.  The only change is that mosml_regexec
   receives the (sub)string's length as an argument, and so avoids
   calling strlen.  This dramatically speeds up the replace,
   substitute, tokens, and fields functions in the Regex ML structure:
   the asymptotic execution time changes from quadratic to linear.

   It is pretty silly to have to copy the entire function just to
   achieve this.  Superficially, the fault is with POSIX 1003.2 for
   not accommodating searches in substrings of long strings.  More
   fundamentally, C's notion of null-terminated string is lame: taking
   time O(n) to determine the length of a string is damn poor.
   
   sestoft@dina.kvl.dk 
*/

typedef char boolean;
#define false 0
#define true 1
#define TALLOC(n, t) ((t *) malloc ((n) * sizeof (t)))

int
mosml_regexec (preg, string, len /* NEW */, nmatch, pmatch, eflags)
    const regex_t *preg;
    const char *string; 
    int len;			/* NEW */
    size_t nmatch; 
    regmatch_t pmatch[]; 
    int eflags;
{
  int ret;
  struct re_registers regs;
  regex_t private_preg;
  boolean want_reg_info = !preg->no_sub && nmatch > 0;

  private_preg = *preg;
  
  private_preg.not_bol = !!(eflags & REG_NOTBOL);
  private_preg.not_eol = !!(eflags & REG_NOTEOL);
  
  /* The user has told us exactly how many registers to return
     information about, via `nmatch'.  We have to pass that on to the
     matching routines.  */
  private_preg.regs_allocated = REGS_FIXED;
  
  if (want_reg_info)
    {
      regs.num_regs = nmatch;
      regs.start = TALLOC (nmatch, regoff_t);
      regs.end = TALLOC (nmatch, regoff_t);
      if (regs.start == NULL || regs.end == NULL)
        return (int) REG_NOMATCH;
    }

  /* Perform the searching operation.  */
  ret = re_search (&private_preg, string, len,
                   /* start: */ 0, /* range: */ len,
                   want_reg_info ? &regs : (struct re_registers *) 0);
  
  /* Copy the register information to the POSIX structure.  */
  if (want_reg_info)
    {
      if (ret >= 0)
        {
          unsigned r;

          for (r = 0; r < nmatch; r++)
            {
              pmatch[r].rm_so = regs.start[r];
              pmatch[r].rm_eo = regs.end[r];
            }
        }

      /* If we needed the temporary register info, free the space now.  */
      free (regs.start);
      free (regs.end);
    }

  /* We want zero return to mean success, unlike `re_search'.  */
  return ret >= 0 ? (int) REG_NOERROR : (int) REG_NOMATCH;
}
