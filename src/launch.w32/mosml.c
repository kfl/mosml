#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "driver.h"

__declspec(dllimport) int caml_main(int argc, char * argv[]);

char* stdlib = NULL;
char* opt = "";
char* usefiles = "";
char* mosmltop = NULL;

int argc2;
char** argv2;

int main(int argc, char* argv[])
{
  int i;
  char * a;

  stdlib = getenv("MOSMLLIB");
  if (stdlib == NULL) 
  {
    fprintf(stderr, "Variable MOSMLLIB is undefined.\n");
    exit(2);
  }

  // argv2 will be filled as follows:
  //     argv[0], %MOSMLLIB%\mosmltop, -stdlib, %MOSMLLIB%, options, filenames

  argv2 = (char **) xmalloc( sizeof(char *) * (argc + 4) );

  argv2[0] = argv[0];
  argv2[2] = "-stdlib";
  argc2 = 4;

  for (i = 1; i < argc; i++) 
  {
    a = argv[i];
    if (eq(a, "-I") || eq(a, "-include") || eq(a, "-P") || eq(a, "-perv") ||
        eq(a, "-m") || eq(a, "-msgstyle")) 
    {
      argv2[argc2++] = a;
      i++;
      if( i < argc )
        argv2[argc2++] = argv[i];
    }
    else if (eq(a, "-valuepoly") || eq(a, "-imptypes"))
    {
      argv2[argc2++] = a;
    } 
    else if (eq(a, "-stdlib")) 
    {
      i++;
      if( i < argc )
        stdlib = argv[i];
    } 
    else if (prefix(a, "-")) 
    {
      fprintf(stderr, "Unknown option \"%s\", ignored\n", a);
    } 
    else 
    {
      /* reformat(&usefiles, "%s %s", usefiles, a); */
    }
  }

  for (i = 1; i < argc; i++) 
  {
    a = argv[i];
    if (eq(a, "-I") || eq(a, "-include") || eq(a, "-P") || eq(a, "-perv") ||
        eq(a, "-m") || eq(a, "-msgstyle")) 
      i++;
    else if (eq(a, "-valuepoly") || eq(a, "-imptypes"))
      ; 
    else if (eq(a, "-stdlib")) 
      i++;
    else if (prefix(a, "-")) 
      ;
    else 
    {
      argv2[argc2++] = a;
    }
  }


  format(&mosmltop, "%s\\mosmltop", stdlib);
  argv2[1] = mosmltop;
  argv2[3] = stdlib;
  argv2[argc2] = NULL;

  return caml_main(argc2, argv2);
}
