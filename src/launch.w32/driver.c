#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
//#include <windows.h>
#include <errno.h>

#include "driver.h"

char * xmalloc(int size)
{
  char * res;

  res = malloc(size);
  if (res == NULL) {
    fprintf(stderr, "Out of memory\n");
    exit(2);
  }
  return res;
}

int suffix(char * name, char * suff)
{
  int lname = strlen(name);
  int lsuff = strlen(suff);

  return lname >= lsuff && strcmp(name + lname - lsuff, suff) == 0;
}

int prefix(char * name, char * pref)
{
  return strncmp(name, pref, strlen(pref)) == 0;
}

void format(char ** res, char * fmt, ...)
{
  va_list ap;
  char buffer[1024];
  
  va_start(ap, fmt);
  vsprintf(buffer, fmt, ap);
  *res = xmalloc(strlen(buffer) + 1);
  strcpy(*res, buffer);
}

void reformat(char ** res, char * fmt, ...)
{
  va_list ap;
  char buffer[1024];
  
  va_start(ap, fmt);
  vsprintf(buffer, fmt, ap);
  if (**res != 0) free(*res);
  *res = xmalloc(strlen(buffer) + 1);
  strcpy(*res, buffer);
}

