#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

double foo;

void access_double(p)
     double * p;
{
  foo = *p;
  *p = foo;
}

jmp_buf failure;

void sig_handler()
{
  longjmp(failure, 1);
}

main()
{
#define ARRSIZE 100
  long n[ARRSIZE];
  long* p = n;
  int i, res;
  signal(SIGSEGV, sig_handler);
  signal(SIGBUS, sig_handler);
  if(setjmp(failure) == 0) {
    for (i=0; i<ARRSIZE; i++) {
      access_double((double *) p);
      p++;
    }
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
  signal(SIGBUS, SIG_DFL);
  exit(res);
}

