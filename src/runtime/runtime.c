/* runtime.c  */

#include "config.h"

#include "runtime.h"

#ifdef macintosh

/* 23Nov93  e */
/* 16Mar94  e  -- revised to not abuse time manager (!) */
/* 31Aug95  e  -- revised to abuse time manager less (!) */
/* 05Sep95  e  -- added mosml required stuff: gettimeofday, e_getrusage */

/* Keeping track of time spent
 - in gc
 - in multifinder
 - overall since startup
*/

#include <Timer.h>
#include <stdlib.h>

#include <time.h>
#include <utime.h>

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "debugger.h"
#include "mlvalues.h"
#include "freelist.h"
#include "gc.h"
#include "major_gc.h"

/* */

#define qRunTimes 8 /* must be at least three for: total, gc, mutlifinder */

#define aMILLION (1000000L)

#if GENERATINGPOWERPC
#pragma options align=mac68k
#endif

typedef struct tinfuRec
{
  TMTask TMTask;
  short filler;
  unsigned long  acc_secs;
    signed long acc_usecs;
} tinfuRec, *tinfuPtr;

#if GENERATINGPOWERPC
#pragma options align=reset
#endif

static tinfuRec gTimeInfo[qRunTimes];

#define TIMe(n) (gTimeInfo[n].TMTask)

/* 09Jan95 e */

static TimerUPP tmt_handlerUPP = NULL;

#if powerc
static pascal void tmt_handler(TMTaskPtr a1)
{
  ((tinfuPtr )a1)->acc_secs++;
  PrimeTime((QElemPtr )a1, -aMILLION);
}
#else
#ifndef __MWERKS__
static pascal void tmt_handler(void)
{
  asm
  {
    ADDQ.L #1, tinfuRec.acc_secs(a1)
    MOVE.L A1, A0
    MOVE.L #-aMILLION, D0
    DC.W 0xA05A ; PrimeTime
  }
}
#else
asm static pascal void tmt_handler(void)
{
    ADDQ.L #1, 0x18(A1) // tinfuRec.acc_secs(a1)
    MOVE.L A1, A0
    MOVE.L #-aMILLION, D0
    DC.W 0xA05A // PrimeTime
    RTS
}
#endif
#endif

static void cancel_timers(void)
{ int i;
  for ( i = 0; i < qRunTimes; i++ )
    // always! 31Aug95 e -- if (TIMe(i).tmAddr != NULL && TIMe(i).qType < 0 )
    { RmvTime((QElemPtr )&TIMe(i));
      TIMe(i).tmAddr = NULL;
    }
}

static time_t systime_init;

void init_timers(void)
{ int i;
  if (tmt_handlerUPP == NULL) tmt_handlerUPP = NewTimerProc(tmt_handler);
  for ( i = 0; i < qRunTimes; i++ )
    if (TIMe(i).tmAddr == NULL)
    { TIMe(i).tmAddr = tmt_handlerUPP;
      // only 0, 31Aug95 e -- InsXTime((QElemPtr )&TIMe(i));
      if ( i == 0 )
      { _atexit(cancel_timers);
        InsXTime((QElemPtr )&TIMe(0));
        systime_init = time(NULL);
        PrimeTime((QElemPtr )&TIMe(0), -aMILLION);
      }
    }
}

void beg_runtime(int i)
{ tinfuPtr p;
  if ( 0 >= i || i >= qRunTimes ) Debugger(); /* coding error */
  p = &gTimeInfo[i];
  p->TMTask.tmWakeUp = 0;
  p->TMTask.tmReserved = 0;
  InsXTime((QElemPtr )&(p->TMTask));
  PrimeTime((QElemPtr )&(p->TMTask), -aMILLION);
}

void acc_runtime(int i)
{ tinfuPtr p;
  if ( 0 > i || i >= qRunTimes ) Debugger(); /* coding error */
  p = &gTimeInfo[i];
  RmvTime((QElemPtr )&(p->TMTask));
  p->acc_usecs += aMILLION + p->TMTask.tmCount;
  if (p->acc_usecs >= aMILLION)
  {
    p->acc_secs += 1;
    p->acc_usecs -= aMILLION;
  }
  else if (p->acc_usecs < 0)
  {
    p->acc_secs -= 1;
    p->acc_usecs += aMILLION;
  }
}

static double double_zero = (double )0;

value get_timer(value x)                          /* ML */
{
  unsigned int i = Long_val(x);
  tinfuPtr p;

  if ( 0 > i || i >= qRunTimes) return copy_double(double_zero);
  p = &gTimeInfo[i];
  if (i == 0)
  {
    RmvTime((QElemPtr )&(p->TMTask));
    p->acc_usecs = aMILLION + p->TMTask.tmCount;
    InsXTime((QElemPtr )&(p->TMTask));
    PrimeTime((QElemPtr )&(p->TMTask), 0);
  }
  return copy_double(p->acc_secs + (p->acc_usecs / (double )aMILLION));
}

value beg_timer(value x)                          /* ML */
{
  unsigned int i = Long_val(x);

  if (i >= qRunTimes || i <= 2) return Val_false;
  beg_runtime(i);
  return Val_true;
}

value end_timer(value x)                          /* ML */
{
  unsigned int i = Long_val(x);

  if (i >= qRunTimes || i <= 2) return Val_false;
  acc_runtime(i);
  return Val_true;
}

value clr_timer(value x)                          /* ML */
{
  unsigned int i = Long_val(x);
  tinfuPtr p;

  if (i >= qRunTimes || i <= 2) return Val_false;
  p = &gTimeInfo[i];
  p->acc_secs  = 0;
  p->acc_usecs = 0;
  return Val_true;
}

/* runtime stats */

value e_getrusage( void )
{
  tinfuPtr p;
  unsigned long rts, sts, gts, uts;
           long rtu, stu, gtu, utu;
  value res = alloc (6, 0);
  /* snapshot run timer */
  p = &gTimeInfo[0];
  RmvTime((QElemPtr )&(p->TMTask));
  rts = p->acc_secs;            // elapsed time
  rtu = p->acc_usecs + aMILLION + p->TMTask.tmCount;
  InsXTime((QElemPtr )&(p->TMTask));
  PrimeTime((QElemPtr )&(p->TMTask), 0);
  /* compute "user" time */
  gts = gTimeInfo[1].acc_secs;  // gc time
  gtu = gTimeInfo[1].acc_usecs;
  sts = gTimeInfo[2].acc_secs;  // multifinder time
  stu = gTimeInfo[2].acc_usecs;
  uts = (rts - sts) - gts;
  utu = (rtu - stu) - gtu;
  while (utu < 0)         { utu += aMILLION; uts -= 1; }
  while (utu >= aMILLION) { utu -= aMILLION; uts += 1; }
  /* pathological case at startup really fries mosml... */
  if (uts > 0x3FFFFFFF)
  {
    uts = 0;
    utu = 1;
  }
  /* fill in SML record */
  Field (res, 0) = Val_long (gts);  // "gc"
  Field (res, 1) = Val_long (gtu);
  Field (res, 2) = Val_long (sts);  // "system"
  Field (res, 3) = Val_long (stu);
  Field (res, 4) = Val_long (uts);  // "user"
  Field (res, 5) = Val_long (utu);
  return res;
}

/* end of MacOS specific code */

#else

/* DOS, Unix, Win32 */

#ifdef WIN32
#include <sys/timeb.h>
#include <sys/utime.h>
#else
#include <sys/times.h>    
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>   
#endif

struct mosml_timeval gc_time = { (long) 0, (long) 0 };

void beg_gc_time(void)
{
#ifdef WIN32
  /*
  // Here I return sysTime = usrTime.
  // Perhaps, win32 enables sysTime and usrTime to be mesured
  // in an accurate way...
  //  Sergei Romanenko
  */
  struct timeb t;
  ftime(&t);
  gc_time.tv_sec  -=  t.time;
  gc_time.tv_usec -= ((long) t.millitm) * 1000;
#elif defined(hpux) || defined(__svr4__)
  struct tms buffer;

  long persec = sysconf(_SC_CLK_TCK);
  times(&buffer);
  gc_time.tv_sec  -= buffer.tms_utime / persec;
  gc_time.tv_usec -= (buffer.tms_utime % persec) * (1000000 / persec);
#else
  struct rusage rusages;

  getrusage(RUSAGE_SELF, &rusages);
  gc_time.tv_sec  -= rusages.ru_utime.tv_sec;
  gc_time.tv_usec -= rusages.ru_utime.tv_usec;
#endif

  if (gc_time.tv_usec < 0) {
    gc_time.tv_usec += 1000000;
    gc_time.tv_sec  -= 1;
  }
}

void end_gc_time(void)
{
#ifdef WIN32
  /*
  // Here I return sysTime = usrTime.
  // Perhaps, win32 enables sysTime and usrTime to be mesured
  // in an accurate way...
  //  Sergei Romanenko
  */
  struct timeb t;
  ftime(&t);
  gc_time.tv_sec  +=  t.time;
  gc_time.tv_usec += ((long) t.millitm) * 1000;
#elif defined(hpux) || defined(__svr4__)
  struct tms buffer;

  long persec = sysconf(_SC_CLK_TCK);
  times(&buffer);
  gc_time.tv_sec  += buffer.tms_utime / persec;
  gc_time.tv_usec += (buffer.tms_utime % persec) * (1000000 / persec);
#else
  struct rusage rusages;

  getrusage(RUSAGE_SELF, &rusages);
  gc_time.tv_sec  += rusages.ru_utime.tv_sec;
  gc_time.tv_usec += rusages.ru_utime.tv_usec;
#endif

  if (gc_time.tv_usec >= 1000000) {
    gc_time.tv_usec -= 1000000;
    gc_time.tv_sec  += 1;
  }

}

#endif
