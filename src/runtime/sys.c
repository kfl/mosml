/* Basic system calls */

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef WIN32
#include <io.h>
#include <direct.h>
#else
#include <unistd.h>
#endif
#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "globals.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "io.h"

#ifdef HAS_STRERROR

extern char * strerror();

char * error_message(void)
{
  return strerror(errno);
}

#else

extern int sys_nerr;
extern char * sys_errlist [];

char * error_message(void)
{
  if (errno < 0 || errno >= sys_nerr)
    return "unknown error";
  else
    return sys_errlist[errno];
}

#endif /* HAS_STRERROR */

char* globalexn[] = { 
       "Out_of_memory",
       "Invalid_argument",
       "Graphic_failure",
       "SysErr",
       "Fail",
       "Size",
       "Interrupt",
       "Subscript",
       "Chr",
       "Div",
       "Domain",
       "Ord",
       "Overflow",
       "Bind",
       "Match",
       "Io" };

void sys_error(char * arg)
{
  char * err = error_message();
  value exnarg;

  /* Raise SysErr with argument (err, SOME errno) */

  Push_roots(r, 2);
  r[0] = copy_string(err);	/* The error message string	*/

  r[1] = alloc(1, SOMEtag);	/* The SOME errno object	*/
  Field(r[1], 0) = Val_long(errno);

  exnarg = alloc_tuple(2);	/* The argument tuple		*/
  Field(exnarg, 0) = r[0];
  Field(exnarg, 1) = r[1];
  Pop_roots();

  raiseprimitive1(SYS__EXN_SYSERR, exnarg);
}

void sys_exit(value retcode)          /* ML */
{
  flush_stdouterr();
  exit(Int_val(retcode));
}

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

static int sys_open_flags[] = {
  O_APPEND, O_BINARY, O_CREAT, O_EXCL, O_RDONLY, O_RDWR, 
  O_TEXT, O_TRUNC, O_WRONLY
};
#ifdef macintosh
static int sys_text_flags []  = { 0, 0, 0, 0, 0, 0, 0, 0, 1 };
static int sys_write_flags [] = { 0, 1, 1, 0, 0, 0, 0, 0, 0 };
#endif

value sys_open(value path, value flags, value perm) /* ML */
{
  int ret;
#ifdef macintosh
  extern void set_file_type (char *name, long type);
#if defined(THINK_C) || defined(__MWERKS__)
# define FILE_NAME_SIZE 256
  char filename_temp[FILE_NAME_SIZE];
  char *expanded;
  extern char *unix_to_mac_filename(char *, char *, int);
  expanded = unix_to_mac_filename(String_val(path), filename_temp, FILE_NAME_SIZE);
  if (expanded == NULL)
    ret = -1;
  else
    ret = open(expanded, convert_flag_list(flags, sys_open_flags));
  if ( ret != -1 && convert_flag_list (flags, sys_text_flags)
  	             && convert_flag_list (flags, sys_write_flags))
    set_file_type (expanded, 'TEXT');
#else
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags));
  if (ret != -1 && convert_flag_list (flags, sys_text_flags))
    set_file_type (String_val (path), 'TEXT');
#endif
#else
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags),
             Int_val(perm));
#endif
  if (ret == -1) sys_error(String_val(path));
  return Val_long(ret);
}

value sys_close(value fd)             /* ML */
{
  if (close(Int_val(fd)) != 0) sys_error(NULL);
  return Atom(0);
}

value sys_remove(value name)          /* ML */
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(String_val(name));
  return Atom(0);
}

value sys_rename(value oldname, value newname) /* ML */
{
#ifdef HAS_RENAME
  if (rename(String_val(oldname), String_val(newname)) != 0) 
    sys_error(String_val(oldname));
#else
  invalid_argument("rename: not implemented");
#endif
  return Atom(0);
}

value sys_chdir(value dirname)        /* ML */
{
  if (chdir(String_val(dirname)) != 0) sys_error(String_val(dirname));
  return Atom(0);
}

value sys_getenv(value var)           /* ML */
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) {
    raiseprimitive0(SYS__EXN_ARGUMENT);
  }
  return copy_string(res);
}

value sys_system_command(value command)   /* ML */
{
  int retcode = system(String_val(command));
  if (retcode == -1) sys_error(String_val(command));
  return Val_int(retcode);
}

static int sys_var_init[] = {
  0400, 0200, 0100,
  0040, 0020, 0010,
  0004, 0002, 0001,
  04000, 02000,
  0444, 0222, 0111
};

/* Create an exn name = a string ref, from a null-terminated string,
   possibly in C malloc space */

value mkexnname(char* name) {
  value ref;
  Push_roots(r, 1);
  r[0] = copy_string(name);
  ref = alloc_shr(1, Reference_tag);
  modify(&Field(ref, 0), r[0]);
  Pop_roots();
  return ref;
}

/* Create an exn value = a pair of a string ref and () : unit */

value mkexn0val(value exnname) {
  value exnval = alloc_tuple(2);
  modify(&Field(exnval, 0), Field(global_data, exnname));
  modify(&Field(exnval, 1), Val_unit);
  return exnval;
}

void sys_init(char ** argv)
{
  value v;
  int i;

  #ifndef MSDOS
  void init_float_handler();
  init_float_handler();
  #endif

  v = copy_string_array(argv);
  modify(&Field(global_data, SYS__COMMAND_LINE), v);
  for (i = SYS__S_IRUSR; i <= SYS__S_IXALL; i++)
    Field(global_data, i) = Val_long(sys_var_init[i - SYS__S_IRUSR]);
  Field(global_data, SYS__INTERACTIVE) = Val_false;
  Field(global_data, SYS__MAX_VECT_LENGTH) = Val_long(Max_wosize);
  Field(global_data, SYS__MAX_STRING_LENGTH) =
    Val_long(Max_wosize * sizeof(value) - 2);

  /* Allocate the exn names for pervasize dynamic exceptions */
  for (i = SYS__FIRST_EXN; i <= SYS__LAST_EXN ; i++) {
    value exn = mkexnname(globalexn[i - SYS__FIRST_EXN]);
    modify(&Field(global_data, i), exn);
  }
  /* Allocate some exn values for use in interprete */
  modify(&Field(global_data, EXN_INTERRUPT), mkexn0val(SYS__EXN_INTERRUPT));
  modify(&Field(global_data, EXN_DIV),       mkexn0val(SYS__EXN_DIV));
  modify(&Field(global_data, EXN_OVERFLOW),  mkexn0val(SYS__EXN_OVERFLOW));
}

/* Handling of user interrupts and floating-point errors */

#ifdef WIN32
static int catch_break = 0;
static int sigint_pending = 0;

void poll_break()
{
  if (catch_break == 0)
    return;
  if (sigint_pending)
  {
    sigint_pending = 0;
    signal_handler = raise_break_exn;
    signal_number = 0;
    execute_signal();
  } 
}
#endif

#ifndef MSDOS

/* Added by sestoft@dina.kvl.dk to make signals work under recent linuxes: */

void mysignal(int signum, sighandler_return_type (*handler)(int)) {
#ifdef linux
  struct sigaction sigact;
  sigset_t emptyset;
  sigemptyset(&emptyset);
  sigact.sa_handler  = handler;
  sigact.sa_mask     = emptyset;
  sigact.sa_flags    = SA_NOMASK;
  sigaction(signum, &sigact, 0); 
#else
  signal(signum, handler);
#endif
}

sighandler_return_type intr_handler(int sig)
{
#ifndef BSD_SIGNALS
  mysignal (SIGINT, intr_handler);
#endif
#ifndef WIN32
  signal_handler = raise_break_exn;
  signal_number = 0;
  execute_signal();
#else
  sigint_pending = 1;
#endif
}

value sys_catch_break(value onoff)    /* ML */
{
#ifdef WIN32
  catch_break = Tag_val(onoff);
#endif
  if (Tag_val(onoff))
    mysignal(SIGINT, intr_handler);
  else
    mysignal(SIGINT, SIG_DFL);
  return Atom(0);
}

sighandler_return_type float_handler(int sig)
{
#ifndef BSD_SIGNALS
  mysignal (SIGFPE, float_handler);
#endif
  if (float_exn == Field(global_data, SYS__EXN_FAIL))
    failwith("floating point error");
  else
    raiseprimitive0(float_exn);
}

void init_float_handler(void)
{
  mysignal(SIGFPE, float_handler);
}
#endif

/* Search path function */

#ifndef MSDOS
#ifndef macintosh

char * searchpath(char * name)
{
  static char fullname[512];
  char * path;
  char * p;
  char * q;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == 0) return 0;
  while(1) {
    p = fullname;
    while (*path != 0 && *path != ':') {
      *p++ = *path++;
    }
    if (p != fullname) *p++ = '/';
    q = name;
    while (*q != 0) {
      *p++ = *q++;
    }
    *p = 0;
    if (access(fullname, 1) == 0) return fullname;
    if (*path == 0) return 0;
    path++;
  }
}

#endif
#endif

