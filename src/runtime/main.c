/* Start-up code */

#include <setjmp.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#ifdef WIN32
#include <io.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "version.h"
#include "alloc.h"
#include "exec.h"
#include "fail.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "globals.h"
#include "intext.h"
#include "io.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "sys.h"
#include "interp.h"
#include "mosml.h"

#ifndef macintosh
#if defined(__STDC__) || defined(WIN32)
#include <stdlib.h>
#else
extern char *getenv ();
#endif /* __STDC__ */
#endif /* not macintosh */

#ifndef O_BINARY
#define O_BINARY 0
#endif

header_t first_atoms[256];
bytecode_t start_code;
asize_t code_size;

static void init_atoms(void)
{
  int i;
  for(i = 0; i < 256; i++) first_atoms[i] = Make_header(0, i, White);
}

static unsigned long read_size(unsigned char * p)
{
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}

#define FILE_NOT_FOUND (-1)
#define TRUNCATED_FILE (-2)
#define BAD_MAGIC_NUM (-3)

static int read_trailer(int fd, struct exec_trailer * trail)
{
  unsigned char buffer[TRAILER_SIZE];

  lseek(fd, (long) -TRAILER_SIZE, 2);
  if (read(fd, (char*)buffer, TRAILER_SIZE) < TRAILER_SIZE) 
    return TRUNCATED_FILE;
  trail->code_size = read_size(buffer);
  trail->data_size = read_size(buffer+4);
  trail->symbol_size = read_size(buffer+8);
  trail->debug_size = read_size(buffer+12);
  trail->magic = read_size(buffer+16);
  if (trail->magic == EXEC_MAGIC) return 0; else return BAD_MAGIC_NUM;
}

extern char * searchpath();

int attempt_open(char ** name, struct exec_trailer * trail, int do_open_script)
{
  char * truename;
  int fd;
  int err;
  char buf [2];

  truename = searchpath(*name);
  if (truename == 0) truename = *name; else *name = truename;
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1) return FILE_NOT_FOUND;
  if (!do_open_script){
    err = read (fd, buf, 2);
    if (err < 2) return TRUNCATED_FILE;
    if (buf [0] == '#' && buf [1] == '!') return BAD_MAGIC_NUM;
  }
  err = read_trailer(fd, trail);
  if (err != 0) { close(fd); return err; }
  return fd;
}

/* Invocation of camlrun: 4 cases.

   1.  runtime + bytecode
       user types:  camlrun [options] bytecode args...
       arguments:  camlrun [options] bytecode args...

   2.  bytecode script
       user types:  bytecode args...
   2a  (kernel 1) arguments:  camlrun ./bytecode args...
   2b  (kernel 2) arguments:  bytecode bytecode args...

   3.  concatenated runtime and bytecode
       user types:  composite args...
       arguments:  composite args...

Algorithm:
  1-  If argument 0 is a valid byte-code file that does not start with #!,
      then we are in case 3 and we pass the same command line to the
      Caml Light program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

#ifdef WIN32
__declspec(dllexport) int caml_main(int argc, char * argv[])
#elif HAS_UI
int caml_main(int argc, char * argv[])
#else
int main(int argc, char * argv[])
#endif
{
  int fd;
  struct exec_trailer trail;
  int i;
  struct longjmp_buffer raise_buf;
  struct channel * chan;
  int verbose_init = 0, percent_free_init = Percent_free_def;
  long minor_heap_init = Minor_heap_def, heap_chunk_init = Heap_chunk_def;
  char * debugger_address = NULL;
#ifdef MSDOS
  extern char ** check_args();
  argv = check_args(argv);
#endif

#ifdef DEBUG
  verbose_init = 1;
#endif

#ifdef WIN32
  BOOL fOk;
  fOk = SetConsoleCtrlHandler(NULL, FALSE);
#endif

  i = 0;
  fd = attempt_open(&argv[0], &trail, 0);

  if (fd < 0) {

    for(i = 1; i < argc && argv[i][0] == '-'; i++) {
      switch(argv[i][1]) {
#ifdef DEBUG
      case 't': {
        extern int trace_flag;
        trace_flag = 1;
        break;
      }
#endif
      case 'v':
        verbose_init = 1;
        break;
      case 'V':
        fprintf(stderr, "The Caml Light runtime system, version %s\n",
                VERSION);
	sys_exit(Val_int(0));
      default:
        fatal_error_arg("Unknown option %s.\n", argv[i]);
      }
    }

    if (argv[i] == 0)
      fatal_error("No bytecode file specified.\n");

    fd = attempt_open(&argv[i], &trail, 1);

    switch(fd) {
    case FILE_NOT_FOUND:
      fatal_error_arg("Fatal error: cannot find file %s\n", argv[i]);
      break;
    case TRUNCATED_FILE:
    case BAD_MAGIC_NUM:
      fatal_error_arg(
        "Fatal error: the file %s is not a bytecode executable file\n",
        argv[i]);
      break;
    }
  }

  /* Runtime options.  The option letter is the first letter of the
     last word of the ML name of the option (see [lib/gc.mli]). */

  { char *opt = getenv ("CAMLRUNPARAM");
    if (opt != NULL){
      while (*opt != '\0'){
	switch (*opt++){
	case 's': sscanf (opt, "=%ld", &minor_heap_init); break;
	case 'i': sscanf (opt, "=%ld", &heap_chunk_init); break;
	case 'o': sscanf (opt, "=%d", &percent_free_init); break;
	case 'v': sscanf (opt, "=%d", &verbose_init); break;
	}
      }
    }
  }

#ifdef HAS_SOCKETS
  if (debugger_address == NULL)
    debugger_address = getenv("CAML_DEBUG_SOCKET");
#endif

  if (setjmp(raise_buf.buf) == 0) {

    external_raise = &raise_buf;

    init_gc (minor_heap_init, heap_chunk_init, percent_free_init,
	     verbose_init);
    init_stack();
    init_atoms();

    lseek(fd, - (long) (TRAILER_SIZE + trail.code_size + trail.data_size
                        + trail.symbol_size + trail.debug_size), 2);

    code_size = trail.code_size;

#if defined(DIRECT_JUMP) && defined(THREADED)
    start_code = (bytecode_t) alloc_string(code_size);
#else
    start_code = (bytecode_t) stat_alloc(code_size);
#endif
    if (read(fd, (char *) start_code, code_size) != code_size)
      fatal_error("Fatal error: truncated bytecode file.\n");

#if defined(MOSML_BIG_ENDIAN) && !defined(ALIGNMENT)
    fixup_endianness(start_code, code_size);
#endif

    chan = open_descr(fd);
    global_data = intern_val(chan);
    modify(&Field(global_data, GLOBAL_DATA), global_data);
    close_in(chan);

    sys_init(argv + i);
    interprete(/* mode=init */ 0, NULL, 0, NULL); 
    interprete(/* mode=byte exec */ 1, start_code, code_size, NULL);
    sys_exit(Val_int(0));

  } else {
    if (Field(exn_bucket, 0) == Field(global_data, SYS__EXN_MEMORY))
      fatal_error ("Fatal error: out of memory.\n");
    else {
      char* buf = (char*)malloc(201);
      char* exnmsg = exnmessage_aux(exn_bucket);
#if defined(__CYGWIN__) || defined(hpux)
      sprintf(buf, "Uncaught exception:\n%s\n", exnmsg);
#elif defined(WIN32)
      _snprintf(buf, 200, "Uncaught exception:\n%s\n", exnmsg);
#else
      snprintf(buf, 200, "Uncaught exception:\n%s\n", exnmsg);
#endif
      free(exnmsg);
      fatal_error(buf);
    }
  }
  return 0;			/* Can't get here */
}

