#include <stddef.h>
#include <std.h>
#include <dos.h>
#include "mlvalues.h"
#include "signals.h"
#include "instruct.h"
#include "fail.h"

CODE raise_break_exn;

static int catch_break = 0;

value sys_catch_break(onoff)
     value onoff;
{
  union REGS r;
  catch_break = Tag_val(onoff);
  r.x.ax = 0x3301;
  r.x.dx = catch_break ? 2 : 1;
  int86(0x21, &r, &r);
  return Atom(0);
}

void poll_break()
{
  union REGS r;
  if (catch_break == 0) return;
  r.x.ax = 0x3303;
  int86(0x21, &r, &r);
  if (r.x.dx != 0) {
    signal_handler = raise_break_exn;
    signal_number = 0;
    execute_signal();
  } 
}

/* Special input function for MS-DOS (to take advantage of external line
   editors when reading from the console. */

static struct {
  unsigned char max_len;
  unsigned char act_len;
  char data[200];
} read_buffer;

static int stdin_is_console = -1;
static int stdin_at_eof = 0;	/* Valid only if stdin_is_console==1 */

int msdos_read(fd, ptr, len)
     int fd;
     char * ptr;
     unsigned len;
{ 
  if (fd == 0) {
    if (stdin_is_console == -1) {
      union REGS regs;         
      regs.x.ax = 0x4400;
      regs.x.bx = fd;
      intdos(&regs, &regs);
      stdin_is_console = (regs.x.dx & 0x81) == 0x81;
     }
    /* Modified for Moscow SML 1994-08-23 to detect control-Z = 0x1A */
    if (stdin_is_console)
      if (stdin_at_eof) 
	return 0;
      else
	{
          read_buffer.max_len = sizeof(read_buffer.data) - 1;
          if (len <= read_buffer.max_len) read_buffer.max_len = len - 1;
          if (bdosptr(0xA, &read_buffer, 0) != 0) poll_break();
          bdos(0x2, '\n', 0);
          read_buffer.data[read_buffer.act_len] = '\n';
          bcopy(read_buffer.data, ptr, read_buffer.act_len + 1);
	  {int i;
           for (i=0; (i<read_buffer.act_len)&&(read_buffer.data[i]!=0x1A);
		 i++);
           if (read_buffer.data[i] == 0x1A)
	     { 
	       stdin_at_eof = 1;
	       return i;
	     }
	   else
	     return read_buffer.act_len + 1;
	  }
	}
      }
      return read(fd, ptr, len);
}

/* Search path function */

char * searchpath(name)
     char * name;
{
  static char fullname[256];
  char * path;
  char * p;
  char * q;

  if (name[1] == ':') return name;
  for (p = name; *p != 0; p++) {
    if (*p == '\\') return name;
  }
  path = getenv("PATH");
  if (path == 0) return 0;
  while(1) {
    p = fullname;
    while (*path != 0 && *path != ';') {
      *p++ = *path++;
    }
    if (p != fullname) *p++ = '\\';
    q = name;
    while (*q != 0) {
      *p++ = *q++;
    }
    *p = 0;
    if (access(fullname, 4) == 0) return fullname;
    *p++ = '.';
    *p++ = 'e';
    *p++ = 'x';
    *p++ = 'e';
    *p = 0;
    if (access(fullname, 4) == 0) return fullname;
    if (*path == 0) return 0;
    path++;
  }
}

/* To reconstruct the command-line arguments when they are passed through
   the environment. */

char ** parse_args(cmdname, cmdline)
	char * cmdname, * cmdline;
{
  char * p;
  int argc;
  char ** argv;
  int i;
  
  p = cmdline;
  argc = 1;
inword:
  switch (*p++) {
    case 0  : argc++; goto finished;
    case ' ': p[-1] = 0; argc++; goto skipblanks;
    default : goto inword;
  }
skipblanks:
  switch (*p++) {
    case 0  : goto finished;
    case ' ': goto skipblanks;
    default : goto inword;
  }
finished:
  argv = (char **) malloc((argc + 1) * sizeof(char *));
  argv[argc] = NULL;
  argv[0] = cmdname;
  for (i = 1, p = cmdline; i < argc; i++) {
    while(*p == ' ') p++;
    argv[i] = p;
    while(*p++ != 0) /*nothing*/;
  }
  return argv;
}

char ** check_args(argv)
	char ** argv;
{
  char * cmdline;

  if (argv[1] == NULL && (cmdline = getenv("*")) != NULL)
    return parse_args(argv[0], cmdline);
  else
    return argv;
}

/* EMX provides the built-in chdir2 which would make this obsolete: */
value sml_setdisk(volno)        /* ML */
     value volno;
{
  setdisk(Long_val(volno));
  return Atom(0);
}
