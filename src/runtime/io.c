/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "sys.h"
#include "globals.h"
#ifdef HAS_UI
#include "ui.h"
#endif

/* Common functions. */

/* Improvement suggested by Doug Currie: 
   memoize std_in, std_out and std_err.
   Used also in function flush_stdouterr.

   As usual, std_channel[0] = stdin, 
             std_channel[1] = stdout, 
	     std_channel[2] = stderr. 
*/

static struct channel *std_channel[3] = {NULL, NULL, NULL};

struct channel * open_descr(int fd)
{
  struct channel * channel;

  if( (unsigned)fd < 3 && std_channel[fd] != NULL )
    return std_channel[fd];
  channel = (struct channel *) stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  channel->offset = 0;
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  if( (unsigned)fd < 3 )
    std_channel[fd] = channel;
  return channel;
}               

value open_descriptor(value fd)       /* ML */
{
  return (value) open_descr(Int_val(fd));
}

value channel_descriptor(struct channel * channel)   /* ML */
{
  return Val_long(channel->fd);
}

value channel_size(struct channel * channel)      /* ML */
{
  long end;

  end = lseek(channel->fd, 0, 2);
  if (end == -1) sys_error(NULL);
  if (lseek(channel->fd, channel->offset, 0) != channel->offset) 
    sys_error(NULL);
  return Val_long(end);
}

/* Output */

static void really_write(int fd, char * p, int n)
{
  int retcode;
  while (n > 0) {
#ifdef HAS_UI
    retcode = ui_write(fd, p, n);
#else
#ifdef EINTR
    do { retcode = write(fd, p, n); } while (retcode == -1 && errno == EINTR);
#else
    retcode = write(fd, p, n);
#endif
#endif
    if (retcode == -1) sys_error(NULL);
    p += retcode;
    n -= retcode;
  }
}   

value flush(struct channel * channel)            /* ML */
{
  int n;
  n = channel->max - channel->buff;
  if (n > 0) {
    really_write(channel->fd, channel->buff, n);
    channel->offset += n;
    channel->curr = channel->buff;
    channel->max  = channel->buff;
  }
  return Atom(0);
}

void flush_stdouterr(void)
{
  if (std_channel[1]) flush(std_channel[1]);
  if (std_channel[2]) flush(std_channel[2]);
}

value output_char(struct channel * channel, value ch)  /* ML */
{
  putch(channel, Long_val(ch));
  return Atom(0);
}

void putword(struct channel * channel, uint32 w)
{
  putch(channel, w >> 24);
  putch(channel, w >> 16);
  putch(channel, w >> 8);
  putch(channel, w);
}

value output_int(struct channel * channel, value w)    /* ML */
{
  putword(channel, Long_val(w));
  return Atom(0);
}

void putblock(struct channel * channel, char * p, unsigned n)
{
  unsigned m;

  m = channel->end - channel->curr;
  if (channel->curr == channel->buff && n >= m) {
    really_write(channel->fd, p, n);
    channel->offset += n;
  } else if (n <= m) {
    bcopy(p, channel->curr, n);
    channel->curr += n;
    if (channel->curr > channel->max) channel->max = channel->curr;
  } else {
    bcopy(p, channel->curr, m);
    p += m;
    n -= m;
    m = channel->end - channel->buff;
    really_write(channel->fd, channel->buff, m);
    channel->offset += m;
    if (n <= m) {
      bcopy(p, channel->buff, n);
      channel->curr = channel->max = channel->buff + n;
    } else {
      really_write(channel->fd, p, n);
      channel->offset += n;
      channel->curr = channel->max = channel->buff;
    }
  }
}

value output(value channel, value buff, value start, value length) /* ML */
{
  putblock((struct channel *) channel,
           &Byte(buff, Long_val(start)),
           (unsigned) Long_val(length));
  return Atom(0);
}

value seek_out(struct channel * channel, value pos)    /* ML */
{
  long dest;

  dest = Long_val(pos);
  if (dest >= channel->offset &&
      dest <= channel->offset + channel->max - channel->buff) {
    channel->curr = channel->buff + dest - channel->offset;
  } else {
    flush(channel);
    if (lseek(channel->fd, dest, 0) != dest) sys_error(NULL);
    channel->offset = dest;
  }
  return Atom(0);
}

value pos_out(struct channel * channel)          /* ML */
{
  return Val_long(channel->offset + channel->curr - channel->buff);
}

value close_out(struct channel * channel)     /* ML */
{
  if ((unsigned)(channel->fd) >= 3) 
    {
      flush(channel);
      close(channel->fd);
      stat_free((char *) channel);
    }
  return Atom(0);
}

/* Input */

#ifdef WIN32
void (__cdecl *old_ctrl_c_handler)(int);
int volatile ctrl_c_during_read;

void handle_ctrl_c(int sig) 
{
    signal(SIGINT, handle_ctrl_c);
    /* Beep(2000, 300); */
    ctrl_c_during_read = 1;
}
#endif /* WIN32 */

void nonblocking_mode(int fd, int nonblocking) {
#ifdef WIN32
  int retcode = -1;
#else
  int retcode = fcntl(fd, F_GETFL);
  if (retcode != -1) {
    if (nonblocking) 
      retcode = fcntl(fd, F_SETFL, retcode | O_NONBLOCK);
    else
      retcode = fcntl(fd, F_SETFL, retcode & (~O_NONBLOCK));
  }
#endif
  if (retcode == -1)
    failwith("nonblocking_mode");
}

/* Risk: If an interrupt occurs after non-blocking mode has been set,
   and before it has been reset (unlikely, just because the read is
   non-blocking), then the next read on the same fd will be
   non-blocking, whether or not that was the intention.  This may
   cause SysError "Resource temporarily unavailable" to be raised,
   when one would have expected the read to block.  Unlikely to be a
   problem in practice, since reads on an fd are likely to be all
   blocking or all non-blocking.  sestoft 2000-03-15 */

static int really_read(int fd, char * p, unsigned n, int nonblocking)
{
  int retcode;

  if (nonblocking)
    nonblocking_mode(fd, nonblocking);	   /* set non-blocking */

  enter_blocking_section();
#ifdef HAS_UI
  retcode = ui_read(fd, p, n);
#else
#ifdef MSDOS
  retcode = msdos_read(fd, p, n);
#else
#if defined(EINTR) && !defined(WIN32)
  do { retcode = read(fd, p, n); } while (retcode == -1 && errno == EINTR);
#else
#ifdef WIN32
  /* Under Win32 the SIGINT handler runs as a separate thread. */
  /* Sleep (0L) causes the current thread to relinquish  */
  /* the remainder of its time slice to any other thread of equal priority */
  /* that is ready to run. */
  if( fd == 0 )
  {
    Sleep(0L);
    ctrl_c_during_read = 0;
    old_ctrl_c_handler = signal(SIGINT, handle_ctrl_c);
    retcode = read(fd, p, n);

    /* It seems that under Windows NT CONTROL-C stops 'read' */
    /* and retcode == 0 is returned. */
    /* However, under Windows 95, CONTROL-C cancels only */
    /* the characters that have already been input. Hence, */
    /* retcode == the number of characters, following CONTROL-C */
    /* (counting also the last '\n'). */
    /* There are so many interesting things in the World! */

    if( !ctrl_c_during_read && retcode == 0 )
    {
      /* There seems to be no way to tell CONTROL-Z from CNTRL-C */
      /* under Windows NT! */
      /* Let's hope that a 1 second delay will be enough */
      /* for the CONTROL-C handler's thread to terminate */
      Sleep(1000L);
    }
    signal(SIGINT, old_ctrl_c_handler);
    if( ctrl_c_during_read )
    {
      signal_handler = raise_break_exn;
      signal_number = 0;
      execute_signal();
    }
  }
  else
  {
    retcode = read(fd, p, n);
  }
#else
  retcode = read(fd, p, n);
#endif
#endif
#endif
#endif
  leave_blocking_section();

  if (nonblocking) {
    nonblocking_mode(fd, 0);			/* unset non-blocking */
    if (retcode == -1 && errno != EAGAIN)
      sys_error(NULL);
  } else if (retcode == -1)
    sys_error(NULL);
  return retcode;
}

unsigned char refill(struct channel * channel)
{
  int n;

  n = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE, 
                  /* nonblocking = */ 0);
  if (n == 0) raiseprimitive0(SYS__EXN_SIZE);
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

value input_char(struct channel * channel)       /* ML */
{
  unsigned char c;
  c = getch(channel);
  return Val_long(c);
}

uint32 getword(struct channel * channel)
{
  int i;
  uint32 res;

  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + getch(channel);
  }
  return res;
}

value input_int(struct channel * channel)        /* ML */
{
  long i;
  i = getword(channel);
#ifdef SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  return Val_long(i);
}

int getblock(struct channel * channel, char * p, unsigned n,
             int nonblocking)
{
  unsigned m, l;

  m = channel->max - channel->curr;
  if (n <= m) {
    bcopy(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else if (m > 0) {
    bcopy(channel->curr, p, m);
    channel->curr += m;
    return m;
  } else if (n < IO_BUFFER_SIZE) {
    l = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE, nonblocking);
    if (l == -1) /* Non-blocking read returned no data */ 
      return -1;
    else {
      channel->offset += l;
      channel->max = channel->buff + l;
      if (n > l) n = l;
      bcopy(channel->buff, p, n);
      channel->curr = channel->buff + n;
      return n;
    }
  } else {
    channel->curr = channel->buff;
    channel->max = channel->buff;
    l = really_read(channel->fd, p, n, nonblocking);
    if (l == -1)	/* Non-blocking read returned no data */ 
      return -1;
    else {
      channel->offset += l;
      return l;
    }
  }
}

int really_getblock(struct channel * chan, char * p, unsigned long n)
{
  unsigned r;
  while (n > 0) {
    r = (unsigned)getblock(chan, p, (unsigned) n,  /* nonblocking = */ 0);
    if (r == 0) return 0;
    p += r;
    n -= r;
  }
  return 1;
}

value input(value channel, value buff, value start, value length) /* ML */
{
  return Val_long(getblock((struct channel *) channel,
                           &Byte(buff, Long_val(start)),
                           (unsigned) Long_val(length),
                           /* nonblocking = */ 0));
}

value input_nonblocking(value channel, value buff, value start, value length) /* ML */
{ int n = getblock((struct channel *) channel,
                   &Byte(buff, Long_val(start)),
                   (unsigned) Long_val(length),
	           /* nonblocking = */ 1);
  if (n == -1)		/* Non-blocking read returned no data */ 
    return NONE;
  else {
    value res = alloc(1, SOMEtag);
    Field(res, 0) = Val_long(n);
    return res;
  }
}

value seek_in(struct channel * channel, value pos)     /* ML */
{
  long dest;

  dest = Long_val(pos);
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    if (lseek(channel->fd, dest, 0) != dest) sys_error(NULL);
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
  return Atom(0);
}

value pos_in(struct channel * channel)           /* ML */
{
  return Val_long(channel->offset - (channel->max - channel->curr));
}

value close_in(struct channel * channel)     /* ML */
{
  close(channel->fd);
  stat_free((char *) channel);
  return Atom(0);
}

value input_scan_line(struct channel * channel)       /* ML */
{
  char * p;
  int n;

  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        bcopy(channel->curr, channel->buff, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return Val_long(-(channel->max - channel->curr));
      }
      /* Fill the buffer as much as possible */
      n = really_read(channel->fd, channel->max, channel->end - channel->max,
		      /* nonblocking = */ 0);
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered 
           a newline. */
        return Val_long(-(channel->max - channel->curr));
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return Val_long(p - channel->curr);
}
