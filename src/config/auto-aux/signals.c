/* To determine the semantics of signal handlers
   (System V: signal is reset to default behavior on entrance to the handler
    BSD: signal handler remains active). */

#include <stdio.h>
#include <signal.h>

int counter;

void sig_handler(dummy)
     int dummy;
{
  counter++;
}

int main(argc, argv)
     int argc;
     char ** argv;
{
  signal(SIGINT, sig_handler);
  counter = 0;
  kill(getpid(), SIGINT);
  kill(getpid(), SIGINT);
  return (counter == 2 ? 0 : 1);
}
