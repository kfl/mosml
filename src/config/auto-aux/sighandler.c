#include <signal.h>

int main()
{
  SIGRETURN (*old)();
  old = signal(SIGQUIT, SIG_DFL);
  return 0;
}
