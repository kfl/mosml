/* File mosml/src/dynlibs/munix/munix.c
   sestoft@dina.kvl.dk 1999-11-07 version 0.1
 */

/* General includes */

#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

/* Moscow ML includes */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <str.h>
#include <signals.h>

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

void failure() {
  switch (errno) {
  case EFAULT : 
    failwith("EFAULT"); break;
  case EMFILE :
    failwith("EMFILE"); break;
  case ENFILE : 
    failwith("ENFILE"); break;
  case EAGAIN : 
    failwith("EAGAIN"); break;
  case ENOMEM : 
    failwith("ENOMEM"); break;
  case EACCES:
    failwith("EACCES"); break;
  case ENOEXEC:
    failwith("ENOEXEC"); break;
  case EPERM:
    failwith("EPERM"); break;
  case E2BIG:
    failwith("E2BIG"); break;
  case ENAMETOOLONG:
    failwith("ENAMETOOLONG"); break;
  case ENOENT:
    failwith("ENOENT"); break;
  case ENOTDIR:
    failwith("ENOTDIR"); break;
  case ELOOP:
    failwith("ELOOP"); break;
  case EIO:
    failwith("EIO"); break;
  case EINVAL:
    failwith("EINVAL"); break;
  case EISDIR:
    failwith("EISDIR"); break;
  case ELIBBAD:
    failwith("ELIBBAD"); break;
  case ECHILD:
    failwith("ECHILD"); break;
  case EINTR:
    failwith("EINTR"); break;
  case ESRCH:
    failwith("ESRCH"); break;
  default:
    failwith("EUNSPECIFIED"); break;
  }
}

char** mkcharptrvec(value strvec) {
  int i;
  int argc = Wosize_val(strvec);
  char **argv = (char**) malloc((argc + 1) * sizeof(char*));
  if (argv == (char**)NULL)
    failwith("mkcharptrvec: malloc failed");
  for (i=0; i<argc; i++)
    argv[i] = String_val(Field(strvec, i));
  argv[argc] = (char*)NULL;
  return argv;
}

/* ML type: string -> string vector -> string vector option 
            -> int * int * int */
EXTERNML value unix_execute(value cmd, value args, value envopt) {
  int p2c[2];			      /* Pipe from parent to child */
  int c2p[2];			      /* Pipe from child to parent */
  int pid;
  char **argv = mkcharptrvec(args);

  if (pipe(p2c) < 0 || pipe(c2p) < 0)
    failure();
  pid = fork();
  if (pid < 0) 
    // In the parent process; fork failed
    failure();
  else if (pid > 0) {
    // In the parent process; fork succeeded
    value res = alloc_tuple(3);
    // printf("<%d>\n", pid); fflush();
    free(argv);
    close(c2p[1]);		      /* Close child's ends of pipes    */
    close(p2c[0]);
    Field(res, 0) = Val_long(pid); 
    Field(res, 1) = Val_long(c2p[0]); /* Parent reads from the c2p pipe */
    Field(res, 2) = Val_long(p2c[1]); /* Parent writes to the p2c pipe  */
    return res;
  } else { 
    // In the child process
    close(p2c[1]);		      /* Close parent's ends of pipes   */
    close(c2p[0]);
    dup2(p2c[0], 0 /* STD_IN  */);    /* Child stdin from the p2c pipe  */
    dup2(c2p[1], 1 /* STD_OUT */);    /* Child stdout to the c2p pipe   */
    if (envopt == NONE) 
      execv(String_val(cmd), argv);
    else {
      char **envv = mkcharptrvec(Field(envopt, 0));
      execve(String_val(cmd), argv, envv);
    }      
    printf("Could not exec %s\n", String_val(cmd));
    exit(1);
    // Never gets here
    return Val_unit;
  }
}

/* ML type: int -> int */
EXTERNML value unix_waitpid(value pid) {
  int status;
  if (waitpid(Long_val(pid), &status, /* options = */ 0) < 0)
    failure();
  if (WIFEXITED(status)) 
    return Val_long(WEXITSTATUS(status));
  else
    return Val_long(-1);
}

/* ML type: int -> unit */
EXTERNML value unix_kill(value pid, value sig) {
  if (kill(Long_val(pid), Long_val(sig)) < 0)
    failure();
  return Val_unit;
}

