#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys\stat.h>
#include "driver.h"

// -----

BOOL WINAPI MyHandlerRoutine( DWORD dwCtrlType )
{
  if( dwCtrlType == CTRL_C_EVENT )
    return TRUE;
  else
    return FALSE;
}

int ExecCmd(char *cmd_args)
{
  BOOL fOk;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  DWORD dwExitCode;

//#ifdef DEBUG
  fprintf(stderr, "\t%s\n", cmd_args);
//#endif

  /* Initialize the STARTUPINFO structure: */

  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);

  /* Ignore CTRL-C. */
  fOk = SetConsoleCtrlHandler(MyHandlerRoutine, TRUE);
  Sleep(1);

  /* Start the shelled application: */
  fOk = CreateProcess(NULL, cmd_args, NULL, NULL, TRUE,
              NORMAL_PRIORITY_CLASS, NULL, NULL, &si, &pi);
  if( !fOk )
  {
    DWORD dwLastError;
    fprintf(stderr, "Unable to execute %s\n", cmd_args);
    dwLastError = GetLastError();
    switch( dwLastError )
    {
    default:
      fprintf(stderr, "Last Error = %x\n", dwLastError);
    }
    exit(2);
  }

  /* Wait for the shelled application to finish: */
  if( WaitForSingleObject(pi.hProcess, INFINITE) == WAIT_FAILED )
  {
    fprintf(stderr, "WaitForSingleObject failed\n");
    exit(2);
  }
  if( !GetExitCodeProcess(pi.hProcess, &dwExitCode) )
  {
    fprintf(stderr, "GetExitCodeProcess failed\n");
    exit(2);
  }
  CloseHandle(pi.hProcess);

  /* Restore the default CTRL-C handler. */
  fOk = SetConsoleCtrlHandler(NULL, FALSE);

  return dwExitCode;
}

// -----


char * camlrunm = "camlrunm.exe";
char * stdlib;
int linkalso = 1;
char * includes = "";
char * compopt = "";
char * linkopt = "";
char * linkfiles = "";
char * linkout = "mosmlout.exe";

main(argc, argv)
     int argc;
     char ** argv;
{
  int i;
  char * a;
  extern char * getenv();
  char * cmd;
  int status;

  stdlib = getenv("MOSMLLIB");
  if (stdlib == NULL) {
    fprintf(stderr, "Variable MOSMLLIB is undefined.\n");
    exit(2);
  }

  for (i = 1; i < argc; i++) {
    a = argv[i];
    if (suffix(a, ".sml")) {
      format(&cmd, "%s \"%s\\mosmlcmp\" -stdlib \"%s\" %s %s \"%s\"",
             camlrunm, stdlib, stdlib, includes, compopt, a);
      /* fprintf(stderr, "%s\n", cmd); */
      status = ExecCmd(cmd);
      if( status ) exit(status);
      free(cmd);
      reformat(&linkfiles, "%s \"%s\"", linkfiles, a);
    } else
    if (suffix(a, ".sig")) {
      format(&cmd, "%s \"%s\\mosmlcmp\" -stdlib \"%s\" %s %s \"%s\"",
             camlrunm, stdlib, stdlib, includes, compopt, a);
      /* fprintf(stderr, "%s\n", cmd); */
      status = ExecCmd(cmd);
      if( status ) exit(status);
      free(cmd);
    } else
    if (suffix(a, ".uo")) {
      reformat(&linkfiles, "%s \"%s\"", linkfiles, a);
    } else
    if (eq(a, "-c")) {
      linkalso = 0;
    } else
    if (eq(a, "-m") || eq(a, "-msgstyle")) {
      reformat(&compopt, "%s -msgstyle %s", compopt, argv[++i]);
    } else
    if (eq(a, "-I") || eq(a, "-include")) {
      reformat(&includes, "%s -I \"%s\"", includes, argv[++i]);
    } else
    if (eq(a, "-P") || eq(a, "-perv")) {
      reformat(&compopt, "%s -P %s", compopt, argv[++i]);
      reformat(&linkopt, "%s -P %s", linkopt, argv[i]);
    } else
    if (eq(a, "-i")) {
      reformat(&compopt, "%s -i", compopt);
    } else
    if (eq(a, "-valuepoly")) {
      reformat(&compopt, "%s -valuepoly", compopt);
    } else
    if (eq(a, "-imptypes")) {
      reformat(&compopt, "%s -imptypes", compopt);
    } else
    if (eq(a, "-q") || eq(a, "-quotation")) {
      reformat(&compopt, "%s -q", compopt);
    } else
    if (eq(a, "-g") || eq(a, "-debug")) {
      reformat(&linkopt, "%s -g", linkopt);
    } else
    if (eq(a, "-noheader")) {
      reformat(&linkopt, "%s -noheader", linkopt);
    } else
    if (eq(a, "-o") || eq(a, "-exec")) {
      linkout = argv[++i];
    } else
    if (eq(a, "-stdlib")) {
      stdlib = argv[++i];
    } else
    if (eq(a, "-v") || eq(a, "-version")) {
      printf("The Moscow ML system for the 80386 PC, version 1.41\n");
      printf("  (standard library from %s)\n", stdlib);
      format(&cmd, "%s -V", camlrunm);
      ExecCmd(cmd);
      format(&cmd, "%s \"%s\\mosmlcmp\" -version", camlrunm, stdlib);
      ExecCmd(cmd);
      free(cmd);
      format(&cmd, "%s \"%s\\mosmllnk\" -version", camlrunm, stdlib);
      ExecCmd(cmd);
      free(cmd);
    } else
    if (eq(a, "-files")) {
      reformat(&linkfiles, "%s -files \"%s\"", linkfiles, argv[++i]);
    } else
    if (prefix(a, "-")) {
      fprintf(stderr, "Unknown option \"%s\", ignored\n", a);
    } else {
      fprintf(stderr, "I don't know what to do with file \"%s\", ignored\n",
              a);
    }
  }

  if (linkalso && linkfiles[0] != 0) {
    format(&cmd,
           "%s \"%s\\mosmllnk\" -stdlib \"%s\" %s %s -exec \"%s\" %s",
           camlrunm, stdlib, stdlib, includes,
           linkopt, linkout, linkfiles);
    /* fprintf(stderr, "%s\n", cmd); */
    status = ExecCmd(cmd);
    return status;
  }
  return 0;
}
