REM On Win32, switch Makefiles from Win32 Makefiles to Unix Makefiles, 
REM if not already done.

if exist Makefile.w32 (
  echo "Already in Unix mode\n"
  exit /B 1
)

echo "Renaming all Makefile to Makefile.w32"

FOR /R . %%f IN (Makefile) DO rename %%f %%f.w32

echo "Renaming all Makefile.unx to Makefile"

FOR /R . %%f IN (Makefile.unx) DO rename %%f Makefile

