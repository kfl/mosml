REM On Win32, switch Makefiles from Unix Makefiles to Win32 Makefiles, 
REM if not already done.
if exist Makefile.unx (
  echo "Already in Win32 mode\n"
  exit /B 1
)

echo "Renaming all Makefile to Makefile.unx"

FOR /R . %%f IN (Makefile) DO rename %%f %%f.unx

echo "Renaming all Makefile.w32 to Makefile"

FOR /R . %%f IN (Makefile.w32) DO rename %%f Makefile

