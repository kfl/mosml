REM On Win32, switch Makefiles from Win32 Makefiles to Unix Makefiles, 
REM if not already done.

if exist Makefile.w32 (
  echo "Already in Unix mode\n"
  exit /B 1
)

echo "Renaming all Makefile to Makefile.w32"

FOR /F  %%f IN ('dir /S /B Makefile') DO move /Y %%f %%f.w32

echo "Renaming all Makefile.unx to Makefile"

FOR /F  %%f IN ('dir /S /B Makefile.unx') DO move /Y %%f %%~dpfMakefile

