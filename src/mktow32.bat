REM On Win32, switch Makefiles from Unix Makefiles to Win32 Makefiles, 
REM if not already done.
if exist Makefile.unx (
  echo "Already in Win32 mode\n"
  exit /B 1
)

echo "Renaming all Makefile to Makefile.unx"


FOR /F  %%f IN ('dir /S /B Makefile) DO move /Y %%f %%f.unx

echo "Renaming all Makefile.w32 to Makefile"

FOR /F  %%f IN ('dir /S /B Makefile.w32') DO move /Y %%f %%~pfMakefile

