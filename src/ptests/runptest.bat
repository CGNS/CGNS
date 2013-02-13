@echo off
setlocal
if "%2" == "" (
  echo usage: runptest mpiexecname exename [numprocs]
  goto done
)
if "%3" == "" (
  set np=2
) else (
  set np=%3
)
set PATH=..\lib;%PATH%
%1 -np %np% %2 > nul 2>&1
if %ERRORLEVEL% == 0 (
  echo %2 ... passed
) else (
  echo %2 ... *** FAILED ***
)
:done
endlocal

