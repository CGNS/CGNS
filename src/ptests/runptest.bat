@echo off
set result=passed
%1 %2 %3 %4 > nul 2>&1
if not %ERRORLEVEL% == 0 set result=*** FAILED ***
echo %4 ... %result%

