@echo off
set result=passed
%1 > nul 2>&1
if not %ERRORLEVEL% == 0 set result=*** FAILED ***
echo %1 ... %result%

