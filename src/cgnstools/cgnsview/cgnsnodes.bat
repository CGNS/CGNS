@echo off
setlocal

rem the standard wish command will work for this
rem set WISH=c:\progra~1\tcl\bin\wish83.exe

set VIEWERDIR=%~dps0

if "%WISH%" == "" set WISH=%VIEWERDIR%cgiowish.exe
if not exist %WISH% goto notfound
if not exist %VIEWERDIR%cgnsnodes.tcl goto notfound

call %VIEWERDIR%..\cgconfig.bat

start /b %WISH% %VIEWERDIR%cgnsnodes.tcl %1
goto done

:notfound
echo %WISH% and/or %VIEWERDIR%cgnsnodes.tcl not found
pause
:done
endlocal
