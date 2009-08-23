@echo off
setlocal

rem may need to set these if system can't find Tcl and TK scripts
rem set TCL_LIBRARY=c:/lib/tcl8.4.13/library
rem set TK_LIBRARY=c:/lib/tk8.4.13/library

set VIEWERDIR=%~dps0
if not exist %VIEWERDIR%plotwish.exe goto notfound
if not exist %VIEWERDIR%cgnsplot.tcl goto notfound

start /b %VIEWERDIR%plotwish.exe %VIEWERDIR%cgnsplot.tcl %1
goto done

:notfound
echo plotwish.exe and/or cgnsplot.tcl not found in %VIEWERDIR%
pause
:done
endlocal
