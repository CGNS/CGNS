@echo off
setlocal

rem may need to set these if system can't find Tcl and TK scripts
rem set TCL_LIBRARY=c:/lib/tcl8.4.13/library
rem set TK_LIBRARY=c:/lib/tk8.4.13/library

set VIEWERDIR=%~dps0
if not exist %VIEWERDIR%cgiowish.exe goto notfound
if not exist %VIEWERDIR%cgnsview.tcl goto notfound

start /b %VIEWERDIR%cgiowish.exe %VIEWERDIR%cgnsview.tcl %1
goto done

:notfound
echo cgiowish.exe and/or cgnsview.tcl not found in %VIEWERDIR%
pause
:done
endlocal
