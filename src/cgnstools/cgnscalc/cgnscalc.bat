@echo off
setlocal

rem may need to set these if system can't find Tcl and TK scripts
rem set TCL_LIBRARY_PATH=c:/progra~1/tcl/lib/tcl8.3
rem set TK_LIBRARY_PATH=c:/progra~1/tcl/lib/tk8.3

set VIEWERDIR=%~dps0
if not exist %VIEWERDIR%calcwish.exe goto notfound
if not exist %VIEWERDIR%cgnscalc.tcl goto notfound

start /b %VIEWERDIR%calcwish.exe %VIEWERDIR%cgnscalc.tcl %1
goto done

:notfound
echo calcwish.exe and/or cgnscalc.tcl not found in %VIEWERDIR%
pause
:done
endlocal
