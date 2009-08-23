@echo off
setlocal

rem may need to set these if system can't find Tcl and TK scripts
rem set TCL_LIBRARY_PATH=c:/progra~1/tcl/lib/tcl8.3
rem set TK_LIBRARY_PATH=c:/progra~1/tcl/lib/tk8.3

rem the standard wish command will work for this
rem set WISH=c:\progra~1\tcl\bin\wish83.exe

set VIEWERDIR=%~dps0

if "%WISH%" == "" set WISH=%VIEWERDIR%calcwish.exe
if not exist %WISH% goto notfound
if not exist %VIEWERDIR%unitconv.tcl goto notfound

start /b %WISH% %VIEWERDIR%unitconv.tcl
goto done

:notfound
echo %WISH% and/or %VIEWERDIR%unitconv.tcl not found
pause
:done
endlocal
