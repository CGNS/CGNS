@echo off
setlocal

set VIEWERDIR=%~dps0
if not exist %VIEWERDIR%cgiowish.exe goto notfound
if not exist %VIEWERDIR%cgnsview.tcl goto notfound

call %VIEWERDIR%..\cgconfig.bat

start /b %VIEWERDIR%cgiowish.exe %VIEWERDIR%cgnsview.tcl %1
goto done

:notfound
echo cgiowish.exe and/or cgnsview.tcl not found in %VIEWERDIR%
pause
:done
endlocal
