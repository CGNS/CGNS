@echo off
setlocal

set VIEWERDIR=%~dps0
if not exist %VIEWERDIR%calcwish.exe goto notfound
if not exist %VIEWERDIR%cgnscalc.tcl goto notfound

call %VIEWERDIR%..\cgconfig.bat

start /b %VIEWERDIR%calcwish.exe %VIEWERDIR%cgnscalc.tcl %1
goto done

:notfound
echo calcwish.exe and/or cgnscalc.tcl not found in %VIEWERDIR%
pause
:done
endlocal
