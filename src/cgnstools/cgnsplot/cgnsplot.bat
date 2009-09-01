@echo off
setlocal

set VIEWERDIR=%~dps0
if not exist %VIEWERDIR%plotwish.exe goto notfound
if not exist %VIEWERDIR%cgnsplot.tcl goto notfound

call %VIEWERDIR%..\cgconfig.bat

start /b %VIEWERDIR%plotwish.exe %VIEWERDIR%cgnsplot.tcl %1
goto done

:notfound
echo plotwish.exe and/or cgnsplot.tcl not found in %VIEWERDIR%
pause
:done
endlocal
