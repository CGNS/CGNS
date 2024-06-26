REM SPDX-FileCopyrightText: 2020 Intel Corporation
REM
REM SPDX-License-Identifier: MIT

set URL=%1
set FILENAME=%2

curl.exe --output webimage.exe --url %URL% --retry 5 --retry-delay 5
start /b /wait webimage.exe -s -x -f webimage_extracted --log extract.log
del webimage.exe
webimage_extracted\bootstrapper.exe --list-components > %FILENAME%
set installer_exit_code=%ERRORLEVEL%
type %FILENAME%
rmdir /s /q webimage_extracted
exit /b %installer_exit_code%
