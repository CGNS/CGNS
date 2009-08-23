@echo off
setlocal

set drive=%~d0
set copts=
set debug=
set do64bit=0
set cgnsdir=
set tcldir=
set tkdir=
set tclinc=
set tcllib=
set tklib=
set plotopts=
set instdir=c:\CGNStools

:next
if "%1" == "" goto doit
if %1 == -help goto usage

rem ----- compiler options

if %1 == -ML (
  echo using -ML : single-threaded library
  set copts=%1
  shift
  goto next
)
if %1 == -MD (
  echo using -MD : multi-threaded library with RTL
  set copts=%1
  shift
  goto next
)
if %1 == -MT (
  echo using -MT : multi-threaded library
  set copts=%1
  shift
  goto next
)
if %1 == -debug (
  echo debug is enabled
  set debug=-Zi
  shift
  goto next
)
if %1 == -64 (
  echo build 64-bit version
  set do64bit=1
  shift
  goto next
)
if %1 == -nocut (
  echo building cgnsplot without cutting plane
  set plotopts=-DNO_CUTTING_PLANE %plotopts%
  shift
  goto next
)
if %1 == -nomesh (
  echo building cgnsplot without structured mesh boundaries
  set plotopts=-DNO_MESH_BOUNDARIES %plotopts%
  shift
  goto next
)

rem ----- cgns directory

if not %1 == -cgns goto tcl
shift
if "%1" == "" (
  echo ERROR:CGNS directory arg to -cgns not given
  goto usage
)
if not exist %1\cgns_io.h (
  echo ERROR:cgns_io.h not found in %1
  goto done
)
set cgnsdir=%~f1
echo using CGNS directory %cgnsdir%
shift
goto next

rem ----- tcl directory

:tcl
if not %1 == -tcl goto tk
shift
if "%1" == "" (
  echo ERROR:tcl directory arg to -tcl not given
  goto usage
)
if exist %1\generic\tcl.h goto got_tcldir
if exist %1\include\tcl.h goto got_tcldir
echo ERROR:can't find tcl.h in %1\include or %1\generic
goto done
:got_tcldir
set tcldir=%~f1
shift
goto next

rem ----- tk directory

:tk
if not %1 == -tk goto install
shift
if "%1" == "" (
  echo ERROR:tk directory arg to -tk not given
  goto usage
)
if exist %1\generic\tk.h goto got_tkdir
if exist %1\include\tk.h goto got_tkdir
echo ERROR:can't find tk.h in %1\include or %1\generic
goto done
:got_tkdir
set tkdir=%~f1
shift
goto next

rem ----- installation directory

:install
if not %1 == -install goto badarg
shift
if "%1" == "" (
  echo ERROR:installation directory arg to -install not given
  goto usage
)
set instdir=%~f1
echo installation to %instdir%
shift
goto next

rem ----- print usage

:badarg
echo ERROR:unknown argument %1
:usage
echo usage: configure [options]
echo options:
echo   -MT           : multi-threaded using libcmt.lib (default)
echo   -ML           : single-threaded using libc.lib
echo   -MD           : multi-threaded using mscvrt.lib and mscvrt.dll
echo   -debug        : add debugging to library
echo   -64           : build 64-bit version
echo   -nocut        : build cgnsplot without cutting plane
echo   -nomesh       : build cgnsplot without structured mesh boundaries
echo   -cgns cgnsdir : specify the CGNS directory
echo   -tcl tcldir   : specify the Tcl source directory
echo   -tk tkdir     : specify the Tk source directory
echo   -install dir  : specify installation directory (default c:\CGNStools)
goto done

:doit

rem ----- CGNS setup

if not "%cgnsdir%" == "" goto gettcl

echo checking for CGNS ...
for /D %%d in ( %drive%\*.* ) do (
  if exist %%d\cgns_io.h (
    echo %%d
    set cgnsdir=%%d
    goto gettcl
  )
  for /D %%e in ( %%d\*.* ) do (
    if exist %%e\cgns_io.h (
      echo %%e
      set cgnsdir=%%e
      goto gettcl
    )
    for /D %%f in ( %%e\*.* ) do (
      if exist %%f\cgns_io.h (
        echo %%f
        set cgnsdir=%%f
        goto gettcl
      )
      for /D %%g in ( %%f\*.* ) do (
        if exist %%g\cgns_io.h (
          echo %%g
          set cgnsdir=%%g
          goto gettcl
        )
      )
    )
  )
)
echo ERROR:couldn't find CGNS directory
goto done

rem ----- Tcl setup

:gettcl
if not "%tcldir%" == "" goto tclincludes
if not "%tkdir%" == "" (
  if exist %tkdir%\include\tcl.h (
    set tcldir=%tkdir%
    goto tclincludes
  )
)
echo checking for Tcl
for /D %%d in ( %drive%\*.* ) do (
  if exist %%d\generic\tcl.h (
    set tcldir=%%d
    goto tclincludes
  )
  if exist %%d\include\tcl.h (
    set tcldir=%%d
    goto tclincludes
  )
  for /D %%e in ( %%d\*.* ) do (
    if exist %%e\generic\tcl.h (
      set tcldir=%%e
      goto tclincludes
    )
    if exist %%e\include\tcl.h (
      set tcldir=%%e
      goto tclincludes
    )
    for /D %%f in ( %%e\*.* ) do (
      if exist %%f\generic\tcl.h (
        set tcldir=%%f
        goto tclincludes
      )
      if exist %%f\include\tcl.h (
        set tcldir=%%f
        goto tclincludes
      )
      for /D %%g in ( %%f\*.* ) do (
        if exist %%g\generic\tcl.h (
          set tcldir=%%g
          goto tclincludes
        )
        if exist %%g\include\tcl.h (
          set tcldir=%%g
          goto tclincludes
        )
      )
    )
  )
)
echo ERROR:couldn't find Tcl directory
goto done

:tclincludes
echo using Tcl directory %tcldir%
if exist %tcldir%\generic\tcl.h (
  set tclinc=-I%tcldir%\generic
) else (
  set tclinc=-I%tcldir%\include
)
for %%i in ( 81 82 83 84 85 86 ) do (
  if exist %tcldir%\lib\tcl%%i.lib (
    set tcllib=%tcldir%\lib\tcl%%i.lib
    goto gettk
  )
  if exist %tcldir%\win\Release\tcl%%i.lib (
    set tcllib=%tcldir%\win\Release\tcl%%i.lib
    goto gettk
  )
  if exist %tcldir%\win\Debug\tcl%%i.lib (
    set tcllib=%tcldir%\win\Debug\tcl%%i.lib
    goto gettk
  )
)
set tcllib=%tcldir%\lib\tcl.lib
echo couldn't find Tcl library - using %tcllib%

:gettk
if "%tkdir%" == "" (
  if exist %tcldir%\include\tk.h set tkdir=%tcldir%
)
if not "%tkdir%" == "" goto tkincludes
echo checking for Tk
for /D %%d in ( %drive%\*.* ) do (
  if exist %%d\generic\tk.h (
    set tkdir=%%d
    goto tkincludes
  )
  if exist %%d\include\tk.h (
    set tkdir=%%d
    goto tkincludes
  )
  for /D %%e in ( %%d\*.* ) do (
    if exist %%e\generic\tk.h (
      set tkdir=%%e
      goto tkincludes
    )
    if exist %%e\include\tk.h (
      set tkdir=%%e
      goto tkincludes
    )
    for /D %%f in ( %%e\*.* ) do (
      if exist %%f\generic\tk.h (
        set tkdir=%%f
        goto tkincludes
      )
      if exist %%f\include\tk.h (
        set tkdir=%%f
        goto tkincludes
      )
      for /D %%g in ( %%f\*.* ) do (
        if exist %%g\generic\tk.h (
          set tkdir=%%g
          goto tkincludes
        )
        if exist %%g\include\tk.h (
          set tkdir=%%g
          goto tkincludes
        )
      )
    )
  )
)
echo ERROR:couldn't find Tk directory
goto done

:tkincludes
echo using Tk directory %tkdir%
if not %tkdir% == %tcldir% (
  if exist %tkdir%\generic\tk.h (
    set tclinc=%tclinc% -I%tkdir%\generic
  ) else (
    set tclinc=%tclinc% -I%tkdir%\include
  )
)
if exist %tkdir%\win\tkWinInt.h (
  set tclinc=%tclinc% -I%tkdir%\win
  goto gotwinint
)
if exist %tkdir%\include\win\tkWinInt.h (
  set tclinc=%tclinc% -I%tkdir%\include\win
  goto gotwinint
)
echo couldn't find tkWinInt.h in %tkdir%\win or %tkdir%\include\win
goto done

:gotwinint
if exist %tkdir%\xlib\nul set tclinc=%tclinc% -I%tkdir%\xlib

for %%i in ( 81 82 83 84 85 86 ) do (
  if exist %tkdir%\lib\tk%%i.lib (
    set tklib=%tkdir%\lib\tk%%i.lib
    goto setopts
  )
  if exist %tkdir%\win\Release\tk%%i.lib (
    set tklib=%tkdir%\win\Release\tk%%i.lib
    goto setopts
  )
  if exist %tkdir%\win\Debug\tk%%i.lib (
    set tklib=%tkdir%\win\Debug\tk%%i.lib
    goto setopts
  )
)
set tklib=%tkdir%\lib\tk.lib
echo couldn't find Tk library - using %tklib%

:setopts
if "%copts%" == "" set copts=-MT
if %copts == -ML (
  set clibs=libc.lib
) else (
  if %copts% == -MD (
    set clibs=msvcrt.lib
  ) else (
    set clibs=libcmt.lib
  )
)
if %do64bit% == 1 (
  set clibs=%clibs% bufferoverflowu.lib
  set windir=WIN64
) else (
  set windir=WIN32
)
if not "%debug%" == "" (
  set copts=%copts%d %debug%
  set lopts=/debug
) else (
  set lopts=/release
)

rem ----- create Makefile

echo creating Makefile
echo # makefile for the CGNS tools under Windows> Makefile
echo.>> Makefile
echo MAKE = nmake -nologo -f Makefile.win>> Makefile
echo.>> Makefile
echo all : adfviewer cgnscalc cgnsplot tools utilities>> Makefile
echo install : install-adfviewer install-cgnscalc install-cgnsplot \>> Makefile
echo 	install-tools install-utilities>> Makefile
echo.>> Makefile
echo clean :>> Makefile
echo 	cd adfviewer ^&^& $(MAKE) clean>> Makefile
echo 	cd cgnscalc ^&^& $(MAKE) clean>> Makefile
echo 	cd cgnsplot ^&^& $(MAKE) clean>> Makefile
echo 	cd tools ^&^& $(MAKE) clean>> Makefile
echo 	cd utilities ^&^& $(MAKE) clean>> Makefile
echo 	cd calclib ^&^& $(MAKE) clean>> Makefile
echo 	cd tkogl ^&^& $(MAKE) clean>> Makefile
echo.>> Makefile
echo distclean : clean>> Makefile
echo 	-del make.win Makefile>> Makefile
echo.>> Makefile
echo adfviewer : prog-adfviewer>> Makefile
echo cgnscalc  : prog-cgnscalc>> Makefile
echo cgnsplot  : prog-cgnsplot>> Makefile
echo tools	  : prog-tools>> Makefile
echo utilities : prog-utilities>> Makefile
echo.>> Makefile
echo prog-adfviewer :>> Makefile
echo 	cd adfviewer ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo prog-cgnscalc : lib-calclib>> Makefile
echo 	cd cgnscalc ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo prog-cgnsplot : lib-tkogl>> Makefile
echo 	cd cgnsplot ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo prog-tools :>> Makefile
echo 	cd tools ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo prog-utilities : lib-calclib>> Makefile
echo 	cd utilities ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo lib-calclib :>> Makefile
echo 	cd calclib ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo lib-tkogl :>> Makefile
echo 	cd tkogl ^&^& $(MAKE)>> Makefile
echo.>> Makefile
echo install-adfviewer :>> Makefile
echo 	cd adfviewer ^&^& $(MAKE) install>> Makefile
echo.>> Makefile
echo install-cgnscalc : lib-calclib>> Makefile
echo 	cd cgnscalc ^&^& $(MAKE) install>> Makefile
echo.>> Makefile
echo install-cgnsplot : lib-tkogl>> Makefile
echo 	cd cgnsplot ^&^& $(MAKE) install>> Makefile
echo.>> Makefile
echo install-tools :>> Makefile
echo 	cd tools ^&^& $(MAKE) install>> Makefile
echo.>> Makefile
echo install-utilities : lib-calclib>> Makefile
echo 	cd utilities ^&^& $(MAKE) install>> Makefile

rem ----- create make.win

echo creating make.win
echo # makefile include for %windir%> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # CGNS setup>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo CGNSDIR  = %cgnsdir%>> make.win
echo !include $(CGNSDIR)\make.%windir%>> make.win
echo CGNSLIB  = $(CGNSDIR)\$(LIBCGNS)>> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # where and how to install>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo INSTALLDIR = %instdir%>> make.win
echo INSTALL    = copy /b>> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # path to the standard Tcl/Tk includes and libraries>> make.win
echo # the include path needs to include tcl.h, tk.h,>> make.win
echo # tkWinInt.h and the X11 include subdirectory>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo TCLINC = %tclinc%>> make.win
echo TCLLIB = %tcllib%>> make.win
echo TKLIB  = %tklib%>> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # TKOGLINCS give the include directories>> make.win
echo # TKOGLLIB is the library relative to cgnsplot directory>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo TKOGLINCS = $(TCLINC)>> make.win
echo TKOGLLIB  = ..\tkogl\tkogl.lib>> make.win
echo.>> make.win
echo #---------------------------------------------------------->> make.win
echo # compile options for cgnsplot>> make.win
echo #    -DNO_MESH_BOUNDARIES - no structured mesh boundaries>> make.win
echo #    -DNO_CUTTING_PLANE   - no cutting plane>> make.win
echo #---------------------------------------------------------->> make.win
echo.>> make.win
echo PLOTOPTS = %plotopts%>> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # set to trap math errors in calculator>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo MATHERR = -DUSE_MATHERR>> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # compiler and linker options>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo CC     = cl>>make.win
echo COPTS  = /nologo -DPROTOTYPE %copts%>>make.win
echo LINK   = link>> make.win
echo LFLAGS = /nologo %lopts%>> make.win
echo.>> make.win
echo #------------------------------------------------------->> make.win
echo # windows libraries>> make.win
echo #------------------------------------------------------->> make.win
echo.>> make.win
echo dlllibs = gdi32.lib comdlg32.lib>> make.win
echo.>> make.win
echo guilibs	= %clibs% oldnames.lib kernel32.lib advapi32.lib \>> make.win
echo 	user32.lib gdi32.lib comdlg32.lib winspool.lib>> make.win
echo.>> make.win
echo ogllibs = opengl32.lib glu32.lib>> make.win

:done
endlocal

