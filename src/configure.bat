@echo off
setlocal

set args=-ML -MD -MT -debug -lfs -64 -dll -gmake -install -f2c
set args=%args% -ifort -absoft -hdf5 -zlib -szip -xml
set copts=
set debug=
set cfgflags=
set dllopts=
set build=-DUSE_CGIO
set do64bit=0
set target=lib
set make=nmake
set instdir=%~d0
set f2c=
set f77=f77
set hdf5inc=
set hdf5lib=
set zliblib=
set sziplib=
set hdf5dll=#HDF5DLL
set expatinc=
set expatlib=

:next
if "%1" == "" goto doit
if %1 == -help goto usage
for %%a in ( %args% ) do (
  if %1 == %%a goto copts
)
goto badarg

rem ----- compiler options

:copts
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

rem ----- enable large file support

if %1 == -lfs (
  echo large file support is enabled
  set cfgflags=-DHAVE_LSEEK64
  shift
  goto next
)

rem ----- build for 64 bit

if %1 == -64 (
  echo building 64-bit version
  set do64bit=1
  shift
  goto next
)

rem ----- build DLL

if %1 == -dll (
  echo building DLL instead of static library
  set target=dll
  set dllopts=-DBUILD_DLL
  shift
  goto next
)

rem ----- use gmake

if %1 == -gmake (
  echo using gmake compatible makefile
  set make=gmake
  shift
  goto next
)

rem ----- installation directory

if not %1 == -install goto f2c
shift
if "%1" == "" (
  echo ERROR:installation directory not given
  goto usage
)
for %%a in ( %args% ) do (
  if %1 == %%a (
    echo ERROR:installation directory not given
    goto usage
  )
)
echo installation to %1\lib and %1\include
set instdir=%1
shift
goto next

rem ----- Fortran to C interface

:f2c
if not %1 == -f2c goto ifort
set f2c=-DLOWERCASE_
shift
if "%1" == "" (
  echo using LOWERCASE_ as Fortran to C interface
  goto doit
)
for %%a in ( %args% ) do (
  if %1 == %%a (
    echo using LOWERCASE_ as Fortran to C interface
    goto next
  )
)
if %1 == none (
  echo Fortran interface is disabled
  set f2c=none
  shift
  goto next
)
if %1 == LOWERCASE goto setf2c
if %1 == LOWERCASE_ goto setf2c
if %1 == LOWERCASE__ goto setf2c
if %1 == UPPERCASE goto setf2c
if %1 == UPPERCASE_ goto setf2c
if %1 == UPPERCASE__ goto setf2c
echo ERROR:-f2c argument %1 is invalid
goto usage

:setf2c
echo using %1 as Fortran to C interface
set f2c=-D%1
shift
goto next

rem ----- ifort Fortran compiler

:ifort
if not %1 == -ifort goto absoft
echo using ifort Fortran compiler with UPPERCASE
set f77=ifort
set f2c=-DUPPERCASE
shift
goto next

rem ----- absoft Fortran compiler

:absoft
if not %1 == -absoft goto hdf5
echo using absoft Fortran compiler with LOWERCASE
set f2c=-DLOWERCASE
shift
goto next

rem ----- HDF5 setup

:hdf5
if not %1 == -hdf5 goto zlib
shift

if "%1" == "" goto findhdf5
for %%a in ( %args% ) do if %1 == %%a goto findhdf5

if not exist %1\nul (
  echo ERROR:HDF5 directory "%1" does not exist or is not a directory
  goto done
)
set hdf5dir=%1
shift
goto gethdf5

:findhdf5
echo checking for HDF5 ...
for /D %%d in ( %~d0\*.* ) do (
  if exist %%d\include\hdf5.h (
    echo %%d
    set hdf5dir=%%d
    goto gethdf5
  )
  if exist %%d\src\hdf5.h (
    echo %%d
    set hdf5dir=%%d
    goto gethdf5
  )
  for /D %%e in ( %%d\*.* ) do (
    if exist %%e\include\hdf5.h (
      echo %%e
      set hdf5dir=%%e
      goto gethdf5
    )
    if exist %%e\src\hdf5.h (
      echo %%e
      set hdf5dir=%%e
      goto gethdf5
    )
    for /D %%f in ( %%e\*.* ) do (
      if exist %%f\include\hdf5.h (
        echo %%f
        set hdf5dir=%%f
        goto gethdf5
      )
      if exist %%f\src\hdf5.h (
        echo %%f
        set hdf5dir=%%f
        goto gethdf5
      )
      for /D %%g in ( %%f\*.* ) do (
        if exist %%g\include\hdf5.h (
          echo %%g
          set hdf5dir=%%g
          goto gethdf5
        )
        if exist %%g\src\hdf5.h (
          echo %%g
          set hdf5dir=%%g
          goto gethdf5
        )
      )
    )
  )
)
echo ERROR:couldn't find hdf5 directory
goto done

:gethdf5
echo checking for hdf5 headers in %hdf5dir% ...
if exist %hdf5dir%\include\hdf5.h (
  echo %hdf5dir%\include
  set hdf5inc=%hdf5dir%\include
  echo checking for hdf5 library in %hdf5dir%\lib ...
  for %%l in ( hdf5dll hdf5 hdf5dlld hdf5d ) do (
    for %%d in ( dll lib ) do (
      if exist %hdf5dir%\%%d\%%l.lib (
        echo %hdf5dir%\%%d\%%l.lib
        set hdf5lib=%hdf5dir%\%%d\%%l.lib
        if %%l == hdf5dll set hdf5dll=HDF5DLL
        if %%l == hdf5dlld set hdf5dll=HDF5DLL
        goto next
      )
    )
  )
  echo ERROR:hdf5 library not found in %hdf5dir%
  goto done
)
if exist %hdf5dir%\src\hdf5.h (
  echo %hdf5dir%\src
  set hdf5inc=%hdf5dir%\src
  echo checking for hdf5 library in %hdf5dir%\proj\ ...
  for %%j in ( Release Debug ) do (
    for %%i in ( hdf5dll hdf5 ) do (
      if exist %hdf5dir%\proj\%%i\%%j\%%i.lib (
        echo %hdf5dir%\proj\%%i\%%j\%%i.lib
        set hdf5lib=%hdf5dir%\proj\%%i\%%j\%%i.lib
        if %%i == hdf5dll set hdf5dll=HDF5DLL
        goto next
      )
      if exist %hdf5dir%\proj\%%i\%%j\%%id.lib (
        echo %hdf5dir%\proj\%%i\%%j\%%id.lib
        set hdf5lib=%hdf5dir%\proj\%%i\%%j\%%id.lib
        if %%i == hdf5dll set hdf5dll=HDF5DLL
        goto next
      )
    )
  )
  echo ERROR:hdf5 library not found in %hdf5dir%\proj
  goto done
)
echo ERROR:hdf5.h not found in "%hdf5dir%\include" or "%hdf5dir%\src"
goto done

rem ----- zlib setup

:zlib
if not %1 == -zlib goto szip
echo checking for zlib ...
shift
if "%1" == "" goto findzlib
for %%a in ( %args% ) do if %1 == %%a goto findzlib
if not exist %1 (
  echo ERROR:zlib library "%1" doesn't exist
  goto done
)
if not exist %1\nul (
  set zliblib=%1
  goto gotzlib
)
for %%i in ( zdll zlib ) do (
  if exist %1\%%i.lib (
    set zliblib=%1\%%i.lib
    goto gotzlib
  )
  for /D %%d in ( %1\*.* ) do (
    if exist %%d\%%i.lib (
      set zliblib=%%d\%%i.lib
      goto gotzlib
    )
  )
)
echo ERROR:couldn't find zlib or zdll library in %1
goto done

:gotzlib
echo %zliblib%
shift
goto next

:findzlib
for %%i in ( zlib zdll ) do (
  if exist %hdf5dir%\lib\%%i.lib (
    echo %hdf5dir%\lib\%%i.lib
    set zliblib=%hdf5dir%\lib\%%i.lib
    goto next
  )
)
for /D %%d in ( %~d0\*.* ) do (
  for %%i in ( zlib zdll ) do (
    if exist %%d\%%i.lib (
      echo %%d\%%i.lib
      set zliblib=%%d\%%i.lib
      goto next
    )
  )
  for /D %%e in ( %%d\*.* ) do (
    for %%i in ( zlib zdll ) do (
      if exist %%e\%%i.lib (
        echo %%e\%%i.lib
        set zliblib=%%e\%%i.lib
        goto next
      )
    )
    for /D %%f in ( %%e\*.* ) do (
      for %%i in ( zlib zdll ) do (
        if exist %%f\%%i.lib (
          echo %%f\%%i.lib
          set zliblib=%%f\%%i.lib
          goto next
        )
      )
      for /D %%g in ( %%f\*.* ) do (
        for %%i in ( zlib zdll ) do (
          if exist %%g\%%i.lib (
            echo %%g\%%i.lib
            set zliblib=%%g\%%i.lib
            goto next
          )
        )
      )
    )
  )
)
echo ERROR:couldn't find zlib or zdll library
goto done

rem ----- szip setup

:szip
if not %1 == -szip goto xml
echo checking for szip ...
shift
if "%1" == "" goto findszip
for %%a in ( %args% ) do if %1 == %%a goto findszip

if not exist %1 (
  echo ERROR:szip library "%1" doesn't exist
  goto done
)
if not exist %1\nul (
  set sziplib=%1
  goto gotszip
)
for %%i in ( szlibdll szlib ) do (
  if exist %1\%%i.lib (
    set sziplib=%1\%%i.lib
    goto gotszip
  )
  for /D %%d in ( %1\*.* ) do (
    if exist %%d\%%i.lib (
      set sziplib=%%d\%%i.lib
      goto gotszip
    )
  )
)
echo ERROR:couldn't find szlib or szlibdll library in %1
goto done

:gotszip
echo %sziplib%
shift
goto next

:findszip
for %%i in ( szip szlib szlibdll ) do (
  if exist %hdf5dir%\lib\%%i.lib (
    echo %hdf5dir%\lib\%%i.lib
    set sziplib=%hdf5dir%\lib\%%i.lib
    goto next
)
for /D %%d in ( %~d0\*.* ) do (
  for %%i in ( szip szlib szlibdll ) do (
    if exist %%d\%%i.lib (
      echo %%d\%%i.lib
      set sziplib=%%d\%%i.lib
      goto next
    )
  )
  for /D %%e in ( %%d\*.* ) do (
    for %%i in ( szip szlib szlibdll ) do (
      if exist %%e\%%i.lib (
        echo %%e\%%i.lib
        set sziplib=%%e\%%i.lib
        goto next
      )
    )
    for /D %%f in ( %%e\*.* ) do (
      for %%i in ( szip szlib szlibdll ) do (
        if exist %%f\%%i.lib (
          echo %%f\%%i.lib
          set sziplib=%%f\%%i.lib
          goto next
        )
      )
      for /D %%g in ( %%f\*.* ) do (
        for %%i in ( szip szlib szlibdll ) do (
          if exist %%g\%%i.lib (
            echo %%g\%%i.lib
            set sziplib=%%g\%%i.lib
            goto next
          )
        )
      )
    )
  )
)
echo ERROR:couldn't find szip, szlib or szlibdll library
goto done

rem ----- expat XML setup

:xml
if not %1 == -xml goto badarg
echo checking for expat ...
shift
if "%1" == "" goto findexpat
for %%a in ( %args% ) do if %1 == %%a goto findexpat

if not exist %1\nul (
  echo ERROR:expat directory "%1" does not exist or is not a directory
  goto done
)
set expatdir=%1
shift
goto getexpat

:findexpat
echo checking for expat ...
for /D %%d in ( %~d0\*.* ) do (
  if exist %%d\expat.h (
    echo %%d
    set expatdir=%%d
    goto getexpat
  )
  for /D %%e in ( %%d\*.* ) do (
    if exist %%e\expat.h (
      echo %%e
      set expatdir=%%e
      goto getexpat
    )
    for /D %%f in ( %%e\*.* ) do (
      if exist %%f\expat.h (
        echo %%f
        set expatdir=%%f
        goto getexpat
      )
      for /D %%g in ( %%f\*.* ) do (
        if exist %%g\expat.h (
          echo %%g
          set expatdir=%%g
          goto getexpat
        )
      )
    )
  )
)
echo ERROR:couldn't find expat directory
goto done

:getexpat
echo checking for expat headers in %expatdir% ...
if exist %expatdir%\expat.h (
  echo %expatdir%
  set expatinc=%expatdir%
  echo checking for expat library in %expatdir% ...
  if exist %expatdir%\libexpat.lib (
    echo %expatdir%\libexpat.lib
    set expatlib=%expatdir%\libexpat.lib
    goto next
  )
  for %%d in ( Release Release_static Debug Debug_static ) do (
    if exist %expatdir%\%%d\libexpat.lib (
      echo %expatdir%\%%d\libexpat.lib
      set expatlib=%expatdir%\%%d\libexpat.lib
      goto next
    )
  )
  echo ERROR:expat library not found in %expatdir%
  goto done
)
if exist %expatdir%\lib\expat.h (
  echo %expatdir%\lib
  set expatinc=%expatdir%\lib
  echo checking for expat library in %expatdir%\lib ...
  if exist %expatdir%\lib\libexpat.lib (
    echo %expatdir%\lib\libexpat.lib
    set expatlib=%expatdir%\lib\libexpat.lib
    goto next
  )
  for %%d in ( Release Release_static Debug Debug_static ) do (
    if exist %expatdir%\lib\%%d\libexpat.lib (
      echo %expatdir%\lib\%%d\libexpat.lib
      set expatlib=%expatdir%\lib\%%d\libexpat.lib
      goto next
    )
  )
  echo ERROR:expat library not found in %expatdir%\lib
  goto done
)
echo ERROR:expat.h not found in "%expatdir%" or "%expatdir%\lib"
goto done

rem ----- print usage

:badarg
echo ERROR:unknown argument %1
:usage
echo usage: configure [options]
echo options:
echo   -MT : multi-threaded using libcmt.lib (default)
echo   -ML : single-threaded using libc.lib
echo   -MD : multi-threaded using mscvrt.lib and mscvrt.dll
echo   -debug : add debugging to library
echo   -lfs : enable large file support (more than 2Gb)
echo   -64 : build 64-bit version
echo   -dll : build DLL istead of static library
echo   -gmake : use gmake instead of nmake (nmake is default)
echo   -ifort : use ifort Fortran compiler (implies -f2c UPPERCASE)
echo   -absoft : use the absoft Fortran compiler (implies -f2c LOWERCASE)
echo   -f2c [type] : set Fortran to C interface. "type" is one of LOWERCASE,
echo        LOWERCASE_,LOWERCASE__,UPPERCASE,UPPERCASE_, or UPPERCASE__ If not
echo        given, LOWERCASE_ is used. This option will also use Unix-like
echo        argument passing, instead of Visual Fortran.
echo        If you specify "type" as none, the Fortran interface is disbled.
echo   -install instdir : set installation directory to "instdir" (default %~d0)
echo        headers are installed to instdir\include
echo        library is installed to instdir\lib
echo   -hdf5 [hdf5dir] : build HDF5 interface. "hdf5dir" is the HDF5 toplevel
echo        directory. If "hdf5dir" is not given, the current drive is searched.
echo   -zlib [zliblib] : use zlib. "zliblib" is the pathname to the library.
echo        If "zliblib" is not given, the current drive is searched.
echo   -szip [sziplib] : use szip. "sziplib" is the pathname to the library.
echo        If "sziplib" is not given, the current drive is searched.
echo   -xml [expatdir] : build XML interface. "expatdir" is the pathname to
echo        the expat library source. If "expatdir" is not given, the
echo        current drive is searched.
goto done

:doit
echo using %make% compatible makefile

if "%copt%" == "" set copts=-MT
if %do64bit% == 1 (
  set clibs=bufferoverflowu.lib
  set windir=WIN64
) else (
  set clibs=
  set windir=WIN32
)

set libs="ADF"

if "%hdf5inc%" == "" (
  set hdf5lib=
  set zliblib=
  set sziplib=
) else (
  set libs=%libs HDF5
  set hdf5inc=-I%hdf5inc%
  set build=%build% -DBUILD_HDF5
)

if "%expatinc%" == "" (
  set expatlib=
) else (
  set libs=%libs XML
  set expatinc=-I%expatinc%
  set build=%build% -DBUILD_XML
)

if not "%debug%" == "" set copts=%copts%d %debug%

if "%f2c%" == "none" (
  set f2cobjs=
  set f2cflags=
) else (
  set f2cobjs=$^(F2COBJS^)
  set f2cflags=%f2c%
)

rem ----- create Makefile

echo creating Makefile
echo # %make% makefile for the CGNS Library under Windows> Makefile
echo.>> Makefile
if %make% == nmake (
  echo !include make.system>> Makefile
  echo !include make.$^(SYSTEM^)>> Makefile
) else (
  echo include make.system>> Makefile
  echo include make.$^(SYSTEM^)>> Makefile
)
echo.>> Makefile
echo .SUFFIXES :>> Makefile
echo .SUFFIXES : .c .$(O) $(EXE)>> Makefile
echo.>> Makefile
echo OBJDIR  = $(SYSTEM)>> Makefile
echo CGNSLIB = $(LIBCGNS)>> Makefile
echo INSTLIB = libcgns.$(A)>> Makefile
if %target% == dll (
  echo CGNSDLL = $^(OBJDIR^)\libcgns.dll>> Makefile
  echo INSTDLL = libcgns.dll>> Makefile
)
echo.>> Makefile
echo COPTS   = $(CFLAGS) $(CFGFLAGS) -I. %build% %dllopts%>> Makefile
if not "%hdf5inc%" == "" (
echo # uncomment the following when using HDF5 DLL>> Makefile
echo %hdf5dll% = -DWIN32 -D_HDF5USEDLL_>> Makefile
)
echo.>> Makefile
echo #---------->> Makefile
echo.>> Makefile
echo CGNSOBJS=\>> Makefile
echo 	$(OBJDIR)\cgns_error.$(O) \>> Makefile
echo 	$(OBJDIR)\cgns_internals.$(O) \>> Makefile
echo 	$(OBJDIR)\cgns_io.$(O) \>> Makefile
echo 	$(OBJDIR)\cgnslib.$(O)>> Makefile
echo.>> Makefile
echo # ADF/ADFH routines>> Makefile
echo.>> Makefile
echo ADFOBJS=\>>Makefile
if not "%hdf5inc%" == "" echo 	$(OBJDIR)\ADFH.$(O) \>>Makefile
if not "%expatinc%" == "" echo 	$(OBJDIR)\ADFX.$(O) \>>Makefile
echo 	$(OBJDIR)\ADF_interface.$(O) \>>Makefile
echo 	$(OBJDIR)\ADF_internals.$(O)>> Makefile
echo.>> Makefile
echo F2COBJS= $(OBJDIR)\cg_ftoc.$(O) $(OBJDIR)\cgio_ftoc.$(O)>> Makefile
echo.>> Makefile
echo #---------->> Makefile
echo.>> Makefile
echo all     : %target%>> Makefile
echo lib     : $(CGNSLIB)>> Makefile
if %target% == dll echo dll     : $(CGNSDLL)>> Makefile
echo current : update all>> Makefile
echo.>> Makefile
echo #---------->> Makefile
echo.>> Makefile
echo $(CGNSLIB) : $(OBJDIR) $(CGNSOBJS) $(ADFOBJS) %f2cobjs%>> Makefile
echo 	-@$(RM) $@>> Makefile
echo 	$(AR) /lib /nologo $(AROUT)$@ $(CGNSOBJS) $(ADFOBJS) %f2cobjs%>> Makefile
if not %target% == dll goto make2
echo.>> Makefile
echo #---------->> Makefile
echo.>> Makefile
echo $(CGNSDLL) : $(OBJDIR) $(CGNSOBJS) $(ADFOBJS) %f2cobjs%>> Makefile
echo 	-@$(RM) $@ $(CGNSLIB)>> Makefile
echo 	$(AR) /dll /nologo $(AROUT)$@ $(CGNSOBJS) $(ADFOBJS) %f2cobjs% $(BUILDLIBS)>> Makefile
:make2
echo.>> Makefile
echo #---------->> Makefile
echo.>> Makefile
echo $(OBJDIR) :>> Makefile
echo 	-$(MKDIR) $(OBJDIR)>> Makefile
echo.>> Makefile
echo update :>> Makefile
echo 	cvs update>> Makefile
echo.>> Makefile
echo #---------->> Makefile
echo.>> Makefile
echo clean :>> Makefile
echo 	-cd $(OBJDIR) ^&^& $(RM) *.$(O)>> Makefile
echo.>> Makefile
echo allclean : distclean>> Makefile
echo.>> Makefile
echo distclean : clean>> Makefile
if %make% == nmake (
echo 	-cd tools ^&^& nmake /nologo allclean>> Makefile
echo 	-cd tests ^&^& nmake /nologo allclean>> Makefile
) else (
echo 	-cd tools ^&^& %make% allclean>> Makefile
echo 	-cd tests ^&^& %make% allclean>> Makefile
)
echo 	-cd tools ^&^& $(RM) Makefile>> Makefile
echo 	-cd tests ^&^& $(RM) Makefile>> Makefile
echo 	-$(RM) $(CGNSLIB)>> Makefile
if %target% == dll echo 	-$(RM) $(CGNSDLL)>> Makefile
echo 	-$(RMDIR) $(OBJDIR)>> Makefile
echo 	-$(RM) make.system make.defs make.$(SYSTEM) make.$(SYSTEM).orig>> Makefile
echo 	-$(RM) *.pdb>> Makefile
echo 	-$(RM) Makefile>> Makefile
echo.>> Makefile
echo install : %target% $(INCLUDEDIR) $(LIBDIR)>> Makefile
echo 	$(INSTALL_DATA) cgnslib.h $(INCLUDEDIR)\cgnslib.h>> Makefile
echo 	$(INSTALL_DATA) cgnslib_f.h $(INCLUDEDIR)\cgnslib_f.h>> Makefile
echo 	$(INSTALL_DATA) cgnswin_f.h $(INCLUDEDIR)\cgnswin_f.h>> Makefile
echo 	$(INSTALL_DATA) cgns_io.h $(INCLUDEDIR)\cgns_io.h>> Makefile
echo 	$(INSTALL_DATA) $(CGNSLIB) $(LIBDIR)\$(INSTLIB)>> Makefile
if %target% == dll echo 	$(INSTALL_DATA) $(CGNSDLL) $(LIBDIR)\$(INSTDLL)>> Makefile
echo.>> Makefile
echo $(INCLUDEDIR) :>> Makefile
echo 	$(MKDIR) $(INCLUDEDIR)>> Makefile
echo $(LIBDIR) :>> Makefile
echo 	$(MKDIR) $(LIBDIR)>> Makefile
echo.>> Makefile
echo uninstall :>> Makefile
echo 	-$(RM) $(INCLUDEDIR)\cgnslib.h>> Makefile
echo 	-$(RM) $(INCLUDEDIR)\cgnslib_f.h>> Makefile
echo 	-$(RM) $(INCLUDEDIR)\cgnswin_f.h>> Makefile
echo 	-$(RM) $(INCLUDEDIR)\cgns_io.h>> Makefile
echo 	-$(RM) $(LIBDIR)\$(INSTLIB)>> Makefile
if %target% == dll echo 	-$(RM) $(LIBDIR)\$(INSTDLL)>> Makefile
echo.>> Makefile
echo #---------- mid-level library>> Makefile
echo.>> Makefile
echo $(OBJDIR)\cgns_error.$(O) : cgns_error.c cgnslib.h cgns_header.h cgns_io.h>> Makefile
echo 	$(CC) $(COPTS) $(COOUT)$@ -c cgns_error.c>> Makefile
echo.>> Makefile
echo $(OBJDIR)\cgns_internals.$(O) : cgns_internals.c cgnslib.h cgns_header.h cgns_io.h>> Makefile
echo 	$(CC) $(COPTS) $(COOUT)$@ -c cgns_internals.c>> Makefile
echo.>> Makefile
echo $(OBJDIR)\cgns_io.$(O) : cgns_io.c cgnslib.h cgns_io.h \>> Makefile
set includes=adf\ADF.h
if not "%hdf5inc%" == "" set includes=%includes% adfh\ADFH.h
if not "%expatinc%" == "" set includes=%includes% adfx\ADFX.h
echo 	%includes%>> Makefile
echo 	$(CC) $(COPTS) $(COOUT)$@ -c cgns_io.c>> Makefile
echo.>> Makefile
echo $(OBJDIR)\cgnslib.$(O) : cgnslib.c cgnslib.h cgns_header.h cgns_io.h>> Makefile
echo 	$(CC) $(COPTS) $(COOUT)$@ -c cgnslib.c>> Makefile
echo.>> Makefile
echo $(OBJDIR)\cg_ftoc.$(O) : cg_ftoc.c fortran_macros.h cgnslib.h cgns_header.h cgns_io.h>> Makefile
echo 	$(CC) $(COPTS) $(F2CFLAGS) $(COOUT)$@ -c cg_ftoc.c>> Makefile
echo $(OBJDIR)\cgio_ftoc.$(O) : cgio_ftoc.c fortran_macros.h cgns_io.h>> Makefile
echo 	$(CC) $(COPTS) $(F2CFLAGS) $(COOUT)$@ -c cgio_ftoc.c>> Makefile
echo.>> Makefile
echo #---------- ADF>> Makefile
echo.>> Makefile
echo $(OBJDIR)\ADF_interface.$(O) : adf\ADF_interface.c \>> Makefile
echo 	adf\ADF.h adf\ADF_internals.h>> Makefile
echo 	$(CC) $(COPTS) -Iadf $(COOUT)$@ -c adf\ADF_interface.c>> Makefile
echo.>> Makefile
echo $(OBJDIR)\ADF_internals.$(O) : adf\ADF_internals.c \>> Makefile
echo 	adf\ADF.h adf\ADF_internals.h>> Makefile
echo 	$(CC) $(COPTS) -Iadf $(COOUT)$@ -c adf\ADF_internals.c>> Makefile
echo.>> Makefile
echo #---------- HDF5>> Makefile
echo.>> Makefile
echo $(OBJDIR)\ADFH.$(O) : adfh\ADFH.c adfh\ADFH.h>> Makefile
echo 	$(CC) $(COPTS) -Iadfh $(HDF5INC) $(HDF5DLL) $(COOUT)$@ -c adfh\ADFH.c>> Makefile
echo.>> Makefile
echo #---------- XML>> Makefile
echo.>> Makefile
echo $(OBJDIR)\ADFX.$(O) : adfx\ADFX.c adfx\ADFX.h>> Makefile
echo 	$(CC) $(COPTS) -Iadfx $(EXPATINC) $(COOUT)$@ -c adfx\ADFX.c>> Makefile

rem ----- create make.system

echo creating make.system
echo SYSTEM = %windir%> make.system

rem ----- create make.defs

echo creating make.defs
echo # makefile include for %windir%> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # these should only be set if building HDF5 interface>> make.defs
echo # HDF5INC - path to HDF5 header files>> make.defs
echo # HDF5LIB - HDF5 library>> make.defs
echo # SZIPLIB - szip library (if needed)>> make.defs
echo # ZLIBLIB - zlib library (if needed)>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo HDF5INC = %hdf5inc%>> make.defs
echo HDF5LIB = %hdf5lib%>> make.defs
echo SZIPLIB = %sziplib%>> make.defs
echo ZLIBLIB = %zliblib%>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # these should only be set if building XML interface>> make.defs
echo # EXPATINC - path to expat header files>> make.defs
echo # EXPATLIB - expat library>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo EXPATINC = %expatinc%>> make.defs
echo EXPATLIB = %expatlib%>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # BUILDLIBS contains the list of additional libraries>> make.defs
echo #           with which a CGNS application needs to link>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo BUILDLIBS = $(HDF5LIB) $(SZIPLIB) $(ZLIBLIB) $(EXPATLIB)>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # SPACE  - used to force a space in the compiler executable output flag>> make.defs
echo # O       - object file extension>> make.defs
echo # A       - library file extension>> make.defs
echo # EXE     - executable extension>> make.defs
echo # LIBCGNS - CGNS library name>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo SPACE   =>> make.defs
echo O       = obj>> make.defs
echo A       = lib>> make.defs
echo EXE     = .exe>> make.defs
echo.>> make.defs
echo LIBCGNS = %windir%\libcgns.lib>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # CC     - C compiler>> make.defs
echo # CFLAGS - compiler flags>> make.defs
echo # COOUT  - flag to name object output file (typically -o)>> make.defs
echo # CEOUT  - flag to name the output executable (typically -o)>> make.defs
echo # CLIBS  - any additional libraries needed to link a CGNS application>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs>> make.defs
echo CC     = cl>> make.defs
echo CFLAGS = -nologo %copts%>> make.defs
echo COOUT  = -Fo>> make.defs
echo CEOUT  = -Fe>> make.defs
echo CLIBS  = %clibs%>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # F2CFLAGS defines the type of Fortran to C interface.>> make.defs
echo # -DUSE_ADF_MACROS causes the ADF Fortran to C interface to use>> make.defs
echo #    the old ADF macros instead of the same macros as the CGNS library.>> make.defs
echo #>> make.defs
echo # CFGFLAGS defines any additional compiler options needed to build>> make.defs
echo # the CGNS library. This is typically set by the configure script.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo F2CFLAGS = %f2cflags%>> make.defs
echo CFGFLAGS = %cfgflags%>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # strip command for executables - set to true if not used>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo STRIP  = :>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # library archiver and ranlib>> make.defs
echo # AROUT may be used to set a library output flag as:>> make.defs
echo #    $(AR) $(AROUT)library_name objects>> make.defs
echo # Set RANLIB to true if not used>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo AR     = link>> make.defs
echo AROUT  = /out:>> make.defs
echo RANLIB = :>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # commands for removing files and creating/deleting directory>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
if %make% == nmake (
echo RM     = del /q>> make.defs
echo RMDIR  = rmdir /s/q>> make.defs
) else (
echo RM     = cmd /c del /q>> make.defs
echo RMDIR  = cmd /c rmdir /s/q>> make.defs
)
echo MKDIR  = mkdir>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # installation library name and directories>> make.defs
echo #>> make.defs
echo # INSTALL      - install command>> make.defs
echo # INSTALL_PROG - install executable>> make.defs
echo # INSTALL_DATA - install data>> make.defs
echo # LIBDIR       - installation directory for CGNS library>> make.defs
echo # INCLUDEDIR   - installation directory for CGNS headers>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
if %make% == nmake (
echo INSTALL      = copy /b>> make.defs
) else (
echo INSTALL      = cmd /c copy /b>> make.defs
)
echo INSTALL_PROG = $(INSTALL)>> make.defs
echo INSTALL_DATA = $(INSTALL)>> make.defs
echo LIBDIR       = %instdir%\lib>> make.defs
echo INCLUDEDIR   = %instdir%\include>> make.defs
echo.>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo # These are not used to build the CGNS library>> make.defs
echo # Fortran compiler (F77) and options (FFLAGS).>> make.defs
echo # FEOUT is the flag to name the output executable (typically -o).>> make.defs
echo # FLIBS lists any additional libraries needed to link a CGNS application>> make.defs
echo #------------------------------------------------------------------------>> make.defs
echo.>> make.defs
echo F77    = %f77%>> make.defs
if %f77% == ifort (
echo FFLAGS = /nologo /extfpp:F /fpp %copts%>> make.defs
) else (
echo FFLAGS = /nologo /extfpp:F /fpp:"/DWINNT" %copts%>> make.defs
)
echo FEOUT  = /exe:>> make.defs
echo FLIBS  =>> make.defs
echo.>> make.defs

rem ----- copy make.defs to make.WIN32

if exist make.%windir% (
  fc make.defs make.%windir% > nul
  if errorlevel 1 (
    echo make.%windir% already exists and differs from make.defs
    echo renaming make.%windir% to make.%windir%.orig
    if exist make.%windir%.orig del make.%windir%.orig
    ren make.%windir% make.%windir%.orig
    echo renaming make.defs to make.%windir%
    ren make.defs make.%windir%
  ) else (
    echo make.defs and make.%windir% are the same - removing make.defs
    del make.defs
  )
) else (
  echo renaming make.defs to make.%windir%
  ren make.defs make.%windir%
)

rem ----- create tools/Makefile

if not exist tools\nul goto tests

echo creating tools\Makefile
echo # %make% makefile> tools\Makefile
echo.>> tools\Makefile
echo CGNSDIR = ..>> tools\Makefile
if %make% == nmake echo !include $(CGNSDIR)\make.system>> tools\Makefile
if %make% == gmake echo include $(CGNSDIR)/make.system>> tools\Makefile
if %make% == nmake echo !include $(CGNSDIR)\make.$(SYSTEM)>> tools\Makefile
if %make% == gmake echo include $(CGNSDIR)/make.$(SYSTEM)>> tools\Makefile
echo.>> tools\Makefile
echo CGNSLIB = $(CGNSDIR)\$(LIBCGNS)>> tools\Makefile
echo.>> tools\Makefile
echo COPTS  = $(CFLAGS) -I$(CGNSDIR)>> tools\Makefile
echo LDLIBS = $(CGNSLIB) $(BUILDLIBS)>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo ALL =	cgnslist$(EXE) \>> tools\Makefile
echo 	cgnscheck$(EXE) \>> tools\Makefile
echo 	cgnsversion$(EXE) \>> tools\Makefile
echo 	cgnsconvert$(EXE) \>> tools\Makefile
echo 	cgns2xml$(EXE) \>> tools\Makefile
echo 	cgnsdiff$(EXE)>> tools\Makefile
echo.>> tools\Makefile
echo all : $(ALL)>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo cgnslist$(EXE) : cgnslist.$(O) getargs.$(O) $(CGNSLIB)>> tools\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ cgnslist.$(O) getargs.$(O) $(LDLIBS) $(CLIBS)>> tools\Makefile
echo cgnslist.$(O) : cgnslist.c getargs.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgnslist.c>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo cgnscheck$(EXE) : cgnscheck.$(O) getargs.$(O) hash.$(O) cgnames.$(O) $(CGNSLIB)>> tools\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ cgnscheck.$(O) getargs.$(O) hash.$(O) cgnames.$(O) $(LDLIBS) $(CLIBS)>> tools\Makefile
echo cgnscheck.$(O) : cgnscheck.c getargs.h hash.h cgnames.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgnscheck.c>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo cgnsversion$(EXE) : cgnsversion.$(O) getargs.$(O) $(CGNSLIB)>> tools\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ cgnsversion.$(O) getargs.$(O) $(LDLIBS) $(CLIBS)>> tools\Makefile
echo cgnsversion.$(O) : cgnsversion.c getargs.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgnsversion.c>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo cgnsconvert$(EXE) : cgnsconvert.$(O) getargs.$(O) $(CGNSLIB)>> tools\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ cgnsconvert.$(O) getargs.$(O) $(LDLIBS) $(CLIBS)>> tools\Makefile
echo cgnsconvert.$(O) : cgnsconvert.c getargs.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgnsconvert.c>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo cgns2xml$(EXE) : cgns2xml.$(O) getargs.$(O) $(CGNSLIB)>> tools\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ cgns2xml.$(O) getargs.$(O) $(LDLIBS) $(CLIBS)>> tools\Makefile
echo cgns2xml.$(O) : cgns2xml.c getargs.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgns2xml.c>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo cgnsdiff$(EXE) : cgnsdiff.$(O) getargs.$(O) $(CGNSLIB)>> tools\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ cgnsdiff.$(O) getargs.$(O) $(LDLIBS) $(CLIBS)>> tools\Makefile
echo cgnsdiff.$(O) : cgnsdiff.c getargs.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgnsdiff.c>> tools\Makefile
echo.>> tools\Makefile
echo #---------->> tools\Makefile
echo.>> tools\Makefile
echo getargs.$(O) : getargs.c getargs.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c getargs.c>> tools\Makefile
echo.>> tools\Makefile
echo hash.$(O) : hash.c hash.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c hash.c>> tools\Makefile
echo.>> tools\Makefile
echo cgnames.$(O) : cgnames.c cgnames.h>> tools\Makefile
echo 	$(CC) $(COPTS) -c cgnames.c>> tools\Makefile
echo.>> tools\Makefile
echo clean :>> tools\Makefile
echo 	-$(RM) *.$(O)>> tools\Makefile
echo.>> tools\Makefile
echo allclean : clean>> tools\Makefile
echo 	-$(RM) *.exe>> tools\Makefile
echo 	-$(RM) *.pdb *.ilk>> tools\Makefile

rem ----- create tests/Makefile

:tests
if not exist tests\nul goto done

echo creating tests\Makefile
echo # %make% makefile> tests\Makefile
echo.>> tests\Makefile
echo CGNSDIR = ..>> tests\Makefile
if %make% == nmake echo !include $(CGNSDIR)\make.system>> tests\Makefile
if %make% == gmake echo include $(CGNSDIR)/make.system>> tests\Makefile
if %make% == nmake echo !include $(CGNSDIR)\make.$(SYSTEM)>> tests\Makefile
if %make% == gmake echo include $(CGNSDIR)/make.$(SYSTEM)>> tests\Makefile
echo.>> tests\Makefile
echo CGNSLIB = $(CGNSDIR)\$(LIBCGNS)>> tests\Makefile
echo.>> tests\Makefile
echo COPTS  = $(CFLAGS) -I$(CGNSDIR)>> tests\Makefile
echo LDLIBS = $(CGNSLIB) $(BUILDLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo FOPTS  = $(FFLAGS) -I$(CGNSDIR)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo ALL = \>> tests\Makefile
echo 	dbtest$(EXE) \>> tests\Makefile
echo 	open_cgns$(EXE) \>> tests\Makefile
echo 	test_exts$(EXE) \>> tests\Makefile
echo 	test_partial$(EXE) \>> tests\Makefile
echo 	test_goto$(EXE) \>> tests\Makefile
echo 	write_array$(EXE) \>> tests\Makefile
echo 	write_links$(EXE) \>> tests\Makefile
echo 	write_bcdata$(EXE) \>> tests\Makefile
echo 	write_test$(EXE) \>> tests\Makefile
echo 	write_zones$(EXE) \>> tests\Makefile
echo 	write_rind$(EXE)>> tests\Makefile
echo.>> tests\Makefile
echo all : $(ALL)>> tests\Makefile
echo.>> tests\Makefile
if not "%f2c%" == "none" (
  echo FALL =	cgwrite$^(EXE^) \>> tests\Makefile
  echo 	cgread$^(EXE^)>> tests\Makefile
  echo.>> tests\Makefile
  echo fortran : $^(FALL^)>> tests\Makefile
  echo.>> tests\Makefile
)
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo dbtest$(EXE) : dbtest.$(O) utils.$(O) $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ dbtest.$(O) utils.$(O) $(LDLIBS) $(CLIBS)>> tests\Makefile
echo dbtest.$(O) : dbtest.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c dbtest.c>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo open_cgns$(EXE) : open_cgns.$(O) utils.$(O) $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ open_cgns.$(O) utils.$(O) $(LDLIBS) $(CLIBS)>> tests\Makefile
echo open_cgns.$(O) : open_cgns.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c open_cgns.c>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo test_exts$(EXE) : test_exts.c $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ test_exts.c $(LDLIBS) $(CLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo test_partial$(EXE) : test_partial.c $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ test_partial.c $(LDLIBS) $(CLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo test_goto$(EXE) : test_goto.c $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ test_goto.c $(LDLIBS) $(CLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo write_array$(EXE) : write_array.$(O) utils.$(O) $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ write_array.$(O) utils.$(O) $(LDLIBS) $(CLIBS)>> tests\Makefile
echo write_array.$(O) : write_array.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c write_array.c>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo write_links$(EXE) : write_links.$(O) utils.$(O) $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ write_links.$(O) utils.$(O) $(LDLIBS) $(CLIBS)>> tests\Makefile
echo write_links.$(O) : write_links.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c write_links.c>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo write_bcdata$(EXE) : write_bcdata.$(O) utils.$(O) $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ write_bcdata.$(O) utils.$(O) $(LDLIBS) $(CLIBS)>> tests\Makefile
echo write_bcdata.$(O) : write_bcdata.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c write_bcdata.c>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo write_test$(EXE) : write_test.c $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ write_test.c $(LDLIBS) $(CLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo write_zones$(EXE) : write_zones.$(O) utils.$(O) $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(CFLAGS) $(CEOUT)$@ write_zones.$(O) utils.$(O) $(LDLIBS) $(CLIBS)>> tests\Makefile
echo write_zones.$(O) : write_zones.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c write_zones.c>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo write_rind$(EXE) : write_rind.c $(CGNSLIB)>> tests\Makefile
echo 	$(CC) $(COPTS) $(CEOUT)$@ write_rind.c $(LDLIBS) $(CLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo cgwrite$(EXE) : cgwrite.F $(CGNSLIB)>> tests\Makefile
echo 	$(F77) $(FOPTS) $(FEOUT)$@ cgwrite.F $(LDLIBS) $(FLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo cgread$(EXE) : cgread.F $(CGNSLIB)>> tests\Makefile
echo 	$(F77) $(FOPTS) $(FEOUT)$@ cgread.F $(LDLIBS) $(FLIBS)>> tests\Makefile
echo.>> tests\Makefile
echo #---------->> tests\Makefile
echo.>> tests\Makefile
echo utils.$(O) : utils.c utils.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c utils.c>> tests\Makefile
echo.>> tests\Makefile
echo getargs.$(O) : getargs.c getargs.h>> tests\Makefile
echo 	$(CC) $(COPTS) -c getargs.c>> tests\Makefile
echo.>> tests\Makefile
echo clean :>> tests\Makefile
echo 	-$(RM) *.$(O)>> tests\Makefile
echo.>> tests\Makefile
echo allclean : clean>> tests\Makefile
echo 	-$(RM) *.exe>> tests\Makefile
echo 	-$(RM) *.pdb *.ilk>> tests\Makefile
echo 	-$(RM) *.cgns>> tests\Makefile

:done
endlocal

