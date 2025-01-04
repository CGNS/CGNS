REM SPDX-FileCopyrightText: 2022 Intel Corporation
REM
REM SPDX-License-Identifier: MIT

set VS_VER=%1
set CGNS_SRC=%2

IF "%VS_VER%"=="2017_build_tools" (
    @call "C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvars64.bat"
) ELSE (
    IF "%VS_VER%"=="2019_build_tools" (
        @call "C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvars64.bat"
    ) ELSE (
          @call "C:\Program Files (x86)\Intel\oneAPI\setvars-vcvarsall.bat" %VS_VER%
    )
)

for /f "tokens=* usebackq" %%f in (`dir /b "C:\Program Files (x86)\Intel\oneAPI\compiler\" ^| findstr /V latest ^| sort`) do @set "LATEST_VERSION=%%f"
@call "C:\Program Files (x86)\Intel\oneAPI\compiler\%LATEST_VERSION%\env\vars.bat"

cmake -G "Visual Studio 17 2022" ^
         -D CGNS_BUILD_SHARED:BOOL=ON -D CGNS_USE_SHARED:BOOL=ON -D CGNS_ENABLE_64BIT:BOOL=ON ^
         -D CMAKE_C_FLAGS:STRING="" ^
         -D CMAKE_BUILD_TYPE:STRING=Debug ^
         -D CMAKE_C_COMPILER=icx ^
         -D CMAKE_FORTRAN_COMPILER=ifx ^
         -D HDF5_NEED_ZLIB:BOOL=ON ^
         -D CMAKE_STATIC_LINKER_FLAGS:STRING="" ^
         -D CGNS_ENABLE_HDF5:BOOL=ON ^
         -D HDF5_DIR:PATH="C:\Program Files\HDF_Group\HDF5\1.14.3\cmake" ^
         -D CMAKE_PREFIX_PATH:PATH="C:\Program Files\HDF_Group\HDF5\1.14.3\cmake" ^
         -D CGNS_ENABLE_TESTS:BOOL=ON ^
         -D CGNS_ENABLE_LFS:BOOL=OFF ^
         -D CGNS_BUILD_CGNSTOOLS:BOOL=OFF ^
         -D CGNS_ENABLE_SCOPING:BOOL=OFF ^
         -D CGNS_ENABLE_FORTRAN:BOOL=ON ^
         -D CGNS_ENABLE_PARALLEL:BOOL=OFF ^
         -D CMAKE_INSTALL_PREFIX:PATH='.' ^
         %CGNS_SRC%

set RESULT=%ERRORLEVEL%
goto exit

:exit
exit /b %RESULT%
