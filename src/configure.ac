
AC_PREREQ(2.69)

#--------------------------------------------------------------------------
# Redefine the cache macros so that a cache file is not created.
# The cache file can cause problems when configuring for multiple machines.
#--------------------------------------------------------------------------

define([AC_CACHE_LOAD], )dnl
define([AC_CACHE_SAVE], )dnl

AC_INIT(cgnslib.c)

#--------------------------------------------------------------------------
# cleanup any previous build
#--------------------------------------------------------------------------

if test -f Makefile -a -f make.defs; then
  AC_MSG_RESULT("removing previous build")
  make distclean >/dev/null 2>&1
fi

#--------------------------------------------------------------------------
# get the CGNS version
#--------------------------------------------------------------------------

AC_MSG_CHECKING([CGNS version])
CGNSVERS=`grep CGNS_DOTVERS cgnslib.h | sed "s/#define *CGNS_DOTVERS *//"`
AC_MSG_RESULT($CGNSVERS)

CGNSDIR=`/bin/pwd`

## The following variables are used to distinguish between building a
## serial and parallel library.
##
##
##    PARALLEL		-- This variable is set to a non-null value if
##			   configure thinks we're compiling a parallel
##			   version of the library.
##
##    MPIEXEC	        -- This is a command which will be prepended to
##			   the executable name to run the executable on
##			   multiple processors. For the serial library the
##			   value will normally be the empty string. For
##			   parallel library it should be something like
##			   "mpiexec -n \$\${NPROCS:=4}" where NPROCS will
##			   eventually contain the number of processors on which
##			   to run the executable (the double dollarsigns are to
##			   protect the expansion until make executes the
##			   command).  The value of this variable is
##			   substituted in *.in files.
##
AC_SUBST([PARALLEL])

#--------------------------------------------------------------------------
# get the C compiler
# If a C compiler not found, fail since can't build library
#--------------------------------------------------------------------------

AC_MSG_CHECKING([if gcc is requested])
AC_ARG_ENABLE(gcc,
  [  --enable-gcc            use gcc if it exists [default=cc]],,
  enableval=no)
test ! "$enableval" = "no" && enableval=yes
use_gcc=$enableval
AC_MSG_RESULT($use_gcc)

# ----------------------------------------------------------------------
# Check for programs.
#
AC_PROG_CC
CC_BASENAME="`echo $CC | cut -f1 -d' ' | xargs basename 2>/dev/null`"

##if test $use_gcc = yes; then
##  AC_PROG_CC
##else
  if test -z "$CC"; then
    AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
    if test -z "$CC"; then
      AC_CHECK_PROG(CC, gcc, gcc)
      if test -z "$CC"; then
        AC_MSG_ERROR([no acceptable cc found in \$PATH])
      fi
    fi
  fi
##  AC_PROG_CC
##  AC_PROG_CC_WORKS
##  AC_PROG_CC_GNU
##fi

if test "$ac_cv_prog_gcc" = "yes"; then
  test -z "$CFLAGS" && CFLAGS=-O2
  # probably don't need this
  # CFLAGS="$CFLAGS -fwritable-strings -ffloat-store"
fi

CEOUT="-o \$(SPACE)"
COOUT="-o \$(SPACE)"

# check if the compiler supports prototyping - required

AC_LANG_PUSH(C)
AC_MSG_CHECKING([if compiler supports prototyping])

AC_LINK_IFELSE([AC_LANG_PROGRAM([[extern int func(int,int);
int func(int i, int j){return i + j;}]],[[func(0,1)]])],
[proto=yes],[proto=no])

AC_MSG_RESULT($proto)
test $proto = no && \
  AC_MSG_ERROR([compiler does not support prototyping])

#--------------------------------------------------------------------------
# get object and executable extensions
#--------------------------------------------------------------------------

LIBEXT=a

# Set the Fortran flags using FCFLAGS. If both FFLAGS (which predates FCFLAGS)
# and FCFLAGS is set then FFLAGS is set and FCFLAGS is ignored.
if test "x" = "x$FFLAGS" && test "x" != "x$FCFLAGS"; then
   FFLAGS="$FCFLAGS"
fi

#--------------------------------------------------------------------------
# get the Fortran compiler
# don't need it to build library, so don't fail if not found
# or the compiler doesn't work (don't use AC_PROG_F77)
# If running under CYGWIN, make sure we get g77 instead of Visual Fortran
#--------------------------------------------------------------------------
if test "x" != "x$FC"; then
    has_f77=yes
    F77=$FC
else
  if test -z "$F77"; then
    if test "$ac_cv_cygwin" = "yes"; then
      AC_CHECK_PROGS(F77, g77 f2c)
    else
      AC_CHECK_PROGS(F77, f90 f95 f2c)
    fi
  fi
  if test -z "$F77"; then
    has_f77=no
    F77=f90
    AC_MSG_WARN(no Fortran compiler was found)
  else
    has_f77=yes
   # --------------------------------------------------------------------
   # Check for a Fortran compiler
   #
   AC_PROG_FC([PAC_FC_SEARCH_LIST],)
  ##  AC_PROG_F77_GNU
    if test "$ac_cv_prog_g77" = "yes"; then
      test -z "$FFLAGS" && FFLAGS=-O2
      test ! "$F77" = "f95" && FFLAGS="$FFLAGS -Wno-globals"
    fi
    rm -f conftest*
  fi
fi
FEOUT="-o \$(SPACE)"

#--------------------------------------------------------------------------
# check if debug requested
#--------------------------------------------------------------------------

BUILDDEBUG=0

AC_MSG_CHECKING([if debugging is enabled])
AC_ARG_ENABLE(debug,
  [  --enable-debug[=OPTS]   build for debugging [default=no]],,
  enableval=no)
if test "$enableval" = "no"; then
  debug=no
else
  if test "$enableval" = "" || test "$enableval" = "yes"; then
    opt=-g
  else
    opt=$enableval
  fi
  CFLAGS="$opt $CFLAGS"
  FFLAGS="$opt $FFLAGS"
  debug=yes
  BUILDDEBUG=1
fi
AC_MSG_RESULT($debug)

#--------------------------------------------------------------------------
# check for programs - initialize library archiver
#--------------------------------------------------------------------------

AR="ar cr"
AC_CHECK_PROG(RANLIB,ranlib,ranlib,:)
if test $debug = yes; then
  STRIP=:
else
  AC_CHECK_PROG(STRIP,strip,strip,:)
fi
AROUT=

#--------------------------------------------------------------------------
# check for header files
#--------------------------------------------------------------------------

AC_HEADER_STDC
test "$ac_cv_header_stdc" = "no" && \
	AC_MSG_WARN(Standard C header files not found - may not compile)

#--------------------------------------------------------------------------
# check for typedefs and structures
#--------------------------------------------------------------------------

CFGFLAGS=

AC_C_CONST
test "ac_cv_c_const" = "no" && \
  CFGFLAGS="$CFGFLAGS -Dconst="

AC_TYPE_SIZE_T
test "$ac_cv_type_size_t" = "no" && \
  CFGFLAGS="$CFGFLAGS -Dsize_t=unsigned"

AC_PROG_CC_C99
AS_IF([test "$ac_cv_prog_cc_c99" = "no"],
      [AC_MSG_WARN([libcgns requires a C99-compliant compiler])
       AC_MSG_ERROR([Cannot continue])])
AC_MSG_CHECKING([for Complex Gnu Extension])
AC_LANG_PUSH(C)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
    #include <complex.h>
    ]],[[
    #if !defined(_MSC_VER)
    float _Complex a[2];
    __real__(a[1]) = (float) 1.0;
    __imag__(a[1]) = (float) 0.0;
    #endif
]])],[ AC_MSG_RESULT([yes])
       HAVE_COMPLEX_C99_EXT=1],
     [ AC_MSG_RESULT([no])
       HAVE_COMPLEX_C99_EXT=0])

#--------------------------------------------------------------------------
# installation program and directory
#--------------------------------------------------------------------------

test ! -x install-sh && \
  chmod +x install-sh
AC_PROG_INSTALL

if test -z $prefix || test $prefix = NONE; then
  prefix=/usr/local
fi
if test -z $exec_prefix || test $exec_prefix = NONE; then
  exec_prefix=$prefix
fi
if test "$datarootdir" = "\${prefix}/share"; then
  datadir=$datarootdir/cgnstools
fi

AC_MSG_CHECKING([where to install library])
eval "LIBDIR=$libdir"
AC_MSG_RESULT($LIBDIR)

AC_MSG_CHECKING([where to install headers])
eval "INCLUDEDIR=$includedir"
AC_MSG_RESULT($INCLUDEDIR)

AC_MSG_CHECKING([where to install executables])
eval "BINDIR=$bindir"
AC_MSG_RESULT($BINDIR)

AC_MSG_CHECKING([where to install Tcl scripts])
eval "LIB_INSTALL_DIR=$datadir"
AC_MSG_RESULT($LIB_INSTALL_DIR)

#--------------------------------------------------------------------------
# get the system name and set system dependent compile flags
#--------------------------------------------------------------------------

BUILDDIR=lib
BUILD64BIT=0

test ! -x cgsystem && \
  chmod +x cgsystem

AC_MSG_CHECKING([if system was specified])
AC_ARG_WITH(system,
  [  --with-system[=SYSTEM]  specify system],withsys=$withval)
if test -z "$withsys" || test "$withsys" = "no"; then
  AC_MSG_RESULT(no)
  AC_MSG_CHECKING([if 64bit support is requested])
  AC_ARG_ENABLE(64bit,
    [  --enable-64bit          enable 64bit support (where applicable)],,
    enableval=yes)
  test ! "$enableval" = "no" && enableval=yes
  do64bit=$enableval
  AC_MSG_RESULT($do64bit)
  test $do64bit = yes && opts=-64
  AC_MSG_CHECKING(cgsystem)
  SYSTEM=`./cgsystem $opts`
  AC_MSG_RESULT($SYSTEM)
  if test $do64bit = yes; then
    BUILD64BIT=1
    AC_CHECK_SIZEOF([void *])
    if test $ac_cv_sizeof_void_p -lt 8; then
      AC_MSG_WARN(64bit support not found for your machine)
      is64bit=no
    else
      is64bit=yes
    fi
  fi
else
  SYSTEM=$withsys
  AC_MSG_RESULT($SYSTEM)
  is64bit=`echo $SYSTEM | grep 64`
  if test -z "$is64bit"; then
    is64bit=no
  else
    is64bit=yes
  fi
fi

if test "$ac_cv_prog_gcc" = "no"; then
  case $SYSTEM in
    CRAY)   AR="bld rz" ;;
    HPPA11) SYSCFLAGS="+DA2.0"
            SYSFFLAGS="+DA2.0" ;;
    HPPA64) SYSCFLAGS="+DA2.0W"
            SYSFFLAGS="+DA2.0W" ;;
    HPIT)   SYSCFLAGS="+DD32"
            SYSFFLAGS="+DD32" ;;
    HPIT64) SYSCFLAGS="+DD64"
            SYSFFLAGS="+DD64" ;;
    IBM64)  SYSCFLAGS="-q64"
            SYSFFLAGS="-q64"
            AR="ar -X64 cr"
            test $debug = no && STRIP="strip -X64" ;;
    SGI6)   SYSCFLAGS="-n32"
            SYSFFLAGS="-n32" ;;
    SGI64)  SYSCFLAGS="-64"
            SYSFFLAGS="-64" ;;
    SUN64)  SYSCFLAGS="-xarch=v9"
            SYSFFLAGS="-xarch=v9" ;;
    DARWIN) CFLAGS="$CFLAGS -I/usr/include/sys" ;;
  esac
else
  if test "$is64bit" = "yes"; then
    gcc64bit=`$CC -dumpmachine | grep 64`
    if test -z "$gcc64bit"; then
      case $SYSTEM in
        HPPA64)  SYSCFLAGS="#-march=2.0" ;;
        HPIT64)  SYSCFLAGS="#-m64" ;;
        IBM64)   SYSCFLAGS="-maix64" ;;
        LINUX64) SYSCFLAGS="-m64" ;;
        SGI64)   SYSCFLAGS="-mabi=64" ;;
        SUN64)   SYSCFLAGS="-mcpu=v9 -m64" ;;
      esac
    fi
  else
    case $SYSTEM in
      SGI6) SYSCFLAGS="-mabi=n32" ;;
    esac
  fi
fi

# see if we can build a 64-bit code

if test "$is64bit" = "yes"; then
  CFLAGS_SAVE=$CFLAGS
  CFLAGS="$CFLAGS $SYSCFLAGS"
  AC_CHECK_SIZEOF([char *], 4)
  test $ac_cv_sizeof_char_p -lt 8 && \
    AC_MSG_WARN(does not appear your compiler can build 64-bit code)
  CFLAGS=$CFLAGS_SAVE
fi

#--------------------------------------------------------------------------
# check if legacy build is requested
#--------------------------------------------------------------------------

BUILDLEGACY=0

AC_MSG_CHECKING([if legacy build is enabled])
AC_ARG_ENABLE(legacy,
  [  --enable-legacy         build legacy code [default=no]],,
  enableval=no)
if test ! "$enableval" = "no" ; then
  enableval=yes
  BUILDLEGACY=1
fi
AC_MSG_RESULT($enableval)

#--------------------------------------------------------------------------
# check if enum scoping is requested
#--------------------------------------------------------------------------

BUILDSCOPE=0

AC_MSG_CHECKING([if scope enumeration values is enabled])
AC_ARG_ENABLE(scope,
  [  --enable-scope          scope enumerations values [default=no]],,
  enableval=no)
if test ! "$enableval" = "no" ; then
  enableval=yes
  BUILDSCOPE=1
fi
AC_MSG_RESULT($enableval)

#--------------------------------------------------------------------------
# check if base scope for family/zone ref-to requested
#--------------------------------------------------------------------------

BUILDBASESCOPE=0

AC_MSG_CHECKING([if base scope for family/zone ref-to enabled])
AC_ARG_ENABLE(basescope,
  [  --enable-basescope          base scope for family/zone ref-to [default=no]],,
  enableval=no)
if test ! "$enableval" = "no" ; then
  enableval=yes
  BUILDBASESCOPE=1
fi
AC_MSG_RESULT($enableval)

#--------------------------------------------------------------------------
# get a usable 64-bit type
#--------------------------------------------------------------------------

CGLONGT=long
CFLAGS_SAVE=$CFLAGS
CFLAGS="$CFLAGS $SYSCFLAGS"
AC_CHECK_SIZEOF(long, 0)
if test $ac_cv_sizeof_long -lt 8 ; then
  AC_CHECK_SIZEOF(long long, 0)
  if test $ac_cv_sizeof_long_long -lt 8 ; then
    AC_MSG_WARN(can't find a 64-bit integer type - assuming long)
  else
    CGLONGT="long long"
  fi
fi
CFLAGS=$CFLAGS_SAVE

#--------------------------------------------------------------------------
# set the Fortran interface
#--------------------------------------------------------------------------

FORTRAN_DEFAULT_INTEGER_C_INT64_T=0
HAVE_FORTRAN_95=0
HAVE_FORTRAN_2003=0
HAVE_FORTRAN_2008=0
HAVE_FORTRAN_2008TS=0
BUILDFORTRAN=0

AC_MSG_CHECKING([if Fortran interface is enabled])
AC_ARG_WITH(fortran,
  [  --with-fortran[=F2C]    enable Fortran interface [default=yes]],
  withfortran=$withval,withfortran=yes)
test -z "$withfortran" && withfortran=yes
AC_MSG_RESULT($withfortran)

if test $withfortran = no; then
  fortran=no
  F2CFLAGS=
else
  BUILDFORTRAN=1
  fortran=yes
  if test $withfortran = yes; then
    need_f2c=yes
  else
    case $withfortran in
      LOWERCASE | LOWERCASE_ | LOWERCASE__) ;;
      UPPERCASE | UPPERCASE_ | UPPERCASE__) ;;
      *) AC_MSG_ERROR([unknown Fortran to C interface $withfortran]) ;;
    esac
    need_f2c=no
    F2CFLAGS=-D$withfortran
  fi

  # Check if the Fortran compiler is F2003 compliant
  ## --------------------------------------------------------------------
  ## Default for FORTRAN 2003 compliant compilers
  ##
  HAVE_F2003_REQUIREMENTS="no"
  AC_MSG_CHECKING([if Fortran compiler meets 2003 standard])

  AC_LANG_PUSH(Fortran)
  AC_COMPILE_IFELSE([AC_LANG_SOURCE([
      PROGRAM testf03
  	USE iso_c_binding
  	IMPLICIT NONE
  	INTEGER(C_INT) :: a
  	INTEGER(C_INT32_T) :: b
  	INTEGER(C_INT64_T) :: c
        REAL(C_FLOAT) :: d
  	REAL(C_DOUBLE) :: e
  	CHARACTER(LEN=1,KIND=C_CHAR), TARGET :: f
  	TYPE(C_PTR) :: g
        INTERFACE
         INTEGER(C_INT) FUNCTION test(A,UserData) BIND(C,name='cg_goto')
           USE ISO_C_BINDING
           INTEGER(C_INT)   , INTENT(IN), VALUE :: A
           CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: UserData
         END FUNCTION test
        END INTERFACE
  	g = C_LOC(f(1:1))
      END PROGRAM testf03
  ])],[  AC_MSG_RESULT([yes])
         HAVE_F2003_REQUIREMENTS="yes"
         HAVE_FORTRAN_2003=1],
      [AC_MSG_RESULT([no])])
  AC_LANG_POP(Fortran)

 ## In F2003 and F2008 optional parameters are not allowed for functions that have BIND(C).
 ## In Fortran 2003 it is constraint C530, in the latest Fortran 2008 draft it is C516:
 ## "The ALLOCATABLE, POINTER, or OPTIONAL attribute shall not be specified
 ## for a dummy argument of a procedure that has a proc-language-binding-spec."

 ## However this is being addressed in:
 ## TS 29113 Further Interoperability of Fortran with C WG5/N1942
 ## Compilers status:
 ##  IMPLEMENTED      -- gnu (4.7,4.8,4.9), intel, portland group
 ##                      xlf 15.1.0

  HAVE_F2008TS_REQUIREMENTS="no"
  if test "$HAVE_F2003_REQUIREMENTS" = "yes"; then

    AC_MSG_CHECKING([if Fortran compiler meets Technical Specification 29113])

    AC_LANG_PUSH(Fortran)
    AC_COMPILE_IFELSE([AC_LANG_SOURCE([
        PROGRAM testf03
         USE ISO_C_BINDING
         INTERFACE
            INTEGER(C_INT) FUNCTION test(A) BIND(C, name='test')
              USE ISO_C_BINDING
              INTEGER(C_INT), OPTIONAL :: A
            END FUNCTION test
         END INTERFACE
       END PROGRAM testf03
    ])],[  AC_MSG_RESULT([yes])
     	 HAVE_F2008TS_REQUIREMENTS="yes"
         HAVE_FORTRAN_2008TS=1],
      [AC_MSG_RESULT([no])])

    AC_LANG_POP(Fortran)

  ## Fortran 2008 permits having different identifiers for the same binding label,
  ## so check if the compiler has this feature.
    HAVE_F2008_REQUIREMENTS="no"
    if test "$HAVE_F2003_REQUIREMENTS" = "yes"; then

      AC_MSG_CHECKING([if Fortran compiler meets the Fortran 2008 Standard])

      AC_LANG_PUSH(Fortran)
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([
       MODULE test
         USE ISO_C_BINDING
         INTERFACE binding
           FUNCTION bind1(x) BIND(C, name='Cfunc')
	     IMPORT :: C_FLOAT
             REAL(C_FLOAT) :: x
	   END FUNCTION bind1
           FUNCTION bind2(x) BIND(C, name='Cfunc')
             IMPORT :: C_DOUBLE
	     REAL(C_DOUBLE) :: x
	   END FUNCTION bind2
         END INTERFACE
      END MODULE test
      ])],[  AC_MSG_RESULT([yes])
         HAVE_F2008_REQUIREMENTS="yes"
         HAVE_FORTRAN_2008=1],
      [AC_MSG_RESULT([no])])

      AC_LANG_POP(Fortran)
    fi
  
  else

  # Check if the Fortran compiler is at least F95 compliant
  ## --------------------------------------------------------------------
  ## Default for FORTRAN 95 compliant compilers
  ##
    HAVE_F95_REQUIREMENTS="no"
    AC_MSG_CHECKING([if Fortran compiler meets 95 standard])
    AC_LANG_PUSH(Fortran)
    AC_COMPILE_IFELSE([AC_LANG_SOURCE([
       PROGRAM testf95
  	 IMPLICIT NONE
         INTEGER, PARAMETER :: cgint_kind = SELECTED_INT_KIND(5)
      END PROGRAM testf95
    ])],[  AC_MSG_RESULT([yes])
           HAVE_F95_REQUIREMENTS="yes"
           HAVE_FORTRAN_95=1],
      [AC_MSG_RESULT([no])])

    AC_LANG_POP(Fortran)

  fi

  # Check the size of the default fortran integer
  ## --------------------------------------------------------------------

  if test "$HAVE_F2003_REQUIREMENTS" = "yes"; then

    AC_MSG_CHECKING([size of default Fortran integer ])

    AC_LANG_PUSH(Fortran)
    AC_COMPILE_IFELSE([AC_LANG_SOURCE([
      MODULE mod

        INTERFACE test_integer
           MODULE PROCEDURE test_int
           MODULE PROCEDURE test_c_int64
        END INTERFACE

      CONTAINS

        SUBROUTINE test_c_int64(a)
          USE ISO_C_BINDING
          INTEGER(C_INT64_T) :: a
        END SUBROUTINE test_c_int64

        SUBROUTINE test_int(a)
          USE ISO_C_BINDING
          INTEGER :: a
        END SUBROUTINE test_int

      END MODULE mod

      PROGRAM main

        USE mod
        INTEGER :: a

        CALL test_integer(a)

      END PROGRAM main
    ])],[ AC_MSG_RESULT(c_int32_t)],
        [ AC_MSG_RESULT(c_int64_t)
	FORTRAN_DEFAULT_INTEGER_C_INT64_T=1])
    AC_LANG_POP(Fortran)
  fi

  if test $need_f2c = yes; then
    if test "$ac_cv_prog_g77" = "yes"; then
      if test "$ac_cv_prog_gcc" = "no"; then
        SYSFFLAGS=-fno-second-underscore
      else
        SYSFFLAGS="-fno-second-underscore $SYSCFLAGS"
      fi
      F2CFLAGS="-DLOWERCASE_"
      need_f2c=no
    else
      if test "$ac_cv_prog_gcc" = "no"; then
        case $SYSTEM in
          CRAY | VAX | WIN32) need_f2c=no ;;
        esac
      fi
    fi
  fi

  if test $need_f2c = yes; then
    f2c=
    AC_MSG_CHECKING([Fortran to C interface])
    if test $has_f77 = yes; then
      LIBS_SAVE="$LIBS"

      AC_LANG_PUSH(C)
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([
       void ffunc(){}
      ])],[mv conftest.$OBJEXT conffunc.$OBJEXT],[])

      LIBS="conffunc.$OBJEXT $LIBS_SAVE"
      AC_LANG_SAVE

      AC_LANG_PUSH(Fortran)
      for func in ffunc_ ffunc FFUNC FFUNC_ ffunc__ FFUNC__; do

	AC_LINK_IFELSE([AC_LANG_SOURCE([
           PROGRAM main
             func=$func()
           END PROGRAM
        ])],[linked="yes"],[])

        if test "$linked" = "yes"; then
          case $func in
            ffunc)   f2c=LOWERCASE ;;
            ffunc_)  f2c=LOWERCASE_ ;;
            ffunc__) f2c=LOWERCASE__ ;;
            FFUNC)   f2c=UPPERCASE ;;
            FFUNC_)  f2c=UPPERCASE_ ;;
            FFUNC__) f2c=UPPERCASE__ ;;
          esac
          break
        fi
      done
      rm -f conffunc.$OBJEXT
      LIBS="$LIBS_SAVE"
      AC_LANG_RESTORE
    fi
    if test -z "$f2c"; then
      f2c=LOWERCASE_
      case $SYSTEM in
        APOLLO)   f2c=LOWERCASE ;;
        BSDM68K)  f2c=UPPERCASE ;;
        BSDMIPS)  f2c=UPPERCASE ;;
        HP*)      f2c=LOWERCASE ;;
        IBM)      f2c=LOWERCASE ;;
        MACOSX)   f2c=LOWERCASE ;;
        M88K)     f2c=UPPERCASE ;;
        NEXT)     f2c=LOWERCASE ;;
      esac
      AC_MSG_RESULT([assuming $f2c])
    else
      AC_MSG_RESULT($f2c)
    fi
    F2CFLAGS="-D$f2c"
  fi

  # check for catenization operator

  AC_MSG_CHECKING([for concatenation operator])
  AC_LANG_C

  AC_EGREP_CPP(function,
  [#define CONCAT(a,b) a##b
   CONCAT(func,tion)
  ],concat=yes,concat=no)

  # above doesn't work on SGI for some reason,
  # while the following does

  if test $concat = no; then
    AC_TRY_LINK(
      [#define CONCAT(a,b) a##b
       void function(){}],
      [CONCAT(func,tion)()],concat=yes,concat=no)
  fi

  AC_MSG_RESULT($concat)
  test $concat = no && F2CFLAGS="$F2CFLAGS -DNO_CONCATENATION"
fi

#--------------------------------------------------------------------------
# check if building shared library - currently very limited support
#--------------------------------------------------------------------------

AR_LIB=$AR
RAN_LIB=$RANLIB
EXT_LIB=$LIBEXT

AC_MSG_CHECKING([if building a shared library])
AC_ARG_ENABLE(shared,
    [  --enable-shared[=all]   build a shared library [default=no]],,
    enableval=no)
test ! "$enableval" = "no" && \
test ! "$enableval" = "all" && \
  enableval=yes
shared=$enableval
AC_MSG_RESULT($shared)

if test $shared = all; then
  exts="so sl a"
  shared=yes
else
  exts="a so sl"
fi
if test $shared = yes; then
  cgnsdir=`pwd`
  if test "$ac_cv_prog_gcc" = "no"; then
    case $SYSTEM in
      HP*)   CFGFLAGS="+z $CFGFLAGS"
             AR_LIB="ld -b -E +s +b $LIBDIR:$cgnsdir/$BUILDDIR:. -o"
             EXT_LIB=sl ;;
      SGI*)  AR_LIB="\$(CC) -shared $SYSCFLAGS -Wl,-rpath,$cgnsdir/$BUILDDIR -o"
             EXT_LIB=so ;;
      ALPHA) AR_LIB="\$(CC) -shared $SYSCFLAGS -o"
             EXT_LIB=so ;;
      SUN*)  CFGFLAGS="-KPIC $CFGFLAGS"
             AR_LIB="\$(CC) -G $SYSCFLAGS -Wl,-R,$LIBDIR -Wl,-R,$cgnsdir/$BUILDDIR -o"
             EXT_LIB=so ;;
      *) AC_WARN([don't know how to build shared lib for $SYSTEM])
         shared=no ;;
    esac
  else
    if test "$ac_cv_cygwin" = "yes"; then
      AC_WARN([can't yet build shared library under CYGWIN])
      shared=no
    else
      CFGFLAGS="-fPIC $CFGFLAGS"
      AR_LIB="\$(CC) -shared $SYSCFLAGS -Wl,-rpath,$LIBDIR:$cgnsdir/$BUILDDIR -o"
      EXT_LIB=so
    fi
  fi
  if test $shared = yes; then
    if test $STRIP = "strip"; then
      case $SYSTEM in
        DARWIN) STRIP="$STRIP -x" ;;
      esac
    fi
    RAN_LIB="\$(STRIP)"
  fi
fi

#---------------------------------------------------------------------------
# locate HDF5 includes and library if using HDF5
#---------------------------------------------------------------------------

BUILDHDF5=0
HDF5DEF=""
HDF5INC=""
HDF5LIB=""
HDF5BIN=""

AC_MSG_CHECKING([if using HDF5])
AC_ARG_WITH(hdf5,
  [  --with-hdf5[=DIR]       use HDF5 [default=no]],
  withhdf5=$withval,withhdf5=no)
if test "$withhdf5" = "no"; then
  AC_MSG_RESULT(no)
else
  BUILDHDF5=1
  if test -z "$withhdf5" || test "$withhdf5" = "yes"; then
    hdf5dir=""
    AC_MSG_RESULT(yes)
    AC_CHECK_HEADER(hdf5.h,installed=yes,installed=no)
    if test "$installed" = "yes"; then
      HDF5LIB=-lhdf5
    else
      AC_MSG_CHECKING([for local HDF5 distribution])
      for d in \
        `ls -dr * 2>/dev/null` \
        `ls -dr ../* 2>/dev/null` \
        `ls -dr ../../* 2>/dev/null` \
        `ls -dr ../../../* 2>/dev/null` ; do
        if test -f "$d/include/hdf5.h" || test -f "$d/src/hdf5.h"; then
          hdf5dir=`(cd $d; pwd)`
          AC_MSG_RESULT($hdf5dir)
          break
        fi
      done
      test -z "$hdf5dir" && AC_MSG_RESULT(not found)
    fi
  else
    hdf5dir=$withhdf5
    withhdf5=yes
    AC_MSG_RESULT($hdf5dir)
  fi

  if test -z "$HDF5LIB"; then
    test -z "$hdf5dir" && \
      AC_MSG_ERROR(need specify the HDF5 directory with --with-hdf5=DIR)
    if test -f "$hdf5dir/include/hdf5.h"; then
      HDF5INC=$hdf5dir/include
      for a in $exts ; do
	if test -f $hdf5dir/lib64/libhdf5.$a; then
          HDF5LIB=$hdf5dir/lib64/libhdf5.$a
          break
        fi
        if test -f $hdf5dir/lib/libhdf5.$a; then
          HDF5LIB=$hdf5dir/lib/libhdf5.$a
          break
        fi
      done
      test -z "$HDF5LIB" && (HDF5LIB=$hdf5dir/lib/libhdf5.a || HDF5LIB=$hdf5dir/lib64/libhdf5.a)
    else
      if test -f "$hdf5dir/src/hdf5.h"; then
        HDF5INC=$hdf5dir/src
        for a in $exts ; do
          if test -f $hdf5dir/src/.libs/libhdf5.$a; then
            HDF5LIB=$hdf5dir/src/.libs/libhdf5.$a
            break
          fi
        done
        test -z "$HDF5LIB" && HDF5LIB=$hdf5dir/src/.libs/libhdf5.a
      else
        AC_MSG_ERROR($hdf5dir directory does not contain include/hdf5.h or src/hdf5.h)
      fi
    fi
  fi
  HDF5DEF=-I$HDF5INC
  HDF5BIN=$hdf5dir/bin

  if test -z "$HDF5BIN"; then
     AC_MSG_ERROR(failed to find the HDF5 bin directory $HDF5BIN)    
  fi

  # Checking version of HDF5 (req. minimum 1.8) 
  ## --------------------------------------------------------------------
  AC_MSG_CHECKING([HDF5 library version])
  H5_VERS_MAJOR=`grep "#define H5_VERS_MAJOR" $HDF5INC/H5public.h | sed "s/H5_VERS_MAJOR//g" | sed "s/[^0-9]*//g"`
  H5_VERS_MINOR=`grep "#define H5_VERS_MINOR" $HDF5INC/H5public.h | sed "s/H5_VERS_MINOR//g" | sed "s/[^0-9]*//g"`
  H5_VERS_RELEASE=`grep "#define H5_VERS_RELEASE" $HDF5INC/H5public.h | sed "s/H5_VERS_RELEASE//g" | sed "s/[^0-9]*//g"`
  AC_MSG_RESULT(${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE})

  AC_MSG_CHECKING([if HDF5 is 1.8 or greater])
  if test ${H5_VERS_MAJOR} -ge 1 && test ${H5_VERS_MINOR} -ge 8; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_ERROR(no)
  fi

  # Check if HDF5 library has multi-dataset read and write APIs
  ## --------------------------------------------------------------------

  AC_LANG_C
  HDF5_HAVE_MULTI_DATASETS=0
  AC_MSG_CHECKING([if HDF5 library has multi-dataset read/write APIs])

  cat >> test.c << EOF
#include "hdf5.h"
void main(void)
{
  H5D_rw_multi_t *multi_info;
  return;
}
EOF
  msg=`${CC} -c test.c ${HDF5DEF} 2> /dev/null`
  if test -f test.o; then
    HDF5_HAVE_MULTI_DATASETS=1
    AC_MSG_RESULT(yes)
    rm test.o
  else
    AC_MSG_RESULT(no)
  fi
  if test -f test.c; then
     rm test.c
  fi

  # Check if HDF5 library has collective metadata APIs, HDF5 >= 1.10.0
  ## --------------------------------------------------------------------

  HDF5_HAVE_COLL_METADATA=0
  AC_MSG_CHECKING([if HDF5 library has collective metadata APIs])
  chck_coll=`grep -i '_coll_metadata_' $HDF5INC/H5Ppublic.h`
  if test "x$chck_coll" = "x"; then
    AC_MSG_RESULT(no)
  else
    HDF5_HAVE_COLL_METADATA=1
    AC_MSG_RESULT(yes)
  fi

  # Check if HDF5 library has H5Pset_file_space_strategy HDF5 >= 1.10.1
  ## --------------------------------------------------------------------

  HDF5_HAVE_FILE_SPACE_STRATEGY=0
  AC_MSG_CHECKING([if HDF5 library has H5Pset_file_space_strategy])
  chck_fs=`grep -i 'H5Pset_file_space_strategy' $HDF5INC/H5Ppublic.h`
  if test "x$chck_fs" = "x"; then
    AC_MSG_RESULT(no)
  else
    HDF5_HAVE_FILE_SPACE_STRATEGY=1
    AC_MSG_RESULT(yes)
  fi      


fi

#---------------------------------------------------------------------------
# locate zlib and szip libraries
#---------------------------------------------------------------------------

# Automatically search for zlib and szip from HDF5 build specs, but
# only do this if --with-zlib and --with-szip is not specified.

H5NEEDZLIB=0
ZLIBLIB=""
H5NEEDSZIP=0
SZIPLIB=""
HDF5LIB_DEP=""

## ----------------------------------------------------------------------
## Fake --with-xxx option to allow us to create a help message for the
## following --with-xxx options which can take either a =DIR or =INC,LIB
## specifier.
##
AC_ARG_WITH([fnord],
  [
 For the --with-zlib option, you can specify where the header
 files and libraries are in two different ways:

    --with-xxx=INC,LIB - Specify individually the include directory and
                         library directory separated by a comma
    --with-xxx=DIR     - Specify only the directory which contains the
                         include/ and lib/ subdirectories

  IMPORTANT NOTE: CGNS will automatically search for zlib and szip used in
        building HDF5 only if options --with-zlib and --with-szip are not
        specified. 

  ])

if test "$withhdf5" = "yes"; then

  AC_ARG_WITH([zlib],,[withval_zlib=yes],[withval_zlib=no])
  AC_ARG_WITH([szip],,[withval_szip=yes],[withval_szip=no])

  if test $withval_zlib == "no" && test $withval_szip == "no"; then

  AC_MSG_CHECKING([if HDF5 library has compression dependencies])      

  H5CC=$HDF5BIN/h5*cc
  H5BLD_CPPFLAGS=`cat $H5CC | grep "H5BLD_CPPFLAGS=" | sed -e 's/.*H5BLD_CPPFLAGS="*\(.*\) ".*/\1/'` 
  H5BLD_LDFLAGS=`cat $H5CC | grep "H5BLD_LDFLAGS=" | sed -e 's/.*H5BLD_LDFLAGS="*\(.*\) ".*/\1/'` 
  H5BLD_LIBS=`cat $H5CC | grep "H5BLD_LIBS=" | sed -e 's/.*H5BLD_LIBS="*\(.*\) ".*/\1/'`

  LDFLAGS="$LDFLAGS $H5BLD_LDFLAGS"
  CPPFLAGS="$CPPFLAGS $H5BLD_CPPFLAGS"

# check if linking in szip or zlib
# HDF5 does not add LDFLAGS for libraries in standard locations, so process the LIBS being
# linked in.
  LZLIB=""
  LSZIP=""
  LNUMLIBS=`echo $H5BLD_LIBS | awk '{print NF}'`
  if test $LNUMLIBS -gt 0; then
    for i in `seq 1 $LNUMLIBS` ; do
       LIBX=`echo $H5BLD_LIBS | cut -d' ' -f$i`
       if test `echo $LIBX | grep "\-lz" > /dev/null; echo $?` == 0; then
         LZLIB="-lz"
       fi 
       if test `echo $LIBX | grep "\-lsz" > /dev/null; echo $?` == 0; then
         LSZIP="-lsz"
       else
         if test `echo $LIBX | grep "\-lszip" > /dev/null; echo $?` == 0; then
           LSZIP="-lszip"
         fi
       fi
    done
  fi

  if test $shared = yes; then
     LDFLAGS="$LDFLAGS $H5BLD_LDFLAGS"
     HDF5LIB_DEP="$LDFLAGS"  
     LIBS="$LIBS $LZLIB $LSZIP"
     FLIBS="$FLIBS $LZLIB $LSZIP"
  else
     H5BLD_LDFLAGS_PATH=`echo $H5BLD_LDFLAGS | sed 's/-L//g'`       
     NUMLIBS=`echo $H5BLD_LDFLAGS_PATH | awk '{print NF}'`

     HDF5LIB_DEP=""
     FOUNDZLIB="false"
     FOUNDSZIP="false"
     for i in `seq 1 $NUMLIBS` ; do
       LIB_PATH=`echo $H5BLD_LDFLAGS_PATH | cut -d' ' -f$i`
       for a in $exts ; do
          if test -f $LIB_PATH/libz.$a; then
            HDF5LIB_DEP="$LIB_PATH/libz.$a $HDF5LIB_DEP"
            FOUNDZLIB="true"
            break
          fi
          if test -f $LIB_PATH/libsz.$a; then
            HDF5LIB_DEP="$LIB_PATH/libsz.$a $HDF5LIB_DEP"
            FOUNDSZIP="true"
            break
          fi
          if test -f $LIB_PATH/libszip.$a; then
            HDF5LIB_DEP="$LIB_PATH/libszip.$a $HDF5LIB_DEP"
            FOUNDSZIP="true"
            break
          fi
       done
     done

     if test "x$LSZIP" != "x" && test "$FOUNDSZIP" == "false";then
       LIBS="$LIBS $LSZIP"
       FLIBS="$FLIBS $LSZIP"
     fi
     if test "x$LZLIB" != "x" && test "$FOUNDZLIB" == "false";then
       LIBS="$LIBS $LZLIB"
       FLIBS="$FLIBS $LZLIB"
     fi
  fi

  if test "x$HDF5LIB_DEP" = "x"; then
    AC_MSG_RESULT(no)
  else
    AC_MSG_RESULT($HDF5LIB_DEP)
  fi

  AC_MSG_CHECKING([HDF5 library LIB dependencies])
  if test "x$LIBS" = "x"; then
    AC_MSG_RESULT(none)
  else
    AC_MSG_RESULT($LIBS)
  fi      

  else

  AC_ARG_WITH([zlib],
               [AS_HELP_STRING([--with-zlib=DIR],
                              [Use zlib library for external deflate I/O
                               filter [default=no]])],,
               [withval=no])

  if test "x$withval" != xno; then
    # Was HDF5 built with zlib ?
    AC_LANG_PUSH(C)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include "H5pubconf.h"],
    [[#if !H5_HAVE_ZLIB_H
    # error
    #endif]
    ])], [], [AC_MSG_WARN([zlib requested but hdf5 does not seems to be built with it.])])
  fi

  case $withval in
    yes)
      H5NEEDZLIB=1
      HAVE_ZLIB="yes"
      AC_CHECK_LIB([z], [compress2],, [unset HAVE_ZLIB])
      if test -z "$HAVE_ZLIB"; then
        AC_MSG_ERROR([couldn't find zlib library: specify the zlib directory with --with-zlib=DIR])
      fi
      ZLIBLIB=-lz
      ;;
    no)
      HAVE_ZLIB="no"
      AC_MSG_CHECKING([if using zlib with HDF5])
      AC_MSG_RESULT([no])
      ;;
    *)
      H5NEEDZLIB=1
      HAVE_ZLIB="yes"
 
      case "$withval" in
        *,*)
          zlib_inc="`echo $withval | cut -f1 -d,`"
          zlib_lib="`echo $withval | cut -f2 -d, -s`"
          ;;
        *)
          if test -n "$withval"; then
            zlib_inc="$withval/include"
            zlib_lib="$withval/lib"
          fi
          ;;
      esac

    # Check if provided libdir exists
    AC_MSG_CHECKING(for $zlib_lib)
    if test -d "$zlib_lib"; then
        AC_MSG_RESULT(yes)
        HAVE_ZLIB="yes"
    else
        AC_MSG_RESULT(no)
        unset HAVE_ZLIB
    fi

    if test $shared = yes; then
      ZLIBLIB="-L$zlib_lib -lz"
    else
      if test -n "$zlib_lib"; then
        unset HAVE_ZLIB
        for a in $exts ; do
          if test -f $zlib_lib/libz.$a; then
            ZLIBLIB=$zlib_lib/libz.$a
            HAVE_ZLIB="yes"
            break
          fi
        done
      fi
    fi

    if test -z "$HAVE_ZLIB"; then
      AC_MSG_ERROR([couldn't find zlib library])
    fi
    AC_MSG_CHECKING([for zlib library path])	
    AC_MSG_RESULT([$zlib_lib])
    ;;
  esac
	
  AC_ARG_WITH([szip],
               [AS_HELP_STRING([--with-szip=DIR],
                              [Use szip library for external deflate I/O
                               filter [default=no]])],,
               [withval=no])

  case $withval in
    yes)
      H5NEEDSZIP=1
      HAVE_SZIP="yes"
      AC_CHECK_LIB([sz], [SZ_BufftoBuffCompress],, [unset HAVE_SZIP])
      if test -z "$HAVE_SZIP"; then
        AC_MSG_ERROR([couldn't find szip library: specify the szip directory with --with-szip=DIR])
      fi
      SZIPLIB=-lsz
      ;;
    no)
      HAVE_SZIP="no"
      AC_MSG_CHECKING([if using szip with HDF5])
      AC_MSG_RESULT([no])
      ;;
    *)
      H5NEEDSZIP=1
      HAVE_SZIP="yes"
 
      case "$withval" in
        *,*)
          szip_inc="`echo $withval | cut -f1 -d,`"
          szip_lib="`echo $withval | cut -f2 -d, -s`"
          ;;
        *)
          if test -n "$withval"; then
            szip_inc="$withval/include"
            szip_lib="$withval/lib"
          fi
          ;;
      esac

    # Check if provided libdir exists
    AC_MSG_CHECKING(for $szip_lib)
    if test -d "$szip_lib"; then
        AC_MSG_RESULT(yes)
        HAVE_SZIP="yes"
    else
        AC_MSG_RESULT(no)
        unset HAVE_SZIP
    fi

    if test $shared = yes; then
      SZIPLIB="-L$szip_lib -lsz"
    else
      if test -n "$szip_lib"; then
        unset HAVE_SZIP
        for a in $exts ; do
          if test -f $szip_lib/libsz.$a; then
            SZIPLIB=$szip_lib/libsz.$a
            HAVE_SZIP="yes"
            break
          fi
        done
      fi
    fi

    if test -z "$HAVE_SZIP"; then
      AC_MSG_ERROR([couldn't find szip library])
    fi
    AC_MSG_CHECKING([for szip library path])
    AC_MSG_RESULT([$szip_lib])
    ;;
  esac

  fi

fi

#---------------------------------------------------------------------------
# does HDF5 require MPI ?
#---------------------------------------------------------------------------
withmpi=no

H5NEEDMPI=0
MPIDEF=""
MPIINC=""
MPIOPTS=""
MPILIBS=""
MPICC=""

#if test "$withhdf5" = "yes"; then
#  AC_MSG_CHECKING([if using MPI])
#  AC_ARG_WITH(mpi,
#    [  --with-mpi[=DIR/mpicc]  use MPI with HDF5 [default=no]],
#    withmpi=$withval,withmpi="no")
#  if test "x$withmpi" = "xno"; then
#    AC_MSG_RESULT(no)
#  else
#    H5NEEDMPI=1
#    if test -z "$withmpi" || test "$withmpi" = "yes"; then
#      AC_MSG_RESULT(yes)
#      AC_PATH_PROG(MPICC, mpicc,)
#      if test "x$MPICC" = "x"; then
#        AC_MSG_CHECKING([for local mpicc executable])
#        for d in \
#          `ls -dr * 2>/dev/null` \
#          `ls -dr ../* 2>/dev/null` \
#          `ls -dr ../../* 2>/dev/null` \
#          `ls -dr ../../../* 2>/dev/null` ; do
#          if test -x "$d/mpicc"; then
#            MPICC=`(cd $d; pwd)`/mpicc
#            break
#          fi
#          if test -x "$d/bin/mpicc"; then
#            MPICC=`(cd $d; pwd)`/bin/mpicc
#            break
#          fi
#        done
#      fi
#    else
#      mpidir=$withmpi
#      test ! -d $withmpi && mpidir=`dirname $withmpi`
#      if test -x "$mpidir/mpicc"; then
#        MPICC=`(cd $mpidir; pwd)`/mpicc
#      fi
#      if test -x "$mpidir/bin/mpicc"; then
#        MPICC=`(cd $mpidir; pwd)`/bin/mpicc
#      fi
#    fi
#
#    if test "x$MPICC" = "x"; then
#      AC_MSG_ERROR(mpicc not found)
#    else
#      AC_MSG_RESULT($MPICC)
#      MPIEXEC=`echo $MPICC | sed "s/mpicc/mpiexec/"`
#      mpibin=`dirname $MPICC`
#      mpidir=`dirname $mpibin`
#      if test -d "$mpidir/src/include" ; then
#        MPIINC=$mpidir/src/include
#      else
#        MPIINC=$mpidir/include
#      fi
#      MPIOPTS=`grep "^CFLAGS=" $MPICC | sed s/CFLAGS=// | sed s/\"//g`
#      mpilib=`grep "^MPILIBNAME=" $MPICC | sed s/MPILIBNAME=// | sed s/\"//g`
#      if test "x$mpilib" = "x"; then
#        MPILIBS="-L$mpidir/lib"
#      else
#        MPILIBS="-L$mpidir/lib -l$mpilib"
#      fi
#      plib=`grep "^NEEDSPLIB=" $MPICC | sed s/NEEDSPLIB=// | sed s/\"//g`
#      if test "x$plib" = "xyes"; then
#        pmpilib=`grep "^PMPILIBNAME=" $MPICC | sed s/PMPILIBNAME=// | sed s/\"//g`
#        if test "x$pmpilib" = "x"; then
#          MPILIBS="$MPILIBS"
#        else
#          MPILIBS="$MPILIBS -l$pmpilib"
#        fi
#      fi
#      extralibs=`grep "^LIBS=" $MPICC | sed s/LIBS=// | sed s/\"//g`
#      MPILIBS="$MPILIBS $extralibs"
#    fi
#    MPIDEF=-I$MPIINC
#    withmpi=yes
#  fi
#fi


#-----------------------------------------------------------------------
# build parallel code ?
#-----------------------------------------------------------------------

BUILDPARALLEL=0
parallel=no

AC_MSG_CHECKING([if parallel IO support is requested])
AC_ARG_ENABLE(parallel,
    [  --enable-parallel       enable parallel IO support [default=no]],,
    enableval="no")
  
if test ! "x$enableval" = "xno" && enableval=yes; then
  BUILDPARALLEL=1
  parallel=$enableval

  ## Set MPIEXEC if not set yet. Defaults to running on
  ## four processes.
  if test -z "$MPIEXEC"; then
    MPIEXEC="mpiexec -n \$\${NPROCS:=4}"
  fi

  ## There *must* be some way to run in parallel even if it's just the
  ## word `none'.
  if test -z "$MPIEXEC"; then
    AC_MSG_ERROR([no way to run a parallel program])
  fi

  ## If MPIEXEC is the word `none' then replace it with
  ## the empty string. This means that no launch commands were requested,
  ## so we will not use any launch commands.
  if test "X$MPIEXEC" = "Xnone"; then
    MPIEXEC=""
  fi

fi
AC_MSG_RESULT($parallel)

if test "$parallel" = "yes"; then
  ## Display what we found about running programs
  AC_MSG_CHECKING([prefix for running in parallel])
  AC_MSG_RESULT([$MPIEXEC])
fi

#--------------------------------------------------------------------------
# check if HDF5 supports parallel I/O if parallel is enabled in CGNS 
#--------------------------------------------------------------------------

if test "$withhdf5" = "yes"; then
  AC_MSG_CHECKING([for parallel HDF5 support])
  if (test "x$H5CC" != "x"); then dnl check if HDF5 was configured with parallel support
      PARALLEL_HDF5_NO="`$H5CC -showconfig | grep -i 'Parallel HDF5: no'`"
  fi
  if (test "x$PARALLEL_HDF5_NO" != "x"); then
      AC_MSG_RESULT(no)
      if test "$parallel" = "yes"; then
         AC_MSG_ERROR([HDF5 library does not include parallel support. Either disable CGNS parallel support or build HDF5 with parallel enabled.])
      fi
  else
      AC_MSG_RESULT(yes)
  fi

fi

#--------------------------------------------------------------------------
# check for libraries
#--------------------------------------------------------------------------

if test "$withhdf5" = "yes"; then
  AC_CHECK_LIB([m], [ceil])

  if test "`uname`" = "SunOS" -o "`uname -sr`" = "HP-UX B.11.00"; then
    dnl ...for Solaris
    AC_CHECK_LIB([nsl], [xdr_int])
  fi
fi

#--------------------------------------------------------------------------
# check for large file support (files > 2Gb)
#--------------------------------------------------------------------------

AC_MSG_CHECKING([if large file support is requested])
AC_ARG_ENABLE(lfs,
  [  --enable-lfs            enable large file support [default=no]],,
  enableval="no")
test ! "$enableval" = "no" && enableval=yes
AC_MSG_RESULT($enableval)
if test $enableval = yes; then
  CFLAGS_SAVE=$CFLAGS
  AC_MSG_CHECKING([for large file support options])
  LFS=`getconf LFS_CFLAGS 2>/dev/null`
  if test "$LFS" = ""; then
    AC_MSG_RESULT(none)
  else
    AC_MSG_RESULT($LFS)
  fi
  test "$LFS" = "undefined" && LFS=""
  CFLAGS="$CFLAGS $LFS"
  cat >>confdefs.h <<\EOF
#include <sys/types.h>
EOF
  AC_CHECK_SIZEOF(off_t, 4)
  if test $ac_cv_sizeof_off_t = 8; then
    has_lfs=yes
  else
    AC_MSG_CHECKING([for off64_t])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>]],[[off64_t n = 0;]])],
	[has_lfs=yes],[has_lfs=no])
##    AC_TRY_COMPILE([#include <sys/types.h>], [off64_t n = 0;],
##      has_lfs=yes, has_lfs=no)
    AC_MSG_RESULT($has_lfs)
    if test $has_lfs = yes; then
      AC_CHECK_FUNC(open64, LFS="-DHAS_OPEN64 $LFS")
      AC_CHECK_FUNC(lseek64, LFS="-DHAS_LSEEK64 $LFS")
    fi
  fi
  CFLAGS=$CFLAGS_SAVE
  test $has_lfs = yes && CFGFLAGS="$LFS $CFGFLAGS"
fi

  # Check for stat64 struct
  ## --------------------------------------------------------------------
  #  stat64 is deprecated for Mac OS X

HAVE_STAT64_STRUCT=0

if test ! "$SYSTEM" = "DARWIN"; then

  AC_MSG_CHECKING([for stat64])

  cat >> test.c << EOF
#include <sys/types.h>
#include <sys/stat.h>
struct stat64 sb;
int main()
{
  return 0;
}
EOF
  msg=`${CC} -c test.c &> /dev/null`
  if test -f test.o; then
    HAVE_STAT64_STRUCT=1
    AC_MSG_RESULT(yes)
    rm test.o
  else
    AC_MSG_RESULT(no)
  fi
  if test -f test.c; then
     rm test.c
  fi

fi

#---------------------------------------------------------------------------
# build CGNStools
#---------------------------------------------------------------------------

CGNSTOOLS=""
INSTALLCGNSTOOLS=""
X11INCS=""
X11LIBS=""
TKINCS=""
TKLIBS=""
OGLINCS=""
OGLLIBS=""
TKOGLXLIB=""
TKOGLOPTS=""
TCL_LIBRARY_SPEC=""
TK_LIBRARY_SPEC=""
LIBRARY_PATH_SPEC=""
PROGLIST="cgnsview cgnscalc utilities"
INSTLIST="install-cgnsview install-cgnscalc install-utilities"
PLOTOPTS=""
CALCOPTS=""

AC_MSG_CHECKING([if building CGNStools])
AC_ARG_ENABLE(cgnstools,
  [  --enable-cgnstools      build CGNStools [default=no]],,
  enableval=no)
test ! "$enableval" = "no" && enableval=yes
AC_MSG_RESULT($enableval)

if test "$enableval" = "yes" ; then
  CGNSTOOLS=cgnstools
  INSTALLCGNSTOOLS=install-cgnstools

#---------------------------------------------------------------------------
# get path to X11 headers and libraries
#---------------------------------------------------------------------------

  if test "$ac_cv_cygwin" = "no"; then
    AC_PATH_X
    if ! test "$x_includes" = ""; then
      X11INCS=-I$x_includes
    fi
    if ! test "$x_libraries" = ""; then
      X11LIBS="-L$x_libraries -lX11"
    else
      X11LIBS=-lX11
    fi
  fi

#---------------------------------------------------------------------------
# try to find the Tk configuration file, tkConfig.sh
#---------------------------------------------------------------------------

  AC_MSG_CHECKING([if Tcl directory was specified])
  AC_ARG_WITH(tcl,
    [  --with-tcl=TCLdir       directory containing Tcl distribution],
    tcldir=$withval,tcldir="")
  if test -z "$tcldir" || test "$tcldir" = "no"; then
    tcldir=""
    AC_MSG_RESULT(no)
  else
    AC_MSG_RESULT($tcldir)
  fi

  AC_MSG_CHECKING([if Tk directory was specified])
  AC_ARG_WITH(tk,
    [  --with-tk=TKdir         directory containing TK distribution],
    tkdir=$withval,tkdir="")
  if test -z "$tkdir" || test "$tkdir" = "no"; then
    tkdir=""
    AC_MSG_RESULT(no)
  else
    AC_MSG_RESULT($tkdir)
  fi

  TKCONFIG=""
  LIBRARY_PATH=""

  if test -z "$tcldir" && test -z "$tkdir"; then
    AC_MSG_CHECKING([for installed version of Tk])
    for d in /lib /usr/lib /usr/local/lib /usr/lib64 ; do
      if test -f $d/tkConfig.sh ; then
        AC_MSG_RESULT($d)
        TKCONFIG=$d/tkConfig.sh
        break
      fi
    done
    if test -z "$TKCONFIG"; then
      AC_MSG_RESULT(no)
      AC_MSG_CHECKING([for local Tk distribution])
      for d in \
        `ls -dr ../tk8* 2>/dev/null` \
        `ls -dr ../../tk8* 2>/dev/null` \
        `ls -dr ../../../tk8* 2>/dev/null` ; do
        if test -f "$d/tkConfig.sh"; then
          tkdir=`(cd $d; pwd)`
          break
        fi
        if test -f "$d/unix/tkConfig.sh"; then
          tkdir=`(cd $d; pwd)`
          break
        fi
      done
      if test -z "$tkdir"; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT($tkdir)
      fi
    else
      . $TKCONFIG
      if test -z "$TK_LIB_FLAG"; then
        if test -z "$TK_LIB_SPEC"; then
          TK_LIB_SPEC=$TK_BUILD_LIB_SPEC
        fi
        for i in $TK_LIB_SPEC ; do
          if test ! -z `echo $i | grep -e -ltk* -` ; then
            TK_LIB_FLAG=$i
            break
          fi
        done
      fi
      if test -z "$TK_LIB_FLAG"; then
        TK_LIB_FLAG=-ltk
      fi
      tcllib=`echo $TK_LIB_FLAG | sed 's/tk/tcl/'`
      X11LIBS=""
      TKLIBS="$tcllib $TK_LIB_FLAG $TK_LIBS"
    fi
  fi

  if test ! -z "$tcldir"; then
    if test ! -d "$tcldir"; then
      AC_MSG_ERROR(Tcl directory $tcldir not found)
    fi
    tcldir=`(cd $tcldir; pwd)`
    TCLCONFIG="$tcldir/tclConfig.sh"
    if test ! -f "$TCLCONFIG"; then
      TCLCONFIG="$tcldir/unix/tclConfig.sh"
      if test ! -f "$TCLCONFIG"; then
        AC_MSG_ERROR(tclConfig.sh not found in $tcldir or $tcldir/unix)
      fi
    fi
    if test -z "$tkdir"; then
      dir=`dirname $tcldir`
      for d in `ls -dr $dir/tk* 2>/dev/null` ; do
        if test -f "$d/tkConfig.sh"; then
          tkdir=$d
          break
        fi
        if test -f "$d/unix/tkConfig.sh"; then
          tkdir=$d
          break
        fi
      done
      if test -z "$tkdir"; then
        AC_MSG_ERROR(can't find a usable Tk directory)
      fi
    fi
  fi
  TKUNIX=""
  TCLUNIX=""
  if test ! -z "$tkdir"; then
    if test ! -d "$tkdir"; then
      AC_MSG_ERROR(Tk directory $tkdir not found)
    fi
    tkdir=`(cd $tkdir; pwd)`
    TKCONFIG="$tkdir/tkConfig.sh"	
    if test ! -f "$TKCONFIG"; then
      TKUNIX="unix"
      TKCONFIG="$tkdir/$TKUNIX/tkConfig.sh"
      if test ! -f "$TKCONFIG"; then
        AC_MSG_ERROR(tkConfig.sh not found in $tkdir or $tkdir/unix)
      fi
    fi
    if test -z "$tcldir"; then
      dir=`dirname $tkdir`
      for d in `ls -dr $dir/tcl* 2>/dev/null` ; do
        if test -f "$d/tclConfig.sh"; then
          tcldir=$d
          break
        fi
        if test -f "$d/unix/tclConfig.sh"; then
	  TCLUNIX="unix"
          tcldir=$d
          break
        fi
      done
      if test -z "$tcldir"; then
        AC_MSG_ERROR(can't find a usable Tcl directory)
      fi
      TCLCONFIG="$tcldir/$TCLUNIX/tclConfig.sh"
    fi
    . $TCLCONFIG
    . $TKCONFIG
    tcllib=`ls -d $tcldir/$TCLUNIX/libtcl8* 2>/dev/null`
    tklib=`ls -d $tkdir/$TKUNIX/libtk8* 2>/dev/null`
    X11LIBS=""
    if test -z "$TK_INCLUDE_SPEC" -a -z "$TCL_INCLUDE_SPEC"; then
      TKINCS="-I$tcldir/generic -I$tkdir/generic"
    else
      TKINCS="$TK_INCLUDE_SPEC $TCL_INCLUDE_SPEC"
    fi
    if test "$ac_cv_cygwin" = "yes"; then
      TKINCS="$TKINCS -I$tkdir/win -I$tkdir/xlib"
    fi
    if test -z "$TK_LIB_SPEC" -a -z "$TCL_LIB_SPEC"; then 
      TKLIBS="$tcllib $tklib $TK_LIBS"
      TCL_LIBRARY="$tcldir/library"
      TK_LIBRARY="$tkdir/library"
      LIBRARY_PATH="$tcldir/$TCLUNIX:$tkdir/$TKUNIX"
    else
      TKLIBS="$TK_LIB_SPEC $TCL_LIB_SPEC $TK_LIBS"
      TCL_LIBRARY="$tcldir/library"
      TK_LIBRARY="$tkdir/library"
      LIBRARY_PATH="$tcldir/$TCLUNIX:$tkdir/$TKUNIX"
    fi
  fi

  if test -z "$TKCONFIG"; then
    AC_MSG_WARN([couldn't find a tkConfig.sh])
    AC_MSG_WARN([edit make.defs and set TKINCS and TKLIBS])
    TKLIBS="-ltcl -ltk"
  else
    if test "$ac_cv_cygwin" = "yes"; then
      if test -z "$TKINCS"; then
        tkvers="$TK_MAJOR_VERSION$TK_MINOR_VERSION"
        if test -d "cygwin/tk$tkvers"; then
          TKINCS="-I../cygwin/tk$tkvers"
        else
          AC_MSG_WARN([edit make.defs and fix TKINCS])
          TKINCS="#-I../cygwin/tk80"
        fi
      fi
      sharepath=`cygpath -m /usr/share`
      if test -z "$TCL_LIBRARY"; then
        for d in `ls -dr $sharepath/tcl8* 2>/dev/null` ; do
          if test -f "$d/init.tcl"; then
            TCL_LIBRARY=$d
            break
          fi
        done
      fi
      if test -z "$TK_LIBRARY"; then
        for d in `ls -dr $sharepath/tk8* 2>/dev/null` ; do
          if test -f "$d/tk.tcl"; then
            TK_LIBRARY=$d
            break
          fi
        done
      fi
    fi
  fi

  if test -z "$TCL_LIBRARY"; then
    TCL_LIBRARY_SPEC="#export TCL_LIBRARY="
  else
    TCL_LIBRARY_SPEC="TCL_LIBRARY=$TCL_LIBRARY; export TCL_LIBRARY"
  fi

  if test -z "$TK_LIBRARY"; then
    TK_LIBRARY_SPEC="#export TK_LIBRARY="
  else
    TK_LIBRARY_SPEC="TK_LIBRARY=$TK_LIBRARY; export TK_LIBRARY"
  fi

  if test -z "$LIBRARY_PATH"; then
    LIBRARY_PATH_SPEC="#export LD_LIBRARY_PATH="
  elif test $SYSTEM = SGI6 ; then
    LIBRARY_PATH_SPEC="LD_LIBRARYN32_PATH=\$LD_LIBRARYN32_PATH:$LIBRARY_PATH; export LD_LIBRARYN32_PATH"
  else
    LIBRARY_PATH_SPEC="LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:$LIBRARY_PATH; export LD_LIBRARY_PATH"
  fi

#---------------------------------------------------------------------------
# try to find OpenGL headers and libraries
# should probably check for Mesa
#---------------------------------------------------------------------------

  AC_CHECK_HEADER(GL/gl.h)
  if test "$ac_cv_header_GL_gl_h" = "no"; then
    if test -f "$x_includes/GL/gl.h"; then
      PROGLIST="$PROGLIST cgnsplot"
      INSTLIST="$INSTLIST install-cgnsplot"
    else
      AC_MSG_WARN([couldn't find OpenGL])
      AC_MSG_WARN([edit make.defs to set OGLINCS and OGLLIBS])
    fi
  else
    PROGLIST="$PROGLIST cgnsplot"
    INSTLIST="$INSTLIST install-cgnsplot"
  fi

  if test "$ac_cv_cygwin" = "yes"; then
    OGLLIBS="-L/lib/win32api -lgdi32 -lopengl32 -lglu32"
  else
    OGLLIBS="-lGL -lGLU"
  fi

#---------------------------------------------------------------------------
# tkogl settings
#---------------------------------------------------------------------------

  if test "$ac_cv_cygwin" = "yes"; then
    TKOGLOPTS="-D_WIN32"
  else
    TKOGLXLIB="-lXmu -lXext"
  fi

#--------------------------------------------------------------------------
# get calc/cgnscalc options
#--------------------------------------------------------------------------

  CALCOPTS=-DREAD_NODE

  AC_CHECK_LIB(m, matherr)
  if test "$ac_cv_lib_m_matherr" = "yes"; then
    CALCOPTS="$CALCOPTS -DUSE_MATHERR"
  fi

  AC_TYPE_SIGNAL
  if test ! "$ac_cv_type_signal" = "void"; then
    CALCOPTS="$CALCOPTS -DSIGNAL=$ac_cv_type_signal"
  fi

#--------------------------------------------------------------------------
# cgnsplot options
#--------------------------------------------------------------------------

  AC_MSG_CHECKING([if cutting plane is requested])
  AC_ARG_ENABLE(cut,
    [  --enable-cut            build cgnsplot with cut plane [default=yes]],,
    enableval=yes)
  if test "$enableval" = "yes"; then
    AC_MSG_RESULT(yes)
  else
    PLOTOPTS="-DNO_CUTTING_PLANE $PLOTOPTS"
    AC_MSG_RESULT(no)
  fi

  AC_MSG_CHECKING([if mesh boundaries is requested])
  AC_ARG_ENABLE(mesh,
    [  --enable-mesh           build cgnsplot with structured mesh boundaries [default=yes]],,
    enableval=yes)
  if test "$enableval" = "yes"; then
    AC_MSG_RESULT(yes)
  else
    PLOTOPTS="-DNO_MESH_BOUNDARIES $PLOTOPTS"
    AC_MSG_RESULT(no)
  fi

fi

FC_D_PRE='' 
if test "$withfortran" = "yes"; then

#----------- Determine if the Fortran compiler xlf ------------------------
# xlf preprocessor needs the -WF,-D prefix, FC_D_PRE is used when passing -D 
# options to fortran programs.
#--------------------------------------------------------------------------
    fc_vendor="`$FC -qversion 2>&1 |grep 'IBM XL'`"
    if test X != "X$fc_vendor"; then
      FC_D_PRE='-WF,' 
    fi
#----------- Determine if the Fortran compiler pgi ------------------------
# The PGI compiler segfaults when va_start is called, so we need to add
# a special compiler flag, see CGNS-40. -Mx,125,0x200 is now called
# -Mvarargs
#--------------------------------------------------------------------------
    fc_vendor="`$FC --version 2>&1 |grep 'PGI'`"
    if test X != "X$fc_vendor"; then
      FFLAGS="$FFLAGS -Mx,125,0x200" 
    fi
#----------- Determine if the Fortran compiler gfortran -------------------
#   Turn off warnings for passing non-ANSI types to BIND().
#   Allow for argument mismatch (gfortran >= 10)
#--------------------------------------------------------------------------
     fc_vendor="`$FC --version 2>&1 | grep -i 'gnu'`"
     if test X != "X$fc_vendor"; then
       FFLAGS="$FFLAGS -Wno-c-binding-type"
   
       # Get the compiler version for GNU fortran
       #
       #   f9x_version:   Version number: x.x.x
       #
       f9x_version="`$FC -v 2>&1 | grep 'gcc version' | cut -d' ' -f3`"
       if test X != "X$f9x_version"; then 
           # Get the compiler version numbers
           f9x_vers_major=`echo $f9x_version | cut -f1 -d.`
           f9x_vers_minor=`echo $f9x_version | cut -f2 -d.`
           f9x_vers_patch=`echo $f9x_version | cut -f3 -d.`
           test -n "$f9x_vers_major" || f9x_vers_major=0
           test -n "$f9x_vers_minor" || f9x_vers_minor=0
           test -n "$f9x_vers_patch" || f9x_vers_patch=0
      fi
      if test $f9x_vers_major -ge 10; then
         FFLAGS="$FFLAGS -fallow-argument-mismatch"
      fi
      # Warn about a gfortran 10.2.0 bug (GCC Bug 100149) which
      # causes cg_goto_f to segfault, other versions are fine.
      if test $f9x_vers_major = 10 && test $f9x_vers_minor = 2; then
         AC_MSG_WARN([Fortran mapping (cg_goto_f) is broken for the specified gfortran version.
                        !!! IT'S ADVISABLE TO AVOID VERSION 10.2 !!!])
      fi
    fi
#----------- Determine if the Fortran compiler NAG -------------------
#   Add compiler flag for C API calls.
#--------------------------------------------------------------------------
     fc_vendor="`$FC -V 2>&1 |grep 'NAG'`"
     if test X != "X$fc_vendor"; then
       FFLAGS="$FFLAGS -mismatch_all"
     fi
fi
#--------------------------------------------------------------------------
# substitutions and write Makefile and make.defs
#--------------------------------------------------------------------------

BUILDOPTS=""
ADFINCS=""
ADFOBJS=""
INSTALLADF=""
INSTALLPCGNS=""
PTESTS=""

if test "$withhdf5" = "yes"; then
  ADFOBJS="\$(OBJDIR)/ADFH.\$(O)"
  ADFINCS="adfh/ADFH.h"
fi

if test "$parallel" = "yes"; then
  ADFOBJS="$ADFOBJS \$(OBJDIR)/pcgnslib.\$(O)"
  INSTALLPCGNS=install-pcgns
  PTESTS=runptests
fi

if test $BUILDLEGACY -ne 0 ; then
  BUILD64BIT=0
  if test "$withhdf5" = "yes"; then
    INSTALLADF="install-adf install-adfh"
  else
    INSTALLADF="install-adf"
  fi
fi

FALL=""
FALL64=""
PFALL=""

if test $fortran = no; then
  F2COBJS=#F2COBJS
  FGNSOBJS=#FGNSOBJS
else
  F2COBJS=F2COBJS
  FGNSOBJS=FGNSOBJS
  FALL=\$\(FALL\)
  if test $BUILD64BIT -eq 0; then
    PFALL=$FALL
  else
    FALL64=\$\(FALL64\)
  fi
fi

CGNSLIB=libcgns.$EXT_LIB
test "$LIBS" = "-lm -lm " && LIBS=-lm

AC_PROG_MAKE_SET

AC_SUBST(BUILDDEBUG)
AC_SUBST(BUILDLEGACY)
AC_SUBST(BUILDSCOPE)
AC_SUBST(BUILD64BIT)
AC_SUBST(BUILDFORTRAN)
AC_SUBST(HAVE_FORTRAN_95)
AC_SUBST(HAVE_FORTRAN_2003)
AC_SUBST(HAVE_FORTRAN_2008TS)
AC_SUBST(FORTRAN_DEFAULT_INTEGER_C_INT64_T)
AC_SUBST(HDF5_HAVE_MULTI_DATASETS)
AC_SUBST(HAVE_STAT64_STRUCT)
AC_SUBST(HDF5_HAVE_COLL_METADATA)
AC_SUBST(HDF5_HAVE_FILE_SPACE_STRATEGY)
AC_SUBST(FC_D_PRE)
AC_SUBST(BUILDPARALLEL)
AC_SUBST(BUILDBASESCOPE)
AC_SUBST(HAVE_COMPLEX_C99_EXT)

AC_SUBST(CGNSTOOLS)
AC_SUBST(CGLONGT)

AC_SUBST(SYSTEM)
AC_SUBST(CGNSVERS)
AC_SUBST(LIBEXT)
AC_SUBST(BUILDOPTS)
AC_SUBST(BUILDDIR)

AC_SUBST(ADFOBJS)
AC_SUBST(ADFINCS)
AC_SUBST(F2COBJS)
AC_SUBST(FGNSOBJS)

AC_SUBST(BUILDHDF5)
AC_SUBST(H5NEEDMPI)
AC_SUBST(H5NEEDSZIP)
AC_SUBST(H5NEEDZLIB)

AC_SUBST(HDF5DEF)
AC_SUBST(HDF5INC)
AC_SUBST(HDF5LIB)
AC_SUBST(HDF5LIB_DEP)
AC_SUBST(ZLIBLIB)
AC_SUBST(SZIPLIB)

AC_SUBST(MPIDEF)
AC_SUBST(MPIINC)
AC_SUBST(MPILIBS)
AC_SUBST(MPIEXEC)

AC_SUBST(SYSCFLAGS)
AC_SUBST(COOUT)
AC_SUBST(CEOUT)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)

AC_SUBST(F2CFLAGS)
AC_SUBST(CFGFLAGS)

AC_SUBST(STRIP)

AC_SUBST(SYSFFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(FEOUT)
AC_SUBST(FLIBS)

AC_SUBST(AR)
AC_SUBST(AROUT)
AC_SUBST(RANLIB)

AC_SUBST(AR_LIB)
AC_SUBST(RAN_LIB)

AC_SUBST(LIBDIR)
AC_SUBST(INCLUDEDIR)
AC_SUBST(BINDIR)
AC_SUBST(INSTALLADF)
AC_SUBST(INSTALLPCGNS)
AC_SUBST(INSTALLCGNSTOOLS)
AC_SUBST(PTESTS)

AC_SUBST(FALL)
AC_SUBST(FALL64)
AC_SUBST(PFALL)

BIN_INSTALL_DIR=$BINDIR
EXE_INSTALL_DIR=$BINDIR/cgnstools

if test "$ac_cv_cygwin" = "yes"; then
  CG_BIN_DIR=`cygpath -m $BIN_INSTALL_DIR`
  CG_LIB_DIR=`cygpath -m $LIB_INSTALL_DIR`
else
  CG_BIN_DIR=$BIN_INSTALL_DIR
  CG_LIB_DIR=$LIB_INSTALL_DIR
fi

AC_SUBST(PROGLIST)
AC_SUBST(INSTLIST)

AC_SUBST(CGNSDIR)
AC_SUBST(CGNSLIB)

AC_SUBST(BIN_INSTALL_DIR)
AC_SUBST(EXE_INSTALL_DIR)
AC_SUBST(LIB_INSTALL_DIR)

AC_SUBST(TKINCS)
AC_SUBST(TKLIBS)

AC_SUBST(X11INCS)
AC_SUBST(X11LIBS)

AC_SUBST(OGLINCS)
AC_SUBST(OGLLIBS)

AC_SUBST(PLOTOPTS)

AC_SUBST(TKOGLXLIB)
AC_SUBST(TKOGLOPTS)

AC_SUBST(CALCOPTS)

AC_SUBST(CG_BIN_DIR)
AC_SUBST(CG_LIB_DIR)
AC_SUBST(TCL_LIBRARY_SPEC)
AC_SUBST(TK_LIBRARY_SPEC)
AC_SUBST(LIBRARY_PATH_SPEC)

SIZEOF_LONG="$ac_cv_sizeof_long"
AC_CHECK_SIZEOF([void *])
SIZEOF_VOID_P="$ac_cv_sizeof_void_p"
AC_SUBST(SIZEOF_LONG)
AC_SUBST(SIZEOF_VOID_P)

AC_OUTPUT(cgnstypes.h cgnstypes_f.h cgnstypes_f03.h cgnsconfig.h \
  cg_hash_types.h Makefile make.defs cgnsBuild.defs tests/Makefile \
  tools/Makefile Test_UserGuideCode/C_code_parallel/Makefile ptests/Makefile cgnstools/Makefile cgnstools/make.defs \
  cgnstools/cgconfig)

