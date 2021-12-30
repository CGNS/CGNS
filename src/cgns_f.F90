!-------------------------------------------------------------------------
!This software is provided 'as-is', without any express or implied warranty.
!In no event will the authors be held liable for any damages arising from
!the use of this software.
!
!Permission is granted to anyone to use this software for any purpose,
!including commercial applications, and to alter it and redistribute it
!freely, subject to the following restrictions:
!
!1. The origin of this software must not be misrepresented; you must not
!   claim that you wrote the original software. If you use this software
!   in a product, an acknowledgment in the product documentation would be
!   appreciated but is not required.
!
!2. Altered source versions must be plainly marked as such, and must not
!   be misrepresented as being the original software.
!
!3. This notice may not be removed or altered from any source distribution.
!-------------------------------------------------------------------------
!   _____ _____ _   _  _____
!  / ____/ ____| \ | |/ ____|
! | |   | |  __|  \| | (___
! | |   | | |_ | . ` |\___ \
! | |___| |__| | |\  |____) |
!  \_____\_____|_| \_|_____/
!
!  PURPOSE:
!    Provides a module for the Fortran wrapper interfaces and CGNS
!    constant parameters.
!
!
!  KNOWN ISSUES:
!    Routines passing an argument to a C API argument of type
!    void * do not have explicit interfaces. They are the routines which
!    are commented out.
!
MODULE cgns

  USE ISO_C_BINDING, ONLY : C_INT, C_FLOAT, C_DOUBLE, C_LONG_LONG, C_CHAR, C_PTR, C_FUNPTR
  IMPLICIT NONE

#include "cgnstypes_f03.h"

!These definitions are needed for Windows DLLs
!DEC$ IF DEFINED(WINNT)
!DEC$ ATTRIBUTES REFERENCE, C, VARYING :: cg_goto_f
!DEC$ ATTRIBUTES REFERENCE, C, VARYING :: cg_array_read_f
!DEC$ ATTRIBUTES REFERENCE, C, VARYING :: cg_array_read_as_f
!DEC$ ATTRIBUTES REFERENCE, C, VARYING :: cg_array_write_f
!DEC$ ENDIF


#if CG_BUILD_64BIT_F
#  if HAVE_FORTRAN_2003
  INTEGER, PARAMETER :: CGSIZE_T = C_LONG_LONG
  INTEGER, PARAMETER :: CGID_T   = C_DOUBLE
  INTEGER, PARAMETER :: CGLONG_T = C_LONG_LONG
#  else
  INTEGER, PARAMETER :: cgint_kind    = SELECTED_INT_KIND(15) ! should map to INTEGER*8 on most modern processors
  INTEGER, PARAMETER :: cgdouble_kind = SELECTED_REAL_KIND(10) ! should map to REAL*8 on most modern processors
  INTEGER, PARAMETER :: CGSIZE_T      = cgint_kind
  INTEGER, PARAMETER :: CGID_T        = cgdouble_kind
  INTEGER, PARAMETER :: CGLONG_T      = cgint_kind
#  endif
  LOGICAL, PARAMETER :: CG_BUILD_64BIT = .TRUE.
#else
#  if HAVE_FORTRAN_2003
  INTEGER, PARAMETER :: CGSIZE_T = C_INT
  INTEGER, PARAMETER :: CGID_T   = C_DOUBLE
  INTEGER, PARAMETER :: CGLONG_T = C_LONG_LONG
#  else
  INTEGER, PARAMETER :: cgint_kind    = SELECTED_INT_KIND(5) ! should map to INTEGER*4 on most modern processors
  INTEGER, PARAMETER :: cglong_kind   = SELECTED_INT_KIND(15) ! should map to INTEGER*8 on most modern processors
  INTEGER, PARAMETER :: cgdouble_kind = SELECTED_REAL_KIND(10) ! should map to REAL*8 on most modern processors
  INTEGER, PARAMETER :: CGSIZE_T      = cgint_kind
  INTEGER, PARAMETER :: CGID_T        = cgdouble_kind
  INTEGER, PARAMETER :: CGLONG_T      = cglong_kind
#  endif
  LOGICAL, PARAMETER :: CG_BUILD_64BIT = .FALSE.
#endif

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: CGSIZE_T
!DEC$ATTRIBUTES DLLEXPORT :: CGID_T
!DEC$ATTRIBUTES DLLEXPORT :: CGLONG_T
!DEC$ATTRIBUTES DLLEXPORT :: CG_BUILD_64BIT
!DEC$endif

  INTEGER, PARAMETER, PRIVATE :: MAX_LEN = 32

  INTERFACE cgio_set_dimensions_f

     SUBROUTINE cgio_set_dimensions_f_0(cgio_num, id, data_type, ndims, dims, ier) ! BIND(C, NAME="cgio_set_dimensions_f_0")
       IMPORT :: CGSIZE_T, C_DOUBLE, C_CHAR
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR, LEN=*) :: data_type
       INTEGER :: ndims
       INTEGER(CGSIZE_T) :: dims
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_set_dimensions_f_0

     SUBROUTINE cgio_set_dimensions_f_1(cgio_num, id, data_type, ndims, dims, ier) ! BIND(C, NAME="cgio_set_dimensions_f_1")
       IMPORT :: CGSIZE_T, C_DOUBLE, C_CHAR
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR, LEN=*) :: data_type
       INTEGER :: ndims
       INTEGER(CGSIZE_T), DIMENSION(*) :: dims
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_set_dimensions_f_1

  END INTERFACE

  INTERFACE cgio_get_dimensions_f

     SUBROUTINE cgio_get_dimensions_f_0(cgio_num, id, ndims, dims, ier) BIND(C, NAME="cgio_get_dimensions_f_0")
       IMPORT :: C_DOUBLE, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       INTEGER :: ndims
       INTEGER(CGSIZE_T) :: dims
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_dimensions_f_0

     SUBROUTINE cgio_get_dimensions_f_1(cgio_num, id, ndims, dims, ier) BIND(C, NAME="cgio_get_dimensions_f_1")
       IMPORT :: C_DOUBLE, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       INTEGER :: ndims
       INTEGER(CGSIZE_T), DIMENSION(*) :: dims
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_dimensions_f_1

  END INTERFACE

  ! Fortran version of cgnslib.h
  !
  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  !*      modes for cgns file                                            *
  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTEGER(C_INT), PARAMETER :: CG_MODE_READ   = 0
  INTEGER(C_INT), PARAMETER :: CG_MODE_WRITE  = 1
  INTEGER(C_INT), PARAMETER :: CG_MODE_MODIFY = 2

  ! file open modes (found in cgns_io.h)
  INTEGER(C_INT), PARAMETER :: CGIO_MODE_READ   = 0
  INTEGER(C_INT), PARAMETER :: CGIO_MODE_WRITE  = 1
  INTEGER(C_INT), PARAMETER :: CGIO_MODE_MODIFY = 2

  ! database file types (found in cgns_io.h)
  INTEGER(C_INT), PARAMETER :: CGIO_FILE_NONE = 0
  INTEGER(C_INT), PARAMETER :: CGIO_FILE_ADF  = 1
  INTEGER(C_INT), PARAMETER :: CGIO_FILE_HDF5 = 2
  INTEGER(C_INT), PARAMETER :: CGIO_FILE_ADF2 = 3

  !* legacy code support
  INTEGER(C_INT) MODE_READ, MODE_WRITE, MODE_MODIFY
  PARAMETER (MODE_READ   = 0)
  PARAMETER (MODE_WRITE  = 1)
  PARAMETER (MODE_MODIFY = 2)

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: CG_MODE_READ
!DEC$ATTRIBUTES DLLEXPORT :: CG_MODE_WRITE
!DEC$ATTRIBUTES DLLEXPORT :: CG_MODE_MODIFY
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_MODE_READ
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_MODE_WRITE
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_MODE_MODIFY
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_FILE_NONE
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_FILE_ADF
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_FILE_HDF5
!DEC$ATTRIBUTES DLLEXPORT :: CGIO_FILE_ADF2
!DEC$endif

  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  !*      file types (found in cgnslib.h)                                *
  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTEGER(C_INT), PARAMETER :: CG_FILE_NONE  = 0
  INTEGER(C_INT), PARAMETER :: CG_FILE_ADF   = 1
  INTEGER(C_INT), PARAMETER :: CG_FILE_HDF5  = 2
  INTEGER(C_INT), PARAMETER :: CG_FILE_ADF2  = 3

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: CG_FILE_NONE
!DEC$ATTRIBUTES DLLEXPORT :: CG_FILE_ADF
!DEC$ATTRIBUTES DLLEXPORT :: CG_FILE_HDF5
!DEC$ATTRIBUTES DLLEXPORT :: CG_FILE_ADF2
!DEC$endif

  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  !*      some error code (found in cgnslib.h)                           *
  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTEGER(C_INT), PARAMETER :: CG_OK              = 0
  INTEGER(C_INT), PARAMETER :: CG_ERROR           = 1
  INTEGER(C_INT), PARAMETER :: CG_NODE_NOT_FOUND  = 2
  INTEGER(C_INT), PARAMETER :: CG_INCORRECT_PATH  = 3
  INTEGER(C_INT), PARAMETER :: CG_CG_NO_INDEX_DIM = 4

  !* legacy code support
  INTEGER(C_INT) ALL_OK, ERROR, NODE_NOT_FOUND, INCORRECT_PATH
  PARAMETER (ALL_OK         = 0)
  PARAMETER (ERROR          = 1)
  PARAMETER (NODE_NOT_FOUND = 2)
  PARAMETER (INCORRECT_PATH = 3)

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: CG_OK
!DEC$ATTRIBUTES DLLEXPORT :: CG_ERROR
!DEC$ATTRIBUTES DLLEXPORT :: CG_NODE_NOT_FOUND
!DEC$ATTRIBUTES DLLEXPORT :: CG_INCORRECT_PATH
!DEC$ATTRIBUTES DLLEXPORT :: CG_CG_NO_INDEX_DIM
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*     Configuration options (found in cgnslib.h)                      *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  INTEGER, PARAMETER :: CG_CONFIG_ERROR      = 1
  INTEGER, PARAMETER :: CG_CONFIG_COMPRESS   = 2
  INTEGER, PARAMETER :: CG_CONFIG_SET_PATH   = 3
  INTEGER, PARAMETER :: CG_CONFIG_ADD_PATH   = 4
  INTEGER, PARAMETER :: CG_CONFIG_FILE_TYPE  = 5
  INTEGER, PARAMETER :: CG_CONFIG_RIND_INDEX = 6
  
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_COMPRESS       = 201
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_MPI_COMM       = 202
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_DISKLESS       = 203
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_DISKLESS_INCR  = 204
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_DISKLESS_WRITE = 205
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_ALIGNMENT      = 206
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_MD_BLOCK_SIZE  = 207
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_BUFFER         = 208
  INTEGER, PARAMETER :: CG_CONFIG_HDF5_SIEVE_BUF_SIZE = 209


  INTEGER, PARAMETER :: CG_CONFIG_RESET = 1000
  INTEGER, PARAMETER :: CG_CONFIG_RESET_HDF5 = 1

  INTEGER, PARAMETER :: CG_CONFIG_RIND_ZERO = 0
  INTEGER, PARAMETER :: CG_CONFIG_RIND_CORE = 1

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_ERROR
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_COMPRESS
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_SET_PATH
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_ADD_PATH
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_FILE_TYPE
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_RIND_INDEX
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_COMPRESS
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_MPI_COMM      
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_DISKLESS      
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_DISKLESS_INCR 
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_DISKLESS_WRITE
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_ALIGNMENT     
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_MD_BLOCK_SIZE 
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_BUFFER        
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_HDF5_SIEVE_BUF_SIZE

!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_RESET 
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_RESET_HDF5

!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_RIND_ZERO
!DEC$ATTRIBUTES DLLEXPORT :: CG_CONFIG_RIND_CORE
!DEC$endif

  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  !*      Parallel CGNS parameters                                       *
  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  ENUM, BIND(C)
      ENUMERATOR :: CGP_INDEPENDENT = 0
      ENUMERATOR :: CGP_COLLECTIVE  = 1
  END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Dimensional Units (found in cgnslib.h)                         *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTEGER(C_INT), PARAMETER :: CG_Null = 0
  INTEGER(C_INT), PARAMETER :: CG_UserDefined = 1

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: CG_Null
!DEC$ATTRIBUTES DLLEXPORT :: CG_UserDefined
!DEC$endif

!* legacy code support
  INTEGER(C_INT) Null, UserDefined
  PARAMETER (Null = 0)
  PARAMETER (UserDefined = 1)

  CHARACTER(LEN=MAX_LEN) :: MassUnitsName(0:5)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(MassUnitsNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(MassUnitsUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Kilogram)             = 2
      ENUMERATOR :: CGNS_ENUMV(Gram)                 = 3
      ENUMERATOR :: CGNS_ENUMV(Slug)                 = 4
      ENUMERATOR :: CGNS_ENUMV(PoundMass)            = 5
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: LengthUnitsName(0:6)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(LengthUnitsNull)         = CG_Null
      ENUMERATOR :: CGNS_ENUMV(LengthUnitsUserDefined)  = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Meter)                   = 2
      ENUMERATOR :: CGNS_ENUMV(Centimeter)              = 3
      ENUMERATOR :: CGNS_ENUMV(Millimeter)              = 4
      ENUMERATOR :: CGNS_ENUMV(Foot)                    = 5
      ENUMERATOR :: CGNS_ENUMV(Inch)                    = 6
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: TimeUnitsName(0:2)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(TimeUnitsNull)         = CG_Null
      ENUMERATOR :: CGNS_ENUMV(TimeUnitsUserDefined)  = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Second)                = 2
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: TemperatureUnitsName(0:5)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(TemperatureUnitsNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(TemperatureUnitsUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Kelvin)                      = 2
      ENUMERATOR :: CGNS_ENUMV(Celsius)                     = 3
      ENUMERATOR :: CGNS_ENUMV(Rankine)                     = 4
      ENUMERATOR :: CGNS_ENUMV(Fahrenheit)                  = 5
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: AngleUnitsName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(AngleUnitsNull)              = CG_Null
      ENUMERATOR :: CGNS_ENUMV(AngleUnitsUserDefined)       = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Degree)                      = 2
      ENUMERATOR :: CGNS_ENUMV(Radian)                      = 3
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: ElectricCurrentUnitsName(0:6)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(ElectricCurrentUnitsNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(ElectricCurrentUnitsUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Ampere)                          = 2
      ENUMERATOR :: CGNS_ENUMV(Abampere)                        = 3
      ENUMERATOR :: CGNS_ENUMV(Statampere)                      = 4
      ENUMERATOR :: CGNS_ENUMV(Edison)                          = 5
      ENUMERATOR :: CGNS_ENUMV(auCurrent)                       = 6
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: SubstanceAmountUnitsName(0:5)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(SubstanceAmountUnitsNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(SubstanceAmountUnitsUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Mole)                            = 2
      ENUMERATOR :: CGNS_ENUMV(Entities)                        = 3
      ENUMERATOR :: CGNS_ENUMV(StandardCubicFoot)               = 4
      ENUMERATOR :: CGNS_ENUMV(StandardCubicMeter)              = 5
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: LuminousIntensityUnitsName(0:6)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(LuminousIntensityUnitsNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(LuminousIntensityUnitsUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Candela)                            = 2
      ENUMERATOR :: CGNS_ENUMV(Candle)                             = 3
      ENUMERATOR :: CGNS_ENUMV(Carcel)                             = 4
      ENUMERATOR :: CGNS_ENUMV(Hefner)                             = 5
      ENUMERATOR :: CGNS_ENUMV(Violle)                             = 6
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: MassUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: LengthUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: TimeUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: TemperatureUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: AngleUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: ElectricCurrentUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: SubstanceAmountUnitsName
!DEC$ATTRIBUTES DLLEXPORT :: LuminousIntensityUnitsName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data Class (found in cgnslib.h                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  CHARACTER(LEN=MAX_LEN) :: DataClassName(0:6)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(DataClassNull)                       = CG_Null
      ENUMERATOR :: CGNS_ENUMV(DataClassUserDefined)                = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Dimensional)                         = 2
      ENUMERATOR :: CGNS_ENUMV(NormalizedByDimensional)             = 3
      ENUMERATOR :: CGNS_ENUMV(NormalizedByUnknownDimensional)      = 4
      ENUMERATOR :: CGNS_ENUMV(NondimensionalParameter)             = 5
      ENUMERATOR :: CGNS_ENUMV(DimensionlessConstant)               = 6
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: DataClassName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Location                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: GridLocationName(0:8)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(GridLocationNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(GridLocationUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Vertex)                  = 2
      ENUMERATOR :: CGNS_ENUMV(CellCenter)              = 3
      ENUMERATOR :: CGNS_ENUMV(FaceCenter)              = 4
      ENUMERATOR :: CGNS_ENUMV(IFaceCenter)             = 5
      ENUMERATOR :: CGNS_ENUMV(JFaceCenter)             = 6
      ENUMERATOR :: CGNS_ENUMV(KFaceCenter)             = 7
      ENUMERATOR :: CGNS_ENUMV(EdgeCenter)              = 8
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: GridLocationName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: GridConnectivityTypeName(0:4)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(GridConnectivityTypeNull)       = CG_Null
      ENUMERATOR :: CGNS_ENUMV(GridConnectivityTypeUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Overset)                         = 2
      ENUMERATOR :: CGNS_ENUMV(Abutting)                        = 3
      ENUMERATOR :: CGNS_ENUMV(Abutting1to1)                    = 4
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: GridConnectivityTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Point Set Types                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: PointSetTypeName(0:8)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(PointSetTypeNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(PointSetTypeUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(PointList)               = 2
      ENUMERATOR :: CGNS_ENUMV(PointListDonor)          = 3
      ENUMERATOR :: CGNS_ENUMV(PointRange)              = 4
      ENUMERATOR :: CGNS_ENUMV(PointRangeDonor)         = 5
      ENUMERATOR :: CGNS_ENUMV(ElementRange)            = 6
      ENUMERATOR :: CGNS_ENUMV(ElementList)             = 7
      ENUMERATOR :: CGNS_ENUMV(CellListDonor)           = 8
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: PointSetTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Governing Equations and Physical Models Types                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: GoverningEquationsTypeName(0:7)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(GoverningEquationsNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(GoverningEquationsUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(FullPotential)                 = 2
      ENUMERATOR :: CGNS_ENUMV(Euler)                         = 3
      ENUMERATOR :: CGNS_ENUMV(NSLaminar)                     = 4
      ENUMERATOR :: CGNS_ENUMV(NSTurbulent)                   = 5
      ENUMERATOR :: CGNS_ENUMV(NSLaminarIncompressible)       = 6
      ENUMERATOR :: CGNS_ENUMV(NSTurbulentIncompressible)     = 7
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: GoverningEquationsTypeName
!DEC$endif

!** Any model type will accept both ModelTypeNull and ModelTypeUserDefined.
!** The following models will accept these values as valid...
!**
!** GasModel_t: Ideal, VanderWaals, CaloricallyPerfect, ThermallyPerfect,
!**    ConstantDensity, RedlichKwong
!**
!** ViscosityModel_t: Constant, PowerLaw, SutherlandLaw
!**
!** ThermalConductivityModel_t: PowerLaw, SutherlandLaw, ConstantPrandtl
!**
!** TurbulenceModel_t: Algebraic_BaldwinLomax, Algebraic_CebeciSmith,
!**    HalfEquation_JohnsonKing, OneEquation_BaldwinBarth,
!**    OneEquation_SpalartAllmaras, TwoEquation_JonesLaunder,
!**    TwoEquation_MenterSST,TwoEquation_Wilcox
!**
!** TurbulenceClosure_t: EddyViscosity, ReynoldsStress,
!**    ReynoldsStressAlgebraic
!**
!** ThermalRelaxationModel_t: Frozen, ThermalEquilib, ThermalNonequilib
!**
!** ChemicalKineticsModel_t: Frozen, ChemicalEquilibCurveFit,
!**    ChemicalEquilibMinimization, ChemicalNonequilib
!**
!** EMElectricFieldModel_t: Voltage, Interpolated, Constant, Frozen
!**
!** EMMagneticFieldModel_t: Interpolated, Constant, Frozen
!**
!** EMConductivityModel_t: Constant, Frozen, Equilibrium_LinRessler,
!**                             Chemistry_LinRessler

  CHARACTER(LEN=MAX_LEN) :: ModelTypeName(0:35)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(ModelTypeNull)               = CG_Null
      ENUMERATOR :: CGNS_ENUMV(ModelTypeUserDefined)        = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Ideal)                       = 2
      ENUMERATOR :: CGNS_ENUMV(VanderWaals)                 = 3
      ENUMERATOR :: CGNS_ENUMV(Constant)                    = 4
      ENUMERATOR :: CGNS_ENUMV(PowerLaw)                    = 5
      ENUMERATOR :: CGNS_ENUMV(SutherlandLaw)               = 6
      ENUMERATOR :: CGNS_ENUMV(ConstantPrandtl)             = 7
      ENUMERATOR :: CGNS_ENUMV(EddyViscosity)               = 8
      ENUMERATOR :: CGNS_ENUMV(ReynoldsStress)              = 9
      ENUMERATOR :: CGNS_ENUMV(ReynoldsStressAlgebraic)     = 10
      ENUMERATOR :: CGNS_ENUMV(Algebraic_BaldwinLomax)      = 11
      ENUMERATOR :: CGNS_ENUMV(Algebraic_CebeciSmith)       = 12
      ENUMERATOR :: CGNS_ENUMV(HalfEquation_JohnsonKing)    = 13
      ENUMERATOR :: CGNS_ENUMV(OneEquation_BaldwinBarth)    = 14
      ENUMERATOR :: CGNS_ENUMV(OneEquation_SpalartAllmaras) = 15
      ENUMERATOR :: CGNS_ENUMV(TwoEquation_JonesLaunder)    = 16
      ENUMERATOR :: CGNS_ENUMV(TwoEquation_MenterSST)       = 17
      ENUMERATOR :: CGNS_ENUMV(TwoEquation_Wilcox)          = 18
      ENUMERATOR :: CGNS_ENUMV(CaloricallyPerfect)          = 19
      ENUMERATOR :: CGNS_ENUMV(ThermallyPerfect)            = 20
      ENUMERATOR :: CGNS_ENUMV(ConstantDensity)             = 21
      ENUMERATOR :: CGNS_ENUMV(RedlichKwong)                = 22
      ENUMERATOR :: CGNS_ENUMV(Frozen)                      = 23
      ENUMERATOR :: CGNS_ENUMV(ThermalEquilib)              = 24
      ENUMERATOR :: CGNS_ENUMV(ThermalNonequilib)           = 25
      ENUMERATOR :: CGNS_ENUMV(ChemicalEquilibCurveFit)     = 26
      ENUMERATOR :: CGNS_ENUMV(ChemicalEquilibMinimization) = 27
      ENUMERATOR :: CGNS_ENUMV(ChemicalNonequilib)          = 28
      ENUMERATOR :: CGNS_ENUMV(EMElectricField)             = 29
      ENUMERATOR :: CGNS_ENUMV(EMMagneticField)             = 30
      ENUMERATOR :: CGNS_ENUMV(EMConductivity)              = 31
      ENUMERATOR :: CGNS_ENUMV(Voltage)                     = 32
      ENUMERATOR :: CGNS_ENUMV(Interpolated)                = 33
      ENUMERATOR :: CGNS_ENUMV(Equilibrium_LinRessler)      = 34
      ENUMERATOR :: CGNS_ENUMV(Chemistry_LinRessler)        = 35
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: ModelTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Boundary Condition Types                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: BCTypeName(0:25)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(BCTypeNull)              = CG_Null
      ENUMERATOR :: CGNS_ENUMV(BCTypeUserDefined)       = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(BCAxisymmetricWedge)     = 2
      ENUMERATOR :: CGNS_ENUMV(BCDegenerateLine)        = 3
      ENUMERATOR :: CGNS_ENUMV(BCDegeneratePoint)       = 4
      ENUMERATOR :: CGNS_ENUMV(BCDirichlet)             = 5
      ENUMERATOR :: CGNS_ENUMV(BCExtrapolate)           = 6
      ENUMERATOR :: CGNS_ENUMV(BCFarfield)              = 7
      ENUMERATOR :: CGNS_ENUMV(BCGeneral)               = 8
      ENUMERATOR :: CGNS_ENUMV(BCInflow)                = 9
      ENUMERATOR :: CGNS_ENUMV(BCInflowSubsonic)        = 10
      ENUMERATOR :: CGNS_ENUMV(BCInflowSupersonic)      = 11
      ENUMERATOR :: CGNS_ENUMV(BCNeumann)               = 12
      ENUMERATOR :: CGNS_ENUMV(BCOutflow)               = 13
      ENUMERATOR :: CGNS_ENUMV(BCOutflowSubsonic)       = 14
      ENUMERATOR :: CGNS_ENUMV(BCOutflowSupersonic)     = 15
      ENUMERATOR :: CGNS_ENUMV(BCSymmetryPlane)         = 16
      ENUMERATOR :: CGNS_ENUMV(BCSymmetryPolar)         = 17
      ENUMERATOR :: CGNS_ENUMV(BCTunnelInflow)          = 18
      ENUMERATOR :: CGNS_ENUMV(BCTunnelOutflow)         = 19
      ENUMERATOR :: CGNS_ENUMV(BCWall)                  = 20
      ENUMERATOR :: CGNS_ENUMV(BCWallInviscid)          = 21
      ENUMERATOR :: CGNS_ENUMV(BCWallViscous)           = 22
      ENUMERATOR :: CGNS_ENUMV(BCWallViscousHeatFlux)   = 23
      ENUMERATOR :: CGNS_ENUMV(BCWallViscousIsothermal) = 24
      ENUMERATOR :: CGNS_ENUMV(FamilySpecified)         = 25
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: BCTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: DataTypeName(0:8)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(DataTypeNull)
      ENUMERATOR :: CGNS_ENUMV(DataTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(Integer)
      ENUMERATOR :: CGNS_ENUMV(RealSingle)
      ENUMERATOR :: CGNS_ENUMV(RealDouble)
      ENUMERATOR :: CGNS_ENUMV(Character)
      ENUMERATOR :: CGNS_ENUMV(LongInteger)
      ENUMERATOR :: CGNS_ENUMV(ComplexSingle)
      ENUMERATOR :: CGNS_ENUMV(ComplexDouble)
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: DataTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BCData_t types                                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: BCDataTypeName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(BCDataTypeNull)        = CG_Null
      ENUMERATOR :: CGNS_ENUMV(BCDataTypeUserDefined) = CG_UserDefined
      ENUMERATOR :: CGNS_ENUMV(Dirichlet)             = 2
      ENUMERATOR :: CGNS_ENUMV(Neumann)               = 3
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: BCDataTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Element types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  CHARACTER(LEN=MAX_LEN) :: ElementTypeName(0:56)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(ElementTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(ElementTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(NODE)
      ENUMERATOR :: CGNS_ENUMV(BAR_2)
      ENUMERATOR :: CGNS_ENUMV(BAR_3)
      ENUMERATOR :: CGNS_ENUMV(TRI_3)
      ENUMERATOR :: CGNS_ENUMV(TRI_6)
      ENUMERATOR :: CGNS_ENUMV(QUAD_4)
      ENUMERATOR :: CGNS_ENUMV(QUAD_8)
      ENUMERATOR :: CGNS_ENUMV(QUAD_9)
      ENUMERATOR :: CGNS_ENUMV(TETRA_4)
      ENUMERATOR :: CGNS_ENUMV(TETRA_10)
      ENUMERATOR :: CGNS_ENUMV(PYRA_5)
      ENUMERATOR :: CGNS_ENUMV(PYRA_14)
      ENUMERATOR :: CGNS_ENUMV(PENTA_6)
      ENUMERATOR :: CGNS_ENUMV(PENTA_15)
      ENUMERATOR :: CGNS_ENUMV(PENTA_18)
      ENUMERATOR :: CGNS_ENUMV(HEXA_8)
      ENUMERATOR :: CGNS_ENUMV(HEXA_20)
      ENUMERATOR :: CGNS_ENUMV(HEXA_27)
      ENUMERATOR :: CGNS_ENUMV(MIXED)
      ENUMERATOR :: CGNS_ENUMV(PYRA_13)
      ENUMERATOR :: CGNS_ENUMV(NGON_n)
      ENUMERATOR :: CGNS_ENUMV(NFACE_n)
      ENUMERATOR :: CGNS_ENUMV(BAR_4)
      ENUMERATOR :: CGNS_ENUMV(TRI_9)
      ENUMERATOR :: CGNS_ENUMV(TRI_10)
      ENUMERATOR :: CGNS_ENUMV(QUAD_12)
      ENUMERATOR :: CGNS_ENUMV(QUAD_16)
      ENUMERATOR :: CGNS_ENUMV(TETRA_16)
      ENUMERATOR :: CGNS_ENUMV(TETRA_20)
      ENUMERATOR :: CGNS_ENUMV(PYRA_21)
      ENUMERATOR :: CGNS_ENUMV(PYRA_29)
      ENUMERATOR :: CGNS_ENUMV(PYRA_30)
      ENUMERATOR :: CGNS_ENUMV(PENTA_24)
      ENUMERATOR :: CGNS_ENUMV(PENTA_38)
      ENUMERATOR :: CGNS_ENUMV(PENTA_40)
      ENUMERATOR :: CGNS_ENUMV(HEXA_32)
      ENUMERATOR :: CGNS_ENUMV(HEXA_56)
      ENUMERATOR :: CGNS_ENUMV(HEXA_64)
      ENUMERATOR :: CGNS_ENUMV(BAR_5)
      ENUMERATOR :: CGNS_ENUMV(TRI_12)
      ENUMERATOR :: CGNS_ENUMV(TRI_15)
      ENUMERATOR :: CGNS_ENUMV(QUAD_P4_16)
      ENUMERATOR :: CGNS_ENUMV(QUAD_25)
      ENUMERATOR :: CGNS_ENUMV(TETRA_22)
      ENUMERATOR :: CGNS_ENUMV(TETRA_34)
      ENUMERATOR :: CGNS_ENUMV(TETRA_35)
      ENUMERATOR :: CGNS_ENUMV(PYRA_P4_29)
      ENUMERATOR :: CGNS_ENUMV(PYRA_50)
      ENUMERATOR :: CGNS_ENUMV(PYRA_55)
      ENUMERATOR :: CGNS_ENUMV(PENTA_33)
      ENUMERATOR :: CGNS_ENUMV(PENTA_66)
      ENUMERATOR :: CGNS_ENUMV(PENTA_75)
      ENUMERATOR :: CGNS_ENUMV(HEXA_44)
      ENUMERATOR :: CGNS_ENUMV(HEXA_98)
      ENUMERATOR :: CGNS_ENUMV(HEXA_125)
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: ElementTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Zone types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  CHARACTER(LEN=MAX_LEN) :: ZoneTypeName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(ZoneTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(ZoneTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(Structured)
      ENUMERATOR :: CGNS_ENUMV(Unstructured)
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: ZoneTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Rigid Grid Motion types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: RigidGridMotionTypeName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(RigidGridMotionTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(RigidGridMotionTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(ConstantRate)
      ENUMERATOR :: CGNS_ENUMV(VariableRate)
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: RigidGridMotionTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Arbitrary Grid Motion types                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: ArbitraryGridMotionTypeName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(ArbitraryGridMotionTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(ArbitraryGridMotionTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(NonDeformingGrid)
      ENUMERATOR :: CGNS_ENUMV(DeformingGrid)
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: ArbitraryGridMotionTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Simulation type                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  CHARACTER(LEN=MAX_LEN) :: SimulationTypeName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(SimulationTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(SimulationTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(TimeAccurate)
      ENUMERATOR :: CGNS_ENUMV(NonTimeAccurate)
  END ENUM

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: SimulationTypeName
!DEC$endif

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BC Property types                                              *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  CHARACTER(LEN=MAX_LEN) :: WallFunctionTypeName(0:2)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(WallFunctionTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(WallFunctionTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(Generic)
  END ENUM

  CHARACTER(LEN=MAX_LEN) :: AreaTypeName(0:3)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(AreaTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(AreaTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(BleedArea)
      ENUMERATOR :: CGNS_ENUMV(CaptureArea)
  END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Property types                               *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  CHARACTER(LEN=MAX_LEN) :: AverageInterfaceTypeName(0:7)
  ENUM, BIND(C)
      ENUMERATOR :: CGNS_ENUMV(AverageInterfaceTypeNull) = CG_Null
      ENUMERATOR :: CGNS_ENUMV(AverageInterfaceTypeUserDefined)
      ENUMERATOR :: CGNS_ENUMV(AverageAll)
      ENUMERATOR :: CGNS_ENUMV(AverageCircumferential)
      ENUMERATOR :: CGNS_ENUMV(AverageRadial)
      ENUMERATOR :: CGNS_ENUMV(AverageI)
      ENUMERATOR :: CGNS_ENUMV(AverageJ)
      ENUMERATOR :: CGNS_ENUMV(AverageK)
  END ENUM

! For portability to Linux Absoft, all data statements were moved after the
! variables and parametres declarations

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Dimensional Units                                              *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  DATA MassUnitsName /'Null','UserDefined','Kilogram','Gram', & 
       'Slug','PoundMass'/
  DATA LengthUnitsName / 'Null', 'UserDefined', &
       'Meter','Centimeter','Millimeter','Foot','Inch'/

  DATA TimeUnitsName /'Null','UserDefined','Second'/

  DATA TemperatureUnitsName /'Null','UserDefined', &
       'Kelvin','Celsius','Rankine','Fahrenheit'/

  DATA AngleUnitsName /'Null','UserDefined','Degree','Radian'/

  DATA ElectricCurrentUnitsName /'Null', 'UserDefined', 'Ampere', &
       'Abampere', 'Statampere', 'Edison', 'a.u.'/

  DATA SubstanceAmountUnitsName /'Null', 'UserDefined', 'Mole', &
       'Entities', 'StandardCubicFoot', 'StandardCubicMeter'/

  DATA LuminousIntensityUnitsName /'Null', 'UserDefined', &
       'Candela', 'Candle', 'Carcel', 'Hefner', 'Violle'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data Class                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  DATA DataClassName / 'Null','UserDefined', &
       'Dimensional','NormalizedByDimensional', &
       'NormalizedByUnknownDimensional', &
       'NondimensionalParameter','DimensionlessConstant'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Location                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA GridLocationName / 'Null','UserDefined', &
       'Vertex','CellCenter','FaceCenter','IFaceCenter', &
       'JFaceCenter','KFaceCenter','EdgeCenter' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA GridConnectivityTypeName / 'Null','UserDefined', &
       'Overset','Abutting','Abutting1to1'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Point Set Types                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA PointSetTypeName / 'Null','UserDefined', &
       'PointList','PointListDonor',  &
       'PointRange','PointRangeDonor', &
       'ElementRange','ElementList','CellListDonor'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Governing Equations and Physical Models Types                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA GoverningEquationsTypeName / 'Null','UserDefined', &
       'FullPotential','Euler', 'NSLaminar', 'NSTurbulent', &
       'NSLaminarIncompressible', 'NSTurbulentIncompressible'/

  DATA ModelTypeName / 'Null','UserDefined', &
       'Ideal','VanderWaals', 'Constant','PowerLaw', &
       'SutherlandLaw','ConstantPrandtl','EddyViscosity', &
       'ReynoldsStress','ReynoldsStressAlgebraic', &
       'Algebraic_BaldwinLomax','Algebraic_CebeciSmith', &
       'HalfEquation_JohnsonKing','OneEquation_BaldwinBarth', &
       'OneEquation_SpalartAllmaras','TwoEquation_JonesLaunder', &
       'TwoEquation_MenterSST','TwoEquation_Wilcox', &
       'CaloricallyPerfect', 'ThermallyPerfect', &
       'ConstantDensity', 'RedlichKwong', 'Frozen', &
       'ThermalEquilib', 'ThermalNonequilib', &
       'ChemicalEquilibCurveFit', 'ChemicalEquilibMinimization', &
       'ChemicalNonequilib', 'EMElectricField', &
       'EMMagneticField', 'EMConductivity', 'Voltage', &
       'Interpolated', 'Equilibrium_LinRessler', &
       'Chemistry_LinRessler'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Boundary Condition Types                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA BCTypeName / 'Null','UserDefined', &
       'BCAxisymmetricWedge','BCDegenerateLine', &
       'BCDegeneratePoint','BCDirichlet','BCExtrapolate', &
       'BCFarfield','BCGeneral','BCInflow','BCInflowSubsonic', &
       'BCInflowSupersonic','BCNeumann','BCOutflow', &
       'BCOutflowSubsonic','BCOutflowSupersonic', &
       'BCSymmetryPlane','BCSymmetryPolar','BCTunnelInflow', &
       'BCTunnelOutflow','BCWall','BCWallInviscid', &
       'BCWallViscous','BCWallViscousHeatFlux', &
       'BCWallViscousIsothermal','FamilySpecified' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA DataTypeName / 'Null','UserDefined', &
       'Integer','RealSingle','RealDouble','Character', &
       'LongInteger','ComplexSingle','ComplexDouble' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BCData_t types                                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA BCDataTypeName / 'Null','UserDefined', &
       'Dirichlet', 'Neumann' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Element types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA ElementTypeName / 'Null','UserDefined', &
       'NODE', 'BAR_2', 'BAR_3', 'TRI_3', 'TRI_6', &
       'QUAD_4', 'QUAD_8', 'QUAD_9', 'TETRA_4', 'TETRA_10', &
       'PYRA_5', 'PYRA_14', 'PENTA_6', 'PENTA_15', &
       'PENTA_18', 'HEXA_8', 'HEXA_20', 'HEXA_27', 'MIXED', &
       'PYRA_13', 'NGON_n', 'NFACE_n', &
       'BAR_4', 'TRI_9', 'TRI_10', &
       'QUAD_12', 'QUAD_16', &
       'TETRA_16', 'TETRA_20', &
       'PYRA_21', 'PYRA_29', 'PYRA_30', &
       'PENTA_24', 'PENTA_38', 'PENTA_40', &
       'HEXA_32', 'HEXA_56', 'HEXA_64', &
       'BAR_5', 'TRI_12', 'TRI_15', &
       'QUAD_P4_16', 'QUAD_25', &
       'TETRA_22', 'TETRA_34', 'TETRA_35', &
       'PYRA_P4_29', 'PYRA_50', 'PYRA_55', &
       'PENTA_33', 'PENTA_66', 'PENTA_75', &
       'HEXA_44', 'HEXA_98', 'HEXA_125' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Zone types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA ZoneTypeName / 'Null','UserDefined', &
       'Structured', 'Unstructured' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Rigid Grid Motion types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA RigidGridMotionTypeName / 'Null','UserDefined', &
       'ConstantRate', 'VariableRate' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Arbitrary Grid Motion types                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA ArbitraryGridMotionTypeName / 'Null','UserDefined', &
       'NonDeformingGrid', 'DeformingGrid' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Simulation type                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA SimulationTypeName / 'Null','UserDefined', &
       'TimeAccurate', 'NonTimeAccurate' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BC Property types                                              *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA WallFunctionTypeName / 'Null','UserDefined',&
       'Generic' /

  DATA AreaTypeName / 'Null','UserDefined', &
       'BleedArea', 'CaptureArea' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Property types                               *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  DATA AverageInterfaceTypeName / 'Null','UserDefined', &
       'AverageAll', 'AverageCircumferential', 'AverageRadial', &
       'AverageI', 'AverageJ', 'AverageK' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      ENUMTYPE FOR FORTRAN FUNCTIONS                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTEGER, PARAMETER :: cgenum_t = KIND(CGP_INDEPENDENT)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      INTERFACES FOR THE FORTRAN FUNCTIONS                           *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTERFACE
     INTEGER(C_INT) FUNCTION cg_is_cgns(filename, file_type) BIND(C,NAME="cg_is_cgns")
       USE ISO_C_BINDING
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
       INTEGER, INTENT(OUT) :: file_type
     END FUNCTION cg_is_cgns

     INTEGER(C_INT) FUNCTION cg_open(filename, mode, fn) BIND(C,NAME="cg_open")
       USE ISO_C_BINDING
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
       INTEGER(C_INT), INTENT(IN), VALUE  :: mode
       INTEGER, INTENT(OUT) :: fn
     END FUNCTION cg_open

     SUBROUTINE cg_version_f(fn,FileVersion, ier) BIND(C,NAME="cg_version_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       INTEGER :: fn
       REAL(C_FLOAT)    :: FileVersion
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_version_f

     SUBROUTINE cg_precision_f(fn, PRECISION, ier) BIND(C,NAME="cg_precision_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: PRECISION
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_precision_f

     SUBROUTINE cg_close_f(fn, ier) BIND(C,NAME="cg_close_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_close_f

     SUBROUTINE cg_save_as_f(fn, filename, file_type, follow_links, ier) ! BIND(C,NAME="cg_save_as_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       INTEGER :: fn
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
       INTEGER :: file_type
       INTEGER :: follow_links
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_save_as_f

     SUBROUTINE cg_set_file_type_f(ft, ier) BIND(C,NAME="cg_set_file_type_f")
       IMPLICIT NONE
       INTEGER :: ft
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_set_file_type_f

     SUBROUTINE cg_get_file_type_f(fn, ft, ier) BIND(C,NAME="cg_get_file_type_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: ft
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_get_file_type_f

     SUBROUTINE cg_set_compress_f(cmpr, ier) BIND(C, NAME="cg_set_compress_f")
       IMPLICIT NONE
       INTEGER :: cmpr
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_set_compress_f

     SUBROUTINE cg_get_compress_f(cmpr, ier) BIND(C, NAME="cg_get_compress_f")
       INTEGER :: cmpr
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_get_compress_f

     SUBROUTINE cg_set_path_f(pathname, ier) !BIND(C, NAME="cg_set_path_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: pathname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_set_path_f

     SUBROUTINE cg_add_path_f(pathname, ier) !BIND(C, NAME="cg_add_path_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: pathname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_add_path_f

     SUBROUTINE cg_set_rind_zero_f(ier) BIND(C, NAME="cg_set_rind_zero_f")
       IMPLICIT NONE
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_set_rind_zero_f

     SUBROUTINE cg_set_rind_core_f(ier) BIND(C, NAME="cg_set_rind_core_f")
       IMPLICIT NONE
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_set_rind_core_f

     SUBROUTINE cg_configure_c_ptr(what, value, ier) BIND(C,NAME="cg_configure_c_ptr")
       IMPORT :: C_PTR
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: what
       TYPE(C_PTR), VALUE :: value
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_configure_c_ptr

     SUBROUTINE cg_configure_c_funptr(what, value, ier) BIND(C,NAME="cg_configure_c_funptr")
       IMPORT :: C_FUNPTR
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: what
       TYPE(C_FUNPTR), VALUE :: value
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_configure_c_funptr

     SUBROUTINE cg_get_cgio_f(fn, cgio_num, ier) BIND(C, NAME="cg_get_cgio_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: cgio_num
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_get_cgio_f

     SUBROUTINE cg_root_id_f(fn, rootid, ier) BIND(C, NAME="cg_root_id_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       INTEGER :: fn
       REAL(C_DOUBLE) :: rootid
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_root_id_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !      Read and write CGNSBase_t Nodes
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     SUBROUTINE cg_nbases_f(fn, nbases, ier) BIND(C, NAME="cg_nbases_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: nbases
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nbases_f

     SUBROUTINE cg_base_read_f(fn, B, basename, cell_dim, phys_dim, ier) !BIND(C, NAME="cg_base_read_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: basename
       INTEGER :: cell_dim
       INTEGER :: phys_dim
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_base_read_f

     SUBROUTINE cg_base_id_f(fn, B, base_id, ier) BIND(C, NAME="cg_base_id_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       REAL(C_DOUBLE) :: base_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_base_id_f

     SUBROUTINE cg_base_write_f(fn, basename, cell_dim, phys_dim, B, ier) !BIND(C, NAME="cg_base_write_f")
       USE ISO_C_BINDING
       IMPLICIT NONE
       INTEGER :: fn
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: basename
       INTEGER :: cell_dim
       INTEGER :: phys_dim
       INTEGER :: B
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_base_write_f

     SUBROUTINE cg_cell_dim_f(fn, B, dim, ier) BIND(C, NAME="cg_cell_dim_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: dim
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_cell_dim_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !       Read and write Zone_t Nodes
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     SUBROUTINE cg_nzones_f(fn, B, nzones, ier) BIND(C, NAME="cg_nzones_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: nzones
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nzones_f

     SUBROUTINE cg_zone_type_f(fn, B, Z, type, ier) BIND(C, NAME="cg_zone_type_f")
       IMPORT :: cgenum_t, c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER(cgenum_t) :: type
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zone_type_f

     SUBROUTINE cg_zone_read_f(fn, B, Z, zonename, size, ier) !BIND(C, NAME="cg_zone_read_f")
       IMPORT :: CGSIZE_T, c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zonename
       INTEGER(CGSIZE_T), DIMENSION(*) :: size
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zone_read_f

     SUBROUTINE cg_zone_id_f(fn, B, Z, zone_id, ier) BIND(C, NAME="cg_zone_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       REAL(C_DOUBLE) :: zone_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zone_id_f

     SUBROUTINE cg_zone_write_f(fn, B, zonename, size, TYPE, Z, ier) !BIND(C, NAME="cg_zone_write_f")
       IMPORT :: cgenum_t, c_char, cgsize_t
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: fn
       INTEGER, INTENT(IN) :: B
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: zonename
       INTEGER(CGSIZE_T), DIMENSION(*), INTENT(IN) :: size
       INTEGER(cgenum_t), INTENT(IN) :: TYPE
       INTEGER, INTENT(OUT) :: Z
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zone_write_f

     SUBROUTINE cg_index_dim_f(fn, B, Z, dim, ier) BIND(C, NAME="cg_index_dim_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: dim
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_index_dim_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write Family_t Nodes                                    *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nfamilies_f(fn, B, nfamilies, ier) BIND(C, NAME="cg_nfamilies_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: nfamilies
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nfamilies_f

     SUBROUTINE cg_family_read_f(fn, B, F, family_name, nboco, ngeos, ier) !BIND(C, NAME="cg_family_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
       INTEGER :: nboco
       INTEGER :: ngeos
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_family_read_f

     SUBROUTINE cg_family_write_f(fn, B, family_name, F, ier) !BIND(C, NAME="cg_family_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
       INTEGER :: F
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_family_write_f

     SUBROUTINE cg_nfamily_names_f(fn, B, F, nnames, ier) BIND(C, NAME="cg_nfamily_names_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       INTEGER :: nnames
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nfamily_names_f

     SUBROUTINE cg_family_name_read_f(fn, B, F, N, name, family, ier) !BIND(C, NAME="cg_family_name_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       INTEGER :: N
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_family_name_read_f

     SUBROUTINE cg_family_name_write_f(fn, B, F, name, family, ier) !BIND(C, NAME="cg_family_name_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_family_name_write_f

     SUBROUTINE cg_node_family_write_f(family_name, F, ier) !BIND(C, NAME="cg_node_family_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
       INTEGER :: F
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_family_write_f

     SUBROUTINE cg_node_nfamilies_f(nfamilies, ier) BIND(C, NAME="cg_node_nfamilies_f")
       IMPLICIT NONE
       INTEGER :: nfamilies
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_nfamilies_f

     SUBROUTINE cg_node_family_read_f(F,family_name, nboco, ngeos, ier) !BIND(C, NAME="cg_node_family_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: F
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
       INTEGER :: nboco
       INTEGER :: ngeos
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_family_read_f

     SUBROUTINE cg_node_family_name_write_f(name, family, ier) !BIND(C, NAME="cg_node_family_name_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_family_name_write_f

     SUBROUTINE cg_node_nfamily_names_f(nnames, ier) BIND(C, NAME="cg_node_nfamily_names_f")
       IMPLICIT NONE
       INTEGER :: nnames
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_nfamily_names_f

     SUBROUTINE cg_node_family_name_read_f(N, name, family, ier) !BIND(C, NAME="cg_node_family_name_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: N
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_family_name_read_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write FamBC_t Nodes                                     *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_fambc_read_f(fn, B, F, BC, fambc_name, bocotype, ier) !BIND(C, NAME="cg_fambc_read_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       INTEGER :: BC
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fambc_name
       INTEGER(cgenum_t) :: bocotype
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_fambc_read_f

     SUBROUTINE cg_fambc_write_f(fn, B, F, fambc_name, bocotype, BC, ier) !BIND(C, NAME="cg_fambc_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fambc_name
       INTEGER(cgenum_t) :: bocotype
       INTEGER :: BC
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_fambc_write_f

     SUBROUTINE cg_node_fambc_read_f(BC, fambc_name, bocotype, ier) !BIND(C, NAME="cg_node_fambc_read_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: BC
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fambc_name
       INTEGER(cgenum_t) :: bocotype
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_fambc_read_f

     SUBROUTINE cg_node_fambc_write_f(fambc_name, bocotype, BC, ier) !BIND(C, NAME="cg_node_fambc_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fambc_name
       INTEGER(cgenum_t) :: bocotype
       INTEGER :: BC
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_fambc_write_f


  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GeometryReference_t Nodes                         *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_geo_read_f(fn, B, F, G, geo_name, geo_file, CAD_name, npart, ier) !BIND(C, NAME="cg_geo_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       INTEGER :: G
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_file
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: CAD_name
       INTEGER :: npart
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_geo_read_f

     SUBROUTINE cg_geo_write_f(fn, B, F, geo_name, geo_file, CAD_name, G, ier) !BIND(C, NAME="cg_geo_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_file
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: CAD_name
       INTEGER :: G
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_geo_write_f

     SUBROUTINE cg_node_geo_read_f(G, geo_name, geo_file, CAD_name, npart, ier) !BIND(C, NAME="cg_node_geo_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: G
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_file
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: CAD_name
       INTEGER :: npart
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_geo_read_f

     SUBROUTINE cg_node_geo_write_f(geo_name, geo_file, CAD_name, G, ier) !BIND(C, NAME="cg_node_geo_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_file
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: CAD_name
       INTEGER :: G
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_geo_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GeometryEntity_t Nodes                            *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_part_read_f(fn, B, F,G, P, part_name, ier) !BIND(C, NAME="cg_part_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       INTEGER :: G
       INTEGER :: P
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: part_name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_part_read_f

     SUBROUTINE cg_part_write_f(fn, B, F, G, part_name, P, ier) !BIND(C, NAME="cg_part_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: F
       INTEGER :: G
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: part_name
       INTEGER :: P
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_part_write_f

     SUBROUTINE cg_node_part_read_f(G, P, part_name, ier) !BIND(C, NAME="cg_node_part_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: G
       INTEGER :: P
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: part_name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_part_read_f

     SUBROUTINE cg_node_part_write_f(G, part_name, P, ier) !BIND(C, NAME="cg_node_part_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: G
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: part_name
       INTEGER :: P
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_node_part_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write DiscreteData_t Nodes                              *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_ndiscrete_f(fn, B, Z, ndiscrete, ier) BIND(C, NAME="cg_ndiscrete_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: ndiscrete
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ndiscrete_f

     SUBROUTINE cg_discrete_read_f(fn, B, Z, D, discrete_name, ier) !BIND(C, NAME="cg_discrete_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: D
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: discrete_name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_discrete_read_f

     SUBROUTINE cg_discrete_write_f(fn, B, Z, discrete_name, D, ier) !BIND(C, NAME="cg_discrete_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: discrete_name
       INTEGER :: D
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_discrete_write_f

     SUBROUTINE cg_discrete_size_f(fn, B, Z, D, ndim, dims, ier) BIND(C, NAME="cg_discrete_size_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: D
       INTEGER :: ndim
       INTEGER(CGSIZE_T), DIMENSION(*) :: dims
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_discrete_size_f

     SUBROUTINE cg_discrete_ptset_info_f(fn, B, Z, S, ptype, npnts, ier) BIND(C, NAME="cg_discrete_ptset_info_f")
       IMPORT :: cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER(cgenum_t) :: ptype
       INTEGER(CGSIZE_T) :: npnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_discrete_ptset_info_f

     SUBROUTINE cg_discrete_ptset_read_f( fn, B, Z, S, pnts, ier) BIND(C, NAME="cg_discrete_ptset_read_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER(CGSIZE_T) :: pnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_discrete_ptset_read_f

     SUBROUTINE cg_discrete_ptset_write_f( fn, B, Z, name, location, ptype, &
          npnts, pnts, D, ier) !BIND(C, NAME="cg_discrete_ptset_write_f")
       IMPORT :: CGSIZE_T, cgenum_t, c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: ptype
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T) :: pnts
       INTEGER :: D
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_discrete_ptset_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GridCoordinates_t/DataArray_t Nodes               *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_ncoords_f(fn, B, Z, ncoords, ier) BIND(C, NAME="cg_ncoords_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: ncoords
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ncoords_f

     SUBROUTINE cg_coord_info_f(fn, B, Z, C, TYPE, coordname, ier) !BIND(C, NAME="cg_coord_info_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: C
       INTEGER(cgenum_t) :: TYPE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_coord_info_f

  END INTERFACE

#if HAVE_FORTRAN_2008
  INTERFACE cg_coord_read_f
    SUBROUTINE cg_coord_read_c_double (fn, B, Z, coordname, TYPE, rmin, rmax, coord, ier) !BIND(C, NAME="cg_coord_read_c_double")
      IMPORT :: c_char, cgenum_t, CGSIZE_T, c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
      INTEGER(cgenum_t) :: TYPE
      INTEGER(CGSIZE_T) :: rmin
      INTEGER(CGSIZE_T) :: rmax
      REAL(C_DOUBLE), DIMENSION(*) :: coord
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_coord_read_c_double

    SUBROUTINE cg_coord_read_c_float (fn, B, Z, coordname, TYPE, rmin, rmax, coord, ier) !BIND(C, NAME="cg_coord_read_c_float")
      IMPORT :: c_char, cgenum_t, CGSIZE_T, c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
      INTEGER(cgenum_t) :: TYPE
      INTEGER(CGSIZE_T) :: rmin
      INTEGER(CGSIZE_T) :: rmax
      REAL(C_FLOAT), DIMENSION(*) :: coord
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_coord_read_c_float

    SUBROUTINE cg_coord_read_f03 (fn, B, Z, coordname, TYPE, rmin, rmax, coord, ier) BIND(C, NAME="cg_coord_read_f03")
      IMPORT :: c_char, cgenum_t, CGSIZE_T, c_ptr
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
      INTEGER(cgenum_t) :: TYPE
      INTEGER(CGSIZE_T) :: rmin
      INTEGER(CGSIZE_T) :: rmax
      TYPE(C_PTR), VALUE :: coord
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_coord_read_f03
 END INTERFACE
#endif

 INTERFACE
     SUBROUTINE cg_coord_id_f(fn, B, Z, C, coord_id, ier) BIND(C, NAME="cg_coord_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: C
       REAL(C_DOUBLE) :: coord_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_coord_id_f

!!$    SUBROUTINE cg_coord_write_f(fn, B, Z, TYPE, coordname, coord, C, ier) BIND(C, NAME="")
!!$      IMPORT :: c_char, cgenum_t, c_ptr
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER(cgenum_t) :: TYPE
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
!!$      TYPE(C_PTR), VALUE :: coord
!!$      INTEGER :: C
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_coord_write_f

!!$    SUBROUTINE cg_coord_partial_write_f( fn, B, Z, TYPE, coordname, rmin, rmax, coord, C, ier) BIND(C, NAME="")
!!$      IMPORT :: c_char, cgenum_t, CGSIZE_T, c_ptr
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER(cgenum_t) :: TYPE
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
!!$      INTEGER(CGSIZE_T) :: rmin
!!$      INTEGER(CGSIZE_T) :: rmax
!!$      TYPE(C_PTR) :: coord
!!$      INTEGER :: C
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_coord_partial_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write Elements_t Nodes                                  *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nsections_f(fn, B, Z, nsections, ier) BIND(C, NAME="cg_nsections_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nsections
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nsections_f

     SUBROUTINE cg_section_read_f(fn, B, Z, E, section_name, TYPE, start, END, nbndry, &
          parent_flag, ier) !BIND(C, NAME="cg_section_read_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: E
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name
       INTEGER(cgenum_t) :: TYPE
       INTEGER(CGSIZE_T) ::start
       INTEGER(CGSIZE_T) ::END
       INTEGER :: nbndry
       INTEGER :: parent_flag
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_section_read_f

!!$!!$     SUBROUTINE cg_elements_read_f(fn, B, Z, E, elements, parent_data, ier) BIND(C, NAME="cg_elements_read_f")
!!$       IMPORT :: CGSIZE_T
!!$       IMPLICIT NONE
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       INTEGER :: E
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: elements
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: parent_data
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_elements_read_f
!!$
     SUBROUTINE cg_elementdatasize_f(fn, B, Z, E, ElementDataSize, ier) BIND(C, NAME="cg_elementdatasize_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: E
       INTEGER(CGSIZE_T) :: ElementDataSize
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_elementdatasize_f

     SUBROUTINE cg_elementpartialsize_f(fn, B, Z, E, start, END, ElementDataSize, ier) BIND(C, NAME="cg_elementpartialsize_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: E
       INTEGER(CGSIZE_T) :: start
       INTEGER(CGSIZE_T) :: END
       INTEGER(CGSIZE_T) :: ElementDataSize
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_elementpartialsize_f

!!$!!$     SUBROUTINE cg_section_write_f(fn, B, Z, section_name, TYPE, start, END, nbndry, elements, S, ier) !BIND(C, NAME="cg_section_write_f")
!!$       IMPORT :: c_char, cgenum_t, CGSIZE_T
!!$       IMPLICIT NONE
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name
!!$       INTEGER(cgenum_t) :: TYPE
!!$       INTEGER(CGSIZE_T) :: start
!!$       INTEGER(CGSIZE_T) :: END
!!$       INTEGER :: nbndry
!!$       INTEGER(CGSIZE_T), DIMENSION() :: elements
!!$       INTEGER :: S
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_section_write_f
!!$
!!$!!$     SUBROUTINE cg_parent_data_write_f(fn, B, Z, S, parent_data, ier) BIND(C, NAME="cg_parent_data_write_f")
!!$       IMPORT :: CGSIZE_T
!!$       IMPLICIT NONE
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       INTEGER :: S
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: parent_data
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_parent_data_write_f
!!$
     SUBROUTINE cg_section_partial_write_f( fn, B, Z, section_name, TYPE, start, END, &
          nbndry, S, ier) !BIND(C, NAME="cg_section_partial_write_f")
       IMPORT :: c_char, CGSIZE_T, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name
       INTEGER(cgenum_t) ::TYPE
       INTEGER(CGSIZE_T) ::start
       INTEGER(CGSIZE_T) ::END
       INTEGER :: nbndry
       INTEGER :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_section_partial_write_f

!!$!!$     SUBROUTINE cg_elements_partial_write_f(fn, B, Z, S, rmin, rmax, elements, ier) &
!!$          BIND(C, NAME="cg_elements_partial_write_f")
!!$       IMPORT :: CGSIZE_T
!!$       IMPLICIT NONE
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       INTEGER :: S
!!$       INTEGER(CGSIZE_T) :: rmin
!!$       INTEGER(CGSIZE_T) :: rmax
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: elements
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_elements_partial_write_f
!!$
!!$!!$     SUBROUTINE cg_parent_data_partial_write_f(fn, B, Z, S, rmin, rmax, parent_data, ier) &
!!$          BIND(C, NAME="cg_parent_data_partial_write_f")
!!$       IMPORT :: CGSIZE_T
!!$       IMPLICIT NONE
!!$
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       INTEGER :: S
!!$       INTEGER(CGSIZE_T) :: rmin
!!$       INTEGER(CGSIZE_T) :: rmax
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: parent_data
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_parent_data_partial_write_f
!!$!!$
!!$!!$     SUBROUTINE cg_elements_partial_read_f(fn, B, Z, S, rmin, rmax, elements, parent, ier) &
!!$          BIND(C, NAME="cg_elements_partial_read_f")
!!$       IMPORT :: CGSIZE_T
!!$       IMPLICIT NONE
!!$
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       INTEGER :: S
!!$       INTEGER(CGSIZE_T) ::rmin
!!$       INTEGER(CGSIZE_T) ::rmax
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: elements
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: parent
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_elements_partial_read_f
!!$
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write FlowSolution_t Nodes                              *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nsols_f(fn, B, Z, nsols, ier) BIND(C, NAME="cg_nsols_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nsols
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nsols_f

     SUBROUTINE cg_sol_info_f(fn, B, Z, S, solname, location, ier) !BIND(C, NAME="cg_sol_info_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: solname
       INTEGER(cgenum_t) :: location
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_info_f

     SUBROUTINE cg_sol_id_f(fn, B, Z, S, sol_id, ier) BIND(C, NAME="cg_sol_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       REAL(C_DOUBLE) :: sol_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_id_f

     SUBROUTINE cg_sol_write_f(fn, B, Z, solname, location, S, ier) !BIND(C, NAME="cg_sol_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: solname
       INTEGER(cgenum_t) :: location
       INTEGER :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_write_f

     SUBROUTINE cg_sol_size_f(fn, B, Z, S, ndim, dims, ier) BIND(C, NAME="cg_sol_size_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER :: ndim
       INTEGER(CGSIZE_T), DIMENSION(*) :: dims
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_size_f

     SUBROUTINE cg_sol_ptset_info_f( fn, B, Z, S, ptype, npnts, ier) BIND(C, NAME="cg_sol_ptset_info_f")
       IMPORT :: cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER(cgenum_t) :: ptype
       INTEGER(CGSIZE_T) :: npnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_ptset_info_f

     SUBROUTINE cg_sol_ptset_read_f(fn, B, Z, S, pnts, ier) BIND(C, NAME="cg_sol_ptset_read_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER(CGSIZE_T) ::pnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_ptset_read_f

     SUBROUTINE cg_sol_ptset_write_f(fn, B, Z, name, location, ptype, npnts, pnts, S, ier) !BIND(C, NAME="cg_sol_ptset_write_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: ptype
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T) :: pnts
       INTEGER :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_sol_ptset_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write solution DataArray_t Nodes                        *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nfields_f(fn, B, Z, S, nfields, ier) BIND(C, NAME="cg_nfields_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER :: nfields
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nfields_f

     SUBROUTINE cg_field_info_f(fn, B, Z, S, F, TYPE, fieldname, ier) !BIND(C, NAME="cg_field_info_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER :: F
       INTEGER(cgenum_t) :: TYPE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_field_info_f

!!$INTERFACE
!!$   SUBROUTINE cg_field_read_f(fn, B, Z, S, fieldname), TYPE, rmin, rmax, field_ptr, ier) BIND(C, NAME="")
!!$     INTEGER :: fn
!!$     INTEGER :: B,
!!$     INTEGER :: Z
!!$     INTEGER :: S, CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname), INTEGER(cgenum_t) :: TYPE
!!$     INTEGER(CGSIZE_T) ::rmin,
!!$     INTEGER(CGSIZE_T) ::rmax
!!$     void *field_ptr,
!!$     INTEGER, INTENT(OUT) :: ier
!!$   END SUBROUTINE cg_field_read_f
!!$END INTERFACE

     SUBROUTINE cg_field_id_f(fn, B, Z, S, F, field_id, ier) !BIND(C, NAME="cg_field_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER :: F
       REAL(C_DOUBLE) :: field_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_field_id_f

!!$   SUBROUTINE cg_field_write_f(fn, B, Z, S, TYPE, fieldname, field_ptr, F, ier) BIND(C, NAME="")
!!$     INTEGER :: fn
!!$     INTEGER :: B,
!!$     INTEGER :: Z
!!$     INTEGER :: S
!!$     INTEGER(cgenum_t) :: TYPE
!!$     CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
!!$     void *field_ptr,
!!$     INTEGER :: F
!!$     INTEGER, INTENT(OUT) :: ier
!!$   END SUBROUTINE cg_field_write_f

!!$   SUBROUTINE cg_field_partial_write_f) (fn, B, Z, S, TYPE, fieldname, rmin, rmax, void *field_ptr, F, ier) BIND(C, NAME="")
!!$     INTEGER :: fn,
!!$     INTEGER :: B
!!$     INTEGER :: Z
!!$     INTEGER :: S, INTEGER(cgenum_t) :: TYPE
!!$     CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
!!$     INTEGER(CGSIZE_T) :: rmin
!!$     INTEGER(CGSIZE_T) :: rmax
!!$     void *field_ptr
!!$     INTEGER :: F
!!$     INTEGER, INTENT(OUT) :: ier
!!$   END SUBROUTINE cg_field_partial_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write ZoneSubRegion_t Nodes                              *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nsubregs_f(fn, B, Z, nsubreg, ier) BIND(C, NAME="cg_nsubregs_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nsubreg
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nsubregs_f

     SUBROUTINE cg_subreg_info_f(fn, B, Z, S, regname, DIMENSION, &
          location, ptset_type, npnts, bcname_len, gcname_len, ier) !BIND(C, NAME="cg_subreg_info_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
       INTEGER :: DIMENSION
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) ::npnts
       INTEGER :: bcname_len
       INTEGER :: gcname_len
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_info_f

     SUBROUTINE cg_subreg_ptset_read_f( fn, B, Z, S, pnts, ier) BIND(C, NAME="cg_subreg_ptset_read_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_ptset_read_f

     SUBROUTINE cg_subreg_bcname_read_f( fn, B, Z, S, bcname, ier) !BIND(C, NAME="cg_subreg_bcname_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bcname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_bcname_read_f

     SUBROUTINE cg_subreg_gcname_read_f(fn, B, Z, S, gcname, ier) !BIND(C, NAME="cg_subreg_gcname_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gcname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_gcname_read_f

     SUBROUTINE cg_subreg_ptset_write_f(fn, B, Z, regname, DIMENSION, location, ptset_type, npnts, &
          pnts, S, ier) !BIND(C, NAME="cg_subreg_ptset_write_f")
       IMPORT :: cgenum_t, c_char, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
       INTEGER :: DIMENSION
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       INTEGER :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_ptset_write_f

     SUBROUTINE cg_subreg_bcname_write_f( fn, B, Z, regname, DIMENSION, bcname, S, ier) !BIND(C, NAME="cg_subreg_bcname_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
       INTEGER :: DIMENSION
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bcname
       INTEGER :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_bcname_write_f

     SUBROUTINE cg_subreg_gcname_write_f( fn, B, Z, regname, DIMENSION, gcname, S, ier) !BIND(C, NAME="cg_subreg_gcname_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
       INTEGER :: DIMENSION
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gcname
       INTEGER :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_subreg_gcname_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write ZoneGridConnectivity_t Nodes                       *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nzconns_f(fn, B, Z, nzconns, ier) BIND(C, NAME="cg_nzconns_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nzconns
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nzconns_f

     SUBROUTINE cg_zconn_read_f(fn, B, Z, C, name, ier) !BIND(C, NAME="cg_zconn_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: C
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zconn_read_f

     SUBROUTINE cg_zconn_write_f(fn, B, Z, name, C, ier) !BIND(C, NAME="cg_zconn_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       INTEGER :: C
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zconn_write_f

     SUBROUTINE cg_zconn_get_f(fn, B, Z, C, ier) BIND(C, NAME="cg_zconn_get_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: C
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zconn_get_f

     SUBROUTINE cg_zconn_set_f(fn, B, Z, C, ier) BIND(C, NAME="cg_zconn_set_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: C
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_zconn_set_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write OversetHoles_t Nodes                              *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nholes_f(fn, B, Z, nholes, ier) BIND(C, NAME="cg_nholes_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nholes
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nholes_f

     SUBROUTINE cg_hole_info_f(fn, B, Z, I, holename, location, ptset_type, nptsets, npnts, ier) !BIND(C, NAME="cg_hole_info_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: holename
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: nptsets
       INTEGER(CGSIZE_T) :: npnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_hole_info_f

     SUBROUTINE cg_hole_read_f(fn, B, Z, I, pnts, ier) BIND(C, NAME="cg_hole_read_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_hole_read_f

     SUBROUTINE cg_hole_id_f(fn, B, Z, I, hole_id, ier) BIND(C, NAME="cg_hole_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_DOUBLE) :: hole_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_hole_id_f

     SUBROUTINE cg_hole_write_f(fn, B, Z, holename, location, ptset_type, nptsets, npnts, &
          pnts, I, ier) !BIND(C, NAME="cg_hole_write_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: holename
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: ptset_type
       INTEGER :: nptsets
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       INTEGER :: I
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_hole_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GridConnectivity_t Nodes                          *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nconns_f(fn, B, Z, nconns, ier) BIND(C, NAME="cg_nconns_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nconns
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nconns_f

     SUBROUTINE cg_conn_info_f(fn, B, Z, I, connectname, location, &
          TYPE, ptset_type, npnts, donorname, donor_zonetype, donor_ptset_type, &
          donor_datatype, ndata_donor, ier) !BIND(C, NAME="cg_conn_info_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: TYPE
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
       INTEGER(cgenum_t) :: donor_zonetype
       INTEGER(cgenum_t) :: donor_ptset_type
       INTEGER(cgenum_t) :: donor_datatype
       INTEGER(CGSIZE_T) :: ndata_donor
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_info_f

     SUBROUTINE cg_conn_read_f(fn, B, Z, I, pnts, donor_datatype, donor_data, ier) BIND(C, NAME="cg_conn_read_f")
       IMPORT :: cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       INTEGER(cgenum_t) :: donor_datatype
       INTEGER(CGSIZE_T), DIMENSION(*) :: donor_data
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_read_f

     SUBROUTINE cg_conn_read_short_f(fn, B, Z, I, pnts, ier) BIND(C, NAME="cg_conn_read_short_f")
       IMPORT :: CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(CGSIZE_T) ::pnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_read_short_f

     SUBROUTINE cg_conn_id_f(fn, B, Z, I, conn_id, ier) BIND(C, NAME="cg_conn_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_DOUBLE) :: conn_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_id_f

     SUBROUTINE cg_conn_write_f(fn, B, Z, connectname, location, TYPE, ptset_type, &
          npnts, pnts, donorname, donor_zonetype, donor_ptset_type, &
          donor_datatype, ndata_donor, donor_data, I, ier) !BIND(C, NAME="cg_conn_write_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: TYPE
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
       INTEGER(cgenum_t) :: donor_zonetype
       INTEGER(cgenum_t) :: donor_ptset_type
       INTEGER(cgenum_t) :: donor_datatype
       INTEGER(CGSIZE_T) :: ndata_donor
       INTEGER(CGSIZE_T), DIMENSION(*) :: donor_data
       INTEGER :: I
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_write_f

     SUBROUTINE cg_conn_write_short_f(fn, B, Z, connectname, location, &
          TYPE, ptset_type, npnts, pnts, donorname, I, ier) !BIND(C, NAME="cg_conn_write_short_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
       INTEGER(cgenum_t) :: location
       INTEGER(cgenum_t) :: TYPE
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T) :: pnts
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
       INTEGER :: I
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_write_short_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GridConnectivity1to1_t Nodes in a zone            *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_n1to1_f(fn, B, Z, n1to1, ier) BIND(C, NAME="cg_n1to1_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: n1to1
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_n1to1_f

     SUBROUTINE cg_1to1_read_f(fn, B, Z, I, connectname, donorname, &
          range, donor_range, transform, ier) !BIND(C, NAME="cg_1to1_read_f")
       IMPORT :: c_char, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
       INTEGER(CGSIZE_T), DIMENSION(*) :: range
       INTEGER(CGSIZE_T), DIMENSION(*) :: donor_range
       INTEGER, DIMENSION(*) :: transform
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_read_f

     SUBROUTINE cg_1to1_id_f(fn, B, Z, I, one21_id, ier) !BIND(C, NAME="cg_1to1_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_DOUBLE) :: one21_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_id_f

     SUBROUTINE cg_1to1_write_f(fn, B, Z, connectname, donorname, range, &
          donor_range, transform, I, ier) !BIND(C, NAME="cg_1to1_write_f")
       IMPORT :: c_char, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
       INTEGER(CGSIZE_T), DIMENSION(*) :: range
       INTEGER(CGSIZE_T), DIMENSION(*) :: donor_range
       INTEGER, DIMENSION(*) :: transform
       INTEGER :: I
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read all GridConnectivity1to1_t Nodes of a base                  *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_n1to1_global_f(fn, B, n1to1_global, ier) BIND(C, NAME="cg_n1to1_global_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: n1to1_global
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_n1to1_global_f

     SUBROUTINE cg_1to1_read_global_f(fn, B, connectname, zonename, donorname, &
          range, donor_range, transform, ier) !BIND(C, NAME="cg_1to1_read_global_f")
       IMPORT :: c_char, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zonename
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
       INTEGER(CGSIZE_T), DIMENSION(*) :: range
       INTEGER(CGSIZE_T), DIMENSION(*) :: donor_range
       INTEGER, DIMENSION(*) :: transform
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_read_global_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write BC_t Nodes                                        *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_nbocos_f(fn, B, Z, nbocos, ier) BIND(C, NAME="cg_nbocos_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: nbocos
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nbocos_f

     SUBROUTINE cg_boco_info_f(fn, B, Z, BC, boconame, bocotype, &
          ptset_type, npnts, NormalIndex, &
          NormalListSize, NormalDataType, ndataset, ier) !BIND(C, NAME="cg_boco_info_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: boconame
       INTEGER(cgenum_t) :: bocotype
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       INTEGER, DIMENSION(*) :: NormalIndex
       INTEGER(CGSIZE_T) :: NormalListSize
       INTEGER(cgenum_t) :: NormalDataType
       INTEGER :: ndataset
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_boco_info_f

!!$ INTERFACE
!!$    SUBROUTINE cg_boco_read_f(fn, B, Z, BC, pnts, NormalList, ier) BIND(C, NAME="")
!!$      IMPORT :: CGSIZE_T
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: BC
!!$      INTEGER(CGSIZE_T) :: pnts
!!$      void *NormalList
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_boco_read_f
!!$ END INTERFACE

     SUBROUTINE cg_boco_id_f(fn, B, Z, BC, boco_id, ier) BIND(C, NAME="cg_boco_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       REAL(C_DOUBLE) :: boco_id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_boco_id_f

     SUBROUTINE cg_boco_write_f(fn, B, Z, boconame, bocotype, ptset_type, npnts, pnts, BC, ier) !BIND(C, NAME="cg_boco_write_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: boconame
       INTEGER(cgenum_t) :: bocotype
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
       INTEGER :: BC
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_boco_write_f

!!$    SUBROUTINE cg_boco_normal_write_f( fn, B, Z, BC, NormalIndex, NormalListFlag,
!!$      NormalDataType, NormalList, ier) BIND(C, NAME="")
!!$
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: BC,
!!$      INTEGER(CGSIZE_T) ::NormalIndex
!!$      INTEGER :: NormalListFlag,
!!$      INTEGER(cgenum_t) :: NormalDataType
!!$      ! void *NormalList,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_boco_normal_write_f

     SUBROUTINE cg_boco_gridlocation_read_f(fn, B, Z, BC, location, ier) BIND(C, NAME="cg_boco_gridlocation_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER(cgenum_t) :: location
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_boco_gridlocation_read_f

     SUBROUTINE cg_boco_gridlocation_write_f( fn, B, Z, BC, location, ier) BIND(C, NAME="cg_boco_gridlocation_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER(cgenum_t) :: location
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_boco_gridlocation_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write BCProperty_t/WallFunction_t Nodes                 *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_bc_wallfunction_read_f(fn, B, Z, BC, WallFunctionType, ier) BIND(C, NAME="cg_bc_wallfunction_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER(cgenum_t) :: WallFunctionType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bc_wallfunction_read_f

     SUBROUTINE cg_bc_wallfunction_write_f(fn, B, Z, BC, WallFunctionType, ier) BIND(C, NAME="cg_bc_wallfunction_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER(cgenum_t) :: WallFunctionType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bc_wallfunction_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write BCProperty_t/Area_t Nodes                         *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_bc_area_read_f(fn, B, Z, BC, AreaType, SurfaceArea, RegionName, ier) !BIND(C, NAME="cg_bc_area_read_f")
       IMPORT :: c_char, cgenum_t, c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER(cgenum_t) :: AreaType
       REAL(C_FLOAT) :: SurfaceArea
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: RegionName
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bc_area_read_f

     SUBROUTINE cg_bc_area_write_f(fn, B, Z, BC, AreaType, SurfaceArea, RegionName, ier) !BIND(C, NAME="cg_bc_area_write_f")
       IMPORT :: c_char, cgenum_t, c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER(cgenum_t) :: AreaType
       REAL(C_FLOAT) :: SurfaceArea
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: RegionName
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bc_area_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_conn_periodic_read_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier) &
          BIND(C, NAME="cg_conn_periodic_read_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_FLOAT), DIMENSION(*) :: RotationCenter
       REAL(C_FLOAT), DIMENSION(*) :: RotationAngle
       REAL(C_FLOAT), DIMENSION(*) :: Translation
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_periodic_read_f

     SUBROUTINE cg_conn_periodic_write_f( fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier) &
          BIND(C, NAME="cg_conn_periodic_write_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_FLOAT), DIMENSION(*)  :: RotationCenter
       REAL(C_FLOAT), DIMENSION(*)  :: RotationAngle
       REAL(C_FLOAT), DIMENSION(*)  :: Translation
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_periodic_write_f

     SUBROUTINE cg_1to1_periodic_read_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier) &
          BIND(C, NAME="cg_1to1_periodic_read_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_FLOAT), DIMENSION(*)  :: RotationCenter
       REAL(C_FLOAT), DIMENSION(*)  :: RotationAngle
       REAL(C_FLOAT), DIMENSION(*)  :: Translation
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_periodic_read_f

     SUBROUTINE cg_1to1_periodic_write_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier) &
          BIND(C, NAME="cg_1to1_periodic_write_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       REAL(C_FLOAT), DIMENSION(*)  :: RotationCenter
       REAL(C_FLOAT), DIMENSION(*)  :: RotationAngle
       REAL(C_FLOAT), DIMENSION(*)  :: Translation
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_periodic_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_conn_average_read_f(fn, B, Z, I, AverageInterfaceType, ier) &
          BIND(C, NAME="cg_conn_average_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(cgenum_t) :: AverageInterfaceType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_average_read_f

     SUBROUTINE cg_conn_average_write_f(fn, B, Z, I, AverageInterfaceType, ier) &
          BIND(C, NAME="cg_conn_average_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(cgenum_t) :: AverageInterfaceType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conn_average_write_f

     SUBROUTINE cg_1to1_average_read_f(fn, B, Z, I, AverageInterfaceType, ier) &
          BIND(C, NAME="cg_1to1_average_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(cgenum_t) :: AverageInterfaceType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_average_read_f

     SUBROUTINE cg_1to1_average_write_f(fn, B, Z, I,AverageInterfaceType, ier) BIND(C, NAME="cg_1to1_average_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: I
       INTEGER(cgenum_t) :: AverageInterfaceType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_1to1_average_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write BCDataSet_t Nodes                                 *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_dataset_read_f(fn, B, Z, BC, DSet, Dataset_name, BCType, DirichletFlag, &
          NeumannFlag, ier) !BIND(C, NAME="cg_dataset_read_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER :: DSet
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
       INTEGER(cgenum_t) :: BCType
       INTEGER :: DirichletFlag
       INTEGER :: NeumannFlag
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_dataset_read_f

     SUBROUTINE cg_dataset_write_f(fn, B, Z, BC, Dataset_name,BCType, Dset, ier) !BIND(C, NAME="cg_dataset_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
       INTEGER(cgenum_t) :: BCType
       INTEGER :: Dset
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_dataset_write_f

     SUBROUTINE cg_bcdataset_write_f(Dataset_name, BCType, BCDataType, ier) !BIND(C, NAME="cg_bcdataset_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
       INTEGER(cgenum_t) :: BCType
       INTEGER(cgenum_t) :: BCDataType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bcdataset_write_f

     SUBROUTINE cg_bcdataset_info_f(ndataset, ier) BIND(C, NAME="cg_bcdataset_info_f")
       IMPLICIT NONE
       INTEGER :: ndataset
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bcdataset_info_f

     SUBROUTINE cg_bcdataset_read_f(index, Dataset_name, BCType, &
          DirichletFlag, NeumannFlag,ier) !BIND(C, NAME="cg_bcdataset_read_f")
       IMPORT :: c_char, cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER ::index
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
       INTEGER(cgenum_t) :: BCType
       INTEGER :: DirichletFlag
       INTEGER :: NeumannFlag
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bcdataset_read_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write BCData_t Nodes                                    *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_bcdata_write_f(fn, B, Z, BC, Dset, BCDataType, ier) BIND(C, NAME="cg_bcdata_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: BC
       INTEGER :: Dset
       INTEGER(cgenum_t) :: BCDataType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_bcdata_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write RigidGridMotion_t Nodes                           *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_n_rigid_motions_f(fn, B, Z, n_rigid_motions, ier) BIND(C, NAME="cg_n_rigid_motions_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: n_rigid_motions
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_n_rigid_motions_f

     SUBROUTINE cg_rigid_motion_read_f(fn, B, Z, R, rmotion_name, TYPE, ier) !BIND(C, NAME="cg_rigid_motion_read_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: R
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: rmotion_name
       INTEGER(cgenum_t) :: TYPE
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_rigid_motion_read_f

     SUBROUTINE cg_rigid_motion_write_f(fn, B, Z, rmotion_name, TYPE, R, ier) !BIND(C, NAME="cg_rigid_motion_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: rmotion_name
       INTEGER(cgenum_t) :: TYPE
       INTEGER :: R
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_rigid_motion_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write ArbitraryGridMotion_t Nodes                       *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_n_arbitrary_motions_f( fn, B, Z, n_arbitrary_motions, ier) BIND(C, NAME="cg_n_arbitrary_motions_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: n_arbitrary_motions
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_n_arbitrary_motions_f

     SUBROUTINE cg_arbitrary_motion_read_f(fn, B, Z, A, amotion_name, TYPE, ier) !BIND(C, NAME="cg_arbitrary_motion_read_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: A
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: amotion_name
       INTEGER(cgenum_t) :: TYPE
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_arbitrary_motion_read_f

     SUBROUTINE cg_arbitrary_motion_write_f(fn, B, Z, amotion_name, TYPE, A, ier) !BIND(C, NAME="cg_arbitrary_motion_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: amotion_name
       INTEGER(cgenum_t) :: TYPE
       INTEGER :: A
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_arbitrary_motion_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write GridCoordinates_t Nodes                           *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_ngrids_f(fn, B, Z, ngrids, ier) BIND(C, NAME="cg_ngrids_f")
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: ngrids
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ngrids_f

     SUBROUTINE cg_grid_read_f(fn, B, Z, G, gridname, ier) ! BIND(C, NAME="cg_grid_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: G
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gridname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_grid_read_f

     SUBROUTINE cg_grid_write_f(fn, B, Z, gridname, G, ier) !BIND(C, NAME="cg_grid_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gridname
       INTEGER :: G
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_grid_write_f

!!$    SUBROUTINE cg_grid_bounding_box_read_f(fn, B, Z, G, datatype, array, ier) ! BIND(C, NAME="cg_grid_bounding_box_read_f")
!!$      IMPORT :: cgenum_t
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: G
!!$      INTEGER(cgenum_t), INTENT(IN) :: datatype
!!$      TYPE(*), DIMENSION(*) :: array
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_grid_bounding_box_read_f
 
!!$!!$     SUBROUTINE cg_grid_bounding_box_write_f(fn, B, Z, G, datatype, array, ier) !BIND(C, NAME="cg_grid_bounding_box_write_f")
!!$       IMPORT :: cgenum_t, c_ptr
!!$       IMPLICIT NONE
!!$       INTEGER :: fn
!!$       INTEGER :: B
!!$       INTEGER :: Z
!!$       INTEGER :: G
!!$       INTEGER(cgenum_t), INTENT(IN) :: datatype
!!$       void *array
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_grid_bounding_box_write_f
!!$
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write SimulationType_t Node                             *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_simulation_type_read_f(fn, B, TYPE, ier) BIND(C, NAME="cg_simulation_type_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER(cgenum_t) :: TYPE
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_simulation_type_read_f

     SUBROUTINE cg_simulation_type_write_f(fn, B, TYPE, ier) BIND(C, NAME="cg_simulation_type_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER(cgenum_t) :: TYPE
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_simulation_type_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write BaseIterativeData_t Node                          *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_biter_read_f(fn, B, bitername, nsteps, ier) !BIND(C, NAME="cg_biter_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bitername
       INTEGER :: nsteps
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_biter_read_f

     SUBROUTINE cg_biter_write_f(fn, B, bitername, nsteps, ier) !BIND(C, NAME="cg_biter_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bitername
       INTEGER :: nsteps
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_biter_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write ZoneIterativeData_t Nodes                         *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_ziter_read_f(fn, B, Z, zitername, ier) !BIND(C, NAME="cg_ziter_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zitername
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ziter_read_f


     SUBROUTINE cg_ziter_write_f(fn, B, Z, zitername, ier) !BIND(C, NAME="cg_ziter_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zitername
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ziter_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write Gravity_t Node                                    *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_gravity_read_f(fn, B, gravity_vector, ier) BIND(C, NAME="cg_gravity_read_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       REAL(C_FLOAT), DIMENSION(*) :: gravity_vector
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_gravity_read_f

     SUBROUTINE cg_gravity_write_f(fn, B, gravity_vector, ier) BIND(C, NAME="cg_gravity_write_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       REAL(C_FLOAT), DIMENSION(*) :: gravity_vector
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_gravity_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write Axisymmetry_t Node                                *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_axisym_read_f(fn, B, ref_point, axis, ier) BIND(C, NAME="cg_axisym_read_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       REAL(C_FLOAT), DIMENSION(*) :: ref_point
       REAL(C_FLOAT), DIMENSION(*) :: axis
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_axisym_read_f

     SUBROUTINE cg_axisym_write_f(fn, B, ref_point, axis, ier) BIND(C, NAME="cg_axisym_write_f")
       IMPORT :: c_float
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       REAL(C_FLOAT), DIMENSION(*) :: ref_point
       REAL(C_FLOAT), DIMENSION(*) :: axis
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_axisym_write_f


  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write RotatingCoordinates_t Node                        *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_rotating_read_f(rot_rate, rot_center, ier) BIND(C, NAME="cg_rotating_read_f")
       IMPORT :: c_float
       IMPLICIT NONE
       REAL(C_FLOAT), DIMENSION(*) :: rot_rate
       REAL(C_FLOAT), DIMENSION(*) :: rot_center
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_rotating_read_f

     SUBROUTINE cg_rotating_write_f(rot_rate, rot_center, ier) BIND(C, NAME="cg_rotating_write_f")
       IMPORT :: c_float
       IMPLICIT NONE
       REAL(C_FLOAT), DIMENSION(*) :: rot_rate
       REAL(C_FLOAT), DIMENSION(*) :: rot_center
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_rotating_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Read and write  IndexArray/Range_t Nodes                         *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_ptset_info_f(ptset_type, npnts, ier) BIND(C, NAME="cg_ptset_info_f")
       IMPORT :: cgenum_t, CGSIZE_T
       IMPLICIT NONE
       INTEGER(cgenum_t) :: ptset_type
       INTEGER(CGSIZE_T) :: npnts
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ptset_info_f

!!$!!$     SUBROUTINE cg_ptset_read_f(pnts, ier) BIND(C, NAME="cg_ptset_read_f")
!!$       IMPORT :: cgenum_t, CGSIZE_T
!!$       IMPLICIT NONE
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_ptset_read_f
!!$
!!$!!$     SUBROUTINE cg_ptset_write_f(ptset_type, npnts, pnts, ier) BIND(C, NAME="cg_ptset_write_f")
!!$       IMPORT :: cgenum_t, CGSIZE_T
!!$       IMPLICIT NONE
!!$       INTEGER(cgenum_t) :: ptset_type
!!$       INTEGER(CGSIZE_T) :: npnts
!!$       INTEGER(CGSIZE_T), DIMENSION() :: pnts
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cg_ptset_write_f
!!$
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Go - To Function                                                 *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_goto_f1(fn, B, ier, name1, index1)
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER, INTENT(OUT) :: ier
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name1
       INTEGER :: index1
     END SUBROUTINE

     SUBROUTINE cg_gorel_f1(fn, ier, name1, index1)
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER, INTENT(OUT) :: ier
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name1
       INTEGER :: index1
     END SUBROUTINE

!!$  SUBROUTINE cg_goto_f, CG_GOTO_F)(cgint_f *fn, cgint_f *B, ier, ...)
!!$    cgint_f *fn, cgint_f *B, ier, ...)
!!$
!!$    INTEGER, INTENT(OUT) :: ier
!!$  END SUBROUTINE
!!$
!!$  SUBROUTINE cg_gorel_f, CG_GOREL_F)(cgint_f *fn, ier, ...)
!!$    cgint_f *fn, ier, ...)
!!$    INTEGER, INTENT(OUT) :: ier
!!$  END SUBROUTINE
!!$
     SUBROUTINE cg_gopath_f(fn,path, ier) !BIND(C, NAME="cg_gopath_f")
       IMPORT :: C_CHAR
       IMPLICIT NONE
       INTEGER :: fn
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: path
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_gopath_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !              Read Multiple path nodes                                 *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_famname_read_f(famname, ier) !BIND(C, NAME="cg_famname_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: famname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_famname_read_f

     SUBROUTINE cg_nmultifam_f(nfam, ier) BIND(C, NAME="cg_nmultifam_f")
       IMPLICIT NONE
       INTEGER :: nfam
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nmultifam_f

     SUBROUTINE cg_multifam_read_f(N,name, family, ier) !BIND(C, NAME="cg_multifam_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: N
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_multifam_read_f

     SUBROUTINE cg_convergence_read_f(iterations, NormDefinitions, ier) !BIND(C, NAME="cg_convergence_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: iterations
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: NormDefinitions
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_convergence_read_f

     SUBROUTINE cg_state_size_f(size, ier) !BIND(C, NAME="cg_state_size_f")
       IMPLICIT NONE
       INTEGER :: size
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_state_size_f

     SUBROUTINE cg_state_read_f(StateDescription, ier) !BIND(C, NAME="cg_state_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: StateDescription
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_state_read_f

     SUBROUTINE cg_equationset_read_f(EquationDimension, GoverningEquationsFlag, &
          GasModelFlag, ViscosityModelFlag, ThermalConductivityModelFlag, &
          TurbulenceClosureFlag, TurbulenceModelFlag, ier) BIND(C, NAME="cg_equationset_read_f")
       IMPLICIT NONE
       INTEGER :: EquationDimension
       INTEGER :: GoverningEquationsFlag
       INTEGER :: GasModelFlag
       INTEGER :: ViscosityModelFlag
       INTEGER :: ThermalConductivityModelFlag
       INTEGER :: TurbulenceClosureFlag
       INTEGER :: TurbulenceModelFlag
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_equationset_read_f

     SUBROUTINE cg_equationset_chemistry_read_f(ThermalRelaxationFlag, ChemicalKineticsFlag, ier) &
          BIND(C, NAME="cg_equationset_chemistry_read_f")
       IMPLICIT NONE
       INTEGER :: ThermalRelaxationFlag
       INTEGER :: ChemicalKineticsFlag
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_equationset_chemistry_read_f

     SUBROUTINE cg_equationset_elecmagn_read_f(ElecFldModelFlag, MagnFldModelFlag, &
          ConductivityModelFlag, ier) BIND(C, NAME="cg_equationset_elecmagn_read_f")
       IMPLICIT NONE
       INTEGER :: ElecFldModelFlag
       INTEGER :: MagnFldModelFlag
       INTEGER :: ConductivityModelFlag
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_equationset_elecmagn_read_f

     SUBROUTINE cg_governing_read_f(EquationsType, ier) BIND(C, NAME="cg_governing_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: EquationsType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_governing_read_f

     SUBROUTINE cg_diffusion_read_f(diffusion_model, ier) BIND(C, NAME="cg_diffusion_read_f")
       IMPLICIT NONE
       INTEGER, DIMENSION(*) :: diffusion_model
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_diffusion_read_f

     SUBROUTINE cg_model_read_f(ModelLabel, ModelType, ier) !BIND(C, NAME="cg_model_read_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ModelLabel
       INTEGER(cgenum_t) :: ModelType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_model_read_f

     SUBROUTINE cg_narrays_f(narrays, ier) BIND(C, NAME="cg_narrays_f")
       IMPLICIT NONE
       INTEGER :: narrays
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_narrays_f

     SUBROUTINE cg_array_info_f(A, ArrayName, DataType, DataDimension, DimensionVector, ier) !BIND(C, NAME="cg_array_info_f")
       IMPORT :: c_char, CGSIZE_T, cgenum_t
       IMPLICIT NONE
       INTEGER :: A
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName
       INTEGER(cgenum_t) :: DataType
       INTEGER :: DataDimension
       INTEGER(CGSIZE_T), DIMENSION(*) :: DimensionVector
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_array_info_f

!!$    SUBROUTINE cg_array_read_f(A, DATA, ier) BIND(C, NAME="")
!!$      INTEGER :: A,
!!$      void *DATA,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_read_f

!!$    SUBROUTINE cg_array_read_as_f(A, TYPE, DATA, ier) BIND(C, NAME="")
!!$      IMPORT :: cgenum_t
!!$      IMPLICIT NONE
!!$      INTEGER :: A
!!$      INTEGER(cgenum_t) :: TYPE
!!$      void *DATA
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_read_as_f

     SUBROUTINE cg_nintegrals_f(nintegrals, ier) BIND(C, NAME="cg_nintegrals_f")
       IMPLICIT NONE
       INTEGER :: nintegrals
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nintegrals_f

     SUBROUTINE cg_integral_read_f(IntegralDataIndex, IntegralDataName,ier) !BIND(C, NAME="cg_integral_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: IntegralDataIndex
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: IntegralDataName
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_integral_read_f

     SUBROUTINE cg_rind_read_f(RindData, ier) BIND(C, NAME="cg_rind_read_f")
       IMPLICIT NONE
       INTEGER, DIMENSION(*) :: RindData
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_rind_read_f

     SUBROUTINE cg_ndescriptors_f(ndescriptors, ier) BIND(C, NAME="cg_ndescriptors_f")
       IMPLICIT NONE
       INTEGER :: ndescriptors
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ndescriptors_f

     SUBROUTINE cg_descriptor_size_f(descr_no, descr_size, ier) BIND(C, NAME="cg_descriptor_size_f")
       IMPLICIT NONE
       INTEGER :: descr_no
       INTEGER :: descr_size
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_descriptor_size_f

     SUBROUTINE cg_descriptor_read_f(descr_no, descr_name, descr_text, ier) !BIND(C, NAME="cg_descriptor_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: descr_no
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_text
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_descriptor_read_f

     SUBROUTINE cg_nunits_f(nunits, ier) BIND(C, NAME="cg_nunits_f")
       IMPLICIT NONE
       INTEGER :: nunits
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nunits_f

     SUBROUTINE cg_units_read_f(mass, length,  time, temperature, angle, ier) BIND(C, NAME="cg_units_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: mass
       INTEGER(cgenum_t) :: length
       INTEGER(cgenum_t) :: time
       INTEGER(cgenum_t) :: temperature
       INTEGER(cgenum_t) :: angle
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_units_read_f

     SUBROUTINE cg_unitsfull_read_f(mass, length, time, temperature, angle, current, &
          amount, intensity, ier) BIND(C, NAME="cg_unitsfull_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: mass
       INTEGER(cgenum_t) :: length
       INTEGER(cgenum_t) :: time
       INTEGER(cgenum_t) :: temperature
       INTEGER(cgenum_t) :: angle
       INTEGER(cgenum_t) :: current
       INTEGER(cgenum_t) :: amount
       INTEGER(cgenum_t) :: intensity
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_unitsfull_read_f

     SUBROUTINE cg_exponents_info_f(DataType, ier) BIND(C, NAME="cg_exponents_info_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: DataType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_exponents_info_f

     SUBROUTINE cg_nexponents_f(nexps, ier) BIND(C, NAME="cg_nexponents_f")
       IMPLICIT NONE
       INTEGER :: nexps
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nexponents_f

!!$    SUBROUTINE cg_exponents_read_f(void *exponents, ier) BIND(C, NAME="")
!!$      void *exponents
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_exponents_read_f

!!$    SUBROUTINE cg_expfull_read_f(exponents, ier) BIND(C, NAME="")
!!$      void *exponents,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_expfull_read_f

     SUBROUTINE cg_conversion_info_f(DataType, ier) BIND(C, NAME="cg_conversion_info_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: DataType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_conversion_info_f

!!$    SUBROUTINE cg_conversion_read_f(ConversionFactors, ier) BIND(C, NAME="")
!!$      void *ConversionFactors,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_conversion_read_f

     SUBROUTINE cg_dataclass_read_f(dataclass, ier) BIND(C, NAME="cg_dataclass_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: dataclass
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_dataclass_read_f

     SUBROUTINE cg_gridlocation_read_f(GridLocation, ier) BIND(C, NAME="cg_gridlocation_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: GridLocation
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_gridlocation_read_f

     SUBROUTINE cg_ordinal_read_f(Ordinal, ier) BIND(C, NAME="cg_ordinal_read_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER :: Ordinal
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ordinal_read_f

     SUBROUTINE cg_npe_f(TYPE,npe, ier) BIND(C, NAME="cg_npe_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: TYPE
       INTEGER :: npe
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_npe_f

     SUBROUTINE cg_is_link_f(path_length, ier) BIND(C, NAME="cg_is_link_f")
       IMPLICIT NONE
       INTEGER :: path_length
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_is_link_f

     SUBROUTINE cg_link_read_f(filename, link_path, ier) !BIND(C, NAME="cg_link_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: link_path
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_link_read_f

     SUBROUTINE cg_nuser_data_f(nuser_data, ier) BIND(C, NAME="cg_nuser_data_f")
       IMPLICIT NONE
       INTEGER :: nuser_data
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_nuser_data_f

     SUBROUTINE cg_user_data_read_f(index,dataname, ier) !BIND(C, NAME="cg_user_data_read_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: index
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: dataname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_user_data_read_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !                   Write Multiple path nodes                           *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_famname_write_f(family_name, ier) !BIND(C, NAME="cg_famname_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_famname_write_f

     SUBROUTINE cg_multifam_write_f(name, family, ier) !BIND(C, NAME="cg_multifam_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_multifam_write_f

     SUBROUTINE cg_convergence_write_f(iterations, NormDefinitions, ier) !BIND(C, NAME="cg_convergence_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: iterations
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: NormDefinitions
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_convergence_write_f

     SUBROUTINE cg_state_write_f(StateDescription, ier) !BIND(C, NAME="cg_state_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: StateDescription
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_state_write_f

     SUBROUTINE cg_equationset_write_f(EquationDimension, ier) BIND(C, NAME="cg_equationset_write_f")
       IMPLICIT NONE
       INTEGER :: EquationDimension
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_equationset_write_f

     SUBROUTINE cg_governing_write_f(Equationstype, ier) BIND(C, NAME="cg_governing_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: Equationstype
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_governing_write_f

     SUBROUTINE cg_diffusion_write_f(diffusion_model, ier) BIND(C, NAME="cg_diffusion_write_f")
       IMPLICIT NONE
       INTEGER, DIMENSION(*) :: diffusion_model
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_diffusion_write_f

     SUBROUTINE cg_model_write_f(ModelLabel, ModelType, ier) !BIND(C, NAME="cg_model_write_f")
       IMPORT :: c_char, cgenum_t
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ModelLabel
       INTEGER(cgenum_t) :: ModelType
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_model_write_f

!!$    SUBROUTINE cg_array_write_f(ArrayName, DataType, DataDimension, DimensionVector, &
!!$     void *Data, ier) BIND(C, NAME="")
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName
!!$      INTEGER(cgenum_t) :: DataType
!!$      INTEGER :: DataDimension
!!$      INTEGER(CGSIZE_T) :: DimensionVector
!!$      void *DATA
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_write_f

!!$    SUBROUTINE cg_array_write_f03(ArrayName, DataType, DataDimension, DimensionVector, DATA, ier) &
!!$         BIND(C, NAME="cg_array_write_f03")
!!$      IMPORT :: c_char, cgenum_t, cgsize_t, c_ptr
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName
!!$      INTEGER(cgenum_t) :: DataType
!!$      INTEGER :: DataDimension
!!$      INTEGER(cgsize_t), DIMENSION(*) :: DimensionVector
!!$      TYPE(C_PTR), VALUE :: Data
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_write_f03

     SUBROUTINE cg_integral_write_f(IntegralDataName, ier) !BIND(C, NAME="cg_integral_write_f")
       IMPORT :: c_char
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: IntegralDataName
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_integral_write_f

     SUBROUTINE cg_rind_write_f(RindData, ier) BIND(C, NAME="cg_rind_write_f")
       IMPLICIT NONE
       INTEGER, DIMENSION(*) :: RindData
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_rind_write_f

     SUBROUTINE cg_descriptor_write_f(descr_name, descr_text, ier) !BIND(C, NAME="cg_descriptor_write_f")
       IMPORT :: c_char
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_text
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_descriptor_write_f

     SUBROUTINE cg_units_write_f(mass, length, time, temperature, angle, ier) BIND(C, NAME="cg_units_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: mass
       INTEGER(cgenum_t) :: length
       INTEGER(cgenum_t) :: time
       INTEGER(cgenum_t) :: temperature
       INTEGER(cgenum_t) :: angle
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_units_write_f

     SUBROUTINE cg_unitsfull_write_f(mass, length, time, temperature, angle, current, &
          amount, intensity, ier) BIND(C, NAME="cg_unitsfull_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: mass
       INTEGER(cgenum_t) :: length
       INTEGER(cgenum_t) :: time
       INTEGER(cgenum_t) :: temperature
       INTEGER(cgenum_t) :: angle
       INTEGER(cgenum_t) :: current
       INTEGER(cgenum_t) :: amount
       INTEGER(cgenum_t) :: intensity
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_unitsfull_write_f

!!$    SUBROUTINE cg_exponents_write_f(DataType, void *exponents, ier) BIND(C, NAME="")
!!$
!!$      INTEGER(cgenum_t) :: DataType_t)*DataType, void *exponents,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_exponents_write_f

!!$    SUBROUTINE cg_expfull_write_f(DataType, void *exponents, ier) BIND(C, NAME="")
!!$      INTEGER(cgenum_t) :: DataType
!!$      void *exponents,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_expfull_write_f

!!$    SUBROUTINE cg_conversion_write_f( DataType, ConversionFactors, ier) BIND(C, NAME="")
!!$      INTEGER(cgenum_t) :: DataType
!!$      void *ConversionFactors
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_conversion_write_f

     SUBROUTINE cg_dataclass_write_f(dataclass, ier) BIND(C, NAME="cg_dataclass_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: dataclass
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_dataclass_write_f

     SUBROUTINE cg_gridlocation_write_f(GridLocation, ier) BIND(C, NAME="cg_gridlocation_write_f")
       IMPORT :: cgenum_t
       IMPLICIT NONE
       INTEGER(cgenum_t) :: GridLocation
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_gridlocation_write_f

     SUBROUTINE cg_ordinal_write_f(Ordinal, ier) BIND(C, NAME="cg_ordinal_write_f")
       IMPLICIT NONE
       INTEGER :: Ordinal
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_ordinal_write_f

     SUBROUTINE cg_link_write_f(nodename, filename, name_in_file, ier) !BIND(C, NAME="cg_link_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: nodename
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name_in_file
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_link_write_f

     SUBROUTINE cg_user_data_write_f(dataname, ier) ! BIND(C, NAME="cg_user_data_write_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: dataname
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_user_data_write_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      General Delete Function                                          *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_delete_node_f(node_name, ier) !BIND(C, NAME="cg_delete_node_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: node_name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_delete_node_f

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
  !      Error Handling Functions                                         *
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     SUBROUTINE cg_get_error_f(errmsg) !BIND(C, NAME="cg_get_error_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: errmsg
     END SUBROUTINE cg_get_error_f

     SUBROUTINE cg_error_exit_f() BIND(C, NAME="cg_error_exit_f")
       IMPLICIT NONE
     END SUBROUTINE cg_error_exit_f

     SUBROUTINE cg_error_print_f() BIND(C, NAME="cg_error_print_f")
       IMPLICIT NONE
     END SUBROUTINE cg_error_print_f

     SUBROUTINE cg_exit_on_error_f(flag) BIND(C, NAME="cg_exit_on_error_f")
       IMPLICIT NONE
       INTEGER :: flag
     END SUBROUTINE cg_exit_on_error_f

#if CG_BUILD_PARALLEL_F

  !======================================================================
  ! parallel IO interface
  !======================================================================

     SUBROUTINE cgp_mpi_comm_f( mpi_comm_f, ier) BIND(C, NAME="cgp_mpi_comm_f")
       IMPLICIT NONE
       INTEGER :: mpi_comm_f
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_mpi_comm_f

     SUBROUTINE cgp_pio_mode_f( mode, ier) BIND(C, NAME="cgp_pio_mode_f")
       IMPORT :: C_INT
       IMPLICIT NONE
       INTEGER(C_INT) :: mode
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_pio_mode_f

     SUBROUTINE cgp_open_f(filename, mode, fn, ier) !BIND(C, NAME="cgp_open_f")
       IMPORT :: C_CHAR, C_INT
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
       INTEGER(C_INT) :: mode
       INTEGER :: fn
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_open_f

     SUBROUTINE cgp_close_f(fn, ier) BIND(C, NAME="cgp_close_f")
       IMPLICIT NONE
       INTEGER, INTENT(IN)  :: fn
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_close_f

     SUBROUTINE cgp_coord_write_f(fn, B, Z, type, coordname, C, ier) !BIND(C, NAME="cgp_coord_write_f")
       IMPORT :: cgenum_t, c_char
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: fn
       INTEGER, INTENT(IN) :: B
       INTEGER, INTENT(IN) :: Z
       INTEGER(cgenum_t), INTENT(IN) :: type
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: coordname
       INTEGER, INTENT(IN)  :: C
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_coord_write_f

!!$    SUBROUTINE cgp_coord_write_data_f(fn, B, Z, C,
!!$     CGSIZE_T *rmin, CGSIZE_T *rmax, void *data, ier) BIND(C, NAME="")
!!$
!!$ INTEGER :: fn
!!$ INTEGER :: B
!!$ INTEGER :: Z
!!$ INTEGER :: C,
!!$ INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, void *DATA,
!!$ INTEGER, INTENT(OUT) :: ier
!!$END SUBROUTINE cgp_coord_write_data_f

!!$   !!$           SUBROUTINE cgp_coord_read_data_f(
!!$     fn, B, Z, C,
!!$     CGSIZE_T *rmin, CGSIZE_T *rmax, void *data, ier) BIND(C, NAME="")
!!$
!!$     INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C,
!!$     INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, void *data,
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      
     SUBROUTINE cgp_section_write_f( fn, B, Z, section_name, &
          TYPE,start,END, nbndry, S, ier) !BIND(C, NAME="cgp_section_write_f")
       IMPORT :: cgenum_t, cgsize_t, c_char
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: fn
       INTEGER, INTENT(IN) :: B
       INTEGER, INTENT(IN) :: Z
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: section_name
       INTEGER(cgenum_t) :: TYPE
       INTEGER(CGSIZE_T), INTENT(IN) :: start
       INTEGER(CGSIZE_T), INTENT(IN) :: END
       INTEGER, INTENT(IN) :: nbndry
       INTEGER, INTENT(IN) :: S
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_section_write_f

!!$!!$     SUBROUTINE cgp_elements_write_data_f( fn, B, Z, S, CGSIZE_T *start, &
!!$          CGSIZE_T *END, CGSIZE_T *elements, ier) BIND(C, NAME="cgp_elements_write_data_f")
!!$       IMPORT :: cgsize_t
!!$       IMPLICIT NONE
!!$       IMPLICIT NONE
!!$       INTEGER, INTENT(IN) :: fn
!!$       INTEGER, INTENT(IN) :: B
!!$       INTEGER, INTENT(IN) :: Z
!!$       INTEGER, INTENT(IN) :: S
!!$       INTEGER(CGSIZE_T), INTENT(IN)  :: start
!!$       INTEGER(CGSIZE_T), INTENT(IN)  :: end
!!$       INTEGER(CGSIZE_T) , INTENT(IN) :: elements
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cgp_elements_write_data_f
!!$
!!$!!$     SUBROUTINE cgp_elements_read_data_f(fn, B, Z, S, start, &
!!$          end, elements, ier) BIND(C, NAME="cgp_elements_read_data_f")
!!$       IMPORT :: cgsize_t
!!$       IMPLICIT NONE
!!$       INTEGER, INTENT(IN) :: fn
!!$       INTEGER, INTENT(IN) :: B
!!$       INTEGER, INTENT(IN) :: Z
!!$       INTEGER, INTENT(IN) :: S
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: start
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: end
!!$       INTEGER(CGSIZE_T) :: elements
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cgp_elements_read_data_f
!!$
     SUBROUTINE cgp_field_write_f(fn, B, Z, S, TYPE, fieldname, F, ier)! BIND(C, NAME="cgp_field_write_f")
       IMPORT :: cgenum_t, c_char
       IMPLICIT NONE
       INTEGER :: fn
       INTEGER :: B
       INTEGER :: Z
       INTEGER :: S
       INTEGER(cgenum_t) :: type
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       INTEGER :: F
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_field_write_f

!!$ INTERFACE
!!$    SUBROUTINE cgp_field_read_data_f( fn, B, Z, S, F, rmin, rmax, void *field_ptr,
!!$     ier) BIND(C, NAME="")
!!$
!!$     INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S,
!!$     INTEGER :: F, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, void *field_ptr
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      
!!$!!$     SUBROUTINE cgp_array_write_f(ArrayName, DataType, DataDimension, DimensionVector, A, ier) !BIND(C, NAME="cgp_array_write_f")
!!$       IMPORT :: cgenum_t, cgsize_t, c_char
!!$       IMPLICIT NONE
!!$       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName
!!$       INTEGER(cgenum_t) :: DataType
!!$       INTEGER :: DataDimension
!!$       INTEGER(CGSIZE_T) :: DimensionVector
!!$       INTEGER :: A
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cgp_array_write_f
!!$
!!$   !!$           SUBROUTINE cgp_array_write_data_f(
!!$     A, CGSIZE_T *rmin, CGSIZE_T *rmax, void *data,
!!$     ier) BIND(C, NAME="")
!!$     INTEGER :: A, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, void *data,
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      
!!$   !!$           SUBROUTINE cgp_array_read_data_f(
!!$     A, CGSIZE_T *rmin, CGSIZE_T *rmax, void *data,
!!$     ier) BIND(C, NAME="")
!!$     INTEGER :: A, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, void *data,
!!$
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      
     SUBROUTINE cgp_error_exit_f() BIND(C, NAME="cgp_error_exit_f")
       IMPLICIT NONE
     END SUBROUTINE cgp_error_exit_f

     SUBROUTINE cgp_mpi_info_f(pcg_mpi_info_f, ier) BIND(C,NAME="cgp_mpi_info_f")
       IMPORT :: C_INT
       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN) :: pcg_mpi_info_f
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgp_mpi_info_f

!!$
!!$   !!$           SUBROUTINE cgp_coord_multi_read_data_f, CGP_COORD_MULTI_READ_DATA_F)(fn, B, Z, C,
!!$     CGSIZE_T *rmin, CGSIZE_T *rmax,
!!$     void *coordsX, void *coordsY, void *coordsZ, ier) BIND(C, NAME="")
!!$INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C,
!!$     INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax,
!!$     void *coordsX, void *coordsY, void *coordsZ,
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      !!$
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!       cgp_coord_multi_write_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$
!!$   !!$           SUBROUTINE cgp_coord_multi_write_data_f, CGP_COORD_MULTI_WRITE_DATA_F)(fn, B, Z, C,
!!$                                                 CGSIZE_T *rmin, CGSIZE_T *rmax,
!!$                                                 void *coordsX, void *coordsY, void *coordsZ, ier) BIND(C, NAME="")
!!$INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C,
!!$                                                 INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax,
!!$                                                 void *coordsX, void *coordsY, void *coordsZ
!!$
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      !!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!       cgp_field_multi_write_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$   !!$           SUBROUTINE cgp_field_multi_write_data_f(
!!$     fn, B, Z, S,
!!$     F, CGSIZE_T *rmin, CGSIZE_T *rmax, ier, CGSIZE_T *nsets, ...)
!!$     fn, B, Z, S,
!!$     F, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, ier, INTEGER(CGSIZE_T) :: nsets, ...
!!$
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      !!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!       cgp_field_multi_read_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$   !!$           SUBROUTINE cgp_field_multi_read_data_f(
!!$     INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S,
!!$     F, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, ier, nsets, ...)
!!$
!!$     INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S,
!!$     INTEGER :: F, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax, ier
!!$      INTEGER :: nsets, ...
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      !!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!        cgp_array_multi_write_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$   !!$           SUBROUTINE cgp_array_multi_write_data_f(
!!$     fn, A, CGSIZE_T *rmin, CGSIZE_T *rmax,
!!$     ier, nsets, ...)
!!$     INTEGER :: fn
!!$      INTEGER :: A, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax,
!!$     ier
!!$      INTEGER :: nsets, ...
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      !!$
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!        cgp_array_multi_read_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$   !!$           SUBROUTINE cgp_array_multi_read_data_f(
!!$     fn, A, CGSIZE_T *rmin, CGSIZE_T *rmax,
!!$     ier, nsets, ...)
!!$
!!$     INTEGER :: fn
!!$      INTEGER :: A, INTEGER(CGSIZE_T) :: rmin, INTEGER(CGSIZE_T) :: rmax,
!!$          INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE
!!$      !!$
!!$#endif HDF5_HAVE_MULTI_DATASETS
#endif

  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
  !*      INTERFACES FOR THE C FUNCTIONS                                 *
  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!!$!!$     INTEGER(C_INT) FUNCTION cgp_open(filename, mode, fn) BIND(C, name="cgp_open")
!!$       IMPORT :: c_int, c_char
!!$       IMPLICIT NONE
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: filename
!!$       INTEGER(C_INT), INTENT(IN), VALUE  :: mode
!!$       INTEGER(C_INT), INTENT(OUT) :: fn
!!$     END FUNCTION cgp_open
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_pio_mode(mode) BIND(C, name="cgp_pio_mode")
!!$       USE ISO_C_BINDING
!!$       INTEGER(KIND(CGP_COLLECTIVE)), INTENT(IN), VALUE  :: mode
!!$     END FUNCTION cgp_pio_mode
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cg_base_write(fn, basename, cell_dim, phys_dim, B) BIND(C, name="cg_base_write")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: basename
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE  :: cell_dim
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE  :: phys_dim
!!$       INTEGER(C_INT)   , INTENT(OUT)  :: B
!!$     END FUNCTION cg_base_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cg_zone_write(fn, B, zonename, nijk, itype, Z) BIND(C, name="cg_zone_write")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE  :: B
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: zonename
!!$       INTEGER(CGSIZE_T), DIMENSION(*), INTENT(IN)  :: nijk
!!$       INTEGER(KIND(CGP_COLLECTIVE)), INTENT(IN), VALUE  :: itype
!!$       INTEGER(C_INT)   , INTENT(OUT)  :: Z
!!$     END FUNCTION cg_zone_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cg_base_read(fn, B, basename, cell_dim, phys_dim) BIND(C, name="cg_base_read")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: basename
!!$       INTEGER(C_INT)   , INTENT(OUT)  :: cell_dim
!!$       INTEGER(C_INT)   , INTENT(OUT)  :: phys_dim
!!$     END FUNCTION cg_base_read
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cg_zone_read(fn, B, Z, zonename, nijk) !BIND(C, name="cg_zone_read")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: zonename
!!$       INTEGER(CGSIZE_T), DIMENSION(*), INTENT(OUT)  :: nijk
!!$     END FUNCTION cg_zone_read
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_coord_write(fn, B, Z, itype, coordname, C) BIND(C, name="cgp_coord_write")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: coordname
!!$       INTEGER(C_INT)   , INTENT(OUT)  :: C
!!$     END FUNCTION cgp_coord_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_coord_write_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name="cgp_coord_write_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: C
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmin
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmax
!!$       TYPE(C_PTR), VALUE :: coords
!!$     END FUNCTION cgp_coord_write_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_field_write(fn, B, Z, S, itype, fieldname, F) BIND(C, name="cgp_field_write")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
!!$       INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: fieldname
!!$       INTEGER(C_INT)   , INTENT(OUT)  :: F
!!$     END FUNCTION cgp_field_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_field_write_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name="cgp_field_write_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: F
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmin
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmax
!!$       TYPE(C_PTR), VALUE :: data
!!$     END FUNCTION cgp_field_write_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_field_read_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name="cgp_field_read_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: F
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmin
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmax
!!$       TYPE(C_PTR), VALUE :: data
!!$     END FUNCTION cgp_field_read_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_coord_read_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name="cgp_coord_read_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: C
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmin
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmax
!!$       TYPE(C_PTR), VALUE :: coords
!!$     END FUNCTION cgp_coord_read_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_section_write(fn,B,Z,sectionname,itype,start,end,nbndry,S) BIND(C, name="cgp_section_write")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: sectionname
!!$       INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
!!$       INTEGER(CGSIZE_T), INTENT(IN), VALUE :: start
!!$       INTEGER(CGSIZE_T), INTENT(IN), VALUE :: end
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: nbndry
!!$       INTEGER(C_INT)   , INTENT(OUT) :: S
!!$     END FUNCTION cgp_section_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_array_write(arrayname,itype,DataDimension,DimensionVector,A) BIND(C, name="cgp_array_write")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: arrayname
!!$       INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: DataDimension
!!$       INTEGER(CGSIZE_T), DIMENSION(1:DataDimension), INTENT(IN) :: DimensionVector
!!$       INTEGER(C_INT)   , INTENT(OUT) :: A
!!$     END FUNCTION cgp_array_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_array_write_data(A, rmin, rmax, data) BIND(C, name="cgp_array_write_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: A
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmin
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmax
!!$       TYPE(C_PTR), VALUE :: data
!!$     END FUNCTION cgp_array_write_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_elements_write_data(fn,B,Z,S,emin,emax,elements) BIND(C, name="cgp_elements_write_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
!!$       INTEGER(CGSIZE_T), INTENT(IN), VALUE :: emin
!!$       INTEGER(CGSIZE_T), INTENT(IN), VALUE :: emax
!!$       TYPE(C_PTR), VALUE :: elements
!!$     END FUNCTION cgp_elements_write_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_elements_read_data(fn,B,Z,S,start,end,elements) BIND(C, name="cgp_elements_read_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
!!$       INTEGER(CGSIZE_T), INTENT(IN), VALUE :: start
!!$       INTEGER(CGSIZE_T), INTENT(IN), VALUE :: end
!!$       TYPE(C_PTR), VALUE :: elements
!!$     END FUNCTION cgp_elements_read_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_array_read_data(A, rmin, rmax, data) BIND(C, name="cgp_array_read_data")
!!$       USE ISO_C_BINDING
!!$       IMPORT :: CGSIZE_T
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: A
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmin
!!$       INTEGER(CGSIZE_T), INTENT(IN) :: rmax
!!$       TYPE(C_PTR), VALUE :: data
!!$     END FUNCTION cgp_array_read_data
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cg_sol_write(fn,B,Z,solname,location,S) BIND(C, name="cg_sol_write")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
!!$       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: solname
!!$       INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: location
!!$       INTEGER(C_INT)   , INTENT(OUT) :: S
!!$     END FUNCTION cg_sol_write
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_error_exit() BIND(C, name="cgp_error_exit")
!!$       USE ISO_C_BINDING
!!$     END FUNCTION cgp_error_exit
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_close(fn) BIND(C, name="cgp_close")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT), INTENT(IN), VALUE :: fn
!!$     END FUNCTION cgp_close
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_queue_set(use_queue) BIND(C, name="cgp_queue_set")
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT), INTENT(IN), VALUE :: use_queue
!!$     END FUNCTION cgp_queue_set
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cgp_queue_flush() BIND(C, name="cgp_queue_flush")
!!$       USE ISO_C_BINDING
!!$     END FUNCTION cgp_queue_flush
!!$!!$
!!$!!$     INTEGER(C_INT) FUNCTION cg_user_data_write(UserDataName) BIND(C, name="cg_user_data_write")
!!$       USE ISO_C_BINDING
!!$       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: UserDataName
!!$     END FUNCTION cg_user_data_write
!!$
#if HAVE_FORTRAN_2008TS
  ! THE FOLLOWING CODE ONLY WORKS FOR COMPILERS HAVING F2008 STANDARD EXTENSION:
  ! TS 29113 Further Interoperability of FORTRAN with C WG5/N1942

  ! The number of optional parameters should be set to
  ! CG_MAX_GOTO_DEPTH, which is currently set to 20.
     INTEGER(C_INT) FUNCTION cg_gorel(fn, &
          UserDataName1, i1, UserDataName2, i2, &
          UserDataName3, i3, UserDataName4, i4, &
          UserDataName5, i5, UserDataName6, i6, &
          UserDataName7, i7, UserDataName8, i8, &
          UserDataName9, i9, UserDataName10, i10, &
          UserDataName11, i11, UserDataName12, i12, &
          UserDataName13, i13, UserDataName14, i14, &
          UserDataName15, i15, UserDataName16, i16, &
          UserDataName17, i17, UserDataName18, i18, &
          UserDataName19, i19, UserDataName20, i20, &
          end) BIND(C, name="cg_gorel_f08")

       USE ISO_C_BINDING
       INTEGER(C_INT) , INTENT(IN), VALUE :: fn
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1,UserDataName2, &
            UserDataName3,UserDataName4,UserDataName5,UserDataName6,UserDataName7,UserDataName8, &
            UserDataName9,UserDataName10,UserDataName11,UserDataName12,UserDataName13,UserDataName14, &
            UserDataName15,UserDataName16,UserDataName17,UserDataName18,UserDataName19,UserDataName20

       INTEGER(C_INT), INTENT(IN), OPTIONAL :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
            i17, i18, i19, i20
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: end
     END FUNCTION cg_gorel

  ! The number of optional parameters should be set to
  ! CG_MAX_GOTO_DEPTH, which is currently set to 20.

     INTEGER(C_INT) FUNCTION cg_goto(fn, B, &
          UserDataName1, i1, UserDataName2, i2, &
          UserDataName3, i3, UserDataName4, i4, &
          UserDataName5, i5, UserDataName6, i6, &
          UserDataName7, i7, UserDataName8, i8, &
          UserDataName9, i9, UserDataName10, i10, &
          UserDataName11, i11, UserDataName12, i12, &
          UserDataName13, i13, UserDataName14, i14, &
          UserDataName15, i15, UserDataName16, i16, &
          UserDataName17, i17, UserDataName18, i18, &
          UserDataName19, i19, UserDataName20, i20, &
          end) BIND(C, name="cg_goto_f08")

       USE ISO_C_BINDING
       INTEGER(C_INT) , INTENT(IN), VALUE :: fn
       INTEGER(C_INT) , INTENT(IN), VALUE :: B
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1,UserDataName2, &
            UserDataName3,UserDataName4,UserDataName5,UserDataName6,UserDataName7,UserDataName8, &
            UserDataName9,UserDataName10,UserDataName11,UserDataName12,UserDataName13,UserDataName14, &
            UserDataName15,UserDataName16,UserDataName17,UserDataName18,UserDataName19,UserDataName20
       INTEGER(C_INT), INTENT(IN), OPTIONAL :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
            i17, i18, i19, i20
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: end
     END FUNCTION cg_goto
#endif

     SUBROUTINE cg_open_f(filename, mode, fn, ier)
       IMPORT :: C_CHAR, C_INT
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
       INTEGER(C_INT), INTENT(IN) :: mode
       INTEGER, INTENT(OUT) :: fn
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_open_f

     SUBROUTINE cg_is_cgns_f(filename, file_type, ier)
        IMPORT :: C_CHAR
        IMPLICIT NONE
        CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
        INTEGER, INTENT(OUT) :: file_type
        INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cg_is_cgns_f
  END INTERFACE

  INTERFACE cg_get_type
     MODULE PROCEDURE cg_get_type_c_int
     MODULE PROCEDURE cg_get_type_c_long_long
     MODULE PROCEDURE cg_get_type_c_float
     MODULE PROCEDURE cg_get_type_c_double
  END INTERFACE

  INTERFACE cg_configure_f
     MODULE PROCEDURE cg_configure_ptr
     MODULE PROCEDURE cg_configure_funptr
  END INTERFACE

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      INTERFACES FOR THE CGIO FORTRAN FUNCTIONS                      *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

  INTERFACE
!*=========================================================
!* paths for searching for linked-to files
!*=========================================================

     SUBROUTINE cgio_path_add_f(path, ier) ! BIND(C,NAME="cgio_path_add_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: path
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_path_add_f

!*---------------------------------------------------------
     SUBROUTINE cgio_path_delete_f(path, ier) ! BIND(C,NAME="cgio_path_delete_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: path
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_path_delete_f
  
!*=========================================================
!* utility routines independent of open files
!*=========================================================
         SUBROUTINE cgio_is_supported_f(file_type, ier) BIND(C,NAME="cgio_is_supported_f")
         IMPORT :: CGSIZE_T
         IMPLICIT NONE
         INTEGER(CGSIZE_T) :: file_type
         INTEGER, INTENT(OUT) :: ier
       END SUBROUTINE cgio_is_supported_f
  
!*---------------------------------------------------------
     SUBROUTINE cgio_check_file_f(filename, file_type, ier) !BIND(C,NAME="cgio_check_file_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
       INTEGER :: file_type
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_check_file_f

!*=========================================================
!* file operations
!*=========================================================
     SUBROUTINE cgio_open_file_f(filename, file_mode, file_type, cgio_num, ier) ! BIND(C,NAME="cgio_open_file_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
       INTEGER :: file_mode
       INTEGER :: file_type
       INTEGER :: cgio_num
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_open_file_f

  !*---------------------------------------------------------
     SUBROUTINE cgio_close_file_f(cgio_num, ier) BIND(C,NAME="cgio_close_file_f")
       IMPLICIT NONE
       INTEGER :: cgio_num
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_close_file_f

  !*---------------------------------------------------------
     SUBROUTINE cgio_flush_to_disk_f(cgio_num, ier) BIND(C,NAME="cgio_flush_to_disk_f")
       IMPLICIT NONE
       INTEGER :: cgio_num
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_flush_to_disk_f

!*=========================================================
!* file information
!*=========================================================
     SUBROUTINE cgio_library_version_f(cgio_num, version, ier) ! BIND(C,NAME="cgio_library_version_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: cgio_num
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: version
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_library_version_f

!*---------------------------------------------------------
     SUBROUTINE cgio_file_version_f(cgio_num, file_version, creation_date, modified_date, ier) ! BIND(C,NAME="cgio_file_version_f")
       IMPORT :: c_char
       IMPLICIT NONE
       INTEGER :: cgio_num
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: file_version
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: creation_date
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: modified_date
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE

!*---------------------------------------------------------
     SUBROUTINE cgio_get_root_id_f(cgio_num, rootid, ier) BIND(C,NAME="cgio_get_root_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(c_double) :: rootid
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_root_id_f

!*---------------------------------------------------------
     SUBROUTINE cgio_get_file_type_f(cgio_num, file_type, ier) BIND(C,NAME="cgio_get_file_type_f")
       IMPLICIT NONE
       INTEGER :: cgio_num
       INTEGER :: file_type
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_file_type_f

!*=========================================================
!* error handling
!*=========================================================
     SUBROUTINE cgio_error_code_f(errcode, file_type) BIND(C,NAME="cgio_error_code_f")
       IMPLICIT NONE
       INTEGER :: errcode
       INTEGER :: file_type
     END SUBROUTINE cgio_error_code_f

!*---------------------------------------------------------
     SUBROUTINE cgio_error_message_f(errmsg, ier) ! BIND(C,NAME="cgio_error_message_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: errmsg
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_error_message_f

!*---------------------------------------------------------
     SUBROUTINE cgio_error_exit_f(errmsg) ! BIND(C,NAME="cgio_error_exit_f")
       IMPORT :: c_char
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: errmsg
     END SUBROUTINE cgio_error_exit_f

!*---------------------------------------------------------
     SUBROUTINE cgio_error_abort_f(abort_flag) BIND(C,NAME="cgio_error_abort_f")
       IMPLICIT NONE
       INTEGER :: abort_flag
     END SUBROUTINE cgio_error_abort_f

!*=========================================================
!* basic node operations
!*=========================================================
     SUBROUTINE cgio_create_node_f(cgio_num, pid, name, id, ier) ! BIND(C,NAME="cgio_create_node_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       REAL(C_DOUBLE) :: id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_create_node_f

!*---------------------------------------------------------
!!$!!$     SUBROUTINE cgio_new_node_f(cgio_num, pid, name, label, data_type, ndims, dims, DATA, id, ier) BIND(C,NAME="")
!!$       IMPORT :: c_char, c_double
!!$       IMPLICIT NONE
!!$       INTEGER :: cgio_num
!!$       REAL(C_DOUBLE) :: pid
!!$       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
!!$       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: label
!!$       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: data_type
!!$       INTEGER :: ndims
!!$       INTEGER(CGSIZE_T), DIMENSION(*) :: dims
!!$       TYPE(C_PTR) ::  DATA
!!$       REAL(C_DOUBLE) :: id
!!$       INTEGER, INTENT(OUT) :: ier
!!$     END SUBROUTINE cgio_new_node_f
!!$
!*---------------------------------------------------------
     SUBROUTINE cgio_delete_node_f(cgio_num, pid, id, ier) BIND(C,NAME="cgio_delete_node_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       REAL(C_DOUBLE) :: id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_delete_node_f

!*---------------------------------------------------------
     SUBROUTINE cgio_move_node_f(cgio_num, pid, id, npid, ier) BIND(C,NAME="cgio_move_node_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       REAL(C_DOUBLE) :: id
       REAL(C_DOUBLE) :: npid
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_move_node_f

!*---------------------------------------------------------
     SUBROUTINE cgio_release_id_f(cgio_num, id, ier) BIND(C,NAME="cgio_release_id_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_release_id_f

!*=========================================================
!* links
!*=========================================================
     SUBROUTINE cgio_is_link_f(cgio_num, id, link_len, ier) BIND(C,NAME="cgio_is_link_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       INTEGER :: link_len
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_is_link_f

!*---------------------------------------------------------
     SUBROUTINE cgio_link_size_f(cgio_num, id, file_len, name_len, ier) BIND(C,NAME="cgio_link_size_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       INTEGER :: file_len
       INTEGER :: name_len
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_link_size_f

!*---------------------------------------------------------
     SUBROUTINE cgio_create_link_f(cgio_num, pid, name, filename, name_in_file, id, ier) ! BIND(C,NAME="cgio_create_link_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name_in_file
       REAL(C_DOUBLE) :: id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_create_link_f

!*---------------------------------------------------------
     SUBROUTINE cgio_get_link_f(cgio_num, id, filename, name_in_file, ier) ! BIND(C,NAME="cgio_get_link_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name_in_file
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_link_f

!*=========================================================
!* node children
!*=========================================================
     SUBROUTINE cgio_number_children_f(cgio_num, pid, num_children, ier) &
                BIND(C,NAME="cgio_number_children_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       INTEGER :: num_children
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_number_children_f

!*---------------------------------------------------------
     SUBROUTINE cgio_children_ids_f(cgio_num, pid, start, max_ret, num_ret, ids, ier) &
                BIND(C,NAME="cgio_children_ids_f")
       IMPORT :: c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       INTEGER :: start
       INTEGER :: max_ret
       INTEGER :: num_ret
       REAL(C_DOUBLE) :: ids
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_children_ids_f

!*---------------------------------------------------------
     SUBROUTINE cgio_children_names_f(cgio_num, pid, start, max_ret, name_len, &
          num_ret, names, ier) !BIND(C,NAME="cgio_children_names_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       INTEGER :: start
       INTEGER :: max_ret
       INTEGER :: name_len
       INTEGER :: num_ret
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: names
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_children_names_f

!*=========================================================
!* read nodes
!*=========================================================
     SUBROUTINE cgio_get_node_id_f(cgio_num, pid, name, id, ier) !BIND(C,NAME="cgio_get_node_id_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       REAL(C_DOUBLE) :: id
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_node_id_f

!*---------------------------------------------------------
     SUBROUTINE cgio_get_name_f(cgio_num, id, name, ier) !BIND(C,NAME="cgio_get_name_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_name_f

!*---------------------------------------------------------
     SUBROUTINE cgio_get_label_f(cgio_num, id, label, ier) !BIND(C,NAME="cgio_get_label_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: label
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_label_f

!*---------------------------------------------------------
     SUBROUTINE cgio_get_data_type_f(cgio_num, id, data_type, ier) !BIND(C,NAME="cgio_get_data_type_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: data_type
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_data_type_f

!*---------------------------------------------------------
     SUBROUTINE cgio_get_data_size_f(cgio_num, id, size, ier) BIND(C,NAME="cgio_get_data_size_f")
       IMPORT :: c_double, cgsize_t
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       INTEGER(CGSIZE_T) :: size
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_get_data_size_f

!*=========================================================
!* write nodes
!*=========================================================
     SUBROUTINE cgio_set_name_f(cgio_num, pid, id, name, ier) !BIND(C,NAME="cgio_set_name_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: pid
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_set_name_f

!*---------------------------------------------------------
     SUBROUTINE cgio_set_label_f(cgio_num, id, label, ier) !BIND(C,NAME="cgio_set_label_f")
       IMPORT :: c_char, c_double
       IMPLICIT NONE
       INTEGER :: cgio_num
       REAL(C_DOUBLE) :: id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: label
       INTEGER, INTENT(OUT) :: ier
     END SUBROUTINE cgio_set_label_f


!*---------------------------------------------------------
!!$!!$     SUBROUTINE cgio_write_all_data_f(
!!$    cgint_f *cgio_num, double *id, void *data, cgint_f *ier) BIND(C,NAME="")
!!$  
!*---------------------------------------------------------
!!$!!$     SUBROUTINE cgio_write_block_data_f(
!!$    cgint_f *cgio_num, double *id, cgsize_t *b_start, cgsize_t *b_end,
!!$    void *data, cgint_f *ier) BIND(C,NAME="")
!!$
!!$  
!*---------------------------------------------------------
!!$!!$     SUBROUTINE cgio_write_data_f(
!!$    cgint_f *cgio_num, double *id, cgsize_t *s_start, cgsize_t *s_end,
!!$    cgsize_t *s_stride, cgsize_t *m_ndims, cgsize_t *m_dims, cgsize_t *m_start,
!!$    cgsize_t *m_end, cgsize_t *m_stride, void *data, cgint_f *ier) BIND(C,NAME="")
!!$
!!$  
  END INTERFACE

  PRIVATE cg_configure_ptr, cg_configure_funptr

CONTAINS

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_goto_f
!DEC$endif
  SUBROUTINE cg_goto_f(fn, B, ier, &
       UserDataName1, i1, UserDataName2, i2, &
       UserDataName3, i3, UserDataName4, i4, &
       UserDataName5, i5, UserDataName6, i6, &
       UserDataName7, i7, UserDataName8, i8, &
       UserDataName9, i9, UserDataName10, i10, &
       UserDataName11, i11, UserDataName12, i12, &
       UserDataName13, i13, UserDataName14, i14, &
       UserDataName15, i15, UserDataName16, i16, &
       UserDataName17, i17, UserDataName18, i18, &
       UserDataName19, i19, UserDataName20, i20, &
       end)
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: fn
     INTEGER, INTENT(IN) :: B
     INTEGER, INTENT(OUT) :: ier
     CHARACTER(*), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1,UserDataName2, &
          UserDataName3,UserDataName4,UserDataName5,UserDataName6,UserDataName7,UserDataName8, &
          UserDataName9,UserDataName10,UserDataName11,UserDataName12,UserDataName13,UserDataName14, &
          UserDataName15,UserDataName16,UserDataName17,UserDataName18,UserDataName19,UserDataName20
     INTEGER, INTENT(IN), OPTIONAL :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
          i17, i18, i19, i20
     CHARACTER(*), DIMENSION(*), INTENT(IN), OPTIONAL :: end

     IF (.NOT. PRESENT(i1)) THEN
        CALL cg_goto_f1(fn, B, ier, UserDataName1, 0)
        RETURN
     ELSE
        CALL cg_goto_f1(fn, B, ier, UserDataName1, i1)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i2)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName2, i2)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i3)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName3, i3)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i4)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName4, i4)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i5)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName5, i5)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i6)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName6, i6)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i7)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName7, i7)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i8)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName8, i8)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i9)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName9, i9)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i10)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName10, i10)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i11)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName11, i11)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i12)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName12, i12)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i13)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName13, i13)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i14)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName14, i14)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i15)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName15, i15)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i16)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName16, i16)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i17)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName17, i17)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i18)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName18, i18)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i19)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName19, i19)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i20)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName20, i20)
        IF(ier .NE. 0) RETURN
     END IF
  END SUBROUTINE cg_goto_f

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_gorel_f
!DEC$endif
  SUBROUTINE cg_gorel_f(fn, ier, &
          UserDataName1, i1, UserDataName2, i2, &
          UserDataName3, i3, UserDataName4, i4, &
          UserDataName5, i5, UserDataName6, i6, &
          UserDataName7, i7, UserDataName8, i8, &
          UserDataName9, i9, UserDataName10, i10, &
          UserDataName11, i11, UserDataName12, i12, &
          UserDataName13, i13, UserDataName14, i14, &
          UserDataName15, i15, UserDataName16, i16, &
          UserDataName17, i17, UserDataName18, i18, &
          UserDataName19, i19, UserDataName20, i20, &
          end)
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: fn
     INTEGER, INTENT(OUT) :: ier
     CHARACTER(*), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1,UserDataName2, &
          UserDataName3,UserDataName4,UserDataName5,UserDataName6,UserDataName7,UserDataName8, &
          UserDataName9,UserDataName10,UserDataName11,UserDataName12,UserDataName13,UserDataName14, &
          UserDataName15,UserDataName16,UserDataName17,UserDataName18,UserDataName19,UserDataName20
     INTEGER, INTENT(IN), OPTIONAL :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
          i17, i18, i19, i20
     CHARACTER(*), DIMENSION(*), INTENT(IN), OPTIONAL :: end
  
     IF (PRESENT(i1)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName1, i1)
     ELSE
        CALL cg_gorel_f1(fn, ier, UserDataName1, 0)
        RETURN
     END IF
     IF (PRESENT(i2)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName2, i2)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i3)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName3, i3)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i4)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName4, i4)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i5)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName5, i5)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i6)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName6, i6)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i7)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName7, i7)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i8)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName8, i8)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i9)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName9, i9)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i10)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName10, i10)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i11)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName11, i11)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i12)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName12, i12)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i13)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName13, i13)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i14)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName14, i14)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i15)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName15, i15)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i16)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName16, i16)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i17)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName17, i17)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i18)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName18, i18)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i19)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName19, i19)
        IF(ier .NE. 0) RETURN
     END IF
     IF (PRESENT(i20)) THEN
        CALL cg_gorel_f1(fn, ier, UserDataName20, i20)
        IF(ier .NE. 0) RETURN
     END IF
  END SUBROUTINE cg_gorel_f

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_get_type_c_int
!DEC$endif
  FUNCTION cg_get_type_c_int(a)
    USE ISO_C_BINDING
    INTEGER(C_INT) :: a
    INTEGER(KIND(CGNS_ENUMV(Integer))) :: cg_get_type_c_int
    cg_get_type_c_int = CGNS_ENUMV(Integer)
  END FUNCTION cg_get_type_c_int

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_get_type_c_long_long
!DEC$endif
  FUNCTION cg_get_type_c_long_long(a)
    USE ISO_C_BINDING
    INTEGER(C_LONG_LONG) :: a
    INTEGER(KIND(CGNS_ENUMV(Longinteger))) :: cg_get_type_c_long_long
    cg_get_type_c_long_long = CGNS_ENUMV(LongInteger)
  END FUNCTION cg_get_type_c_long_long

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_get_type_c_float
!DEC$endif
  FUNCTION cg_get_type_c_float(a)
    USE ISO_C_BINDING
    REAL(C_FLOAT) :: a
    INTEGER(KIND(CGNS_ENUMV(RealSingle))) :: cg_get_type_c_float
    cg_get_type_c_float = CGNS_ENUMV(RealSingle)
  END FUNCTION cg_get_type_c_float

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_get_type_c_double
!DEC$endif
  FUNCTION cg_get_type_c_double(a)
    USE ISO_C_BINDING
    REAL(C_DOUBLE) :: a
    INTEGER(KIND(CGNS_ENUMV(RealDouble))) :: cg_get_type_c_double
    cg_get_type_c_double = CGNS_ENUMV(RealDouble)
  END FUNCTION cg_get_type_c_double

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_get_type_c_complex_float
!DEC$endif
  FUNCTION cg_get_type_c_complex_float(a)
    USE ISO_C_BINDING
    COMPLEX(C_FLOAT_COMPLEX) :: a
    INTEGER(KIND(CGNS_ENUMV(ComplexSingle))) :: cg_get_type_c_complex_float
    cg_get_type_c_complex_float = CGNS_ENUMV(ComplexSingle)
  END FUNCTION cg_get_type_c_complex_float

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_get_type_c_complex_double
!DEC$endif
  FUNCTION cg_get_type_c_complex_double(a)
    USE ISO_C_BINDING
    COMPLEX(C_DOUBLE_COMPLEX) :: a
    INTEGER(KIND(CGNS_ENUMV(ComplexDouble))) :: cg_get_type_c_complex_double
    cg_get_type_c_complex_double = CGNS_ENUMV(ComplexDouble)
  END FUNCTION cg_get_type_c_complex_double

!  These have issues when using xlf and the calling
!  program does not use the modules, CGNS-25
!  SUBROUTINE cg_is_cgns_f(filename, file_type, ier) BIND(C,NAME="")
!    USE ISO_C_BINDING
!    IMPLICIT NONE
!    CHARACTER(KIND=C_CHAR, LEN=*), INTENT(IN) :: filename
!    INTEGER, INTENT(OUT) :: file_type
!    INTEGER, INTENT(OUT) :: ier
!
!    ier = cg_is_cgns(TRIM(filename)//C_NULL_CHAR, file_type)
!
!  END SUBROUTINE cg_is_cgns_f
!
!  SUBROUTINE cg_open_f(filename, mode, fn, ier) BIND(C,NAME="")
!    USE ISO_C_BINDING
!    IMPLICIT NONE
!    CHARACTER(KIND=C_CHAR, LEN=*), INTENT(IN) :: filename
!    INTEGER(C_INT), INTENT(IN) :: mode
!    INTEGER, INTENT(OUT) :: fn
!    INTEGER, INTENT(OUT) :: ier
!
!    ier = cg_open(TRIM(filename)//C_NULL_CHAR, mode, fn)
!
!  END SUBROUTINE cg_open_f

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_configure_ptr
!DEC$endif
  SUBROUTINE cg_configure_ptr(what, value, ier)
    USE ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: what
    TYPE(C_PTR), VALUE :: value
    INTEGER, INTENT(OUT) :: ier

    CALL cg_configure_c_ptr(what, value, ier)

  END SUBROUTINE cg_configure_ptr

!DEC$if defined(BUILD_CGNS_DLL)
!DEC$ATTRIBUTES DLLEXPORT :: cg_configure_funptr
!DEC$endif
  SUBROUTINE cg_configure_funptr(what, value, ier)
    USE ISO_C_BINDING, ONLY : C_FUNPTR
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: what
    TYPE(C_FUNPTR), VALUE :: value
    INTEGER, INTENT(OUT) :: ier

    CALL cg_configure_c_funptr(what, value, ier)

  END SUBROUTINE cg_configure_funptr

END MODULE cgns
