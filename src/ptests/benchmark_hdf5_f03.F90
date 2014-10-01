#define HAVE_F2003_REQUIREMENTS @HAVE_F2003_REQUIREMENTS@
#define HAVE_F2008TS_REQUIREMENTS @HAVE_F2008TS_REQUIREMENTS@
#include "cgnstypes_f.h"
! @file benchmark_hdf5.F90
! @author M. Scot Breitenfeld <brtnfld@hdfgroup.org>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Fortran2003 Benchmarking program for pcgns library
!
!

! Only compile this code if the Fortran compiler is Fortran 2003 compliant
#ifdef HAVE_F2003_REQUIREMENTS
MODULE cgns_c_binding

!
! Contains needed interfaces for calling the C functions
! 
  IMPLICIT NONE

  INTERFACE
     INTEGER(C_INT) FUNCTION cgp_open(filename, mode, fn) BIND(C, name='cgp_open')
       USE ISO_C_BINDING
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: filename
       INTEGER(C_INT)  , INTENT(IN), VALUE  :: mode
       INTEGER(C_INT)  , INTENT(OUT) :: fn
     END FUNCTION cgp_open
  END INTERFACE

!!$  INTERFACE
!!$     INTEGER(C_INT) FUNCTION cgp_pio_mode(mode) BIND(C, name='cgp_pio_mode')
!!$       USE ISO_C_BINDING
!!$       INTEGER(C_INT)  , INTENT(IN), VALUE  :: mode
!!$     END FUNCTION cgp_pio_mode
!!$  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_base_write(fn, basename, cell_dim, phys_dim, B) BIND(C, name='cg_base_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: basename
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: cell_dim
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: phys_dim
       INTEGER(C_INT)   , INTENT(OUT)  :: B
     END FUNCTION cg_base_write
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_zone_write(fn, B, zonename, nijk, itype, Z) BIND(C, name='cg_zone_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: B
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: zonename
       CGSIZE_T, DIMENSION(*), INTENT(IN)  :: nijk
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: itype
       INTEGER(C_INT)   , INTENT(OUT)  :: Z
     END FUNCTION cg_zone_write
  END INTERFACE 

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_base_read(fn, B, basename, cell_dim, phys_dim) BIND(C, name='cg_base_read')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: basename
       INTEGER(C_INT)   , INTENT(OUT)  :: cell_dim
       INTEGER(C_INT)   , INTENT(OUT)  :: phys_dim
     END FUNCTION cg_base_read
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_zone_read(fn, B, Z, zonename, nijk) BIND(C, name='cg_zone_read')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: zonename
       CGSIZE_T, DIMENSION(*), INTENT(OUT)  :: nijk
     END FUNCTION cg_zone_read
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_coord_write(fn, B, Z, itype, coordname, C) BIND(C, name='cgp_coord_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: coordname
       INTEGER(C_INT)   , INTENT(OUT)  :: C
     END FUNCTION cgp_coord_write
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_coord_write_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name='cgp_coord_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: C
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: coords
     END FUNCTION cgp_coord_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_field_write(fn, B, Z, S, itype, fieldname, F) BIND(C, name='cgp_field_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: fieldname
       INTEGER(C_INT)   , INTENT(OUT)  :: F
     END FUNCTION cgp_field_write
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_field_write_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name='cgp_field_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       INTEGER(C_INT)   , INTENT(IN), VALUE :: F
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_field_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_field_read_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name='cgp_field_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       INTEGER(C_INT)   , INTENT(IN), VALUE :: F
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_field_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_coord_read_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name='cgp_coord_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: C
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: coords
     END FUNCTION cgp_coord_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_section_write(fn,B,Z,sectionname,itype,start,end,nbndry,S) BIND(C, name='cgp_section_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: sectionname
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       CGSIZE_T, INTENT(IN), VALUE :: start
       CGSIZE_T, INTENT(IN), VALUE :: end
       INTEGER(C_INT)   , INTENT(IN), VALUE :: nbndry
       INTEGER(C_INT)   , INTENT(OUT) :: S
     END FUNCTION cgp_section_write
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_array_write(arrayname,itype,DataDimension,DimensionVector,A) BIND(C, name='cgp_array_write')
       USE ISO_C_BINDING
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: arrayname
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       INTEGER(C_INT)   , INTENT(IN), VALUE :: DataDimension
       CGSIZE_T, DIMENSION(1:DataDimension), INTENT(IN) :: DimensionVector
       INTEGER(C_INT)   , INTENT(OUT) :: A
     END FUNCTION cgp_array_write
  END INTERFACE


  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_array_write_data(A, rmin, rmax, data) BIND(C, name='cgp_array_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: A
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_array_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_elements_write_data(fn,B,Z,S,emin,emax,elements) BIND(C, name='cgp_elements_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       CGSIZE_T, INTENT(IN), VALUE :: emin
       CGSIZE_T, INTENT(IN), VALUE :: emax
       TYPE(C_PTR), VALUE :: elements
     END FUNCTION cgp_elements_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_elements_read_data(fn,B,Z,S,start,end,elements) BIND(C, name='cgp_elements_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       CGSIZE_T, INTENT(IN), VALUE :: start
       CGSIZE_T, INTENT(IN), VALUE :: end
       TYPE(C_PTR), VALUE :: elements
     END FUNCTION cgp_elements_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_array_read_data(A, rmin, rmax, data) BIND(C, name='cgp_array_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: A
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_array_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_sol_write(fn,B,Z,solname,location,S) BIND(C, name='cg_sol_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: solname
       INTEGER(C_INT)   , INTENT(IN), VALUE :: location
       INTEGER(C_INT)   , INTENT(OUT) :: S
     END FUNCTION cg_sol_write
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_error_exit() BIND(C, name='cgp_error_exit')
       USE ISO_C_BINDING
     END FUNCTION cgp_error_exit
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_close(fn) BIND(C, name='cgp_close')
       USE ISO_C_BINDING
       INTEGER(C_INT), INTENT(IN), VALUE :: fn
     END FUNCTION cgp_close
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_queue_set(use_queue) BIND(C, name='cgp_queue_set')
       USE ISO_C_BINDING
       INTEGER(C_INT), INTENT(IN), VALUE :: use_queue
     END FUNCTION cgp_queue_set
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_queue_flush() BIND(C, name='cgp_queue_flush')
       USE ISO_C_BINDING
     END FUNCTION cgp_queue_flush
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_user_data_write(UserDataName) BIND(C, name='cg_user_data_write')
       USE ISO_C_BINDING
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: UserDataName
     END FUNCTION cg_user_data_write
  END INTERFACE

#ifdef HAVE_F2008TS_REQUIREMENTS

  ! THE FOLLOWING CODE ONLY WORKS FOR COMPILERS HAVING F2008 STANDARD EXTENSION:
  ! TS 29113 Further Interoperability of FORTRAN with C WG5/N1942

  ! The number of optional parameters should be set to
  ! CG_MAX_GOTO_DEPTH, which is currently set to 20.
  INTERFACE
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
          end) BIND(C, name='cg_gorel_f08')
       
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
 END INTERFACE

 ! The number of optional parameters should be set to
 ! CG_MAX_GOTO_DEPTH, which is currently set to 20.

 INTERFACE
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
          end) BIND(C, name='cg_goto_f08')

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
 END INTERFACE

#endif
END MODULE cgns_c_binding

MODULE testing_functions
!
! Contains functions to verify values
!
  INTERFACE check_eq
     MODULE PROCEDURE c_float_eq, c_double_eq, c_long_eq, c_long_long_eq
  END INTERFACE

CONTAINS

  LOGICAL FUNCTION c_float_eq(a,b)
    USE ISO_C_BINDING
    IMPLICIT NONE
    ! Check if two C_FLOAT reals are equivalent
    REAL(C_FLOAT), INTENT(IN):: a,b
    REAL(C_FLOAT), PARAMETER :: eps = 1.e-8
    c_float_eq = ABS(a-b) .LT. eps
  END FUNCTION c_float_eq
  
  LOGICAL FUNCTION c_double_eq(a,b)
    USE ISO_C_BINDING
    IMPLICIT NONE
    ! Check if two C_DOUBLE reals are equivalent
    REAL(C_DOUBLE), INTENT(IN):: a,b
    REAL(C_DOUBLE), PARAMETER :: eps = 1.e-8
    c_double_eq = ABS(a-b) .LT. eps
  END FUNCTION c_double_eq
  
  LOGICAL FUNCTION c_long_eq(a,b)
    USE ISO_C_BINDING
    IMPLICIT NONE
    ! Check if two C_LONG integers are equivalent
    INTEGER(C_INT32_T), INTENT(IN):: a,b
    c_long_eq = a-b .EQ. 0_C_LONG
  END FUNCTION c_long_eq
  
  LOGICAL FUNCTION c_long_long_eq(a,b)
    USE ISO_C_BINDING
    IMPLICIT NONE
    ! Check if two C_LONG_LONG integers are equivalent
    INTEGER(C_INT64_T), INTENT(IN):: a,b
    c_long_long_eq = a-b .EQ. 0_C_LONG_LONG
  END FUNCTION c_long_long_eq
  
END MODULE testing_functions

PROGRAM main

  USE mpi
  USE ISO_C_BINDING
  USE cgns_c_binding
  USE testing_functions
  IMPLICIT NONE

  INCLUDE 'cgnslib_f.h'

  INTEGER, PARAMETER :: dp = KIND(1.d0)
  CGSIZE_T, PARAMETER :: Nelem = 1024 !33554432 ! Use multiples of number of cores per node
  CGSIZE_T, PARAMETER :: NodePerElem = 6

  CGSIZE_T :: Nnodes
  INTEGER(C_INT) :: mpi_err
  INTEGER(C_INT) :: err
  INTEGER :: ierr
  INTEGER(C_INT) :: comm_size
  INTEGER(C_INT) :: comm_rank
  INTEGER(C_INT) :: info
  INTEGER(C_INT) :: fn
  INTEGER(C_INT) :: B
  INTEGER(C_INT) :: Z
  INTEGER(C_INT) :: S
  INTEGER(C_INT) :: Cx,Cy,Cz, Fx, Fy, Fz, Ar, Ai
  INTEGER(C_INT), PARAMETER :: cell_dim = 3
  INTEGER(C_INT), PARAMETER :: phys_dim = 3
  INTEGER(C_INT) :: r_cell_dim = 0
  INTEGER(C_INT) :: r_phys_dim = 0
  CGSIZE_T, DIMENSION(1:3) :: nijk, sizes
  CGSIZE_T, DIMENSION(1:1) :: size_1D
  CGSIZE_T :: min, max
  INTEGER(C_INT) :: k, count
  ! For writing and reading data
  REAL(C_DOUBLE), DIMENSION(:), ALLOCATABLE, TARGET :: Coor_x, Coor_y, Coor_z
  REAL(C_DOUBLE), DIMENSION(:), ALLOCATABLE, TARGET :: Data_Fx, Data_Fy, Data_Fz
  REAL(C_DOUBLE), DIMENSION(:), ALLOCATABLE, TARGET :: Array_r
  CGSIZE_T, DIMENSION(:), ALLOCATABLE, TARGET :: Array_i
  CGSIZE_T :: start, end, emin, emax
  CGSIZE_T, DIMENSION(:), ALLOCATABLE, TARGET :: elements
  LOGICAL :: queue, debug
  TYPE(C_PTR) :: f_ptr, f_ptr1, f_ptr2, f_ptr3
  CHARACTER(KIND=C_CHAR,LEN=180) :: bname, zname
  INTEGER :: indx_null
  ! Timing 
  REAL(KIND=dp) :: t0, t1, t2

  ! Timing storage convention:
  ! timing(0) = Total program time
  ! timing(1) = Time to write nodal coordinates
  ! timing(2) = Time to write connectivity table
  ! timing(3) = Time to write solution data (field data)
  ! timing(4) = Time to write array data
  ! timing(5) = Time to read nodal coordinates
  ! timing(6) = Time to read connectivity table
  ! timing(7) = Time to read solution data (field data)
  ! timing(8) = Time to read array data
  ! timing(9) = Time for cgp_open, CG_MODE_WRITE 
  ! timing(10) = Time for cg_base_write 
  ! timing(11) = Time for cg_zone_write
  ! timing(12) = Time for cgp_open, CG_MODE_READ
  ! timing(13) = Time for cg_base_read 
  ! timing(14) = Time for cg_zone_read
  REAL(KIND=dp), DIMENSION(1:15) :: xtiming, timing, timingMin, timingMax
  CHARACTER(LEN=6) :: ichr6

  ! CGP_INDEPENDENT is the default
  INTEGER(C_INT), DIMENSION(1:2) :: piomode = (/CGP_INDEPENDENT, CGP_COLLECTIVE/)
  CHARACTER(LEN=3), DIMENSION(1:2) :: piomodeC= (/"ind", "col"/)
  CHARACTER(LEN=6), DIMENSION(1:2) :: outmode = (/'direct','queued'/)
  INTEGER :: piomode_i
  INTEGER :: istat
  INTEGER(C_SIZE_T) :: int_sizeof
  INTEGER :: comm_info
  INTEGER(C_INT), DIMENSION(1:3), TARGET :: Cvec
  
  CALL MPI_Init(mpi_err)
  CALL MPI_Comm_size(MPI_COMM_WORLD,comm_size,mpi_err)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,comm_rank,mpi_err)
  CALL MPI_Info_Create(comm_info,mpi_err)

  CALL MPI_Info_set(comm_info, "striping_unit", "8388608", mpi_err);

  WRITE(ichr6,'(I6.6)') comm_size

  ! parameters
  piomode_i = 2
 ! queue = .TRUE.
  queue = .FALSE.
  debug = .FALSE.
  debug = .TRUE.

  t0 = MPI_Wtime() ! Timer

!!$  err = cgp_pio_mode(piomode(piomode_i)) ! default

  CALL cgp_pio_mode_f(piomode_i-1, comm_info, ierr)
  
  Nnodes = Nelem*NodePerElem

  nijk(1) = Nnodes ! Number of vertices
  nijk(2) = Nelem ! Number of cells
  nijk(3) = 0 ! Number of boundary vertices

! ======================================
! ==    **WRITE THE CGNS FILE *       ==
! ======================================

  t1 = MPI_Wtime()
  err = cgp_open("benchmark_"//ichr6//".cgns"//C_NULL_CHAR, CG_MODE_WRITE, fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_open'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(10) = t2-t1

  t1 = MPI_Wtime()
  err = cg_base_write(fn, "Base 1"//C_NULL_CHAR, cell_dim, phys_dim, B)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_base_write'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(11) = t2-t1

  t1 = MPI_Wtime()
  err = cg_zone_write(fn, B, "Zone 1"//C_NULL_CHAR, nijk, Unstructured, Z)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_zone_write'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(12) = t2-t1


  ! use queued IO
  IF(queue) THEN
     err = cgp_queue_set(1_C_INT)
  ELSE
     err = cgp_queue_set(0_C_INT) ! default
  ENDIF
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_queue_set'
     err = cgp_error_exit()
  ENDIF

! ======================================
! == (A) WRITE THE NODAL COORDINATES  ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Coor_x(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_x'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Coor_y(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_y'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Coor_z(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_z'
     err = cgp_error_exit()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  DO k = 1, count
     Coor_x(k) = REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.1_C_DOUBLE
     Coor_y(k) = Coor_x(k) + 0.1_C_DOUBLE
     Coor_z(k) = Coor_y(k) + 0.1_C_DOUBLE
  ENDDO

  err = cgp_coord_write(fn,B,Z,RealDouble,"CoordinateX"//C_NULL_CHAR,Cx)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write (Coor_X)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_write(fn,B,Z,RealDouble,"CoordinateY"//C_NULL_CHAR,Cy)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write (Coor_Y)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_write(fn,B,Z,RealDouble,"CoordinateZ"//C_NULL_CHAR,Cz)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write (Coor_Z)'
     err = cgp_error_exit()
  ENDIF

  f_ptr1 = C_LOC(Coor_x(1))
  f_ptr2 = C_LOC(Coor_y(1))
  f_ptr3 = C_LOC(Coor_z(1))
  t1 = MPI_Wtime()
  err = cgp_coord_write_data(fn,B,Z,Cx,min,max,f_ptr1)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data (Coor_x)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_write_data(fn,B,Z,Cy,min,max,f_ptr2)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data (Coor_y)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_write_data(fn,B,Z,Cz,min,max,f_ptr3)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data (Coor_z)'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(2) = t2-t1 

  ! We need to keep the arrays allocate until cgp_queue_flush
  IF(.NOT.queue)THEN
     DEALLOCATE(Coor_x)
     DEALLOCATE(Coor_y)
     DEALLOCATE(Coor_z)
  ENDIF

! ======================================
! == (B) WRITE THE CONNECTIVITY TABLE ==
! ======================================

  start = 1
  end = nijk(2)

  err = cgp_section_write(fn,B,Z,"Elements"//C_NULL_CHAR,PENTA_6,start,end,0_C_INT,S)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_section_write'
     err = cgp_error_exit()
  ENDIF
 
  count = nijk(2)/comm_size
  ALLOCATE(elements(1:count*NodePerElem), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of elements'
     err = cgp_error_exit()
  ENDIF
  
  ! Create ridiculous connectivity table ...
  DO k = 1, count*NodePerElem
     elements(k) = comm_rank*count*NodePerElem + k
  ENDDO

  emin = count*comm_rank+1
  emax = count*(comm_rank+1)

  t1 = MPI_Wtime()
  f_ptr = C_LOC(elements(1))
  err = cgp_elements_write_data(fn, B, Z, S, emin, emax, f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_elements_write_data (elements)'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(3) = t2-t1

  IF(.NOT.queue)THEN
     DEALLOCATE(elements)
  ENDIF


! ======================================
! == (C) WRITE THE FIELD DATA         ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Data_Fx(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fx'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Data_Fy(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fy'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Data_Fz(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fz'
     err = cgp_error_exit()
  ENDIF

  DO k = 1, count
     Data_Fx(k) = REAL(comm_rank*count+k, KIND=C_DOUBLE) + 0.01_C_DOUBLE
     Data_Fy(k) = REAL(comm_rank*count+k, KIND=C_DOUBLE) + 0.02_C_DOUBLE
     Data_Fz(k) = REAL(comm_rank*count+k, KIND=C_DOUBLE) + 0.03_C_DOUBLE
  ENDDO

  err = cg_sol_write(fn, B, Z, "Solution"//C_NULL_CHAR, Vertex, S)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_sol_write'
     err = cgp_error_exit()
  ENDIF

  err = cgp_field_write(fn,B,Z,S,RealDouble,"MomentumX"//C_NULL_CHAR,Fx)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write  (MomentumX)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_field_write(fn,B,Z,S,RealDouble,"MomentumY"//C_NULL_CHAR,Fy)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write (MomentumY)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_field_write(fn,B,Z,S,RealDouble,"MomentumZ"//C_NULL_CHAR,Fz)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write (MomentumZ)'
     err = cgp_error_exit()
  ENDIF
  t1 = MPI_Wtime()

  f_ptr1 = C_LOC(Data_Fx(1))
  f_ptr2 = C_LOC(Data_Fy(1))
  f_ptr3 = C_LOC(Data_Fz(1))
  err = cgp_field_write_data(fn,B,Z,S,Fx,min,max,f_ptr1)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data (Data_Fx)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_field_write_data(fn,B,Z,S,Fy,min,max,f_ptr2) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data (Data_Fy)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_field_write_data(fn,B,Z,S,Fz,min,max,f_ptr3) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data (Data_Fz)'
     err = cgp_error_exit()
  ENDIF
 
  t2 = MPI_Wtime()
  xtiming(4) = t2-t1  

  IF(.NOT.queue)THEN
     DEALLOCATE(Data_Fx)
     DEALLOCATE(Data_Fy)
     DEALLOCATE(Data_Fz)
  ENDIF

! ======================================
! == (D) WRITE THE ARRAY DATA         ==
! ======================================
 
  count = nijk(1)/comm_size

  ALLOCATE(Array_r(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_r'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Array_i(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_i'
     err = cgp_error_exit()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  DO k = 1, count
     Array_r(k) = REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.001_C_DOUBLE
     Array_i(k) = comm_rank*count*NodePerElem + k
  ENDDO

#ifdef HAVE_F2008TS_REQUIREMENTS
  err = cg_goto(fn,B,"Zone 1"//C_NULL_CHAR,0_C_INT,END="end"//C_NULL_CHAR) 
#else
  CALL cg_goto_f(fn, B, err, "Zone 1", 0_C_INT, 'end')
#endif
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_goto_f'
     err = cgp_error_exit()
  ENDIF
  err = cg_user_data_write("User Data"//C_NULL_CHAR)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_user_data_write'
     err = cgp_error_exit()
  ENDIF
#ifdef HAVE_F2008TS_REQUIREMENTS
  err = cg_gorel(fn, "User Data"//C_NULL_CHAR, 0_C_INT, end="end"//C_NULL_CHAR)
#else
  CALL cg_gorel_f(fn,err,'User Data',0_C_INT,'end')
#endif
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_gorel'
     err = cgp_error_exit()
  ENDIF

  size_1D(1) = nijk(1)
  err = cgp_array_write("ArrayR"//C_NULL_CHAR,RealDouble,1_C_INT,size_1D,Ar)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write (Array_Ar)'
     err = cgp_error_exit()
  ENDIF

#if CG_BUILD_64BIT
  err = cgp_array_write("ArrayI"//C_NULL_CHAR,LongInteger,1_C_INT,size_1D,Ai)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write  (Array_Ai)'
     err = cgp_error_exit()
  ENDIF
#else
  err = cgp_array_write("ArrayI"//C_NULL_CHAR,INTEGER,1_C_INT,size_1D,Ai)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write  (Array Ai)'
     err = cgp_error_exit()
  ENDIF
#endif

  t1 = MPI_Wtime()
  f_ptr = C_LOC(Array_i(1))
  err = cgp_array_write_data(Ai,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write_data (Array_Ai)'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Array_r(1))
  err = cgp_array_write_data(Ar,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write_data (Array_Ar)'
     err = cgp_error_exit()
  ENDIF

  t2 = MPI_Wtime()
  xtiming(5) = t2-t1

  IF(.NOT.queue)THEN
     DEALLOCATE(Array_i)
     DEALLOCATE(Array_r)
  ENDIF

  IF(queue)THEN
     err = cgp_queue_flush()
     IF(err.NE.CG_OK)THEN
        PRINT*,'*FAILED* cgp_queue_flush'
        err = cgp_error_exit()
     ENDIF
     err = cgp_queue_set(0_C_INT) ! set for reading the data back
     IF(err.NE.CG_OK)THEN
        PRINT*,'*FAILED* cgp_queue_set'
        err = cgp_error_exit()
     ENDIF
     DEALLOCATE(Coor_x)
     DEALLOCATE(Coor_y)
     DEALLOCATE(Coor_z)
     DEALLOCATE(Data_Fx)
     DEALLOCATE(Data_Fy)
     DEALLOCATE(Data_Fz)
     DEALLOCATE(Array_i)
     DEALLOCATE(Array_r)
     DEALLOCATE(elements)
  ENDIF

  err = cgp_close(fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_close'
     err = cgp_error_exit()
  ENDIF

! ======================================
! ==    **  READ THE CGNS FILE **     ==
! ======================================
  CALL MPI_Barrier(MPI_COMM_WORLD, mpi_err)

  ! Open the cgns file  
  t1 = MPI_Wtime()
  err = cgp_open("benchmark_"//ichr6//".cgns"//C_NULL_CHAR, CG_MODE_MODIFY, fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_open'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(13) = t2-t1

  ! Read the base information
  t1 = MPI_Wtime()
  err = cg_base_read(fn, B, bname, r_cell_dim, r_phys_dim)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_base_read'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(14) = t2-t1

  IF(r_cell_dim.NE.cell_dim.OR.r_phys_dim.NE.phys_dim)THEN
     WRITE(*,'(2(A,I0))') '*FAILED* bad cell dim = ',r_cell_dim,'or phy dim =',r_phys_dim
     err = cgp_error_exit()
  ENDIF

  indx_null = INDEX(bname,C_NULL_CHAR)-1
  IF(bname(1:indx_null).NE.'Base 1')THEN
     WRITE(*,'(A,A)') '*FAILED* bad base name = ',bname(1:indx_null)
     err = cgp_error_exit()
  ENDIF

  ! Read the zone information

  t1 = MPI_Wtime()
  err = cg_zone_read(fn, B, Z, zname, sizes)
  IF(err.NE.CG_OK) PRINT*,'*FAILED* cgp_zone_read'
  t2 = MPI_Wtime()
  xtiming(15) = t2-t1

  ! Check the read zone information is correct 
  IF(sizes(1).NE.Nnodes)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num points = ',sizes(1)
     err = cgp_error_exit()
  ENDIF
     
  IF(sizes(2).NE.Nelem)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num elements = ',sizes(2)
     err = cgp_error_exit()
  ENDIF 
  IF(sizes(3).NE.0)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num elements = ',sizes(3)
     err = cgp_error_exit()
  ENDIF

  indx_null = INDEX(zname,C_NULL_CHAR)-1
  IF(zname(1:indx_null).NE.'Zone 1')THEN
     WRITE(*,'(A,A)') '*FAILED* bad zone name = ',zname(1:indx_null)
     err = cgp_error_exit()
  ENDIF

! ======================================
! ==  (A) READ THE NODAL COORDINATES  ==
! ======================================

  count = nijk(1)/comm_size
  ALLOCATE(Coor_x(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_x'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Coor_y(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_y'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Coor_z(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_z'
     err = cgp_error_exit()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  ! use queued IO
  IF(queue) err = cgp_queue_set(1_C_INT)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_queue_set'
     err = cgp_error_exit()
  ENDIF

  t1 = MPI_Wtime()  
  f_ptr1 = C_LOC(Coor_x(1))
  f_ptr2 = C_LOC(Coor_y(1))
  f_ptr3 = C_LOC(Coor_z(1))
  err = cgp_coord_read_data(fn,B,Z,Cx,min,max,f_ptr1)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data (Reading Coor_x)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_read_data(fn,B,Z,Cy,min,max,f_ptr2)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data (Reading Coor_y)'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_read_data(fn,B,Z,Cz,min,max,f_ptr3)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data (Reading Coor_z)'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(6) = t2-t1
  
  ! Check if read the data back correctly
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(Coor_x(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.1_C_DOUBLE).OR. &
             .NOT.check_eq(Coor_y(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.2_C_DOUBLE).OR. &
             .NOT.check_eq(Coor_z(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.3_C_DOUBLE)) THEN
           PRINT*,'*FAILED* cgp_coord_read_data values are incorrect'
           err = cgp_error_exit()
        ENDIF
     ENDDO
  ENDIF

  DEALLOCATE(Coor_x, Coor_y, Coor_z)

! ======================================
! == (B) READ THE CONNECTIVITY TABLE  ==
! ======================================

  count = nijk(2)/comm_size
  ALLOCATE(elements(1:count*NodePerElem), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of elements'
     err = cgp_error_exit()
  ENDIF
  
  emin = count*comm_rank+1
  emax = count*(comm_rank+1)

  t1 = MPI_Wtime()
  f_ptr = C_LOC(elements(1))
  err = cgp_elements_read_data(fn, B, Z, S, emin, emax, f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_elements_read_data ( Reading elements)'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(7) = t2-t1
 
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(elements(k), comm_rank*count*NodePerElem + k)) THEN
           PRINT*,'*FAILED* cgp_elements_read_data values are incorrect'
           err = cgp_error_exit()
        ENDIF
     ENDDO
  ENDIF

  DEALLOCATE(elements)


! ======================================
! == (C) READ THE FIELD DATA          ==
! ======================================
  count = nijk(1)/comm_size

  ALLOCATE(Data_Fx(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fx'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Data_Fy(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fy'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Data_Fz(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fz'
     err = cgp_error_exit()
  ENDIF

  t1 = MPI_Wtime()
  f_ptr = C_LOC(Data_Fx(1))
  err = cgp_field_read_data(fn,B,Z,S,Fx,min,max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Data_Fx)'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Data_Fy(1))
  err = cgp_field_read_data(fn,B,Z,S,Fy,min,max,f_ptr) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Data_Fy)'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Data_Fz(1))
  err = cgp_field_read_data(fn,B,Z,S,Fz,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Data_Fz)'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(8) = t2-t1

  ! Check if read the data back correctly
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(Data_Fx(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.01_C_DOUBLE).OR. &
             .NOT.check_eq(Data_Fy(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.02_C_DOUBLE).OR. &
             .NOT.check_eq(Data_Fz(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.03_C_DOUBLE)) THEN
           PRINT*,'*FAILED* cgp_field_read_data values are incorrect'
           err = cgp_error_exit()
        ENDIF
     ENDDO
  ENDIF

  DEALLOCATE(Data_Fx,Data_Fy,Data_Fz)

! ======================================
! == (D) READ THE ARRAY DATA          ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Array_r(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_r'
     err = cgp_error_exit()
  ENDIF
  ALLOCATE(Array_i(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_i'
     err = cgp_error_exit()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

#ifdef HAVE_F2008TS_REQUIREMENTS
  err = cg_goto(fn,B,"Zone_t"//C_NULL_CHAR,Z,"UserDefinedData_t"//C_NULL_CHAR,1_C_INT, END="end"//C_NULL_CHAR) 
#else
  CALL cg_goto_f(fn, B, err, "Zone_t",Z,"UserDefinedData_t",1_C_INT,"end") 
#endif

  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_goto (User Defined Data)'
     err = cgp_error_exit()
  ENDIF

  t1 = MPI_Wtime()
  f_ptr = C_LOC(Array_r(1))
  err = cgp_array_read_data(Ar, min, max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Array_r)'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Array_i(1))
  err = cgp_array_read_data(Ai, min, max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Array_i)'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(9) = t2-t1
  
  ! Check if read the data back correctly
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(Array_r(k), REAL(comm_rank*count + k, KIND=C_DOUBLE) + 0.001_C_DOUBLE).OR. &
             .NOT.check_eq(Array_i(k), comm_rank*count*NodePerElem + k)) THEN
           PRINT*,'*FAILED* cgp_array_read_data values are incorrect'
           err = cgp_error_exit()
        ENDIF
     ENDDO
  ENDIF

  DEALLOCATE(Array_r, Array_i)

  err = cgp_close(fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_close'
     err = cgp_error_exit()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(1) = t2-t0

  CALL MPI_Reduce(xtiming, timing, 15, MPI_DOUBLE, &
               MPI_SUM, 0, MPI_COMM_WORLD, mpi_err)
  CALL MPI_Reduce(xtiming, timingMin, 15, MPI_DOUBLE, &
               MPI_MIN, 0, MPI_COMM_WORLD, mpi_err)
  CALL MPI_Reduce(xtiming, timingMax, 15, MPI_DOUBLE, &
               MPI_MAX, 0, MPI_COMM_WORLD, mpi_err)

  IF(comm_rank.EQ.0)THEN
     OPEN(10,FILE="timing_"//ichr6//"_"//piomodeC(piomode_i)//".dat", FORM='formatted')
     WRITE(10,'(A)')"#nprocs, wcoord, welem, wfield, warray, rcoord, relem, rfield, rarray"
     WRITE(10,'(i0,100(x,3(f14.7)))') comm_size, &
         timing(1)/DBLE(comm_size), timingMin(1), timingMax(1), &
         timing(2)/DBLE(comm_size), timingMin(2), timingMax(2), &
         timing(3)/DBLE(comm_size), timingMin(3), timingMax(3), &
         timing(4)/DBLE(comm_size), timingMin(4), timingMax(4), &
         timing(5)/DBLE(comm_size), timingMin(5), timingMax(5), &
         timing(6)/DBLE(comm_size), timingMin(6), timingMax(6), &
         timing(7)/DBLE(comm_size), timingMin(7), timingMax(7), &
         timing(8)/DBLE(comm_size), timingMin(8), timingMax(8), &
         timing(9)/DBLE(comm_size), timingMin(9), timingMax(9), &
         timing(10)/DBLE(comm_size), timingMin(10), timingMax(10), &
         timing(11)/DBLE(comm_size), timingMin(11), timingMax(11), &
         timing(12)/DBLE(comm_size), timingMin(12), timingMax(12), &
         timing(13)/DBLE(comm_size), timingMin(13), timingMax(13), &
         timing(14)/DBLE(comm_size), timingMin(14), timingMax(14), &
         timing(15)/DBLE(comm_size), timingMin(15), timingMax(15)
  ENDIF

  CALL MPI_FINALIZE(mpi_err)

END PROGRAM main

! A valid F2003 compilers was not found
#else 
   PROGRAM main
     IMPLICIT NONE
     PRINT*,"**      FORTRAN 2003 STANDARD COMPILER NOT FOUND      **"
     PRINT*," benchmark_hdf5_f03 -- placeholder program used instead"
   END PROGRAM main
#endif


        
