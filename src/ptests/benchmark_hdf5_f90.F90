! @file benchmark_hdf5_f90.F90
! @author M. Scot Breitenfeld <brtnfld@hdfgroup.org>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! F90 benchmarking program for pcgns library

MODULE testing_functions
  !
  ! Contains functions to verify values
  !
  INTERFACE check_eq
     MODULE PROCEDURE c_float_eq, c_double_eq, c_long_eq, c_long_long_eq
  END INTERFACE

CONTAINS

  LOGICAL FUNCTION c_float_eq(a,b)
    IMPLICIT NONE
    ! Check if two C_FLOAT reals are equivalent
    REAL*4, INTENT(IN):: a,b
    REAL*4, PARAMETER :: eps = 1.e-8
    c_float_eq = ABS(a-b) .LT. eps
  END FUNCTION c_float_eq

  LOGICAL FUNCTION c_double_eq(a,b)
    IMPLICIT NONE
    ! Check if two C_DOUBLE reals are equivalent
    REAL*8, INTENT(IN):: a,b
    REAL*8, PARAMETER :: eps = 1.e-8
    c_double_eq = ABS(a-b) .LT. eps
  END FUNCTION c_double_eq

  LOGICAL FUNCTION c_long_eq(a,b)
    IMPLICIT NONE
    ! Check if two C_LONG integers are equivalent
    INTEGER*4, INTENT(IN):: a,b
    c_long_eq = a-b .EQ. 0
  END FUNCTION c_long_eq

  LOGICAL FUNCTION c_long_long_eq(a,b)
    IMPLICIT NONE
    ! Check if two C_LONG_LONG integers are equivalent
    INTEGER*8, INTENT(IN):: a,b
    c_long_long_eq = a-b .EQ. 0
  END FUNCTION c_long_long_eq

END MODULE testing_functions

PROGRAM benchmark_hdf5_f90

  USE mpi
  USE ISO_C_BINDING
  USE testing_functions
  USE cgns
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif

  INTEGER, PARAMETER :: dp = KIND(1.d0)
  ! Use powers of 2
  INTEGER(CGSIZE_T), PARAMETER :: Nelem = 65536 ! 33554432 ! Use multiples of number of cores per node
  INTEGER(CGSIZE_T), PARAMETER :: NodePerElem = 6

  INTEGER(CGSIZE_T) :: Nnodes
  INTEGER(C_INT) :: comm_size, comm_rank, comm_info, mpi_err
  INTEGER :: err
  INTEGER :: fn
  INTEGER :: B
  INTEGER :: Z
  INTEGER :: S
  INTEGER :: Cx,Cy,Cz, Fx, Fy, Fz, Ar, Ai
  INTEGER :: cell_dim = 3
  INTEGER :: phys_dim = 3
  INTEGER :: r_cell_dim = 0
  INTEGER :: r_phys_dim = 0
  INTEGER(CGSIZE_T), DIMENSION(1:3) :: nijk, sizes
  INTEGER(CGSIZE_T) :: min, max
  INTEGER(CGSIZE_T) :: k, count
  ! For writing and reading data
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Coor_x
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Coor_y
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Coor_z
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Data_Fx
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Data_Fy
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Data_Fz
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: Array_r
  INTEGER(CGSIZE_T), DIMENSION(:), ALLOCATABLE :: Array_i
  INTEGER(CGSIZE_T) :: start, iend, emin, emax
  INTEGER(CGSIZE_T), DIMENSION(:), ALLOCATABLE :: elements
  CHARACTER(LEN=32) :: fname, name
  CHARACTER(LEN=180) :: bname, zname
  INTEGER :: indx_null
  LOGICAL :: queue, debug
  REAL(KIND=dp) t0, t1, t2

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

  INTEGER :: ierr
  INTEGER :: istat
  CHARACTER(LEN=6) :: ichr6
  INTEGER, DIMENSION(1:3) :: Cvec, Fvec
  INTEGER, DIMENSION(1:2) :: Avec

  CALL MPI_Init(mpi_err)
  CALL MPI_Comm_size(MPI_COMM_WORLD,comm_size,mpi_err)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,comm_rank,mpi_err)

  CALL MPI_Info_Create(comm_info,mpi_err)
  ! set this to what your GPFS block size actually is
  CALL MPI_Info_set(comm_info, "striping_unit", "8388608",mpi_err)

  WRITE(ichr6,'(I6.6)') comm_size

  ! parameters
  queue = .FALSE.
  debug = .FALSE.

  t0 = MPI_Wtime()

  CALL cgp_mpi_info_f(comm_info, ierr)
  CALL cgp_pio_mode_f(CGP_COLLECTIVE, ierr)

  Nnodes = Nelem*NodePerElem

  nijk(1) = Nnodes ! Number of vertices
  nijk(2) = Nelem  ! Number of cells
  nijk(3) = 0      ! Number of boundary vertices

  ! ======================================
  ! ==    **WRITE THE CGNS FILE **      ==
  ! ======================================

  t1 = MPI_Wtime()
  CALL cgp_open_f("benchmark_"//ichr6//".cgns", CG_MODE_WRITE, fn, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_open_f'
     CALL cgp_error_exit_f()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(10) = t2-t1

  t1 = MPI_Wtime()
  CALL cg_base_write_f(fn, "Base 1", cell_dim, phys_dim, B, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_base_write_f'
     CALL cgp_error_exit_f()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(11) = t2-t1

  t1 = MPI_Wtime()
  CALL cg_zone_write_f(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), Z, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_zone_write_f'
     CALL cgp_error_exit_f()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(12) = t2-t1

  ! ======================================
  ! == (A) WRITE THE NODAL COORDINATES  ==
  ! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Coor_x(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_x'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Coor_y(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_y'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Coor_z(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_z'
     CALL cgp_error_exit_f()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  DO k = 1, count
     Coor_x(k) = REAL(comm_rank*count + k, KIND=dp) + 0.1_dp
     Coor_y(k) = Coor_x(k) + 0.1_dp
     Coor_z(k) = Coor_y(k) + 0.1_dp
  ENDDO

  CALL cgp_coord_write_f(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",Cx,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_f (Coor_X)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_write_f(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",Cy,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_f (Coor_Y)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_write_f(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",Cz,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_f (Coor_Z)'
     CALL cgp_error_exit_f()
  ENDIF

  t1 = MPI_Wtime()
#if HDF5_HAVE_MULTI_DATASETS
  Cvec(1:3) = (/Cx,Cy,Cz/)
  CALL cgp_coord_multi_write_data_f(fn,B,Z,Cvec,min,max,Coor_x,Coor_y,Coor_z,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_multi_write_data_f'
     CALL cgp_error_exit_f()
  ENDIF
#else
  CALL cgp_coord_write_data_f(fn,B,Z,Cx,min,max,Coor_x,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data_f (Coor_x)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_write_data_f(fn,B,Z,Cy,min,max,Coor_y,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data_f (Coor_y)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_write_data_f(fn,B,Z,Cz,min,max,Coor_z,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data_f (Coor_z)'
     CALL cgp_error_exit_f()
  ENDIF
#endif
  t2 = MPI_Wtime()
  xtiming(2) = t2-t1

  DEALLOCATE(Coor_x, Coor_y, Coor_z)

  ! ======================================
  ! == (B) WRITE THE CONNECTIVITY TABLE ==
  ! ======================================

  start = 1
  iend = nijk(2)
  CALL cgp_section_write_f(fn,B,Z,"Elements",CGNS_ENUMV(PENTA_6),start,iend,0,S,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_section_write_f'
     CALL cgp_error_exit_f()
  ENDIF

  count = nijk(2)/comm_size
  ALLOCATE(elements(1:count*NodePerElem), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of elements'
     CALL cgp_error_exit_f()
  ENDIF

  ! Create ridiculous connectivity table ...
  DO k = 1, count*NodePerElem
     elements(k) = comm_rank*count*NodePerElem + k
  ENDDO

  emin = count*comm_rank+1
  emax = count*(comm_rank+1)

  t1 = MPI_Wtime()
  CALL cgp_elements_write_data_f(fn, B, Z, S, emin, emax, elements,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_elements_write_data_f (elements)'
     CALL cgp_error_exit_f()
  ENDIF

  t2 = MPI_Wtime()
  xtiming(3) = t2-t1

  DEALLOCATE(elements)

  ! ======================================
  ! == (C) WRITE THE FIELD DATA         ==
  ! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Data_Fx(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fx'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fy(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fy'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fz(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fz'
     CALL cgp_error_exit_f()
  ENDIF

  DO k = 1, count
     Data_Fx(k) = REAL(comm_rank*count+k, KIND=dp) + .01_dp
     Data_Fy(k) = REAL(comm_rank*count+k, KIND=dp) + .02_dp
     Data_Fz(k) = REAL(comm_rank*count+k, KIND=dp) + .03_dp
  ENDDO

  CALL cg_sol_write_f(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), S, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_sol_write_f'
     CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_field_write_f(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumX",Fx, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write  (MomentumX)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_field_write_f(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumY",Fy, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write (MomentumY)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_field_write_f(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumZ",Fz, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write (MomentumZ)'
     CALL cgp_error_exit_f()
  ENDIF

  t1 = MPI_Wtime()
#if HDF5_HAVE_MULTI_DATASETS
  Fvec(1:3) = (/Fx,Fy,Fz/)
  CALL cgp_field_multi_write_data_f(fn,B,Z,S,Fvec,min,max,err,3,Data_Fx,Data_Fy,Data_Fz)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_multi_write_data_f'
     CALL cgp_error_exit_f()
  ENDIF
#else
  call cgp_field_write_data_f(fn,B,Z,S,Fx,min,max,Data_Fx, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data (Data_Fx)'
     CALL cgp_error_exit_f()
  ENDIF
  call cgp_field_write_data_f(fn,B,Z,S,Fy,min,max,Data_Fy, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data (Data_Fy)'
     CALL cgp_error_exit_f()
  ENDIF
  call cgp_field_write_data_f(fn,B,Z,S,Fz,min,max,Data_Fz, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data (Data_Fz)'
     CALL cgp_error_exit_f()
  ENDIF
#endif
  t2 = MPI_Wtime()
  xtiming(4) = t2-t1

  DEALLOCATE(Data_Fx,Data_Fy,Data_Fz)

  ! ======================================
  ! == (D) WRITE THE ARRAY DATA         ==
  ! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Array_r(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_r'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Array_i(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_i'
     CALL cgp_error_exit_f()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  DO k = 1, count
     Array_r(k) = REAL(comm_rank*count + k, KIND=dp) + .001_dp
     Array_i(k) = comm_rank*count*NodePerElem + k
  ENDDO

  CALL cg_goto_f(fn, B, err, 'Zone 1', 0, 'end')
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_goto_f'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cg_user_data_write_f("User Data", err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_user_data_write_f'
     CALL cgp_error_exit_f()
  ENDIF

  CALL cg_gorel_f(fn,err,'User Data',0,'end')
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_gorel_f'
     CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_array_write_f("ArrayR",cg_get_type(Array_r(1)),1,INT(nijk(1),cgsize_t),Ar, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write_f (Array_Ar)'
     CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_array_write_f("ArrayI",cg_get_type(Array_i(1)),1,INT(nijk(1),cgsize_t),Ai, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write_f   (Array_Ai)'
     CALL cgp_error_exit_f()
  ENDIF

  t1 = MPI_Wtime()

#if HDF5_HAVE_MULTI_DATASETS
  Avec = (/Ai,Ar/)
  CALL cgp_array_multi_write_data_f(fn,Avec,min,max,err,2,Array_i,Array_r)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_multi_write_data_f'
     CALL cgp_error_exit_f()
  ENDIF
#else
  CALL cgp_array_write_data_f(Ai,min,max,Array_i, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write_data_f  (Array_Ai)'
     CALL cgp_error_exit_f()
  ENDIF
  call cgp_array_write_data_f(Ar,min,max,Array_r, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write_data_f (Array_Ar)'
     CALL cgp_error_exit_f()
  ENDIF
#endif
  t2 = MPI_Wtime()
  xtiming(5) = t2-t1

  DEALLOCATE(Array_r,Array_i)

  CALL cgp_close_f(fn, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_close_f'
     CALL cgp_error_exit_f()
  ENDIF

  ! ======================================
  ! ==    **  READ THE CGNS FILE **     ==
  ! ======================================

  CALL MPI_Barrier(MPI_COMM_WORLD, mpi_err)

  t1 = MPI_Wtime()
  ! Open the cgns file for reading
  CALL cgp_open_f("benchmark_"//ichr6//".cgns", CG_MODE_MODIFY, fn, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_open_f'
     CALL cgp_error_exit_f()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(13) = t2-t1

  ! Read the base information
  t1 = MPI_Wtime()
  CALL cg_base_read_f(fn, B, bname, r_cell_dim, r_phys_dim, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_base_read_f'
     CALL cgp_error_exit_f()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(14) = t2-t1
  IF(r_cell_dim.NE.cell_dim.OR.r_phys_dim.NE.phys_dim)THEN
     WRITE(*,'(2(A,I0))') '*FAILED* bad cell dim = ',r_cell_dim,'or phy dim =',r_phys_dim
     CALL cgp_error_exit_f()
  ENDIF

  IF(TRIM(bname).NE.'Base 1')THEN
     WRITE(*,'(A,A)') '*FAILED* bad base name = ',TRIM(bname)
     CALL cgp_error_exit_f()
  ENDIF

  ! Read the zone information
  t1 = MPI_Wtime()
  CALL cg_zone_read_f(fn, B, Z, zname, sizes, err)
  IF(err.NE.CG_OK) PRINT*,'*FAILED* cgp_zone_read_f'
  t2 = MPI_Wtime()
  xtiming(15) = t2-t1

  ! Check the read zone information is correct
  IF(sizes(1).NE.Nnodes)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num points = ',sizes(1)
     CALL cgp_error_exit_f()
  ENDIF

  IF(sizes(2).NE.Nelem)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num elements = ',sizes(2)
     CALL cgp_error_exit_f()
  ENDIF
  IF(sizes(3).NE.0)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num elements = ',sizes(3)
     CALL cgp_error_exit_f()
  ENDIF

  IF(TRIM(zname).NE.'Zone 1')THEN
     WRITE(*,'(A,A)') '*FAILED* bad zone name = ',TRIM(zname)
     CALL cgp_error_exit_f()
  ENDIF


  ! ======================================
  ! ==  (A) READ THE NODAL COORDINATES  ==
  ! ======================================

  count = nijk(1)/comm_size
  ALLOCATE(Coor_x(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_x'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Coor_y(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_y'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Coor_z(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Coor_z'
     CALL cgp_error_exit_f()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  t1 = MPI_Wtime()
#if HDF5_HAVE_MULTI_DATASETS
  Cvec(1:3) = (/Cx,Cy,Cz/)
  CALL cgp_coord_multi_read_data_f(fn,B,Z,Cvec,min,max,Coor_x,Coor_y,Coor_z,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_multi_read_data_f'
     CALL cgp_error_exit_f()
  ENDIF
#else
  CALL cgp_coord_read_data_f(fn,B,Z,Cx,min,max,Coor_x,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data_f (Reading Coor_x)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_read_data_f(fn,B,Z,Cy,min,max,Coor_y,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data_f (Reading Coor_y)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_read_data_f(fn,B,Z,Cz,min,max,Coor_z,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data_f (Reading Coor_z)'
     CALL cgp_error_exit_f()
  ENDIF
#endif
  t2 = MPI_Wtime()
  xtiming(6) = t2-t1

  ! Check if read the data back correctly
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(Coor_x(k), REAL(comm_rank*count + k, KIND=DP) + 0.1_DP).OR. &
             .NOT.check_eq(Coor_y(k), REAL(comm_rank*count + k, KIND=DP) + 0.2_DP).OR. &
             .NOT.check_eq(Coor_z(k), REAL(comm_rank*count + k, KIND=DP) + 0.3_DP)) THEN
           PRINT*,'*FAILED* cgp_coord_read_data values are incorrect'
           CALL cgp_error_exit_f()
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
     CALL cgp_error_exit_f()
  ENDIF

  emin = count*comm_rank+1
  emax = count*(comm_rank+1)

  t1 = MPI_Wtime()
  CALL cgp_elements_read_data_f(fn, B, Z, S, emin, emax, elements, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_elements_read_data_f ( Reading elements)'
     CALL cgp_error_exit_f()
  ENDIF
  t2 = MPI_Wtime()
  xtiming(7) = t2-t1
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(elements(k), comm_rank*count*NodePerElem + k)) THEN
           PRINT*,'*FAILED* cgp_elements_read_data values are incorrect'
           CALL cgp_error_exit_f()
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
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fy(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fy'
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fz(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Data_Fz'
     CALL cgp_error_exit_f()
  ENDIF

  t1 = MPI_Wtime()
#if HDF5_HAVE_MULTI_DATASETS
  Fvec(1:3) = (/Fx,Fy,Fz/)
  CALL cgp_field_multi_read_data_f(fn,B,Z,S,Fvec,min,max,err,3,Data_Fx,Data_Fy,Data_Fz)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_multi_read_data_f'
     CALL cgp_error_exit_f()
  ENDIF
#else
  CALL cgp_field_read_data_f(fn,B,Z,S,Fx,min,max,Data_Fx,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Data_Fx)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_field_read_data_f(fn,B,Z,S,Fy,min,max,Data_Fy,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Data_Fy)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_field_read_data_f(fn,B,Z,S,Fz,min,max,Data_Fz,err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Data_Fz)'
     CALL cgp_error_exit_f()
  ENDIF
#endif
  t2 = MPI_Wtime()
  xtiming(8) = t2-t1

  ! Check if read the data back correctly
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(Data_Fx(k), REAL(comm_rank*count + k, KIND=dp) + 0.01_dp).OR. &
             .NOT.check_eq(Data_Fy(k), REAL(comm_rank*count + k, KIND=dp) + 0.02_dp).OR. &
             .NOT.check_eq(Data_Fz(k), REAL(comm_rank*count + k, KIND=dp) + 0.03_dp)) THEN
           PRINT*,'*FAILED* cgp_field_read_data_f values are incorrect'
           CALL cgp_error_exit_f()
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
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Array_i(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of Array_i'
     CALL cgp_error_exit_f()
  ENDIF

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  CALL cg_goto_f(fn, B, err, "Zone_t",Z,"UserDefinedData_t",1,'end')
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_goto (User Defined Data)'
     CALL cgp_error_exit_f()
  ENDIF

  t1 = MPI_Wtime()
#if HDF5_HAVE_MULTI_DATASETS
  Avec = (/Ai,Ar/)
  CALL cgp_array_multi_read_data_f(fn,Avec,min,max,err,2,Array_i,Array_r)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_multi_read_data_f'
     CALL cgp_error_exit_f()
  ENDIF
#else
  CALL cgp_array_read_data_f(Ar, min, max, Array_r, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Array_r)'
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_array_read_data_f(Ai, min, max, Array_i, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data (Array_i)'
     CALL cgp_error_exit_f()
  ENDIF
#endif
  t2 = MPI_Wtime()
  xtiming(9) = t2-t1

  ! Check if read the data back correctly
  IF(debug)THEN
     DO k = 1, count
        IF(.NOT.check_eq(Array_r(k), REAL(comm_rank*count + k, KIND=dp) + 0.001_dp).OR. &
             .NOT.check_eq(Array_i(k), comm_rank*count*NodePerElem + k)) THEN
           PRINT*,'*FAILED* cgp_array_read_data values are incorrect'
           CALL cgp_error_exit_f()
        ENDIF
     ENDDO
  ENDIF

  DEALLOCATE(Array_r, Array_i)

  CALL cgp_close_f(fn, err)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_close'
     CALL cgp_error_exit_f()
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
     OPEN(10,FILE="timing_"//ichr6//".dat", FORM='formatted')
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

END PROGRAM benchmark_hdf5_f90

