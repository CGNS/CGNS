MODULE testing_functions

  ! *********************************
  ! Useful testing helper functions
  ! *********************************

  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, PARAMETER :: TAB_SPACE = 90 ! Tab spacing for printing results

  INTEGER, PARAMETER :: skip   = -1
  INTEGER, PARAMETER :: passed =  0
  INTEGER, PARAMETER :: failed =  1

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

  SUBROUTINE write_test_header(title_header)

    ! Writes the test header

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: title_header ! test name
    INTEGER, PARAMETER :: width = TAB_SPACE+10
    CHARACTER(LEN=2*width) :: title_centered
    INTEGER :: len, i

    title_centered(:) = " "

    len=LEN_TRIM(title_header)
    title_centered(1:3) ="| |"
    title_centered((width-len)/2:(width-len)/2+len) = TRIM(title_header)
    title_centered(width-1:width+2) ="| |"

    WRITE(*,'(1X)', ADVANCE="NO")
    DO i = 1, width-1
       WRITE(*,'("_")', ADVANCE="NO")
    ENDDO
    WRITE(*,'()')
    WRITE(*,'("|  ")', ADVANCE="NO")
    DO i = 1, width-5
       WRITE(*,'("_")', ADVANCE="NO")
    ENDDO
    WRITE(*,'("  |")')

    WRITE(*,'("| |")', ADVANCE="NO")
    DO i = 1, width-5
       WRITE(*,'(1X)', ADVANCE="NO")
    ENDDO
    WRITE(*,'("| |")')

    WRITE(*,'(A)') TRIM(title_centered)

    WRITE(*,'("| |")', ADVANCE="NO")
    DO i = 1, width-5
       WRITE(*,'(1X)', ADVANCE="NO")
    ENDDO
    WRITE(*,'("| |")')

    WRITE(*,'("| |")', ADVANCE="NO")
    DO i = 1, width-5
       WRITE(*,'("_")', ADVANCE="NO")
    ENDDO
    WRITE(*,'("| |")')

    WRITE(*,'("|")', ADVANCE="NO")
    DO i = 1, width-1
       WRITE(*,'("_")', ADVANCE="NO")
    ENDDO
    WRITE(*,'("|",/)')

  END SUBROUTINE write_test_header

  SUBROUTINE write_test_status( test_result, test_title, cause)

    ! Writes the results of the tests

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: test_result  ! negative,  --skip --
                                        ! 0       ,   passed
                                        ! positive,   failed

    CHARACTER(LEN=*), INTENT(IN) :: test_title ! Short description of test
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cause ! Print error cause

    ! Controls the output style for reporting test results

    CHARACTER(LEN=8) :: error_string
    CHARACTER(LEN=8), PARAMETER :: passed = ' PASSED '
    CHARACTER(LEN=8), PARAMETER :: failed = '*FAILED*'
    CHARACTER(LEN=8), PARAMETER :: skip    = '--SKIP--'
    CHARACTER(LEN=10) :: FMT

    error_string = failed
    IF (test_result ==  0) THEN
       error_string = passed
    ELSE IF (test_result == -1) THEN
       error_string = skip
    ENDIF
    WRITE(FMT,'("(A,T",I0,",A)")') TAB_SPACE
    WRITE(*, fmt = FMT) test_title, error_string

    IF(PRESENT(cause)) WRITE(*,'(3X,"FAILURE REPORTED --", A)') cause

  END SUBROUTINE write_test_status

  SUBROUTINE write_test_footer()

    ! Writes the test footer

    IMPLICIT NONE
    INTEGER, PARAMETER :: width = TAB_SPACE+10
    INTEGER :: i

    DO i = 1, width
       WRITE(*,'("_")', ADVANCE="NO")
    ENDDO
    WRITE(*,'(/)')

  END SUBROUTINE write_test_footer

END MODULE testing_functions

MODULE ftests

  USE mpi
  USE ISO_C_BINDING
  USE CGNS
  USE testing_functions
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif

  INTEGER(C_INT) commsize, commrank, mpi_err

CONTAINS

  SUBROUTINE general_ptests()

  INTEGER(cgsize_t), PARAMETER :: totcnt = 40320 * 10
  INTEGER, PARAMETER :: NLOOPS = 5000
  INTEGER(cgsize_t) npp
  INTEGER i, nb, nz, nerrs
  INTEGER ierr, F, B, Z, E, S
  INTEGER Cx, Cy, Cz, Fx, Fy, Fz, Ax, Ay, Az
  INTEGER(cgsize_t) sizes(3), start, END, n, j
  INTEGER(cgsize_t), PARAMETER :: start_1 = 1
  REAL*8 ts, te, tt, dsize
  REAL*8 dx(totcnt), dy(totcnt), dz(totcnt)
  INTEGER(cgsize_t), ALLOCATABLE, DIMENSION(:) :: ie
  CHARACTER*32 name
  CHARACTER*11 piomode(2)
  INTEGER :: istat
  INTEGER :: precision
  INTEGER, TARGET :: value

  DATA piomode /'independent','collective'/

  IF (commsize .GT. 8) THEN
     IF (commrank .EQ. 0) &
          PRINT *, 'number of processes must be 8 or less'
     CALL cgp_error_exit_f
     STOP
  ENDIF

  ALLOCATE(ie(1:4*totcnt), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of ie'
     CALL cgp_error_exit_f()
  ENDIF

  npp = totcnt / commsize
  start = commrank * npp + 1
  end = start + npp - 1

  j = 0
  DO n=1,npp
     dx(n) = start + n - 1
     dy(n) = commrank + 1
     dz(n) = n
     DO i=1,4
        j = j + 1
        ie(j) = start + n - 1
     ENDDO
  ENDDO
  sizes(1) = totcnt
  sizes(2) = totcnt
  sizes(3) = 0

  dsize = (9.0 * totcnt * 8.0 + 4.0 * totcnt * 4.0) / (1024.0 * 1024.0)

  IF (commrank .EQ. 0) THEN
     WRITE(*,'(A,1X,I0)') 'number processes       =', commsize
     WRITE(*,'(A,1X,I0)') 'array size per process =', npp
     WRITE(*,'(A,1X,I0)') 'total array size       =', totcnt
     WRITE(*,'(A,1X,F11.6)') 'total Mb for all data  =', dsize
  ENDIF

  ! default is MPI_COMM_WORLD, but can set another communicator with this
  ! call cgp_mpi_comm_f(MPI_COMM_WORLD,ierr)

  ! Check repeated opening and closing of a file to detect issues with 
  ! missed closed HDF5 objects, CGNS-109.

  ! Note, this is extremely slow when used with Github Actions 
  ! and Linux, so it is disabled in that case.
#ifndef DISABLE_CGNS109
  DO n = 1, NLOOPS
     CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_WRITE,F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_close_f(F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  ENDDO
#endif

  CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_WRITE,F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  DO nb=1,2
     WRITE(name,'(a4,i2)') 'Base',nb
     CALL cg_base_write_f(F,name,3,3,B,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_pio_mode_f(INT(nb-1,C_INT), ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     DO nz=1,2
        WRITE(name,'(a4,i2)') 'Zone',nz
        CALL cg_zone_write_f(F,B,name,sizes,CGNS_ENUMV(Unstructured),Z,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_f(F,B,Z,CGNS_ENUMV(RealDouble),'CoordinateX',Cx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_f(F,B,Z,CGNS_ENUMV(RealDouble),'CoordinateY',Cy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_f(F,B,Z,CGNS_ENUMV(RealDouble),'CoordinateZ',Cz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_section_write_f(F,B,Z,'Tets',CGNS_ENUMV(TETRA_4),start_1,totcnt,0,E,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_sol_write_f(F,B,Z,'Solution',CGNS_ENUMV(Vertex),S,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_f(F,B,Z,S,CGNS_ENUMV(RealDouble),'MomentumX',Fx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_f(F,B,Z,S,CGNS_ENUMV(RealDouble),'MomentumY',Fy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_f(F,B,Z,S,CGNS_ENUMV(RealDouble),'MomentumZ',Fz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_goto_f(F,B,ierr,name,0,'end')
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_user_data_write_f('User Data',ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_gorel_f(F,ierr,'User Data',0,'end')
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_f('ArrayX',CGNS_ENUMV(RealDouble),1,totcnt,Ax,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_f('ArrayY',CGNS_ENUMV(RealDouble),1,totcnt,Ay,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_f('ArrayZ',CGNS_ENUMV(RealDouble),1,totcnt,Az,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
        ts = MPI_WTIME()

        CALL cgp_coord_write_data_f(F,B,Z,Cx,start,END,dx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_data_f(F,B,Z,Cy,start,END,dy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_data_f(F,B,Z,Cz,start,END,dz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_elements_write_data_f(F,B,Z,E,start,END,ie,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_data_f(F,B,Z,S,Fx,start,END,dx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_data_f(F,B,Z,S,Fy,start,END,dy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_data_f(F,B,Z,S,Fz,start,END,dz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_goto_f(F,B,ierr,'Zone_t',Z,'UserDefinedData_t',1,'end')
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_data_f(Ax,start,END,dx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_data_f(Ay,start,END,dy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_data_f(Az,start,END,dz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

        CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
        te = MPI_WTIME()
        tt = te - ts;
        IF (commrank .EQ. 0) &
             WRITE(*,'(A,F11.6,A,F11.6,A)') 'write:',tt,' secs,', dsize/tt, ' Mb/sec ('// piomode(nb)//')'
     ENDDO
  ENDDO
  CALL cgp_close_f(F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_READ,F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cg_precision_f(F, PRECISION, ierr)
  IF (commrank .EQ. 0) WRITE(*,'(A,I0)') 'cg_precision_f PRECISION ',PRECISION

  Z = 1
  S = 1
  E = 1
  DO B=1,2
     CALL cgp_pio_mode_f(INT(B-1,C_INT),ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     ts = MPI_WTIME()

     CALL cgp_coord_read_data_f(F,B,Z,1,start,END,dx,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_coord_read_data_f(F,B,Z,2,start,END,dy,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_coord_read_data_f(F,B,Z,3,start,END,dz,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_elements_read_data_f(F,B,Z,E,start,END,ie,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     te = MPI_WTIME()
     tt = te - ts

     nerrs = 0
     j = 0
     DO n=1,npp
        IF (dx(n) .NE. (start+n-1)) nerrs = nerrs + 1
        IF (dy(n) .NE. (commrank+1)) nerrs = nerrs + 1
        IF (dz(n) .NE. n) nerrs = nerrs + 1
        DO i=1,4
           j = j + 1
         !  IF (ie(j) .NE. (start+n-1)) nerrs = nerrs + 1
        ENDDO
     ENDDO

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     ts = MPI_WTIME()

     CALL cgp_field_read_data_f(F,B,Z,S,1,start,END,dx,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_field_read_data_f(F,B,Z,S,2,start,END,dy,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_field_read_data_f(F,B,Z,S,3,start,END,dz,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     te = MPI_WTIME()
     tt = tt + te - ts

     DO n=1,npp
        IF (dx(n) .NE. (start+n-1)) nerrs = nerrs + 1
        IF (dy(n) .NE. (commrank+1)) nerrs = nerrs + 1
        IF (dz(n) .NE. n) nerrs = nerrs + 1
     ENDDO

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     ts = MPI_WTIME()
     CALL cg_goto_f(F,B,ierr,'Zone_t',Z,'UserDefinedData_t',1,'end')
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_array_read_data_f(1,start,END,dx,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_array_read_data_f(2,start,END,dy,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_array_read_data_f(3,start,END,dz,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     te = MPI_WTIME()
     tt = tt + te - ts

     DO n=1,npp
        IF (dx(n) .NE. (start+n-1)) nerrs = nerrs + 1
        IF (dy(n) .NE. (commrank+1)) nerrs = nerrs + 1
        IF (dz(n) .NE. n) nerrs = nerrs + 1
     ENDDO

     IF (commrank .EQ. 0) THEN
        WRITE(*,'(A,F11.6,A,F11.6,3A,I0)') 'read :',tt,' secs,',dsize/tt,' Mb/sec (', &
             piomode(B),') errors = ',nerrs
     ENDIF
  ENDDO

  CALL cgp_close_f(F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Disable with gfortran, GCC Bugzilla - Bug 99982
#ifndef __GFORTRAN__ 
  ! test cg_configure_f
  value = MPI_COMM_SELF
  CALL cg_configure_f(CG_CONFIG_HDF5_MPI_COMM, C_LOC(value), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  IF (commrank .EQ. 0) THEN
     CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_READ,F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_close_f(F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  ENDIF
#endif

END SUBROUTINE general_ptests

SUBROUTINE multisets()

  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = KIND(1.d0)
  INTEGER(CGSIZE_T), PARAMETER :: NodePerElem = 6
  INTEGER(cgsize_t) :: nelem = 1024

  INTEGER(CGSIZE_T) :: Nnodes
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
  INTEGER(CGSIZE_T), DIMENSION(1) :: vmin, vmax
  INTEGER(CGSIZE_T) :: k, count
  ! For writing and reading data
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Coor_x
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Coor_y
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Coor_z
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Data_Fx
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Data_Fy
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Data_Fz
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE, TARGET :: Array_r
  INTEGER(CGSIZE_T), DIMENSION(:), ALLOCATABLE, TARGET :: Array_i
  INTEGER(CGSIZE_T) :: start, iend, emin, emax
  INTEGER(CGSIZE_T), DIMENSION(:), ALLOCATABLE :: elements
  CHARACTER(LEN=32) :: fname, name
  CHARACTER(LEN=180) :: bname, zname
  INTEGER :: indx_null
  REAL(KIND=dp) t0, t1, t2
  TYPE(C_PTR), DIMENSION(1:2) :: buf2
  TYPE(C_PTR), DIMENSION(1:3) :: buf3

  INTEGER :: ierr
  INTEGER :: istat
  CHARACTER(LEN=6) :: ichr6
  INTEGER, DIMENSION(1:3) :: Cvec, Fvec
  INTEGER, DIMENSION(1:2) :: Avec

  INTEGER :: piomode = CGP_COLLECTIVE

  WRITE(ichr6,'(I6.6)') commsize

  CALL cgp_pio_mode_f(piomode, ierr)

  Nnodes = Nelem*NodePerElem

  nijk(1) = Nnodes ! Number of vertices
  nijk(2) = Nelem  ! Number of cells
  nijk(3) = 0      ! Number of boundary vertices

  ! ======================================
  ! ==    **WRITE THE CGNS FILE **      ==
  ! ======================================

  CALL cgp_open_f('pcgns_ftest_md.cgns', CG_MODE_WRITE, fn, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_open_f")
     CALL cgp_error_exit_f()
  ENDIF

  CALL cg_base_write_f(fn, "Base 1", cell_dim, phys_dim, B, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_base_write_f")
     CALL cgp_error_exit_f()
  ENDIF

  CALL cg_zone_write_f(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), Z, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_zone_write_f")
     CALL cgp_error_exit_f()
  ENDIF

  ! ======================================
  ! == (A) WRITE THE NODAL COORDINATES  ==
  ! ======================================

  count = nijk(1)/commsize

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

  vmin(1) = count*commrank+1
  vmax(1) = count*(commrank+1)

  DO k = 1, count
     Coor_x(k) = REAL(commrank*count + k, KIND=dp) + 0.1_dp
     Coor_y(k) = Coor_x(k) + 0.1_dp
     Coor_z(k) = Coor_y(k) + 0.1_dp
  ENDDO

  CALL cgp_coord_write_f(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",Cx,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed,"cgp_coord_write_f (Coor_X)")
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_write_f(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",Cy,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_coord_write_f (Coor_Y)")
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_coord_write_f(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",Cz,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_coord_write_f (Coor_Z)")
     CALL cgp_error_exit_f()
  ENDIF

  Cvec(1:3) = (/Cx,Cy,Cz/)

  buf3(1) = C_LOC(Coor_x)
  buf3(2) = C_LOC(Coor_y)
  buf3(3) = C_LOC(Coor_z)

  CALL cgp_coord_multi_write_data_f(fn,B,Z,Cvec,vmin,vmax,3,buf3,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_coord_multi_write_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_coord_multi_write_data_f")
  ENDIF

  DEALLOCATE(Coor_x, Coor_y, Coor_z)

  ! ======================================
  ! == (B) WRITE THE CONNECTIVITY TABLE ==
  ! ======================================

  start = 1
  iend = nijk(2)
  CALL cgp_section_write_f(fn,B,Z,"Elements",CGNS_ENUMV(PENTA_6),start,iend,0,S,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_section_write_f")
     CALL cgp_error_exit_f()
  ENDIF

  count = nijk(2)/commsize
  ALLOCATE(elements(1:count*NodePerElem), STAT = istat)
  IF (istat.NE.0)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "allocation of elements")
     CALL cgp_error_exit_f()
  ENDIF

  ! Create ridiculous connectivity table ...
  DO k = 1, count*NodePerElem
     elements(k) = commrank*count*NodePerElem + k
  ENDDO

  emin = count*commrank+1
  emax = count*(commrank+1)

  CALL cgp_elements_write_data_f(fn, B, Z, S, emin, emax, elements,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_elements_write_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_elements_write_data_f")
  ENDIF

  DEALLOCATE(elements)

  ! ======================================
  ! == (C) WRITE THE FIELD DATA         ==
  ! ======================================

  count = nijk(1)/commsize

  ALLOCATE(Data_Fx(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "allocation of Data_Fx")
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fy(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "allocation of Data_Fy")
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fz(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "allocation of Data_Fz")
     CALL cgp_error_exit_f()
  ENDIF

  DO k = 1, count
     Data_Fx(k) = REAL(commrank*count+k, KIND=dp) + .01_dp
     Data_Fy(k) = REAL(commrank*count+k, KIND=dp) + .02_dp
     Data_Fz(k) = REAL(commrank*count+k, KIND=dp) + .03_dp
  ENDDO

  CALL cg_sol_write_f(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), S, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_sol_write_f")
     CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_field_write_f(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumX",Fx, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_field_write  (MomentumX)")
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_field_write_f(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumY",Fy, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_field_write (MomentumY)")
     CALL cgp_error_exit_f()
  ENDIF
  CALL cgp_field_write_f(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumZ",Fz, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_field_write (MomentumZ)")
     CALL cgp_error_exit_f()
  ENDIF

  Fvec(1:3) = (/Fx,Fy,Fz/)

  buf3(1) = C_LOC(Data_Fx)
  buf3(2) = C_LOC(Data_Fy)
  buf3(3) = C_LOC(Data_Fz)

  CALL cgp_field_multi_write_data_f(fn,B,Z,S,Fvec,vmin,vmax,3,buf3,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_field_multi_write_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_field_multi_write_data_f")
  ENDIF

  DEALLOCATE(Data_Fx,Data_Fy,Data_Fz)

  ! ======================================
  ! == (D) WRITE THE ARRAY DATA         ==
  ! ======================================

  count = nijk(1)/commsize

  ALLOCATE(Array_r(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Array_r"
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Array_i(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Array_i"
     CALL cgp_error_exit_f()
  ENDIF

  vmin(1) = count*commrank+1
  vmax(1)= count*(commrank+1)

  DO k = 1, count
     Array_r(k) = REAL(commrank*count + k, KIND=dp) + .001_dp
     Array_i(k) = commrank*count*NodePerElem + k
  ENDDO

  CALL cg_goto_f(fn, B, err, "Zone 1", 0, "end")
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_goto_f")
     CALL cgp_error_exit_f()
  ENDIF
  CALL cg_user_data_write_f("User Data", err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_user_data_write_f")
     CALL cgp_error_exit_f()
  ENDIF

  CALL cg_gorel_f(fn,err,"User Data",0,"end")
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_gorel_f")
     CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_array_write_f("ArrayR",cg_get_type(Array_r(1)),1,INT(nijk(1),cgsize_t),Ar, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_array_write_f (Array_Ar)")
     CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_array_write_f("ArrayI",cg_get_type(Array_i(1)),1,INT(nijk(1),cgsize_t),Ai, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_array_write_f (Array_Ai)")
     CALL cgp_error_exit_f()
  ENDIF

  Avec = (/Ai,Ar/)
  buf2(1) = C_LOC(Array_i)
  buf2(2) = C_LOC(Array_r)
  CALL cgp_array_multi_write_data_f(fn,Avec,vmin,vmax,2,buf2,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_array_multi_write_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_array_multi_write_data_f")
  ENDIF

  DEALLOCATE(Array_r,Array_i)

  CALL cgp_close_f(fn, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_close_f")
     CALL cgp_error_exit_f()
  ENDIF

  ! ======================================
  ! ==    **  READ THE CGNS FILE **     ==
  ! ======================================

  CALL MPI_Barrier(MPI_COMM_WORLD, mpi_err)

  ! Open the cgns file for reading
  CALL cgp_open_f('pcgns_ftest_md.cgns', CG_MODE_MODIFY, fn, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_open_f")
     CALL cgp_error_exit_f()
  ENDIF

  ! Read the base information
  CALL cg_base_read_f(fn, B, bname, r_cell_dim, r_phys_dim, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_base_read_f")
     CALL cgp_error_exit_f()
  ENDIF
  IF(r_cell_dim.NE.cell_dim.OR.r_phys_dim.NE.phys_dim)THEN
     WRITE(*,'(2(A,I0))') '*FAILED* bad cell dim = ',r_cell_dim,'or phy dim =',r_phys_dim
     CALL cgp_error_exit_f()
  ENDIF

  IF(TRIM(bname).NE."Base 1")THEN
     WRITE(*,'(A,A)') "*FAILED* bad base name = ",TRIM(bname)
     CALL cgp_error_exit_f()
  ENDIF

  ! Read the zone information
  CALL cg_zone_read_f(fn, B, Z, zname, sizes, err)
  IF(err.NE.CG_OK.AND. commrank .EQ. 0) CALL write_test_status(failed, "cgp_zone_read_f")

  ! Check the read zone information is correct
  IF(sizes(1).NE.Nnodes)THEN
     WRITE(*,'(A,I0)') "*FAILED* bad num points = ",sizes(1)
     CALL cgp_error_exit_f()
  ENDIF

  IF(sizes(2).NE.Nelem)THEN
     WRITE(*,'(A,I0)') "*FAILED* bad num elements = ",sizes(2)
     CALL cgp_error_exit_f()
  ENDIF
  IF(sizes(3).NE.0)THEN
     WRITE(*,'(A,I0)') "*FAILED* bad num elements = ",sizes(3)
     CALL cgp_error_exit_f()
  ENDIF

  IF(TRIM(zname).NE.'Zone 1')THEN
     WRITE(*,'(A,A)') "*FAILED* bad zone name = ",TRIM(zname)
     CALL cgp_error_exit_f()
  ENDIF


  ! ======================================
  ! ==  (A) READ THE NODAL COORDINATES  ==
  ! ======================================

  count = nijk(1)/commsize
  ALLOCATE(Coor_x(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Coor_x"
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Coor_y(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Coor_y"
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Coor_z(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Coor_z"
     CALL cgp_error_exit_f()
  ENDIF

  vmin = count*commrank+1
  vmax = count*(commrank+1)

  Cvec(1:3) = (/Cx,Cy,Cz/)

  ! Point to the read buffers
  buf3(1) = C_LOC(Coor_x)
  buf3(2) = C_LOC(Coor_y)
  buf3(3) = C_LOC(Coor_z)

  CALL cgp_coord_multi_read_data_f(fn,B,Z,Cvec,vmin,vmax,3,buf3,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_coord_multi_read_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_coord_multi_read_data_f")
  ENDIF

  ! Check if read the data back correctly
  DO k = 1, count
     IF(.NOT.check_eq(Coor_x(k), REAL(commrank*count + k, KIND=DP) + 0.1_DP).OR. &
          .NOT.check_eq(Coor_y(k), REAL(commrank*count + k, KIND=DP) + 0.2_DP).OR. &
          .NOT.check_eq(Coor_z(k), REAL(commrank*count + k, KIND=DP) + 0.3_DP)) THEN
        IF (commrank .EQ. 0) CALL write_test_status(failed, "Check cgp_coord_multi_read_data_f values", &
             "ERR: values are incorrect")
        CALL cgp_error_exit_f()
     ENDIF
  ENDDO
  IF (commrank .EQ. 0) CALL write_test_status(passed, "Check cgp_coord_multi_read_data_f values")

  DEALLOCATE(Coor_x, Coor_y, Coor_z)

  ! ======================================
  ! == (B) READ THE CONNECTIVITY TABLE  ==
  ! ======================================

  count = nijk(2)/commsize
  ALLOCATE(elements(1:count*NodePerElem), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of elements"
     CALL cgp_error_exit_f()
  ENDIF

  emin = count*commrank+1
  emax = count*(commrank+1)

  CALL cgp_elements_read_data_f(fn, B, Z, S, emin, emax, elements, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_elements_read_data_f", "( Reading elements)")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_elements_read_data_f")
  ENDIF

  DO k = 1, count
     IF(.NOT.check_eq(elements(k), commrank*count*NodePerElem + k)) THEN
        IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_elements_read_data_f", &
             "ERR: values are incorrect")
        CALL cgp_error_exit_f()
     ENDIF
  ENDDO

  DEALLOCATE(elements)

  ! ======================================
  ! == (C) READ THE FIELD DATA          ==
  ! ======================================
  count = nijk(1)/commsize

  ALLOCATE(Data_Fx(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Data_Fx"
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fy(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Data_Fy"
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Data_Fz(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Data_Fz"
     CALL cgp_error_exit_f()
  ENDIF

  Fvec(1:3) = (/Fx,Fy,Fz/)

  buf3(1) = C_LOC(Data_Fx)
  buf3(2) = C_LOC(Data_Fy)
  buf3(3) = C_LOC(Data_Fz)

  CALL cgp_field_multi_read_data_f(fn,B,Z,S,Fvec,vmin,vmax,3,buf3,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_field_multi_read_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_field_multi_read_data_f")
  ENDIF

  ! Check if read the data back correctly
  DO k = 1, count
     IF(.NOT.check_eq(Data_Fx(k), REAL(commrank*count + k, KIND=dp) + 0.01_dp).OR. &
          .NOT.check_eq(Data_Fy(k), REAL(commrank*count + k, KIND=dp) + 0.02_dp).OR. &
          .NOT.check_eq(Data_Fz(k), REAL(commrank*count + k, KIND=dp) + 0.03_dp)) THEN
        IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_field_multi_read_data_f", &
                "ERR: values are incorrect")
        CALL cgp_error_exit_f()
     ENDIF
  ENDDO
  IF (commrank .EQ. 0) CALL write_test_status(passed, "Check cgp_field_multi_read_data_f values")
  DEALLOCATE(Data_Fx,Data_Fy,Data_Fz)

  ! ======================================
  ! == (D) READ THE ARRAY DATA          ==
  ! ======================================

  count = nijk(1)/commsize

  ALLOCATE(Array_r(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Array_r"
     CALL cgp_error_exit_f()
  ENDIF
  ALLOCATE(Array_i(1:count), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, "*FAILED* allocation of Array_i"
     CALL cgp_error_exit_f()
  ENDIF

  vmin = count*commrank+1
  vmax = count*(commrank+1)

  CALL cg_goto_f(fn, B, err, "Zone_t",Z,"UserDefinedData_t",1,"end")
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cg_goto_f","cg_goto (User Defined Data)")
     CALL cgp_error_exit_f()
  ENDIF

  Avec = (/Ai,Ar/)

  buf2(1) = C_LOC(Array_i)
  buf2(2) = C_LOC(Array_r)

  CALL cgp_array_multi_read_data_f(fn,Avec,vmin,vmax,2,buf2,err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "Test cgp_array_multi_read_data_f","cgp_array_multi_read_data_f")
     CALL cgp_error_exit_f()
  ELSE
     IF (commrank .EQ. 0) CALL write_test_status(passed, "Test cgp_array_multi_read_data_f")
  ENDIF

  ! Check if read the data back correctly
  DO k = 1, count
     IF(.NOT.check_eq(Array_r(k), REAL(commrank*count + k, KIND=dp) + 0.001_dp).OR. &
          .NOT.check_eq(Array_i(k), commrank*count*NodePerElem + k)) THEN
        IF (commrank .EQ. 0) CALL write_test_status(failed, "Check cgp_array_multi_read_data_f",&
                "ERR: values are incorrect")
        CALL cgp_error_exit_f()
     ENDIF
  ENDDO
  IF (commrank .EQ. 0) CALL write_test_status(passed, "Check cgp_array_multi_read_data_f values")

  DEALLOCATE(Array_r, Array_i)

  CALL cgp_close_f(fn, err)
  IF(err.NE.CG_OK)THEN
     IF (commrank .EQ. 0) CALL write_test_status(failed, "cgp_close")
     CALL cgp_error_exit_f()
  ENDIF

END SUBROUTINE multisets

END MODULE ftests

PROGRAM pcgns_ftest

  USE ftests
  IMPLICIT NONE

  CALL MPI_INIT(mpi_err)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,commsize,mpi_err)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,commrank,mpi_err)

  IF(commrank.eq.0) CALL write_test_header("General Parallel CGNS Testing")
  CALL general_ptests()

  CALL MPI_Barrier(MPI_COMM_WORLD, mpi_err)
  IF(commrank.EQ.0) CALL write_test_header("Multi-sets API Testing")
  CALL multisets()

  CALL MPI_FINALIZE(mpi_err)

END PROGRAM pcgns_ftest

