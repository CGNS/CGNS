PROGRAM test_mixed_par_ser

  USE cgns
  use mpi
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif

  INTEGER :: ierr, base, i, cg, iCoor
  INTEGER(cgsize_t) :: sizes(9)
  INTEGER :: commsize, commrank
  CHARACTER(LEN=11) :: FNAME1 = "fname1.cgns"
  CHARACTER(LEN=11) :: FNAME2 = "fname2.cgns"
  CHARACTER(LEN=11) :: FNAME3 = "fname3.cgns"
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: Dxyz

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,commsize,ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,commrank,ierr)

  ! *******************************
  ! TEST S-P-S, all different files
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST S-P-S, all different files"
  IF(commrank.EQ.0) CALL test_serial(FNAME1)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  CALL test_parallel(FNAME2)
  IF(commrank.EQ.0) CALL test_serial(FNAME3)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! *******************************
  ! TEST S-P-S, all same files
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST S-P-S, all same files"
  IF(commrank.EQ.0) CALL test_serial(FNAME1)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  CALL test_parallel(FNAME1)
  IF(commrank.EQ.0) CALL test_serial(FNAME1)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! *******************************
  ! TEST P-S-P, all different files
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST P-S-P, all different files"
  CALL test_parallel(FNAME1)
  IF(commrank.EQ.0) CALL test_serial(FNAME2)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  CALL test_parallel(FNAME3)
  
  ! *******************************
  ! TEST P-S-P, all same files
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST P-S-P, all same files"
  CALL test_parallel(FNAME1)
  IF(commrank.EQ.0) CALL test_serial(FNAME1)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  CALL test_parallel(FNAME1)
  CALL mpi_finalize(ierr)

CONTAINS

  SUBROUTINE test_serial(fname)

    IMPLICIT NONE
    CHARACTER*(*) :: fname
    INTEGER(cgsize_t) ii,kk,jj, pos

    WRITE(*,"(3X,A)", ADVANCE="NO") "SERIAL CREATE....."

    ! Make a serial file:
    CALL cg_open_f(fname, CG_MODE_WRITE, cg, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f()

    CALL cg_base_write_f(cg, "Base#1", 3, 3, base, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    sizes = 0
    sizes(1) = 10
    sizes(2) = 10
    sizes(3) = 10
    sizes(4) = sizes(1) - 1
    sizes(5) = sizes(2) - 1
    sizes(6) = sizes(3) - 1

    ALLOCATE(Dxyz(1:sizes(1)*sizes(2)*sizes(3)) )
    DO kk=1, sizes(3)
       DO jj=1, sizes(2)
          DO ii=1, sizes(1)
             pos = ii + (jj-1)*sizes(1) + (kk-1)*sizes(1)*sizes(2)
             ! * make up some dummy coordinates just for the test:
             Dxyz(pos) = i
          ENDDO
       ENDDO
    ENDDO

    CALL cg_zone_write_f(cg, base, "zone1", sizes, CGNS_ENUMV(Structured), i, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cg_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateX", Dxyz, iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cg_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateY", Dxyz, iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    
    CALL cg_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateZ", Dxyz, iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cg_close_f(cg, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    DEALLOCATE(Dxyz)

    WRITE(*,"(A)") "PASS"

  END SUBROUTINE test_serial

  SUBROUTINE test_parallel(fName)

    IMPLICIT NONE
    CHARACTER*(*) :: fname

    IF( commrank.EQ.0) WRITE(*,"(3X,A)", ADVANCE="NO") "PARALLEL CREATE..."

    CALL cgp_mpi_comm_f(MPI_COMM_WORLD, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cgp_pio_mode_f(CGP_COLLECTIVE, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cgp_open_f(fName, CG_MODE_WRITE, cg, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cg_base_write_f(cg, "Base#1", 3, 3, base, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f
    
    sizes = 0
    sizes(1) = 10
    sizes(2) = 10
    sizes(3) = 10
    sizes(4) = sizes(1) - 1
    sizes(5) = sizes(2) - 1
    sizes(6) = sizes(3) - 1
    CALL cg_zone_write_f(cg, base, "zone1", sizes, CGNS_ENUMV(Structured), i, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cgp_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateX", iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cgp_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateY", iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f
    
    CALL cgp_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateZ", iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f
    
    CALL cgp_close_f(cg, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f
    
    IF( commrank.EQ.0) WRITE(*,"(A)") "PASS"
    
  END SUBROUTINE test_parallel
  
END PROGRAM test_mixed_par_ser
