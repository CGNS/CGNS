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
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! *******************************
  ! TEST P-P: Multiple consecutive parallel opens
  ! Tests that multiple parallel files work correctly
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST P-P, consecutive parallel"
  CALL test_parallel(FNAME1)
  CALL test_parallel(FNAME2)
  CALL test_parallel(FNAME3)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! *******************************
  ! TEST S-S: Multiple consecutive serial opens
  ! Tests that serial mode doesn't get corrupted
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST S-S, consecutive serial"
  IF(commrank.EQ.0) CALL test_serial(FNAME1)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  IF(commrank.EQ.0) CALL test_serial(FNAME2)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  IF(commrank.EQ.0) CALL test_serial(FNAME3)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! *******************************
  ! TEST Issue #836: Multiple parallel files open simultaneously
  ! This is the core scenario that PR #906 fixed.
  ! Open file1 parallel, open file2 parallel (while file1 still open),
  ! close file1, then write to file2 (should still work in parallel mode)
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST Issue #836, simultaneous parallel files"
  CALL test_multiple_parallel_open()
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! *******************************
  ! TEST: Parallel then immediate serial (no barrier)
  ! Tests that mode handling works without synchronization delays
  ! *******************************
  IF(commrank.EQ.0) WRITE(*,"(A)") "TEST P-S immediate (no barrier)"
  CALL test_parallel(FNAME1)
  ! No barrier here - immediate serial open
  IF(commrank.EQ.0) CALL test_serial(FNAME2)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  CALL mpi_finalize(ierr)

CONTAINS

  SUBROUTINE test_serial(fname)

    IMPLICIT NONE
    CHARACTER*(*) :: fname
    INTEGER(cgsize_t) ii,kk,jj, pos

    WRITE(*,"(3X,A)", ADVANCE="NO") "SERIAL CREATE....."

    ! Make a serial file:
    CALL cg_open_f(fname, CG_MODE_WRITE, cg, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f()

    CALL cg_base_write_f(cg, "Base#1", 3, 3, base, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

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
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cg_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateX", Dxyz, iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cg_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateY", Dxyz, iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cg_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateZ", Dxyz, iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    CALL cg_close_f(cg, ierr)
    IF (ierr == CG_ERROR) CALL cg_error_exit_f

    DEALLOCATE(Dxyz)

    WRITE(*,"(A)") "PASS"

  END SUBROUTINE test_serial

  SUBROUTINE test_parallel(fName)

    IMPLICIT NONE
    CHARACTER*(*) :: fname

    IF( commrank.EQ.0) WRITE(*,"(3X,A)", ADVANCE="NO") "PARALLEL CREATE..."

    CALL cgp_mpi_comm_f(MPI_COMM_WORLD, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cgp_pio_mode_f(CGP_COLLECTIVE, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cgp_open_f(fName, CG_MODE_WRITE, cg, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cg_base_write_f(cg, "Base#1", 3, 3, base, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    
    sizes = 0
    sizes(1) = 10
    sizes(2) = 10
    sizes(3) = 10
    sizes(4) = sizes(1) - 1
    sizes(5) = sizes(2) - 1
    sizes(6) = sizes(3) - 1
    CALL cg_zone_write_f(cg, base, "zone1", sizes, CGNS_ENUMV(Structured), i, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cgp_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateX", iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cgp_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateY", iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cgp_coord_write_f(cg, base, i, CGNS_ENUMV(RealDouble), "CoordinateZ", iCoor, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    CALL cgp_close_f(cg, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    
    IF( commrank.EQ.0) WRITE(*,"(A)") "PASS"
    
  END SUBROUTINE test_parallel

  SUBROUTINE test_multiple_parallel_open()
    ! Tests issue #836: Multiple parallel files open at the same time
    ! This verifies that closing one parallel file doesn't corrupt the
    ! access mode for other open parallel files.

    IMPLICIT NONE
    INTEGER :: cg1, cg2, base1, base2, zone1, zone2
    INTEGER :: iCoor1, iCoor2

    IF(commrank.EQ.0) WRITE(*,"(3X,A)", ADVANCE="NO") "MULTI PARALLEL OPEN..."

    ! Setup MPI for parallel I/O
    CALL cgp_mpi_comm_f(MPI_COMM_WORLD, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    CALL cgp_pio_mode_f(CGP_COLLECTIVE, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    ! Open first parallel file
    CALL cgp_open_f("multi_file1.cgns", CG_MODE_WRITE, cg1, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    ! Open second parallel file (while first is still open)
    CALL cgp_open_f("multi_file2.cgns", CG_MODE_WRITE, cg2, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    ! Create content in file 1
    sizes = 0
    sizes(1) = 10
    sizes(2) = 10
    sizes(3) = 10
    sizes(4) = sizes(1) - 1
    sizes(5) = sizes(2) - 1
    sizes(6) = sizes(3) - 1

    CALL cg_base_write_f(cg1, "Base", 3, 3, base1, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    CALL cg_zone_write_f(cg1, base1, "Zone", sizes, CGNS_ENUMV(Structured), zone1, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    CALL cgp_coord_write_f(cg1, base1, zone1, CGNS_ENUMV(RealDouble), "CoordinateX", iCoor1, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    ! Close first file (this previously would reset mode to NATIVE, breaking file2)
    CALL cgp_close_f(cg1, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    ! Create content in file 2 - should still work in parallel mode
    ! This would fail before PR #906 fix
    CALL cg_base_write_f(cg2, "Base", 3, 3, base2, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    CALL cg_zone_write_f(cg2, base2, "Zone", sizes, CGNS_ENUMV(Structured), zone2, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f
    CALL cgp_coord_write_f(cg2, base2, zone2, CGNS_ENUMV(RealDouble), "CoordinateY", iCoor2, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    ! Close second file
    CALL cgp_close_f(cg2, ierr)
    IF (ierr == CG_ERROR) CALL cgp_error_exit_f

    IF(commrank.EQ.0) WRITE(*,"(A)") "PASS"

  END SUBROUTINE test_multiple_parallel_open

END PROGRAM test_mixed_par_ser
