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
!
! Test Fortran wrappers for cgp_*_general_write_data and cgp_*_general_read_data
! This test focuses on wrapper functionality, not C API correctness
!

PROGRAM test_general_wrappers_f90
  USE mpi
  USE ISO_C_BINDING
  USE CGNS
  IMPLICIT NONE

#include "cgnstypes_f03.h"

  INTEGER :: ierr, commsize, commrank
  INTEGER :: fn, B, Z, S, F, C, A
  INTEGER(cgsize_t) :: sizes(3)
  INTEGER(cgsize_t) :: start_pos, end_pos
  INTEGER(cgsize_t) :: npp, total_size

  ! Memory arrays with different layouts
  REAL(C_DOUBLE), ALLOCATABLE, TARGET :: coords_data(:)
  REAL(C_DOUBLE), ALLOCATABLE, TARGET :: field_data(:)
  REAL(C_DOUBLE), ALLOCATABLE, TARGET :: array_data(:)
  REAL(C_DOUBLE), ALLOCATABLE, TARGET :: read_data(:)

  ! General API parameters
  INTEGER(cgsize_t) :: f_rmin(1), f_rmax(1)  ! File space ranges
  INTEGER(cgsize_t) :: m_rmin(1), m_rmax(1)  ! Memory space ranges
  INTEGER(cgsize_t) :: m_dimvals(1)          ! Memory dimensions
  INTEGER(cgsize_t) :: m_numdim
  INTEGER(cgenum_t) :: m_type

  TYPE(C_PTR) :: data_ptr
  INTEGER :: i, test_passed
  REAL(C_DOUBLE) :: expected_val, read_val
  CHARACTER(LEN=32) :: filename

  CALL MPI_Init(ierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD, commsize, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, commrank, ierr)

  ! Test configuration
  total_size = 100
  npp = total_size / commsize
  start_pos = commrank * npp + 1
  end_pos = start_pos + npp - 1

  ! Allocate memory arrays
  ALLOCATE(coords_data(npp))
  ALLOCATE(field_data(npp))
  ALLOCATE(array_data(npp))
  ALLOCATE(read_data(npp))

  ! Initialize data with unique values per rank
  DO i = 1, INT(npp)
     coords_data(i) = REAL(start_pos + i - 1, C_DOUBLE)
     field_data(i) = REAL(start_pos + i - 1, C_DOUBLE) * 10.0_C_DOUBLE
     array_data(i) = REAL(start_pos + i - 1, C_DOUBLE) * 100.0_C_DOUBLE
  END DO

  ! Setup CGNS file
  filename = 'test_general_wrappers_f90.cgns'
  CALL cgp_open_f(filename, CG_MODE_WRITE, fn, ierr)
  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_open_f'
     CALL cgp_error_exit_f()
  END IF

  ! Create base and zone
  sizes(1) = total_size
  sizes(2) = total_size - 1
  sizes(3) = 0

  CALL cg_base_write_f(fn, 'Base', 3, 3, B, ierr)
  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cg_base_write_f'
     CALL cgp_error_exit_f()
  END IF

  CALL cg_zone_write_f(fn, B, 'Zone', sizes, CG_Unstructured, Z, ierr)
  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cg_zone_write_f'
     CALL cgp_error_exit_f()
  END IF

  !---------------------------------------------------------------------------
  ! Test 1: cgp_coord_general_write_data_f with scalar parameters
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_coord_general_write_data_f (scalar)...'

  CALL cgp_coord_write_f(fn, B, Z, CG_RealDouble, 'CoordinateX', C, ierr)
  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_coord_write_f'
     CALL cgp_error_exit_f()
  END IF

  ! Setup general parameters (scalar version)
  f_rmin(1) = start_pos
  f_rmax(1) = end_pos
  m_rmin(1) = 1
  m_rmax(1) = npp
  m_dimvals(1) = npp
  m_numdim = 1
  m_type = CG_RealDouble

  data_ptr = C_LOC(coords_data)

  CALL cgp_coord_general_write_data_f(fn, B, Z, C, &
       f_rmin(1), f_rmax(1), &
       m_type, m_numdim, m_dimvals(1), m_rmin(1), m_rmax(1), &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_coord_general_write_data_f (scalar)'
     CALL cgp_error_exit_f()
  END IF

  !---------------------------------------------------------------------------
  ! Test 2: cgp_coord_general_write_data_f with array parameters
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_coord_general_write_data_f (array)...'

  CALL cgp_coord_write_f(fn, B, Z, CG_RealDouble, 'CoordinateY', C, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  data_ptr = C_LOC(coords_data)

  CALL cgp_coord_general_write_data_f(fn, B, Z, C, &
       f_rmin, f_rmax, &
       m_type, m_numdim, m_dimvals, m_rmin, m_rmax, &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_coord_general_write_data_f (array)'
     CALL cgp_error_exit_f()
  END IF

  !---------------------------------------------------------------------------
  ! Test 3: cgp_field_general_write_data_f
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_field_general_write_data_f...'

  CALL cg_sol_write_f(fn, B, Z, 'Solution', CG_Vertex, S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  CALL cgp_field_write_f(fn, B, Z, S, CG_RealDouble, 'Density', F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  data_ptr = C_LOC(field_data)

  CALL cgp_field_general_write_data_f(fn, B, Z, S, F, &
       f_rmin, f_rmax, &
       m_type, m_numdim, m_dimvals, m_rmin, m_rmax, &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_field_general_write_data_f'
     CALL cgp_error_exit_f()
  END IF

  !---------------------------------------------------------------------------
  ! Test 4: cgp_array_general_write_data_f
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_array_general_write_data_f...'

  CALL cg_goto_f(fn, B, ierr, 'Zone_t', Z, 'end')
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  ! Create a user-defined node to hold the array
  CALL cg_user_data_write_f('UserData', ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  CALL cg_goto_f(fn, B, ierr, 'Zone_t', Z, 'UserDefinedData_t', 1, 'end')
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  CALL cgp_array_write_f('TestArray', CG_RealDouble, 1, sizes(1:1), A, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  data_ptr = C_LOC(array_data)

  CALL cgp_array_general_write_data_f(A, &
       f_rmin, f_rmax, &
       m_type, m_numdim, m_dimvals, m_rmin, m_rmax, &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_array_general_write_data_f'
     CALL cgp_error_exit_f()
  END IF

  CALL cgp_close_f(fn, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  ! Barrier before reading
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  !---------------------------------------------------------------------------
  ! Test 5: cgp_coord_general_read_data_f
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_coord_general_read_data_f...'

  CALL cgp_open_f(filename, CG_MODE_READ, fn, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  read_data = 0.0_C_DOUBLE
  data_ptr = C_LOC(read_data)

  CALL cgp_coord_general_read_data_f(fn, 1, 1, 1, &
       f_rmin, f_rmax, &
       m_type, m_numdim, m_dimvals, m_rmin, m_rmax, &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_coord_general_read_data_f'
     CALL cgp_error_exit_f()
  END IF

  ! Verify data
  test_passed = 1
  DO i = 1, INT(npp)
     expected_val = coords_data(i)
     read_val = read_data(i)
     IF (ABS(read_val - expected_val) .GT. 1.0E-10) THEN
        PRINT *, 'FAILED: Coord read verification at index', i, &
                 'expected', expected_val, 'got', read_val
        test_passed = 0
        EXIT
     END IF
  END DO

  !---------------------------------------------------------------------------
  ! Test 6: cgp_field_general_read_data_f
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_field_general_read_data_f...'

  read_data = 0.0_C_DOUBLE
  data_ptr = C_LOC(read_data)

  CALL cgp_field_general_read_data_f(fn, 1, 1, 1, 1, &
       f_rmin, f_rmax, &
       m_type, m_numdim, m_dimvals, m_rmin, m_rmax, &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_field_general_read_data_f'
     CALL cgp_error_exit_f()
  END IF

  ! Verify data
  DO i = 1, INT(npp)
     expected_val = field_data(i)
     read_val = read_data(i)
     IF (ABS(read_val - expected_val) .GT. 1.0E-10) THEN
        PRINT *, 'FAILED: Field read verification at index', i, &
                 'expected', expected_val, 'got', read_val
        test_passed = 0
        EXIT
     END IF
  END DO

  !---------------------------------------------------------------------------
  ! Test 7: cgp_array_general_read_data_f
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_array_general_read_data_f...'

  CALL cg_goto_f(fn, 1, ierr, 'Zone_t', 1, 'UserDefinedData_t', 1, 'end')
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  read_data = 0.0_C_DOUBLE
  data_ptr = C_LOC(read_data)

  CALL cgp_array_general_read_data_f(1, &
       f_rmin, f_rmax, &
       m_type, m_numdim, m_dimvals, m_rmin, m_rmax, &
       data_ptr, ierr)

  IF (ierr .NE. CG_OK) THEN
     PRINT *, 'FAILED: cgp_array_general_read_data_f'
     CALL cgp_error_exit_f()
  END IF

  ! Verify data
  DO i = 1, INT(npp)
     expected_val = array_data(i)
     read_val = read_data(i)
     IF (ABS(read_val - expected_val) .GT. 1.0E-10) THEN
        PRINT *, 'FAILED: Array read verification at index', i, &
                 'expected', expected_val, 'got', read_val
        test_passed = 0
        EXIT
     END IF
  END DO

  CALL cgp_close_f(fn, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  ! Gather results from all ranks
  CALL MPI_Allreduce(MPI_IN_PLACE, test_passed, 1, MPI_INTEGER, &
       MPI_MIN, MPI_COMM_WORLD, ierr)

  IF (commrank .EQ. 0) THEN
     IF (test_passed .EQ. 1) THEN
        PRINT *, ''
        PRINT *, 'SUCCESS: All Fortran general wrapper tests passed!'
     ELSE
        PRINT *, ''
        PRINT *, 'FAILED: Some tests failed'
     END IF
  END IF

  ! Cleanup
  DEALLOCATE(coords_data)
  DEALLOCATE(field_data)
  DEALLOCATE(array_data)
  DEALLOCATE(read_data)

  CALL MPI_Finalize(ierr)

  IF (test_passed .NE. 1) THEN
     STOP 1
  END IF

END PROGRAM test_general_wrappers_f90
