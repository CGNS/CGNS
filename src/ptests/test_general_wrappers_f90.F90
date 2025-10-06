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
! Tests Fortran wrappers cgp_*_general_write_data and cgp_*_general_read_data
!

PROGRAM test_general_wrappers
  USE mpi
  USE ISO_C_BINDING
  USE CGNS
  USE testing_utils
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
  filename = 'test_general_wrappers.cgns'
  CALL cgp_open_f(filename, CG_MODE_WRITE, fn, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  ! Create base and zone
  CALL cg_base_write_f(fn, 'Base', 3, 3, B, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  sizes(1) = total_size  ! Vertex size
  sizes(2) = total_size - 1  ! Cell size
  sizes(3) = 0  ! Boundary vertex size

  CALL cg_zone_write_f(fn, B, 'Zone', sizes, CGNS_ENUMV(Unstructured), Z, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  !---------------------------------------------------------------------------
  ! Test 1: cgp_coord_general_write_data_f with scalar parameters
  !---------------------------------------------------------------------------
  IF (commrank .EQ. 0) PRINT *, 'Testing cgp_coord_general_write_data_f (scalar)...'

  CALL cgp_coord_write_f(fn, B, Z, CGNS_ENUMV(RealDouble), 'CoordinateX', C, ierr)
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
  m_type = CGNS_ENUMV(RealDouble)

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

  CALL cgp_coord_write_f(fn, B, Z, CGNS_ENUMV(RealDouble), 'CoordinateY', C, ierr)
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

  CALL cg_sol_write_f(fn, B, Z, 'Solution', CGNS_ENUMV(Vertex), S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  CALL cgp_field_write_f(fn, B, Z, S, CGNS_ENUMV(RealDouble), 'Density', F, ierr)
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

  CALL cgp_array_write_f('TestArray', CGNS_ENUMV(RealDouble), 1, sizes(1:1), A, ierr)
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
     IF (.NOT. check_eq(read_data(i), coords_data(i))) THEN
        PRINT *, 'FAILED: Coord read verification at index', i, &
                 'expected', coords_data(i), 'got', read_data(i)
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
     IF (.NOT. check_eq(read_data(i), field_data(i))) THEN
        PRINT *, 'FAILED: Field read verification at index', i, &
                 'expected', field_data(i), 'got', read_data(i)
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
     IF (.NOT. check_eq(read_data(i), array_data(i))) THEN
        PRINT *, 'FAILED: Array read verification at index', i, &
                 'expected', array_data(i), 'got', read_data(i)
        test_passed = 0
        EXIT
     END IF
  END DO

  CALL cgp_close_f(fn, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f()

  ! Report results
  IF (test_passed .EQ. 1) THEN
     IF (commrank .EQ. 0) THEN
        PRINT *
        PRINT *, 'SUCCESS: All Fortran general wrapper tests passed!'
     END IF
  ELSE
     PRINT *, 'FAILED: One or more tests failed on rank', commrank
     CALL MPI_Abort(MPI_COMM_WORLD, 1, ierr)
  END IF

  ! Cleanup
  DEALLOCATE(coords_data)
  DEALLOCATE(field_data)
  DEALLOCATE(array_data)
  DEALLOCATE(read_data)

  CALL MPI_Finalize(ierr)

END PROGRAM test_general_wrappers
