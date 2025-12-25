! Test Fortran parameter API - direct BIND(C) interface
PROGRAM test_params_fortran
  USE CGNS
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE(C_PTR) :: params
  INTEGER :: ier, fn, B
  INTEGER :: cell_dim = 3, phys_dim = 3

  PRINT *, "=========================================="
  PRINT *, "Fortran Parameter API Test"
  PRINT *, "=========================================="
  PRINT *, ""
  PRINT *, "Testing direct C binding (TYPE(C_PTR))"

  ! Create parameter object
  ier = cg_params_create(params)
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_params_create"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_params_create with TYPE(C_PTR)"

  ! Set parameters using void* pattern with TRANSFER
  ier = cg_params_set(params, CG_PARAM_FILE_TYPE, TRANSFER(CG_FILE_HDF5, C_NULL_PTR))
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_params_set FILE_TYPE"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_params_set FILE_TYPE =", CG_FILE_HDF5

  ier = cg_params_set(params, CG_PARAM_MIN_VERSION, TRANSFER(CG_LIBVER_V40, C_NULL_PTR))
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_params_set MIN_VERSION"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_params_set MIN_VERSION =", CG_LIBVER_V40

  ier = cg_params_set(params, CG_PARAM_COMPRESS, TRANSFER(6, C_NULL_PTR))
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_params_set COMPRESS"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_params_set COMPRESS = 6"

  ! Open file with parameters (polymorphic interface)
  CALL cg_open("test_fortran_params.cgns", CG_MODE_WRITE, params, fn, ier)
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_open with params"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_open with TYPE(C_PTR) params"

  ! Create a base
  CALL cg_base_write_f(fn, "Base", cell_dim, phys_dim, B, ier)
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_base_write"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_base_write"

  ! Close file
  CALL cg_close_f(fn, ier)
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_close"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_close"

  ! Destroy params
  ier = cg_params_destroy(params)
  IF (ier /= CG_OK) THEN
     PRINT *, "FAILED: cg_params_destroy"
     STOP 1
  END IF
  PRINT *, "  PASSED: cg_params_destroy"

  PRINT *, ""
  PRINT *, "=========================================="
  PRINT *, "All Fortran parameter API tests passed!"
  PRINT *, "=========================================="

END PROGRAM test_params_fortran
