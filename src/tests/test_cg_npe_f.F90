      PROGRAM test_cg_npe_f
!
!     Test cg_npe_f wrapper for binary compatibility
!     Verifies the fix for the cgint_f parameter issue
!
#include "cgnstypes_f03.h"
      USE CGNS
      IMPLICIT NONE

      INTEGER :: npe
      INTEGER :: ier

      ! Test 1: Call with TETRA_4 (should return 4)
      npe = -999
      CALL cg_npe_f(CGNS_ENUMV(TETRA_4), npe, ier)
      IF (ier .NE. CG_OK) THEN
        PRINT *, 'ERROR: cg_npe_f failed for TETRA_4'
        STOP 1
      END IF
      IF (npe .NE. 4) THEN
        PRINT *, 'ERROR: Expected 4, got', npe
        STOP 1
      END IF
      PRINT *, 'PASS: TETRA_4 returned', npe

      ! Test 2: Call with HEXA_8 (should return 8)
      npe = -999
      CALL cg_npe_f(CGNS_ENUMV(HEXA_8), npe, ier)
      IF (ier .NE. CG_OK) THEN
        PRINT *, 'ERROR: cg_npe_f failed for HEXA_8'
        STOP 1
      END IF
      IF (npe .NE. 8) THEN
        PRINT *, 'ERROR: Expected 8, got', npe
        STOP 1
      END IF
      PRINT *, 'PASS: HEXA_8 returned', npe

      ! Test 3: Call with TRI_3 (should return 3)
      npe = -999
      CALL cg_npe_f(CGNS_ENUMV(TRI_3), npe, ier)
      IF (ier .NE. CG_OK) THEN
        PRINT *, 'ERROR: cg_npe_f failed for TRI_3'
        STOP 1
      END IF
      IF (npe .NE. 3) THEN
        PRINT *, 'ERROR: Expected 3, got', npe
        STOP 1
      END IF
      PRINT *, 'PASS: TRI_3 returned', npe

      ! Test 4: Call with QUAD_4 (should return 4)
      npe = -999
      CALL cg_npe_f(CGNS_ENUMV(QUAD_4), npe, ier)
      IF (ier .NE. CG_OK) THEN
        PRINT *, 'ERROR: cg_npe_f failed for QUAD_4'
        STOP 1
      END IF
      IF (npe .NE. 4) THEN
        PRINT *, 'ERROR: Expected 4, got', npe
        STOP 1
      END IF
      PRINT *, 'PASS: QUAD_4 returned', npe

      PRINT *, ''
      PRINT *, 'All tests passed!'
      PRINT *, 'cg_npe_f wrapper is working correctly'

      END PROGRAM test_cg_npe_f
