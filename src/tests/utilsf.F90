MODULE testing_utils

  USE ISO_C_BINDING
  IMPLICIT NONE

  !
  ! Contains functions to verify values
  !
  INTERFACE check_eq
     MODULE PROCEDURE c_float_eq, c_double_eq, c_long_eq, c_long_long_eq, character_eq
  END INTERFACE

  PRIVATE c_float_eq, c_double_eq, c_long_eq, c_long_long_eq, character_eq

CONTAINS

  LOGICAL FUNCTION c_float_eq(a,b,ulp)
    IMPLICIT NONE
    ! Check if two C_FLOAT reals are equivalent
    REAL*4, INTENT(IN):: a,b
    REAL*4  :: Rel
    INTEGER, OPTIONAL, INTENT(IN)  :: ulp
    Rel = 1.0
    IF ( PRESENT(ulp) ) Rel = REAL( ABS(ulp), KIND(1.0))
    c_float_eq = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )
  END FUNCTION c_float_eq

  LOGICAL FUNCTION c_double_eq(a,b,ulp)
    IMPLICIT NONE
    ! Check if two C_DOUBLE reals are equivalent
    REAL*8, INTENT(IN):: a,b
    REAL*8  :: Rel
    INTEGER, OPTIONAL, INTENT(IN)  :: ulp
    Rel = 1.D0
    IF ( PRESENT(ulp) ) Rel = REAL( ABS(ulp), KIND(1.D0))
    c_double_eq = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )
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

  LOGICAL FUNCTION character_eq(value,correct_value)
    IMPLICIT NONE
    CHARACTER*(*) :: value, correct_value
    character_eq = .FALSE.
    IF (TRIM(value) .EQ. TRIM(correct_value)) THEN
       character_eq = .TRUE.
    ENDIF
  END FUNCTION character_eq

END MODULE testing_utils
