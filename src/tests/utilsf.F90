MODULE testing_utils

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
     MODULE PROCEDURE c_float_eq, c_double_eq, c_long_eq, c_long_long_eq, character_eq
  END INTERFACE

  PRIVATE c_float_eq, c_double_eq, c_long_eq, c_long_long_eq, character_eq


CONTAINS
  !
  ! Formatting routines for presenting testing results
  !
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
!
! Routines for comparing values
!
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
