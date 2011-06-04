      PROGRAM ArrayConstructor

        IMPLICIT NONE

        INTEGER :: i
        INTEGER, DIMENSION(3,2) :: A

        A = RESHAPE((/1, 4, 6, 12, 23, 52/), (/3,2/))

        PRINT*, "A"
        PRINT*
        DO i = 1, SIZE(A,1)
            PRINT*, A(i,:)
        END DO

      END PROGRAM ArrayConstructor