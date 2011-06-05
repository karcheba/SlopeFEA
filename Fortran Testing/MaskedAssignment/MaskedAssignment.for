      PROGRAM MaskedAssignment

        IMPLICIT NONE

        INTEGER :: i
        INTEGER, DIMENSION(4,4) :: A

        A = RESHAPE((/1,-2,3,-4,-5,6,-7,8,9,-10,11,-12,-13,14,-15,
     *                              16/), (/4,4/), ORDER=(/2,1/))
        
        PRINT*, "A"
        DO i = 1,SIZE(A,1)
            PRINT*, A(i,:)
        END DO
        PRINT*

        WHERE (A .LT. 0) A = -A

        PRINT*, "A (masked to +ve)"
        DO i = 1,SIZE(A,1)
            PRINT*, A(i,:)
        END DO
        PRINT*
        
      END PROGRAM MaskedAssignment