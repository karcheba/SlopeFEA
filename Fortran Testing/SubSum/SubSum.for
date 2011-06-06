      PROGRAM SubSum

	    IMPLICIT NONE

	    REAL :: a,b,c

	    PRINT*, "This program computes the sum of two numbers."
	    PRINT*, "Enter the numbers:"
	    READ*, a, b
	    PRINT*

	    CALL sum2(c,a,b)

	    PRINT*, "The sum is:"
	    PRINT*, c
	

	    CONTAINS

	    SUBROUTINE sum2(ans, n1, n2)

	        REAL, INTENT(IN) :: n1, n2
	        REAL, INTENT(OUT) :: ans

	        ans = n1+n2

	    END SUBROUTINE sum2

	END PROGRAM SubSum