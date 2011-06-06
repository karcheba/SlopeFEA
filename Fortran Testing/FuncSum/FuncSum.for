      PROGRAM FuncSum

	    IMPLICIT NONE

	    REAL :: a, b

	    PRINT*, "This program computes the sum of two numbers."
	    PRINT*, "Enter the numbers:"
	    READ*, a, b
	    PRINT*

	    PRINT*, "The sum is:", sum2(a,b)


	    CONTAINS

	    REAL FUNCTION sum2(x, y)  	        
	        REAL, INTENT(IN) :: x, y
	        sum2 = x+y
	    END FUNCTION sum2

	END PROGRAM FuncSum