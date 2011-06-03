      PROGRAM NewtonSqrt
      ! Square rooting with Newton

	    IMPLICIT NONE

	    INTEGER :: i    ! iteration counter	
	    REAL :: a       ! number to be square rooted							
	    REAL :: x       ! approximate value of square root

	    PRINT*, "Enter number to be square rooted:"
	    READ*, a
	    PRINT*

	    x = 1	! intial guess

	    DO i = 1, 6
	        x = (x + a/x) / 2.0
	        PRINT*, x
	    END DO

	    PRINT*
	    PRINT*, "Fortran 90's value:", SQRT(a)

      END PROGRAM NewtonSqrt