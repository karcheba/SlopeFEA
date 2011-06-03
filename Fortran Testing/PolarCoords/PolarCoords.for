      PROGRAM PolarCoords

	    IMPLICIT NONE

	    REAL, PARAMETER :: PI = 3.14159
	    REAL :: r, theta
	    REAL :: x, y

	    PRINT*, "Enter the length of the vector"
	    PRINT*, "and the angle (in degrees):"
	    READ*, r, theta
	    PRINT*

	    theta = theta * (PI/180.0)
	    x = r * COS(theta)
	    y = r * SIN(theta)

	    PRINT*, "The Cartesian coordinates are:"
	    PRINT*, x, y

	END PROGRAM PolarCoords