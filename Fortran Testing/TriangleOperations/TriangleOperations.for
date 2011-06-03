      MODULE TriangleOperations

	    IMPLICIT NONE

	    CONTAINS

	    REAL FUNCTION Area(x,y,z)

	        REAL, INTENT(in) :: x, y, z
	        REAL :: theta, height

	        theta = ACOS((x**2 + y**2 - z**2) / (2.0*x*y))
	        height = x*SIN(theta)
	        Area = 0.5*y*height

	    END FUNCTION Area

	END MODULE TriangleOperations

C --------------------------------------------------------------------
      PROGRAM Triangle

	    USE TriangleOperations

	    IMPLICIT NONE

	    REAL :: a, b, c

	    PRINT*, 'Welcome, please enter the lengths of the 3 sides.'
	    READ*, a, b, c

	    PRINT*, 'Triangle''s area:  ', Area(a,b,c)

	END PROGRAM Triangle