      PROGRAM Vertical
      ! Vertical motion under gravity

	    IMPLICIT NONE
	     
	    REAL, PARAMETER :: g = 9.81		! gravitational acceleration (m/s**2)

	    REAL :: y   ! vertical position (m)
	    REAL :: t   ! time (s)
	    REAL :: v0  ! initial velocity (m/s)
	     
	    PRINT*, '  Time     Displacement'
	    PRINT*
	     
	    v0 = 60
	    t = 6
	    y = v0*t - 0.5*g*t**2

	    PRINT*, t, y
	
      END PROGRAM Vertical