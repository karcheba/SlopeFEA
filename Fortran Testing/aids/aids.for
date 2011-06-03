      PROGRAM aids
	! Calculates number of accumulated AIDS cases in USA
	     
	    IMPLICIT NONE
          
          INTEGER :: t    ! year
	    REAL :: a       ! number of cases

	    PRINT*, "Enter the year:"
	    READ*, t
	    a = 174.6 * (t - 1981.2)**3
	    PRINT*, "Accumulated AIDS cases in US by year", t, ":", a
	  
	END PROGRAM aids