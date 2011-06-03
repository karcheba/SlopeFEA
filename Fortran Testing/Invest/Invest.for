      PROGRAM Invest
      ! compound growth of an investment

	    IMPLICIT NONE

	    INTEGER :: period   ! period of investment
	    INTEGER :: year     ! counter for years
	    REAL :: bal         ! balance
	    REAL :: rate        ! annual interest rate

	    PRINT*, 'Initial balance:'
	    READ*, bal
	    PRINT*, 'Period of investment (years):'
	    READ*, period
	    PRINT*, 'Interest rate (per annum, as a decimal fraction):'
	    READ*, rate
	    PRINT*
	    PRINT*, 'Year		Balance'
	    PRINT*
	     
	    DO year = 1, period
	        bal = (1+rate)*bal
	        PRINT*, year, bal
	    ENDDO			

      END PROGRAM Invest