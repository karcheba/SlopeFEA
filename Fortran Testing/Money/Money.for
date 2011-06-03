	PROGRAM MONEY
	! Calculates balance after interest compounded

	    IMPLICIT NONE

          REAL :: balance, interest, rate

          balance = 1000
          rate = 0.09
          interest = rate * balance
          balance = balance + interest

          PRINT*, "New balance: ", balance

	END PROGRAM MONEY