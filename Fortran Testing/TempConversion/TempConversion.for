	PROGRAM TempConversion

	    IMPLICIT NONE

	    REAL :: degF, degC, K

	    PRINT*, "Please type the temperature in F"
	    READ*, degF

	    degC = 5. * (degF - 32.) / 9.
	    PRINT*, "This is equal to", degC, "C"

	    K = degC + 273.15
	    PRINT*, "and", K, "K"

	END PROGRAM TempConversion