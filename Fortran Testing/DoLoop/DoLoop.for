	PROGRAM DoLoop
	! Prints 10 numbers to the console

	    IMPLICIT NONE

	    INTEGER :: i
	    REAL :: r

	    DO i = 1, 10
		    !PRINT*, i
		    CALL random_number(r)
		    PRINT*, r
	    END DO

	END PROGRAM DoLoop