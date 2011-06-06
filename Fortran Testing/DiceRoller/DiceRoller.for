	PROGRAM DiceRoller

	    IMPLICIT NONE

	    INTEGER :: i

	    CALL InitRandomSeed()

	    DO i = 1,10
	        PRINT*, "Random number #", i, ":", ThrowDice()
	    END DO


	    CONTAINS

	    INTEGER FUNCTION ThrowDice ()

	        REAL :: r1, r2
	        INTEGER :: die1, die2
	         
	        CALL RANDOM_NUMBER(r1)
	        CALL RANDOM_NUMBER(r2)

	        die1 = CEILING(r1*6)
	        die2 = CEILING(r2*6)

	        ThrowDice = die1 + die2

	    END FUNCTION ThrowDice

          SUBROUTINE InitRandomSeed()

              INTEGER :: i, n, clock
              INTEGER, DIMENSION(:), ALLOCATABLE :: seed

              CALL RANDOM_SEED(SIZE = n)
              ALLOCATE(seed(n))

              CALL SYSTEM_CLOCK(COUNT = clock)

              seed = clock + 37 * (/ (i - 1, i = 1, n) /)
              CALL RANDOM_SEED(PUT = seed)

              DEALLOCATE(seed)

          END SUBROUTINE
      
	END PROGRAM DiceRoller