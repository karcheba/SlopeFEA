      PROGRAM FileInput
      ! Reads some data from a file called 'data.txt'

	    IMPLICIT NONE
	    
          INTEGER :: a, b, c

          OPEN(1, file = 'data.txt')
	    READ(1,*) a, b, c
	    PRINT*, a, b, c

      END PROGRAM FileInput