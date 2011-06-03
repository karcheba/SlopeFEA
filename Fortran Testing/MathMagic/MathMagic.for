      PROGRAM MathMagic

	    IMPLICIT NONE

	    INTEGER :: i, currNum
          INTEGER, PARAMETER :: MAX_NUMS = 30     
	    INTEGER, DIMENSION(MAX_NUMS) :: magicNums

	    PRINT*, "Enter a list of numbers (one per line)"
          PRINT*, "and watch the magic:"

	    currNum = 1
	    i = 1
	    DO WHILE ((currNum .NE. 0) .AND. (i .LE. MAX_NUMS))
	        READ*, currNum
	        magicNums(i) = currNum
	        i = i+1
	    END DO

	    magic:  DO i = 1, SIZE(magicNums)

	            currNum = magicNums(i)

	            PRINT*, currNum

	            DO

	                IF (currNum .EQ. 13) THEN
	                    PRINT*, "This is very unlucky, goodbye ..."
	                    EXIT magic
	                ELSE IF (currNum .EQ. 1) THEN
                          EXIT
	                ELSE IF (currNum .EQ. 0) THEN
	                    EXIT magic
	                ELSE IF (currNum .LT. 0) THEN
	                    PRINT*, "Invalid number, skipping"
	                    EXIT
	                END IF 	                 

	                IF(MOD(currNum,2) .EQ. 0) THEN
                          currNum = currNum / 2
	                ELSE 
                          currNum = currNum*3 + 1
	                END IF

	                PRINT*, currNum

	            END DO

	            PRINT*

        END DO magic
      
      END PROGRAM MathMagic