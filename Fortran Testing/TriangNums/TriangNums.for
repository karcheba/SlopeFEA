      PROGRAM PrintTriangNums

	    IMPLICIT NONE

	    INTEGER :: n

	    PRINT*, "Enter a number, n, to see"
	    PRINT*, "the first n triangular numbers:"
	    READ*, n

	    PRINT*

	    PRINT*, TriangNums(n)


	CONTAINS

      FUNCTION TriangNums (n)

          IMPLICIT NONE

          INTEGER, INTENT(IN) :: n
          INTEGER :: TriangNums(n)
          INTEGER :: i

          TriangNums(1) = 1
          DO i = 2,n
              TriangNums(i) = TriangNums(i-1) + i
          END DO

          END FUNCTION TriangNums 

	END PROGRAM PrintTriangNums