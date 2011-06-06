      PROGRAM Stats

	    IMPLICIT NONE

	    REAL, DIMENSION(10) :: data1(10)
          REAL, DIMENSION(14) :: data2(14)

	    data1 = (/5.0, 3.0, 17.0, -7.56, 78.1, 
     *                99.99, 0.8, 11.7, 33.8, 29.6/)

	    data2 = (/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0,
     *                8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0/)

	    PRINT*, "First data set:"
	    PRINT*, data1
	    PRINT*, "Standard deviation:",
     *            stdev(data1, mean(data1, SIZE(data1)), SIZE(data1))

	    PRINT*

	    PRINT*, "Second data set:"
          PRINT*, data2
          PRINT*, "Standard deviation:",
     *            stdev(data2, mean(data2, SIZE(data2)), SIZE(data2))


	    CONTAINS

	    REAL FUNCTION mean(x, n)

	        INTEGER, INTENT(IN) :: n
	        REAL, DIMENSION(n), INTENT(IN) :: x

	        mean = SUM(x)/n

	    END FUNCTION mean

	    REAL FUNCTION stdev(x, m, n)

	        INTEGER, INTENT(IN) :: n
	        REAL, INTENT(IN) :: m
	        REAL, DIMENSION(n), INTENT(IN) :: x

	        stdev = SQRT(SUM((x-m)**2) / n)

	    END FUNCTION stdev

	END PROGRAM Stats