      PROGRAM CoordPointers

	    IMPLICIT NONE

	    REAL, TARGET :: coords(10)
	    REAL, POINTER :: xg(:), yg(:)

	    coords = (/ 0.0,0.0, 4.2,2.8, 3.0,5.6,
     *                    8.2,9.1, 12.6,14.1 /)

	    xg => coords(1::2)
	    yg => coords(2::2)

	    PRINT*, "x: ", xg
	    PRINT*, "y: ", yg

	END PROGRAM CoordPointers