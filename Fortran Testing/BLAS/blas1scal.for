      PROGRAM BLAS1SCAL

	IMPLICIT NONE

	INTEGER, PARAMETER :: sngKind = KIND(1.0)
	INTEGER, PARAMETER :: dblKind = KIND(1.0D0)
	REAL(sngKind) :: xs(5)
	REAL(dblKind) :: xd(5)

	xs = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)
	xd = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)

	PRINT*, xs
	PRINT*, xd
	PRINT*

	CALL SSCAL(5, 2.0, xs, 1)
	CALL DSCAL(5, 3.0D0, xd, 1)

	PRINT*, xs
	PRINT*, xd

	END PROGRAM BLAS1SCAL