      PROGRAM KindParam

	IMPLICIT NONE

	DOUBLE PRECISION :: dblPrec

	dblPrec = 1.0

	PRINT*, "KIND(1.0) =", KIND(1.0)
	PRINT*, "KIND(1.0D0)=", KIND(1.0D0)
	PRINT*, "KIND(1.0E0)=", KIND(1.0E0)
	PRINT*, "KIND(dblPrec)=", KIND(dblPrec)

	END PROGRAM KindParam