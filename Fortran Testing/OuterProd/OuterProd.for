      PROGRAM OuterProd

	    IMPLICIT NONE

	    INTEGER, PARAMETER :: MAX_INT = 10
	    INTEGER :: m, n, i
	    INTEGER, ALLOCATABLE, DIMENSION(:) :: A, B
	    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: C
	    REAL, ALLOCATABLE, DIMENSION(:) :: harvest

	    PRINT*, "This program will generate two vectors"
	    PRINT*, "of size m and n of random numbers and"
	    PRINT*, "take their outer product."
	    PRINT*
	    PRINT*, "Enter two integers, m and n:"
	    READ*, m, n
	    PRINT*

	    ALLOCATE(A(m))
	    ALLOCATE(B(n))
	    ALLOCATE(C(m,n))

	    ALLOCATE(harvest(m))
	    CALL RANDOM_NUMBER(harvest)
	    A = CEILING(harvest*MAX_INT)

	    DEALLOCATE(harvest)

	    ALLOCATE(harvest(n))
	    CALL RANDOM_NUMBER(harvest)
	    B = CEILING(harvest*MAX_INT)

	    DEALLOCATE(harvest)

	    C = Outer(A,B)

	    PRINT*, "A"
	    PRINT*, A
	    PRINT*
	    PRINT*, "B"
	    PRINT*, B
	    PRINT*
	    PRINT*, "C"
	    DO i = 1,SIZE(C,1)
	        PRINT*, C(i,:)
	    END DO


	CONTAINS

	FUNCTION Outer (X, Y)

	    INTEGER, DIMENSION(:), INTENT(IN) :: X, Y
	    INTEGER :: Outer(SIZE(X), SIZE(Y))
	    INTEGER :: i

	    DO i = 1,SIZE(X)
	        Outer(i,:) = X(i)*B(:)
	    END DO

	END FUNCTION Outer

	END PROGRAM OuterProd