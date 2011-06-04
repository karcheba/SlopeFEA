      PROGRAM ArrayInquiry

        IMPLICIT NONE

        INTEGER, DIMENSION(-1:1,3,2) :: A

        PRINT*, "The number of elements in A is:", SIZE(A)
        PRINT*, "The shape of A is:", SHAPE(A)
        PRINT*, "The lower bound of dimension 2 of A is:", LBOUND(A,2)
        PRINT*, "The upper bound of dimension 3 of A is:", UBOUND(A,3)

      END PROGRAM ArrayInquiry