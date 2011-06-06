      PROGRAM ArrayIntrinsics
      
        IMPLICIT NONE
        
        INTEGER :: i, m, n
        INTEGER :: A(2,3)
        REAL :: X(5)
        
        A = RESHAPE( (/-4,6,5,-7,9,8/), (/2,3/) )
        X = (/ 1.5, -1.9, 1.7, -1.2, 0.3 /)
        
        m = UBOUND(A,1)
        n = UBOUND(A,2)
        
        PRINT*, "A"
        DO i = 1,m
            PRINT*, A(i,:)
        END DO
        PRINT*
        
        PRINT*, "SUM(PRODUCT(A,1)) = ", SUM(PRODUCT(A,1))
        PRINT*, "PRODUCT(SUM(A,2)) = ", PRODUCT(SUM(A,2))
        PRINT*, "MAXVAL(SUM(ABS(A),1)) = ", MAXVAL(SUM(ABS(A),1))
        PRINT*
        
        PRINT*, "X"
        PRINT*, X
        PRINT*
        
        PRINT*, "SUM(X**2) = ", SUM(X**2)
        PRINT*, "Mean of X = ", SUM(X) / SIZE(X)
        PRINT*, "SUM(X, MASK = X .GT. 0) = ", SUM(X, MASK = X .GT. 0)
        PRINT*, "MAXVAL(ABS(X)) = ", MAXVAL(ABS(X))
      
      END PROGRAM ArrayIntrinsics