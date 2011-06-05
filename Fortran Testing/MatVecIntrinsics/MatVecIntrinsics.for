      PROGRAM MatVecIntrinsics

        IMPLICIT NONE

        INTEGER :: A(2,4), B(4,2)

        A(1,:) = (/1,2,3,4/)
        A(2,:) = (/5,6,7,8/)
        B(:,1) = (/4,3,2,1/)
        B(:,2) = (/8,7,6,5/)

        PRINT*, "DOT_PRODUCT(A(1,:), A(2,:)) = ", 
     *           DOT_PRODUCT(A(1,:), A(2,:))

        PRINT*, "MATMUL(A,B) = ", MATMUL(A,B)

      END PROGRAM MatVecIntrinsics