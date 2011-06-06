      PROGRAM ConsecSum
      
        IMPLICIT NONE
        
        INTEGER :: i, iMax
        REAL :: A(20), currSum, maxSum
        
        A = (/ 6.3, 7.6, 9.2, 3.4, 5.6, 7.23, 9.76, 6.83, 5.45, 4.56,
     *         4.86, 5.8, 6.4, 7.43, 7.87, 8.6, 9.25, 8.9, 8.4, 7.23 /)
     
        PRINT*, "A"
        PRINT*, A
        PRINT*
        
        currSum = SUM(A(1:5))
        maxSum = currSum
        iMax = 1
        
        DO i = 2, SIZE(A)-4
            
            currSum = SUM(A(i:i+4))
            
            IF (currSum .GT. maxSum) THEN
                maxSum = currSum
                iMax = i
            END IF
            
        END DO
        
        PRINT*, "The maximum sum of 5 consecutive numbers is:"
        PRINT*, maxSum
        PRINT*
        PRINT*, "The subset is:"
        PRINT*, A(iMax:iMax+4)
      
      END PROGRAM ConsecSum