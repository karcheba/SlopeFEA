      PROGRAM MaxMin
      
        IMPLICIT NONE
        
        INTEGER :: A(3,4), i
        
        A(:,1) = (/0,3,1/)
        A(:,2) = (/-5,4,5/)
        A(:,3) = (/8,-1,6/)
        A(:,4) = (/3,2,-4/)
        
        PRINT*, "MAX( (/2,7,3,5,9,1/), (/1,9,5,3,7,2/) ) = ",
     *           MAX( (/2,7,3,5,9,1/), (/1,9,5,3,7,2/) )
       
        PRINT*, "MIN( (/2,7,3,5,9,1/), (/1,9,5,3,7,2/) ) = ",
     *           MIN( (/2,7,3,5,9,1/), (/1,9,5,3,7,2/) )
       
        PRINT*, "MAXLOC( (/2,7,3,5,9,1/) ) = ",
     *           MAXLOC( (/2,7,3,5,9,1/) )
       
        PRINT*, "MAXVAL( (/2,7,3,5,9,1/) ) = ",
     *           MAXVAL( (/2,7,3,5,9,1/) )
       
        PRINT*, "MINLOC( (/2,7,3,5,9,1/) ) = ",
     *           MINLOC( (/2,7,3,5,9,1/) )
       
        PRINT*, "MINVAL( (/2,7,3,5,9,1/) ) = ",
     *           MINVAL( (/2,7,3,5,9,1/) )
        
        PRINT*
        PRINT*, "A"
        DO i = 1,SIZE(A,1)
            PRINT*, A(i,:)
        END DO
        PRINT*
        
        PRINT*, "MAXLOC(A, MASK = A .LT. 5) = ",
     *           MAXLOC(A, MASK = A .LT. 5)
       
        PRINT*, "MAXVAL(A, MASK = A .LT. 5) = ",
     *           MAXVAL(A, MASK = A .LT. 5)
       
        PRINT*, "MAXLOC(A, MASK = A .LT. 4) = ",
     *           MAXLOC(A, MASK = A .LT. 4)
       
        PRINT*, "MAXVAL(A, MASK = A .LT. 4) = ",
     *           MAXVAL(A, MASK = A .LT. 4)
      
      END PROGRAM MaxMin