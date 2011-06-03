	PROGRAM QuadraticSolver
C    **************************************************************	
C    * Returns the root(s) of a quadratic equation of the form:   *
C    *                                                            *
C    *        ax^2 + bx + c = 0                                   *
C    **************************************************************	
	
          IMPLICIT NONE

          REAL :: a, b, c, disc, Re, Im

          PRINT*, "Type in values for a, b, and c"
          READ*, a, b, c

          IF (a .NE. 0.0) THEN                ! calculate discriminant

              disc = b*b - 4.0*a*c

	        IF (disc .EQ. 0.0) THEN             ! one root
	         
                  PRINT*, "Root is"
                  PRINT*, -b / (2.0*a)

	        ELSE IF (disc .gt. 0.0) THEN        ! real roots
	         
                  PRINT*, "Roots are"
                  PRINT*, (-b + SQRT(disc)) / (2.0*a), " and"
	            PRINT*, (-b - SQRT(disc)) / (2.0*a)

	        ELSE                                ! complex roots
	         
                  Re = -b / (2.0*a)
	            Im = SQRT(-disc) / (2.0*a)
	            PRINT*, "Roots are"
                  PRINT*, Re, " + ", Im, "i and"
	            PRINT*, Re, " - ", Im, "i"

	        END IF

          ELSE        ! a == 0, so equation is not quadratic

	        PRINT*, "Not a quadratic equation..."
	
          END IF                     

	END PROGRAM QuadraticSolver