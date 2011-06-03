      PROGRAM Greeting
      ! My first Fortran 90 program!
      ! Greetings!

	    IMPLICIT NONE

          CHARACTER(20) :: name

          PRINT*, "What is your name?"
          READ*, name
          PRINT*, "Hi there, ", name

      END PROGRAM Greeting