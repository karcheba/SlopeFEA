      PROGRAM Calculator
      ! Computes the sum, product, difference, and quotient of two numbers

	    IMPLICIT NONE

          REAL :: num1, num2
          REAL :: sum, prod, diff, quot

          PRINT*, "This program will calculate the sum, product,"
          PRINT*, " difference, and quotient of two numbers."
          
          PRINT*, "Enter the first number:"
          READ*, num1
          PRINT*, "Enter the second number:"
          READ*, num2

          sum = num1 + num2
          prod = num1 * num2
          diff = num1 - num2
          quot = num1 / num2

          PRINT*, "The sum is:", sum
          PRINT*, "The product is:", prod
          PRINT*, "The difference is:", diff
          PRINT*, "The quotient is:", quot

      END PROGRAM Calculator