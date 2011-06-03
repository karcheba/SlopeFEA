      PROGRAM CondenserEnergy
      ! This program computes the energy stored in a condenser
      ! given its capacitance, C, and potential difference, V

	    IMPLICIT NONE

	    REAL :: c, v
	    REAL :: energy

	    PRINT*, "This program computes the energy stored in a "
	    PRINT*, "condenser given its capacitance and potential"
	    PRINT*, "difference."

	    PRINT*, "Enter the capacitance:"
	    READ*, c

	    PRINT*, "Enter the potential difference:"
	    READ*, v

	    energy = 0.5 * c * v**2

	    PRINT*, "The energy stored is:", energy

      END PROGRAM CondenserEnergy