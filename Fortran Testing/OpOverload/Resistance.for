      MODULE Resistance

	IMPLICIT NONE

	INTERFACE OPERATOR (.SERIES.)
	    MODULE PROCEDURE real_series
	END INTERFACE ! (.SERIES.)
	PRIVATE :: real_series

	INTERFACE OPERATOR (.PARALLEL.)
	    MODULE PROCEDURE real_parallel
	END INTERFACE ! (.PARALLEL.)
	PRIVATE :: real_parallel

	CONTAINS

	REAL FUNCTION real_series (l,r)
	    REAL, INTENT(IN) :: l,r
	    real_series = l+r
	END FUNCTION real_series

	REAL FUNCTION real_parallel (l,r)
	    REAL, INTENT(IN) :: l,r
	    real_parallel = 1.0 / (1.0/l + 1.0/r)
	END FUNCTION real_parallel

	END MODULE Resistance