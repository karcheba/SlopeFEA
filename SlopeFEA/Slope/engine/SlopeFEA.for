      SUBROUTINE slopefea (fname)

	CHARACTER*(*), INTENT(IN) :: fname

	OPEN(UNIT=2, FILE=fname(1:LEN(fname)-1)//".out")
	OPEN(UNIT=3, FILE=fname(1:LEN(fname)-1)//".mtl")
	OPEN(UNIT=4, FILE=fname(1:LEN(fname)-1)//".nod")
	OPEN(UNIT=5, FILE=fname(1:LEN(fname)-1)//".ele")
	OPEN(UNIT=6, FILE=fname(1:LEN(fname)-1)//".bel")
	


	END SUBROUTINE slopefea