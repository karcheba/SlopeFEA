      SUBROUTINE DLLtest (fname)

      CHARACTER*(*), INTENT(IN) :: fname

	OPEN(UNIT=2, FILE=fname(1:LEN(fname)-1)//".out")

	WRITE(2,*), "TESTING SUCCESSFUL!"

	CLOSE(UNIT=2)

	END SUBROUTINE DLLTest