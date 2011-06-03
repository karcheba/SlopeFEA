      PROGRAM FinalMark
      ! Final mark for a course based on class record and/or exams

	    IMPLICIT NONE

	    REAL ::	crm     ! class record mark
	    REAL :: exmAvg  ! average of two exam papers
	    REAL :: final   ! final mark
	    REAL :: p1      ! mark on first paper
	    REAL :: p2      ! mark on second paper
	    INTEGER	stu     ! student counter

	    OPEN(1, file = 'marks.dat')
	    PRINT*, 'CRM			Exam Avg			Final Mark'
	    PRINT*

	    DO stu = 1, 3

	        READ(1,*) crm, p1, p2
	        exmAvg = (p1+p2) / 2.0

	        IF (exmAvg .GT. crm) THEN
		        final = exmAvg
	        ELSE
		        final = (exmAvg+crm) / 2.0
	        ENDIF

	        IF (final .GE. 50) THEN
		        PRINT*, crm, exmAvg, final, 'PASS'
	        ELSE
		        PRINT*, crm, exmAvg, final, 'FAIL'
	        ENDIF

	    ENDDO

      END PROGRAM FinalMark