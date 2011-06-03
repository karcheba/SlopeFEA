      PROGRAM RomanNumerals

	IMPLICIT NONE

	INTEGER :: num, i, nSym, j
	CHARACTER(30) :: rom

	PRINT*, "Enter a number and see it converted to roman numerals:"
	READ*, num
      PRINT*

	    i = 1

	    DO WHILE (num .GT. 0)

	        SELECT CASE (num)

	            CASE (1000:)
	                nSym = num/1000
	                rom(i:i+nSym-1) = REPEAT('m',nSym)
	                num = MOD(num,1000)
	                i = i+nSym
	            CASE (900:999)
	                rom(i:i+1) = 'cm'
	                num = num-900
	                i = i+2
	            CASE (500:899)
	                rom(i:i) = 'd'
	                num = num-500
	                i = i+1
	            CASE (400:499)
	                rom(i:i+1) = 'cd'
	                num = num-400
	                i = i+2
	            CASE (100:399)
	                nSym = num/100
	                rom(i:i+nSym-1) = REPEAT('c',nSym)
	                num = MOD(num,100)
	                i = i+nSym
	            CASE (90:99)
	                rom(i:i+1) = 'xc'
	                num = num-90
	                i = i+2
	            CASE (50:89)
	                rom(i:i) = 'l'
	                num = num-50
	                i = i+1
	            CASE (40:49)
	                rom(i:i+1) = 'xl'
	                num = num-40
	                i = i+2
	            CASE (10:39)
	                nSym = num/10
	                rom(i:i+nSym-1) = REPEAT('x',nSym)
	                num = MOD(num,10)
	                i = i+nSym
	            CASE (9)
	                rom(i:i+1) = 'ix'
	                num = num-9
	                i = i+2
	            CASE (5:8)
	                rom(i:i) = 'v'
	                num = num-5
	                i = i+1
	            CASE (4)
	                rom(i:i+1) = 'iv'
	                num = 0
	            CASE DEFAULT
	                rom(i:i+num-1) = REPEAT('i',num)
	                num = 0
	                i = i+num

	        END SELECT

	    END DO

	    PRINT*, rom

	END PROGRAM RomanNumerals