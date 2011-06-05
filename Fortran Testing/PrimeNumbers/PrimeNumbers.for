      PROGRAM PrimeNumbers

        IMPLICIT NONE

        INTEGER :: i, n, ierr
        INTEGER, ALLOCATABLE, DIMENSION(:) :: Primes
        LOGICAL, ALLOCATABLE, DIMENSION(:) :: Mask

        PRINT*, "Enter an integer to get all primes up to"
        PRINT*, "and including that value."
        READ*, n

        ALLOCATE(Primes(n))
        IF (ALLOCATED(Primes)) Primes = (/(i, i=1,n)/)

        i = 1

        DO WHILE (i .LT. n)
            i = i+1
            IF (Primes(i) .EQ. 0) CYCLE
            WHERE (MOD(Primes(i+1:n),i) .EQ. 0) Primes(i+1:n) = 0
        END DO

        ALLOCATE(Mask(n))
        IF (ALLOCATED(Mask)) THEN
            WHERE (Primes .NE. 0)
                Mask = .TRUE.
            ELSEWHERE
                Mask = .FALSE.
            END WHERE
        END IF

        PRINT*, PACK(Primes, Mask)

        IF(ALLOCATED(Primes)) DEALLOCATE(Primes)

      END PROGRAM PrimeNumbers