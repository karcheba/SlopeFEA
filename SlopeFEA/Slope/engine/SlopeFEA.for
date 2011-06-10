      SUBROUTINE slopefea (fname)
      USE gcontrol; USE mproperty; USE tractions
!
!$    USE OMP_LIB       ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fname    ! name of data files
!
      INTEGER, PARAMETER :: dbl = KIND(1.0D0)                   ! double kind parameter
      INTEGER, PARAMETER :: output=2,mtl=3,nod=4,ele=5,bel=6    ! input file unit numbers
      

      OPEN(output, FILE=fname(1:LEN(fname)-1)//".out")
      OPEN(mtl, FILE=fname(1:LEN(fname)-1)//".mtl")
      OPEN(nod, FILE=fname(1:LEN(fname)-1)//".nod")
      OPEN(ele, FILE=fname(1:LEN(fname)-1)//".ele")
      OPEN(bel, FILE=fname(1:LEN(fname)-1)//".bel")
    


      END SUBROUTINE slopefea