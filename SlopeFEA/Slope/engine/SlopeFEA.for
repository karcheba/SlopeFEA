      SUBROUTINE slopefea (fpath)
      USE gcontrol; USE mproperty; USE tractions
!
!$    USE OMP_LIB       ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fpath    ! path to data files
!
      CALL INPUT(fpath)     ! initialize data in gcontrol                                             
!
      END SUBROUTINE slopefea