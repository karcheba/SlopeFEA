      SUBROUTINE slopefea (fpath)
      USE gcontrol
!
!$    USE OMP_LIB       ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fpath    ! path to data files
!
      ANTYPE = 'PLANE STRAIN ELASTIC'
      CALL INPUT(fpath)     ! initialize data in gcontrol                                             
!
      END SUBROUTINE slopefea