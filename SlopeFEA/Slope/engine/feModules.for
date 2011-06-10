! ......................................................................
! .... MATERIAL PROPERTIES .............................................
! ......................................................................
!     storage of material information
! ......................................................................
      MODULE mproperty
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)  ! int/doub kind params
      REAL(dk), ALLOCATABLE :: GAMMA(:)     ! elastic modulus
      REAL(dk), ALLOCATABLE :: PHI(:)       ! internal angle of fric
      REAL(dk), ALLOCATABLE :: COH(:)       ! cohesion
      REAL(dk), ALLOCATABLE :: PSI(:)       ! dilatancy angle
      REAL(dk), ALLOCATABLE :: EMOD(:)      ! elastic modulus
      REAL(dk), ALLOCATABLE :: NU(:)        ! poisson's ratio
      INTEGER(ik), SAVE :: NMAT     ! # of materials
!
      END MODULE mproperty
!
!
! ......................................................................
! .... TRACTIONS .......................................................
! ......................................................................
!     storage of loading information
! ......................................................................
      MODULE tractions
!
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)  ! int/doub kind params
      REAL(dk), ALLOCATABLE :: TNF(:,:),TSF(:,:)    ! traction element loads
      INTEGER(ik), ALLOCATABLE :: ICOB(:,:)         ! traction element connect
      INTEGER(ik), SAVE :: NELB     ! # of traction elements
      INTEGER(ik), SAVE :: NNODELB  ! # nodes per traction element
!
      END MODULE tractions