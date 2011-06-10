! ......................................................................
! .... MATERIAL PROPERTIES .............................................
! ......................................................................
!     storage of material information
! ......................................................................
      MODULE mproperty
!
      INTEGER, PARAMETER :: dk = KIND(1.0D0)    ! doub kind param
      REAL(dk), ALLOCATABLE :: EMOD(:)      ! elastic modulus
      REAL(dk), ALLOCATABLE :: ANV(:)       ! poisson's ratio
      REAL(dk), ALLOCATABLE :: PHI(:)       ! internal angle of fric
      REAL(dk), ALLOCATABLE :: PSI(:)       ! dilatancy angle
      REAL(dk), ALLOCATABLE :: COH(:)       ! cohesion
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
      INTEGER(ik) :: NELB       ! # of traction elements
!
      END MODULE tractions