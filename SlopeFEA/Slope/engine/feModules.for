! ......................................................................
! .... NUMERIC INFO ....................................................
! ......................................................................
!     data types
! ......................................................................
      MODULE numeric
      IMPLICIT NONE
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)  ! int/doub kind params
      END MODULE numeric
!
!
! ......................................................................
! .... MATERIAL PROPERTIES .............................................
! ......................................................................
!     storage of material information
! ......................................................................
      MODULE mproperty
      USE numeric
!
      IMPLICIT NONE
!
      INTEGER(ik), SAVE :: NMTL             ! # of materials
      REAL(dk), ALLOCATABLE :: GRR(:)       ! unit weight
      REAL(dk), ALLOCATABLE :: PHI(:)       ! internal angle of fric
      REAL(dk), ALLOCATABLE :: COH(:)       ! cohesion
      REAL(dk), ALLOCATABLE :: PSI(:)       ! dilatancy angle
      REAL(dk), ALLOCATABLE :: EMOD(:)      ! elastic modulus
      REAL(dk), ALLOCATABLE :: NU(:)        ! poisson's ratio
!
      END MODULE mproperty
!
!
! ......................................................................
! .... NODE PROPERTIES .................................................
! ......................................................................
!     storage of node information
! ......................................................................
      MODULE nodes
      USE numeric
!
      IMPLICIT NONE
!
      INTEGER(ik), SAVE :: NNOD     ! # of nodes (from .nod file)
      INTEGER(ik), SAVE :: NDIM     ! # of dimensions (e.g. 2d, 3d)
      INTEGER(ik), SAVE :: NVAR     ! # of dofs per node (from .nod file)
      INTEGER(ik), ALLOCATABLE :: IX(:)     ! connectivity/fixity
      REAL(dk), ALLOCATABLE :: COORDS(:,:)  ! grid coords
      REAL(dk), ALLOCATABLE :: PLOADS(:,:)    ! point loads
!
      END MODULE nodes
!
!
! ......................................................................
! .... ELEMENT PROPERTIES ..............................................
! ......................................................................
!     storage of body element information
! ......................................................................
      MODULE elements
      USE numeric
!
      IMPLICIT NONE
!
      INTEGER(ik), SAVE :: NEL      ! # of elements (from .ele file)
      INTEGER(ik), SAVE :: NNODEL   ! # of nodes per element (from .ele file)
      INTEGER(ik), SAVE :: NVEL     ! # of dofs per element (NVAR*NNODEL)
      INTEGER(ik), SAVE :: NNN      ! # of nodes per element +1 for mtl type
      INTEGER(ik), ALLOCATABLE :: LJ(:),ICO(:,:)    ! connectivity/fixity
      REAL(dk), ALLOCATABLE :: AREA(:)    ! element area
      REAL(dk), ALLOCATABLE :: CENT(:,:)  ! element centroid
!
      END MODULE elements
!
!
! ......................................................................
! .... TRACTIONS .......................................................
! ......................................................................
!     storage of loading information
! ......................................................................
      MODULE tractions
      USE numeric
!
      INTEGER(ik), SAVE :: NELT                     ! # of traction elements
      INTEGER(ik), SAVE :: NNODELT                  ! # nodes per traction element
      REAL(dk), ALLOCATABLE :: TNF(:,:),TSF(:,:)    ! traction element loads
      INTEGER(ik), ALLOCATABLE :: ICOT(:,:)         ! traction element connect
!
      END MODULE tractions