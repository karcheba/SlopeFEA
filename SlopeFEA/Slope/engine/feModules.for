!***********************************************************************
! PROJECT:     SlopeFEA (c) 2011 Brandon Karchewski
!              Licensed under the Academic Free License version 3.0
!                  http://www.opensource.org/licenses/afl-3.0.php
! 
! CONTACT:     Brandon Karchewski
!              Department of Civil Engineering
!              McMaster University, JHE-301
!              1280 Main St W
!              Hamilton, Ontario, Canada
!              L8S 4L7
!              p: 905-525-9140 x24287
!              f: 905-529-9688
!              e: karcheba@mcmaster.ca
!              
! 
! SOURCE INFORMATION:
! 
! The repository for this software project is hosted on git at:
!      
!      git://github.com/karcheba/SlopeFEA
!      
! As such, the code for the project is free and open source.
! The relevant license is AFLv3 (see link above). See the
! README file in the root directory of the repository for a
! detailed project description, acknowledgements, references,
! and the revision history.
!***********************************************************************
!
!
! ......................................................................
! .... NUMERIC INFO ....................................................
! ......................................................................
!     data types
! ......................................................................
      MODULE numeric
      IMPLICIT NONE
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)  ! int/doub kind params
      REAL(dk), PARAMETER :: PI = 3.1415926535898
      REAL(dk), PARAMETER :: degTOrad = PI / 180.0D0
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
      INTEGER(ik), SAVE :: IPRINT   ! node # for printing
      INTEGER(ik), ALLOCATABLE :: IX(:)     ! fix info
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
      INTEGER(ik), ALLOCATABLE :: LJ(:),ICO(:,:)    ! connect info
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