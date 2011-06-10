      MODULE gcontrol
      USE mproperty; USE tractions
!
!$    USE OMP_LIB         ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)      ! int/doub kind params
      INTEGER, PARAMETER :: output=2,mtl=3,nod=4,ele=5,bel=6    ! input file unit numbers
      REAL(dk), ALLOCATABLE :: COORDS(:,:)              ! grid coords
      REAL(dk), ALLOCATABLE :: PLOADS(:)                ! point loads
      REAL(dk), ALLOCATABLE :: TLOAD(:),GLOAD(:)        ! load vecs
      REAL(dk), ALLOCATABLE :: DISP(:),TDISP(:)         ! displacement vec
      REAL(dk), ALLOCATABLE :: GSTIF(:,:)               ! global stiffness mat
      REAL(dk), ALLOCATABLE :: ESTIF(:,:)               ! element stiffness mat
      INTEGER(ik), ALLOCATABLE :: LJ(:),ICO(:,:),IX(:)  ! connectivity/fixity
      INTEGER(ik), SAVE :: NNOD     ! # of nodes (from .nod file)
      INTEGER(ik), SAVE :: NDIM     ! # of dimensions (e.g. 2d, 3d)
      INTEGER(ik), SAVE :: NVAR     ! # of dofs per node (from .nod file)
      INTEGER(ik), SAVE :: NEL      ! # of elements (from .ele file)
      INTEGER(ik), SAVE :: NNODEL   ! # of nodes per element (from .ele file)
      INTEGER(ik), SAVE :: NVEL     ! # of dofs per element (NVAR*NNODEL)
      INTEGER(ik), SAVE :: NNET     ! # of system dofs (computed in BANDWH)
      INTEGER(ik), SAVE :: LBAND    ! # of co-diagonal bands in stiff mat (computed in BANDWH)
!	
!
!
      CONTAINS
!
!
! ......................................................................
! .... INPUT ...........................................................
! ......................................................................
!     puts information from data input files into appropriate storage
! ......................................................................
      SUBROUTINE INPUT (fpath)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fpath        ! input file path
      INTEGER(ik) :: i
!
!     open input file units
      OPEN(output,  FILE=fpath(1:LEN(fpath)-1)//".out")
      OPEN(mtl,     FILE=fpath(1:LEN(fpath)-1)//".mtl")
      OPEN(nod,     FILE=fpath(1:LEN(fpath)-1)//".nod")
      OPEN(ele,     FILE=fpath(1:LEN(fpath)-1)//".ele")
      OPEN(bel,     FILE=fpath(1:LEN(fpath)-1)//".bel")
!
!     read in material data
      READ(mtl,*), NMAT
      ALLOCATE( GAMMA(NMAT),
     *          PHI(NMAT),
     *          COH(NMAT),
     *          PSI(NMAT),
     *          EMOD(NMAT),
     *          NU(NMAT))
      GAMMA(:)  = 0.0
      PHI(:)    = 0.0
      COH(:)    = 0.0       ! initialize
      PSI(:)    = 0.0
      EMOD(:)   = 0.0
      NU(:)     = 0.0
      DO i = 1,NMAT
        READ(mtl,*), GAMMA(i), PHI(i), COH(i), EMOD(i), NU(i)
      END DO
      CLOSE(mtl)
!
!     read in node data
      READ(nod,*), 
!
      END SUBROUTINE INPUT
!
!
! ......................................................................
! .... BANDWH ..........................................................
! ......................................................................
!     computes the number of co-diagonal bands of the stiffness matrix
!     (needed for storing the matrix in packed format to
!     make use of efficient solvers)
! ......................................................................
      SUBROUTINE BANDWH ()
!
      IMPLICIT NONE
!
      INTEGER :: lmin, lmax     ! connectivity stats
      INTEGER :: i,j,k          ! loop variables      
      INTEGER :: k1, lbcurr     ! begin index, current band
!
!     initialize bandwidth
      LBAND = 0
!
!$OMP PARALLEL PRIVATE(lmin,lmax,j,k,k1,lbcurr)
!$OMP DO
      DO i = 1,NEL    ! check the bandwidth for each element
!
!         obtain element connectivity
        DO j = 1,NVAR   ! dofs per node
            DO k = 1,NNODEL ! nodes per element
!
                k1 = (k-1)*NVAR ! initial index
	            LJ(j+k1) = IX(NVAR*ICO(k,i)-NVAR+j) ! get global node number
!
            END DO
        END DO
!
!       initialize index statistics
        lmax = 0
        lmin = HUGE(1)   ! largest integer
!
        DO j = 1,NVEL   ! dof per element
!	    
!           skip if the dof is restrained (i.e. not present in stiffness matrix)
	        IF (LJ(j) .EQ. 0) CYCLE
!
!           compare and update as appropriate
	        IF (LJ(j)-lmax .GT. 0) lmax = LJ(j)  
	        IF (LJ(j)-lmin .LT. 0) lmin = LJ(j)  
!
        END DO
!
        lbcurr = lmax-lmin
        IF (lbcurr .GT. LBAND) THEN
!$OMP ATOMIC
            LBAND = lbcurr
        END IF
!
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      RETURN
!
      END SUBROUTINE BANDWH
!
!
! ......................................................................
! .... MAPST ...........................................................
! ......................................................................
!     maps the indices of the full stiffness matrix to packed storage
!     (for use with dpbsvx from lapack, see man pages for this
!     subroutine online for an example of the storage scheme)
! ......................................................................
      SUBROUTINE MAPST (AB,S)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(INOUT) :: AB(:,:)    ! global mat (packed band storage)
      REAL(dk), INTENT(IN) :: S(:,:)        ! element mat (dense storage)
      INTEGER :: i,j,k      ! loop variables
      INTEGER :: ljr, ljc   ! global connect indices
!
!$OMP PARALLEL PRIVATE(j,k,ljr,ljc)
!$OMP DO
      DO i = 1,NVEL   ! rows of element mat
!
          ljr = LJ(i)
          IF (ljr .EQ. 0) CYCLE   ! skip if node is fixed
!
          DO j = i,NVEL   ! cols of element mat
!
            ljc = LJ(j)
            IF (ljc .EQ. 0) CYCLE   ! skip if node is fixed
!
!           ensure row<=col for upper band storage
            IF (ljr .LE. ljc) THEN
                k = LBAND + 1 + ljr - ljc
!$OMP ATOMIC    ! only one thread may increment this at a time
                AB(k,ljc) = AB(k,ljc) + S(i,j)
            ELSE
                k = LBAND + 1 + ljc - ljr
!$OMP ATOMIC    ! only one thread may increment this at a time
                AB(k,ljr) = AB(k,ljr) + S(i,j)
            END IF
!
          END DO  ! cols
!
      END DO  ! rows
!$OMP END DO
!$OMP END PARALLEL
!
      RETURN
!
      END SUBROUTINE MAPST
!
!
      END MODULE gcontrol