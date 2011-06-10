      MODULE gcontrol
      USE mproperty; USE tractions
!
!$    USE OMP_LIB         ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)  ! int/doub kind params
      REAL(dk), ALLOCATABLE :: XG(:),YG(:)              ! grid coords
      REAL(dk), ALLOCATABLE :: TLOAD(:),GLOAD(:)        ! load vecs
      REAL(dk), ALLOCATABLE :: DISP(:),TDISP(:)         ! displacement vec
      REAL(dk), ALLOCATABLE :: GSTIF(:,:)               ! global stiffness mat
      REAL(dk), ALLOCATABLE :: estif(:,:)               ! element stiffness mat
      INTEGER(ik), ALLOCATABLE :: LJ(:),ICO(:,:),IX(:)  ! connectivity/fixity
      INTEGER(ik), SAVE :: NNOD     ! # of nodes (from .nod file)
      INTEGER(ik), SAVE :: NEL      ! # of elements (from .ele file)
      INTEGER(ik), SAVE :: NVAR     ! # of dofs per node (from .nod file)
      INTEGER(ik), SAVE :: NNODEL   ! # of nodes per element (from .ele file)
      INTEGER(ik), SAVE :: NVEL     ! # of dofs per element (nvar*nnodel)
      INTEGER(ik), SAVE :: NNET     ! # of system dofs (computed in BANDWH)
      INTEGER(ik), SAVE :: LBAND    ! half bandwidth of stiffness matrix
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
      SUBROUTINE INPUT (mtl,nod,ele,bel)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: mtl,nod,ele,bel    ! input file units
      END SUBROUTINE INPUT
!
!
! ......................................................................
! .... BANDWIDTH .......................................................
! ......................................................................
!     computes the number of co-diagonal bands of the stiffness matrix
!     (needed for storing the matrix in packed format to
!     make use of efficient solvers)
! ......................................................................
      SUBROUTINE BANDWH (ICO, IX, LJ)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: ICO(:,:), IX(:)  ! global connect/fix
      INTEGER, INTENT(INOUT) :: LJ(:)         ! local connect/fix
      INTEGER :: min, max                     ! connectivity stats
      INTEGER :: i,j,k                        ! loop variables      
      INTEGER :: k1, lbcurr                   ! begin index, current band
!
!     initialize bandwidth
      LBAND = 0
!
!$OMP PARALLEL PRIVATE(min,max,j,k,k1,lbcurr)
!$OMP DO
      DO i = 1,NEL    ! check the bandwidth for each element
!
!         obtain element connectivity
          DO j = 1,NVAR   ! dofs per node
              DO k = 1,NNODEL ! nodes per element
!
	            k1 = (k-1)*NVAR ! initial index
	            LJ(J+k1) = IX(NVAR*ICO(K,I)-NVAR+J) ! get global node number
!
	        END DO
	     END DO
!
!         initialize index statistics
	     max = 0
	     min = HUGE(1)   ! largest integer
!
	     DO j = 1,NVEL   ! dof per element
!	    
!             skip if the dof is restrained (i.e. not present in stiffness matrix)
	        IF (LJ(j) .EQ. 0) CYCLE
!
!             compare and update as appropriate
	        IF (LJ(j)-max .GT. 0) max = LJ(j)  
	        IF (LJ(j)-min .LT. 0) min = LJ(j)  
!
	     END DO
!
	     lbcurr = max-min
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
      END SUBROUTINE bandwidth
!
!
! ......................................................................
! .... MAP UPPER BAND ..................................................
! ......................................................................
!     maps the indices of the full stiffness matrix to packed storage
!     (for use with dpbsvx from lapack, see man pages for this
!     subroutine online for an example of the storage scheme)
! ......................................................................
      SUBROUTINE MAPSTU (AB,S,LJ)
      IMPLICIT NONE
!
      REAL(dk), INTENT(INOUT) :: AB(:,:)     ! banded stiff mat
      REAL(dk), INTENT(IN) :: S(:,:), LJ(:)  ! element stiff/connect
      INTEGER :: i,j,k                        ! loop variables
      INTEGER :: ljr, ljc                     ! global indices
!
!$OMP PARALLEL PRIVATE(j,k,ljr,ljc)
!$OMP DO
      DO i = 1,NVEL   ! rows of element stiff
!
          ljr = LJ(i)
          IF (ljr .EQ. 0) CYCLE   ! skip if node is fixed
!
          DO j = i,NVEL   ! cols of element stiff
!
          ljc = LJ(j)
          IF (ljc .EQ. 0) CYCLE   ! skip if node is fixed
!
!       ensure row<col for upper band storage
          IF (ljr .LE. ljc) THEN
	        k = LBAND + 1 + ljr - ljc
!$OMP ATOMIC
	        AB(k,ljc) = AB(k,ljc) + S(i,j)
          ELSE
	        k = LBAND + 1 + ljc - ljr
!$OMP ATOMIC
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
      END SUBROUTINE MAPSTU
!
!
      END MODULE gcontrol