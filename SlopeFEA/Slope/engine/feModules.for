      MODULE gcontrol

	INTEGER :: NNOD     ! number of nodes (from .nod file)
      INTEGER :: NEL      ! number of elements (from .ele file)
      INTEGER :: NVAR     ! number of dofs per node (from .nod file)
      INTEGER :: NNODEL   ! number of nodes per element (from .ele file)
      INTEGER :: NVEL     ! number of dofs per element (nvar*nnodel)
      INTEGER :: NNET     ! number of system dofs (computed in BANDWH)
	INTEGER :: LBAND    ! half bandwidth of stiffness matrix


	CONTAINS

      ! COMPUTES THE HALF BANDWIDTH OF THE STIFFNESS MATRIX
	! (NEEDED FOR STORING THE MATRIX IN PACKED FORMAT TO
	! MAKE USE OF EFFICIENT SOLVERS)
	SUBROUTINE bandwidth (ICO, IX, LJ)

!$    USE OMP_LIB      

	IMPLICIT NONE

	INTEGER, INTENT(IN) :: ICO(:,:), IX(:)
	INTEGER, INTENT(INOUT) :: LJ(:)
	INTEGER :: min, max
	INTEGER :: i,j,k
	INTEGER :: k1, lbcurr

	! initialize bandwidth
	LBAND = 0

!$OMP PARALLEL PRIVATE(min,max,j,k,k1,lbcurr)
!$OMP DO
	DO i = 1,NEL    ! check the bandwidth for each element

	! obtain element connectivity
	DO j = 1,NVAR   ! dofs per node
	    DO k = 1,NNODEL ! nodes per element

	        k1 = (k-1)*NVAR ! initial index
	        LJ(J+k1) = IX(NVAR*ICO(K,I)-NVAR+J) ! get node number

	    END DO
	END DO

	! initialize index stats
	max = 0
	min = HUGE(1)   ! largest integer
      
	DO j = 1,NVEL   ! dof per element
	    
          ! skip if the dof is restrained (i.e. not present in stiffness matrix)
	    IF (LJ(j) .EQ. 0) CYCLE

	    ! compare and update as appropriate
	    IF (LJ(j)-max .GT. 0) max = LJ(j)  
	    IF (LJ(j)-min .LT. 0) min = LJ(j)  

	END DO

	lbcurr = max-min
	IF (lbcurr .GT. LBAND) THEN
!$OMP ATOMIC
          LBAND = lbcurr
	END IF

	END DO
!$OMP END DO
!$OMP END PARALLEL

	END SUBROUTINE bandwidth

	END MODULE gcontrol

