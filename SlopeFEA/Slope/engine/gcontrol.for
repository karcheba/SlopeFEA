      MODULE gcontrol
      USE mproperty; USE nodes; USE elements; USE tractions
!
!$    USE OMP_LIB         ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)      ! int/doub kind params
      INTEGER, PARAMETER :: output=2,mtl=3,nod=4,ele=5,bel=6    ! input file unit numbers
      CHARACTER(LEN=64) :: ANTYPE   ! string denoting analysis type
!
      INTEGER(ik), SAVE :: NNET     ! # of system dofs (computed in BANDWH)
      INTEGER(ik), SAVE :: LBAND    ! # of co-diagonal bands in stiff mat (computed in BANDWH)
!
      REAL(dk), ALLOCATABLE :: TLOAD(:),GLOAD(:)    ! load vecs
      REAL(dk), ALLOCATABLE :: DISP(:),TDISP(:)     ! disp (and/or vel,acc,press,temp,etc.) vecs
      REAL(dk), ALLOCATABLE :: GSTIF(:,:)           ! global stiffness mat
      REAL(dk), ALLOCATABLE :: ESTIF(:,:)           ! element stiffness mat
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
      INTEGER(ik) :: i,i1,i2,j      ! loop variables
      INTEGER(ik) :: ntot           ! total possible dofs
      INTEGER(ik) :: currtime       ! for printing analysis date/time
      REAL(dk), ALLOCATABLE :: lcoords(:)   ! for computing element area
!
!     open input file units
      OPEN(output,  FILE=fpath(1:LEN(fpath)-1)//".out")
      OPEN(mtl,     FILE=fpath(1:LEN(fpath)-1)//".mtl")
      OPEN(nod,     FILE=fpath(1:LEN(fpath)-1)//".nod")
      OPEN(ele,     FILE=fpath(1:LEN(fpath)-1)//".ele")
      OPEN(bel,     FILE=fpath(1:LEN(fpath)-1)//".bel")
!
!     *********************************
!     ********* MATERIAL DATA *********
!     *********************************
      READ(mtl,*) NMAT          ! get number of materials
      ALLOCATE( GRR(NMAT),
     +          PHI(NMAT),
     +          COH(NMAT),      ! allocate material data storage
     +          PSI(NMAT),
     +          EMOD(NMAT),
     +          NU(NMAT)    )
      GRR(:)    = 0.0
      PHI(:)    = 0.0
      COH(:)    = 0.0       ! initialize
      PSI(:)    = 0.0
      EMOD(:)   = 0.0
      NU(:)     = 0.0
      DO i = 1,NMAT
        READ(mtl,*) GRR(i), PHI(i), COH(i), EMOD(i), NU(i)    ! get data from file
      END DO
      CLOSE(mtl)
!
!     *********************************
!     *********** NODE DATA ***********
!     *********************************
      READ(nod,*) NNOD, NDIM, NVAR      ! # nodes, # dimensions, # dofs/node
      ALLOCATE( COORDS(NDIM,NNOD),
     +          PLOADS(NDIM,NNOD),      ! allocate storage for grid data
     +          IX(NNOD*NVAR)   )
      COORDS(:,:)   = 0.0
      PLOADS(:,:)   = 0.0   ! initialize
      IX(:)         = 0
      DO i = 1,NNOD
        i2 = NVAR*i     ! indices for fixity/node numbering vector
        i1 = i2-1
        READ(nod,*) j,
     +              COORDS(:,i),    ! node locations
     +              IX(i1:i2)       ! fixity data
     +              PLOADS(:,i)     ! point load data
      END DO
      ntot = NVAR*NNOD;     ! compute total possible dofs
      NNET = 0              ! initialize actual total dofs
      DO i = 1,ntot     ! loop through possible dofs
        IF (IX(i) .GT. 0) THEN  ! if the dof is not fixed
            NNET = NNET+1   ! increment actual total dofs
            IX(i) = NNET    ! label the node
        END IF
      END DO
      CLOSE(nod)
!
!     *********************************
!     ********* ELEMENT DATA **********
!     *********************************
      READ(ele,*) NEL, NNODEL   ! # body elements, # nodes per element
      NVEL = NVAR*NNODEL    ! compute #dofs/element
      NNN = NNODEL+1        ! compute #nodes+mtl type/element
      ALLOCATE( LJ(NVEL),
     +          ICO(NNN,NEL),
     +          AREA(NEL),                  ! allocate storage for element data
     +          XMC(NEL), YMC(NEL),
     +          lcoords(NDIM,NNODEL)    )
      LJ(:)         = 0
      ICO(:,:)      = 0
      AREA(:)       = 0.0
      XMC(:)        = 0.0   ! initialize
      YMC(:)        = 0.0
      lcoords(:,:)  = 0.0
      DO i = 1,NEL
        READ(ele,*) j, ICO(:,i)     ! read connect/mtl data for each element
        DO j = 1,NNODEL
            lcoords(j,:) = COORDS(:,ICO(j,i))   ! get vertex coords
        END DO
        AREA(i) = lcoords(1,NNODEL)*lcoords(2,1) 
     +                  - lcoords(1,1)*lcoords(2,NNODEL)    ! compute element area
        DO j = 2,NNODEL
            AREA(i) = AREA(i)   + lcoords(1,j-1)*lcoords(2,  j)
     +                          - lcoords(1,  j)*lcoords(2,j-1)
        END DO
        AREA(i) = 0.5*AREA(i)
        XMC(i) = SUM(lcoords(1,:))/NNODEL   ! compute element centroid
        YMC(i) = SUM(lcoords(2,:))/NNODEL
      END DO
      CLOSE(ele)
!
!     *********************************
!     ********* TRACTION DATA *********
!     *********************************
      READ(bel,*) NELT, NNODELT     ! # traction elements, # nodes/traction element
      ALLOCATE( ICOT(NNODELT,NELT),
     +          TNF(NNODELT,NELT),      ! allocate storage for traction data
     +          TSF(NNODELT,NELT)   )
      ICOT(:,:) = 0
      TNF(:,:)  = 0.0   ! initialize
      TST(:,:)  = 0.0
      DO i = 1,NELT
        READ(bel,*) j,
     +              ICOT(:,i),  ! read traction element data
     +              TNF(:,i),
     +              TSF(:,i)
      END DO
      CLOSE(bel)
!
!     *********************************
!     ********** OUTPUT FILE **********
!     *********************************
!     write header
      CALL DATE_AND_TIME(VALUES=currtime)
      WRITE(output,100)     NNODEL,
     +                      fpath(1:LEN(fpath)-1),
     +                      currtime(2), currtime(3), currtime(1),
     +                      currtime(5:8)
  100 FORMAT(/, '====================================================',
     +       /, 6X, I1, '-NODED FINITE ELEMENT STRESS ANALYSIS',
     +       /, '====================================================',
     +       //, 'FILE: ', A,
     +       /, 'DATE: ', I2, '/', I2, '/', I4,
     +       /, 'TIME: ', I2, ':', I2, ':', I2, '.', I3 )
  110 FORMAT(//)
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
!     (for use with DPBSVX() from LAPACK, see man pages for this
!     subroutine online for an example of the storage scheme)
! ......................................................................
      SUBROUTINE MAPST (AB,S)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(INOUT) :: AB(:,:)    ! global mat (packed band storage)
      REAL(dk), INTENT(IN) :: S(:,:)        ! element mat (dense storage)
      INTEGER :: i,j,k      ! loop variables
      INTEGER :: ljr,ljc    ! global row/col connect indices
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