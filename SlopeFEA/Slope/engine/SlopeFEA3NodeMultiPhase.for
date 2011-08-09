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
! **********************************************************************
! **** NUMERIC *********************************************************
! **********************************************************************
!     data kinds and other constants
! **********************************************************************
      MODULE NUMERIC
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: ik = KIND(1), dk = KIND(1.0D0)  ! int/doub kind params
      REAL(dk), PARAMETER :: PI = 3.1415926535898
      REAL(dk), PARAMETER :: degTOrad = PI/180.0D0,radTOdeg = 180.0D0/PI
      REAL(dk), PARAMETER :: TOLER = 1.0E-7
      REAL(dk), PARAMETER :: ONE_THIRD = 1.0D0 / 3.0D0
!
      END MODULE NUMERIC
!
!
! **********************************************************************
! **** MPROPERTY *******************************************************
! **********************************************************************
!     storage of material information
! **********************************************************************
      MODULE MPROPERTY
      USE NUMERIC     ! number types and constants
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
      END MODULE MPROPERTY
!
!
! **********************************************************************
! **** NODES ***********************************************************
! **********************************************************************
!     storage of node information
! **********************************************************************
      MODULE NODES
      USE NUMERIC     ! number types and constants
!
      IMPLICIT NONE
!
      INTEGER(ik), SAVE :: NNOD     ! # of nodes (from .nod file)
      INTEGER(ik), SAVE :: NDIM     ! # of dimensions (e.g. 2d, 3d)
      INTEGER(ik), SAVE :: NVAR     ! # of dofs per node (from .nod file)
      INTEGER(ik), SAVE :: IPRINT   ! node # for printing
      INTEGER(ik), POINTER :: IX(:,:)
      INTEGER(ik), POINTER :: IXC(:), IXP(:)  ! fix info, C=curr, P=prev
      REAL(dk), ALLOCATABLE :: COORDS(:,:)    ! grid coords
      REAL(dk), POINTER :: PLOAD(:,:)         ! point loads
      REAL(dk), POINTER :: PLOADC(:)          ! current phase point loads
      REAL(dk), ALLOCATABLE :: EVOL(:), DIA(:)!, EVOLi(:), EVOL0(:)  ! for volumetric strain
!
      END MODULE NODES
!
!
! **********************************************************************
! **** ELEMENTS ********************************************************
! **********************************************************************
!     storage of body element information
! **********************************************************************
      MODULE ELEMENTS
      USE NUMERIC     ! number types and constants
!
      IMPLICIT NONE
!
      INTEGER(ik), SAVE :: NEL      ! # of elements (from .ele file)
      INTEGER(ik), SAVE :: NNODEL   ! # of nodes per element (from .ele file)
      INTEGER(ik), SAVE :: NVEL     ! # of dofs per element (NVAR*NNODEL)
      INTEGER(ik), SAVE :: NNN      ! # of nodes per element +1 for mtl type
      INTEGER(ik), ALLOCATABLE :: LJ(:), ICO(:,:)                       ! connect info
      REAL(dk), ALLOCATABLE :: AREA(:)                                  ! element area
      REAL(dk), ALLOCATABLE :: CENT(:,:)                                ! element centroid
      INTEGER(ik), ALLOCATABLE :: IPL(:)                                ! plastic points
      REAL(dk), ALLOCATABLE :: EVOLB(:)                                 ! element volumetric strain
      REAL(dk), ALLOCATABLE :: SXX(:), SYY(:), SXY(:), SZZ(:), FBAR(:)  ! internal stresses
!
      END MODULE ELEMENTS
!
!
! **********************************************************************
! **** TRACTIONS *******************************************************
! **********************************************************************
!     storage of loading information
! **********************************************************************
      MODULE TRACTIONS
      USE NUMERIC     ! number types and constants
!
      INTEGER(ik), SAVE :: NELT                   ! # of traction elements
      INTEGER(ik), SAVE :: NNODELT                ! # nodes per traction element
      INTEGER(ik), SAVE :: NVELT                  ! # of dofs per traction element (NVAR*NNODELT)
      REAL(dk), POINTER :: TNF(:,:), TSF(:,:)     ! traction element loads
      REAL(dk), POINTER :: TNFC(:,:), TSFC(:,:)   ! traction element loads (current phase)
      INTEGER(ik), ALLOCATABLE :: ICOT(:,:)       ! traction element connectivity
!
      END MODULE TRACTIONS
!
!
! **********************************************************************
! **** GCONTROL ********************************************************
! **********************************************************************
!     input/output, grid control (mapping, etc*), solution space
! **********************************************************************
      MODULE GCONTROL
      USE NUMERIC     ! number types and constants
      USE MPROPERTY   ! material property data
      USE NODES       ! node data
      USE ELEMENTS    ! element data
      USE TRACTIONS   ! applied load data
!
!$    USE OMP_LIB         ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: phs=2,mtl=3,nod=4,ele=5,bel=6,  ! input files
     +                      ld=7,uv=8,st=9,                 ! inter-phase files
     +                      op=10,hs=11,nd=12,el=13,er=14   ! output files
      CHARACTER(LEN=64) :: ANTYPE   ! string denoting analysis type
!
      INTEGER(ik), ALLOCATABLE :: BEGPHS(:)   ! phase # to load initial conditions from (stresses, disps, etc.)
      INTEGER(ik), ALLOCATABLE :: RESET(:)    ! reset displacements?
      INTEGER(ik), ALLOCATABLE :: NSTEP(:)    ! # of load steps
      INTEGER(ik), ALLOCATABLE :: NITER(:)    ! # of iterations
      INTEGER(ik), ALLOCATABLE :: NPRINT(:)   ! # of print lines
      INTEGER(ik), SAVE :: IPHASE, NPHASE     ! current analysis phase and total # of analysis phases
!
      REAL(dk), ALLOCATABLE :: GFACT(:)     ! gravity factor
!
      INTEGER(ik), SAVE :: NTOT             ! # of possible system dofs (NNOD*NVAR)
      INTEGER(ik), ALLOCATABLE :: NNET(:)   ! # of system dofs (computed in INPUT)
      INTEGER(ik), ALLOCATABLE :: LBAND(:)  ! # of co-diagonal bands in stiff mat (computed in BANDWH)
!
      REAL(dk), ALLOCATABLE :: GLOADC(:), GLOADP(:)           ! load vecs, C=curr, P=prev
      REAL(dk), ALLOCATABLE :: GLOAD0(:)                      ! for non-linear stepping
      REAL(dk), ALLOCATABLE :: STRC(:), STRP(:)               ! internal forces
      REAL(dk), ALLOCATABLE :: DISP(:), TDISPC(:), TDISPP(:)  ! disp (and/or vel,acc,press,temp,etc.) vecs
      REAL(dk), ALLOCATABLE :: GSTIF(:,:), fGSTIF(:,:)        ! global stiffness mat
      REAL(dk), ALLOCATABLE :: ESTIF(:,:)                     ! element stiffness mat
      INTEGER(ik), ALLOCATABLE :: HBW(:)                      ! half bandwidth of stiff mat (LBAND+1)
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
      REAL(dk), ALLOCATABLE :: lcoords(:,:)   ! for computing element area
      INTEGER(ik) :: ierr   ! for check allocation status
!
!     open input file units
      OPEN(phs,     FILE=fpath(1:LEN(fpath)-1)//".phs")
      OPEN(mtl,     FILE=fpath(1:LEN(fpath)-1)//".mtl")
      OPEN(nod,     FILE=fpath(1:LEN(fpath)-1)//".nod")
      OPEN(ele,     FILE=fpath(1:LEN(fpath)-1)//".ele")
      OPEN(bel,     FILE=fpath(1:LEN(fpath)-1)//".bel")
!
!     open unit for displaying error messages
      OPEN(er,   FILE=fpath(1:LEN(fpath)-1)//".ERR")
!
!
!     *********************************
!     ********* CONTROL DATA **********
!     *********************************
      READ(phs,*) NPHASE        ! # of analysis phases
      WRITE(er,*) NPHASE
      ALLOCATE( BEGPHS(NPHASE), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating BEGPHS."
      ALLOCATE( RESET(NPHASE),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating RESET."
      ALLOCATE( NSTEP(NPHASE),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating NSTEP."
      ALLOCATE( NITER(NPHASE),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating NITER."
      ALLOCATE( NPRINT(NPHASE), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating NPRINT."
      ALLOCATE( GFACT(NPHASE),   STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating GFACT."
      BEGPHS(:) = 0
      RESET(:)  = 0
      NSTEP(:)  = 0
      NITER(:)  = 0
      NPRINT(:) = 0
      GFACT(:)  = 0.0D0
      DO i = 1,NPHASE
        READ(phs,*) j, BEGPHS(i), RESET(i),
     +                  NSTEP(i), NITER(i), NPRINT(i), GFACT(i)
!
        IF (NSTEP(i) .LT. 2)      NSTEP(i) = 2
        IF (NITER(i) .LT. 2)      NITER(i) = 2
        IF (GFACT(i) .LT. 0.0D0)  GFACT(i) = 0.0D0
      END DO
      REWIND(phs);    CLOSE(phs)
!
      READ(mtl,*) NMTL          ! # of materials
      ALLOCATE( GRR(NMTL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating GRR."
      ALLOCATE( PHI(NMTL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating PHI."
      ALLOCATE( COH(NMTL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating COH."
      ALLOCATE( PSI(NMTL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating PSI."
      ALLOCATE( EMOD(NMTL), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating EMOD."
      ALLOCATE( NU(NMTL),   STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating NU."
      GRR(:)    = 0.0D0
      PHI(:)    = 0.0D0
      COH(:)    = 0.0D0
      PSI(:)    = 0.0D0
      EMOD(:)   = 0.0D0
      NU(:)     = 0.0D0
!
      READ(nod,*) NNOD, NDIM, NVAR, IPRINT   ! # nodes, # dimensions, # dofs/node, print node
      WRITE(er,*) NNOD
      NTOT = NVAR*NNOD
      WRITE(er,*) NTOT
      ALLOCATE( COORDS(NDIM,NNOD),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating COORDS."
      ALLOCATE( PLOAD(NTOT,NPHASE),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating PLOAD."
      ALLOCATE( IX(NTOT,NPHASE),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating IX."
      ALLOCATE( EVOL(NNOD),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating EVOL."
      ALLOCATE( DIA(NNOD), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating DIA."
!      ALLOCATE( EVOLi(NNOD),  STAT=ierr)
!      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating EVOLi."
!      ALLOCATE( EVOL0(NNOD),  STAT=ierr)
!      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating EVOL0."
      COORDS(:,:)   = 0.0D0           ! alloc and init grid data storage
      PLOAD(:,:)    = 0.0D0
      IX(:,:)       = 0
      EVOL(:)       = 0.0D0
      DIA(:)        = 0.0D0
!      EVOLi(:)      = 0.0D0
!      EVOL0(:)      = 0.0D0
!
      READ(ele,*) NEL, NNODEL     ! # body elements, # nodes per element
      NVEL = NVAR*NNODEL          ! compute #dofs/element
      NNN = NNODEL+NPHASE         ! compute #nodes + mtl type/element (per phase)
      ALLOCATE( LJ(NVEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating LJ."
      ALLOCATE( ICO(NNN,NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating ICO."
      ALLOCATE( AREA(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating AREA."
      ALLOCATE( CENT(NDIM,NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating CENT."
      ALLOCATE( SXX(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating SXX."
      ALLOCATE( SYY(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating SYY."
      ALLOCATE( SXY(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating SXY."
      ALLOCATE( SZZ(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating SZZ."
      ALLOCATE( FBAR(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating FBAR."
      ALLOCATE( IPL(NEL), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating IPL."
      ALLOCATE( EVOLB(NEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating EVOLB."
      ALLOCATE( ESTIF(NVEL,NVEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating ESTIF."
      LJ(:)         = 0                     ! alloc and init element data storage
      ICO(:,:)      = 0
      AREA(:)       = 0.0D0
      CENT(:,:)     = 0.0D0
      SXX(:)        = 0.0D0
      SYY(:)        = 0.0D0
      SXY(:)        = 0.0D0
      SZZ(:)        = 0.0D0
      FBAR(:)       = 0.0D0
      IPL(:)        = 0
      EVOLB(:)      = 0.0D0
      ESTIF(:,:)    = 0.0D0
!
      READ(bel,*) NELT, NNODELT     ! # traction elements, # nodes/traction element
      NVELT = NVAR*NNODELT    ! compute # dofs per traction element
      ALLOCATE( ICOT(NNODELT,NELT),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating ICOT."
      ALLOCATE( TNF(NNODELT*NPHASE,NELT),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating TNF."
      ALLOCATE( TSF(NNODELT*NPHASE,NELT),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating TSF."
      ICOT(:,:) = 0                     ! alloc and init traction data storage
      TNF(:,:)  = 0.0D0
      TSF(:,:)  = 0.0D0
!
!
!     *********************************
!     ********* MATERIAL DATA *********
!     *********************************
      DO i = 1,NMTL
        READ(mtl,*) GRR(i), PHI(i), COH(i), PSI(i), EMOD(i), NU(i)    ! get data from file
!
        IF (COH(i) .LT. 1.0D-8) COH(i) = 1.0D-8   ! ensure small cohesion for numerical stability
        COH(i) = COH(i)*COS(degTOrad*PHI(i))
        PHI(i) = SIN(degTOrad*PHI(i))             ! pre-calculate trig functions of material props
        PSI(i) = SIN(degTOrad*PSI(i))
      END DO
      REWIND(mtl);    CLOSE(mtl)
!
!     *********************************
!     *********** NODE DATA ***********
!     *********************************
      DO i = 1,NNOD
        i2 = NVAR*i     ! indices for fixity/node numbering vector
        i1 = i2-1
        READ(nod,*) j,
     +              COORDS(:,i),    ! node locations
     +              IX(i1:i2,:),      ! fixity data
     +              PLOAD(i1:i2,:)     ! point load data
      END DO
      REWIND(nod);    CLOSE(nod)
!
!     write IX to ERR file (testing)
      DO i = 1,NTOT
        WRITE(er,*) IX(i,:)
      END DO
      WRITE(er,*)
!
!     *********************************
!     ********* ELEMENT DATA **********
!     *********************************
      ALLOCATE( lcoords(NDIM,NNODEL),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating lcoords."
      lcoords(:,:)  = 0.0D0
      DO i = 1,NEL
        READ(ele,*) j, ICO(:,i)     ! read connect/mtl data for each element
        DO j = 1,NNODEL
            lcoords(:,j) = COORDS(:,ICO(j,i))   ! get vertex coords
        END DO
        AREA(i) = lcoords(1,NNODEL)*lcoords(2,1) 
     +                  - lcoords(1,1)*lcoords(2,NNODEL)    ! compute element area
        DO j = 2,NNODEL
            AREA(i) = AREA(i)   + lcoords(1,j-1)*lcoords(2,  j)
     +                          - lcoords(1,  j)*lcoords(2,j-1)
        END DO
        AREA(i) = 0.5D0*AREA(i)
        DO j = 1,NDIM
          CENT(j,i) = SUM(lcoords(j,:))/NNODEL    ! compute element centroid
        END DO
      END DO
      DEALLOCATE(lcoords)
      REWIND(ele);    CLOSE(ele)
!
!     compute stiffness matrix information
      CALL BANDWH()
!
!     *********************************
!     ********* TRACTION DATA *********
!     *********************************
      DO i = 1,NELT
        READ(bel,*) j,
     +              ICOT(:,i),  ! read traction element data
     +              TNF(:,i),
     +              TSF(:,i)
      END DO
      REWIND(bel);    CLOSE(bel)
!
      RETURN
!
      END SUBROUTINE INPUT
!
!
! ......................................................................
! .... ALLOCPHS ........................................................
! ......................................................................
!     allocate memory and open data files for current phase
! ......................................................................
      SUBROUTINE ALLOCPHS (fpath)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fpath        ! input file path
      INTEGER(ik) :: nnetC,nnetP, hbwC, imtlP
      INTEGER(ik) :: i, ierr
      CHARACTER(LEN=3) :: phsnum   ! string denoting phase number
!
!     allocate solution space for current phase
      nnetC = NNET(IPHASE)
      hbwC = HBW(IPHASE)
      ALLOCATE( GLOADC(nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating GLOADC."
      ALLOCATE( GLOAD0(nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating GLOAD0."
      ALLOCATE( STRC(nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating STRC."
      ALLOCATE( DISP(nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating DISP."
      ALLOCATE( TDISPC(nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating TDISPC."
      ALLOCATE( GSTIF(hbwC,nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating GSTIF."
      ALLOCATE( fGSTIF(hbwC,nnetC),  STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating fGSTIF."
!
!     initialize solution space for current phase
      GLOADC(:)   = 0.0D0
      GLOAD0(:)   = 0.0D0
      STRC(:)     = 0.0D0
      DISP(:)     = 0.0D0
      TDISPC(:)   = 0.0D0
      GSTIF(:,:)  = 0.0D0
      fGSTIF(:,:) = 0.0D0
      SXX(:)      = 0.0D0
      SYY(:)      = 0.0D0
      SXY(:)      = 0.0D0
      SZZ(:)      = 0.0D0
      FBAR(:)     = 0.0D0
      IPL(:)      = 0
      EVOLB(:)    = 0.0D0
!
!     point to listings for current phase
      IXC => IX(:,IPHASE)
      PLOADC => PLOAD(:,IPHASE)
      i = NNODELT*IPHASE
      TNFC => TNF(i-1:i,:)
      TSFC => TSF(i-1:i,:)
!
      IF (BEGPHS(IPHASE) .NE. 0) THEN
!
!       point to dof listing for previous phase
        IXP => IX(:,BEGPHS(IPHASE))
!
!       allocate space for previous load and stress vectors
        nnetP = NNET(BEGPHS(IPHASE))
        ALLOCATE( GLOADP(nnetP),  STAT=ierr)
        IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating GLOADP."
        ALLOCATE( STRP(nnetP),  STAT=ierr)
        IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating STRP."
        GLOADP(:)    = 0.0D0
        STRP(:)      = 0.0D0
!
!       write phase number to string
        WRITE(phsnum,"(I3.3)") BEGPHS(IPHASE)
!
!       open input files
        OPEN(ld,      FILE=fpath(1:LEN(fpath)-1)//".LD"//phsnum)
        OPEN(st,      FILE=fpath(1:LEN(fpath)-1)//".ST"//phsnum)
!
!       read in previous load and stress data
        DO i = 1,nnetP
          READ(ld,*) GLOADP(i)
          READ(st,*) STRP(i)
        END DO
        imtlP = NNODEL + BEGPHS(IPHASE) ! material type index in connectivity array
        DO i = 1,NEL
          IF (ICO(imtlP,i) .NE. 0) THEN    ! only read for elements that were present in previous phase
            READ(st,*) SXX(i),SYY(i),SXY(i),SZZ(i),IPL(i),EVOLB(i)
          END IF
        END DO
!
!       set up initial load and stress state
        DO i = 1,NTOT
!         ***
!         NOTE: There are 4 cases to consider:
!
!                 i) The dof is absent from both prev and curr phases.
!                 ii) The dof is present in curr phase, but was absent from prev phase.
!                 iii) The dof was present in prev phase, but is absent from curr phase.
!                 iv) The dof is present in both prev and curr phase, but may possibly have a different number.
!
!               In case (i), clearly no information must pass between phases.
!               In case (ii), the dof will be initially load/stress-free, which is taken care of in initialization above.
!               In case (iii), the current phase is not influenced by the dof so the information is irrelevant.
!               In case (iv), the dof has an initial state that will influence the current phase.
!
!               Therefore, only pass data in case (iv), but must map between dof numbering for each phase.
!         ***
          IF (IXC(i).NE.0 .AND. IXP(i).NE.0) THEN
            GLOAD0(IXC(i))  = GLOADP(IXP(i))
            STRC(IXC(i))    = STRP(IXP(i))
          END IF
        END DO
!
!       clean up allocations for previous load and stress state
        DEALLOCATE( GLOADP, STRP )
        REWIND(ld); CLOSE(ld)
        REWIND(st); CLOSE(st)
!
!       if displacements are not to be reset, load previous displacement state
        IF (RESET(IPHASE) .EQ. 0) THEN
!
!         allocate and initialize space for previous displacments
          ALLOCATE( TDISPP(nnetP),  STAT=ierr)
          IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating TDISPP."
          TDISPP(:)    = 0.0D0
!
!         open the previous displacement file
          OPEN(uv,      FILE=fpath(1:LEN(fpath)-1)//".UV"//phsnum)
!
!         read in previous displacement data
          DO i = 1,nnetP
            READ(uv,*) TDISPP(i)
          END DO
!
!         map to initial state for curr phase (see NOTE above)
          DO i = 1,NTOT
            IF (IXC(i).NE.0 .AND. IXP(i).NE.0) THEN
              TDISPC(IXC(i)) = TDISPP(IXP(i))
            END IF
          END DO
!
!         clean up allocations for previous displacement state
          DEALLOCATE(TDISPP)
          REWIND(uv); CLOSE(uv)
!
        END IF      ! END OF previous displacement loading
!
      END IF      ! END OF previous load/stress loading
!
      RETURN
!
      END SUBROUTINE ALLOCPHS
!
!
! ......................................................................
! .... DEALLOCPHS ......................................................
! ......................................................................
!     deallocate memory and close data files for current phase
! ......................................................................
      SUBROUTINE DEALLOCPHS ()
!
      REWIND(op); CLOSE(op)
      REWIND(hs); CLOSE(hs)
      REWIND(ld); CLOSE(ld)
      REWIND(uv); CLOSE(uv)
      REWIND(st); CLOSE(st)
      REWIND(nd); CLOSE(nd)
      REWIND(el); CLOSE(el)
!
      DEALLOCATE( GLOADC, GLOAD0 )
      DEALLOCATE( TDISPC, DISP )
      DEALLOCATE( STRC )
      DEALLOCATE( GSTIF, fGSTIF )
!
      NULLIFY( IXC, IXP )
      NULLIFY( PLOADC, TNFC, TSFC )
!
      RETURN
!
      END SUBROUTINE DEALLOCPHS
!
!
! ......................................................................
! .... CLEANUP .........................................................
! ......................................................................
!     deallocate memory, rewind data units, close data files
! ......................................................................
      SUBROUTINE CLEANUP ()
!
!     phase data
      DEALLOCATE( BEGPHS, RESET, NSTEP, NITER, NPRINT, GFACT )
!
!     material data
      DEALLOCATE( GRR, PHI, COH, PSI, EMOD, NU )
!
!     node data
      DEALLOCATE( COORDS, IX, PLOAD, EVOL, DIA )!, EVOLi, EVOL0 )
!
!     body element data
      DEALLOCATE( ICO, LJ, AREA, CENT,
     +            SXX, SYY, SXY, SZZ, FBAR, IPL, EVOLB )
!
!     traction element data
      DEALLOCATE( ICOT, TNF, TSF )
!
!     close error message file
      REWIND(er); CLOSE(er)
!
      RETURN
!
      END SUBROUTINE CLEANUP
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
      INTEGER(ik) :: lmin, lmax     ! connectivity stats
      INTEGER(ik) :: i,j,k          ! loop variables      
      INTEGER(ik) :: k1, lbcurr     ! begin index, current band
      INTEGER(ik) :: ierr
!
!     allocate memory for stiffness matrix information
      ALLOCATE( NNET(NPHASE), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating NNET."
      ALLOCATE( HBW(NPHASE), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating HBW."
      ALLOCATE( LBAND(NPHASE), STAT=ierr)
      IF (ierr .NE. 0)  WRITE(er,*) "Error in allocating LBAND."
!
      NNET(:) = 0              ! initialize actual total dofs
      DO j = 1,NPHASE   ! loop through analysis phases
        DO i = 1,NTOT     ! loop through possible dofs
          IF (IX(i,j) .GT. 0) THEN  ! if the dof is not fixed
              NNET(j) = NNET(j)+1   ! increment actual total dofs
              IX(i,j) = NNET(j)    ! label the node
          END IF
        END DO
      END DO
!
!     write NTOT to ERR file (testing)
      WRITE(er,*)
      WRITE(er,*) NTOT
      WRITE(er,*)
!
!     write IX to ERR file (testing)
      DO i = 1,NTOT
        WRITE(er,*) IX(i,:)
      END DO
      WRITE(er,*)
!
!     initialize bandwidth
      LBAND(:)  = 0
      HBW(:)    = 0
!
      DO IPHASE = 1,NPHASE
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
              LJ(j+k1) = IX(NVAR*ICO(k,i)-NVAR+j, IPHASE) ! get global node number
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
!$OMP ATOMIC    ! only one thread may update at a time
          IF (lbcurr .GT. LBAND(IPHASE)) LBAND(IPHASE) = lbcurr
!
        END DO
!$OMP END DO
!$OMP END PARALLEL
!
        HBW(IPHASE) = LBAND(IPHASE)+1
!
      END DO
!
!
!     write results to ERR file (testing)
      WRITE(er,*) NNET(:)
      WRITE(er,*) LBAND(:)
      WRITE(er,*) HBW(:)
!
      RETURN
!
      END SUBROUTINE BANDWH
!
!
! ......................................................................
! .... LOCAL ...........................................................
! ......................................................................
!     gets local coords and material type of given element, iel
! ......................................................................
      SUBROUTINE LOCAL (iel, lcoords, mtype)
!
      IMPLICIT NONE
!
      INTEGER(ik), INTENT(IN) :: iel          ! element number
      INTEGER(ik), INTENT(OUT) :: mtype       ! material type
      REAL(dk), INTENT(OUT) :: lcoords(:,:)   ! local coords
      INTEGER(ik) :: i, i1, i2, j    ! loop variables
!
      lcoords(1:NDIM,1:NNODEL) = COORDS(1:NDIM,ICO(1:NNODEL,iel)) ! get local coords
      mtype = ICO(NNODEL+IPHASE,iel)    ! get material type
!
!$OMP PARALLEL PRIVATE(i,i1,i2)
!$OMP DO
!     get local connectivity
      DO j = 1,NNODEL   ! element nodes
        i1 = NVAR*(j-1)   ! local offset
        i2 = NVAR*(ICO(j,iel)-1)  ! global offset
        DO i = 1,NVAR   ! dofs
          LJ(i+i1) = IXC(i+i2)   ! map global to local
        END DO  ! dofs
      END DO  ! element nodes
!$OMP END DO
!$OMP END PARALLEL
!
      RETURN
!
      END SUBROUTINE LOCAL
!
!
! ......................................................................
! .... LOCALT ..........................................................
! ......................................................................
!     gets local coords of given traction element, iel
! ......................................................................
      SUBROUTINE LOCALT (iel, lcoordsT)
!
      IMPLICIT NONE
!
      INTEGER(ik), INTENT(IN) :: iel          ! element number
      REAL(dk), INTENT(OUT) :: lcoordsT(:,:)   ! local coords
      INTEGER(ik) :: i, i1, i2, j    ! loop variables
!
!     get local coords
      lcoordsT(1:NDIM,1:NNODELT) = COORDS(1:NDIM,ICOT(1:NNODELT,iel))
!
!     get local connectivity
      DO j = 1,NNODELT    ! traction element nodes
        i1 = NVAR*(j-1)   ! local offset
        i2 = NVAR*(ICOT(j,iel)-1)  ! global offset
        DO i = 1,NVAR   ! dofs
          LJ(i+i1) = IXC(i+i2)   ! map global to local
        END DO  ! dofs
      END DO  ! traction element nodes
!
      RETURN
!
      END SUBROUTINE LOCALT
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
      INTEGER(ik) :: i,j,k      ! loop variables
      INTEGER(ik) :: ljr,ljc    ! global row/col connect indices
      INTEGER(ik) :: lbandC
!
!     initialize number of codiagonal bands
      lbandC = LBAND(IPHASE)
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
            k = lbandC + 1 + ljr - ljc
!$OMP ATOMIC    ! only one thread may increment at a time
            AB(k,ljc) = AB(k,ljc) + S(i,j)
          ELSE
            k = lbandC + 1 + ljc - ljr
!$OMP ATOMIC    ! only one thread may increment at a time
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
! ......................................................................
! .... MAPLD ...........................................................
! ......................................................................
!     maps the indices of the element load vec to the global load vec
! ......................................................................
      SUBROUTINE MAPLD (GLO, loc, nv)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: loc(:)    ! element vec
      INTEGER(ik), INTENT(IN) :: nv     ! # dofs
      REAL(dk), INTENT(INOUT) :: GLO(:)     ! global vec
      INTEGER(ik) :: i, ljr    ! loop variable
!
!$OMP PARALLEL PRIVATE(ljr)
!$OMP DO
      DO i = 1,nv   ! element dofs
        ljr = LJ(i)     ! row of global load vec
        IF (ljr .EQ. 0) CYCLE   ! skip if dof is fixed
!$OMP ATOMIC    ! only one thread may increment at a time
        GLO(ljr) = GLO(ljr) + loc(i)    ! increment global
      END DO  ! element dofs
!$OMP END DO
!$OMP END PARALLEL
!
      RETURN
!
      END SUBROUTINE MAPLD
!
!
! ......................................................................
! .... GLOLOC ..........................................................
! ......................................................................
!     exchange global and local stresses
! ......................................................................
      SUBROUTINE GLOLOC (SXX, SYY, SXY, SZZ, SIG, iel, gl_lo)
!
      IMPLICIT NONE
!
      LOGICAL, INTENT(IN) :: gl_lo   ! .TRUE.=glo->loc, .FALSE.=loc->glo
      INTEGER(ik), INTENT(IN) :: iel  ! element number
      REAL(dk), INTENT(INOUT) :: SXX(:), SYY(:), SXY(:), SZZ(:), SIG(:)  ! stress vectors
!
      IF (gl_lo) THEN
        SIG(1) = SXX(iel)
        SIG(2) = SYY(iel)
        SIG(3) = SXY(iel)
        SIG(4) = SZZ(iel)
      ELSE
        SXX(iel) = SIG(1)
        SYY(iel) = SIG(2)
        SXY(iel) = SIG(3)
        SZZ(iel) = SIG(4)
      END IF
!
      RETURN
!
      END SUBROUTINE GLOLOC
!
!
! ......................................................................
! .... INITOP ..........................................................
! ......................................................................
!     initialize output files
! ......................................................................
      SUBROUTINE INITOP (fpath)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fpath        ! input file path
      INTEGER(ik) :: currtime(8)    ! for printing analysis date/time
      CHARACTER(LEN=3) :: phsnum   ! string denoting phase number
      INTEGER(ik) :: i, i1,i2, mindex
!
!     write phase number to string
      WRITE(phsnum,"(I3.3)") IPHASE
!
!     open output files
      OPEN(op,  FILE=fpath(1:LEN(fpath)-1)//".OP"//phsnum)
      OPEN(hs,  FILE=fpath(1:LEN(fpath)-1)//".HS"//phsnum)
      OPEN(ld,  FILE=fpath(1:LEN(fpath)-1)//".LD"//phsnum)
      OPEN(uv,  FILE=fpath(1:LEN(fpath)-1)//".UV"//phsnum)
      OPEN(st,  FILE=fpath(1:LEN(fpath)-1)//".ST"//phsnum)
      OPEN(nd,  FILE=fpath(1:LEN(fpath)-1)//".ND"//phsnum)
      OPEN(el,  FILE=fpath(1:LEN(fpath)-1)//".EL"//phsnum)
!
!     write output file headers
      CALL DATE_AND_TIME(VALUES=currtime)
      WRITE(op,100) NNODEL,
     +              fpath(1:LEN(fpath)-1),
     +              currtime(2), currtime(3), currtime(1),
     +              currtime(5:8)
      WRITE(hs,100) NNODEL,
     +              fpath(1:LEN(fpath)-1),
     +              currtime(2), currtime(3), currtime(1),
     +              currtime(5:8)
!
!     write control data to output file
      WRITE(op,101) ANTYPE, NMTL, NNOD, NDIM, NVAR,
     +              NEL, NNODEL, NVEL, NELT, NNODELT, NVELT,
     +              NSTEP(IPHASE), NITER(IPHASE), NPRINT(IPHASE),
     +              IPRINT, GFACT(IPHASE)
!
!
!     *********************************
!     ********* MATERIAL DATA *********
!     *********************************
      WRITE(op,140)   ! material data header
      DO i = 1,NMTL
        WRITE(op,141) i, GRR(i),
     +                ASIN(PHI(i))*radTOdeg,
     +                COH(i)/COS(ASIN(PHI(i))),
     +                ASIN(PSI(i))*radTOdeg,
     +                EMOD(i), NU(i)
      END DO
!
!
!     *********************************
!     *********** NODE DATA ***********
!     *********************************
      WRITE(op,110);  WRITE(op,111) ! node data headers
      DO i = 1,NNOD
        i2 = NVAR*i     ! indices for fixity/node numbering vector
        i1 = i2-1
        WRITE(op,112) i,
     +                COORDS(:,i),
     +                IXC(i1:i2),
     +                PLOADC(i1:i2)
      END DO
!
!
!     *********************************
!     ********* ELEMENT DATA **********
!     *********************************
      mindex = NNODEL+IPHASE
      WRITE(op,120);  WRITE(op,121) ! element data headers
      DO i = 1,NEL
        WRITE(op,122) i, ICO(1:NNODEL,i), ICO(mindex,i),
     +                    AREA(i), CENT(:,i)
      END DO
      WRITE(op,130) 
      WRITE(op,131) NNET(IPHASE), LBAND(IPHASE) ! write stiffness matrix stats to output
!
!
!     *********************************
!     ********* TRACTION DATA *********
!     *********************************
      WRITE(op,150);  WRITE(op,151) ! traction element headers
      DO i = 1,NELT
        WRITE(op,152) i, ICOT(:,i), TNFC(:,i), TSFC(:,i)
      END DO
!
!
!     *********************************
!     ********** LOAD VECTOR **********
!     *********************************
      DO i = 1,NNET(IPHASE)
        WRITE(ld,*) GLOADC(i)
      END DO
!
!
!     *********************************
!     ****** OUTPUT FILE FORMATS ******
!     *********************************
!     header
  100 FORMAT(/, '====================================================',
     +       /, 6X, I1, '-NODED FINITE ELEMENT STRESS ANALYSIS',
     +       /, '====================================================',
     +       //, 'FILE: ', A,
     +       /, 'DATE: ', I2, '/', I2, '/', I4,
     +       /, 'TIME: ', I2, ':', I2, ':', I2, '.', I3, // )
!
!     control information
  101 FORMAT(/, 1X, 'type of analysis ................ANTYPE = ', A,
     +       /, 1X, '# of material types .............. NMTL = ', I7,
     +       /, 1X, '# of nodes ....................... NNOD = ', I7,
     +       /, 1X, '# of coordinate dimensions ....... NDIM = ', I7,
     +       /, 1X, '# of dofs/node ................... NVAR = ', I7,
     +       /, 1X, '# of body elements ................ NEL = ', I7,
     +       /, 1X, '# of nodes/element ............. NNODEL = ', I7,
     +       /, 1X, '# of dofs/element ................ NVEL = ', I7,
     +       /, 1X, '# of traction elements ........... NELT = ', I7,
     +       /, 1X, '# of nodes/traction element ... NNODELT = ', I7,
     +       /, 1X, '# of dofs/traction element ...... NVELT = ', I7,
     +       /, 1X, '# of load steps ................. NSTEP = ', I7,
     +       /, 1X, '# of iterations/load step ....... NITER = ', I7,
     +       /, 1X, '# of print lines ............... NPRINT = ', I7,
     +       /, 1X, 'output node number ............. IPRINT = ', I7,
     +       /, 1X, 'gravity factor .................. GFACT = ', E12.5,
     +       / )
!
!     node data
  110 FORMAT(//,
     +  '===================== NODE INFORMATION =====================')
  111 FORMAT(/, 3X, 'NODE', 8X, 'COORDS (X,Y,..)', 8X, 'FIX (U,V,..)',
     +        8X, 'PLOADS (U,V,..)', /)
  112 FORMAT(1X, I5, 5X, F10.3, 2X, F10.3, 5X, 2I5,
     +        5X, F10.3, 2X, F10.3)
!
!     element data
  120 FORMAT(//,
     +  '================= BODY ELEMENT INFORMATION =================')
  121 FORMAT(/, 3X, 'ELEMENT', 9X, 'NODES', 9X, 'MTL', 8X, 'AREA',
     +        8X, 'CENTROID (X,Y,..)', /)
  122 FORMAT(1X, I5, 5X, 3I5, 5X, I3, 5X, E12.4, 5X, 2F10.3)
!
!     stiffness matrix statistics (packed band storage)
  130 FORMAT(//,
     +  '=============== STIFFNESS MATRIX INFORMATION ===============')
  131 FORMAT( /, 1X, '# of degrees of freedom ... NNET = ', I7,
     +        /, 1X, '# of codiagonal bands .... LBAND = ', I7)
!
  140 FORMAT(//,
     +  '==================== MATERIAL PROPERTIES ===================')
  141 FORMAT(//, 'MATERIAL SET', I5,
     +        /, '*************************',
     +        /, 'unit weight ............... GRR = ', E12.5,
     +        /, 'internal friction angle ... PHI = ', E12.5,
     +        /, 'cohesion .................. COH = ', E12.5,
     +        /, 'dilatancy angle ........... PSI = ', E12.5,
     +        /, 'elastic modulus .......... EMOD = ', E12.5,
     +       /, 'poisson''s ratio ............ NU = ', E12.5  )
!
  150 FORMAT(//,
     +  '=============== TRACTION ELEMENT INFORMATION ===============')
  151 FORMAT(/, 3X, 'ELEMENT', 6X, 'NODES', 15X, 'TNF', 20X, 'TSF', /)
  152 FORMAT(1X, I5, 5X, 2I5, 5X, 2E12.4, 5X, 2E12.4)
!
      RETURN
!
      END SUBROUTINE INITOP
!
!
! ......................................................................
! .... OUTPUT ..........................................................
! ......................................................................
!     write analysis results (displacements and stresses) to file
! ......................................................................
      SUBROUTINE OUTPUT (factor)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: factor    ! final load factor
      REAL(dk) :: lcoords(NDIM,NNODEL), ldisp(NVAR), sig(4)    ! local coords and stresses
      INTEGER(ik) :: inod, j, i1, i2, ii, iel, mtype, mindex   ! loop vars and material type
!
!     write final load factor for this loading sequence
      WRITE(op,100) factor
!
!     write node displacements to output
      WRITE(op,110)
      DO inod = 1,NNOD
        i2 = NVAR*inod;  i1 = i2-NVAR+1; ii = 1
        DO j = i1,i2
          ldisp(ii) = 0.0D0
          IF (IXC(j) .NE. 0) ldisp(ii) = TDISPC(IXC(j))
          ii = ii+1
        END DO
        WRITE(op,111) inod, COORDS(:,inod), ldisp(:)
      END DO
!
!     write element stresses to output
      WRITE(op,120)
      DO iel = 1,NEL
        CALL LOCAL(iel, lcoords, mtype)
        IF (mtype .EQ. 0) CYCLE   ! skip if element is inactive
        CALL GLOLOC(SXX,SYY,SXY,SZZ, sig, iel, .TRUE.)
        WRITE(op,121) iel, CENT(:,iel), IPL(iel), sig(:)
      END DO
!
!     write displacement and stress vectors
!     (for reloading by subsequent phases)
      DO j = 1,NNET(IPHASE)
        WRITE(uv,*) TDISPC(j)
        WRITE(st,*) STRC(j)
      END DO
!
!     write element stresses to ST file
!     (for reloading by subsequent phases)
      mindex = NNODEL + IPHASE ! material type index in connectivity array
      DO j = 1,NEL
        IF (ICO(mindex,j) .NE. 0) THEN    ! only write for elements that were present in curr phase
          WRITE(st,*) SXX(j),SYY(j),SXY(j),SZZ(j),IPL(j),EVOLB(j)
        END IF
      END DO
!
!     format statements
  100 FORMAT( //, "OUTPUT FOR LOAD FACTOR = ", E13.5)
  110 FORMAT( /, "NODE DISPLACEMENTS", /, "**********************",
     +        //, 1X, "INOD", 12X , "COORDS", 24X, "DISP", /)
  111 FORMAT(I5, 4E15.6)
!
  120 FORMAT( /, "MATERIAL POINT STRESSES", /, "**********************",
     +        //, 2X, "IEL", 11X, "LOCATION", 13X, "IPL", 12X,
     +        "S11", 12X, "S22", 12X, "S12", 12X, "S33", //)
  121 FORMAT(I5, 2E15.6, I5, 4E15.6)
!
      RETURN
!
      END SUBROUTINE OUTPUT
!
!
! ......................................................................
! .... SMOOTH ..........................................................
! ......................................................................
!     write smoothed results to nod and ele files
! ......................................................................
      SUBROUTINE SMOOTH ()
!
      IMPLICIT NONE
!
      REAL(dk) :: p,q, d, ldisp(NVAR), larea, s1,s2, theta, st(NNODEL)
      REAL(dk), DIMENSION(NNODEL) :: eXX,eYY,eXY,eZZ,eFBAR
      REAL(dk), DIMENSION(NNOD) :: pXX,pYY,pXY,pZZ,pFBAR
      INTEGER(ik) :: inod, iel, ivar, ii, mindex
!
!     initialize
      DIA(:) = 0.0D0
      pXX(:) = 0.0D0
      pYY(:) = 0.0D0
      pXY(:) = 0.0D0
      pZZ(:) = 0.0D0
      pFBAR(:) = 0.0D0
!
!     map element areas and stresses to nodes
      WRITE(el,10)   ! write results header
      mindex = NNODEL + IPHASE
      DO iel = 1,NEL
!
        LJ(1:NNODEL) = ICO(1:NNODEL, iel)   ! get local connectivity
        LJ(NNODEL+1) = ICO(mindex, iel)
        larea = AREA(iel) / NNODEL          ! get local area
!
!       local to global mapping
        st(:) = larea;            CALL MAPLD(DIA,st,NNODEL)
        eXX(:) = larea*SXX(iel);  CALL MAPLD(pXX,eXX,NNODEL)
        eYY(:) = larea*SYY(iel);  CALL MAPLD(pYY,eYY,NNODEL)
        eXY(:) = larea*SXY(iel);  CALL MAPLD(pXY,eXY,NNODEL)
        eZZ(:) = larea*SZZ(iel);  CALL MAPLD(pZZ,eZZ,NNODEL)
        eFBAR(:) = larea*FBAR(iel);  CALL MAPLD(pFBAR,eFBAR,NNODEL)
!
!       compute stress invariants
        p = 0.5D0*(SXX(iel) + SYY(iel))
        q = SQRT(0.25D0*(SXX(iel) - SYY(iel))**2 + SXY(iel)**2)
        s1 = p+q
        s2 = p-q
        d = SXX(iel) - SYY(iel) + 1.0D-14
!
!       compute angle of shear plane
        IF (ABS(d/SQRT(p**2 + q**2)) .LT. 1.0D-5) THEN
          IF (SXY(iel) .GE. 0.0D0) THEN
            theta = -45.0D0
          ELSE
            theta = 45.0D0
          END IF
        ELSE
          theta = (90.0D0/PI) * ATAN(2.0D0*SXY(iel)/d)
        END IF
!
!       write to element data file
        WRITE(el,11) iel, LJ(1:NNODEL+1), IPL(iel),
     +                SXX(iel), SYY(iel), SXY(iel), SZZ(iel), FBAR(iel),
     +                p, q, s1, s2, theta
!
      END DO
!
!     compute smoothed values at nodes
      pXX(:) = pXX(:) / DIA(:)
      pYY(:) = pYY(:) / DIA(:)
      pXY(:) = pXY(:) / DIA(:)
      pZZ(:) = pZZ(:) / DIA(:)
      pFBAR(:) = pFBAR(:) / DIA(:)
      WRITE(nd,20)   ! write results header
      DO inod = 1,NNOD
!
!       compute stress invariants
        p = 0.5D0*(pXX(inod)+pYY(inod))
        q = SQRT(0.25D0*(pXX(inod)-pYY(inod))**2 + pXY(inod)**2)
        s1 = p+q
        s2 = p-q
        d = pXX(inod) - pYY(inod) + 1.0D-14
!
!       compute angle of shear plane
        IF (ABS(d/SQRT(p**2 + q**2)) .LT. 1.0D-5) THEN
          IF (pXY(inod) .GE. 0.0D0) THEN
            theta = -45.0D0
          ELSE
            theta = 45.0D0
          END IF
        ELSE
          theta = (90.0D0/PI) * ATAN(2.0D0*pXY(inod)/d)
        END IF
!
!       get node displacements
        ldisp(:) = 0.0D0
        DO ivar = 1,NVAR
          ii = inod*NVAR - NVAR + ivar
          IF (IXC(ii) .NE. 0) ldisp(ivar) = TDISPC(IXC(ii))
        END DO
!
!       write to node data file
        WRITE(nd,21) inod, COORDS(:,inod), ldisp(:),
     +                pXX(inod), pYY(inod), pXY(inod), pZZ(inod),
     +                pFBAR(inod), p, q, s1, s2, theta
!
      END DO
!
!     format statements
   10 FORMAT(//,
     +        2X, "IEL", 8X, "ICO", 6X, "MTL", 2X, "IPL",
     +        12X, "SXX", 12X, "SYY", 12X, "SXY", 12X, "SZZ",
     +        11X, "FBAR", 14X, "p", 14X, "q", 13X, "s1",
     +        13X, "s2", 10X, "theta", /)
   11 FORMAT(I5, 5I5, 10E15.6)
!
   20 FORMAT(//,
     +        1X, "INOD", 8X, "COORDS (X,Y,..)", 16X, "DISP (U,V,..)",
     +        20X, "SXX", 12X, "SYY", 12X, "SXY", 12X, "SZZ",
     +        11X, "FBAR", 14X, "p", 14X, "q", 13X, "s1",
     +        13X, "s2", 10X, "theta", /)
   21 FORMAT(I5, 2E15.6, 2E15.6, 10E15.6)
!
      RETURN
!
      END SUBROUTINE SMOOTH
!
!
      END MODULE GCONTROL
!
!
! **********************************************************************
! **** FEUTILITY *******************************************************
! **********************************************************************
!     finite element solver utility functions
! **********************************************************************
      MODULE FEUTILITY
      USE GCONTROL      ! stores global information and uses required modules
!
!$    USE OMP_LIB       ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
!
!
      CONTAINS
!
!
! ......................................................................
! .... LOAD ............................................................
! ......................................................................
!     builds load vector for current phase
! ......................................................................
      SUBROUTINE LOAD ()
!
      IMPLICIT NONE
!
      REAL(dk) :: lcoords(NDIM,NNODEL), lcoordsT(NDIM,NNODELT)  ! local coords
      REAL(dk) :: eload(NVEL), eloadT(NVELT)  ! element load vec
      INTEGER(ik) :: mtype            ! element material type
      INTEGER(ik) :: inod, ivar, ii, iel  ! loop vars
!
!     gravity loads
      DO iel = 1,NEL  ! body elements
        CALL LOCAL(iel, lcoords, mtype)   ! get coords and material of element
        IF (mtype .EQ. 0) CYCLE   ! skip if element is inactive
        eload(:) = 0.0D0                  ! initialize element load vec
        eload(2:NVEL:NVAR) = -GRR(mtype)*AREA(iel) / NNODEL ! distribute grav load
        CALL MAPLD(GLOADC, eload, NVEL)    ! insert into global load vec
      END DO  ! body elements
!
!     point loads and tractions (if present)
      DO inod = 1,NNOD    ! point loads at nodes
        DO ivar = 1,NVAR    ! dofs
          ii = inod*NVAR - NVAR + ivar    ! get dof index
          IF (IXC(ii) .NE. 0) THEN
            GLOADC(IXC(ii)) = GLOADC(IXC(ii))  ! increment load
     +                      + PLOADC(NVAR*(inod-1) + ivar)
          END IF
        END DO  ! dofs
      END DO  ! point loads
      DO iel = 1,NELT   ! traction elements
        CALL LOCALT(iel, lcoordsT)      ! get coords of traction element
        CALL TRACT(eloadT, lcoordsT, TNFC(:,iel), TSFC(:,iel)) ! get traction load vec
        CALL MAPLD(GLOADC, eloadT, NVELT)  ! insert into global load vec
      END DO  ! traction elements
!
      RETURN
!
      END SUBROUTINE LOAD
!
!
! ......................................................................
! .... TRACT ...........................................................
! ......................................................................
!     sets up traction element load vector
! ......................................................................
      SUBROUTINE TRACT (eloadT, lcoordsT, tnf, tsf)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: lcoordsT(:,:), tnf(:), tsf(:)   ! local coords and loads
      REAL(dk), INTENT(OUT) :: eloadT(:)                      ! element load vec
      REAL(dk), PARAMETER :: s(2) = (/-0.577350269190, 0.577350269190/) ! Gaussian quad points
      REAL(dk), PARAMETER :: w(2) = (/ 1.0D0, 1.0D0 /)                  ! Gaussian quad wts
      REAL(dk) :: en(2), ens(2), tn, ts, tx, ty, dxs, dys   ! increment vals
      INTEGER(ik) :: i
!
      eloadT(:) = 0.0D0     ! initialize element load vec
!
!     numerical integration of element load vec
      DO i = 1,NNODELT
        CALL ESHAPET(en,ens,s(i))     ! compute traction element shape functions
        dxs = DOT_PRODUCT(ens,lcoordsT(1,:))
        dys = DOT_PRODUCT(ens,lcoordsT(2,:))   ! map gen coords to act coords
        tn = DOT_PRODUCT(en,tnf)
        ts = DOT_PRODUCT(en,tsf)    ! map gen loads to act loads
        tx =  dys*tn + dxs*ts
        ty = -dxs*tn + dys*ts   ! rotate to global coords
!
!       increment element load vec
        eloadT(1:NVELT:NVAR) = eloadT(1:NVELT:NVAR) + en(:)*tx*w(i)
        eloadT(2:NVELT:NVAR) = eloadT(2:NVELT:NVAR) + en(:)*ty*w(i)
      END DO
!
      RETURN
!
      END SUBROUTINE TRACT
!
!
! ......................................................................
! .... ESHAPET .........................................................
! ......................................................................
!     computes shape functions for traction elements
! ......................................................................
      SUBROUTINE ESHAPET (en, ens, s)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: s
      REAL(dk), INTENT(OUT) :: en(:), ens(:)
!
      en(1) = 0.5D0*(1.0D0 - s)
      en(2) = 0.5D0*(1.0D0 + s)
      ens(1) = -0.5D0
      ens(2) =  0.5D0
!
      RETURN
!
      END SUBROUTINE ESHAPET
!
!
! ......................................................................
! .... STFMAT ..........................................................
! ......................................................................
!     builds the global stiffness matrix
! ......................................................................
      SUBROUTINE STFMAT (rerr)
!
      IMPLICIT NONE
!
      INTEGER(ik), INTENT(OUT) :: rerr      ! error return code, == 0 successful return, /= 0 failed
      REAL(dk) :: lcoords(NDIM,NNODEL)      ! local coords
      INTEGER(ik) :: iel, mtype             ! element number and material type
      INTEGER(ik) :: ierr                   ! error code
      INTEGER(ik) :: nnetC, lbandC, hbwC
!
!     initialize return code and matrix parameters
      rerr = 0
      nnetC = NNET(IPHASE)
      lbandC = LBAND(IPHASE)
      hbwC = HBW(IPHASE)
!
!     form global stiffness matrix (packed banded storage)
      DO iel = 1,NEL
        CALL LOCAL(iel, lcoords, mtype)
        IF (mtype .NE. 0) THEN   ! check if element is active
          CALL STIFF(ESTIF, lcoords, mtype, AREA(iel))
          CALL MAPST(GSTIF, ESTIF)
        END IF
      END DO
!
!     print packed stiff mat (testing)
      WRITE(er,*)
      WRITE(er,*) 'GSTIF'
      DO iel = 1,HBW(IPHASE)
        WRITE(er,*) GSTIF(iel,:)
      END DO
!
!      WRITE(hs,*)
!
!     perform initial matrix inversion
      fGSTIF(:,:) = GSTIF(:,:)
      CALL DPBTRF('U', nnetC, lbandC, fGSTIF, hbwC, ierr)
      IF (ierr .NE. 0) THEN
        IF (ierr .LT. 0) THEN
          WRITE(er,*) "Error in factorization. Invalid argument ",
     +                    -ierr
        ELSE
          WRITE(er,*) "Error in factorization. Minor ", ierr,
     +                  " is not positive definite."
        END IF
        rerr = ierr
        RETURN
      END IF
!
!     *******TEST SOLVE***********
!     linear elastic under gravity load
      DISP(:) = GLOAD0(:)
      CALL DPBTRS('U',nnetC,lbandC,1,fGSTIF,hbwC,DISP,nnetC,ierr)
      IF (ierr .LT. 0) THEN
        WRITE(er,*) "Problem with solution. Invalid argument ",
     +                    -ierr
        rerr = ierr
        RETURN
      END IF
!
!     print factored packed stiff mat (testing)
!      WRITE(er,*)
!      WRITE(er,*) 'fGSTIF'
!     DO iel = 1,HBW(IPHASE)
!        WRITE(er,*) fGSTIF(iel,:)
!      END DO
!
!      WRITE(hs,*)
!
!     print gravity load vector (testing)
!      WRITE(hs,*) 'GLOAD'
!      WRITE(hs,*) GLOAD(:)
!
!      WRITE(hs,*)
!
!     print gravity load displacements (testing)
!      WRITE(hs,*) 'DISP'
!      WRITE(hs,*) DISP(:)
!
!      WRITE(hs,*)
!
!     print traction load vector (testing)
!      WRITE(hs,*) 'TLOAD'
!      WRITE(hs,*) TLOAD(:)
!
      RETURN
!
      END SUBROUTINE STFMAT
!
!
! ......................................................................
! .... STIFF ...........................................................
! ......................................................................
!     compute stiffness matrix for each element
! ......................................................................
      SUBROUTINE STIFF (ESTIF, lcoords, mtype, area)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: lcoords(:,:), area    ! element coords and area
      INTEGER(ik), INTENT(IN) :: mtype              ! element material type
      REAL(dk), INTENT(OUT) :: ESTIF(:,:)           ! element stiff mat
      REAL(dk) :: D(3,3), B(3,NVEL)         ! constitutive and kinematic matrices
!
      ESTIF(:,:) = 0.0D0    ! initialize element stiff mat
      CALL DMATRX(D, EMOD(mtype), NU(mtype))   ! get constitutive matrix
      CALL BMATRX(B, lcoords, area)            ! get kinematic matrix
      ESTIF = area*MATMUL(TRANSPOSE(B),MATMUL(D,B)) ! compute element stiffness
!
      RETURN
!
      END SUBROUTINE STIFF
!
!
! ......................................................................
! .... DMATRX ..........................................................
! ......................................................................
!     compute constitutive matrix for each element
! ......................................................................
      SUBROUTINE DMATRX (D, emod, nu)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: emod, nu    ! elastic modulus and poisson's ratio
      REAL(dk), INTENT(OUT) :: D(:,:)     ! constitutive matrix
      REAL(dk) :: emod1, nu1              ! for calculation
!
      D(:,:) = 0.0D0    ! intialize constitutive matrix
!
      emod1 = emod / (1.0D0 - nu*nu)
      nu1 = nu / (1.0D0 - nu)
!
      D(1,1) = emod1 / (1.0D0 - nu1*nu1); D(1,2) = nu1*D(1,1)
      D(2,1) = D(1,2);                    D(2,2) = D(1,1)
      D(3,3) = 0.5D0*(1-nu1)*D(1,1)
!
      RETURN
!
      END SUBROUTINE DMATRX
!
!
! ......................................................................
! .... BMATRX ..........................................................
! ......................................................................
!     compute kinematic matrix for each element
! ......................................................................
      SUBROUTINE BMATRX (B, lcoords, area)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: lcoords(:,:), area  ! element coords and area
      REAL(dk), INTENT(OUT) :: B(:,:)             ! kinematic matrix
      REAL(dk) :: invJac     ! for calc efficiency
!
      B(:,:) = 0.0D0    ! intialize kinematic matrix
      invJac = 1.0D0 / (2.0D0*area)  ! compute jacobian
!
      B(1,1) = (lcoords(2,2)-lcoords(2,3)) * invJac
      B(1,3) = (lcoords(2,3)-lcoords(2,1)) * invJac
      B(1,5) = (lcoords(2,1)-lcoords(2,2)) * invJac
!
      B(2,2) = (lcoords(1,3)-lcoords(1,2)) * invJac
      B(2,4) = (lcoords(1,1)-lcoords(1,3)) * invJac
      B(2,6) = (lcoords(1,2)-lcoords(1,1)) * invJac
!
      B(3,2:6:2) = B(1,1:5:2)
      B(3,1:5:2) = B(2,2:6:2)
!
      RETURN
!
      END SUBROUTINE BMATRX
!
!
! ......................................................................
! .... FEASLV ..........................................................
! ......................................................................
!     solve the system given the appropriate load vector
! ......................................................................
      SUBROUTINE FEASLV (load, load0, rerr)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: load(:), load0(:)   ! incremental and base load vecs
      INTEGER(ik), INTENT(OUT) :: rerr      ! error return code, == 0 successful return, /= 0 failed
      REAL(dk) :: dfact, factor     ! factors for load stepping
      REAL(dk) :: error             ! for non-linear solver convergence
      REAL(dk) :: dprint            ! for tracking print point
      INTEGER(ik) :: istep, iter, i ! loop vars for load stepping and non-linear solver
      INTEGER(ik) :: nplast, nten, nfbar  ! count of plastic, tensile, "???SAFE???" points
      INTEGER(ik) :: ierr           ! error code for DPBTRS()
      INTEGER(ik) :: nstepC,niterC,nprintC, nnetC,lbandC,hbwC
!
!     initialize return code and current phase variables
      rerr = 0
      nstepC = NSTEP(IPHASE)
      niterC = NITER(IPHASE)
      nprintC = NPRINT(IPHASE)
      nnetC = NNET(IPHASE)
      lbandC = LBAND(IPHASE)
      hbwC = HBW(IPHASE)
!
!     write output header
      WRITE(hs,10)
!
!     initialize non-linear stepping variables
      dfact = 1.0D0 / DBLE(nstepC)
      factor = 0.0D0
      istep = 0
!
!     load stepping loop
      DO WHILE (istep .LT. nstepC)
!
!       increment load step vars
        istep = istep+1
        factor = factor+dfact
!
!       initialize non-linear solver vars
        iter = 0
        error = 2.0D0*TOLER
!
!       non-linear solver loop
        DO WHILE (iter.LT.niterC .AND. error.GT.TOLER)
!
          iter = iter+1   ! increment iteration counter
!
!         insert residual load into DISP
          CALL RLOAD(DISP,load,STRC,load0,factor)
!
!         solve this iteration (DISP has load on entry, incremental displacements on exit)
          CALL DPBTRS('U',nnetC,lbandC,1,fGSTIF,hbwC,DISP,nnetC,ierr)
!
!         update volumetric strain
          CALL VOLSTR(DISP)
!
!         update displacements, stresses, error, and number of plastic/tensile points
          CALL UPDATE(DISP,TDISPC,STRC,error,nplast,nten,rerr)
!          IF (rerr .NE. 0) RETURN
!
!         print an output line
          IF (CEILING(DBLE(istep)/nprintC)*nprintC.EQ.istep
     +              .OR. istep.EQ.nstepC) THEN
!
!           count error points
            nfbar = 0
            DO i = 1,NEL
              IF (FBAR(i) .GT. 1.1) nfbar = nfbar+1
            END DO
!
!           get displacement of print point
            dprint = 0.0D0
            IF (IXC(NVAR*IPRINT) .GT. 0) THEN
              dprint = TDISPC(IXC(NVAR*IPRINT))
            END IF
!
!           write data to load history file
            WRITE(hs,11) istep,iter,nplast,nten,
     +                    nfbar,error,factor,dprint
!
          END IF
!
!         check for convergence (stop computation if failed)
          IF (iter.GE.niterC .AND. error.GT.TOLER) THEN
            WRITE(hs,*) '*** FAILED TO CONVERGE ***'
            rerr = -1
            CALL OUTPUT(factor)
            CALL SMOOTH()
            RETURN
          END IF
!
        END DO  ! non-linear solver
!
      END DO  ! load stepping
!
!     print output
      CALL OUTPUT(factor)
      CALL SMOOTH()
!
!     format statements
  10  FORMAT(/,3X,'STEP',3X,'ITER',1X,'NPLAST',3X,'NTEN',2X,'NFBAR',
     +              7X,'ERROR',6X,'FACTOR',6X,'D',/)
  11  FORMAT(5I7, 2X, 3E12.4)   ! for load step history print line
!
      RETURN
!
      END SUBROUTINE FEASLV
!
!
! ......................................................................
! .... RLOAD ...........................................................
! ......................................................................
!     get residual load vector
! ......................................................................
      SUBROUTINE RLOAD(disp,load,str,load0,factor)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(OUT) :: disp(:)
      REAL(dk), INTENT(IN) :: load(:), str(:), load0(:), factor
!
      disp(:) = factor*load(:) + load0(:) - str(:)
!
      RETURN
!
      END SUBROUTINE RLOAD
!
!
! ......................................................................
! .... VOLSTR ..........................................................
! ......................................................................
!     compute volumetric strains
! ......................................................................
      SUBROUTINE VOLSTR (disp)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: disp(:)
      REAL(dk) :: lcoords(NDIM,NNODEL), ldisp(nvel), larea   ! local coords and displacements
      REAL(dk) :: B(3,NVEL), ev, sumEV     ! for computing element vol strain
      INTEGER(ik) :: iel, mtype, mindex     ! loop var and material type
!
!     initialize nodal volumetric strain
      DIA(:)    = 0.0D0   ! nodal area of influence
      EVOL(:)   = 0.0D0
!      EVOL0(:)  = 0.0D0
!
!     initialize material type index
      mindex = NNODEL + IPHASE
!
!     compute volumetric strain at nodes
      DO iel = 1,NEL
!
!       get local area, coords, and displacements
        CALL LOCAL(iel, lcoords, mtype)
        IF (mtype .EQ. 0) CYCLE   ! skip the element if it is inactive
        larea = AREA(iel)
        WHERE (LJ .GT. 0)
          ldisp = disp(LJ)
        ELSEWHERE
          ldisp = 0.0D0
        END WHERE
!
!       get B matrix (kinematic relationship, disp -> strain)
        CALL BMATRX(B, lcoords, larea)
!
!       get element connectivity (efficiency, avoid multiple access to ICO)
        LJ(1:NNODEL) = ICO(1:NNODEL,iel)
!
!       increment vol strain and influence area
        ev = DOT_PRODUCT(B(1,:),ldisp(:)) + DOT_PRODUCT(B(2,:),ldisp(:))
        EVOL(LJ(1:NNODEL)) = EVOL(LJ(1:NNODEL)) + ev*area
        DIA(LJ(1:NNODEL)) = DIA(LJ(1:NNODEL)) + area
!
      END DO
!
!     save volumetric strains for iteration
!      EVOLi(:) = EVOL(:)
!
!     divide weighted strain by influence area weight
      EVOL(:) = EVOL(:) / DIA(:)
!
!     iterate to reduce lumping
!      EVOL0(:) = EVOL(:)
!      EVOL(:) = EVOLi(:)
!      DO iel = 1,NEL
!        IF (ICO(mindex,iel) .EQ. 0) CYCLE
!        LJ(1:NNODEL) = ICO(1:NNODEL,iel)
!        larea = AREA(iel) / 12.0D0
!        sumEV = SUM(EVOL0(LJ(1:NNODEL)))
!        EVOL(LJ(1:NNODEL)) = EVOL(LJ(1:NNODEL)) +
!     +                          larea*(sumEV + EVOL0(LJ(1:NNODEL)))
!      END DO
!
!     update to corrected vol strains
!      EVOL(:) = EVOL0(:) + (EVOLi - EVOL(:))/DIA(:)
!
!     update global volumetric strain
      DO iel = 1,NEL
        IF (ICO(mindex,iel) .EQ. 0) CYCLE
        LJ(1:NNODEL) = ICO(1:NNODEL,iel)
        EVOLB(iel) = SUM(EVOL(LJ(1:NNODEL))) / NNODEL
      END DO
!
      RETURN
!
      END SUBROUTINE VOLSTR
!
!
! ......................................................................
! .... UPDATE ..........................................................
! ......................................................................
!     increment displacements, update internal forces, compute error,
!     and count number of plastic points
! ......................................................................
      SUBROUTINE UPDATE (DISP, TDISP, STR, error, nplast, nten, rerr)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: DISP(:)         ! incremental displacements
      REAL(dk), INTENT(INOUT) :: TDISP(:)     ! total displacements
      REAL(dk), INTENT(OUT) :: STR(:), error  ! internal force and relative err
      INTEGER(ik), INTENT(OUT) :: rerr        ! error code
      INTEGER(ik), INTENT(OUT) :: nplast, nten  ! number of plastic, tensile points
      REAL(dk) :: lcoords(NDIM,NNODEL), larea        ! local coords
      REAL(dk) :: ldisp(NVEL), lstr(NVEL)     ! local displacement and stress increments
      REAL(dk) :: B(3,NVEL), D(3,3), ST(4)    ! kinematic and constitutive matrices
      REAL(dk) :: sig(4), dsig(3), eps(3), evc  ! el stress/strain, vol strain corr
      INTEGER(ik) :: i, iel, mtype
!
!     initialize outputs
      STR(:) = 0.0D0
      nplast = 0
      nten = 0
!
!     update total displacement
      TDISP(:) = TDISP(:) + DISP(:)
!
!     compute error
      error = SQRT(SUM(DISP**2) / (SUM(TDISP**2) + 1.0D-14))
!
!     update internal force (stress)
      DO iel = 1,NEL
!
!       initialize local coords, disp, stress
        CALL LOCAL(iel,lcoords,mtype)
        IF (mtype .EQ. 0) CYCLE   ! skip if element is inactive
        lstr(:) = 0.0D0
        WHERE (LJ .GT. 0)
          ldisp = DISP(LJ)
        ELSEWHERE
          ldisp = 0.0D0
        END WHERE
!
!       get element kinematic, constitutive, local sxx/syy/szz/sxy
        larea = AREA(iel)
        CALL BMATRX(B, lcoords, larea)
        CALL DMATRX(D, EMOD(mtype), NU(mtype))
        CALL GLOLOC(SXX,SYY,SXY,SZZ, sig, iel, .TRUE.)
!
!       compute strain increments
        DO i = 1,3
          eps(i) = DOT_PRODUCT(B(i,:),ldisp(:))
        END DO
        evc = (eps(1)+eps(2) - EVOLB(iel))*ONE_THIRD  ! vol strain correction
        eps(1:2) = eps(1:2) - evc
!
!       compute stress increments
        DO i = 1,3
          dsig(i) = DOT_PRODUCT(D(i,:),eps(:))
        END DO
!
!       update stresses
        sig(1:3) = sig(1:3) + dsig(1:3)
        sig(4) = sig(4) + NU(mtype)*(dsig(1)+dsig(2))
!
!       check for plastic yielding and tensile points
        CALL MOHRC(PHI(mtype), PSI(mtype), COH(mtype), EMOD(mtype),
     +                NU(mtype), sig, nplast, nten, FBAR(iel), IPL(iel),
     +                rerr)
        IF (rerr .NE. 0) WRITE(hs,*) "Element ", iel
!
!       compute increase in internal forces and increment global internal forces
        lstr(:) = (sig(1)*B(1,:) + sig(2)*B(2,:) + sig(3)*B(3,:))*larea
        CALL GLOLOC(SXX,SYY,SXY,SZZ, sig, iel, .FALSE.)
        CALL MAPLD(STR, lstr, NVEL)
!
      END DO
!
      RETURN
!
      END SUBROUTINE UPDATE
!
!
! ......................................................................
! .... MOHRC ...........................................................
! ......................................................................
!     evaluate plastic state according to Mohr-Coulomb failure criterion
! ......................................................................
      SUBROUTINE MOHRC (sphi, spsi, cohs, emod, nu, sig,
     +                      nplast, nten, fbarel, iplel,
     +                      rerr)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: sphi, spsi, cohs, emod, nu    ! material props
      REAL(dk), INTENT(INOUT) :: sig(:)                     !  stress state
      INTEGER(ik), INTENT(OUT) :: nplast, nten    ! count of plastic/tensile points
      INTEGER(ik), INTENT(OUT) :: rerr            ! error return code, == 0 successful return, /= 0 failed
      REAL(dk), INTENT(OUT) :: fbarel           ! mean Mohr-Coulomb criterion
      INTEGER(ik), INTENT(OUT) :: iplel         ! == 0 if point is elastic, == 1 if point is plastic
      REAL(dk) :: gmod, sxe, sstar, tstar       ! shear modulus, stress invariants
      REAL(dk) :: sinalf, cosalf                ! for Mohr-Coulomb model
      REAL(dk) :: sig1,sig2,sig3, sig1T,sig2T,sig3T, sFail  ! principal stresses
      INTEGER(ik) :: isigZZ, iArea              ! switches for stress sorting and plastic failure type
      REAL(dk) :: f21,f32,f31             ! Mohr-Coulomb criteria
      REAL(dk) :: nu1, nu2, nuQ, psiRat, psiMin, psiMet, psiNu  ! for plastic point calculation
      REAL(dk) :: hA, hB, hC              ! for plastic point calculation
      REAL(dk) :: dsp1,dsp2,dsp3, a11,a12, deter, rlam31,rlam32,rlam21  ! for bringing principal stresses back to yield surface
      REAL(dk) :: dsstar,dtstar, dspXX,dspYY,dspXY,dspZZ, tauMax  ! for bringing global stresses back to yield surface
!
!     initialize return code
      rerr = 0
!
!     compute shear modulus
      gmod = 0.5D0 * emod / (1.0D0 + nu)
!
!     compute stress invariants
      sxe = 0.5D0 * (sig(1)-sig(2))
      sstar = 0.5D0 * (sig(1)+sig(2))
      tstar = SQRT(sxe**2 + sig(3)**2)
      sinalf = 0.0D0
      cosalf = 1.0D0
      IF (tstar .GT. 0.0D0) THEN
        sinalf = sig(3) / tstar
        cosalf = sxe / tstar
      END IF
!
!     compute principal stresses
      sig1 = sstar - tstar
      sig2 = sig(4)
      sig3 = sstar + tstar
!
!     sort principal stresses
      sig1T = sig1
      sig2T = sig2
      sig3T = sig3
      isigZZ = 2
      IF (sig2T .GT. sig3T) THEN
        isigZZ = 3
        sig3 = sig2T
        sig2 = sig3T
      ELSE IF (sig2T .LT. sig1T) THEN
        isigZZ = 1
        sig1 = sig2T
        sig2 = sig1T
      END IF
!
!     compute failure surface
      f21 = 0.5D0 * ((sig2-sig1) + (sig2+sig1)*sphi) - cohs
      f32 = 0.5D0 * ((sig3-sig2) + (sig3+sig2)*sphi) - cohs
      f31 = 0.5D0 * ((sig3-sig1) + (sig3+sig1)*sphi) - cohs
      fbarel = -0.5D0*(sig3-sig1) / (0.5D0*(sig3+sig1)*sphi - cohs)
!
!     if point is plastic, bring it back to yield surface
      IF (      f31.GT.0.0D0
     +    .OR.  f32.GT.0.0D0
     +    .OR.  f21.GT.0.0D0) THEN
!
        nplast = nplast + 1     ! increment count of plastic points
        iplel = 1     ! indicate point is plastic
!
!       compute poisson's ratio terms
        nu1 = 1.0D0 - 2.0D0*nu
        nu2 = 1.0D0 + 2.0D0*nu
        nuQ = nu2 / nu1
!
!       compute dilatancy terms
        psiRat = spsi / nu1
        psiMin = gmod * (-1.0D0+psiRat)
        psiMet = gmod * ( 1.0D0+psiRat)
        psiNu  = 2.0D0*gmod*nu*psiRat
!
!       compute yield direction terms
        hA =  ( 1.0D0 - sphi + spsi - sphi*spsi)*sig1 +
     +        (-2.0D0 - 2.0D0/nu1 * sphi*spsi)*sig2 +
     +        ( 1.0D0 - sphi - spsi + nuQ*sphi*spsi)*sig3 +
     +        2.0D0*(1.0D0 + spsi)*cohs
!
        hB = -( 1.0D0 + sphi + spsi + nuQ*sphi*spsi)*sig1 -
     -        (-2.0D0 - 2.0D0/nu1 * sphi*spsi)*sig2 -
     -        ( 1.0D0 + sphi - spsi - sphi*spsi)*sig3 +
     +        2.0D0*(1.0D0 - spsi)*cohs
!
!       select yield type
        IF (hA .LT. 0.0D0) THEN
          iArea = 3                     ! compression yield
        ELSE IF (hB .LT. 0.0D0) THEN
          iArea = 1                     ! tension yield
        ELSE
          iArea = 2                     ! shear yield
        END IF
!
!       compute plastic strain
        SELECT CASE (iArea)
!
          CASE (1)            ! extension
!
            a11 = gmod * (1.0D0 + sphi*spsi/nu1)
            a12 = 0.5D0 * gmod * (1.0D0 + sphi + spsi + nuQ*sphi*spsi)
            deter = a11*a11 - a12*a12
            rlam31 = (f31*a11 - f32*a12) / deter
            rlam32 = (f32*a11 - f31*a12) / deter
            rlam21 = 0.0D0
            dsp1 = rlam31*psiMin  + rlam32*psiNu
            dsp2 = rlam31*psiNu   + rlam32*psiMin
            dsp3 = rlam31*psiMet  + rlam32*psiMet
!
          CASE (2)            ! shear
!
            a11 = gmod * (1.0D0 + sphi*spsi/nu1)
            rlam31 = f31 / a11
            rlam32 = 0.0D0
            rlam21 = 0.0D0
            dsp1 = rlam31*psiMin
            dsp2 = rlam31*psiNu
            dsp3 = rlam31*psiMet
!
          CASE (3)            ! compression
!
            a11 = gmod * (1.0D0 + sphi*spsi/nu1)
            a12 = 0.5D0 * gmod * (1.0D0 - sphi - spsi + nuQ*sphi*spsi)
            deter = a11*a11 - a12*a12
            rlam31 = (f31*a11 - f21*a12) / deter
            rlam32 = 0.0D0
            rlam21 = (f21*a11 - f31*a12) / deter
            dsp1 = rlam31*psiMin  + rlam21*psiMin
            dsp2 = rlam31*psiNu   + rlam21*psiMet
            dsp3 = rlam31*psiMet  + rlam21*psiNu
!
        END SELECT
!
!       increment tensile point count
        IF (sig3 .GT. 0.0D0) nten = nten+1
!
!       test apex points
        IF (sphi .GT. 1.0D-6) THEN
          sFail = cohs/sphi
          hC = sig1+sig2+sig3 - (dsp1+dsp2+dsp3) - 3.0D0*sFail
          IF (hC .GT. 0.0D0) THEN
            dsp1 = sig1-sFail
            dsp2 = sig2-sFail
            dsp3 = sig3-sFail
          END IF
        END IF
!
!       compute Cartesian stress components
        SELECT CASE (isigZZ)
          CASE (1)
            dtstar = 0.5D0 * (dsp3-dsp2)
            dsstar = 0.5D0 * (dsp3+dsp2)
            dspZZ = dsp1
          CASE (2)
            dtstar = 0.5D0 * (dsp3-dsp1)
            dsstar = 0.5D0 * (dsp3+dsp1)
            dspZZ = dsp2
          CASE (3)
            dtstar = 0.5D0 * (dsp2-dsp1)
            dsstar = 0.5D0 * (dsp2+dsp1)
            dspZZ = dsp3
        END SELECT
!
        dspXX = (dsstar + dtstar*cosalf)
        dspYY = (dsstar - dtstar*cosalf)
        dspXY = dtstar*sinalf
        sig(1) = sig(1) - dspXX
        sig(2) = sig(2) - dspYY
        sig(3) = sig(3) - dspXY
        sig(4) = sig(4) - dspZZ
!
!       compute tauMax to check accuracy
        tauMax = 0.5D0 * (sig3 - dsp3 - sig1 + dsp1)
        IF (tauMax .LE. -1.0D-6) THEN
          WRITE(hs,*) "Warning: tauMax is negative !!!"
          rerr = -1
        END IF
!
      ELSE  ! elastic point
!
        iplel = 0   ! indicate point is elastic
!
      END IF  ! plastic point
!
      RETURN
!
      END SUBROUTINE MOHRC
!
!
      END MODULE FEUTILITY
!
!
! **********************************************************************
! **** MAIN PROGRAM ****************************************************
! **********************************************************************
!     Non-Linear Finite Element Analysis
!     Multiple Analysis Phases (e.g. staged construction, load stepping)
!     3-Noded Triangular Body Elements (Linear Interpolation)
!     2-Noded Linear Traction Elements (Linear Interpolation)
!     Mohr-Coulomb Failure Criterion (Linear Elastic, Perfectly Plastic)
! **********************************************************************
      SUBROUTINE SLOPEFEA3NODE (fpath)
!      PROGRAM SLOPEFEA3NODE
      USE GCONTROL    ! controls inputs, grid, and elements
      USE FEUTILITY   ! utility functions for FEA solver
!
!$    USE OMP_LIB       ! parallel lib (gfortran: compile with -fopenmp switch)
!
      IMPLICIT NONE
!
      CHARACTER*(*), INTENT(IN) :: fpath    ! path to data files
!      CHARACTER*(*), PARAMETER :: fpath = "testCut "
      INTEGER(ik) :: rerr     ! error code
!
      ANTYPE = "PLANE STRAIN ELASTO-PLASTIC"//
     +          " (MOHR-COULOMB FAILURE CRITERION)"
      CALL INPUT(fpath)     ! initialize data in GCONTROL
!
      DO IPHASE = 1,NPHASE      ! loop through analysis phases
!
!       allocate solution space for current phase and load
!       state from previous phase (as applicable)
        CALL ALLOCPHS(fpath)
!
!       build load vector for current phase
        CALL LOAD()
!
!       build stiffness matrix for current phase
        CALL STFMAT(rerr)
        IF (rerr .NE. 0) THEN
          CALL DEALLOCPHS()
          CALL CLEANUP()
          RETURN
        END IF
!
!       initialize output printing
        CALL INITOP(fpath)
!
!       solve this phase
        GLOADC(:) = GLOADC(:) - GLOAD0(:)
        CALL FEASLV(GLOADC, GLOAD0, rerr)
!
!       deallocate solution space for current phase and
!       close output files
        CALL DEALLOCPHS()
!
      END DO      ! END analysis phases
!
      CALL CLEANUP()
!
      RETURN
!
      END SUBROUTINE SLOPEFEA3NODE
!      END PROGRAM SLOPEFEA3NODE