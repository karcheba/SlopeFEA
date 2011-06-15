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
      MODULE feutility
      USE gcontrol      ! stores global information and uses required modules
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
!     builds gravity and load vectors
! ......................................................................
      SUBROUTINE LOAD (GLOAD, TLOAD)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(OUT) :: GLOAD(:), TLOAD(:)
      REAL(dk) :: lcoords(NDIM,NNODEL), lcoordsT(NDIM,NNODELT)  ! local coords
      REAL(dk) :: eload(NVEL), eloadT(NVELT)  ! element load vec
      INTEGER(ik) :: mtype          ! element material type
      INTEGER(ik) :: iel            ! loop variable
!
!     gravity loads
      GLOAD(:) = 0.0D0
      DO iel = 1,NEL  ! body elements
        CALL LOCAL(iel, lcoords, mtype)   ! get coords and material of element
        eload(:) = 0.0D0                  ! initialize element load vec
        eload(2:NVEL:NVAR) = -GRR(mtype)*AREA(iel) / NNODEL ! distribute grav load
        CALL MAPLD(GLOAD, eload, NVEL)    ! insert into global load vec
      END DO  ! body elements
!
!     tractions (if present)
      TLOAD(:) = 0.0D0
      DO iel = 1,NELT   ! traction elements
        CALL LOCALT(iel, lcoordsT)      ! get coords of traction element
        CALL TRACT(eloadT, lcoordsT, TNF(:,iel), TSF(:,iel)) ! get traction load vec
        CALL MAPLD(TLOAD, eloadT, NVELT)  ! insert into global load vec
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
      REAL(dk) :: en(2), ens(2), tn, ts, tx, ty, dx, dy   ! increment vals
      INTEGER(ik) :: i
!
      eloadT(:) = 0.0D0     ! initialize element load vec
!
!     numerical integration of element load vec
      DO i = 1,NNODELT
        CALL ESHAPET(en,ens,s(i))     ! compute traction element shape functions
        dx = DOT_PRODUCT(ens,lcoordsT(1,:))
        dy = DOT_PRODUCT(ens,lcoordsT(2,:))   ! map gen coords to act coords
        tn = DOT_PRODUCT(en,tnf)
        ts = DOT_PRODUCT(en,tsf)    ! map gen loads to act loads
        tx =  dy*tn + dx*ts
        ty = -dx*tn + dy*ts   ! rotate to global coords
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
      SUBROUTINE STFMAT (GSTIF)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(OUT) :: GSTIF(:,:)   ! global stiff mat
      REAL(dk) :: lcoords(NDIM,NNODEL)      ! local coords
      INTEGER(ik) :: iel, mtype             ! element number and material type
      REAL(dk) :: rcn, forerr, bakerr       ! est cond num, est forward err, est backward err
      INTEGER(ik) :: ierr                   ! error code for DPBSVX()
!
!     form global stiffness matrix (packed banded storage)
      GSTIF(:,:) = 0.0D0
      DO iel = 1,NEL
        CALL LOCAL(iel, lcoords, mtype)
        CALL STIFF(ESTIF, lcoords, mtype, AREA(iel))
        CALL MAPST(GSTIF, ESTIF)
      END DO
!
!     perform initial matrix inversion
      GLOAD0(:) = 1.0D-2 * GLOAD(:)
      CALL DPBSVX(  'N',    ! FACT  = 
     +              'U',    ! UPLO  = 
     +              NNET,   ! N     = 
     +              LBAND,  ! KD    = 
     +              1,      ! NRHS  = 
     +              GSTIF,  ! AB    = 
     +              HBW,    ! LDAB  = 
     +              fGSTIF, ! AFB   = 
     +              HBW,    ! LDAFB = 
     +              'N',    ! EQUED = 
     +              DISP,   ! S     = 
     +              GLOAD0, ! B     = 
     +              NNET,   ! LDB   = 
     +              DISP,   ! X     = 
     +              NNET,   ! LDX   = 
     +              rcn,    ! RCOND = 
     +              forerr, ! FERR  = 
     +              bakerr, ! BERR  = 
     +              fWORK,  ! WORK  = 
     +              fiWORK, ! IWORK = 
     +              ierr )  ! INFO  = 
!
!     print packed stiff mat (testing)
      WRITE(his,*) 'GSTIF'
      DO iel = 1,HBW
        WRITE(his,*) GSTIF(iel,:)
      END DO
!
      WRITE(his,*)
!
!     print factored packed stiff mat (testing)
      WRITE(his,*) 'fGSTIF'
      DO iel = 1,HBW
        WRITE(his,*) fGSTIF(iel,:)
      END DO
!
      WRITE(his,*)
!
!     print gravity load vector (testing)
      WRITE(his,*) 'GLOAD'
      WRITE(his,*) GLOAD(:)
!
      WRITE(his,*)
!
!     print traction load vector (testing)
      WRITE(his,*) 'TLOAD'
      WRITE(his,*) TLOAD(:)
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
      REAL(dk) :: DMAT(3,3), BMAT(3,6)              ! constitutive and kinematic matrices
!
      ESTIF(:,:) = 0.0D0    ! initialize element stiff mat
      CALL DMATRX(DMAT, EMOD(mtype), NU(mtype))   ! get constitutive matrix
      CALL BMATRX(BMAT, lcoords, area)            ! get kinematic matrix
      ESTIF = area*MATMUL(TRANSPOSE(BMAT),MATMUL(DMAT,BMAT)) ! compute element stiffness
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
      REAL(dk) :: invArea     ! for calc efficiency
!
      B(:,:) = 0.0D0    ! intialize kinematic matrix
      invArea = 1.0D0 / area  ! compute 1/area
!
      B(1,1) = (lcoords(2,2)-lcoords(2,3)) * invArea
      B(1,3) = (lcoords(2,3)-lcoords(2,1)) * invArea
      B(1,5) = (lcoords(2,1)-lcoords(2,2)) * invArea
!
      B(2,2) = (lcoords(1,3)-lcoords(1,2)) * invArea
      B(2,4) = (lcoords(1,1)-lcoords(1,3)) * invArea
      B(2,6) = (lcoords(1,2)-lcoords(1,1)) * invArea
!
      B(3,2:6:2) = B(1,1:5:2)
      B(3,1:5:2) = B(2,2:6:2)
!
      RETURN
!
      END SUBROUTINE BMATRX
!
!
      END MODULE feutility