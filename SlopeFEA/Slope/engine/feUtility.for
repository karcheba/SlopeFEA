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
      SUBROUTINE STFMAT (GSTIF)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(OUT) :: GSTIF(:,:)   ! global stiff mat
      REAL(dk) :: lcoords(NDIM,NNODEL)      ! local coords
      INTEGER(ik) :: iel, mtype             ! element number and material type
      INTEGER(ik) :: ierr                   ! error code
!
!     form global stiffness matrix (packed banded storage)
      GSTIF(:,:) = 0.0D0
      DO iel = 1,NEL
        CALL LOCAL(iel, lcoords, mtype)
        CALL STIFF(ESTIF, lcoords, mtype, AREA(iel))
        CALL MAPST(GSTIF, ESTIF)
      END DO
!
!     print packed stiff mat (testing)
!      WRITE(his,*) 'GSTIF'
!      DO iel = 1,HBW
!        WRITE(his,*) GSTIF(iel,:)
!      END DO
!
!      WRITE(his,*)
!
!     perform initial matrix inversion
      fGSTIF(:,:) = GSTIF(:,:)
      CALL DPBTRF('U', NNET, LBAND, fGSTIF, HBW, ierr)
      IF (ierr .NE. 0) THEN
        IF (ierr .LT. 0) THEN
          WRITE(his,*) "Error in factorization. Invalid argument ",
     +                    -ierr
        ELSE
          WRITE(his,*) "Error in factorization. Minor ", ierr,
     +                  " is not positive definite."
        END IF
      END IF
!
!     *******TEST SOLVE***********
!     linear elastic under gravity load
      DISP(:) = GLOAD(:)
      CALL DPBTRS('U', NNET, LBAND, 1, fGSTIF, HBW, DISP, NNET, ierr)
      IF (ierr .LT. 0) THEN
        WRITE(his,*) "Problem with solution. Invalid argument ",
     +                    -ierr
        CALL CLEANUP()
        STOP
      END IF
!
!     print factored packed stiff mat (testing)
!      WRITE(his,*) 'fGSTIF'
!      DO iel = 1,HBW
!        WRITE(his,*) fGSTIF(iel,:)
!      END DO
!
!      WRITE(his,*)
!
!     print gravity load vector (testing)
!      WRITE(his,*) 'GLOAD'
!      WRITE(his,*) GLOAD(:)
!
!      WRITE(his,*)
!
!     print gravity load displacements (testing)
!      WRITE(his,*) 'DISP'
!      WRITE(his,*) DISP(:)
!
!      WRITE(his,*)
!
!     print traction load vector (testing)
!      WRITE(his,*) 'TLOAD'
!      WRITE(his,*) TLOAD(:)
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
      REAL(dk) :: invArea     ! for calc efficiency
!
      B(:,:) = 0.0D0    ! intialize kinematic matrix
      invArea = 2.0D0 / area  ! compute 2/area
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
! ......................................................................
! .... FEASLV ..........................................................
! ......................................................................
!     solve the system given the appropriate load vector
! ......................................................................
      SUBROUTINE FEASLV (load, load0)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: load(:), load0(:)   ! incremental and base load vecs
      REAL(dk) :: dfact, factor     ! factors for load stepping
      REAL(dk) :: error             ! for non-linear solver convergence
      REAL(dk) :: dprint            ! for tracking print point
      INTEGER(ik) :: istep, iter, i ! loop vars for load stepping and non-linear solver
      INTEGER(ik) :: nplast, nten, nfbar  ! count of plastic, tensile, "???SAFE???" points
      INTEGER(ik) :: ierr           ! error code for DPBTRS()
!
!     write outp header
      WRITE(his,10)
!
!     initialize total displacement
      TDISP(:) = 0.0D0
!
!     initialize non-linear stepping variables
      dfact = 1.0D0 / DBLE(NSTEP)
      factor = 0.0D0
      istep = 0
!
!     load stepping loop
      DO WHILE (istep .LT. NSTEP)
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
        DO WHILE (iter.LT.NITER .AND. error.GT.TOLER)
!
          iter = iter+1   ! increment iteration counter
!
!         insert residual load into DISP
          CALL RLOAD(DISP,load,STR,load0,factor)
!
!         solve this iteration (DISP has load on entry, incremental displacements on exit)
          CALL DPBTRS('U',NNET,LBAND,1,fGSTIF,HBW,DISP,NNET,ierr)
!
!         update volumetric strain
          CALL VOLSTR(DISP)
!
!         update displacements, stresses, error, and number of plastic/tensile points
          CALL UPDATE(DISP,TDISP,STR,error,nplast,nten)
!
!         print an outp line
          IF (MOD(NPRINT,ISTEP) .EQ. 0) THEN
!
!           count "???SAFE POINTS???"
            nfbar = 0
            DO i = 1,NEL
              IF (FBAR(i) .GT. 1.1) nfbar = nfbar+1
            END DO
!
!           get displacement of print point
            dprint = 0.0D0
            IF (IX(NVAR*IPRINT) .GT. 0) dprint = TDISP(IX(NVAR*IPRINT))
!
!           write data to load history file
            WRITE(his,11) istep,iter,nplast,nten,
     +                    nfbar,error,factor,dprint
!
          END IF
!
!         check for convergence (stop computation if failed)
          IF (iter.GE.NITER .AND. error.GT.TOLER) THEN
            WRITE(his,*) '*** FAILED TO CONVERGE ***'
            CALL OUTPUT(factor)
!            CALL SMOOTH()
            CALL CLEANUP()
            STOP
          END IF
!
        END DO  ! non-linear solver
!
      END DO  ! load stepping
!
!     print outp
      CALL OUTPUT(factor)
!      CALL SMOOTH()
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
      INTEGER(ik) :: iel, mtype     ! loop var and material type
!
!     initialize nodal volumetric strain
      DIA(:)    = 0.0D0   ! nodal area of influence
      EVOL(:)   = 0.0D0   
      EVOL0(:)  = 0.0D0
!
!     compute volumetric strain at nodes
      DO iel = 1,NEL
!
!       get local area, coords, and displacements
        larea = AREA(iel)
        CALL LOCAL(iel, lcoords, mtype)
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
        EVOL(LJ(1:NNODEL)) = EVOL(LJ(1:NNODEL)) + ev*area*ONE_THIRD
        DIA(LJ(1:NNODEL)) = DIA(LJ(1:NNODEL)) + area*ONE_THIRD
!
      END DO
!
!     save results for iteration and divide by influence area
      EVOLi(:) = EVOL(:)
      EVOL(:) = EVOL(:) / DIA(:)
!
!     iterate to reduce "???LUMPING???"
      EVOL0(:) = EVOL(:)
      EVOL(:) = EVOLi(:)
      DO iel = 1,NEL
        LJ(1:NNODEL) = ICO(1:NNODEL,iel)
        larea = AREA(iel) / 12.0D0
        sumEV = SUM(EVOL0(LJ(1:NNODEL)))
        EVOL(LJ(1:NNODEL)) = EVOL(LJ(1:NNODEL))
     +                + larea*(sumEV + EVOL0(LJ(1:NNODEL)))
      END DO
      EVOL(:) = EVOL0(:) + (EVOLi(:) - EVOL(:)) / DIA(:)
!
!     update global volumetric strain
      DO iel = 1,NEL
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
      SUBROUTINE UPDATE (DISP, TDISP, STR, error, nplast, nten)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: DISP(:)         ! incremental displacements
      REAL(dk), INTENT(INOUT) :: TDISP(:)     ! total displacements
      REAL(dk), INTENT(OUT) :: STR(:), error  ! internal force and relative err
      INTEGER(ik), INTENT(OUT) :: nplast, nten  ! number of plastic, tensile points
      REAL(dk) :: lcoords(NDIM,NNODEL), larea        ! local coords
      REAL(dk) :: ldisp(NVEL), lstr(NVEL)     ! local displacement and stress increments
      REAL(dk) :: B(3,NVEL), D(3,3), ST(4)    ! kinematic and constitutive matrices
      REAL(dk) :: sig(4), dsig(3), eps(3), evc  ! el stress/strain, vol strain corr
      INTEGER(ik) :: i, iel, mtype
!
!     initialize outps
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
        evc = (eps(1)+eps(2) - EVOLB(iel)) / 3.0D0  ! vol strain correction
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
     +                NU(mtype), sig, nplast, nten, FBAR(iel))
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
     +                      nplast, nten, fbarel)
!
      IMPLICIT NONE
!
      REAL(dk), INTENT(IN) :: sphi, spsi, cohs, emod, nu    ! material props
      REAL(dk), INTENT(INOUT) :: sig(:)                     !  stress state
      INTEGER(ik), INTENT(OUT) :: nplast, nten    ! count of plastic/tensile points
      REAL(dk), INTENT(OUT) :: fbarel
      REAL(dk) :: gmod, sxe, sstar, tstar
      REAL(dk) :: sinalf, cosalf
      REAL(dk) :: sig1,sig2,sig3, sig1T,sig2T,sig3T, sFail
      INTEGER(ik) :: isigZZ, iArea
      REAL(dk) :: f21,f32,f31
      REAL(dk) :: nu1, nu2, nuQ, psiRat, psiMin, psiMet, psiNu
      REAL(dk) :: hA, hB, hC
      REAL(dk) :: dsp1,dsp2,dsp3, a11,a12, deter, rlam31,rlam32,rlam21
      REAL(dk) :: dsstar,dtstar, dspXX,dspYY,dspXY,dspZZ, tauMax
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
      IF (tstar > 0.0D0) THEN
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
      fbarel = -0.5D0*(sig3-sig1) / (0.5D0*(sig3+sig1) - cohs)
!
!     if point is plastic, bring it back to yield surface
      IF (f32 .GT. 0.0D0) THEN
!
        nplast = nplast + 1     ! increment count of plastic points
!
!       compute poisson's ratio terms
        nu1 = 1.0D0 - 2.0D0*nu
        nu2 = 1.0D0 + 2.0D0*nu
        nuQ = nu1 / nu2
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
            dsp1 = rlam31*psiMin + rlam32*psiNu
            dsp2 = rlam31*psiNu + rlam32*psiMin
            dsp3 = rlam31*psiMet + rlam32*psiMet
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
            dsp1 = rlam31*psiMin + rlam21*psiMin
            dsp2 = rlam31*psiNu + rlam21*psiMet
            dsp3 = rlam31*psiMet + rlam21*psiNu
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
!     compute Cartesian stress components
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
!     compute tauMax to check accuracy
      tauMax = 0.5D0 * (sig3 - dsp3 - sig1 + dsp1)
      IF (tauMax .LT. -1.0D-6) THEN
        WRITE(his,*) "Error: tauMax is negative !!!"
        STOP
      END IF
!
      END IF  ! plastic point
!
      RETURN
!
      END SUBROUTINE MOHRC
!
!
      END MODULE feutility