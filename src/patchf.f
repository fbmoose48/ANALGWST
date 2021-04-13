C
C      ********************************************************
C      *                                                      *
C      *                   **** PATCHF ****                   *
C      *                                                      *
C      *   THREE-DIMENSIONAL GROUND-WATER SOLUTE TRANSPORT    *
C      *                                                      *
C      *   MODEL FOR A SEMI-INFINITE AQUIFER WITH A FINITE    *
C      *                                                      *
C      *    WIDTH AND HEIGHT. PATCH SOURCE EXTENDING FROM     *
C      *                                                      *
C      *         Y1 TO Y2 AND Z1 TO Z2 LOCATED AT X=0         *
C      *                                                      *
C      *        GROUND-WATER FLOW IN X-DIRECTION ONLY         *
C      *                                                      *
C      *            VERSION CURRENT AS OF 04/01/90            *
C      *              CODE CLEANUP 03/27/96 - RSR             *
C      *                                                      *
C      ********************************************************
C
C
C      Although this program has been used by the U.S. Geological
C      Survey, no warranty, expressed or implied, is made by the USGS
C      as to the accuracy and functioning of the program and related
C      program material, nor shall the fact of distribution constitute
C      any such warranty, and no responsibility is assumed by the USGS
C      in connection therewith.
C
      INCLUDE 'dimens.inc'
      INCLUDE 'pltdat.inc'
      INCLUDE 'units.inc'
C
      INTEGER In, Io
      COMMON /IOUNIT/ In, Io
C
      DOUBLE PRECISION cxy(MAXX,MAXY), x(MAXX), y(MAXY), z(MAXZ), 
     &                 t(MAXT), c0, cn, dk, dx, dy, dz, h, vx, w, y1, 
     &                 y2, z1, z2
      INTEGER it, ix, iy, iz, j, mmax, nmax, np, np1, np2, npage, nt, 
     &        nx, ny, nz, ifx
      CHARACTER*1 ierr(MAXX,MAXY)
C
C---EXTERNALS
      EXTERNAL OFILE, TITLE, CNORMP, PLTNC3, RDXYZT, RDPLOT, WCONH3
C
C     PROGRAM VARIABLES
C
C     C0       SOLUTE CONCENTRATION AT THE INFLOW BOUNDARY [M/L**3]
C     DX       LONGITUDINAL DISPERSION COEFFICIENT [L**2/T]
C     DY       TRANSVERSE (Y-DIRECTION) DISPERSION COEFFICIENT [L**2/T]
C     DZ       TRANSVERSE (Z-DIRECTION) DISPERSION COEFFICIENT [L**2/T]
C     VX       GROUND-WATER VELOCITY IN X-DIRECTION [L/T]
C     DK       FIRST-ORDER SOLUTE DECAY CONSTANT [1/T]
C     X        X-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     Y        Y-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     Z        Z-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     T        TIME AT WHICH CONCENTRATION IS EVALUATED [T]
C     CN       NORMALIZED CONCENTRATION C/C0 [DIMENSIONLESS]
C     CXY      SOLUTE CONCENTRATION C(X,Y,Z,T) [M/L**3]
C     W        AQUIFER WIDTH (AQUIFER EXTENDS FROM Y=0 TO Y=W) [L]
C     H        AQUIFER HEIGHT (AQUIFER EXTENDS FROM Z=0 TO Z=H) [L]
C     Y1       Y-COORDINATE OF LOWER LIMIT OF PATCH SOLUTE SOURCE [L]
C     Y2       Y-COORDINATE OF UPPER LIMIT OF PATCH SOLUTE SOURCE [L]
C     Z1       Z-COORDINATE OF LOWER LIMIT OF PATCH SOLUTE SOURCE [L]
C     Z2       Z-COORDINATE OF UPPER LIMIT OF PATCH SOLUTE SOURCE [L]
C
C     NX       NUMBER OF X-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NY       NUMBER OF Y-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NZ       NUMBER OF Z-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NT       NUMBER OF TIME VALUES AT WHICH SOLUTION IS EVALUATED
C     NMAX     NUMBER OF TERMS USED IN INNER INFINITE SERIES SUMMATION
C     MMAX     NUMBER OF TERMS USED IN OUTER INFINITE SERIES SUMMATION
C
C     IPLT     PLOT CONTROL. IF IPLT>0, CONTOUR MAPS ARE PLOTTED
C     XSCLP    SCALING FACTOR TO CONVERT X TO PLOTTER INCHES
C     YSCLP    SCALING FACTOR TO CONVERT Y TO PLOTTER INCHES
C     DELTA    CONTOUR INCREMENT FOR PLOT. (VALUE BETWEEN 0 AND 1.0)
C
      Versn = 
     & '$Id: patchf.f,v 1.1 1996/04/03 19:45:08 rsregan Exp rsregan $'
      Versn = 
     &    '@(#)PATCHF - Analytical solution for 3-D GW solute transport'
      Versn = 
     &    '@(#)PATCHF - in a finite-width and finite-height system with'
      Versn = 
     &  '@(#)PATCHF - uniform flow and a finite-width and finite-height'
      Versn = '@(#)PATCHF - solute source'
      Versn = '@(#)PATCHF - USGS TWRI, book 3, chap. B7, E.J. Wexler'
      Versn = '@(#)PATCHF - Contact: h2osoft@usgs.gov'
      Versn = '@(#)PATCHF - Version: 1.1x 1996/04/03'
C
C---DEFINE INPUT/OUTPUT FILES AND PRINT TITLE PAGE
      CALL OFILE
      CALL TITLE
      WRITE (Io,9020)
C
C---READ IN MODEL PARAMETERS
      READ (In,9005) nx, ny, nz, nt, nmax, mmax, Iplt
      WRITE (Io,9025) nx, ny, nz, nt, nmax, mmax
      READ (In,9010) Cunits, Vunits, Dunits, Kunits, Lunits, Tunits
      READ (In,9015) c0, vx, dx, dy, dz, dk
      WRITE (Io,9030) c0, Cunits, vx, Vunits, dx, Dunits, dy, Dunits, 
     &                dz, Dunits, dk, Kunits
      READ (In,9015) w, h, y1, y2, z1, z2
      WRITE (Io,9035) w, Lunits, h, Lunits, y1, Lunits, y2, Lunits, z1, 
     &                Lunits, z2, Lunits
      CALL RDXYZT(nx,ny,nz,nt,x,y,z,t)
      IF (Iplt.GT.0) CALL RDPLOT(Cunits,Xsclp,Ysclp,Delta)
C
C---BEGIN TIME LOOP
      DO 60 it = 1, nt
C
C---BEGIN Z LOOP
        DO 50 iz = 1, nz
C
C---BEGIN X LOOP
          DO 20 ix = 1, nx
C
C---CALCULATE NORMALIZED CONCENTRATION FOR ALL Y AT X=X(IX) AND Z=Z(IZ)
            DO 10 iy = 1, ny
              CALL CNORMP(dk,t(it),x(ix),y(iy),z(iz),w,h,y1,y2,z1,z2,dx,
     &                    dy,dz,vx,cn,nmax,mmax,ierr(ix,iy))
              cxy(ix,iy) = c0*cn
   10       CONTINUE
   20     CONTINUE
C
C---PRINT OUT TABLES OF CONCENTRATION VALUES
          npage = 1 + (ny-1)/9
          WRITE (Io,9040) t(it), Tunits, z(iz), Lunits, Lunits
          ifx = 0
          DO 40 np = 1, npage
            np1 = (np-1)*9
            np2 = 9
            IF ((np1+np2).GT.ny) np2 = ny - np1
            CALL WCONH3(ifx,np1,np2,t(it),z(iz),y)
            ifx = 45
            DO 30 ix = 1, nx
              WRITE (Io,9045) x(ix), 
     &                        (cxy(ix,np1+j),ierr(ix,np1+j),j=1,np2)
              CALL WCONH3(ix,np1,np2,t(it),z(iz),y)
   30       CONTINUE
   40     CONTINUE
C
C---PLOT CONTOUR MAPS OF NORMALIZED CONCENTRATION FOR EACH TIME VALUE.
          IF (Iplt.GT.0) CALL PLTNC3(nx,ny,nz,iz,x,y,t(it),z(iz),cxy,c0)
   50   CONTINUE
   60 CONTINUE
      CLOSE (In)
      CLOSE (Io)
      STOP
C
C---FORMAT STATEMENTS
 9005 FORMAT (20I4)
 9010 FORMAT (8A10)
 9015 FORMAT (8F10.0)
 9020 FORMAT (/////,30X,'ANALYTICAL SOLUTION TO THE THREE-DIMENSIONAL',
     &        /,29X,'ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION',/,
     &        31X,'FOR A SEMI-INFINITE AQUIFER OF FINITE WIDTH',/,29X,
     &        'AND HEIGHT WITH A PATCH SOLUTE SOURCE AT X=0.0',////,41X,
     &        'INPUT DATA',/,41X,10('-'))
 9025 FORMAT (/,26X,'NUMBER OF X-COORDINATES (NX) =',I5,/,26X,
     &        'NUMBER OF Y-COORDINATES (NY) =',I5,/,26X,
     &        'NUMBER OF Z-COORDINATES (NZ) =',I5,/,26X,
     &        'NUMBER OF TIME VALUES (NT) =',I5,/,26X,
     &      'NUMBER OF TERMS IN INNER INFINTE SERIES SUMMATION (NMAX) ='
     &      ,I5,/,26X,
     &      'NUMBER OF TERMS IN OUTER INFINTE SERIES SUMMATION (MMAX) ='
     &      ,I5)
 9030 FORMAT (/,26X,'SOLUTE CONCENTRATION ON MODEL BOUNDARY (C0) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'GROUND-WATER VELOCITY IN X-DIRECTION (VX) =',1P1E13.6,1X,
     &        A10,/,26X,'DISPERSION IN THE X-DIRECTION (DX) =',1P1E13.6,
     &        1X,A10,/,26X,'DISPERSION IN THE Y-DIRECTION (DY) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'DISPERSION IN THE Z-DIRECTION (DZ) =',1P1E13.6,1X,A10,/,
     &        26X,'FIRST-ORDER SOLUTE DECAY RATE (DK) =',1P1E13.6,1X,
     &        A10)
 9035 FORMAT (/,26X,'AQUIFER WIDTH (W) =',1P1E13.6,1X,A10,/,26X,
     &        'AQUIFER HEIGHT (H) =',1P1E13.6,1X,A10,/,26X,
     &        'SOLUTE SOURCE IS LOCATED BETWEEN Y1 =',1P1E13.6,1X,A10,/,
     &        59X,'Y2 =',1P1E13.6,1X,A10,/,59X,'Z1 =',1P1E13.6,1X,A10,/,
     &        55X,'AND Z2 =',1P1E13.6,1X,A10)
 9040 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AT TIME =',F12.4,1X,A10,
     &        /,36X,'AND AT Z =',F12.4,1X,A10,15X,
     &        '*  INDICATES SOLUTION DID NOT CONVERGE',//,26X,
     &        'Y-COORDINATE, IN ',A10)
 9045 FORMAT (6X,F12.4,2X,'! ',9(F11.5,A1))
      END
C
C
C
      SUBROUTINE CNORMP(DK,T,X,Y,Z,W,H,Y1,Y2,Z1,Z2,DX,DY,DZ,VX,CN,NMAX,
     &                  MMAX,IERR)
      DOUBLE PRECISION DK, T, X, Y, Z, W, H, Y1, Y2, Z1, Z2, DX, DY, DZ, 
     &                 VX, CN
      INTEGER NMAX, MMAX
      CHARACTER*1 IERR
C
C       THIS ROUTINE CALCULATES THE NORMALIZED CONCENTRATION AT X,Y,Z
C       BASED ON THE ANALYTIC SOLUTION TO THE THREE-DIMENSIONAL
C       ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION FOR A SEMI-
C       INFINITE AQUIFER WITH A FINITE WIDTH AND HEIGHT. THE SOLUTE
C       SOURCE HAS A FINITE WIDTH AND HEIGHT, EXTENDING FROM Y=Y1 TO
C       Y=Y2 AND Z=Z1 TO Z=Z2. SOLUTE MAY BE SUBJECT TO FIRST-ORDER
C       CHEMICAL TRANSFORMATION. THE SOLUTION CONTAINS TWO INFINITE
C       SERIES SUMMATIONS WHICH MAY CONVERGE SLOWLY.
C
      DOUBLE PRECISION a1, a2, alpha, b1, b2, beta, betat, c1, c2, 
     &                 cosry, cossz, eta, om, pi, pn, rtdxt, sigmam, 
     &                 sigman, subtm, subtn
      DOUBLE PRECISION term1, zeta
      INTEGER m, mm, mmax1, n, nmax1, nn
C
C---EXTERNALS
      EXTERNAL EXERFC
C
C---INTRINSICS
      INTEGER MOD
      DOUBLE PRECISION DSQRT, DSIN, DABS, DCOS
      INTRINSIC DSQRT, DSIN, DABS, DCOS, MOD
C
      IERR = ' '
C
C---FOR T=0, ALL CONCENTRATIONS EQUAL 0.0
      IF (T.LE.0.0D0) THEN
        CN = 0.0D0
C
      ELSEIF (X.GT.0.0D0) THEN
C
C---BEGIN SUMMATION OF TERMS IN INFINITE SERIES (OUTER SERIES)
        rtdxt = 2.0D0*DSQRT(DX*T)
        nmax1 = NMAX + 1
        mmax1 = MMAX + 1
        sigmam = 0.0D0
        subtm = 0.0D0
        pi = 3.14159265358979D0
        DO 30 mm = 1, mmax1
          m = mm - 1
          zeta = m*pi/H
          om = (Z2-Z1)/H
          IF (m.NE.0) om = (DSIN(zeta*Z2)-DSIN(zeta*Z1))/(m*pi)
          cossz = DCOS(zeta*Z)
C
C---BEGIN SUMMATION OF TERMS IN INFINITE SERIES (INNER SERIES)
          sigman = 0.0D0
          subtn = 0.0D0
          DO 10 nn = 1, nmax1
            n = nn - 1
            eta = n*pi/W
            pn = (Y2-Y1)/W
            IF (n.NE.0) pn = (DSIN(eta*Y2)-DSIN(eta*Y1))/(n*pi)
            cosry = DCOS(eta*Y)
            alpha = 4.0D0*DX*(eta*eta*DY+zeta*zeta*DZ+DK)
            beta = DSQRT(VX*VX+alpha)
            betat = beta*T
C
C---IF M>0 AND N>0, USE GENERAL FORM
C
C---TERM 1
            a1 = X*(VX-beta)/(2.0D0*DX)
            b1 = (X-betat)/rtdxt
            CALL EXERFC(a1,b1,c1)
            a2 = X*(VX+beta)/(2.0D0*DX)
            b2 = (X+betat)/rtdxt
            CALL EXERFC(a2,b2,c2)
            term1 = cosry*pn*(c1+c2)
C
C---MULTIPLY TERM BY L(MN)
            IF (m.EQ.0 .AND. n.EQ.0) term1 = term1*0.50D0
            IF (m.GT.0 .AND. n.GT.0) term1 = term1*2.0D0
C
C---ADD TERM TO SUMMATION
            sigman = sigman + term1
C
C---CHECK FOR CONVERGENCE OF INNER SERIES.  BECAUSE SERIES
C---OSCILLATES, CHECK SUBTOTAL OF LAST 10 TERMS.
            subtn = subtn + term1
            IF (MOD(nn,10).EQ.0) THEN
              IF (DABS(subtn).LT.1.0D-12) GOTO 20
              subtn = 0.0D0
            ENDIF
   10     CONTINUE
          IERR = '*'
   20     sigmam = sigmam + sigman*cossz*om
C
C---CHECK FOR CONVERGENCE OF OUTER SERIES.  BECAUSE SERIES
C---OSCILLATES, CHECK SUBTOTAL OF LAST 10 TERMS.
          subtm = subtm + sigman*om*cossz
          IF (MOD(mm,10).EQ.0) THEN
            IF (DABS(subtm).LT.1.0D-12) GOTO 40
            subtm = 0.0D0
          ENDIF
   30   CONTINUE
        IERR = '*'
   40   CN = sigmam
C
C---FOR X=0.0, CONCENTRATIONS ARE SPECIFIED BY BOUNDARY CONDITIONS
      ELSE
        CN = 0.0D0
        IF (Y.EQ.Y1 .OR. Y.EQ.Y2) THEN
          IF (Z.GT.Z1 .AND. Z.LT.Z2) CN = 0.50D0
          IF (Z.EQ.Z1 .OR. Z.EQ.Z2) CN = 0.25D0
        ENDIF
        IF (Z.EQ.Z1 .OR. Z.EQ.Z2) THEN
          IF (Y.GT.Y1 .AND. Y.LT.Y2) CN = 0.50D0
        ENDIF
        IF (Y.GT.Y1 .AND. Y.LT.Y2 .AND. Z.GT.Z1 .AND. Z.LT.Z2)
     &      CN = 1.0D0
      ENDIF
      RETURN
      END
