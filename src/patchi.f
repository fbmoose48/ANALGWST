C
C      ********************************************************
C      *                                                      *
C      *                   **** PATCHI ****                   *
C      *                                                      *
C      *   THREE-DIMENSIONAL GROUND-WATER SOLUTE TRANSPORT    *
C      *                                                      *
C      *    MODEL FOR A SEMI-INFINITE AQUIFER OF INFINITE     *
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
     &                 t(MAXT), c0, cn, dk, dx, dy, dz, vx, y1, y2, z1, 
     &                 z2
      INTEGER it, ix, iy, iz, j, nmax, np, np1, np2, npage, nt, nx, ny, 
     &        nz, ifx
C
C---EXTERNALS
      EXTERNAL OFILE, TITLE, GLQPTS, CNRMLP, PLTNC3, RDXYZT, RDPLOT, 
     &         WCONH3
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
C     Y1       Y-COORDINATE OF LOWER LIMIT OF PATCH SOLUTE SOURCE [L]
C     Y2       Y-COORDINATE OF UPPER LIMIT OF PATCH SOLUTE SOURCE [L]
C     Z1       Z-COORDINATE OF LOWER LIMIT OF PATCH SOLUTE SOURCE [L]
C     Z2       Z-COORDINATE OF UPPER LIMIT OF PATCH SOLUTE SOURCE [L]
C
C     NX       NUMBER OF X-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NY       NUMBER OF Y-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NZ       NUMBER OF Z-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NT       NUMBER OF TIME VALUES AT WHICH SOLUTION IS EVALUATED
C     NMAX     NUMBER OF TERMS USED IN GAUSS-LEGENDRE NUMERICAL
C              INTEGRATION TECHNIQUE (MUST EQUAL 4, 20, 60, 104 OR 256)
C
C     IPLT     PLOT CONTROL. IF IPLT>0, CONTOUR MAPS ARE PLOTTED
C     XSCLP    SCALING FACTOR TO CONVERT X TO PLOTTER INCHES
C     YSCLP    SCALING FACTOR TO CONVERT Y TO PLOTTER INCHES
C     DELTA    CONTOUR INCREMENT FOR PLOT. (VALUE BETWEEN 0 AND 1.0)
C
      Versn = 
     & '$Id: patchi.f,v 1.1 1996/04/03 19:46:45 rsregan Exp rsregan $'
      Versn = 
     &    '@(#)PATCHI - Analytical solution for 3-D GW solute transport'
      Versn = 
     &    '@(#)PATCHI - in an infinite-width and infinite-height system'
      Versn = '@(#)PATCHI - with uniform flow and a finite-width and'
      Versn = '@(#)PATCHI - finite-height solute source'
      Versn = '@(#)PATCHI - USGS TWRI, book 3, chap. B7, E.J. Wexler'
      Versn = '@(#)PATCHI - Contact: h2osoft@usgs.gov'
      Versn = '@(#)PATCHI - Version: 1.1x 1996/04/03'
C
C---DEFINE INPUT/OUTPUT FILES AND PRINT TITLE PAGE
      CALL OFILE
      CALL TITLE
      WRITE (Io,9020)
C
C---READ IN MODEL PARAMETERS
      READ (In,9005) nx, ny, nz, nt, nmax, Iplt
      WRITE (Io,9025) nx, ny, nz, nt, nmax
      READ (In,9010) Cunits, Vunits, Dunits, Kunits, Lunits, Tunits
      READ (In,9015) c0, vx, dx, dy, dz, dk
      WRITE (Io,9030) c0, Cunits, vx, Vunits, dx, Dunits, dy, Dunits, 
     &                dz, Dunits, dk, Kunits
      READ (In,9015) y1, y2, z1, z2
      WRITE (Io,9035) y1, Lunits, y2, Lunits, z1, Lunits, z2, Lunits
      CALL RDXYZT(nx,ny,nz,nt,x,y,z,t)
      IF (Iplt.GT.0) CALL RDPLOT(Cunits,Xsclp,Ysclp,Delta)
C
C---READ IN GAUSS-LEGENDRE POINTS AND WEIGHTING FACTORS
      CALL GLQPTS(nmax)
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
              CALL CNRMLP(dk,t(it),x(ix),y(iy),z(iz),y1,y2,z1,z2,dx,dy,
     &                    dz,vx,cn,nmax)
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
              WRITE (Io,9045) x(ix), (cxy(ix,np1+j),j=1,np2)
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
     &        31X,'FOR A SEMI-INFINITE AQUIFER OF INFINITE WIDTH',/,29X,
     &        'AND HEIGHT WITH A PATCH SOLUTE SOURCE AT X=0.0',////,41X,
     &        'INPUT DATA',/,41X,10('-'))
 9025 FORMAT (/,26X,'NUMBER OF X-COORDINATES (NX) =',I5,/,26X,
     &        'NUMBER OF Y-COORDINATES (NY) =',I5,/,26X,
     &        'NUMBER OF Z-COORDINATES (NZ) =',I5,/,26X,
     &        'NUMBER OF TIME VALUES (NT) =',I5,/,26X,
     &        'NUMBER OF POINTS FOR NUMERICAL INTEGRATION (NMAX) =',I5)
 9030 FORMAT (/,26X,'SOLUTE CONCENTRATION ON MODEL BOUNDARY (C0) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'GROUND-WATER VELOCITY IN X-DIRECTION (VX) =',1P1E13.6,1X,
     &        A10,/,26X,'DISPERSION IN THE X-DIRECTION (DX) =',1P1E13.6,
     &        1X,A10,/,26X,'DISPERSION IN THE Y-DIRECTION (DY) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'DISPERSION IN THE Z-DIRECTION (DZ) =',1P1E13.6,1X,A10,/,
     &        26X,'FIRST-ORDER SOLUTE DECAY RATE (DK) =',1P1E13.6,1X,
     &        A10)
 9035 FORMAT (/,26X,'AQUIFER WIDTH (W) AND HEIGHT (H) ARE INFINITE',/,
     &        26X,'SOLUTE SOURCE IS LOCATED BETWEEN Y1 =',1P1E13.6,1X,
     &        A10,/,59X,'Y2 =',1P1E13.6,1X,A10,/,59X,'Z1 =',1P1E13.6,1X,
     &        A10,/,55X,'AND Z2 =',1P1E13.6,1X,A10)
 9040 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AT TIME =',F12.4,1X,A10,
     &        /,36X,'AND AT Z =',F12.4,1X,A10,//,26X,
     &        'Y-COORDINATE, IN ',A10)
 9045 FORMAT (6X,F12.4,'  !',9F12.6)
      END
C
C
C
      SUBROUTINE CNRMLP(DK,T,X,Y,Z,Y1,Y2,Z1,Z2,DX,DY,DZ,VX,CN,NMAX)
      INTEGER NMAX
      DOUBLE PRECISION DK, T, X, Y, Z, Y1, Y2, Z1, Z2, DX, DY, DZ, VX, 
     &                 CN
C
C       THIS ROUTINE CALCULATES THE NORMALIZED CONCENTRATION AT X,Y,Z
C       BASED ON THE ANALYTIC SOLUTION TO THE THREE-DIMENSIONAL
C       ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION FOR A SEMI-
C       INFINITE AQUIFER WITH INFINITE WIDTH AND HEIGHT. THE SOLUTE
C       SOURCE HAS A FINITE WIDTH AND HEIGHT, EXTENDING FROM Y=Y1 TO
C       Y=Y2 AND Z=Z1 TO Z=Z2. THE SOLUTE MAY BE SUBJECT TO FIRST-ORDER
C       CHEMICAL TRANSFORMATION. THE SOLUTION CONTAINS AN INTEGRAL
C       FROM 0 TO T**.25 WHICH IS EVALUATED NUMERICALLY USING A GAUSS-
C       LEGENDRE QUADRATURE TECHNIQUE.
C
      DOUBLE PRECISION erfc1, erfc2, exp1, exp2, pi, q1, q2, q3, q4, 
     &                 sum, term, tt, wi, xvt, z4, zi, zsq
      INTEGER i
C
      DOUBLE PRECISION Wn, Zn
      COMMON /GLPTS/ Wn(256), Zn(256)
C
C---EXTERNALS
      EXTERNAL EXERFC
C
C---INTRINSICS
      DOUBLE PRECISION DSQRT
      INTRINSIC DSQRT
C
C---FOR T=0, ALL CONCENTRATIONS EQUAL 0.0
      IF (T.LE.0.0D0) THEN
        CN = 0.0D0
C
      ELSEIF (X.GT.0.0D0) THEN
C
C---START NUMERICAL INTEGRATION LOOP
        sum = 0.0D0
        DO 10 i = 1, NMAX
C
C---SCALE THE GAUSS-LEGENDRE COEFFICIENTS TO ACCOUNT FOR THE
C---NON-NORMALIZED LIMITS OF INTEGRATION
C---LIMITS OF INTEGRATION ARE FROM 0 TO T**0.25
          tt = T**0.250D0
          wi = Wn(i)
          zi = tt*(Zn(i)+1.0D0)/2.0D0
          zsq = zi*zi
          z4 = zsq*zsq
C
C---TERM 1
          xvt = X - VX*z4
          exp1 = -xvt*xvt/(4.0D0*DX*z4) - DK*z4
          erfc1 = (Y1-Y)/(2.0D0*zsq*DSQRT(DY))
          CALL EXERFC(exp1,erfc1,q1)
C
C---TERM 2
          erfc2 = (Y2-Y)/(2.0D0*zsq*DSQRT(DY))
          CALL EXERFC(exp1,erfc2,q2)
C
C---TERM 3
          exp2 = 0.0D0
          erfc1 = (Z1-Z)/(2.0D0*zsq*DSQRT(DZ))
          CALL EXERFC(exp2,erfc1,q3)
          erfc2 = (Z2-Z)/(2.0D0*zsq*DSQRT(DZ))
          CALL EXERFC(exp2,erfc2,q4)
          term = (q1-q2)*(q3-q4)*wi/(zi*zsq)
          sum = sum + term
   10   CONTINUE
        sum = sum*tt/2.0D0
        pi = 3.14159265358979D0
        CN = sum*X/(2.0D0*DSQRT(pi*DX))
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
C
      RETURN
      END
