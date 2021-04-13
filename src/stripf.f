C
C      ********************************************************
C      *                                                      *
C      *                   **** STRIPF ****                   *
C      *                                                      *
C      * TWO-DIMENSIONAL GROUND-WATER SOLUTE TRANSPORT MODEL  *
C      *                                                      *
C      *   FOR A SEMI-INFINITE AQUIFER WITH A FINITE WIDTH    *
C      *                                                      *
C      *     A STRIP SOURCE EXTENDS FROM Y1 TO Y2 AT X=0      *
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
C
      INCLUDE 'dimens.inc'
      INCLUDE 'pltdat.inc'
      INCLUDE 'units.inc'
C
      INTEGER In, Io
      COMMON /IOUNIT/ In, Io
C
      DOUBLE PRECISION c0, cn, dk, dx, dy, vx, w, y1, y2
      INTEGER it, ix, iy, j, nmax, np, np1, np2, npage, nt, nx, ny, ifx
      CHARACTER*1 ierr(MAXX,MAXY)
      DOUBLE PRECISION cxy(MAXX,MAXY), x(MAXX), y(MAXY), t(MAXT)
C
C---EXTERNALS
      EXTERNAL OFILE, TITLE, CNRMLF, PLTNC2, RDPLOT, RDXYT, WCONH2
C
C     PROGRAM VARIABLES
C
C     C0       SOLUTE CONCENTRATION AT THE INFLOW BOUNDARY [M/L**3]
C     DX       LONGITUDINAL DISPERSION COEFFICIENT [L**2/T]
C     DY       TRANSVERSE DISPERSION COEFFICIENT [L**2/T]
C     VX       GROUND-WATER VELOCITY IN X-DIRECTION [L/T]
C     DK       FIRST-ORDER SOLUTE DECAY CONSTANT [1/T]
C     X        X-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     Y        Y-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     T        TIME AT WHICH CONCENTRATION IS EVALUATED [T]
C     CN       NORMALIZED CONCENTRATION C/C0 [DIMENSIONLESS]
C     CXY      SOLUTE CONCENTRATION C(X,Y,T) [M/L**3]
C     W        AQUIFER WIDTH (AQUIFER EXTENDS FROM Y=0 TO Y=W) [L]
C     Y1       Y-COORDINATE OF LOWER LIMIT OF STRIP SOLUTE SOURCE [L]
C     Y2       Y-COORDINATE OF UPPER LIMIT OF STRIP SOLUTE SOURCE [L]
C
C     NX       NUMBER OF X-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NY       NUMBER OF Y-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NT       NUMBER OF TIME VALUES AT WHICH SOLUTION IS EVALUATED
C     NMAX     NUMBER OF TERMS USED IN INFINITE SERIES SUMMATION
C
C     IPLT     PLOT CONTROL. IF IPLT>0, CONTOUR MAPS ARE PLOTTED
C     XSCLP    SCALING FACTOR TO CONVERT X TO PLOTTER INCHES
C     YSCLP    SCALING FACTOR TO CONVERT Y TO PLOTTER INCHES
C     DELTA    CONTOUR INCREMENT FOR PLOT. (VALUE BETWEEN 0 AND 1.0)
C
      Versn = 
     & '$Id: stripf.f,v 1.1 1996/04/03 19:39:11 rsregan Exp rsregan $'
      Versn = 
     &    '@(#)STRIPF - Analytical solution for 2-D GW solute transport'
      Versn = '@(#)STRIPF - in a finite-width system with uniform flow'
      Versn = '@(#)STRIPF - and a finite-width solute source'
      Versn = '@(#)STRIPF - USGS TWRI, book 3, chap. B7, E.J. Wexler'
      Versn = '@(#)STRIPF - Contact: h2osoft@usgs.gov'
      Versn = '@(#)STRIPF - Version: 1.1x 1996/04/03'
C
C---DEFINE INPUT/OUTPUT FILES AND PRINT TITLE PAGE
      CALL OFILE
      CALL TITLE
      WRITE (Io,9020)
C
C---READ IN MODEL PARAMETERS
      READ (In,9005) nx, ny, nt, nmax, Iplt
      WRITE (Io,9025) nx, ny, nt, nmax
      READ (In,9010) Cunits, Vunits, Dunits, Kunits, Lunits, Tunits
      READ (In,9015) c0, vx, dx, dy, dk
      WRITE (Io,9030) c0, Cunits, vx, Vunits, dx, Dunits, dy, Dunits, 
     &                dk, Kunits
      READ (In,9015) w, y1, y2
      WRITE (Io,9035) w, Lunits, y1, Lunits, y2, Lunits
      CALL RDXYT(nx,ny,nt,x,y,t)
      IF (Iplt.GT.0) CALL RDPLOT(Cunits,Xsclp,Ysclp,Delta)
C
C---BEGIN TIME LOOP
      DO 50 it = 1, nt
C
C---BEGIN X LOOP
        DO 20 ix = 1, nx
C
C---CALCULATE NORMALIZED CONCENTRATION FOR ALL Y AT X=X(IX)
          DO 10 iy = 1, ny
            CALL CNRMLF(dk,t(it),x(ix),y(iy),w,y1,y2,dx,dy,vx,cn,nmax,
     &                  ierr(ix,iy))
            cxy(ix,iy) = c0*cn
   10     CONTINUE
   20   CONTINUE
C
C---PRINT OUT TABLES OF CONCENTRATION VALUES
        npage = 1 + (ny-1)/9
        WRITE (Io,9040) t(it), Tunits, Lunits
        ifx = 0
        DO 40 np = 1, npage
          np1 = (np-1)*9
          np2 = 9
          IF ((np1+np2).GT.ny) np2 = ny - np1
          CALL WCONH2(ifx,np1,np2,t(it),y)
          ifx = 45
          DO 30 ix = 1, nx
            WRITE (Io,9045) x(ix), 
     &                      (cxy(ix,np1+j),ierr(ix,np1+j),j=1,np2)
            CALL WCONH2(ix,np1,np2,t(it),y)
   30     CONTINUE
   40   CONTINUE
C
C---PLOT CONTOUR MAPS OF NORMALIZED CONCENTRATION FOR EACH TIME VALUE.
        IF (Iplt.GT.0) CALL PLTNC2(nx,ny,nt,it,x,y,t(it),cxy,c0)
   50 CONTINUE
      CLOSE (In)
      CLOSE (Io)
      STOP
C
C---FORMAT STATEMENTS
 9005 FORMAT (20I4)
 9010 FORMAT (8A10)
 9015 FORMAT (8F10.0)
 9020 FORMAT (/////,31X,'ANALYTICAL SOLUTION TO THE TWO-DIMENSIONAL',/,
     &        29X,'ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION',/,
     &        31X,'FOR A SEMI-INFINITE AQUIFER OF FINITE WIDTH',/,27X,
     &        'WITH A FINITE-WIDTH (STRIP) SOLUTE SOURCE AT X=0.0',///,
     &        41X,'INPUT DATA',/,41X,10('-'))
 9025 FORMAT (/,26X,'NUMBER OF X-COORDINATES (NX) =',I5,/,26X,
     &        'NUMBER OF Y-COORDINATES (NY) =',I5,/,26X,
     &        'NUMBER OF TIME VALUES (NT) =',I5,/,26X,
     &        'NUMBER OF TERMS IN INFINTE SERIES SUMMATION (NMAX) =',I5)
 9030 FORMAT (/,26X,'SOLUTE CONCENTRATION ON MODEL BOUNDARY (C0) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'GROUND-WATER VELOCITY IN X-DIRECTION (VX) =',1P1E13.6,1X,
     &        A10,/,26X,'DISPERSION IN THE X-DIRECTION (DX) =',1P1E13.6,
     &        1X,A10,/,26X,'DISPERSION IN THE Y-DIRECTION (DY) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'FIRST-ORDER SOLUTE DECAY RATE (DK) =',1P1E13.6,1X,A10)
 9035 FORMAT (/,26X,'AQUIFER WIDTH (W) =',1P1E13.6,1X,A10,/,26X,
     &        'SOLUTE SOURCE IS LOCATED BETWEEN Y1 =',1P1E13.6,1X,A10,/,
     &        54X,'AND Y2 =',1P1E13.6,1X,A10)
 9040 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AT TIME =',F12.4,1X,A10,
     &        15X,'*  INDICATES SOLUTION DID NOT CONVERGE',//,26X,
     &        'Y-COORDINATE, IN ',A10)
 9045 FORMAT (6X,F12.4,'  ! ',9(F11.5,A1))
      END
C
C
C
      SUBROUTINE CNRMLF(DK,T,X,Y,W,Y1,Y2,DX,DY,VX,CN,NMAX,IERR)
      DOUBLE PRECISION DK, T, X, Y, W, Y1, Y2, DX, DY, VX, CN
      INTEGER NMAX
      CHARACTER*1 IERR
C
C       THIS ROUTINE CALCULATES THE NORMALIZED CONCENTRATION AT X,Y
C       BASED ON THE ANALYTIC SOLUTION TO THE TWO-DIMENSIONAL
C       ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION FOR A SEMI-
C       INFINITE AQUIFER WITH A FINITE WIDTH.  A FINITE-WIDTH (STRIP)
C       SOLUTE SOURCE EXTENDS FROM Y=Y1 TO Y=Y2. THE SOLUTION
C       CONTAINS AN INFINITE SERIES SUMMATION WHICH MAY TAKE A LARGE
C       NUMBER OF TERMS TO CONVERGE FOR SMALL VALUES OF X.
C
      DOUBLE PRECISION a1, a2, alpha, b1, b2, beta, betat, c1, c2, 
     &                 cosry, eta, pi, pn, rtdxt, sigma, subtot, term
      INTEGER n, nmax1, nn
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
      ELSEIF (X.GT.0.0D0) THEN
C
C---BEGIN SUMMATION OF TERMS IN INFINITE SERIES
        rtdxt = 2.0D0*DSQRT(DX*T)
        sigma = 0.0D0
        subtot = 0.0D0
        nmax1 = NMAX + 1
        pi = 3.14159265358979D0
        DO 10 nn = 1, nmax1
          n = nn - 1
          eta = n*pi/W
          pn = (Y2-Y1)/(2.0D0*W)
          IF (n.NE.0) pn = (DSIN(eta*Y2)-DSIN(eta*Y1))/(n*pi)
          cosry = DCOS(eta*Y)
          alpha = 4.0D0*DX*(eta*eta*DY+DK)
          beta = DSQRT(VX*VX+alpha)
          betat = beta*T
C
C---CALCULATE TERM 1
          a1 = X*(VX-beta)/(2.0D0*DX)
          b1 = (X-betat)/rtdxt
          CALL EXERFC(a1,b1,c1)
C
C---CALCULATE TERM 2
          a2 = X*(VX+beta)/(2.0D0*DX)
          b2 = (X+betat)/rtdxt
          CALL EXERFC(a2,b2,c2)
C
C---ADD TERMS TO SUMMATION
          term = pn*cosry*(c1+c2)
          sigma = sigma + term
C
C---CHECK FOR CONVERGENCE. BECAUSE SERIES OSCILLATES, CHECK
C---SUBTOTAL OF LAST 10 TERMS.
          subtot = subtot + term
          IF (MOD(nn,10).EQ.0) THEN
            IF (DABS(subtot).LT.1.0D-12) GOTO 20
            subtot = 0.0D0
          ENDIF
   10   CONTINUE
        IERR = '*'
   20   CN = sigma
C
C---FOR X=0.0, CONCENTRATIONS ARE SPECIFIED BY BOUNDARY CONDITIONS
      ELSE
        CN = 0.0D0
        IF (Y.GT.Y1 .AND. Y.LT.Y2) CN = 1.0D0
        IF (Y.EQ.Y1) CN = 0.50D0
        IF (Y.EQ.Y2) CN = 0.50D0
      ENDIF
      RETURN
      END
