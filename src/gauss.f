C
C      ********************************************************
C      *                                                      *
C      *                   **** GAUSS ****                    *
C      *                                                      *
C      * TWO-DIMENSIONAL GROUND-WATER SOLUTE TRANSPORT MODEL  *
C      *                                                      *
C      *   FOR A SEMI-INFINITE AQUIFER OF INFINITE WIDTH. A   *
C      *                                                      *
C      *  SOURCE HAVING A GAUSSIAN-SHAPED CONCENTRATION DIS-  *
C      *                                                      *
C      * TRIBUTION IS LOCATED AT X=0 AND CENTERED ABOUT Y=YC  *
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
      DOUBLE PRECISION cxy(MAXX,MAXY), x(MAXX), y(MAXY), t(MAXT), cm, 
     &                 cn, dk, dx, dy, sigma, vx, yc
      INTEGER it, ix, iy, j, nmax, np, np1, np2, npage, nt, nx, ny, ifx
C
C---EXTERNALS
      EXTERNAL OFILE, TITLE, CNRMLG, PLTNC2, GLQPTS, RDPLOT, RDXYT, 
     &         WCONH2
C
C     PROGRAM VARIABLES
C
C     CM       MAXIMUM SOLUTE CONCENTRATION AT THE INFLOW BOUNDARY [M/L**3]
C     DX       LONGITUDINAL DISPERSION COEFFICIENT [L**2/T]
C     DY       TRANSVERSE DISPERSION COEFFICIENT [L**2/T]
C     VX       GROUND-WATER VELOCITY IN X-DIRECTION [L/T]
C     DK       FIRST-ORDER SOLUTE DECAY CONSTANT [1/T]
C     X        X-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     Y        Y-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     T        TIME AT WHICH CONCENTRATION IS EVALUATED [T]
C     CN       NORMALIZED CONCENTRATION C/CM [DIMENSIONLESS]
C     CXY      SOLUTE CONCENTRATION C(X,Y,T) [M/L**3]
C     YC       Y-COORDINATE OF THE CENTER OF SOLUTE SOURCE AT X=0 [L]
C     SIGMA    STANDARD DEVIATION OF GAUSSIAN CONCENTRATION DISTRIBUTION
C              FOR THE SOLUTE SOURCE [L]
C
C     NX       NUMBER OF X-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NY       NUMBER OF Y-POSITIONS AT WHICH SOLUTION IS EVALUATED
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
     & '$Id: gauss.f,v 1.1 1996/04/03 19:28:51 rsregan Exp rsregan $'
      Versn = 
     &     '@(#)GAUSS - Analytical solution for 2-D GW solute transport'
      Versn = 
     &       '@(#)GAUSS - in an infinite-width system with uniform flow'
      Versn = 
     &   '@(#)GAUSS - and a solute source having gaussian concentration'
      Versn = '@(#)GAUSS - distribution'
      Versn = '@(#)GAUSS - USGS TWRI, book 3, chap. B7, E.J. Wexler'
      Versn = '@(#)GAUSS - Contact: h2osoft@usgs.gov'
      Versn = '@(#)GAUSS - Version: 1.1x 1996/04/03'
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
      READ (In,9015) cm, vx, dx, dy, dk
      WRITE (Io,9030) cm, Cunits, vx, Vunits, dx, Dunits, dy, Dunits, 
     &                dk, Kunits
      READ (In,9015) yc, sigma
      WRITE (Io,9035) yc, Lunits, sigma, Lunits
      CALL RDXYT(nx,ny,nt,x,y,t)
      IF (Iplt.GT.0) CALL RDPLOT(Cunits,Xsclp,Ysclp,Delta)
C
C---READ IN GAUSS-LEGENDRE POINTS AND WEIGHTING FACTORS
      CALL GLQPTS(nmax)
C
C---BEGIN TIME LOOP
      DO 50 it = 1, nt
C
C---BEGIN X LOOP
        DO 20 ix = 1, nx
C
C---CALCULATE NORMALIZED CONCENTRATION FOR ALL Y AT X=X(IX)
          DO 10 iy = 1, ny
            CALL CNRMLG(dk,t(it),x(ix),y(iy),yc,sigma,dx,dy,vx,cn,nmax)
            cxy(ix,iy) = cm*cn
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
            WRITE (Io,9045) x(ix), (cxy(ix,np1+j),j=1,np2)
            CALL WCONH2(ix,np1,np2,t(it),y)
   30     CONTINUE
   40   CONTINUE
C
C---PLOT CONTOUR MAPS OF NORMALIZED CONCENTRATION FOR EACH TIME VALUE.
        IF (Iplt.GT.0) CALL PLTNC2(nx,ny,nt,it,x,y,t(it),cxy,cm)
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
     &        30X,'FOR A SEMI-INFINITE AQUIFER OF INFINITE WIDTH',/,26X,
     &        'WITH A SOLUTE SOURCE HAVING A GAUSSIAN CONCENTRATION',/,
     &        25X,
     &        'DISTRIBUTION LOCATED AT X=0.0 AND CENTERED ABOUT Y=YC',
     &        ////,41X,'INPUT DATA',/,41X,10('-'))
 9025 FORMAT (/,26X,'NUMBER OF X-COORDINATES (NX) =',I5,/,26X,
     &        'NUMBER OF Y-COORDINATES (NY) =',I5,/,26X,
     &        'NUMBER OF TIME VALUES (NT) =',I5,/,26X,
     &        'NUMBER OF POINTS FOR NUMERICAL INTEGRATION (NMAX) =',I5)
 9030 FORMAT (/,26X,
     &        'MAXIMUM SOLUTE CONCENTRATION AT THE BOUNDARY CM) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'GROUND-WATER VELOCITY IN X-DIRECTION (VX) =',1P1E13.6,1X,
     &        A10,/,26X,'DISPERSION IN THE X-DIRECTION (DX) =',1P1E13.6,
     &        1X,A10,/,26X,'DISPERSION IN THE Y-DIRECTION (DY) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'FIRST-ORDER SOLUTE DECAY RATE (DK) =',1P1E13.6,1X,A10)
 9035 FORMAT (/,26X,'AQUIFER WIDTH (W) IS INFINITE',/,26X,
     &        'SOLUTE SOURCE IS CENTERED AT Y =',1P1E13.6,1X,A10,/,26X,
     &        'STANDARD DEVIATION OF GAUSSIAN DISTRIBUTION (SIGMA) =',
     &        1P1E13.6,1X,A10)
 9040 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AT TIME =',F12.4,1X,A10,
     &        //,26X,'Y-COORDINATE, IN ',A10)
 9045 FORMAT (6X,F12.4,'  !',9F12.6)
      END
C
C
C
      SUBROUTINE CNRMLG(DK,T,X,Y,YC,SIGMA,DX,DY,VX,CN,NMAX)
      DOUBLE PRECISION DK, T, X, Y, YC, SIGMA, DX, DY, VX, CN
      INTEGER NMAX
C
C       THIS ROUTINE CALCULATES THE NORMALIZED CONCENTRATION AT X,Y
C       BASED ON THE ANALYTIC SOLUTION TO THE TWO-DIMENSIONAL
C       ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION FOR A SEMI-
C       INFINITE AQUIFER OF INFINITE WIDTH. THE SOLUTE SOURCE, LOCATED
C       AT X=0.0 AND CENTERED ABOUT Y=YC, HAS A GAUSSIAN CONCENTRATION
C       DISTRIBUTION WITH A STANDARD DEVIATION OF SIGMA. THE SOLUTION
C       CONTAINS AN INTEGRAL FROM 0 TO T**.25 WHICH IS EVALUATED USING
C       A GAUSS-LEGENDRE QUADRATURE INTEGRATION TECHNIQUE.
C
      DOUBLE PRECISION alpha, beta, pi, sigsq, sum, tt, vx2d, wi, x1, 
     &                 x2, y1, z4, zi
      INTEGER i
C
      DOUBLE PRECISION Wn, Zn
      COMMON /GLPTS/ Wn(256), Zn(256)
C
C---INTRINSICS
      DOUBLE PRECISION DSQRT, DEXP
      INTRINSIC DSQRT, DEXP
      IF (T.GT.0.0D0) THEN
        y1 = Y - YC
        sigsq = SIGMA*SIGMA
C
        IF (X.GT.0.0D0) THEN
C
C---START NUMERICAL INTEGRATION LOOP
          sum = 0.0D0
          beta = DK + VX*VX/(4.0D0*DX)
          DO 10 i = 1, NMAX
C
C---SCALE THE GAUSS-LEGENDRE COEFFICIENTS TO ACCOUNT FOR THE
C---NON-NORMALIZED LIMITS OF INTEGRATION
C---LIMITS OF INTEGRATION ARE FROM 0 TO T**0.25
            tt = T**0.250D0
            wi = Wn(i)
            zi = tt*(Zn(i)+1.0D0)/2.0D0
C
C---TERM 1
            z4 = zi**4
            alpha = z4*DY + sigsq/2.0D0
            x1 = DEXP(-z4*beta-X*X/(4.0D0*DX*z4)-y1*y1/(4.0D0*alpha))
            x1 = x1/((zi*zi*zi)*DSQRT(alpha))
            sum = sum + x1*wi
   10     CONTINUE
          sum = sum*tt/2.0D0
C
C---TERM 2
          vx2d = VX*X/(2.0D0*DX)
          x2 = sum*2.0D0*DEXP(vx2d)
          pi = 3.14159265358979D0
          CN = X*SIGMA*x2/(DSQRT(2.0D0*pi*DX))
C
C---FOR X=0.0, CONCENTRATIONS ARE SPECIFIED BY BOUNDARY CONDITIONS
        ELSE
          CN = DEXP(-y1*y1/(2.0D0*sigsq))
        ENDIF
C
C---FOR T=0, ALL CONCENTRATIONS EQUAL 0.0
      ELSE
        CN = 0.0D0
      ENDIF
      RETURN
      END
