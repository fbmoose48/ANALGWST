C
C      ********************************************************
C      *                                                      *
C      *                   **** POINT2 ****                   *
C      *                                                      *
C      * TWO-DIMENSIONAL GROUND-WATER SOLUTE TRANSPORT MODEL  *
C      *                                                      *
C      *    FOR AN AQUIFER OF INFINITE AREAL EXTENT WITH A    *
C      *                                                      *
C      *   CONTINUOUS POINT SOURCE LOCATED AT X=XC AND Y=YC   *
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
      DOUBLE PRECISION cxy(MAXX,MAXY), x(MAXX), y(MAXY), t(MAXT), c0, 
     &                 cn, dk, dx, dy, por, qm, vx, xc, xx, yc, yy
      INTEGER it, ix, iy, j, nmax, np, np1, np2, npage, nt, nx, ny, ifx
C
C---EXTERNALS
      EXTERNAL OFILE, TITLE, GLQPTS, CNRML2, PLTNC2, RDPLOT, RDXYT, 
     &         WCONH2
C
C     PROGRAM VARIABLES
C
C     C0       SOLUTE CONCENTRATION IN INJECTED FLUID [M/L**3]
C     DX       LONGITUDINAL DISPERSION COEFFICIENT [L**2/T]
C     DY       TRANSVERSE DISPERSION COEFFICIENT [L**2/T]
C     VX       GROUND-WATER VELOCITY IN X-DIRECTION [L/T]
C     DK       FIRST-ORDER SOLUTE DECAY CONSTANT [1/T]
C     X        X-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     Y        Y-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     T        TIME AT WHICH CONCENTRATION IS EVALUATED [T]
C     CN       NORMALIZED CONCENTRATION C/C0 [DIMENSIONLESS]
C     CXY      SOLUTE CONCENTRATION C(X,Y,T) [M/L**3]
C     XC       X-COORDINATE OF POINT SOURCE [L]
C     YC       Y-COORDINATE OF POINT SOURCE [L]
C     QM       FLUID INJECTION RATE PER UNIT THICKNESS OF AQUIFER [L**2/T]
C                (UNITS MUST BE SAME AS DISPERSION COEFFICIENT)
C     POR      AQUIFER POROSITY [DIMENSIONLESS]
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
     & '$Id: point2.f,v 1.1 1996/04/03 19:39:11 rsregan Exp rsregan $'
      Versn = 
     &    '@(#)POINT2 - Analytical solution for 2-D GW solute transport'
      Versn = 
     &      '@(#)POINT2 - in an infinite system with uniform flow and a'
      Versn = '@(#)POINT2 - continuous point source'
      Versn = '@(#)POINT2 - USGS TWRI, book 3, chap. B7, E.J. Wexler'
      Versn = '@(#)POINT2 - Contact: h2osoft@usgs.gov'
      Versn = '@(#)POINT2 - Version: 1.1x 1996/04/03'
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
      READ (In,9015) xc, yc, qm, por
      WRITE (Io,9035) xc, Lunits, yc, Lunits, qm, Dunits, por
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
          xx = x(ix) - xc
C
C---CALCULATE NORMALIZED CONCENTRATION FOR ALL Y AT X=X(IX)
          DO 10 iy = 1, ny
            yy = y(iy) - yc
            CALL CNRML2(qm,por,dk,t(it),xx,yy,dx,dy,vx,cn,nmax)
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
            WRITE (Io,9045) x(ix), (cxy(ix,np1+j),j=1,np2)
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
     &        29X,'FOR AN AQUIFER OF INFINITE AREAL EXTENT WITH A',/,
     &        32X,'CONTINUOUS POINT SOURCE AT X=0 AND Y=YC',////,41X,
     &        'INPUT DATA',/,41X,10('-'))
 9025 FORMAT (/,26X,'NUMBER OF X-COORDINATES (NX) =',I5,/,26X,
     &        'NUMBER OF Y-COORDINATES (NY) =',I5,/,26X,
     &        'NUMBER OF TIME VALUES (NT) =',I5,/,26X,
     &        'NUMBER OF POINTS FOR NUMERICAL INTEGRATION (NMAX) =',I5)
 9030 FORMAT (/,26X,'SOLUTE CONCENTRATION IN INJECTED FLUID (C0) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'GROUND-WATER VELOCITY IN X-DIRECTION (VX) =',1P1E13.6,1X,
     &        A10,/,26X,'DISPERSION IN THE X-DIRECTION (DX) =',1P1E13.6,
     &        1X,A10,/,26X,'DISPERSION IN THE Y-DIRECTION (DY) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'FIRST-ORDER SOLUTE DECAY RATE (DK) =',1P1E13.6,1X,A10)
 9035 FORMAT (/,26X,'AQUIFER IS OF INFINITE AREAL EXTENT',/,26X,
     &        'CONTINUOUS POINT SOURCE IS LOCATED AT X =',1P1E13.6,1X,
     &        A10,/,64X,'Y =',1P1E13.6,1X,A10,/,26X,
     &      'FLUID INJECTION RATE PER UNIT THICHKNESS OF AQUIFER (QM) ='
     &      ,1P1E13.6,1X,A10,/,26X,'AQUIFER POROSITY (POR) =',1P1E13.6)
 9040 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AT TIME =',F12.4,1X,A10,
     &        //,26X,'Y-COORDINATE, IN ',A10)
 9045 FORMAT (6X,F12.4,'  !',9F12.5)
      END
C
C
C
      SUBROUTINE CNRML2(QM,POR,DK,T,X,Y,DX,DY,VX,CN,NMAX)
      DOUBLE PRECISION CN, DK, DX, DY, POR, QM, T, VX, X, Y
      INTEGER NMAX
C
C       THIS ROUTINE CALCULATES SOLUTE CONCENTRATION AT X,Y BASED ON
C       THE ANALYTIC SOLUTION TO THE TWO-DIMENSIONAL ADVECTIVE-
C       DISPERSIVE SOLUTE TRANSPORT EQUATION FOR AN AQUIFER OF
C       INFINITE AREAL EXTENT WITH A CONTINUOUS POINT SOURCE LOCATED
C       AT X=XC AND Y=YC.  THE INTEGRAL FROM 0 TO T IS EVALUATED
C       USING A GAUSS-LEGENDRE QUADRATURE INTEGRATION TECHNIQUE.
C
      DOUBLE PRECISION alpha, beta, pi, sum, vx2d, wi, x1, zi
      INTEGER i
C
      DOUBLE PRECISION Wn, Zn
      COMMON /GLPTS / Wn(256), Zn(256)
C
C---INTRINSICS
      DOUBLE PRECISION DEXP, DSQRT
      INTRINSIC DEXP, DSQRT
C
      IF (T.GT.0.0D0) THEN
C
C---START NUMERICAL INTEGRATION LOOP
        alpha = X*X/(4.0D0*DX) + Y*Y/(4.0D0*DY)
        beta = VX*VX/(4.0D0*DX) + DK
        vx2d = VX*X/(2.0D0*DX)
        sum = 0.0D0
        DO 10 i = 1, NMAX
C
C---SCALE THE GAUSS-LEGENDRE COEFFICIENTS TO ACCOUNT FOR THE
C---NON-NORMALIZED LIMITS OF INTEGRATION
          wi = Wn(i)
          zi = T*(Zn(i)+1.0D0)/2.0D0
C
C---TERM 1
          x1 = -alpha/zi - beta*zi
          x1 = DEXP(x1)/zi
          sum = sum + x1*wi
   10   CONTINUE
        sum = sum*T/2.0D0
        pi = 3.14159265358979D0
        CN = QM*sum*DEXP(vx2d)/(4.0D0*POR*pi*DSQRT(DX*DY))
C
C---FOR T=0, ALL CONCENTRATIONS EQUAL 0.0
      ELSE
        CN = 0.0D0
      ENDIF
      RETURN
      END
