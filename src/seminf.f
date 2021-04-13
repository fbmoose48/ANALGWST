C
C      ********************************************************
C      *                                                      *
C      *                   **** SEMINF ****                   *
C      *                                                      *
C      * ONE-DIMENSIONAL GROUND-WATER SOLUTE-TRANSPORT MODEL  *
C      *                                                      *
C      *   FOR A SEMI-INFINITE SYSTEM WITH A FIRST-TYPE OR    *
C      *                                                      *
C      *         THIRD-TYPE BOUNDARY CONDITION AT X=0         *
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
      INCLUDE 'units.inc'
C
      INTEGER In, Io
      COMMON /IOUNIT/ In, Io
C
      DOUBLE PRECISION cxt(MAXX,MAXT), x(MAXX), t(MAXT), c0, cn, dk, dx, 
     &                 vx
      INTEGER iplt, it, ix, j, nbc, np, np1, np2, npage, nt, nx, ifx
      REAL xsclp
C
C---EXTERNALS
      EXTERNAL OFILE, TITLE, CNRMT1, CNRMT3, READXT, PLOTNC, WCONHD
C
C     PROGRAM VARIABLES
C
C     C0       SOLUTE CONCENTRATION AT THE INFLOW BOUNDARY [M/L**3]
C     DX       LONGITUDINAL DISPERSION COEFFICIENT [L**2/T]
C     VX       GROUND-WATER VELOCITY IN X-DIRECTION [L/T]
C     DK       FIRST-ORDER SOLUTE DECAY CONSTANT [1/T]
C     X        X-POSITION AT WHICH CONCENTRATION IS EVALUATED [L]
C     T        TIME AT WHICH CONCENTRATION IS EVALUATED [T]
C     CN       NORMALIZED CONCENTRATION C/C0 [DIMENSIONLESS]
C     CXT      SOLUTE CONCENTRATION C(X,T) [M/L**3]
C
C     NBC      SOURCE BOUNDARY CONDITION TYPE (1 OR 3)
C     NX       NUMBER OF X-POSITIONS AT WHICH SOLUTION IS EVALUATED
C     NT       NUMBER OF TIME VALUES AT WHICH SOLUTION IS EVALUATED
C     IPLT     PLOT CONTROL. IF IPLT>0, CONCENTRATION PROFILES ARE PLOTTED
C
      Versn = 
     & '$Id: seminf.f,v 1.1 1996/04/03 20:09:55 rsregan Exp rsregan $'
      Versn = 
     &    '@(#)SEMINF - Analytical solution for 1-D GW solute transport'
      Versn = '@(#)SEMINF - in a semi-infinite system with uniform flow'
      Versn = '@(#)SEMINF - USGS TWRI, book 3, chap. B7, E.J. Wexler'
      Versn = '@(#)SEMINF - Contact: h2osoft@usgs.gov'
      Versn = '@(#)SEMINF - Version: 1.1x 1996/04/03'
C
C---DEFINE INPUT/OUTPUT FILES AND PRINT TITLE PAGE
      CALL OFILE
      CALL TITLE
      WRITE (Io,9020)
C
C---READ IN MODEL PARAMETERS
      READ (In,9005) nbc, nx, nt, iplt
      IF (nbc.EQ.1) WRITE (Io,9025) 'FIRST'
      IF (nbc.EQ.3) WRITE (Io,9025) 'THIRD'
      WRITE (Io,9030) nx, nt
      READ (In,9010) Cunits, Vunits, Dunits, Kunits, Lunits, Tunits
      READ (In,9015) c0, vx, dx, dk, xsclp
      WRITE (Io,9035) c0, Cunits, vx, Vunits, dx, Dunits, dk, Kunits, 
     &                xsclp
      CALL READXT(nx,nt,x,t)
C
C---BEGIN TIME LOOP
      DO 20 it = 1, nt
C
C---BEGIN X-COORDINATE LOOP
        DO 10 ix = 1, nx
C
C---CALL ROUTINE TO CALCULATE NORMALIZED CONCENTRATION
C---BASED ON TYPE OF BOUNDARY CONDITION SPECIFIED
          IF (nbc.EQ.1) CALL CNRMT1(dk,t(it),x(ix),dx,vx,cn)
          IF (nbc.EQ.3) CALL CNRMT3(dk,t(it),x(ix),dx,vx,cn)
          cxt(ix,it) = cn*c0
   10   CONTINUE
C
C---CONVERT X AND C TO SINGLE PRECISION AND DIVIDE BY C0 TO
C---PLOT NORMALIZED CONCENTRATION PROFILE FOR EACH TIME VALUE.
        IF (iplt.GT.0) CALL PLOTNC(nx,nt,it,x,t(it),cxt,c0,xsclp)
   20 CONTINUE
C
C---PRINT OUT TABLES OF CONCENTRATION VALUES
      npage = 1 + (nt-1)/9
      WRITE (Io,9040) Tunits
      ifx = 0
      DO 40 np = 1, npage
        np1 = (np-1)*9
        np2 = 9
        IF ((np1+np2).GT.nt) np2 = nt - np1
        CALL WCONHD(ifx,np1,np2,t)
        ifx = 45
        DO 30 ix = 1, nx
          WRITE (Io,9045) x(ix), (cxt(ix,np1+j),j=1,np2)
          CALL WCONHD(ix,np1,np2,t)
   30   CONTINUE
   40 CONTINUE
C
      CLOSE (In)
      CLOSE (Io)
      STOP
C
C---FORMAT STATEMENTS
 9005 FORMAT (20I4)
 9010 FORMAT (8A10)
 9015 FORMAT (8F10.0)
 9020 FORMAT (/////31X,'ANALYTICAL SOLUTION TO THE ONE-DIMENSIONAL',/,
     &        29X,'ADVECTIVE-DISPERSIVE SOLUTE TRANSPORT EQUATION',/,
     &        39X,'FOR A SEMI-INFINITE SYSTEM',////,41X,'INPUT DATA',
     &        /,41X,10('-'))
 9025 FORMAT (/,26X,A,'-TYPE BOUNDARY CONDITION AT X = 0.0')
 9030 FORMAT (/,26X,'NUMBER OF X-COORDINATES (NX) =',I5,/,26X,
     &        'NUMBER OF TIME VALUES (NT) =',I5)
 9035 FORMAT (/,26X,'SOLUTE CONCENTRATION ON MODEL BOUNDARY (C0) =',
     &        1P1E13.6,1X,A10,/,26X,
     &        'GROUND-WATER VELOCITY IN X-DIRECTION (VX) =',1P1E13.6,1X,
     &        A10,/,26X,'DISPERSION IN THE X-DIRECTION (DX) =',1P1E13.6,
     &        1X,A10,/,26X,'FIRST-ORDER SOLUTE DECAY RATE (DK) =',
     &        1P1E13.6,1X,A10,/,26X,'PLOT SCALING FACTOR (XSCLP) =',
     &        1P1E13.6)
 9040 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AS A FUNCTION OF TIME',
     &        //,26X,'TIME VALUES, IN ',A10)
 9045 FORMAT (6X,F12.4,'  ! ',9F12.5)
      END
C
C
C
      SUBROUTINE CNRMT1(DK,T,X,D,V,CN)
      DOUBLE PRECISION CN, D, DK, T, V, X
C
C       SOLUTION FOR THE ONE-DIMENSIONAL SOLUTE TRANSPORT EQUATION
C       FOR A SEMI-INFINITE SYSTEM WITH A FIRST-TYPE SOURCE
C       BOUNDARY CONDITION. VALUE RETURNED IS THE NORMALIZED SOLUTE
C       CONCENTRATION AT A GIVEN X-COORDINATE AND TIME VALUE.
C       FOR NO SOLUTE DECAY, A SIMPLIFIED SOLUTION IS USED.
C
      DOUBLE PRECISION alpha, u, x1, x2, x2d, y1, y2, z1, z2
C
C---EXTERNALS
      EXTERNAL EXERFC
C
C---INTRINSICS
      DOUBLE PRECISION DSQRT
      INTRINSIC DSQRT
C
      alpha = 2.0D0*DSQRT(D*T)
C
C---SOLUTION WITH SOLUTE DECAY
      IF (DK.NE.0.0D0) THEN
        u = DSQRT(V*V+4.0D0*D*DK)
        x2d = X/(2.0D0*D)
C
C---TERM 1
        x1 = x2d*(V-u)
        y1 = (X-u*T)/alpha
C
C---TERM 2
        x2 = x2d*(V+u)
        y2 = (X+u*T)/alpha
C
C---SOLUTION WITH NO SOLUTE DECAY
      ELSE
C
C---TERM 1
        x1 = 0.0D0
        y1 = (X-V*T)/alpha
C
C---TERM 2
        x2 = X*V/D
        y2 = (X+V*T)/alpha
      ENDIF
      CALL EXERFC(x1,y1,z1)
      CALL EXERFC(x2,y2,z2)
      CN = (z1+z2)/2.0D0
C
      RETURN
      END
C
C
C
      SUBROUTINE CNRMT3(DK,T,X,D,V,CN)
      DOUBLE PRECISION CN, D, DK, T, V, X
C
C       SOLUTION FOR THE ONE-DIMENSIONAL SOLUTE TRANSPORT EQUATION
C       FOR A SEMI-INFINITE SYSTEM WITH A THIRD-TYPE SOURCE
C       BOUNDARY CONDITION. VALUE RETURNED IS THE NORMALIZED SOLUTE
C       CONCENTRATION AT A GIVEN X-COORDINATE AND TIME VALUE.
C       FOR NO SOLUTE DECAY, A SIMPLIFIED SOLUTION IS USED.
C
      DOUBLE PRECISION alpha, pi, u, vxd, x1, x2, x2d, x3, y1, y2, y3, 
     &                 z1, z2, z3
C
C---EXTERNALS
      EXTERNAL EXERFC
C
C---INTRINSICS
      DOUBLE PRECISION DSQRT, DEXP
      INTRINSIC DSQRT, DEXP
C
      alpha = 2.0D0*DSQRT(D*T)
      vxd = V*X/D
C
C---SOLUTION WITH SOLUTE DECAY
      IF (DK.NE.0.0D0) THEN
        u = DSQRT(V*V+4.0D0*D*DK)
        x2d = X/(2.0D0*D)
C
C---TERM 1
        x1 = vxd - DK*T
        y1 = (X+V*T)/alpha
        CALL EXERFC(x1,y1,z1)
        z1 = z1*2.0D0
C
C---TERM 2
        x2 = x2d*(V-u)
        y2 = (X-u*T)/alpha
        CALL EXERFC(x2,y2,z2)
        z2 = z2*(u/V-1.0D0)
C
C---TERM 3
        x3 = x2d*(V+u)
        y3 = (X+u*T)/alpha
        CALL EXERFC(x3,y3,z3)
        z3 = z3*(u/V+1.0D0)
        CN = V*V*(z1+z2-z3)/(4.0D0*D*DK)
      ELSE
C
C---SOLUTION FOR NO SOLUTE DECAY
        pi = 3.14159265358979D0
C
C---TERM 1
        x1 = 0.0D0
        y1 = (X-V*T)/alpha
        CALL EXERFC(x1,y1,z1)
        z1 = 0.50D0*z1
C
C---TERM 2
        x2 = vxd
        y2 = (X+V*T)/alpha
        CALL EXERFC(x2,y2,z2)
        z2 = z2*0.50D0*(1.0D0+V*(X+V*T)/D)
C
C---TERM 3
        z3 = DEXP(-1.0D0*y1*y1)
        z3 = z3*V*DSQRT(T/(pi*D))
        CN = z1 - z2 + z3
      ENDIF
C
      RETURN
      END
