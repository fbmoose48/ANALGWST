C
C RDXYT, WCONH2, PLTNC2 for use with 2-D programs (point2, stripf, stripi, and gauss)
C
      SUBROUTINE RDXYT(NX,NY,NT,XCOORD,YCOORD,TIME)
C
C     Read X-coordinates, Y-coordinates and times at which solute
C     concentrations are computed.
C
      INTEGER NX, NY, NT, i
      DOUBLE PRECISION XCOORD(NX), YCOORD(NY), TIME(NT)
C
      INCLUDE 'units.inc'
C
      INTEGER In, Io
      COMMON /IOUNIT/ In, Io
C
      READ (In,9005) (XCOORD(i),i=1,NX)
      WRITE (Io,9010) 'X', Lunits
      WRITE (Io,9015) (XCOORD(i),i=1,NX)
C
      READ (In,9005) (YCOORD(i),i=1,NY)
      WRITE (Io,9010) 'Y', Lunits
      WRITE (Io,9015) (YCOORD(i),i=1,NY)
C
      READ (In,9005) (TIME(i),i=1,NT)
      WRITE (Io,9020) Tunits
      WRITE (Io,9015) (TIME(i),i=1,NT)
C
      RETURN
C
 9005 FORMAT (8F10.0)
 9010 FORMAT (/,26X,A,'-COORDINATES AT WHICH SOLUTE CONCENTRATIONS ',
     &        'WILL BE CALCULATED, IN ',A10,/,26X,78('-')/)
 9015 FORMAT (6X,8F12.4)
 9020 FORMAT (/,26X,
     &    'TIMES AT WHICH SOLUTE CONCENTRATIONS WILL BE CALCULATED, IN '
     &    ,A10,/,26X,70('-')/)
      END
C
C
C
      SUBROUTINE WCONH2(IX,NP1,NP2,TIME,Y)
C
C     Print header for solute concentration tables.
C
      INTEGER IX, NP1, NP2
      DOUBLE PRECISION TIME, Y(*)
C
      INCLUDE 'units.inc'
C
      INTEGER In, Io
      COMMON /IOUNIT/ In, Io
C
      INTEGER j, MOD
      INTRINSIC MOD
C
      IF (MOD(IX,45).EQ.0) THEN
        IF (IX.NE.0) WRITE (Io,9005) TIME, Tunits, Lunits
        WRITE (Io,9010) (Y(NP1+j),j=1,NP2)
        WRITE (Io,9015) Cunits, Lunits
      ELSE
        IF (MOD(IX,5).EQ.0) WRITE (Io,9020)
      ENDIF
C
      RETURN
C
 9005 FORMAT ('1',//,16X,'SOLUTE CONCENTRATION AT TIME =',F12.4,1X,A10,
     &        5X,'(CONTINUED)',//,26X,'Y-COORDINATE, IN ',A10)
 9010 FORMAT (21X,9F12.4)
 9015 FORMAT (20X,'*',108('-'),/,5X,'X-COORDINATE,  !',44X,
     &        'SOLUTE CONCENTRATION, IN ',A10,/,5X,'IN ',A10,'  !',/,
     &        20X,'!')
 9020 FORMAT (20X,'!')
      END
C
C
C
      SUBROUTINE PLTNC2(NX,NY,NT,IT,XCOORD,YCOORD,TIME,CXY,C0)
C
C     CONVERT X AND Y TO SINGLE PRECISION AND DIVIDE BY THE
C     PLOT SCALING FACTORS. CONVERT C(X,Y) AND DIVIDE BY C0 TO PLOT
C     CONTOUR MAPS OF NORMALIZED CONCENTRATION FOR EACH TIME VALUE.
C
      INCLUDE 'dimens.inc'
      INCLUDE 'pltdat.inc'
C
      INTEGER NX, NY, NT, IT
      DOUBLE PRECISION XCOORD(MAXX), YCOORD(MAXY), TIME, CXY(MAXX,MAXY), 
     &                 C0
C
      INTEGER i, ip, j
      REAL tp, zp
C
C---EXTERNALS
      EXTERNAL PLOT2D
C
C---INTRINSICS
      REAL SNGL
      INTRINSIC SNGL
C
      DO 20 i = 1, NX
        ip = (i-1)*NY
        Xp(i) = SNGL(XCOORD(i))
        DO 10 j = 1, NY
          Cp(ip+j) = SNGL(CXY(i,j)/C0)
   10   CONTINUE
   20 CONTINUE
      DO 30 j = 1, NY
        Yp(j) = SNGL(YCOORD(j))
   30 CONTINUE
      tp = SNGL(TIME)
      zp = 0.0
      ip = 0
      CALL PLOT2D(zp,tp,NX,NY,IT,NT,ip)
      RETURN
      END
