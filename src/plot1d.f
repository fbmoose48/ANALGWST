C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE PLOT1D                   *
C      *                                                      *
C      *            VERSION CURRENT AS OF 10/01/87            *
C      *              CODE CLEANUP 03/27/96 - RSR             *
C      *                                                      *
C       ******************************************************
C
C       THIS ROUTINE PLOTS CONCENTRATION VS. DISTANCE AT EACH OF THE
C       TIMES SPECIFIED IN THE INPUT DATA.  THE ROUTINE USES DISSPLA
C       SOFTWARE PLOT CALLS.
C
      SUBROUTINE PLOT1D(X,C,NX,T,IT,NT,TUNITS,LUNITS,XSCLP)
      INTEGER NX, IT, NT
      REAL X(NX), C(NX), T, XSCLP
      CHARACTER*10 LUNITS, TUNITS
C
      REAL high, hite, wide, x1, x11, xmax, xmin, xp, xpm, ymax, yp, 
     &     ypm, ypos1, xpos1, xaxis, yaxis, xpos, ypos
      INTEGER ipl, id
      CHARACTER lab*17, lab1*27, labx*26, labx1*36, laby*24
C
      SAVE xpos, ypos, id
C
      CHARACTER*61 Head
      COMMON /TITLES/ Head(4)
C
C---EXTERNALS
      EXTERNAL DGDISS, DONEPL, PAGE, AREA2D, HEADIN, XNAME, YNAME,
     &         INTAXS, YAXANG, XREVTK, GRAF, RESET, XNONUM, XGRAXS,
     &         YNONUM, YGRAXS, HEIGHT, MESSAG, BLREC, BLKEY, MARKER,
     &         CURVE, BLOFF, REALNO, BLON, ENDPL
C
C---INTRINSICS
      INTEGER INT
      INTRINSIC INT
C
C---INITIALIZE PLOT - SCALE BASED ON MAXIMUM X-DISTANCE
      hite = 0.1
      IF (IT.EQ.1) THEN
        CALL DGDISS
        x1 = X(NX) - X(1)
        xaxis = INT(x1/XSCLP)
        x11 = x1/XSCLP
        IF ((x11-xaxis).GT.0.0) xaxis = xaxis + 1.0
        yaxis = 10.0
        xpm = xaxis + 1.5
        ypm = 12.2
        CALL PAGE(xpm,ypm)
        CALL AREA2D(xaxis,yaxis)
        CALL HEADIN(Head(1),100,1.,4)
        CALL HEADIN(Head(2),100,1.,4)
        CALL HEADIN(Head(3),100,1.,4)
        CALL HEADIN(Head(4),100,1.,4)
C---LABEL AXES
        labx = 'DISTANCE ALONG X-AXIS, IN '
        labx1 = labx//LUNITS
        laby = 'NORMALIZED CONCENTRATION'
        CALL XNAME(labx1,36)
        CALL YNAME(laby,24)
C---DRAW AND NUMBER AXES
        CALL INTAXS
        CALL YAXANG(0.)
        CALL XREVTK
        CALL YREVTK
        xmin = X(1)
        xmax = XSCLP*xaxis
        ymax = 1.0
        CALL GRAF(xmin,XSCLP,xmax,0.0,0.1,ymax)
        CALL RESET('XREVTK')
        CALL RESET('YREVTK')
C---DRAW EXTRA AXIS TO CLOSE BOX
        CALL XNONUM
        CALL XGRAXS(xmin,XSCLP,xmax,xaxis,' ',1,0.0,yaxis)
        CALL YNONUM
        CALL YGRAXS(0.0,0.1,ymax,yaxis,' ',1,xaxis,0.0)
        CALL RESET('XNONUM')
        CALL RESET('YNONUM')
C---BEGIN LEGEND
        xpos = xaxis - .85*hite*(27+4) - .1
        ypos = yaxis - .1 - 2.0*hite
        CALL HEIGHT(0.1)
        lab = 'ELAPSED TIME, IN '
        lab1 = lab//TUNITS
        CALL MESSAG(lab1,27,xpos,ypos)
        ypos = ypos - .5*hite
C---BLANK OUT AREA FOR MESSAGE
        wide = hite*0.85*35.
        high = hite*1.5*(NT+3)
        xpos = xaxis - wide - 0.1
        ypos1 = yaxis - high - 0.1
        CALL BLREC(xpos,ypos1,wide,high,1.0)
        CALL BLKEY(id)
        xpos = xaxis - 2.75
      ENDIF
C
C---DRAW PLOT OF C VS X
      CALL MARKER(IT)
      CALL CURVE(X,C,NX,1)
      CALL MARKER(IT)
C---PLACE LABEL IN BOX
      CALL BLOFF(id)
      ypos = ypos - 1.5*hite
      xpos1 = xpos + 3.*.85*hite
      xp = xpos1*XSCLP
      yp = (ypos+0.05)/10.
      CALL CURVE(xp,yp,1,-1)
      xpos1 = xpos + 6*.85*hite
      CALL MESSAG('TIME =',6,xpos1,ypos)
      ipl = 104
      CALL REALNO(T,ipl,'ABUT','ABUT')
      CALL BLON(id)
C
C---CLOSE PLOT FILE
      IF (IT.EQ.NT) THEN
        CALL ENDPL(0)
        CALL DONEPL
      ENDIF
      RETURN
      END
