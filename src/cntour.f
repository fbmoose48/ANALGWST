C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE CNTOUR                   *
C      *                                                      *
C      *            VERSION CURRENT AS OF 10/01/87            *
C      *              CODE CLEANUP 03/27/96 - RSR             *
C      *                                                      *
C       ******************************************************
C
      SUBROUTINE CNTOUR(NX,NY,IPL)
      INCLUDE 'dimens.inc'
      INCLUDE 'pltdat.inc'
      INTEGER NX, NY, IPL
C
C       THIS ROUTINE IS CALLED BY PLOT2D AND PLOT3D TO CONTOUR VALUES
C       OF NORMALIZED CONCENTRATION VALUES ON THE RECTANGULAR GRID.
C       NUMBER OF SEGMENTS DRAWN BEFORE THE CONTOUR LINE IS LABELED
C       (NUM), AND CHARACTER HEIGHT ARE SET HERE, BUT CAN BE
C       EASILY MODIFIED.
C       XPC, YPC, AND IFLAG ARE WORK ARRAYS USED BY THIS ROUTINE.
C       IFLAG MUST BE DIMENSIONED TO TWICE THE NUMBER OF RECTANGULAR
C       BLOCKS SINCE EACH BLOCK IS DIVIDED INTO TWO TRIANGLES.
C
      REAL ang, cp1, cp2, cp3, cpmax, cpmin, delv21, delv31, delv32, 
     &     delx, delx21, delx31, delx32, dely, dely21, dely31, dely32, 
     &     gdel, hite, ofset, rad, ratio, spc1, val, valinc, vmax, vmin, 
     &     x1, x2, x3, xlen, xp1, xpt, y1, y2, y3, yp1, ypt
      REAL xpc(MAXSEG), ypc(MAXSEG)
      SAVE xpc, ypc
      INTEGER ichk, idn, ieven, ifirst, ilast, ip1, ipt, iside, istart, 
     &        iup, jump, m, maxcnt, n, n1, n2, next, ng1, ng2, ng3, ntr, 
     &        num, ny2, nxy, iflag(MAXXY2)
C
      CHARACTER Psname*64, Pcolor*16
      COMMON /PLTNAM/ Psname, Pcolor
C
C---EXTERNALS
      INTEGER LENCHR
      EXTERNAL HEIGHT, NUMODE, SETCLR, ANGLE, RLREAL, CURVE, RESET,
     &         LENCHR
C
C---INTRINSICS
      INTEGER MOD, INT
      REAL ATAN2, COS, SIN, AMAX1, AMIN1, ABS, SQRT
      INTRINSIC ATAN2, COS, SIN, AMAX1, AMIN1, ABS, SQRT, MOD, INT
C
      num = 40
      hite = 0.10
      rad = 57.29578
C
C---COMPUTE SPACE NEEDED FOR CONTOUR LABEL
      CALL HEIGHT(hite)
      CALL NUMODE('NOLEADSPACE')
      spc1 = (IPL+2)*hite
      CALL SETCLR('RED')
C
C---FIND MIN AND MAX VALUES AND NUMBER OF CONTOURS
      vmin = Cp(1)
      vmax = vmin
      nxy = NX*NY
      DO 10 n = 2, nxy
        val = Cp(n)
        IF (val.GT.vmax) vmax = val
        IF (val.LT.vmin) vmin = val
   10 CONTINUE
      gdel = vmax - vmin
      maxcnt = INT(gdel/Delta) + 1
C
C---FIND FIRST CONTOUR VALUE
      valinc = INT(vmin/Delta)*Delta
C
C---SET UP MASTER LOOP FOR ALL CONTOURS
C---EACH RECTANGULAR BLOCK IS DIVIDED INTO TWO TRIANGLES.
C---CONTOURS ARE DRAWN BY LINEARLY INTERPOLATING ACROSS EACH TRIANGLE.
      ntr = (NX-1)*(NY-1)*2
      ny2 = (NY-1)*2
      DO 80 m = 1, maxcnt
        valinc = valinc + Delta
        IF (valinc.GT.vmax) GOTO 80
C
C---INITIALIZE FLAGS ON TRIANGLES WITH CONTOURS PASSING THROUGH
        ifirst = 0
        DO 20 n = ny2+1, ntr
          n1 = (n-1)/ny2
          n2 = (n-(n1*ny2)+1)/2
          ng1 = n1*NY + n2
          ng2 = ng1 + NY
          ng3 = ng1 + 1
          IF (MOD(n,2).EQ.0) THEN
            ng1 = ng1 + 1
            ng2 = ng1 + NY - 1
            ng3 = ng1 + NY
          ENDIF
          iflag(n) = 0
          cp1 = Cp(ng1)
          cp2 = Cp(ng2)
          cp3 = Cp(ng3)
          cpmax = AMAX1(cp1,cp2,cp3)
          cpmin = AMIN1(cp1,cp2,cp3)
          IF (cpmax.GE.valinc .AND. cpmin.LE.valinc) THEN
            iflag(n) = 1
            IF (ifirst.EQ.0) ifirst = n
            ilast = n
          ENDIF
   20   CONTINUE
C
C---LOOP THROUGH ALL FLAGGED TRIANGLES
        DO 70 n = ifirst, ilast
          IF (iflag(n).EQ.0) GOTO 70
C
C---START UP A NEW CONTOUR SEGMENT
          istart = 0
          ichk = 0
          ipt = 1
          next = n
C
C---CONTROL LOOP FOR FOLLOWING CONTOUR SEGMENT THROUGH ELEMENTS
   30     n1 = (next-1)/ny2
          n2 = (next-(n1*ny2)+1)/2
          ieven = 0
          IF (MOD(next,2).EQ.0) ieven = 1
          ng1 = n1*NY + n2
          ng2 = ng1 + NY
          ng3 = ng1 + 1
          IF (ieven.EQ.1) THEN
            ng1 = ng1 + 1
            ng2 = ng1 + NY - 1
            ng3 = ng1 + NY
          ENDIF
          cp1 = Cp(ng1)
          cp2 = Cp(ng2)
          cp3 = Cp(ng3)
          delv21 = cp2 - cp1
          delv31 = cp3 - cp1
          delv32 = cp3 - cp2
          x1 = Xp(n1)
          x2 = Xp(n1+1)
          x3 = Xp(n1)
          y1 = Yp(n2)
          y2 = Yp(n2)
          y3 = Yp(n2+1)
          IF (ieven.EQ.1) THEN
            x3 = Xp(n1+1)
            y1 = Yp(n2+1)
          ENDIF
          delx21 = x2 - x1
          delx31 = x3 - x1
          delx32 = x3 - x2
          dely21 = y2 - y1
          dely31 = y3 - y1
          dely32 = y3 - y2
C
C---RESET FLAG, INCREMENT COUNTER, AND FIND NEIGHBORING ELEMENTS
          iflag(next) = 0
          ipt = ipt + 1
          iup = next + 1
          idn = next - 1
          iside = next - ny2 + 1
          IF (ieven.EQ.1) iside = next + ny2 - 1
C
C---SPECIAL CASE 1.  CONTOURS ALONG ELEMENT SIDES
          IF (cp1.EQ.cp2 .AND. cp1.EQ.valinc) THEN
            next = -1
            xpc(1) = x1
            ypc(1) = y1
            xpc(2) = x2
            ypc(2) = y2
            IF (cp3.NE.cp1) GOTO 40
            ipt = 4
            xpc(3) = x3
            ypc(3) = y3
            xpc(4) = x1
            ypc(4) = y1
          ELSEIF (cp1.EQ.cp3 .AND. cp1.EQ.valinc) THEN
            next = -1
            xpc(1) = x3
            ypc(1) = y3
            xpc(2) = x1
            ypc(2) = y1
          ELSEIF (cp2.EQ.cp3 .AND. cp2.EQ.valinc) THEN
            next = -1
            xpc(1) = x2
            ypc(1) = y2
            xpc(2) = x3
            ypc(2) = y3
          ENDIF
          IF (next.NE.-1) THEN
C
C---SPECIAL CASE 2.  SINGLE POINTS EQUAL TO CONTOUR VALUE
C---CHECK NODE 1 FIRST
            jump = 0
C
C---CHECK IF SEGMENT DEAD-ENDS AT NODE 1
            IF (cp1.EQ.valinc .AND. istart.EQ.2) THEN
              next = -1
              xpc(ipt) = x1
              ypc(ipt) = y1
C
C---OTHERWISE, START NEW SEGMENT AT NODE 1
            ELSEIF (cp1.EQ.valinc .AND. istart.EQ.0) THEN
              IF ((cp2.GT.valinc .AND. cp3.GT.valinc) .OR. 
     &            (cp2.LT.valinc .AND. cp3.LT.valinc)) GOTO 70
              jump = 1
              xpc(1) = x1
              ypc(1) = y1
              next = iup
              istart = 1
              IF (ieven.EQ.1) THEN
                next = iside
                istart = 3
              ENDIF
              ratio = (valinc-cp2)/delv32
              xpc(2) = x2 + ratio*delx32
              ypc(2) = y2 + ratio*dely32
C
C---NEXT CHECK NODE 2
            ELSEIF (cp2.EQ.valinc .AND. istart.EQ.3) THEN
              next = -1
              xpc(ipt) = x2
              ypc(ipt) = y2
            ELSEIF (cp2.EQ.valinc .AND. istart.EQ.0) THEN
              IF ((cp1.GT.valinc .AND. cp3.GT.valinc) .OR. 
     &            (cp1.LT.valinc .AND. cp3.LT.valinc)) GOTO 70
              jump = 1
              xpc(1) = x2
              ypc(1) = y2
              next = iside
              istart = 2
              IF (ieven.EQ.1) THEN
                next = iup
                istart = 2
              ENDIF
              ratio = (valinc-cp1)/delv31
              xpc(2) = x1 + ratio*delx31
              ypc(2) = y1 + ratio*dely31
C
C---NEXT CHECK NODE 3
            ELSEIF (cp3.EQ.valinc .AND. istart.EQ.1) THEN
              next = -1
              xpc(ipt) = x3
              ypc(ipt) = y3
            ELSEIF (cp3.EQ.valinc .AND. istart.EQ.0) THEN
              IF ((cp1.GT.valinc .AND. cp2.GT.valinc) .OR. 
     &            (cp1.LT.valinc .AND. cp2.LT.valinc)) GOTO 70
              jump = 1
              xpc(1) = x3
              ypc(1) = y3
              next = idn
              istart = 3
              IF (ieven.EQ.1) istart = 2
              ratio = (valinc-cp1)/delv21
              xpc(2) = x1 + ratio*delx21
              ypc(2) = y1 + ratio*dely21
            ENDIF
            IF (jump.NE.1 .AND. next.NE.-1) THEN
C
C---ROUTINE FOR DRAWING CONTOUR SEGMENT THROUGH TYPICAL ELEMENTS
C---START SEGMENT, IF NECCESSARY
              IF (istart.EQ.0) THEN
C
C---CHECK FOR CONTOUR ENTERING ON BOTTOM OF TRIANGLE (SIDE 1-2)
                IF ((cp1.GT.valinc .AND. cp2.LT.valinc) .OR. 
     &              (cp1.LT.valinc .AND. cp2.GT.valinc)) THEN
                  istart = 1
                  ratio = (valinc-cp1)/delv21
                  xpc(1) = x1 + ratio*delx21
                  ypc(1) = y1 + ratio*dely21
C
C---CONTOUR MUST START ON SIDE 2 OR 3. PICK STARTING POINT
C---BASED ON WHETHER ELEMENT IS ODD OR EVEN
C
C---FOR ODD ELEMENT, START ON SIDE 1-3
                ELSEIF (MOD(next,2).NE.0) THEN
                  istart = 3
                  ratio = (valinc-cp1)/delv31
                  xpc(1) = x1 + ratio*delx31
                  ypc(1) = y1 + ratio*dely31
C
C---IF EVEN,START CONTOUR ON SIDE 2-3
                ELSE
                  istart = 2
                  ratio = (valinc-cp2)/delv32
                  xpc(1) = x2 + ratio*delx32
                  ypc(1) = y2 + ratio*dely32
                ENDIF
              ENDIF
C
C---CHECK FOR CONTOUR ENTERING ON BOTTOM OF TRIANGLE (SIDE 1-2)
              IF (istart.EQ.1) THEN
C
C---CHECK WHETHER CONTOUR EXITS SIDE OR TOP
                IF ((cp3.GT.valinc .AND. cp1.LT.valinc) .OR. 
     &              (cp3.LT.valinc .AND. cp1.GT.valinc)) THEN
C
C---CONTOUR MUST EXIT BETWEEN NODES 1 AND 3
                  next = iside
                  istart = 2
                  IF (ieven.EQ.1) THEN
                    next = iup
                    istart = 1
                  ENDIF
                  ratio = (valinc-cp1)/delv31
                  xpc(ipt) = x1 + ratio*delx31
                  ypc(ipt) = y1 + ratio*dely31
C
C---CONTOUR MUST EXIT BETWEEN NODES 2 AND 3
                ELSE
                  next = iup
                  istart = 1
                  IF (ieven.EQ.1) THEN
                    next = iside
                    istart = 3
                  ENDIF
                  ratio = (valinc-cp2)/delv32
                  xpc(ipt) = x2 + ratio*delx32
                  ypc(ipt) = y2 + ratio*dely32
                ENDIF
C
C---CHECK FOR CONTOUR ENTERING ON SIDE 2-3
              ELSEIF (istart.EQ.2) THEN
C
C---CHECK WHETHER CONTOUR EXITS BOTTOM OR SIDE 1-3
                IF ((cp3.GT.valinc .AND. cp1.LT.valinc) .OR. 
     &              (cp3.LT.valinc .AND. cp1.GT.valinc)) THEN
C
C---CONTOUR MUST EXIT BETWEEN NODES 1 AND 3
                  next = iside
                  istart = 2
                  IF (ieven.EQ.1) THEN
                    next = iup
                    istart = 1
                  ENDIF
                  ratio = (valinc-cp1)/delv31
                  xpc(ipt) = x1 + ratio*delx31
                  ypc(ipt) = y1 + ratio*dely31
C
C---CONTOUR MUST EXIT BETWEEN NODES 1 AND 2
                ELSE
                  next = idn
                  istart = 3
                  IF (ieven.EQ.1) istart = 2
                  ratio = (valinc-cp1)/delv21
                  xpc(ipt) = x1 + ratio*delx21
                  ypc(ipt) = y1 + ratio*dely21
                ENDIF
C
C---CHECK FOR START OF CONTOUR SEGMENT ALONG SIDE 3-1
              ELSEIF (istart.EQ.3) THEN
C
C---CHECK WHETHER CONTOUR EXITS BOTTOM OR SIDE 2-3
                IF ((cp2.GT.valinc .AND. cp1.LT.valinc) .OR. 
     &              (cp2.LT.valinc .AND. cp1.GT.valinc)) THEN
C
C---CONTOUR MUST EXIT BETWEEN NODES 1 AND 2
                  next = idn
                  istart = 3
                  IF (ieven.EQ.1) istart = 2
                  ratio = (valinc-cp1)/delv21
                  xpc(ipt) = x1 + ratio*delx21
                  ypc(ipt) = y1 + ratio*dely21
C
C---CONTOUR MUST EXIT BETWEEN NODES 2 AND 3
                ELSE
                  next = iup
                  istart = 1
                  IF (ieven.EQ.1) THEN
                    next = iside
                    istart = 3
                  ENDIF
                  ratio = (valinc-cp2)/delv32
                  xpc(ipt) = x2 + ratio*delx32
                  ypc(ipt) = y2 + ratio*dely32
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C---CHECK IF CONTOUR LINE SEGMENT HAS ENDED
   40     IF (next.NE.-1) THEN
C
C---CHECK IF CONTOUR LINE SEGMENT HAS LEFT BOUNDARY
            IF (next.GT.0 .AND. next.LE.ntr) THEN
              IF (MOD(next,ny2).NE.0 .OR. istart.NE.3) THEN
                IF (MOD((next-1),ny2).NE.0 .OR. istart.NE.1) THEN
C
C---CHECK FOR END OF CLOSED CONTOUR LOOP
                  IF (iflag(next).EQ.0) GOTO 60
C
C---OTERWISE, CONTINUE SEGMENT, OR BREAK AFTER 'NUM' SEGMENTS
                  IF (ipt.LT.num) GOTO 30
                  ichk = 1
                ENDIF
              ENDIF
            ENDIF
C
C---BLANK OUT SPACE AT END OF SEGMENT TO WRITE LABEL
            IF (ipt.GE.num) THEN
              xpt = xpc(ipt)
              ypt = ypc(ipt)
              ip1 = ipt
C
C---CHECK IF ENOUGH SPACE IS CREATED BY BLANKING OUT ONE POINT
   50         ip1 = ip1 - 1
              IF (ip1.GT.1) THEN
                xp1 = xpc(ip1)
                yp1 = ypc(ip1)
                delx = (xpt-xp1)/Xsclp
                dely = (ypt-yp1)/Ysclp
                xlen = SQRT(delx*delx+dely*dely)
C
C---IF NOT, DROP ANOTHER POINT ON CURVE
                IF (xlen.LT.spc1) GOTO 50
              ENDIF
C
C---MAKE SURE LABELS ARE RIGHT-SIDE UP
              ofset = (xlen-spc1)/2.0
              ang = 90.
              IF (dely.LT.0.0) ang = 270.
              IF (delx.NE.0.0) ang = ATAN2(dely,delx)*rad
              IF (ABS(ang).LE.90.0) THEN
                CALL ANGLE(ang)
                xp1 = xp1 + (ofset*COS(ang/rad)+hite*SIN(ang/rad)/2.0)
     &                *Xsclp
                yp1 = yp1 + (ofset*SIN(ang/rad)-hite*COS(ang/rad)/2.0)
     &                *Ysclp
                CALL RLREAL(valinc,IPL,xp1,yp1)
              ELSE
                ang = ang - 180.
                CALL ANGLE(ang)
                xpt = xpt + (ofset*COS(ang/rad)+hite*SIN(ang/rad)/2.0)
     &                *Xsclp
                ypt = ypt + (ofset*SIN(ang/rad)-hite*COS(ang/rad)/2.0)
     &                *Ysclp
                CALL RLREAL(valinc,IPL,xpt,ypt)
              ENDIF
              CALL RESET('ANGLE')
              ipt = ip1
            ENDIF
          ENDIF
C
C---DRAW CONTOUR SEGMENT
   60     CALL CURVE(xpc,ypc,ipt,0)
C
C---EITHER CONTINUE CONTOUR SEGMENT WHERE IT LEFT OFF
          IF (ichk.EQ.1) THEN
            ichk = 0
            istart = 0
            ipt = 1
            GOTO 30
          ENDIF
C
C---OR START SEARCH FOR NEXT SEGMENT
   70   CONTINUE
   80 CONTINUE
      CALL RESET('HEIGHT')
      CALL RESET('NUMODE')
      CALL SETCLR(Pcolor(1:LENCHR(Pcolor)))
      RETURN
      END
