C
C       THE FOLLOWING PARAMETER STATEMENT MUST BE CHANGED IF PROBLEM
C       DIMENSIONS ARE GREATER THAN THOSE GIVEN HERE.
C         MAXX = MAXIMUM NUMBER OF X-VALUES
C         MAXY = MAXIMUM NUMBER OF Y-VALUES
C         MAXZ = MAXIMUM NUMBER OF Z-VALUES
C         MAXT = MAXIMUM NUMBER OF TIME VALUES
C         MAXRT = MAXIMUM NUMBER OF ROOTS IN THE SERIES SUMMATION
C         MAXXY = MAXX * MAXY
C         MAXXY2 = 2 * MAXX * MAXY
C         MAXSEG = MAXIMUM NUMBER OF POINTS IN CONTOUR SEGMENT
C
      INTEGER MAXX, MAXY, MAXZ, MAXT, MAXRT, MAXXY, MAXXY2, MAXSEG
      PARAMETER (MAXX=100,MAXY=50,MAXZ=30,MAXT=20,MAXRT=1000)
      PARAMETER (MAXXY=MAXX*MAXY, MAXXY2=MAXXY*2, MAXSEG=50)
C
