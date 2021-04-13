      SUBROUTINE SYSDAT
     O                 ( YR, MO, DA )
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED TO REPLACE THE AMDAHL
C     SYSTEM CALLS TO SYSDAT, WHICH RETRIEVE THE SYSTEM DATE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C
C     + + + EXTERNALS + + +
      EXTERNAL IDATE
C     IDATE IS A DG SYSTEM SUBROUTINE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL IDATE(MO,DA,YR)
      RETURN
      END
C
C
C
      SUBROUTINE SYSTIM
     O                 ( HR, MN, SE )
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED TO RETRIEVE THE SYSTEM TIME.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER HR, MN, SE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - hour
C     MN     - minute
C     SE     - second
C
C     + + + EXTERNALS + + +
      EXTERNAL SYSSEC
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SYSSEC(SE)
C---round to nearest second
      SE = (SE+49)/100
      HR = SE/3600
      SE = SE-HR*3600
      MN = SE/60
      SE = SE-MN*60
      RETURN
      END
C
C
C
      SUBROUTINE SYSSEC
     O                 ( SEC )
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED TO RETRIEVE THE SYSTEM TIME IN TERMS
C     OF SECONDS ELAPSED SINCE MIDNIGHT TIME 100.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SEC    - NUMBER OF SECONDS SINCE MIDNIGHT TIMES 100
C
C     + + + EXTERNALS + + +
      REAL SECNDS
      EXTERNAL SECNDS
C     SECNDS IS A DG SYSTEM SUBROUTINE
C
C     + + + INTRINSICS + + +
      INTEGER INT
      INTRINSIC INT
C
C     + + + END SPECIFICATIONS + + +
C
      SEC = INT(SECNDS(0.0)+0.005)
      SEC = SEC*100
      RETURN
      END
