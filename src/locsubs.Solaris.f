      SUBROUTINE SYSDAT
     O                 (YR,MO,DA)
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED ON SUN COMPUTERS TO REPLACE THE AMDAHL
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
C     + + + LOCAL VARIABLES + + +
      INTEGER STRING(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  IDATE
C     IDATE IS A STANDARD SUNOS SUN SYSTEM SUBROUTINE
C     THE SUN VMS VERSION OF IDATE IS CALL IDATE(M,D,Y)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL IDATE(STRING)
      YR=MOD(STRING(3),100)
      MO=STRING(2)
      DA=STRING(1)
      RETURN
      END
C
C
C
      SUBROUTINE SYSTIM
     O                 (HR, MN, SEC)
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED TO RETRIEVE THE SYSTEM TIME.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER HR, MN, SEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - Hour
C     MN     - Minute
C     SEC    - Seconds
C
C     + + + LOCAL VARIABLES + + +
      INTEGER string(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITIME
C     ITIME IS A SUN SYSTEM SUBROUTINE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ITIME(string)
C     STRING(1) = HOUR SINCE MIDNIGHT
C     STRING(2) = MINUTE SINCE HOUR
C     STRING(3) = SEC SINCE MINUTE
      HR = string(1)
      MN = string(2)
      SEC = string(3)
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
C     OF SECONDS SINCE MIDNIGHT TIMES 100.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SEC    - NUMBER OF SECONDS SINCE MIDNIGHT TIMES 100
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ihr, imn, isec
C
C     + + + EXTERNALS + + +
      EXTERNAL  SYSTIM
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SYSTIM(ihr,imn,isec)
      SEC=(ihr*3600+imn*60+isec)*100
      RETURN
      END
