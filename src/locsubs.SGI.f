      SUBROUTINE SYSDAT
     O                 (YR,MO,DA)
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
C     + + + LOCAL VARIABLES + + +
      CHARACTER*24 DATBUF
C
C     + + + EXTERNALS + + +
      CHARACTER*24 FDATE
      EXTERNAL  FDATE
C     FDATE IS A SILICON GRAPHICS SYSTEM SUBROUTINE
C
C     + + + END SPECIFICATIONS + + +
C
      DATBUF=FDATE()
      READ(DATBUF,'(5X,I2,1X,I2,12X,I2)') MO,YR,DA
      RETURN
      END
C
C
C
      SUBROUTINE SYSTIM
     O                 ( IHR, IMN, ISEC )
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED TO RETRIEVE THE SYSTEM TIME.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IHR, IMN, ISEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IHR     - Hour since midnight
C     IMN     - Minute since hour
C     ISEC    - Seconds since minute
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*24 DATBUF
C
C     + + + EXTERNALS + + +
      CHARACTER*24 FDATE
      EXTERNAL  FDATE
C     FDATE IS A SILICON GRAPHICS SYSTEM SUBROUTINE
C
C     + + + END SPECIFICATIONS + + +
C
      DATBUF=FDATE()
      READ(DATBUF,'(11X,I2,1X,I2,1X,I2)') IHR, IMN, ISEC
      RETURN
      END
C
C
C
      SUBROUTINE SYSSEC
     O                 ( SEC )
C
C     + + + PURPOSE + + +
C     THIS SUBROUTINE IS USED TO RETRIEVE THE SYSTEM TIME.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SEC    - NUMBER OF SECONDS SINCE MIDNIGHT TIMES 100
C
C     + + + LOCAL VARIABLES + + +
      INTEGER hh, mm, ss
C
C     + + + EXTERNALS + + +
      EXTERNAL  SYSTIM
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SYSTIM(hh,mm,ss)
      SEC = hh*3600 + mm*60 + ss
      SEC = SEC*100
      RETURN
      END
