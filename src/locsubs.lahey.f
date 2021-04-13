      SUBROUTINE SYSDAT(YR,MO,DA)
C
C      SYSDAT IS A LAHEY FORTRAN SYSTEM SUBROUTINE
C
      CHARACTER*8 CDATE
      INTEGER   YR,MO,DA
      CALL DATE(CDATE)
      READ(CDATE,'(2(I2,1X),I2)') MO,DA,YR
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
C     IHR    - Hour since midnight
C     IMN    - Minute since hour
C     ISEC   - Second since minute
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*11 CTIME
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIME
C     TIME IS A LAHEY FORTRAN SYSTEM SUBROUTINE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIME(CTIME)
      READ(CTIME,'(2(I2,1X),I2)') IHR, IMN, ISEC
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
      CHARACTER*11 CTIME
      INTEGER   IH,IM,IS,I100
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIME
C     TIME IS A LAHEY FORTRAN SYSTEM SUBROUTINE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIME(CTIME)
      READ(CTIME,'(3(I2,1X),I2)') IH,IM,IS,I100
      SEC=(IH*3600+IM*60+IS)*100+I100
      RETURN
      END
