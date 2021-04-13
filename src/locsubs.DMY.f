      SUBROUTINE SYSDAT
     O                 ( YR, MO, DA )
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
      INTEGER ida
      SAVE ida
      DATA ida/24/
C
C     + + + END SPECIFICATIONS + + +
C
      YR=94
      MO=12
      DA=ida
      ida = ida+1
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
C     + + +EXTERNALS + + +
      EXTERNAL SYSSEC
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SYSSEC(ISEC)
      ISEC = (ISEC+49)/100
      IHR = ISEC/3600
      ISEC = ISEC-IHR*3600
      IMN = ISEC/60
      ISEC = ISEC-IMN*60
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
C     + + + INTRINSICS + + +
      INTEGER MOD
      INTRINSIC MOD
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ise
      SAVE ise
      DATA ise/86400/
C
C     + + + END SPECIFICATIONS + + +
C
      SEC=ise
      ise=ise+600
      ise=MOD(ise,86400)
      RETURN
      END
