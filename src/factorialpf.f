C------------------------------------------------------------------------------
Comment
C
C DOUBLE PRECISION FUNCTION FACTORIALPF(N)
C
C Input: N
C Output: FACTORIALPF (function)
C
C Calculate N factorial
C
C INTEGER N
C
Comment
C------------------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION FACTORIALPF(N)
        IMPLICIT NONE
        INTEGER N
C
        INTEGER I
C------------------------------------------------------------------------------
        IF(N.LT.0)THEN
          WRITE(*,101)'FATAL ERROR: factorial(n<0)!'
          STOP
        END IF
        IF(N.GT.30)THEN
          WRITE(*,101)'FATAL ERROR: factorial(n>30)!'
          STOP
        END IF
        FACTORIALPF=1.D0
        IF(N.EQ.0) RETURN
        DO I=1,N
          FACTORIALPF=DBLE(I)*FACTORIALPF 
        END DO
C
101     FORMAT(A)
        END
