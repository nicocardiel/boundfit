C------------------------------------------------------------------------------
Comment
C
C DOUBLE PRECISION FUNCTION COMBPF(N,K)
C
C Input: N,K
C Output: COMBPF (function)
C
C Calculate the binomial coefficient N over K
C
C INTEGER N
C INTEGER K
C
Comment
C------------------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION COMBPF(N,K)
        IMPLICIT NONE
C        
        DOUBLE PRECISION FACTORIALPF
        INTEGER N,K
C------------------------------------------------------------------------------
        IF(K.GT.N)THEN
          WRITE(*,101)'FATAL ERROR: in function COMBPF(N,K), K>N'
          STOP
        END IF
        COMBPF=FACTORIALPF(N)/(FACTORIALPF(K)*FACTORIALPF(N-K))
101     FORMAT(A)
        END
