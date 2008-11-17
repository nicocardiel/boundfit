C
C******************************************************************************
C Ordena aleatoriamente los numeros 1,2,...N en la matriz IX(). Esta subrutina
C es util para optimizar los Knots en orden aleatorio para evitar efectos
C sistematicos
        SUBROUTINE RANSPL(N,IX)
        IMPLICIT NONE
        INTEGER NMAX
        PARAMETER(NMAX=100)
        INTEGER N
        INTEGER IX(N),IS(NMAX)
C
        INTEGER NSEED
        INTEGER I,K,M,L
        REAL RANRED
        COMMON/BLKSPLNSEED/NSEED
C------------------------------------------------------------------------------
        IF(N.GT.NMAX)THEN
          WRITE(*,101)'FATAL ERROR: N.GT.NMAX in RANSPL'
        END IF
C
        DO I=1,N
          IS(I)=I
        END DO
C
        K=N
        I=0
C
        DO WHILE(K.GE.1)
          M=INT(RANRED(NSEED)*REAL(K))+1          !numero aleatorio entre 1 y K
          I=I+1
          IX(I)=IS(M)
          IF(M.LT.K)THEN
            DO L=M,K-1
              IS(L)=IS(L+1)
            END DO
          END IF
          K=K-1
        END DO
C
101     FORMAT(A)
        END
