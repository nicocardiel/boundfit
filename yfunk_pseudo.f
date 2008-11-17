        REAL FUNCTION YFUNK_PSEUDO(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INCLUDE 'ndegmax.inc'
C
        REAL X(NDEGMAX+1)
C
        REAL FPOLY
C
        INTEGER J
        INTEGER NF,NTERMS
        INTEGER NDEG
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL WEIGHT,POWER
        REAL W1,W2
        REAL YPOL
        DOUBLE PRECISION DSUM
        LOGICAL LUP
        REAL TSIGMA
C
        COMMON/BLKFUNKPSEUDO0/NF,NTERMS
        COMMON/BLKFUNKPSEUDO1/XF,YF,EYF
        COMMON/BLKFUNKPSEUDO2/WEIGHT,POWER,TSIGMA
        COMMON/BLKFUNKPSEUDO3/LUP
C------------------------------------------------------------------------------
C comprobacion inicial
        IF(TSIGMA.LT.0.0)THEN
          WRITE(*,101) 'FATAL ERROR: invalid TSIGMA (must be >= 0.0)'
          WRITE(*,100) 'TSGIMA='
          WRITE(*,*) TSIGMA
          STOP
        END IF
C------------------------------------------------------------------------------
        DSUM=0.D0
        NDEG=NTERMS-1
C------------------------------------------------------------------------------
        IF(TSIGMA.EQ.0.0)THEN !.....................................sin errores
          IF(LUP)THEN
            W1=1.0
            W2=WEIGHT
          ELSE
            W1=WEIGHT
            W2=1.0
          END IF
          DO J=1,NF
            YPOL=FPOLY(NDEG,X,XF(J))
            IF(YPOL.GE.YF(J))THEN
              DSUM=DSUM+W1*((YPOL-YF(J))**POWER)
            ELSE
              DSUM=DSUM+W2*((YF(J)-YPOL)**POWER)
            END IF
          END DO
        ELSE !......................................................con errores
          IF(LUP)THEN
            !aqui tenemos que usar ABS() porque podemos tener argumentos
            !negativos debido a que el IF() lo estamos calculando
            !considerando las barras de error
            W1=1.0
            W2=WEIGHT
            DO J=1,NF
              YPOL=FPOLY(NDEG,X,XF(J))
              IF(YPOL.GE.YF(J)-TSIGMA*EYF(J))THEN !.......aqui usamos signo "-"
                DSUM=DSUM+W1*(ABS(YPOL-YF(J))**POWER)
              ELSE
                DSUM=DSUM+W2*(ABS(YF(J)-YPOL)**POWER)
              END IF
            END DO
          ELSE
            W1=WEIGHT
            W2=1.0
            DO J=1,NF
              YPOL=FPOLY(NDEG,X,XF(J))
              IF(YPOL.GE.YF(J)+TSIGMA*EYF(J))THEN !.......aqui usamos signo "+"
                DSUM=DSUM+W1*(ABS(YPOL-YF(J))**POWER)
              ELSE
                DSUM=DSUM+W2*(ABS(YF(J)-YPOL)**POWER)
              END IF
            END DO
          END IF
        END IF
C
        YFUNK_PSEUDO=REAL(DSUM)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
