C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
C
C This file is part of boundfit.
C 
C Boundfit is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C Boundfit is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with boundfit. If not, see <http://www.gnu.org/licenses/>.
C------------------------------------------------------------------------------
        REAL FUNCTION YFUNK_PSEUDO(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nfixedmax.inc'
        INCLUDE 'ndegmax.inc'
C
        REAL X(NDEGMAX+1)
C
        REAL FPOLY
C
        INTEGER J
        INTEGER NF,NTERMS
        INTEGER NDEG
        INTEGER NFIXED_F,NFIXED_D
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX)
        REAL XFIXED_F(NFIXEDMAX),YFIXED_F(NFIXEDMAX)
        REAL XFIXED_D(NFIXEDMAX),YFIXED_D(NFIXEDMAX)
        REAL FIXEDWEIGHT_F,FIXEDWEIGHT_D
        REAL WEIGHT,POWER,EPOWER
        REAL YPOL
        DOUBLE PRECISION W1,W2
        DOUBLE PRECISION DSUM
        LOGICAL LUP
        REAL TSIGMA
C
        COMMON/BLKFUNKPSEUDO0/NF,NTERMS
        COMMON/BLKFUNKPSEUDO1/XF,YF,EYF
        COMMON/BLKFUNKPSEUDO2/WEIGHT,POWER,EPOWER,TSIGMA
        COMMON/BLKFUNKPSEUDO3/LUP
        COMMON/BLKFIXED1/NFIXED_F,NFIXED_D
        COMMON/BLKFIXED2F/XFIXED_F,YFIXED_F
        COMMON/BLKFIXED2D/XFIXED_D,YFIXED_D
        COMMON/BLKFIXED3/FIXEDWEIGHT_F,FIXEDWEIGHT_D
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
            W1=1.0D0
            W2=DBLE(WEIGHT)
          ELSE
            W1=DBLE(WEIGHT)
            W2=1.0D0
          END IF
          DO J=1,NF
            YPOL=FPOLY(NDEG,X,XF(J))
            IF(YPOL.GE.YF(J))THEN
              DSUM=DSUM+W1*(DBLE(YPOL-YF(J))**DBLE(POWER))/
     +         (DBLE(EYF(J))**DBLE(EPOWER))
            ELSE
              DSUM=DSUM+W2*(DBLE(YF(J)-YPOL)**DBLE(POWER))/
     +         (DBLE(EYF(J))**DBLE(EPOWER))
            END IF
          END DO
        ELSE !......................................................con errores
          IF(LUP)THEN
            !aqui tenemos que usar ABS() porque podemos tener argumentos
            !negativos debido a que el IF() lo estamos calculando
            !considerando las barras de error
            W1=1.0D0
            W2=DBLE(WEIGHT)
            DO J=1,NF
              YPOL=FPOLY(NDEG,X,XF(J))
              IF(YPOL.GE.YF(J)-TSIGMA*EYF(J))THEN !.......aqui usamos signo "-"
                DSUM=DSUM+W1*(DABS(DBLE(YPOL-YF(J)))**DBLE(POWER))/
     +           (DBLE(EYF(J))**DBLE(EPOWER))
              ELSE
                DSUM=DSUM+W2*(DABS(DBLE(YF(J)-YPOL))**DBLE(POWER))/
     +           (DBLE(EYF(J))**DBLE(EPOWER))
              END IF
            END DO
          ELSE
            W1=DBLE(WEIGHT)
            W2=1.0D0
            DO J=1,NF
              YPOL=FPOLY(NDEG,X,XF(J))
              IF(YPOL.GE.YF(J)+TSIGMA*EYF(J))THEN !.......aqui usamos signo "+"
                DSUM=DSUM+W1*(DABS(DBLE(YPOL-YF(J)))**DBLE(POWER))/
     +           (DBLE(EYF(J))**DBLE(EPOWER))
              ELSE
                DSUM=DSUM+W2*(DABS(DBLE(YF(J)-YPOL))**DBLE(POWER))/
     +           (DBLE(EYF(J))**DBLE(EPOWER))
              END IF
            END DO
          END IF
        END IF
C------------------------------------------------------------------------------
        IF(NFIXED_F.GT.0)THEN
          DO J=1,NFIXED_F
            YPOL=FPOLY(NDEG,X,XFIXED_F(J))
            DSUM=DSUM+
     +       DBLE(FIXEDWEIGHT_F)*
     +       DABS(DBLE(YFIXED_F(J)-YPOL))**DBLE(POWER)
          END DO
        END IF
C------------------------------------------------------------------------------
C
        DSUM=DSUM/DBLE(NF)
        YFUNK_PSEUDO=REAL(DSUM)
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
