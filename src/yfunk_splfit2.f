C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
C
C This file is part of BoundFit.
C 
C BoundFit is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C BoundFit is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with BoundFit. If not, see <http://www.gnu.org/licenses/>.
C------------------------------------------------------------------------------
C Funcion para minimizar la coordenada Y de un solo Knot
        REAL FUNCTION YFUNK_SPLFIT2(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nknotsmax.inc'
        REAL X(NKNOTSMAX)
C
        INTEGER I
        INTEGER I0
        INTEGER NF,ND
        INTEGER NREF
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX),YF0
        REAL XDD(NKNOTSMAX)
        REAL S(NKNOTSMAX),A(NKNOTSMAX),B(NKNOTSMAX),C(NKNOTSMAX)
        REAL YD(NKNOTSMAX)
        REAL WEIGHT,POWER,TSIGMA
        REAL W1,W2
        DOUBLE PRECISION F
        LOGICAL LUP
C
        COMMON/BLKSPLFUNK1/NF
        COMMON/BLKSPLFUNK2/XF,YF,EYF
        COMMON/BLKSPLFUNK3/ND
        COMMON/BLKSPLFUNK4/XDD
        COMMON/BLKSPLFUNK5/YD
        COMMON/BLKSPLFUNK6/NREF
        COMMON/BLKSPLFUNK7/WEIGHT,POWER,TSIGMA
        COMMON/BLKSPLFUNK8/LUP
C------------------------------------------------------------------------------
C comprobacion inicial
        IF(TSIGMA.LT.0.0)THEN
          WRITE(*,101) 'FATAL ERROR: invalid TSIGMA (must be >= 0.0)'
          WRITE(*,100) 'TSGIMA='
          WRITE(*,*) TSIGMA
          STOP
        END IF
C------------------------------------------------------------------------------
        YD(NREF)=X(1)
        CALL CUBSPL(XDD,YD,ND,1,S,A,B,C) !IMODE=1
C------------------------------------------------------------------------------
        IF(TSIGMA.EQ.0.0)THEN !.....................................sin errores
          IF(LUP)THEN
            W1=1.0
            W2=WEIGHT
          ELSE
            W1=WEIGHT
            W2=1.0
          END IF
          F=0.D0
          I0=1                   !la primera vez busca en el inicio de la tabla
          DO I=1,NF
            CALL CUBSPLX(XDD,YD,A,B,C,ND,I0,XF(I),YF0)
            IF(YF0.GE.YF(I))THEN
              F=F+W1*((DBLE(YF0)-DBLE(YF(I)))**POWER)
            ELSE
              F=F+W2*((DBLE(YF(I))-DBLE(YF0))**POWER)
            END IF
          END DO
        ELSE !......................................................con errores
          IF(LUP)THEN
            !aqui tenemos que usar ABS() porque podemos tener argumentos
            !negativos debido a que el IF() lo estamos calculando
            !considerando las barras de error
            W1=1.0
            W2=WEIGHT
            F=0.D0
            I0=1                 !la primera vez busca en el inicio de la tabla
            DO I=1,NF
              CALL CUBSPLX(XDD,YD,A,B,C,ND,I0,XF(I),YF0)
              IF(YF0.GE.YF(I)-TSIGMA*EYF(I))THEN !........aqui usamos signo "-"
                F=F+DBLE(W1)*(ABS(DBLE(YF0)-DBLE(YF(I)))**DBLE(POWER))
              ELSE
                F=F+DBLE(W2)*(ABS(DBLE(YF(I))-DBLE(YF0))**DBLE(POWER))
              END IF
            END DO
          ELSE
            W1=WEIGHT
            W2=1.0
            F=0.D0
            I0=1                 !la primera vez busca en el inicio de la tabla
            DO I=1,NF
              CALL CUBSPLX(XDD,YD,A,B,C,ND,I0,XF(I),YF0)
              IF(YF0.GE.YF(I)+TSIGMA*EYF(I))THEN !........aqui usamos signo "+"
                F=F+DBLE(W1)*(ABS(DBLE(YF0)-DBLE(YF(I)))**DBLE(POWER))
              ELSE
                F=F+DBLE(W2)*(ABS(DBLE(YF(I))-DBLE(YF0))**DBLE(POWER))
              END IF
            END DO
          END IF
        END IF
C------------------------------------------------------------------------------
        F=F/DBLE(NF)
        YFUNK_SPLFIT2=REAL(F)
C
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
