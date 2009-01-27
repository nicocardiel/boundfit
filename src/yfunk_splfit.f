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
C Funcion para minimizar las coordenadas Y de todos los Knots
        REAL FUNCTION YFUNK_SPLFIT(X)
        IMPLICIT NONE
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nfixedmax.inc'
        INCLUDE 'nknotsmax.inc'
        REAL X(NKNOTSMAX)
C
        INTEGER I
        INTEGER NF,ND
        INTEGER I0
        INTEGER NFIXED_F,NFIXED_D
        REAL XF(NDATAMAX),YF(NDATAMAX),EYF(NDATAMAX),YF0
        REAL XFIXED_F(NFIXEDMAX),YFIXED_F(NFIXEDMAX)
        REAL XFIXED_D(NFIXEDMAX),YFIXED_D(NFIXEDMAX)
        REAL FIXEDWEIGHT_F,FIXEDWEIGHT_D
        REAL XDD(NKNOTSMAX)
        REAL S(NKNOTSMAX),A(NKNOTSMAX),B(NKNOTSMAX),C(NKNOTSMAX)
        REAL WEIGHT,POWER,EPOWER,TSIGMA
        DOUBLE PRECISION W1,W2
        DOUBLE PRECISION F
        LOGICAL LUP
C
        COMMON/BLKSPLFUNK1/NF
        COMMON/BLKSPLFUNK2/XF,YF,EYF
        COMMON/BLKSPLFUNK3/ND
        COMMON/BLKSPLFUNK4/XDD
        COMMON/BLKSPLFUNK7/WEIGHT,POWER,EPOWER,TSIGMA
        COMMON/BLKSPLFUNK8/LUP
        COMMON/BLKFIXED1/NFIXED_F,NFIXED_D
        COMMON/BLKFIXED2F/XFIXED_F,YFIXED_F
        COMMON/BLKFIXED2D/XFIXED_D,YFIXED_D
        COMMON/BLKFIXED3/FIXEDWEIGHT_F,FIXEDWEIGHT_D
c------------------------------------------------------------------------------
C comprobacion inicial
        IF(TSIGMA.LT.0.0)THEN
          WRITE(*,101) 'FATAL ERROR: invalid TSIGMA (must be >= 0.0)'
          WRITE(*,100) 'TSGIMA='
          WRITE(*,*) TSIGMA
          STOP
        END IF
C------------------------------------------------------------------------------
        CALL CUBSPL(XDD,X,ND,1,S,A,B,C)                                !IMODE=1
C------------------------------------------------------------------------------
        IF(TSIGMA.EQ.0.0)THEN !.....................................sin errores
          IF(LUP)THEN
            W1=1.0D0
            W2=DBLE(WEIGHT)
          ELSE
            W1=DBLE(WEIGHT)
            W2=1.0D0
          END IF
          F=0.D0
          I0=1                   !la primera vez busca en el inicio de la tabla
          DO I=1,NF
            CALL CUBSPLX(XDD,X,A,B,C,ND,I0,XF(I),YF0)
            IF(YF0.GE.YF(I))THEN
              F=F+W1*((DBLE(YF0)-DBLE(YF(I)))**DBLE(POWER))/
     +         (DBLE(EYF(I))**DBLE(EPOWER))
            ELSE
              F=F+W2*((DBLE(YF(I))-DBLE(YF0))**DBLE(POWER))/
     +         (DBLE(EYF(I))**DBLE(EPOWER))
            END IF
          END DO
        ELSE !......................................................con errores
          IF(LUP)THEN
            !aqui tenemos que usar ABS() porque podemos tener argumentos
            !negativos debido a que el IF() lo estamos calculando
            !considerando las barras de error
            W1=1.0D0
            W2=DBLE(WEIGHT)
            F=0.D0
            I0=1                 !la primera vez busca en el inicio de la tabla
            DO I=1,NF
              CALL CUBSPLX(XDD,X,A,B,C,ND,I0,XF(I),YF0)
              IF(YF0.GE.YF(I)-TSIGMA*EYF(I))THEN !........aqui usamos signo "-"
                F=F+W1*(DABS(DBLE(YF0)-DBLE(YF(I)))**DBLE(POWER))/
     +           (DBLE(EYF(I))**DBLE(EPOWER))
              ELSE
                F=F+W2*(DABS(DBLE(YF(I))-DBLE(YF0))**DBLE(POWER))/
     +           (DBLE(EYF(I))**DBLE(EPOWER))
              END IF
            END DO
          ELSE
            W1=DBLE(WEIGHT)
            W2=1.0D0
            F=0.D0
            I0=1                 !la primera vez busca en el inicio de la tabla
            DO I=1,NF
              CALL CUBSPLX(XDD,X,A,B,C,ND,I0,XF(I),YF0)
              IF(YF0.GE.YF(I)+TSIGMA*EYF(I))THEN !........aqui usamos signo "+"
                F=F+W1*(DABS(DBLE(YF0)-DBLE(YF(I)))**DBLE(POWER))/
     +           (DBLE(EYF(I))**DBLE(EPOWER))
              ELSE
                F=F+W2*(DABS(DBLE(YF(I))-DBLE(YF0))**DBLE(POWER))/
     +           (DBLE(EYF(I))**DBLE(EPOWER))
              END IF
            END DO
          END IF
        END IF
C------------------------------------------------------------------------------
        I0=1
        IF(NFIXED_F.GT.0)THEN
          DO I=1,NFIXED_F
            CALL CUBSPLX(XDD,X,A,B,C,ND,I0,XFIXED_F(I),YF0)
            F=F+DBLE(FIXEDWEIGHT_F)*
     +       DBLE(ABS(YFIXED_F(I)-YF0))**DBLE(POWER)
          END DO
        END IF
C------------------------------------------------------------------------------
        I0=1
        IF(NFIXED_D.GT.0)THEN
          DO I=1,NFIXED_D
            CALL CUBSPLX(XDD,X,A,B,C,ND,I0,XFIXED_D(I),YF0)
            F=F+DBLE(FIXEDWEIGHT_D)*
     +       DBLE(ABS(YFIXED_D(I)-YF0))**DBLE(POWER)
          END DO
        END IF
C------------------------------------------------------------------------------
        F=F/DBLE(NF)
        YFUNK_SPLFIT=REAL(F)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
