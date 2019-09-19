C------------------------------------------------------------------------------
C Copyright 2008-2019, Universidad Complutense de Madrid
C Author: Nicolas Cardiel
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
Comment
C
C SUBROUTINE PSEUDOFIT(IOPC,LINCREMENTAL,XF,YF,EYF,NF,NTERMS,
C                      YRMSTOL,NEVALMAX,
C                      WEIGHT,POWER,EPOWER,LUP,TSIGMA,A)
C
C Input: IOPC,LINCREMENTAL,XF,YF,EYF,NF,NTERMS
C Input: YRMSTOL,NEVALMAX,WEIGHT,POWER,EPOWER,LUP,TSIGMA
C Output: A
C
C Calculate the polynomial fit to the upper/lower side of a set of data
C points.
C
C INTEGER IOPC -> 1: generic version; 2: simplified version
C LOGICAL LINCREMENTAL -> .TRUE.: incremental fit of coefficients
C REAL XF(NF),YF(NF), EYF(NF) -> data points to be fitted
C INTEGER NF -> number of data points
C INTEGER NTERMS -> number of coeffcients
C REAL YRMSTOL -> stopping criterion for DOWNHILL
C INTEGER NEVALMAX -> allowed maximum number of iterations for DOWNHILL
C REAL WEIGHT -> weighting factor to enhance one side of the fit
C REAL POWER -> power to be used to compute distances
C REAL EPOWER -> power to be used to weight errors
C LOGICAL LUP -> .TRUE.: fit upper side
C                .FALSE.: fit lower side
C REAL TSIGMA -> cut-off parameter for errors
C REAL A(NTERMS) -> fitted coefficients
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE PSEUDOFIT(IOPC,LINCREMENTAL,XF,YF,EYF,NF,NTERMS,
     +   YRMSTOL,NEVALMAX,WEIGHT,POWER,EPOWER,LUP,TSIGMA,A)
        IMPLICIT NONE
C
        INCLUDE 'ndatamax.inc'
        INCLUDE 'ndegmax.inc'
C
        INTEGER IOPC
        INTEGER NF
        REAL XF(NF),YF(NF),EYF(NF)
        INTEGER NTERMS
        REAL YRMSTOL
        INTEGER NEVALMAX
        REAL WEIGHT
        REAL POWER
        REAL EPOWER
        LOGICAL LUP
        REAL TSIGMA
        REAL A(NTERMS)
        LOGICAL LINCREMENTAL
C
        EXTERNAL YFUNK_PSEUDO
        REAL YFUNK_PSEUDO
        REAL FPOLY
C
        INTEGER NNF,NNTERMS
        INTEGER J,K
        INTEGER NEVAL
        INTEGER MODE
        INTEGER NINSIDE,NOUTSIDE
        REAL WWEIGHT
        REAL PPOWER
        REAL EEPOWER
        REAL XXF(NDATAMAX),YYF(NDATAMAX),EYYF(NDATAMAX)
        REAL X0(NDEGMAX+1),DX0(NDEGMAX+1),X(NDEGMAX+1),DX(NDEGMAX+1)
        REAL AA(NDEGMAX+1),DA(NDEGMAX+1)
        REAL CHISQR
        REAL TTSIGMA
        REAL YDUM
        REAL XICOEFF(NDATAMAX)
        LOGICAL LLUP
        LOGICAL LOOP
C
        COMMON/BLKFUNKPSEUDO0/NNF,NNTERMS
        COMMON/BLKFUNKPSEUDO1/XXF,YYF,EYYF
        COMMON/BLKFUNKPSEUDO2/WWEIGHT,PPOWER,EEPOWER,TTSIGMA
        COMMON/BLKFUNKPSEUDO3/LLUP
C------------------------------------------------------------------------------
C protecciones
        IF(NF.GT.NDATAMAX)THEN
          WRITE(*,100) 'NF, NDATAMAX: '
          WRITE(*,*) NF,NDATAMAX
          STOP 'FATAL ERROR: NF.GT.NDATAMAX in PSEUDOFIT.'
        END IF
        IF(NTERMS.GT.NDEGMAX+1)THEN
          WRITE(*,100) 'NTERMS...: '
          WRITE(*,*) NTERMS
          WRITE(*,100) 'NDEGMAX..: '
          WRITE(*,*) NDEGMAX
          STOP 'FATAL ERROR: NTERMS.GT.(NDEGMAX+1) in PSEUDOFIT.'
        END IF
C------------------------------------------------------------------------------
        IF(IOPC.EQ.2)THEN !..................................simplified version
          IF(EPOWER.EQ.0.0)THEN
            MODE=0 !sin pesar con errores
          ELSE
            MODE=1 !pesando con errores
          END IF
C hacemos un ajuste inicial con uso simetrico de los datos
          DO J=1,NF
            XICOEFF(J)=1.0
          END DO
          CALL POLFIT_XIC(XF,YF,EYF,XICOEFF,NF,NTERMS,MODE,A)
          WRITE(*,*)
          WRITE(*,101) '***********************************************'
          WRITE(*,101) '* Initial fit results:'
          DO K=1,NTERMS
            WRITE(*,'(A6,I2.2,A2,$)') '>>> A(',K-1,')='
            WRITE(*,*) A(K)
          END DO
          WRITE(*,101) '-----------------------------------------------'
C distinguimos entre los puntos que estan dentro y los que estan fuera
          NEVAL=0
          LOOP=.TRUE.
          DO WHILE(LOOP)
            NINSIDE=0
            NOUTSIDE=0
            DO J=1,NF
              YDUM=FPOLY(NTERMS-1,A,XF(J))
              IF(LUP)THEN
                IF(YDUM.LT.YF(J)-TSIGMA*EYF(J))THEN
                  XICOEFF(J)=WEIGHT
                  NOUTSIDE=NOUTSIDE+1
                ELSE
                  XICOEFF(J)=1.0
                  NINSIDE=NINSIDE+1
                END IF
              ELSE
                IF(YDUM.GT.YF(J)+TSIGMA*EYF(J))THEN
                  XICOEFF(J)=WEIGHT
                  NOUTSIDE=NOUTSIDE+1
                ELSE
                  XICOEFF(J)=1.0
                  NINSIDE=NINSIDE+1
                END IF
              END IF
            END DO
            WRITE(*,100) '>>> NEVAL, NFIT, NIN, NOUT: '
            WRITE(*,*) NEVAL,NF,NINSIDE,NOUTSIDE
            CALL POLFIT_XIC(XF,YF,EYF,XICOEFF,NF,NTERMS,MODE,AA)
C comparamos AA con A
            LOOP=.FALSE.
            DO K=1,NTERMS
              DA(K)=AA(K)-A(K)
              IF(ABS(DA(K)).GT.YRMSTOL) LOOP=.TRUE.
            END DO
            DO K=1,NTERMS
              A(K)=AA(K)
            END DO
            IF(LOOP)THEN
              NEVAL=NEVAL+1
              IF(NEVAL.GE.NEVALMAX) LOOP=.FALSE.
            END IF
          END DO
          WRITE(*,*)
          WRITE(*,101) '***********************************************'
          WRITE(*,101) '* Final fit results:'
          WRITE(*,*)
          WRITE(*,100) 'NEVAL: '
          WRITE(*,*) NEVAL
          DO K=1,NTERMS
            WRITE(*,'(A6,I2.2,A2,$)') '>>> A(',K-1,')='
            WRITE(*,*) A(K),DA(K)
          END DO
          WRITE(*,101) '-----------------------------------------------'
          RETURN
        END IF
C------------------------------------------------------------------------------
C Si no hemos abandonado la subrutina es porque vamos a realizar el
C ajuste utilizando DOWNHILL. En primer lugar duplicamos los argumentos de 
C entrada de la subrutina para poder pasar la información mediante COMMON 
C blocks a la funcion a minimizar
        NNF=NF
        WWEIGHT=WEIGHT
        PPOWER=POWER
        EEPOWER=EPOWER
        TTSIGMA=TSIGMA
        LLUP=LUP
        DO J=1,NF
          XXF(J)=XF(J)
          YYF(J)=YF(J)
          EYYF(J)=EYF(J)
        END DO
C
        IF(LINCREMENTAL)THEN
          NNTERMS=1
        ELSE
          NNTERMS=NTERMS
        END IF
C
C Hacemos un ajuste tradicional para obtener una primera estimacion 
C (aunque pasamos array de errores en Y, el ajuste lo hacemos sin pesar)
        CALL POLFIT(XXF,YYF,EYYF,NNF,NNTERMS,0,
     +   A,CHISQR,.FALSE.,0.,0.,0.,0.)
C
        LOOP=.TRUE.
        DO WHILE(LOOP)
          !Usamos DOWNHILL para calcular el ajuste final
          DO K=1,NNTERMS
            X0(K)=A(K)
            IF(A(K).NE.0.0)THEN
              DX0(K)=0.01*A(K)
            ELSE
              DX0(K)=1.0
            END IF
          END DO
          CALL DOWNHILL(NNTERMS,X0,DX0,YFUNK_PSEUDO,1.0,0.5,2.0,
     +     YRMSTOL,X,DX,NEVAL,NEVALMAX)
          DO K=1,NNTERMS
            A(K)=X(K)
          END DO
          IF(LINCREMENTAL)THEN
            IF(NNTERMS.EQ.NTERMS)THEN
              LOOP=.FALSE.
            ELSE
              NNTERMS=NNTERMS+1
              A(NNTERMS)=-A(NNTERMS-1) !valor inicial para nuevo coeficiente
            END IF
          ELSE
            LOOP=.FALSE.
          END IF
        END DO
C------------------------------------------------------------------------------
        WRITE(*,*)
        WRITE(*,101) '***********************************************'
        WRITE(*,101) '* Fit results:'
        WRITE(*,100) 'NEVAL: '
        WRITE(*,*) NEVAL
        DO K=1,NNTERMS
          WRITE(*,'(A6,I2.2,A2,$)') '>>> A(',K-1,')='
          WRITE(*,*) X(K),DX(K)
        END DO
        WRITE(*,101) '-----------------------------------------------'
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
