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
Comment
C
C SUBROUTINE PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,NEVALMAX,
C                      WEIGHT,POWER,EPOWER,LUP,TSIGMA,A)
C
C Input: XF,YF,EYF,NF,NTERMS,YRMSTOL,WEIGHT,POWER,EPOWER,LUP,CERR
C Output: A
C
C Calculate the polynomial fit to the upper/lower side of a set of data
C points.
C
C REAL XF(NF),YF(NF) -> data points to be fitted
C INTEGER NF -> number of data points
C INTEGER NTERMS -> number of coeffcients
C REAL YRMSTOL -> stopping criterion for DOWNHILL
C INTEGER NEVALMAX -> allowed maximum number of iterations for DOWNHILL
C REAL WEIGHT -> weighting factor to enhance one side of the fit
C REAL POWER -> power to be used to compute distances
C REAL EPOWER -> power to be used to weight errors
C LOGICAL LUP -> .TRUE.: fit upper side
C                .FALSE.: fit lower side
C REAL A(NTERMS) -> fitted coefficients
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE PSEUDOFIT(XF,YF,EYF,NF,NTERMS,YRMSTOL,NEVALMAX,
     +   WEIGHT,POWER,EPOWER,LUP,TSIGMA,A)
        IMPLICIT NONE
C
        INCLUDE 'ndatamax.inc'
        INCLUDE 'ndegmax.inc'
C
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
C
        EXTERNAL YFUNK_PSEUDO
        REAL YFUNK_PSEUDO
C
        INTEGER NNF,NNTERMS
        INTEGER J,K
        INTEGER NEVAL
        REAL WWEIGHT
        REAL PPOWER
        REAL EEPOWER
        REAL XXF(NDATAMAX),YYF(NDATAMAX),EYYF(NDATAMAX)
        REAL X0(NDEGMAX+1),DX0(NDEGMAX+1),X(NDEGMAX+1),DX(NDEGMAX+1)
        REAL CHISQR
        REAL TTSIGMA
        LOGICAL LLUP
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
C inicializacion (duplicamos los argumentos de entrada de la subrutina para 
C poder pasar la información mediante COMMON blocks a la funcion a minimizar)
        NNF=NF
        NNTERMS=NTERMS
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
C------------------------------------------------------------------------------
C Primero hacemos un ajuste tradicional para obtener una primera estimacion 
C (aunque pasamos array de errores en Y, el ajuste lo hacemos sin pesar)
        CALL POLFIT(XF,YF,EYF,NF,NTERMS,0,A,CHISQR,.FALSE.,0.,0.,0.,0.)
C------------------------------------------------------------------------------
C Usamos DOWNHILL para calcular el ajuste final
        DO K=1,NTERMS
          X0(K)=A(K)
          IF(A(K).NE.0.0)THEN
            DX0(K)=0.01*A(K)
          ELSE
            DX0(K)=1.0
          END IF
        END DO
        CALL DOWNHILL(NTERMS,X0,DX0,YFUNK_PSEUDO,1.0,0.5,2.0,YRMSTOL,
     +   X,DX,NEVAL,NEVALMAX)
        DO K=1,NTERMS
          A(K)=X(K)
        END DO
C------------------------------------------------------------------------------
        WRITE(*,101) '***********************************************'
        WRITE(*,101) '* Fit results:'
        WRITE(*,100) 'NEVAL: '
        WRITE(*,*) NEVAL
        DO K=1,NTERMS
          WRITE(*,'(A6,I2.2,A2,$)') '>>> A(',K-1,')='
          WRITE(*,*) X(K),DX(K)
        END DO
        WRITE(*,101) '-----------------------------------------------'
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
