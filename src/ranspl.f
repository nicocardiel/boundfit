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
C Ordena aleatoriamente los numeros 1,2,...N en la matriz IX(). Esta subrutina
C es util para optimizar los Knots en orden aleatorio para evitar efectos
C sistematicos
C------------------------------------------------------------------------------
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
