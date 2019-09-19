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
C REAL FUNCTION FDERI(NDEG,COEFF,X)
C
C Input: NDEG,COEFF,X
C Output: FDERI (function)
C
C Evaluate the first derivative of a polynomial of degree NDEG and coefficients
C COEFF at X.
C
C INTEGER NDEG -> polynomial degree
C REAL    COEFF(NDEG+1) -> polynomial coefficients
C REAL    X -> abscissa at which the derivative is going to be evaluated
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION FDERI(NDEG,COEFF,X)
        IMPLICIT NONE
        INTEGER NDEG
        REAL COEFF(NDEG+1)
        REAL X
C
        INTEGER K
        DOUBLE PRECISION DSUM
C------------------------------------------------------------------------------
        
        DSUM=DBLE(COEFF(NDEG+1))*DBLE(NDEG)
        IF(NDEG.GT.1)THEN
          DO K=NDEG,2,-1
            DSUM=DSUM*DBLE(X)+DBLE(COEFF(K))*DBLE(K-1)
          END DO
        END IF
C
        FDERI=REAL(DSUM)
        END
