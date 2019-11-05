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
C REAL FUNCTION FMEDIAN1(N,X)
C
C Input: N,X
C Output: FMEDIAN (function), X (sorted)
C
C Calculate the median value of X(N). It is important to note that this 
C subroutine rearranges the matrix X which is returned sorted.
C
C INTEGER N -> no. of elements
C REAL    X(N) -> input matrix
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION FMEDIAN1(N,X)
        IMPLICIT NONE
        INTEGER N
        REAL X(N)
C variables locales
        INTEGER NN
C------------------------------------------------------------------------------
        IF(N.EQ.0) STOP 'FATAL ERROR: in function FMEDIAN: N=0.'
        CALL ORDENA1F(N,X)
        NN=N/2
        IF(MOD(N,2).EQ.0)THEN
          FMEDIAN1=(X(NN)+X(NN+1))/2.
        ELSE
          FMEDIAN1=X(NN+1)
        END IF
C
        END
