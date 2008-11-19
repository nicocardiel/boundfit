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
C SUBROUTINE FINDMML(N,N1,N2,X,XMIN,XMAX)
C
C Input: N,N1,N2,X
C Output: XMIN,XMAX
C
C Return the maximum and minimum value of matrix X of N elements (in the
C range from N1 to N2 exclusively)
C
C INTEGER N -> no. of elements of matrix X
C INTEGER N1 -> first element of X() to search minimum/maximum
C INTEGER N2 -> last element of X() to search minimum/maximum
C REAL    X(N) -> data matrix
C REAL    XMIN -> minimum value of X()
C REAL    XMAX -> maximum value of X()
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE FINDMML(N,N1,N2,X,XMIN,XMAX)
        IMPLICIT NONE
        INTEGER N,N1,N2
        REAL X(N)
        REAL XMIN,XMAX
C
        INTEGER I
C------------------------------------------------------------------------------
        IF((N1.LT.1).OR.(N2.GT.N).OR.(N2.LT.N1))THEN
          WRITE(*,101)'ERROR: limits out of range in FINDMML'
          WRITE(*,101)'=> Returned values: XMIN = XMAX = 0'
          XMIN=0.
          XMAX=0.
          RETURN
        END IF
C
        XMIN=X(N1)
        XMAX=XMIN
        DO I=N1+1,N2
          IF(X(I).LT.XMIN) XMIN=X(I)
          IF(X(I).GT.XMAX) XMAX=X(I)
        END DO
101     FORMAT(A)
        END
