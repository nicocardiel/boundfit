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
C DOUBLE PRECISION FUNCTION FACTORIALPF(N)
C
C Input: N
C Output: FACTORIALPF (function)
C
C Calculate N factorial
C
C INTEGER N
C
Comment
C------------------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION FACTORIALPF(N)
        IMPLICIT NONE
        INTEGER N
C
        INTEGER I
C------------------------------------------------------------------------------
        IF(N.LT.0)THEN
          WRITE(*,101)'FATAL ERROR: factorial(n<0)!'
          STOP
        END IF
        IF(N.GT.30)THEN
          WRITE(*,101)'FATAL ERROR: factorial(n>30)!'
          STOP
        END IF
        FACTORIALPF=1.D0
        IF(N.EQ.0) RETURN
        DO I=1,N
          FACTORIALPF=DBLE(I)*FACTORIALPF 
        END DO
C
101     FORMAT(A)
        END
