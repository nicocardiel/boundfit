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
C INTEGER FUNCTION TRUELEN(CADENA)
C
C Input: CADENA
C Output: TRUELEN (function)
C
C Return the position of the last non-blank character in CADENA (ignoring also
C control characters with ASCII value < 32)
C
C CHARACTER*(*) CADENA -> input character string
C
Comment
C------------------------------------------------------------------------------
        INTEGER FUNCTION TRUELEN(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        TRUELEN=0
        L=LEN(CADENA)
C
        IF(L.GT.0)THEN
          DO I=L,1,-1
            IF(ICHAR(CADENA(I:I)).GT.32)THEN
              TRUELEN=I
              RETURN
            END IF
          END DO
        END IF
        END
