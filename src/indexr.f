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
C Busca la posicion en la que aparece en caracter CH en la cadena
C CSTRING, empezando la busqueda por el final
C------------------------------------------------------------------------------
        INTEGER FUNCTION INDEXR(CSTRING,CH)
        IMPLICIT NONE
        CHARACTER*(*) CSTRING
        CHARACTER*1 CH
C
        INTEGER TRUELEN
C
        INTEGER I,L
C------------------------------------------------------------------------------
        INDEXR=0 !salvo que se demuestre lo contrario
C
        L=TRUELEN(CSTRING)
        IF(L.EQ.0) RETURN
C
        DO I=L,1,-1
          IF(CSTRING(I:I).EQ.CH)THEN
            INDEXR=I
            RETURN
          END IF
        END DO
        END
