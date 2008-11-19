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
        SUBROUTINE CLEANTAB(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER TRUELEN
C
        INTEGER L
C------------------------------------------------------------------------------
        DO L=1,TRUELEN(CADENA)
          IF(CADENA(L:L).EQ.CHAR(9)) CADENA(L:L)=' '
        END DO
C
        END
