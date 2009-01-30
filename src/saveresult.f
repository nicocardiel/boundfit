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
        SUBROUTINE SAVERESULT
        IMPLICIT NONE
C
        INCLUDE 'ndatamax.inc'
C
        INTEGER I
        INTEGER NDATA
        REAL XP(NDATAMAX),YP(NDATAMAX)
        CHARACTER*255 OUTFILE
        LOGICAL LECHO
C
        COMMON/BLKLECHO/LECHO
        COMMON/BLKOUT_NDATA/NDATA
        COMMON/BLKOUT_XY/XP,YP
C------------------------------------------------------------------------------
        CALL ASKOUTFILE(OUTFILE)
C
        OPEN(20,FILE=OUTFILE,STATUS='NEW',FORM='FORMATTED')
        DO I=1,NDATA
          WRITE(20,*) XP(I),YP(I)
        END DO
        CLOSE(20)
C
        END
