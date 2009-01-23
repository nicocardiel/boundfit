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
C Fusiona los knots, modificando las variables de entrada
C------------------------------------------------------------------------------
        SUBROUTINE MERGE_KNOTS(N,X,Y,DY,LMERGE,NN,XX,YY,DYY)
        IMPLICIT NONE
C
        INCLUDE 'nknotsmax.inc'
C
        INTEGER N
        REAL X(NKNOTSMAX),Y(NKNOTSMAX),DY(NKNOTSMAX)
        LOGICAL LMERGE(NKNOTSMAX)
        REAL XX(NKNOTSMAX),YY(NKNOTSMAX),DYY(NKNOTSMAX)
C
        INTEGER I,II
        INTEGER IMAX
        INTEGER NN
        LOGICAL LOOP
C------------------------------------------------------------------------------
        NN=0
C
        I=0
        DO WHILE(I.LE.N-1)
          I=I+1
          IMAX=I
          LOOP=.TRUE.
          IF(LMERGE(I))THEN
            II=I
            IMAX=II+1
            DO WHILE(LOOP)
              II=II+1
              IF(II.GT.N)THEN
                LOOP=.FALSE.
              ELSE
                IF(LMERGE(II))THEN
                  IMAX=II+1
                  IF(IMAX.GT.N)THEN
                    IMAX=N
                    LOOP=.FALSE.
                  END IF
                ELSE
                  LOOP=.FALSE.
                END IF
              END IF
            END DO
          END IF
          IF((I.EQ.1).AND.(IMAX.EQ.N))THEN
            WRITE(*,101) 'FATAL ERROR in subroutine MERGE_KNOTS'
            WRITE(*,100) 'I...: '
            WRITE(*,*) I
            WRITE(*,100) 'IMAX: '
            WRITE(*,*) IMAX
            WRITE(*,101) 'It is not possible to merge the first and'//
     +       ' the last knot!'
            STOP
          ELSEIF(I.EQ.1)THEN
            NN=NN+1
            XX(NN)=X(1)
            YY(NN)=Y(1)
            DYY(NN)=DY(1)
          ELSEIF(IMAX.EQ.N)THEN
            NN=NN+1
            XX(NN)=X(N)
            YY(NN)=Y(N)
            DYY(NN)=DY(N)
          ELSE
            NN=NN+1
            XX(NN)=(X(I)+X(IMAX))/2.0
            YY(NN)=(Y(I)+Y(IMAX))/2.0
            DYY(NN)=(ABS(DY(I))+ABS(DY(IMAX)))/2.0
          END IF
          I=IMAX
        END DO
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
