C------------------------------------------------------------------------------
C Copyright 2008-2020, Universidad Complutense de Madrid
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
C SUBROUTINE CUBSPLARCLENGTH(X,Y,A,B,C,N,NSAMPLING,ARCLENGTH)
C
C Input: X,Y,A,B,C,N
C Output: ARCLENGTH
C
C The subroutine estimates the total arc length of the cubic spline 
C from X(1) to C X(N), by splitting the X range in NSAMPLING values 
C and computing the total arc length by simply adding the corresponding 
C segments using Pythagoras' theorem. The spline defined in the interval
C between X(I),Y(I) and X(I+1),Y(I+1) is given by:
C
C      Y = A(I)*(X-X(I))**3 + B(I)*(X-X(I))**2 + C(I)*(X-X(I)) + Y(I)
C
C REAL X(N) -> X-values fitted with CUBSPL
C REAL Y(N) -> Y-values fitted with CUBSPL
C REAL A(N) -> spline coefficients
C REAL B(N) -> spline coefficients
C REAL C(N) -> spline coefficients
C INTEGER N -> number of data points
C INTEGER NSAMPLING -> number of points employed to subdivide the X range
C REAL ARCLENGTH -> total arc length
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE CUBSPLARCLENGTH(X,Y,A,B,C,N,NSAMPLING,ARCLENGTH)
        IMPLICIT NONE
C
        INTEGER N
        REAL X(N),Y(N)
        REAL A(N),B(N),C(N)
        INTEGER NSAMPLING
        REAL ARCLENGTH
C local variables
        INTEGER I,I0
        REAL XMIN,XMAX
        REAL X0,Y0,XPREV,YPREV
        REAL DX
C------------------------------------------------------------------------------
C See cubsplx.f for details
        I0=1
        XMIN=X(1)
        XMAX=X(N)
        ARCLENGTH=0.0
        DO I=1,NSAMPLING
          X0=XMIN+REAL(I-1)*(XMAX-XMIN)/REAL(NSAMPLING-1)
          CALL BINSEARCH(X,N,X0,I0)
          IF(I0.EQ.0) I0=1
          IF(I0.EQ.N) I0=N-1
          DX=X0-X(I0)
          Y0=Y(I0)+DX*(C(I0)+DX*(B(I0)+DX*A(I0)))
          IF(I.GT.1)THEN
            ARCLENGTH=ARCLENGTH+SQRT((X0-XPREV)**2+(Y0-YPREV)**2)
          END IF
          XPREV=X0
          YPREV=Y0
        END DO
C------------------------------------------------------------------------------
        END
