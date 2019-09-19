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
C SUBROUTINE CUBSPLXP(X,A,B,C,N,I0,X0,Y0)
C
C Input: X,A,B,C,N,I0,X0
C Output: Y0
C
C The subroutine returns the first derivative of the cubic spline evaluated 
C at X0, using the coefficients determined in a previous call to CUBSPL. 
C The spline defined in the interval between X(I),Y(I) and X(I+1),Y(I+1) is 
C given by:
C
C      Y = A(I)*(X-X(I))**3 + B(I)*(X-X(I))**2 + C(I)*(X-X(I)) + D(I)
C
C The corresponding first derivative is then
C
C      Y'= 3*A(I)*(X-X(I))**2 + 2*B(I)*(X-X(I))+C(I)
C
C If X0.LT.X(1), I=1 is employed (first computed spline)
C If X0.GT.X(N), I=N-1 is employed (last computed spline)
C
C REAL X(N) -> X-values fitted with CUBSPL
C REAL A(N) -> spline coefficients
C REAL B(N) -> spline coefficients
C REAL C(N) -> spline coefficients
C INTEGER N -> number of data points
C INTEGER I0 -> initial location to start the search of the place of X0 in
C               the X array
C REAL X0 -> X-value where the spline function will be evaluated
C REAL Y0 -> spline first derivative at X0
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE CUBSPLXP(X,A,B,C,N,I0,X0,Y0)
        IMPLICIT NONE
C
        INTEGER N
        REAL X(N)
        REAL A(N),B(N),C(N)
        INTEGER I0
        REAL X0,Y0
C local variables
        REAL DX
C------------------------------------------------------------------------------
C buscamos el lugar en la tabla en la que se encuentra X0, para lo cual
C empleamos la subrutina BINSEARCH, la cual permite emplear un valor de prueba
C para iniciar la busqueda, lo cual acelera el proceso cuando se realizan
C llamadadas sucesivas a esta funcion, con valores de X0 consecutivos.
        CALL BINSEARCH(X,N,X0,I0)
        IF(I0.EQ.0) I0=1
        IF(I0.EQ.N) I0=N-1
C------------------------------------------------------------------------------
C evaluate the spline first derivative
        DX=X0-X(I0)
        Y0=3.*A(I0)*DX*DX+2.*B(I0)*DX+C(I0)
C------------------------------------------------------------------------------
        END
