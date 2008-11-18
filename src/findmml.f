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
