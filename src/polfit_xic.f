C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE POLFIT_XIC(X,Y,SIGMAY,XICOEFF,NPTS,NTERMS,MODE,A)
C
C >>> This subroutine is based on the subroutine from Data Reduction and 
C Error Analysis for the Physical Sciences (Bevington, 1969) <<<
C The subroutine has been modified to include the asymmetry
C coefficients.
C
C        LEAST-SQUARES FIT TO A POLYNOMIAL
C        INPUT: X  -  ARRAY FOR INDEPENDENT VARIABLE
C               Y  -  ARRAY FOR DEPENDENT VARIABLE
C               SIGMAY  -  STANDARD DEVIATIONS FOR Y DATA POINTS
C               XICOEFF - ASYMMETRY COEFFCIENT FOR EACH DATA POINT
C               NPTS  -  NUMBER OF PAIRS OF DATA POINTS
C               NTERMS  - NUMBER OF COEFFICIENTS (DEGREE + 1)
C               MODE  -  METHOD OF WEIGHTING (0 = NO WEIGHTING)
C                        +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                         0 (NO WEIGHTING) WEIGHT(I)=1.
C                        -1 (STATISTICAL)  WEIGTH(I)=1./Y(I)
C        OUTPUT:A  - ARRAY OF COEFFICIENTS
C
C        IT USES FUNCTION DETERM TO EVALUATE DETERMINANT OF MATRIX
C        SUPPORTS NTERM UP TO 20
C        FOR DETAILS SEE BEVINGTON(1969)
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE POLFIT_XIC(X,Y,SIGMAY,XICOEFF,NPTS,NTERMS,MODE,A)
        implicit none
        integer npts,nterms,mode
C
        integer i,n,nmax,j,k,l
        real xi,yi,weight
        DOUBLE PRECISION SUMX(39),SUMY(20)
        DOUBLE PRECISION XTERM,YTERM
        DOUBLE PRECISION DETERM,DELTA,ARRAY(20,20)
        REAL X(NPTS),Y(NPTS),SIGMAY(NPTS),A(NTERMS)
        REAL XICOEFF(NPTS)
C------------------------------------------------------------------------------
C Protecciones
        IF(NTERMS.GT.20)THEN
          WRITE(*,100) 'NTERMS='
          WRITE(*,*) NTERMS
          STOP 'FATAL ERROR in POLFIT: NTERMS must be <=20'
        END IF
C
        IF((MODE.NE.-1).AND.(MODE.NE.0).AND.(MODE.NE.+1))THEN
          WRITE(*,100) 'MODE='
          WRITE(*,*) MODE
          STOP 'FATAL ERROR in POLFIT: MODE must be -1, 0 or +1'
        END IF
C------------------------------------------------------------------------------
C        ACCUMULATE WEIGHTED SUMS
C
        NMAX=2*NTERMS-1
        DO N=1,NMAX
          SUMX(N)=0.d0
        END DO
        DO J=1,NTERMS
          SUMY(J)=0.d0
        END DO
        DO I=1,NPTS
          XI=X(I)
          YI=Y(I)
          IF (MODE) 32,37,39
32        IF (YI) 35,37,33
33        WEIGHT=1./YI
          GO TO 41
35        WEIGHT=1./(-YI)
          GO TO 41
37        WEIGHT=1.
          GO TO 41
39        WEIGHT=1./SIGMAY(I)**2
41        XTERM=dble(XICOEFF(I))*dble(WEIGHT)
          DO N=1,NMAX
            SUMX(N)=SUMX(N)+XTERM
            XTERM=XTERM*dble(XI)
          END DO
          YTERM=dble(XICOEFF(I))*dble(WEIGHT)*dble(YI)
          DO N=1,NTERMS
            SUMY(N)=SUMY(N)+YTERM
            YTERM=YTERM*dble(XI)
          END DO
        END DO
C
C        CONSTRUCT MATRICES AND CALCULATE COEFFICIENTS
C
        DO J=1,NTERMS
          DO K=1,NTERMS
            N=J+K-1
            ARRAY(J,K)=SUMX(N)
          END DO
        END DO
        DELTA=DETERM(ARRAY,NTERMS)
        IF (DELTA) 61,57,61
57      DO J=1,NTERMS
          A(J)=0.
        END DO
        GO TO 80
61      DO L=1,NTERMS
          DO J=1,NTERMS
            DO K=1,NTERMS
              N=J+K-1
              ARRAY(J,K)=SUMX(N)
            END DO
            ARRAY(J,L)=SUMY(J)
          END DO
          A(L)=real(DETERM(ARRAY,NTERMS)/DELTA)
        END DO
80      CONTINUE
c------------------------------------------------------------------------------
100     FORMAT(A,$)
        END
C
C******************************************************************************
C******************************************************************************
C La siguiente subrutina ya esta definida en el fichero polfit.f
C       DOUBLE PRECISION FUNCTION DETERM(ARRAY,NORDER)
C
