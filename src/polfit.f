C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR,LNOR,X1,X2,Y1,Y2)
C
C >>> This subroutine is based on the subroutine from Data Reduction and 
C Error Analysis for the Physical Sciences (Bevington, 1969) <<<
C
C        LEAST-SQUARES FIT TO A POLYNOMIAL
C        INPUT: X  -  ARRAY FOR INDEPENDENT VARIABLE
C               Y  -  ARRAY FOR DEPENDENT VARIABLE
C               SIGMAY  -  STANDARD DEVIATIONS FOR Y DATA POINTS
C               NPTS  -  NUMBER OF PAIRS OF DATA POINTS
C               NTERMS  - NUMBER OF COEFFICIENTS (DEGREE + 1)
C               MODE  -  METHOD OF WEIGHTING (0 = NO WEIGHTING)
C                        +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                         0 (NO WEIGHTING) WEIGHT(I)=1.
C                        -1 (STATISTICAL)  WEIGTH(I)=1./Y(I)
C               LNOR - if .TRUE., renormalize X and Y ranges
C               X1,X2,Y1,Y2 - new data ranges for fit
C        OUTPUT:A  - ARRAY OF COEFFICIENTS
C               CHISQR  -  REDUCED CHI SQUARE FOR FIT
C
C        IT USES FUNCTION DETERM TO EVALUATE DETERMINANT OF MATRIX
C        SUPPORTS NTERM UP TO 20
C        FOR DETAILS SEE BEVINGTON(1969)
C
Comment
C------------------------------------------------------------------------------
C Nota: en la llamada a esta subrutina no introducir un "0" en CHISQR aunque
C no nos interese el valor de esta variable a la salida de la subrutina
        SUBROUTINE POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR,
     +   LNOR,X1,X2,Y1,Y2)
        implicit none
        integer npts,nterms,mode
        real chisqr
        logical lnor
        real x1,x2,y1,y2
C
        integer i,n,nmax,j,k,l
        real xi,yi,weight,free
        DOUBLE PRECISION SUMX(39),SUMY(20)
        DOUBLE PRECISION XTERM,YTERM,CHISQ
        real dsum
        real cx1,cx2,cy1,cy2
        real xmin,xmax,ymin,ymax
        DOUBLE PRECISION DETERM,DELTA,ARRAY(20,20)
        REAL X(NPTS),Y(NPTS),SIGMAY(NPTS),A(NTERMS)
        real aa(20)
        double precision combpf
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
C
        IF((LNOR).AND.(MODE.NE.0))THEN
          WRITE(*,100) 'MODE='
          WRITE(*,*) MODE
          WRITE(*,101) 'FATAL ERROR in POLFIT: data normalization '//
     +     'not possible with MODE.NE.0.0'
          STOP
        END IF
C------------------------------------------------------------------------------
c normalizamos X,Y para que tengan valores en el intervalo [-1,+1], solo
c cuando MODE=0
        if(lnor)then
          xmax=x(1)
          ymax=y(1)
          xmin=x(1)
          ymin=y(1)
          if(npts.gt.1)then
            do i=2,npts
              if(x(i).gt.xmax) xmax=x(i)
              if(y(i).gt.ymax) ymax=y(i)
              if(x(i).lt.xmin) xmin=x(i)
              if(y(i).lt.ymin) ymin=y(i)
            end do
          end if
          if(xmin.eq.xmax)then
            cx1=1.
            cx2=0.
          else
            cx1=(x2-x1)/(xmax-xmin)
            cx2=(-x1*xmax+x2*xmin)/(xmax-xmin)
          end if
          !como medida de precaucion, evitamos el caso xmax=xmin
          if(cx1.eq.0.0)then
            cx1=1.0
            cx2=0.0
          end if
          do i=1,npts
            x(i)=x(i)*cx1-cx2
          end do
          if(ymin.eq.ymax)then
            cy1=1.
            cy2=0.
          else
            cy1=(y2-y1)/(ymax-ymin)
            cy2=(-y1*ymax+y2*ymin)/(ymax-ymin)
          end if
          !como medida de precaucion, evitamos el caso ymax=ymin
          if(cy1.eq.0.0)then
            cy1=1.0
            cy2=0.0
          end if
          do i=1,npts
            y(i)=y(i)*cy1-cy2
          end do
        else
          cx1=1.0
          cx2=0.0
          cy1=1.0
          cy2=0.0
        end if
!       write(*,100) 'polfit> cx1,cx2: '
!       write(*,*) cx1,cx2
!       write(*,100) 'polfit> cy1,cy2: '
!       write(*,*) cy1,cy2
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
        CHISQ=0.
        DO I=1,NPTS
          XI=X(I)
          YI=Y(I)
!         IF (MODE) 32,37,39    !Fortran 2018: arithmetic IF considered harmful
          IF(MODE.LT.0)THEN
            GOTO 32
          ELSEIF(MODE.EQ.0)THEN
            GOTO 37
          ELSE
            GOTO 39
          END IF
!32        IF (YI) 35,37,33     !Fortran 2018: arithmetic IF considered harmful
32        IF(YI.LT.0)THEN
            GOTO 35
          ELSEIF(YI.EQ.0)THEN
            GOTO 37
          ELSE
            GOTO 33
          END IF
33        WEIGHT=1./YI
          GO TO 41
35        WEIGHT=1./(-YI)
          GO TO 41
37        WEIGHT=1.
          GO TO 41
39        WEIGHT=1./SIGMAY(I)**2
41        XTERM=dble(WEIGHT)
          DO N=1,NMAX
            SUMX(N)=SUMX(N)+XTERM
            XTERM=XTERM*dble(XI)
          END DO
          YTERM=dble(WEIGHT)*dble(YI)
          DO N=1,NTERMS
            SUMY(N)=SUMY(N)+YTERM
            YTERM=YTERM*dble(XI)
          END DO
          CHISQ=CHISQ+dble(WEIGHT)*dble(YI)**2.d0
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
!       IF (DELTA) 61,57,61     !Fortran 2018: arithmetic IF considered harmful
        IF(DELTA.LT.0)THEN
          GOTO 61
        ELSEIF(DELTA.EQ.0)THEN
          GOTO 57
        ELSE
          GOTO 61
        END IF
57      CHISQR=0.
        DO J=1,NTERMS
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
C
C        CALCULATE CHI SQUARE
C
        if(lnor)then
          chisqr=0.
          goto 80
        end if
c
        DO J=1,NTERMS
          CHISQ=CHISQ-2.d0*dble(A(J))*SUMY(J)
          DO K=1,NTERMS
            N=J+K-1
            CHISQ=CHISQ+dble(A(J))*dble(A(K))*SUMX(N)
          END DO
        END DO
C CHISQR, modificado por NCL cuando NPTS=NTERMS
        if(npts-nterms.gt.0)then
          free=real(npts-nterms)
          CHISQR=real(CHISQ/dble(FREE))
        else
          chisqr=0.
        end if
80      continue
c------------------------------------------------------------------------------
c deshacemos el cambio de variable
        if(lnor)then
!         chisqr=0.       !porque el cambio de variable tambien afecta a CHISQR
          do i=1,npts
            x(i)=(x(i)+cx2)/cx1
            y(i)=(y(i)+cy2)/cy1
          end do
          if(cx2.eq.0.0)then
            do k=0,nterms-1
              a(k+1)=a(k+1)*(cx1**(k))
            end do
          else
            do k=0,nterms-1
              aa(k+1)=a(k+1)
            end do
            do k=0,nterms-1
              a(k+1)=0.
              do i=k,nterms-1
                a(k+1)=a(k+1)+aa(i+1)*real(combpf(i,i-k))*(cx1**k)*
     +           ((-cx2)**(i-k))
              end do
            end do
          end if
          a(1)=a(1)+cy2
          do k=0,nterms-1
            a(k+1)=a(k+1)/cy1
          end do
c
          chisqr=0.
          free=real(npts-nterms)
          if(free.gt.0)then
            do i=1,npts
              dsum=a(nterms)
              do k=nterms-1,1,-1
                dsum=dsum*x(i)+a(k)
              end do
              chisqr=chisqr+(y(i)-dsum)*(y(i)-dsum)
            end do
            chisqr=chisqr/free
          end if
        end if
c
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C******************************************************************************
C
        DOUBLE PRECISION FUNCTION DETERM(ARRAY,NORDER)
C
        DOUBLE PRECISION ARRAY(20,20),SAVE
C
        DETERM=1.d0
        DO 50 K=1,NORDER
!         IF(ARRAY(K,K)) 41,21,41             !arithmetic IF considered harmful
          IF(ARRAY(K,K).LT.0)THEN
            GOTO 41
          ELSEIF(ARRAY(K,K).EQ.0)THEN
            GOTO 21
          ELSE
            GOTO 41
          END IF
21        DO 23 J=K,NORDER
!           IF(ARRAY(K,J)) 31,23,31           !arithmetic IF considered harmful
            IF(ARRAY(K,J).LT.0)THEN
              GOTO 31
            ELSEIF(ARRAY(K,J).EQ.0)THEN
              GOTO 23
            ELSE
              GOTO 31
            END IF
23        CONTINUE
          DETERM=0.
          GO TO 60
31        DO I=K,NORDER
            SAVE=ARRAY(I,J)
            ARRAY(I,J)=ARRAY(I,K)
            ARRAY(I,K)=SAVE
          END DO
          DETERM=-DETERM
41        DETERM=DETERM*ARRAY(K,K)
!         IF (K-NORDER) 43,50,50              !arithmetic IF considered harmful
          IF(K-NORDER.LT.0)THEN
            GOTO 43
          ELSEIF(K-NORDER.EQ.0)THEN
            GOTO 50
          ELSE
            GOTO 50
          END IF
43        K1=K+1
          DO I=K1,NORDER
            DO J=K1,NORDER
              ARRAY(I,J)=ARRAY(I,J)-ARRAY(I,K)*ARRAY(K,J)/ARRAY(K,K)
            END DO
          END DO
50      CONTINUE
60      RETURN
        END
