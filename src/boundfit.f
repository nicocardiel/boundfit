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
        PROGRAM BOUNDFIT
        IMPLICIT NONE
C included files
        INCLUDE 'version.inc'
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nfixedmax.inc'
        INCLUDE 'nknotsmax.inc'
        INCLUDE 'ndegmax.inc'
C additional parameters
        INTEGER NPLOTMAX
        PARAMETER (NPLOTMAX=1000)
C functions
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER READI_B
        INTEGER READILIM_B
        REAL RANRED
        REAL READF_B
        REAL FPOLY
        DOUBLE PRECISION COMBPF
        CHARACTER*255 READC_B
C variables
        INTEGER I,K
        INTEGER L,L1,L2
        INTEGER IOPC
        INTEGER ISTATUS
        INTEGER NDATABUFF
        INTEGER NTERMS
        INTEGER NFIXED_F,NFIXED_D
        INTEGER ILUP
        INTEGER NEVALMAX
        INTEGER IKNOT,NKNOTS
        INTEGER IDUM
        INTEGER NSEED
        INTEGER NDATA
        INTEGER I0SPL
        REAL XDATA(NDATAMAX),XDATA_SORTED(NDATAMAX)
        REAL YDATA(NDATAMAX)
        REAL EYDATA(NDATAMAX)
        REAL XFIXED_F(NFIXEDMAX),YFIXED_F(NFIXEDMAX)
        REAL XFIXED_D(NFIXEDMAX),YFIXED_D(NFIXEDMAX)
        REAL FIXEDWEIGHT_F,FIXEDWEIGHT_D
        REAL XMINBUFF,XMAXBUFF
        REAL WEIGHT,POWER,EPOWER
        REAL TSIGMA
        REAL YRMSTOL
        REAL A(NDEGMAX+1),AA(NDEGMAX+1)
        REAL XKNOT(NKNOTSMAX),YKNOT(NKNOTSMAX)
        REAL ASPL(NKNOTSMAX),BSPL(NKNOTSMAX),CSPL(NKNOTSMAX)
        REAL RDUMMY
        REAL XP(NDATAMAX),YP(NDATAMAX)
        REAL XMINF,XMAXF
        REAL BX,CX,BY,CY
        REAL FNPOINTSINTERVAL
        CHARACTER*1 COPC
        CHARACTER*1 CVERBOSE
        CHARACTER*1 CEQUI
        CHARACTER*1 CWEIGHT
        CHARACTER*1 CCONSTRAINTS
        CHARACTER*1 CINCREMENTAL
        CHARACTER*50 CDUMMY
        CHARACTER*255 COEFFFILE
        LOGICAL LOOP
        LOGICAL LUP
        LOGICAL LECHO
        LOGICAL LCONSTRAINTS
        LOGICAL LINCREMENTAL
C common blocks
        COMMON/BLKLECHO/LECHO
        COMMON/BLKCVERBOSE/CVERBOSE
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA,EYDATA
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF
        COMMON/BLKNORM/BX,CX,BY,CY
        COMMON/BLKOUT_NDATA/NDATA
        COMMON/BLKOUT_XY/XP,YP
        COMMON/BLKFIXED1/NFIXED_F,NFIXED_D
        COMMON/BLKFIXED2F/XFIXED_F,YFIXED_F
        COMMON/BLKFIXED2D/XFIXED_D,YFIXED_D
        COMMON/BLKFIXED3/FIXEDWEIGHT_F,FIXEDWEIGHT_D
C------------------------------------------------------------------------------
C welcome message
        WRITE(*,*)
        WRITE(*,101) '***********************************************'
        WRITE(*,101) '       Welcome to BoundFit '//
     +   '(version '//VERSION//')'
        WRITE(*,101) '-----------------------------------------------'
        WRITE(*,101) '> For more details see:'
        WRITE(*,101) 'Cardiel, N., 2009, MNRAS, 396, 680-695'
        WRITE(*,101) '> and visit:'
        WRITE(*,101) 'http://boundfit.readthedocs.io/'
        WRITE(*,101) '***********************************************'
        WRITE(*,*)
C------------------------------------------------------------------------------
        INQUIRE(FILE='.running_BoundFit',EXIST=LECHO)
C------------------------------------------------------------------------------
C read data file
        CALL LEENEWFILE(ISTATUS)
        IF(ISTATUS.NE.1)THEN
          WRITE(*,101) 'FATAL ERROR#1: problems while reading data file'
          STOP
        END IF
C------------------------------------------------------------------------------
C type of fit
10      WRITE(*,*)
        WRITE(*,101) '(1) Simple polynomial (generic version)'
        WRITE(*,101) '(2) Simple polynomial (simplified version)'
        WRITE(*,101) '(3) Adaptive splines'
        WRITE(*,101) '(0) EXIT'
        WRITE(*,100) 'Select type of fit............'
        IOPC=READILIM_B('0',0,3)
        IF(LECHO)THEN
          WRITE(CDUMMY,*) IOPC
          WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
        END IF
        WRITE(*,*)
        IF(IOPC.EQ.0)THEN
          WRITE(*,101) 'End of BoundFit execution!'
          STOP
        END IF
C------------------------------------------------------------------------------
        IF(IOPC.NE.2)THEN
          WRITE(*,100) 'Are you using fit constraints.....(y/n) '
          CCONSTRAINTS(1:1)=READC_B('n','yn')
          IF(LECHO) WRITE(*,101) CCONSTRAINTS
          LCONSTRAINTS=(CCONSTRAINTS.EQ.'y')
        ELSE
          LCONSTRAINTS=.FALSE.
        END IF
C
        IF(LCONSTRAINTS)THEN
          WRITE(*,*)
          WRITE(*,100) 'Number of fixed function points.........'
          NFIXED_F=READI_B('0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NFIXED_F
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          IF(NFIXED_F.GT.NFIXEDMAX)THEN
            WRITE(*,100) 'NFIXEDMAX: '
            WRITE(*,*) NFIXEDMAX
            WRITE(*,100) 'NFIXED_F.: '
            WRITE(*,*) NFIXED_F
            WRITE(*,101) 'FATAL ERROR#2: NFIXED_F.GT.NFIXEDMAX'
            STOP
          END IF
          IF(NFIXED_F.LT.0)THEN
            WRITE(*,101) 'WARNING: invalid number. NFIXED_F set to 0.'
            NFIXED_F=0
          ELSEIF(NFIXED_F.GT.0)THEN
            WRITE(*,100) 'Weight for fixed points.....(Lambda) '
            FIXEDWEIGHT_F=READF_B('1.E6')
            IF(LECHO)THEN
              WRITE(CDUMMY,*) FIXEDWEIGHT_F
              WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
            END IF
            DO I=1,NFIXED_F
              WRITE(*,'(A,I2,A,$)') 'X-coordinate of point #',I,
     +         '...................'
              XFIXED_F(I)=READF_B('@')
              IF(LECHO)THEN
                WRITE(CDUMMY,*) XFIXED_F(I)
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              XFIXED_F(I)=BX*XFIXED_F(I)-CX
              WRITE(*,'(A,I2,A,$)') 'Y-coordinate of point #',I,
     +         '...................'
              YFIXED_F(I)=READF_B('@')
              IF(LECHO)THEN
                WRITE(CDUMMY,*) YFIXED_F(I)
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              YFIXED_F(I)=BY*YFIXED_F(I)-CY
            END DO
          END IF
          WRITE(*,*)
          WRITE(*,100) 'Number of fixed derivative points.......'
          NFIXED_D=READI_B('0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NFIXED_D
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          IF(NFIXED_D.GT.NFIXEDMAX)THEN
            WRITE(*,100) 'NFIXEDMAX: '
            WRITE(*,*) NFIXEDMAX
            WRITE(*,100) 'NFIXED_D.: '
            WRITE(*,*) NFIXED_D
            WRITE(*,101) 'FATAL ERROR#2: NFIXED_D.GT.NFIXEDMAX'
            STOP
          END IF
          IF(NFIXED_D.LT.0)THEN
            WRITE(*,101) 'WARNING: invalid number. NFIXED_D set to 0.'
            NFIXED_D=0
          ELSEIF(NFIXED_D.GT.0)THEN
            WRITE(*,100) 'Weight for fixed points.....(Lambda) '
            FIXEDWEIGHT_D=READF_B('1.E6')
            IF(LECHO)THEN
              WRITE(CDUMMY,*) FIXEDWEIGHT_D
              WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
            END IF
            DO I=1,NFIXED_D
              WRITE(*,'(A,I2,A,$)') 'X-coordinate of point #',I,
     +         '...................'
              XFIXED_D(I)=READF_B('@')
              IF(LECHO)THEN
                WRITE(CDUMMY,*) XFIXED_D(I)
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              XFIXED_D(I)=BX*XFIXED_D(I)-CX
              WRITE(*,'(A,I2,A,$)') 'Y-coordinate of point #',I,
     +         '...................'
              YFIXED_D(I)=READF_B('@')
              IF(LECHO)THEN
                WRITE(CDUMMY,*) YFIXED_D(I)
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              YFIXED_D(I)=BY*YFIXED_D(I)/BX
            END DO
          END IF
          WRITE(*,*)
        ELSE
          NFIXED_F=0
          NFIXED_D=0
        END IF
C------------------------------------------------------------------------------
C..............................................................................
C                                                      fit to simple polynomial
C..............................................................................
        IF((IOPC.EQ.1).OR.(IOPC.EQ.2))THEN
          !parametros para el ajuste
          WRITE(*,100) 'Polynomial degree............'
          WRITE(CDUMMY,*) NFIXED_F+NFIXED_D
          NTERMS=READILIM_B(CDUMMY,0,NDEGMAX)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NTERMS
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          NTERMS=NTERMS+1
          WRITE(*,100) 'Asymmetry coefficient.........(xi) '
          WEIGHT=READF_B('1000.0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) WEIGHT
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          IF(IOPC.EQ.2)THEN
            POWER=2.0
            WRITE(*,100) 'Are you weighting with errors.....(y/n) '
            CWEIGHT(1:1)=READC_B('n','yn')
            IF(LECHO) WRITE(*,101) CWEIGHT
            IF(CWEIGHT.EQ.'y')THEN
              EPOWER=2.0
            ELSE
              EPOWER=0.0
            END IF
          ELSE
            WRITE(*,100) 'Power for distances...........(alpha) '
            POWER=READF_B('2.0')
            IF(LECHO)THEN
              WRITE(CDUMMY,*) POWER
              WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
            END IF
            WRITE(*,100) 'Power for errors...............(beta) '
            EPOWER=READF_B('0.0')
            IF(LECHO)THEN
              WRITE(CDUMMY,*) EPOWER
              WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
            END IF
          END IF
          LOOP=.TRUE.
          DO WHILE(LOOP)
            WRITE(*,100) 'Cut-off parameter for errors....(tau) '
            TSIGMA=READF_B('0.0')
            IF(LECHO)THEN
              WRITE(CDUMMY,*) TSIGMA
              WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
            END IF
            IF(TSIGMA.LT.0.0)THEN
              WRITE(*,100) 'WARNING: this number must be >= 0.0.'
              WRITE(*,101) ' Try again!'
            ELSE
              LOOP=.FALSE.
            END IF
          END DO
          WRITE(*,100) 'Side: 1=upper, 2=lower........'
          ILUP=READILIM_B('1',1,2)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) ILUP
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          LUP=(ILUP.EQ.1)
          !parametros para DOWNHILL
          IF(IOPC.EQ.2)THEN
            WRITE(*,100) 'YRMSTOL for coefficients.............'
            YRMSTOL=READF_B('1E-5')
          ELSE
            WRITE(*,100) 'YRMSTOL for DOWNHILL.................'
            YRMSTOL=READF_B('1E-5')
          END IF
          IF(LECHO)THEN
            WRITE(CDUMMY,*) YRMSTOL
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          IF(IOPC.EQ.2)THEN
            WRITE(*,100) 'Nmaxiter..............'
            NEVALMAX=READILIM_B('100',1,1000000)
          ELSE
            WRITE(*,100) 'Nmaxiter in DOWNHILL '
            NEVALMAX=READILIM_B('1000',1,1000000)
          END IF
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NEVALMAX
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          IF(IOPC.EQ.2)THEN
            LINCREMENTAL=.FALSE.
          ELSE
            WRITE(*,100) 'Incremental fit of coefficients...(y/n) '
            CINCREMENTAL(1:1)=READC_B('n','yn')
            IF(LECHO) WRITE(*,101) CINCREMENTAL
            LINCREMENTAL=(CINCREMENTAL.EQ.'y')
          END IF
          !realizamos el ajuste
          CALL PSEUDOFIT(IOPC,LINCREMENTAL,XDATA,YDATA,EYDATA,NDATABUFF,
     +     NTERMS,YRMSTOL,NEVALMAX,WEIGHT,POWER,EPOWER,LUP,TSIGMA,A)
          !deshacemos la normalizacion
          WRITE(*,100)'>>> bx,cx: '
          WRITE(*,*) BX,CX
          WRITE(*,100)'>>> by,cy: '
          WRITE(*,*) BY,CY
          IF(CX.EQ.0.0)THEN
            DO K=0,NTERMS-1
              A(K+1)=A(K+1)*(BX**K)
            END DO
          ELSE
            DO K=0,NTERMS-1
              AA(K+1)=A(K+1)
            END DO
            DO K=0,NTERMS-1
              A(K+1)=0.0
              DO I=K,NTERMS-1
                A(K+1)=A(K+1)+AA(I+1)*REAL(COMBPF(I,I-K))*(BX**K)*
     +           ((-CX)**(I-K))
              END DO
            END DO
          END IF
          A(1)=A(1)+CY
          DO K=0,NTERMS-1
            A(K+1)=A(K+1)/BY
          END DO
          !muestra el ajuste final
          WRITE(*,101) '***********************************************'
          WRITE(*,101) '* Final coefficients:'
          DO K=1,NTERMS
            WRITE(*,'(A6,I2.2,A2,$)') '>>> a(',K-1,')='
            WRITE(*,*) A(K)
          END DO
          WRITE(*,101) '-----------------------------------------------'
C..............................................................................
C                                                       fit to adaptive splines
C..............................................................................
        ELSEIF(IOPC.EQ.3)THEN
          !parametros para el ajuste
          WRITE(*,100) 'Number of knots..................'
          NKNOTS=READILIM_B('@',2,20)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NKNOTS
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          WRITE(*,100) 'Equidistant knot arrangement (y/n/r)....'
          CEQUI(1:1)=READC_B('y','ynr')
          IF(LECHO) WRITE(*,101) CEQUI
          IF(CEQUI.EQ.'y')THEN
            !como los datos no tienen por que venir ordenados, usamos
            !los extremos en el eje X para fijar ahi al menos dos knots
            XKNOT(1)=XMINBUFF
            DO IKNOT=1,NKNOTS-1
              XKNOT(IKNOT+1)=XMINBUFF+
     +         (XMAXBUFF-XMINBUFF)*REAL(IKNOT)/REAL(NKNOTS-1)
            END DO
          ELSEIF(CEQUI.EQ.'r')THEN
            !redistribuimos los knots dejando un número similar de
            !puntos a ajustar entre cada dos knots
            XKNOT(1)=XMINBUFF
            XKNOT(NKNOTS)=XMAXBUFF
            FNPOINTSINTERVAL = REAL(NDATABUFF)/REAL(NKNOTS-1)
            DO I=1,NDATABUFF
              XDATA_SORTED(I)=XDATA(I)
            END DO
            CALL ORDENA1F(NDATABUFF,XDATA_SORTED)
            IF(NKNOTS.GT.2)THEN
              DO K=2,NKNOTS-1
                IDUM=INT(FNPOINTSINTERVAL*(K-1)+0.5)
                XKNOT(K)=(XDATA_SORTED(IDUM)+XDATA_SORTED(IDUM+1))/2.0
              END DO
            END IF
          ELSE
            !fijamos los knots de los extremos y pedimos la insercion
            !manual del resto
            XKNOT(1)=XMINBUFF
            WRITE(*,'(A,I2,$)') 'X-coordinate of knot #',1
            WRITE(*,100) '....................: '
            WRITE(*,*) (XKNOT(1)+CX)/BX
            XKNOT(NKNOTS)=XMAXBUFF
            WRITE(*,'(A,I2,$)') 'X-coordinate of knot #',NKNOTS
            WRITE(*,100) '....................: '
            WRITE(*,*) (XKNOT(NKNOTS)+CX)/BX
            IF(NKNOTS.GT.2)THEN
              DO K=2,NKNOTS-1
                WRITE(*,'(A,I2,$)') 'X-coordinate of knot #',K
                WRITE(*,100) '....................'
                XKNOT(K)=READF_B('@')
                IF(LECHO)THEN
                  WRITE(CDUMMY,*) XKNOT(K)
                  WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
                END IF
                XKNOT(K)=BX*XKNOT(K)-CX
              END DO
              CALL ORDENA1F(NKNOTS,XKNOT)
            END IF
          END IF
          WRITE(*,100) 'Asymmetry coefficient.........(xi) '
          WEIGHT=READF_B('1000.0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) WEIGHT
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          WRITE(*,100) 'Power for distances...........(alpha) '
          POWER=READF_B('2.0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) POWER
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          WRITE(*,100) 'Power for errors...............(beta) '
          EPOWER=READF_B('0.0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) EPOWER
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          LOOP=.TRUE.
          DO WHILE(LOOP)
            WRITE(*,100) 'Cut-off parameter for errors....(tau) '
            TSIGMA=READF_B('0.0')
            IF(LECHO)THEN
              WRITE(CDUMMY,*) TSIGMA
              WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
            END IF
            IF(TSIGMA.LT.0.0)THEN
              WRITE(*,100) 'WARNING: this number must be >= 0.0.'
              WRITE(*,101) ' Try again!'
            ELSE
              LOOP=.FALSE.
            END IF
          END DO
          WRITE(*,100) 'Side: 1=upper, 2=lower........'
          ILUP=READILIM_B('1',1,2)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) ILUP
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          LUP=(ILUP.EQ.1)
          !parametros para DOWNHILL
          WRITE(*,100) 'YRMSTOL for DOWNHILL.................'
          YRMSTOL=READF_B('1E-5')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) YRMSTOL
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          WRITE(*,100) 'Nmaxiter in DOWNHILL '
          NEVALMAX=READILIM_B('1000',1,1000000)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NEVALMAX
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          !semilla para numeros aleatorios
          WRITE(*,100) 'NSEED, negative to call srand(time())..'
          NSEED=READI_B('-1')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) NSEED
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          IF(NSEED.LT.0) RDUMMY=RANRED(NSEED)
          !verbosity
          WRITE(*,100) 'Enhanced verbosity (y/n)................'
          CVERBOSE(1:1)=READC_B('n','yn')
          IF(LECHO) WRITE(*,101) CVERBOSE
          !realizamos el ajuste
          CALL SPLFIT(NDATABUFF,XDATA,YDATA,EYDATA,NKNOTS,XKNOT,
     +     YRMSTOL,NEVALMAX,NSEED,
     +     WEIGHT,POWER,EPOWER,LUP,TSIGMA,
     +     NPLOTMAX,XP,YP,XKNOT(1),XKNOT(NKNOTS),YKNOT,ASPL,BSPL,CSPL)
          !deshacemos la normalizacion en los knots y en los coeficientes
          !muestra el ajuste final
          WRITE(*,101) '***********************************************'
          WRITE(*,100)'>>> bx,cx: '
          WRITE(*,*) BX,CX
          WRITE(*,100)'>>> by,cy: '
          WRITE(*,*) BY,CY
          DO K=1,NKNOTS
            XKNOT(K)=(XKNOT(K)+CX)/BX
            YKNOT(K)=(YKNOT(K)+CY)/BY
          END DO
          DO K=1,NKNOTS-1
            ASPL(K)=ASPL(K)*BX*BX*BX/BY
            BSPL(K)=BSPL(K)*BX*BX/BY
            CSPL(K)=CSPL(K)*BX/BY
          END DO
          !muestra el ajuste final
          WRITE(*,101) '***********************************************'
          WRITE(*,101) '* Final knots:'
          DO K=1,NKNOTS
            WRITE(*,100) '>>> Knot #'
            WRITE(*,'(I2.2,$)') K
            WRITE(*,100) '  X_knot,Y_knot: '
            WRITE(*,*) XKNOT(K),YKNOT(K)
          END DO
          WRITE(*,101) '-----------------------------------------------'
          WRITE(*,101) '* Final coefficients:'
          DO K=1,NKNOTS-1
            WRITE(*,100) '>>> s_3,s_2,s_1 '
            WRITE(*,'(A1,I2.2,A1,I2.2,A2,$)') '[',K,'-',K+1,']:'
            WRITE(*,*) ASPL(K),BSPL(K),CSPL(K)
          END DO
          WRITE(*,101) '-----------------------------------------------'
C..............................................................................
C                                                           something is wrong!
C..............................................................................
        ELSE
          WRITE(*,100) 'IOPC='
          WRITE(*,*) IOPC
          STOP 'FATAL ERROR#3 invalid IOPC'
        END IF
C------------------------------------------------------------------------------
C save result
        COPC=' '
        DO WHILE(COPC.NE.'0')
          WRITE(*,*)
          WRITE(*,101) '(1) Save last fit'
          WRITE(*,101) '(2) Save fit predictions'
          WRITE(*,101) '(C) Save fit coefficients'
          WRITE(*,101) '(N) New fit'
          WRITE(*,101) '(0) EXIT'
          WRITE(*,100) 'Option..................................'
          COPC(1:1)=READC_B('0','012cCnN')
          IF(LECHO) WRITE(*,101) COPC
          IF((COPC.EQ.'n').OR.(COPC.EQ.'N'))THEN
            GOTO 10
          ELSEIF((COPC.EQ.'c').OR.(COPC.EQ.'C'))THEN
            CALL ASKOUTFILE(COEFFFILE)
            OPEN(30,FILE=COEFFFILE,STATUS='NEW',FORM='FORMATTED')
            IF((IOPC.EQ.1).OR.(IOPC.EQ.2))THEN
              WRITE(30,*) NTERMS-1
              DO K=1,NTERMS
                WRITE(30,*) K,A(K)
              END DO
            ELSE
              WRITE(30,*) NKNOTS
              DO K=1,NKNOTS
                WRITE(30,*) K,XKNOT(K),YKNOT(K)
              END DO
              DO K=1,NKNOTS-1
                WRITE(30,*) K,ASPL(K),BSPL(K),CSPL(K)
              END DO
            END IF
            CLOSE(30)
          ELSEIF(COPC.EQ.'0')THEN
            WRITE(*,*)
            WRITE(*,101) 'End of BoundFit execution!'
            STOP
          ELSE
            IF(COPC.EQ.'1')THEN
              !xmin
              WRITE(CDUMMY,*) (XMINBUFF+CX)/BX
              L1=TRUEBEG(CDUMMY)
              L2=TRUELEN(CDUMMY)
              WRITE(*,100) 'Xmin'
              DO L=1,37-(L2-L1+1)
                WRITE(*,100) '.'
              END DO
              XMINF=READF_B(CDUMMY)
              IF(LECHO)THEN
                WRITE(CDUMMY,*) XMINF
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              !xmax
              WRITE(CDUMMY,*) (XMAXBUFF+CX)/BX
              L1=TRUEBEG(CDUMMY)
              L2=TRUELEN(CDUMMY)
              WRITE(*,100) 'Xmax'
              DO L=1,37-(L2-L1+1)
                WRITE(*,100) '.'
              END DO
              XMAXF=READF_B(CDUMMY)
              IF(LECHO)THEN
                WRITE(CDUMMY,*) XMAXF
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              !number of points
              WRITE(*,100) 'Number of points......'
              NDATA=READILIM_B('1000',2,NDATAMAX)
              IF(LECHO)THEN
                WRITE(CDUMMY,*) NDATA
                WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
              END IF
              !generate X data
              DO I=1,NDATA
                XP(I)=XMINF+(XMAXF-XMINF)*REAL(I-1)/REAL(NDATA-1)
              END DO
            ELSEIF(COPC.EQ.'2')THEN
              NDATA=NDATABUFF
              DO I=1,NDATA
                XP(I)=(XDATA(I)+CX)/BX
              END DO
            ELSE
              WRITE(*,101) 'COPC='//COPC
              WRITE(*,101) 'FATAL ERROR#4 invalid COPC'
              STOP
            END IF
            IF((IOPC.EQ.1).OR.(IOPC.EQ.2))THEN !..............simple polynomial
              DO I=1,NDATA
                YP(I)=FPOLY(NTERMS-1,A,XP(I))
              END DO
            ELSEIF(IOPC.EQ.3)THEN !............................adaptive splines
              I0SPL=1 !comenzar buscando en el inicio de la tabla
              DO I=1,NDATA
                CALL CUBSPLX(XKNOT,YKNOT,ASPL,BSPL,CSPL,
     +           NKNOTS,I0SPL,XP(I),YP(I))
              END DO
            ELSE
              WRITE(*,100) 'IOPC='
              WRITE(*,*) IOPC
              STOP 'FATAL ERROR#5 invalid IOPC'
            END IF
            !save data in external ASCII file
            CALL SAVERESULT
          END IF
        END DO
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
