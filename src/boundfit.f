        PROGRAM BOUNDFIT
        IMPLICIT NONE
C included files
        INCLUDE 'version.inc'
        INCLUDE 'ndatamax.inc'
        INCLUDE 'nknotsmax.inc'
        INCLUDE 'ndegmax.inc'
C additional parameters
        INTEGER NPLOTMAX
        PARAMETER (NPLOTMAX=1000)
C functions
        INTEGER READI_B
        INTEGER READILIM_B
        REAL RANRED
        REAL READF_B
        REAL FPOLY
        CHARACTER*255 READC_B
C variables
        INTEGER I
        INTEGER IOPC
        INTEGER ISTATUS
        INTEGER NDATABUFF
        INTEGER NTERMS
        INTEGER ILUP
        INTEGER NEVALMAX
        INTEGER IKNOT,NKNOTS
        INTEGER NSEED
        INTEGER NDATA
        INTEGER I0SPL
        REAL XDATA(NDATAMAX)
        REAL YDATA(NDATAMAX)
        REAL EYDATA(NDATAMAX)
        REAL XMINBUFF,XMAXBUFF
        REAL YMINBUFF,YMAXBUFF
        REAL WEIGHT,POWER
        REAL TSIGMA
        REAL YRMSTOL
        REAL A(NDEGMAX+1)
        REAL XKNOT(NKNOTSMAX),YKNOT(NKNOTSMAX)
        REAL ASPL(NKNOTSMAX),BSPL(NKNOTSMAX),CSPL(NKNOTSMAX)
        REAL RDUMMY
        REAL XP(NDATAMAX),YP(NDATAMAX)
        REAL XMINF,XMAXF
        CHARACTER*1 CERR
        CHARACTER*1 COPC
        CHARACTER*50 CDUMMY
        CHARACTER*255 INFILE
        LOGICAL LOOP
        LOGICAL LUP
C common blocks
        COMMON/BLKINFILE/INFILE
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA,EYDATA
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKOUT_NDATA/NDATA
        COMMON/BLKOUT_XY/XP,YP
C------------------------------------------------------------------------------
C welcome message
        WRITE(*,101) '************************************************'
        WRITE(*,101) '       Welcome to boundfit '//
     +   '(version '//VERSION//')'
        WRITE(*,101) '------------------------------------------------'
        WRITE(*,101) 'For more details see Cardiel (2009) or visit:'
        WRITE(*,101) 'http://www.ucm.es/info/Astrof/software/boundfit/'
        WRITE(*,101) '************************************************'
        WRITE(*,*)
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
        WRITE(*,101) '(1) boundary fitting with a simple polynomial'
        WRITE(*,101) '(2) boundary fitting with adaptive splines'
        WRITE(*,101) '(0) EXIT'
        WRITE(*,100) 'Select type of fit'
        IOPC=READILIM_B('0',0,2)
        IF(IOPC.EQ.0) STOP 'End of program execution!'
C------------------------------------------------------------------------------
C..............................................................................
C                                                      fit to simple polynomial
C..............................................................................
        IF(IOPC.EQ.1)THEN
          !parametros para el ajuste
          WRITE(*,100) 'Polynomial degree'
          NTERMS=READILIM_B('@',0,10)
          NTERMS=NTERMS+1
          WRITE(*,101) '(Note: WEIGHT=1.0 is equivalent to a '//
     +     'normal fit to a simple polynomial)'
          WRITE(*,100) 'WEIGHT for pseudofit '
          WEIGHT=READF_B('1000.0')
          WRITE(*,100) 'POWER for pseudofit '
          POWER=READF_B('2.0')
          WRITE(*,100) 'Which side: 1=upper, 2=lower '
          ILUP=READILIM_B('@',1,2)
          LUP=(ILUP.EQ.1)
          WRITE(*,100) 'Are you considering error bars (y/n) '
          CERR(1:1)=READC_B('n','yn')
          IF(CERR.EQ.'y')THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              WRITE(*,100) 'Times sigma to fit data (0.0=none) '
              TSIGMA=READF_B('1.0')
              IF(TSIGMA.LT.0.0)THEN
                WRITE(*,100) 'WARNING: this number must be >= 0.0.'
                WRITE(*,101) ' Try again!'
              ELSE
                LOOP=.FALSE.
              END IF
            END DO
          ELSE
            TSIGMA=0.0
          END IF
          !parametros para DOWNHILL
          WRITE(*,100) 'YRMSTOL for DOWNHILL '
          YRMSTOL=READF_B('1E-5')
          WRITE(*,100) 'NEVALMAX for DOWNHILL '
          NEVALMAX=READILIM_B('1000',10,1000000)
          !realizamos el ajuste
          CALL PSEUDOFIT(XDATA,YDATA,EYDATA,NDATABUFF,NTERMS,YRMSTOL,
     +     NEVALMAX,WEIGHT,POWER,LUP,TSIGMA,A)
C..............................................................................
C                                                       fit to adaptive splines
C..............................................................................
        ELSEIF(IOPC.EQ.2)THEN
          !parametros para el ajuste
          WRITE(*,100) 'Number of knots '
          NKNOTS=READILIM_B('@',2,20)
          !como los datos no tienen por que venir ordenados, usamos
          !los extremos en el eje X para fijar ahi al menos dos knots
          XKNOT(1)=XMINBUFF
          DO IKNOT=1,NKNOTS-1
            XKNOT(IKNOT+1)=XMINBUFF+
     +       (XMAXBUFF-XMINBUFF)*REAL(IKNOT)/REAL(NKNOTS-1)
          END DO
          WRITE(*,101) '(Note: WEIGHT=1.0 is equivalent to a '//
     +     'normal fit by splines)'
          WRITE(*,100) 'WEIGHT for pseudofit '
          WEIGHT=READF_B('1000.0')
          WRITE(*,100) 'POWER for pseudofit '
          POWER=READF_B('2.0')
          WRITE(*,100) 'Which side: 1=upper, 2=lower '
          ILUP=READILIM_B('@',1,2)
          LUP=(ILUP.EQ.1)
          WRITE(*,100) 'Are you considering error bars (y/n) '
          CERR(1:1)=READC_B('n','yn')
          IF(CERR.EQ.'y')THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              WRITE(*,100) 'Times sigma to fit data (0.0=none) '
              TSIGMA=READF_B('1.0')
              IF(TSIGMA.LT.0.0)THEN
                WRITE(*,100) 'WARNING: this number must be >= 0.0.'
                WRITE(*,101) ' Try again!'
              ELSE
                LOOP=.FALSE.
              END IF
            END DO
          ELSE
            TSIGMA=0.0
          END IF
          !parametros para DOWNHILL
          WRITE(*,100) 'YRMSTOL for DOWNHILL '
          YRMSTOL=READF_B('1E-5')
          WRITE(*,100) 'NEVALMAX for DOWNHILL '
          NEVALMAX=READILIM_B('1000',10,1000000)
          !semilla para numeros aleatorios
          WRITE(*,101) '(Note: NSEED must be > 0 to make the '//
     +     'merging-knots process repeatable)'
          WRITE(*,100) 'NSEED, negative to call srand(time()) '
          NSEED=READI_B('-1')
          IF(NSEED.LT.0) RDUMMY=RANRED(NSEED)
          !realizamos el ajuste
          CALL SPLFIT(NDATABUFF,XDATA,YDATA,EYDATA,NKNOTS,XKNOT,
     +     YRMSTOL,NEVALMAX,NSEED,
     +     WEIGHT,POWER,LUP,TSIGMA,
     +     NPLOTMAX,XP,YP,XKNOT(1),XKNOT(NKNOTS),YKNOT,ASPL,BSPL,CSPL)
C..............................................................................
C                                                           something is wrong!
C..............................................................................
        ELSE
          WRITE(*,100) 'IOPC='
          WRITE(*,*) IOPC
          STOP 'FATAL ERROR#2 in program boundfit.f'
        END IF
C------------------------------------------------------------------------------
C save result
        COPC=' '
        DO WHILE(COPC.NE.'0')
          WRITE(*,101) '(1) Save last fit into buffer'
          WRITE(*,101) '(2) Save fit predictions into buffer'
          IF(IOPC.EQ.2)THEN
            WRITE(*,101) '(K) Save knots'
          END IF
          WRITE(*,101) '(N) New fit'
          WRITE(*,101) '(0) EXIT'
          WRITE(*,100) 'Option '
          IF(IOPC.EQ.1)THEN
            COPC(1:1)=READC_B('0','012nN')
          ELSE
            COPC(1:1)=READC_B('0','012kKnN')
            IF(COPC.EQ.'k') COPC='K'
          END IF
          IF((COPC.EQ.'n').OR.(COPC.EQ.'N')) GOTO 10
          IF(COPC.NE.'0')THEN
            IF(COPC.EQ.'1')THEN
              WRITE(*,100) 'Xmin '
              WRITE(CDUMMY,*) XMINBUFF
              XMINF=READF_B(CDUMMY)
              WRITE(*,100) 'Xmax '
              WRITE(CDUMMY,*) XMAXBUFF
              XMAXF=READF_B(CDUMMY)
              WRITE(*,100) 'Number of points '
              NDATA=READILIM_B('1000',2,NDATAMAX)
              DO I=1,NDATA
                XP(I)=XMINF+(XMAXF-XMINF)*REAL(I-1)/REAL(NDATA-1)
              END DO
            ELSEIF(COPC.EQ.'2')THEN
              NDATA=NDATABUFF
              DO I=1,NDATA
                XP(I)=XDATA(I)
              END DO
            ELSEIF(COPC.EQ.'K')THEN
              NDATA=NKNOTS
              DO I=1,NDATA
                XP(I)=XKNOT(I)
                YP(I)=YKNOT(I)
              END DO
            ELSE
              WRITE(*,101) 'FATAL ERROR#3 in function boundfit'
              WRITE(*,101) 'COPC='//COPC
              STOP
            END IF
            IF(COPC.NE.'K')THEN
              IF(IOPC.EQ.1)THEN !.............................simple polynomial
                DO I=1,NDATA
                  YP(I)=FPOLY(NTERMS-1,A,XP(I))
                END DO
              ELSEIF(IOPC.EQ.2)THEN !..........................adaptive splines
                I0SPL=1 !comenzar buscando en el inicio de la tabla
                DO I=1,NDATA
                  CALL CUBSPLX(XKNOT,YKNOT,ASPL,BSPL,CSPL,
     +             NKNOTS,I0SPL,XP(I),YP(I))
                END DO
              ELSE
                WRITE(*,100) 'IOPC='
                WRITE(*,*) IOPC
                STOP 'FATAL ERROR#4 in subroutine otherfit.f'
              END IF
            END IF
            !save data in external ASCII file
            CALL SAVERESULT
          END IF
        END DO
        STOP 'End of program execution!'
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
