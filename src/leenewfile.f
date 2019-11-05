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
C Si el fichero se lee correctamente ISTATUS retorna 1. En caso contrario 
C retorna 0. 
C------------------------------------------------------------------------------
        SUBROUTINE LEENEWFILE(ISTATUS)
        IMPLICIT NONE
        INTEGER ISTATUS
C included files
        INCLUDE 'lenlinea.inc'
        INCLUDE 'ndatamax.inc'
C functions
        INTEGER READI_B
        INTEGER SYSTEMFUNCTION
        INTEGER TRUEBEG,TRUELEN
        REAL READF_B
        CHARACTER*255 READC_B
C variables
        INTEGER I
        INTEGER L1,L2
        INTEGER ISYSTEM
        INTEGER NSKIP,NDATA
        INTEGER NX,NY,NEY
        INTEGER NDATABUFF,NDATABUFF_INITIAL
        INTEGER ISTATUSEXTRAE
        INTEGER NCOMMENTS
        REAL XDATA(NDATAMAX)
        REAL XDATA_INITIAL(NDATAMAX)
        REAL YDATA(NDATAMAX)
        REAL EYDATA(NDATAMAX)
        REAL FEXTRAE
        REAL XMINBUFF,XMAXBUFF
        REAL YMINBUFF,YMAXBUFF
        REAL EYMINBUFF,EYMAXBUFF
        REAL XMINFIT,XMAXFIT
        REAL BX,CX,BY,CY
        CHARACTER*1 CWHOLE
        CHARACTER*1 CRENORM
        CHARACTER*50 CDUMMY
        CHARACTER*255 INFILE
        CHARACTER*(LENLINEA) CLINEA
        LOGICAL LOGFILE
        LOGICAL LUNREAD
        LOGICAL LECHO
C common blocks
        COMMON/BLKLECHO/LECHO
        COMMON/BLKNDATABUFF/NDATABUFF,NDATABUFF_INITIAL
        COMMON/BLKXYDATA/XDATA,YDATA,EYDATA
        COMMON/BLKXDATA_INITIAL/XDATA_INITIAL
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF
        COMMON/BLKNORM/BX,CX,BY,CY
C------------------------------------------------------------------------------
        ISTATUS=0                          !salvo que se demuestre lo contrario
        LUNREAD=.FALSE.                    !indica si algun dato no se ha leido
5       WRITE(*,100) 'Input data file name (wildcars allowed) '
        INFILE=READC_B('*','@')
        L1=TRUEBEG(INFILE)
        L2=TRUELEN(INFILE)
        IF(LECHO) WRITE(*,101) INFILE(L1:L2)
        IF((INDEX(INFILE,'*').NE.0).OR.
     +   (INDEX(INFILE,'?').NE.0))THEN
          ISYSTEM=SYSTEMFUNCTION('ls '//INFILE(L1:L2))
          GOTO 5
        END IF
C
        INQUIRE(FILE=INFILE,EXIST=LOGFILE)
C
        IF(.NOT.LOGFILE)THEN
          WRITE(*,101) 'ERROR: this file does not exist. Try again.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          GOTO 5
        END IF
C------------------------------------------------------------------------------
C Leemos fichero ASCII
        OPEN(10,FILE=INFILE,STATUS='OLD',FORM='FORMATTED')
        WRITE(*,100) 'No. of initial rows to be skipped.......'
        NSKIP=READI_B('0')
        IF(LECHO)THEN
          WRITE(CDUMMY,*) NSKIP
          WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
        END IF
        IF(NSKIP.LT.0) NSKIP=0
        IF(NSKIP.GT.0)THEN
          DO I=1,NSKIP
            READ(10,*,END=901)
          END DO
        END IF
        WRITE(*,100) 'No. of rows to be read (0=ALL)..........'
        NDATA=READI_B('0')
        IF(LECHO)THEN
          WRITE(CDUMMY,*) NDATA
          WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
        END IF
        IF(NDATA.GT.NDATAMAX)THEN
          WRITE(*,101) 'FATAL ERROR: this number of data is too large.'
          WRITE(*,101) 'You must modify the parameter NDATAMAX.'
          STOP
        END IF
C..............................................................................
7       WRITE(*,100) 'Column No. for X data.......................'
        NX=READI_B('@')
        IF(LECHO)THEN
          WRITE(CDUMMY,*) NX
          WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
        END IF
        IF(NX.LT.1)THEN
          WRITE(*,101) 'ERROR: this number must be > 0. Try again.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          GOTO 7
        END IF
8       WRITE(*,100) 'Column No. for Y data.......................'
        NY=READI_B('@')
        IF(LECHO)THEN
          WRITE(CDUMMY,*) NY
          WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
        END IF
        IF(NY.LT.1)THEN
          WRITE(*,101) 'ERROR: this number must be > 0. Try again.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          GOTO 8
        END IF
9       WRITE(*,100) 'Column No. for err(Y) data (0=NONE).....'
        NEY=READI_B('0')
        IF(LECHO)THEN
          WRITE(CDUMMY,*) NEY
          WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
        END IF
        IF(NEY.LT.0)THEN
          WRITE(*,101) 'ERROR: this number must be >= 0. Try again.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          GOTO 9
        END IF
C------------------------------------------------------------------------------
        WRITE(*,*)
        WRITE(*,101) 'Reading file...'
        NCOMMENTS=0
        I=0
10      READ(10,101,END=902) CLINEA
        IF(TRUELEN(CLINEA).EQ.0) GOTO 10 !saltamos lineas en blanco
        !sustituimos tabuladores por espacios en blanco
        IF(INDEX(CLINEA,CHAR(9)).NE.0) CALL CLEANTAB(CLINEA)
        IF(CLINEA(1:1).EQ.'#')THEN !ignora lineas con comentarios
          NCOMMENTS=NCOMMENTS+1
          GOTO 10
        END IF
        I=I+1
        IF(I.GT.NDATAMAX)THEN
          WRITE(*,101) 'FATAL ERROR: data number is too large.'
          WRITE(*,101) 'You must modify the parameter NDATAMAX.'
          STOP
        END IF
C leemos variable X
        XDATA(I)=FEXTRAE(CLINEA,NX,ISTATUSEXTRAE)
        IF(ISTATUSEXTRAE.EQ.0) GOTO 903
        IF(ISTATUSEXTRAE.EQ.-1)THEN
          I=I-1
          LUNREAD=.TRUE.
          GOTO 10
        END IF
C leemos variable Y
        YDATA(I)=FEXTRAE(CLINEA,NY,ISTATUSEXTRAE)
        IF(ISTATUSEXTRAE.EQ.0) GOTO 903
        IF(ISTATUSEXTRAE.EQ.-1)THEN
          I=I-1
          LUNREAD=.TRUE.
          GOTO 10
        END IF
C leemos error en variable Y (si procede)
        IF(NEY.GT.0)THEN
          EYDATA(I)=FEXTRAE(CLINEA,NEY,ISTATUSEXTRAE)
          IF(ISTATUSEXTRAE.EQ.0) GOTO 903
          IF(ISTATUSEXTRAE.EQ.-1)THEN
            I=I-1
            LUNREAD=.TRUE.
            GOTO 10
          END IF
          IF(EYDATA(I).LE.0.0)THEN
            WRITE(*,101) 'WARNING: error.le.0.0 while reading:'
            WRITE(*,100) '> Point #, X,Y,EY: '
            WRITE(*,*) I,XDATA(I),YDATA(I),EYDATA(I)
          END IF
        ELSE
          !si no hay errores, usamos un mismo valor para todos los
          !puntos; de esta forma las fórmulas siguen siendo válidas para
          !cualquier valor de la potencia beta
          EYDATA(I)=1.
        END IF
C------------------------------------------------------------------------------
        IF(I.EQ.NDATA) GOTO 902
        GOTO 10
C------------------------------------------------------------------------------
901     CLOSE(10)
        WRITE(*,101) 'FATAL ERROR: unexpected end of file reached'
        STOP
C..............................................................................
902     CLOSE(10)
        IF(NDATA.EQ.0)THEN
          IF(I.EQ.0)THEN
            WRITE(*,*)
            WRITE(*,101) 'FATAL ERROR: unexpected end of file reached'
            STOP
          ELSE
            NDATABUFF=I
            ISTATUS=1
            WRITE(*,101) 'File read and closed!'
          END IF
        ELSE
          IF(I.NE.NDATA)THEN
            WRITE(*,*)
            WRITE(*,101) 'FATAL ERROR: unexpected end of file reached'
            STOP
          ELSE
            NDATABUFF=I
            ISTATUS=1
            WRITE(*,101) 'File read and closed!'
          END IF
        END IF
C conservamos valores iniciales para la prediccion final
        NDATABUFF_INITIAL=NDATABUFF
        DO I=1,NDATABUFF
          XDATA_INITIAL(I)=XDATA(I)
        END DO
C mostramos datos basicos sobre los puntos leidos
        WRITE(*,*)
        WRITE(*,100) '>>> No. of rows with comments (unread)......: '
        WRITE(*,*) NCOMMENTS
        WRITE(*,100) '>>> No. of rows read........................: '
        WRITE(*,*) NDATABUFF
        CALL FINDMML(NDATABUFF,1,NDATABUFF,XDATA,XMINBUFF,XMAXBUFF)
        CALL FINDMML(NDATABUFF,1,NDATABUFF,YDATA,YMINBUFF,YMAXBUFF)
        CALL FINDMML(NDATABUFF,1,NDATABUFF,EYDATA,EYMINBUFF,EYMAXBUFF)
        WRITE(*,100) '>>> Xmin, Xmax..............................: '
        WRITE(*,*) XMINBUFF,XMAXBUFF
        WRITE(*,100) '>>> Ymin, Ymax..............................: '
        WRITE(*,*) YMINBUFF,YMAXBUFF
        WRITE(*,100) '>>> EYmin, EYmax............................: '
        WRITE(*,*) EYMINBUFF,EYMAXBUFF
        IF(LUNREAD)THEN
          WRITE(*,101) 'WARNING: there were unread data'
        END IF
C------------------------------------------------------------------------------
        WRITE(*,*)
        WRITE(*,100) 'Are you using the whole x-range...(y/n) '
        CWHOLE(1:1)=READC_B('y','yn')
        IF(LECHO) WRITE(*,101) CWHOLE
        IF(CWHOLE(1:1).EQ.'n')THEN
          WRITE(*,100) 'Xmin'
          WRITE(CDUMMY,*) XMINBUFF
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          DO I=1,37-(L2-L1+1)
            WRITE(*,100) '.'
          END DO
          XMINFIT=READF_B(CDUMMY)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) XMINFIT
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          WRITE(*,100) 'Xmax'
          WRITE(CDUMMY,*) XMAXBUFF
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          DO I=1,37-(L2-L1+1)
            WRITE(*,100) '.'
          END DO
          XMAXFIT=READF_B(CDUMMY)
          IF(LECHO)THEN
            WRITE(CDUMMY,*) XMAXFIT
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          NDATABUFF=0
          DO I=1,NDATABUFF_INITIAL
            IF((XDATA(I).GE.XMINFIT).AND.(XDATA(I).LE.XMAXFIT))THEN
              NDATABUFF=NDATABUFF+1
              XDATA(NDATABUFF)=XDATA(I)
              YDATA(NDATABUFF)=YDATA(I)
              EYDATA(NDATABUFF)=EYDATA(I)
            END IF
          END DO
          WRITE(*,100) '>>> No. of data points in the new range.....: '
          WRITE(*,*) NDATABUFF
          IF(NDATABUFF.EQ.0)THEN
            WRITE(*,*)
            WRITE(*,101) 'FATAL ERROR: unexpected end of file reached'
            STOP
          END IF
          CALL FINDMML(NDATABUFF,1,NDATABUFF,XDATA,XMINBUFF,XMAXBUFF)
          CALL FINDMML(NDATABUFF,1,NDATABUFF,YDATA,YMINBUFF,YMAXBUFF)
          CALL FINDMML(NDATABUFF,1,NDATABUFF,EYDATA,EYMINBUFF,EYMAXBUFF)
          WRITE(*,100) '>>> Xmin, Xmax..............................: '
          WRITE(*,*) XMINBUFF,XMAXBUFF
          WRITE(*,100) '>>> Ymin, Ymax..............................: '
          WRITE(*,*) YMINBUFF,YMAXBUFF
          WRITE(*,100) '>>> EYmin, EYmax............................: '
          WRITE(*,*) EYMINBUFF,EYMAXBUFF
        END IF
C------------------------------------------------------------------------------
C normalizacion de los datos al intervalo [-1,+1] en ambos ejes
        WRITE(*,*)
        WRITE(*,100) 'Normalise to [-1,+1] (y/n) or '//
     +   '(r)escale data ranges '
        CRENORM(1:1)=READC_B('y','ynr')
        IF(LECHO) WRITE(*,101) CRENORM
        IF(CRENORM.EQ.'y')THEN
          IF(XMINBUFF.EQ.XMAXBUFF)THEN
            WRITE(*,101) 'ERROR: normalization in X-axis not possible'
            WRITE(*,100) '>>> Xmin, Xmax: '
            WRITE(*,*) XMINBUFF,XMAXBUFF
            BX=1.0
            CX=0.0
          ELSE
            BX=2./(XMAXBUFF-XMINBUFF)
            CX=(XMINBUFF+XMAXBUFF)/(XMAXBUFF-XMINBUFF)
          END IF
          IF(YMINBUFF.EQ.YMAXBUFF)THEN
            WRITE(*,101) 'ERROR: normalization in Y-axis not possible'
            WRITE(*,100) '>>> Ymin, Ymax: '
            WRITE(*,*) YMINBUFF,YMAXBUFF
            BY=1.0
            CY=0.0
          ELSE
            BY=2./(YMAXBUFF-YMINBUFF)
            CY=(YMINBUFF+YMAXBUFF)/(YMAXBUFF-YMINBUFF)
          END IF
        ELSEIF(CRENORM.EQ.'r')THEN
          WRITE(*,100) 'Multiplicative factor for X data '
          BX=READF_B('1.0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) BX
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          CX=0.0
          WRITE(*,100) 'Multiplicative factor for Y data '
          BY=READF_B('1.0')
          IF(LECHO)THEN
            WRITE(CDUMMY,*) BY
            WRITE(*,101) CDUMMY(TRUEBEG(CDUMMY):TRUELEN(CDUMMY))
          END IF
          CY=0.0
        ELSE
          BX=1.0
          CX=0.0
          BY=1.0
          CY=0.0
        END IF
        WRITE(*,100)'>>> bx, cx: '
        WRITE(*,*) BX,CX
        WRITE(*,100)'>>> by, cy: '
        WRITE(*,*) BY,CY
        DO I=1,NDATABUFF
          XDATA(I)=BX*XDATA(I)-CX
          YDATA(I)=BY*YDATA(I)-CY
          EYDATA(I)=BY*EYDATA(I)
        END DO
        CALL FINDMML(NDATABUFF,1,NDATABUFF,XDATA,XMINBUFF,XMAXBUFF)
        CALL FINDMML(NDATABUFF,1,NDATABUFF,YDATA,YMINBUFF,YMAXBUFF)
        CALL FINDMML(NDATABUFF,1,NDATABUFF,EYDATA,EYMINBUFF,EYMAXBUFF)
        WRITE(*,100) '>>> Xmin, Xmax..............................: '
        WRITE(*,*) XMINBUFF,XMAXBUFF
        WRITE(*,100) '>>> Ymin, Ymax..............................: '
        WRITE(*,*) YMINBUFF,YMAXBUFF
        WRITE(*,100) '>>> EYmin, EYmax............................: '
        WRITE(*,*) EYMINBUFF,EYMAXBUFF
C------------------------------------------------------------------------------
        RETURN
C..............................................................................
903     CLOSE(10)
        WRITE(*,100) 'ERROR: while reading data #'
        WRITE(*,*) I+1
        STOP
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
