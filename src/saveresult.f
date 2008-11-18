        SUBROUTINE SAVERESULT
        IMPLICIT NONE
C
        INCLUDE 'ndatamax.inc'
C
        CHARACTER*255 READC_B
C
        INTEGER I
        INTEGER NDATA
        REAL XP(NDATAMAX),YP(NDATAMAX)
        CHARACTER*255 OUTFILE
        LOGICAL LOGFILE
C
        COMMON/BLKOUT_NDATA/NDATA
        COMMON/BLKOUT_XY/XP,YP
C------------------------------------------------------------------------------
        LOGFILE=.TRUE.
        DO WHILE(LOGFILE)
          WRITE(*,100) 'Output ASCII file name'
          OUTFILE=READC_B('@','@')
          INQUIRE(FILE=OUTFILE,EXIST=LOGFILE)
          IF(LOGFILE)THEN
            WRITE(*,101) 'ERROR: this file already exist. Try again.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          END IF
        END DO
C
        OPEN(20,FILE=OUTFILE,STATUS='NEW',FORM='FORMATTED')
        DO I=1,NDATA
          WRITE(20,*) XP(I),YP(I)
        END DO
        CLOSE(20)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
