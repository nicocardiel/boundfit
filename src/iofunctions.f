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
C
C******************************************************************************
C******************************************************************************
C Funciones de entrada/salida por teclado, modificadas para trabajar con un
C fichero de comandos en modo BATH (LBATCH=.TRUE.)
C******************************************************************************
C******************************************************************************
C
        CHARACTER*(*) FUNCTION READC_B(CDEF,CVAL)
        IMPLICIT NONE
        CHARACTER*(*) CDEF,CVAL
C
        INTEGER I,L1,L2,LL1,LL2
        INTEGER TRUEBEG,TRUELEN
        INTEGER INDEXR
        INTEGER NERR
        CHARACTER*255 CADENA
        LOGICAL LBATCH
        LOGICAL LOOP
C
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        NERR=0
10      IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100)'['
            WRITE(*,100)CDEF(L1:L2)
            WRITE(*,100)'] ? '
          END IF
        ELSE
          WRITE(*,100)'? '
        END IF
        IF(LBATCH)THEN
          LOOP=.TRUE.
          DO WHILE(LOOP)
            READ(78,'(A)',ERR=20) CADENA
            IF(CADENA(1:1).NE.'#') LOOP=.FALSE. !saltamos comentario en col. 1
          END DO
          LL2=INDEXR(CADENA,'#') !truncamos comentario no en primera columna
          IF(LL2.GT.1)THEN
            CADENA=CADENA(1:LL2-1)
          END IF
          LL1=TRUEBEG(CADENA) !eliminamos espacios por delante
          LL2=TRUELEN(CADENA) !eliminamos espacios por detras
          CADENA=CADENA(LL1:LL2)
          WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(*,'(A)',ERR=20) CADENA
        END IF
        IF(CVAL.EQ.'@')THEN
          IF(TRUELEN(CADENA).EQ.0)THEN
            IF(CDEF.EQ.'@')THEN
              GOTO 10
            END IF
            L1=TRUEBEG(CDEF)
            L2=TRUELEN(CDEF)
            CADENA=CDEF(L1:L2)
          END IF
        ELSE
          IF(TRUELEN(CADENA).EQ.0)THEN
            IF(CDEF.EQ.'@')THEN
              GOTO 10
            END IF
            L1=TRUEBEG(CDEF)
            L2=TRUELEN(CDEF)
            CADENA=CDEF(L1:L2)
          ELSE
            DO I=1,TRUELEN(CADENA)
              IF(INDEX(CVAL,CADENA(I:I)).EQ.0)THEN
                WRITE(*,101)'ERROR: invalid character(s). Try again.'
                IF(CDEF.EQ.'@') WRITE(*,100)'? '
                NERR=NERR+1
                IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
                GOTO 10
              END IF
            END DO
          END IF
        END IF
        READC_B=CADENA
        RETURN
20      WRITE(*,101)'ERROR: invalid entry. Try again.'
        IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        INTEGER FUNCTION READI_B(CDEF)
        IMPLICIT NONE
        CHARACTER*(*) CDEF
C
        INTEGER I,L1,L2
        INTEGER LCOMMENT
        INTEGER N
        INTEGER NERR
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CADENA
        LOGICAL LBATCH
        LOGICAL LOOP
C
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        NERR=0
10      IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100)'['
            WRITE(*,100)CDEF(L1:L2)
            WRITE(*,100)'] ? '
          END IF
        ELSE
          WRITE(*,100)'? '
        END IF
        IF(LBATCH)THEN
          LOOP=.TRUE.
          DO WHILE(LOOP)
            READ(78,'(A)',ERR=20) CADENA
            IF(CADENA(1:1).NE.'#') LOOP=.FALSE.
          END DO
          LCOMMENT=INDEX(CADENA,'#')
          IF(LCOMMENT.NE.0) CADENA=CADENA(1:LCOMMENT-1) !elimina el comentario
          WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(*,'(A)',ERR=20)CADENA
        END IF
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) N
        READI_B=N
        RETURN
20      WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
        IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        INTEGER FUNCTION READILIM_B(CDEF,N1,N2)
        IMPLICIT NONE
        CHARACTER*(*) CDEF
        INTEGER N1,N2
C
        INTEGER I,L1,L2
        INTEGER LCOMMENT
        INTEGER N
        INTEGER NERR
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CDUMMY
        CHARACTER*255 CADENA
        LOGICAL LBATCH
        LOGICAL LOOP
C
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        IF(N2.LT.N1)THEN
          WRITE(*,101)'ERROR: N2.LT.N1 in function: READILIM_B'
          WRITE(*,101)'=> returned value is 0'
          READILIM_B=0
          RETURN
        END IF
        NERR=0
        WRITE(CDUMMY,'(A1,I10,A5,I10,A1)') '(',N1,',...,',N2,')'
        CALL RMBLANK(CDUMMY,CDUMMY,L2)
        WRITE(*,100) CDUMMY(1:L2)
10      IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100)' ['
            WRITE(*,100)CDEF(L1:L2)
            WRITE(*,100)'] ? '
          END IF
        ELSE
          WRITE(*,100)' ? '
        END IF
        IF(LBATCH)THEN
          LOOP=.TRUE.
          DO WHILE(LOOP)
            READ(78,'(A)',ERR=20) CADENA
            IF(CADENA(1:1).NE.'#') LOOP=.FALSE.
          END DO
          LCOMMENT=INDEX(CADENA,'#')
          IF(LCOMMENT.NE.0) CADENA=CADENA(1:LCOMMENT-1) !elimina el comentario
          WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(*,'(A)',ERR=20)CADENA
        END IF
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) N
        READILIM_B=N
C
        IF((N.LT.N1).OR.(N.GT.N2)) GOTO 30
        RETURN
C------------------------------------------------------------------------------
20      WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
C
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
C------------------------------------------------------------------------------
30      WRITE(*,100)'ERROR: invalid number. Valid range is '
        WRITE(CDUMMY,*)N1
        CALL RMBLANK(CDUMMY,CDUMMY,L2)
        WRITE(*,100) CDUMMY(1:L2)//' to '
        WRITE(CDUMMY,*)N2
        CALL RMBLANK(CDUMMY,CDUMMY,L2)
        WRITE(*,101) CDUMMY(1:L2)//'. Try again.'
C
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        REAL FUNCTION READF_B(CDEF)
        IMPLICIT NONE
        CHARACTER*(*) CDEF
C
        INTEGER I,L1,L2
        INTEGER LCOMMENT
        INTEGER NERR
        REAL F
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CADENA
        LOGICAL LBATCH
        LOGICAL LOOP
C
        COMMON/BLKLBATCH/LBATCH
C------------------------------------------------------------------------------
        NERR=0
10      IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100)'['
            WRITE(*,100)CDEF(L1:L2)
            WRITE(*,100)'] ? '
          END IF
        ELSE
          WRITE(*,100)'? '
        END IF
        IF(LBATCH)THEN
          LOOP=.TRUE.
          DO WHILE(LOOP)
            READ(78,'(A)',ERR=20) CADENA
            IF(CADENA(1:1).NE.'#') LOOP=.FALSE.
          END DO
          LCOMMENT=INDEX(CADENA,'#')
          IF(LCOMMENT.NE.0) CADENA=CADENA(1:LCOMMENT-1) !elimina el comentario
          WRITE(*,'(A)') CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(*,'(A)',ERR=20)CADENA
        END IF
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcfghijklmnoprstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCFGHIJKLMNOPRSTUVWXYZ/',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) F
        READF_B=F
        RETURN
20      WRITE(*,101)'ERROR: invalid character(s) found in '//
     +   'number. Try again.'
        IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        SUBROUTINE RMBLANK(C1,C2,L)
        IMPLICIT NONE
        INTEGER L
        CHARACTER*(*) C1,C2
C
        INTEGER I,K,L0
C------------------------------------------------------------------------------
        K=0
        L0=LEN(C1)
        DO I=1,L0
          IF(C1(I:I).NE.CHAR(32))THEN
            K=K+1
            C2(K:K)=C1(I:I)
          END IF
        END DO
        L=K
        L0=LEN(C2)
        IF(L.LT.L0)THEN
          DO I=L+1,L0
            C2(I:I)=' '
          END DO
        END IF
        END
