        SUBROUTINE CLEANTAB(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER TRUELEN
C
        INTEGER L
C------------------------------------------------------------------------------
        DO L=1,TRUELEN(CADENA)
          IF(CADENA(L:L).EQ.CHAR(9)) CADENA(L:L)=' '
        END DO
C
        END
