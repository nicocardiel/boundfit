C Busca la posicion en la que aparece en caracter CH en la cadena
C CSTRING, empezando la busqueda por el final
        INTEGER FUNCTION INDEXR(CSTRING,CH)
        IMPLICIT NONE
        CHARACTER*(*) CSTRING
        CHARACTER*1 CH
C
        INTEGER TRUELEN
C
        INTEGER I,L
C------------------------------------------------------------------------------
        INDEXR=0 !salvo que se demuestre lo contrario
C
        L=TRUELEN(CSTRING)
        IF(L.EQ.0) RETURN
C
        DO I=L,1,-1
          IF(CSTRING(I:I).EQ.CH)THEN
            INDEXR=I
            RETURN
          END IF
        END DO
        END
