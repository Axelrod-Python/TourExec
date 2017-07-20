      FUNCTION K62R(J,M,K,L,R,JA)
C BY HOWARD R HOLLANDER
C TYPED BY JM 2/25/79
      K62R=JA       ! Added 7/32/93 to report own old value
      IF (M .NE. 1) GOTO 505
      JOLD = 0
      IRAN = 23 * R + 1
505   K62R = 0
      IF (M .NE. IRAN) GOTO 510
      K62R = 1
      IRAN = 23 * R + M + 1
      GOTO 515
510   IF ((JOLD .EQ. 1) .AND. (J .EQ. 1)) K62R = 1
515   JOLD = J
      RETURN
      END
