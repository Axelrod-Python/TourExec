      FUNCTION K51R(J,M,K,L,R,JA)
C BY JOHN WILLIAM COLBERT
C TYPED BY JM
      K51R=JA     ! Added 7/32/93 to report own old value
      IF (M .GT. 8) GOTO 5
        K51R = 0
      IF (M .EQ. 6) K51R = 1
        LASTI = 0
        GOTO 10
5     K51R = 0
      LASTI = LASTI - 1
      IF (LASTI .EQ. 3) K51R = 1
      IF (LASTI .GT. 0) GOTO 10
      IF (J .EQ. 1) K51R = 1
      IF (J .EQ. 1) LASTI = 4
10    RETURN
      END
