       FUNCTION K66R(J,M,K,L,R, JA)
C BY RAY MIKKELSON
C TYPED BY JM 3/16/80
      k66r=ja    ! Added 7/27/93 to report own old value
      IF (M .GT. 1) GOTO 20
      D = 0
      J2 = -3
20    D = D + J
      RR = D / FLOAT(M)
      J2 = J2 - 1 + 3 * J
      IF (J2 .GT. 10) J2 = 10
      IF (J2 .LT. -5) J2 = -5
      IF (M .LT. 3) GOTO 90
      IF (J2 .LT. 3) GOTO 90
      IF (M .GT. 10) GOTO 58
      J2 = -1
      GOTO 80
58    IF (RR .LT. .15) GOTO 90
80    K66R = 1
      GOTO 95
90    K66R = 0
95    RETURN
      END
