      FUNCTION K79R(J,M,K,L,R, JA)
C BY DENNIS AMBUEHL AND KEVIN HICKEY
C FROM CARDS BY JM 3/16/79
      DIMENSION JBACK(5)
C      COOPERATES IF OPPONENT COOPERATED ON MAJORITY OF LAST PLAYS
      k79r=ja    ! Added 7/27/93 to report own old value
      IF (M.EQ.1) GO TO 3000
      IF (M.LT.6) GO TO 4000
      I1 = 0
      DO 1500 I2 = 1,5
 1500 I1 = I1 + JBACK(I2)
      IF (I1.LT.3) GO TO 1000
      K79R = 1
      GO TO 2000
 3000 DO 2500 I2 = 1,5
 2500 JBACK(I2) = 0
 1000 K79R = 0
 2000 DO 3500 I2 = 1,4
 3500 JBACK(I2) = JBACK(I2 + 1)
      JBACK(5) = J
      RETURN
 4000 K79R = J
      GO TO 2000
      END
