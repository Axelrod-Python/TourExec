      FUNCTION K82R(J,M,K,L,R,JA)
C BY ROBERT A LEYLAND
C TYPED BY JM 3/22/79
      K82R=JA       ! Added 7/32/93 to report own old value
      K82R = J
      IF (M .EQ. 1) GOTO 2180
      I5 = I5 + J
      D4 = D4 + J
      IF (J .EQ. 0 .AND. I5 .GT. 1) GOTO 2100
2010  IF (M .LT. 30) RETURN
      IF (I3 .EQ. 0) GOTO 2190
      IF (ABS(D4/(M - 1.0) - 0.5) .LT. 0.1) X = X - 0.2
      IF (I2 .EQ. 1) GOTO 2070
2030  IF (R .GT. X) GOTO 2150
      I2 = I1
      RETURN
2070  IF (J .EQ. 0) GOTO 2120
      X = X + 0.15
      IF (X .GT. 1.0) X = 1.0
      GOTO 2190
2100  IF (I5 .GT. 5) GOTO 2200
      I5 = 0
      GOTO 2010
2120  X = X - 0.05
      IF (X .LT. 0.0) X = 0.0
      I2 = 0
      IF (X .GE. 0.3) RETURN
      GOTO 2030
2150  K82R = 1
      I1 = 1
      RETURN
2180  X = 0.75
      I5 = 0
      D4 = 0.0
2190  I2 = 0
      I3 = 1
      IF (I5 .GT. 5) I3 = 0
2200  I5 = 0
      I1 = 0
      K82R = 0
      RETURN
      END
