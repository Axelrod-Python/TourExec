      FUNCTION K65R(J,M,K,L,R, JA)
C BY MARK F. BATELL
C TYPED BY JM 3/15/79
      k65r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 1) GOTO 10
      GOTO 20
10    LASTD = 0
      DIFF = 0
      TOTD = 0
      K65R = 0
      GOTO 100
20    IF (TOTD .GE. 10) GOTO 90
      IF (J .EQ. 1) GOTO 30
      K65R = 0
      GOTO 100
30    TOTD = TOTD + 1
      IF (TOTD .GE. 10) GOTO 90
      IF (LASTD .EQ. 0) GOTO 40
      DIFF = M - LASTD
      IF (DIFF .LE. 4) GOTO 80
40    LASTD = M
      K65R = 0
      GOTO 100
80    TOTD = 10
90    K65R = 1
100   RETURN
      END
