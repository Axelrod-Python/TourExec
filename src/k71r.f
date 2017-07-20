      FUNCTION K71R(J,M,K,L,R,JA)
C BY JAMES E HILL
C TYPED BY JM 3/16/79
      K71R=JA       ! Added 7/32/93 to report own old value
      IF (M .EQ. 1) GOTO 1700
      IF (M .EQ. 2) GOTO 1600
      IF (J .EQ. 0) GOTO 1000
      IB = IB + 1
      IF (IB .EQ. 2) GOTO 500
      K71R = 0
500   K71R = 1
      IB = 0
      GOTO 1710
1000  IA = IA + 1
      IF (IA .EQ. 2) GOTO 110
      K71R = 0
      GOTO 1710
110   K71R = 1
      IA = 0
      GOTO 1710
1600  K71R = 1
      IF (J .EQ. 1) K71R = 0
      GOTO 1710
1700  IA = 0
      IB = 0
      K71R = 0
1710  RETURN
      END
