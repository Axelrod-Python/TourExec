      FUNCTION K80R(J,M,K,L,R, JA)
C BY ROBYN M DAWES AND MARK BATELL
C TYPED BY JM 3/22/79
      k80r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 1) GOTO 10
      IF (MODE .EQ. 1) GOTO 35
      IF (J .EQ. 1) GOTO 20
      GOTO 15
5     INOC = M - INOD
      T1 = 1.6667 ** INOD
      T2 = 0.882 ** INOC
      TEST = T1 * T2
      IF (TEST .GE. 5.) GOTO 30
      GOTO 15
10    MODE = 0
      INOD = 0
      INOC = 0
      T1 = 0
      T2 = 0.
      TEST = 0.
15    K80R = 0
      GOTO 40
20    INOD = INOD + 1
      GOTO 5
30    MODE = 1
35    K80R = 1
40    RETURN
      END
