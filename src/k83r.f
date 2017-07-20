       FUNCTION K83R(JPICK,MOVEN,I,J,RAND, JA)
C BY PAUL E BLACK
C TYPED BY JM 3/31/79
      DIMENSION JHIS(5)
      k83r=ja    ! Added 7/27/93 to report own old value
      IF (MOVEN .GT. 5) GOTO 20
      IF (MOVEN .NE. 1) GOTO 10
      JTOT = 0
      MCNT = 1
10    K83R = 0
      JHIS(MOVEN) = JPICK
      JTOT = JTOT + JPICK
      RETURN
20    JTOT = JTOT - JHIS(MCNT) + JPICK
      JHIS(MCNT) = JPICK
      MCNT = MCNT + 1
      IF (MCNT .GT. 5) MCNT = 1
      K83R = 0
      IF (RAND * 25 .LT. JTOT * JTOT - 1) K83R = 1
      RETURN
      END
