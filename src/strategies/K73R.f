      FUNCTION K73R(J,M,K,L,R, JA)
C BY GEORGE ZIMMERMAN
C TYPED BY JM 3/20/79
      k73r=ja    ! Added 7/27/93 to report own old value
      IF (M .GT. 1) GOTO 10
      IAGGD = 4
      IDUNU = 0
      IDUNB = 0
      IPAYB = 8
      ITEST = 1
      IPOST = 0
10    K73R = IPOST
      IF (J .NE. ITEST) RETURN
      IF (ITEST .EQ. 1) IDUNU = IDUNU + 1
      IF (ITEST .EQ. 0) IDUNB = IDUNB + 1
      IF  ((IDUNU .LT. IAGGD) .AND. (IDUNB .LT. IPAYB)) RETURN
       IDUNU = 0
      IDUNB = 0
      IPOST = 0
      IF (J .EQ. 1) IPOST = 1
      K73R = IPOST
      ITEST = 0
      IF (IPOST .EQ. 0) ITEST = 1
      IF (ITEST .EQ. 0) GOTO 20
      IAGGD = IAGGD - 3 + (K / M)
      IF (IAGGD .LE. 0) IAGGD = 1
      RETURN
20    IPAYB = INT(1.6667 * FLOAT(IAGGD + 1))
      RETURN
      END
