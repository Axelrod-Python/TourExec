      FUNCTION K70R(J,M,K,L,R, JA)
C BY ROBERT PEBLY
C EDITED FROM BASIC BY AX 3/10/79
C TYPED BY JM 3/16/79
      k70r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 1) JZ = 0
      IF (JZ .EQ. J) GOTO 510
      K70R = 0
      IF (R .GT. .2) K70R = 1
506   JZ = K70R
      RETURN
510   K70R = JZ
      GOTO 506
      END
