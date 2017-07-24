      FUNCTION K68R(J,M,K,L,R, JA)
C BY FRANSOIS LEYVRAZ
C EDITED FROM BASIC BY AX, 3/10/79
C TYPED BY JM 3/16/79
      k68r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 1) GOTO 600
      IF (J1 * J .EQ. 1) GOTO 540
      IF (J2 * 2 + J1 + J * 2 + J .EQ. 1) GOTO 550
      IF (J2 * 2 + J1 * 2 + J .EQ. 1) GOTO 560
      K68R = 0
      GOTO 650
540   IF (R .LT. 0.75) GOTO 550
      K68R = 0
      GOTO 650
550   K68R = 1
      GOTO 650
560   IF (R .LT. 0.5) GOTO 550
      K68R = 0
      GOTO 650
600   J2 = 0
      J1 = 0
      K68R = 0
      RETURN
650   J2 = J1
      J1 = J
      RETURN
      END
