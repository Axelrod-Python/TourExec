      FUNCTION K87R(J,M,K,L,R, JA)
C BY E E H SCHURMANN
C EDITED FROM BASIC BY AX 3/25/79
C TYPED BY JM 3/31/79
      k87r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 1) GOTO 695
      S = 2 * J + H + 1
      IF (Z .EQ. 1) GOTO 630
      IF (J .EQ. 0) GOTO 692
      Z = 1
630   IF (S .GT. 1) GOTO 650
      Q6 = Q6 * .57 + .43
      GOTO 680
650   IF (S .EQ. 4) GOTO 670
      Q6 = .5 * Q6
      GOTO 680
670   Q6 = .74 * Q6 + .104
680   K87R = 1
      H = 1
      IF (R .GT. Q6) RETURN
692   K87R = 0
      H = 0
      RETURN
695   Z = 0
      Q6 = .5
      S = 0
      K87R = 0
      H = 0
      RETURN
      END
