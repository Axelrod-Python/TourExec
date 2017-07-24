      FUNCTION K72R(J,M,K,L,R, JA)
C BY EDWARD C WHITE, JR.
C TYPED BY JM 3/22/79; COR BY AX 3/31/79
      k72r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 1) JOLD = 0
      K72R = 0
      IF (M .EQ. 1) JCOUNT = 0
      JOLD = J
      IF (JOLD .EQ. 1) JCOUNT = JCOUNT + 1
      N = 1
      IF (JOLD .EQ. 1 .AND. M .GT. 10) N = ALOG(FLOAT(M))
      IF (R .LE. ((N * JCOUNT) / M)) K72R = 1
      RETURN
      END
