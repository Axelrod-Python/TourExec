      FUNCTION K67R(J,M,K,L,R,JA)
C EDITED FROM BASIC FROM AX. 3/10/79
C TYPED BY JM 3/16/80
C BY CRAIG FEATHERS
      REAL NO,NK
      K67R=JA       ! Added 7/32/93 to report own old value
      IF (M .NE. 1) GOTO 510
      S = 0
      AD = 5
      NO = 0
      NK = 1
      AK = 1
      FD = 0
      C = 0
510   IF (FD .NE. 2) GOTO 520
      FD = 0
      NO = (NO * NK + 3 - 3 * J + 2 * K67R - K67R * J) / (NK + 1)
      NK = NK + 1
520   IF (FD .NE. 1) GOTO 530
      FD = 2
      AD = (AD * AK + 3 - 3 * J + 2 * K67R - K67R * J) / (AK + 1)
      AK = AK + 1
530   IF (J .EQ. 0) GOTO 540
      S = S + 1
      GOTO 545
540   S = 0
      C = C + 1
545   K67R = 0
      IF (ABS(FD - 1.5) .EQ. .5) GOTO 599
      IF (K .LT. 2.25 * M) GOTO 575
      P = .95 - (AD + NO - 5) / 15 + 1./ M**2 - J / 4.
      IF (R .LE. P) GOTO 599
      K67R = 1
      FD = 1
      GOTO 599
575   IF (K .LT. 1.75 * M) GOTO 595
      P = .25 + C / M - S * .25 + (K - L) / 100. + 4. / M
      IF (R .LE. P) GOTO 599
      K67R = 1
      GOTO 599
595   K67R = J
599   RETURN
      END
