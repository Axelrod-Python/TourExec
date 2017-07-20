      FUNCTION K58R(J,M,K,L,R, JA)
C BY GLEN ROWSAM
C TYPED BY JM
      k58r=ja    ! Added 7/27/93 to report own old value
      IF (M .GT. 1) GOTO 99
      KAM = 0
      NPHA = 0
99    IF (KAM .GT. 6) GOTO 87
      IF (NPHA .GE. 1) GOTO 89
      IF ((M / 18) * 18 .EQ. M .AND. KAM .GT. 2) KAM = KAM - 1
      IF ((M / 6) * 6 .NE. M) GOTO 88
      IF (K .LT. M) GOTO 10
      IF (K * 10 .LT. M * 15) GOTO 11
      IF (K .LT. M * 2) GOTO 12
      IF (K * 10 .LT. M * 25) GOTO 13
      GOTO 88
10    KAM = KAM + 2
11    KAM = KAM + 1
12    KAM = KAM + 1
13    KAM = KAM + 1
      NPHA = 2
      GOTO 87
89    NPHA = NPHA - 1
      IF (NPHA .EQ. 0) GOTO 87
88    K58R = 0
      GOTO 86
87    K58R = 1
86    RETURN
      END
