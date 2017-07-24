      FUNCTION K48R(J,M,K,L,R,JA)
C BY GEORGE HUFFORD
C TYPED BY JM
      DIMENSION IARRAY(5),IPO2(5)
C NOT NICE, DETERMINISTIC, FORGIVING
      DATA IPO2/2,4,3,5,1/
      K48R=JA       ! Added 7/32/93 to report own old value
      IF (M .EQ. 1) GOTO 1
      IF (M .LE. 5) GOTO 2
      MM = MOD(M-1,5) + 1
      K48R = IARRAY(MM)
      IF (MM .NE. 1) RETURN
      KOLD = K5
      K5 = K - KLAST
      KLAST = K
      IF (KOLD .GT. K5) ICHAN = -ICHAN
      IF (KOLD .GT. K5) IPO1 = IPO1 + ICHAN
      IF (IPO1 .LT. 1) IPO1 = 0
      IF (IPO1 .GT. 5) IPO1 = 6
      IF (IPO1 .LT. 1 .OR. IPO1 .GT. 5) RETURN
      IARRAY (IPO2(IPO1)) = IARRAY(IPO2(IPO1)) + ICHAN
      IPO1 = IPO1 + ICHAN
      K48R = IARRAY(MM)
      RETURN
1     KOLD = 0
      K5 = 0
      KLAST = 0
      DO 3 I =1,5
3     IARRAY(I) = 0
      MM = 0
      ICHAN = 1
      IPO1 = 1
2     IARRAY(IPO2(IPO1)) = J
      IPO1 = IPO1 + J
      K48R = J
      RETURN
      END
