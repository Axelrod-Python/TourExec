      FUNCTION K44R(J,M,K,L,R, JA)
C BY WM. ADAMS
C EDITED FROM BASIC BY AX, 1/26/79
      k44r=ja    ! Added 7/27/93 to report own old value
      IF(M.NE.1) GOTO 520
C COUNT HIS DEFECTS
      MC=0
C ADJUST FACTOR
      F=2
C NR. DEFECTS ALLOWED
      AM=4
C COOP AT FIRST
520   IF(M.LT.3) GOTO 1800
      MC=MC+J
C COOP UNTIL THRESHOLD
      IF(MC.LT.AM) GOTO 1800
      IF(MC.EQ.AM) GOTO 1900
C ADJUST: LOWER THRESHOLD
      AM=AM/F
      MC=0
C ANOTHER CHANCE WITH PROB. P
      IF(R.LT.AM) GOTO 1800
1900  K44R=1
      RETURN
1800  K44R=0
      RETURN
      END