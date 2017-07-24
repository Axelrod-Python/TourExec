      FUNCTION K61R(ISPICK,ITURN,K,L,R, JA)
C BY DANNY C. CHAMPION
C TYPED BY JM 3/27/79
      k61r=ja    ! Added 7/27/93 to report own old value
      IF (ITURN .EQ. 1) K61R = 0
      IF (ISPICK .EQ. 0) ICOOP = ICOOP + 1
      IF (ITURN .LE. 10) RETURN
      K61R = ISPICK
      IF (ITURN .LE. 25) RETURN
      K61R = 0
      COPRAT = FLOAT(ICOOP) / FLOAT(ITURN)
      IF (ISPICK .EQ. 1 .AND. COPRAT .LT. .6 .AND. R .GT. COPRAT)
     +K61R = 1
      RETURN
      END
