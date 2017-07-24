      FUNCTION K52R(J,M,K,L,R,JA)
C BY DAVID A. SMITH
C EDITED FROM BASIC BY AX,2/11/79
C TYPED BY JM
      INTEGER D8,D9
      K52R=JA       ! Added 7/32/93 to report own old value
      K52R = 0
      IF (M .GT. 1) GOTO 305
      D9 = 0
      D8 = 0
305   D9 = D9 + 1
      IF (J .GT. 0) GOTO 320
      D9 = 0
320   IF (D9 .LT. 2) GOTO 345
      K52R = 1
      IF (D9 .LT. (5+ 3*D8)) GOTO 345
      D9 = 0
      D8 = D8 + 1
345   IF (R .GT. .05) GOTO 355
      K52R = 1 - K52R
355   RETURN
      END
