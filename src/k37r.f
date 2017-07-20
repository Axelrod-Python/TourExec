      FUNCTION K37R(J,M,K,L,R, JA)
C BY GEORGE LEFEVRE
C EDITED FROM BASIC BY AX, 2/11/79
C TYPED BY JM
      k37r=ja    ! Added 7/27/93 to report own old value
      IF  (M .GT. 1) GOTO 500
      ND = 0
500   K37R = 0
C TIMES HE DEFECTED
      ND = ND + J
      IF (5*ND .GT. M) K37R = 1
      RETURN
      END
