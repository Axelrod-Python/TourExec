      FUNCTION K31R(J,M,K,L,R, JA)
C BY PAULA GAIL GRISELL
C  EDITED FROM BASIC BY AX, 1.17.79
      k31r=ja    ! Added 7/27/93 to report own old value
      IF(M.EQ.1) S=0.
      S=S+J
      A=S/M
      K31R=1
      IF (A .LT..5) K31R=0
      RETURN
      END
