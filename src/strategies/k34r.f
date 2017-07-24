      FUNCTION K34R(J,M,K,L,R, JA)
C BY JAMES W. FRIEDMAN
C TYPED FROM FORTRAN BY AX, 1.17,79
      k43r=ja    ! Added 7/27/93 to report own old value
      K34R=0
      IF(M.EQ.1) JT=0
      JT=JT+J
      IF(JT.GT.0) K34R=1
      RETURN
      END FUNCTION
