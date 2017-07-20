       FUNCTION K46R(J,M,K,L,R, JA)
C BY GRAHAM J. EATHERLEY
C TYPED FROM FORTRAN BY AX, 1/26/79
      k46r=ja    ! Added 7/27/93 to report own old value
      IF(M.EQ.1) NJ=0
      NJ=NJ+J
      K46R=0
      IF(J.EQ.0) RETURN
      P=FLOAT(NJ)/FLOAT(M-1)
      IF(R.LT.P) K46R=1
      RETURN
      END
