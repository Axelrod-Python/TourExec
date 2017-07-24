      FUNCTION K35R(J,M,K,L,R, JA)
C BY ABRAHAM GETZLER
C TYPED FROM FORTRAN BY AX, 1,17,79
      k35r=ja    ! Added 7/27/93 to report own old value
      IF(M.EQ.1) FLACK=0.
C FLACK IS THE RELATIVE RECENT UNTRUSTWORTHINESS OF MY PPONENT
      FLACK = (FLACK + J) * .5
C DEFECTIONS HAVE A HALF-LIFE OF ONE ROUND
      K35R = 0
      IF (FLACK.GT.R) K35R=1
      RETURN
      END