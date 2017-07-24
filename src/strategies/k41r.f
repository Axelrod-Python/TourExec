       FUNCTION K41R(J,M,K,L,R, JA)
C BY HERB WEINER
C TYPED BY AX, 1/29/79
      INTEGER LAST(12)
      k41r=ja    ! Added 7/27/93 to report own old value
      IF(M.NE.1)GOTO 10
      ICASE=1
      IFORGV=0
      DO 5 I=1,12
5     LAST(I)=0
10    GOTO(100,200,300),ICASE
100   K41R=J
      ICASE=J+1
      GOTO 400
200   K41R=J
      ICASE=3
      IF(J.EQ.1)ICASE=1
      GOTO 400
300   K41R=J
      IF(IFORGV.LT.M)K41R=0
      IFORGV=IFORGV+20*J
      ICASE=1
400   LSUM=LAST(1)
      DO 405 I=2,12
      LSUM=LSUM+LAST(I)
405   LAST(I-1)=LAST(I)
      LAST(12)=J
      IF(LSUM.GE.5)K41R=1
      RETURN
      END
