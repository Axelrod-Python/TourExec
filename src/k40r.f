       FUNCTION K40R(J,M,K,L,R, JA)
C BY ROBERT ADAMS
C EDITED FROM BASIC BY AX, 1,18,79
      k40r=ja    ! Added 7/27/93 to report own old value
      IF(M.NE.1) GO TO 505
      S=3
      W=0
      Q=.8
  505 S=S+1
      IF(J.NE.1) GO TO 510
      W=W+1
      Q=Q/2
  510 IF(M.GE.3) GO TO 520
      K40R=0
      RETURN
  520 IF(J.EQ.1) GO TO 522
      GO TO 530
  522 W=W+1
      IF(W.GT.2.AND.(W/3.EQ.IFIX(W/3)).OR.(W-1)/3.EQ.IFIX((W-1)/3))
     1GO TO 901
      GO TO 550
  901 S=1
      Q=Q/2
      GO TO 580
  530 GO TO 580
  550 IF(R.GE.Q) GO TO 560
      K40R=0
      Q=Q/2
      RETURN
  560 Q=Q/2
      K40R=1
      RETURN
  580 IF(S.EQ.1.OR.S.EQ.2) GO TO 1000
c Ax added ()
  590 IF(W.GT.2.AND.(W/3.EQ.IFIX(W/3).OR.(W-1)/3.EQ.IFIX((W-1)/3)))
     1GO TO 901
      K40R=0
      RETURN
 1000 K40R=1
      RETURN
      END
