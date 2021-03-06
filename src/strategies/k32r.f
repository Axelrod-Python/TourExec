      FUNCTION K32R(J,M,K,L,R, JA)
C BY CHARLES KLUEPFEL
C EDITED FROM BASIC BY AX, 1.19.79
      k32r=ja    ! Added 7/27/93 to report own old value
      IF(M.GT.1) GO TO 520
C # OF HIS COOPS AFTER MY DEF.
      C1=0
C # OF HIS DEFECTIONS AFTER MY DEF.
      C2=0
C # OF HIS COOPS AFTER MY COOPERATION
      C3=0
C # OF HIS DEF. AFTER MY COOPERATION
      C4=0
C HIS 3RD PREV. CHOICE
      J2=0
C HIS 2ND PREV. CHOICE
      J1=0
C MY 2ND PREV. CHOICE
      I2=0
C MY PREV. CHOICE
      I1=0
C PROB. OF MY RESPONDING IN KIND
      P=0
      GO TO 550
  520 IF(M.EQ.2) GO TO 550
C RECORD HIS RESPONSES
      IF(I2.EQ.0) GO TO 530
      IF(J.EQ.0) GO TO 528
      C2=C2+1
      GO TO 540
  528 C1=C1+1
      GO TO 540
  530 IF(J.EQ.0) GO TO 537
      C4=C4+1
      GO TO 540
  537 C3=C3+1
C AFTER M. 26 TRY DETECTING RNDOM
  540 IF(M.LT.27) GO TO 550
      IF (C1.LT.((C1+C2) - 1.5*SQRT(C1+C2)) / 2 ) GO TO 550
      IF (C4.LT.((C3+C4) - 1.5*SQRT(C3+C4)) / 2 ) GO TO 550
      K32R=1
      GO TO 590
  550 K32R=0
      IF(J1.NE.J) GO TO 570
      IF(J2.NE.J1) GO TO 580
C RESPOND IN KIND TO 3 IN ROW.
      K32R = J
      GO TO 590
C PROB .6 OF GIVING DEF. AFTER SINGLE DEF.
  570 P=.6
      IF(J.EQ.1) GO TO 585
C PROB .7 GIVING COOP AFTER SINGLE COOP.
      P=.7
      GO TO 585
C PROB .9 RESP. IN KIND TO 2 IN ROW
  580 P=.9
  585 K32R=J
      IF(R.LT.P) GO TO 590
      K32R=1-J
C PUSH BACK CHOICES
  590 J2=J1
      J1=J
      I2=I1
      I1=K32R
      RETURN
      END
