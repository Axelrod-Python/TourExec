      FUNCTION K69R(J,M,K,L,R,JA)
C BY JOHANN JOSS
C EDITED FROM BASIC BY AX, 3/10/79
C TYPED BY JM 3/16/79
      K69R=JA       ! Added 7/32/93 to report own old value
      IF (M .EQ. 1) GOTO 600
      IF (J .EQ. 1) GOTO 512
      C = C + 1
512   IF (S .EQ. 1) GOTO 700
      IF (S .EQ. 2) GOTO 800
      IF (S .EQ. 3) GOTO 900
      IF (S .EQ. 4) GOTO1000
      IF (S .EQ. 5) GOTO 1100
600   S = 1
      F = 0
      D = 0
      C = 0
      K69R = 0
      RETURN
700   IF (R .LT. 0.1) GOTO 720
702   IF (J .EQ. 0) GOTO 708
      D = D + 1
      GOTO 710
708   D = 0
710   IF (D .GT. 20) GOTO 820
      IF (C .LT. 0.7 * (M - 3)) GOTO 800
      K69R = J
      RETURN
720   S = 5
      K69R = 1
      RETURN
800   S = 2
      IF (J .EQ. 0) GOTO 808
      D = D + 1
      GOTO 810
808   D = 0
810   IF (D .GT. 10) GOTO 830
      K69R = 1
      RETURN
820   S = 3
      K69R = 0
      D = 0
      RETURN
830   S= 3
      K69R = 1
      RETURN
900   IF (J .EQ. 0) GOTO 908
      D = D + 1
      GOTO 910
908   D = 0
910   IF (D .GT. 20) GOTO 820
      K69R = J
      RETURN
1000  IF (J .EQ. 0) GOTO 1006
      F = F + 1
      IF (F .GT. 3) GOTO 820
1006  S = 1
      K69R = 0
      RETURN
1100  S = 4
      GOTO 702
      END
