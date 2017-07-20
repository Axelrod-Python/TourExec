      FUNCTION K85R(J,M,K,L,R, JA)
C BY ROGER B FALK AND JAMES M LANGSTED
C EDITED FROM BASIC BY AX 3/18/79
C TYPED BY JM 4/4/79
C INITIALIZE ON FIRST MOVE AND COOPERATE
      IMPLICIT REAL (A-Z)
      INTEGER J,M,K,L,JA,K85R
      k85r=ja    ! Added 7/27/93 to report own old value
      IF (M .NE. 1) GOTO 100
      J2 = 0
      J4 = 0
      J8 = 0
      J0 = 0
      F4 = 0
      F8 = 0
      F0 = 0
      K85R = 0
      F1 = 0
      C = 0
      D = 0
      T = 0
      I1 = 0
      I2 = 0
      I3 = 0
      I4 = 0
      GOTO 900
C SERVICE SHIFT REGESTERS J0 AND F0
100   J5 = J0 / 1E07
      J3 = INT(J5)
      J8 = J5 - J3
      J8 = J8 * 1E07
      F5 = F0 / 1E07
      F3 = INT(F5)
      F8 = F5 - F3
      F8 = F8 * 1E07
      J0 = J8 * 10 + 5
      F0 = F8 * 10 + 5
C SERVICE COUNTERS TO TALLY NUMBER OF TIMES
C HIS VARIOUS RESPONSES FOLLOW MY VARIOUS RESPONSES
      IF (F1 .EQ. 0) GOTO 175
      IF (J .EQ. 0) I1 = I1 + 1
      IF (J .EQ. 1) I2 = I2 + 1
      GOTO 185
175   IF (J .EQ. 0) I3 = I3 + 1
      IF (J .EQ. 1) I4 = I4 + 1
C CHECK FOR RANDOMNESS AFTER FIRST 20 MOVES
185   IF (M .LE. 20) GOTO 245
      I5 = I1 + 1E-6
      I6 = I2 + 1E-6
      X8 = I3 + 1E-6
      I8 = I4 + 1E-6
      A = I5 / I6
      B = X8 / I8
      IF (A .GT. 1.5) GOTO 245
      IF (A .LT. .5) GOTO 245
      IF (B .GT. 1.5) GOTO 245
      IF (B .LT. .5) GOTO 245
      GOTO 910
C CHECK IF WE ARE IN TIT FOR TAT MODE
245   IF (T .EQ. 1) GOTO 920
C CHECK IF HE CONTINUALY DEFECTS
      IF (J0 .EQ. 11111111)GOTO 920
C CHECK IF WE ARE IN D THEN C MODE
      IF (C .EQ. 1) GOTO 980
C CHECK IF HE HAS COOPERATED TWICE IN A ROW
C IN FIRST 30 MOVES
      Z1 = J0 / 100
      Z2 = INT(Z1)
      Z3 = Z1 - Z2
      J2 = Z3 * 100
      IF (M .GT. 30) GOTO 295
      IF (J2 .NE. 11) GOTO 295
      GOTO 390
C CHECK IF  HE IS PLAYING TIT FOR TAT
295   Z4 = J0 / 10000
      Z5 = INT (Z4)
      Z6 = Z4 - Z5
      J4 = Z6 * 10000
      W8 = F0 / 10000
      Z8 = INT(W8)
      Z9 = W8 - Z8
      F4 = Z9 * 10000
      IF (J4 .NE. 1011) GOTO 350
      IF (F4 .NE. 111) GOTO 350
      GOTO 930
C CHECK IF HE IS OVER 3 DEFECTS AHEAD
350   Y1 = I2 + I4
      Y2 = I1 + I2 + 3
      IF (Y1 .GE. Y2) GOTO 910
C USE BASIC RULES
      IF (K85R .NE. 1) GOTO 380
      IF (J .NE. 0) GOTO 380
      GOTO 940
380   IF (D .EQ. 1) GOTO 995
      IF (K85R .NE.0) GOTO 400
390   IF (J .NE. 0) GOTO 400
      GOTO 900
400   IF (K85R .NE. 0) GOTO 415
      IF (J .NE. 1) GOTO 415
      GOTO 910
415   IF (K85R .NE. 0) GOTO 950
C COOPERATE RETURN
900   F1 = K85R
      K85R = 0
      RETURN
C DEFECT RETURN
910   F1 = K85R
      K85R = 1
      D = 0
      RETURN
C TIT FOR TAT MODE RETURN
920   T = 1
      K85R = J
      RETURN
C CC RETURN (FIRST TIME)
930   C = 1
      GOTO 981
C DEFECT AND RESET D RETURN
940   F1 = K85R
      K85R = 1
      D = 0
      RETURN
C D THEN C RETURN (FIRST TIME)
950   F1 = K85R
      K85R = 1
      D = 1
      RETURN
C CC RETURN (SECOND TIME)
980   C = 0
981   F1 = K85R
      K85R = 0
      RETURN
C D THEN C RETURN (SECOND TIME)
995   F1 = K85R
      K85R = 0
      D = 0
      RETURN
      END
