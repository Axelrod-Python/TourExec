      FUNCTION K74RXX(J,M,K,L,R,JA)
C BY EDWARD FRIEDLAND
C TYPED BY JM 3/20/79
c k74dummy added by ax 7/22/93
      K74RXX=JA       ! Added 7/32/93 to report own old value
      IF (M .NE. 1) GOTO 9
      ALPHA = 1.0
      BETA = .3
      IOLD = 0
      QCA = 0
      QNA = 0
      QCB = 0
      QNB = 0
      K74RXX = 0
      k74dummy=0
      JSW = 0
      JS4 = 0
      JS11 = 0
      JR = 0
      JL = 0
      JT = 0
      JSM = 1
9     IF (JR .NE. 1) GOTO 10
      K74RXX = 1
      k74dummy=1
      RETURN
10    IF (M .LE. 2) GOTO 30
      IF (IOLD .EQ. 1) GOTO 20
      IF (J .EQ. 0) QCA = QCA + 1
      QNA = QNA + 1
      ALPHA = QCA / QNA
      QCA = QCA * .8
      QNA = QNA * .8
      GOTO 30
20    IF (J .EQ. 0) QCB = QCB + 1
      QNB = QNB + 1
      BETA = QCB / QNB
      QCB = QCB * .8
      QNB = QNB * .8
30    IOLD = K74dummy
C CHECK FOR RANDOM
      IF (M .EQ. 37) GOTO 80
      IF (M .GT. 37) GOTO 15
      IF (M .EQ. 1) GOTO 15
      IF (J .EQ. JL) JSM = JSM + 1
      IF (JSM .GE. 3) JS4 = 1
      IF (JSM .GE. 11) JS11 = 1
      IF (J .NE. JL) JSW = JSW + 1
      JSM = 1
      JT = JT + J
15    POLC = 6 * ALPHA - 8 * BETA - 2
      POLALT = 4 * ALPHA - 5 * BETA - 1
      IF (POLC .EQ. 0) GOTO 40
      IF (POLALT .GE. 0) GOTO 70
      GOTO 60
40    IF (POLC .GE. POLALT) GOTO 50
50    K74RXX = 0
      k74dummy = 0
      RETURN
60    K74RXX = 1
      k74dummy=1
      RETURN
c70    K74R = 1 - K74R
70    K74RXX = 1-k74dummy
      RETURN
80    IF (JS4 .EQ. 0) GOTO 15
      IF (JS11 .EQ. 1) GOTO 15
      IF (JT .LE. 10) GOTO 15
      IF (JT .GE. 26) GOTO 15
      IF (JSW .GE. 26) GOTO 15
      JR = 1
      GOTO 9
      END
