      FUNCTION K91R(J,M,K,L,R, JA)
C BY JONATHAN PINKLEY
C MODIFIED FROM K15C BY JM 3/27/79
      DIMENSION IPOL(11,4), QC(4), QN(4), E(11)
      k91r=ja    ! Added 7/27/93 to report own old value
      IF (M .NE. 1) GO TO 30
C INITIAL BELIEFS
      X = .999
      PX = .001
      Y = .001
      PY = .999
      Z = .999
      PZ = .001
      W = .001
      PW = .999
      QC(1) = 1.999
      QC(2) =1.999
      QC(3) = 0.001
      QC(4) = 0.001
      DO 10 N = 1, 4
   10 QN(N) = 2
C DEFINE POLICIES(FIRST,WHAT IF OUTCOME=1)
      DATA IPOL /4*0, 7*1, 0, 3*1, 3*0, 4*1, 3*0, 1, 2*0, 1, 0, 0, 1, 1,
     1 2*0, 1, 0, 0, 1, 0, 0, 1, 0, 1/
       IOLD=0
      K91R = 0
      N = 0
      GO TO 100
C UPDATE STATS OF HIS CONTINGENCIES
C N IS OUTCOME OF M-2
   30 IF (M .LE. 2) GO TO 100
      IF (J .EQ. 0) QC(N) = QC(N) + 1
      QN(N) = QN(N) + 1
C REVERSE  Y AND Z
      GO TO (40, 60, 50, 70), N
   40 X = QC(1) / QN(1)
      PX = 1 - X
      GO TO 100
   50 Y = QC(3) / QN(3)
      PY = 1 - Y
      GO TO 100
   60 Z = QC(2) / QN(2)
      PZ = 1 - Z
      GO TO 100
   70 W = QC(4) / QN(4)
      GO TO 100
C CALC EXPECTATIONS OF POLICIES
  100 E(1) = (3*Z) / (Z + PX)
      E(2) = (3*(Y*Z + W*PZ) + 5*Z*PX + PX*PZ) / (Y*Z + W*PZ + PX + Z*
     1 PX + PX*PZ)
      E(3) = (3*W*Y + 5*W*PX + PX*PZ) / (W*Y + 2*W*PX + PX*PZ)
      E(4) = (3*W*PY + 5*Z*PX + PX*PY) / (W*PY + PX*PY + Z*PX + PX*PY)
      E(5) = (3*Z + 5*X*Z + Z*PX) / (1 - X*Y - W*PX + 2*Z)
      E(6) = (8*W*Z + Z*PX) / (2*W*Z + W*PY + Z*PX)
      E(7) = (3*Z*PY + 5*X*Z + Z*PY) / (2*Z*PY + PW*PY + X*Z)
      E(8) = (3*(Y*Z + W*PZ) + 5*(Z*PW + W*X) + 1 - X*Y - Z*PY) /(Y *Z +
     1 W*PZ + 2 - 2*X*Y - W*PX + Z*PW + W*X - Z*PY)
      E(9) = (3*W*Y + 5*W + 1 - X*Y - Z*PY) / (2*W + 1 - X*Y - Z*PY)
      E(10) = (3*W*PY + 5*(Z*PW + W*X) + PY) / (PY + Z*PW + W*X + PY)
      E(11) = (5*W + PY) / (W + PY)
C FIND POL WITH MAX E
      IBEST = 1
      BESTE = E(1)
      DO 80 I = 2, 11
        IF (E(I) .LE. BESTE) GO TO 80
        IBEST = I
        BESTE = E(I)
   80 CONTINUE
C CALC OUTCOME FOR USE IN CHOICE AND NEXT MV STATS
   90 N = 2 *  IOLD + J + 1
C CHOICE(CHOSEN POLICY,PREV OUTCOME)
      K91R = IPOL(IBEST,N)
       IOLD=K91R
      RETURN
      END FUNCTION
