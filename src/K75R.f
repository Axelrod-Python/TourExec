      FUNCTION K75R(J,M,K,L,R,JA)
C BY P D HARRINGTON
C TYPED BY JM 3/20/79
      DIMENSION HIST(4,2),ROW(4),COL(2),ID(2)
      K75R=JA       ! Added 7/32/93 to report own old value
      IF (M .EQ. 2) GOTO 25
      IF (M .GT. 1) GOTO 10
      DO 5 IA = 1,4
      DO 5 IB = 1,2
5     HIST(IA,IB) = 0
      IBURN = 0
      ID(1) = 0
      ID(2) = 0
      IDEF = 0
      ITWIN = 0
      ISTRNG = 0
      ICOOP = 0
      ITRY = 0
      IRDCHK = 0
      IRAND = 0
      IPARTY = 1
      IND = 0
      MY = 0
      INDEF = 5
      IOPP = 0
      PROB = .2
      K75R = 0
      RETURN
10    IF (IRAND .EQ. 1) GOTO 70
      IOPP = IOPP + J
      HIST(IND,J+1) = HIST(IND,J+1) + 1
      IF (M .EQ. 15 .OR. MOD(M,15) .NE. 0 .OR. IRAND .EQ. 2) GOTO 25
      IF (HIST(1,1) / (M - 2) .GE. .8) GOTO 25
      IF (IOPP * 4 .LT. M - 2 .OR. IOPP * 4 .GT. 3 * M - 6) GOTO 25
      DO 12 IA = 1,4
12    ROW(IA) = HIST(IA,1) + HIST(IA,2)
      DO 14 IB = 1,2
      SUM = .0
      DO 13 IA = 1,4
13    SUM = SUM + HIST(IA,IB)
14    COL(IB) = SUM
      SUM = .0
      DO 16 IA = 1,4
      DO 16 IB = 1,2
      EX = ROW(IA) * COL(IB) / (M - 2)
      IF (EX .LE. 1.) GOTO 16
      SUM = SUM + ((HIST(IA,IB) - EX) ** 2) / EX
16    CONTINUE
      IF (SUM .GT. 3) GOTO 25
      IRAND = 1
      K75R = 1
      RETURN
25    IF (ITRY .EQ. 1 .AND. J .EQ. 1) IBURN = 1
      IF (M .LE. 37 .AND. J .EQ. 0) ITWIN = ITWIN + 1
      IF (M .EQ. 38 .AND. J .EQ. 1) ITWIN = ITWIN + 1
      IF (M .GE. 39 .AND. ITWIN .EQ. 37 .AND. J .EQ. 1) ITWIN = 0
      IF (ITWIN .EQ. 37) GOTO 80
      IDEF = IDEF * J + J
      IF (IDEF .GE. 20) GOTO 90
      IPARTY = 3 - IPARTY
      ID(IPARTY) = ID(IPARTY) * J + J
      IF (ID(IPARTY) .GE. INDEF) GOTO 78
      IF (ICOOP .GE. 1) GOTO 80
      IF (M .LT. 37 .OR. IBURN .EQ. 1) GOTO 34
      IF (M .EQ. 37) GOTO 32
      IF (R .GT. PROB) GOTO 34
32    ITRY = 2
      ICOOP = 2
      PROB = PROB + .05
      GOTO 92
34    IF (J .EQ. 0) GOTO 80
      GOTO 90
70    IRDCHK = IRDCHK + J * 4 - 3
      IF (IRDCHK .GE. 11) GOTO 75
      K75R = 1
      RETURN
75    IRAND = 2
      ICOOP = 2
      K75R = 0
      RETURN
78    ID(IPARTY) = 0
      ISTRNG = ISTRNG + 1
      IF (ISTRNG .EQ. 8) INDEF = 3
80    K75R = 0
      ITRY = ITRY - 1
      ICOOP = ICOOP - 1
      GOTO 95
90    ID(IPARTY) = ID(IPARTY) + 1
92    K75R = 1
95    IND = 2 * MY + J + 1
      MY = K75R
      RETURN
      END
