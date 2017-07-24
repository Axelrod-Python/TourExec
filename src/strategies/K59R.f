      FUNCTION K59R(LASTMV,MOVEN,K,L,R, JA)
C BY LESLIE DOWNING
C TYPED BY AX, 3/27/79 (SAME AS ROUND ONE REV.DOWNING)
c Redone as copy of K56=RevDowning by Ax, 7/27/93
c     INTEGER XDOWNC
c      T=0
c      K59R=XDOWNC(J,M,K,L,T,R)
c     RETURN
c     END
       INTEGER LASTMV,MOVEN
      INTEGER PAST,NICE1,NICE2
      REAL GOOD,BAD,C,ALT
      INTEGER DEFECT, COOP
      INTEGER TOTCOP,TOTDEF
      k59r=ja    ! Added 7/27/93 to report own old value
      IF (MOVEN - 2) 1,4,2
1     GOOD = 1.0
      BAD = 0.0
      PAST = 0
      TOTCOP = 0
      TOTDEF = 0
      NICE1 = 0
      NICE2 = 0
      COOP = 0
      DEFECT = 1
      GOTO 4
2     IF (PAST .EQ. DEFECT) GOTO 3
      IF (LASTMV .EQ. COOP) NICE1 = NICE1 + 1
      TOTCOP = TOTCOP + 1
      GOOD = FLOAT(NICE1) / FLOAT(TOTCOP)
      GOTO 4
3     IF (LASTMV .EQ. COOP) NICE2 = NICE2 + 1
      TOTDEF = TOTDEF + 1
      BAD = FLOAT(NICE2) / FLOAT(TOTDEF)
4     PAST = K59R
      C = 6.0 * GOOD - 8.0 * BAD - 2.0
      ALT = 4.0 * GOOD - 5.0 * BAD - 1.0
      IF (C .GE. 0.0 .AND. C .GE. ALT) GOTO 5
      IF (C .GE. 0.0 .AND. C .LT. ALT) GOTO 6
      IF (ALT .GE. 0.0) GOTO 6
      K59R = DEFECT
      GOTO 7
5     K59R = COOP
      GOTO 7
6     K59R = 1 - K59R
7     RETURN
      END
