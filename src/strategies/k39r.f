      FUNCTION K39R(J,M,K,L,R,JA)
C      BY TOM ALMY (FROM HIS PAPER TAPE)
C       EDITED BY AX, 1.16.79
      IMPLICIT INTEGER(A-Z)
      REAL R
      DIMENSION OK(3)
      K39R=JA       ! Added 7/32/93 to report own old value
cc ax test
c   write(6,77) m, step, substp
c77   format(' test k39r. m, step, substp', 3i3)
      IF(M.NE.1) GOTO 10
      STEP=1
      SUBSTP=1
      BOTHD=0
      TITCNT=0
      TATCNT=0
      EVIL=0
      N=1
      F=0
      DO 1 I=1,3
      OK(I)=0
1      CONTINUE
      TOTK=0
      OLDMOV=0
10      CONTINUE
C      DO TABULATION
      IF(K39R+J.EQ.2) BOTHD=BOTHD+1
      IF(K39R+J.LT.2) BOTHD=0
      COUNT=COUNT-1
      K39R=0
      VOLDMV=OLDMOV
      OLDMOV=J
      IF(J.EQ.1) TATCNT=TATCNT+1
      IF(EVIL.EQ.0 .AND. J.EQ.1) EVIL=1
20      CONTINUE
      GOTO (100,200,300,400,500), STEP
C      PLAY TIT FOR TWO TATS
100      CONTINUE
      GOTO(101,110,120), SUBSTP
C INITIALIZE ALL DEFENSIVE MODES
C      OK AND TOTK NOT RESET IN ORDER TO BIAS TOWARDS KEEPING
C      THIS PLAY MODE IF WE HAVE JUST FINISHED EXPLOITING.
101      CONTINUE
      COUNT=10
      TATCNT=0
      TITCNT=0
      SUBSTP=2
      GOTO 20
C PLAY TIT FOR TWO TATS
110      CONTINUE
      IF((VOLDMV+OLDMOV).EQ.2) K39R=1
      TITCNT=TITCNT+K39R
      IF(COUNT.EQ.0) SUBSTP=3
      RETURN
C EVALUATE PLAY
120      CONTINUE

cc ax test
c   if (m.eq. 51) write(6,7120) m, step, substp
c7120   format(' test 7120 after 120. m, step, substp', 3i3)
      OLDSTP=STEP
      OK(STEP)=K-TOTK
      TOTK=K
      SUBSTP=1
      IF(TATCNT.GT.0) GOTO 130
C      NICE OPPONENT--TRY TO TAKE ADVANTAGE!
      STEP=4
C      IF OPPONENT NOT REALLY NICE--DON'T TRY TO TAKE DVANTAGE
      IF (EVIL.EQ.1) STEP=1
      IF (EVIL.EQ.0) EVIL=-1
      GOTO 20
130      CONTINUE
C      LET US FIND BEST DEFENSE (HIGHEST SCORE)
      STEP=1
      DO 150 I1=1,2
      DO 150 I2=2,3
cc ax test
c   if (m.eq. 51) write(6,71302) m, step, substp, i1, i2, ok(i1), ok(i2)
c71302    format(' test 71302 After 130. m, step, substp, i1, i2, ok(i1), ok(I2)', 7i3)
      IF(OK(I1).EQ.0.OR. OK(I2).EQ.0) GOTO 150
      IF(OK(I1).GE.OK(I2)) GOTO 150
      IF(STEP.EQ.I1) STEP=I2
150      CONTINUE
C      ADVANCE TO NEW STEP IF NEXT ONE NOT TESTED AND EITHER PPONENT
C      IS VERY NASTY OR IS EXPLOITING US
c next 2 lines are test4 added by Ax 7/23
c   if (step .gt. 2) write(6, 737) j, m, k, l, step, substp
c737    format(' test737 from K39r. j,m,k,l,step, substp: ', 6i4)
c Next statement broken up to prevent complier error. Two clauses separated.Ax 7/26/93
c   IF (STEP.NE.3 .AND. OK(STEP+1).EQ.0 .AND.
c    1(TATCNT.GE.4 .OR. TITCNT.EQ.0))
c    1 STEP=STEP+1
      if (step.eq.3) goto 777       ! if step=3 skip next test
      IF ( (OK(STEP+1).EQ.0) .AND.
     1(TATCNT.GE.4 .OR. TITCNT.EQ.0))
     1 STEP=STEP+1
777   continue
C      IF WE PUNISHED TOO SEVERLY, THEN GO ALL C TO ECOOPERATE
cc ax test
c   if (m.eq. 51) write(6,747) m, step, substp
c747    format(' test 747 k39r After 737. m, step, substp', 3i3)
      IF(STEP.LT.OLDSTP .AND. BOTHD .GT.0) STEP=5
      GOTO 20
C      PLAY TIT FOR TAT
200      CONTINUE
      GOTO (101,210,120), SUBSTP
210      CONTINUE
      IF(OLDMOV.EQ.1) K39R=1
      TITCNT=TITCNT+K39R
      IF (COUNT.EQ.0) SUBSTP=3
      RETURN
C      PLAY ALL DEFECTS
300      CONTINUE
      GOTO (101,310,120), SUBSTP
310      CONTINUE
cc ax test
c   if (m.eq. 51) write(6,7727) m, step, substp
c7727   format(' test 7727. m, step, substp', 3i3)
      K39R=1
      TITCNT=TITCNT+1
      IF (COUNT.EQ.0) SUBSTP=3
      RETURN
C      EXPLOIT
400      CONTINUE
      GOTO(401,402,403,404), SUBSTP
C      DO A DISRUPT
401      CONTINUE
      SUBSTP=2
      K39R=1
      COUNT=N
      TATCNT=0
      RETURN
C      COOPERATE FOR A WHILE
402      CONTINUE
      IF(COUNT.EQ.0) SUBSTP=3
      RETURN
C      DECIDE WHAT TO DO
403      CONTINUE
      IF(TATCNT.NE.0) GOTO 410
C      WE HAVEN'T BEEN PUNISHED--TRY IT AGAIN
      F=1
      GOTO 401
C      WE HAVE BEEN PUNISHED--DECIDE ACTION
410      CONTINUE
      IF(F.EQ.0) GOTO 420
C      WE HAD BEEN RUNNING  -TRY LATER WITH LARGER GAP
      N=N+1
      SUBSTP=1
      STEP=1
      GOTO 20
C      TOUCHY PROGRAM--COOPERATE UNTIL DEFECTION THEN RESUME FOR 2T
420      CONTINUE
      SUBSTP=4
      IF(J.EQ.1) N=N+1
      TATCNT=J
      RETURN
C      COOP UNTIL DEFECTION
404      CONTINUE
C      ALLOW A GROTESQUE PUNISHMENT (5 TATS WITHOUT US EFECTING)
      IF(TATCNT.LE.4) RETURN
      SUBSTP=1
      STEP=1
      GOTO 20
C      DO ALL C FOR 5 MOVES TO COOL THINGS OFF
500      CONTINUE
      IF(SUBSTP.EQ.2) GOTO 520
      COUNT=5
      SUBSTP=2
520      CONTINUE

cc ax test
c   if (m.eq. 51) write(6,7520) m, step, substp
c7520   format(' test 7520 after 520. m, step, substp', 3i3)
      IF(COUNT.NE.0) RETURN
      SUBSTP=1
      GOTO 130
      END
