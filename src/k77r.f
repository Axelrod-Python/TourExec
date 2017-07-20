      FUNCTION K77R(JPICK,MOVEN,ISCORE,JSCORE,RANDOM,JA)
      DIMENSION KEXP(5)
C BY SCOTT FELD
C TYPED BY JM 3/22/79
      K77R=JA       ! Added 7/32/93 to report own old value
      IF (MOVEN .GT. 1) GOTO 6
      JSTR = 3
      KTRY = 0
      KEXP(1) = 100
      KEXP(2) = 100
      KEXP(3) = 100
      KEXP(4) = 100
      KEXP(5) = 100
      KI = 0
6     IF (KTRY .LT. 20) GOTO 9
      KEXP(JSTR) = ISCORE - KI
      IF (JSTR .EQ. 5) GOTO 7
      IF (KEXP(JSTR + 1) .LE. KEXP(JSTR)) GOTO 7
      JSTR = JSTR + 1
      GOTO 8
7     IF (JSTR .EQ. 1) GOTO 8
      IF (KEXP(JSTR - 1) .LE. KEXP(JSTR)) GOTO 8
      JSTR = JSTR - 1
      JPICK = 0
8     KI = ISCORE
      KTRY = 0
9     KTRY = KTRY + 1
      GOTO (10,20,30,40,50), JSTR
10    K77R = 0
      RETURN
20    K77R = 0
      IF (JPICK .EQ. 0) RETURN
      IF (RANDOM .LE. .75) K77R = 1
      RETURN
30    K77R = JPICK
      RETURN
40    K77R = 1
      IF (JPICK .EQ. 1) RETURN
      IF (RANDOM .LE. .75) K77R = 0
      RETURN
50    K77R = 1
      RETURN
      END
