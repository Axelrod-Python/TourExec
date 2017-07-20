      Integer FUNCTION GRASR(JPICK, MOVEN, ISCOR, JSCOR, RANDO,JA)
      DIMENSION NMOV(4)
      GRASR=JA        ! Added 7/32/93 to report own old value
c Next line for debugging
c   if(moven. eq. 57)  write(6,99) jscor
c99   format(' TEST from GRASR at move 57. jscor = ', i6)
      IF (MOVEN .NE. 1) GO TO 9997
      DO 9996 I = 1, 4
      NMOV(I) = 0
9996    CONTINUE
      NMOVE = 0
      IGAME = 0
      N = 0
9997    CONTINUE
      IF (MOVEN - 1) 25, 25, 26
25    GRASR = 0
      RETURN
26    IF (MOVEN - 51) 1, 2, 3
1     GRASR = JPICK
      RETURN
2     GRASR = 1
      RETURN
3     IF (MOVEN - 57) 4, 5, 6
4     IF (MOVEN - 52) 9, 9, 10
10    NMOV(MOVEN - 52) = MMOVE + JPICK
9     GRASR = JPICK
      IF (GRASR -1) 7, 8, 8
7     MMOVE = 2
      GO TO 11
8     MMOVE = 4
11    RETURN
5     IF (JSCOR - 135) 19, 19, 20
20    J = NMOV(2)
      GO TO (12, 12, 30, 31, 32), J
31    IF (NMOV(1) - 3) 12, 35, 12
35    IF (NMOV(3) - 3) 12, 16, 12
32    IF (NMOV(1) - 5) 12, 33, 12
33    IF (NMOV(3) - 5) 12, 16, 12
30    IF (NMOV(1) - 2) 12, 34, 12
34    IF (NMOV(3) - 4) 12, 40, 12
40    IF (NMOV(4) - 2) 12, 41, 12
12    IGAME = 1
      N = RANDO * 10.0 + 5.0
      GRASR = 0
      RETURN
16    IGAME = 2
      GRASR = 0
      RETURN
19    IGAME = 3
27    GRASR = 1
      RETURN
41    IGAME = 4
42    GRASR = 0
      IF (MOVEN - 118) 44, 43, 43
43    IGAME=2
44    RETURN
6     GO TO (21, 22, 27, 42), IGAME
21    IF (N) 23, 23, 24
23    GRASR = 1
      N = RANDO * 10.0 + 5.0
      RETURN
24    GRASR = JPICK
      N = N-1
      RETURN
22    GRASR = JPICK
      RETURN
      END
