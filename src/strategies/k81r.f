      FUNCTION K81R(J,M,K,L,R, JA)
C BY MARTYN JONES
C EDITED FROM BASIC BY AX 3/25/79
C TYPED BY JM 3/27/79, COR BY AX 3/28/79
      INTEGER C,T4,T5,D4
      REAL L4(8,2)
      DIMENSION X(8)
      k81r=ja    ! Added 7/27/93 to report own old value
      IF (M .EQ. 81 .AND. K .EQ. L .AND. K .EQ. 237) T0 = 1
      IF (M .NE. 1) GOTO 555
      DO 535 C = 1,8
      L4(C,1) = 0
535   L4(C,2) = 0
      T0 = 0
      T4 = 0
      T5 = 0
      T6 = 25
      T8 = 0
      T9 = 5
      D4 = 0
      A = 0
      B = 0
      S1 = 0
      DO 9997 C = 1,8
9997  X(C)=0
555   IF (M .EQ. 2 .AND. J .EQ. 1) T9 = 9
      IF (M .LT. T9) GOTO 800
      IF (T5 .GT. 7) T5 = T5 - 8
      IF (J .EQ. 0) L4(T5+1,1) = L4(T5+1,1) + 1
      IF ((T9 .EQ. 9) .AND. (T0 .EQ. 1)) GOTO 1270
      GOTO 1020
564   IF (L .GT. K + T6) GOTO 800
      D4 = T4
       IF (D4 .GT. 7) D4 = D4 - 8
c put gosub 1200 here to avoid compiler error 7/29/93
      A1 = L4(D4+1,1)
      A2 = L4(D4+1,2)
      IF (A2 .EQ. 0) A2 = 1
      A3 = A1 / A2
      A = 3 * A3
      B = A + A3 + 1
610   DO 630 C = 1,4
      X(C) = A
630   X(C + 4) = B
      E0 = 5
      E1 = 6
      E2 = 7
      E3 = 8
      F0 = 3
      F1 = 4
      F2 = 7
      F3 = 8
      L900 = 1
      GOTO 900
670   E0 = 3
      E1 = 4
      F0 = 2
      F2 = 6
      L900 = 2
      GOTO 900
710   GOTO 1100
720   K81R = 1
      IF (S1 .LT. 5) K81R = 0
      GOTO 810
800   K81R = J
810   T5 = T4
      IF ((M/10) * 10 .EQ. M) GOTO 860
815   IF (T4 .GT. 7) T4 = T4 - 8
      IF (M .GT. 3) L4(T4+1,2) = L4(T4+1,2) + 1
      IF (T4 .GT. 4) T4 = T4 - 4
      T4 = T4 * 2 + K81R
      RETURN
860   DO 880 C = 1,8
      L4(C,1) = L4(C,1) * 9
880   CONTINUE
      T6 = T6 + 1
      GOTO 815
900   IF (T4 .GT. 4) T4 = T4 - 4
      T4 = T4 * 2
      DO 1000 C = 1,8
      D4 = T4
      IF (C .EQ. E0 .OR. C .EQ. E1 .OR. C .EQ. E2 .OR. C .EQ. E3)
     +D4 = T4 + 1
      IF (D4 .EQ. 9) D4 = 1
      IF (D4 .GT. 7) D4 = D4 - 8
c put gosub 1200 here Ax 7/29/93
      A1 = L4(D4+1,1)
      A2 = L4(D4+1,2)
      IF (A2 .EQ. 0) A2 = 1
      A3 = A1 / A2
      A = 3 * A3
      B = A + A3 + 1
960   IF (C .EQ. F0 .OR. C .EQ. F1 .OR. C .EQ. F2 .OR. C .EQ. F3)
     +GOTO 990
      X(C) = X(C) + A
      GOTO 1000
990   X(C) = X(C) + B
1000  CONTINUE
      GOTO (670,710), L900
1020  IF (J .NE. 1) GOTO 1025
      T8 = T8 + 1
      GOTO 1070
1025  IF (.NOT.(T8 .GE. 0. .AND. T8 .LT. 6.)) GOTO 1030
      T8 = 0
      GOTO 564
1030  IF (T8 .GT. 0) T8 = -200
      K81R = 0
      T8 = T8 + 1
      GOTO 810
1070  IF (T8 .LT. 8 .OR. T8 .GT. 9) GOTO 1080
      K81R = 0
      GOTO 810
1080  IF (T8 .GT. 1) T8 = 1
      GOTO 564
1100  S = 0
      DO 1150 C = 1,8
      IF (X(C) .LE. S) GOTO 1150
      S = X(C)
      S1 = C
1150  CONTINUE
      GOTO 720
c moved "GOSUB1200" in proper places to avoid compiler error.7/29/93
1270  IF (J .NE. 1) GOTO 1272
      T0 = 0
      GOTO 1020
1272  T2 = 0
1275  IF (.NOT.((M .GT. 80 + T2) .AND. (M .LT. 140 + T2))) GOTO 1280
      K81R = 1
      GOTO 810
1280  IF (.NOT.((M .GE. 140 + T2) .AND. (M .LE. 180 + T2))) GOTO 1285
      K81R = 0
      GOTO 810
1285  T2 = T2 + 100
      GOTO 1275
      END
