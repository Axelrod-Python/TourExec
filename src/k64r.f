      FUNCTION K64R(J,M,K,L,R, JA)
C BY BRIAN YAMACHI
C EDITED FROM BASIC BY AX, 2/28/79
C TYPED BY JM 3/1/79
      IMPLICIT INTEGER (A-Z)
      REAL R
      DIMENSION A(2,2)
      k64r=ja    ! Added 7/27/93 to report own old value
      IF (M .GT. 1) GOTO 640
      E = 0
      F = 0
      DO 560 C = 1,2
         DO 560 D = 1,2
560   A(C,D) = 0
      X = 1
      Y = 1
      K64R = 0
      Y = K64R + 1
      RETURN
640   IF (A(X,Y) .GE. 0) K64R = 0
      IF (A(X,Y) .LT. 0) K64R = 1
      IF (J .EQ. 0) A(X,Y) = A(X,Y) + 1
      IF (J .EQ. 1) A(X,Y) = A(X,Y) - 1
      X = J + 1
      Y = K64R + 1
      IF (J .EQ. 0) E = E + 1
      IF (J .EQ. 1) F = F + 1
      P = E - F
      IF (P .LT. 0) P = -P
      IF ((M .GT. 40) .AND. (10 * P .LT. M)) K64R = 1
      RETURN
      END
