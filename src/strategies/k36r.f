      FUNCTION K36R(J,M,K,L,R,JA)
C BY ROGER HOTZ
C TYPED BY JM
C EDITED FROM BASIC BY AX, 2/11/79
      K36R=JA       ! Added 7/32/93 to report own old value
      K36R = 1
      IF (M .GE. 1 .AND. M .LT. 100) PR0BC = .1
      IF (M .GE. 100 .AND. M .LT. 200) PR0BC = .05
      IF (M .GE. 200 .AND. M .LT. 300) PR0BC = .15
      IF (M .GE. 300) PR0BC = .0
      IF (R .LT. PR0BC) K36R = 0
      RETURN
      END
