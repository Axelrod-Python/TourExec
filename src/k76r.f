      FUNCTION K76R(J,M,K,L,R,JA)
C BY DAVID GLADSTEIN
C FROM CARDS BY JM 3/16/79
      LOGICAL PATSY
      K76R=JA       ! Added 7/32/93 to report own old value
      IF (M .NE. 1) GO TO 1
      PATSY = .TRUE.
      DC = 0
      MDC = 0
      G = 1
      K76R = 1
      RETURN
1     IF (PATSY) GO TO 2
      K76R = J
      RETURN
2     IF (J .NE. 1) GO TO 3
      PATSY = .FALSE.
      K76R = 0
      RETURN
3     DC = DC + 1
      IF (G .EQ. 0) MDC = MDC + 1
      G = 0
      IF (MDC / (DC + 1) .GE. .5) G = 1
      K76R = G
      RETURN
      END
