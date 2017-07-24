      FUNCTION K88R(J,M,K,L,R, JA)
C BY SCOTT APPOLD
C EDITED FROM NEAR-FORTRAN BY AX 3/27/79
C TYPED BY JM 3/31/79
      k88r=ja    ! Added 7/27/93 to report own old value
      K88R = 0
      IF (M .NE. 1) GOTO 10
      MMC = 0
      LMV = 0
      MP = 0
      MMV = 0
      MP2 = 0
      MMD = 1
      DFLG = 0
10    IF (M .LT. 2) GOTO 20
      IF (MMV .NE. 0) GOTO 15
      MMC = MMC + 1
      MP = MP + J
      PRC = FLOAT(MP) / FLOAT(MMC)
      GOTO 20
15    MMD = MMD + 1
      MP2 = MP2 + J
      PRD = FLOAT(MP2) / FLOAT(MMD)
20    CONTINUE
      IF (M .GT. 4) GOTO 25
      K88R = 0
      GOTO 30
25    IF (.NOT.(J .EQ. 1 .AND. DFLG .EQ. 0)) GOTO 28
      DFLG = 1
      K88R = 0
      GOTO 30
28    IF (MMV .EQ. 0 .AND. R .LT. PRC) K88R = 1
      IF (MMV .EQ. 1 .AND. R .LT. PRD) K88R = 1
30    CONTINUE
      MMV = LMV
      LMV = K88R
      RETURN
      END
