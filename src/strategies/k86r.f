      FUNCTION K86R(JPICK,MOVEN,ISCORE,JSCORE,RANDOM, JA)
C BY BERNARD GROFMAN
C FROM CARDS BY JM 3/27/79
        DIMENSION IOPPNT(999)
      k86r=ja    ! Added 7/27/93 to report own old value
        IOPPNT(MOVEN) = JPICK
        MYOLD = K86R
        IF (MOVEN .GT. 2) GOTO 10
        K86R = 0
        RETURN
10      IF (MOVEN. GT. 7) GOTO 20
        K86R = JPICK
        RETURN
20      IPREV7 = 0
      J = MOVEN - 7
      K = MOVEN - 1
        DO 25 I = J,K
25              IPREV7 = IPREV7 + IOPPNT(I)
        IF (MYOLD .EQ. 0 .AND. IPREV7 .LE. 2) K86R = 0
        IF (MYOLD .EQ. 0 .AND. IPREV7 .GT. 2) K86R = 1
        IF (MYOLD .EQ. 1 .AND. IPREV7 .LE. 1) K86R = 0
        IF (MYOLD .EQ. 1 .AND. IPREV7 .GT. 1) K86R = 1
        RETURN
      END
