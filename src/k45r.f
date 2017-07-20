      FUNCTION K45R(J,M,K,L,R,JA)
C BY MICHAEL F. MCGURRIN
C TYPED FROM FORTRAN BY AX, 1/26/79
      K45R=JA       ! Added 7/32/93 to report own old value
      IF(M.GT.3) GOTO 40
      IF(M.NE.1) GOTO 10
      JOLD=0
      A=0
      B=0
      C=0
      E=0
      K45R=1
      RETURN
10    IF(M.NE.2) GOTO 20
      IF(J.EQ.1) GOTO 30
      K45R=0
      D=0
      RETURN
30    K45R=0
      D=1
      RETURN
20    IF(J.EQ.1) GOTO 50
      IF(D.EQ.1) GOTO 60
      K45R=0
      A=1
      RETURN
60    K45R=0
      RETURN
50    K45R=0
      IF(D.EQ.1) C=1
      RETURN
40    IF(C.EQ.1) GOTO 70
      IF(B.EQ.1) GOTO 80
      IF(A.EQ.1) GOTO 90
      IF(D.EQ.1) GOTO 120
      IF(J.EQ.1) GOTO 100
      K45R=0
      B=1
      RETURN
100   K45R=0
      C=1
      RETURN
120   IF(J.EQ.1) GOTO 130
      K45R=0
      B=1
      RETURN
130   K45R=1
      C=1
      RETURN
70    K45R=J
      RETURN
80    K45R=0
      IF((JOLD.EQ.1).AND.(J.EQ.1)) K45R=1
      JOLD=J
      RETURN
90    K45R=1
      E=E+1
      IF(E.NE.8) GOTO 110
      E=0
      JOLD=J
      RETURN
110   IF(.NOT.((JOLD.EQ.1).AND.(J.EQ.1))) K45R=0
      JOLD=J
      RETURN
      END
