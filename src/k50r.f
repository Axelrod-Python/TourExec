      FUNCTION K50R(J,MOVN,KM,KH,R,JA)
C BY RIK
C TYPED BY JM, CORRECTED BY AX, 2/27/79
      K50R=JA       ! Added 7/32/93 to report own old value
      K50R = 0
      IF ((J .EQ. 0) .AND. (R .GE. 0.9)) K50R = 1
      RETURN
      END
