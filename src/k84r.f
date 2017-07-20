      FUNCTION K84R(JP,M,IS,JS,R, JA)
C BY T NICOLAUS TIDEMAN AND PAULA CHIERUZZI
C TYPED BY JM 3/31/79
      k84r=ja    ! Added 7/27/93 to report own old value
      K84R = 1
      IF (M .GT. 1) GOTO 2
      ISIG = 0
      DS = 0
      JQ = 0
      FJD = 0
      JDR = 0
      FM = 0
      GOTO 3
2     IF (JP .EQ. 1) FJD = FJD + 1
      IF (ISIG .EQ. 1) GOTO 5
      FM = M
      IF (JQ .EQ. 0 .AND. JP .EQ. 1) JDR = JDR + 1
3     IF (IS - JS - DS - 5 * JDR * (JDR - 1) / 2 .GE. 0)
     1  K84R = 0
      IF (K84R .EQ. 1) GOTO 4
1     JQ = JP
      RETURN
4     IF ((JQ - JP) .LT. 0 .OR. (M - ISIG) .LT. 10) GOTO 1
      IF (ABS(FJD - (FM - 1.) / 2.) .LT. (1.5 * SQRT(FM - 1.)
     1 )) GOTO 1
      ISIG = 1
      JQ = JP
      GOTO 6
5     ISIG = M
      JQ = 0
      JDR = 0
      DS = IS - JS
6     K84R = 0
      RETURN
      END
