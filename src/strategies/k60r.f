      FUNCTION K60R(J,M,K,L,R, JA)
C BY JIM GRAASKAMP AND KEN KATZEN
C FROM CARDS BY JM 2/22/79
      k60r=ja    ! Added 7/27/93 to report own old value
      IF (M-1)1,1,2
1     ID=0
      K60R=0
      GO TO 50
2     IF (ID-1)3,4,4
3     K60R=J
      IF (M-11)50,5,6
5     IF (K-23)51,50,50
6     IF (M-21)50,7,8
7     IF(K-53)51,50,50
8     IF (M-31)50,9,10
9     IF (K-83)51,50,50
10    IF (M-41)50,11,12
11    IF (K-113)51,50,50
12    IF (M-51)50,13,14
13    IF (K-143)51,50,50
14    IF (M-101)50,15,50
15    IF (K-293)51,50,50
51    ID=1
4     K60R=1
50    RETURN
      END
