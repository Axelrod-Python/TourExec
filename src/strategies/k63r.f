      FUNCTION K63R(J,M,K,L,R,JA)
C BY GEORGE DUISMAN
C EDITED FROM BASIC BY AX, 3/7/79
C TYPED BY JM 3/15/79
      K63R=JA       ! Added 7/32/93 to report own old value
      IF (M .EQ. 1) ik = 1
      ik = 1 - ik
      K63R = IK
cc test 2 lines Ax 7/93. Also rewritten by Ax 7/21/93 putting ik where K63r was
cc  write (6,993) k63r
cc993   format (' test from k63r, k63r= ', i3)
C COOP ON ODD MOVES ONLY
      RETURN
      END
