      FUNCTION K90R(J,M,K,L,R, JA)
C BY JOHN MAYNARD SMITH
C TYPED BY AX 3/27/79 (SAME AS ROUND ONE TIT FOR TWO TATS)
      k90r=ja    ! Added 7/27/93 to report own old value
C recoded by Ax 7/27/93
      if(m.eq.1) jold=0
      k90r=0
      if((jold.eq.1).and.(j.eq.1)) k90r=1
      jold=j
      RETURN
      END
