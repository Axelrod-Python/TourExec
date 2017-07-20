      FUNCTION K57R(J,M,K,L,R, JA)
C BY RUDY NYDEGGER
C TYPED BY AX, 3/27/79 (SAME AS ROUND ONE NYDEGR)
c Replaced by Nydegr retyped from rnd 1 by Ax, 7/27/93
c   T=0
c    K57R=NYDEGR(J,M,K,L,T,R)
c    RETURN
c   END
      k57r=ja    ! Added 7/27/93 to report own old value
      if(m.ne.1) goto 5
      k57r = 0
      n = 0
c update 3 move history
    5   N = 4 * (n-16*(N/16)) + 2 * k57r + J
      if(m.gt.3) goto 8
      k57r=j
      if(m.eq.3 .and. n.eq.6) k57r=1
      return
c coop if 0, 27, 28, 32, 40-4, 46-8, 56-7,59-60,62-63
   8    k57r=1
      if(n-39) 10,110,50
 10   if(n) 100,100,20
 20   if(n-28) 30,100,40
 30   if(n-27) 110,100,100
 40   if(n-32) 110,100,110
 50   if(n-45) 100,110,60
 60   if(n-49) 100,110,70
 70   if(n-58) 80,110,90
 80   if(n-55) 110,110,100
 90   if(n-61) 100,110,100
 100    k57r = 0
 110    return
      end
