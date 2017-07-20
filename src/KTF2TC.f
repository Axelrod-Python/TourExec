      Function KTF2TC(J,M,K,L,R)          !  Tit for Two Tats, Col rule
      if(m .eq. 1)  jold = 0
      ktf2tc = 0
      if ((jold .EQ. 1) .and. (j .eq. 1)) ktf2tc = 1
      jold = j
      Return
      End     ! TF2T Col Rule
