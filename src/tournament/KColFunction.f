      Function KColFunction(J,M,K,L,R,IColType,JB)        ! Look up col rule, return col choice
      if (icoltype. eq. 1) KColFunction= KTitForTatC(J,M,K,L,R)
      if (icoltype .eq. 2) KColFunction= KTF2TC(J,M,K,L,R)
      if (icoltype .eq. 3) KColFunction= KRandomC(J,M,K,L,R)
      if (icoltype .eq. 4) KColFunction= KPavlovC(J,M,K,L,R, JB)   ! JB is own, col's prev move
      return
      end
