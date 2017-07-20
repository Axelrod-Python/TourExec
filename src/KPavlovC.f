      Function KPavlovC(J,M,K,L,R,JB)     ! Pavlov, JB is own (Col) previous move
c   coded by Ax 7/22-3/93. Assumes C on first move.
      KPavlovC = 1
      if (J .eq. JB) KPavlovC = 0 ! coop iff other's previous choice= own previous ch
C test3
c   write(6,81) J, JB
c81   format(2i3, 'j,jb from test3')
      Return
      end
