      Function KRowFunction(J,M,K,L,R,iRow,JA)        ! Look up row rule, return rowchoice
c add JA to row fcns to report their own previous move, 7/23/93
      if (irow>32 ) goto 133
      if (irow>16 ) goto 117
      if (irow>8 ) goto 109
      if (irow>4 ) goto 105
      if(irow==1) KRowFunction = K92R(J,M,K,L,R,JA)
      if(irow==2) KRowFunction = K61R(J,M,K,L,R,JA)
      if(irow==3) KRowFunction = K42R(J,M,K,L,R,JA)
      if(irow==4) KRowFunction = K49R(J,M,K,L,R,JA)
      return
105   if(irow==5) KRowFunction = K44R(J,M,K,L,R,JA)
      if(irow==6) KRowFunction = K60R(J,M,K,L,R,JA)
      if(irow==7) KRowFunction = K41R(J,M,K,L,R,JA)
      if(irow==8) KRowFunction = K75R(J,M,K,L,R,JA)
      return
109   if(irow>12) goto 113
      if(irow==9) KRowFunction = K84R(J,M,K,L,R,JA)
      if(irow==10) KRowFunction = K32R(J,M,K,L,R,JA)
      if(irow==11) KRowFunction = K35R(J,M,K,L,R,JA)
      if(irow==12) KRowFunction = K68R(J,M,K,L,R,JA)
      return
113   if(irow==13) KRowFunction = K72R(J,M,K,L,R,JA)
      if(irow==14) KRowFunction = K46R(J,M,K,L,R,JA)
      if(irow==15) KRowFunction = K83R(J,M,K,L,R,JA)
      if(irow==16) KRowFunction = K47R(J,M,K,L,R,JA)
      return
117   if (irow>24 ) goto 125
      if (irow>20 ) goto 121
      if(irow==17) KRowFunction = K64R(J,M,K,L,R,JA)
      if(irow==18) KRowFunction = K51R(J,M,K,L,R,JA)
      if(irow==19) KRowFunction = K78R(J,M,K,L,R,JA)
      if(irow==20) KRowFunction = K66R(J,M,K,L,R,JA)
      return
121   if(irow==21) KRowFunction = K58R(J,M,K,L,R,JA)
      if(irow==22) KRowFunction = K88R(J,M,K,L,R,JA)
      if(irow==23) KRowFunction = K31R(J,M,K,L,R,JA)
      if(irow==24) KRowFunction = K90R(J,M,K,L,R,JA)
      return
125   if (irow>28 ) goto 129
      if(irow==25) KRowFunction = K39R(J,M,K,L,R,JA)
      if(irow==26) KRowFunction = K79R(J,M,K,L,R,JA)
      if(irow==27) KRowFunction = K67R(J,M,K,L,R,JA)
      if(irow==28) KRowFunction = K86R(J,M,K,L,R,JA)
      return
129   if(irow==29) KRowFunction = K69R(J,M,K,L,R,JA)
      if(irow==30) KRowFunction = K91R(J,M,K,L,R,JA)
      if(irow==31) KRowFunction = K57R(J,M,K,L,R,JA)
      if(irow==32) KRowFunction = K70R(J,M,K,L,R,JA)
      return
133   if (irow>48 ) goto 149
      if (irow>40 ) goto 141
      if (irow>36 ) goto 137
      if(irow==33) KRowFunction = K85R(J,M,K,L,R,JA)
      if(irow==34) KRowFunction = K38R(J,M,K,L,R,JA)
      if(irow==35) KRowFunction = K40R(J,M,K,L,R,JA)
      if(irow==36) KRowFunction = K80R(J,M,K,L,R,JA)
      return
137   if(irow==37) KRowFunction = K37R(J,M,K,L,R,JA)
      if(irow==38) KRowFunction = K56R(J,M,K,L,R,JA)
      if(irow==39) KRowFunction = K43R(J,M,K,L,R,JA)
      if(irow==40) KRowFunction = K59R(J,M,K,L,R,JA)
      return
141   if(irow>44) goto 145
      if(irow==41) KRowFunction = K73R(J,M,K,L,R,JA)
      if(irow==42) KRowFunction = K55R(J,M,K,L,R,JA)
      if(irow==43) KRowFunction = K81R(J,M,K,L,R,JA)
      if(irow==44) KRowFunction = K87R(J,M,K,L,R,JA)
      return
145   if(irow==45) KRowFunction = K53R(J,M,K,L,R,JA)
      if(irow==46) KRowFunction = K76R(J,M,K,L,R,JA)
      if(irow==47) KRowFunction = K65R(J,M,K,L,R,JA)
      if(irow==48) KRowFunction = K52R(J,M,K,L,R,JA)
      return
149   if (irow>56 ) goto 157
      if (irow>52 ) goto 153
      if(irow==49) KRowFunction = K82R(J,M,K,L,R,JA)
      if(irow==50) KRowFunction = K45R(J,M,K,L,R,JA)
      if(irow==51) KRowFunction = K62R(J,M,K,L,R,JA)
      if(irow==52) KRowFunction = K34R(J,M,K,L,R,JA)
      return
153   if(irow==53) KRowFunction = K48R(J,M,K,L,R,JA)
      if(irow==54) KRowFunction = K50R(J,M,K,L,R,JA)
      if(irow==55) KRowFunction = K77R(J,M,K,L,R,JA)
      if(irow==56) KRowFunction = K89R(J,M,K,L,R,JA)
      return
157   if (irow>60) goto 161
      if(irow==57) KRowFunction = K63R(J,M,K,L,R,JA)
      if(irow==58) KRowFunction = K54R(J,M,K,L,R,JA)
      if(irow==59) KRowFunction = K33R(J,M,K,L,R,JA)
      if(irow==60) KRowFunction = K71R(J,M,K,L,R,JA)
      return
161   if(irow==61) KRowFunction = K74R(J,M,K,L,R,JA)
      if(irow==62) KRowFunction = K93R(J,M,K,L,R,JA)
      if(irow==63) KRowFunction = K36R(J,M,K,L,R,JA)
      return
      END
