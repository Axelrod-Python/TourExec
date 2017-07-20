      Program AxTest
c   For  testings of PD tour program.
c   Begun 7/19/93. Ver 1.0 begun 7/27 for nice rules as well as not nice rules
c Compile: set directory. then: RUN tourexec2 -debug -saveall -ov -r

c Changes to make:
c   Add Almost-Pavlov and Almost-TFT to col rules

      real Version /1.1/
c Next few lines  are control parameters
      integer ColType/4/                                  ! 1=TFT, 2=TF2T, 3=Random, 4= Pavlov
      integer MoveReport/0/                               ! 0= no report of moves, 1 = report moves
      integer GameReport/0/                           ! 0 = no report of games 1= report games
      real Noise/0./                                      ! prob a choice will be changed
      integer minRow/1/                                   ! normally /1/ to run all rules
      integer maxRow/63/                                  ! normally /63/ to run all rules
      integer outcome(308)                                ! 1=R, 2=T, 3=S, 4=P for Column
      integer length(5) /63,77,151,156,308/   ! Game Lengths in Tour
      integer game                                            ! Game no. with this pair, 1 to 5
      integer*4 RandomSeed                                    !
      integer Row, Rank                                   ! Row = Rank = 1..63 for 2nd round rules
      integer RowGameScore, ColGameScore  ! Score in Current Game
      integer Tally(4)                                        ! tally of col's outcomes for game
      integer ColOutcomeType                                  ! 1=R, 2=T, 3=S, 4=P for Column
      integer RowGameSc, ColGameSc        ! Scores in one game
      integer RowPairSc, ColPairSc            ! Scores over 5 games
      integer MoveRecord(308)             ! Moves of current game
      character*9 day
      character*8 timenow
      integer ActualTFTTourSc(63)/
     1 453,453,453,453,453,  453,453,452,453,453,
     1 453,453,453,453,453,   449,453,452,450,453,
     2 453,453,453,453,452,   453,446,453,449,453,
     3 453,453,453,453,453,   453,453,453,452,453,
     4 453,453,453,453,453,   452,453,443,422,452,
     5 442,453,452,442,342,    398,377,388,438,155,
     6 376,341,198/
      integer IRowPairSc(63), IColPairSc(63)              ! Integer total over 5 games
      real    AveRowPairSc(63), AveColPairSc(63)          ! real, truncated
      integer rowchoice, colchoice
      call date_and_time(day, timenow)
      write(6,100) Version, day, timenow
100   format('  Ax TourExec Program Output, Version ',f6.2, '.', 1H,
     1 A10, A10)
      RandomSeed = secnds(0.0)                     ! uses elapsed time since midnight as random seed
c   RandomSeed=66222                                ! Uses fixed random number
      Write(6,103) RandomSeed
103   format(' RandomSeed = ', i16)

      write(6,85) noise
85    format(' Noise (per choice) = ', f8.4)

      write(6, 104) ColType
104   format(' Col Type, 1=TFT, 2=TF2F, 3=Random, 4=Pavlov. Col Type = '
     1 , i3)
      if (movereport==1) write(6, 105)
105   format(' Move report: 1 means R, 2 means T, 3  means S, 4 means P
     1 for column.')
      if (GameReport==1) write(6,101)
101   format(' Rank  Game RScore CScore #ColR #ColT #ColS #ColP')
      ITotalColPoints = 0                                 ! Initialize Col's total points
      Do 30 row= minRow,maxRow                    ! normally 1 to 63
      rank = row
      RowPairSc = 0
      ColPairSc = 0

        Do 20 Game = 1,5
            RowGameSc = 0
            ColGameSc = 0
            JA = 0          ! Row's previous move, reported to column
            JB = 0          ! Col's previous move, reported to row
            Do 10 ColOutcomeType = 1,4
            Tally(ColOutcomeType) = 0                   ! Zero Col's RTSP game count
10    Continue            ! End Do tallyType
            Do 15 Move = 1, Length(Game)
                RandomNumber = RAN(RandomSeed)
                RowChoice = KRowFunction(JB,Move, RowGameSc,ColGameSc,
     1            RandomNumber,Row,JA)
                if ( RAN(RandomSeed) < noise ) RowChoice = 1-RowChoice  !  noise happened to Row
                RandomNumber = RAN(RandomSeed)
                ColChoice = KColFunction(JA,Move,ColGameSc,RowGameSc,
     1             RandomNumber,ColType,JB)
                if ( RAN(RandomSeed) < noise ) ColChoice = 1 - ColChoice ! noise happened to Col
C temp test:
c               Write(6, 999) Move, RowChoice, ColChoice
c999    Format(' move, rowchoice, colchoice ', 3i6)
                ColOutcomeType = 1 + 2*RowChoice + ColChoice    ! *check col: 1=R,2=T
                Tally(ColOutcomeType) = Tally(ColOutcomeType) + 1
                JA = RowChoice          ! Reported to col next time
                JB = ColChoice              ! Reported to row next time

                Select Case (ColOutcomeType)
                    Case (1)                    ! Both Get R
                        RowGameSc=RowGameSc+3
                        ColGameSc=ColGameSc+3
                    Case (2)                    ! Col Gets T
                        ColGameSc=ColGameSc+5
                    Case (3)                    ! Col Gets S
                        RowGameSc=RowGameSc+5
                    Case (4)                    ! Both Get P
                        RowGameSc=RowGameSc+1
                        ColGameSc=ColGameSc+1
                End Select

                MoveRecord(move)=ColOutcomeType
15      Continue        ! End Do Move

C  write game output
            RowPairSc=RowPairSc+RowGameSc        ! sum over 5 games
            ColPairSc=ColPairSc+ColGameSc
            if (GameReport==1) Write(6, 110)  Rank, Game, RowGameSc,
     1 ColGameSc, Tally(1), Tally(2), Tally(3), Tally(4)
110   format(9i6, 10i3)
      if (movereport .eq. 1) write(6, 112) (MoveRecord(ir), ir=1,
     1   length(game))
112   format('   ', 10i2, 2H, 10i2, 2H, 10i2, 2H, 10i2)
20    Continue        ! End Do Game
      if (GameReport==1) write(6, 115) RowPairSc, ColPairSc
      IRowPairSc(Row) = RowPairSc             !  total over 5 games
      IColPairSc(Row) = ColPairSc
      IColTourSc = IColTourSc +ColPairSc  ! running total of col's points
115   format('Totals over 5 games: RowPairSc= ',I7, ' ColPairSc = ', I7)
      if (GameReport==1) write (6, 120)
120   format()

30    Continue        ! End Do Row

C final report: calc tour score, write tour output

      Write(6, 135)
135   format(' Rank    RowSc   ColSc   AveRowSc AveColSc 2ndRndTFT
     1 2ndRndTFT-Col')
      Do 40 Row = minRow,maxRow
      IRowTourPairSc = IRowPairSc(Row)/5
      IColTourPairSc = IColPairSc(Row)/5
      ITotalColPoints =  ITotalColPoints + IColPairSc(Row)    ! accumulate col points
      Write(6, 140) Row, IRowPairSc(Row), IColPairSc(Row),
     1 IRowTourPairSc,IColTourPairSc, ActualTFTTourSc(Row),
     2 ActualTFTTourSc(Row)-IColTourPairSc
140   format(i6, 4i8, '    ',i8,'    ',i8)
40    continue        ! end final report
      TotalColPoints = ITotalColPoints                ! to make floating point (total over 63*5 games)
      ColTourSc =(TotalColPoints/5 )/63   !   Ave per game over 63 pairs
      write(6, 150) ColType,  ITotalColPoints, ColTourSc
150   format(' Col Type= ', i4, '. Col Pts = ', i7, '   Cols Tour Sc = '
     1 , f7.3)
      end   ! Main Program
