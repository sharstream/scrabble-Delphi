; Version 1.1 multiplayer

; june 12, 2001 - changed some prints to text

; November 16, 2000
; Mark Incitti
; added screen to prompt for single or multiplayer, added switches at all net calls

; mostly working....minor issues with tiles lost when players drop out
; final scoring may be off because of those lost tiles

AppTitle "Scrabble"

Type tiletype
	Field x
	Field y
	Field tilenum
End Type

Type word
	Field wordstring$
	Field startx,starty,endx,endy
	Field points
	Field id
	Field playernum
End Type

Type Info
	Field txt$
End Type

Type player
	Field name$
	Field net_id
	Field playernum
End Type

Type coltype
	Field r,g,b
End Type

Const MSG_RECVBAG = 1		; incoming bag of tiles and tilesused
Const MSG_RECVTILES = 2		; incoming tiles/word
Const MSG_CLEARTILES = 3	; word was rejected clear tiles from board
Const MSG_RECVSYNC = 4		; new player receives all info
Const MSG_CHAT = 5			; text messages
Const MSG_PLACETILES = 6	; word accepted - place it on board
Const MSG_NEXTPLAYER = 7	; turn complete - pass it to next player
Const MSG_SYNCME = 8		; request for bag, tilesused and a playernum
Const MSG_CONFIRM = 9		; ask for a y/n true false response
Const MSG_ACC_REJ = 10		; the y/n response
Const MSG_RECVWORDS = 11	; list of words and locations - for synch up
Const MSG_MAPPING = 12		; net_id - playernum mapping
Const MSG_SCORE = 13		; game is over, final score
Const MSG_NEWPLAYER = 100	
Const MSG_PLAYERQUIT = 101
Const MSG_NEWHOST = 102
Const MSG_SESSIONLOST = 200

Const MAXPLAYERS = 4
Const MSGTICKS = 100 	; number of ticks that the message is displayed

Global numplayers = 1 
Global mynum = -5	; my turn number set to 0 to 3; 0 is the host
Global playernum = 0	; cycles 0,1,0,1... or 0,1,2,0,1,2... or 0,1,2,3,0,1,2,3
Global tilesused = 0  	; 0-100
Global rowcol = 0	; 1 = col, 2 = row
Global tilesplaced	; 0-7
Global turnscore	; score for the current turn
Dim totalscore(4)	; total scores for all players
Global exmode=False
Global showmess
Global mess$		;
Global numwords		; number of words played
Global holdingtile
Global tileheld
Global quit
Global multiplay=0
Global nsync=False
Global gameover=False
Global numrsvps
Dim reply(4)
Global chat$
Global passed = 0
Dim playeralive(4)
Global scoresent=False
Global imhost = False
Global singleplayer

Global showpanel	; toggle 0,1,2,...  words, tiles, points, ...
Global numpanels = 3   ; 4 for debug
Dim tpw$(4)					; tile points words panels
tpw$(0) = "Show Tile List"
tpw$(1) = "Show Point List"
If numpanels = 3
	tpw$(2) = "Show Word List"
Else
	tpw$(2) = "Show Bag Tiles"  ; for debug - show list of tiles in bag
	tpw$(3) = "Show Word List"
EndIf

Dim pcol.coltype(4)
For t = 0 To 3
	pcol.coltype(t) = New coltype
	Read pcol(t)\r
	Read pcol(t)\g
	Read pcol(t)\b
Next
; player color data r,g,b
Data 0,100,255
Data 255,255,100
Data 255,100,255
Data 100,255,100

Dim boardbg(15,15)
Dim boardt(15,15)

For x=0 To 14
	For y = 0 To 14
		Read boardbg(x,y)
	Next
Next
;star=0,dw,tw,dl,tl=4,b=5
Data 2,5,5,3,5,5,5,2,5,5,5,3,5,5,2
Data 5,1,5,5,5,4,5,5,5,4,5,5,5,1,5
Data 5,5,1,5,5,5,3,5,3,5,5,5,1,5,5
Data 3,5,5,1,5,5,5,3,5,5,5,1,5,5,3
Data 5,5,5,5,1,5,5,5,5,5,1,5,5,5,5
Data 5,4,5,5,5,4,5,5,5,4,5,5,5,4,5
Data 5,5,3,5,5,5,3,5,3,5,5,5,3,5,5
Data 2,5,5,3,5,5,5,0,5,5,5,3,5,5,2
Data 5,5,3,5,5,5,3,5,3,5,5,5,3,5,5
Data 5,4,5,5,5,4,5,5,5,4,5,5,5,4,5
Data 5,5,5,5,1,5,5,5,5,5,1,5,5,5,5
Data 3,5,5,1,5,5,5,3,5,5,5,1,5,5,3
Data 5,5,1,5,5,5,3,5,3,5,5,5,1,5,5
Data 5,1,5,5,5,4,5,5,5,4,5,5,5,1,5
Data 2,5,5,3,5,5,5,2,5,5,5,3,5,5,2

SeedRnd MilliSecs()

Dim distribution(27)
Dim shuffled(100)

t=0
For l = 0 To 26
	Read lcount
	distribution(l) = lcount
	For x = t To t + lcount-1
		shuffled(x) = l
	Next
	t=t+lcount
Next

; 100 tiles
.distributiondata
Data 9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1,2

Dim points(27)
For t = 0 To 26
	Read points(t)
Next 
; points
Data 1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10,0

Dim rack(12),exrack(12)

Global menux=18*33
Global menuy=15*33+10

; Set up display:
Graphics 800,600,32,2

; Load images:
Global letters = LoadAnimImage ("letters.bmp",32,32,0,70)
Global pointer = LoadAnimImage ("pointer.bmp",32,32,0,1)


SetBuffer BackBuffer()
DoIntro()

SetBuffer FrontBuffer()
;get player name
Cls
Color 255,255,255
Text 16,116,"Player name?"
Locate 16,132
Repeat
	name$=Input$()
Until name$<>""

FlushKeys()

; single or mutiplayer game?
Text 16,148,"Single Player Game? Y/N" 
answer=0
Repeat
	Delay 1
	If KeyHit(21) Then answer = 1 ; Y
	If KeyHit(49) Then answer = -1; N
Until answer <> 0
If answer = 1
	singleplayer = True
EndIf


If singleplayer = False
	multiplay = StartNetGame()
Else
	multiplay = 2
EndIf
;create a local player
Global player.player = New player
player\name=name$
If singleplayer = False
	player\net_id=CreateNetPlayer( name$ )
Else
	player\net_id=0
EndIf
player\playernum = 0	; the only place the host playernum gets assigned

For t = 0 To 3:playeralive(t)=False:Next

If multiplay = 2 ;=2 created new game
	tilesused = 0
	mynum = 0
	imhost = True
	playeralive(0) = True
	ShuffleTiles()
	ClearBoard()
	ClearRack()
	RefillRack()
ElseIf multiplay = 1 
	;=1 joining a game
	ClearBoard()
	ClearRack()
	; send this request when joining a game 
	SendNetMsg 8,"Sync me up!",player\net_id,0,0
	cnt = 0
	While nsync=False Or mynum=-5
		; get bag of tiles and tilesused and mynum
		GetNetMessages()
		cnt = cnt+1
		Delay 10
		; try again?
		If cnt > 100 Then cnt=0:SendNetMsg 8,"Sync me up!",player\net_id,0,0
	Wend
	If mynum <0 ; (-1 returned - only 3 can join a game)
		Cls
		Text 16,16,"Host is not accepting any more players."
		Delay 4000
		End
	EndIf
	playeralive(mynum) = True
	RefillRack()
	SendBag()
Else 
	; =0 couldn't start game
	Cls
	Text 16,16,"Could not start Game.(Multiplayer problem.)"
	Delay 4000
	End
EndIf

holdingtile=False
quit=False
showpanel=0
numwords=0

SetBuffer BackBuffer()



; Main loop:
Repeat

If MouseHit(1)
	x=MouseX()/33
	y=MouseY()/33
	If x>=0 And x < 15 And y>=0 And y< 15 And exmode=False And playernum = mynum
		tilethere = boardt(x,y)
		For tin.tiletype=Each tiletype
			If tin\x = x And tin\y = y Then tilethere=tin\tilenum
		Next
		Select tilethere
			Case -1
				; nothing there
				If holdingtile = True
					; action depends on how many tiles are down
					Select tilesplaced
						Case 0
							; drop tile anywhere
							tin.tiletype=New tiletype
							tin\x = x
							tin\y = y
							tin\tilenum = tileheld
							tilesplaced = tilesplaced+1
							holdingtile=False
						Case 1
							;drop tile here if same row or column
							tin.tiletype = First tiletype
							xfirst = tin\x
							If x = xfirst Or y= tin\y
								tin.tiletype=New tiletype
								tin\x = x
								tin\y = y
								tin\tilenum = tileheld
								tilesplaced = tilesplaced+1
								holdingtile=False
								; next tile must be same row or column
								If x = xfirst Then rowcol = 1 Else rowcol = 2
							EndIf
						Default
							; 3rd to 7th tiles - compare against the 1st
							tin.tiletype = First tiletype
							Select rowcol
								Case 1
									; same column?
									If x = tin\x
										tin.tiletype=New tiletype
										tin\x = x
										tin\y = y
										tin\tilenum = tileheld
										tilesplaced = tilesplaced+1
										holdingtile=False
									Else
										showmess=MSGTICKS
										mess$="Must be in the same column or row!"
									EndIf 
								Case 2
									; same row?
									If y = tin\y
										tin.tiletype=New tiletype
										tin\x = x
										tin\y = y
										tin\tilenum = tileheld
										tilesplaced = tilesplaced+1
										holdingtile=False
									Else
										showmess=MSGTICKS
										mess$="Must be in the same row or column!"
									EndIf
							End Select
					End Select
				Else
					; click on board while not holding a tile
					; so do nothing ?
				EndIf
			Default
				If holdingtile = False
					; can only pick up tiles we are placing this turn
					; find the tile, remove it from board and hold it
					For tin.tiletype=Each tiletype
						If x = tin\x And y=tin\y
							holdingtile = True
							tileheld = tin\tilenum
							tilesplaced = tilesplaced - 1
							Delete tin
						EndIf
					Next
				Else
					; there is a tile there, swap it with the one in hand
					If boardt(x,y)=-1
						For tin.tiletype=Each tiletype
							If tin\x = x And tin\y = y
								temp = tin\tilenum
								tin\tilenum = tileheld
								tileheld=temp
							EndIf
						Next
					Else
						; can't swap with tiles placed in previous turns
						showmess=MSGTICKS
						mess$ = "Can't Overlap Tiles"
					EndIf
				EndIf
		End Select
	Else 
		CheckMenu()
		CheckRack(x,y)
	EndIf
EndIf

If singleplayer = False GetNetMessages()
If singleplayer = False GetChatString()

If KeyDown (1)=1	; ESC- quit?
	If Confirm("Quit") = True 
		quit=True
	EndIf
EndIf

DrawAll()
; show it....
Flip
counter = counter + 1
;If KeyHit(88) SaveBuffer(FrontBuffer(),"SNAPHOT"+counter+".bmp")
VWait

Until quit=True 
End






;----------------------------------------------------------------------------------
Function DrawAll()
; draw everything....
Cls
Color 255,255,255
Rect 0,0,800,600,0
DrawTiles()
DrawScores()
If singleplayer = False DrawChat()
If holdingtile = True 
	DrawBlock letters,MouseX()-14,MouseY()-14,35
	DrawBlock letters,MouseX()-15,MouseY()-15,35
	DrawBlock letters,MouseX()-16,MouseY()-16,tileheld
EndIf
DrawImage pointer,MouseX(),MouseY(),0
If gameover = True
	Color 255,255,60
	Text MouseX(),MouseY(),"Game Over"
EndIf
If showmess >0
	showmess=showmess-1
	Color 255,60,60
	Text 10,17*33+8,mess$
EndIf
End Function





Function DrawChat()
y=33*13-2-10
r=255
Color 255,255,0
Rect 33*15+2,33*13-14,300,12*6+9,0
Text 33*15+5,y,">"+chat$
For i.Info=Each Info
	If r> 180
		Color r,r,0
		y=y+12
		Text 33*15+5,y,i\txt$
		r=r-16
	Else
		Delete i
	EndIf
Next
End Function






Function GetNetMessages()
	While RecvNetMsg()
		Select NetMsgType()
		Case 1:
			; updated bag of tiles, tilesused
			RecvBag( NetMsgData$() )
		Case 2:
			; list of tiles came in
			RecvTiles( NetMsgData$() )
		Case 3:
			; word rejected - delete it
			ClearTiles()
		Case 4:
			; received mynum
			mynum = Int(Mid$( NetMsgData$(),1,6 ))
			playernum = Int(Mid$( NetMsgData$(),6,6 ))
		Case 5:
			; chat message
			info( NetPlayerName$( NetMsgFrom() )+":"+NetMsgData$() )
		Case 6:
			; all have accepted - so place the tiles
			passed=0
			AcceptTiles()
		Case 7:
			; next player
			playernum=NextPlayer()
			If tilesused = 100
				passed=passed+1:IsGameOver()
			EndIf
;			info("Player "+Abs(playernum+1)+" is Up")
		Case 8:
			If imhost = True  ; only the host replies
				; sync request - send bag of tiles and tilesused and playernumber
				; info("synch req from " + NetPlayerName$(NetMsgFrom()))
				SendPlayerNum(NetMsgFrom())
				SendBag()
				SendWords(NetMsgFrom())
				SendPlayerNumAll()		;let all players know who's who
			EndIf
		Case 9:
			; pop up the confirm y/n, then send response to player that asked
			rsvp = Confirm(NetMsgData$())
			If rsvp 
				SendNetMsg 10,"Accept",player\net_id,NetMsgFrom(),0
			Else
				SendNetMsg 10,"Reject",player\net_id,NetMsgFrom(),0
			EndIf
		Case 10:
			; confirm response y/n
			p.player=FindPlayer( NetMsgFrom() )
			If p<>Null
				info( p\name+" says "+NetMsgData$())
				numrsvps=numrsvps+1
				If NetMsgData$() = "Accept"
					reply(p\playernum) = True
				Else
					reply(p\playernum) = False		
				EndIf
			EndIf
		Case 11:
			; list of words came in
			RecvWords( NetMsgData$() )
		Case 12:
			; recv mappings of net_id->playernum from the host 
			net_id = Int(Mid$( NetMsgData$(),1,10 ))
			pnum = Int(Mid$( NetMsgData$(),10,6 ))
			p.Player=FindPlayer( net_id )
			If p<>Null
				p\playernum = pnum
				playeralive(pnum) = True
				;info(NetMsgData$()) 
			EndIf
		Case 13:; MSG_SCORE
			; recv final scores 
			pscore = Int(Mid$( NetMsgData$(),1,4 ))
			p.Player=FindPlayer( NetMsgFrom() )
			If p<>Null
				 totalscore(p\playernum ) = pscore
			EndIf
			gameover=True
			SendMyScore()
		Case 100:
			If numplayers =< MAXPLAYERS
				p.player=New player
				p\net_id=NetMsgFrom()
				p\name=NetPlayerName$( NetMsgFrom() )
				info( p\name + " has joined the game. " )
				numplayers=numplayers+1
			EndIf
		Case 101:
			p.player=FindPlayer( NetMsgFrom() )
			If p<>Null
				info( p\name+" has left the game. " )
				; if it was their turn, then pass turn to next player
				; if they had tiles on their rack??? TODO!!!!! - recover tiles
				playeralive(p\playernum) = False
				If p\playernum = playernum
					Delete p
					playernum = NextPlayer()
				Else
					Delete p
				EndIf
				numplayers=numplayers-1
			EndIf
		Case 102:
			info( "I'm the new host! " )
			imhost=True
		Case 200:
			EndGraphics
			Text 0,0,"The session has been lost!"
			WaitKey
			End
		End Select
	Wend

End Function




Function GetChatString()
;Chat - build the chat string - send on enter key
key=GetKey()
If key
	If key=13
		If chat$<>""
			SendNetMsg MSG_CHAT,chat$,player\net_id,0,0
			info(chat$)
		EndIf
		chat$=""
	Else If key=8
		If Len(chat$)>0 Then chat$=Left$(chat$,Len(chat$)-1)
	Else If key>=32 And key<127
		If Len(chat$)<31 Then chat$=chat$+Chr$(key)
	EndIf
EndIf
End Function





Function info( t$ )
	i.Info=New Info
	i\txt$=Mid$(t$,1,31)
	Insert i Before First Info
End Function





Function RecvBag( msg$ )
For t= 1 To 100
	kar$ = Mid$(msg$,t,1)
	If kar$ <> "?" Then num = Asc(kar$)-65 Else num = 26
	If num >25 Then num=26
	shuffled(t-1)=num
Next
x=Int(Mid$( msg$,101,6 ))
;info("recvbag tilesused="+Mid$(msg$,101,6))
tilesused = x
nsync=True
End Function





Function RecvTiles( msg$ )
For t= 0 To Len(msg$)/3-1
	kar$ = Mid$(msg$,t*3+1,1)
	If kar$ <> "?" Then num = Asc(kar$)-65 Else num = 26
	x = Asc(Mid$(msg$,t*3+2,1))-65
	y = Asc(Mid$(msg$,t*3+3,1))-65
	tin.tiletype=New tiletype
	tin\x = x
	tin\y = y
	tin\tilenum = num
	tilesplaced = tilesplaced+1
Next
End Function





Function FindPlayer.player( id )
;find player with player id
For p.player=Each player
	If p\net_id=id Then Return p
Next
End Function






Function SendBag()
; update all the other players about the newly changed bag of tiles - shuffle()
; 100 characters + tilesused (6 chrs)
shuffles$=""
For t= 0 To 99
	If shuffled(t) < 26 Then kar$ = Chr$(shuffled(t)+65) Else kar$="?"
	shuffle$=shuffle$+kar$
Next
shuffle$=shuffle$+LSet$(Int(tilesused),6)
;info("sendbag tilesused="+LSet$(Int(tilesused),6))
SendNetMsg MSG_RECVBAG,shuffle$,player\net_id,0,0
End Function






Function SendPlaceTiles()
SendNetMsg MSG_PLACETILES,"Place tiles!",player\net_id,0,0
End Function






Function SendPlayerNum(net_id)
; someone wants to join - find a spot, if full send a -1
; find free playernum
pn = -1
For t = MAXPLAYERS-1 To 0 Step -1
	If playeralive(t) = False
		pn = t
	EndIf
Next
playnum$=LSet$(Int(pn),6)
; tack on the current playernum too
playnum$=playnum$+LSet$(Int(playernum),6)
SendNetMsg 4,playnum$,player\net_id,net_id,0
p.player=FindPlayer( net_id )
If p<>Null
	p\playernum = pn
	If pn > -1
		playeralive(pn) = True
	EndIf
EndIf
End Function






Function SendPlayerNumAll()
; the host sends out list of net_id->playernum mapping after new player joins
For p.player = Each player
	playnums$=LSet$(Int(p\net_id),10)
	playnums$=playnums$+LSet$(Int(p\playernum),4)
	SendNetMsg MSG_MAPPING,playnums$,player\net_id,0,0
Next
End Function






Function SendTiles()
; update all the other players about the tiles we are placing down
; eg A at 0,0  C at 0,1 E at 0,2  = AAACABEAC 
; - unpacked in 3s at other end - max 21 characters
tile$=""
For t.tiletype = Each tiletype
	If t\tilenum < 26 Then kar$ = Chr$(t\tilenum+65) Else kar$="?"
	x$ = Chr$(t\x+65)
	y$ = Chr$(t\y+65)
	tiles$=tiles$+kar$+x$+y$
Next
SendNetMsg MSG_RECVTILES,tiles$,player\net_id,0,0
End Function






Function SendWords(net_id)
; update the player's word list
words$ = ""
For w.word = Each word
	; pack the word info into a string
	words$=words$+LSet$(Int(Len(w\wordstring$)),3)
	words$=words$+w\wordstring$
	words$=words$+LSet$(Int(w\startx),3)
	words$=words$+LSet$(Int(w\starty),3)
	words$=words$+LSet$(Int(w\endx),3)
	words$=words$+LSet$(Int(w\endy),3)
	words$=words$+LSet$(Int(w\points),3)
	words$=words$+LSet$(Int(w\id),3)
	words$=words$+LSet$(Int(w\playernum),3)
Next
SendNetMsg MSG_RECVWORDS,words$,player\net_id,net_id,0
End Function






Function RecvWords(msg$)
;unpack the words and add them to board and add up the scores
pos=1
While pos < Len(msg$)
	w.word = New word
	wlen = Int(Mid$( msg$,pos,3 ))
	w\wordstring$ = Mid$( msg$, pos+3, wlen )
	w\startx = Int(Mid$( msg$,pos+3+wlen,3 ))
	w\starty = Int(Mid$( msg$,pos+3+wlen+3,3 ))
	w\endx = Int(Mid$( msg$,pos+3+wlen+6,3 ))
	w\endy = Int(Mid$( msg$,pos+3+wlen+9,3 ))
	w\points = Int(Mid$( msg$,pos+3+wlen+12,3 ))
	w\id = Int(Mid$( msg$,pos+3+wlen+15,3 ))
	w\playernum = Int(Mid$( msg$,pos+3+wlen+18,3 ))
	pos=pos+wlen+24
	;info(w\wordstring$ +" "+ w\starty + " " +w\endy )
Wend
; do something with the words....
For w.word=Each word
	If w\endx-w\startx > 0
		pos = 1
		For t = w\startx To w\endx
				kar$ = Mid$(w\wordstring$, pos, 1)
				If kar$ <> "?" Then num = Asc(kar$)-65 Else num = 26
				boardt(t,w\starty)=num
				pos=pos+1
		Next
	Else
		pos = 1
		For t = w\starty To w\endy
				kar$ = Mid$(w\wordstring$, pos, 1)
				If kar$ <> "?" Then num = Asc(kar$)-65 Else num = 26
				boardt(w\startx,t)=num
				pos=pos+1
		Next
	EndIf 
	totalscore(w\playernum) = totalscore(w\playernum) + w\points
Next
End Function






Function AcceptTiles()
; ordered to place the tiles from another player down on board, update counters
turnscore = CalculatePoints()
totalscore(playernum)=totalscore(playernum)+turnscore
tilesplaced = 0
tilesused = tilesused + PlaceTiles()
If tilesused > 100 Then tilesused = 100
End Function





Function GetResponses()
x=MouseX()
y=MouseY()
If x>800-200 Then x=800-200
If x<50 Then x=50
If y>600-100 Then y=600-100
If y<50 Then y=50
For t = 0 To 3 
	reply(t) = True
Next
numrsvps = 0
SendNetMsg MSG_CONFIRM,"Accept",player\net_id,0,0
While numrsvps < numplayers-1
	DrawAll()
	Color 255,255,255
	Rect 0,0,800,600,0
	Color 255,255,0
	Rect x-2,y-2,136,34
	Color 80,80,220
	Rect x,y,132,30
	Color 255,255,255
	Text x+10,y+2,"Please Wait"
	Flip
	GetNetMessages()
Wend
For t = 0 To 3
	If reply(t) = False Then Return False
Next
Return True				
End Function







Function DrawScores()
For t=0 To 3
	Color pcol(t)\r,pcol(t)\g,pcol(t)\b
	If playeralive(t) 
	Text 33*13,15*33+15+t*12,"P"+Abs(t+1)+" Score: " + totalscore(t)
	EndIf
Next
Color pcol(playernum)\r,pcol(playernum)\g,pcol(playernum)\b
If playernum = mynum
	Text 33*13,15*33+15+4*12,"You're Up P"+Abs(playernum+1)+"!"
Else
	Text 33*13,15*33+15+4*12,"Player " + Abs(playernum+1)+ " is Up"
EndIf
Color 240,30,30
Text 33*13,15*33+15+5*12,"Tiles Left: " + Abs(100-tilesused)
If singleplayer = False 
	Text 33*13,15*33+15+6*12,"Player " + Abs(mynum+1) + " of " + Abs(numplayers)
EndIf
End Function






Function NextPlayer()

	t = playernum
	Repeat
	 	t=t+1:If t > 3 Then t = 0
	Until playeralive(t)= True
	Return t

End Function





Function IsGameOver()
If tilesused = 100 And gameover=False  ; out of tiles and not already ended
	rcount = 0
	For t = 0 To 11
		If rack(t) > -1 Then rcount = rcount + 1
	Next
	If rcount = 0 Then gameover = True
	; check if everyone has passed since we ran out of tiles
	If passed = numplayers Then gameover = True
	
	If gameover = True
		rackscore = 0
		For t = 0 To 11
			If rack(t) > -1
				rackscore = rackscore + points(rack(t))
			EndIf
		Next
		
		If rcount = 0
			; add rackscores from other players - calculate by what's not on the board.
			; incorrect if player drops out, taking tiles with them, this includes those tiles
			tscore = 0
			For t = 0 To 26
				tscore = tscore + points(t)*distribution(t)
			Next 
			bscore = 0
			For x = 0 To 14
				For y = 0 To 14
					If boardt(x,y) > -1 Then bscore = bscore + points(boardt(x,y))
				Next
			Next
			totalscore(mynum) = totalscore(mynum) + tscore - bscore	
		Else
			totalscore(mynum) = totalscore(mynum) - rackscore	
		EndIf 
		sc$ = LSet$(Int(totalscore(mynum)),4)
		If singleplayer = False SendNetMsg MSG_SCORE,sc$,player\net_id,0,0
		scoresent = True
	EndIf
EndIf
End Function





Function SendMyScore()
If scoresent = False
	rackscore = 0
	For t = 0 To 11
		If rack(t) > -1
			rackscore = rackscore + points(rack(t))
		EndIf
	Next

	totalscore(mynum) = totalscore(mynum) - rackscore

	sc$ = LSet$(Int(totalscore(mynum)),4) 
	SendNetMsg MSG_SCORE,sc$,player\net_id,0,0
	scoresent = True
EndIf
End Function





Function SendNextPlayer()
	SendNetMsg MSG_NEXTPLAYER,"Next Player",player\net_id,0,0
End Function




Function CheckMenu()
x= MouseX()/33
If x=18 And holdingtile = False 
	; menu items
	y1 = (MouseY()-menuy)/12
	;DrawImage letters,menux,menuy+12*y1,39
	Select y1
		Case 0 
			; player wants to end their turn by playing tiles
			If playernum = mynum
				If CheckTilePlacement()
					allok = False
					If singleplayer = False SendTiles()
					; pass info to other players and wait for accept or reject responses
					If singleplayer = False
						allok=GetResponses()
					Else
						allok=True
					EndIf
					; if all accept then place them and add points
					If allok
						If singleplayer = False SendPlaceTiles() ; tell all players to place word, update tilesused
						turnscore = CalculatePoints()
						totalscore(playernum)=totalscore(playernum)+turnscore
						tilesplaced = 0
						PlaceTiles()
						RefillRack()	; alters tilesused
						; if rack is now empty then the game is done
						IsGameOver()
						playernum = NextPlayer()
						; send net message - update playernum
						If singleplayer = False SendNextPlayer()
					Else
						If singleplayer = False SendNetMsg MSG_CLEARTILES,"Clear Tiles",player\net_id,0,0
						mess$="Your word was rejected!"
						showmess=MSGTICKS
						; TODO: clear tiles and skip turn???? - left to players for now
					EndIf
				EndIf
			EndIf
		Case 1 
			; player wants to end their turn by skipping
			If playernum = mynum And singleplayer = False
				If Confirm("Pass") = True
					If tilesplaced = 0
						IsGameOver()
						playernum = NextPlayer()
						; send net message - update playernum
						If singleplayer = False SendNextPlayer()
					Else
						mess$="Please remove tiles from board first!"
						showmess=MSGTICKS
					EndIf
				EndIf
			EndIf
		Case 2
			; player wants to end their turn by swapping tiles
			If playernum = mynum And tilesused < 100
				Select exmode
					Case True
						numtiles = 0
						For t = 0 To 11
							If exrack(t) > -1 Then numtiles = numtiles + 1
						Next
						If numtiles > 0 
							If Confirm("Swap") = True
								If ExchangeTiles() = True 
									exmode=False
									showmess=0
									playernum = NextPlayer()
									; send new shuffled array to all players
									If singleplayer = False SendBag()
									; send net message - update playernum
									If singleplayer = False SendNextPlayer()
								EndIf
							EndIf
						Else
							mess$="Swap canceled."
							showmess=MSGTICKS
							exmode=False
						EndIf
					Case False
						If tilesplaced > 0
							mess$="Please remove tiles from board first!"
							showmess=MSGTICKS
						Else
							exmode=True
							mess$="Place tiles to discard on the upper rack."
							showmess=MSGTICKS
						EndIf
				End Select
			EndIf
		Case 3
			; can shuffle anytime
			ShuffleRack()
		Case 4
			; rotate through the display panels - anytime
			; 0 - words, 1 - tiles, 2 - points
			showpanel = showpanel + 1
			If showpanel > numpanels-1 Then showpanel = 0 ; debug
		Case 5
			If singleplayer = True
				; only - will be removed from final version
				If Confirm("Reset") = True 
					gameover=False
					exmode=False
					ShuffleTiles()
					ClearTiles()
					ClearWordList()
					For t = 0 To 3:totalscore(t)=0:Next
					tilesused = 0
					numwords=0
					ClearBoard()
					ClearRack()
					RefillRack()
				EndIf
			EndIf
		Case 6
			If Confirm("Quit") = True 
				quit=True
			EndIf
	End Select
EndIf
End Function






Function CheckRack(x,y)
; check if we clicked on the rack or exchange rack
If holdingtile = True 
	; if holding a tile
	If y=16 And x>=0 And x<12
		If rack(x) = -1
			;put back on the rack in empty spot
			rack(x) = tileheld
			holdingtile=False
		Else
			;swap tile with the one under it
			temp=tileheld
			tileheld=rack(x)
			rack(x)=temp
		EndIf
	EndIf
	If y=15 And x>=0 And x<12 And exmode=True
		If exrack(x) = -1
			;put on the  exchange rack
			exrack(x) = tileheld
			holdingtile=False
		Else
			;swap tile with the one under it
			temp=tileheld
			tileheld=exrack(x)
			exrack(x)=temp
		EndIf
	EndIf
Else
	If y=16 And x>=0 And x<12
		If rack(x) >-1
			;Pick up a tile from the rack
			tileheld = rack(x)
			rack(x) = -1
			holdingtile=True
		EndIf
	EndIf
	If y=15 And x>=0 And x<12 And exmode=True
		If exrack(x) >-1
			;Pick up a tile from the exrack
			tileheld = exrack(x)
			exrack(x) = -1
			holdingtile=True
		EndIf
	EndIf
EndIf
End Function








Function Confirm(conf$)
; draw box with   No   Yes  boxes at the bottom
x=MouseX()
y=MouseY()
If x>800-200 Then x=800-200
If x<50 Then x=50
If y>600-100 Then y=600-100
If y<50 Then y=50
answer=False
While answer=False
	If KeyHit(21) Then answer = True:ret=True
	If KeyHit(49) Then answer = True:ret=False
	If MouseHit(1)
		; check position
		xm=MouseX():ym=MouseY()
		If xm>x+5 And xm< x+25 And ym>y+19 And ym<y+29
			; no hit
			answer = True:ret=False
		EndIf
		If xm>x+105 And xm< x+125 And ym>y+19 And ym<y+29
			; yes hit
			answer = True:ret=True
		EndIf
	EndIf
	DrawAll()
	If tmr < 50 Then Color 255,255,0 Else Color 255,0,0
	Rect x-2,y-2,136,34
	Color 80,80,220
	Rect x,y,132,30
	Color 255,255,255
	Text x+10,y+2,conf$+" y/n?"
	DrawImage letters,x+5,y+19,38
	DrawImage letters,x+105,y+19,37
	DrawImage pointer,MouseX(),MouseY(),0
	tmr=tmr+1:If tmr> 100 Then tmr=0
	Flip
Wend
Return ret
End Function






Function PlaceTiles()
t=0
For tin.tiletype=Each tiletype
	;place floating tiles on board
	boardt(tin\x,tin\y) = tin\tilenum
	Delete tin
	t=t+1
Next
Return t
End Function






Function CheckTilePlacement()

tin.tiletype = First tiletype
If tin <> Null
	highx = tin\x
	highy = tin\y
	lowx = tin\x
	lowy = tin\y
	tcount=0
	For tin.tiletype=Each tiletype
		boardt(tin\x,tin\y) = -2 ; mark this for next step
		If tin\x > highx Then highx=tin\x
		If tin\x < lowx Then lowx=tin\x
		If tin\y > highy Then highy=tin\y
		If tin\y < lowy Then lowy=tin\y
		tcount=tcount+1
	Next

	;check that there are tiles between the highx and lowx and highy and lowy
	For x = lowx To highx
		If boardt(x,lowy) =-1
			showmess=MSGTICKS
			mess$ = "Must be a continuous Row or Column!"
			For tin.tiletype=Each tiletype
				boardt(tin\x,tin\y) = -1
			Next		
			Return False ; gap in the row
		EndIf
	Next 
	For y = lowy To highy
		If boardt(lowx,y) =-1
			showmess=MSGTICKS
			mess$ = "Must be a continuous Column or Row!"
			For tin.tiletype=Each tiletype
				boardt(tin\x,tin\y) = -1
			Next		
			Return False ; gap in the column
		EndIf
	Next 

	If boardt(7,7) <= -1  ; -1 or -2 on first turn , -2 is ok
		; not enough tiles
		If tcount < 2
			showmess=MSGTICKS
			mess$ = "First word must be at least 2 tiles!"
			For tin.tiletype=Each tiletype
				boardt(tin\x,tin\y) = -1
			Next		
			Return False 
		EndIf
		
		 ; first word must be on center square
		If boardt(7,7) = -1
			showmess=MSGTICKS
			mess$ = "First word must be on center square!"
			For tin.tiletype=Each tiletype
				boardt(tin\x,tin\y) = -1
			Next		
			Return False
		EndIf	
	Else
		;check single tile is next to existing tile
		tok=0
		For tin.tiletype = Each tiletype
			tabove=0
			tbelow=0
			tright=0
			tleft=0
			If tin\y-1 >=0 Then tabove=boardt(tin\x,tin\y-1)
			If tin\y+1 <15 Then tbelow=boardt(tin\x,tin\y+1)
			If tin\x+1 <15 Then tright=boardt(tin\x+1,tin\y)
			If tin\x-1 >=0 Then tleft=boardt(tin\x-1,tin\y)
			If tabove>-1 Or tbelow > -1 Or tright >-1 Or tleft >-1
				tok=tok+1
			EndIf
		Next
		If tok = 0
			showmess=MSGTICKS
			mess$ = "Must be next to another tile!"
			For tin.tiletype=Each tiletype
				boardt(tin\x,tin\y) = -1
			Next		
			Return False ; lone tile
		EndIf
	EndIf
Else
	; no tiles placed 
	showmess=MSGTICKS
	mess$="No tiles placed on board!"
	Return False
EndIf

Return True

End Function







Function CalculatePoints()
score = 0
tin.tiletype = First tiletype
If tin <> Null
	highx = tin\x
	highy = tin\y
	lowx = tin\x
	lowy = tin\y
	tcount=0
	For tin.tiletype=Each tiletype
		boardt(tin\x,tin\y) = -2
		If tin\x > highx Then highx=tin\x
		If tin\x < lowx Then lowx=tin\x
		If tin\y > highy Then highy=tin\y
		If tin\y < lowy Then lowy=tin\y
		tcount=tcount+1
	Next

	If tilesplaced > 1
		; are tiles in a row?
		If highx-lowx > 0
			;tiles in a row
			;find complete row - check lower and higher
			If lowx > 0
				If boardt(lowx-1,lowy) >-1
					ok=True
					Repeat
						lowx=lowx-1
						If lowx=0
							ok=False
						Else
							If boardt(lowx-1 ,lowy) < 0 Then ok=False
						EndIf
					Until Not ok
				EndIf
			EndIf
			If highx < 14
				If boardt(highx+1,lowy) >-1
					ok=True
					Repeat
						highx=highx+1
						If highx=14
							ok=False
						Else
							If boardt(highx+1,lowy) < 0 Then ok=False ; here!!!
						EndIf
					Until Not ok
				EndIf
			EndIf
	
			bonusx = 1:word$=""
			For x = lowx To highx
				If boardt(x,lowy) >-1
					score = score + points(boardt(x,lowy))
					If boardt(x,lowy) <26 Then word$=word$+Chr$(boardt(x,lowy)+65) Else word$=word$+"?"
				Else
					For tin.tiletype=Each tiletype
						If tin\x = x
							; found the tile on this square
							score = score + points(tin\tilenum)*TileBonus(x,lowy)
							If tin\tilenum <26 Then word$=word$+Chr$(tin\tilenum+65) Else word$=word$+"?"
						EndIf
					Next
					bonusx = bonusx * WordBonus(x,lowy)
				EndIf
			Next
			score = score * bonusx
			If tcount = 7
				score=score+50
				mess = 100
				mess$=mess$+"50 Point Bonus for using all tiles!"
			EndIf
			AddNewWord(lowx,highx,lowy,lowy,word$,score)
			
			For tin.tiletype=Each tiletype
				; check for tiles above and below, if there are then get the points
				lowy= tin\y
				highy= tin\y
				If lowy>0
					If boardt(tin\x,lowy-1) > -1
						ok=True
						Repeat
							lowy=lowy-1
							If lowy=0
								ok=False
							Else 
								If boardt(tin\x ,lowy-1) < 0 Then ok=False
							EndIf
						Until Not ok
					EndIf
				EndIf
				If highy <14
					If boardt(tin\x,highy+1) > -1
						ok=True
						Repeat
							highy=highy+1
							If highy=14
								ok=False
							Else
								If boardt(tin\x ,highy+1) < 0 Then ok=False
							EndIf
						Until Not ok
					EndIf
				EndIf
				If highy-lowy > 0
					score = score + PointsForCol(tin\x,lowy,highy,tin\tilenum)
				EndIf
			Next
		Else
			;the tiles are in a column
			;find complete column - check lower and higher
			If lowy > 0
				If boardt(lowx,lowy-1) >-1
					ok=True
					Repeat
						lowy=lowy-1
						If lowy=0 
							ok=False
						Else
							If boardt(lowx ,lowy-1) < 0 Then ok = False
						EndIf
					Until Not ok 
				EndIf
			EndIf
			If highy < 14
				If boardt(lowx,highy+1) >-1
					ok=True
					Repeat
						highy=highy+1
						If highy=14
							ok=False
						Else
							If boardt(lowx ,highy+1) < 0 Then ok=False
						EndIf
					Until Not ok 
				EndIf
			EndIf
	
			bonusx = 1:word$=""
			For y = lowy To highy
				If boardt(lowx,y) >-1
					score = score + points(boardt(lowx,y))
					If boardt(lowx,y) <26 Then word$=word$+Chr$(boardt(lowx,y)+65) Else word$=word$+"?"
				Else
					For tin.tiletype=Each tiletype
						If tin\y = y
							; found the tile on this square
							score = score + points(tin\tilenum)*TileBonus(lowx,y)
							If tin\tilenum <26 Then word$=word$+Chr$(tin\tilenum+65) Else word$=word$+"?"
						EndIf
					Next
					bonusx = bonusx * WordBonus(lowx,y)
				EndIf
			Next 
			score = score * bonusx
			If tcount = 7
				score=score+50
				mess = 100
				mess$=mess$+"50 Point Bonus for using all tiles!"
			EndIf
			AddNewWord(lowx,lowx,lowy,highy,word$,score)

			For tin.tiletype=Each tiletype
				; check for tiles left and right, if there are then get the points
				lowx= tin\x
				highx= tin\x
				If lowx>0
					If boardt(lowx-1,tin\y) > -1
						ok=True
						Repeat
							lowx=lowx-1
							If lowx=0
								ok=False
							Else
								If boardt(lowx-1 ,tin\y) < 0 Then ok=False
							EndIf
						Until Not ok
					EndIf
				EndIf
				If highx <14
					If boardt(highx+1,tin\y) > -1
						ok=True
						Repeat
							highx=highx+1
							If highx=14
								ok=False
							Else
								If boardt(highx+1 ,tin\y) < 0 Then ok=False
							EndIf
						Until Not ok 
					EndIf
				EndIf
				If highx-lowx > 0
					score = score + PointsForRow(lowx,highx,tin\y,tin\tilenum)
				EndIf
			Next
		EndIf
	Else
		; single tile placed - check both row and column 
		tin.tiletype = First tiletype
		lowx= tin\x
		highx= tin\x
		lowy= tin\y
		highy= tin\y
		If lowx > 0
			If boardt(lowx-1,lowy) >-1
				ok=True
				Repeat
					lowx=lowx-1
					If lowx = 0 
						ok = False
					Else
						If boardt(lowx-1,lowy) < 0 Then ok = False
					EndIf
				Until Not ok 
			EndIf
		EndIf
		If highx < 14
			If boardt(highx+1,lowy) >-1
				ok=True
				Repeat
					highx=highx+1
					If highx=14 
						ok=False
					Else
						If boardt(highx+1,lowy) < 0 Then ok=False
					EndIf
				Until Not ok
			EndIf
		EndIf

		If highx-lowx > 0
			score = score + PointsForRow(lowx,highx,tin\y,tin\tilenum)
		EndIf

		tin.tiletype= First tiletype
		lowx= tin\x
		highx= tin\x
		lowy= tin\y
		highy= tin\y
		If lowy > 0
			If boardt(lowx,lowy-1) >-1
				ok=True
				Repeat
					lowy=lowy-1
					If lowy=0
						ok=False
					Else
						If boardt(lowx ,lowy-1) < 0 Then ok=False
					EndIf
				Until Not ok
			EndIf
		EndIf
		If highy < 14
			If boardt(lowx,highy+1) >-1
				ok=True
				Repeat
					highy=highy+1
					If highy=14
						ok=False
					Else
						If boardt(lowx ,highy+1) < 0 Then ok=False
					EndIf
				Until Not ok
			EndIf
		EndIf
		If highy-lowy > 0
			score = score + PointsForCol(tin\x,lowy,highy,tin\tilenum)
		EndIf
	EndIf
EndIf

Return score
End Function




Function PointsForRow(xlow,xhigh,y,tilenum)
bonusx = 1:word$=""
For x = xlow To xhigh
	If boardt(x,y) >-1
		rowscore = rowscore + points(boardt(x,y))
		If boardt(x,y)<26 Then word$=word$+Chr$(boardt(x,y)+65) Else word$=word$+"?"
	Else
		rowscore = rowscore + points(tilenum)*TileBonus(x,y)
		bonusx = bonusx * WordBonus(x,y)
		If tilenum < 26 Then word$=word$+Chr$(tilenum+65) Else word$=word$+"?"
	EndIf
Next 
rowscore = rowscore * bonusx
AddNewWord(xlow,xhigh,y,y,word$,rowscore)

Return rowscore
End Function




Function PointsForCol(x,ylow,yhigh,tilenum)
bonusx = 1:word$=""
For y = ylow To yhigh
	If boardt(x,y) >-1
		colscore = colscore + points(boardt(x,y))
		If boardt(x,y)<26 Then word$=word$+Chr$(boardt(x,y)+65) Else word$=word$+"?"
	Else
		colscore = colscore + points(tilenum)*TileBonus(x,y)
		bonusx = bonusx * WordBonus(x,y)
		If tilenum < 26 Then word$=word$+Chr$(tilenum+65) Else word$=word$+"?"
	EndIf
Next 
colscore = colscore * bonusx
AddNewWord(x,x,ylow,yhigh,word$,colscore)

Return colscore
End Function




Function AddNewWord(lx,hx,ly,hy,word$,score)
	numwords=numwords+1
	w.word = New word
	w\wordstring = word$
	w\startx = lx
	w\starty = ly
	w\endx = hx
	w\endy = hy
	w\points = score
	w\playernum = playernum
	w\id=numwords
End Function





Function TileBonus(x,y)
Select boardbg(x,y)
	Case 3 ; double letter square
		Return 2
	Case 4 ; triple letter square
		Return 3
	Default
		Return 1
End Select
End Function




Function WordBonus(x,y)
Select boardbg(x,y)
	Case 0 ; double word center square
		Return 2
	Case 1 ; double word square
		Return 2
	Case 2 ; triple word square
		Return 3
	Default
		Return 1
End Select
End Function






Function ExchangeTiles()
numtiles = 0
For t = 0 To 11
	If exrack(t) > -1 Then numtiles = numtiles + 1
Next

If 100-tilesused >= numtiles

	;swap the tiles with the top ones
	nt=0
	For t = 0 To 11
		If exrack(t) > -1
			temp=exrack(t)
			exrack(t)=shuffled(tilesused+nt)
			shuffled(tilesused+nt)=temp
			nt=nt+1
		EndIf
	Next
	
	;shuffle the exchanged tiles back into the shuffled array
	For t = 0 To numtiles-1 ; max 0 to 6
		loc= Rnd(tilesused,100) 
		temp=shuffled(loc)
		shuffled(loc) = shuffled(tilesused+t)
		shuffled(tilesused+t) = temp
	Next
	
	;transfer them from exrack to the rack
	nt=0
	For t = 0 To 11
		If exrack(t) > -1
			While rack(nt) > -1
				nt=nt+1
			Wend
			rack(nt)=exrack(t)
			exrack(t)=-1
			nt=nt+1
		EndIf
	Next

Else
	If tilesused =100
		mess$="There are no tiles left!"
	Else
		mess$="There are only "+ Abs(100-tilesused)+" tiles left!"	
	EndIf
	showmess = MSGTICKS
	Return False
EndIf

Return True
End Function






Function RefillRack()
numtiles = 0
For t = 0 To 11
If rack(t) > -1 Then numtiles = numtiles + 1
Next
t = 0
While numtiles < 7 And tilesused<100 And t < 12
	 If rack(t) = -1
		rack(t)=shuffled(tilesused)
		tilesused = tilesused +1
		numtiles = numtiles +1
	EndIf
	t= t+1
Wend
End Function







Function DrawTiles()
;white background - eliminate this by changing bkground of board tiles to white
Color 255,255,255
Rect 0,0,15*33+1,15*33+1
;draw the board
For x = 0 To 14
	For y = 0 To 14
		If boardt(x,y) >= 0
			DrawBlock letters,x*33+1,y*33+1,boardt(x,y)
		Else
			DrawBlock letters,x*33+1,y*33+1,30+boardbg(x,y)
		EndIf
	Next
Next

;draw rack and tiles on rack
Color 160,80,50
Rect 0,16*33-3,12*33+3,33+6,1
For t= 0 To 11
	If rack(t) > -1 
		DrawBlock letters,t*33+1,16*33,rack(t)
	EndIf
Next

;draw exrack and tiles on exrack
If exmode=True
	Color 80,140,50
	Rect 0,15*33,12*33+3,33,1
	For t= 0 To 11
		If exrack(t) > -1 
			DrawBlock letters,t*33+1,15*33,exrack(t)
		EndIf
	Next
EndIf

; draw menu items
Color 60,60,200
DrawImage letters,menux,menuy   ,39:Text menux+28,menuy-2,"Done"
If singleplayer = False And numplayers >1
	DrawImage letters,menux,menuy+12,39:Text menux+28,menuy+10,"Pass"
EndIf
If tilesused < 100
	DrawImage letters,menux,menuy+24,39:Text menux+28,menuy+22,"Swap Tiles"
EndIf 
DrawImage letters,menux,menuy+36,39:Text menux+28,menuy+34,"Shuffle Rack"
DrawImage letters,menux,menuy+48,39:Text menux+28,menuy+46,tpw$(showpanel)
If singleplayer = True 
	DrawImage letters,menux,menuy+60,39:Text menux+28,menuy+58,"Reset Game"
EndIf
DrawImage letters,menux,menuy+72,39:Text menux+28,menuy+70,"Quit"

;draw the tiles in play
Color 255,0,0
For tin.tiletype=Each tiletype
	Rect tin\x*33-1+1,tin\y*33-1+1,34,34
	DrawBlock letters,tin\x*33+1,tin\y*33+1,tin\tilenum+40
Next

Select showpanel
	Case 0
		Text 33*16,0,"Words played (points):"
		row=1:col=0
		For w.word=Each word
			Color pcol(w\playernum)\r,pcol(w\playernum)\g,pcol(w\playernum)\b
			Text 33*15+5+col*140,row*12+3,w\wordstring+" ("+w\points+")"
			;,w\wordstring+"("+w\startx+","+w\starty+")-("+w\endx+","+w\endy+")"+w\points
			row = row+1:If row>33 Then col=col+1:row=1
		Next
	Case 1	
		Text 33*16,0,"Available Tiles:"
		For t=0 To 26
			kar$=Chr$(t+65):If t=26 Then kar$="Blank"
			Text 33*16+15,t*12+15,kar$+" ("+distribution(t)+")"
		Next
	Case 2
		Text 33*16,0,"Points per Tile:"
		For t=0 To 26
			kar$=Chr$(t+65):If t=26 Then kar$="Blank"
			Text 33*16+15,t*12+15,kar$+" - "+points(t)
		Next
	Case 3
		Text 33*16,0,"Tiles in the Bag:"	
		row=1:col=0
		For t = 0 To 99
			If t> 99-tilesused Then Color 255,255,0 Else Color 255,0,0
			If shuffled(99-t) < 26 Then kar$ = Chr$(shuffled(99-t)+65) Else kar$="?"  
			Text 33*15+25+col*40,row*12+3,kar$
			row = row+1:If row>25 Then col=col+1:row=1
		Next
End Select

End Function





Function ShuffleRack()
;shuffle the tiles in the rack
For t=0 To 100
	f= Rnd(0,11)
	s= Rnd(0,11)
	temp = rack(f)
	rack(f) = rack(s)
	rack(s) = temp	
Next
End Function



Function ShuffleTiles()
;shuffle the  full bag of tiles
For t=0 To 1000
	f= Rnd(0,100)
	s=Rnd(0,100)
	temp = shuffled(f)
	shuffled(f) = shuffled(s)
	shuffled(s) = temp	
Next
End Function






Function ClearBoard()
For t = 0 To 14
	For s= 0 To 14
		boardt(t,s)=-1
	Next
Next
End Function



Function ClearRack()
For t = 0 To 11
	rack(t) = -1
	exrack(t) = -1
Next
End Function



Function ClearWordList()
For w.word=Each word
	Delete w
Next
End Function



Function ClearTiles()
For tin.tiletype=Each tiletype
	Delete tin
Next
End Function





Function DoIntro()

Delay 100

done = False

While Not done 
	Restore introletters
	For x = 0 To 14
		For y = 0 To 14
			boardt(x,y) = -1
		Next
	Next
	intletter$ = "a"
	flash = False
	While intletter$ <> "0" And (Not done)
		Cls
		For x = 0 To 14
			For y = 0 To 14
				If boardt(x,y) >= 0
					DrawBlock letters,x*33+140,y*33+140,boardt(x,y)
				EndIf
			Next
		Next
		Read intletter$,x,y
		If intletter$ <> "0"
			boardt(x,y) = Asc(intletter$)-65
		EndIf
		If flash Then Color 255,0,0 Else Color 255,255,255
		Text 250,490,"Press ESC to Play"
		flash = Not flash
		Flip
;		If KeyHit(88) SaveBuffer(FrontBuffer(),"SNAPHOT"+counter+".bmp")
		If KeyHit(1) Then done = True
		Delay 300
	Wend
	Delay 100
Wend	

End Function


.introletters
Data "S",2,3
Data "C",3,3
Data "R",4,3
Data "A",5,3
Data "B",6,3
Data "B",7,3
Data "L",8,3
Data "E",9,3
Data "E",9,3
;Data "E",9,3
;Data "E",9,3
;Data "E",9,3

Data "B",7,3
Data "Y",7,4
Data "Y",7,4
;Data "Y",7,4
;Data "Y",7,4
;Data "Y",7,4


Data "M",5,2
;Data "A",5,3
Data "R",5,4
Data "K",5,5
Data "K",5,5
;Data "K",5,5
;Data "K",5,5
;Data "K",5,5

Data "I",3,1
Data "N",3,2
;Data "C",3,3
Data "I",3,4
Data "T",3,5
Data "T",3,6
Data "I",3,7
Data "I",3,7
;Data "I",3,7
;Data "I",3,7
;Data "I",3,7


Data "C",9,0
Data "O",9,1
Data "D",9,2
;Data "E",9,3
Data "D",9,4
Data "D",9,4
;Data "D",9,4
;Data "D",9,4
;Data "D",9,4


;Data "I",3,1
Data "N",4,1
Data "N",4,1
;Data "N",4,1
;Data "N",4,1
;Data "N",4,1

Data "B",1,7
Data "L",2,7
;Data "I",3,7
Data "T",4,7
Data "Z",5,7
Data "B",6,7
Data "A",7,7
Data "S",8,7
Data "I",9,7
Data "C",10,7
Data "C",10,7
;Data "C",10,7
;Data "C",10,7
;Data "C",10,7

Data "0",0,0