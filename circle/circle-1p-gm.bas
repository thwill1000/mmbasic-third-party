' Circle-One for Picomite LCD 5.07.08
' Copyright (c) 2023 @Volhout
'
' An original game for the PicoMite VGA and GameMite.

'------------------------------ setup PicoMite ---------------------------

  'system setup
  CLS
  set_io_pins
  Const HIGH_SCORE_FILE$ = "A:/high-scores/n_string.txt"
  Const CURRENT_PATH$ = Choice(Mm.Info(Path) <> "", Mm.Info(Path), Cwd$)


'initial values for the players and the target food
'target (green) index 0, player (keyboard - blue) index 1
'right player (AI - red) index 2
  sz=MM.HRes/40

'x,y are coordinates, r=radius, c=color, s=speed
  x0=MM.HRes/2:y0=MM.VRes/3:r0=sz:c0=RGB(green)
  x1=MM.HRes/3:y1=2*MM.VRes/3:r1=sz:c1=RGB(blue):   s1=5  'player speed, tweak
  x2=2*MM.HRes/3:y2=2*MM.VRes/3:r2=sz:c2=RGB(red):  s2=3  'AI speed, tweak
  cw=RGB(white)

'intro and start score reset
  introscreen
  u1=0:u2=0  'scores player and AI

'the game uses framebuffer to prevent screen drawing artefacts
  FRAMEBUFFER create
  FRAMEBUFFER write f

'initial target
  food
  counter=0

'game music
  play modfile CURRENT_PATH$+"circle.mod"


'---------------------- this is the main game loop --------------------------
'the game stops when one player size exceeds the screen limits

  Do

'read keyboard
    t = Timer + 30
    Do While Timer < t
      'scan keys
      if pin(gp8)=0 then p1=4   'down
      if pin(gp9)=0 then p1=2   'left
      if pin(gp10)=0 then p1=8  'up
      if pin(gp11)=0 then p1=1  'right
      if pin(gp15)=0 then s1=12 'turbo run, tweak for fun
    Loop
    if s1>5 then s1=s1-1

'read AI
    moveAI

'wipe old positions players
    eraseplayers

'move players
    v1=0:v2=0:dx1=0:dx2=0:dy1=0:dy2=0
    If (p1 And 2) Then Inc v1,1:Inc x1,-s1:dx1=-1
    If (p1 And 1) Then Inc v1,1:Inc x1,s1:dx1=1
    If (p1 And 8) Then Inc v1,1:Inc y1,-s1:dy1=-1
    If (p1 And 4) Then Inc v1,1:Inc y1,s1:dy1=1
    If (p2 And 2) Then Inc v2,1:Inc x2,-s2:dx2=-1
    If (p2 And 1) Then Inc v2,1:Inc x2,s2:dx2=1
    If (p2 And 8) Then Inc v2,1:Inc y2,-s2:dy2=-1
    If (p2 And 4) Then Inc v2,1:Inc y2,s2:dy2=1

'allow wrap around
    If x1<0 Then x1=x1+MM.HRes
    If x1>MM.HRes Then x1=x1-MM.HRes
    If y1<0 Then y1=y1+MM.VRes
    If y1>MM.VRes Then y1=y1-MM.VRes


'calculate distances
    d12=Sqr((x1-x2)^2 + (y1-y2)^2)
    d10=Sqr((x1-x0)^2 + (y1-y0)^2)
    d20=Sqr((x0-x2)^2 + (y0-y2)^2)

'game rules, collision between players is punished
'player who moves is culprit
    If d12<(r1+r2) Then
      If v1>0 Then r1=r1/1.5
      If v2>0 Then r2=r2/1.5
      r1=Max(r1,3)
      r2=Max(r2,3)
    EndIf

'you eat, you grow....
    If d10<(r1+r0) Then r1=r1*2:newfood
    If d20<(r0+r2) Then r2=r2*2:newfood

'write new player /target positions and sizes
    drawplayers
    food

'decide winner
    If r1>MM.VRes/2 Then
      Text MM.HRes/2,MM.VRes/2,"Blue Wins","CM",1,1,RGB(yellow)
      u1=u1+1
      prestart
    EndIf
    If r2>MM.VRes/2 Then
      Text MM.HRes/2,MM.VRes/2,"Red Wins","CM",1,1,RGB(yellow)
      u2=u2+1
      prestart
    EndIf

'score update
    Text 0,0,Str$(u1),"LT",1,1,RGB(blue)
    Text MM.HRes,0,Str$(u2),"RT",1,1,RGB(red)

'update screen
    FRAMEBUFFER copy f,n,b
    Pause 50
  Loop Until pin(gp14)=0  'B pressed

  CLS
  Text MM.HRes/2, MM.VRes/2, "Bye","CM",1,1,RGB(yellow)
  FRAMEBUFFER copy f,n,b
  run "a:/GameMite/menu.bas"
End


'------------------------------- subs ------------------------------
sub moveAI
  AIx=int((x0-x2)/2)
  AIy=int((y0-y2)/2)
  p2=0
'decide preferred direction
  if abs(AIx)>=abs(AIy) then
'X is preferred direction
    movAIx
  end if
  if Abs(AIx)<=abs(AIy) then
'Y preferred direction
    movAIy
  end if
end sub

sub movAIx
  if AIx<0 then
    p2=p2+2
  else
    p2=p2+1
  end if
end sub

sub movAIy
  if AIy<0 then
    p2=p2+8
  else
    p2=p2+4
  end if
end sub


'show player info and hold restart until controller key pressed
Sub prestart
  Text MM.HRes/2,30+MM.VRes/2,"A=continue, B=stop","CM",1,1,RGB(yellow)
  FRAMEBUFFER copy f,n,b

  Do
    Pause 50
  Loop while pin(gp14)+pin(gp15)=2

  if pin(gp14)=0 then run "a:/GameMite/menu.bas"

  CLS
'x,y are coordinates, r=radius, c=color, s=speed
'speed is increased every level...so prepare.....
  x0=MM.HRes/2:y0=MM.VRes/3:r0=sz:c0=RGB(green)
  x1=MM.HRes/3:y1=2*MM.VRes/3:r1=sz:c1=RGB(blue):s1=5+(u1+u2)/4
  x2=2*MM.HRes/3:y2=2*MM.VRes/3:r2=sz:c2=RGB(red):s2=1+(u1+u2)/4

'initial target
  food
End Sub


'seed new green circle
Sub newfood
  c=c0:c0=0:food:c0=c 'erase old food
  x0=MM.HRes*Rnd()
  y0=MM.VRes*Rnd()
End Sub


'the introscreen
Sub introscreen
  CLS
  Text MM.HRes/2,MM.VRes/2-50,"CIRCLE ONE","CM",1,2,RGB(yellow)
  Pause 2000
  Text MM.HRes/2,MM.VRes/2,"a single player game","CM",1,1,RGB(yellow)
  Pause 1500
  Text MM.HRes/2,MM.VRes/2+20,"for Game*Mite","CM",1,1,RGB(yellow)
  Pause 1500
  Text MM.HRes/2,MM.VRes/2+40,"Arrow keys to control","CM",1,1,RGB(yellow)
  Pause 1500
  Box 0,MM.VRes/2-10,MM.HRes,MM.VRes,,0,0
  Pause 1500
  Text MM.HRes/2,MM.VRes/2,"A to sprint, B to stop","CM",1,1,RGB(yellow)
  Pause 1500
  Text MM.HRes/2,MM.VRes/2+20,"eat apples to grow and win","CM",1,1,RGB(yellow)
  Pause 1500
  Text MM.HRes/2,MM.VRes/2+40,"avoid collisions !!","CM",1,1,RGB(yellow)
  Pause 1500
  Text MM.HRes/2,MM.VRes/2+60,"--- GET READY ---","CM",1,1,RGB(yellow)
  Pause 2000
End Sub

Sub food
  Circle x0-4,y0-2,r0,,,c0,c0
  Circle x0+4,y0,r0,,,c0,c0
  Line x0-3,y0,x0+5,y0-2*r0,1,c0
End Sub

Sub eraseplayers
  Circle x1,y1,r1+10,,,0,0
  Circle x2,y2,r2+10,,,0,0
End Sub

Sub drawplayers
  Inc counter,1
'if r1>50 then c1=rgb(cyan) else c1=rgb(blue)
'draw player
  If v1>0 Then
'draw player 1 body
    Circle x1,y1,r1,,,c1,c1
'eyes when moving
    v=0.7+(v1=1)*0.3 'sqrt 2 if 45 degrees
    dy=6*dy1:dx=6*dx1
    Circle x1+v*((dx1*r1)-dy),y1+v*((dy1*r1)+dx),5,,,cw,cw
    Circle x1+v*((dx1*r1)+dy),y1+v*((dy1*r1)-dx),5,,,cw,cw
    Circle x1+v*((dx1*(r1+2)-dy)),y1+v*((dy1*(r1+2))+dx),2,,,9,9
    Circle x1+v*((dx1*(r1+2)+dy)),y1+v*((dy1*(r1+2))-dx),2,,,0,0
  Else
'draw player 1 body
    Circle x1,y1,r1,,,c1,c1
'not moving, eyes sleepy
    Circle x1+6,y1+2,5,,,cw,cw
    Circle x1-6,y1+2,5,,,cw,cw
    Circle x1+6,y1-1,5,,,c1,c1
    If counter And 28 Then
      Circle x1-6,y1+4,2,,,0,0
    Else
      Circle x1-6,y1-1,5,,,c1,c1
    EndIf
  EndIf

'if r2>50 then c1=rgb(cyan) else c1=rgb(blue)
'draw player
  If v2>0 Then
'draw player 1 body
    Circle x2,y2,r2,,,c2,c2
'eyes when moving
    v=0.7+(v2=1)*0.3 'sqrt 2 if 45 degrees
    dy=6*dy2:dx=6*dx2
    Circle x2+v*((dx2*r2)-dy),y2+v*((dy2*r2)+dx),5,,,cw,cw
    Circle x2+v*((dx2*r2)+dy),y2+v*((dy2*r2)-dx),5,,,cw,cw
    Circle x2+v*((dx2*(r2+2)-dy)),y2+v*((dy2*(r2+2))+dx),2,,,9,9
    Circle x2+v*((dx2*(r2+2)+dy)),y2+v*((dy2*(r2+2))-dx),2,,,0,0
  Else
'draw player 1 body
    Circle x2,y2,r2,,,c2,c2
'not moving, eyes sleepy
    Circle x2+6,y2+2,5,,,cw,cw
    Circle x2-6,y2+2,5,,,cw,cw
    Circle x2+6,y2-1,5,,,c2,c2
    If counter+14 And 30 Then
      Circle x2-6,y2-1,5,,,c2,c2
    Else
      Circle x2-6,y2+4,2,,,0,0
    EndIf
  EndIf
End Sub

'select IO direction for key input pins
sub set_io_pins
  setpin gp8,din,pullup  'down
  setpin gp9,din,pullup  'left
  setpin gp10,din,pullup 'up
  setpin gp11,din,pullup 'right
  setpin gp12,din,pullup 'select
  setpin gp13,din,pullup 'start
  setpin gp14,din,pullup 'B
  setpin gp15,din,pullup 'A
end sub
