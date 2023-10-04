  ' Snake for Picomite LCD 5.07.08
  ' By @Volhout 2023
  ' Play snake game using cursor keys.
  ' Avoid eating scrub and yourself.
  ' Eat all fruit to grow.

  ' Versions:
  '   s1 = basic ASCII text snake
  '   s2 = using sprites for snake
  '   s3 = adding fruit and tone
  '   s4/s4_1 = 8 sprites
  '   s5/s5_1 = dynamic game experience, music
  '   s6/s6_1 = highscore + compressed music
  '   s7/S7_1 = add sprites to bas file, add intro
  '   snake8 = release for VGA
  '   s8p = release for LCD picomite (ILI9341 320x240)
  '   s8p1 = bug fix food eaten, for V50708b12 or later (uses blit mirror)
  '   s9p = game*mite version test release
  '   s10/s10a = adaptations by Tom

  Const HIGH_SCORE_FILE$ = "A:/high-scores/n_string.txt"
  Const CURRENT_PATH$ = Choice(Mm.Info(Path) <> "", Mm.Info(Path), Cwd$)

  'init arrays and restore previous highscore list
  z=255:Dim s(z,1),n$(9)
  If Not Mm.Info(Exists Dir "A:/high-scores") Then MkDir "A:/high-scores"
  if dir$(HIGH_SCORE_FILE$,file)<>"" then var_restore
  set_io_pins

  'start graphics and sound
  '  MODE 2
  SetTick 120,music

  'variables used, and default values
  ' s(x,y)=head coord, (v,w)=head motion, tail erase unless food
  ' h=head poiter, t=tail pointer, l=length snake = level
  ' z=max length snake, p=pixel ahead, n$()=highscore list
  ' c=food delay, (i,j)=food coordiates, u=field color dark green
  ' d=tick counter music, e=music pointer, m$=music, f=tone
  ' a$=input, b=key
  h=0:t=0:l=2:d=0:b=131:x=120:y=x:s(h,0)=x:s(h,1)=y:c=79:e=1:u=16384
  v=1:w=0

  'compressed sound score containing bass, metrum, and melody
  m$="QQE\STSE\Q\SOC\X\SC\\\QMA\STSA\Q\QL@\P\Q@\S\QQE\STSE\Q\SOC\X\SC\\\TMA\VXVA\T\VO@\[\XC\\\"

  'create sprite file in filesystem and load 10 sprites
  Blit load 1,CURRENT_PATH$ + "1.bmp",0,0,8,8
  Blit load 2,CURRENT_PATH$ + "2.bmp",0,0,8,8
  Blit load 3,CURRENT_PATH$ + "3.bmp",0,0,8,8
  Blit load 4,CURRENT_PATH$ + "4.bmp",0,0,8,8
  Blit load 5,CURRENT_PATH$ + "5.bmp",0,0,8,8
  Blit load 6,CURRENT_PATH$ + "6.bmp",0,0,8,8
  Blit load 7,CURRENT_PATH$ + "7.bmp",0,0,8,8
  Blit load 8,CURRENT_PATH$ + "8.bmp",0,0,8,8

  'intro screen
  CLS u                              'empty grass field

  'game title
  j=3*Pi/2
  For i=0 To 3*Pi Step Pi/16        'print S as a series of snake sprites
    Blit write 1,80-40*Cos(i),(1+(i>j))*80-40*Sin(i+Pi*(i>j))
    Pause 100
  Next
  Blit write 3,120,160              'append head
  Text 140,40,"nake",,,5,RGB(brown),u

  'legend and enter player name
  For i=4 To 7
    Blit write i,120+i*12,110
  Next
  Blit write 1,168,130:Blit write 8,180,130
  Text 220,110,"good food",,,,,u
  Text 220,130,"death",,,,,u
  Text 168,160,"use cursor keys",,,,,u
  Text 168,175,"and START",,,,,u
  Text 168,200,"enter name  .....",,,,,u
  n$(0)=""

  'enter text player name, save and add to highscore list
  get_name
  var save name$
  n$(0)=name$

  'playfield
  For i=0 To 39                       'bushes around field
    For j=0 To 30
      Blit write 8,8*i,8*j
  Next :Next
  Box 8,8,304,224,1,u,u               'dark green ground

  'main game loop
  Do
    t = Timer + Max(180-l,80)
    Do While Timer < t
      'scan keys
      if pin(gp8)=0 then v=0:w=1
      if pin(gp9)=0 then v=-1:w=0
      if pin(gp10)=0 then v=0:w=-1
      if pin(gp11)=0 then v=1:w=0
    Loop

    'for game debug, use PC connection
    a$=Inkey$:b=Asc(a$)               'detect arrow keys
    If b=27 Then Save image "game.bmp" 'detect <ESC> to save game
'
    'movement of snake head and tail
    x=x+8*v:y=y+8*w                   'new x,y no protection relies on fence color
    p=Pixel(x+4,y+4)                  'look ahead what is coming
    Text 0,0,Str$(l)                  'show level = length

    'free move on open terrain
    If p=u Then                       'when background then empty
      advancehead                     'make snake move

      'check for end of game by eating yourself or fence
    ElseIf p=64512 Or p=&hFC8000 Then  'if wall or body then die, LCD
                                       'sprite colors are different from VGA
      SetTick 0,0:Play stop
      Text 160,100,"HIGHSCORE",C,,,,u
      hiscore
      Text 160,60,"You died, START=play, SELECT=quit",C,,,,u
      Do:Loop While pin(gp13)+pin(gp12)=2
      If pin(gp12)=0 Then
        run "a:/GameMite/menu.bas"    'END
      Else
        Run                           'very brute game control
      end if

      'we stumbled upon food. Not categorized, just eat and grow
    Else                              'is food
      Box x,y,8,8,1,u,u               'erase food eaten, change to open terrain
      l=Min(l+4,z):c=79               'and (c=79) immediately create new
      advancehead                     'make snake move
    EndIf

    'when food eaten, or time elapsed, generate new food
    Inc c,1                           'new food after 80 moves
    If c=80 Then                      'after 80 snake moves provide new food
      Do                              'find free location
        i=8*Int(1+Rnd()*38):j=8*Int(1+Rnd()*28)
      Loop Until Pixel(i+4,j+4)=u
      c=0:Blit write 4+(l>50)+Int(Rnd()*4),i,j    'OK and plot
    EndIf
  Loop

  'dynamic music play
  'beginning of the game only bass is played
  'when gaining points more voices are added, and volume increases
Sub music
  gennote
  If f<700 And l>50 Then Play sound 1,B,Q,f,l/16   'melody sounds when l>50
  If (d And 3) = 0 Then
    gennote :Play sound 2,B,Q,f,l/16  'metrum increases
  EndIf
  If d=0 Or d=8 Then
    gennote :Play sound 3,B,S,f       'bass always
  EndIf
  d=(d+1) And 15                      'count in 1/16 note
  If e>Len(m$) Then e=1               'wrap score to beginning
End Sub

Sub gennote
  f=110*2^((Asc(Mid$(m$,e,1))-59)/12) 'calc freq
  Inc e,1                             'next note
End Sub

  'manages the highscore list, uses n$(0) for new entry, show n$(1..9)
Sub hiscore
  n$(0)=Right$("00"+Str$(l),3)+"  "+n$(0)  'combine level and name
  Sort n$():var_save                  'sort and save
  Font 7                              'smaller font to fit list LCD only
  For i=1 To 9
    Text 133,200-i*9,n$(i),,,,,u      'show list
  Next i
  Font 1
End Sub

  'write new head in right direction, cut tail
sub advancehead
  Blit write 1,s(h,0),s(h,1)      'write body shape over old head
  h=(h+1) And z:s(h,0)=x:s(h,1)=y 'new head pointer and store x,y
  t=(h-l) And z                   'new tail pointer
  Blit write 2+(v=0),x,y,4+(v=1)+2*(w=1) 'VGA write new head post 50708b12
  Box s(t,0),s(t,1),8,8,1,u,u     'erase tail
end sub

'saves n$() to a file in a:/high-scores
sub var_save
  open HIGH_SCORE_FILE$ for output as #1
  for i=0 to 9:print #1,n$(i):next i
  close #1
end sub

'reads n$() from a file in a:/high-scores
sub var_restore
  open HIGH_SCORE_FILE$ for input as #1
  for i=0 to 9:input #1,n$(i):next i
  close #1
end sub

'use the cursor keys to enter a name
sub get_name
  var restore 'retieves name$
  if name$="" then  'first boot after loading game
    name$="ike.."
  else
    name$=left$(name$+".....",5)
  end if
  posit=1
  do
    Text 264,200,name$,,,,,u
    box 255+posit*8,200,10,10
    pause 200
    if pin(gp8)=0 then mid$(name$,posit,1)=chr$(asc(mid$(name$,posit,1))-1)
    if pin(gp9)=0 then box 255+posit*8,200,10,10,1,0,0:posit=max(1,posit-1)
    if pin(gp10)=0 then mid$(name$,posit,1)=chr$(asc(mid$(name$,posit,1))+1)
    if pin(gp11)=0 then box 255+posit*8,200,10,10,1,0,0:posit=MIN(5,posit+1)
  loop until pin(gp13)=0
end sub

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
