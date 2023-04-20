'Pico Frog
' by Martin Herhaus 2022
' This is a Frogger game which resembles the original Arcade version
' from Konami  (c) released in 1981, but completely rewritten from the
' ground up in MMBasicVGA for the Raspberry Pico
'
' This code has an open source license, you can copy it, change it,
' for any purpose for commercial use or not. But these comments
' should be in you main file.
' The graphics and Music are based upon but not the original
'------
'
' developed on PicoMite Version 5.07.05b14
' would not run in 5.06 or lower due to the use of
' Framebuffer, Layer and Sprites. For this, the Pico also has to run
' at 252000 KHz or higher
'
'Use OPTION CPUSPEED 252000 or OPTION CPUSPEED 378000
'----------History so far-------------------------------------
'todo:
''# Game logic / Levels
'diving turtles
'Gimmics like Fly*, Lady Frog*
'Opponents like Snake and Aligator
'
'* Parts allready Created but no function in the Game jet
'# WIP
'--------------------------------------------
'Changes:
'V0.5.7
' Frog dies when
'  time is up
'  touching cars or water
'V0.5.5
' Added Konami-Style Font
' Added Scoringsystem:
' Every forward step(one lane closer to home) scores 10 points
' Every frog arriving home scores 50 points.
' 10 points are also awarded per each unused 1/2 second of time
' when all 5 frogs reach home to end the level the player earns 1,000 points
' #Guiding a lady frog home or eating a fly scores 200 points each
' added Timer Bar
'V0.5.0
'some cleaning
'collition detection registers, whats under the Frog
'Adding the Intro Screen#
'more Cleaning
'optimised the Compression Routin for higher efectivity
'V0.4.0
'Some Optimisation in Programcode,
'Compressed SpriteData
'added Background
'start working on Collition detection

' V0.3.9
' Frog Home Detection
' Music change on events
' added Frog Home and Fly Sprite
' enhanced Music routine to individual ADSR envelopes per Music-Chanal
' V0.3.5
' Added Sprite-reading for Ladyfrog
' change the Colornubers in Sprites to default values,
'   published by  matherp
'   https://www.thebackshed.com/forum/ViewTopic.php?FID=16&TID=14884
'
'V0.3.2 Added Frog Automovement on Water (Lane 1 to 5)
'V0.3.1
' PicoGAME board NES Controller added by thwill
' more Music
' Player Movement added
' corrected the Lane scrolling (100%)
'v0.2.1
' added Music Tracker routine and first song.
' enable layer For Spritemoving
' Defining the Frog Sprite
'V0.2.0
' added routine to let appear new Sprites on the Lanes
' added the sprites for wood and turtles, corrected the
' Lane scrolling (95%)
'V0.1.9 (Proof of concept)
' using Framebuffer
' changes: Use BILT Command for moving a lanes at once
'Tick4 Sound voice 1-3 for Music (Working)
'Tick1 Sound voice 4 for FX (Working)
'Tick? for scrolling (maybee)
'------------------------------------------------------------------------------
Option BASE 0
Option DEFAULT NONE
Option EXPLICIT ON
Option Break 4: On Key 3, on_break
Dim Col%(15)
Dim FPOSX%,FPOSY%,FPDIR%,FPOSP%,FMOV%,tmp$,f%,RSTRT%,FDEL%,Level%,Homes%,Time%
Dim flx%,fly%,fox%,foy%,cl1%,cl2%,snr%,mt%,FL%,FR%,JP%,n%,Score%,High%,GOV%
Dim Lane$(10) Length 32,PLD$ Length 1,lps%(10),Fmax%,LTime%,Lives%,Tm%,FD%=0
Dim ld%(10)=(0,2,-1,1,2,-2,-2,4,-2,1,-1)
Dim ly%(10)=(0,24,40,56,72,88,120,136,152,168,184)
Dim ADSR%(10)=(25,25,25,24,22,20,18,13,7,3,0)
Dim ADSR2%(10)=(2,8,12,17,22,20,15,10,5,2,0)
Dim FRB%,SND%,LP%,TX%,TY%,TDX%,TDY%,spnr%,Slot%,Freq1%,Freq2%,Nest%(5),str%(20)

' @thwill
' Button values as returned by controller read functions.
Const ctrl.A%      = 1  ' FIRE on 1-button joystick and "B" on SNES.
Const ctrl.B%      = 2  ' "Y" on SNES.
Const ctrl.SELECT% = 4
Const ctrl.START%  = 8
Const ctrl.UP%     = 16
Const ctrl.DOWN%   = 32
Const ctrl.LEFT%   = 64
Const ctrl.RIGHT%  = 128
Const ctrl.C%      = 256 ' "A" on SNES.
Const ctrl.D%      = 512 ' "X" on SENS.
Const ctrl.E%      = 1204 ' Left-Bumper on SNES.
Const ctrl.F%      = 2048 ' Right-Bumper on SNES.

' Pin mappings for ATARI 1-button joystick.
Dim ATARI_FIRE_PIN%(1)  = ( MM.Info(PinNo GP14), MM.Info(PinNo GP28) )
Dim ATARI_UP_PIN%(1)    = ( MM.Info(PinNo  GP0), MM.Info(PinNo GP28) )
Dim ATARI_DOWN_PIN%(1)  = ( MM.Info(PinNo  GP1), MM.Info(PinNo  GP1) )
Dim ATARI_LEFT_PIN%(1)  = ( MM.Info(PinNo  GP2), MM.Info(PinNo  GP2) )
Dim ATARI_RIGHT_PIN%(1) = ( MM.Info(PinNo  GP3), MM.Info(PinNo  GP3) )

' Pin mappings for NES/SNES gamepad.
Dim NES_CLOCK_PIN%(1) = ( MM.Info(PinNo  GP3), MM.Info(PinNo GP22) )
Dim NES_DATA_PIN%(1)  = ( MM.Info(PinNo  GP1), MM.Info(PinNo  GP4) )
Dim NES_LATCH_PIN%(1) = ( MM.Info(PinNo  GP2), MM.Info(PinNo  GP5) )

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const NES_PULSE! = 0.001 ' 1 micro-second

Dim controller$ ' @thwill

' @thwill
' When a key is pressed ctrl.on_key() sets the corresponding byte of this
' 256-byte map to 1. When ctrl.keydown(i%) is called the corresponding
' byte is read and set to 0. Note that a 256-bit map could be used but would
' be slower.
Dim ctrl.key_map%(31)

'--------- prepare the Graphic
High%=2500
Restore colors:For f%=1 To 15:Read Col%(f%):Next f%
JP%=600:FRB%=1:SND%=1:MODE 2:Font 9
 FRAMEBUFFER CREATE
 FRAMEBUFFER LAYER Create
'Prepare the playing field
disp
Box 0,0,224,8,,COL%(2),COL%(2)
Box 0,8,8,16,,COL%(2),COL%(2)
Box 216,8,8,16,,COL%(2),COL%(2)
Box 24,8,32,16,,COL%(2),COL%(2)
Box 72,8,32,16,,COL%(2),COL%(2)
Box 120,8,32,16,,COL%(2),COL%(2)
Box 168,8,32,16,,COL%(2),COL%(2)
Box 0,104,224,16,,COL%(13),COL%(13)
For f%=0 To 218 Step 16
  flowers f%,104,col%(1),Col%(4):flowers f%,200,col%(1),Col%(4)
Next f%
FRAMEBUFFER WRITE L :FRAMEBUFFER COPY N,F,b
CLS
Box 0,0,224,218,,col%(1),col%(1)
For f%=0 To 26:Text 0,f%*8,"000000000000000000000000000000",,,,col%(7),col%(1):Next f%
Box 224,0,96,216,,COL%(7),COL%(1)
Box 226,2,92,212,,COL%(7),COL%(1)
Box 239,6,64,44,,Col%(6),COL%(4)
bing
FRAMEBUFFER WRITE F
read_Sprites
Box 0,0,224,24,,0,0
For f%=0 To 192 Step 8:Sprite write #29,f%,0: Next f%
For f%=0 To 192 Step 48:Box f%,0,32,24,,0,0:Sprite write #28,f%,0: Next f%
start:

 Lane$(01)="0WWWWw000WWWw00000WWWWw000WWw000"
 Lane$(02)="0UU000UUU000UUU000UU00U0000UU000"
 Lane$(03)="00WWWWWw000WWWWWw00WWWWw0000WWw0"
 Lane$(04)="00Ww0Ww000WWw000WWw0000WWWw00WWw"
 Lane$(05)="00UUU00UUU00UUU00UUU000000UUU0U0"
 Lane$(06)="000000Tt00000000Tt00000Tt000Tt00"
 Lane$(07)="000r000000r0000000000r000000r000"
 Lane$(08)="00C00000C00000C000000C0000C00000"
 Lane$(09)="00000000B00000B00000B0000B0000B0"
 Lane$(10)="0000R000000R00000R0000000R000R00"

FRAMEBUFFER WRITE L
Box 0,0,224,240,,0,0
ctrl.init_keys() ' @thwill
startscreen
ctrl.open(controller$) ' @thwill
Score%=0:Level%=1:Lives%=3
ShowLives
Create_street
RSTRT%=0
'Start Music
Restore intro
'V1%=25:V2%=25:
Mt%=1
If SND% Then SetTick 23, music,4
LTime%=Timer:FDEL%=50
restart:
Homes%=0
GOV%=0
'clear nests
FRAMEBUFFER WRITE F
' clear Homeslots
 For n%=1 To 5
  nest%(n%)=0
  Box 8+(n%-1)*48,8,16,16,,col%(1),col%(1)
 Next n%
FRAMEBUFFER WRITE L
'clear the whole Playfeeld on the layer
Box 0,0,220,240,,0,0
Inc Level%
Text 240,80, "SCORE:",,,,Col%(7),Col%(1)
Text 240,104,"HI-SCORE:",,,,Col%(7),Col%(1)
Text 240,136,"LIVES:",,,,Col%(6),Col%(1)
Text 240,172,"TIME:",,,,Col%(6),Col%(1)
write_high
restart1:
Inc Score%,10*(Int((Timer-LTime%)/500))
write_score
If FD%=0 Then Box 0,0,220,240,,0,0
fd%=0
LTime%=Timer
' Do While Inkey$<>"":Loop :'clear Keyboard buffer ' @thwill
FPOSX%=96:FPOSY%=200:FPDIR%=1:FPOSP%=10:FMOV%=0:FMAX%=FPOSY%
'----------gameloop-----------------------------------
LP%=1
TX%=0:TY%=0:TDX%=2:TDY%=2
spnr%=10
Do
   move_lanes
   FRAMEBUFFER COPY F,N
   FRAMEBUFFER WRITE L
   If FDEL%=1 Then

     Box 0,0,220,240,,0,0
     LTime%=Timer:tm%=1
     Sprite write FPOSP%+1,FPOSX%,FPOSY%
     ' Do While Inkey$<>"":Loop ' @thwill
   EndIf
   If FDEL% Then
     Inc FDEL%,-1
   '  if FD%=0 Then  Box 0,0,220,240,,0,0
   Else
     Move_Player
     Inc Tm%,-1:If tm%=0 Then
     Update_Timer 60-Int((Timer-LTime%)/500)
     tm%=20
     EndIf
   EndIf
   If RSTRT%=1 Then  RSTRT%=0:GoTo restart1
   If RSTRT%=2 Then RSTRT%=0: GoTo restart
   If FMOV%=0 Then automove
   Get_frog_Undergrnd
   If FD% Then
    'frog death
     Restore FKill
     JP%=220:SetTick 15,DieSnd,1
     Hide_Frog
     FRAMEBUFFER WRITE L:Sprite write #30,FPOSX%,FPOSY%
     FDEL%=20
     Inc Lives%,-1
     ShowLives
     If Lives%=0 Then GOV%=1:Restore Game_OverSng
     If Not GOV% Then GoTo restart1 Else GameOver
   EndIf
 Loop
Sub GameOver
FRAMEBUFFER WRITE N
Print @(72,108)" GAME OVER! "

Pause 10000
disp
FRAMEBUFFER WRITE F
Box 0,24,224,80,,Col%(1),Col%(1)
Box 0,120,224,80,,0,0

FRAMEBUFFER WRITE L
CLS
Box 0,0,224,120,,Col%(1),Col%(1)
Box 224,0,96,216,,COL%(7),COL%(1)
Box 226,2,92,212,,COL%(7),COL%(1)
Box 239,6,64,44,,Col%(6),COL%(4)
GoTo Start


End Sub
Sub ShowLives
Local F%
FRAMEBUFFER WRITE L
   Box 236,144,80,16,,col%(1),col%(1)
   For f%=1 To Lives%
   Sprite write #11,222+f%*16,144
   Next f%
End Sub

Sub write_score
  Local S$
  s$=Str$(Score%)
   FRAMEBUFFER WRITE L
   Do While Len(s$)<5:s$="0"+s$:Loop
   Text 240,88, S$,,,,Col%(7),Col%(1)
   If Score%>High% Then High%=Score%: write_High
End Sub
Sub write_high
  Local S$
  s$=Str$(High%)
   FRAMEBUFFER WRITE L
  Do While Len(s$)<5:s$="0"+s$:Loop
   Text 240,112, S$,,,,Col%(7),Col%(1)
End Sub
Sub Update_Timer TimeLeft%
  If TimeLeft%>0 Then
  FRAMEBUFFER WRITE L
  Box 240+TimeLeft%,180,60-TimeLeft%,10,,Col%(1),Col%(1)
  Box 240,180,TimeLeft%,10,,,Col%(2+2*(TimeLeft%<20))
  Box 240,180,60,10,,col%(7)
  Else
  FD%=1
  EndIf
End Sub

Sub startscreen
 Local title$,tk%,tc%,x%,y%,f%,bit%
'  Do While Inkey$<>"":Loop ' @thwill
  FRAMEBUFFER write N
  CLS
  For x%=200 To 16 Step -8:For y%=16 To 184 Step 16:Sprite write #29,x%,y%
  Next y%: Next x%
  Restore Logo
  For y%=1 To 17:Read str%(y%):Next
  For f%=0 To 218 Step 16
  flowers f%,0,col%(1),Col%(4):flowers f%,200,col%(1),Col%(4)
  Next
  bit%=4
  Box 16,16,200,6,,0,0
  Box 204,16,8,136,,0,0
  Box 16,16,12,136,,0,0
  Box 16,124,200,64,,0,0
  For x%=30 To 1 Step -1
  For y%=1 To 17
   If Not (str%(y%) And bit%) Then
    Box 20+x%*6,16+y%*6,6,6,,0,0
   Else
    FRAMEBUFFER write L
      Box 240+x%*2,10+y%*2,2,2,,col%(6),Col%(6)
    FRAMEBUFFER write N
   EndIf
  Next
  Inc bit%,bit%:Pause 20
  Next
  FRAMEBUFFER write L
  For y%=16 To 200 Step 16
   flowers 0,y%,col%(1),Col%(4):flowers 208,y%,col%(1),Col%(4)
  Next
  FRAMEBUFFER write N
  Colour col%(7):'Font 7
  Print @(16,140)"    PicoMite Version    "; ' @thwill
  Print @(16,148)" by Martin Herhaus 2022 "; ' @thwill
  Print @(16,156)"  PicoGAME VGA Support  "; ' @thwill
  Print @(16,164)"   by Thomas Williams   "; ' @thwill
  Print @(16,184)"Press Fire,'A' or Space "; ' @thwill
  Title$=" Game based on the Arcade Game Frogger - copyright Konami 1981  - "
  Title$=Title$+"The Graphics and Music are based upon but not the original ***"
  tc%=0: tk%=7
  controller$ = "" ' @thwill
  Do While controller$ = "" ' @thwill
    controller$ = ctrl.poll$(1) ' @thwill
    If TK%=7 Then
      Colour col%(6):Print @(14,176) Left$(title$,34)
      Title$=Right$(title$,Len(title$)-1)+Left$(title$,1)
      Colour Col%(tc%):Inc tc%:TC%=TC% And 15
      Print @(16,184)"Press Fire,'A' or Space";
    Else
      Sprite 17,176,16,176,197,8
      Box 207,168,1,8,,0,0
    EndIf
    Inc tk%,-1:If tk%=0 Then TK%=7
    Pause 15
   Loop
   bing: Pause 500:start_sound
   FRAMEBUFFER WRITE L
   'clear the Game layer
   Box 0,0,224,240,,0,0
End Sub

Sub Get_frog_Undergrnd
'Guess what is under the Frog
  If FMOV%>=2 Then Exit Sub
  Local LPX%,fu$
  LP%=Int(FPOSY% /16)
  If LP%=6 Then Exit Sub
  If LP%>6 Then Inc LP%,-1
  If LP%<11 Then
   If ld%(LP%) > 0 Then
     LPX%=(FPOSX%-Lps%(LP%)-4)/16+2
   Else
     LPX%=(FPOSX%+Lps%(LP%)-8)/16+1
   EndIf
   FU$=Mid$(lane$(LP%),LPx%,1)
   If LP%>5  Then
    If FU$<>"0" Then FD%=1
   Else
    If FU$="0" Then FD%=1
   EndIf
 EndIf
End Sub

Sub automove
 LP%=Int(FPOSY% /16)
 If LP%<6 Then
   FRAMEBUFFER WRITE L
   Box FPOSX%,FPOSY%,14,16,,0,0
   Inc FPOSX%,ld%(LP%)
   If FPOSX%>200 Then FPOSX%=200
   If FPOSX%<0 Then FPOSX%=0
   Sprite write FPOSP%+1,FPOSX%,FPOSY%
 EndIf
End Sub

Sub Hide_Frog
  FRAMEBUFFER WRITE L
  Box FPOSX%,FPOSY%,16,16,,0,0
  FRAMEBUFFER WRITE F
End Sub

Sub Frog
  FRAMEBUFFER WRITE L
    Sprite write FPOSP%+1,FPOSX%,FPOSY%
  FRAMEBUFFER WRITE F
End Sub

Sub test_home
slot% =0
'Print @(240,4);FPOSX%;"  ";
Select Case FPOSx%
   Case 4 To 20
     slot% =1
   Case 52 To 68
     slot% =2
   Case 102 To 128
     slot% =3
   Case 148 To 164
     slot% =4
   Case 196 To 212
     slot% = 5
End Select
If slot%=0 Then Exit Sub
If Nest%(slot%) Then slot%=0:Exit Sub
Nest%(slot%)=1
Inc Score%,50
write_score
Hide_Frog
FDEL%=70
Inc Homes%
'draw HomeFrog in the Slot
FRAMEBUFFER WRITE F
Sprite write #26,8+(slot%-1)*48,8
'Last_Slot?
If Homes%=5 Then
  Homes%=0:Inc Level%
  Restore complete
  FDEL%=150
  RSTRT%=2
  Inc score%,1000
Exit Sub
EndIf
 Select Case homes%
 Case 1:Restore Yankee:Case 2:Restore Mus2
 Case 3:Restore Mus3:Case 4:Restore home1
 End Select
 RSTRT%=1
End Sub

Sub Move_Player
  'allready in Motion?, then give time to show animated Sprite
  If FMOV%>=2 Then Inc FMOV%,-1:Exit Sub
    If FMOV%=1 Then
      Hide_Frog
      FRAMEBUFFER WRITE L
      If FPDIR%=1 Then
        Inc FPOSY%,-8
        If FPOSY%< FMAX% Then Inc Score%,10:FMAX%=FPOSY%:write_score
        GoTo fmout
       EndIf
      If FPDIR%=2 Then
        Inc FPOSY%,8
        GoTo fmout
       EndIf
      If FPDIR%=3 Then
        Inc FPOSX%,-8
        GoTo fmout
      EndIf
      If FPDIR%=4 Then
        Inc FPOSX%,8
        If FPOSX%>200 Then FPOSX%=200
      EndIf
      FMout:
      Sprite write FPOSP%+1,FPOSX%,FPOSY%
      FMOV%=0
      Exit Sub
      FRAMEBUFFER write f
    EndIf

  ' Read selected controller/keyboard
  Select Case Call(controller$) ' @thwill
    Case 16 : PLD$="U"
    Case 32 : PLD$="D"
    Case 64 : PLD$="L"
    Case 128 : PLD$="R"
    Case Else : PLD$ = ""
  End Select

  'UP
   Select Case PLD$
      Case "U"
        If FPOSY%>16 Then
          If FPOSY%-8 <32 Then
            test_home
            If slot%=0 Then Exit Sub
          EndIf
          FPDIR%=1:FPOSP%=10:FMOV%=4
          Fjump 0,-8
        EndIf
  'down
     Case "D"
       If PLD$="D" And FPOSY%<200  Then
         FPDIR%=2:FPOSP%=12:FMOV%=4
         Fjump 0,8
       EndIf
  'left
     Case "L"
      If FPOSX%>8 Then
        FPDIR%=3:FPOSP%=14:FMOV%=4
        FJump -8,0
      EndIf
  'right
     Case "R"
       If FPOSX%<208 Then
         FPDIR%=4:FPOSP%=16:FMOV%=4
         Fjump 8,0
       EndIf
  End Select
End Sub

Sub FJump(FDX%,FDY%)
    SetTick 20,jump,1
    FRAMEBUFFER WRITE l
    Box FPOSX%,FPOSY%,16,16,,0,0
    Inc FPOSX%,FDX%
    Inc FPOSY%,FDY%
    Sprite write FPOSP%,FPOSX%,FPOSY%
End Sub


Sub move_lanes
FRAMEBUFFER WRITE F
Local f As integer,co%,tst$
'Get a Lane, move it ld%() Pixels to
' Right if Positiv
' Left if negativ
' and fill the gap with Color co%
  For f=1 To 10
    co%=(f<6)
    If ld%(f)>0 Then
     Sprite 0,ly%(f),ld%(f),ly%(f),224-ld%(f),16
     Box 0,ly%(f),LD%(f),16,,col%(co%),col%(co%)
     Inc Lps%(f),LD%(f)
     ' if Pixelsteps>= 16 Rotate the Lane$() Right
    If Lps%(f)>=16 Then
       Lane$(f)=Right$(Lane$(f),1)+Left$(Lane$(f),Len(Lane$(f))-1)
       Lps%(f)=0
     EndIf
     tst$=Left$(Lane$(f),1)
     If tst$<>"0" Then
        snr%=Instr("BrCRTtwWU",tst$)
        'Add2Lane snr%,-14,ly%(f),Lps%(f)
        Sprite write snr%,-12+Lps%(f),ly%(f)
     EndIf
    Else
      Sprite -ld%(f),ly%(f),0,ly%(f),222,16
      Box 223+ld%(f),ly%(f),-ld%(f),16,,col%(co%),col%(co%)
      Inc Lps%(f),-LD%(F)
      ' if Pixelsteps>= 16 Rotate the Lane$() Left
       If Lps%(f)>=16 Then
        Lane$(f)=Right$(Lane$(f),Len(Lane$(f))-1)+Left$(Lane$(f),1)
        Lps%(f)=0
      EndIf
      tst$=Mid$(Lane$(f),14,1)
      If tst$<>"0" Then
        'add Sprite to the lane
         snr%=Instr("BrCRTtwWU",tst$)
         Box 222-Lps%(f),ly%(f),18,16,,col%(co%),col%(co%)
         Sprite write snr%,220-Lps%(f),ly%(f)
      EndIf
    EndIf
   Next f
End Sub
Sub Add2Lane SNo%,Posx%,Posy%,Offs%
  FRAMEBUFFER WRITE F
  Sprite write #SNo%,Posx%+offs%,Posy%
End Sub

'some Decoration
Sub flowers flx%,fly%,cl1%,cl2%
  Box flx%,fly%,16,16,,COL%(13),COL%(13)
  flower flx%+1,fly%+3,cl1%,cl2%:flower flx%+0,fly%+10,cl1%,cl2%
  flower flx%+2,fly%+7,cl1%,cl2%:flower flx%+6,fly%+3,cl1%,cl2%
  flower flx%+6,fly%+8,cl1%,cl2%:flower flx%+6,fly%+8,cl1%,cl2%
  flower flx%+7,fly%+12,cl1%,cl2%:flower flx%+9,fly%+6,cl1%,cl2%
  flower flx%+12,fly%+3,cl1%,cl2%
End Sub

Sub Flower fox%,foy%,cl1%,cl2%
  Line fox%,foy%,flx%+fox%+2,fly%+foy%,,cl1%
  Line fox%+1,foy%-1,flx%+fox%+1,fly%+foy%+1,,cl1%
  Pixel fox%+1,foy%,cl2%
End Sub

'Street "BrCRTtwWU"
Sub Create_street
Local LNE%,lx%,tst$
FRAMEBUFFER WRITE F
For LNE%=1 To 10
   For lx%=1 To 13
   tst$=Mid$(Lane$(LNE%),lx%,1)
   snr%=Instr("BrCRTtwWU",tst$)
   If snr%<>0 Then Sprite write #snr%,16*lx%,ly%(LNE%)
    Next lx%
Next LNE%
End Sub

Sub Disp
CLS
 Box 0,0,320,112,,RGB(blue),RGB(blue)
Box 0,0,224,8,,COL%(2),COL%(2)
Box 0,8,8,16,,COL%(2),COL%(2)
Box 216,8,8,16,,COL%(2),COL%(2)
Box 24,8,32,16,,COL%(2),COL%(2)
Box 72,8,32,16,,COL%(2),COL%(2)
Box 120,8,32,16,,COL%(2),COL%(2)
Box 168,8,32,16,,COL%(2),COL%(2)
Box 0,104,224,16,,COL%(13),COL%(13)
For f%=0 To 218 Step 16
  flowers f%,104,col%(1),Col%(4):flowers f%,200,col%(1),Col%(4)
Next f%

End Sub

Sub read_Sprites
  Local nr%,p%,n%,byt$,m$
  '--- Read/Create Sprites
  Restore Bull
  'because the Cars are symmetric, reading half a Sprite is enough
  For nr%=1 To 6
   For p%=1 To 8
    Read Byt$:Byt$=expand$(Byt$)
    For n%=1 To Len(Byt$)
     m$=Mid$(Byt$,n%,1)
     Pixel n%-1,p%+119,COL%(Val("&H"+m$))
     Pixel n%-1,136-p%,COL%(Val("&H"+m$)))
    Next n%
   Next p%
   Sprite read #nr%,0,120,16,16
  Next nr%
  For nr%=7 To 9
   For p%=1 To 16
    Read Byt$:Byt$=expand$(Byt$)
    For n%=1 To Len(Byt$)
    Pixel n%-1,p%+119,COL%(Val("&H"+Mid$(Byt$,n%,1)))
    Next n%
   Next p%
   Sprite read #nr%,0,120,16,16
  Next nr%
  'Frog (sprite 10 - 17)
  'Read the Frog-Sprites and rotate in all Directions ;-)
  For p%=1 To 32
   Read Byt$:Byt$=expand$(Byt$)
    For n%=1 To Len(Byt$)
     m$=Mid$(Byt$,n%,1)
     Pixel n%-1,p%+119,COL%(Val("&H"+m$))
     Pixel 16+n%-1,152-p%,COL%(Val("&H"+m$))
     Pixel 32+p%-1,n%+119,COL%(Val("&H"+m$))
     Pixel 64-p%-1,n%+135,COL%(Val("&H"+m$))
    Next n%
   Next p%
  Sprite read 10,0,120,16,16:Sprite read 11,0,136,16,16
  Sprite read 12,16,136,16,16:Sprite read 13,16,120,16,16
  Sprite read 14,32,120,16,16:Sprite read 15,48,120,16,16
  Sprite read 16,48,136,16,16:Sprite read 17,32,136,16,16
  Box 0,120,96,32,,0,0
  'Lady Frog (sprite 18 - 25) same template but swapped colors
  Restore FROG11
  For p%=1 To 32
   Read Byt$:Byt$=expand$(Byt$)
   'swap colors Green>Cyan, Yello > Magena, Red > Green
   For n%=1 To Len(Byt$)
    m$=Mid$(Byt$,n%,1)
    If m$="A" Then m$="3"
    If m$="6" Then m$="5"
    If m$="4" Then m$="2"
    Pixel n%-1,p%+119,COL%(Val("&H"+m$))
    Pixel 16+n%-1,152-p%,COL%(Val("&H"+m$))
    Pixel 32+p%-1,n%+119,COL%(Val("&H"+m$))
    Pixel 64-p%-1,n%+135,COL%(Val("&H"+m$))
   Next n%
  Next p%
  Sprite read 18,0,120,16,16:Sprite read 19,0,136,16,16
  Sprite read 20,16,136,16,16:Sprite read 21,16,120,16,16
  Sprite read 22,32,120,16,16:Sprite read 23,48,120,16,16
  Sprite read 24,48,136,16,16:Sprite read 25,32,136,16,16
  Box 0,120,96,32,,0,0
  ' Frog_Home 26, Fly 27
  For p%=1 To 32
   Read Byt$:Byt$=expand$(Byt$)
   For n%=1 To Len(Byt$)
   Pixel n%-1,p%+119,COL%(Val("&H"+Mid$(Byt$,n%,1)))
   Next n%
  Next p%
  Sprite read #26,0,120,16,16
  Sprite read #27,0,136,16,16
  Box 0,120,96,32,,0,0
  For p%=1 To 24
   Read Byt$:Byt$=expand$(Byt$)
   For n%=1 To Len(Byt$)
   Pixel n%-1,p%+119,COL%(Val("&H"+Mid$(Byt$,n%,1)))
   Next n%
  Next p%
  Sprite read #28,0,120,32,24
  Box 0,120,32,32,,0,0
  For p%=1 To 24
   Read Byt$:Byt$=expand$(Byt$)
   For n%=1 To Len(Byt$)
   Pixel n%-1,p%+119,COL%(Val("&H"+Mid$(Byt$,n%,1)))
   Next n%
  Next p%
  Sprite read #29,0,120,8,24
  Box 0,120,32,32,,0,0
  For p%=1 To 16
   Read Byt$:Byt$=expand$(Byt$)
   For n%=1 To Len(Byt$)
   Pixel n%-1,p%+119,COL%(Val("&H"+Mid$(Byt$,n%,1)))
   Next n%
  Next p%
  Sprite read #30,0,120,16,16
  Box 0,120,32,32,,0,0


End Sub

'----------Audio subs--------
Sub bing
Local vol%
  For Vol%=25 To 0 Step -1
    Play sound 4,"B","S",2700,Vol%
    Pause 20
  Next vol%
  Play stop
End Sub


Sub start_sound
Local n%,f%
  For n%=1 To 8:For f%=150+n%*50 To 450+n%*50 Step 20
     Play sound 4,"B","Q",f%,25:Pause 6
     Next f%:Next n%
Play sound 4,"B","Q",1,0
End Sub

Sub DieSnd
  Play sound 4,"B","Q",JP%,25
  Inc JP%,-5
  If JP%<65 Then Play sound 4,"B","O",JP%,0: SetTick 0,0,1:JP%=600
End Sub


Sub jump
  Play sound 4,"B","Q",JP%,25
  Inc JP%,100
  If JP%>1500 Then Play sound 4,"B","O",JP%,0: SetTick 0,0,1:JP%=600
End Sub

Sub music
Local Fcent%
  If mt%=1 Then
    Read Freq1%,Freq2%
    If Freq1%=99999 Then Restore Main_song:Read Freq1%,Freq2%
    If Freq1%=88888 Then SetTick 0,0,4:Play Stop:Exit Sub

    'Pseudo Chorus
    Fcent%=Int(Freq1%/100)
    FL%=Freq1% - Fcent%
    FR%=Freq1% + Fcent%
  EndIf
  If Freq1%>1 Then
    Play sound 1,"L","q",FL%,adsr%(mt%):Play sound 3,"R","q",FR%,adsr%(mt%)
  EndIf
  If Freq2%>1  Then Play sound 2,"B","w",Freq2%,adsr2%(mt%)
  Inc mt%:If mt%>10 Then mt%=1
End Sub

'--- @thwill

' Initialises keyboard reading via ON KEY.
Sub ctrl.init_keys()
  ctrl.term_keys()
  On Key ctrl.on_key()
End Sub

' Terminates keyboard reading via ON_KEY.
Sub ctrl.term_keys()
  On Key 0
  Do While Inkey$ <> "" : Loop
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
End Sub

Sub ctrl.on_key()
  Local ch$ = Inkey$
  If ch$ <> "" Then Poke Var ctrl.key_map%(), Asc(ch$), 1
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
  Poke Var ctrl.key_map%(), i%, 0
End Function

' Calls xxx_open() subroutine for the named controller if it exists.
Sub ctrl.open(ctrl$)
  Local open_fn$ = Choice(Mid$(ctrl$, Len(ctrl$)) = "%", Left$(ctrl$, Len(ctrl$) - 1), ctrl$)
  Cat open_fn$, "_open"
  On Error Skip 1
  Call open_fn$
  If Mm.ErrMsg$ <> "" Then
    If InStr(Mm.ErrMsg$, "Unknown user subroutine") = 0 Then Error Mm.ErrMsg$
  EndIf
End Sub

' Calls xxx_close() subroutine for the named controller if it exists.
Sub ctrl.close(ctrl$)
  Local close_fn$ = Choice(Mid$(ctrl$, Len(ctrl$)) = "%", Left$(ctrl$, Len(ctrl$) - 1), ctrl$)
  Cat close_fn$, "_close"
  On Error Skip 1
  Call close_fn$
  If Mm.ErrMsg$ <> "" Then
    If Mm.ErrMsg$ <> "Unknown user subroutine" Then Error Mm.ErrMsg$
  EndIf
End Sub

' Waits a specified duration for the user to press 'A' on a (S)NES gamepad
' connected to Port A, or FIRE on an ATARI joystick connected to Port A or
' SPACE on the keyboard.
'
' Note that this function does not clear the keyboard buffer before checking
' to see if SPACE has been pressed.
'
' @param   duration%  duration in milliseconds; if 0 then indefinite.
' @return             "nes_a%" if the (S)NES 'A' button is pressed,
'                     or "atari_a%" if the ATARI FIRE button is pressed,
'                     or "keys_cursor%" if the SPACE key is pressed,
'                     or "" if none of the above before the duration expires.
Function ctrl.poll$(duration%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)

  ' Just check the keyboard if we are not on a PicoMite.
  If InStr(Mm.Device$, "PicoMite") = 0 Then
    Do While ctrl.poll$ = "" And Timer < expires%)
      ctrl.poll$ = Choice(keys_cursor%() = ctrl.A%, "keys_cursor%", "")
    Loop
    Exit Function
  EndIf

  Local read_fn$(2) = ("nes_a%", "atari_a%", "keys_cursor%")
  Local expected%(2) = (ctrl.A%, ctrl.A%, ctrl.A%)
  Local i%, t%
  Do
    For i% = 0 To Bound(read_fn$(), 1)
      ctrl.open(read_fn$(i%))
      t% = Min(expires%, Timer + 100)
      ' Try to read the controller multiple times within a 100ms window.
      Do
        If Call(read_fn$(i%)) = expected%(i%) Then
          ctrl.poll$ = read_fn$(i%)
          Exit Function
        EndIf
      Loop Until Timer >= t%
    Next
  Loop Until Timer >= expires%
End Function

' Opens port A connected to an ATARI 1-button joystick.
Sub atari_a_open()
  SetPin ATARI_UP_PIN%(0), DIn
  SetPin ATARI_DOWN_PIN%(0), DIn
  SetPin ATARI_LEFT_PIN%(0), DIn
  SetPin ATARI_RIGHT_PIN%(0), DIn
  SetPin ATARI_FIRE_PIN%(0), DIn
End Sub

' Reads port A connected to an ATARI 1-button joystick.
'
' @return  bitmap corresponding to current controller state:
'            7 6 5 4 3 2 1 0
'            | | | | | | | |
'            | | | | | | | +-- Fire Button
'            | | | | | | +---- unused
'            | | | | | +------ unused
'            | | | | +-------- unused
'            | | | +---------- Up
'            | | +------------ Down
'            | +-------------- Left
'            +---------------- Right
Function atari_a%()
  Inc atari_a%, Not Pin(ATARI_FIRE_PIN%(0))
  Inc atari_a%, Not Pin(ATARI_UP_PIN%(0)) * 16
  Inc atari_a%, Not Pin(ATARI_DOWN_PIN%(0)) * 32
  Inc atari_a%, Not Pin(ATARI_LEFT_PIN%(0)) * 64
  Inc atari_a%, Not Pin(ATARI_RIGHT_PIN%(0)) * 128
End Function

' Opens port A connected to a NES gamepad.
Sub nes_a_open()
  SetPin NES_LATCH_PIN%(0), Dout
  SetPin NES_CLOCK_PIN%(0), Dout
  SetPin NES_DATA_PIN%(0), Din
  Pin(NES_LATCH_PIN%(0)) = 0
  Pin(NES_CLOCK_PIN%(0)) = 0
End Sub

' Reads port A connected to a NES gamepad.
'
' @return  bitmap corresponding to current controller state:
'            7 6 5 4 3 2 1 0
'            | | | | | | | |
'            | | | | | | | +-- Button A
'            | | | | | | +---- Button B
'            | | | | | +------ Select
'            | | | | +-------- Start
'            | | | +---------- Up
'            | | +------------ Down
'            | +-------------- Left
'            +---------------- Right
'
' Note that in theory it should not be necessary to pulse the clock pin after
' reading bit 7 (Right), but in practice there are some clone NES controllers
' which return the wrong value on subsequent reads of bit 7 if it not pulsed.
Function nes_a%()
  Local i%
  Pulse NES_LATCH_PIN%(0), NES_PULSE!
  For i% = 0 To 7
    Inc nes_a%, Not Pin(NES_DATA_PIN%(0)) * 2^i%
    Pulse NES_CLOCK_PIN%(0), NES_PULSE!
  Next
End Function

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
'
' @return  bitmap corresponding to key read:
'            7 6 5 4 3 2 1 0
'            | | | | | | | |
'            | | | | | | | +-- Space
'            | | | | | | +---- unused
'            | | | | | +------ unused
'            | | | | +-------- unused
'            | | | +---------- Up
'            | | +------------ Down
'            | +-------------- Left
'            +---------------- Right
Function keys_cursor%()
  keys_cursor% = keys_cursor% Or ctrl.keydown%(32)  * ctrl.A%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(128) * ctrl.UP%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(129) * ctrl.DOWN%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(130) * ctrl.LEFT%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(131) * ctrl.RIGHT%
End Function

Function expand$(pxl$)
  Local n%,nmb%,tmp$,co$
  For n%=1 To Len(pxl$)
    If Asc(Mid$(pxl$,n%,1))< 71 Then
      tmp$=tmp$+Mid$(pxl$,n%,1)
    Else
      co$=Hex$(Asc(Mid$(pxl$,n%,1))-71)
      Inc n%:nmb%=Val("&H"+Mid$(pxl$,n%,1)):tmp$=tmp$+String$(nmb%+1,co$)
    EndIf
  Next n%
  expand$=tmp$
End Function

' Interrupt routine to stop music, close Buffers and restore default Break Key.
Sub on_break
 Play Stop
 Sprite CLOSE all
 FRAMEBUFFER CLOSE
 ctrl.term_keys() ' @thwill
 If controller$ <> "" Then ctrl.close(controller$) '@thwill
 CLS
 Option Break 3
 End
End Sub
'
'
'----------Data Section----------
' I try to store everything in this File to make it possible to run without an SD Card
'
colors:
'--Colorscheme accordung to matherp
Data RGB(BLUE),RGB(GREEN),RGB(CYAN),RGB(RED)
Data RGB(MAGENTA),RGB(YELLOW),RGB(WHITE),RGB(MYRTLE)
Data RGB(COBALT) ,RGB(MIDGREEN),RGB(CERULEAN),RGB(RUST)
Data RGB(FUCHSIA),RGB(BROWN),RGB(LILAC)

'Sprites
'repainted the sprites mostly from this source
'https://www.spriters-resource.com/resources/sheets/11/11067.png
'
'Compression of repeating Pixels:
'if a Pixel repeats more than two times, Colors replaced
'to Color +16  from '0123456789ABCDEF to GHIJKLMNOPQRSTUV
'followed by the number (single Hex value) of repeats-1
Bull:
'Bulldozer 1/2
Data "GF","GF","04747474G3N20","04747474G37400","002G2200I2N20","00N7007400"
Data "0N4227200N20","077I27277007400"
'Racer1  1/2
Data "GF","0K4G9","0K4G2K400","0K4G2K400","G2DG6DG3","00MB00"
Data "T566T2M30","064646466T2M3"
'car  1/2
Data "GF","GF","GF","G266G566G2","03T500T300","3T3J2D3T330"
Data "36DD33T3J2D00","3T233T3J2D00"
'Racer4  1/2
Data "GF","G9K40","00K4G2K40","00K4G2K40","G32G62G2","00NC0"
Data "0N3I277I5","N3I2774047400"
'Truck  1/2
Data "GF","GF","GF","G4I200I300","G24N30N6","004N40N6","004N44N6"
Data "004N44N6","GF","GF","GF","G8I2G3","NCG2","NCG2","NCG2","NCG2"
'Wood1
Data "HF","HF","HF","U41U4N211","U67EEN41","U97U271","87U77U277"
Data "U87U47","U87EE7EE7","U4EEN2E7E77","EES77S271","S97CC771"
Data "S61S287711","HF","HF","HF"
'Wood2
Data "HF","HF","HF","11U61U31E","U57U47EEE","EE77UB","UEE","U977U3"
Data "E77U37U57E","U67U7","U61C7S5","11C1SB","1EEECCC1S61","HF","HF","HF"
'Turtle 1
Data "HF","HF","H22H62H3","11I2H4I2H2","H32K42H4","H3K7H4","172K81121"
Data "I2K9211","172K674H3","H347K274H4","H324N243H4","11I2H4I2H2"
Data "H22H62H3","HF","HF","HF"
FROG11:
'Frog  Hop
Data "GF","GF","G2AG7AG2","00AA006A6600AA00","G2A04A66A40AG2","G2A0AA66AA0AG2"
Data "G3AM5AG3","G46AM3G4","G46AM3G4","G4A6A66AG4","G3Q266Q2G3","G2AAG5AAG2"
Data "00AAG7AA00","00AAG7AA00","G2AG7AG2","GF"
'Frog Sit
Data "GF","GF","GF","G2A006A6600AG2","00AA04A66A40AA00","G2A0AA66AA0AG2"
Data "G2AAM5AAG2","G46AM3G4","G2AA6AM3AAG2","G2A0A6A66A0AG2"
Data "00AA00A66A00AA00","G2AG7AG2","GF","GF","GF","GF"
Fhome:
'Frog Home Sprite
Data "H2AAH5AAH2","11A11AH3A11A11","11ACCQ5CCA11","H2Q9H2","H4Q5H4"
Data "H3AACAACAAH3","1A11AAACCAAA11A1","AAA13Q531AAA","Q33Q53Q3","Q333Q333Q3"
Data "Q4J5Q4","1Q33CCC33Q31","1Q33CCC33Q31","H2AAAJ3AAAH2","11Q3H3Q311"
Data "1AAA1AH3A1AAA1"
'Fly
Data "HF","HF","HF","H6J4H3","H5J4H4","H2CC1J3CCH3"""
Data "H2CAAJ2S3H2","11AAAS7311","H2CAAJ2S3H2","H2CC1J3CCH3","H5J4H4"
Data "H6J4H3","HF","HF","HF","HF"
Home_slot:
Data "0I50I20I60I60I20","I20EI70EEI40EEI4022","20220EI20EE220220EE220220EE2202"
Data "I90I60I60I4","020EEIEI70EE2","I2022U220U220E20U220EEI2022"
Data "0I3EE1E1EE1E1EE1EE1E1E1EEI30","I4EHEH4EI4","I40EHF11EEI4"
Data "0EEI2HEH4220E22","20I2EEHF11E0220E0","I4EHEH4EI4","20EE2EHEH4EI4"
Data "220220EHF11EE22U2","I5HEH4I304","0I3EEHF11E0I4","I4EHEH4EI4"
Data "20EE2EHEH3EE20EE2","220220EHEH3E22022","0I4HEH4I40","I4EEHF11E0I4"
Data "U221EHEH4EE21EE","U22HEH7E1E11","HFHF"
'green
Data "0I50","I4022","I7","20EEI3","220I30","I7","202200EE2","I4022","0I50"
Data "I4022","I7","20EEI3","220I30","I7","202200EE2","I4022","I20I3"
Data "I40EE","20EE2202","220I4","I7","E20U220","E1EE1E1E","H7"
'Death
Data "G4M4G5","G3M6G4","G2660M2066G3","G260M406G3","660M80660"
Data "66G2M4G2660","G26G66G3","006G2M2G26G2","G36G46G4","G46G26G5"
Data "G5606G6","G66G60","G5606G6","G46G26G5","G266G466G3","G266G466G3"
' more to come
'

Logo:

Data 1060110336,830472192,833872864,1060517424,808846896,808859184,808707040
Data 0,4261412864,3221225472,3235508720,4242742040,3234011928,3234011928
Data 3233935864, 24, 496


' ----- Music -----
'Music data. Values are the Frequencies alternating Voice 1 and Voice 2
'1 is muted,99999 marks the End of the Song

Yankee:
Data 741,185,741,233,829,139,932,233,741,185,932,233,829,139
Data 554,233,741,185,741,233,829,139,932,233,741,185,1,233,699,208
Data 554,139,741,185,741,233,829,139,932,233,988,185,932,247,829,156
Data 741,247,699,174,554,277,621,139,699,277,741,185,1,233,741,139,1,233
Data 621,185,699,247,621,156,554,247,621,185,699,247,741,156,1,247
Data 554,185,621,233,554,139,493,233,466,185,1,233,554,139,1,233
Data 621,185,699,247,621,156,554,247,621,185,699,247,741,156,621,247
Data 554,174,741,247,699,139,829,247,741,185,1,233,741,139,99999,99999
Mus2:
Data 1,233,1,233,1,1,1,233,1,1,1,233,1,1,1,233,350,1,466,233,1,1,466,233
Data 554,1,699,233,1,1,1,233,350,1,440,174,350,1,440,174,523,1
Data 621,174,1,1,1,174,350,1,440,174,1,1,440,174,523,1,621,174
Data  1,1,1,174,350,1,466,233,350,1,466,233,554,1,699,233,1,1
Data 1,233,350,1,466,233,1,1,466,233,554,1,699,233,1,1,1,233,350,1
Data 440,174,350,1,440,174,523,1,621,174,1,1,1,174,621,1,621,156,621,1
Data 699,1,621,1,554,156,554,1,621,1,554,1,523,174,523,1,554,1,621,1
Data 699,174,1,1,1,1,1,1,621,156,554,1,523,1,350,1,1,174,440,1
Data 466,1,1,1,1,233,1,1,99999,99999
Mus3:
Data 1,1,1,1,1,1,523,147,1,1,589,1,658,1
Data 699,131,1,1,880,1,782,1,699,165,1,1,523,147,699,165,658,174
Data 523,220,1,131,1,220,350,174,392,220,440,131,392,220,466,174,440,233
Data 392,147,350,233,466,174,523,233,589,147,523,233,621,174,589,220
Data  523,131,466,220,440,174,523,220,350,131,440,220,261,165,350,196
Data 220,131,261,196,330,147,261,196,392,165,330,196,523,174,392,220
Data 658,131,523,220,699,174,523,220,350,131,261,220,174,174,131,233
Data 87,147,65,233,58,174,87,233,117,147,174,233,233,174,350,233
Data 466,147,699,233,932,174,1177,233,1398,147,1865,233,1398,196,1177,261
Data 932,131,699,261,658,196,589,261,523,131,589,261,658,174,699,220
Data 782,131,932,220,880,174,782,220,699,131,589,220,523,174,440,233
Data 392,147,350,233,466,174,523,233,589,147,658,233,699,165,782,233
Data 699,131,589,233,658,165,589,233,523,131,466,233,440,174,392,220
Data 294,131,330,220,350,185,1,1,1,1,1,1,99999,99999
Main_song:
Data 1,1,589,392,1,1,493,1
Data 392,1,782,330,1,1,741,1,658,1,589,392,1,1,782,1,493,1
Data 440,370,589,1,1,1,1,1,1,196,589,247,589,147,589,247,589
Data 196,493,247,440,147,392,247,1,196,782,261,782,165,782
Data 261,881,196,782,261,741,165,658,261,589,196,493,247,1
Data 147,493,247,782,196,1,247,493,147,1,247,589,220,440,294
Data 1,147,1,294,1,165,1,294,1,185,1,294,1,196,493,247,493
Data 147,523,247,589,196,658,247,741,147,658,247,1,196,1,261
Data 1,165,1,261,1,196,1,261,1,165,1,261,523,196,523,261,589
Data 165,658,261,1,196,741,261,782,165,741,261,1,220,1,294,1
Data 147,1,294,1,220,589,294,1,147,782,294,1,196,1,247,1,147
Data 782,247,741,196,658,247,589,147,741,247,1,196,1,247,1
Data 156,658,247,1,165,658,261,1,220,589,261,1,220,881,294
Data 1,147,782,294,1,220,741,294,1,147,782,294,1,196,1,247,1
Data 147,1,247,1,196,1,1,1,1,782,1,782,261,782,1,782,1,782,1
Data 782,1,741,1,658,1,782,1,1,247,1,1,1,1,589,1,1,1,493,1
Data 493,1,440,1,1,1,440,1,440,1,782,1,782,1,741,1,658,1,658
Data 1,1,1,1,1,1,1,589,1,1,1,1,1,1,1,589,1,493,1,493,1,493,1
Data 493,1,1,1,440,1,392,1,523,1,493,1,523,1,589,1,658,1,1,1
Data 1,1,1,1,589,1,523,1,440,1,440,1,440,1,1,1,392,1,370,1
Data 392,1,370,1,392,1,440,1,493,1,1,1,1,1,1,1,589,1,493,1
Data 493,1,493,1,493,1,1,1,440,1,392,1,523,1,493,1,523,1,589
Data 1,658,1,1,1,741,1,658,1,589,1,1,1,589,1,658,1,589,1,523
Data 1,493,1,440,1,330,1,1,1,1,1,1,1,370,1,1,1,440,1,1,1,392
Data 1,1,1,1,1,1,1,99999,99999
intro:
'Main song intro
Data 493,196,392,247,392,147,392,247,493,196,392,247,392
Data 147,392,247,523,220,523,261,493,147,493,261,440,220,1
Data 261,1,147,1,261,523,220,523,261,493,147,493,261,440
Data 220,440,261,658,147,658,261,589,220,523,261,493,147
Data 440,261,392,247,1,1,99999,99999
complete:
'Level complete
Data 1,1,1,1,1,1,392,247,440,294,493,247
Data 523,294,589,247,1,294,493,247,1,294,392,247,440,294
Data 493,247,440,294,392,247,1,294,392,247,1,294,392,247
Data 440,294,493,247,523,294,589,247,1,294,493,247,1,294
Data 589,261,523,294,493,261,440,294,392,247,1,1,1,1
Data 1,1,99999,99999
Home1:
'Frog-Home1
Data 1,1,1,1,881,741,990,699,881,741,1,1,741,589,1,1,881,741
Data 990,699,881,741,1,1,741,589,1,1,881,741,881,658,990,621
Data 1,1,1,1,881,621,782,658,554,699,741,589,1,1,1,1,1,1,440
Data 440,440,440,440,440,554,554,658,658,1,1,440,440,440,392
Data 440,370,589,440,741,589,1,1,881,741,881,658,990,589,1,1
Data 1,1,881,554,782,440,554,392,589,370,1,1,1,1,99999,99999
FKill:
Data 1,1,1,1,1,1,1,1,1,1,1,1,1,11,99999,99999
Game_OverSng:
Data 311, 156,311, 156,311, 156, 311, 156,440, 185,440, 185,1, 220,370, 220
Data 589, 185, 589, 185, 589, 220, 589, 220, 741, 147, 741, 147, 658, 220
Data 658, 220, 658, 185, 658, 185, 589, 220, 589, 220, 589, 165, 589, 165
Data 554, 220, 554, 220, 554, 196, 554, 196, 493, 220, 493, 220, 493, 147
Data  493,147,440,220,440,220,440,185,440,185,392,185,392,185,440,0,88888,88888

' Konami Style Font (Martin H.)
' Font type    : Full (95 ChArACtErs)
' Font size    : 8x8 pixels
' Memory usage : 764 Bytes
DefineFont #9
  5F200808
00000000 00000000 18181818 00180018 006C6C6C 00000000 367F3636 0036367F
3E683F0C 00187E0B 180C6660 00066630 386C6C38 003B666D 0030180C 00000000
3030180C 000C1830 0C0C1830 0030180C 3C7E1800 0000187E 7E181800 00001818
00000000 30181800 7E000000 00000000 00000000 00181800 180C0600 00006030
7E6E663C 003C6676 18183818 007E1818 0C06663C 007E3018 1C06663C 003C6606
6C3C1C0C 000C0C7E 067C607E 003C6606 7C60301C 003C6666 180C067E 00303030
3C66663C 003C6666 3E66663C 00380C06 18180000 00181800 18180000 30181800
6030180C 000C1830 007E0000 0000007E 060C1830 0030180C 180C663C 00180018
6A6E663C 003C606E 7E66663C 00666666 7C66667C 007C6666 6060663C 003C6660
66666C78 00786C66 7C60607E 007E6060 7C60607E 00606060 6E60663C 003C6666
7E666666 00666666 1818187E 007E1818 0C0C0C3E 00386C0C 70786C66 00666C78
60606060 007E6060 6B7F7763 0063636B 7E766666 0066666E 6666663C 003C6666
7C66667C 00606060 6666663C 00366C6A 7C66667C 0066666C 3C60663C 003C6606
1818187E 00181818 66666666 003C6666 66666666 00183C66 6B6B6363 0063777F
183C6666 0066663C 3C666666 00181818 180C067E 007E6030 6060607C 007C6060
18306000 0000060C 0606063E 003E0606 42663C18 00000000 00000000 FF000000
7C30361C 007E3030 063C0000 003E663E 667C6060 007C6666 663C0000 003C6660
663E0606 003E6666 663C0000 003C607E 7C30301C 00303030 663E0000 3C063E66
667C6060 00666666 18380018 003C1818 18380018 70181818 6C666060 00666C78
18181838 003C1818 7F360000 00636B6B 667C0000 00666666 663C0000 003C6666
667C0000 60607C66 663E0000 07063E66 766C0000 00606060 603E0000 007C063C
307C3030 001C3030 66660000 003E6666 66660000 00183C66 6B630000 00367F6B
3C660000 00663C18 66660000 3C063E66 0C7E0000 007E3018 7018180C 000C1818
00181818 00181818 0E181830 00301818 00466B31 00000000 FFFFFFFF FFFFFFFF
End DefineFont
