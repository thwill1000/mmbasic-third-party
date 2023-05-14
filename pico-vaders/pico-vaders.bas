' Transpiled on 07-12-2022 19:41:09

' BEGIN:     #Include "ctrl.ipp" -----------------------------------------------
' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
'
' MMBasic Controller Library
'
' Preprocessor flag PICOMITE defined
' Preprocessor flag CTRL_USE_ON_PS2 defined

Const ctrl.VERSION = 906  ' 0.9.6

' Button values as returned by controller driver subroutines.
Const ctrl.R      = &h01
Const ctrl.START  = &h02
Const ctrl.HOME   = &h04
Const ctrl.SELECT = &h08
Const ctrl.L      = &h10
Const ctrl.DOWN   = &h20
Const ctrl.RIGHT  = &h40
Const ctrl.UP     = &h80
Const ctrl.LEFT   = &h100
Const ctrl.ZR     = &h200
Const ctrl.X      = &h400
Const ctrl.A      = &h800
Const ctrl.Y      = &h1000
Const ctrl.B      = &h2000
Const ctrl.ZL     = &h4000

Const ctrl.OPEN  = -1
Const ctrl.CLOSE = -2
Const ctrl.SOFT_CLOSE = -3

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const ctrl.PULSE = 0.001 ' 1 micro-second

' When a key is down the corresponding byte of this 256-byte map is set,
' when the key is up then it is unset.
'
' Note that when using INKEY$ (as opposed to the CMM2 'KEYDOWN' function or
' the PicoMiteVGA 'ON PS2' command) to read the keyboard we cannot detect
' keyup events and instead automatically clear a byte after it is read.
Dim ctrl.key_map%(31 + Mm.Info(Option Base))

' Map used to convert PS/2 set 2 scan codes to entries in ctrl.key_map%().
' The scan code first has to be converted into a single byte value,
' see ctrl.on_ps2().
Dim ctrl.scan_map%(31)

' Initialises keyboard reading.
'
' @param  period%  CMM2 only - interval to read KEYDOWN state, default 40 ms.
' @param  nbr%     CMM2 only - timer nbr to read KEYDOWN state, default 4.
Sub ctrl.init_keys(period%, nbr%)
  ctrl.term_keys()
  Read Save
  Restore ctrl.scan_map_data
  Local i%
  For i% = Bound(ctrl.scan_map%(), 0) To Bound(ctrl.scan_map%(), 1)
    Read ctrl.scan_map%(i%)
  Next
  Read Restore
  On Ps2 ctrl.on_ps2()
End Sub

Sub ctrl.on_ps2()
  Local ps2% = Mm.Info(PS2)
  Select Case ps2%
    Case < &hE000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), ps2% And &hFF), 1
    Case < &hF000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), (ps2% And &hFF) + &h80), 1
    Case < &hE0F000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), ps2% And &hFF), 0
    Case Else : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), (ps2% And &hFF) + &h80), 0
  End Select
End Sub

' Terminates keyboard reading.
Sub ctrl.term_keys()
  On Ps2 0
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Do While Inkey$ <> "" : Loop
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
End Function

Function ctrl.poll_multiple$(drivers$(), mask%, duration%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF), i%
  Do
    For i% = Bound(drivers$(), 0) To Bound(drivers$(), 1)
      If ctrl.poll_single%(drivers$(i%), mask%) Then
        ctrl.poll_multiple$ = drivers$(i%)
        Exit Do
      EndIf
    Next
  Loop While Timer < expires%
End Function

' Opens, polls (for a maximum of 5ms) and closes a controller.
'
' @param  driver$  controller driver function.
' @param  mask%    bit mask to match against.
' @return          1 if any of the bits in the mask match what is read from the
'                  controller, otherwise 0.
Function ctrl.poll_single%(driver$, mask%)
  On Error Ignore
  Call driver$, ctrl.OPEN
  If Mm.ErrNo = 0 Then
    Local key%, t% = Timer + 5
    Do
      Call driver$, key%
      If key% And mask% Then
        ctrl.poll_single% = 1
        ' Wait for user to release key.
        Do While key% : Pause 5 : Call driver$, key% : Loop
        Exit Do
      EndIf
    Loop While Timer < t%
    Call driver$, ctrl.SOFT_CLOSE
  EndIf
  On Error Abort
End Function

' Gets a string representation of bits read from a controller driver.
'
' @param  x%  bits returned by driver.
' @return     string representation.
Function ctrl.bits_to_string$(x%)
  Static BUTTONS$(14) = ("R","Start","Home","Select","L","Down","Right","Up","Left","ZR","X","A","Y","B","ZL")

  If x% = 0 Then
    ctrl.bits_to_string$ = "No buttons down"
    Exit Function
  EndIf

  ctrl.bits_to_string$ = Str$(x%) + " = "
  Local count%, i%, s$
  For i% = 0 To Bound(BUTTONS$(), 1)
    If x% And 2^i% Then
      s$ = BUTTONS$(i%)
      If count% > 0 Then Cat ctrl.bits_to_string$, ", "
      Cat ctrl.bits_to_string$, s$
      Inc count%
    EndIf
  Next
End Function

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
Sub keys_cursor(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A
  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
End Sub

' Atari joystick on PicoGAME Port A.
Sub atari_a(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(GP14) * ctrl.A
      Inc x%, Not Pin(GP0)  * ctrl.UP
      Inc x%, Not Pin(GP1)  * ctrl.DOWN
      Inc x%, Not Pin(GP2)  * ctrl.LEFT
      Inc x%, Not Pin(GP3)  * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP0, DIn : SetPin GP1, DIn : SetPin GP2, DIn : SetPin GP3, DIn : SetPin GP14, DIn
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP0, Off : SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off : SetPin GP14, Off
  End Select
End Sub

' Atari joystick on PicoGAME Port B.
Sub atari_b(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(GP15) * ctrl.A
      Inc x%, Not Pin(GP28) * ctrl.UP
      Inc x%, Not Pin(GP4)  * ctrl.DOWN
      Inc x%, Not Pin(GP5)  * ctrl.LEFT
      Inc x%, Not Pin(GP22) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP4, DIn : SetPin GP5, DIn : SetPin GP15, DIn : SetPin GP22, DIn : SetPin GP28, DIn
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP15, Off : SetPin GP22, Off : SetPin GP28, Off
    End Select
End Sub

' SNES gamepad on PicoGAME Port A.
'
'   GP2: Latch, GP3: Clock, GP1: Data
Sub snes_a(x%)
  Select Case x%
    Case >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.Y      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.X      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.L      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.R      : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case Else
      nes_a(x%)
  End Select
End Sub

' SNES gamepad on PicoGAME Port B.
'
'   GP5: Latch, GP22: Clock, GP4: Data
Sub snes_b(x%)
  Select Case x%
    Case >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.Y      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.X      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.L      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.R      : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case Else
      nes_b(x%)
  End Select
End Sub

' Reads port A connected to a NES gamepad.
'
' Note that the extra pulse after reading bit 7 (Right) should not be necessary,
' but in practice some NES clone controllers require it to behave correctly.
'
'   GP2: Latch, GP3: Clock, GP1: Data
Sub nes_a(x%)
  Select Case x%
    Case >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP1, Din : SetPin GP2, Dout : SetPin GP3, Dout
      Pin(GP2) = 0 : Pin(GP3) = 0
      nes_a(0) ' Discard the first reading.
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off
  End Select
End Sub

' NES gamepad on PicoGAME Port B.
'
'   GP5: Latch, GP22: Clock, GP4: Data
Sub nes_b(x%)
  Select Case x%
    Case >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP4, Din : SetPin GP5, Dout : SetPin GP22, Dout
      Pin(GP5) = 0 : Pin(GP22) = 0
      nes_b(0) ' Discard the first reading.
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP22, Off
  End Select
End Sub

ctrl.scan_map_data:

Data &h9C92919395009900, &h0060099496989A00, &h0031710000008B00, &h00327761737A0000
Data &h0033346564786300, &h0035727466762000, &h0036796768626E00, &h003837756A6D0000
Data &h0039306F696B2C00, &h002D703B6C2F2E00, &h00003D5B00270000, &h000023005D0A0000
Data &h0008000000005C00, &h0000003734003100, &h001B383635322E30, &h0000392A2D332B9B
Data &h0000000097000000, &h0000000000000000, &h0000000000008B00, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000008682008700, &h0000808300817F84, &h0000889D00890000
' END:       #Include "ctrl.ipp" -----------------------------------------------

' This is the default startup file for new code.
' It is saved as 'newfile.txt' in the data folder.
' If you don't want any startup text, just delete the file.


'PicoVaders
'-------------------------------------
'
' Martin Herhaus 2022
' V 0.9.8
' Concept based on the Game Space Invaders
'  (c) 1978 by Tomohiro Nishikado of Taito,
'
'-----------------Init----------------
'MODE 2:Font 1
Font 1

' Vars and Arrays
Dim alien$(3,2) :'3 Alientypes with 2 Animantion states
Dim aliens(55,4):'x,y,type,alive
Dim ply$(3)
Dim Bnk%(4,2,8):'4 Bunker, 2 Rows of 8 Blocks
Dim A_Bomb%(10,4):'x,y,alive,owned_by
Dim Noise%(200)
Dim Uxpl%(3)
Dim snd%(4) = (100,90,85,80,70)
Udir%=1:Ux%=0:UA%=0
USCR=50
UfoSndMin%=800:UfoSndMax%=1100:UfoSnd%=800:Ustp%=100
anr%=55
Myst%=0
Score%=0:HighScore%=0
mvsnd=0
x_max=204:adir=1
BX=0:BY=0:BA=0
plx=103
weiss=RGB(white)
Rot=RGB(RED)
gelb=RGB(yellow)
grn=RGB(green)
gray=RGB(gray)
'
'Setup User Defined Graphics
'
Restore sr1
For al=1 To 3:For f=1 To 2:bitm$="":For n=1 To 16
 Read a:bitm$=bitm$+Chr$(a):Next n
 alien$(al,f)=bitm$:Next f
Next al
For f=1 To 3
  bitm$=""
  For n=1 To 16:Read a:bitm$=bitm$+Chr$(a):Next n:ply$(f)=bitm$
Next f
Restore xpld
xpl$=""
For n=1 To 16:Read a:xpl$=xpl$+Chr$(a):Next n
Restore ufo
bitm$=""
For n=1 To 16:Read a:bitm$=bitm$+Chr$(a):Next n:uf1$=bitm$
bitm$=""
For n=1 To 16:Read a:bitm$=bitm$+Chr$(a):Next n:uf2$=bitm$
'
'Generate Noise
'
For f=1 To 200:noise%(f)=Int(Rnd*1000):Next f
'
'-------------------------------------
'
coldstart:
plx=103
Y_Pos=48
l=1
tn=1
Intro
Level%=1
Score%=0
plyers%=3
start:
tic=1
bn%=1
bmax%=2+Int(Level%/2):If bmax%>10 Then bmax%=10
Setup_aliens
'Draw Screen
CLS
Box 50,229,220,1,,,grn
Print @(58,0);"SCORE <1>  HI-SCORE"
Print @(66,16);:Pscore Score%
Print @(154,16);:PScore HighScore%
Print @(46,230);Level%;
draw_bunkers
Levelup%=0
ADth%=0
nxtPlyr:
plHit%=0
For f=1 To 10
If A_Bomb%(f,3)=1 Then
 Line 50+A_Bomb%(f,1),A_Bomb%(f,2),50+A_Bomb%(f,1),A_Bomb%(f,2)+4,,0
EndIf
 A_Bomb%(f,3)=0
Next f
Bombs_out%=0
Box 72,232,40,8,1,0,0
If plyers%>1 Then GUI BITMAP 72,232,ply$(1),16,8,1,grn,0
If plyers%=3 Then GUI BITMAP 88,232,ply$(1),16,8,1,grn,0
'
'------------------------------
'Game Loop
'
Do
  ps=5 :'Timing
  move_single
  Draw_Player
  Move_Player
  Draw_Bullet
  Draw_Bomb
  If Not (tic Mod 16) Then drop_bomb
  If Not (tic Mod 4) Then Draw_Ufo
  Inc tic
  start_ufo
  If aDth%=1 Then Levelup%=1:Exit
  If plHit% Then Exit
  If A_Ground Then Expl_Player:GameOver%=1: Exit
  If ps>0 Then Pause ps
  Inc bn%: If bn%>Bmax% Then bn%=1

Loop
If plHit% Then
  Expl_Player
  Inc plyers%,-1
  If plyers%=0 Then GameOver%=1:GoTo GmOv
  Pause 2000
  GoTo nxtPlyr
EndIf

If Levelup%=1 Then
 Inc Level%:Pause 2000
 If Level%<6 Then Inc y_pos,8
 GoTo Start
EndIf
GmOV:
If gameover%=1 Then
   Print @(128,100);"PLAYER<1>"
   For f=1 To 10
   Print @(128,116);"GAME OVER"
   Pause 600
   Print @(128,116);"         "
   Pause 600
   If Inkey$=" " Then Exit For
   Next f
   gameover%=0:level%=1:GoTo coldstart
 EndIf
'
'-------------------------------
'
Sub Intro
CLS
Box 0,0,50,240,,0,0:Box 270,0,50,240,,0,0
Box 50,229,220,1,,,grn
Print @(146,30) "PLA"
Text 176, 40, "Y", "I", 1
Pause 600
Print @(122,50) "PICOVADERS"
Pause 600
Print @(74,70) "*SCORE ADVANCE TABLE*"
Pause 600
GUI BITMAP 104,88,uf1$,16,8,1,rot,0
Print @(130,88) "= ? MYSTERY"
Pause 600
Print @(130,108) "=30 POINTS"
Pause 600
GUI BITMAP 104,108,alien$(1,1),16,8,1,weiss,0
Pause 600
Print @(130,128) "=20 POINTS"
Pause 600
GUI BITMAP 104,128,alien$(2,1),16,8,1,weiss,0
Pause 600
Print @(130,148) "=10 POINTS"
Pause 600
GUI BITMAP 104,148,alien$(3,1),16,8,1,weiss,0
Print @(90,170) "(C) 1978 BY TAITO"
Print @(82,190) "PICOMITE-VGA VERSION"
Print @(74,210) "2022 BY MARTIN HERHAUS"
Pause 2000
For f=270 To 178 Step -1
  GUI BITMAP f,30,alien$(1,1+(f Mod 2)),16,8,1,weiss,0
  Box 270,30,16,10,,0,0
Pause 30
Next f
For f=178 To 278
   Text f, 40, "Y", "I", 1
   If f<270 Then GUI BITMAP f,30,alien$(1,1+(f Mod 2)),16,8,1,weiss,0
  Box 270,30,16,10,,0,0
  Pause 30
Next f
For f=278 To 176 Step -1
   Text f-6, 30, "Y", "", 1
   If f<270 Then GUI BITMAP f,30,alien$(1,1+(f Mod 2)),16,8,1,weiss,0
  Box 270,30,16,10,,0,0
  Pause 30
Next f
For f=178 To 270
  GUI BITMAP f,30,alien$(1,1+(f Mod 2)),16,8,1,weiss,0
  Box 270,30,16,10,,0,0
Pause 30
Next f

'adaptation for piones handler
'do
'  'Do While Inkey$<>" "
'  Print @(74,210) "  PRESS FIRE TO START   "
'  Pause 600
'  Print @(82,210) "                    "
'  Pause 600
'  'Loop
'  pio read 1,0,5,NES%()
'  buttn=255-NES%(4)
'loop until buttn=&h80
'end of adaptation for piones handler
  Call "nes_a", ctrl.OPEN
End Sub
'
Sub start_ufo
  ufo_x
  If Myst% >20 And Ua%=0 Then
   ua%=1: Myst%=0
   USCR=50
   f=Int(Rnd*10): If f>6 Then USCR=100:If f=9 Then USCR=150
   udir%=2:UX%=0:If Int(Rnd*2)=1 Then udir%=-2:Ux%=204
 EndIf
End Sub
'
Sub Drop_Bomb
'Number of active bombs =Max then Get out
If Bombs_out%>=BMax% Then Exit Sub
'start at the bottom right Alien
Inc ps,-3:'Timing
aln%=56
test_next_alien:
Inc  aln%,-1
'
'Has the Countdown arrived 0? No bomb possible: Get out
If aln%=0 Then Inc ps,-1:Exit Sub
'active? no, test next alien..
If Not aliens(aln%,4) Then GoTo test_next_alien
'---only one Bomb at once per Alien
'no own bomb on the way yet? No, test next alien.
 For bn%=1 To 10
 If A_Bomb%(bn%,3)=1 And A_Bomb%(bn%,4)=aln% Then GoTo test_next_alien
 Next bn%

'near the ship? no? Random(1/25) otherwise test next alien.
If aliens(aln%,1)<plx-8 Or aliens(aln%,1)>plx+8 And Int(Rnd*25)>1 Then
GoTo test_next_alien
EndIf
'And Int(Rnd*25)>1
'if row less than 5: is there an active alien below me? Yes,test next alien.
If aln%<45 Then If aliens(aln%+11,4)=1 Then GoTo test_next_alien
'Drop the bomb
 bn%=1
'find_free place in Array:
testSlot:
 If A_Bomb%(bn%,3)=1 Then
   Inc bn%: If bn%>10 Then Exit Sub
   GoTo testSlot
 EndIf
 A_Bomb%(bn%,1)=aliens(aln%,1)+8
 A_Bomb%(bn%,2)=aliens(aln%,2)+6
 A_Bomb%(bn%,3)=1
 A_Bomb%(bn%,4)=aln%
 Inc Bombs_out%
End Sub
'----------------------------

Sub Draw_Bomb
For f=1 To 10
If A_Bomb%(f,3)=1 Then
 'x,y,active
 'delete old position
 Line 50+A_Bomb%(f,1),A_Bomb%(f,2),50+A_Bomb%(f,1),A_Bomb%(f,2)+4,,0
   Inc A_Bomb%(f,2),1
   If Hit_Bunker(A_Bomb%(f,1),A_Bomb%(f,2)+4) Then
     A_Bomb%(f,3)=0
     Inc Bombs_out%,-1
     Exit Sub
   EndIf
  If A_Bomb%(f,2)>224 Then
    A_Bomb%(f,3)=0
    Inc Bombs_out%,-1
    Exit Sub
  EndIf
 'draw one bomb
  Line 50+A_Bomb%(f,1),A_Bomb%(f,2),50+A_Bomb%(f,1),A_Bomb%(f,2)+4,,gelb

 'Hit bullet?
  If BA Then
    Select Case A_Bomb%(f,1)
    Case bx-2 To bx+2
    Select Case A_Bomb%(f,2)
     Case by-4 To by
      ba=0: A_Bomb%(f,3)=0
      Inc Bombs_out%,-1
      expl 42+A_Bomb%(f,1),A_Bomb%(f,2),0
      Exit Sub
     End Select
    End Select
  EndIf

 'Hit Player?
  If A_Bomb%(f,2) >210 Then
   If A_Bomb%(f,1) >=plx And A_Bomb%(f,1)<plx+16 Then plHit%=1
  EndIf

 EndIf
Line 50+bx,by,50+bx,by+4,,0

EndIf
Next f
End Sub

'
Sub Draw_Ufo
 If Ua%=0 Then Inc ps,3:Exit Sub
 Play tone UfoSnd%,UfoSnd%,150
 Inc UfoSnd%,Ustp%
 If UfoSnD%=UfoSndMin% Or UfoSnd%>UfoSndMax% Then Ustp%=-Ustp%
 Box 50+ux%,32,16,10,,0,0
 Inc Ux%,udir%
 If Ux%>204 Or Ux%<0 Then Ua%=0: Exit Sub
 GUI BITMAP 50+Ux%,32,uf1$,16,8,1,rot,0
End Sub

Sub ufo_x
  If Uxpl%(1) Then
    Inc Uxpl%(3)
    Play tone 900+15*Uxpl%(3),900+15*Uxpl%(3),100
    If uxpl%(3)=40 Then Print @(42+Uxpl%(2),32) USCR
    If uxpl%(3)=60 Then
      Box 50+Uxpl%(2),32,32,10,,0,0
      Uxpl%(1)=0
      Uxpl%(3)=0
      Inc Score%,USCR
      PRN_SCR
    EndIf
  EndIf
End Sub
Sub Draw_Bunkers
  For f=0 To 3
   Draw_Bunker 80+F*45,184
   For n=1 To 8
    Bnk%(f+1,1,n)=1
    Bnk%(f+1,2,n)=1
   Next n
  Next f
End Sub

Sub Draw_Bunker(bx,by)
 Box bx  ,by+4,22,12,,grn,grn
 Box bx+1,by+3,20,1 ,,grn,grn
 Box bx+2,by+2,18,1 ,,grn,grn
 Box bx+3,by+1,16,1 ,,grn,grn
 Box bx+4,by  ,14,1 ,,grn,grn
 Box bx+5,by+14,12,2,,0,0
 Box bx+6,by+13,10,1,,0,0
 Box bx+7,by+12,8,1,,0,0
End Sub


Function Hit_Bunker(TsX%,Tsy%)
Hit_Bunker=0
  'Y in Range?
  Select Case TsY%
    Case 184 To 200
    'Yes, X in Range of one of the 4 Bunkers?
     Select Case TsX%
       Case 30 To 51
         'Bunker1
          bhy=Int((Tsy%-184)/8)
          bhx=1+Int((TsX%-30)/3)
          If Bnk%(1,bhy,bhx)=1 Then
            Bnk%(1,bhy,bhx)=0
            Hit_Bunker=1
            Line 50+TsX%,Tsy%,50+TsX%,TsY%+4,,0
            debunk 50+TsX%,tsy%
            'BA=0
          EndIf
        Case 75 To 96
         'Bunker2

          bhy=Int((TsY%-184)/8)
          bhx=1+Int((TsX%-75)/3)
          If Bnk%(2,bhy,bhx)=1 Then
            Bnk%(2,bhy,bhx)=0
            Hit_Bunker=2
            Line 50+TsX%,Tsy%,50+TsX%,TsY%+4,,0
            debunk 50+TsX%,TsY%
            'BA=0
          EndIf

        Case 120 To 141
         'Bunker3

         bhy=Int((TsY%-184)/8)
          bhx=1+Int((TsX%-120)/3)
          If Bnk%(3,bhy,bhx)=1 Then
            Bnk%(3,bhy,bhx)=0
            Hit_Bunker=3
            Line 50+TsX%,Tsy%,50+TsX%,TsY%+4,,0
            debunk 50+TsX%,TsY%
            'BA=0
          EndIf

        Case 165 To 186
         'Bunker4
         bhy=Int((TsY%-184)/8)
          bhx=1+Int((TsX%-165)/3)
          If Bnk%(4,bhy,bhx)=1 Then
            Bnk%(4,bhy,bhx)=0
            Hit_Bunker=4
            Line 50+TsX%,Tsy%,50+TsX%,TsY%+4,,0
            debunk 50+TsX%,TsY%

          EndIf
     End Select

  End Select
End Function

Sub debunk zx,zy
' Destroy part of the Bunker
For xf= 1 To 40
   Pixel zx-3+Rnd*8,zy-5+Rnd*8,0
 Next xf
 Inc ps,-3
End Sub

Sub Draw_Bullet
  If Not BA Then Inc ps,4:Exit Sub :'No Bullet active

  Line 50+bx,by,50+bx,by+4,,0
   Inc BY,-2
  If BY<=32 Then BA=0:Exit Sub
  Line 50+bx,by,50+bx,by+4,,weiss
  If BY Mod 8 Then Exit Sub :' to speed up, do the Test only every 8 Pixel
  If collition1(bx,by) Then
     Line 50+bx,by,50+bx,by+4,,0
     BA=0
     'last Alien?
     ADth%=aldeath()
  EndIf
  If Hit_Bunker(bx,by) Then BA=0:Exit Sub
  'ufo?
  If ua% Then
  Select Case by
    Case 32 To 40
    Select Case bx
      Case ux% To ux%+15
      'expl ufo
      Uxpl%(1)=1:Uxpl%(2)=ux%:Uxpl%(3)=0
      GUI BITMAP 50+ux%,32,uf2$,16,8,1,rot,0
      ua%=0
    End Select
  End Select
EndIf
End Sub
'
Function collition1(blx,bly)
collition1 =0
Select Case bly
 Case ypos+16 To 214
  'Alien?
  For f=1 To 55
   If aliens(f,4)<>0 Then
      ax=aliens(f,1):ay=aliens(f,2)
      Select Case blx
        Case ax+1 To ax+13
          Select Case bly
            Case ay To ay+7
              collition1=1
              expl ax+50,ay,1
              aliens(f,4)=0
              Inc Score%,40-(10*Aliens(f,3))
              PRN_SCR
              Exit Function
          End Select
      End Select
     EndIf
    Next f
End Select
End Function

Sub PRN_SCR
  Print @(66,16);:Pscore Score%
  If Score%>HighScore% Then
    Highscore%=Score%
    Print @(154,16);:PScore HighScore%
  EndIf

End Sub
Sub PScore wert
  If wert<1000 Then Print "0";
  If wert<100 Then Print "0";
  If Wert<10 Then Print "0";
  Print Str$(wert);
End Sub
' Explode Alien or Bomb
Sub expl ex,ey,snd
 GUI BITMAP ex,ey,xpl$,16,8,1,gelb,0
 Draw_Ufo
 If snd=1 Then
   For nse=1 To 75
    Play tone noise%(nse),noise%(nse),2
    Pause 1
   Next nse
   Play tone 0,0,1
 Else
   Pause 20
 EndIf
 Draw_Ufo
 Box ex,ey,16,10,,0,0
End Sub

Sub Move_Player
  'adaptations for piones handler
  Local key%
  Call "nes_a", key%
'  pio read 1,0,5,NES%()
'  buttn=255-NES%(4)
  if key%=0 then exit sub
  if key% = ctrl.LEFT then plx=plx-1:If plx<16 Then plx=16:Exit Sub
  if key% = ctrl.RIGHT Then plx=plx+1:If plx>188 Then plx=188:Exit Sub
  if key% = ctrl.A and BA=0 then
  'end of adaptations for piones handler

  'k$=Inkey$
  'If k$="" Then Exit Sub
  'If Asc(k$)=130 Then plx=plx-1:If plx<16 Then plx=16:Exit Sub
  'If Asc(k$)=131 Then plx=plx+1:If plx>188 Then plx=188:Exit Sub
  'If k$=" " And BA=0 Then
    If Not ua% Then Inc Myst%,Int(Rnd*3)
    BA=1:BX=plx+7:BY=210
    For f=1000 To 1 Step -50:Play tone 1000+f,1000+f,5:Pause 2:Next f
  EndIf
End Sub

Sub Draw_Player
  GUI BITMAP 50+plx,214,ply$(1),16,8,1,grn,0
End Sub

Sub Expl_Player
  'show Player Explosion
  For f=1 To 3
    GUI BITMAP 50+plx,214,ply$(2),16,8,1,grn,0
    For nse=1 To 100:Play tone noise%(nse),noise%(nse),2:Pause 1:Next nse
    GUI BITMAP 50+plx,214,ply$(3),16,8,1,grn,0
    For nse=100 To 200:Play tone noise%(nse),noise%(nse),2:Pause 1:Next nse
  Next f
  For nse=1 To 200:Play tone noise%(nse),noise%(nse),2:Pause 1:Next nse
  Play tone 0,0,1
  Pause 500
  Box 50+plx,214,16,10,,0,0
End Sub
'
Sub Setup_aliens
'Fills the array of Aliens with x,y,type and Live Values
  num=1
  A_Ground=0
  Num_aliens%=55
  For R=1 To 5
    at=3: ' Alien Type
    If R=1 Then at=1
    If R=2 Or R=3 Then at=2
    For n=1 To 11
      aliens(num,1)=n*16
      aliens(num,2)=Y_Pos+R*16
      aliens(num,3)=at
      aliens(num,4)=1
      Inc num
    Next n
  Next R
  trn=0
  wait%=80
End Sub
'
Sub Draw_aliens
 For f=55 To 1 Step -1
   ax=50+aliens(f,1)
   ay=aliens(f,2)
   at=aliens(f,3)
   If aliens(f,4) Then
    Box ax,ay,16,10,,0,0
    GUI BITMAP ax,ay,alien$(at,l),16,8,1,weiss,0
   EndIf
 Next f
End Sub
'
Sub move_single
' Move a Single Alien one Step
' Aliens are counted from bottom right to top left
 mslife:
 'Is this alien alive
 If aliens(anr%,4) Then
   ax=aliens(anr%,1)
   ay=aliens(anr%,2)
   at=aliens(anr%,3)
   Box ax+50,ay,16,10,,0,0
   Inc ax,Adir
   GUI BITMAP ax+50,ay,alien$(at,l),16,8,1,weiss,0
   aliens(anr%,1)=ax
   If ax >=x_max Or ax <1 Then trn=1
  Else
   'move to previous alien
   Inc anr%, -1
   If anr%<1 Then
    anr%=55
    If trn=1 Then Adir=-Adir:down_aliens:Draw_aliens:trn=0
    Inc mvsnd: mvsnd=mvsnd And 3
    'Play Sound if all Aliens move a Step.. but not if Ufo is out
    If Ua%=0 Then Play TONE snd%(mvsnd+1),snd%( mvsnd+1), 80
    Inc l:If l=3 Then l=1:'Flip Alien Sprites (one or two)
  EndIf
  GoTo mslife
 EndIf
 Inc anr%,-1

End Sub

Sub move_aliens
'Old Sub .. mova all Aliens
  trn=0
Inc mvsnd: mvsnd=mvsnd And 3
Play TONE snd%(mvsnd+1),snd%( mvsnd+1), 80
  For f=55 To 1 Step -1
    If aliens(f,4) Then
      ax= aliens(f,1)
      Inc ax,Adir
      aliens(f,1)=ax
      If ax >=x_max Or ax <1 Then trn=1
    EndIf
  Next f
  If trn=1 Then Adir=-Adir:down_aliens
End Sub
'
Sub down_aliens
' Moves all Aliens Down by 8 Pixel
 For f=55 To 1 Step -1
  If aliens(f,4) Then
    ax= aliens(f,1):ay=aliens(f,2)
    Box ax+50,ay,16,10,,0,0
    aliens(f,2)=ay+8
    If ay+8>=202 Then A_Ground=1
   EndIf
  Next f
 End Sub

Function aldeath()
'counts number of Aliens in Num_Aliens%,
'returns 1 if no Alien left, else 0
 Num_aliens%=0
 aldeath=1
  For f=1 To 55
    Inc Num_aliens%,aliens(f,4)
  Next f
  If Num_aliens% Then aldeath=0
End Function
'
'----UDG----
'
sr1:
Data  1, 128, 3, 192, 7, 224, 13, 176, 15, 240, 5, 160, 8, 16, 4, 32
Data  1, 128, 3, 192, 7, 224, 13, 176, 15, 240, 2, 64, 5, 160, 10, 80

sr2:
Data  8, 32, 4, 64, 15, 224, 27, 176, 63, 248, 47, 232, 40, 40, 6, 192
Data  8, 32, 36, 72, 47, 232, 59, 184, 63, 248, 31, 240, 8, 32, 16, 16

sr3:
Data  7, 224, 31, 248, 63, 252, 57, 156, 63, 252, 14, 112, 25, 152, 48, 12
Data  7, 224, 31, 248, 63, 252, 57, 156, 63, 252, 14, 112, 25, 152, 12, 48

plyr:
Data  1, 0, 3, 128, 3, 128, 63, 248, 127, 252, 127, 252, 127, 252, 127, 252
Data  2, 0, 0, 16, 2, 160, 18, 0, 1, 176, 69, 168, 31, 228, 63, 245
Data  16, 4, 130, 25, 16, 192, 2, 2, 75, 97, 33, 196, 31, 224, 55, 228

xpld:
Data  4, 64, 34, 136, 16, 16, 8, 32, 96, 12, 8, 32, 18, 144, 36, 72

ufo:
Data  0, 0, 7, 224, 31, 248, 63, 252, 109, 182, 255, 255, 57, 156, 16, 8
ufo_xpl:
Data  148, 10, 64, 48, 143, 24, 31, 206, 58, 167, 143, 140, 5, 24, 136, 136
