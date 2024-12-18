'===========================================
'              3D Monster Maze
'===========================================
' (c) 1981 New Generation Software Ltd,
'        written by Malcolm Evans.
'===========================================
'
' v1.0.0  Ported to PicoMiteVGA, Game*Mite
'         and MMBasic for Windows by
'         Martin Herhaus, April 2024.
' v1.1.0  Ported to MMB4L and messed about
'         with by Tom Williams, Dec 2024.
'===========================================

Option Base 0
Option Default None
Option Explicit On

Const VERSION = 101300 ' 1.1.0

'!if defined(PICOMITEVGA)
  '!replace { Option Simulate "Colour Maximite 2" } { Option Simulate "PicoMiteVGA" }
  '!dynamic_call nes_a
'!elif defined(GAMEMITE)
  '!replace { Option Simulate "Colour Maximite 2" } { Option Simulate "Game*Mite" }
  '!dynamic_call ctrl.gamemite
'!endif

If Mm.Device$ = "MMB4L" Then
  Option Simulate "Colour Maximite 2"
  Option CodePage CMM2
EndIf

#Include "../splib/system.inc"
#Include "../splib/ctrl.inc"
#Include "../splib/game.inc"
#Include "../splib/string.inc"
#Include "../splib/msgbox.inc"

'!dynamic_call game.on_break
sys.override_break("game.on_break")

Const FRAME_DURATION = 120
Const STATE_SHOW_TITLE = 0, STATE_PLAY_GAME = 1, STATE_WIN_GAME = 2, STATE_LOSE_GAME = 3

Dim a As Integer
Dim Integer mapSize.x = 17
Dim Integer mapSize.y = 18
Dim String screen(25) Length 48, esc = Chr$(27), v_inv = esc + "[7m", v_norm = esc + "[0m"
Dim String grey$ = Chr$(135) Length 1
Dim String txt(22) Length 64
Dim d$ = Chr$(223), u$ = Chr$(220), b$ = Chr$(160)
Dim Integer gameMap(mapSize.y%, mapSize.x%), GAMEFLAGS, Rex.x, Rex.y, Rex.a
Dim blk$(16) Length 2
Dim String MSG(7) Length 22
Dim exi$ = " " + Chr$(176) + Chr$(177) + Chr$(178) + Chr$(219) + Chr$(178) + Chr$(177) + Chr$(176)
Dim Integer el = Len(exi$)
MSG(0) = "   REX LIES IN WAIT   " : MSG(1) = " run HE IS BEHIND YOU "
MSG(2) = " run HE IS BESIDE YOU " : MSG(3) = "   REX HAS SEEN YOU   "
MSG(4) = " FOOTSTEPS APPROACHING" : MSG(5) = " HE IS HUNTING FOR YOU"
MSG(6) = String$(22, 32)
Dim Integer wall = 1, none = 0
Dim Integer status = 0 ' 0 .. 6
Dim Integer die, direction, f, mt, n, o_n, px, py, pp.x, pp.y, rmov, score, tt
Dim state%
Dim next_frame%
Dim key%
Dim ctrl$ = ctrl.default_driver$()

If InStr(Mm.Device$, "PicoMiteVGA") Then Mode 2
game.init_window("3D Monster Maze for MMBasic", VERSION)
ctrl.init_keys()
If ctrl.open_no_error%(ctrl$) <> sys.SUCCESS Then ctrl$ = "ctrl.no_controller"

Font 9
Restore Blocks
For n = 0 To 15 : Read a : blk$(n) = Chr$(a) : Next

restrt:
state% = STATE_SHOW_TITLE
Randomize(Timer)
generateMap
score = 0
placeExit
placeRex
o_n = -1 : MT = 0

' Bit 7: 1=The player has been caught.
' Bit 6: 1=The player has moved forwards.
' Bit 5: 1=The player has not moved and so there is no need to redraw the view of the maze.
' Bit 4: 1=The Exit is visible.
' Bit 3: 1=Rex has moved.
' Bit 2: 1=Rex has moved into a new location.
' Bit 1: 1=Rex has his left foot forward, 0=Rex has his right foot forward.
' Bit 0: Controls the movement speed of Rex. It combines with bits 1 and 2 to form a 3 bit counter. Bit 0 will be forced to 1
'        when the played is moving thereby forcing Rex to take quicker steps.
GAMEFLAGS = &b00000000

If Not InStr(Mm.CmdLine$, "--no-intro") Then intro()
'insert PlayerPosition into Maze
Nxtrnd:
pp.x = mapSize.x - 1 : pp.y = mapSize.y - 1 : direction = 3
'clear terminal screen
Cls
For f = 0 To 23 : Text 32, 24 + f * 8, String$(32, " ") : Next
'Print @(430,96)"S C O R E";
Text 240, 96, "SCORE"

'Do
'For k=5 To 0 Step -1
clr
'pass k,2:pass k,0:If k Then pass k,1
' For f=0 To 23:Text 32,24+f*8,screen$(f): Next
'Pause 50
'Next k
'Loop

'===========================================
'Main loop
'===========================================
state% = STATE_PLAY_GAME
key% = 0
status = 6
Do
  next_frame% = Timer + FRAME_DURATION
  view
  For f = 0 To 23 : Text 32, 24 + f * 8, screen$(f) : Next

  GAMEFLAGS = GAMEFLAGS Or &b00100000 ' set bit 5
  Select Case key%
    Case ctrl.RIGHT
      Inc direction : direction = direction Mod 4
      GAMEFLAGS = GAMEFLAGS And &b11011111 ' reset bit 5
    Case ctrl.LEFT
      Inc direction, -1 : If direction = -1 Then direction = 3
      GAMEFLAGS = GAMEFLAGS And &b11011111 ' reset bit 5
    Case ctrl.UP
      GAMEFLAGS = GAMEFLAGS Or  &b01000000 ' set bit 6
      GAMEFLAGS = GAMEFLAGS And &b11011111 ' reset bit 5
      move_player()
      GAMEFLAGS = GAMEFLAGS And &b11011111 ' reset bit 5
  End Select

  rmov = GAMEFLAGS And &b00000011
  Inc rmov
  GAMEFLAGS = GAMEFLAGS And &b11111100
  If rmov = 3 Then
    Move_Rex
    Rex.a = 0
  Else
    Rex.a = 1
    GAMEFLAGS = GAMEFLAGS Or rmov
  EndIf
  If (Rex.x = pp.x) And (Rex.y = pp.y) And Rex.a Then die = 1 : Exit
  If gameMap(pp.y, pp.x) = 2 Then die = 0 : Exit

  exi$ = Right$(exi$, el - 1) + Left$(exi$, 1)

  clear_input()
  key% = get_input%()
  Do While Timer < next_frame%
    key% = key% Or get_input%()
  Loop

  Text 240, 96 + Mm.Info(FontHeight), str.centre$(Str$(score), 5)
Loop

If die = 1 Then
  EndGame
  Goto restrt
Else
  WinGame
  generateMap
  placeExit
  placeRex
  Goto Nxtrnd
EndIf

'===========================================
'Intro related Subs
'===========================================

Sub Intro
  Local Integer f, y
  Local a$
  Local String RM$(34)
  For f = 0 To 2 : txt(f) = "" : Next

  Restore ringmaster0
  For y = 0 To 33
    Read a$
'    For f=1 To Len(a$) Step 2:RM$(y)=RM$(y)+Chr$(Val("&H"+Mid$(a$,f,2))):Next
    For f = 1 To Len(a$) Step 2 : RM$(y) = RM$(y) + Chr$(Val("&H" + Mid$(a$, f, 2))) : Next
  Next
  Cls
  For f = 0 To 23 : Text 32, 24 + f * 8, String$(32, " ") : Next
  For f = 0 To 22 : txt(f) = "" : Text 32, 24 + f * 8, String$(32, " ") : Next

  'Show Ringmaster
  For y = 0 To 20 : Text 32, 24 + y * 8, RM$(y) : Next
  Text 32, 200, invert$("     (C)1981 BY MALCOM EVANS    ")
  Text 32, 208, invert$(" MMBASIC BY MARTIN HERHAUS 2024 ")

  ' Show intro text.
  Restore text_intro
  If showtxt%() = ctrl.START Then Exit Sub

  ' Show Ringmaster Bow
  For y = 0 To 12 : Text 32, 24 + y * 8, RM$(21 + y) : Next

  ' Scroll some empty Lines
  Restore text_empty
  If showtxt%() = ctrl.START Then Exit Sub

  'Show Ringmaster
  For y = 0 To 20 : Text 32, 24 + y * 8, RM$(y) : Next

  clear_input()
  Do
    Select Case get_input%()
      Case ctrl.B
        Restore text_instructions
        Exit Do
      Case ctrl.A, ctrl.START
        Restore text_mist
        Exit Do
    End Select
  Loop

  If showtxt%() = ctrl.START Then Exit Sub

  ' Show Ringmaster Bow
  For y = 0 To 12 : Text 32, 24 + y * 8, RM$(21 + y) : Next
  Pause 1000

  ' Show Ringmaster
  For y = 0 To 20 : Text 32, 24 + y * 8, RM$(y) : Next
  Pause 2000
  Print @(0, 0);
End Sub

Function invert$(in$)
  Local Integer f
  invert$ = ""
  For f = 1 To Len(in$)
    invert$ = invert$ + Chr$(Asc(Mid$(in$, f, 1)) Xor 128)
  Next f
End Function

' Shows scrolling text read from current DATA pointer.
Function showtxt%()
  Local a$, b1$
  Local Integer f

  clear_input()

  Do
    Read b1$
    a$ = ""
    If b1$ = "@" Then Exit Function
    b1$ = Left$(replace_text$(b1$), 22)
    For f = 1 To Len(b1$)
      If Asc(Mid$(b1$, f, 1)) < 95 Then
        a$ = a$ + Mid$(b1$, f, 1)
      Else
        a$ = a$ + Chr$(Asc(Mid$(b1$, f, 1)) + 96)
      EndIf
    Next

    For f = 0 To 20 : txt(f) = txt(f + 1) : Next
    txt(21) = a$
    showtxt% = update_txt_display%()
    If showtxt% = ctrl.START Then Exit Do

    For f = 0 To 20 : txt(f) = txt(f + 1) : Next
    txt(21) = String$(22, " ")
    showtxt% = update_txt_display%()
    If showtxt% = ctrl.START Then Exit Do
  Loop
End Function

Function replace_text$(s$)
  replace_text$ = s$
  If ctrl$ <> "ctrl.no_controller" Then
    replace_text$ = str.replace$(s$, "'i'", "b")
    replace_text$ = str.replace$(replace_text$, "space", "start")
    replace_text$ = str.replace$(replace_text$, "esc", "select")
  EndIf
  replace_text$ = str.replace$(replace_text$, "score", Str$(score))
End Function

Function update_txt_display%()
  Local f%
  For f% = 0 To 21 : Text 112, 24 + f% * 8, txt(f%) : Next
  Const t% = Timer + 500
  Do While Timer < t%
    Pause 50
    Select Case get_input%()
      Case 0
        ' Do nothing
      Case ctrl.A, ctrl.START
        update_txt_display% = ctrl.START
        Exit Do
      Case Else
        Exit Do
    End Select
  Loop
End Function

Sub EndGame
  state% = STATE_LOSE_GAME
  Rex.a = 0
  shRex(0)
  show_end_text("text_lose")
End Sub

Sub winGame
  state% = STATE_WIN_GAME
  Inc score, 200
  show_end_text("text_win")
End Sub

Sub show_end_text(label$)
  Local mes$
  Local Integer w
  Pause 500
  show_message(6)
  For f = 0 To 23
    screen$(f) = Left$(screen$(f), 23) + "  "
    Text 32, 24 + f * 8, screen$(f)
  Next
  Restore label$
  f = 7
  Do
    Read w, Mes$
    If w = -1 Then Exit Do
    mes$ = replace_text$(mes$)
    mes$ = ZXFNT$(mes$)
    Text 40 + 8 * w, 24 + f * 8, mes$
    Inc f, 2
  Loop
  clear_input()
  Do
    If get_input%() And (ctrl.A Or ctrl.START) Then Exit Do
  Loop
End Sub

Function ZXFNT$(in$)
  Local Integer n, A
  For n = 1 To Len(in$)
    If Asc(Mid$(in$, n, 1)) < 97 Then
      ZXFNT$ = ZXFNT$ + Mid$(in$, n, 1)
    Else
      a = Asc(Mid$(in$, n, 1)) + 96
      ZXFNT$ = ZXFNT$ + Chr$(A)
    EndIf
  Next
End Function

'===========================================
'Player related Subs
'===========================================

Sub move_player()
'move Player position one step forward
  Select Case direction
    Case 0
      If pp.y > 1 Then
        If gameMap(pp.y - 1, pp.x) <> wall Then Inc pp.y, -1
      EndIf
    Case 1
      If pp.x < (mapSize.x - 1) Then
        If gameMap(pp.y, pp.x + 1) <> wall Then Inc pp.x
      EndIf
    Case 2
      If pp.y < (mapSize.y - 1) Then
        If gameMap(pp.y + 1, pp.x) <> wall Then Inc pp.y
      EndIf
    Case 3
      If pp.x > 1 Then
        If gameMap(pp.y, pp.x - 1) <> wall Then Inc pp.x, -1
      EndIf
  End Select
  Inc score, 5 * (status > 0 And status < 4)
End Sub

'===========================================
'InGame Display related Subs
'===========================================
Sub view
  '0=N,1=E,2=S,3=W
  'Prepare players view by drawing the corridors in the walls
  Local Integer n1, sr
  clr
  sr = -1
  Select Case direction
    Case 0
      For n1 = 0 To 5
        If gameMap(pp.y - n1, pp.x - 1) <> wall Then Pass n1, 0
        If gameMap(pp.y - n1, pp.x + 1) <> wall Then Pass n1, 2
        If gameMap(pp.y - n1, pp.x) = wall Then Pass n1 - 1, 1 : Exit For
        If gameMap(pp.y - n1, pp.x) = 2 Then ext n1 - 1 : Exit For
        If gameMap(pp.y - n1, pp.x) = 3 Then sr = n1
      Next
    Case 1
      For n1 = 0 To 5
        If gameMap(pp.y - 1, pp.x + n1) <> wall Then Pass n1, 0
        If gameMap(pp.y + 1, pp.x + n1) <> wall Then Pass n1, 2
        If gameMap(pp.y, pp.x + n1) = wall Then Pass n1 - 1, 1 : Exit For
        If gameMap(pp.y, pp.x + n1) = 2 Then ext n1 - 1 : Exit For
        If gameMap(pp.y, pp.x + n1) = 3 Then sr = n1
      Next
    Case 2
      For n1 = 0 To 5
        If gameMap(pp.y + n1, pp.x + 1) <> wall Then Pass n1, 0
        If gameMap(pp.y + n1, pp.x - 1) <> wall Then Pass n1, 2
        If gameMap(pp.y + n1, pp.x) = wall Then Pass n1 - 1, 1 : Exit For
        If gameMap(pp.y + n1, pp.x) = 2 Then ext n1 - 1 : Exit For
        If gameMap(pp.y + n1, pp.x) = 3 Then sr = n1
      Next
    Case 3
      For n1 = 0 To 5
        If gameMap(pp.y + 1, pp.x - n1) <> wall Then Pass n1, 0
        If gameMap(pp.y - 1, pp.x - n1) <> wall Then Pass n1, 2
        If gameMap(pp.y, pp.x - n1) = wall Then Pass n1 - 1, 1 : Exit For
        If gameMap(pp.y, pp.x - n1) = 2 Then ext n1 - 1 : Exit For
        If gameMap(pp.y, pp.x - n1) = 3 Then sr = n1
      Next
  End Select
  If sr > -1 Then shRex sr
  show_message(status)
End Sub

Sub show_message(nr As Integer)
  Local kk As Integer = nr
  'Show Status Messsages
  Local Integer n
  If kk = o_n Then
    'increment message Timer
    'Controls how long the message is displayed
    Inc mt
  Else
    mt = 0 : o_n = kk
  EndIf
  If mt > 5 Then kk = 6
  Local mes$ = ZXFNT$(MSG$(kk))
  For n = 2 To 23
    Mid$(screen$(23), n, 1) = Mid$(mes$, n - 1, 1)
  Next
End Sub

Sub clr
  'Prepare the Maze View in screen$ array
  ' all walls closed
  For f = 0 To 23 : screen$(f) = String$(24, " ") : Next

  screen$(0) = Chr$(156) + String$(23, 32) + Chr$(155)
  screen$(23) = Chr$(154) + String$(23, 32) + Chr$(157)

  For f = 1 To 11
    screen$(f) = String$(f, 160) + Chr$(156) + " " + String$((11 - f), " ")
    screen$(f) = screen$(f) + String$((11 - f), " ") + Chr$(155)
    screen$(f) = screen$(f) + String$(f, 160)
    Screen$(23 - f) = String$(f, 160) + Chr$(154) + " " + String$((11 - f), " ")
    screen$(23 - f) = screen$(23 - f) + String$((11 - f), " ") + Chr$(157)
    screen$(23 - f) = screen$(23 - f) + String$(f, 160)
  Next
End Sub

'----  corridors and walls
Sub Pass(n As Integer, st As Integer)
  Local Integer h, v(3), wb, xw, y, ys, ye
  Local ss$
  Select Case n
    Case 0 : ys = 0 : ye = 23 : xw = 1 : V(0) = 1 : V(2) = 25 : H = 0
    Case 1 : ys = 1 : ye = 22 : xw = 4 : V(0) = 2 : V(2) = 21 : H = 3
    Case 2 : ys = 5 : ye = 18 : xw = 3 : v(0) = 6 : V(2) = 18 : H = 2
    Case 3 : ys = 8 : ye = 15 : xw = 2 : v(0) = 9 : V(2) = 16 : H = 1
    Case 4 : ys = 10 : ye = 13 : xw = 1 : v(0) = 11 : v(2) = 15 : H = 0
    Case 5 : ys = 11 : ye = 12 : xw = 1 : V(0) = 12 : v(2) = 14 : H = 0
  End Select

  If st = 1 Then
    v(0) = v(0) + xw
    For y = ys To ye
      ss$ = grey$
      If (y <= ys + h) Or (y >= ye - h) Then ss$ = " "
      wb = V(2) - V(0)
      If wb Then Mid$(Screen$(y), V(0), wb) = String$(wb, Asc(ss$))
    Next
  Else
    If n < 5 Then
      For y = ys To ye
        ss$ = grey$
        If y <= ys + h Or y >= ye - h Then ss$ = " "
        Mid$(Screen$(y), V(st), xw) = String$(xw, Asc(ss$))
      Next
    Else
      Mid$(Screen$(ys), V(st), xw) = String$(xw, 136)
      Mid$(Screen$(ye), V(st), xw) = String$(xw, 137)
    EndIf
  EndIf
End Sub

Sub ext(num As Integer)
'Draw the Exit in front of the player
  Local Integer y, ch
  exi$ = Right$(exi$, el - 1) + Left$(exi$, 1)
  ch = Asc(Left$(exi$, 1))
  Select Case num
    Case 0
      For y = 1 To 22 : Mid$(Screen$(y), 2, 22) = String$(22, ch) : Next
    Case 1
      For y = 5 To 18 : Mid$(Screen$(y), 6, 15) = String$(15, ch) : Next
    Case 2
      For y = 8 To 15 : Mid$(Screen$(y), 9, 9) = String$(9, ch) : Next
    Case 3
      For y = 10 To 13 : Mid$(Screen$(y), 11, 5) = String$(5, ch) : Next
    Case 4
      Mid$(Screen$(11), 12, 3) = String$(3, ch)
      Mid$(Screen$(12), 12, 3) = String$(3, ch)
    Case 5
      Mid$(Screen$(11), 13, 1) = String$(1, ch)
      Mid$(Screen$(12), 13, 1) = String$(1, ch)
  End Select
End Sub

Sub shRex(n As Integer)
'show Rex
' (Insert Rex into the "Screenarray")
  Local Integer f, m, sx, sy, t, x, y
  Local s$, tm$
  n = n * 2
  Select Case(N - Rex.a)
    Case 10 : Restore rex5R
    Case 9 : Restore rex5L
    Case 8 : Restore rex4R
    Case 7 : Restore rex4L
    Case 6 : Restore rex3R
    Case 5 : Restore rex3L
    Case 4 : Restore rex2R
    Case 3 : Restore rex2L
    Case 2 : Restore rex1R
    Case 1 : Restore rex1L
    Case 0 : Restore rex0L
    Case - 1 : Restore rex0R
  End Select
  Read x, y : sx = 13 - x / 2 : sy = 11 - (y / 2)
  For m = sy To sy + y - 1
    Read tm$ : T = 1
    For f = sx + 1 To sx + x - 1
      s$ = Mid$(tm$, t, 1) : Inc t
      If s$ <> " " Then Mid$(Screen$(m), f, 1) = blk$(Val("&H" + s$))
    Next
  Next
End Sub

'===========================================
'  Rex movement "AI"
'===========================================

Sub Move_Rex
  'clear movement indicators
  Local Integer pdx, pdy, pd, scx, scy, scs, stp
  GAMEFLAGS = GAMEFLAGS And &b11110011
  pdx = Abs(rex.x - pp.x) : pdy = Abs(rex.y - pp.y)
  pd = Sqr((pdx * pdx) + (pdy * pdy))

  ' For debugging, distance of T-Rex from player.
  ' Text 240, 128, Str$(pd) + " "

 '-----------------------------------
 'choose the appropriate status message
  Select Case pd
    Case 9 To 100
      Status = 5
    Case 8
      Status = 6
    Case 3 To 7
      Status = 4
    Case 0 To 2
      Status = 2
  End Select
  'scan for direct line of sight (REX HAS SEEN YOU)
  scs = 0
  If pdx = 0 And Status <> 2 Then
    scs = 0
    stp = -1 : If rex.y < pp.y Then stp = 1
    For scy = rex.y To pp.y Step stp
      Inc scs, (gameMap(scy, Rex.x) = wall)
    Next
    If Not scs Then Status = 3
  EndIf

  If pdy = 0 And Status <> 2 Then
    scs = 0
    stp = -1 : If rex.x < pp.x Then stp = 1
    For scx = rex.x To pp.x Step stp
      Inc scs, (gameMap(Rex.y, scx) = wall)
    Next
    If Not scs Then Status = 3
  EndIf
  '-------------------

  'first try to move, depending on the player position
  'move WEST
  If Rex.x > pp.x Then
    If gameMap(Rex.y, Rex.x - 1) = None Then
      gameMap(Rex.y, Rex.x) = None : Inc Rex.x, -1
      gameMap(Rex.y, Rex.x) = 3
      GAMEFLAGS = GAMEFLAGS And &b11011111
      GAMEFLAGS = GAMEFLAGS Or &b00001100 : Exit Sub
    EndIf
  EndIf

  'move East
  If Rex.x < pp.x Then
    If gameMap(Rex.y, Rex.x + 1) = None Then
      gameMap(Rex.y, Rex.x) = None : Inc Rex.x
      gameMap(Rex.y, Rex.x) = 3
      GAMEFLAGS = GAMEFLAGS And &b11011111
      GAMEFLAGS = GAMEFLAGS Or &b00001100 : Exit Sub
    EndIf
  EndIf

  'move South
  If Rex.y < pp.y Then
    If gameMap(Rex.y + 1, Rex.x) = None Then
      gameMap(Rex.y, Rex.x) = None : Inc Rex.y
      gameMap(Rex.y, Rex.x) = 3
      GAMEFLAGS = GAMEFLAGS And &b11011111
      GAMEFLAGS = GAMEFLAGS Or &b00001100 : Exit Sub
    EndIf
  EndIf

  'move North
  If Rex.y > pp.y Then
    If gameMap(Rex.y - 1, Rex.x) = None Then
      gameMap(Rex.y, Rex.x) = None : Inc Rex.y, -1
      gameMap(Rex.y, Rex.x) = 3
      GAMEFLAGS = GAMEFLAGS And &b11011111
      GAMEFLAGS = GAMEFLAGS Or &b00001100 : Exit Sub
    EndIf
  EndIf

  ' no way?
  'then try a random move
  Select Case Int(Rnd * 8)
    Case 0
      'move North
      If gameMap(Rex.y - 1, Rex.x) = None Then
        gameMap(Rex.y, Rex.x) = None : Inc Rex.y, -1
        gameMap(Rex.y, Rex.x) = 3
        GAMEFLAGS = GAMEFLAGS And &b11011111 Or &b00001100 : Exit Sub
      EndIf
    Case 1
      'move South
      If gameMap(Rex.y + 1, Rex.x) = None Then
        gameMap(Rex.y, Rex.x) = None : Inc Rex.y
        gameMap(Rex.y, Rex.x) = 3
        GAMEFLAGS = GAMEFLAGS And &b11011111 Or &b00001100 : Exit Sub
      EndIf
    Case 2
      'move East
      If gameMap(Rex.y, Rex.x + 1) = None Then
        gameMap(Rex.y, Rex.x) = None : Inc Rex.x
        gameMap(Rex.y, Rex.x) = 3
        GAMEFLAGS = GAMEFLAGS And &b11011111 Or &b00001100 : Exit Sub
      EndIf
    Case 3
      'move WEST
      If gameMap(Rex.y, Rex.x - 1) = None Then
        gameMap(Rex.y, Rex.x) = None : Inc Rex.x, -1
        gameMap(Rex.y, Rex.x) = 3
        GAMEFLAGS = GAMEFLAGS And &b11011111 Or &b00001100 : Exit Sub
      EndIf
  End Select
  Status = 0
End Sub

Sub shexit(ex, ey, ew, eh)
'show exit (insert into screen$)
  Local Integer c, n, y, el, t
  el = Len(exi$)
  t = 1
  For y = 0 To eh
    n = ew - (y * 2)
    c = Asc(Mid$(exi$, t, 1)) : Inc t : If t > el Then t = 1
    For h = 0 To n
      Mid$(Screen$(ey + y), 20, n * 2) = String$(n * 2, c)
      'print @(y*16,(y+h)*12);string$(n*2,c)
    Next h
  Next y
  exi$ = Right$(exi$, el - 1) + Chr$(c)
End Sub

Sub on_quit()
  msgbox.beep(1)
  Font 7
  Const fg% = Rgb(White), bg% = Rgb(Black), frame% = Rgb(Rust), flags% = msgbox.NO_PAGES
  Local buttons$(1) Length 3 = ("Yes", "No")
  Const msg$ = "Quit game?"
  Const x% = 15, y% = 9, w% = 23, h% = 9, btn% = 1
  Const answer% = msgbox.show%(x%,y%,w%,h%,msg$,buttons$(),btn%,ctrl$,fg%,bg%,frame%,flags%)
  If buttons$(answer%) = "Yes" Then game.end()
  Font 9
End Sub

' Sub showMaze
'   Local Integer x, y
'   For y = 0 To mapSize.y
'     For x = 0 To mapSize.x : Print Chr$(32 + gameMap(y, x)); Chr$(32 + gameMap(y, x)); : Next
'     Print
'   Next
' End Sub

'===========================================
' Map Creation Subs
'===========================================

'Code to generate the Map, inspired by the explanation on
'https://softtangouk.wixsite.com/soft-tango-uk/3d-monster-maze
Sub generateMap
  Local Integer x, y, t, a, dr
  For y = 0 To mapSize.y : For x = 0 To mapSize.x: gameMap(y, x) = wall : Next : Next
  px = mapSize.x - 1 : py = mapSize.y - 1 : dr = 3 : gameMap(py, px) = 0 : t = 0
  Do
    a = Int(Rnd * 6) : Inc t, a : dr = Int(Rnd * 4)
    If py >= (mapSize.y) Then dr = 0
    If py < 2 Then dr = 2
    mkpath(dr, a)
  Loop While t < 1000
End Sub

Sub mkpath(dir As Integer, leng As Integer)
  ' dir $00=North, $01=West, $02=South, $03=East
  Local Integer mx, my
  Select Case Dir
    Case 0 : mx = 0 : my = -1
    Case 1 : mx = -1 : my = 0
    Case 2 : mx = 0 : my = 1
    Case 3 : mx = 1 : my = 0
  End Select
  Local Integer f
  For f = 1 To leng
    If Not(square(py, px)) Then gameMap(py, px) = None
    If (px + mx) > 0 And (px + mx) < mapSize.x Then Inc px, mx
    If (py + my) > 0 And (py + my) < mapSize.y Then Inc py, my
  Next
End Sub

Function square(sy As Integer, sx As Integer) As Integer
  'prevent to create a square with 4 free areas
  Local Integer tp
  If gameMap(sy, sx + 1) + gameMap(sy + 1, sx + 1) + gameMap(sy + 1, sx) = 0 Then Inc tp
  If gameMap(sy, sx + 1) + gameMap(sy - 1, sx + 1) + gameMap(sy - 1, sx) = 0 Then Inc tp
  If gameMap(sy, sx - 1) + gameMap(sy - 1, sx - 1) + gameMap(sy - 1, sx) = 0 Then Inc tp
  If gameMap(sy, sx - 1) + gameMap(sy + 1, sx - 1) + gameMap(sy + 1, sx) = 0 Then Inc tp
  square = 0 : If tp Then square = 1
End Function

Sub placeExit
  Local Integer rnd.y, rnd.x, nr
  Do
    rnd.y = 1 + Int(Rnd * 6) : rnd.x = 1 + Int(Rnd * 17)
  Loop Until gameMap(rnd.y, rnd.x) = 0
  gameMap(rnd.y, rnd.x) = 2
  'fill any empty space around the chosen tile until only 1 remains.
  'It does this in the priority order north, east, west and south.
  nr = (Not gameMap(rnd.y - 1, rnd.x)) + (Not gameMap(rnd.y, rnd.x - 1))
  nr = nr + (Not gameMap(rnd.y, rnd.x + 1)) + (Not gameMap(rnd.y + 1, rnd.x))
  If nr = 1 Then Exit Sub

  If Not gameMap(rnd.y - 1, rnd.x) Then
    gameMap(rnd.y - 1, rnd.x) = wall : Inc nr, -1
    If nr = 1 Then Exit Sub
  EndIf

  If Not gameMap(rnd.y, rnd.x + 1) Then
    gameMap(rnd.y, rnd.x + 1) = wall : Inc nr, -1
    If nr = 1 Then Exit Sub
  EndIf

  If Not gameMap(rnd.y, rnd.x - 1) Then
    gameMap(rnd.y, rnd.x - 1) = wall : Inc nr, -1
    If nr = 1 Then Exit Sub
  EndIf

  If Not gameMap(rnd.y + 1, rnd.x) Then
    gameMap(rnd.y + 1, rnd.x) = wall
  EndIf

  e.x = rnd.x : e.y = rnd.y
End Sub

Sub placeRex
  Local Integer rnd.y, rnd.x
  Do
    rnd.y = 1 + Int(Rnd * 4) : rnd.x = 1 + Int(Rnd * 17)
  Loop Until gameMap(rnd.y, rnd.x) = 0
  gameMap(rnd.y, rnd.x) = 3
  Rex.x = rnd.x : Rex.y = rnd.y
End Sub

Function get_input%(ignore%)
  Call ctrl$, get_input%
  If Not get_input% Then keys_cursor_ext(get_input%)
  If Not get_input% Then If ctrl.keydown%(105) Then get_input% = ctrl.B ' i = Instructions
  If ignore% Then Exit Function
  Select Case get_input%
    Case ctrl.START
      If state% = STATE_PLAY_GAME Then on_quit()
    Case ctrl.HOME, ctrl.SELECT
      on_quit()
  End Select
End Function

Sub clear_input()
  Do While get_input%(1) : Pause 5 : Loop
End Sub

'-------------------------------------
'Data for REX Pseudo"Sprites"
'-------------------------------------
'Bitblocks for Pseudo"Sprites"

'0=## 1=.# 2=#. 3=## 4=## 5=#. 6=.# 7=..
'  ##   ##   ##   #.   .#   ..   ..   .#

'8=.. 9=## A=.. B=#. C=.# D=#. E=.# F=..
'  #.   ..   ##   #.   .#   .#   #.   ..
' Space= Transparent

Blocks:
Data 160, 141, 142, 134, 147, 128, 129, 150
Data 131, 130, 140, 132, 148, 149, 133, 32
'Data 219,219,220,219,219,220,219,223,223,219,223,32 , 32,223, 32,220
'Data 220,32 ,223,223,220,220,219,32 , 32,219,223,220,220,223, 32, 32

'Sprites
REX0L:
Data 24, 22
Data "7AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA8"
Data "C000000009900003900000000000000B"
Data "C09035F00FFC500FFFCC00000000000B"
Data "6F70BFC50FFBF4BFF75C0BF90000000B"
Data "7F162F1F6BCFFC5F75FC05FEF400000B"
Data "6BBF48BFF65FF6AA5FFCBFA5F035F64B"
Data "F65FF65FFFFFFFFFFFF62EFFF05FF79F"
Data "FFFFFFFFFFFFFFFFFFFFFFFFC3F7E5FF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFC5A5FFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFF69FFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "7BF7AAFFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FB7034BFFFFFFFFFFFFFFFFFFFFFFFFF"
Data "CBE5FCBFFA8FFFFFFFFFFFFFFFFFFFFF"
Data "C05FFCF735BFFFFFFFFFFFFFFFFFFFFF"
Data "C002FB13FFBFFFFFFFFFFFFFFFFFFFFF"
Data "C000013FFFBF7148FFFFFFFFFFFFFFFF"
Data "C000002FFCF705FBF14FFFFFFFFFFFFF"
Data "C0000002AB705FCF13FB738F78FFAFFF"
Data "699999999995FF569FF56F5F56F6F5FF"

REX0R:
Data 23, 22
Data "0095FF100000000008F60B000"
Data "FFFF7100000000000FFF60100"
Data "AAA100000000000002FFFF995"
Data "00000000000000000028FFFFF"
Data "000000035F60003940000AAA8"
Data "0000000AFF7000FFF40000000"
Data "0000000000000002A10000000"
Data "0000000000000000000000000"
Data "0000394099903994000000000"
Data "9F0BFF10FFCCBFFB00FFF4000"
Data "F750F750FFBC2FCF40FFFB0FF"
Data "FEF68EFCAEFF0A5FC08FEF0FF"
Data "8BFF9FFF9FFF65FFF42EFF487"
Data "9FFFFFFFFFFFFFFFFFFFFFC25"
Data "FFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFF"
Data "FFFFFFFFFFFFFFFFFFFFFFFFF"
Data "F78FFFFFFFFFFFFFFFFFFFFFF"
Data "700FFFFFFFFFFFFFFFFFFFFFF"
Data "0B68FFFFFFFFFFFFFFFFFFFFF"
Data "3FFBFF7AFFFFFFFFFFFFFFFFF"
Data "2FFB7095BFF78FFFAFFFFFFFF"
Data "00A205FFCF7098FC3DF70DFF7"
Data "0000BFFFBF15FCF0F6815FDC5"

REX1L:
Data 15, 20
Data "     7AAA8      "
Data "    7000008     "
Data "   70000000F    "
Data "   10F000F0B    "
Data "  F00000000B    "
Data "  700030400B    "
Data " 700900000908   "
Data "F00BFD493EF02   "
Data "C0008FF9F7000F  "
Data "C000CFFFFB000B  "
Data "C000ADFFFB000B  "
Data "F40006AFEA000B  "
Data "F24000A9D00002  "
Data " 4240000000D00F "
Data " 6020000000D00F "
Data "  400300B10240F "
Data "  C00B000000B5  "
Data "7A000BC003C0B   "
Data "C0003 F95 C0028F"
Data "          F00008"
Data "           69995"
REX1R:
Data 12, 17
Data "    FAAA8    "
Data "   F10000B   "
Data "   C000000   "
Data "   C2C0083   "
Data "  730000008  "
Data " F006011302  "
Data " C008699700F "
Data " 1000FFF100F "
Data "70B002A1003F "
Data "C3C00000005  "
Data "C510000000B  "
Data " F0000000008F"
Data " 70000000000F"
Data " C0000006003F"
Data " F000000000BF"
Data "  C005009108F"
Data "7A000 65 999F"
Data "69995F       "
REX2L:
Data 9, 13
Data "   7002F  "
Data "  F0000B  "
Data "  F0403B  "
Data "  74000DF "
Data "  C840508F"
Data " F02FF102F"
Data " F002A0308"
Data "  60000B42"
Data "  70000080"
Data " 700000000"
Data " 00400300B"
Data " 40F401005"
Data "A03 60B03F"
Data "      F428"
REX2R:
Data 7, 11, "  1008 ", "  202B ", " C903B ", "F0BF70F", "F00A00B"
Data "C300005", "CD0002 ", " 100002", "7000B10", " 40C040", "A0B    "
REX3L:
Data 6, 9, " 7008 ", " 012B ", " D998 ", " 0210F", " 4000B"
Data " 1000F", "C0000F", "A0640F", "   695"
REX3R:
Data 4, 6, " 02 ", " 43 ", "C21F", "3008", "C00B", "1099"
REX4L:
Data 3, 4, "C0F ", "AD8F", "004F", "10B ", " F9F"
REX4R:
Data 3, 4, " 00 ", " 00 ", "0000", "0000", "00F0"
REX5L:
Data 3, 3, " 00 ", "0000", "0000", "0F00"
REX5R:
Data 3, 2, "CB8F", "405F", "09FF"
ringmaster0:
Data "A0A0A0868293A0A0A0A0"
Data "A0A0A0842094A0A0A0A0"
Data "A0A08C2D2D2D8C98A0A0"
Data "A0A0A02B4F2BA09920A0"
Data "A0A0A0833D96A08620A0"
Data "A0A086818C80938A2093"
Data "A0802020202020492094"
Data "84202020832020202094"
Data "84492020A02096202094"
Data "84492096808420958CA0"
Data "844920852A952094A0A0"
Data "8496852A2A2A9594A0A0"
Data "84492A2A2A2A2A93A0A0"
Data "8E2B2A2A2A2A2A94A0A0"
Data "A0832A2A492A2A90A0A0"
Data "A0842A2A492A2AA0A0A0"
Data "A0842A2A492A2AA0A0A0"
Data "A0842A2A8E2A2AA0A0A0"
Data "A0842A2AA02A2AA0A0A0"
Data "A0A02A96A02A96A0A0A0"
Data "82828081822082828282"
rmb:
Data "A0A0A0A0A0A0A0A0A0A0"
Data "A0A0A0A0A0A0A0A0A0A0"
Data "A0A0A0A0A0A0A0A0A0A0"
Data "A0A0A0A098A0A0A0A0A0"
Data "A0A0A0979797A0A0A0A0"
Data "A0A0A0979797A0A0A0A0"
Data "A080209399868193A0A0"
Data "842020204F202020A0A0"
Data "84209685828C962094A0"
Data "844984202020932094A0"
Data "84498420209680208DA0"
Data "8496858C96809690A0A0"
Data "84492A2A95928081A0A0"

text_intro:
' --------------------------
' Introduction Text Messages
' --------------------------

Data "   ROLL UP, ROLL UP,  ", " SEE THE AMAZING      ", " TYRANNOSAURUS REX    "
Data " KING OF THE DINOSAURS", " IN HIS LAIR.         ", " PERFECTLY PRESERVED  "
Data " IN SILICON SINCE     ", " PREHISTORIC TIMES,   ", " HE IS BROUGHT TO     "
Data " YOU FOR YOUR         ", " ENTERTAINMENT AND    ", " EXHILARATION.        "
Data "   IF YOU DARE TO     ", " ENTER HIS LAIR, YOU  ", " DO SO AT YOUR OWN    "
Data " RISK. THE MANAGEMENT ", " ACCEPT NO            ", " RESPONSIBILITY FOR   "
Data " THE HEALTH AND SAFETY", " OF THE ADVENTURER WHO", " ENTERS HIS REALM.    "
Data " THE MANAGEMENT ADVISE", " THAT THIS IS NOT A   ", " GAME FOR THOSE OF A  "
Data " NERVOUS DISPOSITION. ", "   IF YOU ARE IN ANY  ", " DOUBT THEN PRESS     "
Data " esc, IF              ", " INSTRUCTIONS ARE     ", " NEEDED TO PROCEED    "
Data " THEN PRESS 'i'       ", " OTHERWISE PRESS      ", " space                ", "@"

text_empty:
Data "                      ", "                      ", "                      ", "@"

text_instructions:
' ----------------------
' Controls Text Messages
' ----------------------

Data " THE CURSOR CONTROLS ", " YOU REQUIRE ARE:-    ", "  LEFT TO TURN LEFT   "
Data "  UP TO MOVE FORWARD  ", "  RIGHT TO TURN RIGHT ", " FURTHER INFORMATION  "
Data " IS PROVIDED DURING   ", " THE ENCOUNTER.       ", "   FOR EACH MOVE      "
Data " SCORING IS AS FOLLOWS", "   5 PTS-WHILE HE IS  ", "         TRACKING YOU."
Data " 200 PTS-IF YOU ESCAPE", "         HIS LAIR.    ", "   SINCE REX IS ALWAYS"
Data " TRYING TO MOVE       ", " TOWARDS HIS PREY, A  ", " SKILFUL ADVENTURER   "
Data " CAN CONTROL THE      ", " MONSTERS MOVEMENTS TO", " IMPROVE HIS SCORE.   "
Data " THE ESCAPE ROUTE,    ", " WHICH IS AT THE END  ", " OF A CUL-DE-SAC, IS  "
Data " VISIBLE UP TO 5 MOVES", " AWAY.                ", "   THE GAME ENDS WHEN "
Data " HE CATCHES YOU.      ", " IF YOU ESCAPE A NEW  ", " MAZE IS GENERATED AND"
Data " YOUR PREVIOUS SCORE  ", " CARRIED FORWARD.     ", "                      "

text_mist:
' ----------------------------
' 'Mist of Time' Text Messages
' ----------------------------
Data "   THE MISTS OF TIME  ", " WILL PASS OVER YOU   ", " FOR SOME SECONDS     "
Data " WHILE TRANSPORTING   ", " YOU TO THE LAIR OF   ", " TYRANNOSAURUS REX.   "
Data "   BEST OF LUCK.....  ", "", "", "", "", "@"

text_win:
Data 0, "YOU HAVE ELUDED HIM AND"
Data 0, "SCORED score POINTS."
Data 0, "REX IS VERY ANGRY."
Data 0, "YOU'LL NEED MORE"
Data 0, "LUCK THIS TIME."
Data -1, ""

text_lose:
Data 0, "YOU HAVE BEEN"
Data 0, "POSTHUMOUSLY AWARDED score"
Data 0, "POINTS AND SENTENCED"
Data 0, "TO ROAM THE MAZE FOREVER."
Data 4, "IF YOU WISH TO APPEAL"
Data 8, "PRESS esc, ELSE  "
Data 13, "PRESS space."
Data -1, ""
'
' font_ZX2
' Font type    : Full (224 Characters)
' Font size    : 8x8 pixels
' Memory usage : 1792
Sub SFnt
  Cls
  Font 9
  Print "  ";
  x = 0
  r = 2
  For f = 0 To 15 : Print Hex$(F, 1); " "; : Next : Print : Print Hex$(R, 1); " "; : Inc r
  For f = 32 To 255
    Print Chr$(f); " "; : Inc x : If x > 15 Then x = 0 : Print : Print Hex$(R, 1); " "; : Inc r
  Next
End Sub

DefineFont #9
  E0200808
  FFFFFFFF FFFFFFFF EFEFEFFF FFEFFFEF FFDBDBFF FFFFFFFF DB81DBFF FFDB81DB
  D7C1F7FF F7C1F5C1 F79B9DFF FFB9D9EF EFD7EFFF FFC5BBD5 FFEFF7FF FFFFFFFF
  F7F7FBFF FFFBF7F7 EFEFDFFF FFDFEFEF F7EBFFFF FFEBF7C1 F7F7FFFF FFF7F7C1
  FFFFFFFF EFF7F7FF FFFFFFFF FFFFFFC1 FFFFFFFF FFE7E7FF FBFDFFFF FFDFEFF7
  B5B9C3FF FFC39DAD F7D7E7FF FFC1F7F7 FDBDC3FF FF81BFC3 F3BDC3FF FFC3BDFD
  D7E7F7FF FFF781B7 83BF81FF FFC3BDFD 83BFC3FF FFC3BDBD FBFD81FF FFEFEFF7
  C3BDC3FF FFC3BDBD BDBDC3FF FFC3FDC1 EFFFFFFF FFEFFFFF FFEFFFFF DFEFEFFF
  F7FBFFFF FFFBF7EF C1FFFFFF FFFFC1FF F7EFFFFF FFEFF7FB FBBDC3FF FFF7FFF7
  A9B5C3FF FFC3BFA1 BDBDC3FF FFBDBD81 83BD83FF FF83BDBD BFBDC3FF FFC3BDBF
  BDBB87FF FF87BBBD 83BF81FF FF81BFBF 83BF81FF FFBFBFBF BFBDC3FF FFC3BDB1
  81BDBDFF FFBDBDBD F7F7C1FF FFC1F7F7 FDFDFDFF FFC3BDBD 8FB7BBFF FFBDBBB7
  BFBFBFFF FF81BFBF A599BDFF FFBDBDBD AD9DBDFF FFBDB9B5 BDBDC3FF FFC3BDBD
  BDBD83FF FFBFBF83 BDBDC3FF FFC3B5AD BDBD83FF FFBDBB83 C3BFC3FF FFC3BDFD
  EFEF01FF FFEFEFEF BDBDBDFF FFC3BDBD BDBDBDFF FFE7DBBD BDBDBDFF FFDBA5BD
  E7DBBDFF FFBDDBE7 D7BB7DFF FFEFEFEF F7FB81FF FF81DFEF F7F7F1FF FFF1F7F7
  DFBFFFFF FFFBF7EF EFEF8FFF FF8FEFEF ABC7EFFF FFEFEFEF FFFFFFFF 00FFFFFF
  87DDE3FF FF81DFDF FBC7FFFF FFC3BBC3 C3DFDFFF FFC3DDDD DFE3FFFF FFE3DFDF
  C3FBFBFF FFC3BBBB BBC7FFFF FFC3BF87 E7EFF3FF FFEFEFEF BBC3FFFF C7FBC3BB
  87BFBFFF FFBBBBBB CFFFEFFF FFC7EFEF FBFFFBFF E7DBFBFB CFD7DFFF FFDBD7CF
  EFEFEFFF FFF3EFEF AB97FFFF FFABABAB BB87FFFF FFBBBBBB BBC7FFFF FFC7BBBB
  BB87FFFF BFBF87BB BBC3FFFF F9FBC3BB DFE3FFFF FFDFDFDF BFC7FFFF FF87FBC7
  EFC7EFFF FFF3EFEF BBBBFFFF FFC7BBBB BBBBFFFF FFEFD7D7 ABBBFFFF FFD7ABAB
  D7BBFFFF FFBBD7EF BBBBFFFF C7FBC3BB F783FFFF FF83DFEF CFF7F1FF FFF1F7F7
  F7F7F7FF FFF7F7F7 F3EF8FFF FF8FEFEF FFD7EBFF FFFFFFFF 5E66BDC3 C3BD665E
  0F0F0F0F FFFFFFFF F0F0F0F0 FFFFFFFF 00000000 FFFFFFFF FFFFFFFF 0F0F0F0F
  0F0F0F0F 0F0F0F0F F0F0F0F0 0F0F0F0F 00000000 0F0F0F0F AA55AA55 AA55AA55
  FFFFFFFF AA55AA55 AA55AA55 FFFFFFFF 0F0F0F0F 0F0F0F0F 00000000 0F0F0F0F
  FFFFFFFF 00000000 F0F0F0F0 00000000 0F0F0F0F 00000000 00000000 00000000
  F0F0F0F0 00000000 0F0F0F0F 00000000 FFFFFFFF 00000000 00000000 F0F0F0F0
  F0F0F0F0 F0F0F0F0 0F0F0F0F F0F0F0F0 FFFFFFFF F0F0F0F0 55AA55AA 55AA55AA
  00000000 55AA55AA 55AA55AA 00000000 07030100 7F3F1F0F F0F8FCFE 0080C0E0
  0F1F3F7F 00010307 E0C08000 FEFCF8F0 0129C7C7 FFC7EF29 0183C7EF FFC7EF83
  00000000 00000000 10101000 00100010 00242400 00000000 247E2400 00247E24
  283E0800 083E0A3E 08646200 00462610 10281000 003A442A 00100800 00000000
  08080400 00040808 10102000 00201010 08140000 0014083E 08080000 0008083E
  00000000 10080800 00000000 0000003E 00000000 00181800 04020000 00201008
  4A463C00 003C6252 08281800 003E0808 02423C00 007E403C 0C423C00 003C4202
  28180800 00087E48 7C407E00 003C4202 7C403C00 003C4242 04027E00 00101008
  3C423C00 003C4242 42423C00 003C023E 10000000 00100000 00100000 20101000
  08040000 00040810 3E000000 00003E00 08100000 00100804 04423C00 00080008
  564A3C00 003C405E 42423C00 0042427E 7C427C00 007C4242 40423C00 003C4240
  42447800 00784442 7C407E00 007E4040 7C407E00 00404040 40423C00 003C424E
  7E424200 00424242 08083E00 003E0808 02020200 003C4242 70484400 00424448
  40404000 007E4040 5A664200 00424242 52624200 0042464A 42423C00 003C4242
  42427C00 0040407C 42423C00 003C4A52 42427C00 0042447C 3C403C00 003C4202
  1010FE00 00101010 42424200 003C4242 42424200 00182442 42424200 00245A42
  18244200 00422418 28448200 00101010 08047E00 007E2010 08080E00 000E0808
  20400000 00040810 10107000 00701010 54381000 00101010 00000000 FF000000
  78221C00 007E2020 04380000 003C443C 3C202000 003C2222 201C0000 001C2020
  3C040400 003C4444 44380000 003C4078 18100C00 00101010 443C0000 38043C44
  78404000 00444444 30001000 00381010 04000400 18240404 30282000 00242830
  10101000 000C1010 54680000 00545454 44780000 00444444 44380000 00384444
  44780000 40407844 443C0000 06043C44 201C0000 00202020 40380000 00780438
  10381000 000C1010 44440000 00384444 44440000 00102828 54440000 00285454
  28440000 00442810 44440000 38043C44 087C0000 007C2010 30080E00 000E0808
  08080800 00080808 0C107000 00701010 00281400 00000000 A199423C 3C4299A1
End DefineFont
