'_Circle-One for Game*Mite (PicoMite 5.08.00)
' Copyright (c) 2023-2024 @Volhout
' Titivated for Game*Mite by Thomas Hugo Williams

Option Base 0
Option Default Float
Option Explicit On

Const VERSION = 101301 ' 1.1.1

'!define NO_INCLUDE_GUARDS
#Include "../splib/system.inc"
#Include "../splib/ctrl.inc"
#Include "../splib/gamemite.inc"

sys.override_break("break_cb")

Const CURRENT_PATH$ = Choice(Mm.Info(Path) <> "NONE", Mm.Info(Path), Cwd$)
Const CB = Rgb(Blue), CC= Rgb(Cyan),   CG = Rgb(Green)
Const CR = Rgb(Red),  CW = Rgb(White), CY = Rgb(Yellow)
Const VERSION_STRING$ = "Game*Mite Version " + sys.format_version$(VERSION)

' Index 0 is food, 1 is player 1, 2 is player 2
Dim c(2) ' Colour
Dim dx(2), dy(2) ' Direction of movement in x & y directions
Dim pause_flag%  ' = 1 then pause the game
Dim p(2) ' Player input; bitset of ctrl.DOWN|LEFT|RIGHT|UP
Dim r(2) ' Radius
Dim s(2) ' Speed
Dim score(2)
Dim t%
Dim v(2) ' > 0 if player moving
Dim x(2), y(2) ' Coordinates

ctrl.init_keys()
'!dynamic_call ctrl.gamemite
Dim ctrl$ = "ctrl.gamemite"
Call ctrl$, ctrl.OPEN

' Game music
Play ModFile CURRENT_PATH$ + "circle.mod"

' The game uses the FrameBuffer to prevent screen drawing artifacts
FrameBuffer Create
FrameBuffer Write F

Do
  show_intro()
  score(1) = 0 : score(2) = 0
  start_round()
  Do
    t% = Timer + 100
    If Not c(0) Then create_food()
    If c(0) Then draw_food(c(0))
    ctrl_player()
    ctrl_ai()
    erase_players()
    move_players()
    handle_collisions()
    draw_players()
    If Not handle_winning%() Then Exit Do
    draw_score()
    If pause_flag% Then
      If Not handle_pause%() Then Exit Do
    EndIf
    FrameBuffer Copy F, N, B
    Do While Timer < t% : Loop
  Loop
Loop
Error "Unexpected program end"

'!dynamic_call break_cb
Sub break_cb()
  end_program(1)
End Sub

Sub end_program(break%)
  If Not break% Then
    Cls
    Text Mm.HRes / 2, Mm.VRes / 2 - 10, "Bye!", "CM", 8, 2, CY
    FrameBuffer Copy F, N, B
    Pause 2000
  EndIf
'!if defined(GAMEMITE)
  '!uncomment_if true
  ' gamemite.end(break%)
  '!endif
'!else
  If sys.is_platform%("gamemite") Then
    gamemite.end(break%)
  Else
    Page Write 0
    Colour CW, 0
    Cls
    sys.restore_break()
    ctrl.term_keys()
    On Error Ignore
    Call ctrl$, ctrl.CLOSE
    On Error Abort
    End
  EndIf
'!endif
End Sub

Sub show_intro()
  Cls
  Const k% = display_text%("intro_data", Mm.VRes / 2 - 80, 0, 1000)
  If k% = ctrl.SELECT Then end_program()
End Sub

intro_data:
Data "CIRCLE ONE", 2, CY, 17
Data "2023-2024 @Volhout", 1, CC, 13
Data "<version>", 1, CG, 17
Data "", 1, CW, 17
Data "Eat apples to grow and win", 1, CW, 17
Data "Use arrow keys to steer", 1, CW, 17
Data "A to sprint, START to pause", 1, CW, 17
Data "Avoid collisions !!", 1, CW, 17
Data "", 1, CW, 17
Data "Press START to play", 1, CY, 17
Data "or SELECT to quit", 1, CY, 17
Data "", 0, 0, 0

' Reads and displays text from DATA statements
'
' @param   label$     Label for the DATA to read
' @param   top%       Initial y-coordinate
' @param   key_mask%  Key/button mask for exiting
' @param   msec%      Pause duration between showing each line of text
' @return  the controller code for the key/button pressed
Function display_text%(label$, top%, key_mask%, msec%)
  Const _key_mask% = Choice(key_mask%, key_mask%, ctrl.SELECT Or ctrl.START)
  Local col%, dy%, h%, s$, sz%, t%, w%, y% = top%
  Local k% = Not msec%, k_old%
  Call ctrl$, k_old%
  Restore label$
  Do
    Read s$, sz%, col%, dy%
    If Not sz% Then Exit Do
    If s$ = "<version>" Then s$ = VERSION_STRING$
    w% = Len(s$) * 8 * sz% + 4
    h% = 8 * sz% + 4
    If Len(s$) Then Box (Mm.HRes - w%) / 2, y% - h% / 2, w%, h%, 1, 0, 0
    Text Mm.HRes / 2, y%, s$, "CM", 8, sz%, col% : Inc y%, dy%
    If k% Then Continue Do ' Pressing a key interrupts the PAUSE-ing.
    FrameBuffer Wait
    FrameBuffer Copy F, N
    t% = Timer + msec%
    Do While (Timer < t%) And (Not k%)
      Call ctrl$, k%
      ' Require the user to have released key or be pressing different key.
      If k% = k_old% Then k% = 0 Else k_old% = k%
    Loop
  Loop
  FrameBuffer Wait
  FrameBuffer Copy F, N
  ctrl.wait_until_idle(ctrl$)
  Do : Call ctrl$, k% : Loop Until k% And _key_mask%
  ctrl.wait_until_idle(ctrl$)
  display_text% = k%
End Function

Sub start_round()
  Cls
  Const SIZE = Mm.HRes / 40
  x(0) = Mm.HRes / 2 : y(0) = Mm.VRes / 3 : r(0) = SIZE : c(0) = CG
  x(1) = Mm.HRes / 3 : y(1) = 2 * Mm.VRes / 3 : r(1) = SIZE : c(1) = CB : s(1) = 5  'player speed, tweak
  x(2) = 2 * Mm.HRes / 3 : y(2) = 2 * Mm.VRes / 3 : r(2) = SIZE : c(2) = CR : s(2) = 3  'AI speed, tweak
End Sub

' Creates new food in a random location. If the result is too
' close to a player then do not create food on this call.
Sub create_food()
  x(0) = Mm.HRes * Rnd()
  y(0) = Mm.VRes * Rnd()
  Const d10 = Sqr((x(1) - x(0)) ^ 2 + (y(1) - y(0)) ^ 2)
  Const d20 = Sqr((x(2) - x(0)) ^ 2 + (y(2) - y(0)) ^ 2)
  If d10 < (r(1) + r(0) + 20) Then Exit Sub
  If d20 < (r(0) + r(2) + 20) Then Exit Sub
  c(0) = CG
End Sub

Sub draw_food(c%)
  Circle x(0) - 4, y(0) - 2, r(0), , , c%, c%
  Circle x(0) + 4, y(0), r(0), , , c%, c%
  Line x(0) - 3, y(0), x(0) + 5, y(0) - 2 * r(0), 1, c%
End Sub

Sub ctrl_player()
  Local key%, p_old% = p(1)
  Call ctrl$, key%
  p(1) = key% And (ctrl.DOWN Or ctrl.LEFT Or ctrl.RIGHT Or ctrl.UP)
  If key% And ctrl.A Then s(1) = 12 ' Turbo run, tweak for fun
  If key% And ctrl.START Then pause_flag% = 1
  If Not p(1) Then p(1) = p_old%
  If s(1) > 5 Then Inc s(1), -1 ' Slow player if turbo-running
End Sub

Sub ctrl_ai()
  Local AIx% = Int((x(0) - x(2)) / 2), AIy% = Int((y(0) - y(2)) / 2)
  p(2) = 0
  If Abs(AIx%) > 1 Then p(2) = p(2) Or Choice(AIx% < 0, ctrl.LEFT, ctrl.RIGHT)
  If Abs(AIy%) > 1 Then p(2) = p(2) Or Choice(AIy% < 0, ctrl.UP, ctrl.DOWN)
End Sub

Sub erase_players()
  Local i%
  For i% = 1 To 2
    Circle x(i%), y(i%), r(i%) + 10, , , 0, 0
  Next
End Sub

Sub move_players()
  Local i%
  For i% = 1 To 2
    v(i%) = 0 : dx(i%) = 0 : dy(i%) = 0

    If p(i%) And ctrl.LEFT  Then Inc v(i%) : Inc x(i%), -s(i%) : dx(i%) = -1
    If p(i%) And ctrl.RIGHT Then Inc v(i%) : Inc x(i%),  s(i%) : dx(i%) =  1
    If p(i%) And ctrl.UP    Then Inc v(i%) : Inc y(i%), -s(i%) : dy(i%) = -1
    If p(i%) And ctrl.DOWN  Then Inc v(i%) : Inc y(i%),  s(i%) : dy(i%) =  1

    ' Allow wrap around
    Inc x(i%), Choice(x(i%) < 0, Mm.HRes, Choice(x(i%) > Mm.HRes, -Mm.HRes, 0))
    Inc y(i%), Choice(y(i%) < 0, Mm.VRes, Choice(y(i%) > Mm.VRes, -Mm.VRes, 0))
  Next
End Sub

Sub handle_collisions()
  ' Calculate distances
  Const d12 = Sqr((x(1) - x(2)) ^ 2 + (y(1) - y(2)) ^ 2)
  Const d10 = Sqr((x(1) - x(0)) ^ 2 + (y(1) - y(0)) ^ 2)
  Const d20 = Sqr((x(2) - x(0)) ^ 2 + (y(2) - y(0)) ^ 2)

  ' Game rules:
  '  - collision between players is punished
  '  - player who moves is culprit
  If d12 < (r(1) + r(2)) Then
    If v(1) > 0 Then r(1) = r(1) / 1.5
    If v(2) > 0 Then r(2) = r(2) / 1.5
    r(1) = Max(r(1), 3)
    r(2) = Max(r(2), 3)
  EndIf

  ' You eat, you grow
  If c(0) Then
    If d10 < (r(1) + r(0)) Then eat_food(1)
    If d20 < (r(0) + r(2)) Then eat_food(2)
  EndIf
End Sub

Sub eat_food(p%)
  r(p%) = r(p%) * 2
  draw_food(0)
  c(0) = 0
End Sub

Sub draw_players()
  Static counter% = 0
  Local i%, dyy, dxx, vv
  Inc counter%, 1
  For i% = 1 To 2
    ' Draw body
    Circle x(i%), y(i%), r(i%), , , c(i%), c(i%)
    If v(i%) > 0 Then
      ' Draw eyes when moving
      vv = 0.7 + (v(i%) = 1) * 0.3 'sqrt 2 if 45 degrees
      dyy = 6 * dy(i%) : dxx = 6 * dx(i%)
      draw_circle(x(i%) + vv * ((dx(i%) * r(i%)) - dyy), y(i%) + vv * ((dy(i%) * r(i%)) + dxx), 5, CW)
      draw_circle(x(i%) + vv * ((dx(i%) * r(i%)) + dyy), y(i%) + vv * ((dy(i%) * r(i%)) - dxx), 5, CW)
      draw_circle(x(i%) + vv * ((dx(i%) * (r(i%) + 2) - dyy)), y(i%) + vv * ((dy(i%) * (r(i%) + 2)) + dxx), 2)
      draw_circle(x(i%) + vv * ((dx(i%) * (r(i%) + 2) + dyy)), y(i%) + vv * ((dy(i%) * (r(i%) + 2)) - dxx), 2)
    Else
      ' Draw eyes when sleepy
      Circle x(i%) + 6, y(i%) + 2, 5, , , CW, CW
      Circle x(i%) - 6, y(i%) + 2, 5, , , CW, CW
      Circle x(i%) + 6, y(i%) - 1, 5, , , c(i%), c(i%)
      If (counter% + Choice(i% = 1, 0, 14)) And Choice(i% = 1, 28, 30) Then
        Circle x(i%) - 6, y(i%) + 4, 2, , , 0, 0
      Else
        Circle x(i%) - 6, y(i%) - 1, 5, , , c(i%), c(i%)
      EndIf
    EndIf
  Next
End Sub

' Draws circle whilst working around strange clipping behaviour when circle
' goes off screen.
Sub draw_circle(x%, y%, r%, col%)
  Select Case x%
    Case < 0 - r%, >= Mm.HRes + r%: Exit Sub
  End Select
  Select Case y%
    Case < 0 - r%, >= Mm.VRes + r%: Exit Sub
  End Select
  Circle x%, y%, r%, , , col%, col%
End Sub

' @return  0  to return to the intro screen
Function handle_winning%()
  handle_winning% = 1
  Local win%
  If r(1) > Mm.VRes / 2 Then win% = 1
  If (Not win%) And (r(2) > Mm.VRes / 2) Then win% = 2
  If Not win% Then Exit Function

  Inc score(win%)
  draw_score()
  Const label$ = "win_" + Str$(win%) + "_data"
  Const k% = display_text%(label$, Mm.VRes / 2 - 30)
  If k% = ctrl.SELECT Then
    handle_winning% = 0
  Else
    start_round()
  EndIf
End Function

win_1_data:
Data "Blue Wins", 2, CY, 17
Data "", 1, 0, 17
Data "Press START to continue", 1, CY, 17
Data "or SELECT to quit", 1, CY, 17
Data "", 0, 0, 0

win_2_data:
Data "Red Wins", 2, CY, 17
Data "", 1, 0, 17
Data "Press START to continue", 1, CY, 17
Data "or SELECT to quit", 1, CY, 17
Data "", 0, 0, 0

Sub draw_score()
  Text 0, 0, Str$(score(1)), "LT", 8, 2, CB
  Text Mm.HRes, 0, Str$(score(2)), "RT", 8, 2, CR
End Sub

' @return  0  to return to the intro screen
Function handle_pause%()
  handle_pause% = 1
  pause_flag% = 0
  Const k% = display_text%("pause_data", Mm.VRes / 2 - 30)
  If k% = ctrl.SELECT Then
    handle_pause% = 0
  Else
    Cls
  EndIf
End Function

pause_data:
Data "PAUSED", 2, CY, 17
Data "", 1, 0, 17
Data "Press START to continue", 1, CY, 17
Data "or SELECT to quit", 1, CY, 17
Data "", 0, 0, 0

' Konami Style Font (Martin H.)
' Font type    : Full (95 ChArACtErs)
' Font size    : 8x8 pixels
' Memory usage : 764 Bytes
DefineFont #8
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
