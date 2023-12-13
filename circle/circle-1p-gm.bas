'_Circle-One for Game*Mite 5.07.08
'_An original game for the PicoMite VGA and Game*Mite.
' Copyright (c) 2023 @Volhout

Option Base 0
Option Default Float
Option Explicit On

#Include "../splib/system.inc"
#Include "../splib/ctrl.inc"
#Include "../splib/gamemite.inc"

sys.override_break("break_cb")

Const CURRENT_PATH$ = Choice(Mm.Info(Path) <> "NONE", Mm.Info(Path), Cwd$)
Const CW = Rgb(White)

' Index 0 is food, 1 is player 1, 2 is player 2.
Dim c(2) ' Colour.
Dim dx(2), dy(2) ' Direction of movement in x & y directions.
Dim p(2) ' Controller value; boolean OR of ctrl.DOWN|LEFT|RIGHT|UP.
Dim r(2) ' Radius.
Dim s(2) ' Speed.
Dim u(2) ' Score.
Dim v(2) ' > 0 if player moving.
Dim x(2), y(2) ' Coordinates.

ctrl.init_keys()
'!dynamic_call ctrl.gamemite
Dim ctrl$ = "ctrl.gamemite"
Call ctrl$, ctrl.OPEN

' show_intro()

' The game uses the FrameBuffer to prevent screen drawing artifacts.
FrameBuffer Create
FrameBuffer Write f

' Game music.
Play ModFile CURRENT_PATH$ + "circle.mod"

start_round(1)
Dim t%
Do
  t% = Timer + 100
  draw_food(c(0))
  ctrl_player()
  ctrl_ai()
  erase_players()
  move_players()
  handle_collisions()
  draw_players()
  handle_winning()
  draw_score()
  FrameBuffer Copy F, N, B
  Do While Timer < t% : Loop
Loop
end_program()

Sub start_round(first%)
  If first% Then
    u(1) = 0 : u(2) = 0
  Else
    Text Mm.HRes / 2, 30 + Mm.VRes / 2, "A=continue, B=stop", "CM", 1, 1, Rgb(Yellow)
    FrameBuffer Copy f, n, b

    Do
      Pause 50
    Loop While Pin(Gp14) + Pin(Gp15) = 2
  EndIf

  Cls

  Const SIZE = Mm.HRes / 40
  x(0) = Mm.HRes / 2 : y(0) = Mm.VRes / 3 : r(0) = SIZE : c(0) = Rgb(Green)
  x(1) = Mm.HRes / 3 : y(1) = 2 * Mm.VRes / 3 : r(1) = SIZE : c(1) = Rgb(Blue) : s(1) = 5  'player speed, tweak
  x(2) = 2 * Mm.HRes / 3 : y(2) = 2 * Mm.VRes / 3 : r(2) = SIZE : c(2) = Rgb(Red) : s(2) = 3  'AI speed, tweak
End Sub

Sub create_food()
  draw_food(0) ' Erase old food.
  x(0) = Mm.HRes * Rnd()
  y(0) = Mm.VRes * Rnd()
End Sub

Sub show_intro()
  Cls
  Text Mm.HRes / 2, Mm.VRes / 2 - 50, "CIRCLE ONE", "CM", 1, 2, Rgb(Yellow)
  Pause 2000
  Text Mm.HRes / 2, Mm.VRes / 2, "a single player game", "CM", 1, 1, Rgb(Yellow)
  Pause 1500
  Text Mm.HRes / 2, Mm.VRes / 2 + 20, "for Game*Mite", "CM", 1, 1, Rgb(Yellow)
  Pause 1500
  Text Mm.HRes / 2, Mm.VRes / 2 + 40, "Arrow keys to control", "CM", 1, 1, Rgb(Yellow)
  Pause 1500
  Box 0, Mm.VRes / 2 - 10, Mm.HRes, Mm.VRes, , 0, 0
  Pause 1500
  Text Mm.HRes / 2, Mm.VRes / 2, "A to sprint, B to stop", "CM", 1, 1, Rgb(Yellow)
  Pause 1500
  Text Mm.HRes / 2, Mm.VRes / 2 + 20, "eat apples to grow and win", "CM", 1, 1, Rgb(Yellow)
  Pause 1500
  Text Mm.HRes / 2, Mm.VRes / 2 + 40, "avoid collisions !!", "CM", 1, 1, Rgb(Yellow)
  Pause 1500
  Text Mm.HRes / 2, Mm.VRes / 2 + 60, "--- GET READY ---", "CM", 1, 1, Rgb(Yellow)
  Pause 2000
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
  If key% And ctrl.A Then s(1) = 12 ' Turbo run, tweak for fun.
  If key% And ctrl.B Then Exit Sub
  If Not p(1) Then p(1) = p_old%
  If s(1) > 5 Then Inc s(1), -1 ' Slow player if turbo-running.
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

    ' Allow wrap around.
    Inc x(i%), Choice(x(i%) < 0, Mm.HRes, Choice(x(i%) > Mm.HRes, -Mm.HRes, 0))
    Inc y(i%), Choice(y(i%) < 0, Mm.VRes, Choice(y(i%) > Mm.VRes, -Mm.VRes, 0))
  Next
End Sub

Sub handle_collisions()
  ' Calculate distances.
  Local d12 = Sqr((x(1) - x(2)) ^ 2 + (y(1) - y(2)) ^ 2)
  Local d10 = Sqr((x(1) - x(0)) ^ 2 + (y(1) - y(0)) ^ 2)
  Local d20 = Sqr((x(0) - x(2)) ^ 2 + (y(0) - y(2)) ^ 2)

  ' Game rules:
  '  - collision between players is punished.
  '  - player who moves is culprit.
  If d12 < (r(1) + r(2)) Then
    If v(1) > 0 Then r(1) = r(1) / 1.5
    If v(2) > 0 Then r(2) = r(2) / 1.5
    r(1) = Max(r(1), 3)
    r(2) = Max(r(2), 3)
  EndIf

  ' You eat, you grow.
  If d10 < (r(1) + r(0)) Then r(1) = r(1) * 2 : create_food()
  If d20 < (r(0) + r(2)) Then r(2) = r(2) * 2 : create_food()
End Sub

Sub draw_players()
  Static counter% = 0
  Local i%, dyy, dxx, vv
  Inc counter%, 1
  For i% = 1 To 2
    ' Draw body.
    Circle x(i%), y(i%), r(i%), , , c(i%), c(i%)
    If v(i%) > 0 Then
      ' Draw eyes when moving.
      vv = 0.7 + (v(i%) = 1) * 0.3 'sqrt 2 if 45 degrees
      dyy = 6 * dy(i%) : dxx = 6 * dx(i%)
      Circle x(i%) + vv * ((dx(i%) * r(i%)) - dyy), y(i%) + vv * ((dy(i%) * r(i%)) + dxx), 5, , , CW, CW
      Circle x(i%) + vv * ((dx(i%) * r(i%)) + dyy), y(i%) + vv * ((dy(i%) * r(i%)) - dxx), 5, , , CW, CW
      Circle x(i%) + vv * ((dx(i%) * (r(i%) + 2) - dyy)), y(i%) + vv * ((dy(i%) * (r(i%) + 2)) + dxx), 2, , , 9, 9
      Circle x(i%) + vv * ((dx(i%) * (r(i%) + 2) + dyy)), y(i%) + vv * ((dy(i%) * (r(i%) + 2)) - dxx), 2, , , 0, 0
    Else
      ' Draw eyes when sleepy.
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

Sub handle_winning()
  Local i%, s$
  For i% = 1 To 2
    If r(i%) > Mm.VRes / 2 Then
      s$ = Choice(i% = 1, "Blue", "Red") + " Wins"
      Text Mm.HRes / 2, Mm.VRes / 2, s$, "CM", 1, 1, Rgb(Yellow)
      Inc u(i%)
      start_round()
      Exit For
    EndIf
  Next
End Sub

Sub draw_score()
  Text 0, 0, Str$(u(1)), "LT", 1, 1, Rgb(Blue)
  Text Mm.HRes, 0, Str$(u(2)), "RT", 1, 1, Rgb(Red)
End Sub

'!dynamic_call break_cb
Sub break_cb()
  end_program(1)
End Sub

Sub end_program(break%)
  If Not break% Then
    Cls
    Text Mm.HRes / 2, Mm.VRes / 2, "Bye", "CM", 1, 1, Rgb(Yellow)
    FrameBuffer Copy F, N, B
    Pause 2000
  EndIf
  If sys.is_device%("gamemite") Then
    gamemite.end(break%)
  Else
    Page Write 0
    Colour Rgb(White), Rgb(Black)
    Cls
    sys.restore_break()
    ctrl.term_keys()
    On Error Ignore
    Call ctrl$, ctrl.CLOSE
    On Error Abort
    End
  EndIf
End Sub
