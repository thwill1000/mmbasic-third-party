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

Const CURRENT_PATH$ = Choice(Mm.Info(Path) <> "", Mm.Info(Path), Cwd$)

ctrl.init_keys()
'!dynamic_call ctrl.gamemite
Dim ctrl$ = "ctrl.gamemite"
Call ctrl$, ctrl.OPEN

'initial values for the players and the target food
'target (green) index 0, player (keyboard - blue) index 1
'right player (AI - red) index 2
Const SIZE = Mm.HRes / 40
Const CW = Rgb(White)

'x,y are coordinates, r=radius, c=color, s=speed
Dim c(2), dx(2), dy(2), p(2), r(2), s(2), u(2), v(2), x(2), y(2)

'intro and start score reset
' intro_screen()
u(1) = 0 : u(2) = 0  'scores player and AI

'the game uses framebuffer to prevent screen drawing artefacts
FrameBuffer Create
FrameBuffer Write f

'initial target
'food
'counter = 0

'game music
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
  If Not first% Then
    Text Mm.HRes / 2, 30 + Mm.VRes / 2, "A=continue, B=stop", "CM", 1, 1, Rgb(Yellow)
    FrameBuffer Copy f, n, b

    Do
      Pause 50
    Loop While Pin(Gp14) + Pin(Gp15) = 2
  EndIf

  Cls

  x(0) = Mm.HRes / 2 : y(0) = Mm.VRes / 3 : r(0) = SIZE : c(0) = Rgb(Green)
  x(1) = Mm.HRes / 3 : y(1) = 2 * Mm.VRes / 3 : r(1) = SIZE : c(1) = Rgb(Blue) : s(1) = 5  'player speed, tweak
  x(2) = 2 * Mm.HRes / 3 : y(2) = 2 * Mm.VRes / 3 : r(2) = SIZE : c(2) = Rgb(Red) : s(2) = 3  'AI speed, tweak
End Sub

'seed new green circle
Sub create_food()
  draw_food(0) ' Erase old food
  x(0) = Mm.HRes * Rnd()
  y(0) = Mm.VRes * Rnd()
End Sub

Sub intro_screen()
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
  If key% And ctrl.A Then s(1) = 12 ' Turbo run, tweak for fun
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
  Circle x(1), y(1), r(1) + 10, , , 0, 0
  Circle x(2), y(2), r(2) + 10, , , 0, 0
End Sub

Sub move_players()
  v(1) = 0 : v(2) = 0 : dx(1) = 0 : dx(2) = 0 : dy(1) = 0 : dy(2) = 0

'move players
  If (p(1) And ctrl.LEFT) Then Inc v(1), 1 : Inc x(1), -s(1) : dx(1) = -1
  If (p(1) And ctrl.RIGHT) Then Inc v(1), 1 : Inc x(1), s(1) : dx(1) = 1
  If (p(1) And ctrl.UP) Then Inc v(1), 1 : Inc y(1), -s(1) : dy(1) = -1
  If (p(1) And ctrl.DOWN) Then Inc v(1), 1 : Inc y(1), s(1) : dy(1) = 1
  If (p(2) And ctrl.LEFT) Then Inc v(2), 1 : Inc x(2), -s(2) : dx(2) = -1
  If (p(2) And ctrl.RIGHT) Then Inc v(2), 1 : Inc x(2), s(2) : dx(2) = 1
  If (p(2) And ctrl.UP) Then Inc v(2), 1 : Inc y(2), -s(2) : dy(2) = -1
  If (p(2) And ctrl.DOWN) Then Inc v(2), 1 : Inc y(2), s(2) : dy(2) = 1

'allow wrap around
  If x(1) < 0 Then Inc x(1), Mm.HRes
  If x(1) > Mm.HRes Then Inc x(1), - Mm.HRes
  If y(1) < 0 Then Inc y(1), Mm.VRes
  If y(1) > Mm.VRes Then Inc y(1), - Mm.VRes
End Sub

Sub handle_collisions()
'calculate distances
  Local d12 = Sqr((x(1) - x(2)) ^ 2 + (y(1) - y(2)) ^ 2)
  Local d10 = Sqr((x(1) - x(0)) ^ 2 + (y(1) - y(0)) ^ 2)
  Local d20 = Sqr((x(0) - x(2)) ^ 2 + (y(0) - y(2)) ^ 2)

'game rules, collision between players is punished
'player who moves is culprit
  If d12 < (r(1) + r(2)) Then
    If v(1) > 0 Then r(1) = r(1) / 1.5
    If v(2) > 0 Then r(2) = r(2) / 1.5
    r(1) = Max(r(1), 3)
    r(2) = Max(r(2), 3)
  EndIf

'you eat, you grow....
  If d10 < (r(1) + r(0)) Then r(1) = r(1) * 2 : create_food()
  If d20 < (r(0) + r(2)) Then r(2) = r(2) * 2 : create_food()
End Sub

Sub draw_players()
  Static counter = 0
  Local dyy, dxx, vv
  Inc counter, 1
'if r(1)>50 then c(1)=rgb(cyan) else c(1)=rgb(blue)
'draw player
  If v(1) > 0 Then
'draw player 1 body
    Circle x(1), y(1), r(1), , , c(1), c(1)
'eyes when moving
    vv = 0.7 + (v(1) = 1) * 0.3 'sqrt 2 if 45 degrees
    dyy = 6 * dy(1) : dxx = 6 * dx(1)
    Circle x(1) + vv * ((dx(1) * r(1)) - dyy), y(1) + vv * ((dy(1) * r(1)) + dxx), 5, , , cw, cw
    Circle x(1) + vv * ((dx(1) * r(1)) + dyy), y(1) + vv * ((dy(1) * r(1)) - dxx), 5, , , cw, cw
    Circle x(1) + vv * ((dx(1) * (r(1) + 2) - dyy)), y(1) + vv * ((dy(1) * (r(1) + 2)) + dxx), 2, , , 9, 9
    Circle x(1) + vv * ((dx(1) * (r(1) + 2) + dyy)), y(1) + vv * ((dy(1) * (r(1) + 2)) - dxx), 2, , , 0, 0
  Else
'draw player 1 body
    Circle x(1), y(1), r(1), , , c(1), c(1)
'not moving, eyes sleepy
    Circle x(1) + 6, y(1) + 2, 5, , , cw, cw
    Circle x(1) - 6, y(1) + 2, 5, , , cw, cw
    Circle x(1) + 6, y(1) - 1, 5, , , c(1), c(1)
    If counter And 28 Then
      Circle x(1) - 6, y(1) + 4, 2, , , 0, 0
    Else
      Circle x(1) - 6, y(1) - 1, 5, , , c(1), c(1)
    EndIf
  EndIf

'if r(2)>50 then c(1)=rgb(cyan) else c(1)=rgb(blue)
'draw player
  If v(2) > 0 Then
'draw player 1 body
    Circle x(2), y(2), r(2), , , c(2), c(2)
'eyes when moving
    vv = 0.7 + (v(2) = 1) * 0.3 'sqrt 2 if 45 degrees
    dyy = 6 * dy(2) : dxx = 6 * dx(2)
    Circle x(2) + vv * ((dx(2) * r(2)) - dyy), y(2) + vv * ((dy(2) * r(2)) + dxx), 5, , , cw, cw
    Circle x(2) + vv * ((dx(2) * r(2)) + dyy), y(2) + vv * ((dy(2) * r(2)) - dxx), 5, , , cw, cw
    Circle x(2) + vv * ((dx(2) * (r(2) + 2) - dyy)), y(2) + vv * ((dy(2) * (r(2) + 2)) + dxx), 2, , , 9, 9
    Circle x(2) + vv * ((dx(2) * (r(2) + 2) + dyy)), y(2) + vv * ((dy(2) * (r(2) + 2)) - dxx), 2, , , 0, 0
  Else
'draw player 1 body
    Circle x(2), y(2), r(2), , , c(2), c(2)
'not moving, eyes sleepy
    Circle x(2) + 6, y(2) + 2, 5, , , cw, cw
    Circle x(2) - 6, y(2) + 2, 5, , , cw, cw
    Circle x(2) + 6, y(2) - 1, 5, , , c(2), c(2)
    If counter + 14 And 30 Then
      Circle x(2) - 6, y(2) - 1, 5, , , c(2), c(2)
    Else
      Circle x(2) - 6, y(2) + 4, 2, , , 0, 0
    EndIf
  EndIf
End Sub

Sub handle_winning()
  If r(1) > Mm.VRes / 2 Then
    Text Mm.HRes / 2, Mm.VRes / 2, "Blue Wins", "CM", 1, 1, Rgb(Yellow)
    Inc u(1)
    start_round()
  EndIf
  If r(2) > Mm.VRes / 2 Then
    Text Mm.HRes / 2, Mm.VRes / 2, "Red Wins", "CM", 1, 1, Rgb(Yellow)
    Inc u(2)
    start_round()
  EndIf
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
