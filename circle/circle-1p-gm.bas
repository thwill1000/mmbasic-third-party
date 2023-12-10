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
Dim x0, x1, x2, y0, y1, y2, r0, r1, r2, c0, c1, c2, s0, s1, s2
Dim u1, u2, p0, p1, p2
Dim v1, v2, dx1, dx2, dy1, dy2

'intro and start score reset
' intro_screen()
u1 = 0 : u2 = 0  'scores player and AI

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
  draw_food(c0)
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
    Loop While Pin(gp14) + Pin(gp15) = 2

'    If Pin(gp14) = 0 Then end_program()
  EndIf

  Cls

  x0 = Mm.HRes / 2 : y0 = Mm.VRes / 3 : r0 = SIZE : c0 = Rgb(Green)
  x1 = Mm.HRes / 3 : y1 = 2 * Mm.VRes / 3 : r1 = SIZE : c1 = Rgb(Blue) : s1 = 5  'player speed, tweak
  x2 = 2 * Mm.HRes / 3 : y2 = 2 * Mm.VRes / 3 : r2 = SIZE : c2 = Rgb(Red) : s2 = 3  'AI speed, tweak
End Sub

'seed new green circle
Sub create_food()
  draw_food(0) ' Erase old food
  x0 = Mm.HRes * Rnd()
  y0 = Mm.VRes * Rnd()
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
  Circle x0 - 4, y0 - 2, r0, , , c%, c%
  Circle x0 + 4, y0, r0, , , c%, c%
  Line x0 - 3, y0, x0 + 5, y0 - 2 * r0, 1, c%
End Sub

Sub ctrl_player()
  Local key%, p_old% = p1
  Call ctrl$, key%
  p1 = key% And (ctrl.DOWN Or ctrl.LEFT Or ctrl.RIGHT Or ctrl.UP)
  If key% And ctrl.A Then s1 = 12 ' Turbo run, tweak for fun
  If key% And ctrl.B Then Exit Sub
  If Not p1 Then p1 = p_old%
  If s1 > 5 Then Inc s1, -1 ' Slow player if turbo-running.
End Sub

Sub ctrl_ai()
  Local AIx = Int((x0 - x2) / 2), AIy = Int((y0 - y2) / 2)
  p2 = 0
  If Abs(AIx) >= Abs(AIy) Then
    p2 = p2 Or Choice(AIx < 0, ctrl.LEFT, ctrl.RIGHT)
  EndIf
  If Abs(AIx) <= Abs(AIy) Then
    p2 = p2 Or Choice(AIy < 0, ctrl.UP, ctrl.DOWN)
  EndIf
End Sub

Sub erase_players()
  Circle x1, y1, r1 + 10, , , 0, 0
  Circle x2, y2, r2 + 10, , , 0, 0
End Sub

Sub move_players()
  v1 = 0 : v2 = 0 : dx1 = 0 : dx2 = 0 : dy1 = 0 : dy2 = 0

'move players
  If (p1 And ctrl.LEFT) Then Inc v1, 1 : Inc x1, -s1 : dx1 = -1
  If (p1 And ctrl.RIGHT) Then Inc v1, 1 : Inc x1, s1 : dx1 = 1
  If (p1 And ctrl.UP) Then Inc v1, 1 : Inc y1, -s1 : dy1 = -1
  If (p1 And ctrl.DOWN) Then Inc v1, 1 : Inc y1, s1 : dy1 = 1
  If (p2 And ctrl.LEFT) Then Inc v2, 1 : Inc x2, -s2 : dx2 = -1
  If (p2 And ctrl.RIGHT) Then Inc v2, 1 : Inc x2, s2 : dx2 = 1
  If (p2 And ctrl.UP) Then Inc v2, 1 : Inc y2, -s2 : dy2 = -1
  If (p2 And ctrl.DOWN) Then Inc v2, 1 : Inc y2, s2 : dy2 = 1

'allow wrap around
  If x1 < 0 Then x1 = x1 + Mm.HRes
  If x1 > Mm.HRes Then x1 = x1 - Mm.HRes
  If y1 < 0 Then y1 = y1 + Mm.VRes
  If y1 > Mm.VRes Then y1 = y1 - Mm.VRes
End Sub

Sub handle_collisions()
'calculate distances
  Local d12 = Sqr((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
  Local d10 = Sqr((x1 - x0) ^ 2 + (y1 - y0) ^ 2)
  Local d20 = Sqr((x0 - x2) ^ 2 + (y0 - y2) ^ 2)

'game rules, collision between players is punished
'player who moves is culprit
  If d12 < (r1 + r2) Then
    If v1 > 0 Then r1 = r1 / 1.5
    If v2 > 0 Then r2 = r2 / 1.5
    r1 = Max(r1, 3)
    r2 = Max(r2, 3)
  EndIf

'you eat, you grow....
  If d10 < (r1 + r0) Then r1 = r1 * 2 : create_food()
  If d20 < (r0 + r2) Then r2 = r2 * 2 : create_food()
End Sub

Sub draw_players()
  Static counter = 0
  Local dy, dx, v
  Inc counter, 1
'if r1>50 then c1=rgb(cyan) else c1=rgb(blue)
'draw player
  If v1 > 0 Then
'draw player 1 body
    Circle x1, y1, r1, , , c1, c1
'eyes when moving
    v = 0.7 + (v1 = 1) * 0.3 'sqrt 2 if 45 degrees
    dy = 6 * dy1 : dx = 6 * dx1
    Circle x1 + v * ((dx1 * r1) - dy), y1 + v * ((dy1 * r1) + dx), 5, , , cw, cw
    Circle x1 + v * ((dx1 * r1) + dy), y1 + v * ((dy1 * r1) - dx), 5, , , cw, cw
    Circle x1 + v * ((dx1 * (r1 + 2) - dy)), y1 + v * ((dy1 * (r1 + 2)) + dx), 2, , , 9, 9
    Circle x1 + v * ((dx1 * (r1 + 2) + dy)), y1 + v * ((dy1 * (r1 + 2)) - dx), 2, , , 0, 0
  Else
'draw player 1 body
    Circle x1, y1, r1, , , c1, c1
'not moving, eyes sleepy
    Circle x1 + 6, y1 + 2, 5, , , cw, cw
    Circle x1 - 6, y1 + 2, 5, , , cw, cw
    Circle x1 + 6, y1 - 1, 5, , , c1, c1
    If counter And 28 Then
      Circle x1 - 6, y1 + 4, 2, , , 0, 0
    Else
      Circle x1 - 6, y1 - 1, 5, , , c1, c1
    EndIf
  EndIf

'if r2>50 then c1=rgb(cyan) else c1=rgb(blue)
'draw player
  If v2 > 0 Then
'draw player 1 body
    Circle x2, y2, r2, , , c2, c2
'eyes when moving
    v = 0.7 + (v2 = 1) * 0.3 'sqrt 2 if 45 degrees
    dy = 6 * dy2 : dx = 6 * dx2
    Circle x2 + v * ((dx2 * r2) - dy), y2 + v * ((dy2 * r2) + dx), 5, , , cw, cw
    Circle x2 + v * ((dx2 * r2) + dy), y2 + v * ((dy2 * r2) - dx), 5, , , cw, cw
    Circle x2 + v * ((dx2 * (r2 + 2) - dy)), y2 + v * ((dy2 * (r2 + 2)) + dx), 2, , , 9, 9
    Circle x2 + v * ((dx2 * (r2 + 2) + dy)), y2 + v * ((dy2 * (r2 + 2)) - dx), 2, , , 0, 0
  Else
'draw player 1 body
    Circle x2, y2, r2, , , c2, c2
'not moving, eyes sleepy
    Circle x2 + 6, y2 + 2, 5, , , cw, cw
    Circle x2 - 6, y2 + 2, 5, , , cw, cw
    Circle x2 + 6, y2 - 1, 5, , , c2, c2
    If counter + 14 And 30 Then
      Circle x2 - 6, y2 - 1, 5, , , c2, c2
    Else
      Circle x2 - 6, y2 + 4, 2, , , 0, 0
    EndIf
  EndIf
End Sub

Sub handle_winning()
  If r1 > Mm.VRes / 2 Then
    Text Mm.HRes / 2, Mm.VRes / 2, "Blue Wins", "CM", 1, 1, Rgb(Yellow)
    u1 = u1 + 1
    start_round()
  EndIf
  If r2 > Mm.VRes / 2 Then
    Text Mm.HRes / 2, Mm.VRes / 2, "Red Wins", "CM", 1, 1, Rgb(Yellow)
    u2 = u2 + 1
    start_round()
  EndIf
End Sub

Sub draw_score()
  Text 0, 0, Str$(u1), "LT", 1, 1, Rgb(Blue)
  Text Mm.HRes, 0, Str$(u2), "RT", 1, 1, Rgb(Red)
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
