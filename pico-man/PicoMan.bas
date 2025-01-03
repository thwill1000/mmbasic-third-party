'-------------------------------------------------------------------------------
' PicoMan
' A PacMan look alike game for the PicoGamer and Game*Mite
' (c) 2023 Geoff Graham
'
' v1.1.0 - Original release           - by Geoff Graham, 2023
' v1.2.0 - Adapted for MMB4L Gamepack - by Tom Williams, Dec 2024
'-------------------------------------------------------------------------------

Option Default Integer
Option Explicit
Option Base 0

Const VERSION = 102300 ' 1.2.0

'!dynamic_call ctrl.gamemite

If Mm.Device$ = "MMB4L" Then
  Option Simulate "Game*Mite"
  Option CodePage CMM2
EndIf

#Include "../splib/system.inc"
#Include "../splib/ctrl.inc"
#Include "../splib/game.inc"

'!dynamic_call game.on_break
sys.override_break("game.on_break")
game.init_window("Pico-Man", VERSION)
ctrl.init_keys()
Dim ctrl$ = ctrl.default_driver$()
If ctrl.open_no_error%(ctrl$) <> sys.SUCCESS Then ctrl$ = "ctrl.no_controller"

Const cWall = RGB(Blue)
Const cDot = RGB(White)
Const cSuper = RGB(155, 255, 126)
Const cMan = RGB(Yellow)
Const WallWidth = 2
Const OrangeLevel = 1000
Const BonusLevel = 3000
Const ManR = 9
Const ax = 18, ay = 18

Dim i, TestMode
If MM.Device$ = "MMBasic for Windows" Then TestMode = 1

If TestMode = 1 Then
  Const xMargin = 200, yMargin = 200
  Dim   ManS = 25        ' Speed of the PacMan
  Dim   GhostS = 35      ' Speed of the Ghosts
  Dim   GhostSlow = 45   ' Speed of the Ghosts when the power ball is active
Else
  Const xMargin = 10, yMargin = 2
  Font 2
  Dim   ManS = 18        ' Speed of the PacMan, Examples: 10 = fast, 18 = normal, 25 = slow
  Dim   GhostS = 19      ' Speed of the Ghosts
  Dim   GhostSlow = 23   ' Speed of the Ghosts when the power ball is active
  FrameBuffer Create
EndIf

Dim x, y, j, k, l, mx, my, tt
Dim a(ax, ay), d(16, 16, 16)
Dim GhostX(3), GhostY(3), GhostDirection(3), GAction(3), GInGate(3)
Dim GhostColour(3) = (RGB(red), RGB(magenta), RGB(green), RGB(cyan))
Dim ManX, ManY, ManDirection, NextManDirection = 4
Dim ManMouth = ManR, ManMouthDx = -1
Dim NextM, NextG, GTimer = -1, GColour = 0
Dim KillMode, NbrDots, VulnerableGhosts, NbrMen
Dim OrangeX, OrangeY, NextOrange
Dim Score, BonusPacMan, Level
Dim Float note

' used to turn a direction number into x & y directions
Dim ddx(4) = (0, 1, 0, -1, 0), ddy(4) = (-1, 0, 1, 0, 0)

' various sounds
Dim Float ack(4) = (987.77, 1567.98, 1975.53, 30.87, 0)
Dim Float nack(5) = (1046.50, 987.77, 739.99, 698.46, 30.87, 0)
Dim Float bigack(11) = (400, 500, 600, 700, 800, 900, 1000, 1500, 2000, 2500, 300, 0)

Randomize  Timer

LoadData  ' load the initial board layout

For x = 0 To 16
  For y = 0 To 16
    For i = 0 To 16
      Read d(x, y, i)  ' load the shortest path database
    Next i
  Next y
Next x

StartGame:
If TestMode <> 1 Then StartScreen
Score = 0
NbrMen = 3
NextOrange = OrangeLevel
OrangeX = 0 : OrangeY = 0
BonusPacMan = BonusLevel
Level = 0

NewLevel:
LoadData

LostALife:
' scramble the ghost colours to prevent repition
For i = 1 To 10
  j = Int(Rnd * 4) : k = Int(Rnd * 4)
  l = GhostColour(j) : GhostColour(j) = GhostColour(k) : GhostColour(k) = l
Next i

' draw the ghosts on the screen and save them as a BLIT object
CLS
For i = 0 To 3
  CreateGhost i, 1, -4, GhostColour(i)   ' normal, eyes left
  CreateGhost i, 2, +4, GhostColour(i)   ' normal, eyes right
  CreateGhost i, 3, -4, RGB(LightGrey)   ' grey, eyes left
  CreateGhost i, 4, +4, RGB(LightGrey)   ' grey, eyes right
  CreateGhost i, 5, -4, RGB(white)       ' white, eyes left
  CreateGhost i, 6, +4, RGB(white)       ' white, eyes right
Next i

DrawBoard

' display the right panel
Box ax * 13 + 2 + xMargin, yMargin, 320, 240, 1, 0, 0
Text 275 + xMargin, 40 + yMargin, "Score", CM
Text 275 + xMargin, 60 + yMargin, Str$(Score), CM
Text 275 + xMargin, 160 + yMargin, "Level", CM
Text 275 + xMargin, 180 + yMargin, Str$(Level + 1), CM

DrawSpareMen

' count down to the start of play
If TestMode <> 1 Then
  FrameBuffer Copy N , F
  Play Tone 1000, 1000, 60
  Text 128, 120, "3", CM, 6, 2, RGB(white), -1
  Pause 1000
  FrameBuffer Copy F , N
  Play Tone 1000, 1000, 60
  Text 128, 120, "2", CM, 6, 2, RGB(white), -1
  Pause 1000
  FrameBuffer Copy F , N
  Play Tone 1000, 1000, 60
  Text 128, 120, "1", CM, 6, 2, RGB(white), -1
  Pause 1000
  FrameBuffer Copy F , N
EndIf

' starting position and direction for the PacMan
ManY = (13 * 13) : ManX = (10 * 13) : ManDirection = 3 : NextManDirection = 3

Timer = 0 : NextM = 0 : NextG = 0 : KillMode = 0

' starting position for the ghosts
For i = 0 To 3
  InitGhost i
  GInGate(i) = Timer + 750 + (i * 750)
Next i

' this is the main program loop
Do
  If OrangeX + OrangeY <> 0 Then Circle OrangeX * 13 + xMargin, OrangeY * 13 + yMargin, ManR - 3, 1, 1, 0, RGB(orange)
  If Timer > NextM Then
    ' animate and draw the PacMan
    NextM = Timer + ManS
'    if Level < 4 Then NextM = NextM - Level * 3 Else NextM = NextM - 15
    ManX = ManX + ddx(ManDirection) : ManY = ManY + ddy(ManDirection)
    Circle ManX + xMargin, ManY + yMargin, ManR, 1, 1, 0, cMan
    If ManDirection = 3 Then
      Triangle ManX + xMargin, ManY + yMargin, ManX - ManR + xMargin, ManY - ManMouth + yMargin, ManX - ManR + xMargin, ManY + ManMouth + yMargin, 0, 0
    ElseIf ManDirection = 1 Then
      Triangle ManX + xMargin, ManY + yMargin, ManX + ManR + xMargin, ManY - ManMouth + yMargin, ManX + ManR + xMargin, ManY + ManMouth + yMargin, 0, 0
    ElseIf ManDirection = 0 Then
      Triangle ManX + xMargin, ManY + yMargin, ManX - ManMouth + xMargin, ManY - ManR + yMargin, ManX + ManMouth + xMargin, ManY - ManR + yMargin, 0, 0
    ElseIf ManDirection = 2 Then
      Triangle ManX + xMargin, ManY + yMargin, ManX - ManMouth + xMargin, ManY + ManR + yMargin, ManX + ManMouth + xMargin, ManY + ManR + yMargin, 0, 0
    EndIf

    ' animate the PacMan's mouth
    ManMouth = ManMouth + ManMouthDx
    If ManMouth  < 2 Then ManMouthDx = 1
    If ManMouth  > ManR Then ManMouthDx = -1

      ' check if any change of direction is requested
      i = Timer
      Select Case get_input%()
        Case ctrl.UP
          NextManDirection = 0
        Case ctrl.DOWN
          NextManDirection = 2
        Case ctrl.LEFT
          NextManDirection = 3
        Case ctrl.RIGHT
          NextManDirection = 1
        Case ctrl.A
          Inc ManS, -1 : Inc GhostS, -1 : Inc GhostSlow, -1
        Case ctrl.B
          Inc ManS, 1 : Inc GhostS, 1 : Inc GhostSlow, 1
        Case ctrl.SELECT
          If QuitCheck() Then GoTo StartGame
        Case ctrl.START
          Do While get_input%() : Pause 30 : Loop
          Do While get_input%() <> ctrl.START : Pause 30 : Loop
          Timer = i
      End Select

    ' we have reached the center of a cell
    If ManX Mod 13 = 0 And ManY Mod 13 = 0 Then
      Play Stop
      x = ManX\13 : y = ManY\13
      i = a(x, y)
      If i Then
        If i = 1 Then
          Play Tone 1000, 1000, 2
          Score = Score + 10 : NbrDots = NbrDots - 1
          ' there are 133 dots in a full board, when they are all eaten we advance a level
          If NbrDots <= 0 Then
            Level = Level + 1
            If Level < 5 Then ManS = ManS - 2 : GhostS  = GhostS - 2 : GhostSlow = GhostSlow - 2
            Text 128, 120, "Level", CB, 5, 2, RGB(white), 0
            Text 128, 120, Str$(Level + 1), CT, 5, 2, RGB(white), 0
            Pause 3000
            GoTo NewLevel
          EndIf
        EndIf
        If i = 2 Then
          Play Tone 600, 1000, 100
          Score = Score + 50
          KillMode = Timer + 6000
          VulnerableGhosts = 4
          ' give the ghosts a target that is as far away as possible
          For j = 0 To 3
            k = GhostX(j)\13 : l = GhostY(j)\13
            If k <= x And l <= y Then GAction(j) = ((Int(Rnd * 8) + 1) << 8) + 1
            If k > x And l <= y Then  GAction(j) = ((Int(Rnd * 8) + 10) << 8) + 1
            If k > x And l > y Then   GAction(j) = ((Int(Rnd * 8) + 10) << 8) + &h11
            If k <= x And l > y Then  GAction(j) = ((Int(Rnd * 8) + 1) << 8) + &h11
          Next j
        EndIf
        If i = 4 Then i = 0 : Score = Score + 300 : OrangeX = 0 : OrangeY = 0 : NextOrange = Score + OrangeLevel :  Play Stop : Play Tone 400, 400, 60

        a(x, y) = 0
      EndIf

      If Score > BonusPacMan Then
        BonusPacMan = BonusPacMan + BonusLevel
        If NbrMen < 4 Then NbrMen = NbrMen + 1 : DrawSpareMen
      EndIf

      If Score > NextOrange And OrangeX + OrangeY = 0 Then
        NextOrange = Score + OrangeLevel
        Do
          i = GetRandom(&b1111)
        Loop While a(i >> 8, i And &hFF) <> 0
        OrangeX = i >> 8 : OrangeY = i And &hFF
        a(OrangeX, OrangeY) = &b100
        Circle OrangeX * 13 + xMargin, OrangeY * 13 + yMargin, ManR - 3, 1, 1, 0, RGB(orange)
      EndIf

      ' check if the change of direction is valid
      i = x + ddx(NextManDirection) : j = y + ddy(NextManDirection)
      If  i >= 0 And i <= 18 And j >= 0 And j <= 18 Then
        If (a(i, j) And &h30) = 0 Then ManDirection = NextManDirection
      EndIf

      If x = 0 And ManDirection = 3 Then
        ' exit left
        Circle ManX + xMargin, ManY + yMargin, ManR, 1, 1, 0, 0
        x = ax : ManX = x * 13
      ElseIf x = ax And ManDirection = 1 Then
        ' exit right
        Circle ManX + xMargin, ManY + yMargin, ManR, 1, 1, 0, 0
        x = 0 : ManX = 0
      EndIf

      ' stop moving if we have hit a wall, this causes the PacMan to bob back and forth
      If a(x + ddx(ManDirection), y + ddy(ManDirection)) And &h30 Then ManX = ManX - ddx(ManDirection) * 3 : ManY = ManY - ddy(ManDirection) * 4

      Text 275 + xMargin, 60 + yMargin, Str$(Score), CM
    EndIf
  EndIf


  If Timer > NextG Then
    ' animate and draw the Ghosts
    For i = 0 To 3
      GhostX(i) = GhostX(i) + ddx(GhostDirection(i))
      GhostY(i) = GhostY(i) + ddy(GhostDirection(i))

      If GhostX(i) Mod 13 = 0 And GhostY(i) Mod 13 = 0 Then
        ' the ghost has reached the center of a cell
        x = GhostX(i)\13 : y = GhostY(i)\13
        mx = ManX\13 : my = ManY\13

        ' if we have moved over a dot then replace it
        k = x - ddx(GhostDirection(i)) : l = y - ddy(GhostDirection(i))
        j = a(k, l) And &h0F
        If j = 1 Then Line (k * 13) - 1 + xMargin, (l * 13) + yMargin, (k * 13) + 1 + xMargin, (l * 13) + yMargin, 3, cDot
        If j = 2 Then Circle (k * 13) + xMargin, (l * 13) + yMargin, 3, 0, 1, cDot, cDot
        If j = 4 Then Circle (k * 13) + xMargin, (l * 13) + yMargin, ManR - 3, 1, 1, 0, RGB(Orange)

        ' if the ghost was aimed at a target and reached it we can cancel the target
        If GAction(i) = (x << 8) + y Then GAction(i) = 0

        ' set a target for the ghost
        If KillMode <= Timer  Then
          ' note that the nbr of dots on a full screen is 133
          If i = 0 Or (i = 1 And NbrDots < 88) Or (i = 2 And NbrDots < 58) Or (i = 3 And NbrDots < 28) Then
            GAction(i) = 0  ' the ghost is targeting the PacMan
          Else
            If GAction(i) = 0 Then GAction(i) = GetRandom(&b1111) '  the ghost is free to roam
          EndIf
        Else
          ' run away
          If GAction(i) = 0 Then GAction(i) = GetRandom(GetQuadrant(x, y))
          If GInGate(i) > Timer Then GInGate(i) = KillMode + i*500
        EndIf

        ' if it is not banished to the gate set the direction for the ghost based on its target
        If GInGate(i) < Timer Then
          If mx > 0 And mx < 18 Then
            If GAction(i) <> 0 Then
              ' the ghost has a specific target square, set a direction towards it
              GhostDirection(i) = GetDirection(x, y, GAction(i) >> 8, GAction(i) And &hFF)
            Else
              ' if our action is chasing the PacMan then set a direction for him
              ' ghosts 0 and 3 will aim directly, 1 will aim four squares in front and 2 similar but behind
              If (i = 0 Or i = 3) Then GhostDirection(i) = (d(x-1, y-1, mx-1) >> ((my - 1) * 3)) And &b111
              If i = 1 Then
                ' aim in front
                j = mx + ddx(ManDirection) * 4 : k = my + ddy(ManDirection) * 4
                j = Max(1, j) : j = Min(17, j) : k = Max(1, k) : k = Min(17, k)
                Do While j <> mx Or y <> my
                  If a(j, k) < 16 Then Exit
                  j = j - ddx(ManDirection) : k = k - ddy(ManDirection)
                Loop
                GhostDirection(i) = (d(x-1, y-1, j-1) >> ((k - 1) * 3)) And &b111
              EndIf
              If i = 2 Then
                ' aim behind
                j = mx - ddx(ManDirection) * 4 : k = my - ddy(ManDirection) * 4
                j = Max(1, j) : j = Min(17, j) : k = Max(1, k) : k = Min(17, k)
                Do While j <> mx Or y <> my
                  If a(j, k) < 16 Then Exit
                  j = j + ddx(ManDirection) : k = k + ddy(ManDirection)
                Loop
                GhostDirection(i) = (d(x-1, y-1, j-1) >> ((k - 1) * 3)) And &b111
              EndIf
            EndIf
            ' regardless, if the ghost is close to the PacMan then abandon any target and go for him
            If KillMode <= Timer And Abs(x - mx) < 5 And Abs(y = my) < 5 Then GhostDirection(i) = (d(x-1, y-1, mx-1) >> ((my - 1) * 3)) And &b111
          EndIf
        EndIf

        ' safety check, if the ghost is headed into a wall cancel the movement
        If a(x + ddx(GhostDirection(i)), y + ddy(GhostDirection(i))) And &hF0 Then GAction(i) = 0 : GhostDirection(i) = 4
      EndIf

      j = ((Timer And &h100) = 0) Xor (i Mod 2)
      If KillMode > Timer  Then
        ' if the PacMan has eaten the power ball and the ghosts are running away
        NextG = Timer + GhostSlow        ' move slowly
        k = (KillMode - Timer)\100
        If k < 10 Then
          '  last second: flash the ghosts on/off
          If k Mod 2 = 0 Then
             Box GhostX(i) + xMargin - 9, GhostY(i) + yMargin - ManR, 17, ManR * 2 + 2, 6, 0, 0
          Else
            Blit Write(i * 6) + 5 + j , GhostX(i) + xMargin - 9, GhostY(i) + yMargin - ManR
          EndIf
        Else
          ' running away so use a grey ghost
          Blit Write(i * 6) + 3 + j , GhostX(i) + xMargin - 9, GhostY(i) + yMargin - ManR
        EndIf
      Else
        ' we are in normal mode with the ghosts chasing the PacMan
        Blit Write(i * 6) + 1 + j, GhostX(i) + xMargin - 9, GhostY(i) + yMargin - ManR
        NextG = Timer + GhostS
      EndIf

      ' if a ghost and PacMan have collided
      If Abs(GhostX(i) - ManX) < 12 And Abs(GhostY(i) - ManY) < 12 Then
        If KillMode > Timer  Then
          If TestMode = 0 Then  Play Stop : Play Sound 4, B, N, 800, 25
          VulnerableGhosts = VulnerableGhosts - 1
          Score = Score + (4 - VulnerableGhosts) * 300
          ' erase the ghost
          Box GhostX(i) + xMargin - 9, GhostY(i) + yMargin - ManR, 17, ManR * 2 + 2, 6, 0, 0
          GInGate(i) = KillMode + i*500 : InitGhost i   ' move him to the center box
        Else
          NbrMen = NbrMen - 1
          note = 4000
          Do
            If TestMode = 0 Then  Play Stop : Play Sound 4, B, S, note, 25
            Pause 50
            note = note * 0.9
          Loop While note > 400
          Play Stop
          If NbrMen > 0 Then GoTo LostALife
          RBox 50, 70, 220, 80,,,0
          Text 160, 110, "You have died!", CB, 2
          Text 160, 110, "Press any key", CT, 2
          Do While get_input%() : Loop
          Do While Not get_input%() : Loop
          GoTo StartGame
        EndIf
      EndIf
    Next i
  EndIf

Loop


' initialise a ghost
Sub InitGhost i
  ' starting position and targets for the ghosts
  If i = 0 Then GhostX(0) =  9 * 13 : GAction(0) = 0
  If i = 1 Then GhostX(1) =  8 * 13 : GAction(1) = GetRandom(&b0111)
  If i = 2 Then GhostX(2) =  9 * 13 : GAction(2) = GetRandom(&b0111)
  If i = 3 Then GhostX(3) = 10 * 13 : GAction(3) = GetRandom(&b0111)
  GhostDirection(i) = 4 : GhostY(i) = 9 * 13
End Sub


' load the board layout from the data statements into the array a()
Sub LoadData
  Local x, y, i
  Restore picoman_data
  For y = 0 To ay
    i = 18
    For x = 0 To 8
      Read a(x, y)
      a(i, y) = a(x, y)
      i = i - 1
    Next x
    Read a(9, y)
  Next y
End Sub


' draws the board on the LCD according to the data stored in the array a()
Sub DrawBoard
  Local x, y, i, j, k
  NbrDots = 0
  CLS
  For x = 0 To ax
    For y = 0 To ay
      If a(x, y) = 1 Then Line (x * 13) - 1 + xMargin, (y * 13) + yMargin, (x * 13) + 1 + xMargin, (y * 13) + yMargin, 3, cDot : NbrDots = NbrDots + 1
      If a(x, y) = 2 Then Circle (x * 13) + xMargin, (y * 13) + yMargin, 3, 0, 1, cDot, cDot
      If a(x, y) = 4 Then Circle (x * 13) + xMargin, (y * 13) + yMargin, ManR - 3, 1, 1, 0, RGB(Orange)
      If a(x, y) And &h10 Then
        ' a vertical line
        If a(x, y) And &h80 Then i = 0 Else i = 11
        If a(x, y) And &h40 Then j = 0 Else j = 12
        Line (x * 13) + xMargin, (y * 13) - i + yMargin, (x * 13) + xMargin, (y * 13) + j + yMargin, WallWidth, cWall
      EndIf
      If a(x, y) And &h20 Then
        ' a horizontal line
        If a(x, y) And &h80 Then i = 2 Else i = 14
        If a(x, y) And &h40 Then j = 2 Else j = 14
        If x > 9 Then k = i : i = j : j = k    ' reverse start and finish for the mirrored part of the board
        Line (x * 13) - i + xMargin + 2, (y * 13) + yMargin, (x * 13) + j + xMargin - 1, (y * 13) + yMargin, WallWidth, cWall
      EndIf
    Next y
  Next x
End Sub


' draw a ghost and save its image in a BLIT buffer
Sub CreateGhost i, bbuf, eyes, clr
  RBox 9*13 + xMargin - 9, 9*13 + yMargin - ManR, 17, ManR * 2 + 2, 6, 0, clr
  Circle 9*13 + xMargin - 1, 9*13 + yMargin - ManR/2 + 1, 3, 1, 0.7, 0, 0
  Circle 9*13 + xMargin - 1 * eyes - 1, 9*13 + yMargin - ManR/2 + 1, 3, 1, 0.7, 0, 0
  On Error Skip
  Blit Close (i*6)+bbuf
  Blit Read (i*6)+bbuf, 9*13 + xMargin - 9, 9*13 + yMargin - ManR, 17, ManR * 2 + 2
End Sub


' return a number representing the quadrant of the coordinates
' as following:
' &b0001 = upper left
' &b0010 = upper right
' &b0100 = lower right
' &b1000 = lower left
Function GetQuadrant(x, y)
  If y < 9 Then
    If x < 9 Then GetQuadrant = &b0001 Else GetQuadrant = &b0010
  Else
    If x < 9 Then GetQuadrant = &b1000 Else GetQuadrant = &b0100
  EndIf
End Function


' get a random position on the board that is not on a wall
' returns as a target (ie, &hXXYY)
' q is the quadrant being an OR of the above listed bits
Function GetRandom(q)
  Local x, y
  Do
    x = Rnd * 16 + 1 : y = Rnd * 16 + 1
    If x = 9 Or y = 9 Then Continue Do
    If (a(x, y) And &hF0) > 15 Then Continue Do
  Loop Until (q And GetQuadrant(x, y)) <> 0
  GetRandom = (x << 8) Or y
End Function


' this is used by a ghost to determin the direction towards its target
' the ghost is at gx and gy while the target is tx and ty
' this returns a direction number (0 = up, 1 = right, 2 = down, 3 = left)
' it uses a pre calculated matrix giving the shortest route between any two points
Function GetDirection(gx, gy, tx, ty)
  Local k
  k = d(gx-1, gy-1, tx-1)
  GetDirection = (k >> ((ty - 1) * 3)) And &b111
End Function


' draw the number of spare PacMen
Sub DrawSpareMen
  Local i, x, y
  'Box ax * 13 + 2 + xMargin, 205 + yMargin, 320, 24, 1, 0, 0
  For i = 1 To NbrMen - 1
    x = 245 + i*20 + xMargin + (3 - NbrMen)*10 : y = 216 + yMargin
    Circle x, y, ManR, 1, 1, 0, cMan
    Triangle x, y, x - ManR, y - ManR/2, x - ManR, y + ManR/2, 0, 0
  Next i
End Sub


' handle the QUIT action by the player
Function QuitCheck()
  Local key, state, s$, tt
  tt = Timer
  MakeSound ack()
  If Not TestMode Then FrameBuffer Copy N , F
  Do
    RBox 56, 40, 320-112, 135, , RGB(white), 0
    Text 160, 60, "Quit?", C, , , RGB(white), 0
    If state Then
      RBox 90, 100, 56, 40, , RGB(white), RGB(white)
      RBox 170, 100, 56, 40, , RGB(white), 0
      Text 118, 120, "YES", CM, , , 0, RGB(white)
      Text 198, 120, "NO",  CM, , , RGB(white), 0
    Else
      RBox 90, 100, 55, 40, , RGB(white), 0
      RBox 170, 100, 55, 40, , RGB(white), RGB(white)
      Text 118, 120, "YES", CM, , , RGB(white), 0
      Text 198, 120, "NO",  CM, , , 0, RGB(white)
    EndIf
    Do
      Do While get_input%() : Loop
      Do : key = get_input%() : Loop While Not key
      If key = ctrl.LEFT And State = 0 Then
        State = 1 : MakeSound ack() : Exit Do
      ElseIf key = ctrl.RIGHT And State = 1 Then
        State = 0 : MakeSound ack() : Exit Do
      ElseIf key = ctrl.A Or key = ctrl.SELECT Then
        MakeSound ack()
        Do While get_input%() : Loop
        If state = 0 Then
          If Not TestMode Then FrameBuffer Copy F , N
          Timer = tt
          Exit Function
        Else
          QuitCheck = 1
          Exit Function
        EndIf
      Else
        MakeSound nack()
      EndIf
    Loop
  Loop
End Function


' make a sound, is passed an array of frequencies terminated with a zero
Sub MakeSound snd() As Float
  Local i
  Play Stop
  Do
    Play Sound 4, B, S, snd(i), 25
    Pause 40
    i = i + 1
  Loop Until snd(i) = 0
  Play Stop
End Sub


' draw the start screen
Sub StartScreen
  Local i, k
  CLS
  Text 160, 0, "PicoMan", CT, 5
  Text 160, 32, "v" + sys.format_version$(VERSION) +"  (c) 2023 Geoff Graham", CT, 1
  Line 90, 59, 93, 59, 3, RGB(White)
  Text 120, 59, "10 points", LM, 2
  Circle 90, 79, 3, 0, 1, RGB(White), RGB(White)
  Text 120, 79, "50 points", LM, 2
  Circle 90, 99, 6, 1, 1, 0, RGB(Orange)
  Text 120, 99, "300 points", LM, 2
  For i = 1 To 4
    For k = 1 To i
      RBox 95 - k*15, 92 + i*20, 17, 20, 6, 0, RGB(LightGrey)
      Circle 100 - k*15, 98 + i*20, 3, 1, 0.7, 0, 0
      Circle 106 - k*15, 98 + i*20, 3, 1, 0.7, 0, 0
    Next k
    Text 120, 103 + i*20, Str$(i * 300)+" points", LM, 2
  Next i
  Text 160, 210, "START  = Play/Pause  ", CM, 2
  Text 160, 228, "SELECT = Exit to Menu", CM, 2
  Do While get_input%() : Loop
  Do
    Select Case get_input%()
      Case ctrl.SELECT
        game.end()
      Case ctrl.A, ctrl.START
        Exit Do
    End Select
  Loop
  Do While get_input%() : Loop
End Sub


Function get_input%()
  Static last_input%
  Call ctrl$, get_input%
  If Not get_input% Then keys_cursor_ext(get_input%)
  If get_input% = ctrl.HOME Then get_input% = ctrl.SELECT
  If get_input = last_input% Then
    get_input% = 0
  Else
    last_input% = get_input%
  EndIf
End Function


picoman_data:

' define the board.  this includes the walls, dots, etc
' each data line defines one horizontal row of the board.
' But it only defines half the horizontal width, the code loading
' the data statements mirrors this to give the board's full width
' bit 0 = a dot
' bit 2 = bonus dot
' bit 3 = fruit
' bit 4 = reserved
' bit 5 = a vertical barrier
' bit 6 = a horizontal barrier
' bit 7 = shorten the end of the barrier's line
' bit 8 = shorten the start of the barrier's line
'
'      0     1     2     3     4     5     6     7     8      9
Data &hA0, &h20, &h20, &h20, &h20, &h20, &h20, &h20, &h20,  &h20  '  0
Data &h10, &h01, &h01, &h01, &h01, &h01, &h01, &h01, &h01,  &h50  '  1
Data &h10, &h02, &hA0, &h60, &h01, &hA0, &h20, &h60, &h01,  &h01  '  2
Data &h10, &h01, &h01, &h01, &h01, &h01, &h01, &h01, &h01,  &h90  '  3
Data &h10, &h01, &hA0, &h60, &h01, &h90, &h01, &hA0, &h20,  &h20  '  4
Data &h10, &h01, &h01, &h01, &h01, &h10, &h01, &h01, &h01,  &h10  '  5
Data &h50, &h20, &h20, &h90, &h01, &h50, &h20, &h60, &h00,  &h00  '  6
Data &hA0, &h20, &h20, &h50, &h01, &h00, &h00, &h00, &h00,  &h00  '  7
Data &hC0, &hC0, &hC0, &h00, &h01, &h90, &h00, &hA0, &h60,  &h00  '  8
Data &hA0, &h20, &h20, &h90, &h01, &h10, &h00, &h10, &h00,  &h00  '  9
Data &h90, &h20, &h20, &h50, &h01, &h50, &h00, &hA0, &h20,  &h20  ' 10
Data &h10, &h01, &h01, &h01, &h01, &h00, &h00, &h00, &h00,  &h00  ' 11
Data &h10, &h01, &hA0, &h60, &h01, &hA0, &h60, &h00, &hA0,  &h20  ' 12
Data &h10, &h02, &h01, &h10, &h01, &h01, &h01, &h01, &h01,  &h01  ' 13
Data &h10, &h60, &h01, &h50, &h01, &h90, &h01, &hA0, &h20,  &h20  ' 14
Data &h10, &h01, &h01, &h01, &h01, &h10, &h01, &h01, &h01,  &h10  ' 15
Data &h10, &h01, &hA0, &h20, &h20, &h20, &h20, &h60, &h01,  &h50  ' 16
Data &h10, &h01, &h01, &h01, &h01, &h01, &h01, &h01, &h01,  &h01  ' 17
Data &hA0, &h20, &h20, &h20, &h20, &h20, &h20, &h20, &h20,  &h20  ' 18


' this data defines the optimal route between any two locations in the maze
' this must be recalculated if the dimensions of the maze or any of the walls are changed
Data &h1241240002490,&h1049040002081,&h1040040202081,&h1049249249249,&h1001040040041,&h1049049241241
Data &h1041240041041,&h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241
Data &h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,&h2482480002480
Data &h2092080002080,&h2080080402080,&h2092492492480,&h2002080080080,&h2092092482480,&h2082480082080
Data &h2482082092080,&h2002082490000,&h2482082092000,&h2082480082000,&h2092092480000,&h2002080080000
Data &h2092492480000,&h2080080400000,&h2092080000000,&h2482480000000,&h1241240002400,&h1049040002040
Data &h1040040202040,&h1049249249248,&h1001040040040,&h1049049241240,&h1041240041040,&h1241041049048
Data &h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,&h2482480002000,&h2092080002000,&h2080080402000
Data &h2092492492000,&h2002080080000,&h2092092480000,&h2082480080000,&h2482082080000,&h2002082480000
Data &h2482082080000,&h2082480080000,&h2092092480000,&h2002080080000,&h2092492490000,&h2080080400000
Data &h2092080000000,&h2482480000000,&h1241240000000,&h1049040001000,&h1040040201000,&h1049249249200
Data &h1001040040000,&h1049049240000,&h1041240040000,&h1241041048000,&h1001041248000,&h1241041049000
Data &h1041240041000,&h1049049241000,&h1001040040000,&h1049249249000,&h1040040201000,&h1049040001000
Data &h1241240001000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,&h2482400001249,&h2092040001041,&h2080040201041,&h2049249249249
Data &h2001040040041,&h2049049241241,&h1041240041041,&h1241041049049,&h1001041248008,&h1241041049049
Data &h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,&h2482000000000,&h2092000000000,&h2080000000000,&h2080000000000,&h2000000000000
Data &h2000000000000,&h2000000000000,0,0,0,0,0,0,0,0,0,0,&h1240000000000,&h1049000000000
Data &h1040000000000,&h1048000000000,&h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000
Data &h1000000000000,&h1240000000000,&h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000
Data &h1000000000000,&h1000000000000,&h1000000000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h2401240001249,&h2049040001041,&h2040040201041,&h2049249249249,&h2001040040041,&h2049049241241
Data &h2041240041041,&h2441041049049,&h2001041248008,&h2481041049049,&h2081240041041,&h2049049241241
Data &h2001040040041,&h2049249249249,&h2040040201041,&h2049040001041,&h2441240001249,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data &h2480000000000,&h2000000000000,&h2480000000000,&h2080000000000,&h2080000000000,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2480000000000,0,&h1000000000000
Data &h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000,&h1040000000000,&h1240000000000
Data &h1000000000000,&h1240000000000,&h1040000000000,&h1048000000000,&h1000000000000,&h1000000000000
Data &h1000000000000,&h1049000000000,&h1241000000000,&h12412400036DB,&h10490400030C0,&h1040040201041
Data &h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041,&h1241041049049,&h1001041248008
Data &h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12412400036DB
Data &h1049040003003,&h1040040201041,&h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041
Data &h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h12412400036DB,&h10490400000C3,&h1040040201041,&h1049249249249,&h1001040040041
Data &h1049049241241,&h1041240041041,&h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041
Data &h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,&h36C36C0001249,&h30DB000001041,&h3040040201041,&h3049249249249,&h3001040040041
Data &h1049049241241,&h1041240041041,&h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041
Data &h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24836C00036DB,&h20900C00030C3,&h20800C06030C3
Data &h20924DB6DB6DB,&h20020C00C00C3,&h20920DB6C36C3,&h20824C00C30C3,&h24820C30DB0DB,&h20020C36D8018
Data &h24820C30DB0DB,&h20824C00C30C3,&h20920DB6C36C3,&h20020C00C00C3,&h20924DB6DB6DB,&h20800C06030C3
Data &h20920C00030C3,&h24836C00036DB,&h2480000000000,&h2080000000000,&h2080000000000,&h2092400000000
Data &h2002000000000,&h2092000000000,&h2082400000000,&h2482000000000,&h2002000000000,&h2482000000000
Data &h2082400000000,&h2092000000000,&h2002000000000,&h2092400000000,&h2080000000000,&h2092000000000
Data &h2480000000000,&h36C0000001249,&h3000000001041,&h3040000201041,&h3049249249249,&h3001040040041
Data &h3049049241241,&h3041240041041,&h3241041049049,&h3001041248008,&h36C1041049049,&h3041240041041
Data &h3049049241241,&h3001040040041,&h3049249249249,&h3040040201041,&h3049040001041,&h3241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&hDB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB
Data &h10030C00C00C3,&h104B0DB6C36C3,&h10436C00C30C3,&h12430C30DB0DB,&h10030C36D8018,&h12430C30DB0DB
Data &h10412400C30C3,&h10490492436C3,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,&h12412400036DB,&h1049040001043,&h1040040201040,&h1049249249249,&h1001040040041
Data &h1049049241241,&h1041240041041,&h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041
Data &h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12412400036DB,&h10490400010C1,&h1040040201001
Data &h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041,&h1241041049049,&h1001041248008
Data &h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12412400036DB
Data &h1049040003041,&h1040040200041,&h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041
Data &h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h1241240001249,&h1049040001041
Data &h1040040001041,&h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041,&h1241041049049
Data &h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12436C0001249,&h105B0C0001041,&h1040000201041
Data &h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041,&h1241041049049,&h1001041248008
Data &h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C0001249,&h30DB040001041
Data &h3000040201041,&h3049249249249,&h3001040040041,&h3049049241241,&h3041240041041,&h1241041049049
Data &h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h36C36C00036DB,&h30DB0C00030C3,&hC00C06030C3,&h10DB6DB6DB6DB,&h10030C00C00C3,&h10490DB6C36C3
Data &h10412400C30C3,&h12410430DB0DB,&h10010436D8018,&h12410430DB0DB,&h1041240043043,&h1049049243641
Data &h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,&h248248000249B
Data &h2092080002083,&h2080080402083,&h2092492492490,&h2002080080081,&h2092092482481,&h2082480082081
Data &h2482082092049,&h2002082490008,&h2482082092049,&h2082480081041,&h2092092481241,&h2002080080041
Data &h2049249249249,&h2040040201041,&h2049040001041,&h1241240001249,&h2482480002480,&h2092080002080
Data &h2080080402080,&h2092492492480,&h2002080080080,&h2092092482480,&h2082480082080,&h2482082092080
Data &h2002082490000,&h2482082092000,&h2082480082000,&h2092092480000,&h2002080080000,&h2092492480000
Data &h2080080400000,&h2092080000000,&h2482480000000,&h24824800026D8,&h20920800020C0,&h20800804020C0
Data &h2092492492400,&h2002080080040,&h2092092481240,&h2082480081040,&h2482081049048,&h2002081248008
Data &h2482081049049,&h2082480041041,&h2092049241241,&h2002040040041,&h2049249249249,&h2040040201041
Data &h2049040001041,&h1241240001249,&h2482480002000,&h2092080002000,&h2080080402000,&h2092492492000
Data &h2002080080000,&h2092092480000,&h2082480080000,&h2482082080000,&h2002082480000,&h2482082080000
Data &h2082480080000,&h2092092480000,&h2002080080000,&h2092492490000,&h2080080400000,&h2092080000000
Data &h2482480000000,&h2482480003600,&h2092080003000,&h2080080403000,&h2092492490000,&h2002080080000
Data &h2092092480000,&h2082480080000,&h2482082090000,&h2002082490000,&h2482082092000,&h2082480082000
Data &h2092092482000,&h2002080080000,&h2092492492000,&h2080080402000,&h2092080002000,&h2482480002000
Data &h2482480000000,&h2092080000000,&h2080080400000,&h2092492480000,&h2002080080000,&h2092092480000
Data &h2082480080000,&h2482082092000,&h2002082490000,&h2482082092000,&h2082480082000,&h2092092482400
Data &h2002080080000,&h2092492492400,&h2080080402000,&h2092080002000,&h2482480002400,&h2482480000000
Data &h2092080000000,&h2080080400000,&h2092492400000,&h2002080040000,&h2092049240000,&h2081240041000
Data &h2481041049000,&h2001041248000,&h2441041049000,&h2041240041000,&h2049049241240,&h2001040040040
Data &h2049249249248,&h2040040201040,&h2049040001040,&h1241240001248,&h2482480000000,&h2092080000000
Data &h2080080600000,&h2092492000000,&h2002080000000,&h2092080000000,&h2082480000000,&h2482080000000
Data &h2002080000000,&h2482080000000,&h2082480000000,&h2092080000000,&h2002080000000,&h2092480000000
Data &h2080080000000,&h2092080000000,&h2482480000000,&h2482480000000,&h2092080000000,&h2080080000000
Data &h2092490000000,&h2002080000000,&h2092090000000,&h2082480000000,&h2482080000000,&h2002080000000
Data &h2482080000000,&h2082480000000,&h2092090000000,&h2002080000000,&h2092490000000,&h2080080000000
Data &h2092080000000,&h2482480000000,&h2482480000000,&h2092080000000,&h2080080000000,&h2092480000000
Data &h2002080000000,&h2092092000000,&h2082480000000,&h2482080000000,&h2002080000000,&h2482080000000
Data &h2082480000000,&h2092092000000,&h2002080000000,&h2092492000000,&h2080080000000,&h2092080000000
Data &h2482480000000,&h24836C0000000,&h20930C0000000,&h20800C0000000,&h2092400000000,&h2002040000000
Data &h2092049200000,&h2081240000000,&h2481040000000,&h2001040000000,&h2441040000000,&h2041240000000
Data &h2049049200000,&h2001040000000,&h2049249200000,&h2040040200000,&h2049040000000,&h1241240000000
Data &h2480000000000,&h2090000000000,&h2080000000000,&h2092000000000,&h2002000000000,&h2092000000000
Data &h2082000000000,&h2482000000000,&h2002000000000,&h2482000000000,&h2082000000000,&h2092000000000
Data &h2002000000000,&h2092000000000,&h2080000000000,&h2090000000000,&h2480000000000,&h2480000000000
Data &h2092000000000,&h2080000000000,&h2090000000000,&h1001000000000,&h1049000000000,&h1041200000000
Data &h1241000000000,&h1001000000000,&h1241000000000,&h1041200000000,&h1049000000000,&h1001000000000
Data &h1049200000000,&h1040000000000,&h1049000000000,&h1240000000000,&h2482000000000,&h2092000000000
Data &h2080000000000,&h2080000000000,&h2000000000000,0,0,0,0,0,0,0,0,0,0,0,0,&h36C3600000000
Data &h30DB000000000,&h30C0000000000,&h3000000000000,&h3000000000000,&h3000000000000,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3
Data &h30C00C06030C3,&hDB6DB6DB6DB,&h10010C00C00C3,&h10490492416C3,&h10412400410C3,&h12410410490DB
Data &h1001041248018,&h124104104905B,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h30030C00C00C0,&h30DB0DB6C36C1,&h30C36C00C3041,&h36C30C30DB049,&h30030C36D8008
Data &h36C30C30D9049,&h30C36C00C1041,&h30DB0DB6C1241,&h3001040040041,&h3049249249249,&h3040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C0003,&h30DB0DB6C1243,&h30C36C0041041
Data &h36C30C1049049,&h30030C1248008,&h36430C1049049,&h3041240041041,&h3049049241241,&h3001040040041
Data &h3049249249249,&h3040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h10030400000C3
Data &h10490492412C3,&h10412400410C3,&h12410410490DB,&h1001041248018,&h124104104905B,&h1041240041043
Data &h1049049241243,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB
Data &h10030000400C3,&h10490492412C3,&h10412400410C3,&h12410410490DB,&h1001041248018,&h124104104905B
Data &h1041240041043,&h1049049241243,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3
Data &h30C00C06030C3,&h10DB6DB6DB6DB,&h10000C00C00C3,&h10490492416C3,&h10412400410C3,&h12410410490DB
Data &h1001041248018,&h12410410490DB,&h1041240041043,&h1049049241243,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C0001249
Data &h30DB0C0001041,&h30C0040201041,&h30D9249249249,&h1040040041,&h1049049241241,&h1041240041041
Data &h1241041049049,&h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,&h36C36C00036DB,&h30DB0C00030C3
Data &h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C1240,&h30C36C00C1041,&h36C30C1049049
Data &h30030C1248008,&h36430C1049049,&h30436C0041041,&h3049049241241,&h3001040040041,&h3049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB092482401
Data &h30C2480082041,&h36C2082092049,&h3002082490008,&h3482082092049,&h3082480082041,&h3092092481241
Data &h3002080080041,&h3092492489249,&h2080080401041,&h2092080001041,&h2482480001249,0
Data 0,0,0,0,&h92482000,&h2480082000,&h2082092000,&h2082490000,&h482082092000,&h82480082000
Data &h92092482000,&h2080080000,&h92492490000,&h2080080400000,&h2092080000000,&h2482480000000
Data 0,0,0,0,&h1000040040000,&h1049049240000,&h1041240041000,&h1241041049000,&h1001041248000
Data &h1241041049000,&h1041240041000,&h1049049241200,&h1001040040000,&h1049249249000,&h1040040201000
Data &h1049040001000,&h1241240001000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24824800036DB
Data &h20920800030C3,&h20800806030C3,&h209249B6DB6DB,&h20020800C00C3,&h2092092401243,&h2082480041043
Data &h2482081049049,&h2002081248008,&h2482081049049,&h2082480041041,&h2092049241241,&h2002040040041
Data &h2049249249249,&h2040040201041,&h1049040001041,&h1241240001249,&h2482480000000,&h2092080000000
Data &h2080080000000,&h2092480000000,&h2002080000000,&h2092092000000,&h2082480000000,&h2482080000000
Data &h2002080000000,&h2482080000000,&h2082480000000,&h2092080000000,&h2002080000000,&h2092480000000
Data &h2080080000000,&h2092080000000,&h2482480000000,&h2482480000000,&h2092080000000,&h2080080000000
Data &h2092490000000,&h2002080000000,&h2092090000000,&h2082480000000,&h2482080000000,&h2002080000000
Data &h2482080000000,&h2082480000000,&h2092090000000,&h2002080000000,&h2092490000000,&h2080080000000
Data &h2092080000000,&h2482480000000,&h2482480000000,&h2092080000000,&h2080080000000,&h2092492000000
Data &h2002080000000,&h2092080000000,&h2082480000000,&h2482080000000,&h2002080000000,&h2482080000000
Data &h2082480000000,&h2092092000000,&h2002080000000,&h2092492000000,&h2080080000000,&h2092080000000
Data &h2482480000000,&h36C36C0000000,&h30DB0C0000000,&h30C00C0600000,&h10DB6DB600000,&h10010C0000000
Data &h1049000000000,&h1041240000000,&h1241040000000,&h1001040000000,&h1241040000000,&h1041240000000
Data &h1049049200000,&h1001040000000,&h1049249200000,&h1040040200000,&h1049040000000,&h1241240000000
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h20C00C06030C3
Data &h20DB6DB6DB6DB,&h20030400400C3,&h20900492412C3,&h20812400410C3,&h24810410490DB,&h2001041248018
Data &h244104104905B,&h2041240041043,&h2049049241241,&h2001040040041,&h2049249249249,&h2040040201041
Data &h2049040001041,&h1241240001249,0,0,&h2000000000000,&h2000000000000,&h2000000000000
Data &h2080000000000,&h2080000000000,&h2480000000000,&h2000000000000,&h2480000000000,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data 0,&h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000
Data &h1040000000000,&h1240000000000,&h1000000000000,&h1240000000000,&h1040000000000,&h1000000000000
Data &h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000,&h1200000000000,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C0001249,&h30DB040001041,&h30C0040201041,&h30C9249249249
Data &h3001040040041,&h49049241241,&h1041240041041,&h1241041049049,&h1001041248008,&h1241041049049
Data &h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C0043
Data &h30DB0DB6C1243,&h30C36C0041040,&h36C30C1049049,&h30030C1248008,&h32430C1049049,&h3041240041041
Data &h3049049241241,&h3001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h30030C00C00C1,&h30DB0DB6C36C1,&h30C36C00C3001,&h36C30C30DB049,&h30030C36D8008
Data &h36C30C30DB049,&h30C36C00C1041,&h30DB0DB6C1241,&h30010C00C0041,&h3049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12412400036DB
Data &h10490400030C3,&h10400402030C3,&h104924924B6DB,&h10010400400C3,&h10490492436C3,&h10412400400C3
Data &h12410410490DB,&h1001041248018,&h12410410490DB,&h1041240041043,&h1049049241243,&h1001040040043
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C0043
Data &h30DB0DB6C1243,&h30C36C0001041,&h36C30C1049049,&h30030C1248008,&h32430C1049049,&h3041240041041
Data &h3049049241241,&h3001040040041,&h3049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,&h24836C00036DB,&h20930C00030C3,&h20800C06030C3,&h20926DB6DB6DB
Data &h20020C00C00C3,&h20920DB6C36C3,&h20824000C30C3,&h24820430DB0DB,&h20020436D8018,&h22420430DB0DB
Data &h20412400430C1,&h20490492436C1,&h2001040040041,&h2049249249249,&h2040040201041,&h1049040001041
Data &h1241240001249,&h2480000000000,&h2090000000000,&h2080000000000,&h2092000000000,&h2002000000000
Data &h2092000000000,&h2082000000000,&h2482000000000,&h2002000000000,&h2482000000000,&h2082000000000
Data &h2092000000000,&h2002000000000,&h2092000000000,&h2080000000000,&h2090000000000,&h2480000000000
Data &h36C0000000000,&h30DB000000000,&h30C0000000000,&h30DB600000000,&h3003000000000,&h30DB000000000
Data &h30C0000000000,&h36C1000000000,&h3001000000000,&h3241000000000,&h3041200000000,&h3049000000000
Data &h3001000000000,&h3049200000000,&h3040000000000,&h1049000000000,&h1240000000000,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12C36C00036DB,&h10DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB
Data &h10030C00C00C3,&h10DB0DB6C36C3,&h10036C00C30C3,&h12430C30DB0DB,&h10030C36D8018,&h12430C30DB0DB
Data &h10436C00C30C3,&h104B0DB6C36C3,&h10030C00C00C3,&h10DB6DB6DB6DB,&h10C00C06030C3,&h10490C00030C3
Data &h12416C00036DB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C3640001249,&h30DB040001041
Data &h30C0040201041,&h3049249249249,&h3001040040041,&h3049049241241,&h41240041041,&h1241041049049
Data &h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,&h248248000249B,&h2092080002083,&h2080080402083
Data &h209249249249B,&h2002080080083,&h2092092482483,&h2082480082083,&h2482082092090,&h2002082490010
Data &h2482082092092,&h2082480082082,&h2092092482482,&h2002080080082,&h2092492492492,&h2080080402082
Data &h2092080002082,&h2482480002492,&h2482480002480,&h2092080002080,&h2080080402080,&h2092492492480
Data &h2002080080080,&h2092092482480,&h2082480082080,&h2482082092080,&h2002082490008,&h1241042049049
Data &h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,&h36C36C00036D8,&h30DB0C00030C0,&h30C00C06030C0,&h30DB6DB6DB6D8,&h30030C00C00C0
Data &h30DB0DB6C36C0,&h30C36C00C30C0,&h36C30C30DB000,&h30030C36D8000,&h30030C30D8000,&h30036C00C0000
Data &h30000DB6C0000,&h3000000000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24824800026DB
Data &h20920800020C3,&h20800804020C3,&h20924924926DB,&h20020800800C3,&h20920924836C3,&h20824800830C3
Data &h24820820900DB,&h2002082490018,&h248208209209B,&h2082480082083,&h2092092482483,&h2002080080082
Data &h2092492492492,&h2080080402082,&h2092080002082,&h2482480002492,&h2482480002000,&h2092080002000
Data &h2080080402000,&h2092492492000,&h2002080080000,&h2092092480000,&h2082480080000,&h2482081080000
Data &h2002081248000,&h1241041049040,&h1041240041040,&h1049049241240,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,&h36C36C0003600,&h30DB0C0003000,&h30C00C0603000
Data &h30DB6DB6DB600,&h30030C00C0000,&h30DB0DB6C0000,&h30C36C00C0000,&h36C30C1000000,&h30030C1240000
Data &h1241041040000,&h1041240040000,&h1049049240000,&h1001040040000,&h1049249249200,&h1040040201000
Data &h1049040001000,&h1241240001200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h1241240001249
Data &h1049040001041,&h1040040201041,&h1049249249249,&h1001040040041,&h1049049241241,&h1041240041041
Data &h1241040049049,&h1001041248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3
Data &h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30030DB0DB,&h30030436D8018,&h1241043049049,&h1041240041041
Data &h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C00C30DB0DB,&h30010C36D8018
Data &h1241043049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24836C00036DB
Data &h20DB0C00030C3,&h20C00C06030C3,&h20DB6DB6DB6DB,&h20030C00C00C3,&h20DB0DB6C36C3,&h20C36C00C30C3
Data &h24030C30DB0DB,&h20030C36D8018,&h24830C30DB0DB,&h20836C00C30C3,&h20920DB6C36C2,&h20020C00C0082
Data &h2092492492492,&h2080080402082,&h2092080002082,&h2482480002492,&h2480000000000,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data &h2000000000000,&h2480000000000,&h2080000000000,&h2092000000002,&h2002000000082,&h2092492492492
Data &h2080080402082,&h2092080002082,&h2482480002492,&h36C3000000000,&h30DB000000000,&h3000000000000
Data &h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000,0,&h1000000000000
Data &h1240000000040,&h1041240040041,&h1049049240041,&h1001040040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3
Data &h36C30C10DB0DB,&h1001041248000,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,&h24824800024DB,&h20920800020C3,&h20800804020C3,&h20924924924DB,&h20020800800C3
Data &h20920924836C3,&h20824800830C3,&h248208209B0DB,&h1001042480008,&h1241042049049,&h1041240041041
Data &h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data &h36C36C0003600,&h30DB0C0003000,&h30C00C0603000,&h30DB6DB6DB600,&h30030C00C0000,&h30DB0DB6C0000
Data &h30C36C00C0000,&h36C30C20C0000,&h1001042400000,&h1241042040000,&h1041240040000,&h1049049240000
Data &h1001040040000,&h1049249249200,&h1040040201000,&h1049040001000,&h1241240001200,0
Data 0,0,0,0,0,0,&h2000000,&h2000000,&h2000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h3000000,0
Data &h1000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3
Data &h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C10DB0DB
Data &h1001001248008,&h1241041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3
Data &h30C36C00C30C3,&h36C30C10DB0DB,&h1000041248008,&h1241041049049,&h1041240041041,&h1049049241241
Data &h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB
Data &h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C10DB0DB,&h1041248008,&h1241041049049
Data &h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,&h2482480002492,&h2092080002082,&h2080080402082,&h2092492492492,&h1002080080082
Data &h1092092482482,&h1082480082082,&h1481042092092,&h1001042490010,&h1241042092090,&h1041240082081
Data &h1049049242481,&h1001040040081,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data &h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h20030C00C00C3,&h20DB0924836C3
Data &h20C24800830C3,&h26C20820930DB,&h2002082490018,&h2482082092080,&h2082480082080,&h2092092482480
Data &h2002080080080,&h2092492492480,&h2080080402080,&h2092080002080,&h2482480002480,0
Data 0,0,0,&h1000000000000,&h1000049240000,&h1001240040000,&h1001041048000,&h1001041248000
Data &h1241041049000,&h1041240041040,&h1049049241240,&h1001040040040,&h1049249249248,&h1040040201040
Data &h1049040001040,&h1241240001248,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h2482480002492
Data &h2092080002082,&h2080080402082,&h2092492492492,&h2002080080081,&h2092092482481,&h2082480082081
Data &h2482082092049,&h2002082490008,&h2482082090049,&h2082480081041,&h2092092481241,&h2002080080041
Data &h2092492491249,&h2080080401041,&h2092080001041,&h2482480001249,&h24824800024DB,&h20920800020C3
Data &h20800804020C3,&h20924924924DB,&h20020800800C3,&h20920924836C0,&h20824800830C0,&h248208209B0C0
Data &h2002082498000,&h2482082080000,&h2082480080000,&h2092092480000,&h2002080080000,&h2092492492000
Data &h2080080402000,&h2092080002000,&h2482480002000,&h36C36C0003600,&h30DB0C0003000,&h30C00C0603000
Data &h10DB6DB6DB600,&h10030C00C0000,&h10DB0DB6C0000,&h10C36C00C0000,&h16C10430C0000,&h10010436C0000
Data &h1241043000000,&h1041240040000,&h1049049240000,&h1001040040000,&h1049249249200,&h1040040201000
Data &h1049040001000,&h1241240001200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3
Data &h36C30C30DB0DB,&h30030C36D8018,&h36C30C00DB0DB,&h30C36C00C30C3,&h30DB0DB6C36C3,&h30030C00C00C3
Data &h30DB6DB6DB6DB,&h30C00C06030C3,&h30DB0C00030C3,&h36C36C00036DB,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB,&h10030C00C00C3
Data &h10DB0DB6C1243,&h10C36C00C1043,&h16C10C1049049,&h10010C1248008,&h1241001049049,&h1041240041041
Data &h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h10C00C06030C3
Data &h10DB6DB6DB6DB,&h10030C00C00C3,&h10DB0DB6C1243,&h10C36C00C1043,&h16C3041049049,&h1003041248008
Data &h1240041049049,&h1041240041041,&h1049049241241,&h1001040040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h2482240001249
Data &h2092040001041,&h2040040201041,&h2049249249249,&h2001040040041,&h2091049241241,&h2081240041041
Data &h2481041049049,&h2001041248008,&h2401041049049,&h2041240041041,&h2049049241241,&h2001040040041
Data &h2049249249249,&h2040040201041,&h2049040001041,&h2441240001249,&h2482480002492,&h2092080002082
Data &h2080080402082,&h2092492492492,&h2002000000082,&h2092000000002,&h2080000000000,&h2480000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2480000000000,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C00C3,&h30C36C00C00C3,&h36C00000000C0,&h3000000000000
Data 0,&h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000,&h1000000000000
Data &h1049000000000,&h1241000000000,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h10DB6DB6DB6DB
Data &h10030C00C00C3,&h10DB0DB6C36C3,&h10C12400C30C3,&h12C10430DB0DB,&h10010436D8018,&h12410430DB0DB
Data &h10412400430C0,&h10490492436C1,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h12412400036DB,&h10490400030C3
Data &h10400402030C3,&h104924925B6DB,&h10010400400C3,&h10490492436C3,&h10412400410C3,&h12410410490DB
Data &h1001041248018,&h12410410490DB,&h1041240041003,&h1049049241243,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h36C36C00036D9,&h30DB0C00030C1,&h30C00C06030C1,&h30DB6DB6DB6D9,&h30030C00C00C1,&h30DB0DB6C36C1
Data &h30C36C00C3041,&h36C30C30DB049,&h30030C36D8008,&h36C30C30DB049,&h30C36C00C0041,&h30DB0DB6C1241
Data &h30030C00C0041,&h30DB6DB6C9249,&h30C00C0601041,&h30DB0C0001041,&h36C36C0001249,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB
Data &h10010C00C00C3,&h10490DB6C36C3,&h10412400C30C3,&h12410430DB0DB,&h10010436D8018,&h12410430DB0DB
Data &h10412400030C1,&h10490492436C1,&h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24836C00036DB,&h20930C00030C3,&h20800C06030C3
Data &h20926DB6DB6DB,&h20020C00C0043,&h20920DB6C1243,&h20826C0041041,&h24820C1049049,&h20020C1248008
Data &h24820C1049049,&h2082400041041,&h2092049241241,&h2002040040041,&h2049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,&h2480000000000,&h2090000000000,&h2080000000000,&h2092000000000
Data &h2002000000000,&h2092000000000,&h2082000000000,&h2482000000000,&h2002000000000,&h2482000000000
Data &h2082000000000,&h2092000000000,&h2002000000000,&h2092000000000,&h2080000000000,&h2090000000000
Data &h2480000000000,&h36C0000000000,&h10DB000000000,&h10C0000000000,&h10DB600000000,&h1003000000000
Data &h10DB000000000,&h10C3600000000,&h12C3000000000,&h1003000000000,&h1243000000000,&h1040000000000
Data &h1049000000000,&h1001000000000,&h1049200000000,&h1040000000000,&h1049000000000,&h1240000000000
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C1240001249,&h3049040001041,&h3040040201041
Data &h3049249249249,&h3001040040041,&h30C9049241241,&h30C1240041041,&h36C1041049049,&h3001041248008
Data &h36C1041049049,&h3001240041041,&h3049049241241,&h3001040040041,&h3049249249249,&h3040040201041
Data &h3049040001041,&h3241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3
Data &h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&hC36C00C30C3,&h10DB0DB6C36C3,&h10030C00C00C3
Data &h105B6DB6DB6DB,&h10400C06030C3,&h10490C00030C3,&h12412400036DB,&h36C36C00036DB,&h30DB0C00030C3
Data &h10C00C06030C3,&h10DB6DB6DB6DB,&h10030C00C00C3,&h10490492436C3,&h10412400430C3,&h124104104B0DB
Data &h1001041248018,&h12410410490DB,&h10412400410C3,&h1049049241240,&h1001040040041,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h24824800036DB,&h20920800030C3,&h10800804030C3,&h10924924936DB,&h10020800800C3,&h10920924826C3
Data &h10824800820C3,&h12810420920DB,&h1001042490018,&h12410420920DB,&h10412400820C3,&h1049049242401
Data &h1001040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,&h2482480000000
Data &h2092080000000,&h2080080400000,&h92492490000,&h2080080000,&h92092482000,&h82480082000
Data &h482082092000,&h2082490000,&h2082092000,&h2480082000,&h92482000,0,0,0,0,0,&h36C36C0003000
Data &h30DB0C0003000,&h30C00C0603000,&h30DB6DB6DB000,&h30030C00C0000,&h30DB0DB6C3600,&h30C36C00C3000
Data &h36C30C30DB000,&h30030C36D8000,&h36C30C30DB000,&h30C36C00C3000,&h30DB0DB6C0000,&h30000C00C0000
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24824800036DB,&h20920800030C3,&h20800806030C3
Data &h209249B6DB6DB,&h20020800C00C3,&h209209B6C36C1,&h20824800C30C1,&h24820830DB0C9,&h20020836D8008
Data &h24820830DB049,&h20824800C3041,&h2092092403641,&h2001080040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,&h2482480000000,&h2092080000000,&h2080080000000,&h2092480000000
Data &h2002080000000,&h2092080000000,&h2082480000000,&h2482080000000,&h2002080000000,&h2482080000000
Data &h2082480000000,&h2092092000000,&h2002080000000,&h2092480000000,&h2080080000000,&h2092080000000
Data &h2482480000000,&h2482480000000,&h2092080000000,&h2080080000000,&h2092490000000,&h2002080000000
Data &h2092090000000,&h2082480000000,&h2482080000000,&h2002080000000,&h2482080000000,&h2082480000000
Data &h2092090000000,&h2002080000000,&h2092490000000,&h2080080000000,&h2092080000000,&h2482480000000
Data &h2482480000000,&h2092080000000,&h2080080000000,&h2092492000000,&h2002080000000,&h2092092000000
Data &h2082480000000,&h2482080000000,&h2002080000000,&h2482080000000,&h2082480000000,&h2092080000000
Data &h2002080000000,&h2092492000000,&h2080080000000,&h2092080000000,&h2482480000000,&h36C36C0000000
Data &h30DB0C0000000,&h30C00C0600000,&h30DB6DB600000,&h30030C0000000,&h30DB0DB600000,&h30C36C0000000
Data &h36C30C0000000,&h30030C0000000,&h36C30C0000000,&h30C36C0000000,&h30DB000000000,&h3001040000000
Data &h1049249200000,&h1040040200000,&h1049040000000,&h1241240000000,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h26C36C00036DB,&h20DB0C00030C3,&h20C00C06030C3,&h20DB6DB6DB6DB,&h20030C00C00C3
Data &h20DB0DB6C36C1,&h20C36C00C30C1,&h24830C30DB049,&h20030C36D8008,&h24830C30DB049,&h20836C00C3041
Data &h20900DB6C3241,&h2001040040041,&h2049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data &h2000000000000,&h2480000000000,&h2000000000000,&h2480000000000,&h2080000000000,&h2080000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,0,0,&h3600000000000,&h3000000000000
Data &h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000,&h30C0000000000,&h36C0000000000
Data &h3000000000000,&h36C0000000000,&h30C0000000000,&h3000000000000,&h3000000000000,&h3000000000000
Data &h3000000000000,&h3000000000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3
Data &h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3,&hDB0DB6C36C3,&h10030C00C00C3
Data &h104B6DB6DB6DB,&h10400C06030C3,&h10490400030C3,&h12412400036DB,&h36C36C00036DB,&h10DB0C00030C3
Data &h10C00C06030C3,&h10DB6DB6DB6DB,&h10010400400C3,&h10490492436C3,&h10412400430C3,&h12410410490DB
Data &h1001041248018,&h12410410490DB,&h1041240041043,&h1049049241243,&h1001040040040,&h1049249249249
Data &h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h36C36C00036DB,&h10DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB,&h10010C00C00C3,&h10490DB6C36C3
Data &h10412400C30C3,&h12410430DB0DB,&h10010436D8018,&h12410430DB0DB,&h10412400430C1,&h10490492436C1
Data &h1001040040001,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h10DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB
Data &h10030C00C00C1,&h10DB0DB6C36C1,&h10C36C00C30C1,&h12C30C30DB049,&h10030C36D8008,&h12430C30DB049
Data &h10436C00C3041,&h10490DB6C3241,&h1001040000041,&h1049249249249,&h1040040201041,&h1049040001041
Data &h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h10DB0C00030C3,&h10C00C06030C3
Data &h10DB6DB6DB6DB,&h10030C00C00C1,&h10DB0DB6C36C1,&h10C36C00C30C1,&h12C30C30DB049,&h10030C36D8008
Data &h12430C30DB049,&h10436C00C3041,&h10490DB6C3241,&h1001000040041,&h1049249249249,&h1040040201041
Data &h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C0041,&h30DB0DB6C1241,&h30C36C0041041
Data &h36C30C1049049,&h30030C1248008,&h36C30C1049049,&h30C36C0041041,&h30DB049241241,&h3000040040041
Data &h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3
Data &h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0C9,&h30030C36D8008,&h36C30C30DB049,&h30C36C00C3041
Data &h30DB0DB6C3241,&h3040040041,&h1049249249249,&h1040040201041,&h1049040001041,&h1241240001249
Data &h24824800036DB,&h20920800030C3,&h20800804030C3,&h209249249B6DB,&h20020800800C3,&h20920924836C3
Data &h20824800820C3,&h24820820920DB,&h2002082490018,&h248208209209B,&h2082480082083,&h2092092482483
Data &h2002080080083,&h2092492492490,&h2080080402081,&h2092080002081,&h2482480001249,&h2482480000000
Data &h2092080000000,&h2080080400000,&h2092492480000,&h2002080080000,&h2092092480000,&h2082480082000
Data &h2482082092000,&h2002082490000,&h2482082092080,&h2082480082080,&h2092092482480,&h2002080080080
Data &h2092492492480,&h2080080402080,&h2092080002080,&h2482480002480,&h24824800036DB,&h20920800030C3
Data &h20800804030C3,&h20924924936DB,&h20020800800C3,&h20920924836C3,&h20824800830C3,&h248208209B0DB
Data &h2002082498018,&h248208209B0D8,&h20824800830C0,&h20920924836C0,&h20020800800C0,&h2092492492400
Data &h2080080402040,&h2092080002040,&h2482480001248,&h2482480000000,&h2092080000000,&h2080080400000
Data &h2092492490000,&h2002080080000,&h2092092480000,&h2082480080000,&h2482082080000,&h2002082480000
Data &h2482082080000,&h2082480080000,&h2092092480000,&h2002080080000,&h2092492492000,&h2080080402000
Data &h2092080002000,&h2482480002000,&h2482480002000,&h2092080002000,&h2080080402000,&h2092492492000
Data &h2002080080000,&h2092092482000,&h2082480082000,&h2482082092000,&h2002082490000,&h2482082090000
Data &h2082480080000,&h2092092480000,&h2002080080000,&h2092492490000,&h2080080401000,&h2092080001000
Data &h2482480001200,&h2482480002400,&h2092080002000,&h2080080402000,&h2092492492400,&h2002080080000
Data &h2092092482400,&h2082480082000,&h2482082092000,&h2002082490000,&h2482082092000,&h2082480080000
Data &h2092092480000,&h2002080080000,&h2092492480000,&h2080080400000,&h2092080000000,&h2482480000000
Data &h24824800036D8,&h20920800030C0,&h20800806030C0,&h209249B6DB6D8,&h20020800C00C0,&h209209B6C36C0
Data &h20824800C3000,&h24820830DB000,&h20020836D8000,&h24820830DB000,&h20824800C3000,&h209209B6C0000
Data &h20020800C0000,&h2092492400000,&h2080080400000,&h2092080000000,&h2482480000000,&h2482480000000
Data &h2092080000000,&h2080080000000,&h2092480000000,&h2002080000000,&h2092080000000,&h2082480000000
Data &h2482080000000,&h2002080000000,&h2482080000000,&h2082480000000,&h2092080000000,&h2002080000000
Data &h2092492000000,&h2080080200000,&h2092080000000,&h2482480000000,&h2482480000000,&h2092080000000
Data &h2080080000000,&h2092490000000,&h2002080000000,&h2092090000000,&h2082480000000,&h2482080000000
Data &h2002080000000,&h2482080000000,&h2082480000000,&h2092090000000,&h2002080000000,&h2092490000000
Data &h2080080000000,&h2092080000000,&h2482480000000,&h2482480000000,&h2092080000000,&h2080080000000
Data &h2092492000000,&h2002080000000,&h2092092000000,&h2082480000000,&h2482080000000,&h2002080000000
Data &h2482080000000,&h2082480000000,&h2092092000000,&h2002080000000,&h2092480000000,&h2080080000000
Data &h2092080000000,&h2482480000000,&h24836C0000000,&h20930C0000000,&h20800C0600000,&h20926DB600000
Data &h20020C0000000,&h20920DB600000,&h20826C0000000,&h24820C0000000,&h20020C0000000,&h24820C0000000
Data &h20826C0000000,&h20920DB600000,&h20020C0000000,&h2092400000000,&h2080040000000,&h2089040000000
Data &h2481240000000,&h2480000000000,&h2090000000000,&h2080000000000,&h2092000000000,&h2002000000000
Data &h2092000000000,&h2082000000000,&h2482000000000,&h2002000000000,&h2482000000000,&h2082000000000
Data &h2092000000000,&h2002000000000,&h2092000000000,&h2080000000000,&h2090000000000,&h2480000000000
Data &h36C0000000000,&h30DB000000000,&h30C0000000000,&h30DB600000000,&h3003000000000,&h30DB000000000
Data &h30C3600000000,&h36C3000000000,&h3003000000000,&h36C3000000000,&h30C3600000000,&h30DB000000000
Data &h2003000000000,&h2090000000000,&h2080000000000,&h2092000000000,&h2480000000000,0
Data 0,0,0,0,0,0,0,0,0,0,0,&h2000000000000,&h2080000000000,&h2080000000000,&h2092000000000
Data &h2482000000000,0,0,0,0,0,0,0,0,0,0,0,&h1000000000000,&h1000000000000,&h1000000000000
Data &h1040000000000,&h1049000000000,&h1241200000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data &h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C12C3
Data &h30C36C00C10C1,&h36C30C1049049,&h30030C1248008,&h36C30C1049049,&h30C36C0041041,&h30DB049241241
Data &h3001040040041,&h49249249249,&h1040040201041,&h1049040001041,&h1241240001249,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3
Data &h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3,&h30DB0DB6C36C3,&h30030C00C00C3
Data &h30DB6DB6DB6DB,&h30C00C06030C0,&h30DB0C0001041,&h36C36C0001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3
Data &h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3
Data &h30DB0DB6C36C3,&h30030C00C00C3,&h30DB6DB6DB6DB,&h30C00C0603003,&h30DB0C0001041,&h36C36C0001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018
Data &h36C30C30DB0DB,&h30C36C00C30C3,&h30DB0DB6C36C3,&h30030C00C00C3,&h30DB6DB6DB6DB,&h30C00C06000C3
Data &h30DB0C0001041,&h36C36C0001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB
Data &h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB
Data &h30C36C00C30C3,&h30DB0DB6C36C3,&h30030C00C00C3,&h30DB6DB6DB6DB,&h30C00C00030C3,&h30DB0C00030C3
Data &h36C36C00036DB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3
Data &h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3
Data &h30DB0DB6C36C3,&h10030C00C00C3,&h10DB6DB6DB6DB,&h10C00006030C3,&h10490400030C3,&h12412400036DB
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,&h16C36C00036DB,&h10DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB
Data &h10030C00C00C3,&h10DB0DB6C36C3,&h10C36C00C30C3,&h12430C30DB0DB,&h10030C36D8018,&h16C30C30DB0DB
Data &h10C36C00C30C3,&h10DB0DB6C36C3,&h10030C00C00C3,&h10DB6DB6DB6DB,&h10000C06030C3,&h10490400030C3
Data &h12412400036DB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3
Data &h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C1241,&h30C36C0041041,&h36C1041049049
Data &h3001041248008,&h36C1041049049,&h30C1240041041,&h30D9049241241,&h3001040040041,&h3049249249249
Data &h40040201041,&h1049040001041,&h1241240001249,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018
Data &h36C30C30DB0DB,&h30C36C00C30C3,&h30DB0DB6C36C3,&h30030C00C00C3,&h30DB6DB6DB6DB,&h30C00C0601043
Data &h30DB0C0001040,&h36C36C0001249,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB
Data &h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3
Data &h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3,&h30DB0DB6C36C3,&h30030C00C00C3
Data &h30DB6DB6DB6DB,&h30C00C06010C1,&h30DB0C0001001,&h36C36C0001249,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3
Data &h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3
Data &h30DB0DB6C36C3,&h30030C00C00C3,&h30DB6DB6DB6DB,&h30C00C0603041,&h30DB0C0000041,&h36C36C0001249
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3
Data &h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB,&h30030C36D8018,&h36C30C30DB0DB,&h30C36C00C30C3
Data &h10DB0DB6C36C3,&h10030C00C00C3,&h10DB6DB6DB6DB,&h10400C06030C3,&h10490000030C3,&h12412400036DB
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h2481240001249,&h2091040001041,&h2080040201041
Data &h2092249249249,&h2002040040041,&h2092049241241,&h2082240041041,&h2482041049049,&h2002041248008
Data &h2482041049049,&h2082240041041,&h2092049241241,&h2002040040041,&h2092249249249,&h2080040201041
Data &h2090040001041,&h2481240001249,&h2480000000000,&h2092000000000,&h2080000000000,&h2092400000000
Data &h2002000000000,&h2092000000000,&h2082400000000,&h2482000000000,&h2002000000000,&h2482000000000
Data &h2082400000000,&h2092000000000,&h2002000000000,&h2092400000000,&h2080000000000,&h2080000000000
Data &h2480000000000,&h12C36C00036DB,&h10DB0C00030C3,&h10C00C06030C3,&h10DB6DB6DB6DB,&h10030C00C00C3
Data &h10DB0DB6C36C3,&h10436C00C30C3,&h12430C30DB0DB,&h10030C36D8018,&h12C30C30DB0DB,&h10C36C00C30C3
Data &h10DB0DB6C36C3,&h10030C00C00C3,&h10DB6DB6DB6DB,&h10C00006030C3,&h10000000030C3,&h12400000036DB
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3,&h30C00C06030C3
Data &h30DB6DB6DB6DB,&h3003040040041,&h30DB049241241,&h30C1240041041,&h36C1041049049,&h3001041248008
Data &h36C1041049049,&h30C1240041041,&h30C9049241241,&h3001040040041,&h3049249249249,&h3040040201041
Data &h49040001041,&h1241240001249,&h24824800036DB,&h20920800030C3,&h20800804030C3,&h209249249B6DB
Data &h20020800800C3,&h20920924836C3,&h20824800820C3,&h24820820920DB,&h2002082490018,&h248208209209B
Data &h2082480082083,&h2092092482483,&h2002080080083,&h209249249249B,&h2080080402083,&h2092080002083
Data &h2482480002490,&h2482480000000,&h2092080000000,&h2080080400000,&h2092492480000,&h2002080080000
Data &h2092092480000,&h2082480082000,&h2482082092000,&h2002082490000,&h2482082092080,&h2082480082080
Data &h2092092482480,&h2002080080080,&h2092492492480,&h2080080402080,&h2092080002080,&h2482480002480
Data &h24824800036DB,&h20920800030C3,&h20800804030C3,&h20924924936DB,&h20020800800C3,&h20920924836C3
Data &h20824800830C3,&h248208209B0DB,&h2002082498018,&h248208209B0D8,&h20824800830C0,&h20920924836C0
Data &h20020800800C0,&h20924924926D8,&h20800804020C0,&h20920800020C0,&h2482480002400,&h2482480000000
Data &h2092080000000,&h2080080400000,&h2092492490000,&h2002080080000,&h2092092480000,&h2082480080000
Data &h2482082080000,&h2002082480000,&h2482082080000,&h2082480080000,&h2092092480000,&h2002080080000
Data &h2092492492000,&h2080080402000,&h2092080002000,&h2482480002000,&h36C36C0003000,&h30DB0C0003000
Data &h30C00C0603000,&h30DB6DB6DB000,&h30030C00C0000,&h30DB0DB6C3000,&h30C36C00C3000,&h36C30C30DB000
Data &h30030C36D8000,&h36C30C30D8000,&h30C36C00C0000,&h30DB0DB6C0000,&h30030C00C0000,&h30DB6DB6DB600
Data &h30C00C0603000,&h30DB0C0003000,&h36C36C0000000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h36C36C00036DB,&h30DB0C00030C3
Data &h30C00C06030C3,&h30DB6DB6DB6DB,&h30030C00C00C3,&h30DB0DB6C36C3,&h30C36C00C30C3,&h36C30C30DB0DB
Data &h30030C36D8018,&h36C30C30DB0DB,&h20C36C00C30C3,&h20DB0DB6C36C3,&h20030C00C00C3,&h209B6DB6DB6DB
Data &h20800C06030C3,&h20920C00030C3,&h24824000036DB,0,0,0,0,0,0,0,0,0,0,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2080000000000,&h2080000000000,&h2092000000000,&h2482000000000
Data &h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000
Data &h3000000000000,&h36C0000000000,&h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000
Data &h3000000000000,&h30D8000000000,&h30C0000000000,&h30DB000000000,&h36C0000000000,0
Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&h24836C00036DB,&h20DB0C00030C3,&h20C00C06030C3,&h20DB6DB6DB6DB
Data &h20030C00C00C3,&h209B0DB6C36C3,&h20836C00C30C3,&h24830C30DB0DB,&h20030C36D8018,&h24830C30DB0DB
Data &h20C36C00C30C3,&h20DB0DB6C36C3,&h20030C00C00C3,&h20DB6DB6DB6DB,&h20C00C06030C3,&h20DB0C00030C3
Data &h24036C00036DB,&h2480000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data &h2080000000000,&h2080000000000,&h2480000000000,&h2000000000000,&h2480000000000,&h2000000000000
Data &h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000,&h2000000000000
Data &h36C3000000000,&h30DB000000000,&h3000000000000,&h3000000000000,&h3000000000000,&h30D8000000000
Data &h30C0000000000,&h36C0000000000,&h3000000000000,&h36C0000000000,&h30C0000000000,&h3000000000000
Data &h3000000000000,&h3000000000000,&h3000000000000,&h3000000000000,0
