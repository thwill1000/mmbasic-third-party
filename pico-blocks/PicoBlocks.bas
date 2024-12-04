'-------------------------------------------------------------------------------
' Blocks
' (c) 2023-2024 Geoff Graham
'
' v1.1.0 - Written for the PicoGamer or Game*Mite - by Geoff Graham, Dec 2023
' v1.2.0 - Adapted for MMB4L Gamepack             - by Tom Williams, Dec 2024
'-------------------------------------------------------------------------------

Option Base 0
Option Default Integer
Option Explicit

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
game.init_window("Pico-Blocks", VERSION)
ctrl.init_keys()
Dim ctrl$ = ctrl.default_driver$()
If ctrl.open_no_error%(ctrl$) <> sys.SUCCESS Then ctrl$ = "ctrl.no_controller"

Randomize Timer
Const TitleF = 2, TitleFS = 1        ' font and font scaling used for the title
Const GeneralF = 1, GeneralFS = 1    ' font and font scaling used for everything else

'''''''''''''''''''''''''''''''''''''''''''''''''''''
' These constants define the look of the game
' ===========================================
'
' width and height of the board in block size
' note these must be one less than the actual size
' so this is a board made up of 10 x 21 blocks
Const Bdw = 9, Bdh = 20
'
' background colour
Const BackColour = &H4000
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''


' the falling pieces are made up of these basic blocks
' define the width and height in pixels
Const Bh = Int(MM.VRes/(Bdh + 1)), Bw = Bh - 1


' the top left origin of the board in pixels
Const Bdx = MM.HRes/2 - (Bw * Bdw)/2), Bdy = Bh/4

' centre of the left and right side material in pixels
Const LeftX = Bdx / 2)
Const RightX = ((((MM.HRes/6) * 5) \ Bw) * Bw) + Bw
Const NextWinX = RightX - Bw * 2
Const NextWinY = Bh * 4

' define globals
Dim Board(Bdw, Bdh)
Dim Pieces(6, 3, 3, 3)
Dim PieceColour(6)
Dim p, r, x, y
Dim key%
Dim NewPiece, NextPiece, NextRotation
Dim Score, Difficulty = 3
Dim i
Dim note!
Dim r_next
Dim num_lines

FRAMEBUFFER CREATE
LoadPieces
Font GeneralF, GeneralFS
Colour RGB(white), BackColour

StartGame:
DrawScreen
ClearBoard

'''''''''''''''''''''''''''''''''''''''''''''''''''''''
' main program loop

Do
  ClearNext
  GetDifficulty
  NextPiece = Int(Rnd * 6)
  NextRotation = Int(Rnd * 4)
  NewPiece = 1
  Score = 0
  num_lines = 0

  ' the game runs within this loop
  ' get a character from the keyboard and act on it
  ' also implement the automatic descent of the piece.
  Do
    ' if a flag has been set, introduce
    ' a new piece at the top of the board
    If NewPiece Then
      NewPiece = 0
   ' identify the new piece, its rotation and location
      p = NextPiece
      r = NextRotation
      x = Bdw/2 - 2 + Int(Rnd * 2)
      y = -3
      If Not CheckValidMove(p, r, x, y) Then
        show_game_over()
        MakeSound 3
        Exit
      EndIf
   ' draw the new piece
      DrawPiece p, r, x, y, 0
   ' update the window showing the next piece to be launched
      ErasePiece NextPiece, NextRotation, NextWinX / Bw, NextWinY / Bh, Rgb(Black), 1
      NextPiece = Int(Rnd * 6)
      NextRotation = Int(Rnd * 4)
      DrawPiece NextPiece, NextRotation, NextWinX / Bw, NextWinY / Bh, Rgb(Black), 1
    EndIf

    ' process any key presses
    key% = get_input%()
    Select Case key%
      Case ctrl.LEFT
        If CheckValidMove(p, r, x - 1, y) Then
          ErasePiece p, r, x, y
          x = x - 1
          DrawPiece p, r, x, y
          MakeSound 2
        EndIf
      Case ctrl.RIGHT
        If CheckValidMove(p, r, x + 1, y) Then
          ErasePiece p, r, x, y
          x = x + 1
          DrawPiece p, r, x, y
          MakeSound 2
        EndIf
      Case ctrl.DOWN
        If Timer > 50 Then Timer = 550 ' Bring forward the next move down.
      Case ctrl.A, ctrl.B
        r_next = r + Choice(key% = ctrl.A, 1, -1)
        r_next = Choice(r_next > 3, 0, Choice(r_next < 0, 3, r_next))
        If CheckValidMove(p, r_next, x, y) Then
          ErasePiece p, r, x, y
          r = r_next
          DrawPiece p, r, x, y
          MakeSound 1
        EndIf
      Case ctrl.UP ' Quick drop
        Play Stop
        note! = 4000
        Do While CheckValidMove(p, r, x, y + 1)
          If note! > 16.0 Then Play Sound 4, B, S, note!, 25
          note! = note! *0.8
          ErasePiece p, r, x, y
          y = y + 1
          DrawPiece p, r, x, y
          Pause 5
          Score = Score + 1
        Loop
        Play Stop
     ' we have reached the bottom or hit another piece
        SavePieceToBoard p, r, x, y
        RemoveBridges
        NewPiece = 1
        Timer = 0
      Case ctrl.START
        Text LeftX, MM.VRes/5, "GAME PAUSED", c, , , RGB(red), BackColour
        Text LeftX, MM.VRes/5 + MM.Info(FONTHEIGHT), "START=RESTART", c, , , RGB(red), BackColour
        Text LeftX, MM.VRes/5 + MM.Info(FONTHEIGHT) * 2, "SELECT=EXIT", c, , , RGB(red), BackColour
        MakeSound 0
        Do
          i = get_input%()
          If i = ctrl.SELECT Then
            If QuitCheck() Then
              Box Bdx - 2, Bdy - 2, ((Bdw + 1) * Bw) + 3, ((Bdh + 1) * Bh) + 3, 1, RGB(white), 0
              GoTo StartGame
            EndIf
          EndIf
        Loop Until i = ctrl.A Or i = ctrl.START
        Text LeftX, MM.VRes/5, "           ", c, , , BackColour, BackColour
        Text LeftX, MM.VRes/5 + MM.Info(FONTHEIGHT), "             ", c, , , BackColour, BackColour
        Text LeftX, MM.VRes/5 + MM.Info(FONTHEIGHT) * 2, "             ", c, , , BackColour, BackColour
        Timer = 0
      Case ctrl.SELECT
        If QuitCheck() Then
          Box Bdx - 2, Bdy - 2, ((Bdw + 1) * Bw) + 3, ((Bdh + 1) * Bh) + 3, 1, RGB(white), 0
          GoTo StartGame
        EndIf
    End Select

    ' cause the piece to automatically drop down one row
    If Timer > 550 - Difficulty * 50 Then
      If CheckValidMove(p, r, x, y + 1) Then
        ErasePiece p, r, x, y
        y = y + 1
        DrawPiece p, r, x, y
        Timer = 0
      Else
     ' we have reached the bottom or hit another piece
        SavePieceToBoard p, r, x, y
        RemoveBridges
        NewPiece = 1
      EndIf
    EndIf

    ' update the score
    Text LeftX, (MM.VRes/4)*3, "Score", c,,,,BackColour
    Text LeftX, (MM.VRes/4)*3 + MM.Info(FONTWIDTH)*2, Str$(Score * Difficulty), c,,2,,BackColour
  Loop
Loop


Sub show_game_over()
  Local w% = bdw * bw, h% = bdy + bdh * bh
  Box bdx + 4, bdy + h% / 2 - Mm.Info(FontHeight), w%, 3 * Mm.Info(FontHeight) - 4, , 0, 0
  Text bdx + w% / 2 + 4, bdy + h% / 2 - 2, "GAME OVER", c, , , RGB(Red), RGB(Black)
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' PicoGamer specific routines
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


Function get_input%()
  Static last_input%
  Call ctrl$, get_input%
  If Not get_input% Then keys_cursor_ext(get_input%)
  If get_input% = ctrl.HOME Then get_input% = ctrl.SELECT
  ' Only DOWN is allowed to auto-repeat.
  If get_input% <> ctrl.DOWN And get_input = last_input% Then
    get_input% = 0
  Else
    last_input% = get_input%
  EndIf
End Function


' handle the QUIT action by the player
Function QuitCheck()
  Local key, state, s$, tt
  tt = Timer
  MakeSound 1
  FrameBuffer Copy N , F
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
      Do : key = get_input%() : Loop While key = 0
      If key = ctrl.LEFT And State = 0 Then
        State = 1 : MakeSound 1 : Exit Do
      ElseIf key = ctrl.RIGHT And State = 1 Then
        State = 0 : MakeSound 1 : Exit Do
      ElseIf key = ctrl.A Or key = ctrl.SELECT Then
        MakeSound 1
        Do While get_input%() : Loop
        If state = 0 Then
          FrameBuffer Copy F , N
          Timer = tt
          Exit Function
        Else
          QuitCheck = 1
          Exit Function
        EndIf
      Else
        MakeSound 2
      EndIf
    Loop
  Loop
End Function


' make a confirmation sound or an error sound
' from PicoVaders by MARTIN HERHAUS
Sub MakeSound Nbr
  Local i
  If Nbr = 1 Then
    Local notes!(3) = (987.77, 1567.98, 1975.53, 30.87)
  ElseIf Nbr = 2 Then
    Local notes!(4) = (1046.50, 987.77, 739.99, 698.46, 30.87)
  Else
    Local notes!(10) = (3000, 2500, 2000, 1500, 1000, 900, 800, 700, 600, 500, 400)
  EndIf
  Play Stop
  For i = Bound(notes!(), 0) To Bound(notes!(), 1)
    If notes!(i) > 16.0 Then Play Sound 4, B, S, notes!(i), 25
    Pause 40
  Next
  Play Stop
End Sub


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' General subroutines and functions
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


' search through the array Board(x,y) which represents the on screen display
' detect any completely filled horizontal lines and remove them
' then redraw the on screen image
Sub RemoveBridges
  Const flash = 20
  Local x, y, oy, count
  For y = Bdh To 0 Step -1
  Do While y > 0
    count = 0
    For x = 0 To Bdw
      If Board(x, y) Then count = count + 1
    Next x
    If count > Bdw Then
      ' found a full row - flash the row in various colours
      Play Sound 4, B, N, 800, 25
      For x = 0 To Bdw : DrawBlock(RGB(red), 0 , x, y) : Next x
      Pause flash
      For x = 0 To Bdw : DrawBlock(RGB(rust), 0, x, y) : Next x
      Pause flash
      For x = 0 To Bdw : DrawBlock(RGB(brown), 0, x, y) : Next x
      Pause flash
      For x = 0 To Bdw : DrawBlock(RGB(black), 0, x, y) : Next x
      Pause flash * 3
      ' copy down the board to delete the row
      For oy = y To 1 Step -1
        For x = 0 To Bdw
          Board(x, oy) = Board(x, oy - 1)
        Next x
      Next oy
      ' erase the top row
      For x = 0 To Bdw : Board(x, 0) = 0 : Next x
      ' redraw the entire board
      For y = 0 To Bdh
        For x = 0 To Bdw
          DrawBlock Board(x, y), 0, x, y
        Next x
      Next y
      Score = Score + 40
      Inc num_lines
      If num_lines = 10 Then
        num_lines = 0
        IncDifficulty()
      EndIf
      Play Stop
    EndIf
    y = y - 1
  Loop
End Sub


' draw a piece on the screen
' p = index of the piece, r = rotation, ox & oy top left origin
' bc = background colour
Sub DrawPiece p, r, ox, oy, bc, no_offset
  Local x, y
  For y = 0 To 3
    For x = 0 To 3
      If Pieces(p, r, x, y) Then
       DrawBlock(PieceColour(p), bc, ox + x, oy + y, no_offset)
      EndIf
    Next x
  Next y
End Sub


' draw a block (square portion of a piece) on the screen
' c = colour, bc = background colour, ox & oy top left origin
Sub DrawBlock c, bc, ox, oy, no_offset
  If oy < 0 Then Exit Sub
  Local x = (ox * Bw) + Bdx * Not(no_offset)
  Local y = (oy * Bh) + Bdy * Not(no_offset)
  If c Then
    Box x - 1, y - 1, Bw + 1, Bh + 1, 1, bc, c
    If MM.HRes < 500 Then
        Line x + 1, y + 1, x + Bw - 3, y + 1, 1, bc          ' top shadow
        Line x + Bw - 3, y + 1, x + Bw - 3, y + Bh - 3, 1, bc ' right shadow
    ElseIf MM.HRes < 800 Then
        Line x + 2, y + 3, x + Bw - 5, y + 3, 1, bc          ' top shadow
        Line x + Bw - 5, y + 3, x + Bw - 5, y + Bh - 5, 1, bc ' right shadow
    Else
        Line x + 3, y + 4, x + Bw - 6, y + 4, 2, bc          ' top shadow
        Line x + Bw - 6, y + 4, x + Bw - 6, y + Bh - 6, 2, bc ' right shadow
    EndIf
  Else
    Box x, y, Bw, Bh, 1, c, c  ' fast erase for blank block
  EndIf
End Sub


' erase a piece from the screen
' p = index of the piece, r = rotation, ox & oy top left origin
' bc = background colour
Sub ErasePiece p, r, ox, oy, bc, no_offset
  Local x, y
  For y = 0 To 3
    For x = 0 To 3
      If Pieces(p, r, x, y) Then
        DrawBlock(bc, bc, ox + x, oy + y, no_offset)
      EndIf
    Next x
  Next y
End Sub


' check if this piece is valid, returns true if yes
' valid means within borders and not overlapping another piece
' p = index of the piece, r = rotation, ox & oy top left origin
Function CheckValidMove(p, r, ox, oy)
  Local x, y
  For y = 0 To 3
    For x = 0 To 3
      If Pieces(p, r, x, y) Then
        If ox + x < 0 Then Exit Function
        If ox + x > Bdw Then Exit Function
        If oy + y > Bdh Then Exit Function
        If oy + y >= 0 Then
          If Board(ox + x, oy + y) Then Exit Function
        EndIf
      EndIf
    Next x
  Next y
  CheckValidMove = 1
End Function


' copy the colour of a piece to Board(x,y) representing the screen
' this is only done when a falling piece has stopped falling
' p = index of the piece, r = rotation, ox & oy top left origin
Sub SavePieceToBoard p, r, ox, oy
  Local x, y
  For y = 0 To 3
    For x = 0 To 3
      If Pieces(p, r, x, y) Then
        If oy + y >= 0 Then Board(ox + x, oy + y) = PieceColour(p)
      EndIf
    Next x
  Next y
End Sub


' initialise the screen and draw the static elements
Sub DrawScreen
  Local x, y

  CLS
  Text LeftX, 5, "Blocks", c, TitleF, TitleFS, RGB(white), BackColour

  Colour RGB(cyan)
  Font 7
  y = 14 * Mm.Info(FONTHEIGHT)
  Text RightX, y, "LEFT/RIGHT/DOWN", c
  y = y + MM.Info(FONTHEIGHT)
  Text RightX, y + 2, "MOVE PIECE", c
  y = y + MM.Info(FONTHEIGHT) * 2 + 4
  Text RightX, y, "A  = CLOCKWISE", c
  y = y + MM.Info(FONTHEIGHT) + 2
  Text RightX, y, "B  = ANTICLOCK", c
  y = y + MM.Info(FONTHEIGHT) + 2
  Text RightX, y, "UP = FAST DROP", c
  Font GeneralF, GeneralFS
  Colour RGB(white)
End Sub


' clear the on screen board and the array Board(x, y)
Sub ClearBoard
  Local x, y
  Box Bdx - 2, Bdy - 2, ((Bdw + 1) * Bw) + 3, ((Bdh + 1) * Bh) + 3, 1, RGB(white), 0
  For x = 0 To Bdw
    For y = 0 To Bdh
      Board(x, y) = 0
  Next y, x
End Sub


' clear the box that shows the next piece to be introduced
Sub ClearNext
  Box NextWinX - Bw, NextWinY - Bh, Bw * 6, Bh * 6, 1, Rgb(White), Rgb(Black)
  Text RightX, MM.Info(FONTHEIGHT), "NEXT", c
End Sub


' this is responsible for getting the difficulty level
' from the user.  It then sets up the left hand
' side of the screen for the game.
Sub GetDifficulty
  Local x, y, key, dPos

  y = MM.VRes/3
  Box 0, y, Bdx - 2, MM.VRes/3, 1, BackColour, BackColour
  Box (MM.HRes/3) * 2 + 3, MM.Info(FONTHEIGHT) * 14, MM.HRes, MM.VRes, 1, BackColour, BackColour
  Text LeftX, 5, "Blocks", c, TitleF, TitleFS, RGB(white), BackColour
  Text LeftX, y, "DIFFICULTY:  ", c
  dPos = Mm.Info(FONTWIDTH) * 13
  y = y + MM.Info(FONTHEIGHT)
  Text LeftX, y, "USE UP/DOWN", c
  y = y + MM.Info(FONTHEIGHT)
  Text LeftX, y, "TO CHANGE", c
  y = y + MM.Info(FONTHEIGHT) * 2
  Text LeftX, y, "START  = PLAY", c
  y = y + MM.Info(FONTHEIGHT)
  Text LeftX, y, "SELECT = EXIT", c
  y = y + MM.Info(FONTHEIGHT)

  Text dPos, MM.VRes/3, Str$(Difficulty), c, , , &HFFFFFF, 0

  Do
    Select Case get_input%()
      Case ctrl.UP
        If Difficulty < 9 Then Difficulty = Difficulty + 1
        Text dPos, MM.VRes/3, Str$(Difficulty), c, , , &HFFFFFF, 0
      Case ctrl.DOWN
        If Difficulty > 1 Then Difficulty = Difficulty - 1
        Text dPos, MM.VRes/3, Str$(Difficulty), c, , , &HFFFFFF, 0
      Case ctrl.SELECT
        game.end()
      Case ctrl.A, ctrl.START
        Colour RGB(cyan)
        Font 7
        Text RightX, MM.Info(FONTHEIGHT) * 22 + 2, "START  = PAUSE", c
        Text RightX, MM.Info(FONTHEIGHT) * 23 + 4, "SELECT = QUIT ", c
        Font GeneralF, GeneralFS
        Colour RGB(white)
        Box 0, MM.Info(FONTHEIGHT) * 2, Bdx - 2, MM.HRes - MM.Info(FONTHEIGHT) * 2, 1, BackColour, BackColour
        Text LeftX, 5, "Blocks", c, TitleF, TitleFS, RGB(white), BackColour
        Text LeftX, MM.VRes/3 + MM.Info(FONTHEIGHT) * 2, "Difficulty", c
        Text LeftX, MM.VRes/3 + MM.Info(FONTHEIGHT) * 3, Str$(Difficulty), c
        ClearBoard
        Exit Sub
    End Select
  Loop
End Sub


Sub IncDifficulty()
  Inc Difficulty
  Text LeftX, MM.VRes/3 + MM.Info(FONTHEIGHT) * 2, "Difficulty", c
  Text LeftX, MM.VRes/3 + MM.Info(FONTHEIGHT) * 3, Str$(Difficulty), c
End Sub


' read the data statements and load the pieces into the
' array Pieces((p, r, x, y) where p = index of the piece,
' r = rotation, x & y are the blocks that make up the piece
Sub LoadPieces
  Local p, r, x, y
  Restore piece_data
  For p = 0 To 6
    Read PieceColour(p)
    For r = 0 To 3
      For y = 0 To 3
        For x = 0 To 3
          Read Pieces(p, r, x, y)  ' 1 = block
        Next x
      Next y
    Next r
  Next p
End Sub


piece_data:

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' These define each piece which is represented as a 4x4 array
' with 1 = a block to be drawn & 0 = nothing (empty).
' The four lines represent the block rotated 0, 90, 180 & 270 degrees
' Uses the "Nintendo rotation system":
'   https://strategywiki.org/wiki/Tetris/Rotation_systems

' define piece 1
Data RGB(white)

Data 0,0,0,0
Data 0,0,0,0
Data 1,1,1,1
Data 0,0,0,0

Data 0,0,1,0
Data 0,0,1,0
Data 0,0,1,0
Data 0,0,1,0

Data 0,0,0,0
Data 0,0,0,0
Data 1,1,1,1
Data 0,0,0,0

Data 0,0,1,0
Data 0,0,1,0
Data 0,0,1,0
Data 0,0,1,0

' define Piece 2
Data RGB(magenta)

Data 0,0,0,0
Data 0,0,0,0
Data 0,1,1,1
Data 0,0,1,0

Data 0,0,0,0
Data 0,0,1,0
Data 0,1,1,0
Data 0,0,1,0

Data 0,0,0,0
Data 0,0,1,0
Data 0,1,1,1
Data 0,0,0,0

Data 0,0,0,0
Data 0,0,1,0
Data 0,0,1,1
Data 0,0,1,0

' define piece 3
Data &H80FF   'rgb(cerulean)

Data 0,0,0,0
Data 0,0,0,0
Data 0,1,1,1
Data 0,0,0,1

Data 0,0,0,0
Data 0,0,1,0
Data 0,0,1,0
Data 0,1,1,0

Data 0,0,0,0
Data 0,1,0,0
Data 0,1,1,1
Data 0,0,0,0

Data 0,0,0,0
Data 0,0,1,1
Data 0,0,1,0
Data 0,0,1,0

' define piece 4
Data RGB(cyan)

Data 0,0,0,0
Data 0,0,0,0
Data 0,1,1,1
Data 0,1,0,0

Data 0,0,0,0
Data 0,1,1,0
Data 0,0,1,0
Data 0,0,1,0

Data 0,0,0,0
Data 0,0,0,1
Data 0,1,1,1
Data 0,0,0,0

Data 0,0,0,0
Data 0,0,1,0
Data 0,0,1,0
Data 0,0,1,1

' define piece 5
Data RGB(yellow)

Data 0,0,0,0
Data 0,1,1,0
Data 0,1,1,0
Data 0,0,0,0

Data 0,0,0,0
Data 0,1,1,0
Data 0,1,1,0
Data 0,0,0,0

Data 0,0,0,0
Data 0,1,1,0
Data 0,1,1,0
Data 0,0,0,0

Data 0,0,0,0
Data 0,1,1,0
Data 0,1,1,0
Data 0,0,0,0

' define piece 6
Data RGB(red)

Data 0,0,0,0
Data 0,0,0,0
Data 0,0,1,1
Data 0,1,1,0

Data 0,0,0,0
Data 0,0,1,0
Data 0,0,1,1
Data 0,0,0,1

Data 0,0,0,0
Data 0,0,0,0
Data 0,0,1,1
Data 0,1,1,0

Data 0,0,0,0
Data 0,0,1,0
Data 0,0,1,1
Data 0,0,0,1

' define piece 7
Data RGB(green)

Data 0,0,0,0
Data 0,0,0,0
Data 0,1,1,0
Data 0,0,1,1

Data 0,0,0,0
Data 0,0,0,1
Data 0,0,1,1
Data 0,0,1,0

Data 0,0,0,0
Data 0,0,0,0
Data 0,1,1,0
Data 0,0,1,1

Data 0,0,0,0
Data 0,0,0,1
Data 0,0,1,1
Data 0,0,1,0
