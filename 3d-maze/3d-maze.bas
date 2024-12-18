' 3D Maze Game
' Copyright (c) 2022-2024 Martin Herhaus
' GameMite port by Thomas H. Williams

Option Explicit On

Const VERSION = 101302 ' 1.1.2

'!if defined(PICOMITEVGA)
  '!replace { Option Simulate "Colour Maximite 2" } { Option Simulate "PicoMiteVGA" }
  '!replace { Page Copy 1 To 0 , B } { FrameBuffer Copy F , N , B }
  '!replace { Page Write 1 } { FrameBuffer Write F }
  '!replace { Page Write 0 } { FrameBuffer Write N }
  '!replace { Mode 7 } { Mode 2 : FrameBuffer Create }
  '!dynamic_call atari_a
  '!dynamic_call nes_a
'!elif defined(GAMEMITE)
  '!replace { Option Simulate "Colour Maximite 2" } { Option Simulate "Game*Mite" }
  '!replace { Page Copy 1 To 0 , B } { FrameBuffer Copy F , N }
  '!replace { Page Write 1 } { FrameBuffer Write F }
  '!replace { Page Write 0 } { FrameBuffer Write N }
  '!replace { Mode 7 } { FrameBuffer Create }
  '!dynamic_call ctrl.gamemite
'!else
  '!dynamic_call atari_dx
  '!dynamic_call wii_classic_3
'!endif

If Mm.Device$ = "MMB4L" Then
  Option Simulate "Colour Maximite 2"
  Option CodePage CMM2
EndIf

#Include "../splib/system.inc"
#Include "../splib/ctrl.inc"
#Include "../splib/string.inc"
#Include "../splib/msgbox.inc"
#Include "../splib/game.inc"

'!dynamic_call game.on_break
sys.override_break("game.on_break")

'!if defined(GAMEMITE)
  '!uncomment_if true
  ' Const VERSION_STRING$ = "Game*Mite Version " + sys.format_version$(VERSION)
  ' Dim CONTROLLERS$(1) = ("keys_cursor_ext", "ctrl.gamemite")
  '!endif
'!else
If sys.is_platform%("pmvga") Then
  Dim CONTROLLERS$(2) = ("keys_cursor_ext", "nes_a", "atari_a")
ElseIf sys.is_platform%("gamemite") Then
  Dim CONTROLLERS$(1) = ("keys_cursor_ext", "ctrl.gamemite")
ElseIf sys.is_platform%("pm*", "mmb4w") Then
  Dim CONTROLLERS$(1) = ("keys_cursor_ext", "ctrl.no_controller")
ElseIf sys.is_platform%("cmm2*") Then
  Dim CONTROLLERS$(2) = ("keys_cursor_ext", "wii_classic_3", "atari_dx")
Else
  Error "Unsupported device: " + Mm.Device$
EndIf
Const VERSION_STRING$ = "Version " + sys.format_version$(VERSION)
'!endif

Const STATE_SHOW_TITLE = 0, STATE_PLAY_GAME = 1, STATE_WIN_GAME = 2

Const MAP_Y = 37 ' Vertical offset for mini-map.

Dim pd% ' Player direction (N=0,E=1,S=2,W=3)
Dim redraw%
Dim tmp_int%
Dim state%

'!if !defined(GAMEMITE)
If sys.is_platform%("mmb4w", "cmm2*") Then Option Console Serial
'!endif
Mode 7
game.init_window("3D Maze", VERSION)
Font 1
Page Write 1

ctrl.init_keys()
Dim ctrl$ = show_title$()

Dim WallC1% = 0
Dim WallC2% = RGB(RED)
'Read the XY-Coordinates of the Wall Elements Corners
Dim Wall%(6,4,2)
Dim N%, C%, F%
Restore wall_data
For N%=0 To 5 'Walls
 For C%=0 To 3 '4 Corners
   For F%=0 To 1 '2 Coordinates (X,Y)
     Read Wall%(N%,C%,F%)
   Next
 Next
Next
Dim MazeW%=24
Dim MazeH%=24
' create array and fill
Dim Maze$(MazeW%,MazeH%) length 1
Dim ex_x%, ex_y%, key%, MovDir$, oldkey%, ox%, oy%, plrx%, plry%, show_map%, x%, xs%, y%, ys%
restart_game:
For x% = 0 To MazeW%
  For y% = 0 To MazeH%
    Maze$(x%, y%) = "#"
  Next
Next
'generate random Maze
generator
MovDir$=Chr$(146)+Chr$(148)+Chr$(147)+Chr$(149)
show_map%=0
Cls Rgb(White)

' Place player
PlrX% = MazeW% - 1
PlrY% = MazeH% - 1
For pd% = 0 To 3
  If Not blocked%(pd%) Then Exit For
Next
If pd% = 4 Then Error "Invalid state"

'place Exit
Ex_X%=2:Ex_Y%=0:If Maze$(Ex_X%,1)="#" Then Inc Ex_X%
Maze$(Ex_X%,Ex_Y%)="E"
If show_map% Then Box 243+Ex_X%*3,MAP_Y+Ex_Y%*3,3,3,,WallC2%,WallC2%

Colour 0, Rgb(White)
Text 280, 10, "D  X  Y", CT
Select Case ctrl$
  Case "ctrl.gamemite"
    Text 280, 164, "GAMEPAD", CT
  Case "nes_a"
    Text 280, 164, "NES PAD", CT
  Case "snes_a"
    Text 280, 164, "SNES PAD", CT
  Case "wii_classic_3"
    Text 280, 164, "WII CTRL", CT
  Case "atari_a"
    Text 280, 164, "ATARI JOY", CT
  Case "keys_cursor_ext"
    Text 280, 120, "KEYS:    ", CT
    Text 280, 131, Chr$(146)+" FORWARD", CT
    Text 280, 142, Chr$(147)+" BACKWRD", CT
    Text 280, 153, Chr$(149)+" TURN L.", CT
    Text 280, 164, Chr$(148)+" TURN R.", CT
End Select
Text 280, 175, Choice(ctrl$ = "keys_cursor_ext", "M", "B") + " MAP ON ", CT

' --- Game Loop ---
state% = STATE_PLAY_GAME
redraw% = 1
Do
  If redraw% Then
    Text 280,20," " + Mid$(MovDir$,PD%+1,1)+" "+Str$(PlrX%,2)+" "+Str$(PlrY%,2)+" ",CT
    Draw_3D
    Select Case PD%
      Case 0:XS%=0 :YS%=-1
      Case 1:XS%=1 :YS%=0
      Case 2:XS%=0 :YS%=1
      Case 3:XS%=-1:YS%=0
    End Select
    redraw% = 0
    Page Copy 1 To 0, B
  EndIf

  ' Wait for user to press key
  Key% = 0
  Do While Key% = 0
    Key% = get_input%()
    If Key% = OldKey% Then Key% = 0 Else OldKey% = Key%  ' Don't auto-repeat.

    If show_map% Then
      ' Show player location on map with pulsing red dot.
      Page Write 0
      tmp_int% = Choice((Timer Mod 1000) < 500, Rgb(Red), Rgb(White))
      Box 243 + PlrX% * 3, MAP_Y + PlrY% * 3, 3, 3, , tmp_int%, tmp_int%
      Page Write 1
    EndIf
  Loop

  Select Case Key%
    Case ctrl.LEFT
      pd% = Choice(pd% = 0, 3, pd% - 1)
      redraw% = 1
    Case ctrl.RIGHT
      pd% = Choice(pd% = 3, 0, pd% + 1)
      redraw% = 1
    Case ctrl.UP, ctrl.DOWN
      OX%=PlrX%:OY%=PlrY%
      Inc PlrX%, Choice(Key% = ctrl.UP, 1, -1) * XS%
      Inc PlrY%, Choice(Key% = ctrl.UP, 1, -1) * YS%
      If Maze$(PlrX%,PlrY%)="#" Then
        PlrX%=OX%:PlrY%=OY%
        msgbox.beep(0)
      EndIf
      If show_map% Then Box 243 + ox% * 3, MAP_Y + oy% * 3, 3, 3, ,Rgb(White), Rgb(White)
      redraw% = 1
    Case ctrl.B
      show_map% = Not show_map%
      If show_map% Then
        Box 243, MAP_Y, 76, 76, , Rgb(White), Rgb(White)
        Show_Maze
        Box 243+Ex_X%*3,MAP_Y+Ex_Y%*3,3,3,,255.255
        Text 280, 175, Choice(ctrl$ = "keys_cursor_ext", "M", "B") + " MAP OFF", CT
      Else
        Box 243, MAP_Y, 76, 76, , Rgb(White), Rgb(White)
        Text 280, 175, Choice(ctrl$ = "keys_cursor_ext", "M", "B") + " MAP ON ", CT
      EndIf
      redraw% = 1
    Case ctrl.SELECT, ctrl.START, ctrl.HOME
      on_quit()
      redraw% = 1
    Case ctrl.A Or ctrl.B
      ' Jump to the exit
      PlrX% = Ex_X% : PlrY% = Ex_Y% + 1 : pd% = 0
      redraw% = 1
  End Select

  If PlrX%=Ex_X% And PlrY%=Ex_Y% Then Exit Do
Loop

win_game()
GoTo restart_game

Function get_input%()
  Call ctrl$, get_input%
  If Not get_input% Then keys_cursor_ext(get_input%)
  If Not get_input% Then If ctrl.keydown%(109) Then get_input% = ctrl.B ' m = Map
End Function

Function blocked%(direction%)
  Local x% = PlrX%, y% = PlrY%
  Select Case direction%
    Case 0 : Inc y%, -1
    Case 1 : Inc x%, 1
    Case 2 : Inc y%, 1
    Case 3 : Inc x%, -1
  End Select
  blocked% = Maze$(x%, y%) = "#"
End Function

Sub on_quit()
  msgbox.beep(1)
  Local buttons$(1) Length 3 = ("Yes", "No")
  Const msg$ = "Quit game?"
  Select Case state%
    Case STATE_SHOW_TITLE
      Const x% = 9, y% = 5, fg% = Rgb(White), bg% = Rgb(Black), frame% = Rgb(Green)
    Case Else
      Const x% = 4, y% = 5, fg% = Rgb(Black), bg% = Rgb(White), frame% = -1
  End Select
  Const answer% = msgbox.show%(x%, y%, 22, 9, msg$, buttons$(), 1, ctrl$, fg%, bg%, frame%)
  If buttons$(answer%) = "Yes" Then game.end()
End Sub

'----------------------------
Sub draw_3d
 Box 0, 0, 241, 240, , Rgb(White), Rgb(White)
 Select Case PD%
  Case 0
    For f%=0 To 5
      If PlrY%-f%<0 Then Exit For
      If Maze$(PlrX%-1,PlrY%-f%)="#" Then Draw_Element f%,0,0 Else Draw_Element f%,0,1
      If Maze$(PlrX%+1,PlrY%-f%)="#" Then Draw_Element f%,1,0 Else Draw_Element f%,1,1
      If Maze$(PlrX%,PlrY%-f%)="#" Then Draw_Element f%,1,2:Exit For
    Next f%
   Case 1
     For f%=0 To 5
       If PlrX%+f%>MazeW% Then Exit For
       If Maze$(PlrX%+f%,PlrY%-1)="#" Then Draw_Element f%,0,0 Else Draw_Element f%,0,1
       If Maze$(PlrX%+f%,PlrY%+1)="#" Then Draw_Element f%,1,0 Else Draw_Element f%,1,1
       If Maze$(PlrX%+f%,PlrY%)="#" Then Draw_Element f%,1,2:Exit For
       Next f%
   Case 2
     For f%=0 To 5
      If PlrY%+f%>MazeH% Then Exit For
      If Maze$(PlrX%+1,PlrY%+f%)="#" Then Draw_Element f%,0,0 Else Draw_Element f%,0,1
      If Maze$(PlrX%-1,PlrY%+f%)="#" Then Draw_Element f%,1,0 Else Draw_Element f%,1,1
      If Maze$(PlrX%,PlrY%+f%)="#" Then Draw_Element f%,1,2:Exit For
    Next f%
  Case 3
    For f%=0 To 5
      If PlrX%-f%<0 Then Exit For
      If Maze$(PlrX%-f%,PlrY%+1)="#" Then Draw_Element f%,0,0 Else Draw_Element f%,0,1
      If Maze$(PlrX%-f%,PlrY%-1)="#" Then Draw_Element f%,1,0 Else Draw_Element f%,1,1
      If Maze$(PlrX%-f%,PlrY%)="#" Then Draw_Element f%,1,2:Exit For
    Next f%
 End Select
End Sub

'draw the elements
Sub Draw_Element nr%,mir%,Gap%
Local x1%,y1%,x2%,y2%,x3%,y3%,x4%,y4%
x1%=Wall%(nr%,0,0):y1%=Wall%(nr%,0,1)
x2%=Wall%(nr%,1,0):y2%=Wall%(nr%,1,1)
x3%=Wall%(nr%,2,0):y3%=Wall%(nr%,2,1)
x4%=Wall%(nr%,3,0):y4%=Wall%(nr%,3,1)
If mir% Then x1%=240-x1%:x2%=240-x2%:x3%=240-x3%:x4%=240-x4%
WallC1%=RGB(0,64,0):WallC2%=RGB(0,128,0)
 If Not Gap% Then
   'Wall
   Triangle x1%,y1%,x2%,y2%,x4%,y4%,WallC1%,WallC1%
   Triangle x1%,y1%,x3%,y3%,x4%,y4%,WallC1%,WallC1%
 ElseIf Gap%=1 Then
   'Gap
   Triangle x1%,y3%,x3%,y3%,x4%,y4%,WallC2%,WallC2%
   Triangle x1%,y4%,x1%,y3%,x4%,y4%,WallC2%,WallC2%
 Else
   'Blocker
   Triangle x1%,y1%,240-x1%,y1%,240-x1%,y2%,WallC2%,WallC2%
   Triangle x1%,y1%,240-x1%,y2%,x1%,y2%,WallC2%,WallC2%
 EndIf
End Sub

Sub show_maze()
  For y% = 0 To MazeH%
    For x% = 0 To MazeW%
      If Maze$(x%,y%)="#" Then Box 243+x%*3,MAP_Y+y%*3,3,3,,0,0
    Next
  Next
End Sub

' --- 2D Maze generator ---
' algorithm based on
' https://rosettacode.org/wiki/Maze_generation#BASIC256
' --------------------------
Sub generator
 Local done%,i%,CurX%,CurY%,OldX%,OldY%,x%,y%
 ' initial start location
 CurX%=Int(Rnd * (MazeW% - 1))
 CurY%=Int(Rnd * (MazeH% - 1))
 ' value must be odd
 If CurX% Mod 2=0 Then Inc CurX%
 If CurY% Mod 2=0 Then Inc CurY%
 Maze$(CurX%, CurY%) = " "
 ' generate maze
 done%=0
 Do While done%=0
     For i% = 0 To 99
       OldX%=CurX%
       OldY%=CurY%
       ' move in random direction
       Select Case Int(Rnd*4)
           Case 0
               If CurX%+2<MazeW% Then Inc CurX%,2
           Case 1
               If CurY%+2<MazeH% Then Inc CurY%,2
           Case 2
               If CurX%-2>0 Then Inc CurX%,-2
           Case 3
               If CurY%-2>0 Then Inc CurY%,-2
       End Select
       ' if cell is unvisited then connect it
       If Maze$(CurX%,CurY%)="#" Then
           Maze$(CurX%,CurY%)=" "
           Maze$(Int((CurX%+OldX%)/2),((CurY%+OldY%)/2))=" "
       EndIf
   Next i%
   ' check if all cells are visited
   done%=1
   For x%=1 To MazeW%-1 Step 2
       For y%=1 To MazeH%-1 Step 2
           If Maze$(x%,y%)="#" Then done%=0
       Next y%
   Next x%
Loop
End Sub

Function show_title$()
'!if defined(GAMEMITE)
  '!uncomment_if true
  ' Const txt$ = "Press START to play"
  '!endif
'!else
  If sys.is_platform%("gamemite") Then
    Const txt$ = "Press START to play"
  ElseIf sys.is_platform%("pm") Then
    Const txt$ = "Press SPACE to play"
  ElseIf sys.is_platform%("pmvga") Then
    Const txt$ = "Press START, FIRE or SPACE to play"
  Else
    Const txt$ = "Press START, FIRE or SPACE to play"
  EndIf
'!endif

  Const X_OFFSET% = MM.HRes \ 2
  Const Y_OFFSET% = MM.VRes \ 2

  Cls
  Text X_OFFSET%, Y_OFFSET% - 27, "3D MAZE", "CM", 1, 2, RGB(White)
  Text X_OFFSET%, Y_OFFSET% - 2, VERSION_STRING$, "CM", 7, 1, Rgb(Green)
  Text X_OFFSET%, Y_OFFSET% + 10, "(c) 2022-2024 Martin Herhaus", "CM", 7, 1, RGB(Green)
  Text X_OFFSET%, Y_OFFSET% + 30, txt$, "CM", 1, 1, RGB(White)
  Page Copy 1 To 0, B

  ' Prompt user to select controller.
  Const mask% = ctrl.A Or ctrl.B Or ctrl.START Or ctrl.SELECT Or ctrl.HOME
  Local key%
  Do
    ctrl$ = ctrl.poll_multiple$(CONTROLLERS$(), mask%, 100, key%)
    If key% And (ctrl.SELECT Or ctrl.HOME) Then
      Call ctrl$, ctrl.OPEN
      on_quit()
      Call ctrl$, ctrl.CLOSE
      key% = 0
    EndIf
  Loop While Not key%
  Call ctrl$, ctrl.OPEN

  msgbox.beep(1)
  show_title$ = ctrl$
End Function

Sub win_game()
  Text 64, 70, Chr$(&h97) + " WELL DONE! " + Chr$(&h97)
  Text 120, 105, "Press " + Choice(ctrl$ = "keys_cursor_ext", "ESCAPE", "SELECT") , CM
  Text 120, 120, "to quit,", CM
  Text 120, 135, "or " + Choice(ctrl$ = "keys_cursor_ext", "SPACE", "START") + " to try", CM
  Text 120, 150, "another maze", CM
  Page Copy 1 To 0, B

  Do While get_input%() : Pause 5 : Loop
  Local key% = 1
  Do
    Select Case get_input%()
      Case ctrl.SELECT, ctrl.HOME: game.end()
      Case ctrl.A, ctrl.START: Exit Do
    End Select
  Loop
End Sub

' --- WallData ---
wall_data:
Data 0,0,0,239,10,10,10,229
Data 11,11,11,228,50,50,50,189
Data 51,51,51,188,80,80,80,159
Data 81,81,81,158,100,100,100,139
Data 101,101,101,138,110,110,110,129
Data 111,111,111,128,120,120,120,120
