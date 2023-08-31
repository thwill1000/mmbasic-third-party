' 3D Maze Game
' Copyright (c) 2022-2023 Martin Herhaus
' GameMite port by Thomas H. Williams

Const VERSION = 911 ' 0.9.11

#Include "../splib/system.inc"

'!if defined PICOMITEVGA
  '!replace { Page Copy 1 To 0 , B } { FrameBuffer Copy F , N , B }
  '!replace { Page Write 1 } { FrameBuffer Write F }
  '!replace { Page Write 0 } { FrameBuffer Write N }
  '!replace { Mode 7 } { Mode 2 : FrameBuffer Create }
'!elif defined PICOMITE
  '!replace { Page Copy 1 To 0 , B } { FrameBuffer Copy F , N }
  '!replace { Page Write 1 } { FrameBuffer Write F }
  '!replace { Page Write 0 } { FrameBuffer Write N }
  '!replace { Mode 7 } { FrameBuffer Create }
'!endif

#Include "../splib/ctrl.inc"
#Include "../splib/string.inc"
#Include "../splib/msgbox.inc"
'!if defined(PGLCD) || defined(PGLCD2)
#Include "../splib/gamemite.inc"
'!endif

sys.override_break("on_break")

If sys.is_device%("pmvga") Then
  Dim CONTROLLERS$(2) = ("keys_wasd", "nes_a", "atari_a")
ElseIf sys.is_device%("pglcd") Then
  Dim CONTROLLERS$(1) = ("keys_wasd", "ctrl.gamemite")
ElseIf sys.is_device%("pm*", "mmb4w") Then
  Dim CONTROLLERS$(1) = ("keys_wasd", "keys_wasd")
ElseIf sys.is_device%("cmm2*") Then
  Dim CONTROLLERS$(2) = ("keys_wasd", "wii_classic_3", "atari_dx")
Else
  Error "Unsupported device: " + Mm.Device$
EndIf

Const STATE_SHOW_TITLE% = 0
Const STATE_PLAY_GAME% = 1
Const STATE_WIN_GAME% = 2

Const MAP_Y% = 37 ' Vertical offset for mini-map.

Dim pd% ' Player direction (N=0,E=1,S=2,W=3)
Dim redraw%
Dim tmp_int%
Dim state%

If sys.is_device%("mmb4w", "cmm2*") Then Option Console Serial
Mode 7
Font 1
Page Write 1

ctrl.init_keys()
Dim ctrl$ = show_title$()

WallC1%=0:WallC2%=RGB(RED)
'Read the XY-Coordinates of the Wall Elements Corners
Dim Wall%(6,4,2)
For N%=0 To 5 'Walls
 For C%=0 To 3 '4 Corners
   For F%=0 To 1 '2 Coordinates (X,Y)
     Read Wall%(N%,C%,F%)
   Next
 Next
Next
MazeW%=24:MazeH%=24
' create array and fill
Dim Maze$(MazeW%,MazeH%) length 1
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
If show_map% Then Box 243+Ex_X%*3,MAP_Y%+Ex_Y%*3,3,3,,WallC2%,WallC2%

Colour 0, Rgb(White)
Text 280, 10, "D  X  Y", CT
Select Case ctrl$
  Case "ctrl.gamemite"
    Text 280, 164, "GAMEPAD", CT
  Case "nes_a"
    Text 280, 164, "NES PAD", CT
  Case "atari_a"
    Text 280, 164, "ATARI JOY", CT
  Case "keys_wasd"
    Text 280, 120, "KEYS:    ", CT
    Text 280, 131, "W:FORWARD", CT
    Text 280, 142, "S:BACKWRD", CT
    Text 280, 153, "A:TURN L.", CT
    Text 280, 164, "D:TURN R.", CT
End Select
Text 280, 175, Choice(ctrl$ = "keys_wasd", "M", "B") + ":MAP ON ", CT

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
    Call ctrl$, Key%
    If Key% = OldKey% Then Key% = 0 Else OldKey% = Key%  ' Don't auto-repeat.

    If show_map% Then
      ' Show player location on map with pulsing red dot.
      Page Write 0
      tmp_int% = Choice((Timer Mod 1000) < 500, Rgb(Red), Rgb(White))
      Box 243 + PlrX% * 3, MAP_Y% + PlrY% * 3, 3, 3, , tmp_int%, tmp_int%
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
      If show_map% Then Box 243 + ox% * 3, MAP_Y% + oy% * 3, 3, 3, ,Rgb(White), Rgb(White)
      redraw% = 1
    Case ctrl.B
      show_map% = Not show_map%
      If show_map% Then
        Box 243, MAP_Y%, 76, 76, , Rgb(White), Rgb(White)
        Show_Maze
        Box 243+Ex_X%*3,MAP_Y%+Ex_Y%*3,3,3,,255.255
        Text 280, 175, Choice(ctrl$ = "keys_wasd", "M", "B") + ":MAP OFF", CT
      Else
        Box 243, MAP_Y%, 76, 76, , Rgb(White), Rgb(White)
        Text 280, 175, Choice(ctrl$ = "keys_wasd", "M", "B") + ":MAP ON ", CT
      EndIf
      redraw% = 1
    Case ctrl.SELECT
      on_select()
      redraw% = 1
    case ctrl.SELECT Or ctrl.START
      ' Jump to the exit
      PlrX% = Ex_X% : PlrY% = Ex_Y% + 1 : pd% = 0
      redraw% = 1
  End Select

  If PlrX%=Ex_X% And PlrY%=Ex_Y% Then Exit Do
Loop

win_game()
GoTo restart_game

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

Sub on_select()
  msgbox.beep(1)
  Local buttons$(1) Length 3 = ("Yes", "No")
  Const msg$ = "Quit game?"
  Select Case state%
    Case STATE_SHOW_TITLE
      Const x% = 9, y% = 5, fg% = Rgb(White), bg% = Rgb(Black), frame% = Rgb(Green)
    Case Else
      Const x% = 4, y% = 5, fg% = Rgb(Black), bg% = Rgb(White), frame% = -1
  End Select

  ' Store the current screen (rather than redraw it).
  Local buf%(4799), pbuf% = Peek(VarAddr buf%()) ' 38400 bytes
  Memory Copy Mm.Info(WriteBuff), pbuf%, 38400

  Const answer% = msgbox.show%(x%, y%, 22, 9, msg$, buttons$(), 1, ctrl$, fg%, bg%, frame%)
  If buttons$(answer%) = "Yes" Then end_program()

  ' Restore the current screen.
  Memory Copy pbuf%, Mm.Info(WriteBuff), 38400
  Page Copy 1 To 0, B
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
      If Maze$(x%,y%)="#" Then Box 243+x%*3,MAP_Y%+y%*3,3,3,,0,0
    Next
  Next
End Sub

' --- 2D Maze generator ---
' algorithmen based on
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

Sub on_break()
  end_program(1)
End Sub

Sub end_program(break%)
  If sys.is_device%("pglcd") Then
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

Function show_title$()
  If sys.is_device%("pglcd") Then
    Const platform$ = "GameMite"
    Const txt$ = "Press START"
  ElseIf sys.is_device%("pm") Then
    Const platform$ = "PicoMite"
    Const txt$ = "Press SPACE"
  ElseIf sys.is_device%("pmvga") Then
    Const platform$ = "PicoGAME VGA"
    Const txt$ = "Press START, FIRE or SPACE"
  Else
    Const platform$ = "Colour Maximite 2"
    Const txt$ = "Press START, FIRE or SPACE"
  EndIf

  Const X_OFFSET% = MM.HRes \ 2
  Const Y_OFFSET% = MM.VRes \ 2

  Cls
  Text X_OFFSET%, Y_OFFSET% - 27, "3D MAZE", "CM", 1, 2, RGB(White)
  Text X_OFFSET%, Y_OFFSET% - 2, platform$ + " Version", "CM", 7, 1, Rgb(Green)
  Text X_OFFSET%, Y_OFFSET% + 10, "(c) 2022-2023 Martin Herhaus", "CM", 7, 1, RGB(Green)
  Text X_OFFSET%, Y_OFFSET% + 30, txt$, "CM", 1, 1, RGB(White)
  Page Copy 1 To 0, B

  ' Prompt user to select controller.
  Local key%
  Do
    ctrl$ = ctrl.poll_multiple$(CONTROLLERS$(), ctrl.A Or ctrl.START Or ctrl.SELECT, 100, key%)
    If key% And ctrl.SELECT Then
      Call ctrl$, ctrl.OPEN
      on_select()
      Call ctrl$, ctrl.CLOSE
      key% = 0
    EndIf
  Loop While Not key%
  Call ctrl$, ctrl.OPEN

  msgbox.beep(1)
  show_title$ = ctrl$
End Function

Sub keys_wasd(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32) * ctrl.A
  Inc x%, (ctrl.keydown%(Asc("w")) Or ctrl.keydown%(128)) * ctrl.UP
  Inc x%, (ctrl.keydown%(Asc("s")) Or ctrl.keydown%(129)) * ctrl.DOWN
  Inc x%, (ctrl.keydown%(Asc("a")) Or ctrl.keydown%(130)) * ctrl.LEFT
  Inc x%, (ctrl.keydown%(Asc("d")) Or ctrl.keydown%(131)) * ctrl.RIGHT
  Inc x%, ctrl.keydown%(Asc("m")) * ctrl.B
  Inc x%, ctrl.keydown%(Asc("q")) * ctrl.SELECT
End Sub

Sub win_game()
  Text 64, 70, Chr$(&h97) + " WELL DONE! " + Chr$(&h97)
  Text 120, 105, "Press " + Choice(ctrl$ = "keys_wasd", "'Q'", "SELECT") , CM
  Text 120, 120, "to quit,", CM
  Text 120, 135, "or " + Choice(ctrl$ = "keys_wasd", "SPACE", "START") + " to try", CM
  Text 120, 150, "another maze", CM
  Page Copy 1 To 0, B

  Local key% = 1
  Do While key% : Pause 5 : Call ctrl$, key% : Loop
  Do
    Call ctrl$, key%
    If key% = ctrl.SELECT Then end_program()
    If key% = ctrl.A Then key% = ctrl.START
  Loop Until key% = ctrl.START
End Sub

' --- WallData ---
Data 0,0,0,239,10,10,10,229
Data 11,11,11,228,50,50,50,189
Data 51,51,51,188,80,80,80,159
Data 81,81,81,158,100,100,100,139
Data 101,101,101,138,110,110,110,129
Data 111,111,111,128,120,120,120,120
