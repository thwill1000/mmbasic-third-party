'Maze Game by Mart!n Herhaus 2022
'Init, switch to 16 Col 320x240 Screen

#Include "ctrl.ipp"

Option Break 4
On Key 3, on_exit

Select Case Mm.Device$
  Case "PicoMiteVGA"
    Mode 2
    Font 1
    FrameBuffer Create
    FrameBuffer Write F
  Case "Colour Maximite 2", "Colour Maximite 2 G2", "MMBasic for Windows"
    Mode 7
    Font 1
    Page Write 1
  Case Else
    Error "Unsupported device: " + Mm.Device$
End Select

ctrl.init_keys()
Dim ctrl$ = show_title$()

cw%=RGB(white):WallC1%=0:WallC2%=RGB(RED):cP%=RGB(green)
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
show_map%=0:CLS cw%
'place Player
PlrX%=MazeW%-1:PlrY%=MazeH%-1:PD%=3
'place Exit
Ex_X%=2:Ex_Y%=0:If Maze$(Ex_X%,1)="#" Then Inc Ex_X%
Maze$(Ex_X%,Ex_Y%)="E"
If show_map% Then Box 243+Ex_X%*3,3+Ex_Y%*3,3,3,,WallC2%,WallC2%

Colour 0, cw%
Text 244,98,"D  X  Y"
Select Case ctrl$
  Case "nes_a"
    Text 244,130,"NES PAD"
  Case "atari_a"
    Text 244,130,"ATARI JOY"
  Case "keys_wasd"
    Text 244,130,"KEYS:"
    Text 244,140,"W:FORWARD"
    Text 244,150,"S:BACKWRD"
    Text 244,160,"A:TURN L."
    Text 244,170,"D:TURN R."
End Select
Text 244,190,"M:SHOW/"
Text 244,204," HIDE MAP"

' --- Game Loop ---
redraw% = 1
Do
  If redraw% Then
    Text 244,110,Mid$(MovDir$,PD%+1,1)+" "+Str$(PlrX%,2)+" "+Str$(PlrY%,2)+" "
    Draw_3D
    Select Case PD%
      Case 0:XS%=0 :YS%=-1
      Case 1:XS%=1 :YS%=0
      Case 2:XS%=0 :YS%=1
      Case 3:XS%=-1:YS%=0
    End Select
    If show_map% Then Box 243+PlrX%*3,3+PlrY%*3,3,3,,cp%,cp%
    redraw% = 0
    If Mm.Device$ = "PicoMiteVGA" Then
      FrameBuffer Copy F, N, B
    Else
      Page Copy 1 To 0, B
    EndIf
  EndIf

  ' Wait for user to press key
  Key% = 0
  Do While Key% = 0
    Call ctrl$, Key%
    If Key% = 0 Then
      If ctrl.keydown%(Asc("m")) Then Key% = ctrl.SELECT
    EndIf
    If Key% = OldKey% Then Key% = 0 Else OldKey% = Key%  ' Don't auto-repeat.
  Loop

  Select Case Key%
    Case ctrl.LEFT
      Inc PD%,-1:Inc PD%,4*(PD%=-1)
      redraw% = 1
    Case ctrl.RIGHT
      Inc PD%:PD%=PD% And 3
      redraw% = 1
    Case ctrl.UP, ctrl.DOWN
      OX%=PlrX%:OY%=PlrY%
      Inc PlrX%, Choice(Key% = ctrl.UP, 1, -1) * XS%
      Inc PlrY%, Choice(Key% = ctrl.UP, 1, -1) * YS%
      If Maze$(PlrX%,PlrY%)="#" Then PlrX%=OX%:PlrY%=OY%
      If show_map% Then Box 243+ox%*3,3+oy%*3,3,3,,cw%,cw%
      redraw% = 1
    Case ctrl.SELECT
      show_map% = Not show_map%
      If show_map% Then
        Box 243,3,76,76,,cw%,cw%
        Show_Maze
        Box 243+Ex_X%*3,3+Ex_Y%*3,3,3,,255.255
      Else
        Box 243,3,76,76,,cw%,cw%
      EndIf
      redraw% = 1
  End Select

  If PlrX%=Ex_X% And PlrY%=Ex_Y% Then Exit
Loop

Text 64,50,Chr$(151)+" WELL DONE! "+Chr$(151)
Text 60,85,"PRESS Q TO QUIT"
Text 60,105,"OR ANY OTHER KEY"
Text 60,115," TO TRY ANOTHER"
Text 64,125,"     MAZE"
If Mm.Device$ = "PicoMiteVGA" Then
  FrameBuffer Copy F, N, B
Else
  Page Copy 1 To 0, B
EndIf
ctrl.term_keys()
ikey$ = ""
Do While ikey$ = "" : ikey$ = LCase$(Inkey$) : Loop
If ikey$ = "q" Then CLS 0 : on_exit()
ctrl.init_keys()
GoTo restart_game
'----------------------------
Sub draw_3d
 Box 0,0,241,240,,cw%,cw%
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
Sub show_maze
For y% = 0 To MazeH%
   For x% = 0 To MazeW%
       If Maze$(x%,y%)="#" Then Box 243+x%*3,3+y%*3,3,3,,0,0
   Next x%
Next y%
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

Sub on_exit()
  On Key 3, 0
  Option Break 3
  If Mm.Device$ = "PicoMiteVGA" Then
    FrameBuffer Close F
  Else
    Page Write 0
  EndIf
  ctrl.term_keys()
  On Error Ignore
  Call ctrl$, ctrl.CLOSE
  On Error Abort
  End
End Sub

Function show_title$()
  Const X_OFFSET% = MM.HRes \ 2
  Const Y_OFFSET% = MM.VRes \ 2

  Cls

  Text X_OFFSET%, Y_OFFSET% - 15, "3D MAZE", "CM", 1, 2, RGB(Green)
  Text X_OFFSET%, Y_OFFSET% + 8, "(c) 2022 Martin Herhaus", "CM", 7, 1, RGB(Green)
  Local text$ = Choice(Mm.Device$ <> "MMBasic for Windows", "Press START, FIRE or SPACE", "Press SPACE")
  Text X_OFFSET%, Y_OFFSET% + 40, text$, "CM", 1, 1, RGB(White)
  If Mm.Device$ = "PicoMiteVGA" Then
    FrameBuffer Copy F, N, B
  Else
    Page Copy 1 To 0, B
  EndIf

  ' Prompt user to select controller.
  Select Case Mm.Device$
    Case "PicoMiteVGA"
      Static POLL_THESE$(2) = ("keys_wasd", "nes_a", "atari_a")
    Case "Colour Maximite 2", "Colour Maximite 2 G2"
      Static POLL_THESE$(2) = ("keys_wasd", "wii_classic_3", "atari_dx")
    Case "MMBasic for Windows"
      Static POLL_THESE$(1) = ("keys_wasd", "keys_wasd")
  End Select
  Local ctrl$ = ctrl.poll$(0, POLL_THESE$())
  Call ctrl$, ctrl.OPEN

  Cls
  If Mm.Device$ = "PicoMiteVGA" Then
    Framebuffer Copy F, N, B
  Else
    Page Copy 1 To 0, B
  EndIf

  show_title$ = ctrl$
End Function

Sub keys_wasd(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32) * ctrl.A
  Inc x%, (ctrl.keydown%(Asc("w")) Or ctrl.keydown%(128)) * ctrl.UP
  Inc x%, (ctrl.keydown%(Asc("s")) Or ctrl.keydown%(129)) * ctrl.DOWN
  Inc x%, (ctrl.keydown%(Asc("a")) Or ctrl.keydown%(130)) * ctrl.LEFT
  Inc x%, (ctrl.keydown%(Asc("d")) Or ctrl.keydown%(131)) * ctrl.RIGHT
  Inc x%, ctrl.keydown%(Asc("m")) * ctrl.SELECT
End Sub

  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT

' --- WallData ---
Data 0,0,0,239,10,10,10,229
Data 11,11,11,228,50,50,50,189
Data 51,51,51,188,80,80,80,159
Data 81,81,81,158,100,100,100,139
Data 101,101,101,138,110,110,110,129
Data 111,111,111,128,120,120,120,120
