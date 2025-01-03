' PicoVaders
'
' Copyright (c) 2022-2024 Martin Herhaus
'
' Concept based on the Game Space Invaders
' (c) 1978 by Tomohiro Nishikado of Taito
'
' GameMite port by Thomas H. Williams

Option Base 0
Option Default None
Option Explicit On

Const VERSION = 101304 ' 1.1.4

'!if defined(PICOMITEVGA)
  '!replace { Option Simulate "Colour Maximite 2" } { Option Simulate "PicoMiteVGA" }
  '!dynamic_call atari_a
  '!dynamic_call nes_a
  '!replace { Page Copy 1 To 0 , B } { FrameBuffer Copy F , N , B }
  '!replace { Page Copy 0 To 1 , B } { FrameBuffer Copy N , F , B }
  '!replace { Page Write 1 } { FrameBuffer Write F }
  '!replace { Page Write 0 } { FrameBuffer Write N }
  '!replace { Mode 7 } { Mode 2 : FrameBuffer Create }
'!elif defined(GAMEMITE)
  '!replace { Option Simulate "Colour Maximite 2" } { Option Simulate "Game*Mite" }
  '!dynamic_call ctrl.gamemite
  '!replace { Page Copy 1 To 0 , B } { FrameBuffer Copy F , N }
  '!replace { Page Copy 0 To 1 , B } { FrameBuffer Copy N , F }
  '!replace { Page Write 1 } { FrameBuffer Write F }
  '!replace { Page Write 0 } { FrameBuffer Write N }
  '!replace { Mode 7 } { FrameBuffer Create }
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
#Include "../splib/file.inc"
#Include "../splib/string.inc"
#Include "../splib/msgbox.inc"
#Include "../splib/game.inc"

'!dynamic_call game.on_break
sys.override_break("game.on_break")

'!if defined(GAMEMITE)
  '!uncomment_if true
  ' Dim CONTROLLERS$(1) = ("keys_cursor_ext", "ctrl.gamemite")
  ' Const VERSION_STRING$ = "Game*Mite Version " + sys.format_version$(VERSION)
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

ctrl.init_keys()

Mode 7
Font 1
game.init_window("PicoVaders", VERSION)

Const X_MAX% = 204

Dim ctrl$          ' Current controller driver.
Dim alien$(3, 2)   ' Alien sprites with 2 animation states.
Dim aliens%(55, 4) ' Aliens: x, y, type, alive.
Dim ply$(3)        ' Player sprites.
Dim bnk%(4, 2, 8)  ' 4 bunkers, 2 rows of 8 blocks.
Dim a_bomb%(10, 4) ' Alien bombs: x, y, alive, owned_by
Dim noise%(200)
Dim uxpl%(3)
Dim snd%(4) = (100, 90, 85, 80, 70)
Dim udir%         ' UFO direction.
Dim ux%           ' UFO x-coordinate.
Dim ua%           ' Is UFO active?
Dim uscr%         ' Score for destroying UFO.
Dim UfoSndMin% = 800, UfoSndMax% = 1100, UfoSnd% = 800, Ustp% = 100
Dim anr% = 55     ' Next alien to move.
Dim myst% = 0     ' When this is > 20 a UFO is spawned.
Dim score%
Dim high_score%
Dim mvsnd% = 0
Dim adir% = 1     ' Direction that the aliens are moving in.
Dim ba%, bx%, by% ' Bullet active, x-coordinate, y-coordinate.
Dim plx%          ' Player x-coordinate.
Dim a_ground%     ' Flag set when an alien has reached ground level.
Dim num_aliens%   ' Number of aliens remaining.
Dim trn%          ' Flag set when an alien has reached the edge of the screen.
Dim plhit%        ' Flag set when the player has been hit.
Dim bombs_out%    ' Number of active alien bombs.
Dim next_frame%
Dim game_over%    ' Is the GAME OVER?
Dim y_pos%        ' Initial y-coordinate for top row of aliens.
Dim xpl$          ' Explosion sprite.
Dim uf1$, uf2$    ' UFO sprites.
Dim level%        ' Current level.
Dim lives%        ' Number of player lives remaining.
Dim anim%         ' Alien animation frame: 1 or 2
Dim tick%
Dim bn%
Dim bmax%         ' Maximum number of alien bombs.
Dim dummy%

init_gfx()
init_sound()
read_high_score()
Cls

new_game_label:

intro()
Call ctrl$, ctrl.OPEN
ctrl.wait_until_idle(ctrl$)

plx% = 103 : y_pos% = 48
anim% = 1 : tick% = 1
level% = 1 : lives% = 3 : score% = 0 : game_over% = 0

next_level_label:

num_aliens% = 55
bn% = 1
bmax% = Min(2 + Int(level% / 2), 10)
ua% = 0

setup_aliens()
draw_screen()

next_life_label:

plhit% = 0
clear_bombs()
Box 72, 232, 40, 8, 1, 0, 0
If lives% > 1 Then Gui Bitmap 72, 232, ply$(1), 16, 8, 1, Rgb(Green), 0
If lives% > 2 Then Gui Bitmap 88, 232, ply$(1), 16, 8, 1, Rgb(Green), 0

' Game Loop
Do
  next_frame% = Timer + 15
  move_single()
  draw_player()
  move_player()
  draw_bullet()
  draw_bomb()
  If Not(tick% Mod 16) Then drop_bomb()
  If Not(tick% Mod 4) Then draw_ufo()
  Inc tick%
  start_ufo()
  If num_aliens% = 0 Then Exit Do
  If plhit% Then Exit Do
  If a_ground% Then expl_player() : game_over% = 1 : Exit Do
  Inc bn% : If bn% > bmax% Then bn% = 1
  Do While Timer < next_frame% : Loop
Loop

If plhit% Then
  explode_player()
  Inc lives%, -1
  If lives% = 0 Then game_over% = 1 : Goto game_over_label
  dummy% = wait%(2000)
  Goto next_life_label
EndIf

If num_aliens% = 0 Then
  Inc level%
  If level% < 6 Then Inc y_pos%, 8
  dummy% = wait%(2000)
  Goto next_level_label
EndIf

game_over_label:

If game_over% Then
  write_high_score()
  show_game_over()
  Goto new_game_label
EndIf

' Initialises "sprites".
Sub init_gfx()
  Local a%, al%, i%, n%

  Restore sr1
  For al% = 1 To 3
    For i% = 1 To 2
      alien$(al%, i%) = ""
      For n% = 1 To 16
        Read a%
        Cat alien$(al%, i%), Chr$(a%)
      Next
    Next
  Next

  For i% = 1 To 3
    ply$(i%) = ""
    For n% = 1 To 16 : Read a% : Cat ply$(i%), Chr$(a%) : Next
  Next

  Restore xpld
  xpl$ = ""
  For n% = 1 To 16 : Read a% : Cat xpl$, Chr$(a%) : Next

  Restore ufo
  uf1$ = ""
  For n% = 1 To 16 : Read a% : Cat uf1$, Chr$(a%) : Next

  uf2$ = ""
  For n% = 1 To 16 : Read a% : Cat uf2$, Chr$(a%) : Next
End Sub

' Initialises sound.
Sub init_sound()
  Local i%
  For i% = 1 To 200 : noise%(i%) = Int(Rnd * 1000) : Next
End Sub

Sub read_high_score()
  Local s$(1) Length 32
  game.highscore_read(s$())
  high_score% = Val(Field$(s$(0), 2, ","))
End Sub

Sub write_high_score()
  Local s$(1) Length 32
  s$(0) = "PLAYER 1, " + Str$(high_score%)
  game.highscore_write(s$())
End Sub

' Removes any alien and player bombs/bullets.
Sub clear_bombs()
  Local i%
  For i% = 1 To 10
    If a_bomb%(i%, 3) Then
      Line 50 + a_bomb%(i%, 1), a_bomb%(i%, 2), 50 + a_bomb%(i%, 1), a_bomb%(i%, 2) + 4, , Rgb(Black)
    EndIf
    a_bomb%(i%, 3) = 0
  Next
  bombs_out% = 0

  If ba% Then Line 50 + bx%, by%, 50 + bx%, by% + 4, , Rgb(Black)
  ba% = 0
End Sub

' Shows intro screen.
Sub intro()
  Local key%, y%
  If ctrl$ <> "" Then ctrl.wait_until_idle(ctrl$)
  Box 0, 30, 320, 210, , 0, 0
  inc_score(0, 1)

'!if defined(GAMEMITE)
  '!uncomment_if true
  ' Const txt$ = "Press START to play"
  '!endif
'!else
  If sys.is_platform%("gamemite") Then
    Const txt$ = "Press START to play"
  ElseIf sys.is_platform%("cmm2*", "pmvga") Then
    Const txt$ = "Press START, FIRE or SPACE to play"
  Else
    Const txt$ = "Press SPACE To play"
  EndIf
'!endif
  Text 160, 216, txt$, CT, , , Rgb(Green)
  Box 50, 229, 220, 1, , , Rgb(Green)

  y% = 30
  Text 144, y%, "PLA"
  Text 176, y% + Mm.Info(FontHeight) - 2 , "Y", I : Inc y%, 18
  If Not key% Then key% = poll_ctrl%(600)
  Text 160, y%, "PICOVADERS", CT : Inc y%, 25
  If Not key% Then key% = poll_ctrl%(600)
  Text 160, y%, "*SCORE ADVANCE TABLE*", CT: Inc y%, 20

  If Not key% Then key% = poll_ctrl%(600)
  Gui Bitmap 104, y%, uf1$, 16, 8, 1, Rgb(Red), 0
  Text 130, y%, "= ? MYSTERY" : Inc y%, 18

  If Not key% Then key% = poll_ctrl%(600)
  Gui Bitmap 104, y%, alien$(1, 1), 16, 8, 1, Rgb(White), 0
  Text 130, y%, "=30 POINTS" : Inc y%, 20

  If Not key% Then key% = poll_ctrl%(600)
  Gui Bitmap 104, y%, alien$(2, 1), 16, 8, 1, Rgb(White), 0
  Text 130, y%, "=20 POINTS" : Inc y%, 20

  If Not key% Then key% = poll_ctrl%(600)
  Gui Bitmap 104, y%, alien$(3, 1), 16, 8, 1, Rgb(White), 0
  Text 130, y%, "=10 POINTS" : Inc y%, 2 * Mm.Info(FontHeight)

  If Not key% Then key% = poll_ctrl%(600)

  If InStr(Mm.Device$, "PicoMite") Then Font 7
  Text 160, y%, "(C) 1978 BY TAITO", CT
  Inc y%, Mm.Info(FontHeight) + 1
  Text 160, y%, UCase$(VERSION_STRING$), CT
  Inc y%, Mm.Info(FontHeight) + 1
  Text 160, y%, "2022-2023 BY MARTIN HERHAUS", CT
  Font 1
  If Not key% Then key% = poll_ctrl%(2000)

  ' Animate alien fixing "Y".
  Local x% = 271
  Do While (x% > 177) And (Not key%)
    key% = intro_alien%(x%, -1)
  Loop
  Do While (x% < 278) And (Not key%)
    Text x% + 1, 40, "Y", I
    key% = intro_alien%(x%, 1)
  Loop
  Do While (x% > 176) And (Not key%)
    Text x% - 7, 30, "Y"
    key% = intro_alien%(x%, -1)
  Loop
  Do While (x% < 270) And (Not key%)
    key% = intro_alien%(x%, 1)
  Loop
  Text 168, 30, "Y"
  Box 174, 30, 320 - 174, 10, , 0, 0

  If key% Then
    msgbox.beep(1)
    Pause 1000
  Else
    key% = poll_ctrl%()
    msgbox.beep(1)
  EndIf
End Sub

Function intro_alien%(x%, dir%)
  Inc x%, dir%
  Select Case x%
    Case < 270
      Gui Bitmap x%, 30, alien$(1, 1 + (x% Mod 2)), 16, 8, 1, Rgb(White), 0
    Case 270
      Gui Bitmap x%, 30, alien$(1, 1 + (x% Mod 2)), 16, 8, 1, Rgb(Black), 0
  End Select
  intro_alien% = poll_ctrl%(30)
End Function

' Waits the given duration% polling for the user to choose a
' controller by pressing A, HOME, SELECT or START.
Function poll_ctrl%(duration%)
  Const mask% = ctrl.A Or ctrl.HOME Or ctrl.SELECT Or ctrl.START
  Local d% = duration%
  Do While d% > 0 Or duration% = 0
    ctrl$ = ctrl.poll_multiple$(CONTROLLERS$(), mask%, d%, poll_ctrl%)
    If poll_ctrl% And (ctrl.SELECT Or ctrl.HOME) Then
      Call ctrl$, ctrl.OPEN
      on_quit()
      Call ctrl$, ctrl.CLOSE
      poll_ctrl% = 0
    Else If poll_ctrl% Then
      Exit Do
    EndIf
  Loop
End Function

' Waits the given duration% or until user presses button(s) given by mask%.
Function wait%(duration%, mask%)
  Local t% = Timer + duration%
  Do While Timer < t%
    Call ctrl$, wait%
    If Not wait% Then keys_cursor_ext(wait%)
    If wait% And (ctrl.SELECT Or ctrl.HOME) Then on_quit()
    If wait% And mask% Then Exit Do
    Pause 5
  Loop
End Function

' Button handler that shows the Quit dialog.
Sub on_quit()
  msgbox.beep(1)
  Local buttons$(1) Length 3 = ("Yes", "No")
  Const msg$ = "Quit game?"
  Const fg% = Rgb(White), bg% = Rgb(Black), frame% = Rgb(Green), flags% = msgbox.NO_PAGES
  Const x% = 9, y% = 5, w% = 22, h% = 9, btn% = 1

'  Page Copy 0 To 1, B ' Store screen
  Const a% = msgbox.show%(x%, y%, w%, h%, msg$, buttons$(), btn%, ctrl$, fg%, bg%, frame%, flags%)
  If buttons$(a%) = "Yes" Then game.end()
'  Page Copy 1 To 0, B ' Restore screen.
End Sub

Sub start_ufo()
  ufo_x()
  If myst% > 20 And ua% = 0 Then
    ua% = 1
    myst% = 0
    udir% = Choice(Int(Rnd * 2) = 1, -2, 2)
    ux% = Choice(udir% = 2, 0, X_MAX%)
    Select Case Int(Rnd * 10)
      Case 7 To 8 : uscr% = 100
      Case 9 :      uscr% = 150
      Case Else:    uscr% = 50
    End Select
  EndIf
End Sub

Sub drop_bomb()
  If bombs_out% >= bmax% Then Exit Sub

  ' Start at the bottom right alien.
  Local aln%
  For aln% = 55 To 1 Step -1
    ' If the alien is dead then test next alien.
    If Not aliens%(aln%, 4) Then Continue For

    ' If the alien already has an active bomb then test next alien.
    For bn% = 1 To 10
      If a_bomb%(bn%, 3) Then
        If a_bomb%(bn%, 4) = aln% Then bn% = 0 : Exit For
      EndIf
    Next
    If Not bn% Then Continue For

    ' Aliens not near the player only drop bomb 1 in 25 times.
    Select Case aliens%(aln%, 1)
      Case < plx% - 8, > plx% + 8
        If Int(Rnd * 25) Then Continue For
    End Select

    ' Alien on the bottom row.
    If aln% > 44 Then Exit For

    ' Alien with no other alien beneath it.
    If Not aliens%(aln% + 11, 4) Then Exit For
  Next

  ' No bomb being dropped.
  If Not aln% Then Exit Sub

  ' Drop a bomb.
  For bn% = 1 To 10
    If Not a_bomb%(bn%, 3) Then
      a_bomb%(bn%, 1) = aliens%(aln%, 1) + 8
      a_bomb%(bn%, 2) = aliens%(aln%, 2) + 6
      a_bomb%(bn%, 3) = 1
      a_bomb%(bn%, 4) = aln%
      Inc bombs_out%
      Exit For
    EndIf
  Next
End Sub

Sub draw_bomb()
  Local i%
  For i% = 1 To 10
    If a_bomb%(i%, 3) = 1 Then
 
      ' Erase bomb from old position.
      Line 50 + a_bomb%(i%, 1), a_bomb%(i%, 2), 50 + a_bomb%(i%, 1), a_bomb%(i%, 2) + 4, , 0

      Inc a_bomb%(i%, 2), 1
 
      ' Check for bomb hitting bunker.
      If hit_bunker%(a_bomb%(i%, 1), a_bomb%(i%, 2) + 4) Then
        a_bomb%(i%, 3) = 0
        Inc bombs_out%, -1
        Exit Sub
      EndIf

      ' Check for bomb dropping off bottom of screen.
      If a_bomb%(i%, 2) > 224 Then
        a_bomb%(i%, 3) = 0
        Inc bombs_out%, -1
        Exit Sub
      EndIf

      ' Draw bomb in new position.
      Line 50 + a_bomb%(i%, 1), a_bomb%(i%, 2), 50 + a_bomb%(i%, 1), a_bomb%(i%, 2) + 4, , Rgb(Yellow)
 
      ' Check for bomb hitting bullet.
      If ba% Then
        Select Case a_bomb%(i%, 1)
          Case bx% - 2 To bx% + 2
            Select Case a_bomb%(i%, 2)
              Case by% - 4 To by%
                ba% = 0 : a_bomb%(i%, 3) = 0
                Inc bombs_out%, -1
                explode(42 + a_bomb%(i%, 1), a_bomb%(i%, 2), 0)
                Exit Sub
            End Select
        End Select
      EndIf

      ' Check for bomb hitting player.
      If a_bomb%(i%, 2) > 210 Then
        If a_bomb%(i%, 1) >= plx% And a_bomb%(i%, 1) < plx% + 16 Then plHit% = 1
      EndIf
    EndIf
    'Line 50 + bx%, by%, 50 + bx%, by% + 4, , 0
  Next
End Sub

Sub draw_ufo()
  If ua% = 0 Then Exit Sub
  Play Tone UfoSnd%, UfoSnd%, 150
  Inc UfoSnd%, Ustp%
  If UfoSnD% = UfoSndMin% Or UfoSnd% > UfoSndMax% Then Ustp% = -Ustp%
  Box 50 + ux%, 32, 16, 10, , 0, 0
  Inc ux%, udir%
  If ux% > X_MAX% Or ux% < 0 Then ua% = 0 : Exit Sub
  Gui Bitmap 50 + ux%, 32, uf1$, 16, 8, 1, Rgb(Red), 0
End Sub

Sub ufo_x()
  If Not Uxpl%(1) Then Exit Sub
  Inc Uxpl%(3)
  Play Tone 900 + 15 * Uxpl%(3), 900 + 15 * Uxpl%(3), 100
  Select Case uxpl%(3)
    Case 40
      Text 58 + uxpl%(2), 30, " " + Str$(uscr%) + " ", C, , , Rgb(Red)
    Case 70
      Text 58 + uxpl%(2), 30, " " + Str$(uscr%) + " ", C, , , Rgb(Black)
      Uxpl%(1) = 0
      Uxpl%(3) = 0
      inc_score(uscr%)
  End Select
End Sub

Sub draw_bunkers()
  Local i%, j%
  For i% = 0 To 3
    draw_bunker(80 + i% * 45, 184)
    For j% = 1 To 8
      bnk%(i% + 1, 1, j%) = 1
      bnk%(i% + 1, 2, j%) = 1
    Next
  Next
End Sub

Sub draw_bunker(bx%, by%)
  Box bx%, by% + 4, 22, 12, , Rgb(Green), Rgb(Green)
  Box bx% + 1, by% + 3, 20, 1, , Rgb(Green), Rgb(Green)
  Box bx% + 2, by% + 2, 18, 1, , Rgb(Green), Rgb(Green)
  Box bx% + 3, by% + 1, 16, 1, , Rgb(Green), Rgb(Green)
  Box bx% + 4, by%, 14, 1, , Rgb(Green), Rgb(Green)
  Box bx% + 5, by% + 14, 12, 2, , 0, 0
  Box bx% + 6, by% + 13, 10, 1, , 0, 0
  Box bx% + 7, by% + 12, 8, 1, , 0, 0
End Sub

Function hit_bunker%(TsX%, Tsy%)
  Local bhx%, bhy%

  'Y in Range?
  Select Case TsY%
    Case 184 To 200
    'Yes, X in Range of one of the 4 Bunkers?
      Select Case TsX%
        Case 30 To 51
         'Bunker1
          bhy% = Int((Tsy% - 184) / 8)
          bhx% = 1 + Int((TsX% - 30) / 3)
          If Bnk%(1, bhy%, bhx%) = 1 Then
            Bnk%(1, bhy%, bhx%) = 0
            hit_bunker% = 1
            Line 50 + TsX%, Tsy%, 50 + TsX%, TsY% + 4, , 0
            debunk 50 + TsX%, tsy%
            'BA=0
          EndIf
        Case 75 To 96
         'Bunker2
          bhy% = Int((TsY% - 184) / 8)
          bhx% = 1 + Int((TsX% - 75) / 3)
          If Bnk%(2, bhy%, bhx%) = 1 Then
            Bnk%(2, bhy%, bhx%) = 0
            hit_bunker% = 2
            Line 50 + TsX%, Tsy%, 50 + TsX%, TsY% + 4, , 0
            debunk 50 + TsX%, TsY%
            'BA=0
          EndIf
        Case 120 To 141
         'Bunker3
          bhy% = Int((TsY% - 184) / 8)
          bhx% = 1 + Int((TsX% - 120) / 3)
          If Bnk%(3, bhy%, bhx%) = 1 Then
            Bnk%(3, bhy%, bhx%) = 0
            hit_bunker% = 3
            Line 50 + TsX%, Tsy%, 50 + TsX%, TsY% + 4, , 0
            debunk 50 + TsX%, TsY%
            'BA=0
          EndIf
        Case 165 To 186
         'Bunker4
          bhy% = Int((TsY% - 184) / 8)
          bhx% = 1 + Int((TsX% - 165) / 3)
          If Bnk%(4, bhy%, bhx%) = 1 Then
            Bnk%(4, bhy%, bhx%) = 0
            hit_bunker% = 4
            Line 50 + TsX%, Tsy%, 50 + TsX%, TsY% + 4, , 0
            debunk 50 + TsX%, TsY%
          EndIf
      End Select
  End Select
End Function

' Destroys part of a bunker.
Sub debunk(x%, y%)
  Local i%
  For i% = 1 To 40 : Pixel x% - 3 + Rnd * 8, y% - 5 + Rnd * 8, 0 : Next
End Sub

Sub draw_bullet()
  If Not ba% Then Exit Sub
  Line 50 + bx%, by%, 50 + bx%, by% + 4, , Rgb(Black)
  Inc by%, -2
  If by% <= 32 Then ba% = 0 : Exit Sub
  Line 50 + bx%, by%, 50 + bx%, by% + 4, , Rgb(White)
  If by% Mod 8 Then Exit Sub : ' to speed up, do the Test only every 8 Pixel

  ' Check for alien being hit.
  If collision%(bx%, by%) Then
    Line 50 + bx%, by%, 50 + bx%, by% + 4, , 0
    ba% = 0
  EndIf
  If hit_bunker%(bx%, by%) Then ba% = 0 : Exit Sub

  ' Check for UFO being hit.
  If ua% Then
    Select Case by%
      Case 32 To 40
        Select Case bx%
          Case ux% To ux% + 15
            uxpl%(1) = 1 : uxpl%(2) = ux% : uxpl%(3) = 0
            Gui Bitmap 50 + ux%, 32, uf2$, 16, 8, 1, Rgb(Red), 0
            ua% = 0
        End Select
    End Select
  EndIf
End Sub

Function collision%(x%, y%)
  Local ax%, ay%, i%
  Select Case y%
    Case y_pos% + 16 To 214
      For i% = 1 To 55
        If aliens%(i%, 4) Then
          ax% = aliens%(i%, 1) : ay% = aliens%(i%, 2)
          Select Case x%
            Case ax% + 1 To ax% + 13
              Select Case y%
                Case ay% To ay% + 7
                  collision% = 1
                  explode(ax% + 50, ay%, 1)
                  aliens%(i%, 4) = 0
                  Inc num_aliens%, -1
                  inc_score(40 - (10 * aliens%(i%, 3)))
                  Exit Function
              End Select
          End Select
        EndIf
      Next i%
  End Select
End Function

' Increments score (and if necessary the high-score) and updates the display.
Sub inc_score(delta%, full%)
  Inc score%, delta%
  high_score% = Max(score%, high_score%)
  print_score_at(74, 16, score%)
  If (score% = high_score%) Or full% Then print_score_at(214, 16, high_score%)
  If full% Then Text 58, 0, "SCORE<1>" : Text 198, 0, "HI-SCORE"
End Sub

' Prints score at given x, y co-ordinates.
Sub print_score_at(x%, y%, score%)
  Local s$ = Str$(score%)
  If Len(s$) < 4 Then s$ = String$(4 - Len(s$), "0") + s$
  Text x%, y%, s$
End Sub

' Explode alien or bomb.
Sub explode(x%, y%, snd%)
  Local i%
  Gui Bitmap x%, y%, xpl$, 16, 8, 1, Rgb(Yellow), 0
  draw_ufo()
  If snd% = 1 Then
    For i% = 1 To 75
      Play Tone noise%(i%), noise%(i%), 2
      Pause 1
    Next
    Play Stop
  Else
    Pause 20
  EndIf
  draw_ufo()
  Box x%, y%, 16, 10, , 0, 0
End Sub

Sub move_player()
  Local i%, key%
  Call ctrl$, key%
  If key% = 0 Then keys_cursor_ext(key%)
  Select Case key%
    Case 0
      Exit Sub
    Case ctrl.LEFT
      If plx% > 16 Then Inc plx%, -1
    Case ctrl.RIGHT
      If plx% < 188 Then Inc plx%, 1
    Case ctrl.A
      If ba% Then Exit Sub
      If Not ua% Then Inc myst%, Int(Rnd * 3)
      ba% = 1 : bx% = plx% + 7 : by% = 210
      For i% = 1000 To 1 Step -50 : Play Tone 1000 + i%, 1000 + i%, 5 : Pause 2 : Next
    Case ctrl.HOME, ctrl.SELECT, ctrl.START
      on_quit()
  End Select
End Sub

Sub draw_player()
  Gui Bitmap 50 + plx%, 214, ply$(1), 16, 8, 1, Rgb(Green), 0
End Sub

' Shows player explosion.
Sub explode_player()
  Local i%, nse%
  For i% = 1 To 3
    Gui Bitmap 50 + plx%, 214, ply$(2), 16, 8, 1, Rgb(Green), 0
    For nse% = 1 To 100 : Play Tone noise%(nse%), noise%(nse%), 2 : Pause 1 : Next
    Gui Bitmap 50 + plx%, 214, ply$(3), 16, 8, 1, Rgb(Green), 0
    For nse% = 100 To 200 : Play Tone noise%(nse%), noise%(nse%), 2 : Pause 1 : Next
  Next
  For nse% = 1 To 200 : Play Tone noise%(nse%), noise%(nse%), 2 : Pause 1 : Next
  Play Stop
  dummy% = wait%(500)
  clear_bombs()
  Box 50 + plx%, 214, 16, 10, , 0, 0
End Sub

' Fills the array of Aliens with x,y,type and Live Values
Sub setup_aliens()
  Local at%, num% = 1, r%, n%
  a_ground% = 0
  For r% = 1 To 5
    Select Case r%
      Case 1:    at% = 1 ' Alien type
      Case 2, 3: at% = 2
      Case Else: at% = 3
    End Select
    For n% = 1 To 11
      aliens%(num%, 1) = n% * 16
      aliens%(num%, 2) = y_pos% + r% * 16
      aliens%(num%, 3) = at%
      aliens%(num%, 4) = 1
      Inc num%
    Next
  Next
  trn% = 0
End Sub

Sub draw_screen()
  Cls
  Box 50, 229, 220, 1, , , Rgb(Green)
  inc_score(0, 1)
  Text 46, 230, Str$(level%)
  draw_bunkers()
End Sub

' Redraws all the aliens from bottom right to top left.
Sub draw_aliens()
  Local i%, ax%, ay%, at%
  For i% = 55 To 1 Step -1
    ax% = 50 + aliens%(i%, 1)
    ay% = aliens%(i%, 2)
    at% = aliens%(i%, 3)
    If aliens%(i%, 4) Then
      Box ax%, ay%, 16, 10, , 0, 0
      Gui Bitmap ax%, ay%, alien$(at%, anim%), 16, 8, 1, Rgb(White), 0
    EndIf
  Next
End Sub

' Move a Single Alien one Step
' Aliens are counted from bottom right to top left
Sub move_single()
  ' Find a live alien.
  Do While Not aliens%(anr%, 4)
    ' Move to previous alien
    Inc anr%, -1
    If anr% < 1 Then
      anr% = 55
      If trn% = 1 Then adir% = -adir% : down_aliens() : draw_aliens() : trn% = 0
      Inc mvsnd% : mvsnd% = mvsnd% And 3
      ' Play Sound if all Aliens move a Step.. but not if Ufo is out
      If Not ua% Then Play Tone snd%(mvsnd% + 1), snd%(mvsnd% + 1), 80
      anim% = Choice(anim% = 1, 2, 1)
    EndIf
  Loop

  Local ax% = aliens%(anr%, 1), ay% = aliens%(anr%, 2), at% = aliens%(anr%, 3)
  Box ax% + 50, ay%, 16, 10, , 0, 0
  Inc ax%, adir%
  Gui Bitmap ax% + 50, ay%, alien$(at%, anim%), 16, 8, 1, Rgb(White), 0
  aliens%(anr%, 1) = ax%
  If ax% >= X_MAX% Or ax% < 1 Then trn% = 1

  Inc anr%, -1
End Sub

' Moves all aliens down by 8 pixels.
Sub down_aliens()
  Local ax%, ay%, i%
  For i% = 55 To 1 Step -1
    If aliens%(i%, 4) Then
      ax% = aliens%(i%, 1) : ay% = aliens%(i%, 2)
      Box ax% + 50, ay%, 16, 10, , 0, 0
      aliens%(i%, 2) = ay% + 8
      If ay% + 8 >= 202 Then a_ground% = 1
    EndIf
  Next
End Sub

Sub show_game_over()
  ctrl.wait_until_idle(ctrl$)
  Box 110, 92, 100, 44, 1, 0, 0
  Text 160, 100, "PLAYER<1>", C
  Do
    Text 160, 116, "GAME OVER", C
    If wait%(600, ctrl.A Or ctrl.START) Then Exit Do
    Text 160, 116, "         ", C
    If wait%(600, ctrl.A Or ctrl.START) Then Exit Do
  Loop
End Sub

' Sprite data
sr1:
Data 1, 128, 3, 192, 7, 224, 13, 176, 15, 240, 5, 160, 8, 16, 4, 32
Data 1, 128, 3, 192, 7, 224, 13, 176, 15, 240, 2, 64, 5, 160, 10, 80

sr2:
Data 8, 32, 4, 64, 15, 224, 27, 176, 63, 248, 47, 232, 40, 40, 6, 192
Data 8, 32, 36, 72, 47, 232, 59, 184, 63, 248, 31, 240, 8, 32, 16, 16

sr3:
Data 7, 224, 31, 248, 63, 252, 57, 156, 63, 252, 14, 112, 25, 152, 48, 12
Data 7, 224, 31, 248, 63, 252, 57, 156, 63, 252, 14, 112, 25, 152, 12, 48

plyr:
Data 1, 0, 3, 128, 3, 128, 63, 248, 127, 252, 127, 252, 127, 252, 127, 252
Data 2, 0, 0, 16, 2, 160, 18, 0, 1, 176, 69, 168, 31, 228, 63, 245
Data 16, 4, 130, 25, 16, 192, 2, 2, 75, 97, 33, 196, 31, 224, 55, 228

xpld:
Data 4, 64, 34, 136, 16, 16, 8, 32, 96, 12, 8, 32, 18, 144, 36, 72

ufo:
Data 0, 0, 7, 224, 31, 248, 63, 252, 109, 182, 255, 255, 57, 156, 16, 8

ufo_xpl:
Data 148, 10, 64, 48, 143, 24, 31, 206, 58, 167, 143, 140, 5, 24, 136, 136
