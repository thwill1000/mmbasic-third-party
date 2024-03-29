' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
'
' MMBasic Controller Library

'!if not(defined(PICOMITE) or defined(CMM2) or defined(MMB4L))
  '!error "File 'ctrl.inc' was transpiled without CMM2, MMB4L or PICOMITE defined"
'!endif

'!if not(defined(PICOMITE) xor defined(CMM2) xor defined(MMB4L))
  '!error "File 'ctrl.inc' was transpiled with more than one of CMM2, MMB4L or PICOMITE defined"
'!endif

'!info defined CMM2
'!info defined MMB4L
'!info defined PICOMITE

'!if not defined(CTRL_USE_INKEY)
  '!if defined(CMM2)
    '!define CTRL_USE_KEYDOWN
  '!elif defined(MMB4L)
    '!define CTRL_USE_INKEY
  '!elif defined(PICOMITE)
    '!define CTRL_USE_ON_PS2
  '!endif
'!endif

'!info defined CTRL_ONE_PLAYER
'!info defined CTRL_NO_CURSORS
'!info defined CTRL_NO_SNES
'!info defined CTRL_USE_INKEY
'!info defined CTRL_USE_ON_PS2
'!info defined CTRL_USE_KEYDOWN

Const ctrl.VERSION = 908  ' 0.9.8

' Button values as returned by controller driver subroutines.
Const ctrl.R      = &h01
Const ctrl.START  = &h02
Const ctrl.HOME   = &h04
Const ctrl.SELECT = &h08
Const ctrl.L      = &h10
Const ctrl.DOWN   = &h20
Const ctrl.RIGHT  = &h40
Const ctrl.UP     = &h80
Const ctrl.LEFT   = &h100
Const ctrl.ZR     = &h200
Const ctrl.X      = &h400
Const ctrl.A      = &h800
Const ctrl.Y      = &h1000
Const ctrl.B      = &h2000
Const ctrl.ZL     = &h4000

Const ctrl.OPEN  = -1
Const ctrl.CLOSE = -2
Const ctrl.SOFT_CLOSE = -3

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const ctrl.PULSE = 0.001 ' 1 micro-second

' When a key is down the corresponding byte of this 256-byte map is set,
' when the key is up then it is unset.
'
' Note that when using INKEY$ (as opposed to the CMM2 'KEYDOWN' function or
' the PicoMiteVGA 'ON PS2' command) to read the keyboard we cannot detect
' keyup events and instead automatically clear a byte after it is read.
Dim ctrl.key_map%(31 + Mm.Info(Option Base))

'!ifdef CTRL_USE_ON_PS2
' Map used to convert PS/2 set 2 scan codes to entries in ctrl.key_map%().
' The scan code first has to be converted into a single byte value,
' see ctrl.on_ps2().
Dim ctrl.scan_map%(31)
'!endif

'!ifdef CTRL_USE_KEYDOWN
' Timer number configured for reading the KEYDOWN state on the CMM2.
Dim ctrl.tick_nbr%
'!endif

' Gets controller configuration read from ".config" file.
' If the file does not exist then prompts the user and creates it.
' Currently can only return the values "pico_game_vga" or "generic".
Function ctrl.get_config$()
  Const CONFIG_FILE = "A:/.config"
  Local i%, s$, key$, value$

  If Mm.Info(Exists File CONFIG_FILE$) Then
    Open CONFIG_FILE For Input As #1
    Do
      Line Input #1, s$
      key$ = LCase$(Field$(Field$(s$, 1, "=", Chr$(34)),1, "#;", Chr$(34)))
      value$ = LCase$(Field$(Field$(s$, 2, "=", Chr$(34)), 1, "#;", Chr$(34)))
    Loop While s$ <> "" And key$ <> "pcb"
    ctrl.get_config$ = value$
  Else
    Do
      Line Input "Enable PicoGAME VGA controls [y|n] ? ", s$
      s$ = LCase$(s$)
    Loop While s$ <> "y" And s$ <> "n"
    ctrl.get_config$ = Choice(s$ = "y", "pico-game-vga", "generic")
    Open CONFIG_FILE For Output As #1
    Print #1, "pcb = " + ctrl.get_config$
  EndIf
  Close #1

  ' Replace '-' with '_'
  For i% = 1 To Len(ctrl.get_config$)
    If Peek(Var ctrl.get_config$, i%) = 45 Then Poke Var ctrl.get_config$, i%, 95
  Next
End Function

' Initialises keyboard reading.
'
' @param  period%  CMM2 only - interval to read KEYDOWN state, default 40 ms.
' @param  nbr%     CMM2 only - timer nbr to read KEYDOWN state, default 4.
Sub ctrl.init_keys(period%, nbr%)
  ctrl.term_keys()
'!uncomment_if CTRL_USE_INKEY
'  On Key ctrl.on_key()
'!endif
'!uncomment_if CTRL_USE_ON_PS2
  ' Read Save
  ' Restore ctrl.scan_map_data
  ' Local i%
  ' For i% = Bound(ctrl.scan_map%(), 0) To Bound(ctrl.scan_map%(), 1)
  '   Read ctrl.scan_map%(i%)
  ' Next
  ' Read Restore
  ' On Ps2 ctrl.on_ps2()
'!endif
'!ifdef CTRL_USE_KEYDOWN
  ctrl.tick_nbr% = Choice(nbr% = 0, 4, nbr%)
  SetTick Choice(period% = 0, 40, period%), ctrl.on_tick(), ctrl.tick_nbr%
'!endif
End Sub

'!ifdef CTRL_USE_KEYDOWN
' Note there is little point in calling KeyDown(0) to determine the number of
' keys that are down, hardware limitations mean it's unlikely ever to be > 4
' and if a given key isn't down it just returns 0 so we harmlessly set that
' byte in the key map.
Sub ctrl.on_tick()
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Poke Var ctrl.key_map%(), KeyDown(1), 1
  Poke Var ctrl.key_map%(), KeyDown(2), 1
  Poke Var ctrl.key_map%(), KeyDown(3), 1
  Poke Var ctrl.key_map%(), KeyDown(4), 1
End Sub
'!endif

'!ifdef CTRL_USE_INKEY
' TODO: use the 'lower-case' character for all keys, not just letters.
Sub ctrl.on_key()
  Poke Var ctrl.key_map%(), Asc(LCase$(Inkey$)), 1
End Sub
'!endif

'!ifdef CTRL_USE_ON_PS2
Sub ctrl.on_ps2()
  Local ps2% = Mm.Info(PS2)
  Select Case ps2%
    Case < &hE000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), ps2% And &hFF), 1
    Case < &hF000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), (ps2% And &hFF) + &h80), 1
    Case < &hE0F000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), ps2% And &hFF), 0
    Case Else : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), (ps2% And &hFF) + &h80), 0
  End Select
End Sub
'!endif

' Terminates keyboard reading.
Sub ctrl.term_keys()
'!uncomment_if CTRL_USE_INKEY
  ' On Key 0
'!endif
'!uncomment_if CTRL_USE_ON_PS2
  ' On Ps2 0
'!endif
'!ifdef CTRL_USE_KEYDOWN
  If ctrl.tick_nbr% <> 0 Then SetTick 0, 0, ctrl.tick_nbr%
'!endif
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Do While Inkey$ <> "" : Loop
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
'!uncomment_if CTRL_USE_INKEY
  ' Poke Var ctrl.key_map%(), i%, 0
'!endif
End Function

Function ctrl.poll_multiple$(drivers$(), mask%, duration%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF), i%
  Do
    For i% = Bound(drivers$(), 0) To Bound(drivers$(), 1)
      If ctrl.poll_single%(drivers$(i%), mask%) Then
        ctrl.poll_multiple$ = drivers$(i%)
        Exit Do
      EndIf
    Next
  Loop While Timer < expires%
End Function

' Opens, polls (for a maximum of 5ms) and closes a controller.
'
' @param  driver$  controller driver function.
' @param  mask%    bit mask to match against.
' @return          1 if any of the bits in the mask match what is read from the
'                  controller, otherwise 0.
Function ctrl.poll_single%(driver$, mask%)
  On Error Ignore
  Call driver$, ctrl.OPEN
  If Mm.ErrNo = 0 Then
    Local key%, t% = Timer + 5
    Do
      Call driver$, key%
      If key% And mask% Then
        ctrl.poll_single% = 1
        ' Wait for user to release key.
        Do While key% : Pause 5 : Call driver$, key% : Loop
        Exit Do
      EndIf
    Loop While Timer < t%
    Call driver$, ctrl.SOFT_CLOSE
  EndIf
  On Error Abort
End Function

' Gets a string representation of bits read from a controller driver.
'
' @param  x%  bits returned by driver.
' @return     string representation.
Function ctrl.bits_to_string$(x%)
  Static BUTTONS$(14) = ("R","Start","Home","Select","L","Down","Right","Up","Left","ZR","X","A","Y","B","ZL")

  If x% = 0 Then
    ctrl.bits_to_string$ = "No buttons down"
    Exit Function
  EndIf

  ctrl.bits_to_string$ = Str$(x%) + " = "
  Local count%, i%, s$
  For i% = 0 To Bound(BUTTONS$(), 1)
    If x% And 2^i% Then
      s$ = BUTTONS$(i%)
      If count% > 0 Then Cat ctrl.bits_to_string$, ", "
      Cat ctrl.bits_to_string$, s$
      Inc count%
    EndIf
  Next
End Function

'!ifndef CTRL_NO_CURSORS

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
Sub keys_cursor(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A
  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
End Sub

'!endif ' CTRL_NO_CURSORS

'!ifdef PICOMITE

' Atari joystick on PicoGAME Port A.
Sub atari_a(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(GP14) * ctrl.A
      Inc x%, Not Pin(GP0)  * ctrl.UP
      Inc x%, Not Pin(GP1)  * ctrl.DOWN
      Inc x%, Not Pin(GP2)  * ctrl.LEFT
      Inc x%, Not Pin(GP3)  * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP0, DIn : SetPin GP1, DIn : SetPin GP2, DIn : SetPin GP3, DIn : SetPin GP14, DIn
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP0, Off : SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off : SetPin GP14, Off
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' Atari joystick on PicoGAME Port B.
Sub atari_b(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(GP15) * ctrl.A
      Inc x%, Not Pin(GP28) * ctrl.UP
      Inc x%, Not Pin(GP4)  * ctrl.DOWN
      Inc x%, Not Pin(GP5)  * ctrl.LEFT
      Inc x%, Not Pin(GP22) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP4, DIn : SetPin GP5, DIn : SetPin GP15, DIn : SetPin GP22, DIn : SetPin GP28, DIn
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP15, Off : SetPin GP22, Off : SetPin GP28, Off
    End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!ifndef CTRL_NO_SNES

' SNES gamepad on PicoGAME Port A.
'
'   GP2: Latch, GP3: Clock, GP1: Data
Sub snes_a(x%)
  Select Case x%
    Case >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.Y      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.X      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.L      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.R      : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case Else
      nes_a(x%)
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' SNES gamepad on PicoGAME Port B.
'
'   GP5: Latch, GP22: Clock, GP4: Data
Sub snes_b(x%)
  Select Case x%
    Case >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.Y      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.X      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.L      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.R      : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case Else
      nes_b(x%)
  End Select
End Sub

'!endif ' !ifndef CTRL_ONE_PLAYER

'!endif ' !ifndef CTRL_NO_SNES

' Reads port A connected to a NES gamepad.
'
' Note that the extra pulse after reading bit 7 (Right) should not be necessary,
' but in practice some NES clone controllers require it to behave correctly.
'
'   GP2: Latch, GP3: Clock, GP1: Data
Sub nes_a(x%)
  Select Case x%
    Case >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP1, Din : SetPin GP2, Dout : SetPin GP3, Dout
      Pin(GP2) = 0 : Pin(GP3) = 0
      nes_a(0) ' Discard the first reading.
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' NES gamepad on PicoGAME Port B.
'
'   GP5: Latch, GP22: Clock, GP4: Data
Sub nes_b(x%)
  Select Case x%
    Case >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP4, Din : SetPin GP5, Dout : SetPin GP22, Dout
      Pin(GP5) = 0 : Pin(GP22) = 0
      nes_b(0) ' Discard the first reading.
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP22, Off
  End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!endif ' PICOMITE

'!ifdef CMM2

' Atari joystick port on CMM2 Deluxe G2.
Sub atari_dx(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(32) * ctrl.A
      Inc x%, Not Pin(33) * ctrl.B
      Inc x%, Not Pin(35) * ctrl.UP
      Inc x%, Not Pin(36) * ctrl.DOWN
      Inc x%, Not Pin(38) * ctrl.LEFT
      Inc x%, Not Pin(40) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin 32, Din, PullUp : SetPin 33, Din, PullUp : SetPin 35, Din, PullUp
      SetPin 36, Din, PullUp : SetPin 38, Din, PullUp : SetPin 40, Din, PullUp
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin 32, Off : SetPin 33, Off : SetPin 35, Off
      SetPin 36, Off : SetPin 38, Off : SetPin 40, Off
  End Select
End Sub

' NES gamepad attached USING ADAPTER to Atari joystick port on CMM2 Deluxe G2.
'
' IMPORTANT! the adapter is required to swap the Male DB9 (CMM2) +5V supply on
' Pin 7 to Pin 6 on the Female DB9 (Gamepad).
'
'   Pin 38: Latch, Pin 40: Clock, Pin 36: Data
Sub nes_dx(x%)
  Select Case x%
    Case >= 0
      Pulse 38, ctrl.PULSE
      x% =    Not Pin(36) * ctrl.A      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.B      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.SELECT : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.START  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.UP     : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.DOWN   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.LEFT   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.RIGHT  : Pulse 40, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin 36, Din, PullUp : SetPin 38, Dout : SetPin 40, Dout
      Pin(38) = 0 : Pin(40) = 0
      nes_dx(0) ' Discard the first reading.
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin 36, Off : SetPin 38, Off : SetPin 40, Off
  End Select
End Sub

'!ifndef CTRL_NO_SNES

' SNES gamepad attached USING ADAPTER to Atari joystick port on CMM2 Deluxe G2.
'
' IMPORTANT! the adapter is required to swap the Male DB9 (CMM2) +5V supply on
' Pin 7 to Pin 6 on the Female DB9 (Gamepad).
'
'   Pin 38: Latch, Pin 40: Clock, Pin 36: Data
Sub snes_dx(x%)
  Select Case x%
    Case >= 0
      Pulse 38, ctrl.PULSE
      x% =    Not Pin(36) * ctrl.B      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.Y      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.SELECT : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.START  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.UP     : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.DOWN   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.LEFT   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.RIGHT  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.A      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.X      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.L      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.R      : Pulse 40, ctrl.PULSE
      Exit Sub
    Case Else
      nes_dx(x%)
  End Select
End Sub

'!endif '!ifndef CTRL_NO_SNES

Sub wii_internal(i2c%, x%, type%)
  Static is_open%(3)
  If is_open%(i2c%) = -1 Then Error "I2C" + Str$(i2c%) + " failed to open" : Exit Sub

  If x% >= 0 Then
    Select Case is_open%(i2c%)
      Case &hA4200101
        x% = Classic(B, i2c%)
'        Inc x%, (Classic(LY, i2c%) > 200) * ctrl.UP
'        Inc x%, (Classic(LY, i2c%) < 60)  * ctrl.DOWN
'        Inc x%, (Classic(LX, i2c%) < 60)  * ctrl.LEFT
'        Inc x%, (Classic(LX, i2c%) > 200) * ctrl.RIGHT
        If x% = &h7FFF Then x% = 0 ' Ignore this glitch.
      Case &hA4200000
        x% =    Nunchuk(Z,  i2c%) * ctrl.A
        Inc x%, Nunchuk(C,  i2c%) * ctrl.B
        Inc x%, (Nunchuk(JY, i2c%) > 200) * ctrl.UP
        Inc x%, (Nunchuk(JY, i2c%) < 60)  * ctrl.DOWN
        Inc x%, (Nunchuk(JX, i2c%) < 60)  * ctrl.LEFT
        Inc x%, (Nunchuk(JX, i2c%) > 200) * ctrl.RIGHT
    End Select
    Exit Sub
  EndIf

  Select Case x%
    Case ctrl.SOFT_CLOSE
      ' Do nothing
      Exit Sub

    Case ctrl.CLOSE
      Select Case is_open%(i2c%)
        Case &hA4200000
          Controller Nunchuk Close i2c%
        Case &hA4200101
          Controller Classic Close i2c%
      End Select
      is_open%(i2c%) = 0
      Exit Sub

    Case ctrl.OPEN
      If is_open%(i2c%) Then Exit Sub
      is_open%(i2c%) = -1
      Controller Nunchuk Open i2c%
      If Mm.ErrNo Then
        If InStr(Mm.ErrMsg$, "already OPEN") Then
          Error "I2C" + Str$(i2c%) + " already open"
        Else
          Error "I2C" + Str$(i2c%) + " not connected"
        EndIf
        Exit Sub
      EndIf
      is_open%(i2c%) = Nunchuk(T, i2c%)
      Select Case is_open%(i2c%)
        Case &hA4200000
          If Not(type% And &h01) Then
            Controller Nunchuk Close i2c%
            is_open%(i2c%) = -1
            Error "Nunchuck controller on I2C" + Str$(i2c%) + " not supported"
          EndIf
        Case &hA4200101
          Controller Nunchuk Close i2c%
          If Not(type% And &h10) Then
            is_open%(i2c%) = -1
            Error "Classic controller on I2C" + Str$(i2c%) + " not supported"
          Else
            Controller Classic Open i2c%
          EndIf
        Case Else
          Controller Nunchuck Close i2c%
          is_open%(i2c%) = -1
          Error "Unrecognised controller on I2C" + Str$(i2c%)
      End Select
  End Select
End Sub

' Wii Nunchuk OR Classic gamepad on I2C1.
Sub wii_any_1(x%)
  wii_internal(1, x%, &h11)
End Sub

' Wii Nunchuk OR Classic gamepad on I2C2.
Sub wii_any_2(x%)
  wii_internal(2, x%, &h11)
End Sub

' Wii Nunchuk OR Classic gamepad on I2C3.
Sub wii_any_3(x%)
  wii_internal(3, x%, &h11)
End Sub

' Wii Classic gamepad on I2C1.
Sub wii_classic_1(x%)
  wii_internal(1, x%, &h10)
End Sub

' Wii Classic gamepad on I2C2.
Sub wii_classic_2(x%)
  wii_internal(2, x%, &h10)
End Sub

' Wii Classic gamepad on I2C3.
Sub wii_classic_3(x%)
  wii_internal(3, x%, &h10)
End Sub

' Wii Nunchuk on I2C1.
Sub wii_nunchuk_1(x%)
  wii_internal(1, x%, &h01)
End Sub

' Wii Nunchuk on I2C2.
Sub wii_nunchuk_2(x%)
  wii_internal(2, x%, &h01)
End Sub

' Wii Nunchuk on I2C3.
Sub wii_nunchuk_3(x%)
  wii_internal(3, x%, &h01)
End Sub

'!endif ' CMM2

'!ifdef CTRL_USE_ON_PS2
ctrl.scan_map_data:

Data &h9C92919395009900, &h0060099496989A00, &h0031710000008B00, &h00327761737A0000
Data &h0033346564786300, &h0035727466762000, &h0036796768626E00, &h003837756A6D0000
Data &h0039306F696B2C00, &h002D703B6C2F2E00, &h00003D5B00270000, &h000023005D0A0000
Data &h0008000000005C00, &h0000003734003100, &h001B383635322E30, &h0000392A2D332B9B
Data &h0000000097000000, &h0000000000000000, &h0000000000008B00, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000008682008700, &h0000808300817F84, &h0000889D00890000
'!endif
