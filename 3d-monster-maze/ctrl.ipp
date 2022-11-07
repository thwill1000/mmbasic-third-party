' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
'
' MMBasic Controller Library
'
'!ifndef SPTRANS
Error "File 'ctrl.ipp' requires transpiling"
'!endif
'!ifdef PICOMITE
  '!set CTRL_PLATFORM_SET
' Preprocessor flag PICOMITE defined
'!endif
'!ifdef CMM2
  '!ifdef CTRL_PLATFORM_SET
    '!error "File 'ctrl.ipp' was transpiled with more than one of the CMM2, MMB4L or PICOMITE flags set"
  '!endif
  '!set CTRL_PLATFORM_SET
' Preprocessor flag CMM2 defined
'!endif
'!ifdef MMB4L
  '!ifdef CTRL_PLATFORM_SET
    '!error "File 'ctrl.ipp' was transpiled with more than one of the CMM2, MMB4L or PICOMITE flags set"
  '!endif
  '!set CTRL_PLATFORM_SET
' Preprocessor flag MMB4L defined
'!endif
'!ifndef CTRL_PLATFORM_SET
  '!error "File 'ctrl.ipp' was transpiled without CMM2, MMB4L or PICOMITE flags set"
'!endif
'!ifdef CTRL_ONE_PLAYER
' Preprocessor flag CTRL_ONE_PLAYER defined
'!endif
'!ifdef CTRL_NO_CURSORS
' Preprocessor flag CTRL_NO_CURSORS defined
'!endif
'!ifdef CTRL_NO_SNES
' Preprocessor flag CTRL_NO_SNES defined
'!endif

Const ctrl.VERSION = 900  ' 0.9.00

' Button values as returned by controller read functions.
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

'!ifdef PICOMITE

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const ctrl.PULSE = 0.001 ' 1 micro-second

'!endif

'!ifndef CMM2

' When a key is pressed ctrl.on_key() sets the corresponding byte of this
' 256-byte map to 1. When ctrl.keydown(i%) is called the corresponding
' byte is read and set to 0. Note that a 256-bit map could be used but would
' be slower.
Dim ctrl.key_map%(31 + Mm.Info(Option Base))

'!endif

'!ifndef CTRL_NO_CURSORS

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
Sub keys_cursor(x%)
  If x% >= 0 Then
    x% =    ctrl.keydown%(32)  * ctrl.A
    Inc x%, ctrl.keydown%(128) * ctrl.UP
    Inc x%, ctrl.keydown%(129) * ctrl.DOWN
    Inc x%, ctrl.keydown%(130) * ctrl.LEFT
    Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
  EndIf
End Sub

'!endif ' CTRL_NO_CURSORS

' Initialises keyboard reading.
Sub ctrl.init_keys()
  ctrl.term_keys()
'!ifndef CMM2
  On Key ctrl.on_key()
'!endif
End Sub

'!ifndef CMM2
Sub ctrl.on_key()
  Local ch$ = Inkey$
  If ch$ <> "" Then Poke Var ctrl.key_map%(), Asc(ch$), 1
End Sub
'!endif

' Terminates keyboard reading.
Sub ctrl.term_keys()
'!ifndef CMM2
  On Key 0
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
'!endif
  Do While Inkey$ <> "" : Loop
End Sub

Function ctrl.keydown%(i%)
'!ifndef CMM2
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
  If ctrl.keydown% Then Poke Var ctrl.key_map%(), i%, 0
'!endif
'!ifdef CMM2
  Local j%
  For j% = 1 To KeyDown(0)
    If KeyDown(j%) = i% Then
      ctrl.keydown% = 1
      Exit Function
    EndIf
  Next
'!endif
End Function

Function ctrl.poll$(duration%, ctrls$())
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)
  Local key%, i%, t%
  Do
    For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
      On Error Ignore
      Call ctrls$(i%), ctrl.OPEN
      On Error Abort
      If Mm.ErrNo <> 0 Then Continue For
      t% = Timer + 20
      Do
        Call ctrls$(i%), key%
        If (key% = ctrl.A) Or (key% = ctrl.START) Then
          ctrl.poll$ = ctrls$(i%)
          ' Wait for user to release key.
          Do While key% <> 0 : Call ctrls$(i%), key% : Loop
          Call ctrls$(i%), ctrl.CLOSE
          Exit Function
        EndIf
      Loop While Timer < t%
      Call ctrls$(i%), ctrl.CLOSE
    Next
  Loop Until Timer >= expires%
End Function

'!ifdef PICOMITE

' Atari joystick on PicoGAME Port A.
Sub atari_a(x%)
  Select Case x%
    Case Is >= 0
      x% =    Not Pin(GP14) * ctrl.A
      Inc x%, Not Pin(GP0)  * ctrl.UP
      Inc x%, Not Pin(GP1)  * ctrl.DOWN
      Inc x%, Not Pin(GP2)  * ctrl.LEFT
      Inc x%, Not Pin(GP3)  * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP14, DIn
      SetPin GP0, DIn
      SetPin GP1, DIn
      SetPin GP2, DIn
      SetPin GP3, DIn
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' Atari joystick on PicoGAME Port B.
Sub atari_b(x%)
  Select Case x%
    Case Is >= 0
      x% =    Not Pin(GP15) * ctrl.A
      Inc x%, Not Pin(GP28) * ctrl.UP
      Inc x%, Not Pin(GP4)  * ctrl.DOWN
      Inc x%, Not Pin(GP5)  * ctrl.LEFT
      Inc x%, Not Pin(GP22) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP15, DIn
      SetPin GP28, DIn
      SetPin GP4, DIn
      SetPin GP5, DIn
      SetPin GP22, DIn
    End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!ifndef CTRL_NO_SNES

' SNES gamepad on PicoGAME Port A.
Sub snes_a(x%)
  Select Case x%
    Case Is >= 0
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
    Case ctrl.OPEN
      nes_a(ctrl.OPEN)
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' SNES gamepad on PicoGAME Port B.
Sub snes_b(x%)
  Select Case x%
    Case Is >= 0
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
    Case ctrl.OPEN
      nes_b(ctrl.OPEN)
  End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!endif ' CTRL_NO_SNES

' Reads port A connected to a NES gamepad.
'
' Note that the extra pulse after reading bit 7 (Right) should not be necessary,
' but in practice some NES clone controllers require it to behave correctly.
Sub nes_a(x%)
  Select Case x%
    Case Is >= 0
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
      SetPin GP2, Dout ' Latch
      SetPin GP3, Dout ' Clock
      SetPin GP1, Din  ' Data
      Pin(GP2) = 0
      Pin(GP3) = 0
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' NES gamepad on PicoGAME Port B.
Sub nes_b(x%)
  Select Case x%
    Case Is >= 0
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
      SetPin GP5,  Dout ' Latch
      SetPin GP22, Dout ' Clock
      SetPin GP4,  Din  ' Data
      Pin(GP5) = 0
      Pin(GP22) = 0
  End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!endif ' PICOMITE

'!ifdef CMM2

' Atari joystick port on CMM2 Deluxe G2.
Sub atari_dx(x%)
  Select Case x%
    Case Is >= 0
      x% =    Not Pin(32) * ctrl.A
      Inc x%, Not Pin(33) * ctrl.B
      Inc x%, Not Pin(35) * ctrl.UP
      Inc x%, Not Pin(36) * ctrl.DOWN
      Inc x%, Not Pin(38) * ctrl.LEFT
      Inc x%, Not Pin(40) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin 32, Din, PullUp
      SetPin 33, Din, PullUp
      SetPin 35, Din, PullUp
      SetPin 36, Din, PullUp
      SetPin 38, Din, PullUp
      SetPin 40, Din, PullUp
  End Select
End Sub

' Wii Classic gamepad on I2C1.
Sub wii_classic_1(x%)
  Select Case x%
    Case Is >= 0
      x% = Classic(B, 1)
      Exit Sub
    Case ctrl.OPEN
      Controller Classic Open 1
    Case ctrl.CLOSE
      Controller Classic Close 1
  End Select
End Sub

' Wii Classic gamepad on I2C2.
Sub wii_classic_2(x%)
  Select Case x%
    Case Is >= 0
      x% = Classic(B, 2)
      Exit Sub
    Case ctrl.OPEN
      Controller Classic Open 2
    Case ctrl.CLOSE
      Controller Classic Close 2
  End Select
End Sub

' Wii Classic gamepad on I2C3.
Sub wii_classic_3(x%)
  Select Case x%
    Case Is >= 0
      x% = Classic(B, 3)
      Exit Sub
    Case ctrl.OPEN
      Controller Classic Open 3
    Case ctrl.CLOSE
      Controller Classic Close 3
  End Select
End Sub

'!endif
