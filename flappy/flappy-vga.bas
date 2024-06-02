  'flappy bird for picomiteVGA

  'versions
  ' flappy7   first release
  ' flappy8   fixed collision bug, screen corruption in fall
  ' flappy9   fl_day2 background and pipe/animation adaptions
  ' flappy10  sfx added, end game added, fl_day6 background, store scores
  ' flappy11  development for the LCD version (uses PETSCII graphics mechanism)
  ' flappy12  VGA version for development, night mode, cup, progressive speed

  If Mm.Device$ = "MMB4L" Then Option Simulate "PicoMiteVGA"

  Const PATH$ = Choice(Mm.Info(Path) = "NONE", "", Mm.Info(Path))

  'setup memory and screen
  Option DEFAULT integer
  MODE 2
  FRAMEBUFFER layer '9 'magenta = transparent


  'load sprites
  Sprite Load PATH$ + "gfx/tt.spr",1
  Sprite Load PATH$ + "gfx/bs.spr",2
  Sprite Load PATH$ + "gfx/ts.spr",3
  Sprite Load PATH$ + "gfx/flap1.spr",4
  Sprite Load PATH$ + "gfx/flap2.spr",5
  Sprite Load PATH$ + "gfx/flapd2.spr",6
  Sprite Load PATH$ + "gfx/nulll.spr",7
  Sprite Load PATH$ + "gfx/one.spr",8
  Sprite Load PATH$ + "gfx/two.spr",9
  Sprite Load PATH$ + "gfx/three.spr",10
  Sprite Load PATH$ + "gfx/four.spr",11
  Sprite Load PATH$ + "gfx/five.spr",12
  Sprite Load PATH$ + "gfx/six.spr",13
  Sprite Load PATH$ + "gfx/seven.spr",14
  Sprite Load PATH$ + "gfx/eight.spr",15
  Sprite Load PATH$ + "gfx/nine.spr",16
  Sprite Load PATH$ + "gfx/flap3.spr",17
  Sprite Load PATH$ + "gfx/bmedalw.spr",18'22
  Sprite Load PATH$ + "gfx/smedalw.spr",19'23
  Sprite Load PATH$ + "gfx/gmedalw.spr",20'24
  Sprite Load PATH$ + "gfx/cup_medalw.spr",21'24


  'load background on to layer N
  FRAMEBUFFER write n
  Load Image PATH$ + Choice(Rnd() < 0.5, "gfx/fl_day.bmp", "gfx/fl_night.bmp")


  'game variables
  h_beat=50     'speed of the game
  force=7       'force of the wing

  'game defines
  gap=90        'between columns
  xb=100        'bird start position (constant)
  yb=100        'bird vertical position
  v=1           'falling speed due to gravity
  xnp=320       'new pipe position
  ynp=40        'new pipe length (default)
  pgap=80       'gap between top and bottom pites
  spr=4         'default bird sprite
  Dim yy(1)     'pipe height last 2 pipes
  pn=1          'pipe number to be shifted in right side
  die=16:hit=17:swoosh=18:wing=19   'sfx samples
  Dim scores(3)=(5,10,15,16) 'default score list bronze,silver,gold,cup
  Dim medal$(3)=("b","s","g","cup_") 'prefix names for medal graphics


  'get old scores from flash
  Open PATH$ + "score.txt" For input As #1
  For i=0 To 3:Input #1,a$:scores(i)=Val(a$):Next
  Close #1


  'title screen
  FRAMEBUFFER write l:CLS
  Load image PATH$ + "gfx/flappy.bmp",70,50
  Pause 3000


  'create start instructions
  CLS
  Load image PATH$ + "gfx/getready.bmp",70,40
  Load image PATH$ + "gfx/taptap.bmp",136,110
  Do : Loop While read_input$()=""
  CLS


  'init for main game loop
  Timer =0
  Play modfile PATH$ + "sfx/sll3sfx2.mod"
  FRAMEBUFFER write l


  'main game loop

  'to distribute the CPU load, the actions are sequenced depending the xnp number
  '320...296: draw a new pipe (pn) at right side
  '295...255: check bird collisions with pipe (pn-2)
  '254      : write number to pipe pn
  '253      : check progressive game speed
  '252...231: nothing happens here yet
  '230      : change xpn back to 320.

  Do
    'debug loop time
    'Text 0,0,Right$("00"+Str$(Timer,3,0),3)

    'player input through keyboard, clearing buffer, check loop time
    k$=""
    Do
      tmp$=read_input$()
      If tmp$<>"" Then k$=tmp$  'keep last valid key
    Loop Until Timer>h_beat
    Timer =0


    'determine new bird position and sprite depending player key entry
    if v=4 then play modsample swoosh,4 'speed dive sound
    v=v+1                       'falling speed increases
    If v>-2 Then spr=4          'flaps up
    If v>5 Then spr=17:v=5      'to a max of 5
    If k$=" " Then Play modsample wing,4:spr=5:v=-force  'flap down and you rise
    If yb+v<0 Or yb+v>196 Then
      If pn<3 Then pn=2
      you_die
    EndIf


    'add a new pipe at the right side of the screen
    Inc xnp,-1
    If xnp=(320-gap) Then 'we need a new post
      xnp=320                           'wrap around
      Inc pn,1                          'new pipe
      yy(0)=yy(1):yy(1)=ynp+8           'memory of pipe lenght
      ynp=8*Int(14*Rnd())               'new pipe length defined
      FRAMEBUFFER write n:Sprite scroll -1,0:FRAMEBUFFER write l'scroll background
    EndIf

    'check collision with pipe (pn-2) that is closest to the bird
    xx=xnp-2*gap
    If xx>76 And xx<=116 And pn>2 Then
      If yb+v<yy(0)+12 Or yb+v>yy(0)+pgap-12 Then you_die
    EndIf


    'number the new pipe
    If xnp=254 Then
      Box xnp+5,2,16,16,1,0,RGB(red)
      writenumber(xnp+8,5,pn)
    EndIf


    'progressive difficulty
    if xnp=253 then
      if pn>10 then force=8               'more vertical speed
      if pn>20 then h_beat=max(70-pn,30)  'faster forward
    end if


    'move screen
    FRAMEBUFFER wait
    Line 0,0,0,239,,RGB(magenta)  'erase old column line for line
    Sprite scroll -1,0            'scroll forground layer 1 to left

    'update bird
    Box xb-1,yb,16,12,1,RGB(magenta),RGB(magenta) 'kill old bird
    yb=yb+v                                       'new bird position
    Sprite write spr,xb,yb,0                      'write new bird

    'write new pipe, when 320>xpn>295
    If xnp>295 Then write_pipe(xnp,ynp)

  Loop


  'keyboard or controller input, can be expanded for controllers
Function read_input$()
  read_input$=Inkey$
End Function


  'write a number n to screen at position x,y
Sub writenumber(x,y,n)
  If n<10 Then                      'single digit number
    Sprite write n+7,x+2,y
  Else                              'write both digits
    Sprite write(n\10)+7,x,y        'tens
    Sprite write(n Mod 10)+7,x+5,y  'units
  EndIf
End Sub


  'create a pipe at xnp from top to ynp, and the antagonist
Sub write_pipe(x,n)
  Local i,j
  For i=0 To n Step 8               'top pipe
    Sprite write 1,x,i,0
  Next
  j=i
  Sprite write 3,x,j,0              'end piece top pipe
  For i=j+12+pgap To 200 Step 8
    Sprite write 1,x,i,0            'bottom pipe
  Next
  Sprite write 1,x,201,0            'bottom end of pipe
  Sprite write 2,x,j+pgap,0         'top end piece of bottom pipe
End Sub


  'end of game sequence
Sub you_die
  Box xb,yb,16,12,1,RGB(magenta),RGB(magenta) '  'kill old bird

  Play modsample hit,4
  animate_fall
  Play modsample die,4

  i=rangescores(pn-2) 'check if the score should give a medal

  Load image "gameover.bmp",70,30
  Load image "score.bmp",80,90
  writenumber(80+129,90+27,pn-2)          'your score this run
  writenumber(80+129,90+60,scores(3))     'all time top score for winning the cup
  if i>=0 then sprite write 18+i,100,120,0  'the medal/cup sprite

  Do : Loop While read_input$()=""
  Run
End Sub


  'drop the bird from collision point to the ground
Sub animate_fall
  Sprite framebuffer L,n,0,0,0,0,320,240,9  'copy L to N to avoid corruption
  CLS Rgb(magenta)                          'clear L

  v=1
  Do                'move the falling bird down in progressive speed
    yb=yb+v
    Sprite show 6,xb,yb,l,4   'falling bird, sprite show restores background magenta
    Pause 50
    v=Min(v+1,8)    'max falling speed is 8 pixels per 50ms
  Loop Until yb>196 'drop in the pavement

End Sub


  'gives medals at first 3 fixed levels in score file, highscore is adaptive (cup)
Function rangescores(n)
  rangescores=-1
  For i=0 To 3
    If scores(i)<n Then rangescores=i
  Next
  if rangescores=3 then scores(3)=n:savescore 'save new high, leave bronze/silver/gold same.
End Function


  'write the new score list to flash
Sub savescore
  'save new scores
  Open "score.txt" For output As #1
  For i=0 To 3:Print #1,Str$(scores(i)):Next
  Close #1
End Sub
