  'flappy bird for picomiteVGA
  
  'versions
  ' flappy7   first release
  ' flappy8   fixed collision bug, screen corruption in fall
  ' flappy9   fl_day2 background and pipe/animation adaptions
  ' flappy10  sfx added, end game added, fl_day6 background, store scores
  ' flappy11  uses graphics in flash slot in preparation of LCD
  
  
  'setup memory and screen
  Option DEFAULT integer
  MODE 2
  FRAMEBUFFER layer 9 'mageta = transparent
  
  
  'load graphics element index and prepare flash data
  
  fl_adr=mm.info(flash address 3) 'load flash start adress
  'flash slot #3 has the exact same start address as the library
  flash disk load 3,"gfx/gfx.bin",o
  
  open "gfx/gfx_index.txt" for input as #1
  dim indx(26):for i=0 to 25:input #1,a$:indx(i)=val(a$)+fl_adr:next
  close #1
  
  
  'load background on to layer N
  FRAMEBUFFER write n
  Load image "gfx/fl_dayc.bmp"
  
  
  'game defines
  h_beat=50     'speed of the game
  gap=90        'between columns
  xb=100        'bird start position (constant)
  yb=100        'bird vertical position
  v=1           'falling speed due to gravity
  xnp=320       'new pipe position
  ynp=40        'new pipe length (default)
  pgap=80       'gap between top and bottom pites
  spr=4         'default bird sprite
  dim yy(1)     'pipe height last 2 pipes
  pn=1          'pipe number to be shifted in right side
  die=16:hit=17:swoosh=18:wing=19   'sfx samples
  dim scores(3)=(0,5,10,15) 'default highscore list
  dim medal$(3)=("no_","b","s","g") 'prefix names for medal graphics
  
  
  'get old scores from flash
  open "score.txt" for input as #1
  for i=1 to 3:input #1,a$:scores(i)=val(a$):next
  close #1
  
  
  'title screen
  framebuffer write l:cls
  Sprite memory indx(17),70,50 'title
  pause 3000
  
  
  'create start instructions
  cls
  Sprite memory indx(18),70,40 'get ready
  Sprite memory indx(19),130,110 'tap tap
  do : loop while read_input$()=""
  cls
  
  
  'init for main game loop
  timer=0
  play modfile "sfx/sll3sfx2.mod"
  FRAMEBUFFER write l
  
  
  'main game loop
  
  'to distribute the CPU load, the actions are sequenced depending the xnp number
  '320...296: draw a new pipe (pn) at right side
  '295...255: check bird collisions with pipe (pn-2)
  '254      : write number to pipe pn
  '253...231: nothing happens here yet
  '230      : change xpn back to 320.
  
  Do
    'debug loop time
    Text 0,0,Right$("00"+Str$(timer,3,0),3)
    
    'player input through keyboard, clearing buffer, check loop time
    k$=""
    Do
      tmp$=read_input$()
      If tmp$<>"" Then k$=tmp$  'keep last valid key
    Loop Until Timer>h_beat
    Timer=0
    
    
    'determine new bird position and sprite depending player key entry
    v=v+1                       'falling speed increases
    if v>-2 then spr=4          'flaps up
    if v>5 then spr=17:v=5      'to a max of 5
    if k$=" " then play modsample wing,4:spr=5:v=-7  '-8 flaps down and you rise
    if yb+v<0 or yb+v>196 then
      if pn<3 then pn=2
      you_die
    end if
    
    
    'add a new pipe at the right side of the screen
    inc xnp,-1
    if xnp=(320-gap) then 'we need a new post
      xnp=320                           'wrap around
      inc pn,1                          'new pipe
      yy(0)=yy(1):yy(1)=ynp+8           'memory of pipe lenght
      ynp=8*int(14*rnd())               'new pipe length defined
      'framebuffer write n:sprite scroll -1,0:framebuffer write l 'eye candy
    end if
    
    'check collision with pipe (pn-2) that is closest to the bird
    xx=xnp-2*gap
    if xx>76 and xx<=116 and pn>2 then
      if yb+v<yy(0)+12 or yb+v>yy(0)+pgap-12 then you_die
    end if
    
    
    'number the new pipe
    if xnp=254 then
      box xnp+5,2,16,16,1,0,rgb(red)
      writenumber(xnp+8,5,pn)
    end if
    
    
    'move screen
    framebuffer wait
    line 0,0,0,239,,rgb(magenta)  'erase old column line for line
    Sprite scroll -1,0            'scroll forground layer 1 to left
    
    'update bird
    box xb-1,yb,16,12,1,rgb(magenta),rgb(magenta) 'kill old bird
    yb=yb+v                                       'new bird position
    Sprite memory indx(spr-1),xb,yb               'write new bird
    
    'write new pipe, when 320>xpn>295
    if xnp>295 then write_pipe(xnp,ynp)
    
  Loop
  
  
  'keyboard or controller input, can be expanded for controllers
function read_input$()
  read_input$=inkey$
end function
  
  
  'write a number n to screen at position x,y
sub writenumber (x,y,n)
  if n<10 then
    Sprite memory indx(n+6),x+2,y,0    'single digit
  else
    Sprite memory indx(n\10+6),x,y,0    'first digit
    Sprite memory indx((n mod 10)+6),x+5,y,0 'second digit
  end if
end sub
  
  
  'create a pipe at xnp from top to ynp, and the antagonist
sub write_pipe(x,n)
  local i,j
  for i=0 to n step 8
    Sprite memory indx(0),x,i 'top pipe
  next
  j=i
  Sprite memory indx(2),x,j 'top fitting
  for i=j+12+pgap to 200 step 8
    Sprite memory indx(0),x,i 'bottom pipe
  next
  Sprite memory indx(0),x,201 'last part bottom pipe
  Sprite memory indx(1),x,j+pgap 'bottom fitting
end sub
  
  
  'end of game sequence
sub you_die
  box xb,yb,16,12,1,rgb(magenta),rgb(magenta) '  'kill old bird (non-smart)
  
  play modsample hit,4
  animate_fall
  play modsample die,4
  
  i=arrangescores(pn-2)
  
  Sprite memory indx(20),70,30 'gameover
  Sprite memory indx(21),80,90 'scoreboard
  writenumber(80+129,90+27,pn-2)
  writenumber(80+129,90+60,scores(3))
  Sprite memory indx(22+i),100,120 'title
  
  do : loop while read_input$()=""
  run
end sub
  
  
  'drop the bird from collision point to the ground
sub animate_fall
  sprite framebuffer L,N,0,0,0,0,320,240,9  'copy L to N to avoid corruption
  cls                                       'clear L
  v=1
  do 
    box xb,yb,12,16,1,rgb(magenta),rgb(magenta)   'kill old bird
    yb=yb+v
    Sprite memory indx(5),xb,yb 'dropping bird
    pause 50
    v=min(v+1,8)
  loop until yb>196
end sub
  
  
  'sort the scores, and return current score position in the list
function arrangescores(n)
  scores(0)=n
  sort scores(),,0  'sort in ascending order
  for i=0 to 3
    if scores(i)=n then arrangescores=i
  next
  savescore
end function
  
  
  'write the new score list to flash
sub savescore
  'save new scores
  open "score.txt" for output as #1
  for i=1 to 3:print #1,str$(scores(i)):next
  close #1
end sub
  
  
  
  
  
  
  
