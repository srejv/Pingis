.proc fillInitialData
   ; set stuff to zero

   LDA   #$00
   STA   ballup
   STA   ballleft
   STA   score1lo
   STA   score1hi
   STA   score2lo
   STA   score2hi
   STA   buttons1
   STA   buttons2

   LDA   #$04
   STA   ballspeedx
   STA   ballspeedy
   STA   playerspeed

   LDA   #$40
   STA   bally
   LDA   #$DB
   STA   ballx
   
   LDA   #$01
   STA   balldown
   STA   ballright
   
   ; load Paddle initial position (hopefully updated in main loop)

   LDA   #$40  
   STA   paddle1top
   STA   paddle2top 
   LDA   #$48
   STA   paddle1mid
   STA   paddle2mid
   LDA   #$50
   STA   paddle1bot
   STA   paddle2bot
   
   LDA   #TITLESCREEN
   STA   gamestate
   NOP
   ;; osv
   RTS
.endproc

.proc initgraphics

LoadPalettes:
   LDA   $2002             ; read PPU status to reset the high/low latch
   LDA   #$3F
   STA   $2006             ; write the high byte of $3F00 address
   LDA   #$00
   STA   $2006             ; write the low byte of $3F00 address
   LDX   #$00              ; start out at 0
LoadPalettesLoop:
   LDA   palette, x        ; load data from address (palette + the value in x)
   STA   $2007             ; write to PPU
   INX                   ; X = X + 1
   CPX   #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
   BNE   LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
LoadSprites:
   LDX   #$00              ; start at 0
LoadSpritesLoop:
   LDA   sprites, x        ; load data from address (sprites +  x)
   STA   $0200, x          ; store into RAM address ($0200 + x)
   INX                   ; X = X + 1
   CPX   #$20              ; Compare X to hex $20, decimal 32
   BNE   LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
   RTS
.endproc