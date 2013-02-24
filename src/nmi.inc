
.proc nmi
   LDA #$00
   STA $2003       ; set the low byte (00) of the RAM address
   LDA #$02
   STA $4014       ; set the high byte (02) of the RAM address, start the transfer

   LDA   gamestate
   CMP   #PLAYINGSCREEN
   BNE   nmiReadControllers
   JSR   drawSprites
   
nmiReadControllers:
   JSR   readControllers

   INC   nmi_count
   RTI
.endproc



.proc readControllers
   LDA   #$01
   STA   $4016
   LDA   #$00
   STA   $4016
   LDX   #$08
ReadController1Loop:
   LDA   $4016
   LSR   A           ; bit0 -> Carry
   ROL   buttons1     ; bit0 <- Carry
   DEX
   BNE   ReadController1Loop
   LDX   #$08

ReadController2Loop:
   LDA   $4017
   LSR   A           ; bit0 -> Carry
   ROL   buttons2     ; bit0 <- Carry
   DEX
   BNE   ReadController2Loop
   RTS  
.endproc

;;; Update sprite positions
.proc drawSprites
   LDA   bally
   STA   $0200
   LDA   ballx
   STA   $0203

   LDA   paddle1top  ; load Paddle position (hopefully updated in main loop)
   STA   $0204       ; save sprite Y position
   LDA   paddle1mid  
   STA   $0208
   LDA   paddle1bot
   STA   $020C
   LDA   #PADDLE1X
   STA   $0207
   STA   $020B
   STA   $020F

   LDA   paddle2top  ; load Paddle position (hopefully updated in main loop)
   STA   $0210       ; set position Y
   LDA   paddle2mid 
   STA   $0214
   LDA   paddle2bot 
   STA   $0218
   LDA   #PADDLE2X
   STA   $0213
   STA   $0217
   STA   $021B
   RTS
.endproc

.proc wait_nmi
   LDA   nmi_count
   loop: CMP nmi_count
   BEQ   loop
   RTS
 .endproc
 