;
; Russian Roulette game for NES
; Copyright 2010 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty
; provided the copyright notice and this notice are preserved.
; This file is offered as-is, without any warranty.
;
.include "nes.h"
.p02

.segment "ZEROPAGE"
ballx: .res  1
bally: .res  1
ballup:  .res  1
balldown:   .res  1
ballleft:   .res  1
ballright:  .res  1
score1:   .res  1  ;score for player 1
score2:   .res  1  ;score for player 2
;  Ctrl data: |A|B|SELECT|START|UP|DOWN|LEFT|RIGHT
buttons1: .res  1  ;controller data for player 1 
buttons2: .res  1  ; controller data for player 2
paddle1top:  .res  1  ; Paddle1Top
paddle1mid:  .res  1  ; Paddle1Mid
paddle1bot:  .res  1  ; Paddle1Bot
paddle2top:  .res  1  ; Paddle2Top
paddle2mid:  .res  1  ; Paddle2Mid
paddle2bot:  .res  1  ;  Paddle2Bot
nmi_count:   .res  1
ballspeedx:  .res  1
ballspeedy: .res  1
playerspeed: .res 1  

.segment "INESHDR"
  .byt "NES",$1A
  .byt 1  ; 16 KiB PRG ROM
  .byt 1  ; 8 KiB CHR ROM
  .byt 1  ; vertical mirroring; low mapper nibble: 0
  .byt 0  ; high mapper nibble: 0

.segment "VECTORS"
  .addr nmi, reset, irq

.segment "CODE"

; we don't use irqs yet
.proc irq
   rti
.endproc

.proc nmi

   JSR   readControllers
   JSR   drawSprites
   
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

.proc fillInitialData
   ; set stuff to zero
   LDA   #$00
   STA   ballup
   STA   ballleft
   STA   score1
   STA   score2
   STA   buttons1
   STA   buttons2
   
   LDA   #$04
   STA   ballspeedx
   STA   ballspeedy
   STA   playerspeed
   
   
   LDA   #$40
   STA   bally
   LDA   #$80
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

   ;; osv
   rts
.endproc

;;; Update sprite positions
.proc drawSprites
   LDA #$00
   STA $2003  ; set the low byte (00) of the RAM address
   LDA #$02
   STA $4014  ; set the high byte (02) of the RAM address, start the transfer

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
   
   LDA   paddle2top  ; load Paddle position (hopefully updated in main loop)
   STA   $0210       ; set position Y
   LDA   paddle2mid 
   STA   $0214
   LDA   paddle2bot 
   STA   $0218
   
   RTS
.endproc

.proc reset
  sei

  ; Acknowledge and disable interrupt sources during bootup
  ldx #0
  stx PPUCTRL    ; disable vblank NMI
  stx PPUMASK    ; disable rendering (and rendering-triggered mapper IRQ)
  lda #$40
  sta $4017      ; disable frame IRQ
  stx $4010      ; disable DPCM IRQ
  bit PPUSTATUS  ; ack vblank NMI
  bit $4015      ; ack DPCM IRQ
  cld            ; disable decimal mode to help generic 6502 debuggers
                 ; http://magweasel.com/2009/08/29/hidden-messagin/
  dex            ; set up the stack
  txs

  ; Wait for the PPU to warm up (part 1 of 2)
vwait1:
  bit PPUSTATUS
  bpl vwait1

  ; While waiting for the PPU to finish warming up, we have about
  ; 29000 cycles to burn without touching the PPU.  So we have time
  ; to initialize some of RAM to known values.
  ; Ordinarily the "new game" initializes everything that the game
  ; itself needs, so we'll just do zero page and shadow OAM.
  ldy #$00
  lda #$F0
  ldx #$00
clear_zp:
  sty $00,x
  inx
  bne clear_zp
  ; the most basic sound engine possible
  lda #$0F
  sta $4015
  
  JSR fillInitialData
  jsr initgraphics

  
  ; Wait for the PPU to warm up (part 2 of 2)
vwait2:
  bit PPUSTATUS
  bpl vwait2

  ; Draw HELLO WORLD text
  jsr drawHelloWorld

  ; Turn screen on
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  lda #VBLANK_NMI|BG_1000|OBJ_1000
  sta PPUCTRL
  lda #BG_ON|OBJ_ON
  sta PPUMASK


mainLoop:            ;;;;;;; MAIN LOOP

   ;JSR   check_input
   ;JSR   update_ball
   JSR   wait_nmi
   
   jmp   mainLoop
.endproc

.proc update_ball
   ; Move ball
;MoveBallRight:
   LDA   ballright
   BEQ   MoveBallRightDone   ;;if ballright=0, skip this section

   LDA ballx
   CLC
   ADC ballspeedx        ;;ballx position = ballx + ballspeedx
   STA ballx

   LDA ballx
   CMP #RIGHTWALL
   BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
   LDA #$00
   STA ballright
   LDA #$01
   STA ballleft         ;;bounce, ball now moving left
   ;;in real game, give point to player 1, reset ball
MoveBallRightDone:

;MoveBallLeft:
   LDA   ballleft
   BEQ   MoveBallLeftDone
   
   LDA   ballx
   SEC
   SBC   ballspeedx
   STA   ballx

   LDA   ballx
   CMP   #LEFTWALL
   BCS   MoveBallLeftDone  ;; if ball x > left wall, still on screen, skip
   LDA   #$01
   STA   ballright
   LDA   #$00
   STA   ballleft
   ; hit left wall, point for player 2
MoveBallLeftDone:

updateball_end:   
   RTS
.endproc

.proc resetball
   LDA   $80
   STA   ballx
   LDA   $40
   STA   bally
   rts
.endproc

.proc player1Scores
   inc   score1
   JSR   resetball
   rts
.endproc

.proc player2Scores
   INC   score2
   JSR   resetball
   RTS
.endproc

.proc check_input
; Player 1 UP
   LDA   buttons1
   AND   #KEY_UP
   BEQ   movePaddle1UpDone
   
   LDA   paddle1top
   SEC
   SBC   playerspeed
   STA   paddle1top
   LDA   paddle1mid
   SEC
   SBC   playerspeed
   STA   paddle1mid
   LDA   paddle1bot
   SEC
   SBC   playerspeed
   STA   paddle1bot
movePaddle1UpDone:
; Player 1 DOWN
   LDA   buttons1
   AND   #KEY_DOWN
   BEQ   movePaddle1DownDone
   
   LDA   paddle1top
   CLC
   ADC   playerspeed
   STA   paddle1top
   LDA   paddle1mid
   CLC
   ADC   playerspeed
   STA   paddle1mid
   LDA   paddle1bot
   CLC
   ADC   playerspeed
   STA   paddle1bot
movePaddle1DownDone:
; Player 2 UP
   LDA   buttons2
   AND   #KEY_UP
   BEQ   movePaddle2UpDone
   
   LDA   paddle2top
   SEC
   SBC   playerspeed
   STA   paddle2top
   LDA   paddle2mid
   SEC
   SBC   playerspeed
   STA   paddle2mid
   LDA   paddle2bot
   SEC
   SBC   playerspeed
   STA   paddle2bot
movePaddle2UpDone:
; PLAYER 2 DOWN
   LDA   buttons2
   AND   #KEY_DOWN
   BEQ   movePaddle2DownDone
   
   LDA   paddle2top
   CLC
   ADC   playerspeed
   STA   paddle2top
   LDA   paddle2mid
   CLC
   ADC   playerspeed
   STA   paddle2mid
   LDA   paddle2bot
   CLC
   ADC   playerspeed
   STA   paddle2bot
movePaddle2DownDone:
.endproc

.proc wait_nmi
   LDA   nmi_count
   loop: CMP nmi_count
   BEQ   loop
   RTS
.endproc


.proc initgraphics
LoadPalettes:
   LDA $2002             ; read PPU status to reset the high/low latch
   LDA #$3F
   STA $2006             ; write the high byte of $3F00 address
   LDA #$00
   STA $2006             ; write the low byte of $3F00 address
   LDX #$00              ; start out at 0
LoadPalettesLoop:
   LDA palette, x        ; load data from address (palette + the value in x)
   STA $2007             ; write to PPU
   INX                   ; X = X + 1
   CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
   BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down


LoadSprites:
   LDX #$00              ; start at 0
LoadSpritesLoop:
   LDA sprites, x        ; load data from address (sprites +  x)
   STA $0200, x          ; store into RAM address ($0200 + x)
   INX                   ; X = X + 1
   CPX #$20              ; Compare X to hex $20, decimal 32
   BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
   rts
.endproc


.proc cls
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$20
  ldx #$00
  stx PPUMASK
  sta PPUADDR
  stx PPUADDR
  ldx #240
:
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  dex
  bne :-
  ldx #64
  lda #0
:
  sta PPUDATA
  dex
  bne :-
  rts
.endproc

.proc drawHelloWorld
  jsr cls

  ; set monochrome palette
  lda #$3F
  sta PPUADDR
  lda #$00
  sta PPUADDR
  ldx #8
:
  lda #$17
  sta PPUDATA
  lda #$38
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  dex
  bne :-
  
  ; load source and dest for press start
  lda #>pressStart
  sta 1
  lda #<pressStart
  sta 0
  lda #$22
  sta 3
  lda #$6A
  sta 2
  JSR printMsg

  ; load source and destination addresses
  lda #>helloWorld
  sta 1
  lda #<helloWorld
  sta 0
  lda #$20     ; startminnesaddress hög
  sta 3
  lda #$62     ; startminnesaddress låg
  sta 2
  ; fall through
.endproc
.proc printMsg
dstLo = 2
dstHi = 3
src = 0
  lda dstHi
  sta PPUADDR
  lda dstLo
  sta PPUADDR
  ldy #0
loop:
  lda (src),y
  beq done
  iny
  bne :+
  inc src+1
:
  cmp #10
  beq newline
  sta PPUDATA
  bne loop
newline:
  lda #32
  clc
  adc dstLo
  sta dstLo
  lda #0
  adc dstHi
  sta dstHi
  sta PPUADDR
  lda dstLo
  sta PPUADDR
  jmp loop
done:
  rts
.endproc

.segment "RODATA"
helloWorld:
  .byt "pingis! HURRA!",0
pressStart:
   .byt "Press Start",0
onPlayer:
   .byt "1 Player",0
twoPlayers:
   .byt "2 Players",0
palette:
   .byt  $20,$10,$00,$0D,$20,$10,$00,$0D,$20,$10,$00,$0D,$20,$10,$00,$0D
   .byt  $20,$10,$00,$0D,$20,$10,$00,$0D,$20,$10,$00,$0D,$20,$10,$00,$0D

sprites:
   .byt  $80,$02,$00,$80   ; ball?
   .byt  $60,$04,$00,$10   ; paddle1 top
   .byt  $68,$05,$00,$10   ; paddle1 mid
   .byt  $70,$03,$00,$10   ; paddle1 bot
   .byt  $80,$04,$00,$E0   ; paddle2 top
   .byt  $88,$05,$00,$E0
   .byt  $90,$03,$00,$E0   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; paddle2 bot

