;
; Based on Russian Roulette game for NES
; Copyright 2010 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty
; provided the copyright notice and this notice are preserved.
; This file is offered as-is, without any warranty.
;
; Modifications by Oscar Dragén 
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
   LDA #$00
   STA $2003       ; set the low byte (00) of the RAM address
   LDA #$02
   STA $4014       ; set the high byte (02) of the RAM address, start the transfer

   JSR   drawSprites
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
   NOP
   ;; osv
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



.proc reset
   SEI
  
  ; Acknowledge and disable interrupt sources during bootup
   LDX   #0
   STX   PPUCTRL    ; disable vblank NMI
   STX   PPUMASK    ; disable rendering (and rendering-triggered mapper IRQ
   LDA   #$40
   STA   $4017      ; disable frame IRQ
   STX   $4010      ; disable DPCM IRQ
   BIT   PPUSTATUS  ; ack vblank NMI
   BIT   $4015      ; ack DPCM IRQ
   CLD            ; disable decimal mode to help generic 6502 debuggers
   DEX            ; set up the stack
   TXS
  
  ; Wait for the PPU to warm up (part 1 of 2)
vwait1:
   BIT PPUSTATUS
   BPL vwait1



   ; While waiting for the PPU to finish warming up, we have about
   ; 29000 cycles to burn without touching the PPU.
   LDY #$00
   LDA #$F0
   LDX #$00
clear_zp:
   STY $00,x
   INX
   BNE   clear_zp

  ; the most basic sound engine possible
   LDA   #$0F
   STA   $4015
   ; Wait for the PPU to warm up (part 2 of 2)

vwait2:
   BIT   PPUSTATUS
   BPL   vwait2

   JSR   fillInitialData
   JSR   initgraphics

  ; prints some text on the screen. not
   JSR   drawText

  ; Turn screen on

   LDA   #0
   STA   PPUSCROLL
   STA   PPUSCROLL

   LDA   #VBLANK_NMI|BG_1000|OBJ_1000
   STA   PPUCTRL

   LDA   #BG_ON|OBJ_ON
   STA   PPUMASK

mainLoop:            ;;;;;;; MAIN LOOP
   JSR   check_input
   JSR   update_ball
   JSR   wait_nmi

   JMP   mainLoop
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
   BCC p2tryhit      ;;if ball x < right wall, still on screen, skip next section
   LDA #$00
   STA ballright
   LDA #$01
   STA ballleft         ;;bounce, ball now moving leftw
   ;;in real game, give point to player 1, reset ball
   JSR   player1Scores
   JMP   MoveBallRightDone
   
p2tryhit:
   ;if ballx is in exatcly the right position
   LDA   ballx
   CMP   #$E4
   BNE   MoveBallRightDone
   
   ; Check if  paddle top - 4  < bally ((A) < (Mem))
   ; if bally is greater than paddletop-4, continue
   LDA   paddle2top
   SEC
   SBC   #$08
   CMP   bally
   BCS   MoveBallRightDone
   ; maybe hit
   CLC
   ADC   #$08
   CMP   bally
   BCS   p2hittop
   CLC
   ADC   #$10
   CMP   bally
   BCS   p2hitmid
   CLC
   ADC   #$08
   CMP   bally
   BCS   p2hitbot
   JMP   MoveBallRightDone
   
p2hittop:
   LDA   #$01
   STA   ballleft
   STA   ballup
   LDA   #$00
   STA   ballright
   STA   balldown
   JMP   MoveBallRightDone
p2hitmid:
   LDA   #$01
   STA   ballleft
   LDA   #$00
   STA   ballup
   STA   balldown
   STA   ballright
   JMP   MoveBallRightDone
p2hitbot:
   LDA   #$01
   STA   ballleft
   STA   balldown
   LDA   #$00
   STA   ballright
   STA   ballup
  
MoveBallRightDone:

;MoveBallLeft:

   LDA   ballleft
   BEQ   MoveBallLeftDone

   LDA   ballx
   SEC
   SBC   ballspeedx
   STA   ballx

   ; check against player 1
   ; IF Y > paddle1bot
   ; IF Y < paddle1top
   ;  maybe hit
   LDA   ballx
   CMP   #LEFTWALL
   BCS   p1tryhit  ;; if ball x > left wall, still on screen, skip
   LDA   #$01
   STA   ballright
   LDA   #$00
   STA   ballleft
   ; hit left wall, point for player 2
   JSR   player2Scores
   JMP   MoveBallLeftDone
   
p1tryhit:
   ;if ballx is in exatcly the right position
   LDA   ballx
   CMP   #$10
   BNE   MoveBallLeftDone
   
   ; Check if  paddle top - 4  < bally ((A) < (Mem))
   ; if bally is greater than paddletop-4, continue
   LDA   paddle1top
   SEC
   SBC   #$04
   CMP   bally
   BCS   MoveBallLeftDone
   ; maybe hit
   CLC
   ADC   #$08
   CMP   bally
   BCS   p1hittop
   CLC
   ADC   #$10
   CMP   bally
   BCS   p1hitmid
   CLC
   ADC   #$08
   CMP   bally
   BCS   p1hitbot
   JMP   MoveBallLeftDone
   
p1hittop:
   LDA   #$01
   STA   ballright
   STA   ballup
   LDA   #$00
   STA   ballleft
   STA   balldown
   JMP   MoveBallLeftDone
p1hitmid:
   LDA   #$01
   STA   ballright
   LDA   #$00
   STA   ballup
   STA   balldown
   STA   ballleft
   JMP   MoveBallLeftDone
p1hitbot:
   LDA   #$01
   STA   ballright
   STA   balldown
   LDA   #$00
   STA   ballleft
   STA   ballup

MoveBallLeftDone:

;MoveBallUp
   LDA   ballup
   BEQ   MoveBallUpDone
   
   LDA   bally
   SEC
   SBC   ballspeedy
   STA   bally

   LDA   bally
   CMP   #TOPWALL
   BCS   MoveBallUpDone  ;; if ball x > left wall, still on screen, skip
   LDA   #$01
   STA   balldown
   LDA   #$00
   STA   ballup
MoveBallUpDone:

; MoveBallDown
   LDA   balldown
   BEQ   MoveBallDownDone
   
   LDA   bally
   CLC
   ADC   ballspeedy
   STA   bally

   LDA   bally
   CMP   #BOTTOMWALL
   BCC   MoveBallDownDone  ;; if ball x > left wall, still on screen, skip
   LDA   #$01
   STA   ballup
   LDA   #$00
   STA   balldown
MoveBallDownDone:
updateball_end:   
   RTS
.endproc

.proc resetball

   LDA   #$80
   STA   ballx
   STA   bally
   RTS
.endproc

.proc player1Scores

   INC   score1
   JSR   resetball
   RTS
.endproc



.proc player2Scores

   INC   score2
   JSR   resetball
   RTS
.endproc



.proc check_input
; Player 1 UP
   LDA   buttons1 ; some error? :S
   AND   #KEY_UP
   BEQ   movePaddle1UpDone

   LDA   paddle1top
   CMP   #TOPWALL
   BCC   movePaddle1UpDone
   
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
   CMP   #$00
   BEQ   movePaddle1DownDone

   LDA   paddle1bot
   CLC
   ADC   #$08
   CMP   #BOTTOMWALL
   BCS   movePaddle1DownDone
   
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
   CMP   #TOPWALL
   BCC   movePaddle2UpDone
   
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
   
   LDA   paddle2bot
   CLC
   ADC   #$08
   CMP   #BOTTOMWALL
   BCS   movePaddle2DownDone
  
   LDA   paddle2top
   CLC
   ADC   playerspeed
   STA   paddle2top
   CLC
   ADC   #$08
   STA   paddle2mid
   CLC
   ADC   #$08
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
   RTS
.endproc

.proc cls
  LDA #VBLANK_NMI
  STA PPUCTRL
  LDA #$20
  LDX #$00
  STA PPUMASK
  STA PPUADDR
  STA PPUADDR
  LDX #240
:
  STA PPUDATA
  STA PPUDATA
  STA PPUDATA
  STA PPUDATA
  DEX
  BNE :-
  LDX #64
  LDA #0
:
  STA PPUDATA
  DEX
  BNE :-
  RTS
.endproc

.proc drawText
  JSR cls
  ; set monochrome palette
  LDA #$3F
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDX #8
:
  LDA #$17
  STA PPUDATA
  lda #$38
  STA PPUDATA
  STA PPUDATA
  STA PPUDATA
  DEX
  BNE :-
  
  ; load source and dest for press start
  LDA #>pressStart
  STA 1
  LDA #<pressStart
  STA 0
  LDA #$22
  STA 3
  LDA #$6A
  STA 2
  JSR printMsg

  ; load source and destination addresses
  LDA #>helloWorld
  STA 1
  LDA #<helloWorld
  STA 0
  LDA #$20     ; startminnesaddress hög
  STA 3
  LDA #$62     ; startminnesaddress låg
  STA 2
  ; fall through
.endproc

.proc printMsg
dstLo = 2
dstHi = 3
src = 0
  LDA dstHi
  STA PPUADDR
  LDA dstLo
  STA PPUADDR
  LDY #0
loop:
  LDA (src),y
  BEQ done
  INY
  BNE :+
  INC src+1
:
  CMP #10
  BEQ newline
  STA PPUDATA
  BNE loop
newline:
  LDA #32
  CLC
  ADC dstLo
  STA dstLo
  LDA #0
  ADC dstHi
  STA dstHi
  STA PPUADDR
  LDA dstLo
  STA PPUADDR
  JMP loop
done:
  RTS
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
   .byt  $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
   .byt  $0F,$1C,$15,$14,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C

sprites:
   .byt  $80,$02,$00,$80   ; ball?
   .byt  $30,$04,$00,$10   ; paddle1 top
   .byt  $38,$05,$00,$10   ; paddle1 mid
   .byt  $40,$03,$00,$10   ; paddle1 bot
   .byt  $80,$04,$00,$E0   ; paddle2 top
   .byt  $88,$05,$00,$E0
   .byt  $90,$03,$00,$E0   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; paddle2 bot

