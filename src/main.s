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
score1lo:   .res  1  ;score for player 1
score1hi:   .res  1
score2lo:   .res  1  ;score for player 2
score2hi:   .res  1
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
gamestate:  .res  1



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

;;;;;;;;;;;;;;;;;;;;; NMI -
.include "nmi.inc"


;;;;;;;;;;;;;;;;;;;;; INITIALIZATION
.include "init.inc"

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
   JSR   cls   ; clear screen of text
   JSR   setMonochromePalette
   JSR   drawTitleScreen
   
  ; Turn screen on

   LDA   #0
   STA   PPUSCROLL
   STA   PPUSCROLL

   LDA   #VBLANK_NMI|BG_1000|OBJ_1000
   STA   PPUCTRL

   LDA   #BG_ON|OBJ_ON
   STA   PPUMASK

mainLoop:            ;;;;;;; MAIN LOOP
   LDA   gamestate
   CMP   #PLAYINGSCREEN
   BEQ   mainPlayingState
   CMP   #TITLESCREEN
   BEQ   mainTitleState
   CMP   #GAMEOVERSCREEN
   BEQ   mainGameOverState
   LDA   #TITLESCREEN
   STA   gamestate
   JMP   mainLoopEnd

mainTitleState:
   JSR   check_input_title
   JMP   mainLoopEnd
   
mainGameOverState:
   JSR   check_input_gameOver
   JMP   mainLoopEnd
   
mainPlayingState:
   JSR   check_input_game
   JSR   update_ball
   
mainLoopEnd:
   JSR   wait_nmi
   JMP   mainLoop
.endproc


.proc check_input_title
   LDA   buttons1 ; some error? :S
   AND   #KEY_START
   BNE   yeah
   LDA   buttons2
   AND   #KEY_START
   BEQ   nope
yeah:
   LDA   #PLAYINGSCREEN
   STA   gamestate
   JSR   drawGameScreen
nope:
   RTS
.endproc

.proc check_input_gameOver
   LDA   buttons1 ; some error? :S
   AND   #KEY_START
   BNE   yeaha
   LDA   buttons2
   AND   #KEY_START
   BEQ   nopea
yeaha:
   LDA   #TITLESCREEN
   STA   gamestate
nopea:
   RTS
.endproc

.include "ball.inc"

.proc player1Scores

   LDA   #$0A
   INC   score1lo
   CMP   score1lo
   BNE   player1ScoresDone
   LDA   #$00
   STA   score1lo
   INC   score1hi
player1ScoresDone:

   ; check win
   LDA   score1hi
   CMP   #$01
   BNE   p1resball
   LDA   score1lo
   CMP   #$05
   BNE   p1resball
   JSR   drawPlayer1won
   JSR   drawGameOver
   LDA   #GAMEOVERSCREEN
   STA   gamestate
   
p1resball:
   JSR   resetball
   RTS
.endproc


.proc player2Scores

   LDA   #$0A
   INC   score2lo
   CMP   score2lo
   BNE   player2ScoresDone
   LDA   #$00
   STA   score2lo
   INC   score2hi
player2ScoresDone:

   ; check win
   LDA   score2hi
   CMP   #$01
   BNE   p2resball
   LDA   score2lo
   CMP   #$05
   BNE   p2resball
   JSR   drawPlayer1won
   JSR   drawGameOver
   LDA   #GAMEOVERSCREEN
   STA   gamestate
   
p2resball:
   JSR   resetball
   RTS
.endproc


.proc check_input_game
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

   RTS
.endproc

.proc cls
   LDA   #VBLANK_NMI
   STA   PPUCTRL
   LDA   #$20
   LDX   #$00
   STA   PPUMASK
   STA   PPUADDR
   STA   PPUADDR
   LDX   #240
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


.proc setMonochromePalette
   ;  set monochrome palette
   LDA   #$3F
   STA   PPUADDR
   LDA   #$00
   STA   PPUADDR
   LDX   #8
:
   LDA   #$17
   STA   PPUDATA
   lda   #$38
   STA   PPUDATA
   STA   PPUDATA
   STA   PPUDATA
   DEX   
   BNE   :-
   RTS
.endproc

.proc clear
   LDA   #>clearAscii
   STA   1
   LDA   #<clearAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$6C
   STA   2
   JSR   printMsg
      ; Credits
   LDA   #>clearAscii
   STA   1
   LDA   #<clearAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$E8
   STA   2
   JSR   printMsg
   
   ; Press Start
   LDA   #>clearAscii
   STA   1
   LDA   #<clearAscii
   LDA   0
   LDA   #$22
   STA   3
   LDA   #$08
   JSR   printMsg
   
   ; Game over
   LDA   #>clearAscii
   STA   1
   LDA   #<clearAscii
   STA   0
   LDA   #$22
   STA   3
   LDA   #$0C
   STA   2
   JSR   printMsg

   ; Player 2 won
   LDA   #>clearAscii
   STA   1
   LDA   #<clearAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$EA
   STA   2
   JSR   printMsg
   RTS
.endproc
.proc drawGameOver
   ; Game Over
   LDA   #>gameOverAscii
   STA   1
   LDA   #<gameOverAscii
   STA   0
   LDA   #$22
   STA   3
   LDA   #$0C
   STA   2
   JSR   printMsg
   RTS
.endproc
.proc drawPlayer1won
   ; Player 1 won
   LDA   #>player1winAscii
   STA   1
   LDA   #<player1winAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$EA
   STA   2
   JSR   printMsg
   RTS
.endproc
.proc drawPlayer2won
   ; Player 2 won
   LDA   #>player2winAscii
   STA   1
   LDA   #<player2winAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$EA
   STA   2
   JSR   printMsg
   RTS
.endproc

.proc drawTitleScreen
   JSR clear
   
   ; Pingis
   LDA   #>titleAscii
   STA   1
   LDA   #<titleAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$6C
   STA   2
   JSR   printMsg
   
   ; Credits
   LDA   #>creditsAscii
   STA   1
   LDA   #<creditsAscii
   STA   0
   LDA   #$20
   STA   3
   LDA   #$E8
   STA   2
   JSR   printMsg
   
   ; Press Start
   LDA   #>pressStartAscii
   STA   1
   LDA   #<pressStartAscii
   LDA   0
   LDA   #$22
   STA   3
   LDA   #$08
   JSR   printMsg
   
   RTS
.endproc

.proc drawGameScreen
   JSR clear

   RTS
.endproc

;.proc drawText
;   JSR cls
   ; set monochrome palette
 ;  JSR setMonochromePalette;
 ; 
   ; load source and dest for press start
;   LDA #>pressStart
;   STA 1
;   LDA #<pressStart
;   STA 0
;   LDA #$22    ; startminnesaddress hög ( från där man börjar rita)
;   STA 3
;   LDA #$6A    ; startminnesaddress låg ( från där man börjar rita)
;   STA 2
;   JSR printMsg

  ; load source and destination addresses
;  LDA #>helloWorld
;  STA 1
;  LDA #<helloWorld
;;  STA 0
;  LDA #$20     ; startminnesaddress hög
;  STA 3
;  LDA #$62     ; startminnesaddress låg
;  STA 2
;  ; fall through
;.endproc

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
titleAscii:       .byt "Pingis!!",0
creditsAscii:     .byt "By: Oscar Dragen",0
pressStartAscii:  .byt "Press Start",0
onPlayer:         .byt "1 Player",0
twoPlayers:       .byt "2 Players",0
player1winAscii:  .byt "PLAYER 1 WINS!",0
player2winAscii:  .byt "PLAYER 2 WINS!",0
gameOverAscii:    .byt "GAME OVER",0
clearAscii:       .byt "                ",0

palette:
   .byt  $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
   .byt  $0F,$1C,$15,$14,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C

sprites:
   .byt  $80,$02,$00,$80   ; ball?
   .byt  $30,$04,$00,$10   ; paddle1 top
   .byt  $38,$05,$00,$10   ; paddle1 mid
   .byt  $40,$03,$00,$10   ; paddle1 bot
   .byt  $80,$04,$00,$E0   ; paddle2 top
   .byt  $88,$05,$00,$E0   ; paddle2 mid
   .byt  $90,$03,$00,$E0   ; paddle2 bot
   .byt  $F0,$02,$00,$88   ; misc
   .byt  $F0,$02,$00,$88   ; misc
   .byt  $F0,$02,$00,$88   ; misc

