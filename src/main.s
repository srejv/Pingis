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


LatchController:
   LDA #$01
   STA $4016
   LDA #$00
   STA $4016       ; tell both the controllers to latch buttons


ReadA: 
   LDA $4016       ; player 1 - A
   AND #%00000001  ; only look at bit 0
   BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
   LDA $0203       ; load sprite X position
   CLC             ; make sure the carry flag is clear
   ADC #$01        ; A = A + 1
   STA $0203       ; save sprite X position
ReadADone:        ; handling this button is done
  

ReadB: 
   LDA $4016       ; player 1 - B
   AND #%00000001  ; only look at bit 0
   BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
   LDA $0203       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC #$01        ; A = A - 1
   STA $0203       ; save sprite X position
ReadBDone:        ; handling this button is done
   RTI
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
  jsr initgraphics
  jsr initinput
  jsr initsound
  
  ; Wait for the PPU to warm up (part 2 of 2)
vwait2:
  bit PPUSTATUS
  bpl vwait2

  ; Draw HELLO WORLD text
  jsr drawHelloWorld
  
  JSR drawPaddles

  ; Turn screen on
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  lda #VBLANK_NMI|BG_1000|OBJ_1000
  sta PPUCTRL
  lda #BG_ON|OBJ_ON
  sta PPUMASK


mainLoop:
   
   jmp mainLoop
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
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
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

.proc initinput
   rts
.endproc

.proc initsound
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

.proc drawPaddles

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
   .byt  $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
   .byt  $0F,$1C,$15,$14,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C

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

