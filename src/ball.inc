.proc update_ball

   ; Move ball

;MoveBallRight:
   LDA   ballright
   BEQ   MoveBallRightDone   ;;if ballright=0, skip this section

   LDA   ballx
   CLC
   ADC   ballspeedx        ;;ballx position = ballx + ballspeedx
   STA   ballx

   LDA   ballx
   CMP   #RIGHTWALL
   BCC   p2tryhit      ;;if ball x < right wall, still on screen, skip next section
   LDA   #$00
   STA   ballright
   LDA   #$01
   STA   ballleft         ;;bounce, ball now moving leftw
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
