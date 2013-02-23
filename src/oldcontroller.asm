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
   LDA $0203  ; load sprite X position
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
ReadSelect:
   LDA   $4016
ReadStart:
   LDA   $4016
ReadUp:
   LDA   $4016
   AND   #%00000001  ; only look at bit 0
   BEQ   ReadUpDone   ; branch to ReadBDone if button is NOT pressed (0)
   
   LDA   $0210       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC   #$01        ; A = A - 1
   STA   $0210       ; save sprite X position
   
   LDA   $0214       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC   #$01        ; A = A - 1
   STA   $0214       ; save sprite X position
   
   LDA   $0218       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC   #$01        ; A = A - 1
   STA   $0218       ; save sprite X position
ReadUpDone:

ReadDown:
   LDA   $4016
   AND   #%00000001  ; only look at bit 0
   BEQ   ReadDownDone   ; branch to ReadBDone if button is NOT pressed (0)
   
   LDA   $0204       ; load sprite X position
   CLC             ; make sure carry flag is set
   ADC   #$01        ; A = A - 1
   STA   $0204       ; save sprite X position
   
   LDA   $0208       ; load sprite X position
   CLC             ; make sure carry flag is set
   ADC   #$01        ; A = A - 1
   STA   $0208       ; save sprite X position
   
   LDA   $020C       ; load sprite X position
   CLC             ; make sure carry flag is set
   ADC   #$01        ; A = A - 1
   STA   $020C       ; save sprite X position
ReadDownDone:

ReadLeft:
   LDA   $4016
ReadRight:
   LDA   $4016
 
ReadA2:
   LDA   $4017
ReadB2:
   LDA   $4017
ReadSelect2:
   LDA   $4017
ReadStart2:
   LDA   $4017
ReadUp2:
   LDA   $4017
   AND   #%00000001  ; only look at bit 0
   BEQ   ReadUp2Done   ; branch to ReadBDone if button is NOT pressed (0)
   
   LDA   $0210       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC   #$01        ; A = A - 1
   STA   $0210       ; save sprite X position
   
   LDA   $0214       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC   #$01        ; A = A - 1
   STA   $0214       ; save sprite X position
   
   LDA   $0218       ; load sprite X position
   SEC             ; make sure carry flag is set
   SBC   #$01        ; A = A - 1
   STA   $0218       ; save sprite X position
ReadUp2Done:

ReadDown2:
   LDA   $4017
   AND   #%00000001  ; only look at bit 0
   BEQ   ReadDown2Done   ; branch to ReadBDone if button is NOT pressed (0)
   
   LDA   $0210       ; load sprite X position
   CLC             ; make sure carry flag is set
   ADC   #$01        ; A = A - 1
   STA   $0210       ; save sprite X position
   
   LDA   $0214       ; load sprite X position
   CLC             ; make sure carry flag is set
   ADC   #$01        ; A = A - 1
   STA   $0214       ; save sprite X position
   
   LDA   $0218       ; load sprite X position
   CLC             ; make sure carry flag is set
   ADC   #$01        ; A = A - 1
   STA   $0218       ; save sprite X position
ReadDown2Done:

ReadLeft2:
   LDA   $4017
ReadRight2:
   LDA   $4017