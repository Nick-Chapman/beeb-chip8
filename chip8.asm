
;;; MOS entry points
osasci = &ffe3
oswrch = &ffee

;;; Mode-1
screenStart = &3000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70

.ScreenX SKIP 1 ; 0..63
.ScreenY SKIP 1 ; 0..31

.ScreenAddr SKIP 2

.MemPointer SKIP 2
.SpriteStrip SKIP 1
.StripCount SKIP 1
.NumLines SKIP 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Screen Address calculation. TODO: picture doc here!

.calcScreenAddr: ;; ScreenX: 0..63, ScreenY: 0..31 --> ScreenAddr
    lda ScreenX : and #63
    clc : adc #8 ; X shifted by 8 to centralize horizontally
    sta smc_x+1
    lsr a : lsr a : lsr a : lsr a : sta smc_hbOnRow+1 ; x/16
    lda ScreenY : and #31
    clc : adc #16 ; Y shifted by 16 to centralize vertically
    tay
    lsr a : sta smc_halfY+1 ; y/2
    asl a : asl a
    clc : .smc_halfY : adc #&EE
    .smc_hbOnRow : adc #&EE
    lsr a
    clc : adc #HI(screenStart)
    sta ScreenAddr+1
    tya : lsr a : and #1            ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    .smc_x : eor #&EE               ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta smc_alo+1
    tya : asl a : asl a             ; y*4
    and #&7
    clc : .smc_alo : adc #&EE
    sta ScreenAddr
    rts

.plotXY: ;; (logical 3)
    ldy #0
    lda #%11101110
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y
    rts

.unplotXY: ;; (logical 2)
    ldy #0
    lda #%11100000
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y
    rts

.getXY:
    ldy #0
    lda (ScreenAddr),y
    and #&f ;; just look at low nibble to distinuish logical 3 (on) from logical 2 (off)
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw things...

.drawGrid: {
    lda #0 : sta ScreenY
.loopY
    lda #63 : sta ScreenX
.loopX:
    jsr calcScreenAddr
    jsr unplotXY
    dec ScreenX
    bpl loopX
    inc ScreenY
    lda ScreenY : cmp #32
    bne loopY
    rts }

.drawBorder: {
    lda #2 : sta ScreenX
.loopA:
    lda #1  : sta ScreenY : jsr calcScreenAddr : jsr plotXY
    lda #30 : sta ScreenY : jsr calcScreenAddr : jsr plotXY
    inc ScreenX
    lda ScreenX
    cmp #62 : bne loopA
    lda #2 : sta ScreenY
.loopB:
    lda #1  : sta ScreenX : jsr calcScreenAddr : jsr plotXY
    lda #62 : sta ScreenX : jsr calcScreenAddr : jsr plotXY
    inc ScreenY
    lda ScreenY
    cmp #30 : bne loopB
    rts }

.drawSpriteStrip: {
    lda #8 : sta StripCount
.loop:
    jsr calcScreenAddr
    jsr getXY
    bne amOn
.amOff:
    asl SpriteStrip
    bcc off
    jmp on
.amOn:
    asl SpriteStrip
    bcs off
.on:
    jsr plotXY
    jmp after
.off:
    jsr unplotXY
.after:
    inc ScreenX
    lda ScreenX
    cmp #64
    beq done
    dec StripCount
    bne loop
.done:
    rts }

.drawSprite: {
    lda #0 : sta smc_y+1
.loop:
    .smc_y : ldy #&EE
    lda (MemPointer),y : sta SpriteStrip
    lda ScreenX : sta smc+1
    jsr drawSpriteStrip
    dec NumLines
    beq done
    .smc : lda #&EE : sta ScreenX
    inc ScreenY
    inc smc_y+1
    jmp loop
.done:
    rts }

.digitData: equb &f0,&90,&90,&90,&f0, &20,&60,&20,&20,&70, &f0,&10,&f0,&80,&f0, &f0,&10,&f0,&10,&f0, &90,&90,&f0,&10,&10, &f0,&80,&f0,&10,&f0, &f0,&80,&f0,&90,&f0, &f0,&10,&20,&40,&40, &f0,&90,&f0,&90,&f0, &f0,&90,&f0,&10,&f0, &f0,&90,&f0,&90,&90, &e0,&90,&e0,&90,&e0, &f0,&80,&80,&80,&f0, &e0,&90,&90,&90,&e0, &f0,&80,&f0,&80,&f0, &f0,&80,&f0,&80,&80

.setDigitPointer: {
    lda #LO(digitData) : sta MemPointer
    lda #HI(digitData) : sta MemPointer+1
.loop:
    dex
    bmi done
    clc
    lda MemPointer   : adc #5 : sta MemPointer
    lda MemPointer+1 : adc #0 : sta MemPointer+1
    jmp loop
.done:
    rts }

.DigitN skip 1
.DigitPosX skip 1
.drawDigits: {
    lda #4 : sta DigitN
    lda #3 : sta DigitPosX
.loop:
    lda DigitPosX : sta ScreenX
    lda #4 : sta ScreenY
    lda #5 : sta NumLines
    ldx DigitN
    jsr setDigitPointer
    jsr drawSprite
    lda DigitPosX : clc : adc #5 : sta DigitPosX
    inc DigitN
    lda DigitN
    cmp #&10
    beq done
    jmp loop
.done:
    rts }

.drawStuff:
    jsr drawGrid
    jsr drawBorder
    jsr drawDigits
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init

.mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.cursorOff:
    lda #23 : jsr oswrch
    lda #1 : jsr oswrch
    lda #0 :
    jsr oswrch : jsr oswrch : jsr oswrch : jsr oswrch
    jsr oswrch : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.setLogicalZeroAsBlue:
    lda #19 : jsr oswrch
    lda #0 : jsr oswrch ; logical 0
    lda #4 : jsr oswrch ; physical blue
    lda #0 : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.setLogicalTwoAsBlack:
    lda #19 : jsr oswrch
    lda #2 : jsr oswrch ; logical 2
    lda #0 : jsr oswrch ; physical black
    lda #0 : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.setLogicalThreeAsCyan:
    lda #19 : jsr oswrch
    lda #3 : jsr oswrch ; logical three
    lda #6 : jsr oswrch ; physical cyan
    lda #0 : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.init:
    jsr mode1
    jsr cursorOff
    jsr setLogicalZeroAsBlue
    jsr setLogicalTwoAsBlack
    jsr setLogicalThreeAsCyan
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr drawStuff
    jmp spin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
save "Code", start, end
