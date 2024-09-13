
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

.SpriteStrip SKIP 1
.Count SKIP 1

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
    jsr calcScreenAddr ;; TODO: caller should do this, to avoid dup in getXY
    ldy #0
    lda #%11101110
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y
    rts

.unplotXY: ;; (logical 2)
    jsr calcScreenAddr ;; TODO: caller should do this, to avoid dup in getXY
    ldy #0
    lda #%11100000
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y
    rts

.getXY:
    jsr calcScreenAddr ;; TODO: caller should do this, to avoid dup in (un)plotXY
    ldy #0
    lda (ScreenAddr),y
    and #&f ;; just look at low nibble to distinuish logical 3 (on) from logical 2 (off)
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw things...

.drawGrid: { ;; TODO: loop in reverse for easier loop termination (test against 0)
    lda #0 : sta ScreenY
.loopY
    lda #0 : sta ScreenX
.loopX:
    jsr unplotXY
    inc ScreenX
    lda ScreenX
    cmp #64 : bne loopX
    inc ScreenY
    lda ScreenY
    cmp #32 : bne loopY
    rts }

.drawBorder: {
    ;; horizontal
    lda #2 : sta ScreenX
.loopA:
    lda #1  : sta ScreenY : jsr plotXY
    lda #30 : sta ScreenY : jsr plotXY
    inc ScreenX
    lda ScreenX
    cmp #62 : bne loopA
    ;; vertical
    lda #2 : sta ScreenY
.loopB:
    lda #1  : sta ScreenX : jsr plotXY
    lda #62 : sta ScreenX : jsr plotXY
    inc ScreenY
    lda ScreenY
    cmp #30 : bne loopB
    rts }

.drawSpriteStrip: { ;; TODO: take advantage of fall through
    lda #8 : sta Count
.loop:
    jsr getXY
    beq amOff
    jmp amOn
.amOff:
    asl SpriteStrip
    bcc off
    jmp on
.amOn:
    asl SpriteStrip
    bcc on
    jmp off
.on:
    jsr plotXY
    jmp after
.off:
    jsr unplotXY
    jmp after
.after:
    inc ScreenX
    lda ScreenX
    cmp #64
    beq done
    dec Count
    bne loop
.done:
    rts }

.drawStuff:
    jsr drawGrid
    jsr drawBorder

    ;;lda #%10100101 : sta SpriteStrip

    lda #3 : sta ScreenX
    lda #3 : sta ScreenY
    lda #255 : sta SpriteStrip
    jsr drawSpriteStrip

    lda #9 : sta ScreenX
    lda #3 : sta ScreenY
    lda #255 : sta SpriteStrip
    jsr drawSpriteStrip

    lda #60 : sta ScreenX
    lda #5 : sta ScreenY
    lda #255 : sta SpriteStrip
    jsr drawSpriteStrip

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
