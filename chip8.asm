
;;; MOS entry points
osasci = &ffe3
oswrch = &ffee

;;; Mode-1
screenStart = &3000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70

.theX skip 2 ; plot from Hi byte + hi-bit of LO byte
.theY skip 2 ; plot only from HI byte

.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; graphics


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate screen address from X/Y

.calcA: ; theX/theY --> the{A,FX,CX,FY,CY}
    {
    lda theX+1
    lsr a
    sta theCX
    lsr a : lsr a : lsr a : lsr a
    sta smc_hbOnRow+1

    lda theY+1
    lsr a : lsr a : lsr a
    sta theCY
    asl a : asl a : clc
    adc theCY
    .smc_hbOnRow : adc #&ee
    lsr a
    clc : adc #HI(screenStart)
    sta theA+1

    lda theCY : and #1              ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    eor theCX                       ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta smc_alo+1

    lda theY+1
    and #&7 : sta theFY
    clc : .smc_alo : adc #&ee       ; adjust A with fineY
    sta theA

    lda theX ; (only need hi-order bit; rotate into carry)
    asl a
    lda theX+1
    rol a : and #&3 : sta theFX
    rts
    }


.draw_chip8_pixel: ;; x:0..63, y:0..31

    lda #0
    sta theX
    txa : asl a
    sta theX+1

    lda #0
    sta theY
    tya : asl a : asl a
    sta theY+1

    jsr calcA

    ldy #0
    lda #%11101110
    sta (theA),y : iny
    sta (theA),y : iny
    sta (theA),y

    rts

.count skip 1
.draw_something:
    {
    lda #1
    sta count
.loop:
    ldx count : ldy #0 : jsr draw_chip8_pixel
    ldx count : ldy #31 : jsr draw_chip8_pixel
    inc count
    lda count
    cmp #63 : bne loop
    }
    {
    lda #1
    sta count
.loop:
    ldy count : ldx #0 : jsr draw_chip8_pixel
    ldy count : ldx #63 : jsr draw_chip8_pixel
    inc count
    lda count
    cmp #31 : bne loop
    }
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init

.mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.cursor_off:
    lda #23 : jsr oswrch
    lda #1 : jsr oswrch
    lda #0 :
    jsr oswrch : jsr oswrch : jsr oswrch : jsr oswrch
    jsr oswrch : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.init:
    jsr mode1
    jsr cursor_off
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr draw_something
    jmp spin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
save "Code", start, end
