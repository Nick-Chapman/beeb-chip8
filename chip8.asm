
;;; MOS entry points
osasci = &ffe3
oswrch = &ffee

;;; Mode-1
screenStart = &3000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70

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

.draw_chip8_pixel: ;; x:0..63, y:0..31 --> theA
    txa : lsr a : lsr a : lsr a : lsr a ; x/16
    sta smc_hbOnRow+1
    tya : lsr a ; y/2
    sta smc_halfY+1
    asl a : asl a : clc
    .smc_halfY : adc #&EE
    .smc_hbOnRow : adc #&EE
    lsr a
    clc : adc #HI(screenStart)
    sta theA+1
    tya : lsr a : and #1            ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    stx smc_x+1
    .smc_x : eor #&EE               ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta smc_alo+1
    tya : asl a : asl a             ; y*4
    and #&7 : clc
    .smc_alo : adc #&EE
    sta theA
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
