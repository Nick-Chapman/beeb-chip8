
interpreterStart = &1100
chip8memStart = &2000 ;; 4k
screenStart = &3000 ;; mode-1

guard screenStart

;;; MOS entry points
osasci = &ffe3
oswrch = &ffee

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros

macro copy16iv A,B
    lda #LO(A) : sta B
    lda #HI(A) : sta B+1
endmacro

macro shiftRight4
    lsr a : lsr a : lsr a : lsr a
endmacro

macro position X,Y
    lda #31 : jsr osasci
    lda #X : jsr osasci
    lda #Y : jsr osasci
endmacro

macro puts S
    copy16iv msg, MsgPtr
    jmp after
.msg: equs S, 0
.after:
    jsr printString
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70

.MsgPtr skip 2

.ScreenX skip 1 ; 0..63
.ScreenY skip 1 ; 0..31
.ScreenAddr skip 2

.SpriteStrip skip 1
.StripCount skip 1
.NumLines skip 1

.ProgramCounter skip 2
.OpH skip 1
.OpL skip 1

.Registers skip 16
.RegI skip 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

org interpreterStart

.start:
    jmp main

.spin: jmp spin

.printString: {
    ldy #0
.loop
    lda (MsgPtr),y
    beq done
    jsr osasci
    iny
    bne loop
.done:
    rts
    }

.printHexA: {
    pha
    and #&f0 : shiftRight4 : tay
    lda digits,y
    jsr osasci
    pla
    and #&f : tay
    lda digits,y
    jsr osasci
    rts
.digits: equs "0123456789abcdef"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Screen Address calculation. TODO: picture doc here!

.calcScreenAddr: ;; ScreenX: 0..63, ScreenY: 0..31 --> ScreenAddr
    lda ScreenX : and #63
    clc : adc #8 ; X shifted by 8 to centralize horizontally
    sta smc_x+1
    shiftRight4 : sta smc_hbOnRow+1 ; x/16
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
;;; draw sprites

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
    lda (RegI),y : sta SpriteStrip
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

.setDigitPointer: {
    lda #LO(digitData) : sta RegI
    lda #HI(digitData) : sta RegI+1
.loop:
    dex
    bmi done
    clc
    lda RegI   : adc #5 : sta RegI
    lda RegI+1 : adc #0 : sta RegI+1
    jmp loop
.done:
    rts }

.clearScreen: { ;; TODO: optimize this!
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
;;; debug

.debugDecode:
    position 1,1
    puts "pc "
    lda ProgramCounter+1 : jsr printHexA
    lda ProgramCounter   : jsr printHexA

    position 9,1
    puts "op "
    lda OpH : jsr printHexA
    lda OpL : jsr printHexA
    rts

macro panic s
    jsr debugDecode
    puts s
    jmp spin
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dispatch

.op0: {
    lda OpH : cmp #0 : bne unknown
    lda OpL
    ;; 00EE (return)
    { cmp #&ee : bne no : panic " -op-return" : .no }
    ;; 00E0 (clear screen)
    { cmp #&e0 : bne no : jmp clearScreen : .no }
    panic " -00??"
.unknown:
    panic " -0???"
    }

.op1: panic " -1???"
.op2: panic " -2???"
.op3: panic " -3???"
.op4: panic " -4???"
.op5: panic " -5???"

.op6: {
    ;; 6XNN (Set register VX)
    lda OpH : and #&f : tax
    lda OpL : sta Registers,x
    rts
    }

.op7: {
    ;; 7XNN (Add value to register VX)
    lda OpH : and #&f : tax
    lda OpL : clc : adc Registers,x : sta Registers,x
    rts
    }

.op8: panic " -8???"
.op9: panic " -9???"

.opA: {
    ;; ANNN (set index register)
    lda OpH : and #&f : ora #&20 : sta RegI+1
    lda OpL : sta RegI
    rts
    }

.opB: panic " -B???"
.opC: panic " -C???"

.opD: {
    ;; DXYN (draw)
    lda OpH : and #&f : tax : lda Registers,x : sta ScreenX
    lda OpL : shiftRight4 : tay : lda Registers,y : sta ScreenY
    lda OpL : and #&f : sta NumLines
    jmp drawSprite
    }

.opE: panic " -E???"
.opF: panic " -F???"

.dispatchTable : equw op0,op1,op2,op3,op4,op5,op6,op7,op8,op9,opA,opB,opC,opD,opE,opF

.dispatch: {
    lda OpH : and #&f0 : shiftRight4 : asl a : tay
    lda dispatchTable,y : sta smc+1 : iny
    lda dispatchTable,y : sta smc+2
    .smc : jmp &EEEE }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; execute

.bumpPC: {
    inc ProgramCounter
    inc ProgramCounter
    bne done
    inc ProgramCounter+1
.done:
    rts }

.execute: {
    lda #LO(romStart) : sta ProgramCounter
    lda #HI(romStart) : sta ProgramCounter+1
.loop:
    ;; fetch; decode
    ldy #0 : lda (ProgramCounter),y : sta OpH
    ldy #1 : lda (ProgramCounter),y : sta OpL
    jsr bumpPC
    jsr dispatch
    jmp loop }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jmp execute

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

print "bytes taken by interpreter: ", *-interpreterStart
;print "bytes remaining for interpreter: ", chip8memStart-*
org chip8memStart
;;original interpreter lived here in 512 bytes -- now fonts live here.
.digitData: equb &f0,&90,&90,&90,&f0, &20,&60,&20,&20,&70, &f0,&10,&f0,&80,&f0, &f0,&10,&f0,&10,&f0, &90,&90,&f0,&10,&10, &f0,&80,&f0,&10,&f0, &f0,&80,&f0,&90,&f0, &f0,&10,&20,&40,&40, &f0,&90,&f0,&90,&f0, &f0,&90,&f0,&10,&f0, &f0,&90,&f0,&90,&90, &e0,&90,&e0,&90,&e0, &f0,&80,&80,&80,&f0, &e0,&90,&90,&90,&e0, &f0,&80,&f0,&80,&f0, &f0,&80,&f0,&80,&80
sizeDigitData = *-digitData
assert (sizeDigitData = 80)
for i, 1, 512-sizeDigitData : equb 0 : next
.romStart:
incbin ROM
assert (romStart = &2200)
.end:

save "Code", start, end
