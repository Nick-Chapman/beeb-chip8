
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

.Count skip 1

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
.digits: equs "0123456789ABCDEF"
    }

.randomOffset skip 1

.getRandomByte: { ; -->A
    inc randomOffset
    ;;sty restoreY+1
    ldy randomOffset
    lda randomBytes,y
    ;;.restoreY : ldy #&77
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; debug

.debugDecode:
    position 1,1
    puts "pc' "
    lda ProgramCounter+1 : and #&0f : jsr printHexA
    lda ProgramCounter   : jsr printHexA

    position 10,1
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
;;; Screen Address calculation.

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

.xorPlotXY:
    jsr getXY
    bne unplotXY ;; TODO: collision detection -> VF
    jmp plotXY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw sprites

.drawSpriteStrip: {
    lda #8 : sta StripCount
.loop:
    jsr calcScreenAddr ;; TODO: avoid doing this for every horizontal pixel
    asl SpriteStrip
    bcc after
    jsr xorPlotXY
.after:
    inc ScreenX
    lda ScreenX
    cmp #64
    beq done
    dec StripCount
    bne loop
.done:
    rts
    }

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
;;; dispatch

.clearScreen: {
    ;; 00E0 (clear screen) ;; TODO: optimize this!
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

.return:
    ;; 00EE (Return)
    pla : sta ProgramCounter
    pla : sta ProgramCounter+1
    jmp next

.op0:
    lda OpH : cmp #0 : bne unknown
    lda OpL
    { cmp #&ee : bne no : jmp return : .no }
    { cmp #&e0 : bne no : jsr clearScreen : jmp next : .no }
    panic " -00??"
.unknown:
    panic " -0???"

.op1:
    ;; 1NNN (Jump)
    lda OpH : and #&f : ora #&20 : sta ProgramCounter+1
    lda OpL : sta ProgramCounter
    jmp next

.op2:
    ;; 2NNN (Call)
    lda ProgramCounter+1 : pha
    lda ProgramCounter : pha
    lda OpH : and #&f : ora #&20 : sta ProgramCounter+1
    lda OpL : sta ProgramCounter
    jmp next

.op3: {
    ;; 3XNN (Skip Equal Literal)
    lda OpH : and #&f : tax : lda Registers,x
    cmp OpL
    bne noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.op4: {
    ;; 4XNN (Skip Not Equal Literal)
    lda OpH : and #&f : tax : lda Registers,x
    cmp OpL
    beq noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.op5: {
    ;; 5XY0 (Skip Equal Regs)
    lda OpH : and #&f : tax :
    lda OpL : shiftRight4 : tay :
    lda Registers,x
    cmp Registers,y
    bne noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.op6:
    ;; 6XNN (Set Register)
    lda OpH : and #&f : tax
    lda OpL : sta Registers,x
    jmp next

.op7:
    ;; 7XNN (Add To Register)
    lda OpH : and #&f : tax
    lda OpL : clc : adc Registers,x : sta Registers,x
    jmp next

.op80:
    ;; 8XY0 (Set)
    lda OpH : and #&f : tax
    lda OpL : shiftRight4 : tay
    lda Registers,y
    sta Registers,x
    jmp next

.op81: panic " -8??1"
.op82: panic " -8??2"
.op83: panic " -8??3"
.op84: panic " -8??4"
.op85: panic " -8??5"
.op86: panic " -8??6"
.op87: panic " -8??7"
.op8E: panic " -8??E"
.op8x: panic " -8???"

.dispatchTable8 : equw op80,op81,op82,op83,op84,op85,op86,op87,op8x,op8x,op8x,op8x,op8x,op8x,op8E,op8x

.op8:
    ;; dispatch2
    lda OpL : and &0F : asl a : tay
    {
    lda dispatchTable8,y : sta smc+1 : iny
    lda dispatchTable8,y : sta smc+2
    .smc : jmp &EEEE
    }

.op9: {
    ;; 9XY0 (Skip Not Equal Regs)
    lda OpH : and #&f : tax :
    lda OpL : shiftRight4 : tay :
    lda Registers,x
    cmp Registers,y
    beq noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.opA:
    ;; ANNN (Set Index Register)
    lda OpH : and #&f : ora #&20 : sta RegI+1
    lda OpL : sta RegI
    jmp next

.opB: panic " -B???"

.opC:
    ;; CXNN (Random)
    lda OpH : and #&f : tax
    jsr getRandomByte
    and OpL
    sta Registers,x
    jmp next

.opD:
    ;; DXYN (Draw)
    lda OpH : and #&f : tax : lda Registers,x : sta ScreenX
    lda OpL : shiftRight4 : tay : lda Registers,y : sta ScreenY
    lda OpL : and #&f : sta NumLines
    jsr drawSprite ;; TODO: collision detection -> VF
    jmp next

.opE: panic " -E???"

.addToIndex: {
    ;; FX1E (Add To Index)
    lda OpH : and #&f : tax : lda Registers,x
    clc : adc RegI : sta RegI
    bcc done
    inc RegI+1
.done:
    jmp next
    }

.fontCharacter: {
    ;; FX29 (Font Character)
    lda OpH : and #&f : tax : ldy Registers,x
    lda #LO(fontData) : sta RegI
    lda #HI(fontData) : sta RegI+1
    ;; TODO: this loop to multiply by 5 is a silly way to do things
    ;; better to use a dispatch table, or maybe just space out the chars by 8 not 5
.loop:
    dey
    bmi done
    clc
    lda RegI   : adc #5 : sta RegI
    lda RegI+1 : adc #0 : sta RegI+1
    jmp loop
.done:
    jmp next
    }

.storeBCD:
    ;; FX33 (Binary Coded Decimal Conversion)
    lda OpH : and #&f : tax : lda Registers,x
    ;; TODO: split acc to three decimal digits. for now hack it as 567 -- see 67 on screen in brix
    ldy #0
    lda #5
    sta (RegI),y
    ldy #1
    lda #6
    sta (RegI),y
    ldy #2
    lda #7
    sta (RegI),y
    jmp next

.saveRegs: {
    ;; FX55 (Save Registers)
    lda OpH : and #&f : sta Count
    ldy #0
.loop:
    lda Registers,y
    sta (RegI),y ;; TODO: orig chip8 behav was to increment RegI
    cpy Count
    beq done
    iny
    jmp loop
.done:
    jmp next }

.restoreRegs: {
    ;; FX65 (Restore Registers)
    lda OpH : and #&f : sta Count
    ldy #0
.loop:
    lda (RegI),y ;; TODO: orig chip8 behav was to increment RegI
    sta Registers,y
    cpy Count
    beq done
    iny
    jmp loop
.done:
    jmp next }

.opF:
    lda OpL
    { cmp #&1E : bne no : jmp addToIndex : .no }
    { cmp #&29 : bne no : jmp fontCharacter : .no }
    { cmp #&33 : bne no : jmp storeBCD : .no }
    { cmp #&55 : bne no : jmp saveRegs : .no }
    { cmp #&65 : bne no : jmp restoreRegs : .no }
    panic " -F???"

.dispatchTable : equw op0,op1,op2,op3,op4,op5,op6,op7,op8,op9,opA,opB,opC,opD,opE,opF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; execute

.bumpPC: {
    inc ProgramCounter
    inc ProgramCounter
    bne done
    inc ProgramCounter+1
.done:
    rts }

.execute:
    lda #LO(romStart) : sta ProgramCounter
    lda #HI(romStart) : sta ProgramCounter+1

 ;;; ops jump back here after execution
.next:
    ;; jsr debugDecode
    ;; fetch
    ldy #1 : lda (ProgramCounter),y : sta OpL
    ldy #0 : lda (ProgramCounter),y : sta OpH
    ;; dispatch
    and #&f0 : shiftRight4 : asl a : tay
    jsr bumpPC
    {
    lda dispatchTable,y : sta smc+1 : iny
    lda dispatchTable,y : sta smc+2
    .smc : jmp &EEEE
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr clearScreen
    jmp execute

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

print "bytes taken by interpreter: ", *-interpreterStart
;print "bytes remaining for interpreter: ", chip8memStart-*
org chip8memStart
;;original interpreter lived here in 512 bytes -- now fonts live here.

.fontData: equb &f0,&90,&90,&90,&f0, &20,&60,&20,&20,&70, &f0,&10,&f0,&80,&f0, &f0,&10,&f0,&10,&f0, &90,&90,&f0,&10,&10, &f0,&80,&f0,&10,&f0, &f0,&80,&f0,&90,&f0, &f0,&10,&20,&40,&40, &f0,&90,&f0,&90,&f0, &f0,&90,&f0,&10,&f0, &f0,&90,&f0,&90,&90, &e0,&90,&e0,&90,&e0, &f0,&80,&80,&80,&f0, &e0,&90,&90,&90,&e0, &f0,&80,&f0,&80,&f0, &f0,&80,&f0,&80,&80
sizeFontData = *-fontData
assert (sizeFontData = 80)

.randomBytes:
equb &22,&52,&6a,&51,&a7,&35,&26,&bc,&ce,&54,&e8,&56,&60,&af,&45,&04
equb &ce,&65,&54,&70,&df,&d4,&36,&b1,&7c,&0f,&0d,&dd,&1f,&66,&bd,&98
equb &7e,&a0,&8e,&36,&27,&5a,&9b,&31,&7e,&70,&48,&65,&6f,&39,&45,&60
equb &db,&4f,&fb,&ba,&e4,&7a,&a7,&a7,&96,&f0,&b0,&e6,&a8,&e9,&99,&bb
equb &10,&6f,&28,&02,&dc,&79,&bc,&b3,&18,&18,&81,&cc,&bb,&b3,&e0,&ff
equb &8b,&4f,&11,&e0,&f2,&1b,&ff,&7a,&ee,&37,&c5,&ca,&9d,&57,&ba,&c4
equb &cd,&65,&b5,&43,&f7,&5c,&82,&10,&d2,&8c,&5e,&b0,&c5,&aa,&c6,&1a
equb &bd,&a4,&3a,&f7,&37,&0f,&5c,&5f,&63,&61,&93,&0a,&05,&54,&21,&7a
equb &b2,&c3,&fe,&3f,&74,&a6,&5c,&3e,&ca,&1b,&5c,&26,&57,&ef,&01,&32
equb &f9,&ff,&82,&b4,&ee,&df,&7c,&d6,&2f,&f2,&e5,&20,&84,&3b,&a6,&d0
equb &ac,&2a,&88,&c3,&9b,&01,&81,&9f,&a5,&3a,&c4,&fa,&fc,&6d,&d4,&46
equb &e2,&f6,&7d,&39,&63,&0a,&97,&6d,&b9,&9a,&97,&71,&f8,&ea,&ff,&7f
equb &85,&bc,&88,&06,&3b,&30,&c5,&3f,&33,&3a,&67,&d7,&a7,&f7,&f7,&83
equb &8f,&f6,&ae,&f1,&1f,&07,&3b,&a6,&0b,&3b,&b3,&9b,&f9,&f4,&67,&fe
equb &1a,&c0,&ce,&41,&cc,&26,&13,&b8,&64,&c0,&77,&42,&00,&9f,&63,&e2
equb &70,&3b,&a5,&0d,&f2,&13,&e8,&72,&9b,&e0,&ad,&7e,&aa,&8e,&d0,&f5
sizeRandomBytes = *-randomBytes
assert sizeRandomBytes = 256

for i, 1, 512-sizeFontData-sizeRandomBytes : equb 0 : next
.romStart:
incbin ROM
assert (romStart = &2200)
.end:

save "Code", start, end
