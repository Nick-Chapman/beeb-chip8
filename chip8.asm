
Debug = FALSE

interpreterStart = &1100
chip8memStart = &2000 ;; 4k
screenStart = &3000 ;; mode-1

;;; MOS vectors & zero page use
interruptSaveA = &fc
irq1v = &204

;;; Sheila
system_VIA_portB            = &fe40
system_VIA_dataDirectionA   = &fe43
system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

soundChipData = system_VIA_portA

;;; MOS entry points
osasci = &ffe3
osnewl = &ffe7
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

macro positionVarY X
    lda #31 : jsr osasci
    lda #X : jsr osasci
    tya : jsr osasci
endmacro

macro puts S
    copy16iv msg, MsgPtr
    jmp after
.msg: equs S, 0
.after:
    jsr printString
endmacro

macro emit C
    lda #C
    jsr osasci
endmacro

macro space
    emit ' '
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70
guard &100

.ProgramCounter skip 2
.Index skip 2
.DelayTimer skip 1
.SoundTimer skip 1
.Registers skip 16

.MsgPtr skip 2

.ScreenX skip 1 ; 0..63
.ScreenY skip 1 ; 0..31
.ScreenAddr skip 2

.SpriteStrip skip 1
.StripCount skip 1
.NumLines skip 1

.OpH skip 1
.OpL skip 1

.Count skip 1

.Modulus skip 1
.Divisor skip 1

.RandomSeed skip 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

org interpreterStart
guard screenStart

.start:
    jmp main

.spin: jmp spin

;;; A tiny, fast, 8-bit pseudo-random number generator in 6502 assembly; by White Flame.
;;; https://www.codebase64.org/doku.php?id=base:small_fast_8-bit_prng
.getRandomByte: ; -->A
    lda RandomSeed
    beq doEor
    asl a
    bcc noEor
.doEor:
    eor #&1d
.noEor:
    sta RandomSeed
    rts

.writeChar: { ; converting asci 10 --> NL
    cmp #10
    bne ok
    ;;jmp osnewl ;; not working
    rts
.ok:
    jmp osasci ; has special handling for 13 -> NL
    }

.printString: {
    ldy #0
.loop
    lda (MsgPtr),y
    beq done
    jsr writeChar
    iny
    bne loop
.done:
    rts
    }

.HexDigits: equs "0123456789ABCDEF"

.printHexA: {
    pha
    and #&f0 : shiftRight4 : tay
    lda HexDigits,y
    jsr osasci
    pla
    and #&f : tay
    lda HexDigits,y
    jsr osasci
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read Keys

;;; https://beebwiki.mdfs.net/Keyboard
.BeebCode equb &42,&30,&31,&11,&10,&21,&22,&41,&51,&32,&61,&52,&12,&33,&43,&63
;;;            X,  1,  2,  3,  Q,  W,  E,  A,  S,  D,  Z,  C,  4,  R,  F,  V

;;;  press             chip8
;;; -------           -------
;;; 1 2 3 4      =>   1 2 3 C
;;;  q w e r     =>   4 5 6 D
;;;   a s d f    =>   7 8 9 E
;;;    z x c v   =>   A 0 B F

.Keys skip 16

.readKeys: {
    ldx #0
.loop:
    lda #0 : sta Keys,x
    lda BeebCode,x : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta Keys,x : .no
    inx
    cpx #16
    bne loop
    rts }

.KeyPadLayoutOrder equb 1,2,3,&C,4,5,6,&D,7,8,9,&E,&A,0,&B,&F

macro panic S
    puts S
    jmp spin
endmacro

.badop:
    position 1,30
    lda OpH : jsr printHexA
    lda OpL : jsr printHexA
    panic " badop"

macro badop : jmp badop : endmacro

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialize

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

.initialize:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    ;; start in keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
    jsr mode1
    jsr cursorOff
    jsr setLogicalZeroAsBlue
    jsr setLogicalTwoAsBlack
    jsr setLogicalThreeAsCyan
    copy16iv myIRQ, irq1v
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sound

macro DEL20 : nop:nop:nop:nop:nop:nop:nop:nop:nop:nop : endmacro

.pulseWriteSound:
    ;; switch to sound mode
    lda #%11111111 : sta system_VIA_dataDirectionA ; (All bits output)
    lda #%00001011 : sta system_VIA_portB ; set bit 3 to 1
    ;; pulse sound write
    lda #%00000000 : sta system_VIA_portB
    DEL20
    lda #%00001000 : sta system_VIA_portB
    DEL20
    ;; back to keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
    rts

macro SEND
    sta soundChipData
    jsr pulseWriteSound
endmacro

.soundOn:
    lda #&89 : SEND
    lda #&3b : SEND
    lda #&90 : SEND
    rts

.silenceMaybe: {
    lda SoundTimer
    bne no
    lda #&9f : SEND ; sound off
    rts
.no:
    dec SoundTimer
    rts }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screen interrupts

.vsyncNotify equb 0 ; -> 1

.myIRQ: {
    lda system_VIA_interruptFlags : and #2 : bne vblank
    panic "unexpected interrupt"
    lda #&7f : sta system_VIA_interruptFlags ; ack
    lda interruptSaveA
    rti
.vblank:
    sta system_VIA_interruptFlags ; ack
    inc vsyncNotify
    lda interruptSaveA
    rti }

.onSync:
    jsr silenceMaybe
    { lda DelayTimer : beq no : dec DelayTimer : .no }
    jsr readKeys
    if Debug
      inc frameCounter
      position 37,30 : lda frameCounter : jsr printHexA
      jsr debugKeys
      jsr debugState
    endif
    rts

.awaitSync:
    lda vsyncNotify
    beq awaitSync
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros

macro BumpIndex ; by A
    clc : adc Index : sta Index
    { bcc done : inc Index+1 : .done: }
endmacro

macro SetVF N
    lda #N : sta Registers+&F
endmacro

macro SetXfromY
    lda Registers,y
    sta Registers,x
endmacro

macro DecodeX
    lda OpH : and #&f : tax
endmacro

macro DecodeY
    lda OpL : shiftRight4 : tay
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw sprites

.plotXY: ;; (logical 3)
    ldy #0
    lda #%11111111
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y
    rts

.unplotXY: ;; (logical 2)
    ldy #0
    lda #%11110000
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y : iny
    sta (ScreenAddr),y
    rts

.getXY:
    ldy #0
    lda (ScreenAddr),y
    and #&f ;; just look at low nibble to distinuish logical 3 (on) from logical 2 (off)
    rts

.xorPlotXY: {
    jsr getXY
    bne collision
    jmp plotXY
.collision:
    SetVF 1
    jmp unplotXY
    }

.drawSpriteStrip: {
    lda #8 : sta StripCount
.loop:
    jsr calcScreenAddr ;; TODO: avoid doing this for every horizontal pixel
    asl SpriteStrip
    bcc after
    jsr xorPlotXY
.after:
    inc ScreenX
    lda ScreenX : cmp #64 : beq done ;; quirk, clipping on
    dec StripCount
    bne loop
.done:
    rts
    }

.drawSprite: {
    lda #0 : sta smc_y+1
.loop:
    .smc_y : ldy #&EE
    lda (Index),y : sta SpriteStrip
    lda ScreenX : sta smc+1
    jsr drawSpriteStrip ;; TODO inline
    dec NumLines
    beq done
    .smc : lda #&EE : sta ScreenX
    inc ScreenY
    lda ScreenY : cmp #32 : beq done ;; quirk, clipping on
    inc smc_y+1
    jmp loop
.done:
    rts }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ops

.checkCarryNext: {
    bcs carry
    SetVF 0
    jmp next
.carry:
    SetVF 1
    jmp next }

.bumpPC: {
    inc ProgramCounter
    inc ProgramCounter
    bne done
    inc ProgramCounter+1
.done:
    rts }

.op00E0: {
    ;; 00E0 (Clear Screen) ;; TODO: optimize this!
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
    jmp next }

.op00EE:
    ;; 00EE (Return)
    pla : sta ProgramCounter
    pla : sta ProgramCounter+1
    jmp next

.op0: {
    lda OpH : cmp #0 : bne bad
    lda OpL
    { cmp #&ee : bne no : jmp op00EE : .no }
    { cmp #&e0 : bne no : jmp op00E0 : .no }
.bad:
    badop }

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
    DecodeX
    lda Registers,x
    cmp OpL
    bne noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.op4: {
    ;; 4XNN (Skip Not Equal Literal)
    DecodeX
    lda Registers,x
    cmp OpL
    beq noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.op5: {
    ;; 5XY0 (Skip Equal Regs)
    DecodeX
    DecodeY
    lda Registers,x
    cmp Registers,y
    bne noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.op6:
    ;; 6XNN (Set Register from Literal)
    DecodeX
    lda OpL : sta Registers,x
    jmp next

.op7:
    ;; 7XNN (Add To Register)
    DecodeX
    lda OpL : clc : adc Registers,x : sta Registers,x
    jmp next

.op8XY0:
    ;; 8XY0 (Set Register: X = Y)
    DecodeX
    DecodeY
    SetXfromY
    jmp next

.op8XY1:
    ;; 8XY1 (Register Bitwise Or)
    DecodeX
    DecodeY
    lda Registers,x
    ora Registers,y
    sta Registers,x
    SetVF 0 ;; "vf-reset" quirk
    jmp next

.op8XY2:
    ;; 8XY2 (Register Bitwise And)
    DecodeX
    DecodeY
    lda Registers,x
    and Registers,y
    sta Registers,x
    SetVF 0 ;; "vf-reset" quirk
    jmp next

.op8XY3:
    ;; 8XY3 (Register Bitwise Xor)
    DecodeX
    DecodeY
    lda Registers,x
    eor Registers,y
    sta Registers,x
    SetVF 0 ;; "vf-reset" quirk
    jmp next

.op8XY4:
    ;; 8XY4 (Register Add)
    DecodeX
    DecodeY
    lda Registers,x
    clc : adc Registers,y
    sta Registers,x
    jmp checkCarryNext

.op8XY5:
    ;; 8XY5 (Register Subtract)
    DecodeX
    DecodeY
    lda Registers,x
    sec : sbc Registers,y
    sta Registers,x
    jmp checkCarryNext

.op8XY6:
    ;; 8XY6 (Register Shift Right)
    DecodeX
    DecodeY
    ;;SetXfromY ;; uncomment for "shifting" quirk off. breaks invaders
    lsr Registers,x
    jmp checkCarryNext

.op8XY7:
    ;; 8XY7 (Register Subtract Reverse)
    DecodeX
    DecodeY
    lda Registers,y
    sec : sbc Registers,x
    sta Registers,x
    jmp checkCarryNext

.op8XYE:
    ;; 8XYE (Register Shift Left)
    DecodeX
    DecodeY
    ;;SetXfromY ;; uncomment for "shifting" quirk off. breaks invaders
    asl Registers,x
    jmp checkCarryNext

.dispatchOp8:
    equw op8XY0,op8XY1,op8XY2,op8XY3,op8XY4,op8XY5,op8XY6,op8XY7
    equw badop,badop, badop, badop, badop, badop, op8XYE,badop

.op8:
    ;; dispatch2
    lda OpL : and #&f : asl a : tay
    {
    lda dispatchOp8,y : sta smc+1 : iny
    lda dispatchOp8,y : sta smc+2
    .smc : jmp &EEEE
    }

.op9: {
    ;; 9XY0 (Skip Not Equal Regs)
    DecodeX :
    DecodeY :
    lda Registers,x
    cmp Registers,y
    beq noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.opA:
    ;; ANNN (Set Index Register)
    lda OpH : and #&f : ora #&20 : sta Index+1
    lda OpL : sta Index
    jmp next

.opB: {
    ;; BNNN (Jump with offset)
    lda OpH : and #&f : ora #&20 : sta ProgramCounter+1
    lda OpL : clc : adc Registers+0
    sta ProgramCounter
    bcc done
    inc ProgramCounter+1
.done:
    jmp next }

.opC:
    ;; CXNN (Random)
    DecodeX
    jsr getRandomByte
    and OpL
    sta Registers,x
    jmp next

.opD:
    ;; DXYN (Draw)
    DecodeX
    DecodeY
    lda Registers,x : sta ScreenX
    lda Registers,y : sta ScreenY
    lda OpL : and #&f : sta NumLines
    SetVF 0
    jsr awaitSync ;; display wait (quirk?) ;; needed to slow brix
    jsr drawSprite ;; TODO: inline
    jmp next

.opEX9E: {
    ;; EX9E (Skip If Key pressed)
    DecodeX
    ldy Registers,x : lda Keys,y
    beq noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.opEXA1: {
    ;; EXA1 (Skip If Key NOT pressed)
    DecodeX
    ldy Registers,x : lda Keys,y
    bne noSkip
    jsr bumpPC
.noSkip:
    jmp next }

.opE:
    lda OpL
    { cmp #&9E : bne no : jmp opEX9E : .no }
    { cmp #&A1 : bne no : jmp opEXA1 : .no }
    badop

.opFX07:
    ;; FX07 (Read Delay Timer)
    DecodeX
    lda DelayTimer
    sta Registers,x
    jmp next

.opFX0A: {
    ;; FX0A (Blocking Get Key)
.again:
    ldy #0
.loop:
    lda Keys,y
    bne press
    iny
    cpy #16
    bne loop
    jsr readKeys ;; TODO: okay here? normally we do it in sync
    jmp again
.press:
    DecodeX
    sty Registers,x
    jmp next
    }

.opFX15: {
    ;; FX15 (Set Delay Timer)
    DecodeX
    lda Registers,x : sta DelayTimer
    jmp next }

.opFX18: {
    ;; FX15 (Set Sound Timer)
    DecodeX
    lda Registers,x : sta SoundTimer
    jsr soundOn
    jmp next }

.opFX1E:
    ;; FX1E (Add To Index)
    DecodeX
    lda Registers,x
    BumpIndex
    jmp next

.opFX29: {
    ;; FX29 (Font Character)
    DecodeX
    ldy Registers,x
    lda #LO(fontData) : sta Index
    lda #HI(fontData) : sta Index+1
    ;; TODO: this loop to multiply by 5 is a silly way to do things
    ;; better to use a dispatch table, or maybe just space out the chars by 8 not 5
.loop:
    dey
    bmi done
    clc
    lda Index   : adc #5 : sta Index
    lda Index+1 : adc #0 : sta Index+1
    jmp loop
.done:
    jmp next
    }

.opFX33: {
    ;; FX33 (BCD - Binary Coded Decimal Conversion)
    DecodeX
    lda Registers,x
    sta Divisor
    ldy #3
.each_digit:
    lda #0
    sta Modulus
    clc
    ldx #8
.each_bit:
    rol Divisor
    rol Modulus
    sec
    lda Modulus
    sbc #10
    bcc ignore_result
    sta Modulus
.ignore_result:
    dex
    bne each_bit
    rol Divisor
    lda Modulus
    dey
    sta (Index),y
    bne each_digit
    jmp next }

.opFX55: {
    ;; FX55 (Save Registers)
    lda OpH : and #&f : sta Count
    ldy #0
.loop:
    lda Registers,y
    sta (Index),y
    cpy Count
    beq done
    iny
    jmp loop
.done:
    iny : tya : BumpIndex ;; "memory" quirk
    jmp next }

.opFX65: {
    ;; FX65 (Restore Registers)
    lda OpH : and #&f : sta Count
    ldy #0
.loop:
    lda (Index),y
    sta Registers,y
    cpy Count
    beq done
    iny
    jmp loop
.done:
    iny : tya : BumpIndex ;; "memory" quirk
    jmp next }

.opF:
    lda OpL
    { cmp #&07 : bne no : jmp opFX07 : .no }
    { cmp #&0A : bne no : jmp opFX0A : .no }
    { cmp #&15 : bne no : jmp opFX15 : .no }
    { cmp #&18 : bne no : jmp opFX18 : .no }
    { cmp #&1E : bne no : jmp opFX1E : .no }
    { cmp #&29 : bne no : jmp opFX29 : .no }
    { cmp #&33 : bne no : jmp opFX33 : .no }
    { cmp #&55 : bne no : jmp opFX55 : .no }
    { cmp #&65 : bne no : jmp opFX65 : .no }
    badop

.dispatchOp : equw op0,op1,op2,op3,op4,op5,op6,op7,op8,op9,opA,opB,opC,opD,opE,opF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; execute/next

.execute:
    lda #LO(romStart) : sta ProgramCounter
    lda #HI(romStart) : sta ProgramCounter+1
    jmp op00E0 ;; initial clear screen; continues at next

;;; ops jump back here after execution
.next:
    lda vsyncNotify : { beq no : jsr onSync : lda #0 : sta vsyncNotify : .no }
    ;; fetch
    ldy #1 : lda (ProgramCounter),y : sta OpL
    ldy #0 : lda (ProgramCounter),y : sta OpH
    ;; dispatch
    and #&f0 : shiftRight4 : asl a : tay
    jsr bumpPC
    {
    lda dispatchOp,y : sta smc+1 : iny
    lda dispatchOp,y : sta smc+2
    .smc : jmp &EEEE
    }

.info:
    incbin INFO
    equb 0

.displayRomInfo:
    position 1,1
    copy16iv info, MsgPtr
    jmp printString

.main:
    jsr initialize
    jsr displayRomInfo
    jmp execute

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; debug

if Debug

.frameCounter: skip 1

.debugKeys: {
    lda #0 : sta Count
.loop:
    lda Count : tax
    and #&3 : bne noRePosition : txa : lsr a : lsr a : tay : iny : positionVarY 35 : .noRePosition
    lda KeyPadLayoutOrder,x : tax
    lda HexDigits,x : ldy Keys,x : { bne no : lda #'.' : .no } : jsr osasci : inx
    inc Count
    lda Count
    cmp #16
    bne loop
    rts }

.debugPC:
    lda ProgramCounter+1 : and #&0f : jsr printHexA
    lda ProgramCounter   : jsr printHexA
    rts

.debugIndex:
    lda Index+1 : and #&0f : jsr printHexA
    lda Index   : jsr printHexA
    rts

.debugRegisters: {
    position 16,26
    ldx #0
.loop:
    lda Registers,x : jsr printHexA : space : inx
    cpx #8 : bne cont
    position 16,27
.cont:
    cpx #16 : bne loop
    rts }

.debugState: {
    position 1,26 : puts "PC " : jsr debugPC
    position 1,27 : puts "I  " : jsr debugIndex
    position 1,28 : puts "T  " : lda DelayTimer : jsr printHexA
    position 1,29 : puts "S  " : lda SoundTimer : jsr printHexA
    jsr debugRegisters
    rts
}

endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

print "bytes taken by interpreter: ", *-interpreterStart

org chip8memStart
;;;original interpreter lived here in 512 bytes -- now fonts live here.

.fontData: equb &f0,&90,&90,&90,&f0, &20,&60,&20,&20,&70, &f0,&10,&f0,&80,&f0, &f0,&10,&f0,&10,&f0, &90,&90,&f0,&10,&10, &f0,&80,&f0,&10,&f0, &f0,&80,&f0,&90,&f0, &f0,&10,&20,&40,&40, &f0,&90,&f0,&90,&f0, &f0,&90,&f0,&10,&f0, &f0,&90,&f0,&90,&90, &e0,&90,&e0,&90,&e0, &f0,&80,&80,&80,&f0, &e0,&90,&90,&90,&e0, &f0,&80,&f0,&80,&f0, &f0,&80,&f0,&80,&80
sizeFontData = *-fontData
assert (sizeFontData = 80)

for i, 1, 511-sizeFontData : equb 0 : next
;;; final byte before rom, controls behaviour of the quicks test rom
equb 1 ;; select chip8 without needing input form the keypad

.romStart:
assert (romStart = &2200)

incbin ROM

.end:

save "Code", start, end
