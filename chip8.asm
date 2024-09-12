;;; Minimal asm program

osasci = &FFE3

org &2000

.start:
    lda #'N'
    jsr osasci
.spin:
    jmp spin
.end:

save "code", start, end
