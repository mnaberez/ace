;*** jif program - by Craig Bruce - 21-Feb-94

.seq "acehead.s"
.org aceAppAddress
.obj "@0:jif"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;===window===

main = *
   lda #<msg
   ldy #>msg
   jsr puts
   ldx #2
   jsr aceTimeJif
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #2
   jsr aceMiscUtoa
   lda zp+0
   ldy zp+1
   jsr puts
   lda #chrCR
   jmp putchar

msg = *
   .asc "jiffy count = "
   .byte 0 
numbuf .buf 12

;******** standard library ********

puts = *
   ldx #stdout
fputs = *
   sta zp+0
   sty zp+1
   ldy #$ff
-  iny
   lda (zp),y
   bne -
   tya
   ldy #0
   jmp write
eputs = *
   ldx #stderr
   jmp fputs

putchar = *
   ldx #stdout
putc = *
   sta putcBuffer
   lda #<putcBuffer
   ldy #>putcBuffer
   sta zp+0
   sty zp+1
   lda #1
   ldy #0
   jmp write
   putcBuffer .buf 1

;===the end===

bss = *
