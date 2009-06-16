;* "forty" program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:forty"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

main = *
   lda #0
   ldx #40
   jmp aceWinScreen
