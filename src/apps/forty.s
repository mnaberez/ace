;* "forty" program

!src "../system/acehead.s"
!to "../../build/forty", cbm

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

main = *
   lda #0
   ldx #40
   jmp aceWinScreen
