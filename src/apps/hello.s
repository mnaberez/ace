;* "hello" program

!src "../system/acehead.s"
!to "../../build/hello", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

main = *
   lda #<helloMsg
   ldy #>helloMsg
   sta zp+0
   sty zp+1
   lda #<helloMsgEnd-helloMsg
   ldy #>helloMsgEnd-helloMsg
   ldx #stdout
   jsr write
   rts

helloMsg = *
   !pet "Hello, cruel world."
   !byte chrBEL,chrCR
helloMsgEnd = *
