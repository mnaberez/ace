;date program

;the program is in Buddy-assembler format.

.seq "acehead.s"
.org aceAppAddress
.obj "@:date"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

main = *
   lda #<dateBuf
   ldy #>dateBuf
   jsr aceTimeGetDate
   ldy #7
-  lda dateBuf,y
   jsr getasc
   pha
   txa
   ldx dateNumFields,y
   sta dateNum+1,x
   pla
   sta dateNum,x
   dey
   bpl -
   lda #13
   sta dateNum+21
   lda #<dateNum
   ldy #>dateNum
   sta zp
   sty zp+1
   lda #22
   ldy #0
   ldx #stdout
   jmp write

getasc = *  ;( .A=bcd ) : .A=aschi, .X=asclo
   pha
   and #$0f
   ora #$30
   cmp #$3a
   bcc +
   adc #6
+  tax
   pla
   lsr
   lsr
   lsr
   lsr
   ora #$30
   cmp #$3a
   bcc +
   adc #6
+  rts

dateNum = *
        ;0123456789012345678901
   .asc "1993/05/16-18:04:50.3z"
dateNumFields = *
   .byte 0,2,5,8,11,14,17,20
dateStr = *
        ;0123456789012345678901234567
   .asc "Sun-16-May-1993  06:03:50 pm"
   .byte 0
dateBuf = *
