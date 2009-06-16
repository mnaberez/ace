;eighty - change to 80-column screen program, version 1.0

;by Craig Bruce, 07-Apr-1995.

!src "../system/acehead.s"
!to "../../build/eighty", cbm

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

rows  = $02
digit = $03

main = *
   lda #0
   sta rows
   lda #1
   ldy #0
   jsr getarg
   beq doScreen
   ldy #0
-- lda (zp),y
   beq doScreen
   cmp #"0"
   bcs +
-  jmp usage
+  cmp #"9"+1
   bcs -
   and #$0f
   sta digit
   lda rows
   asl ;x2
   bcs overflow
   asl ;x4
   bcs overflow
   adc rows ;x5
   bcs overflow
   asl ;x10
   bcs overflow
   adc digit
   bcs overflow
   sta rows
   iny
   bne --

   overflow = *
   lda #255
   sta rows

   doScreen = *
   lda rows
   ldx #80
   jsr aceWinScreen
   bcs +
   rts
+  lda #<errmsg
   ldy #>errmsg
   jmp eputs

errmsg = *
   !pet "error: cannot activate 80-column screen as specified"
   !byte chrCR,0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jmp eputs
   usageMsg = *
   !pet "usage: eighty [rows]"
   !byte chrCR,0

;******** standard library ********

eputs = *
   ldx #stderr
   jmp fputs
puts = *
   ldx #stdout
fputs = *
   sta zp+0
   sty zp+1
zpputs = *
   ldy #$ff
-  iny
   lda (zp),y
   bne -
   tya
   ldy #0
   jmp write

getarg = *
   sty zp+1
   asl
   sta zp+0
   rol zp+1
   clc
   lda aceArgv+0
   adc zp+0
   sta zp+0
   lda aceArgv+1
   adc zp+1
   sta zp+1
   ldy #0
   lda (zp),y
   tax
   iny
   lda (zp),y
   stx zp+0
   sta zp+1
   ora zp+0
   rts
