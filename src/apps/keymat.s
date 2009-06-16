;keymat - keyboard matrix changer program, verson 1.00

;by Craig Bruce, 21-June-1995.

!src "../system/acehead.s"
!to "../../build/keymat", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

main = *
   lda #1
   ldy #0
   jsr getarg
   bne loadKeymat

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jmp eputs
   usageMsg = *
   !pet "usage: keymat keymatFilename"
   !byte chrCR,0

loadKeymat = *
   lda #<bssEnd
   ldy #>bssEnd
   sta zw+0
   sty zw+1
   lda #<keymatBuffer
   ldy #>keymatBuffer
   jsr aceFileBload
   bcc +
   lda zp+0
   ldy zp+1
   jsr eputs
   lda #<errBloadMsg
   ldy #>errBloadMsg
   jsr eputs
   rts
+  lda #<keymatBuffer
   ldy #>keymatBuffer
   sta zp+0
   sty zp+1
   jsr aceConKeyMat
   bcc +
   lda #<errmsg
   ldy #>errmsg
   jsr eputs
+  rts

errBloadMsg = *
   !pet ": cannot bload keymatrix file"
   !byte chrCR,0
errmsg = *
   !pet "error: cannot load keymatrix"
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

;=== bss ===

bss = *
keymatBuffer = bss+0
bssEnd = keymatBuffer+616
