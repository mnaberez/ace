;*** Mouse Test - by Craig Bruce - 08-Sep-1995

;this file is in ACE-assembler format

.seq "acehead.s"
.org aceAppAddress
.obj "@0:mousetest"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

IMAGE_COLS = 5
IMAGE_ROWS = 11
IMAGE_SIZE = IMAGE_ROWS * IMAGE_COLS

mouseX       = $04  ;(4)
mouseY       = $08  ;(4)
mouseButtons = $0c  ;(1)
imageX       = $10  ;(2)
imageY       = $12  ;(2)
p0           = $16  ;(2)
p1           = $18  ;(2)
scrX         = $1a  ;(1)
scrY         = $1c  ;(2)
temp         = $20  ;(4)

main = *
   ldx #50
   lda #$00
-  sta mouseX,x
   dex
   bpl -

   lda #$01
   ldx aceArgc+0
   cpx #2
   bcc +
   lda #$00
+  ldx #$00
   ldy #$0e
   jsr aceGrScreen
   sta scrX
   lda syswork+0
   ldy syswork+1
   sta scrY+0
   sty scrY+1
   lda #$00
   jsr aceGrFill
   jsr InitImages
   jsr DisplayImage

   lda aceArgc+0
   cmp #3
   bcc again
   jsr speedTest

   again = *
   jsr checkStop
   jsr aceConMouse
   sta mouseButtons
   lda syswork+0
   ldy syswork+1
   sta mouseX+0
   sty mouseX+1
   lda syswork+2
   ldy syswork+3
   sta mouseY+0
   sty mouseY+1
   jsr RedisplayImage
   jmp again

   lda #<msg1
   ldy #>msg1
   jsr puts
   ldx #mouseX
   jsr putnum
   lda #<msg2
   ldy #>msg2
   jsr puts
   ldx #mouseY
   jsr putnum
   lda #<msg3
   ldy #>msg3
   jsr puts
   ldx #mouseButtons
   jsr putnum
   lda #<msg4
   ldy #>msg4
   jsr puts
   jsr checkStop
   jmp again

msg1 = *
   .asc "mouseX="
   .byte 0
msg2 = *
   .asc ", mouseY="
   .byte 0
msg3 = *
   .asc ", buttons="
   .byte 0
msg4 = *
   .asc "       "
   .byte chrBOL,0

;******** standard library ********
puts = *
   ldx #stdout
fputs = *
   sta zp
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
   stx xsave
   sty ysave
   ldx #stdout
   jsr putc
   ldx xsave
   ldy ysave
   rts
   xsave .buf 1
   ysave .buf 1

putc = *
   sta putcBuffer
   lda #<putcBuffer
   ldy #>putcBuffer
   sta zp
   sty zp+1
   lda #1
   ldy #0
   jmp write
   putcBuffer .buf 1

checkStop = *
   jsr aceConStopkey
   bcs ++
   jsr aceConKeyAvail
   bcc +
   rts
+  jsr aceConGetkey
   cmp #"q"
   beq +
   rts
+  jsr aceGrExit
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit
   stoppedMsg = *
   .byte chrCR
   .asc "<Stopped>"
   .byte chrCR,0

putnum = *
   ldy #<numbuf
   sty zp+0
   ldy #>numbuf
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr puts
   rts
   numbuf .buf 11

RedisplayImage = *  ;( mouseX, mouseY )
   ldx #1
-  lda mouseX,x
   cmp imageX,x
   bne +
   lda mouseY,x
   cmp imageY,x
   bne +
   dex
   bpl -
   rts
+  lda imageY+0
   ldy imageY+1
   sta syswork+0
   sty syswork+1
   lda #<IMAGE_ROWS
   ldy #>IMAGE_ROWS
   sta syswork+2
   sty syswork+3
   lda #0
   sta syswork+4
   lda imageX+1
   sta temp
   lda imageX+0
   lsr temp
   ror
   lsr temp
   ror
   lsr temp
   ror
   tax
   lda #$10
   ldy #IMAGE_COLS
   cli
   jsr grOpClip

DisplayImage = *
   ldx #1
-  lda mouseX,x
   sta imageX,x
   lda mouseY,x
   sta imageY,x
   dex
   bpl -

   lda imageX+0
   and #$07
   asl
   tax
   lda images+0,x
   sta syswork+6
   lda images+1,x
   sta syswork+7
   lda imageY+0
   ldy imageY+1
   sta syswork+0
   sty syswork+1
   lda #<IMAGE_ROWS
   ldy #>IMAGE_ROWS
   sta syswork+2
   sty syswork+3
   lda #0
   sta syswork+4
   lda imageX+1
   sta temp
   lda imageX+0
   lsr temp
   ror
   lsr temp
   ror
   lsr temp
   ror
   tax
   lda #$40
   ldy #IMAGE_COLS
   jsr grOpClip
   cli
   rts

grOpClip = *
   pha
   stx temp
   sec
   lda scrX
   sbc temp
   sta temp
   cpy temp
   bcc +
   sty temp+1
   ldy temp
   sec
   lda temp+1
   sbc temp
   sta syswork+4
+  sec
   lda scrY+0
   sbc syswork+0
   sta temp+0
   lda scrY+1
   sbc syswork+1
   sta temp+1
   lda syswork+2
   cmp temp+0
   lda syswork+3
   sbc temp+1
   bcc +
   lda temp+0
   sta syswork+2
   lda temp+1
   sta syswork+3
+  pla
   jmp aceGrOp

curImage .buf 1
pHi      .buf 1
cHi      .buf 1

InitImages = *
   lda #1
   sta curImage
   lda #<bmImage0
   ldy #>bmImage0
   sta p0+0
   sty p0+1
   initImLoop = *
   lda curImage
   asl
   tax
   lda images+0,x
   sta p1+0
   lda images+1,x
   sta p1+1
   sta pHi
   lda #0
   sta cHi

   clc
   php
   ldy #0
-  lda (p0),y
   plp
   ror
   php
   sta (p1),y
   iny
   bne +
   inc p0+1
   inc p1+1
   inc cHi
+  cpy #<IMAGE_SIZE
   bne -
   lda cHi
   cmp #>IMAGE_SIZE
   bne -
   plp

   lda p1+0
   ldy pHi
   sta p0+0
   sty p0+1
   inc curImage
   lda curImage
   cmp #8
   bcc initImLoop
   rts

speedTest = *
   lda #chrBEL
   jsr aceConPutchar
   lda #255
   sta curImage
-  jsr DisplayImage
   dec curImage
   bne -
   lda #chrBEL
   jsr aceConPutchar
   rts

images .word bmImage0,bmImage1,bmImage2,bmImage3
       .word bmImage4,bmImage5,bmImage6,bmImage7

bmImage0 = *  ;cols=5, rows=11
   .byte %11111111,%11100000,%00000000,%11111111,%00000000
   .byte %11111111,%10000000,%00000000,%00000011,%00000000
   .byte %11111111,%10000001,%10000000,%00000011,%00000000
   .byte %11111111,%11000001,%10000000,%00000011,%00000000
   .byte %11100011,%11100001,%10000000,%00000000,%00000000
   .byte %10000000,%11001111,%11111100,%00000000,%00000000
   .byte %00000000,%00000001,%10000000,%00000000,%00000000
   .byte %11000000,%00000001,%10000000,%00000011,%00000000
   .byte %11000000,%00000001,%10000000,%00000011,%00000000
   .byte %11000000,%00000000,%00000000,%00000011,%00000000
   .byte %11111111,%00000000,%00000000,%11111111,%00000000

bmImage1 = *
bmImage2 = bmImage1+IMAGE_SIZE
bmImage3 = bmImage2+IMAGE_SIZE
bmImage4 = bmImage3+IMAGE_SIZE
bmImage5 = bmImage4+IMAGE_SIZE
bmImage6 = bmImage5+IMAGE_SIZE
bmImage7 = bmImage6+IMAGE_SIZE
