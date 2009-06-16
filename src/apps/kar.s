;*** KAR file encoder version 1.00 - by Craig Bruce - 10-Sep-94

;kar [-help] file ...

;Kevin's ARchiver format, created by Kevin Phillips and Craig Bruce, 1991

!src "../system/acehead.s"
!to "../../build/kar", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

chrQUOTE = $22

arg          =  2 ;(2)  ;current argument number
name         =  4 ;(2)  ;name of file being kared
inBufLen     = 10 ;(2)  ;total size of input buffer
cbufLen      = 12 ;(1)  ;total size of line-counting input buffer
fileCount    = 24 ;(4)  ;files to put into archive
lineCount    = 28 ;(4)  ;lines in current file
inFile       = 32 ;(1)  ;input file descriptor
lastFileChar = 33 ;(1)  ;last char read while counting a file
work         = 112 ;(16);temporary work area, lowest level

;===main===

main = *
   ;** check for a large enough TPA
   sec
   lda #<bssEnd
   cmp aceMemTop+0
   lda #>bssEnd
   sbc aceMemTop+1
   bcs +
   jmp mainInit
+  lda #<tpaMsg
   ldy #>tpaMsg
   jsr eputs
die = *
   lda #1
   ldx #0
   jmp aceProcExit

tpaMsg = *
   !pet "Insufficient program space to run"
   !byte chrCR,0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg = *
   !pet "usage: kar [-help] file ..."
   !byte chrCR,0

mainInit = *
   lda #0
   sta arg+0
   sta arg+1
   jsr putFileCount

   mainNext = *
   jsr checkStop
   inc arg+0
   bne +
   inc arg+1
+  lda arg+0
   ldy arg+1
   jsr getarg
   beq mainExit
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   jsr kar
   jmp mainNext

mainExit = *
   rts

checkStop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   jmp die

   stoppedMsg = *
   !pet "<Stopped>"
   !byte chrCR,0

putFileCount = *
   lda aceArgc+0
   ldy aceArgc+1
   ldx #$00
   sta fileCount+0
   sty fileCount+1
   stx fileCount+2
   stx fileCount+3
   lda #1
   ldy #0
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jmp usage
+  lda fileCount+0
   bne +
   dec fileCount+1
+  dec fileCount+0
   ldx #fileCount
   ;xx fall through
   
putnum = *  ;( .X=zpoff )
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr puts
   lda #chrCR
   jsr putchar
   rts

   numbuf = *
      !fill 11

kar = *
   jsr echo
   lda name+0
   ldy name+1
   jsr puts
   lda #chrCR
   jsr putchar
   jsr countLines
   bcc +
   rts
+  ldx #lineCount
   jsr putnum
   ;** open file
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   jsr echoName
   lda #<openErrMsg
   ldy #>openErrMsg
   jsr eputs
   rts
+  sta inFile
   jsr karBody
   lda inFile
   jsr close
   rts
   openErrMsg = *
   !pet ": cannot open"
   !byte chrCR,0

echo = *
   lda #<echoMsg1
   ldy #>echoMsg1
   jsr eputs
   jsr echoName
   lda #<echoMsg2
   ldy #>echoMsg2
   jmp eputs
echoMsg1 = *
   !pet "karing "
   !byte chrQUOTE,0
echoMsg2 = *
   !byte chrQUOTE,chrCR,0

echoName = *
   lda name+0
   ldy name+1
   jsr eputs
   rts

karBody = *
   jsr initInBuf
-  jsr checkStop
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
   lda inBufLen+0
   ldy inBufLen+1
   ldx inFile
   jsr read
   bcs ++
   beq +
   pha
   tya
   pha
   jsr checkStop
   pla
   tay
   pla
   ldx #stdout
   jsr write
   bcc -
   bcs ++
+  rts
++ jsr echoName
   lda #<ioErrMsg
   ldy #>ioErrMsg
   jsr eputs
   rts
   ioErrMsg = *
   !pet ": read/write error"
   !byte chrCR,0

;=== line counting portion ===

countLines = *
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   jsr echoName
   lda #<openErrMsg
   ldy #>openErrMsg
   jsr eputs
   sec
   rts
+  sta inFile
   jsr countInit
   jsr countBody
   lda inFile
   jsr close
   rts

countInit = *
   lda #0
   ldx #3
-  sta lineCount,x
   dex
   bpl -
   rts

countBody = *
-  jsr checkStop
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
   lda #254
   ldy #0
   ldx inFile
   jsr read
   sta cbufLen
   beq +
   jsr countScanBuffer
   jmp -
+  rts

countScanBuffer = *  ;this seems to cause all of the problems...?
   ldy #0
-  lda inBuf,y
   cmp #chrCR
   bne +
   inc lineCount+0
   bne +
   inc lineCount+1
   bne +
   inc lineCount+2 
   bne +
   inc lineCount+3
+  iny
   cpy cbufLen
   bcc -
   sta lastFileChar
   rts

;=== standard library ===

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

eputchar = *
   ldx #stderr
   jmp putc
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
   putcBuffer = *
      !fill 1

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

initInBuf = *
   sec
   lda aceMemTop+0
   sbc #<inBuf
   sta inBufLen+0
   lda aceMemTop+1
   sbc #>inBuf
   sta inBufLen+1
   rts

;===bss===

bss    = *
inBuf  = bss+0
bssEnd = inBuf+256
