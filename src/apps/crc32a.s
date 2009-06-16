;*** crc32a program - by Craig Bruce

!src "../system/acehead.s"
!to "../../build/crc32a", cbm
!convtab pet

*= aceAppAddress

jmp crcMain
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $40

chrQuote = $22

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
   ldx #stdout
putc = *
   sta putcBuffer
   lda #<putcBuffer
   ldy #>putcBuffer
   sta zp
   sty zp+1
   lda #1
   ldy #0
   jmp write
   putcBuffer = *
      !fill 1

getchar = *
   ldx #stdin
getc = *
   lda #<getcBuffer
   ldy #>getcBuffer
   sta zp
   sty zp+1
   lda #1
   ldy #0
   jsr read
   beq +
   lda getcBuffer
   rts
+  sec
   rts
   getcBuffer = *
      !fill 1

getarg = *
   sty zp+1
   asl
   sta zp
   rol zp+1
   clc
   lda aceArgv
   adc zp
   sta zp
   lda aceArgv+1
   adc zp+1
   sta zp+1
   ldy #0
   lda (zp),y
   tax
   iny
   lda (zp),y
   stx zp
   sta zp+1
   rts

;===crc32===
crcArg = 2
crcName = 4
inBufLen = 6

crcMain = *
   ;** check argument count
   lda aceArgc+1
   bne crcEnoughArgs
   lda aceArgc
   cmp #2
   bcs crcEnoughArgs

crcUsage = *
   lda #<crcUsageMsg
   ldy #>crcUsageMsg
   jmp eputs

crcUsageMsg = *
   !pet "Usage: crc32a file1 file2 ... fileN"
   !byte chrCR
   !byte 0

crcEnoughArgs = *
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<crcInBuf
   sta inBufLen
   lda aceMemTop+1
   sbc #>crcInBuf
   sta inBufLen+1
   ;** main loop
   lda #1
   ldy #0
   sta crcArg
   sty crcArg+1
-  jsr checkstop
   lda crcArg
   ldy crcArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta crcName
   sty crcName+1
   ora zp+1
   beq crcExit
   jsr crc32
   bcc +
   jsr crcError
+  inc crcArg
   bne +
   inc crcArg+1
+  jmp -

crcExit = *
   rts

checkstop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit
   stoppedMsg = *
   !pet "<Stopped>"
   !byte chrCR,0

crcError = *
   lda #<crcErrorMsg1
   ldy #>crcErrorMsg1
   jsr eputs
   lda crcName
   ldy crcName+1
   jsr eputs
   lda #<crcErrorMsg2
   ldy #>crcErrorMsg2
   jmp eputs

crcErrorMsg1 = *
   !pet "Error reading file "
   !byte chrQuote,0

crcErrorMsg2 = *
   !byte chrQuote,chrCR,0

bufPtr = 8
bufCount = 10
infile = 12

crc32 = *
   ;** open file
   lda crcName
   ldy crcName+1
   sta zp
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   ;** encode file
   jsr crcBody
   ;** close file
   lda infile
   jsr close
   rts

;crc32 version 1.0 for the C-128 and C-64 by Craig Bruce 23-May-92

ch = 13         ;(1)
crcAccum = 34   ;(4)

crcLow  = $b7
crcMid1 = $1d
crcMid2 = $c1
crcHigh = $04

crcBody = *
   ldx #3
   lda #0
-  sta crcAccum,x
   dex
   bpl -
   sta bufCount
   sta bufCount+1

   ;***scan file

   nextChar = *
   jsr getByte
   bcs crcPrint
   sta ch
   ldx #8

   nextBit = *
   asl ch
   rol crcAccum+0
   rol crcAccum+1
   rol crcAccum+2
   rol crcAccum+3
   bcc +
   lda crcAccum+0 
   eor #crcLow
   sta crcAccum+0
   lda crcAccum+1
   eor #crcMid1
   sta crcAccum+1
   lda crcAccum+2 
   eor #crcMid2
   sta crcAccum+2
   lda crcAccum+3 
   eor #crcHigh
   sta crcAccum+3
+  dex
   bne nextBit
   jmp nextChar

   crcPrint = *
   lda #<resMsg1+9
   ldy #>resMsg1+9
   sta zp+0
   sty zp+1
   ldx #crcAccum
   lda #10
   jsr aceMiscUtoa
   lda #" "
   sta resMsg1+19
   lda #<resMsg1
   ldy #>resMsg1
   jsr puts
   lda crcName
   ldy crcName+1
   jsr puts
   lda #<resMsg2
   ldy #>resMsg2
   jsr puts
   clc
   rts

resMsg1 = *
   !pet "crc32a = 1234567890 for "
   !byte chrQuote,0
resMsg2 = *
   !byte chrQuote,chrCR,0

getByte = *
   lda bufCount
   ora bufCount+1
   beq getByteFillBuf
   ldy #0
   lda (bufPtr),y
   inc bufPtr
   bne +
   inc bufPtr+1
+  ldx bufCount
   bne +
   dec bufCount+1
+  dec bufCount
   clc
   rts

getByteFillBuf = *
   jsr checkstop
   lda #<crcInBuf
   ldy #>crcInBuf
   sta zp
   sty zp+1
   sta bufPtr
   sty bufPtr+1
   lda inBufLen
   ldy inBufLen+1
   ldx infile
   jsr read
   beq +
   bcs +
   sta bufCount
   sty bufCount+1
   jmp getByte
+  sec
   rts

;===the end===
crcBss = *
crcInBuf  = crcBss
