;*** crc32b program - by Craig Bruce - 14-Oct-93

!src "../system/acehead.s"
!to "../../build/crc32", cbm
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
   putcBuffer = *
      !fill 1

getchar = *
   ldx #stdin
getc = *
   lda #<getcBuffer
   ldy #>getcBuffer
   sta zp+0
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
   sta zp+0
   rol zp+1
   clc
   lda aceArgv
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
   rts

;===crc32===
crcArg = 2
crcName = 4
inBufLen = 6
bufPtr = 8
bufCount = 10
infile = 12
ch = 13         ;(1)
crcAccum = 34   ;(4)
crcTemp  = 38   ;(4)
crc      = crcAccum

crcMain = *
   ;** check argument count
   lda aceArgc+1
   bne crcEnoughArgs
   lda aceArgc+0
   cmp #2
   bcs crcEnoughArgs

crcUsage = *
   lda #<crcUsageMsg
   ldy #>crcUsageMsg
   jmp eputs

crcUsageMsg = *
   !pet "Usage: crc32 file1 file2 ... fileN"
   !byte chrCR
   !byte 0

crcEnoughArgs = *
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<crcInBuf
   sta inBufLen+0
   lda aceMemTop+1
   sbc #>crcInBuf
   sta inBufLen+1
   jsr crcGen
   ;** main loop
   lda #1
   ldy #0
   sta crcArg+0
   sty crcArg+1
-  jsr checkstop
   lda crcArg+0
   ldy crcArg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta crcName
   sty crcName+1
   ora zp+1
   beq crcExit
   jsr crc32
   bcc +
   jsr crcError
+  inc crcArg+0
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
   lda crcName+0
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

crc32 = *
   ;** open file
   lda crcName+0
   ldy crcName+1
   sta zp+0
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

crcBody = *
   ldx #3
   lda #$ff
-  sta crcAccum,x
   dex
   bpl -
   lda #0
   sta bufCount+0
   sta bufCount+1

   ;***scan file

   nextChar = *
   jsr getByte
   bcs crcPrint
   sta ch

;** crc = 0xFFFFFFFF;
;** while( (c=getc(fp)) != EOF ) {
;**     crc = (crc>>8) & 0x00FFFFFF ^ crcTable[ (crc^c) & 0xFF ];
;** }
;** return( crc^0xFFFFFFFF );

   lda crcAccum+0       ;.X = (crc^c) & 0xFF
   eor ch
   tax

   lda crcAccum+1       ;crc = (crc>>8) & 0x00FFFFFF ^ crcTable[ .X ]
   eor crcTable0,x
   sta crcAccum+0
   lda crcAccum+2
   eor crcTable1,x
   sta crcAccum+1
   lda crcAccum+3
   eor crcTable2,x
   sta crcAccum+2
   lda crcTable3,x
   sta crcAccum+3
   jmp nextChar

   crcPrint = *
   lda #<resMsg1+8
   ldy #>resMsg1+8
   sta syswork+0
   sty syswork+1
   ldy #8
   ldx #3
-  lda crcAccum,x
   eor #$ff
   pha
   lsr
   lsr
   lsr
   lsr
   jsr putHex
   pla
   jsr putHex
   dex
   bpl -
   lda #<resMsg1
   ldy #>resMsg1
   jsr puts
   lda crcName+0
   ldy crcName+1
   jsr puts
   lda #<resMsg2
   ldy #>resMsg2
   jsr puts
   clc
   rts

putHex = *
   and #$0f
   ora #$30
   cmp #$3a
   bcc +
   adc #6
+  sta resMsg1,y
   iny
   rts

resMsg1 = *
   !pet "crc32 = 12345678 for "
   !byte chrQuote,0
resMsg2 = *
   !byte chrQuote,chrCR,0

;** poly = 0xEDB88320L;
;** for (i=0; i<256; i++) {
;**     crc = i;
;**     for (j=8; j>0; j--) {
;**         if (crc&1) {
;**             crc = (crc >> 1) ^ poly;
;**         } else {
;**             crc >>= 1;
;**         }
;**     }
;**     crcTable[i] = crc;
;** }

crcGen = *
   ;** generate CRC table at runtime
   ldy #0
-- ldx #0
   sty crc+0
   stx crc+1
   stx crc+2
   stx crc+3

   ldx #8
-  lsr crc+3
   ror crc+2
   ror crc+1
   ror crc+0
   bcc +
   lda crc+0
   eor #$20
   sta crc+0
   lda crc+1
   eor #$83
   sta crc+1
   lda crc+2
   eor #$b8
   sta crc+2
   lda crc+3
   eor #$ed
   sta crc+3
+  dex
   bne -

   lda crc+0
   sta crcTable0,y
   lda crc+1
   sta crcTable1,y
   lda crc+2
   sta crcTable2,y
   lda crc+3
   sta crcTable3,y
   iny
   bne --
   rts

getByte = *
   lda bufCount+0
   ora bufCount+1
   beq getByteFillBuf
   ldy #0
   lda (bufPtr),y
   inc bufPtr
   bne +
   inc bufPtr+1
+  ldx bufCount+0
   bne +
   dec bufCount+1
+  dec bufCount+0
   clc
   rts

getByteFillBuf = *
   jsr checkstop
   lda #<crcInBuf
   ldy #>crcInBuf
   sta zp+0
   sty zp+1
   sta bufPtr+0
   sty bufPtr+1
   lda inBufLen+0
   ldy inBufLen+1
   ldx infile
   jsr read
   beq +
   bcs +
   sta bufCount+0
   sty bufCount+1
   jmp getByte
+  sec
   rts

;===the end===
crcBss    = *
crcTable0 = crcBss+0
crcTable1 = crcTable0+256
crcTable2 = crcTable1+256
crcTable3 = crcTable2+256
crcInBuf  = crcTable3+256
