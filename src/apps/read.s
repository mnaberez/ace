;*** file reader program

.seq "acehead.s"
.org aceAppAddress
.obj "@:read"

jmp crcMain
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

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
   putcBuffer .buf 1

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
   getcBuffer .buf 1

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

itoaBin = libwork       ;(4)
itoaBcd = libwork+4     ;(5)
itoaFlag = libwork+9    ;(1)
itoaNumber .buf 11
itoa = *  ;( .X=numZpaddr ) : itoaNumber
   ldy #0
-  lda 0,x
   sta itoaBin,y
   inx
   iny
   cpy #4
   bcc - 
   ldx #5
   lda #0
-  sta itoaBcd,x   ;zeros "itoaFlag" too
   dex
   bpl -
   ldy #32
   sei
   sed

   itoaNextBit = *
   asl itoaBin+0
   rol itoaBin+1
   rol itoaBin+2
   rol itoaBin+3
   ldx #4
-  lda itoaBcd,x
   adc itoaBcd,x
   sta itoaBcd,x
   dex
   bpl -
   dey
   bne itoaNextBit
   cld
   cli
   ldx #0
   ldy #0
-  lda itoaBcd,x
   jsr itoaPutHex
   inx
   cpx #5
   bcc -
   lda #0
   sta itoaNumber,y
   rts
   
   itoaPutHex = *
   pha
   lsr
   lsr
   lsr
   lsr
   jsr itoaPutDigit
   pla
   and #$0f

   itoaPutDigit = *
   cmp itoaFlag
   bne +
   cpy #7
   bcs +
   lda #$20
   bne itoaPoke
+  ora #$30
   sta itoaFlag
   
   itoaPoke = *
   sta itoaNumber,y
   iny
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
   .asc "usage: read file1 file2 ... fileN"
   .byte chrCR
   .byte 0

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
-  jsr aceConStopkey
   bcs crcStopped
   lda crcArg
   ldy crcArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta crcName
   sty crcName+1
   ora zp+1
   beq crcExit
   jsr doRead
   bcc +
   jsr crcError
+  inc crcArg
   bne +
   inc crcArg+1
+  jmp -

crcExit = *
   rts

crcStopped = *
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   rts
   stoppedMsg = *
   .asc "<Stopped>"
   .byte chrCR,0

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
   .asc "Error reading file "
   .byte chrQuote,0

crcErrorMsg2 = *
   .byte chrQuote,chrCR,0

bufPtr = 8
bufCount = 10
infile = 12

doRead = *
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
   lda #<readStartMsg
   ldy #>readStartMsg
   jsr eputs
   ;** read file
   jsr readBody
   ;** close file
   lda #<readFinishMsg
   ldy #>readFinishMsg
   jsr eputs
   lda infile
   jsr close
   rts

readStartMsg = *
   .asc "start"
   .byte chrBEL,chrCR,0

readFinishMsg = *
   .byte chrBEL
   .asc "finished"
   .byte chrCR,0

readBody = *
   lda #<crcInBuf
   ldy #>crcInBuf
   sta zp
   sty zp+1
   lda inBufLen+0
   ldy inBufLen+1
   ldx infile
   jsr read
   beq +
   bcs +
   jmp readBody
+  sec
   rts

;===the end===
crcBss = *
crcInBuf  = crcBss
