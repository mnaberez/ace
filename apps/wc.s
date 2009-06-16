;*** word counter program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:wc"

jmp wcMain
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $60

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
   .asc "<Stopped>"
   .byte chrCR,0

;===word count===
wcArg = 2
wcName = 4
inBufLen = 6
files = $50

wcMain = *
   ;** check argument count
   lda aceArgc+1
   bne wcEnoughArgs
   lda aceArgc
   cmp #2
   bcs wcEnoughArgs

wcUsage = *
   lda #<wcUsageMsg
   ldy #>wcUsageMsg
   jmp eputs

wcUsageMsg = *
   .asc "Usage: wc file1 file2 ... fileN"
   .byte chrCR,0

wcEnoughArgs = *
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<wcInBuf
   sta inBufLen
   lda aceMemTop+1
   sbc #>wcInBuf
   sta inBufLen+1
   ;** main loop
   lda #1
   ldy #0
   sta wcArg
   sty wcArg+1
   sty files
   jsr totInit
-  jsr checkstop
   lda wcArg
   ldy wcArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta wcName
   sty wcName+1
   ora zp+1
   beq wcExit
   jsr wcFile
   bcs +
   lda files
   bmi +
   inc files
+  bcc +
   jsr wcError
+  inc wcArg
   bne +
   inc wcArg+1
+  jmp -

wcExit = *
   lda files
   cmp #2
   bcc +
   jsr reportTotal
+  rts

wcError = *
   lda #<wcErrorMsg1
   ldy #>wcErrorMsg1
   jsr eputs
   lda wcName
   ldy wcName+1
   jsr eputs
   lda #<wcErrorMsg2
   ldy #>wcErrorMsg2
   jmp eputs

wcErrorMsg1 = *
   .asc "Error reading file "
   .byte chrQuote,0

wcErrorMsg2 = *
   .byte chrQuote,chrCR,0

bufCount = 10
infile = 12

wcFile = *
   ;** open file
   lda wcName
   ldy wcName+1
   sta zp
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   ;** count file
   jsr wcInit
   jsr wcBody
   ;** close file
   lda infile
   jsr close
   jsr wcReport
   jsr addToTotal
   rts

wcBody = *
-  jsr checkstop
   lda #<wcInBuf
   ldy #>wcInBuf
   sta zp+0
   sty zp+1
   lda #254
   ldy #0
   ldx infile
   jsr read
   sta bufCount
   sty bufCount+1
   beq +
   jsr wcScanBuffer
   jmp -
+  rts

wcInWord = $4c ;(1)
wcLines = $40  ;(3)
wcWords = $44  ;(3)
wcBytes = $48  ;(3)
totLines = $20  ;(3)
totWords = $24  ;(3)
totBytes = $28  ;(3)

wcInit = *
   lda #0
   ldx #11
-  sta wcLines,x
   dex
   bpl -
   sta wcInWord
   rts

totInit = *
   lda #0
   ldx #11
-  sta totLines,x
   dex
   bpl -
   rts

wcScanBuffer = *
   ldy #0
   cpy bufCount
   bne +
   rts
+  ldx wcInWord
-  lda wcInBuf,y
   cmp #chrCR
   bne +
   inc wcLines
   bne +
   inc wcLines+1
   bne +
   inc wcLines+2 
   bne +
   inc wcLines+3
+  cmp #33
   bcs isLetter
   cmp #" "
   beq isDelimiter
   cmp #chrCR
   beq isDelimiter
   cmp #9
   beq isDelimiter

   isLetter = *
   cpx #1
   beq scanCont
   ldx #1
   inc wcWords
   bne scanCont
   inc wcWords+1
   bne scanCont
   inc wcWords+2
   bne scanCont
   inc wcWords+3
   jmp scanCont

   isDelimiter = *
   ldx #0

   scanCont = *
   iny
   cpy bufCount
   bcc -
   clc
   lda wcBytes
   adc bufCount
   sta wcBytes
   bcc +
   inc wcBytes+1
   bne +
   inc wcBytes+2
   bne +
   inc wcBytes+3
+  stx wcInWord
   rts

wcReport = *
   ldx #wcLines
   lda #6
   jsr putnum
   ldx #wcWords
   lda #6
   jsr putnum
   ldx #wcBytes
   lda #7
   jsr putnum
   lda wcName
   ldy wcName+1
   jsr puts
   lda #chrCR
   jsr putchar
   rts

putnum = *
   ldy #<numbuf
   sty zp+0
   ldy #>numbuf
   sty zp+1
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr puts
   lda #" "
   jsr putchar
   rts

addToTotal = *
   clc
   ldx #0
-  lda totLines,x
   adc wcLines,x
   sta totLines,x
   inx
   txa
   and #$03
   bne -
   clc
   ldx #0
-  lda totWords,x
   adc wcWords,x
   sta totWords,x
   inx
   txa
   and #$03
   bne -
   clc
   ldx #0
-  lda totBytes,x
   adc wcBytes,x
   sta totBytes,x
   inx
   txa
   and #$03
   bne -
   rts

reportTotal = *
   lda #<totalMsg
   ldy #>totalMsg
   sta wcName
   sty wcName+1
   ldx #11
-  lda totLines,x
   sta wcLines,x
   dex
   bpl -
   jsr wcReport
   rts
   totalMsg = *
   .asc "<total>"
   .byte 0

;===the end===
wcBss = *
numbuf = wcBss
wcInBuf = numbuf+12
