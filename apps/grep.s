;*** grep program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:grep"

jmp grepMain
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $40

chrQuote = $22
maxLineLen = 2049

inverseFlag .buf 1
ignoreCaseFlag .buf 1
displayFilenameFlag .buf 1
anchorLeftFlag .buf 1
anchorRightFlag .buf 1
stringLen .buf 1

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

;===grep===
grepArg = 2
grepName = 4
inBufLen = 6
grepString = 8

grepMain = *
   ;** check argument count
   lda #0
   sta inverseFlag
   sta ignoreCaseFlag
   sta displayFilenameFlag
   sta anchorLeftFlag
   sta anchorRightFlag
   lda aceArgc+1
   bne grepEnoughArgs
   lda aceArgc
   cmp #3
   bcs grepEnoughArgs

grepUsage = *
   lda #<grepUsageMsg
   ldy #>grepUsageMsg
   jmp eputs

grepUsageMsg = *
   .asc "usage: grep [-[i][v]] [^]substr[$] files"
   .byte chrCR,0

grepEnoughArgs = *
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<grepInBuf
   sta inBufLen
   lda aceMemTop+1
   sbc #>grepInBuf
   sta inBufLen+1
   ;** main loop
   lda #1
   ldy #0
   sta grepArg
   sty grepArg+1
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne substrArg
   inc grepArg
-  iny
   lda (zp),y
   beq substrArg
   cmp #"i"
   bne +
   lda #$ff
   sta ignoreCaseFlag
+  cmp #"v"
   bne +
   lda #$ff
   sta inverseFlag
+  jmp -

   substrArg = *
   lda grepArg
   ldy #0
   jsr getarg
   lda zp
   ldy zp+1
   sta grepString
   sty grepString+1
   bit ignoreCaseFlag
   bpl +
   jsr foldString
+  jsr checkAnchors
   inc grepArg
   
   firstArg = *
   lda grepArg
   ldy #0
   jsr getarg
   lda zp
   ora zp+1
   bne +
   jmp grepUsage
+  clc
   lda grepArg
   adc #1
   ldy #0
   jsr getarg
   lda zp
   ora zp+1
   beq nextArg
   lda #$ff
   sta displayFilenameFlag

   nextArg = *
   jsr checkstop
   lda grepArg
   ldy grepArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta grepName
   sty grepName+1
   ora zp+1
   beq grepExit
   jsr grep
   bcc +
   jsr grepError
+  inc grepArg
   bne +
   inc grepArg+1
+  jmp nextArg

grepExit = *
   rts

foldString = *
   ldy #0
-  lda (grepString),y
   bne +
   rts
+  cmp #"A"
   bcc +
   cmp #"Z"+1
   bcs +
   sec
   sbc #"A"-"a"
   sta (grepString),y
+  iny
   bne -
   rts

checkAnchors = *
   ldy #0
   lda (grepString),y
   cmp #"^"
   bne +
   lda #$ff
   sta anchorLeftFlag
   inc grepString
   bne +
   inc grepString+1
+  ldy #255
-  iny
   lda (grepString),y
   bne -
   sty stringLen
   dey
   cpy #255
   beq +
   lda (grepString),y
   cmp #"$"
   bne +
   lda #$ff
   sta anchorRightFlag
   lda #0
   sta (grepString),y
   sty stringLen
+  rts

grepError = *
   lda #<grepErrorMsg1
   ldy #>grepErrorMsg1
   jsr eputs
   lda grepName
   ldy grepName+1
   jsr eputs
   lda #<grepErrorMsg2
   ldy #>grepErrorMsg2
   jmp eputs

grepErrorMsg1 = *
   .asc "Error reading file "
   .byte chrQuote,0

grepErrorMsg2 = *
   .byte chrQuote,chrCR,0

bufPtr = 10
bufCount = 12
infile = 14

grep = *
   ;** open file
   lda grepName
   ldy grepName+1
   sta zp
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   ;** encode file
   jsr grepBody
   ;** close file
   lda infile
   jsr close
   rts

lineLen = $40      ;(2)
linePtr = $42      ;(2)

grepBody = *
   lda #0
   sta bufCount
   sta bufCount+1
-  jsr getline
   bcc +
   rts
+  jsr checkLine
   bit inverseFlag
   bmi +
   bcc -
   jsr writeLine
   jmp -
+  bcs -
   jsr writeLine
   jmp -

lineReset = $44   ;(2)
stringPos = $46   ;(1)

checkLine = *  ;() : .CC=no_match, .CS=match
   bit anchorRightFlag
   bpl checkSubstr
   clc
   lda linePtr
   adc lineLen
   sta linePtr
   lda linePtr+1
   adc lineLen+1
   sta linePtr+1
   sec
   lda linePtr
   sbc stringLen
   sta linePtr
   lda linePtr+1
   sbc #0
   sta linePtr+1

   checkSubstr = *
   lda linePtr
   ldy linePtr+1
   sta lineReset
   sty lineReset+1
   inc lineReset
   bne +
   inc lineReset+1
+  ldy #0
   sty stringPos

   checkChar = *
+  ldy stringPos
   lda (grepString),y
   beq endOfString
   ldy #0
   cmp (linePtr),y
   bne mismatch
-  inc stringPos
   inc linePtr
   bne +
   inc linePtr+1
+  jmp checkChar

   mismatch = *
   ldy #0
   lda (linePtr),y
   beq endOfLine
   bit ignoreCaseFlag
   bpl +
   cmp #"A"
   bcc +
   cmp #"Z"+1
   bcs +
   sec
   sbc #"A"-"a"
   ldy stringPos
   cmp (grepString),y
   beq -
+  lda lineReset
   ldy lineReset+1
   sta linePtr
   sty linePtr+1
   jmp checkSubstr

   endOfLine = *
   clc
   rts

   endOfString = *
   bit anchorLeftFlag
   bmi +
   sec
   rts
+  lda lineReset
   bne +
   dec lineReset+1
+  dec lineReset
   lda lineReset
   cmp #<grepLine
   bne endOfLine
   lda lineReset+1
   cmp #>grepLine
   bne endOfLine
   sec
   rts

writeLine = *
   lda displayFilenameFlag
   beq +
   lda grepName
   ldy grepName+1
   jsr puts
   lda #":"
   jsr putchar
+  lda #<grepLine
   ldy #>grepLine
   sta zp
   sty zp+1
   lda lineLen
   ldy lineLen+1
   ldx #stdout
   jsr write
   lda #chrCR
   jsr putchar
   rts

getline = *  ;() : lineLen, linePtr
   ;** ignores chars beyond max line len, ignores last line not ending in CR,
   ;** line ends with \0
   lda #0
   sta lineLen
   sta lineLen+1
   lda #<grepLine
   ldy #>grepLine
   sta linePtr
   sty linePtr+1
-  jsr getByte
   bcc +
   rts
+  cmp #chrCR
   bne +
   lda #0
   ldy #0
   sta (linePtr),y
   lda #<grepLine
   ldy #>grepLine
   sta linePtr
   sty linePtr+1
   clc
   rts
+  ldx lineLen+1
   cpx #>maxLineLen
   bcs -
   ldy #0
   sta (linePtr),y
   inc linePtr
   bne +
   inc linePtr+1
+  inc lineLen
   bne +
   inc lineLen+1
+  jmp -

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
   lda #<grepInBuf
   ldy #>grepInBuf
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
grepBss = *
grepLine = grepBss
grepInBuf  = grepLine+maxLineLen
