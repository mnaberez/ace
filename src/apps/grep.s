;*** grep program

;*** crc32b program - by Craig Bruce - 14-Oct-93

!src "../system/acehead.s"
!to "../../build/grep", cbm

*= aceAppAddress

jmp grepMain
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved
!convtab pet

;*** global declarations

libwork = $40

chrQuote = $22
maxLineLen = 2049

inverseFlag = *
   !fill 1
ignoreCaseFlag = *
   !fill 1
displayFilenameFlag = *
   !fill 1
anchorLeftFlag = *
   !fill 1
anchorRightFlag = *
   !fill 1
stringLen = *
   !fill 1
onebyteLine = *
   !fill 1
lineLimitedFlag = *
   !fill 1
countlWritten = *
   !fill 1
lwCutoffFlag = *
   !fill 1
fileShownFlag = *
   !fill 1
unixAsciiFlag = *
   !fill 1

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
   sta lineLimitedFlag
   sta lwCutoffFlag
   sta unixAsciiFlag
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
   !pet "usage: grep [-[i][v]] [^]substr[$] files"
   !byte chrCR,0

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
+  cmp #"l"
   bne +
   lda #$ff
   sta lineLimitedFlag
+  cmp #"u"
   bne +
   lda #$ff
   sta unixAsciiFlag
   lda #$ff
   sta ignoreCaseFlag
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
   lda #0
   sta onebyteLine
   lda grepArg
   ldy grepArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta grepName
   sty grepName+1
   ora zp+1
   beq grepExit
;main routine  !!!!!!!
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
   !pet "Error reading file "
   !byte chrQuote,0

grepErrorMsg2 = *
   !byte chrQuote,chrCR,0

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
   bit fileShownFlag
   bmi +
   lda grepName
   ldy grepName+1
   jsr puts
   lda #<msgNull
   ldy #>msgNull
   jsr puts
+  rts

msgNull = *
   !pet " - <0 lines selected>"
   !byte chrCR,0

lineLen = $40      ;(2)
linePtr = $42      ;(2)

grepBody = *
   lda #0
   sta bufCount
   sta bufCount+1
   sta fileShownFlag
-  inc onebyteLine
   bne gnextLine
   bit lineLimitedFlag
   bpl gnextLine
   rts
gnextLine = *
   jsr checkstop
   jsr getline
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

;sadly checkstop only works part of the time,
;because apparently much of the greping time is
;spent in the system 'read' routine.

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
   rechecklet = *
   ldy stringPos
   cmp (grepString),y
   beq -
-  lda lineReset
   ldy lineReset+1
   sta linePtr
   sty linePtr+1
   jmp checkSubstr

+  bit unixAsciiFlag
   bpl -
   cmp #"a"+$20
   bcc -
   cmp #"z"+$21
   bcs -
   sec
   sbc #$61-"a"
   jmp rechecklet

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
   ldx countlWritten
   cpx #10
   bne +
   lda lwCutoffFlag
   beq +
   rts
+  inx
   stx countlWritten
   lda #$ff
   sta fileShownFlag
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
+  bit unixAsciiFlag
   bpl +
   cmp #chrBOL
   beq linedone
+  cmp #chrCR
   bne +
   linedone = *
   lda #0
   ldy #0
   sta (linePtr),y
   lda #<grepLine
   ldy #>grepLine
   sta linePtr
   sty linePtr+1
   clc
   rts
;  or check line > 80 when -u ...?
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
