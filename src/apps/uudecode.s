;*** uudecode program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:uudecode"

jmp uudecodeMain
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $60

chrLF = $0a
chrQuote = $22

asciiFile .buf 1
temp .buf 1

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

;===uudecode===
uudArg = 2
uudName = 4
inBufLen = 6

uudecodeMain = *
   ;** check argument count
   lda aceArgc+1
   bne uudEnoughArgs
   lda aceArgc
   cmp #2
   bcs uudEnoughArgs

uudUsage = *
   lda #<uudUsageMsg
   ldy #>uudUsageMsg
   jmp puts

uudUsageMsg = *
   .asc "Usage: uudecode file1 file2 ... fileN"
   .byte chrCR,0

uudEnoughArgs = *
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<uudInBuf
   sta inBufLen
   lda aceMemTop+1
   sbc #>uudInBuf
   sta inBufLen+1
   ;** main loop
   lda #1
   ldy #0
   sta uudArg
   sty uudArg+1
-  jsr checkstop
   lda uudArg
   ldy uudArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta uudName
   sty uudName+1
   ora zp+1
   beq uudExit
   jsr uudEcho
   jsr uudecode
   bcc +
   jsr uudError
+  inc uudArg
   bne +
   inc uudArg+1
+  jmp -

uudExit = *
   rts

uudError = *
   lda #<uudErrorMsg1
   ldy #>uudErrorMsg1
   jsr eputs
   lda uudName
   ldy uudName+1
   jsr eputs
   lda #<uudErrorMsg2
   ldy #>uudErrorMsg2
   jmp eputs

uudErrorMsg1 = *
   .asc "Error attempting to uudecode file "
   .byte chrQuote,0
uudErrorMsg2 = *
   .byte chrQuote,chrCR,0

uudEcho = *
   lda #<uudEchoMsg1
   ldy #>uudEchoMsg1
   jsr eputs
   lda uudName
   ldy uudName+1
   jsr eputs
   lda #<uudEchoMsg2
   ldy #>uudEchoMsg2
   jmp eputs

uudEchoMsg1 = *
   .asc "uudecoding file "
   .byte chrQuote,0

uudEchoMsg2 = *
   .byte chrQuote
   .asc "..."
   .byte chrCR,0

bufPtr = 8
bufCount = 10
infile = 12
outfile = 13

uudecode = *
   ;** open file
   lda uudName
   ldy uudName+1
   sta zp
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   ;** decode file
   jsr uudecodeBody
   ;** close file
   lda infile
   jsr close
   rts

uudecodeBody = *
   lda #0
   sta bufCount
   sta bufCount+1
   sta hitLastLine
   sta lastLineTerminator

   ;** search for "begin" line
   uudSearchLine = *
   jsr getline
   bcc +
   rts
+  ldx #0
   lda uudInLine
   cmp #"b"
   beq +
   ldx #asciiBegin-petsciiBegin
+  ldy #0
-  lda uudInLine,y
   cmp petsciiBegin,x
   bne uudSearchLine
   inx
   iny
   cpy #6
   bcc -
   ldy #0
   cpx #asciiBegin-petsciiBegin+1
   bcc +
   ldy #$ff
+  sty asciiFile
   lda uudInLine+9
   cmp #" "
   bne uudSearchLine
   ldx #0
-  lda uudInLine+6,x
   cmp #"0"
   bcc uudSearchLine
   cmp #"8"
   bcs uudSearchLine
   inx
   cpx #3
   bcc -
   jmp processBegin

   petsciiBegin = *
   .asc "begin "
   asciiBegin = *
   .byte $62,$65,$67,$69,$6e,$20
   
   ;** process "begin" line
   processBegin = *
   lda asciiFile
   beq +
   ;** jsr convert filename
+  jsr makePetsciiName
   jsr defaultPrgFile
   jsr echoExtractName
   lda #<uudInLine+10
   ldy #>uudInLine+10
   sta zp
   sty zp+1
   lda #"w"
   jsr open
   bcc openOk
   lda errno
   cmp #aceErrFileExists
   beq +
-  jsr reportOpenError
   jmp uudSearchLine
+  jsr scratchFile
   lda #"w"
   jsr open
   bcs -

   openOk = *
   sta outfile
 
   ;** read uuencoded data
-  jsr getline
   bcs uudFinishFile
   jsr uuConvertLine
   bcc +
   ;** report invalid characters
   lda #<badCharsMsg
   ldy #>badCharsMsg
   jsr eputs
   jmp -
+  lda uudInLine
   beq uudFinishFile
   jsr uuCrunchLine
   jsr uuWriteLine
   jmp -

   ;** finish with file
   uudFinishFile = *
   lda outfile
   jsr close
   ;** process for another file
   jmp uudSearchLine

badCharsMsg = *
   .asc "warning: bad characters in line; ignoring line."
   .byte chrCR,0

;%%%
makePetsciiName = *
   bit asciiFile
   bmi +
   rts
+  ldx #0
-  lda uudInLine+10,x
   beq +
   jsr convAsc2Pet
   sta uudInLine+10,x
   inx
   bne -
+  rts

convAsc2Pet = *
   and #$7f
   cmp #$60
   bcc +
   clc
   adc #$c0-$60
+  tay
   and #$7f
   cmp #"a"
   bcs +
-  tya
   rts
+  cmp #"z"+1
   bcs -
   tya
   eor #$80
   rts

defaultPrgFile = *
   ldx #0
-  lda uudInLine+10,x
   beq +
   inx
   bne -
+  lda uudInLine+8,x
   cmp #","
   bne +
   rts
+  lda #","
   sta uudInLine+10,x
   lda #"p"
   sta uudInLine+11,x
   lda #0
   sta uudInLine+12,x
   rts

echoExtractName = *
   lda #<echoExtractMsg1
   ldy #>echoExtractMsg1
   jsr eputs
   lda #<uudInLine+10
   ldy #>uudInLine+10
   jsr eputs
   lda #<echoExtractMsg2
   ldy #>echoExtractMsg2
   jmp eputs

echoExtractMsg1 = *
   .asc "extracting file "
   .byte chrQuote,0

echoExtractMsg2 = *
   .byte chrQuote
   .asc "..."
   .byte chrCR,0

reportOpenError = *
   lda zp
   ldy zp+1
   jsr eputs
   lda #<reportOpenErrorMsg
   ldy #>reportOpenErrorMsg
   jsr eputs
   rts

   reportOpenErrorMsg = *
   .asc ": cannot open; skipping this file."
   .byte chrCR,0

scratchFile = *
   lda #<scratchFileMsg1
   ldy #>scratchFileMsg1
   jsr eputs
   lda #<uudInLine+10
   ldy #>uudInLine+10
   jsr eputs
   lda #<scratchFileMsg2
   ldy #>scratchFileMsg2
   jsr eputs
   jsr getchar
   pha
-  cmp #chrCR
   beq +
   jsr getchar
   jmp -
+  lda #<uudInLine+10
   ldy #>uudInLine+10
   sta zp
   sty zp+1
   pla
   cmp #"y"
   beq +
   cmp #"Y"
   beq +
   rts
+  jsr aceFileRemove
   rts

   scratchFileMsg1 = *
   .asc "Overwrite existing file "
   .byte chrQuote,0
   scratchFileMsg2 = *
   .byte chrQuote
   .asc "? "
   .byte 0

convertFill .buf 1
convertLen = 15
uuConvertLine = *
   lda #0
   sta convertFill
   lda uudInLine
   bne +
   clc
   rts
+  jsr uuConvertChar
   bcc +
   rts
+  sta uudInLine
   ldx #60
   cmp #46
   bcc +
   ldx #84
+  stx convertLen
   ldx #0
-  cpx convertLen
   bcc +
   clc
   rts
+  bit convertFill
   bmi doConvertFill
   lda uudInLine+1,x
   beq +
   jsr uuConvertChar
   bcc convertCont
   rts
+  lda #$ff
   sta convertFill

   doConvertFill = *
   lda #0

   convertCont = *
   sta uudInLine+1,x
   inx
   bne -
   rts

uuConvertChar = *
   cmp #" "
   bcs +
   sec
   rts
+  cmp #"_"+1
   bcs +
   sec
   sbc #" "
   clc
   rts
+  cmp #96
   bne +
-  clc
   lda #0
   rts
+  cmp #"`"
   beq -
   bcs +
   sec
   rts
+  cmp #"Z"+1
   bcc +
   rts
+  sec
   sbc #"A"-33
   clc
   rts

uuCrunchLine = *
   ldx #0
   ldy #0
-  jsr uuCrunchGroup
   cpy uudInLine
   bcc -
   rts

;pos  76543210  76543210  76543210  76543210
;byt  xx111111  xx112222  xx222233  xx333333
;bit    765432    107654    321076    543210

uuCrunchGroup = * ;(.X=In4bytesOffset, .Y=Out3bytesOffset) : .X++, .Y++
   lda uudInLine+1,x  ;*** output byte 0
   asl
   asl
   sta temp
   inx
   lda uudInLine+1,x
   lsr
   lsr
   lsr
   lsr
   and #%00000011
   ora temp
   sta uudInLine+1,y
   iny
   lda uudInLine+1,x  ;*** output byte 1
   asl
   asl
   asl
   asl
   sta temp
   inx
   lda uudInLine+1,x
   lsr
   lsr
   and #%00001111
   ora temp
   sta uudInLine+1,y
   iny
   lda uudInLine+1,x  ;*** output byte 2
   inx
   ror
   ror
   ror
   and #%11000000
   sta temp
   lda uudInLine+1,x
   inx
   and #%00111111
   ora temp
   sta uudInLine+1,y
   iny
   rts

uuWriteLine = *
   lda #<uudInLine+1
   ldy #>uudInLine+1
   sta zp
   sty zp+1
   lda uudInLine
   ldy #0
   ldx outfile
   jsr write
   rts

getlinePos = 14
hitLastLine .buf 1
lastLineTerminator .buf 1

getline = *
   lda hitLastLine
   beq +
   sec
   rts
+  ldx #0
   stx getlinePos

   ;** toss an LF that follows a CR
   jsr getByte
   bcs getlineProcess
   cmp #chrLF
   clc
   bne getlineProcess
   ldx lastLineTerminator
   cpx #chrCR
   clc
   bne getlineProcess

   getlineChar = *
   jsr getByte
   getlineProcess = *
   bcc +
   lda #$ff
   sta hitLastLine
   jmp getlineFinish
+  cmp #chrCR
   beq getlineFinish
   cmp #chrLF
   beq getlineFinish
   ldx getlinePos
   cpx #98
   bcs +
   sta uudInLine,x
   inc getlinePos
+  jmp getlineChar

   getlineFinish = *
   sta lastLineTerminator
   ldx getlinePos
   lda #0
   sta uudInLine,x
   cpx #0
   beq +
   clc
   rts
+  lda hitLastLine
   cmp #1
   rts

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
   lda #<uudInBuf
   ldy #>uudInBuf
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
uudBss = *
uudInLine = uudBss+0
uudInBuf  = uudBss+100
