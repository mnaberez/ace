;*** KAR file decoder version 1.00 - by Craig Bruce - 05-Mar-94

;unkar [-help] file ...

;Kevin's ARchiver format, created by Kevin Phillips and Craig Bruce, 1991
;line length limited to 250 characters

!src "../system/acehead.s"
!to "../../build/unkar", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

chrQUOTE = $22

arg          =  2 ;(2)  ;current argument number
name         =  4 ;(2)  ;name of file being bcoded
inBufPtr     =  6 ;(2)  ;next char to extract from inBuf
inBufCount   =  8 ;(2)  ;number of valid chars in inBuf
inBufLen     = 10 ;(2)  ;total size of input buffer
scanVal      = 12 ;(4)  ;scanNum: value returned
scanSave     = 16 ;(4)  ;scanNum: save for *4 to *5
scanDigit    = 20 ;(1)  ;scanNum: new digit
scanTemp     = 21 ;(1)  ;scanNum: temp
scanIndex    = 22 ;(1)  ;scanNum: scanning index
scanAnything = 23 ;(1)  ;scanNum: flag for if anything scanned
fileCount    = 24 ;(4)  ;files left in current archive
lineCount    = 28 ;(4)  ;lines left in current file
inFile       = 32 ;(1)  ;input file descriptor
outFile      = 33 ;(1)  ;output file descriptor
work         = 112 ;(16);temporary work area, lowest level

;===main===

main = *
   ;** check for large enough TPA
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
   !pet "Insufficient program space to run more"
   !byte chrCR,0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg = *
   !pet "usage: unkar [-help] file ..."
   !byte chrCR,0

mainInit = *
   lda #0
   sta arg+0
   sta arg+1

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
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jmp usage
+  jsr unkar
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

unkar = *
   jsr echo
   ;** open file
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   ;xx open error
   rts
+  sta inFile
   jsr initGetByte
   ;** decode file
   jsr unkarBody
   ;** close file
   lda inFile
   jsr close
   rts

invalidFormatMsg = *
   !pet ": invalid KAR file format"
   !byte chrCR,0
invalidFormat = *
   lda name+0
   ldy name+1
   jsr eputs
   lda #<invalidFormatMsg
   ldy #>invalidFormatMsg
   jsr eputs
   rts

unkarBody = *
   ;** get the file count
   jsr getline
   bcs invalidFormat
   ldx #fileCount
   jsr getnum
   bcs invalidFormat

   unkarNextFile = *
   jsr checkStop
   lda fileCount+0
   ora fileCount+1
   ora fileCount+2
   ora fileCount+3
   bne +
   rts
   ;** get filename
+  jsr getline
   bcs invalidFormat
   jsr echoExtract
   lda #<line
   ldy #>line
   sta zp+0
   sty zp+1
   lda #"w"
   jsr open
   bcc +
   ;xx check for file exists
   lda #<line
   ldy #>line
   jsr eputs
   lda #<cannotOpenMsg
   ldy #>cannotOpenMsg
   jsr eputs
   rts
+  sta outFile
   ;** get file line count
   jsr getline
   bcs invalidFormatClose
   ldx #lineCount
   jsr getnum
   bcc unkarNextLine
   invalidFormatClose = *
   lda outFile
   jsr close
   jmp invalidFormat

   unkarNextLine = *
   lda lineCount+0
   ora lineCount+1
   ora lineCount+2
   ora lineCount+3
   bne +
   lda outFile
   jsr close
   ldx #fileCount
   jsr dec32
   jmp unkarNextFile

   ;** copy lines of file
+  jsr getline
   bcs invalidFormatClose
   lda #chrCR
   sta line,y
   iny
   lda #<line
   ldx #>line
   sta zp+0
   stx zp+1
   tya
   ldy #0
   ldx outFile
   jsr write
   ldx #lineCount
   jsr dec32
   jmp unkarNextLine

   cannotOpenMsg = *
   !pet ": cannot open for writing"
   !byte chrCR,0

dec32 = *
   clc
   ldy #4
-  lda 0,x
   sbc #0
   sta 0,x
   inx
   dey
   bne -
   rts

echo = *
   lda #<echoMsg1
   ldy #>echoMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<echoMsg2
   ldy #>echoMsg2
   jmp eputs
echoMsg1 = *
   !pet "unkaring file "
   !byte chrQUOTE,0
echoMsg2 = *
   !byte chrQUOTE,chrCR,0

echoExtract = *
   lda #<echoExtractMsg1
   ldy #>echoExtractMsg1
   jsr eputs
   lda #<line
   ldy #>line
   jsr eputs
   lda #<echoExtractMsg2
   ldy #>echoExtractMsg2
   jmp eputs
echoExtractMsg1 = *
   !pet "extracting file "
   !byte chrQUOTE,0
echoExtractMsg2 = *
   !byte chrQUOTE,chrCR,0

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

getline = *  ;( ) : line, .Y=lineLen, .CS=eof
   ldy #0
-  sty work+0
   jsr getByte
   bcc +
   rts
+  ldy work+0
   sta line,y
   iny
   cpy #254
   bcs getlineSkip
   cmp #chrCR
   bne -
   dey

   getlineExit = *
   lda #0
   sta line,y
   clc
   rts

   getlineSkip = *
   lda #<getlineTooLongMsg
   ldy #>getlineTooLongMsg
   jsr eputs
-  jsr getByte
   bcc +
   rts
+  cmp #chrCR
   bne -
   ldy #254
   jmp getlineExit

   getlineTooLongMsg = *
   !pet "unkar: line too long, truncating"
   !byte chrCR,0

initGetByte = *
   lda #0
   sta inBufCount+0
   sta inBufCount+1
   sec
   lda aceMemTop+0
   sbc #<inBuf
   sta inBufLen+0
   lda aceMemTop+1
   sbc #>inBuf
   sta inBufLen+1
   rts

getByte = *
   lda inBufCount+0
   ora inBufCount+1
   beq getByteFillBuf
   ldy #0
   lda (inBufPtr),y
   inc inBufPtr+0
   bne +
   inc inBufPtr+1
+  ldx inBufCount+0
   bne +
   dec inBufCount+1
+  dec inBufCount+0
   clc
   rts

getByteFillBuf = *
   jsr checkStop
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
   sta inBufPtr+0
   sty inBufPtr+1
   lda inBufLen+0
   ldy inBufLen+1
   ldx inFile
   jsr read
   beq +
   bcs +
   sta inBufCount+0
   sty inBufCount+1
   jmp getByte
+  sec
   rts

scanNum = *  ;( (zp)=num, .Y=inLineIndex ) : .Y=scan, [scanVal]=num, .CS=err
   ldx #3
   lda #0
-  sta scanVal,x
   dex
   bpl -
   lda #0
   sta scanAnything
-  lda (zp),y
   cmp #" "
   bne scanNumNext
   iny
   bne -
   sec
   rts

   scanNumNext = *
   lda (zp),y
   cmp #"0"
   bcc +
   cmp #"9"+1
   bcc ++
+  lda scanAnything
   beq scanError
   clc
   rts
++ and #$0f
   sta scanDigit
   lda #$ff
   sta scanAnything
   ;** times ten
   sty scanTemp
   ldx #3
-  lda scanVal,x
   sta scanSave,x
   dex
   bpl -
   lda #2
   sta scanIndex
-- clc
   ldy #4
   ldx #0
-  rol scanVal,x
   inx
   dey
   bne -
   bcs scanError
   dec scanIndex
   bne --
   clc
   ldy #4
   ldx #0
-  lda scanVal,x
   adc scanSave,x
   sta scanVal,x
   inx
   dey
   bne -
   bcs scanError
   clc
   ldy #4
   ldx #0
-  rol scanVal,x
   inx
   dey
   bne -
   bcs scanError
   clc
   ldy #4
   ldx #0
   lda scanDigit
-  adc scanVal,x
   sta scanVal,x
   lda #0
   inx
   dey
   bne -
   bcs scanError

   ldy scanTemp
   iny
   beq scanError
   jmp scanNumNext

   scanError = *
   sec
   rts

getnum = *  ;( line, .X=zpaddr ) : .CS=err
   stx work+0
   lda #<line
   ldy #>line
   sta zp+0
   sty zp+1
   ldy #0
   jsr scanNum
   bcc +
   rts
+  ldy #0
   ldx work+0
-  lda scanVal,y
   sta 0,x
   inx
   iny
   cpy #4
   bcc -
   clc
   rts

;===bss===

bss    = *
line   = bss+0  
inBuf  = line+256
bssEnd = inBuf+64
