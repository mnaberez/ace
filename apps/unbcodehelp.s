unbcode = *
   ;** open file
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   ;** decode file
   jsr unbcodeBody
   ;** close file
   lda infile
   jsr close
   rts

unbcodeBody = *
   lda #0
   sta bufCount+0
   sta bufCount+1
   sta hitLastLine
   sta lastLineTerminator

   ;** search for "begin" line
   searchLine = *
   jsr getline
   bcc +
   clc
   rts
+  lda inLine
   cmp #"-"
   bne searchLine
   jsr checkStop
   ldx #0
   lda inLine+2
   cmp #"b"
   beq +
   ldx #asciiBegin-petsciiBegin
+  ldy #0
-  lda inLine,y
   cmp petsciiBegin,x
   bne searchLine
   inx
   iny
   cpy #asciiBegin-petsciiBegin
   bcc -
   ldy #0
   cpx #asciiBegin-petsciiBegin+1
   bcc +
   ldy #$ff
+  sty asciiFile
   jmp processBegin

   petsciiBegin = *
   .asc "--bcode-begin "
   asciiBegin = *
   .byte $2d,$2d,$62,$63,$6f,$64,$65,$2d,$62,$65,$67,$69,$6e,$20
   
   ;** process "begin" line
   processBegin = *
   ;** extract the segment number
   ldy #14
   jsr scanNum
   bcc +
   jmp beginError
+  iny
   lda scanVal+2
   ora scanVal+3
   beq +
   lda #<segTooBigMsg
   ldy #>segTooBigMsg
   jsr eputs
   jmp searchLine
+  lda scanVal+0
   ldx scanVal+1
   sta segnum+0
   stx segnum+1
   lda #0
   sta segnum+2
   sta segnum+3
   lda segnum+0
   ora segnum+1
   bne +
   jmp beginError

   ;** extract filename, trunc to 16 chars
+  clc
   tya
   adc #<inLine
   sta outName+0
   lda #>inLine
   adc #0
   sta outName+1
   ldy #16
   lda #0
   sta (outName),y
   jsr makePetsciiName
   jsr echoExtractName

   ;** open output file
   jsr getTempFile
   bcc +
   jmp searchLine

   ;** read bcoded data
+  jsr buildDecodeTable
   jsr crcInit
-  jsr getline
   bcs unexpectedEof
   jsr convertLine
   bcc +
   ;** check if segment-end line
   lda inLine
   cmp #"-"
   beq finishFile
   ;** report invalid characters
   lda #<badCharsMsg
   ldy #>badCharsMsg
   jsr eputs
   jmp -
+  jsr crunchLine
   jsr crcLine
   jsr writeLine
   jmp -

   ;** finish with file
   finishFile = *
   lda outfile
   jsr close
   jsr crcFinish
   jsr processFinish
   ;** process for another file
   jmp searchLine

unexpectedEof = *
   lda #<unexEofMsg
   ldy #>unexEofMsg
   jsr eputs
   lda outfile
   jsr close
   jsr discardSegment
   sec
   rts

unexEofMsg = *
   .asc "unexpected EOF, ignoring segment."
   .byte chrCR,0

segTooBigMsg = *
   .asc "segment number is larger than 65535, ignoring segment."
   .byte chrCR,0

badCharsMsg = *
   .asc "warning: bad characters on line, ignoring line."
   .byte chrCR,0

makePetsciiName = *
   bit asciiFile
   bmi +
   rts
+  ldy #0
-  lda (outName),y
   beq +
   jsr convAsc2Pet
   sta (outName),y
   iny
   bne -
+  rts

convAsc2Pet = *
   and #$7f
   cmp #$60
   bcc +
   clc
   adc #$c0-$60
+  tax
   and #$7f
   cmp #"a"
   bcs +
-  txa
   rts
+  cmp #"z"+1
   bcs -
   txa
   eor #$80
   rts

echoExtractName = *
   lda #<echoExtractMsg1
   ldy #>echoExtractMsg1
   jsr eputs
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #segnum
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr eputs
   lda #<echoExtractMsg2
   ldy #>echoExtractMsg2
   jsr eputs
   lda outName+0
   ldy outName+1
   jsr eputs
   lda #<echoExtractMsg3
   ldy #>echoExtractMsg3
   jmp eputs

echoExtractMsg1 = *
   .asc "extracting seg "
   .byte 0
echoExtractMsg2 = *
   .asc " of "
   .byte chrQuote,0
echoExtractMsg3 = *
   .byte chrQuote,chrCR,0
numbuf .buf 12

reportOpenError = *
   lda zp+0
   ldy zp+1
   jsr eputs
   lda #<reportOpenErrorMsg
   ldy #>reportOpenErrorMsg
   jsr eputs
   rts

   reportOpenErrorMsg = *
   .asc ": cannot open, continuing"
   .byte chrCR,0

scanDigit .buf 1
scanSave .buf 4
scanTemp .buf 1
scanIndex .buf 1
scanAnything .buf 1

scanNum = *  ;( .Y=inLineIndex ) : .Y=scan, [scanVal]=num, .CS=err
   ldx #3
   lda #0
-  sta scanVal,x
   dex
   bpl -
   lda #0
   sta scanAnything
-  lda inLine,y
   cmp #" "
   bne scanNumNext
   iny
   bne -
   sec
   rts

   scanNumNext = *
   lda inLine,y
   cmp #"0"
   bcc +
   cmp #"9"+1
   bcc ++
+  lda scanAnything
   beq scanError
   clc
   rts
+  and #$0f
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
-  clc
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
   
beginError = *
   lda #<beginErrorMsg
   ldy #>beginErrorMsg
   jsr eputs
   jmp searchLine

beginErrorMsg = *
   .asc "invalid --bcode-begin line format, ignoring segment"
   .byte chrCR,0

convertPads .buf 1
convertChars .buf 1

convertLine = *
   ldx #0
   stx convertPads
-  lda inLine,x
   bne +
   stx convertChars
   clc
   rts
+  cmp #"="
   bne +
   inc convertPads
+  tay
   lda decodeTable,y
   bmi +
   sta inLine,x
   inx
   bne -
+  sec
   rts

crunchBytes .buf 1

crunchLine = *
   ldx #0
   ldy #0
-  jsr crunchQuantum
   cpx convertChars
   bcc -
   tya
   sec
   sbc convertPads
   sta crunchBytes
   rts

;pos  76543210  76543210  76543210  76543210
;byt  xx111111  xx112222  xx222233  xx333333
;bit    765432    107654    321076    543210

crunchQuantum = * ;(.X=In4bytesOffset, .Y=Out3bytesOffset) : .X++, .Y++
   lda inLine,x  ;*** output byte 0
   asl
   asl
   sta temp
   inx
   lda inLine,x
   lsr
   lsr
   lsr
   lsr
   and #%00000011
   ora temp
   sta inLine,y
   iny
   lda inLine,x  ;*** output byte 1
   asl
   asl
   asl
   asl
   sta temp
   inx
   lda inLine,x
   lsr
   lsr
   and #%00001111
   ora temp
   sta inLine,y
   iny
   lda inLine,x  ;*** output byte 2
   inx
   ror
   ror
   ror
   and #%11000000
   sta temp
   lda inLine,x
   inx
   and #%00111111
   ora temp
   sta inLine,y
   iny
   rts

writeLine = *
   lda #<inLine
   ldy #>inLine
   sta zp+0
   sty zp+1
   lda crunchBytes
   ldy #0
   ldx outfile
   jsr write
   rts

;** crc = 0xFFFFFFFF;
;** while( (c=getc(fp)) != EOF ) {
;**     crc = (crc>>8) & 0x00FFFFFF ^ crcTable[ (crc^c) & 0xFF ];
;** }
;** return( crc^0xFFFFFFFF );

crcInit = *
   ldx #3
-  lda #$ff
   sta crc,x
   lda #0
   sta bytes,x
   dex
   bpl -
   rts

crcLine = *
   ldy #0
   cpy crunchBytes
   bcs +
-  lda inLine,y         ;.X = (crc^c) & 0xFF
   eor crc+0
   tax
   lda crc+1            ;crc = (crc>>8) & 0x00FFFFFF ^ crcTable[ .X ]
   eor crcTable0,x
   sta crc+0
   lda crc+2
   eor crcTable1,x
   sta crc+1
   lda crc+3
   eor crcTable2,x
   sta crc+2
   lda crcTable3,x
   sta crc+3
   iny
   cpy crunchBytes
   bcc -
+  clc
   lda bytes+0
   adc crunchBytes
   sta bytes+0
   bcc +
   inc bytes+1
   bne +
   inc bytes+2
   bne +
   inc bytes+3
+  rts

crcFinish = *
   ldx #3
-  lda crc,x
   eor #$ff
   sta crc,x
   dex
   bpl -
   rts

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
-  ldx #0
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
   sta inLine,x
   inc getlinePos
+  jmp getlineChar

   getlineFinish = *
   sta lastLineTerminator
   ldx getlinePos
   lda #0
   sta inLine,x
   cpx #0
   beq +
   clc
   rts
+  lda hitLastLine
   cmp #1
   rts

getByte = *
   lda bufCount+0
   ora bufCount+1
   beq getByteFillBuf
   ldy #0
   lda (bufPtr),y
   inc bufPtr+0
   bne +
   inc bufPtr+1
+  ldx bufCount+0
   bne +
   dec bufCount+1
+  dec bufCount+0
   clc
   rts

getByteFillBuf = *
   jsr checkStop
   lda #<inBuf
   ldy #>inBuf
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
   jsr checkStop
   jmp getByte
+  sec
   rts

setIndex .buf 1
setCountdown .buf 1

buildDecodeTable = *
   ldx #0
   lda #$ff
-  sta decodeTable,x
   inx
   bne -
   ldy #0
   ldx #0
   bit asciiFile
   bpl +
   ldx #ascTableSet-petTableSet
+  stx setIndex
   lda #5
   sta setCountdown
-  ldx setIndex
   lda petTableSet+1,x
   sta temp
   lda petTableSet+0,x
   inx
   inx
   stx setIndex
   ldx temp
   jsr buildSet
   dec setCountdown
   bne -
   lda #0
   sta decodeTable+$3d
   rts

petTableSet .asc "AZaz09++//"
ascTableSet .byte $41,$5a,$61,$7a,$30,$39,$2b,$2b,$2f,$2f

setLimit .buf 1

buildSet = *
   inx
   stx setLimit
   tax
-  tya
   sta decodeTable,x
   iny
   inx
   cpx setLimit
   bcc -
   rts

processFinish = *  ;process the bcode-end line
   lda #true
   sta isEnd
   ldx #ascEnd-petEnd-1
   bit asciiFile
   bpl +
   ldx #petCont-petEnd-1
+  ldy #ascEnd-petEnd-1
   jsr compareFinish
   ldy #ascEnd-petEnd-1
   bcc finCheckSegment
   lda #false
   sta isEnd
   ldx #ascCont-petEnd-1
   bit asciiFile
   bpl +
   ldx #ascContEnd-petEnd-1
+  ldy #ascCont-petCont-1
   jsr compareFinish
   ldy #ascCont-petCont
   bcc finCheckSegment
   lda #0
   jmp badFinish

   finCheckSegment = *
   jsr scanNum
   bcc +
-  lda #1
   jmp badFinish
+  ldx #3
-  lda scanVal,x
   cmp segnum,x
   bne --
   dex
   bpl -

   finCheckSize = *
   jsr scanNum
   bcc +
-  lda #2
   jmp badFinish
+  ldx #3
-  lda scanVal,x
   cmp bytes,x
   bne --
   dex
   bpl -
   
   finCheckCrc = *
   iny
   jsr scanHex
   bcc +
-  lda #3
   jmp badFinish
+  ldx #3
-  lda scanVal,x
   cmp crc,x
   bne --
   dex
   bpl -
   jsr commitSegment
   bit debug
   bpl +
   ldx #stderr
   stx statFcb
   jsr writeStatusData
+  jsr checkStop
   clc
   rts

scanHex = *
   ldx #3
-  lda inLine,y
   iny
   jsr hexToBin
   bcs +
   asl
   asl
   asl
   asl
   sta temp
   lda inLine,y
   iny
   jsr hexToBin
   bcs +
   ora temp
   sta scanVal,x
   dex
   bpl -
   clc
+  rts

hexToBinXsave .buf 1

hexToBin = *
   bit asciiFile
   bpl +
   stx hexToBinXsave
   jsr convAsc2Pet
   ldx hexToBinXsave
+  cmp #"0"
   bcs +
-  sec
   rts
+  cmp #"9"+1
   bcc +
   and #$7f
   cmp #"a"
   bcc -
   cmp #"f"+1
   bcs -
   sec
   sbc #"a"-$0a
+  and #$0f
   clc
   rts

compareFinish = *
-  lda inLine,y
   cmp petEnd,x
   bne +
   dex
   dey
   bpl -
   clc
   rts
+  sec
   rts

badFinish = *  ;.A=error [0=token,1=segment,2=size,3=crc]
   pha
   lda name+0
   ldy name+1
   jsr eputs
   pla
   asl
   tax
   lda badFinVec+1,x
   tay
   lda badFinVec+0,x
   jsr eputs
   lda #<badFinDiscard
   ldy #>badFinDiscard
   jsr eputs
   jsr discardSegment
   jsr checkStop
   rts

badFinVec = *
   .word badFinToken,badFinSegment,badFinSize,badFinCrc
badFinToken = *
   .asc ": invalid token on finish line"
   .byte 0
badFinSegment = *
   .asc ": segment number mismatch"
   .byte 0
badFinSize = *
   .asc ": file size mismatch"
   .byte 0
badFinCrc = *
   .asc ": CRC-32 checksum mismatch"
   .byte 0
badFinDiscard = *
   .asc ", ignoring segment"
   .byte chrCR,0

petEnd .asc "--bcode-end "
ascEnd .byte $2d,$2d,$62,$63,$6f,$64,$65,$2d,$65,$6e,$64,$20

petCont .asc "--bcode-continued "
ascCont .byte $2d,$2d,$62,$63,$6f,$64,$65,$2d,$63,$6f,$6e,$74,$69,$6e,$75
        .byte $65,$64,$20
ascContEnd = *

getTempNameStr = *  ;( tempName, .A=putFiletype ) : tempNameStr, (zp)
   pha
   lda #"0"
   sta tempNameStr+0
   lda #"B"
   sta tempNameStr+1
   lda #"C"
   sta tempNameStr+2
   lda #<tempNameStr+3
   ldy #>tempNameStr+3
   sta zp+0
   sty zp+1
   ldx #tempName
   lda #5
   jsr utoaz
   pla
   cmp #false
   beq +
   lda #","
   sta tempNameStr+3,y
   lda #"p"
   sta tempNameStr+4,y
   lda #0
   sta tempNameStr+5,y
+  rts

utoaz = *  ;( 0+.X=var32, .A=width, (zp)=store )
   jsr aceMiscUtoa
   ldy #0
-  lda (zp),y
   beq ++
   cmp #" "
   bne +
   lda #"0"
   sta (zp),y
+  iny
   bne -
+  rts

fileMode .buf 1

getTempFile = *  ;( outName, segnum ) : curHave, curHaveNum, outfile, .CS=err
   ;** search to append to existing file
   lda #255
   sta curHave

   checkNext = *
   inc curHave
   lda curHave
   cmp haveCount
   bcc +
   jmp cannotAppend
+  jsr getH
   jsr cmpOutNameH
   cmp #0
   bne checkNext
   ldy #hrFromSeg
   lda segnum+0
   cmp (h),y
   lda segnum+1
   iny
   sbc (h),y
   bcc +
   ldy #hrToSeg
   lda (h),y
   cmp segnum+0
   iny
   lda (h),y
   sbc segnum+1
   bcc +
   lda #<ignoreDupMsg
   ldy #>ignoreDupMsg
   jsr eputs
   sec
   rts
   ignoreDupMsg = *
   .asc "ignoring duplicate segment"
   .byte chrCR,0

+  ldy #hrToSeg
   lda (h),y
   tax
   iny
   lda (h),y
   tay
   inx
   bne +
   iny
+  cpx segnum+0
   bne +
   cpy segnum+1
   beq canAppend
+  jmp checkNext

   ;** here we know we can append to an existing temp file
   canAppend = *
   lda #"a"
   sta fileMode
   ldy #hrTempFileName
   lda (h),y
   sta tempName+0
   iny
   lda (h),y
   sta tempName+1
   ldy #hrToSeg
   lda segnum+0
   sta (h),y
   iny
   lda segnum+1
   sta (h),y
   ldy #hrIsEnd
   lda #false
   sta (h),y
   jmp openTempFile

   ;** cannot append
   cannotAppend = *
   jsr getTempFileNameOnly

   ;** create new have record
   createNewHaveRec = *
   jsr insertHaveRec
   bcs +
   lda #"w"
   sta fileMode
   jmp openTempFile
+  lda #<haveOverMsg
   ldy #>haveOverMsg
   jsr eputs
   sec
   rts
   haveOverMsg = *
   .asc "fragment table full, ignoring current segment"
   .byte chrCR,0

   ;** open temporary file
   openTempFile = *
   lda #true
   jsr getTempNameStr
   lda #<tempNameStr
   ldy #>tempNameStr
   sta zp+0
   sty zp+1
   lda fileMode
   jsr openOverwrite
   bcs +
   sta outfile
   clc
   rts
+  lda #<tempNameStr
   ldy #>tempNameStr
   jsr eputs
   lda #<openErrMsg
   ldy #>openErrMsg
   jsr eputs
   sec
   rts
   openErrMsg = *
   .asc ": cannot open, ignoring segment"
   .byte chrCR,0

getTempFileNameOnly = *  ;( nextTempName ) : tempName
   lda nextTempName+0
   ldy nextTempName+1
   sta tempName+0
   sty tempName+1
   inc nextTempName+0
   bne +
   inc nextTempName+1
+  lda #0
   sta work
-  cmp haveCount
   bcc +
   rts
+  lda work
   jsr getH
   ldy #hrTempFileName
   lda (h),y
   cmp tempName+0
   bne +
   iny
   lda (h),y
   cmp tempName+1
   bne +
   jmp getTempFileNameOnly
+  inc work
   lda work
   jmp -

getH = *  ;( .A=haveRecNum ) : h
   ldx #0
   stx h+1
   ldx #5
-  asl
   rol h+1
   dex
   bne -
   clc
   adc #<haves
   sta h+0
   lda h+1
   adc #>haves
   sta h+1
   rts

getNextH = *  ;( h ) : nextH
   clc
   lda h+0
   adc #hrSize
   sta nextH+0
   lda h+1
   adc #0
   sta nextH+1
   rts

cmpOutNameH = *  ;( (outName)=str1, (h)=hrec ) : .A=cmpResult[0=EQ,1=GT,-1=LT]
   lda h+0
   ldy h+1
   clc
   adc #hrFilename
   bcc +
   iny
+  sta work+0
   sty work+1
   ldy #255
-  iny
   lda (outName),y
   cmp (work),y
   bne +
   cmp #0
   bne -
   lda #0
   rts
+  bcc +
   lda #1
   rts
+  lda #$ff
   rts

insertHaveRec = *  ;( segnum, tempName, outName ) : curHave
   ;** allocate new record
   lda haveCount
   cmp #maxHave
   bcc +
   rts
+  sta curHave
   inc haveCount

   ;** find correct position for new record
-  dec curHave
   lda curHave
   cmp #$ff
   beq insertHaveRecInit
   jsr getH
   jsr cmpOutNameH
   cmp #1
   beq insertHaveRecInit
   cmp #0
   bne +
   ldy #hrFromSeg
   lda (h),y
   cmp segnum+0
   iny
   lda (h),y
   sbc segnum+1
   bcc insertHaveRecInit
+  jsr getNextH
   ldy #hrSize-1
-  lda (h),y
   sta (nextH),y
   dey
   bpl -
   jmp --

   ;** initialize record
   insertHaveRecInit = *
   inc curHave
   ldx #hrSize-1
   lda #0
-  sta tempHaveRec,x
   dex
   bpl -
   lda segnum+0
   ldy segnum+1
   sta tempHaveRec+hrFromSeg+0
   sty tempHaveRec+hrFromSeg+1
   sta tempHaveRec+hrToSeg+0
   sty tempHaveRec+hrToSeg+1
   lda #false
   sta tempHaveRec+hrIsEnd
   lda tempName+0
   ldy tempName+1
   sta tempHaveRec+hrTempFileName+0
   sty tempHaveRec+hrTempFileName+1
   ldy #0
-  lda (outName),y
   sta tempHaveRec+hrFilename,y
   beq +
   iny
   cpy #16
   bcc -
+  lda curHave
   jsr getH
   ldy #hrSize-1
-  lda tempHaveRec,y
   sta (h),y
   dey
   bpl -
   clc
   rts

writeNum5 = *  ;( 0+.X=num16 )
   lda 0,x
   sta work+12
   lda 1,x
   sta work+13
   lda #0
   sta work+14
   sta work+15
   ldx #work+12
   lda #5
   jmp writeNum

writeNum10 = *  ;( 0+.X=num32 )
   lda #10
writeNum = *
   pha
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   pla
   jsr utoaz
   lda #<numbuf
   ldy #>numbuf
   ldx statFcb
   jsr fputs
   jsr writeSpaces
   rts

writeSpaces = *
   lda #<spacesMsg
   ldy #>spacesMsg
   ldx statFcb
   jsr fputs
   rts
   spacesMsg = *
   .byte $20,$20,0

commitSegment = *
   ;** add byte length, update IsEnd flag in haverec
   lda curHave
   jsr getH
   ldx #4
   ldy #hrValidLength
   clc
-  lda (h),y
   adc bytes-hrValidLength,y
   sta (h),y
   iny
   dex
   bne -
   ldy #hrIsEnd
   lda isEnd
   sta (h),y
   jsr checkCoalesce
   jsr checkComplete
   rts

checkFilenamePtr .buf 2

checkComplete = *
   lda curHave
   jsr getH
   ldy #hrIsEnd
   lda (h),y
   bne +
-  rts
+  ldy #hrFromSeg
   lda (h),y
   cmp #1
   bne -
   iny
   lda (h),y
   bne -
   jsr fetchTempName
   lda #false
   jsr getTempNameStr
   lda #<tempNameStr
   ldy #>tempNameStr
   sta zp+0
   sty zp+1
   lda h+0
   ldy h+1
   clc
   adc #hrFilename
   bcc +
   iny
+  sta zw+0
   sty zw+1
   sta checkFilenamePtr+0
   sty checkFilenamePtr+1
   jsr renameOverwrite
   lda #<completeMsg1
   ldy #>completeMsg1
   jsr eputs
   lda checkFilenamePtr+0
   ldy checkFilenamePtr+1
   jsr eputs
   lda #<completeMsg2
   ldy #>completeMsg2
   jsr eputs
   lda curHave
   jsr removeHaveRec
   rts

completeMsg1 = *
   .asc "--Reassembled "
   .byte chrQuote,0
completeMsg2 = *
   .byte chrQuote,chrCR,0

removeHaveRec = *  ;( .A=haveRec )
   dec haveCount
   sta haveRec
-  lda haveRec
   cmp haveCount
   bcc +
   rts
+  lda haveRec
   jsr getH
   jsr getNextH
   ldy #hrSize-1
-  lda (nextH),y
   sta (h),y
   dey
   bpl -
   inc haveRec
   jmp --

checkFromPlus1 .buf 2

checkCoalesce = *  ;( curHave )
   ldx curHave
   inx
   cpx haveCount
   bcc +
-  rts
+  lda curHave
   jsr getH
   jsr getNextH
   clc
   lda nextH+0
   adc #hrFilename
   sta outName+0
   lda nextH+1
   adc #0
   sta outName+1
   jsr cmpOutNameH
   cmp #0
   bne -
   ldy #hrToSeg
   clc
   lda (h),y
   adc #1
   sta checkFromPlus1+0
   iny
   lda (h),y
   adc #0
   sta checkFromPlus1+1
   ldy #hrFromSeg
   lda (nextH),y
   cmp checkFromPlus1+0
   bne -
   iny
   lda (nextH),y
   cmp checkFromPlus1+1
   bne -

   lda #<coalesceMsg1
   ldy #>coalesceMsg1
   jsr eputs
   lda h+0
   ldy h+1
   jsr eputRange
   lda #","
   jsr eputchar
   lda #" "
   jsr eputchar
   lda nextH+0
   ldy nextH+1
   jsr eputRange
   lda #<coalesceMsg2
   ldy #>coalesceMsg2
   jsr eputs
   clc
   lda h+0
   adc #hrFilename
   ldy h+1
   bcc +
   iny
+  jsr eputs
   lda #chrQuote
   jsr eputchar
   lda #chrCR
   jsr eputchar

   ldx curHave
   inx
   lda #"r"
   jsr openTemp
   bcc +
   rts
+  sta fin
   ldx curHave
   lda #"a"
   jsr openTemp
   bcc +
   lda fin
   jsr close
+  sta fout
   lda curHave
   jsr getH
   jsr getNextH
   ldy #hrValidLength+3
   ldx #3
-  lda (nextH),y
   sta bytes,x
   dey
   dex
   bpl -
   jsr copyFile
   lda fout
   jsr close
   lda fin
   jsr close
   ldy #hrIsEnd
   lda (nextH),y
   sta (h),y
   ldy #hrToSeg
   lda (nextH),y
   sta (h),y
   iny
   lda (nextH),y
   sta (h),y
   ldx #4
   ldy #hrValidLength
   clc
-  lda (h),y
   adc (nextH),y
   sta (h),y
   iny
   dex
   bne -
   lda curHave
   clc
   adc #1
   jsr getH
   jsr fetchTempName
   lda #false
   jsr getTempNameStr
   lda #<tempNameStr
   ldy #>tempNameStr
   sta zp+0
   sty zp+1
   jsr aceFileRemove
   bcc +
   nop  ;xx error msg
+  lda curHave
   clc
   adc #1
   jsr removeHaveRec
   rts

coalesceMsg1 = *
   .asc "coalescing segs "
   .byte 0
coalesceMsg2 = *
   .asc " of "
   .byte chrQuote,0

eputRange = *
   sta work+0
   sty work+1
   ldy #hrFromSeg
   jsr eputHaveNum
   lda #"-"
   jsr eputchar
   ldy #hrToSeg

eputHaveNum = *
   lda (work),y
   sta work+4
   iny
   lda (work),y
   sta work+5
   lda #0
   sta work+6
   sta work+7
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #work+4
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr eputs
   rts

fetchTempName = *  ;( h )
   ldy #hrTempFileName
   lda (h),y
   sta tempName+0
   iny
   lda (h),y
   sta tempName+1
   rts

openTemp = *  ;( .X=haveRec, .A=mode ) : .A=fcb, .CS=err
   pha
   txa
   jsr getH
   jsr fetchTempName
   lda #true
   jsr getTempNameStr
   lda #<tempNameStr
   ldy #>tempNameStr
   sta zp+0
   sty zp+1
   pla
   jsr open
   bcc +
   lda #<tempNameStr
   ldy #>tempNameStr
   jsr eputs
   lda #<openTempMsg
   ldy #>openTempMsg
   jsr eputs
   sec
+  rts

   openTempMsg = *
   .asc ": cannot open, should be able to, continuing"
   .byte chrCR,0

copySegLen .buf 2
           .byte $00,$00

copyFile = *  ;( fin, fout, bytes-- )
   lda #<copyBuf
   ldy #>copyBuf
   sta zp+0
   sty zp+1
   lda bytes+2
   ora bytes+3
   bne +
   lda bytes+0
   cmp #<copyBufSize
   lda bytes+1
   sbc #>copyBufSize
   bcs +
   lda bytes+0
   ldy bytes+1
   jmp ++
+  lda #<copyBufSize
   ldy #>copyBufSize
+  ldx fin
   jsr read
   beq +
   sta copySegLen+0
   sty copySegLen+1
   ldx fout
   jsr write
   sec
   ldy #4
   ldx #0
-  lda bytes,x
   sbc copySegLen,x
   sta bytes,x
   inx
   dey
   bne -
   lda bytes+0
   ora bytes+1
   ora bytes+2
   ora bytes+3
   bne copyFile
   rts
+  lda bytes+0
   ora bytes+1
   ora bytes+2
   ora bytes+3
   bne +
   rts
+  lda #<copyLenMsg
   ldy #>copyLenMsg
   jsr eputs
   rts

   copyLenMsg = *
   .asc "Insufficient temp data coalesced, shouldn't happen, continuing"
   .byte chrCR,0

openOvMode .buf 1

openOverwrite = *  ;( (zp)=name, .A=mode ) : .A=Fcb, .CS=err
   sta openOvMode
   jsr open
   bcs +
   rts
+  lda errno
   cmp #aceErrFileExists
   beq +
-  jsr reportOpenError
   sec
   rts
+  jsr aceFileRemove
   lda openOvMode
   jsr open
   bcs -
   rts

renameOvName .buf 2

renameOverwrite = *  ;( (zp)=name, (zw)=newName ) : .CS=err
   jsr aceFileRename
   bcs +
   rts
+  lda zp+0
   ldy zp+1
   sta renameOvName+0
   sty renameOvName+1
   lda errno
   cmp #aceErrFileExists
   beq +
-  jsr reportRenameError
   sec
   rts
+  lda zw+0
   ldy zw+1
   sta zp+0
   sty zp+1
   jsr aceFileRemove
   lda renameOvName+0
   ldy renameOvName+1
   sta zp+0
   sty zp+1
   jsr aceFileRename
   bcs -
   rts

renameOvNewName .buf 2

reportRenameError = *
   lda zw+0
   ldy zw+1
   sta renameOvNewName+0
   sty renameOvNewName+1
   lda #<renameErrMsg1
   ldy #>renameErrMsg1
   jsr eputs
   lda renameOvName+0
   ldy renameOvName+1
   jsr eputs
   lda #<renameErrMsg2
   ldy #>renameErrMsg2
   jsr eputs
   lda renameOvNewName+0
   ldy renameOvNewName+1
   jsr eputs
   lda #<renameErrMsg3
   ldy #>renameErrMsg3
   jsr eputs
   rts

renameErrMsg1 = *
   .asc "Cannot rename "
   .byte chrQuote,0
renameErrMsg2 = *
   .byte chrQuote
   .asc " to "
   .byte chrQuote,0
renameErrMsg3 = *
   .byte chrQuote
   .asc ", continuing."
   .byte chrCR,0

