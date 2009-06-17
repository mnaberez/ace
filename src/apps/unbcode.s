;*** UNBCODE: bcode decoder version 1.00 - by Craig Bruce - 25-Nov-93

;assumes 1-65535 segments, 1-65536 temp files max, 0-4 G file length, 64 frags

!src "../system/acehead.s"
!to "../../build/unbcode", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

chrLF = $0a
chrQuote = $22
true = $ff
false = $00

maxChunk    = 64
maxLine     = 85
maxFilename = 85
maxHave     = 64
maxTempname = 20
copyBufSize = 4096
version = *
   !pet "1.00"
   !byte 0

hrFromSeg      = 0  ;(2)
hrToSeg        = 2  ;(2)
hrTempFileName = 4  ;(2)
hrValidLength  = 6  ;(4)
hrIsEnd        = 10 ;(1)
hrFilename     = 11 ;(17)
hrSize         = 32 ;(really 28)

asciiFile = *
   !fill 1
temp = *
   !fill 1

progName = *
   !fill 2
informative = *
   !fill 1
verbose = *
   !fill 1
debug = *
   !fill 1
readFilename = *
   !fill 2
readLineNum = *
   !fill 4
haveCount = *
   !fill 1
statusFileExists = *
   !fill 1
nextTempName = *
   !fill 2
filenameUsed = *
   !fill 1

arg        = 2  ;(2)
name       = 4  ;(2)
inBufLen   = 6  ;(2)
bufPtr     = 8  ;(2)
bufCount   = 10 ;(2)
infile     = 12 ;(1)
outfile    = 13 ;(1)
outName    = 14 ;(2)
segnum     = 16 ;(4)
convertLen = 20 ;(1)
getlinePos = 21 ;(1)
scanVal    = 22 ;(4)
crc        = 26 ;(4)
bytes      = 30 ;(4)
isEnd      = 34 ;(1)
curHave    = 35 ;(1)
tempName   = 36 ;(4)  : really only uses 2
h          = 40 ;(2)
nextH      = 42 ;(2)
haveRec    = 44 ;(1)
fin        = 45 ;(1)
fout       = 46 ;(1)
work       = 112 ;(16)

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

;===unbcode===
main = *
   ;** check for large enough TPA
   sec
   lda #<bssEnd
   cmp aceMemTop+0
   lda #>bssEnd
   sbc aceMemTop+1
   bcc tpaOk
   lda #<tpaMsg
   ldy #>tpaMsg
   jsr eputs
die = *
   lda #1
   ldx #0
   jmp aceProcExit

tpaMsg = *
   !pet "Insufficient program space to run unbcode"
   !byte chrCR,0

tpaOk = *
   ;** check argument count
   lda aceArgc+1
   beq +
   jmp enoughArgs
+  lda aceArgc+0
   cmp #2
   bcc usage
   jmp enoughArgs

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg = *
   !pet "usage: unbcode [-v] [-i] [-d] [-help] filename ..."
   !byte chrCR
   !pet "       [-v]=verbose, [-i]=informative, [-d]=debugging info"
   !byte chrCR,0

enoughArgs = *
   ;** set globals
   lda #true
   sta informative
   lda #false
   sta verbose
   lda #false
   sta debug
   lda #0
   ldy #0
   jsr getarg
   lda zp+0
   ldy zp+1
   sta progName+0
   sty progName+1
   lda #false
   sta filenameUsed
   lda #0
   sta tempPrefix
   sta tempName+2
   sta tempName+3
   jsr crcGen
   jsr loadStatusFile

   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<inBuf
   sta inBufLen+0
   lda aceMemTop+1
   sbc #>inBuf
   sta inBufLen+1
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
   lda zp+0
   ora zp+1
   beq mainExit
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jsr handleFlags
   jmp mainNext
+  jsr echo
   jsr unbcode
   bcc +
   jsr error
+  jmp mainNext

mainExit = *
   ;xx should read from stdin if no files
   jsr saveStatusFile
   rts

handleFlags = *
   iny
   lda (zp),y
   bne +
   rts
+  cmp #"d"
   beq flagD
   cmp #"v"
   beq flagV
   cmp #"i"
   beq flagI
   cmp #"h"
   bne +
   jmp usage
+  nop
   ;xx unrecognized option
   jmp handleFlags

flagD = *
   lda #true
   sta debug
   sta verbose
   sta informative
   ;xx print stuff
   jmp handleFlags

flagV = *
   lda #true
   sta verbose
   sta informative
   ;xx print version
   jmp handleFlags

flagI = *
   lda #true
   sta informative
   jmp handleFlags

error = *
   lda #<errorMsg1
   ldy #>errorMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<errorMsg2
   ldy #>errorMsg2
   jsr eputs
   rts

errorMsg1 = *
   !pet "Error attempting to unbcode file "
   !byte chrQuote,0
errorMsg2 = *
   !byte chrQuote
   !pet ", continuing"
   !byte chrCR,0

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
   !pet "unbcoding file "
   !byte chrQuote,0

echoMsg2 = *
   !byte chrQuote
   !pet "..."
   !byte chrCR,0

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
    
!src "./unbcodehelp.s"

statFcb = *
   !fill 1
statHR = *
   !fill 1

writeStatusData = *  ;( statFcb )
   lda #255
   sta statHR
   writeStatusNext = *
   inc statHR
   lda statHR
   cmp haveCount
   bcc +
   rts
+  jsr getH
   ldy #10
-  lda (h),y
   sta work,y
   dey
   bpl -
   ;** from segment
   ldx #work+hrFromSeg
   jsr writeNum5
   ;** to segment
   ldx #work+hrToSeg
   jsr writeNum5
   ;** beg/mid/end
   ldx #12
   lda work+hrIsEnd
   bne +
   ldx #6
   lda work+hrFromSeg+0
   cmp #1
   bne +
   lda work+hrFromSeg+1
   cmp #0
   bne +
   ldx #0
+  txa
   ldy #>begMidEndMsg
   clc
   adc #<begMidEndMsg
   bcc +
   iny
+  ldx statFcb
   jsr fputs
   ;** length
   ldx #work+hrValidLength
   jsr writeNum10
   ;** temp name
   lda #<tempNamePrefix
   ldy #>tempNamePrefix
   ldx statFcb
   jsr fputs
   ldx #work+hrTempFileName
   jsr writeNum5
   ;** filename
   lda h+0
   ldy h+1
   clc
   adc #hrFilename
   bcc +
   iny
+  ldx statFcb
   jsr fputs
   lda #chrCR
   ldx statFcb
   jsr putc
   jmp writeStatusNext

   begMidEndMsg = *
      !byte "b","e","g"," "," ",0
      !byte "m","i","d"," "," ",0
      !byte "e","n","d"," "," ",0
   tempNamePrefix = *
      !pet "0BC"
      !byte 0

saveStatusFile = *
   bit verbose
   bpl .ssf1
   lda #<saveStatMsg
   ldy #>saveStatMsg
   jsr eputs
.ssf1:
   lda #<statusFilename
   ldy #>statusFilename
   sta zp+0
   sty zp+1
   lda haveCount
   bne .ssf3
   lda statusFileExists
   bne .ssf2
   rts
.ssf2:
   jsr aceFileRemove
   rts
.ssf3:
   lda #"w"
   jsr openOverwrite
   bcc .ssf4
   lda #<statusWriteErrMsg
   ldy #>statusWriteErrMsg
   jsr eputs
   lda #stderr
.ssf4:
   sta statFcb
   jsr writeStatusData
   lda statFcb
   cmp #stderr
   beq .ssf5
   jsr close
.ssf5:
   rts

saveStatMsg = *
   !pet "saving status file 0BC-STAT"
   !byte chrCR,0
statusFilename = *
   !pet "0BC-STAT"
   !byte 0
statusWriteErrMsg = *
   !pet "Cannot open "
   !byte chrQuote
   !pet "0BC-STAT"
   !byte chrQuote
   !pet ", writing status to stderr:"
   !byte chrCR,0

discardSegment = *
   lda curHave
   jsr getH
   ldy #hrFromSeg
   lda segnum+0
   cmp (h),y
   bne discardAppendedSeg
   iny
   lda segnum+1
   cmp (h),y
   bne discardAppendedSeg

   ;** this is the only segment in the fragment, delete it
   lda #<discMsg
   ldy #>discMsg
   jsr eputs
   jsr fetchTempName
   lda #true
   jsr getTempNameStr
   lda #<tempNameStr
   ldy #>tempNameStr
   sta zp+0
   sty zp+1
   jsr aceFileRemove
   lda curHave
   jsr removeHaveRec

   discardSegExit = *
   bit debug
   bpl +
   ldx #stderr
   stx statFcb
   jsr writeStatusData
+  rts

discMsg = *
   !pet "discarding segment"
   !byte chrCR,0

discardAppendedSeg = *
   ldy #hrToSeg
   sec
   lda segnum+0
   sbc #1
   sta (h),y
   iny
   lda segnum+1
   sbc #0
   sta (h),y
   lda #<discAppMsg
   ldy #>discAppMsg
   jsr eputs
   ;** get and open new temp file
   jsr getTempFileNameOnly
   lda #"w"
   sta fileMode
   jsr openTempFile
   lda outfile
   sta fout
   bcs discardSegExit
   ;** open old temp file
   lda curHave
   jsr getH
   ldy #hrTempFileName
   lda (h),y
   pha
   lda tempName+0
   sta (h),y
   pla
   sta tempName+0
   iny
   lda (h),y
   pha
   lda tempName+1
   sta (h),y
   pla
   sta tempName+1
   lda #"r"
   sta fileMode
   jsr openTempFile
   lda outfile
   sta fin
   bcc +
   lda fout
   jsr close
   jmp discardSegExit
+  sta fin
   ;** copy valid contents into new temp file
   ldx #3
   ldy #hrValidLength+3
-  lda (h),y
   sta bytes,x
   dey
   dex
   bpl -
   jsr copyFile
   ;** remove old temp file
   lda #<tempNameStr
   ldy #>tempNameStr
   sta zp+0
   sty zp+1
   jsr aceFileRemove
   jmp discardSegExit

discAppMsg = *
   !pet "discarding appended segment"
   !byte chrCR,0

loadStatusFile = *
   bit verbose
   bpl +
   lda #<loadStatMsg
   ldy #>loadStatMsg
   jsr eputs
+  lda #0
   sta haveCount
   lda #false
   sta statusFileExists
   lda #1
   ldy #0
   sta nextTempName+0
   sty nextTempName+1
   lda #<statusFilename
   ldy #>statusFilename
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcs +
   sta statFcb
   lda #true
   sta statusFileExists
   jsr scanStatusFile
   lda statFcb
   jsr close
+  rts

loadStatMsg = *
   !pet "scanning status file 0BC-STAT"
   !byte chrCR,0

ssPos = *
   !fill 1

scanStatusFile = *
   ;** read status line
   ldx #0
   stx ssPos
-  ldx statFcb
   jsr getc
   bcc +
   rts
+  cmp #chrCR
   beq +
   ldx ssPos
   cpx #maxLine-1
   bcs -
   sta inLine,x
   inc ssPos
   jmp -
+  lda #0
   ldx ssPos
   sta inLine,x
   cpx #42
   bcc scanStatusFile
   sta inLine+52

   ;** scan status line
   bit debug
   bpl +
   lda #<inLine
   ldy #>inLine
   jsr eputs
   lda #chrCR
   jsr eputchar
+  ldx #hrSize-1
   lda #0
-  sta tempHaveRec,x
   dex
   bpl -

;0----+----1----+----2----+----3----+----4----+----5----+--
;00001  00002  beg  0000001140  0BC00007  new4
;00004  00004  end  0000000189  0BC00004  1234567890123456.
;00002  00003  end  0000000529  0BC00006  new5

   ;** filename
   ldx #0
-  lda inLine+41,x
   sta tempHaveRec+hrFilename,x
   beq +
   inx
   bne -
   ;** isEnd
+  ldx #true
   lda inLine+14
   cmp #"e"
   beq +
   ldx #false
+  stx tempHaveRec+hrIsEnd
   ;** fromSeg
   ldy #0
   jsr scanNum
   bcs scanErr
   lda scanVal+0
   ldy scanVal+1
   sta tempHaveRec+hrFromSeg+0
   sty tempHaveRec+hrFromSeg+1
   ;** toSeg
   ldy #7
   jsr scanNum
   bcs scanErr
   lda scanVal+0
   ldy scanVal+1
   sta tempHaveRec+hrToSeg+0
   sty tempHaveRec+hrToSeg+1
   ;** validLength
   ldy #19
   jsr scanNum
   bcs scanErr
   lda scanVal+0
   ldy scanVal+1
   sta tempHaveRec+hrValidLength+0
   sty tempHaveRec+hrValidLength+1
   lda scanVal+2
   ldy scanVal+3
   sta tempHaveRec+hrValidLength+2
   sty tempHaveRec+hrValidLength+3
   ;** tempFileName
   ldy #34
   jsr scanNum
   bcs scanErr
   lda scanVal+0
   ldy scanVal+1
   sta tempHaveRec+hrTempFileName+0
   sty tempHaveRec+hrTempFileName+1

   ;** store status info
   lda haveCount
   cmp #maxHave
   bcc +
   lda #<scanTooManyFrags
   ldy #<scanTooManyFrags
   jsr eputs
   scanErr = *
   jmp scanStatusFile
+  lda haveCount
   inc haveCount
   jsr getH
   ldy #hrSize-1
-  lda tempHaveRec,y
   sta (h),y
   dey
   bpl -
   jmp scanStatusFile

scanTooManyFrags = *
   !pet "too many fragments in 0BC-STAT, ignoring fragment"
   !byte chrCR,0

;===bss===
bss         = *
inLine      = bss+0
tempPrefix  = inLine+maxLine
tempNameStr = tempPrefix+20
haves       = tempNameStr+maxFilename
copyBuf     = hrSize*maxHave+haves
decodeTable = copyBuf+copyBufSize
tempHaveRec = decodeTable+256
crcTable0   = tempHaveRec+hrSize
crcTable1   = crcTable0+256
crcTable2   = crcTable1+256
crcTable3   = crcTable2+256
inBuf       = crcTable3+256
bssEnd      = inBuf+64
