;*** BCODE: bcode encoder version 1.00 - by Craig Bruce - 17-Nov-93

;bcode [-help] [-v] [-u] [-m] [-l max_line_count] filename ...

!src "../system/acehead.s"
!to "../../build/bcode", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

chrLF       = $0a
chrQuote    = $22
true        = $ff
false       = $00
maxChunk    = 54
maxLine     = 80
trPetscii   = 0
trAsciiCrLf = 1
trAsciiLf   = 2

arg         = 2  ;(2)  ;current argument number
name        = 4  ;(2)  ;name of file being bcoded
string      = 8  ;(2)  ;temp string
maxlines    = 10 ;(4)  ;max number of lines per segment
linelimit   = 14 ;(1)  ;whether there is a restriction on lines/seg
alias       = 16 ;(2)  ;current alias name to use
transTo     = 18 ;(1)  ;output file translation: petscii, asc-crlf, asc-lf
progname    = 19 ;(2)  ;pointer to argv[0]
verbose     = 21 ;(1)  ;flag for giving verbose information
filenameUsed = 22;(1)  ;flag: if a filename has been encountered on the cmd line
bufPtr      = 23 ;(2)  ;pointer to next char in input buffer
bufCount    = 25 ;(2)  ;number of bytes left in input buffer
inBufSize   = 27 ;(2)  ;maximum size of input buffer
infile      = 29 ;(1)  ;fd of input binary file
outfile     = 30 ;(1)  ;fd to output bcoded data to
chunkLen    = 31 ;(1)  ;length of chunk for encoding
chunkPos    = 32 ;(1)  ;scanning position in chunk during encoding
bctemp      = 33 ;(1)  ;temporary for conversion from 8 to 6 bits
trPutPtr    = 34 ;(2)  ;pointer to translated-puts string
trPutIndex  = 36 ;(1)  ;index into translated-puts string
crc         = 37 ;(4)  ;cumulative crc-32 of segment
bytes       = 41 ;(4)  ;cumulative bytes in segment
segnum      = 45 ;(4)  ;current segment number
stopCountdn = 49 ;(1)  ;countdown to check stop key
scanVal     = 50 ;(4)  ;result of converting string to 32-bit number
linenum     = 54 ;(4)  ;current line number being encoded
isLastSeg   = 58 ;(1)  ;whether we have just encoded the last segment
work        = 112;(16) ;misc work area

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
   !pet "Insufficient program space to run bcode"
   !byte chrCR,0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg = *
   !pet "usage: bcode [-help] [-v] [-u] [-m] [-l max_line_count] filename ..."
   !byte chrCR
   !pet "flags: -v:verbose, -u:unix-ascii, -m:ms-dos-ascii"
   !byte chrCR,0

defaultAlias = *
   !pet "stdin"
   !byte 0

mainInit = *
   ;** set globals
   lda #true
   sta verbose
   lda #0
   ldy #0
   jsr getarg
   lda zp+0
   ldy zp+1
   sta progname+0
   sty progname+1
   lda #false
   sta filenameUsed
   lda #0
   sta arg+0
   sta arg+1
   lda #<defaultAlias
   ldy #>defaultAlias
   sta alias+0
   sty alias+1
   lda #false
   sta linelimit
   lda #trPetscii
   sta transTo
   jsr crcGen
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<inBuf
   sta inBufSize+0
   lda aceMemTop+1
   sbc #>inBuf
   sta inBufSize+1

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
   jmp handleFlags
+  jsr echo
   jsr bcode
   jmp mainNext

mainExit = *
   bit filenameUsed
   bmi +
   ;xx should read from stdin if no files
   nop
+  rts

handleFlags = *
   iny
   lda (zp),y
   bne +
   jmp mainNext
+  cmp #"v"
   beq flagV
   cmp #"m"
   beq flagM
   cmp #"a"
   beq flagM
   cmp #"u"
   beq flagU
   cmp #"l"
   beq flagL
   cmp #"h"
   bne +
   jmp usage
+  nop
   ;xx unrecognized option
   jmp handleFlags

flagV = *
   lda #true
   sta verbose
   jmp handleFlags

flagM = *
   lda #trAsciiCrLf
   sta transTo
   jmp handleFlags

flagU = *
   lda #trAsciiLf
   sta transTo
   jmp handleFlags

flagL = *
   inc arg+0
   bne +
   inc arg+1
+  lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ora zp+1
   beq flagLerror
   ldy #0
   jsr scanNum
   bcs flagLerror
   ldx #3
-  lda scanVal,x
   sta maxlines,x
   dex
   bpl -
   lda maxlines+0
   ora maxlines+1
   ora maxlines+2
   ora maxlines+3
   beq flagLerror
   lda #true
   sta linelimit
   jmp mainNext
flagLerror = *
   lda #<flagLerrorMsg
   ldy #>flagLerrorMsg
   jsr eputs
   jmp die
flagLerrorMsg = *
   !pet "ERROR: invalid maximum line limit given with -l option"
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
   jsr eputs
   rts

echoMsg1 = *
   !pet "bcoding file "
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

bcode = *
   lda #true
   sta filenameUsed
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcs openError
   sta infile
   lda #0
   sta bufCount+0
   sta bufCount+1
   jsr setBase64Table
   lda #1
   ldy #0
   sta segnum+0
   sty segnum+1
   sty segnum+2
   sty segnum+3

   bcodeNextSegment = *
   lda #stdout
   sta outfile
   bit linelimit
   bpl +
   jsr getOutfile
+  jsr bcodeSegment
   lda outfile
   cmp #stdout
   beq +
   jsr close
+  bit isLastSeg
   bpl +
   lda infile
   jsr close
   rts
+  inc segnum+0
   bne +
   inc segnum+1
   bne +
   inc segnum+2
   bne +
   inc segnum+3
+  jmp bcodeNextSegment

openError = *
   lda #<openErrorMsg1
   ldy #>openErrorMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<openErrorMsg2
   ldy #>openErrorMsg2
   jsr eputs
   rts
openErrorMsg1 = *
   !pet "ERROR: cannot open "
   !byte chrQuote,0
openErrorMsg2 = *
   !byte chrQuote,chrCR,0

outfileFileLen = work+0
outfileExtLen  = work+1
outfileTemp    = work+2

getOutfile = *
   ;** get filename
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   jsr basename
   stx outfileFileLen
   ;** get seg number, 2+ digits
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #segnum
   jsr aceMiscUtoa
   cpy #1
   bne +
   lda numbuf+0
   sta numbuf+1
   lda #"0"
   sta numbuf+0
   lda #0
   sta numbuf+2
   iny
+  tya
   clc
   adc #2
   sta outfileExtLen
   clc
   lda outfileFileLen
   adc outfileExtLen
   ldy outfileFileLen
   cmp #17
   bcc +
   sec
   lda #16
   sbc outfileExtLen  ;gives allowed filename len
   tay
+  lda #"."
   sta outBuf,y
   iny
   lda #"b"
   sta outBuf,y
   iny
   ldx #0
-  lda numbuf,x
   sta outBuf,y
   beq +
   inx
   iny
   bne -
   ;** open the file
+  lda #<outBuf
   ldy #>outBuf
   sta zp+0
   sty zp+1
   lda #"W"
   jsr open
   sta outfile
   bcc +
   lda #<outfileErrMsg1
   ldy #>outfileErrMsg1
   jsr eputs
   lda #<outBuf
   ldy #>outBuf
   jsr eputs
   lda #<outfileErrMsg2
   ldy #>outfileErrMsg2
   jsr eputs
   jmp die
   ;** echo opening
+  lda #<outfileMsg
   ldy #>outfileMsg
   jsr eputs
   lda #<outBuf
   ldy #>outBuf
   jsr eputs
   lda #chrQuote
   jsr eputchar
   lda #chrCR
   jmp eputchar

outfileMsg = *
   !pet "outputting to file "
   !byte chrQuote,0
outfileErrMsg1 = *
   !pet "ERROR: cannot open "
   !byte chrQuote,0
outfileErrMsg2 = *
   !byte chrQuote
   !pet ", aborting!"
   !byte chrCR,0

bcodeSegment = *  ;( ) : isLastSeg
   ;** header line
   lda #<bcodeHeaderMsg
   ldy #>bcodeHeaderMsg
   jsr trPuts
   ldx #segnum
   jsr trPutnum
   lda #" "
   jsr trPutchar
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   jsr basename
   lda #<outBuf
   ldy #>outBuf
   jsr trPuts
   lda #chrCR
   jsr trPutchar
   jsr crcInit
   lda #0
   sta linenum+0
   sta linenum+1
   sta linenum+2
   sta linenum+3
   ;** loop
   encodeNext = *
   jsr encodeLine
   bcc .bs1
   lda #true
   sta isLastSeg
   jmp encodeSegFinish
.bs1:
   bit linelimit
   bpl encodeNext
   inc linenum+0
   bne .bs2
   inc linenum+1
   bne .bs2
   inc linenum+2
   bne .bs2
   inc linenum+3
.bs2:
   sec
   ldy #4
   ldx #0
.bs3:
   lda linenum,x
   sbc maxlines,x
   inx
   dey
   bne .bs3
   bcc encodeNext
   lda #false
   sta isLastSeg

   ;** end line
   encodeSegFinish = *
   jsr crcFinish
   bit isLastSeg
   bpl .bs4
   lda #<bcodeEndMsg
   ldy #>bcodeEndMsg
   jmp .bs5
.bs4:
   lda #<bcodeContinuedMsg
   ldy #>bcodeContinuedMsg
.bs5:
   jsr trPuts
   ldx #segnum
   jsr trPutnum
   lda #" "
   jsr trPutchar
   ldx #bytes
   jsr trPutnum
   lda #" "
   jsr trPutchar
   ldx #crc
   jsr trPuthex
   lda #chrCR
   jsr trPutchar
   rts

bcodeHeaderMsg = *
   !pet "--bcode-begin "
   !byte 0
bcodeEndMsg = *
   !pet "--bcode-end "
   !byte 0
bcodeContinuedMsg = *
   !pet "--bcode-continued "
   !byte 0

basenameStart = *
   !fill 1
basename = * ;( (zp)=inname ) : outBuf=outname, .X=basenameLen
   ldy #255
   sty basenameStart
-  iny
   lda (zp),y
   beq basenameDone
   cmp #":"
   beq +
   cmp #"/"
   bne -
+  sty basenameStart
   jmp -
   basenameDone = *
   ldy basenameStart
   ldx #255
-  iny
   inx
   lda (zp),y
   sta outBuf,x
   bne -
   cpx #2
   bcc +
   lda outBuf-2,x
   cmp #","
   bne +
   lda #0
   sta outBuf-2,x
   dex
   dex
+  rts

encodeLine = *
   inc stopCountdn
   lda stopCountdn
   and #7
   bne +
   jsr checkStop
   ;** get the chunk
+  jsr readChunk
   bcc +
   rts
+  stx chunkLen
   lda #0
   sta inChunk+0,x
   sta inChunk+1,x
   jsr crcChunk
   ;** encode the chunk
   ldx #0  ;chunkpos
   ldy #0  ;linepos
-  jsr encodeFourChars
   cpx chunkLen
   bcc -
   ;** fix non-integral-length (last) line
   beq +
   lda #"="
   sta outBuf-1,y
   dex
   cpx chunkLen
   beq +
   sta outBuf-2,y
   ;** output the line
+  lda #chrCR
   sta outBuf,y
   ldx transTo
   cpx #trPetscii
   beq +
   iny
   lda #chrLF
   sta outBuf,y
   cpx #trAsciiCrLf
   beq +
   dey
   sta outBuf,y
+  iny
   tya
   lda #<outBuf
   ldx #>outBuf
   sta zp+0
   stx zp+1
   tya
   ldy #0
   ldx outfile
   jsr write
   rts

encodeFourChars = *  ;( .X++=chunkpos, .Y++=linepos )
   stx chunkPos
   ;** put bytes into output line
;pos  76543210  76543210  76543210  76543210
;byt  xx111111  xx112222  xx222233  xx333333
;bit    765432    107654    321076    543210
   ;** first byte
   lda inChunk+0,x
   lsr
   lsr
   tax
   lda base64Char,x
   sta outBuf,y
   iny

   ;** second byte
   ldx chunkPos
   lda inChunk+0,x
   asl
   asl
   asl
   asl
   sta bctemp
   lda inChunk+1,x
   lsr
   lsr
   lsr
   lsr
   ora bctemp
   and #%00111111
   tax
   lda base64Char,x
   sta outBuf,y
   iny

   ;** third byte
   ldx chunkPos
   lda inChunk+1,x
   asl
   asl
   sta bctemp
   lda inChunk+2,x
   asl
   rol
   rol
   and #%00000011
   ora bctemp
   and #%00111111
   tax
   lda base64Char,x
   sta outBuf,y
   iny

   ;** fourth byte
   ldx chunkPos
   lda inChunk+2,x
   and #%00111111
   tax
   lda base64Char,x
   sta outBuf,y
   iny
   ldx chunkPos
   inx
   inx
   inx
   rts

base64Index = *
   !fill 1
setBase64Table = *
   ldy #0
   ldx #0
   lda transTo
   cmp #trPetscii
   beq +
   ldx #base64DescAsc-base64DescPet
+  stx base64Index
-- ldx base64Index
   lda base64DescPet+0,x
   beq +
   pha
   lda base64DescPet+1,x
   tax
   pla
-  sta base64Char,y
   clc
   adc #1
   iny
   dex
   bne -
   inc base64Index
   inc base64Index
   bne --
+  rts

base64DescPet = *
   !byte "A",26,"a",26,"0",10,"+",1,"/",1,$00
base64DescAsc = *
   !byte $41,26,$61,26,"0",10,"+",1,"/",1,$00

readChunk = *  ;( ) : .X=len
   ldx #0
-  stx chunkLen
   jsr getByte
   ldx chunkLen
   bcs +
   sta inChunk,x
   inx
   cpx #maxChunk
   bcc -
-  clc
   rts
+  cpx #0
   bne -
   sec
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
   lda inBufSize+0
   ldy inBufSize+1
   ldx infile
   jsr read
   beq +
   bcs +
   sta bufCount+0
   sty bufCount+1
   jmp getByte
+  sec
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

crcChunk = *
   ldy #0
   cpy chunkLen
   bcs +
-  lda inChunk,y        ;.X = (crc^c) & 0xFF
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
   cpy chunkLen
   bcc -
+  clc
   lda bytes+0
   adc chunkLen
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

trPuts = *
   sta trPutPtr+0
   sty trPutPtr+1
   ldy #0
   sty trPutIndex
-  ldy trPutIndex
   lda (trPutPtr),y
   beq +
   jsr trPutchar
   inc trPutIndex
   bne -
+  rts

trPutchar = *
   ldx transTo
   cpx #trPetscii
   beq .tp1
   cmp #chrCR
   beq .tp2
   jsr convPet2Asc
.tp1:
   ldx outfile
   jmp putc
.tp2:
   ldx transTo
   cpx #trAsciiLf
   beq .tp3
   lda #chrCR
   ldx outfile
   jsr putc
.tp3:
   lda #chrLF
   ldx outfile
   jmp putc

convPet2Asc = *
   cmp #"a"
   bcs +
   rts
+  tax
   bpl +
   sbc #$c0-$60
   tax
+  and #$1f
   bne +
-  txa
   rts
+  cmp #$1b
   bcs -
   txa
   eor #$20
   rts

trPutnum = * ;( .X=number32 )
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   trPutnumDump = *
   lda #<numbuf
   ldy #>numbuf
   jsr trPuts
   rts

trPuthex = * ;( .X=number32 )
   lda #4
   sta work
   ldy #0
   inx
   inx
   inx
-  lda 0,x
   pha
   lsr
   lsr
   lsr
   lsr
   jsr trPuthexDigit
   pla
   jsr trPuthexDigit
   dex
   dec work
   bne -
   lda #0
   sta numbuf,y
   jmp trPutnumDump

trPuthexDigit = *  ;( .A=digit, .Y=numbufIndex )
   and #$0f
   ora #$30
   cmp #$3a
   bcc +
   adc #6
+  sta numbuf,y
   iny
   rts

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
   rts

scanDigit = *
   !fill 1
scanSave = *
   !fill 4
scanTemp = *
   !fill 1
scanIndex = *
   !fill 1
scanAnything = *
   !fill 1

scanNum = *  ;( (zp)=numStr, .Y=numIndex ) : .Y=scan, [scanVal]=num, .CS=err
   ldx #3
   lda #0
.sn1:
   sta scanVal,x
   dex
   bpl .sn1
   lda #0
   sta scanAnything
.sn2:
   lda (zp),y
   cmp #" "
   bne scanNumNext
   iny
   bne .sn2:
   sec
   rts

   scanNumNext = *
   lda (zp),y
   cmp #"0"
   bcc .sn3
   cmp #"9"+1
   bcc .sn4
.sn3:
   lda scanAnything
   beq scanError
   clc
   rts
.sn4:
   and #$0f
   sta scanDigit
   lda #$ff
   sta scanAnything
   ;** times ten
   sty scanTemp
   ldx #3
.sn5:
   lda scanVal,x
   sta scanSave,x
   dex
   bpl .sn5
   lda #2
   sta scanIndex
.sn6:
   clc
   ldy #4
   ldx #0
.sn7:
   rol scanVal,x
   inx
   dey
   bne .sn7
   bcs scanError
   dec scanIndex
   bne .sn6
   clc
   ldy #4
   ldx #0
.sn8:
   lda scanVal,x
   adc scanSave,x
   sta scanVal,x
   inx
   dey
   bne .sn8
   bcs scanError
   clc
   ldy #4
   ldx #0
.sn9
   rol scanVal,x
   inx
   dey
   bne .sn9
   bcs scanError
   clc
   ldy #4
   ldx #0
   lda scanDigit
.sn10
   adc scanVal,x
   sta scanVal,x
   lda #0
   inx
   dey
   bne .sn10
   bcs scanError

   ldy scanTemp
   iny
   beq scanError
   jmp scanNumNext

   scanError = *
   sec
   rts
   
;===bss===

bss = *
outBuf     = bss
inChunk    = outBuf+maxLine+1
base64Char = inChunk+maxChunk+5
numbuf     = base64Char+64
crcTable0  = numbuf+12
crcTable1  = crcTable0+256
crcTable2  = crcTable1+256
crcTable3  = crcTable2+256
inBuf      = crcTable3+256
bssEnd     = inBuf+64
