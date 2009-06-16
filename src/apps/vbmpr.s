;*** VBM Bitmap Printer 1.00 - by Craig Bruce - 07-Feb-1995

;this file is in ACE-assembler format

;vbmpr [-help] file ...

.include "acehead.s"
.org aceAppAddress

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

chrQUOTE = $22

arg          =  2 ;(2)  ;current argument number
name         =  4 ;(2)  ;name of file being kared
inBufLen     =  6 ;(2)  ;total size of input buffer
inFile       =  8 ;(1)  ;input file descriptor
aspect       = 10 ;(1)  ;x
bmCols       = 11 ;(1)  ;x
bmRows       = 12 ;(2)  ;x
topMargin    = 14 ;(2)  ;x
leftMargin   = 16 ;(1)  ;x
imageRows    = 18 ;(2)  ;x
imageCols    = 20 ;(1)  ;x
displayRow   = 22 ;(2)  ;x
displayCol   = 24 ;(1)  ;x
displayRows  = 26 ;(2)  ;x
displayCols  = 28 ;(1)  ;x
gotoNext     = 29 ;(1)  ;flag for skipping keypress after error
printerFd    = 30 ;(1)  ;file descriptor for printer
prgrFirst    = 31 ;(1)  ;first
imageChopMask= 32 ;(1)  ;and of last column
work         = 112 ;(16);temporary work area, lowest level

;===main===

main = *
   ;** check for a large enough TPA
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
   .byte "Insufficient program space to run\n",0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg = *
   .byte "usage: vbmpr [-help] file ...\n",0

mainInit = *
   lda #1
   ldy #0
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jmp usage
+  lda #0
   sta arg+0
   sta arg+1
   jsr startGraphics

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
   jsr vbmprint
   jmp mainNext

mainExit = *
   lda #<printerLineNorm
   ldy #>printerLineNorm
   ldx printerFd
   jsr fputs
   lda printerFd
   jsr close
   lda #chrCR
   jsr putchar
   rts
   printerLineNorm: .byte 27,"2",0

startGraphics = *
   lda #<printerFilename
   ldy #>printerFilename
   sta zp+0
   sty zp+1
   lda #"w"
   jsr open
   bcc +
   lda #<printerFilenameErr
   ldy #>printerFilenameErr
   jsr eputs
   jmp die
+  sta printerFd
   lda #<printerLinespace
   ldy #>printerLinespace
   ldx printerFd
   jsr fputs
   lda #80
   ldx #1
   sta bmCols
   stx aspect
   lda #255
   ldy syswork+1
   sta bmRows+0
   sty bmRows+1
   rts

   printerFilename:    .byte "q:",0
   printerFilenameErr: .byte 'Cannot open "q:" file\n',0
   printerLinespace:   .byte 27,"a",8,0

checkStop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   jmp die
   stoppedMsg: .byte "<Stopped>\n",0

vbmprint = *
   lda #0
   sta topMargin+0
   sta topMargin+1
   sta leftMargin

   ;** open file
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   jsr echoName
   lda #<openErrMsg
   ldy #>openErrMsg
   jsr eputs
   rts
+  sta inFile
   ;** check and extract header information
   lda #8
   jsr binRead
   bcs invalidFormat
   lda inBuf+0
   cmp #"b"
   bne invalidFormat
   lda inBuf+1
   cmp #"m"
   bne invalidFormat
   lda inBuf+2
   cmp #$cb
   bne invalidFormat
   lda inBuf+3
   cmp #2
   bne invalidFormat
   lda inBuf+5
   and #$07
   tax
   lda imageChopMasks,x
   sta imageChopMask
   ldy inBuf+4  ;hi/lo
   lda inBuf+5
   clc
   adc #7
   bcc +
   iny
+  sty work
   lsr work
   ror
   lsr work
   ror
   lsr work
   ror
   sta imageCols
   ldy inBuf+6  ;hi/lo
   lda inBuf+7
   sta imageRows+0
   sty imageRows+1
   ;** display
   jsr viewBody
   ;** close
   lda inFile
   jsr close
   rts
   imageChopMasks: .byte %11111111,%10000000,%11000000,%11100000
                   .byte %11110000,%11111000,%11111100,%11111110

   invalidFormat = *
   jsr echoName
   lda #<invalidFormatMsg
   ldy #>invalidFormatMsg
   jsr eputs
   rts
   invalidFormatMsg: .byte ": invalid VBM format\n",0
   openErrMsg:       .byte ": cannot open\n",0

echoName = *
   lda name+0
   ldy name+1
   jsr eputs
   rts

binReadLen: .buf 1

binRead = *  ;( .A=bytes, inFile )
   sta binReadLen
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
   ldx inFile
   lda binReadLen
   ldy #0
   jsr read
   beq +
   bcs +
   cmp binReadLen
   bne +
   clc
   rts
+  sec
   rts

viewBody = *  ;( no pun intended... )
   ;** set default display margins
   lda #0
   sta displayRow+0
   sta displayRow+1
   sta displayCol

   fiddleCols = *
   lda bmCols
   cmp imageCols
   bcc +
   lda #0
   sta leftMargin
   lda imageCols
   sta displayCols
   jmp fiddleRows
+  lda bmCols
   sta displayCols
   lda #0
   sta leftMargin

   fiddleRows = *
   lda #0
   sta topMargin+0
   sta topMargin+1
   lda imageRows+0
   ldy imageRows+1
   sta displayRows+0
   sty displayRows+1

   ;** skip top margin
   lda topMargin+0
   ldy topMargin+1
   ldx imageCols
   jsr skipPixelLines

   jsr prgrInit

   ;** display main portion of image
   viewNextLine = *
   jsr checkStop
   lda displayRows+0
   ora displayRows+1
   bne +
   jsr prgrFlush
   rts
+  ldx leftMargin
   sec
   lda imageCols
   sbc leftMargin
   sec
   sbc displayCols
   tay
   lda displayCols
   jsr readPixelLine
   lda displayRow+0
   ldy displayRow+1
   sta syswork+0
   sty syswork+1
   lda #<pixelBuf
   ldy #>pixelBuf
   sta syswork+2
   sty syswork+3
   lda displayCol
   ldx displayCols
   ldy #1
   jsr prgrLoad
   inc displayRow+0
   bne +
   inc displayRow+1
+  lda displayRows+0
   bne +
   dec displayRows+1
+  dec displayRows+0
   jmp viewNextLine

skipCount: .buf 2
skipBytes: .buf 1

skipPixelLines = * ;( .AY=lines, .X=lineLen )
   sta skipCount+0
   sty skipCount+1
   stx skipBytes
-  lda skipCount+0
   ora skipCount+1
   bne +
   rts
+  lda skipBytes
   jsr binRead
   lda skipCount+0
   bne +
   dec skipCount+1
+  dec skipCount+0
   jmp -

skipLeft:  .buf 1
payDirt:   .buf 1
skipRight: .buf 1

readPixelLine = * ;( .X=leftSkip, .A=payDirt, .Y=rightSkip ) : pixelBuf
   stx skipLeft
   sta payDirt
   sty skipRight
   lda skipLeft
   beq +
   jsr binRead
+  lda payDirt
   beq +
   jsr binRead
   ldx payDirt
   dex
-  lda inBuf,x
   sta pixelBuf,x
   dex
   bpl -
+  lda skipRight
   beq +
   jsr binRead
+  rts

prgrInit = *
   lda #$ff
   sta prgrFirst
   jsr prgrFill
   rts

prgrFill = *
   ldx #0
   lda #$00
-  sta prbitmap+$000,x
   sta prbitmap+$100,x
   sta prbitmap+$200,x
   sta prbitmap+$300,x
   sta prbitmap+$400,x
   sta prbitmap+$500,x
   sta prbitmap+$600,x
   sta prbitmap+$700,x
   inx
   bne -
   rts

prCols = work-1
prDataPtr = work-3

prgrLoad = *  ;( .A=X, (sw+0)=Y, .X=cols, .Y=1, (sw+2)=ptr )
   stx prCols
   lda syswork+2
   ldy syswork+3
   sta prDataPtr+0
   sty prDataPtr+1
   bit prgrFirst
   bmi +
   lda syswork+0
   and #$07
   bne +
   jsr prgrFlush
   jsr prgrFill
+  lda #0
   sta prgrFirst
   lda syswork+0
   and #$07
   clc
   adc #>prbitmap
   ldx #<prbitmap
   stx work+0
   sta work+1
   ldy #0
-  lda (prDataPtr),y
   sta (work+0),y
   iny
   cpy prCols
   bcc -
   dey
   lda (work+0),y
   and imageChopMask
   sta (work+0),y
   lda #"l"
   jsr putchar
   rts

flOff = work+2 ;(1)
flBit = work+3 ;(1)
flNcols = work+4 ;(2)

prgrFlush = *
   lda #"f"
   jsr putchar
   lda #0
   sta flOff
   lda #27
   ldx printerFd
   jsr putc
   lda #"*"
   ldx printerFd
   jsr putc
   lda #4     ;graphics mode%%%
   ldx printerFd
   jsr putc
   lda displayCols
   ldy #$00
   sty flNcols+1
   asl
   rol flNcols+1
   asl
   rol flNcols+1
   asl
   rol flNcols+1
   ldx printerFd
   jsr putc  ;low byte
   lda flNcols+1
   ldx printerFd
   jsr putc  ;high byte
-  lda #$80
   sta flBit
-  jsr flushBitcol
   lsr flBit
   bcc -
   inc flOff
   lda flOff
   cmp displayCols
   bcc --
   lda #13
   ldx printerFd
   jsr putc
   lda #10
   ldx printerFd
   jsr putc
   rts

flPtr    = work+6 ;(2)
flByte   = work+8 ;(1)
flRow    = work+9 ;(1)

flushBitcol = *  ;( flOff, flBit )
   ;** init
   lda #$00
   sta flByte
   sta flRow
   lda #<prbitmap
   ldy #>prbitmap
   sta flPtr+0
   sty flPtr+1
   ;** work
-  ldy flOff
   lda (flPtr),y
   and flBit
   cmp #0+1
   rol flByte
   ;** next
   inc flPtr+1
   inc flRow
   lda flRow
   cmp #8
   bcc -
   ;** finish
   lda flByte
   ldx printerFd
   jsr putc
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
   putcBuffer: .buf 1

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

;===bss===

bss      = *
pixelBuf = bss
inBuf    = pixelBuf+256
prbitmap = inBuf+256
bssEnd   = prbitmap+2048   ;256*8
