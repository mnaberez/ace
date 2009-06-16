;*** VBM Bitmap Viewer 1.50 - by Craig Bruce - 16-May-1995

;this program is in ACE-assembler format.

;vbm [-help] file ...

.include "acehead.s"

.org aceAppAddress

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

VBMFMT_VERSION2       = $02
VBMFMT_VERSION3       = $03
VBMFMT_UNCOMPRESSED   = $00
VBMFMT_RLE_COMPRESSED = $01

arg          =  2 ;(2)  ;current argument number
name         =  4 ;(2)  ;name of file being kared
inBufLen     =  6 ;(2)  ;total size of input buffer
inFile       =  8 ;(1)  ;input file descriptor
aspect       = 10 ;(1)  ;aspect ratio of screen
bmCols       = 11 ;(1)  ;number of cols that screen can handle (divided by 8)
bmRows       = 12 ;(2)  ;number of rows that screen can handle (literal)
topMargin    = 14 ;(2)  ;top margin into image to start displaying at
leftMargin   = 16 ;(1)  ;left margin into image to start displaying at
imageRows    = 18 ;(2)  ;total number of rows in the image
imageCols    = 20 ;(1)  ;total number of cols in the image
displayRow   = 22 ;(2)  ;current row being displayed (also start)
displayCol   = 24 ;(1)  ;current col being displayed (also start)
displayRows  = 26 ;(2)  ;number of rows to display on screen
displayCols  = 28 ;(1)  ;number of cols to display on screen
gotKeystroke = 29 ;(1)  ;flag for skipping keypress after error
viewComplete = 30 ;(1)  ;flag for image displayed being complete
smallDelta   = 31 ;(1)  ;number of pixels to move on "small" scrolls
maxTopMargin = 32 ;(2)  ;maximum value for the top margin
maxLeftMargin= 34 ;(1)  ;maximum value for the left margin
inFormat     = 35 ;(1)  ;format of input VBM file (version 2 or 3)
inEncoding   = 36 ;(1)  ;input encoding format (compressed/uncompressed)
inRepCode    = 37 ;(1)  ;input char code for repeating arbitrary byte
inZeroCode   = 38 ;(1)  ;input char code for repeating a zero
inFfCode     = 39 ;(1)  ;input char code for repeating an $FF
inDblZeroCode= 40 ;(1)  ;input char code for two zeroes
inDblFfCode  = 41 ;(1)  ;input char code for two $FFs
inExpandByte = 42 ;(1)  ;input byte value
inExpandCount= 43 ;(1)  ;input bytes remaining to be expanded
getBytePtr   = 44 ;(1)  ;pointer to readBuf
getByteCount = 45 ;(1)  ;bytes remaining to be read in readBuf
work         = 112;(16) ;temporary work area, lowest level

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
gdie = *
   jsr aceGrExit
   jmp die

tpaMsg: .byte "Insufficient program space to run\n",0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg: .byte "usage: vbm [-help] file ...\n",0

mainInit = *
   lda #1
   ldy #0
   sta arg+0
   sty arg+1
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jmp usage
+  jsr startGraphics
   lda #8
   sta smallDelta

   mainNext = *
   jsr checkStop
   lda arg+0
   ldy arg+1
   jsr getarg
   beq mainExit
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   jsr viewControl
   jmp mainNext

mainExit = *
   jsr aceGrExit
   rts

startGraphics = *
   lda #2
   ldx #$6
   ldy #$0e
   jsr aceGrScreen
   sta bmCols
   stx aspect
   lda syswork+0
   ldy syswork+1
   sta bmRows+0
   sty bmRows+1
   rts

checkStop = *
   jsr aceConStopkey
   bcs +
   rts
   checkStopWork = *
+  jsr aceGrExit
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   jmp die

   stoppedMsg: .byte "<Stopped>\n",0

viewControl = *
   lda #0
   sta topMargin+0
   sta topMargin+1
   sta leftMargin

   viewControlTop = *
   lda #0
   sta gotKeystroke
   jsr view
   viewControlKey = *
   lda gotKeystroke
   bne +
   jsr aceConGetkey
+  ldx #0
   stx gotKeystroke
   cmp #$03
   bne +
   jmp checkStopWork
+  cmp #"1"
   bcc +
   cmp #"9"+1
   bcs +
   and #$0f
   asl
   asl
   asl
   sta smallDelta
   bit viewComplete
   bpl viewControlTop
   jmp viewControlKey
+  ldx #keycodeDispatch-keycodeTable-1
-  cmp keycodeTable,x
   beq +
   dex
   bpl -
   jmp viewControlInvalid
+  txa
   asl
   tax
   lda keycodeDispatch+0,x
   sta work+0
   lda keycodeDispatch+1,x
   sta work+1
   jmp (work+0)
viewControlInvalid = *
   lda #chrBEL
   jsr aceConPutctrl
   bit viewComplete
   bpl +
   jmp viewControlKey
+  jmp viewControlTop

keycodeTable = *
   .byte $91,$07,$16,$0c,"i"
   .byte $11,$0a,$17,$0f,"m"
   .byte $9d,$06,$19,$10,"j"
   .byte $1d,$0b,$1a,$15,"k"
   .byte "-","+"," ","n",chrCR
   .byte "0","$","?",$04,"q",$03
   .byte $13,$93
keycodeDispatch = *
   .word cursorUp,halfUp,pageUp,endUp,smallUp
   .word cursorDown,halfDown,pageDown,endDown,smallDown
   .word cursorLeft,halfLeft,pageLeft,endLeft,smallLeft
   .word cursorRight,halfRight,pageRight,endRight,smallRight
   .word gotoPrevArg,gotoNextArg,gotoNextArg,gotoNextArg,gotoNextArg
   .word gotoFirstArg,gotoLastArg,showName,showName,quitKey,quitKey
   .word cursorHome,cursorCenter

quitKey = *
   jmp gdie

gotoNextArg = *
   inc arg+0
   bne +
   inc arg+1
+  lda arg+0
   ldy arg+1
   jsr getarg
   beq +
   rts
+  lda arg+0
   bne +
   dec arg+1
+  dec arg+0
   jmp viewControlInvalid

gotoPrevArg = *
   lda arg+1
   bne +
   lda arg+0
   cmp #2
   bcs +
   jmp viewControlInvalid
+  lda arg+0
   bne +
   dec arg+1
+  dec arg+0
   rts

gotoFirstArg = *
   lda #1
   ldy #0
   sta arg+0
   sty arg+1
   rts

gotoLastArg = *
   lda aceArgc+0
   ldy aceArgc+1
   sec
   sbc #1
   bcs +
   dey
+  sta arg+0
   sty arg+1
   rts

cursorCenter = *
   sec
   lda imageCols
   sbc bmCols
   bcs +
   lda #0
   jmp ++
+  lsr
+  sta leftMargin
   sec
   lda imageRows+0
   sbc bmRows+0
   sta topMargin+0
   lda imageRows+1
   sbc bmRows+1
   sta topMargin+1
   bcs +
   lda #0
   sta topMargin+0
   sta topMargin+1
+  lsr topMargin+1
   ror topMargin+0
   jmp viewControlTop

cursorHome = *
   lda #0
   sta topMargin+0
   sta topMargin+1
   sta leftMargin
   jmp viewControlTop

endLeft = *
   lda leftMargin
   jmp +
pageLeft = *
   lda bmCols
   jmp +
halfLeft = *
   lda bmCols
   lsr
   jmp +
smallLeft = *
   lda smallDelta
   lsr
   lsr
   lsr
   jmp +
cursorLeft = *
   lda bmCols
   lsr
   lsr
+  sta work
   lda leftMargin
   bne +
   jmp viewControlInvalid
+  sec
   lda leftMargin
   sbc work
   bcs +
   lda #0
+  sta leftMargin
   jmp viewControlTop

endRight = *
   lda imageCols
   jmp +
pageRight = *
   lda bmCols
   jmp +
halfRight = *
   lda bmCols
   lsr
   jmp +
smallRight = *
   lda smallDelta
   lsr
   lsr
   lsr
   jmp +
cursorRight = *
   lda bmCols
   lsr
   lsr
+  sta work
   lda leftMargin
   cmp maxLeftMargin
   bcc +
   jmp viewControlInvalid
+  clc
   lda work
   adc leftMargin
   cmp maxLeftMargin
   bcc +
   lda maxLeftMargin
+  sta leftMargin
   jmp viewControlTop

endUp = *
   lda topMargin+0
   ldy topMargin+1
   sta work+0
   sty work+1
   jmp cursorUpWork
pageUp = *
   ldx #0
   jmp +
halfUp = *
   ldx #1
   jmp +
smallUp = *
   lda smallDelta
   ldy #0
   sta work+0
   sty work+1
   jmp cursorUpWork
cursorUp = *
   ldx #2
+  lda bmRows+0
   ldy bmRows+1
   sty work+1
   cpx #0
   beq +
-  lsr work+1
   ror
   dex
   bne -
+  sta work+0
   cursorUpWork = *
   lda topMargin+0
   ora topMargin+1
   bne +
   jmp viewControlInvalid
+  sec
   lda topMargin+0
   sbc work+0
   sta topMargin+0
   lda topMargin+1
   sbc work+1
   sta topMargin+1
   bcs +
   lda #0
   sta topMargin+0
   sta topMargin+1
+  jmp viewControlTop

endDown = *
   lda maxTopMargin+0
   ldy maxTopMargin+1
   sta work+0
   sty work+1
   jmp cursorDownWork
pageDown = *
   ldx #0
   jmp +
halfDown = *
   ldx #1
   jmp +
smallDown = *
   lda smallDelta
   ldy #0
   sta work+0
   sty work+1
   jmp cursorDownWork
cursorDown = *
   ldx #2
+  lda bmRows+0
   ldy bmRows+1
   sty work+1
   cpx #0
   beq +
-  lsr work+1
   ror
   dex
   bne -
+  sta work+0
   cursorDownWork = *
   lda topMargin+0
   cmp maxTopMargin+0
   lda topMargin+1
   sbc maxTopMargin+1
   bcc +
   jmp viewControlInvalid
+  clc
   lda topMargin+0
   adc work+0
   sta topMargin+0
   lda topMargin+1
   adc work+1
   sta topMargin+1
   lda topMargin+0
   cmp maxTopMargin+0
   lda topMargin+1
   sbc maxTopMargin+1
   bcc +
   lda maxTopMargin+0
   ldy maxTopMargin+1
   sta topMargin+0
   sty topMargin+1
+  jmp viewControlTop

showName = *
   jsr aceGrExit
   lda arg+0
   ldy arg+1
   ldx #$00
   sta work+0
   sty work+1
   stx work+2
   stx work+3
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #work
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr eputs
   lda #"."
   jsr eputchar
   lda #" "
   jsr eputchar
   jsr echoName
   lda #<showPkMsg
   ldy #>showPkMsg
   sta zp+0
   sty zp+1
   lda #<showPkMsgEnd-showPkMsg
   ldy #>showPkMsgEnd-showPkMsg
   ldx #stderr
   jsr write
   jsr aceConGetkey
   jsr startGraphics
   jmp viewControlTop
   numbuf: .buf 11
   showPkMsg = *
         ;----+----1----+----2----+----3----+----4
   .byte chrCR,chrCR,"Commands:",chrCR,chrCR
   .byte "UP,DOWN,LEFT,RIGHT: 1/4 page scroll",chrCR
   .byte "SH-RIGHT,SH-LEFT  : 1/2 page scroll l/r",chrCR
   .byte "SH-LINEFEED,LF    : 1/2 page scroll u/d",chrCR
   .byte "CT-LEFT,CT-RIGHT  : full page scroll l/r",chrCR
   .byte "CT-UP,CT-DOWN     : full page scroll u/d",chrCR
   .byte "i,m,j,k: small scroll up,down,left,right",chrCR
   .byte "1 to 9 : set small-scroll jump (times 8)",chrCR
   .byte "CO-LEFT,CO-RIGHT  : go to end l/r",chrCR
   .byte "CO-UP,CO-DOWN     : go to end u/d",chrCR
   .byte "HOME   : go to top left of image",chrCR
   .byte "CLR    : go to center of image",chrCR
   .byte "HELP   : display this help information",chrCR,chrCR
   .byte "(basic keyboard has equivalent cursor-",chrCR
   .byte " movement keys)",chrCR,chrCR
   .byte "Press a key..."
   showPkMsgEnd = *

view = *
   lda #$00
   sta viewComplete
   sta maxTopMargin+0
   sta maxTopMargin+1
   sta maxLeftMargin
   lda #$aa
   jsr aceGrFill
   jsr aceConKeyAvail
   bcs +
   rts
   ;** open file
+  lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   jsr aceGrExit
   jsr echoName
   lda #<openErrMsg
   ldy #>openErrMsg
   jsr eputs
   jsr aceConGetkey
   sta gotKeystroke
   jsr startGraphics
   rts
+  sta inFile
   ;** check and extract header information
   jsr VbmScanHeader
   bcs invalidFormat
   ;** display
   jsr viewBody
   ;** close
   lda inFile
   jsr close
   rts

   invalidFormat = *
   jsr aceGrExit
   jsr echoName
   lda #<invalidFormatMsg
   ldy #>invalidFormatMsg
   jsr eputs
   jsr aceConGetkey
   sta gotKeystroke
   jsr startGraphics
   lda inFile
   jsr close
   rts
   invalidFormatMsg: .byte ": invalid VBM format, press cmd key",chrCR,0

   openErrMsg: .byte ": cannot open, press cmd key",chrCR,0

echoName = *
   lda name+0
   ldy name+1
   jsr eputs
   rts

binReadLen: .buf 1
binReadPtr: .buf 1

binRead = *  ;( .A=bytes, inFile )
   sta binReadLen
   lda #0
   sta binReadPtr
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
-  ldx inFile
   sec
   lda binReadLen
   sbc binReadPtr
   ldy #0
   jsr read
   beq ++
   bcs ++
   tax
   clc
   adc binReadPtr
   sta binReadPtr
   clc
   txa
   adc zp+0
   sta zp+0
   bcc +
   inc zp+1
+  lda binReadPtr
   cmp binReadLen
   bcc -
   clc
   rts
+  sec  ;it should not happen that we hit eof while bytes have been read, or
   rts  ; else there's something wrong with the source file.

viewBody = *  ;( no pun intended... )
   ;** set default display margins
   lda #0
   sta displayRow+0
   sta displayRow+1
   sta displayCol
   sta inExpandByte
   sta inExpandCount
   ;** set maximum display margins
   sec
   lda imageCols
   sbc bmCols
   bcc +
   sta maxLeftMargin
+  sec
   lda imageRows+0
   sbc bmRows+0
   tax
   lda imageRows+1
   sbc bmRows+1
   bcc fiddleCols
   stx maxTopMargin+0
   sta maxTopMargin+1

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
   lda leftMargin
   cmp maxLeftMargin
   bcc fiddleRows
   lda maxLeftMargin
   sta leftMargin

   fiddleRows = *
   lda bmRows+0
   cmp imageRows+0
   lda bmRows+1
   sbc imageRows+1
   bcc +
   lda #0
   sta topMargin+0
   sta topMargin+1
   lda imageRows+0
   ldy imageRows+1
   sta displayRows+0
   sty displayRows+1
   jmp fiddleDisplayMargins
+  lda bmRows+0
   ldy bmRows+1
   sta displayRows+0
   sty displayRows+1
   clc
   lda topMargin+0
   adc bmRows+0
   sta work+0
   lda topMargin+1
   adc bmRows+1
   sta work+1
   lda imageRows+0
   cmp work+0
   lda imageRows+1
   sbc work+1
   bcs fiddleDisplayMargins
   sec
   lda imageRows+0
   sbc bmRows+0
   sta topMargin+0
   lda imageRows+1
   sbc bmRows+1
   sta topMargin+1

   fiddleDisplayMargins = *
   sec
   lda bmCols
   sbc imageCols
   bcc +
   lsr
   sta displayCol
+  sec
   lda bmRows+0
   sbc imageRows+0
   sta displayRow+0
   lda bmRows+1
   sbc imageRows+1
   sta displayRow+1
   php
   lsr displayRow+1
   ror displayRow+0
   plp
   bcs +
   lda #0
   sta displayRow+0
   sta displayRow+1
+  nop

   ;** skip top margin
   lda topMargin+0
   ldy topMargin+1
   ldx imageCols
   jsr skipPixelLines

   ;** display main portion of image
   viewNextLine = *
   jsr checkStop
   lda displayRows+0
   ora displayRows+1
   bne +
   lda #$ff
   sta viewComplete
-  rts
+  jsr aceConKeyAvail
   bcc -
   ldx leftMargin
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
   lda #<1
   ldy #>1
   sta syswork+2
   sty syswork+3
   lda #0
   sta syswork+4
   lda #<pixelBuf
   ldy #>pixelBuf
   sta syswork+6
   sty syswork+7
   lda #$40
   ldx displayCol
   ldy displayCols
   jsr aceGrOp
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
+  jsr checkStop
   jsr aceConKeyAvail
   bcs +
   rts
+  lda skipBytes
   jsr binReadBody
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
   jsr binReadBody
+  lda payDirt
   beq +
   jsr binReadBody
   ldx #0
-  lda inBuf,x
   sta pixelBuf,x
   inx
   cpx payDirt
   bcc -
+  lda skipRight
   beq +
   jsr binReadBody
+  rts

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

;=== new stuff ===

VbmScanHeader = * ;( inFile ) : .CS=err, variousParms
   lda #0
   sta imageRows+0
   sta imageRows+1
   sta imageCols
   lda #8
   jsr binRead
   bcc +
-  sec
   rts
+  lda inBuf+0
   cmp #"b"
   bne -
   lda inBuf+1
   cmp #"m"
   bne -
   lda inBuf+2
   cmp #$cb
   bne -
   lda inBuf+3
   cmp #VBMFMT_VERSION2
   beq +
   cmp #VBMFMT_VERSION3
   bne -
+  sta inFormat
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
   lda inFormat
   cmp #VBMFMT_VERSION3
   beq +
   lda #VBMFMT_UNCOMPRESSED
   sta inEncoding
   clc
   rts
+  lda #10
   jsr binRead
   bcs -
   lda inBuf+0
   cmp #VBMFMT_UNCOMPRESSED
   beq +
   cmp #VBMFMT_RLE_COMPRESSED
   bne -
+  sta inEncoding
   lda inBuf+1
   sta inRepCode
   lda inBuf+2
   sta inZeroCode
   lda inBuf+3
   sta inFfCode
   lda inBuf+4
   sta inDblZeroCode
   lda inBuf+5
   sta inDblFfCode
   lda inBuf+8
   ldy inBuf+9
   ;xx comment length - skip comment for now
   jsr GetByteInit ;in case it's used
   clc
   rts

binReadBodyLen: .buf 1

binReadBody = *  ;( .A=bytes, inFile )
   sta binReadBodyLen
   ldx inFormat
   cpx #VBMFMT_VERSION3
   beq +
   jsr binRead
   ldx #0
-  lda inBuf,x
   eor #$ff
   sta inBuf,x
   inx
   cpx binReadLen
   bcc -
   clc
   rts
+  ldx inEncoding
   cpx #VBMFMT_RLE_COMPRESSED
   beq +
   jmp binRead
+  ldx #0
-  jsr VbmConvertGetByte
   sta inBuf,x
   inx
   cpx binReadBodyLen
   bcc -
   clc
   rts

VbmConvertGetByte = *  ;( inFile ) : .A=byte, .X=preserved
   stx work+0
   lda inExpandCount
   bne vbmConvGotByte
   jsr GetByte
   sta inExpandByte
   ldx #1
   stx inExpandCount
   cmp inRepCode
   bne +
   jsr GetByte
   sta inExpandByte
   jsr GetByte
   sta inExpandCount
   jmp vbmConvGotByte
+  cmp inZeroCode
   bne +
   lda #$00
   sta inExpandByte
   jsr GetByte
   sta inExpandCount
   jmp vbmConvGotByte
+  cmp inFfCode
   bne +
   lda #$ff
   sta inExpandByte
   jsr GetByte
   sta inExpandCount
   jmp vbmConvGotByte
+  cmp inDblZeroCode
   bne +
   lda #$00
   sta inExpandByte
   lda #2
   sta inExpandCount
   jmp vbmConvGotByte
+  cmp inDblFfCode
   bne vbmConvGotByte
   lda #$ff
   sta inExpandByte
   lda #2
   sta inExpandCount

   vbmConvGotByte = *
   ldx work+0
   dec inExpandCount
   lda inExpandByte
   rts

GetByteInit = *
   lda #0
   sta getBytePtr
   sta getByteCount
   rts

GetByte = *  ;( ) : .A=byte, .CS=err
   lda getByteCount
   beq +
   ldy getBytePtr
   lda readBuf,y
   inc getBytePtr
   dec getByteCount
   clc
   rts
+  lda #<readBuf
   ldy #>readBuf
   sta zp+0
   sty zp+1
   lda #255
   ldy #0
   ldx inFile
   jsr read
   beq +
   bcs +
   sta getByteCount
   lda #0
   sta getBytePtr
   jmp GetByte
+  lda #$00
   sec
   rts

;===bss===

bss      = *
pixelBuf = bss+0
inBuf    = pixelBuf+256
readBuf  = inBuf+256
bssEnd   = readBuf+256
