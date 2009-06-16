;*** "more" text file displayer version 1.00 - by Craig Bruce - 23-Feb-94

;more [-help] file ...

.seq "acehead.s"
.org aceAppAddress
.obj "@0:more"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

true   = $ff
false  = $00

arg         = 2  ;(2)  ;current argument number
name        = 4  ;(2)  ;name of file being bcoded
row         = 6  ;(1)  ;current row on display
rowAddr     = 8  ;(2)  ;address of current row on display
rowInc      = 10 ;(2)  ;increment between successive rows on display
rowHome     = 12 ;(2)  ;address of home position on display
scrRows     = 14 ;(1)  ;number of rows on display
scrCols     = 15 ;(1)  ;number of columns on display
fileLine    = 16 ;(4)  ;line number in current file
inBufPtr    = 20 ;(1)  ;next byte to extract from input buffer
inBufValid  = 21 ;(1)  ;number of valid bytes in input buffer
inBufFd     = 22 ;(1)  ;file descriptor for input
pos         = 23 ;(1)  ;scan position
glBufChar   = 24 ;(1)  ;buffered character for getline
glBufEof    = 25 ;(1)  ;whether buffed char is eof for getline
promptColor = 26 ;(1)  ;color of "more" prompt
textColor   = 27 ;(1)  ;color of regular text
promptLen   = 28 ;(1)  ;length of prompt string
catStrPtr   = 30 ;(2)  ;string pointer for cat operation
cursorColor = 32 ;(1)  ;color of cursor on "more" prompt
fileParm    = 33 ;(1)  ;flag that a filename has appeared

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
   .asc "Insufficient program space to run more"
   .byte chrCR,0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg = *
   .asc "usage: more [-help] file ..."
   .byte chrCR,0

windowMsg = *
   .asc "error: display must have at least 10 columns and 2 rows."
   .byte chrCR,0

mainInit = *
   lda #false
   sta fileParm
   lda #0
   sta arg+0
   sta arg+1
   jsr aceWinSize
   sta scrRows
   stx scrCols
   cpx #10
   bcc +
   cmp #2
   bcs ++
+  lda #<windowMsg
   ldy #>windowMsg
   jsr eputs
   jmp die
+  lda syswork+2
   ldy syswork+3
   sta rowHome+0
   sty rowHome+1
   lda syswork+4
   ldy #0
   sta rowInc+0
   sty rowInc+1

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
+  jsr more
   jmp mainNext

mainExit = *
   lda fileParm
   beq +
   jsr scrShutdown
+  rts

checkStop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   jmp die

   stoppedMsg = *
   .asc "<Stopped>"
   .byte chrCR,0

more = *
   lda fileParm
   bne +
   lda #true
   sta fileParm
   jsr scrInit
+  jsr scrClear
   lda #0
   ldx #3
-  sta fileLine,x
   dex
   bpl -
   lda name+0
   ldy name+1
   jsr initGetbyte
   bcc +
   lda #2
   jsr prompt
   cmp #"q"
   beq moreExit
   cmp #$03
   beq moreExit
   rts
+  jsr getlineInit
   bcs +

   moreNextLine = *
   jsr getline
   bcs moreEOF
   jsr putline
   ldx row
   inx
   cpx scrRows
   bcc moreNextLine
   bit glBufEof
   bmi moreEOF
   morePrompt = *
   lda #0
   jsr prompt
   cmp #"q"
   beq moreExit
   cmp #$03
   beq moreExit
   cmp #"n"
   bne +
   lda inBufFd
   jsr close
   rts
+  cmp #chrCR
   beq moreCR
   cmp #$11
   beq moreCR
   cmp #"b"
   beq moreBack
   jsr scrClear
   jmp moreNextLine

   moreEOF = *
   lda #1
   jsr prompt
   cmp #"q"
   beq moreExit
   cmp #$03
   beq moreExit
   lda inBufFd
   jsr close
   rts

moreExit = *
   lda inBufFd
   jsr close
   jsr scrShutdown
   lda #0
   ldx #0
   jmp aceProcExit

moreCR = *
   ldx row
   inx
   cpx scrRows
   bcc +
   lda #" "
   sta syswork+4
   lda #$88
   ldx #1
   jsr aceWinScroll
   dec row
   sec
   lda rowAddr+0
   sbc rowInc+0
   sta rowAddr+0
   lda rowAddr+1
   sbc rowInc+1
   sta rowAddr+1
+  jmp moreNextLine

moreBack = *
   lda #chrBEL
   jsr putchar
   jmp morePrompt

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
   putcBuffer .buf 1

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

initGetbyte = *  ;( (.AY)=filename ) : .CS=openError
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcs +
   sta inBufFd
   lda #0
   sta inBufValid
   sta inBufPtr
   clc
+  rts

getbyte = *  ;( ) : .A=byte, .CS=eof
   ldx inBufPtr
   cpx inBufValid
   bcs fillInBuf
   lda inBuf,x
   inc inBufPtr
   clc
   rts

fillInBuf = *
   jsr checkStop
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
   lda #<250
   ldy #>250
   ldx inBufFd
   jsr read
   beq +
   bcs +
   sta inBufValid
   lda #0
   sta inBufPtr
   jmp getbyte
+  lda #0
   sta inBufPtr
   sta inBufValid
   sec
   rts

getlineInit = *
   lda #false
   sta glBufEof
   jmp getlineExit

getline = *
   ldx #0
   stx pos
   sec
   lda glBufEof
   bne +
   lda glBufChar
   clc
   bcc +

   getlineNextChar = *
   jsr getbyte
+  bcs getlineEOF
   cmp #chrCR
   beq getlineCR
   cmp #chrTAB
   beq getlineTAB
   cmp #chrCLS
   ;beq getlineCLR
   ldx pos
   sta line,x
   inx
   stx pos
   cpx scrCols
   bcc getlineNextChar
   ;** line wrap
   jsr getbyte
   cmp #chrCR
   beq getlineCR
   clc
   bcc getlineExitNoGet

getlineExit = *
   jsr getbyte
   getlineExitNoGet = *
   sta glBufChar
   bcc +
   lda #true
   sta glBufEof
+  lda pos
   sta lineLen
   clc
   rts

getlineCR = *
   inc fileLine+0
   bne +
   inc fileLine+1
   bne +
   inc fileLine+2
   bne +
   inc fileLine+3
+  jmp getlineExit

getlineEOF = *
   lda #true
   sta glBufEof
   sec
   rts

glTabAdd .buf 1

getlineTAB = *
   ldx pos
   ldy #8
   lda #" "
-  sta line,x
   inx
   dey
   bne -
   lda pos
   and #7
   sta glTabAdd
   sec
   lda #8
   sbc glTabAdd
   clc
   adc pos
   cmp scrCols
   bcc +
   jmp getlineExit
+  sta pos
   jmp getlineNextChar

getlineCLR = *
   brk

scrInit = *
   jsr aceWinPalette
   lda syswork+4
   sta promptColor
   lda syswork+0
   sta textColor
   lda syswork+1
   sta cursorColor
   lda #$40
   ldx #" "
   ldy syswork+0
   jsr aceWinCls
   ;** fall through

scrClear = *
   lda #$80
   ldx #" "
   jsr aceWinCls
   lda #0
   sta row
   lda rowHome+0
   ldy rowHome+1
   sta rowAddr+0
   sty rowAddr+1
   rts

scrShutdown = *
   sec
   lda scrRows
   sbc #1
   ldx #0
   jsr aceConPos
   rts

putline = *
   lda rowAddr+0
   ldy rowAddr+1
   sta syswork+0
   sty syswork+1
   lda #<line
   ldy #>line
   sta syswork+2
   sty syswork+3
   lda #$80
   ldx lineLen
   stx syswork+5
   jsr aceWinPut
   inc row
   clc
   lda rowAddr+0
   adc rowInc+0
   sta rowAddr+0
   lda rowAddr+1
   adc rowInc+1
   sta rowAddr+1
   rts

scrScroll = *
   brk

promptChar .buf 1

prompt = *  ;(.A=promptType{0:more,1:eof,2:error}) : .A=keyChar
   ;** get prompt string
   ldx #0
   stx string+0
   asl
   tax
   lda promptTab+1,x
   tay
   lda promptTab+0,x
   jsr catString
   lda name+0
   ldy name+1
   jsr catString
   lda #<promptColon
   ldy #>promptColon
   jsr catString
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #fileLine
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   jsr catString
   lda #<promptClose
   ldy #>promptClose
   jsr catString
   stx promptLen
   inx
   cpx scrCols
   bcc +
   ldx scrCols
   dex
   stx promptLen
   ;** display prompt
+  lda rowAddr+0
   ldy rowAddr+1
   sta syswork+0
   sty syswork+1
   lda #<string
   ldy #>string
   sta syswork+2
   sty syswork+3
   lda #$40
   sta syswork+6
   lda #$e0
   ldx promptLen
   stx syswork+5
   ldy promptColor
   jsr aceWinPut
   ;** display cursor
   clc
   lda rowAddr+0
   adc promptLen
   sta syswork+0
   lda rowAddr+1
   adc #0
   sta syswork+1
   lda #$ff
   ldy cursorColor
   jsr aceWinCursor
   ;** get key
   jsr aceConGetkey
   sta promptChar
   ;** erase cursor
   clc
   lda rowAddr+0
   adc promptLen
   sta syswork+0
   lda rowAddr+1
   adc #0
   sta syswork+1
   lda #$00
   jsr aceWinCursor
   ;** erase prompt
   lda rowAddr+0
   ldy rowAddr+1
   sta syswork+0
   sty syswork+1
   lda #" "
   sta syswork+4
   lda promptLen
   sta syswork+5
   lda #$c0
   ldx #0
   ldy textColor
   jsr aceWinPut
   lda promptChar
   rts

promptTab .word promptMore,promptEOF,promptError
promptMore = *
   .asc "--More-- ("
   .byte 0
promptEOF = *
   .asc "--EOF-- ("
   .byte 0
promptError = *
   .asc "--ERROR!-- ("
   .byte 0
promptColon = *
   .asc ":"
   .byte 0
promptClose = *
   .asc ")"
   .byte 0

catString = *  ;( (.AY)=string ) : .X=strLen
   sta catStrPtr+0
   sty catStrPtr+1
   ldx #255
-  inx
   lda string,x
   bne -
   ldy #0
-  lda (catStrPtr),y
   sta string,x
   beq +
   iny
   inx
   bne -
+  rts

rvsString = *
   ldy #0
   rvsStringNext = *
   lda string,y
   bne +
   rts
+  pha
   sec
   sbc #32
   and #%01000000
   bne +
   pla
   sec
   sbc #64
   jmp ++
+  pla
   clc
   adc #64
+  sta string,y
   iny
   jmp rvsStringNext

;===bss===

bss = *
inBuf    = bss+0
linePrev = inBuf+256+0
lineNext = inBuf+256+4
lineNum  = inBuf+256+8
lineLen  = inBuf+256+12
line     = inBuf+256+13
string   = linePrev+256
numbuf   = string+256
bssEnd   = numbuf+12
