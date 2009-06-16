;*** wrap program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:wrap"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

work1 = $02 ;(16)
work2 = $12 ;(16)
work3 = $22 ;(14)

chrQuote = $22

;screen

scrTopAddr      = $30 ;(2)
scrLineAddr     = $32 ;(2)
curRow          = $34 ;(1)
curCol          = $35 ;(1)
scrRows         = $36 ;(1)
scrCols         = $37 ;(1)
scrRowInc       = $38 ;(2)
curLeftMargin   = $3a ;(1)

;document

linePtr         = $40 ;(4)
lineNum         = $44 ;(4)
tosLinePtr      = $48 ;(4)
topLinePtr      = $4c ;(4)
botLinePtr      = $50 ;(4)
lineCount       = $54 ;(4)
fileLength      = $58 ;(4)
targetLen       = $5c ;(1)
wrapFlag        = $5d ;(1) ;128=wrap,64=showCR

;management

modified        = $3b ;(1) ;$00=no, $ff=modified
modeFlags       = $3c ;(1) ;$80=insert, $40=indent
statusUpdate    = $3d ;(1) ;128=line,64=col,32=mod,16=ins,8=byt,4=fre,2=nm,1=msg
markedLinePtr   .buf 4 ;(4)
markedLineNum   .buf 4 ;(4)
markedCol       .buf 4 ;(1)

;line format

headNextPtr     = $70 ;(4)
headPrevPtr     = $74 ;(4)
headLineLen     = $78 ;(1)
headIndent      = $79 ;(1)
headFlags       = $7a ;(1) ;$80=connected, $40=nullLine
headLength      = $0b

;kill buffer

bufferTopPtr    .buf 4 ;(4)
bufferBotPtr    .buf 4 ;(4)
bufferLineCount .buf 4 ;(4)

;document buffers

;=== main ===

main = *
   jsr mainInit
   lda #1
   ldy #0
   jsr getarg
   jsr loadFile
   rts

mainInit = *
   lda #$80
   sta wrapFlag
   lda #75
   sta targetLen
   rts

;*** load file: uses work2 ***

loadHead  = work2+0
loadTail  = work2+4
loadLines = work2+8
loadBytes = work2+12
loadFcb      = work3+0
loadLineScan = work3+1
loadLineLen  = work3+2
loadBufCount = work3+3
loadBufPtr   = work3+4

loadFile = *  ;( (zp)=name ) : [w2]=head, [w2]=tail, [w2]=lines, [w2]=bytes
   jsr saveWork3
   jsr loadInit
   lda #"r"
   jsr open
   sta loadFcb
-  jsr loadLine
   bcs +
   jsr loadLineWrap
   jsr loadLineStore
   jsr loadLineOverflow
   jmp -
+  lda loadLineLen
   beq +
   jsr loadLineWrap
   lda #$00
   sta lineFlags
   jsr loadLineStore
+  lda loadFcb
   jsr close
   jsr restoreWork3
   rts

loadInit = *
   lda #0
   ldx #16+14-1
-  sta loadHead,x
   dex
   bpl -
   rts

loadLine = *  ;( ) : .CS=end
   ;tab expansion will go into this routine
   ldx loadBufPtr
   ldy loadLineLen

   loadNextByte = *
   lda loadBufCount
   bne ++
   sty loadLineLen
   jsr loadBuf
   bcc +
   rts
+  ldy loadLineLen
   ldx loadBufPtr
+  nop

-  lda filebuf,x
   sta line,y
   inx
   iny
   cmp #chrCR
   beq ++
   cpy targetLen
   beq +  ;determines if CRs will go beyond len
   bcs ++
+  dec loadBufCount
   bne -
   beq loadNextByte

+  dec loadBufCount
   stx loadBufPtr
   sty loadLineLen
   clc
   rts

loadBuf = *  ;( ) : .CS=eof
   jsr aceConStopkey
   bcs +
   lda #<filebuf
   ldy #>filebuf
   sta zp+0
   sty zp+1
   lda #<254
   ldy #>254
   ldx loadFcb
   jsr read
   bcs +
   beq +
   sta loadBufCount
   lda #0
   sta loadBufPtr
   clc
   rts
+  sec
   rts

loadLineWrap = *
   ldx loadLineLen
   dex
   ldy #$00
   lda line,x
   cmp #chrCR
   beq +
   ldy #$80
+  sty lineFlags
   cmp #chrCR
   bne +
-  stx lineLineLen
   stx loadLineScan
   stx loadLineLen
   rts

+  ldx loadLineLen
   cpx targetLen
   bcc -

+  bit wrapFlag
   bmi +
-  lda targetLen
   sta loadLineScan
   sta lineLineLen
   rts

+  ldx targetLen
-  dex
   cpx #255
   beq --
   lda line,x
   cmp #" "
   bne -
+  inx
   stx loadLineScan
   stx lineLineLen
   rts

loadLineStore = *
   lda #<line
   ldy #>line
   sta zp+0
   sty zp+1
   lda lineLineLen
   ldy #0
   ldx #stdout
   jsr write
   lda #chrCR
   jsr putchar
   rts

loadLineOverflow = *
   ldx loadLineScan
   ldy #0
-  cpx loadLineLen
   bcs +
   lda line,x
   sta line,y
   inx
   iny
   bne -
+  sty loadLineLen
   rts

;=== management routines ===

work3Save .buf 14

saveWork3 = *
   ldx #13
-  lda work3,x
   sta work3Save,x
   dex
   bpl -
   rts

restoreWork3 = *
   ldx #13
-  lda work3Save,x
   sta work3,x
   dex
   bpl -
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

putchar = *
   stx xsave
   sty ysave
   ldx #stdout
   jsr putc
   ldx xsave
   ldy ysave
   rts
   xsave .buf 1
   ysave .buf 1

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
   getcBuffer .buf 1

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

;===bss===

bss             = *
linebuf         = bss+0  ;(256)
lineNextPtr     = linebuf+$0 ;(4)
linePrevPtr     = linebuf+$4 ;(4)
lineLineLen     = linebuf+$8 ;(1)
lineIndent      = linebuf+$9 ;(1)
lineFlags       = linebuf+$a ;(1) ;$80=connected, $40=tailLine
line            = linebuf+$b ;(241)

filebuf         = linebuf+256  ;(256)
tpaFreemap      = filebuf+256  ;(256)
bssEnd          = tpaFreemap+256
