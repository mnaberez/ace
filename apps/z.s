;*** Zed for ACE - version 0.50 - by Craig Bruce - started 30-July-1995

;this program is in the ACE-assembler format

.include "acehead.s"

.org aceAppAddress

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;====== global declarations ======

work1 = $02 ;(16)  ;used by malloc
work2 = $12 ;(16)  ;used by file-load
work3 = $22 ;(14)

chrQuote = $22

;screen

scrTopAddr      = $30 ;(2)  ;screen address of the top line on the display
scrLineAddr     = $32 ;(2)  ;screen address of the current line on display
scrRow          = $34 ;(1)  ;row number of the current line on the display
scrCol          = $35 ;(1)  ;virtual screen column number of cursor position
scrRows         = $36 ;(1)  ;number of rows on the display
scrCols         = $37 ;(1)  ;number of columns on the display
scrStartRow:    .buf 1      ;starting row on the display
scrStartCol:    .buf 1      ;starting column on the display
scrRowInc       = $38 ;(1)  ;row increment for the display
scrLeftMargin   = $39 ;(1)  ;left margin for displaying lines
statusMargin:   .buf 1      ;left margin of the status line on the display

;management

targetLen       = $3a ;(1)  ;length to display lines
wrapFlag        = $3b ;(1)  ;$80=wrap,$40=showCR
modified        = $3c ;(1)  ;$00=no, $ff=modified
modeFlags       = $3d ;(1)  ;$80=insert, $40=indent
statusUpdate    = $3e ;(1)  ;128=lin,64=col,32=mod,16=ins,8=byt,4=fre,2=nm,1=msg
markedLinePtr:  .buf 4      ;line that is marked, NULL of none
markedLineNum:  .buf 4      ;line number that is marked
markedSolCol:   .buf 4      ;start-of-line column number
markedCol:      .buf 1      ;column of marked logical line

;document buffer

linePtr         = $40 ;(4)  ;pointer to current line
lineNum         = $44 ;(4)  ;number of current physical line
solColNum       = $48 ;(4)  ;column number of the beginning of the cur line [NI]
headLinePtr     = $4c ;(4)  ;pointer to the special header/trailer line
lineCount       = $50 ;(4)  ;number of physical lines in buffer
byteCount       = $54 ;(4)  ;number of bytes in buffer

;line format

headBuffer      = $70 ;(10) ;buffer for holding the head of the current line
headNext        = $70 ;(4)  ;pointer to the next line in a document
headPrev        = $74 ;(4)  ;pointer to the prev line in a document
headLineLen     = $78 ;(1)  ;length of the text line
headFlags       = $79 ;(1)  ;$80=CR-end, $40=headerLine, &$3F=indent
headLength      = 10        ;length of the line header

;kill buffer

killBufHeadPtr: .buf 4      ;pointer to special header/trailer line of kill buf
killLineCount:  .buf 4      ;number of lines in kill buffer
killByteCount:  .buf 4      ;number of bytes in kill buffer

;colors

conColor:     .buf 8
keychar       = $58 ;(1)
keyshift      = $59 ;(1)
sameKeyCount: .buf 1
sameKeyChar:  .byte $00
sameKeyShift: .byte $ff
exitFlag:     .buf 1
arg:          .buf 2
temp          = $60 ;(4)

;====== main ======

main = *
   jsr MainInit
   jsr MallocInit
   jsr ScreenInit
   lda #1
   ldy #0
   sta arg+0
   sty arg+1
-  lda arg+0
   ldy arg+1
   jsr getarg
   beq +
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jsr HandleArgFlag
   inc arg+0
   bne -
   inc arg+1
   jmp -
+  bne +
   lda #<nonameStr
   ldy #>nonameStr
   sta zp+0
   sty zp+1
+  lda zp+0
   ldy zp+1
   sta loadName+0
   sty loadName+1
   ldy #0
-  lda (zp),y
   sta docbufFilename,y
   beq +
   iny
   bne -
+  sty docbufFilenameLen
   lda #$02
   jsr SetUpdateStat
   jsr UpdateStatus
   lda #<loadingMsg
   ldy #>loadingMsg
   jsr DisplayMessage
   jsr LoadFile
   ldx #3
-  lda loadHead,x
   sta headLinePtr,x
   lda loadLines,x
   sta lineCount,x
   lda loadBytes,x
   sta byteCount,x
   dex
   bpl -
   lda loadError
   bne +
   lda #<versionMsg
   ldy #>versionMsg
   jsr DisplayMessage
   jmp ++
+  lda #<loadErrorMsg
   ldy #>loadErrorMsg
   jsr DisplayMessage
+  lda #$ff
   jsr SetUpdateStat
   jsr CmdEndUp
   lda #2
   ldx #0
   jsr CursorPos
-  jsr GetKey
   jsr CmdDispatch
   bit exitFlag
   bpl -
   lda #$40
   ldy conColor+0
   jsr aceWinCls
   lda scrRows
   sec
   sbc #1
   ldx #0
   jsr aceConPos
   lda #$f1
   jsr aceConPutctrl
   rts

versionMsg: .byte "ZED for ACE version 0.50 by Craig Bruce, 17-Dec-1995",0
            .byte "for Angela.",0
loadingMsg: .byte "Loading...",0
loadErrorMsg:.byte "Load Error!",0

nonameStr: .byte "noname",0

MainInit = *
   ldx #$7f-$02
   lda #0
-  sta 2,x
   dex
   bpl -
   lda #$c0  ;wp mode
   lda #$80  ;text mode
   sta wrapFlag
   jsr aceWinSize
   ;;ldx #240  ;long lines
   stx targetLen
   lda #$c0  ;ins+ind
   lda #$80  ;ins only
   sta modeFlags
   jsr InitRubBuffer
   lda #0
   sta docbufFilenameLen
   sta docbufFilename+0
   rts

;====== screen ======

ScreenInit = *
   ;** get screen parms
   jsr aceWinSize
   sta scrRows
   stx scrCols
   lda syswork+0
   ldx syswork+1
   sta scrStartRow
   stx scrStartCol
   lda syswork+2
   ldy syswork+3
   sta scrTopAddr+0
   sty scrTopAddr+1
   lda syswork+4
   sta scrRowInc
   ;** get color palette
   jsr aceWinPalette
   ldx #7
-  lda syswork,x
   sta conColor,x
   dex
   bpl -
   jsr GetPaletteChars
ScreenInitDisplay = *
   ;** clear screen
   lda #$c0
   ldx #$20
   ldy conColor+0
   jsr aceWinCls
   lda #2
   ldx #0
   jsr aceConPos
   ;** set status color
   lda scrTopAddr+0
   ldy scrTopAddr+1
   sta syswork+0
   sty syswork+1
   lda scrCols
   sta syswork+5
   lda #$40
   ldy conColor+2
   ldx #0
   jsr aceWinPut
   jsr DisplaySeparator
   jsr DisplayStatus
   lda #$fe
   sta statusUpdate
   jsr UpdateStatus
   rts

DisplayStatus = *
   lda scrTopAddr+0
   ldy scrTopAddr+1
   sta syswork+0
   sty syswork+1
   lda #<statusLine
   ldy #>statusLine
   clc
   adc statusMargin
   bcc +
   iny
+  sta syswork+2
   sty syswork+3
   lda scrCols
   sta syswork+5
   lda #" "
   sta syswork+4
   lda #$80
   ldx #80
   cpx scrCols
   bcc +
   ldx scrCols
+  jsr aceWinPut
   rts

statusLine = *
   .byte "L:        C:                  B:        "
       ;  0----+----1----+----2----+----3----+----
       ;  L:12345678C:1234 *  Ins  Ind  B:12345678
   .byte " F:           D:1                       "
       ;  4----+----5----+----6----+----7----+----
       ;   F:12345678   D:123 _2345678901234567890

statMask = work1+4
statIndex= work1+5

UpdateStatus = *
   lda #$80
   sta statMask
   lda #0
   sta statIndex
-  lda statMask
   and statusUpdate
   beq +
   ldx statIndex
   lda updateDispatch+0,x
   sta work1+0
   lda updateDispatch+1,x
   sta work1+1
   jsr ++
+  inc statIndex
   inc statIndex
   lsr statMask
   bne -
   lda statusUpdate
   and #$01
   sta statusUpdate
   rts
+  jmp (work1+0)

updateDispatch = *
   .word UpdateLine, UpdateCol, UpdateModified, UpdateInsInd, UpdateBytes
   .word UpdateFree, UpdateName, UpdateNull

UpdateNull: rts

UpdateLine = *
   ldx #lineNum
   lda #8
   ldy #2
   jmp UpdateNumber

UpdateCol = *
   ldx scrCol
   inx
   stx work1+0
   ldx #2
   lda #$00
-  sta work1+1,x
   dex
   bpl -
   ldx #work1+0
   lda #4
   ldy #12
   jmp UpdateNumber

UpdateModified = *
   lda #"*"
   bit modified
   bmi +
   lda #" "
+  sta statusLine+17
   lda #17
   ldx #1
   jmp PutStatField

UpdateInsInd = *
   jsr +
   jsr ++
   lda #20
   ldx #8
   jmp PutStatField
+  ldx #uinsInsMsg-uinsInsMsg
   ldy #20
   bit modeFlags
   bmi uinsShowOn
   bpl uinsShowOff
+  ldx #uinsIndMsg-uinsInsMsg
   ldy #25
   bit modeFlags
   bvs uinsShowOn
   uinsShowOff = *
   ldx #uinsOffMsg-uinsInsMsg
   uinsShowOn = *
   lda #3
   sta temp
-  lda uinsInsMsg,x
   sta statusLine,y
   inx
   iny
   dec temp
   bne -
   rts
   uinsInsMsg: .byte "Ins"
   uinsIndMsg: .byte "Ind"
   uinsOffMsg: .byte "   "

UpdateBytes = *
   ldx #byteCount
   lda #8
   ldy #32
   jmp UpdateNumber

UpdateFree = *
   ldx #work1+8
   jsr aceMemStat
   ldx #work1+8
   lda #8
   ldy #43
   jmp UpdateNumber

STAT_NAME_LEN = 20
nameField = statusLine+60

UpdateName = *
   lda #" "
   ldx #STAT_NAME_LEN-1
-  sta nameField,x
   dex
   bpl -
   lda docbufFilenameLen
   bne +
   rts
   ;** name shorter than or same as field
+  ldx #0
   sec
   lda #STAT_NAME_LEN
   sbc docbufFilenameLen
   bcc +
   tay
   jmp ++
   ;** name longer than field
+  lda charPalLeft
   sta nameField+0
   ldy #1
   sec
   lda docbufFilenameLen
   sbc #STAT_NAME_LEN
   tax
   inx
+- lda docbufFilename,x
   sta nameField,y
   inx
   iny
   cpy #STAT_NAME_LEN
   bcc -
   lda #nameField-statusLine
   ldx #STAT_NAME_LEN
   jmp PutStatField

UpdateNumber = *  ;( .X=var32, .A=maxlen, .Y=offset )
   sta numMaxLen
   sty numOffset
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
-  cpy numMaxLen
   bcs +
   lda #" "
   sta numbuf,y
   iny
   bne -
+  ldx #0
   ldy numOffset
-  lda numbuf,x
   sta statusLine,y
   iny
   inx
   cpx numMaxLen
   bcc -
   lda numOffset
   ldx numMaxLen
   jmp PutStatField
   numbuf:    .buf 11
   numMaxLen: .buf 1
   numOffset: .buf 1

PutStatField = *  ;( .A=fieldOff, .X=len )
   ;** check start of field
   cmp statusMargin
   bcs +
   sec
   sta temp
   lda statusMargin
   sbc temp
   sta temp
   sec
   txa
   sbc temp
   txa
   lda statusMargin
   bcs +
-  rts
   ;** check field length
+  sta temp
   stx temp+1
   clc
   adc temp+1
   sta temp+2
   clc
   lda statusMargin
   adc scrCols
   sta temp+3
   sec
   lda temp+2
   sbc temp+3
   bcc +
   sta temp+3
   sec
   txa
   sbc temp+3
   bcc -
   tax
   beq -
+  lda temp
   ;** set data and screen pointers
   pha
   ldy #>statusLine
   clc
   adc #<statusLine
   bcc +
   iny
+  sta syswork+2
   sty syswork+3
   pla
   sec
   sbc statusMargin
   ldy scrTopAddr+1
   clc
   adc scrTopAddr+0
   bcc +
   iny
+  sta syswork+0
   sty syswork+1
   ;** display
   lda #$80
   stx syswork+5
   jmp aceWinPut

UndisplayMessage = *
DisplaySeparator = *
   clc
   lda scrTopAddr+0
   adc scrRowInc
   sta syswork+0
   lda scrTopAddr+1
   adc #$00
   sta syswork+1
   lda #"-"
   sta syswork+4
   lda scrCols
   sta syswork+5
   lda #$c0
   ldx #0
   ldy conColor+3
   jsr aceWinPut
   rts

DisplayMessage = *  ;( (.AY)=messageString )
   sta syswork+2
   sty syswork+3
   ldy #$ff
-  iny
   lda (syswork+2),y
   sta messageBuffer,y
   bne -
   clc
   lda syswork+2
   adc statusMargin
   sta syswork+2
   bcc +
   inc syswork+3
+  sec
   tya
   sbc statusMargin
   bcs +
-  rts
+  beq -
   tay
   clc
   lda scrTopAddr+0
   adc scrRowInc
   sta syswork+0
   lda scrTopAddr+1
   adc #$00
   sta syswork+1
   cpy scrCols
   bcc +
   ldy scrCols
+  sty syswork+5
   ldx syswork+5
   ldy conColor+4
   lda #$40
   sta syswork+6
   lda #$e0
   jsr aceWinPut
   lda #$01
   jmp SetUpdateStat

displayRow:   .buf 1
displayLimit: .buf 1
displayFill:  .buf 1
displayDebugColor: .byte $00

DisplayScreen = *  ;( [mp]=firstLinePtr, .A=startScrLine, .Y=endScrLine+1 )
   sta displayRow
   sty displayLimit
   cmp displayLimit
   bcc +
   rts
+  ldx #0
   stx displayFill
   jsr aceWinPos
-  bit displayFill
   bmi +
   jsr FetchLine
   ;** put in CR if necessary
   bit wrapFlag
   bvc +
   bit headFlags
   bpl +
   ldx headLineLen
   lda charPalCR
   sta line,x
   inc headLineLen
+  lda #<line
   ldy #>line
   clc
   adc scrLeftMargin
   bcc +
   iny
+  sta syswork+2
   sty syswork+3
   lda #$20   ;;$80 ;;$20
   sta syswork+4
   lda scrCols
   sta syswork+5
   bit displayFill
   bmi +
   sec
   lda headLineLen
   sbc scrLeftMargin
   bcc +
   cmp scrCols
   bcc ++
   lda scrCols
   jmp ++
+  lda #0
+  tax
   lda #$80
   ldy displayDebugColor
   beq +
   lda displayDebugColor
   and #$0f
   ora #$00
   tay
   lda #$c0
+  jsr aceWinPut
   clc
   lda syswork+0
   adc scrRowInc
   sta syswork+0
   bcc +
   inc syswork+1
+  bit headFlags
   bvc +
   lda #$80
   sta displayFill
+  bit displayFill
   bmi +
   ldx #3
-  lda headNext,x
   sta mp,x
   dex
   bpl -
+  inc displayRow
   lda displayRow
   cmp displayLimit
   bcs +
   jmp --
+  rts

;====== load file: uses work2 ======

loadName     = work2+0  ;(2)
loadError    = work2+2  ;(1) not0==err, message displayed
loadHead     = work2+4  ;(4)
loadLines    = work2+8  ;(4)
loadBytes    = work2+12 ;(4)
loadTail     = work3+0  ;(4)
loadFcb      = work3+4  ;(1)
loadLineScan = work3+5  ;(1)
loadLineLen  = work3+6  ;(1)
loadBufCount = work3+7  ;(1)
loadBufPtr   = work3+8  ;(1)

LoadFile = *  ;( loadName ) : [w2]=head, [w2]=tail, [w2]=lines, [w2]=bytes
   jsr SaveWork3
   lda #$ff
   sta loadError
   jsr LoadInit
   bcc +
   rts
+  lda loadName+0
   ldy loadName+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcs loadExit
+  sta loadFcb
-  jsr LoadLine
   bcs ++
+  jsr LoadLineWrap
   jsr LoadLineStore
   jsr LoadLineOverflow
   jmp -
+  lda loadLineLen
   beq +
   sta headLineLen
   lda #$00
   sta headFlags
   jsr LoadLineStore
+  lda loadFcb
   jsr close
   lda #$00
   sta loadError
   loadExit = *
   jsr LoadFinish
   jsr RestoreWork3
   rts

LoadInit = *  ;() : .CS=err
   lda #0
   ldx #16+14-1-2
-  sta work2+2,x
   dex
   bpl -
   lda charPalEof
   sta line+0
   lda #$40
   sta headFlags
   lda #1
   sta headLineLen
   jsr StashLine
   bcc +
   rts
+  ldx #3
-  lda mp,x
   sta loadHead,x
   sta loadTail,x
   dex
   bpl -
   clc
   rts

LoadFinish = *
   ldx #3
-  lda loadHead,x
   sta mp,x
   dex
   bpl -
   jsr FetchHead
   ldx #3
-  lda loadTail,x
   sta headPrev,x
   dex
   bpl -
   jsr StashHead
   ldx #3
-  lda loadTail,x
   sta mp,x
   dex
   bpl -
   ldx #loadHead
   ldy #4
   jsr aceMemZpstore
   rts

LoadLine = *  ;( ) : .CS=end
   ;tab expansion and char translation will go into this routine
   ldx loadBufPtr
   ldy loadLineLen

   loadNextByte = *
   lda loadBufCount
   bne ++
   sty loadLineLen
   jsr LoadBuf
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
   beq +++
   cpy targetLen
   bne +
   bit wrapFlag  ;test show-CR
   bvc ++
   bvs +++
+  bcs ++
+  dec loadBufCount
   bne -
   beq loadNextByte

+  dec loadBufCount
   stx loadBufPtr
   sty loadLineLen
   clc
   rts

LoadBuf = *  ;( ) : .CS=eof
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

LoadLineWrap = *
   ldx loadLineLen
   dex
   ldy #$80
   lda line,x
   cmp #chrCR
   beq +
   ldy #$00
+  sty headFlags
   cmp #chrCR
   bne +
-  stx headLineLen
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
   sta headLineLen
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
   stx headLineLen
   rts

LoadLineStore = *
   inc loadLines+0
   bne +
   inc loadLines+1
   bne +
   inc loadLines+2
   bne +
   inc loadLines+3
+  sec
   bit headFlags
   bmi +
   clc
+  lda loadBytes+0
   adc headLineLen
   sta loadBytes+0
   bcc +
   inc loadBytes+1
   bne +
   inc loadBytes+2
   bne +
   inc loadBytes+3
+  ldx #3
-  lda #aceMemNull
   sta headNext,x
   lda loadTail,x
   sta headPrev,x
   dex
   bpl -
   jsr StashLine
   ldx #3
-  lda mp,x
   ldy loadTail,x
   sta loadTail,x
   sty mp,x
   dex
   bpl -
   ldx #loadTail
   ldy #4
   jsr aceMemZpstore
   rts

LoadLineOverflow = *
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

;====== management routines ======

work3Save: .buf 14

SaveWork3 = *
   ldx #13
-  lda work3,x
   sta work3Save,x
   dex
   bpl -
   rts

RestoreWork3 = *
   ldx #13
-  lda work3Save,x
   sta work3,x
   dex
   bpl -
   rts

fetchLineExtra = 10

FetchLine = *  ;( [mp]=farLine ) : headBuffer, linebuf
   lda #<linebuf
   ldy #>linebuf
   sta zp+0
   sty zp+1
   lda #headLength+fetchLineExtra
   ldy #0
   jsr aceMemFetch
   ldx #headLength-1
-  lda linebuf,x
   sta headBuffer,x
   dex
   bpl -
   lda headLineLen
   cmp #fetchLineExtra+1
   bcc +
   clc
   adc #headLength
   ldy #0
   jsr aceMemFetch
+  rts

StashLine = *  ;( headBuffer, line ) : [mp]=storedLine, .CS=err
   ldx #headLength-1
-  lda headBuffer,x
   sta linebuf,x
   dex
   bpl -
   clc
   lda headLineLen
   adc #headLength
   ldy #0
   jsr Malloc
   bcc +
   rts
+  lda #<linebuf
   ldy #>linebuf
   sta zp+0
   sty zp+1
   clc
   lda headLineLen
   adc #headLength
   ldy #0
   jsr aceMemStash
   clc
   rts
   
FetchHead = *  ;( [mp]=farLinePtr ) : headBuffer
   ldx #headBuffer
   ldy #headLength
   jsr aceMemZpload
   rts

StashHead = *  ;( [mp]=farLinePtr, headBuffer )
   ldx #headBuffer
   ldy #headLength
   jsr aceMemZpstore
   rts

;====== dynamic-memory routines ======

mallocWork = work1

mallocHead:   .buf 4
tpaFreeFirst: .buf 1
tpaFreeMin:   .buf 1
tpaFreePages: .buf 1
tpaAreaStart: .buf 1
tpaAreaEnd:   .buf 1

;*** mallocInit()

MallocInit = *
   lda #aceMemNull
   sta mallocHead+3
   ldx #0
   lda #$ff
-  sta tpaFreemap,x
   inx
   bne -
   ldx #>bssEnd
   lda #<bssEnd
   beq +
   inx
+  stx tpaFreeFirst
   stx tpaAreaStart
   ldx aceMemTop+1
   stx mallocWork
   stx tpaAreaEnd
   txa
   sec
   sbc tpaFreeFirst
   bcs +
   lda #0
+  sta tpaFreePages
   clc
   adc #1
   sta tpaFreeMin
   ldx tpaFreeFirst
   cpx mallocWork
   bcs +
   lda #$00
-  sta tpaFreemap,x
   inx
   cpx mallocWork
   bcc -
+  rts

libPages: .buf 1

LibPageAlloc = *  ;( .A=pages ) : [mp]
   sta libPages
   ldx #$00
   ldy #aceMemInternal-1
   jsr aceMemAlloc
   bcs +
   rts
+  jsr TpaPageAlloc
   bcs +
   rts
+  lda libPages
   ldx #aceMemInternal
   ldy #$ff
   jsr aceMemAlloc
   bcs +
   rts
+  lda #<nomemMsg
   ldy #>nomemMsg
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit

   nomemMsg = *
   .byte "\nInsufficient memory, aborting.\n",0

newmax: .buf 1

TpaPageAlloc = *  ;( libPages ) : [mp]
   lda libPages
   cmp tpaFreeMin
   bcs tpaFreemapFull
   ;** first free
   ldx tpaFreeFirst
   lda tpaFreemap,x
   beq ++
-  inx
   beq tpaFreemapFull
   lda tpaFreemap,x
   bne -
   stx tpaFreeFirst
   jmp ++
   tpaFreemapFull = *
   lda libPages
   cmp tpaFreeMin
   bcs +
   sta tpaFreeMin
+  sec
   rts

   ;** search
+  dex
-  ldy libPages
-  inx
   beq tpaFreemapFull
   lda tpaFreemap,x
   bne --
   dey
   bne -

   ;** allocate
   stx newmax
   ldy libPages
   lda #$41
-  sta tpaFreemap,x
   dex
   dey
   bne -
   inx
   cpx tpaFreeFirst
   bne +
   ldy newmax
   iny
   sty tpaFreeFirst
+  sec
   lda tpaFreePages
   sbc libPages
   sta tpaFreePages
   lda #0
   ldy #aceMemInternal
   sta mp+0
   stx mp+1
   sta mp+2
   sty mp+3
   clc
   rts

;*** Malloc( .AY=Bytes ) : [mp]=FarPointer

mallocLenSave: .buf 3

Malloc = *
QuickMalloc = *
   sta mallocLenSave+0
   sty mallocLenSave+1
   jsr LibMalloc
   bcs +
   rts
+  ldx mallocLenSave+1
   lda mallocLenSave+0
   beq +
   inx
+  txa
   cpx #>512
   bcs +
   ldx #>512
+  txa
   sta mallocLenSave+2
   jsr LibPageAlloc
   bcc +
   rts
+  lda #0
   ldy mallocLenSave+2
   jsr Free
   lda mallocLenSave+0
   ldy mallocLenSave+1
   jmp Malloc

mallocMemNextPtr = mallocWork+0 ;(4)
mallocMemLength  = mallocWork+4 ;(2)
mallocLength     = mallocWork+6 ;(2)
mallocQ          = mallocWork+8 ;(4)

LibMalloc = *
   clc
   adc #7
   bcc +
   iny
+  and #$f8
   sta mallocLength
   sty mallocLength+1
   ldx #3
-  lda mallocHead,x
   sta mp,x
   lda #aceMemNull
   sta mallocQ,x
   dex
   bpl -

   mallocLook = *
   lda mp+3
   cmp #aceMemNull
   bne +

   mallocErrorExit = *
   lda #aceMemNull
   sta mp+3
   lda #aceErrInsufficientMemory
   sta errno
   sec
   rts

+  ldx #mallocMemNextPtr
   ldy #6
   jsr aceMemZpload
   lda mallocMemLength
   cmp mallocLength
   lda mallocMemLength+1
   sbc mallocLength+1
   bcs mallocGotBlock
   ldx #3
-  lda mp,x
   sta mallocQ,x
   lda mallocMemNextPtr,x
   sta mp,x
   dex
   bpl -
   jmp mallocLook

   mallocGotBlock = *
   lda mallocMemLength
   cmp mallocLength
   bne +
   lda mallocMemLength+1
   sbc mallocLength+1
   beq mallocTakeWholeBlock
+  sec
   lda mallocMemLength+0
   sbc mallocLength+0
   sta mallocMemLength+0
   lda mallocMemLength+1
   sbc mallocLength+1
   sta mallocMemLength+1
   clc
   lda mp+0
   pha
   adc mallocLength+0
   sta mp+0
   lda mp+1
   pha
   adc mallocLength+1
   sta mp+1
   ldx #mallocMemNextPtr
   ldy #6
   jsr aceMemZpstore
   ldx #3
-  lda mp,x
   ldy mallocQ,x
   sta mallocQ,x
   sty mp,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   beq +
   ldx #mallocQ
   ldy #4
   jsr aceMemZpstore
   jmp ++
+  ldx #3
-  lda mallocQ,x
   sta mallocHead,x
   dex
   bpl -
+  pla
   sta mp+1
   pla
   sta mp+0
   lda mallocQ+2
   ldy mallocQ+3
   sta mp+2
   sty mp+3
   clc
   rts

   mallocTakeWholeBlock = *
   lda mallocQ+3
   cmp #aceMemNull
   bne +
   ldx #3
-  lda mallocMemNextPtr,x
   sta mallocHead,x
   dex
   bpl -
   clc
   rts
+  ldx #3
-  lda mp,x
   ldy mallocQ,x
   sta mallocQ,x
   sty mp,x
   dex
   bpl -
   ldx #mallocMemNextPtr
   ldy #4
   jsr aceMemZpstore
   ldx #3
-  lda mallocQ,x
   sta mp,x
   dex
   bpl -
   clc
   rts

;*** free( [mp]=FarPointer, .AY=Length )  {alters [mp]}

freeMemNextPtr = mallocWork+0  ;(4)
freeMemLength  = mallocWork+4  ;(2)
freeLength     = mallocWork+6  ;(2)
freeNewPtr     = mallocWork+8  ;(4)
freeQ          = mallocWork+12 ;(4)

Free = *
   clc
   adc #7
   bcc +
   iny
+  and #$f8
   sta freeLength+0
   sty freeLength+1
   ldx #3
-  lda mp,x
   sta freeNewPtr,x
   lda mallocHead,x
   sta mp,x
   lda #aceMemNull
   sta freeQ,x
   dex
   bpl -

   freeSearchLoop = *
   lda mp+3
   cmp #aceMemNull
   beq freeCoalesceQandNew
   lda mp+0
   cmp freeNewPtr+0
   lda mp+1
   sbc freeNewPtr+1
   lda mp+2
   sbc freeNewPtr+2
   lda mp+3
   sbc freeNewPtr+3
   bcs freeCoalesceQandNew
+  ldx #freeMemNextPtr
   ldy #4
   jsr aceMemZpload
   ldx #3
-  lda mp,x
   sta freeQ,x
   lda freeMemNextPtr,x
   sta mp,x
   dex
   bpl -
   bmi freeSearchLoop

   freeCoalesceQandNew = *
   ldx #3
-  lda freeQ,x
   sta mp,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   bne +
   ;** prev is head
   ldx #3
-  lda mallocHead,x
   sta freeMemNextPtr,x
   lda freeNewPtr,x
   sta mallocHead,x
   dex
   bpl -
   lda freeLength+0
   ldy freeLength+1
   sta freeMemLength+0
   sty freeMemLength+1
   jmp freeCoalesceNewAndP

   ;** prev is real
+  ldx #freeMemNextPtr
   ldy #6
   jsr aceMemZpload
   lda mp+3
   cmp freeNewPtr+3
   bne +
   lda mp+2
   cmp freeNewPtr+2
   bne +
   clc
   lda mp+0
   adc freeMemLength
   tax
   lda mp+1
   adc freeMemLength+1
   cmp freeNewPtr+1
   bne +
   cpx freeNewPtr
   bne +
   ;** prev does coalesce
   clc
   lda freeMemLength
   adc freeLength
   sta freeMemLength
   lda freeMemLength+1
   adc freeLength+1
   sta freeMemLength+1
   ldx #3
-  lda freeQ,x
   sta freeNewPtr,x
   dex
   bpl -
   bmi freeCoalesceNewAndP

   ;** prev does not coalesce
+  ldx #freeNewPtr
   ldy #4
   jsr aceMemZpstore
   lda freeLength+0
   ldy freeLength+1
   sta freeMemLength+0
   sty freeMemLength+1

   freeCoalesceNewAndP = *
   lda freeNewPtr+3
   cmp freeMemNextPtr+3
   bne +
   lda freeNewPtr+2
   cmp freeMemNextPtr+2
   bne +
   clc
   lda freeNewPtr
   adc freeMemLength
   tax
   lda freeNewPtr+1
   adc freeMemLength+1
   cmp freeMemNextPtr+1
   bne +
   cpx freeMemNextPtr
   bne +

   ;** new and next coalesce
   ldx #3
-  lda freeMemNextPtr,x
   sta mp,x
   dex
   bpl -
   lda freeMemLength+1
   pha
   lda freeMemLength+0
   pha
   ldx #freeMemNextPtr
   ldy #6
   jsr aceMemZpload
   clc
   pla
   adc freeMemLength+0
   sta freeMemLength+0
   pla
   adc freeMemLength+1
   sta freeMemLength+1

+  ldx #3
-  lda freeNewPtr,x
   sta mp,x
   dex
   bpl -
   ldx #freeMemNextPtr
   ldy #6
   jsr aceMemZpstore
   clc
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
   xsave: .buf 1
   ysave: .buf 1

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
   getcBuffer: .buf 1

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
   tay
   txa
   sta zp+0
   sty zp+1
   rts

;====== editing control ======

GetKey = *  ;( ) : keychar=.A=key, keyshift
   jsr UpdateStatus
   jsr CursorPos
   lda #$ff
   bcc +
   lda #$fa
+  ldy conColor+1
   jsr aceWinCursor
   jsr aceConGetkey
   sta keychar
   stx keyshift
   lda #$00
   jsr aceWinCursor
   lda statusUpdate
   and #$01
   beq +
   eor statusUpdate
   sta statusUpdate
   jsr DisplaySeparator
+  ldy #1
   lda keychar
   cmp sameKeyChar
   bne +
   lda keyshift
   cmp sameKeyShift
   bne +
   ldy sameKeyCount
   iny
   bne +
   dey
+  sty sameKeyCount
   lda keychar
   ldx keyshift
   sta sameKeyChar
   stx sameKeyShift
   rts

CursorPos = *  ;( scrRow, scrCol, scrLfMrgn ) : .AY=(syswork+0)=addr, .CS=col80
   sec
   lda scrCol
   sbc scrLeftMargin
   tax
   cpx scrCols
   php
   bcc +
   ldx scrCols
   dex
+  lda scrRow
   jsr aceWinPos
   lda syswork+0
   ldy syswork+1
   plp
   rts

CmdDispatch = *
   lda keychar
   asl
   rol
   rol
   rol
   and #$07
   tax
   lda cmdDispTableBase,x
   cmp #$ff
   bne +
   jmp CmdRegularInput
+  cmp #$fe
   bne +
   jmp CmdAlt
+  sta work1+0
   lda keychar
   and #$1f
   asl
   adc work1+0
   tax
   lda cmdDispatchTable+0,x
   sta work1+0
   lda cmdDispatchTable+1,x
   sta work1+1
   jmp (work1+0)

   cmdDispTableBase: .byte $00,$ff,$ff,$fe,$40,$80,$ff,$c0
   cmdDispatchTable = *
   .word CmdNull,CmdNotImp,CmdNotImp,CmdNull           ;$00-$03
   .word CmdNotImp,CmdNotImp,CmdWordLeft,CmdNotImp     ;$04-$07
   .word CmdRub,CmdNotImp,CmdNotImp,CmdWordRight       ;$08-$0b
   .word CmdEndUp,CmdReturn,CmdNull,CmdEndDown         ;$0c-$0f
   .word CmdBol,CmdDown,CmdRvsOn,CmdHome               ;$10-$13
   .word CmdDelete,CmdEol,CmdPageUp,CmdPageDown        ;$14-$17
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdEscape       ;$18-$1b
   .word CmdNotImp,CmdRight,CmdNotImp,CmdNotImp        ;$1c-$1f
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNull         ;$80-$83
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNotImp       ;$84-$87
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNotImp       ;$88-$8b
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNotImp       ;$8c-$8f
   .word CmdNotImp,CmdUp,CmdRvsOff,CmdScrTop           ;$90-$93
   .word CmdDelete,CmdNotImp,CmdNotImp,CmdNotImp         ;$94-$97
   .word CmdNull,CmdNull,CmdNull,CmdNotImp             ;$98-$9b
   .word CmdNotImp,CmdLeft,CmdStatusMargin,CmdNotImp   ;$9c-$9f
   .word CmdNotImp,CmdBol,CmdWordLeft,CmdNotImp        ;$a0-$a3
   .word CmdDebug,CmdNotImp,CmdNotImp,CmdNotImp        ;$a4-$a7
   .word CmdNotImp,CmdPageUp,CmdNotImp,CmdNotImp       ;$a8-$ab
   .word CmdNotImp,CmdPageDown,CmdWordRight,CmdNotImp  ;$ac-$af
   .word CmdNotImp,CmdNotImp,CmdRecall,CmdEol          ;$b0-$b3
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdEndUp        ;$b4-$b7
   .word CmdNotImp,CmdNotImp,CmdEndDown,CmdBol         ;$b8-$bb
   .word CmdNotImp,CmdEol,CmdNotImp,CmdNotImp          ;$bc-$bf
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNotImp       ;$e0-$e3
   .word CmdDebug,CmdNotImp,CmdNotImp,CmdNotImp        ;$e4-$e7
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNotImp       ;$e8-$eb
   .word CmdNotImp,CmdNotImp,CmdNewName,CmdNotImp      ;$ec-$ef
   .word CmdNotImp,CmdQuit,CmdRecall,CmdSave           ;$f0-$f3
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdNotImp       ;$f4-$f7
   .word CmdNotImp,CmdNotImp,CmdNotImp,CmdToggleIns    ;$f8-$fb
   .word CmdToggleModified,CmdToggleInd,CmdNotImp,CmdNotImp ;$fc-$ff

;====== commands ======

CmdNull = *
CmdAlt  = *
   rts

CmdNotImp = *
   jmp NotImplemented

CmdQuit = *
   bit modified
   bmi +
   lda #$ff
   sta exitFlag
   rts
+  lda #<noQuitMsg
   ldy #>noQuitMsg
   jsr DisplayMessage
   jmp Buzz
   noQuitMsg: .byte "Cannot quit; press CT-BritishPound -- document modified",0

scanCount: .buf 1
scanned:   .buf 1

ScanDown = *  ;( .A=num_lines, [mp]=startLine ) : [mp]=targLine, .A=lines, .V=e
   ldy #0
   sty scanned
   sta scanCount
   cmp #0
   clv
   beq +
-  jsr FetchHead
   bit headFlags
   bvs +
   ldx #3
-  lda headNext,x
   sta mp,x
   dex
   bpl -
   inc scanned
   dec scanCount
   bne --
   clv
+  lda scanned
   rts

ScanUp = *  ;( .A=num_lines, [mp]=startLine ) : [mp]=targLine, .A=lines, .C=v
   ldy #0
   sty scanned
   sta scanCount
   cmp #0
   beq +
   jsr FetchHead
-  ldx #3
-  lda headPrev,x
   sta mp,x
   dex
   bpl -
   jsr FetchHead
   bit headFlags
   bvs ++
   inc scanned
   dec scanCount
   bne --
+  lda scanned
   clc
   rts
+  ldx #3
-  lda headNext,x
   sta mp,x
   dex
   bpl -
   lda scanned
   sec
   rts

MvLineToMp = *
   ldx #3
-  lda linePtr,x
   sta mp,x
   dex
   bpl -
   rts

MvMpToLine = *
   ldx #3
-  lda mp,x
   sta linePtr,x
   dex
   bpl -
   rts

mpSave: .buf 4

MvMpToSave = *
   ldx #3
-  lda mp,x
   sta mpSave,x
   dex
   bpl -
   rts

MvSaveToMp = *
   ldx #3
-  lda mpSave,x
   sta mp,x
   dex
   bpl -
   rts

CmdPageDown = *
   lda #$80
   jsr SetUpdateStat
   jsr MvLineToMp
   sec
   lda scrRows
   sbc scrRow
   jsr ScanDown
   bvc +
   jmp CmdEndDown
+  jsr AddToLineNum
   ldx #3
-  lda mp,x
   sta work1+0,x
   dex
   bpl -
   sec
   lda scrRow
   sbc #2
   jsr ScanDown
   pha
   jsr AddToLineNum
   pla
   clc
   adc #2
   sta scrRow
   ldx #3
-  lda mp,x
   sta linePtr,x
   lda work1+0,x
   sta mp,x
   dex
   bpl -
   lda #2
   ldy scrRows
   jsr DisplayScreen
   jmp CheckColBounds

CmdEndUp = *
   lda #$c0
   jsr SetUpdateStat
   ldx #3
-  lda headLinePtr,x
   sta mp,x
   lda #0
   sta lineNum,x
   sta solColNum,x
   dex
   bpl -
   lda #1
   sta lineNum+0
   jsr FetchHead
   ldx #3
-  lda headNext,x
   sta linePtr,x
   sta mp,x
   dex
   bpl -
   lda #2
   ldx #0
   sta scrRow
   stx scrCol
   stx scrLeftMargin
   lda #2
   ldy scrRows
   jsr DisplayScreen
   rts

CmdEndDown = *
   lda #$c0
   jsr SetUpdateStat
   ldx #3
-  lda headLinePtr,x
   sta mp,x
   sta linePtr,x
   dex
   bpl -
   ldx #3
-  lda lineCount,x
   sta lineNum,x
   lda #0
   sta solColNum,x
   dex
   bpl -
   lda #1
   jsr AddToLineNum
   lda #0
   sta scrLeftMargin
   sta scrCol
   clc ;sic
   lda scrRows
   sbc #2
   jsr ScanUp
   clc
   adc #2
   sta scrRow
   lda #2
   ldy scrRows
   jsr DisplayScreen
   rts

CmdPageUp = *
   lda #$80
   jsr SetUpdateStat
   jsr MvLineToMp
   sec
-  lda scrRows
   sbc #2
   jsr ScanUp
   jsr SubtractFromLineNum
   jsr MvMpToLine
   sec
   lda scrRow
   sbc #2
   jsr ScanUp
   php
   clc
   adc #2
   sta scrRow
   plp
   bcc +
   sec
   sbc #2
   cmp #0
   beq +
   jsr SubtractFromLineNum
   jmp -
+  lda #2
   ldy scrRows
   jsr DisplayScreen
   jmp CheckColBounds

CmdDown = *
   lda #$80
   jsr SetUpdateStat
   jsr MvLineToMp
   lda #1
   jsr ScanDown
   bvc +
   rts ;hit end
+  lda #1
   jsr AddToLineNum
   jsr MvMpToLine
   inc scrRow
   lda scrRow
   cmp scrRows
   bcc +
   dec scrRow
   lda #$88
   jsr ScrollDisplay
   sec
   lda scrRows
   sbc #1
   ldy scrRows
   jsr DisplayScreen
+  jsr CheckColBounds
   rts

ScrollDisplay = *  ;( .A=cmd )
   pha
   clc
   lda scrStartRow
   adc #2
   ldx scrStartCol
   sta syswork+0
   stx syswork+1
   sec
   lda scrRows
   sbc #2
   ldx scrCols
   jsr aceWinSet
   pla
   ldx #1
   ldy #$20
   sty syswork+4
   jsr aceWinScroll
   lda scrStartRow
   ldx scrStartCol
   sta syswork+0
   stx syswork+1
   lda scrRows
   ldx scrCols
   jsr aceWinSet
   rts

CmdUp = *
   lda #$80
   jsr SetUpdateStat
   jsr MvLineToMp
   lda #1
   jsr ScanUp
   bcc +
   rts ;hit top
+  lda #1
   jsr SubtractFromLineNum
   jsr MvMpToLine
   dec scrRow
   lda scrRow
   cmp #2
   bcs +
   inc scrRow
   lda #$84
   jsr ScrollDisplay
   sec
   lda #2
   ldy #3
   jsr DisplayScreen
+  jsr CheckColBounds
   clc
   rts

CmdBol = *
   lda #$40
   jsr SetUpdateStat
   lda #0
   sta scrCol
   jsr CheckColBounds
   rts

CmdEol = *
   lda #$40
   jsr SetUpdateStat
   jsr MvLineToMp
   jsr FetchHead
   lda headLineLen
   sta scrCol
   jsr CheckColBounds
   rts

CmdRightIntern = *
CmdRight = *
   lda #$40
   jsr SetUpdateStat
   jsr MvLineToMp
   jsr FetchHead
   lda scrCol
   cmp headLineLen
   bcc +
-  lda #0
   sta scrCol
   jmp CmdDown
+  inc scrCol
   bit headFlags
   bmi +
   lda scrCol
   cmp headLineLen
   bcs -
+  jsr CheckColBounds
   rts

CmdLeftIntern = *
CmdLeft = *
   lda #$40
   jsr SetUpdateStat
   lda scrCol
   bne ++
   jsr MvLineToMp
   lda #1
   jsr ScanUp
   bcc +
   rts
+  lda #255
   sta scrCol
   jmp CmdUp
+  dec scrCol
   jsr CheckColBounds
   rts

CheckColBounds = *
   jsr MvLineToMp
   jsr FetchHead
   lda scrCol
   cmp headLineLen
   bcc ++
   lda headLineLen
   bit headFlags
   bmi +
   sec
   sbc #1
   bcs +
   lda #0
+  sta scrCol
   lda #$40
   jsr SetUpdateStat
+  bit headFlags
   bvc +
   lda #$40
   jsr SetUpdateStat
   lda #0
   sta scrCol
+  lda scrCol
   cmp scrLeftMargin
   bcs +
   sta scrLeftMargin
   jmp RedisplayScreen
   rts
+  clc ;sic
   lda scrLeftMargin
   adc scrCols
   cmp scrCol
   beq +
   bcc ++
   rts
+  bit wrapFlag
   bvs +
   lda scrLeftMargin
   bne +
   lda scrCol
   cmp headLineLen
   bne +
   rts
+  sec
   lda scrCol
   sbc scrCols
   clc
   adc #1
   sta scrLeftMargin
   jmp RedisplayScreen

RedisplayScreen = *
   jsr MvLineToMp
   sec
   lda scrRow
   sbc #2
   jsr ScanUp
   lda #2
   ldy scrRows
   jmp DisplayScreen

SetUpdateStat = *
   ora statusUpdate
   sta statusUpdate
   rts

AddToLineNum = *  ;( .A=linesToAdd )
   clc
   adc lineNum+0
   sta lineNum+0
   bcc +
   inc lineNum+1
   bne +
   inc lineNum+2
   bne +
   inc lineNum+3
+  rts

AddToLineCount = *  ;( .A=linesToAdd )
   clc
   adc lineCount+0
   sta lineCount+0
   bcc +
   inc lineCount+1
   bne +
   inc lineCount+2
   bne +
   inc lineCount+3
+  rts

SubtractFromLineNum = *  ;( .A=linesToSubtract )
   sta subVal
   ldx #0
   ldy #4
   sec
-  lda lineNum,x
   sbc subVal,x
   sta lineNum,x
   inx
   dey
   bne -
   rts
   subVal: .buf 1
           .byte $00,$00,$00

SubtractFromLineCount = *  ;( .A=linesToSubtract )
   sta subLcVal
   ldx #0
   ldy #4
   sec
-  lda lineCount,x
   sbc subLcVal,x
   sta lineCount,x
   inx
   dey
   bne -
   rts
   subLcVal: .buf 1
             .byte $00,$00,$00

HandleArgFlag = *
   ldy #1
   lda (zp),y
   cmp #"t"
   bne +
   lda #$80
   sta wrapFlag
   jsr aceWinSize
   stx targetLen
+  cmp #"l"
   bne +
   lda #$00
   sta wrapFlag
   ldx #240
   stx targetLen
+  cmp #"w"
   bne +
   lda #$c0
   sta wrapFlag
   jsr aceWinSize
   stx targetLen
+  cmp #"n"
   bne +
   lda wrapFlag
   and #$7f
   sta wrapFlag
+  rts

GetPaletteChars = *
   lda #<stringbuf
   ldy #>stringbuf
   sta syswork+0
   sty syswork+1
   lda #%0110_0000
   ldx #$00
   ldy #40
   jsr aceWinChrset
   lda stringbuf+$1b
   sta charPalCR
   lda stringbuf+$20
   sta charPalEof
   lda stringbuf+$1e
   sta charPalLeft
   lda stringbuf+$1f
   sta charPalRight
   lda stringbuf+$01
   sta charPalVline
   lda stringbuf+$02
   sta charPalHline
   rts

charPalCR:    .byte $9b ;;buf 1
charPalEof:   .byte $df ;;buf 1
charPalLeft:  .byte $9e ;;buf 1
charPalRight: .byte $9f ;;buf 1
charPalVline: .byte $81 ;;buf 1
charPalHline: .byte $82 ;;buf 1

DeallocLine = *  ;( [mp]=line, headBuffer )  {alters [mp]}
   clc
   lda headLineLen
   adc #headLength
   ldy #0
   jmp Free

LinkPrevNextToMp = *  ;( [mp]=newline, [headNext],[headPrev] ) : [mp]=in {w1}
   ldx #3
-  lda mp,x
   sta work1+0,x
   lda headPrev,x
   sta mp,x
   dex
   bpl -
   ldx #work1+0
   ldy #4
   jsr aceMemZpstore
   ldx #3
-  lda headNext,x
   sta mp,x
   dex
   bpl -
   clc
   lda mp+0
   adc #4
   sta mp+0
   bcc +
   inc mp+1
+  ldx #work1+0
   ldy #4
   jsr aceMemZpstore
   ldx #3
-  lda work1+0,x
   sta mp,x
   dex
   bpl -
   rts

CmdRvsOn = *
   ldx #5
   lda #$ff
   sec
   jmp aceWinOption

CmdRvsOff = *
   ldx #5
   lda #$00
   sec
   jmp aceWinOption

CmdToggleIns = *
   lda #$80
   bne +
CmdToggleInd = *
   lda #$40
+  eor modeFlags
   sta modeFlags
   lda #$10
   jmp SetUpdateStat

SetModified = *
   bit modified
   bpl +
   rts
+  lda #$ff
   sta modified
   lda #$2c
   jmp SetUpdateStat

AddToByteCount = *  ;( .A=bytesToAdd )
   clc
   adc byteCount+0
   sta byteCount+0
   bcc +
   inc byteCount+1
   bne +
   inc byteCount+2
   bne +
   inc byteCount+3
+  lda #$08
   jmp SetUpdateStat

SubtractFromByteCount = *  ;( .A=bytesToSubtract )
   sta subByteVal
   ldx #0
   ldy #4
   sec
-  lda byteCount,x
   sbc subByteVal,x
   sta byteCount,x
   inx
   dey
   bne -
   lda #$08
   jmp SetUpdateStat
   subByteVal: .buf 1
               .byte $00,$00,$00
   rts

oldLen:   .buf 1
oldFlags: .buf 1

CmdRegularInput = *
   ;** get current line
   jsr MvLineToMp
   jsr FetchLine
   lda headLineLen
   sta oldLen
   ;** handle case of EOF
   bit headFlags
   bvc +
   jsr CmdReturn
   jsr CmdLeft
   jmp CmdRegularInput
   ;** update line in buffer
+  jsr PutCharInLine
   jsr ReplaceCurrentLine
   bcs putCharError
   jsr Slosh
   jsr RedisplayAfterSlosh
   jmp CmdRight
   rts
   ;** handle line-allocation failure
   putCharError = *
   rts

PutCharInLine = *
   ;** insert space for character if in insert mode or over CR
   bit modeFlags
   bmi +
   ldx scrCol
   cpx headLineLen
   bcc ++
+  inc headLineLen
   lda #1
   jsr AddToByteCount
   ldx headLineLen
-  lda line-1,x
   sta line,x
   dex
   cpx scrCol
   bne -
   ;** put the character onto the line
+  lda keychar
   ldx scrCol
   sta line,x
   jmp SetModified

NotImplemented = *
   lda #<notImplementedMsg
   ldy #>notImplementedMsg
   jsr DisplayMessage
   jsr Buzz
   rts
   notImplementedMsg: .byte "Command Key not implemented",0

Buzz = *
   lda #chrBEL
   jmp putchar

CmdRub = *
   ;** get current line
   jsr MvLineToMp
   jsr FetchLine
   lda headLineLen
   sta oldLen
   ;** handle case of EOF
   bit headFlags
   bvc +
-  lda #<noRubMsg
   ldy #>noRubMsg
   jsr DisplayMessage
   jmp Buzz
   ;** update line in buffer
+  lda scrCol
   cmp headLineLen
   bcs -
   jsr RubCharFromLine
   jsr ReplaceCurrentLine
   jsr Slosh
   jsr RedisplayAfterSlosh
   jmp CheckColBounds
   rts
   ;** handle line-allocation failure
   delError = *
   rts
   noRubMsg: .byte "No characters to rub out",0

RubCharFromLine = *
   ;** rub out the character in the current position
   ldx scrCol
   lda line,x
   jsr AddToRubBuffer
   ldx scrCol
-  lda line+1,x
   sta line,x
   inx
   cpx headLineLen
   bne -
   dec headLineLen
   lda #1
   jsr SubtractFromByteCount
   jmp SetModified

replaceFlag: .buf 1
replacePtr:  .buf 4

ReplaceCurrentLine = *  ;( linePtr, oldLen ) : [mp]=new[linePtr],.CS=allocError
   jsr MvLineToMp
   ldy #$80
   jmp +
ReplaceMpLine = *  ;( [mp]=oldLine, oldLen ) : [mp]=newLine, .CS=allocError
   ldy #$00
   ldx #3
-  lda mp,x
   cmp linePtr,x
   bne +
   dex
   bpl -
   ldy #$80
+  sty replaceFlag
   ldx #3
-  lda mp,x
   sta replacePtr,x
   dex
   bpl -
   ;** allocate & stash the replacement line
   jsr StashLine
   bcc +
   rts
   ;** link new line
+  jsr LinkPrevNextToMp
   ;** deallocate existing line
   ldx #3
-  lda mp,x
   ldy replacePtr,x
   sta replacePtr,x
   sty mp,x
   dex
   bpl -
   clc
   lda oldLen
   adc #headLength
   ldy #0
   jsr Free
   lda #$04
   jsr SetUpdateStat
   ldx #3
-  lda replacePtr,x
   bit replaceFlag
   bpl +
   sta linePtr,x
+  sta mp,x
   dex
   bpl -
   clc
+  rts

MvHeadPrevToMp = *
   ldx #3
-  lda headPrev,x
   sta mp,x
   dex
   bpl -
   rts

MvHeadNextToMp = *
   ldx #3
-  lda headNext,x
   sta mp,x
   dex
   bpl -
   rts

CmdDelete = *
   bit modeFlags
   bmi +
   jmp CmdLeft
+  lda scrCol
   beq +
-  jsr CmdLeft
   jmp CmdRub
   ;** check if previous line ends in CR
+  jsr MvLineToMp
   jsr FetchHead
   bit headFlags
   bvc +
   jmp CmdLeft
+  lda headLineLen
   sta oldLen
   lda headFlags
   sta temp
   jsr MvHeadPrevToMp
   jsr FetchHead
   bit headFlags
   bvc +
   rts
+  bpl -
   lda headFlags
   and #$7f
   sta headFlags
   ldx #headBuffer
   ldy #headLength
   jsr aceMemZpstore
   jsr CmdLeft
   jsr MvLineToMp
   jsr FetchHead
   lda headLineLen
   sta scrCol
   jsr Slosh
   jsr RedisplayAfterSlosh
   lda #1
   jsr SubtractFromByteCount
   jsr SetModified
   jmp CheckColBounds

CmdToggleModified = *
   lda modified
   eor #$ff
   sta modified
   lda #$20
   jmp SetUpdateStat

PadCrLine = *
   ;** add a carriage return to the end of CR line if in display mode
   ;** destroys all registers
   bit wrapFlag
   bvc +
   bit headFlags
   bpl +
   bvs +
   lda #chrCR
   ldx headLineLen
   sta line,x
   inc headLineLen
+  rts

UnpadCrLine = *
   ;** remove carriage return from end of CR line if in display mode
   ;** preserves all registers
   bit wrapFlag
   bvc +
   bit headFlags
   bpl +
   bvs +
   dec headLineLen
+  rts

CmdEscape = *
   jsr ScreenInitDisplay
   jsr RedisplayScreen
   rts

removeIsCurFlag: .buf 1

RemoveMpLine = *  ;( [mp]=line ) : [mp]=nextLine
   ;** updates linePtr if necessary
   jsr FetchHead
   ldy #$00
   ldx #3
-  lda mp,x
   cmp linePtr,x
   bne +
   dex
   bpl -
   dey
+  sty removeIsCurFlag
   clc
   lda headLineLen
   adc #headLength
   ldy #0
   jsr Free
   ;** modify previous link
   jsr MvHeadPrevToMp
   jsr MvMpToSave
   ldx #headNext
   ldy #4
   jsr aceMemZpstore
   jsr MvHeadNextToMp
   jsr FetchHead
   ldx #3
-  lda mpSave,x
   sta headPrev,x
   dex
   bpl -
   jsr StashHead
   bit removeIsCurFlag
   bpl +
   jsr MvMpToLine
+  lda #1
   jsr SubtractFromLineCount
   rts

sloshLinesAltered  = work2+0
sloshRedisplayAll  = work2+1  ;$80=force redisplay to bottom, $ff=force all
sloshRedisplayPrev = work2+2
sloshMaxChars      = work2+3
sloshTailChar      = work2+4
sloshTheCr         = work2+5
sloshLinesInserted = work2+6
sloshLinesDeleted  = work2+7
sloshCurAltered    = work2+8
sloshTerminate     = work2+9
sloshCurTailChar   = work2+10

Slosh = *  ;( linePtr, lineNum, scrCol, scrRow, lineCount )
   ;** out : linePtr, lineNum, scrCol, scrRow, lineCount,
   ;** out : sloshLinesAltered, sloshRedisplayAll, .CS=allocError
   ;** uses work2
   lda #0
   sta sloshLinesAltered
   sta sloshRedisplayAll
   sta sloshRedisplayPrev
   sta sloshLinesInserted
   sta sloshLinesDeleted
   sta sloshTerminate
   jsr MvLineToMp
   jsr FetchHead

   sloshBackward = *
   lda #$00
   sta sloshTheCr
   sta sloshCurAltered
   bit headFlags
   bvc +
   jmp sloshForward
+  jsr MvHeadPrevToMp
   jsr FetchLine
   lda headFlags
   and #$c0
   beq +
-  jsr MvHeadNextToMp
   jsr FetchHead
   jmp sloshForward
+  sec
   lda targetLen
   sbc headLineLen
   sta sloshMaxChars
   bcc -
   beq -
   ldx headLineLen
   lda line-1,x
   sta sloshTailChar
   jsr MvHeadNextToMp
   jsr FetchLine
   ldx headLineLen
   lda line-1,x
   bit headFlags
   bpl +
   lda #" "
+  sta sloshCurTailChar
   jsr PadCrLine
   lda #" "
   sta line+255
   ldx sloshMaxChars
   cpx headLineLen
   bcs +
-  dex
   cmp line,x
   bne -
   jsr UnpadCrLine
   inx
   bne sloshBackwardDo
   ldx sloshMaxChars
   lda sloshTailChar
   cmp #" "
   bne sloshBackwardDo
   jmp sloshForward
+  jsr UnpadCrLine
   ldx headLineLen
   bit headFlags
   bpl sloshBackwardDo
   lda #$80
   sta sloshTheCr

   sloshBackwardDo = *  ;( .X=chars, sloshTheCr )
   stx sloshMaxChars
   lda sloshCurTailChar
   cmp #" "
   beq +
   ;;xxx jmp sloshForward  ;doing this is worse than not for the algorithm
+  lda #$80
   sta sloshCurAltered
   lda sloshTheCr
   sta sloshTerminate
   ;** save the characters to be sloshed back
   ldx #0
-  lda line,x
   sta stringbuf,x
   inx
   cpx sloshMaxChars
   bcc -
   ;** delete the sloshed-back characters from the current line
   ldx sloshMaxChars
   ldy #0
-  lda line,x
   sta line,y
   iny
   inx
   cpx headLineLen
   bcc -
   sec
   lda headLineLen
   sta oldLen
   sbc sloshMaxChars
   sta headLineLen
   lda headFlags
   eor sloshTheCr
   sta headFlags
   ;** stash the replacement for the current line
   jsr ReplaceMpLine
   ;** move the sloshed characters back to the previous line
   jsr MvHeadPrevToMp
   jsr FetchLine
   clc
   lda headLineLen
   sta oldLen
   adc sloshMaxChars
   sta headLineLen
   ldx #0
   ldy oldLen
-  lda stringbuf,x
   sta line,y
   iny
   inx
   cpx sloshMaxChars
   bcc -
   lda headFlags
   ora sloshTheCr
   sta headFlags
   jsr ReplaceMpLine
   ;** adjust the cursor location if on cursor line
   lda sloshLinesAltered
   bne +
   lda #$ff
   sta sloshRedisplayPrev
+  ldx #3
-  lda headNext,x
   cmp linePtr,x
   bne +++
   dex
   bpl -
   sec
   lda scrCol
   sbc sloshMaxChars
   sta scrCol
   bcc +
   bne +++
   bit sloshTheCr
   bpl +++
+  clc
   lda headLineLen
   adc scrCol
   sta scrCol
   dec scrRow
   lda scrRow
   cmp #2
   bcs +
   inc scrRow
   lda sloshRedisplayAll
   ora #$80
   sta sloshRedisplayAll
+  lda #1
   jsr SubtractFromLineNum
   jsr MvMpToLine
   lda sloshLinesAltered
   bne +
   lda #$00
   sta sloshRedisplayPrev
   lda sloshLinesAltered
   cmp #$ff
   bcs +
   inc sloshLinesAltered
+  jsr MvHeadNextToMp
   jsr FetchHead

   sloshForward = *
   jsr FetchLine
   ;** check if the current slosh line is null and remove it if so
   lda headFlags
   and #$c0
   bne +++
   lda headLineLen
   bne +++
   jsr RemoveMpLine
   lda sloshLinesAltered
   cmp #$ff
   beq +
   dec sloshLinesAltered
+  lda sloshLinesDeleted
   cmp #$ff
   bcs +
   inc sloshLinesDeleted
+  jsr FetchLine
   ;** check for terminate
+  bit sloshTerminate
   bpl +
-  jmp sloshContinue
   ;** check if line is longer than target length
+  bit headFlags
   bvs -
   jsr PadCrLine
   lda headLineLen
   cmp targetLen
   php
   jsr UnpadCrLine
   plp
   beq +
   bcs +++
+  bit headFlags
   bmi +
   ldx headLineLen
   dex
   lda line,x
   cmp #" "
   bne sloshForwardBrokenWord
+  jmp sloshContinue
   ;** line is too long; figure out where to break it
+  ldx targetLen
   bit wrapFlag
   bpl +
   lda #" "
   sta line+255
-  dex
   cmp line,x
   bne -
   inx
   bne +
   ldx targetLen
+  jmp sloshForwardDo

   sloshForwardBrokenWord = *
   ;** line not too long; check if end of word will fit on current line
   sec
   lda targetLen
   sbc headLineLen
   sta sloshMaxChars
   jsr MvHeadNextToMp
   jsr FetchLine
   sec
   bit headFlags
   bvs +
   jsr PadCrLine
   lda sloshMaxChars
   cmp headLineLen
   bcs +
   ldx headLineLen
   lda #" "
   sta line,x
   ldx #$ff
-  inx
   cmp line,x
   bne -
   inx
   lda sloshMaxChars
   stx sloshMaxChars
   cmp sloshMaxChars
+  php
   jsr MvHeadPrevToMp
   jsr FetchLine
   plp
   bcc +
   jmp sloshContinue
   ;** end of word will not fit, so break current line at beginning of word
+  lda #" "
   sta line+255
   ldx headLineLen
-  dex
   cmp line,x
   bne -
   inx
   bne +
   jmp sloshContinue
+  nop

   sloshForwardDo = *  ;( .X=cutPoint )
   stx sloshMaxChars
   lda sloshCurAltered
   ora #$40
   sta sloshCurAltered
   jsr SplitMpLine
   ;** adjust cursor location if necessary
   ldx #3
-  lda headPrev,x
   cmp linePtr,x
   bne +
   dex
   bpl -
   sec
   lda scrCol
   sbc sloshMaxChars
   bcc +
   sta scrCol
   lda #$ff
   sta sloshRedisplayPrev
   jsr MvMpToLine
   lda #1
   jsr AddToLineNum
   inc scrRow
   lda scrRow
   cmp scrRows
   bcc ++
   dec scrRow
   lda #$ff
   sta sloshRedisplayAll
+  inc sloshLinesAltered
   bne +
   dec sloshLinesAltered
+  inc sloshLinesInserted
   bne +
   dec sloshLinesInserted
+  nop

   sloshContinue = *
   inc sloshLinesAltered
   bne +
   dec sloshLinesAltered
+  bit sloshTerminate
   bmi sloshExit
   jsr FetchHead
   bit headFlags
   bmi sloshExit
   bvs sloshExit
   lda sloshCurAltered
   bne +
   lda sloshLinesAltered
   cmp #1
   beq +
   jmp sloshExit
+  jsr MvHeadNextToMp
   jsr FetchHead
   bit headFlags
   bvs sloshExit
   jmp sloshBackward
   
   sloshExit = *
   ;** set sloshLinesAltered, sloshRedisplayAll, sloshRedisplayPrev
   bit sloshTerminate
   bmi +
   lda sloshCurAltered
   bne +
   dec sloshLinesAltered
   bne +
   inc sloshLinesAltered
+  lda sloshLinesDeleted
   cmp #$ff
   beq +
   lda sloshLinesInserted
   cmp #$ff
   beq +
   cmp sloshLinesDeleted
   bne +
   rts
+  lda sloshRedisplayAll
   ora #$80
   sta sloshRedisplayAll
   rts

rdslStart: .buf 1

RedisplayAfterSlosh = *
   ldx #"-"
   lda sloshRedisplayAll
   beq +
   ldx #"b"
   cmp #$ff
   bne +
   ldx #"a"
+  stx stringbuf+0
   ldx #"-"
   lda sloshRedisplayPrev
   beq +
   ldx #"p"
+  stx stringbuf+1
   lda sloshLinesAltered
   jsr sloshStatCount
   sta stringbuf+2
   lda sloshLinesInserted
   jsr sloshStatCount
   sta stringbuf+3
   lda sloshLinesDeleted
   jsr sloshStatCount
   sta stringbuf+4
   ldx #"-"
   bit sloshTerminate
   bpl +
   ldx #"t"
+  stx stringbuf+5
   lda #0
   sta stringbuf+6
   lda #<stringbuf
   ldy #>stringbuf
   ;;jsr DisplayMessage

   lda sloshRedisplayAll
   cmp #$ff
   bne +
   jmp RedisplayScreen
+  jsr MvLineToMp
   lda scrRow
   sta rdslStart
   cmp #2
   beq +
   bit sloshRedisplayPrev
   bpl +
   dec rdslStart
   jsr FetchHead
   jsr MvHeadPrevToMp
   inc sloshLinesAltered
   bne +
   dec sloshLinesAltered
+  bit sloshRedisplayAll
   bmi +
   clc
   lda sloshLinesAltered
   adc rdslStart
   bcs +
   cmp scrRows
   bcs +
   tay
   jmp ++
+  ldy scrRows
+  lda rdslStart
-  inc dbColor
   lda dbColor
   and #$0f
   beq -
   ;;sta displayDebugColor
   lda rdslStart
   jsr DisplayScreen
   lda #$00
   sta displayDebugColor
   rts
   dbColor: .byte $00

sloshStatCount = *
   cmp #10
   bcs +
   ora #$30
   rts
+  adc #$41-10
   rts

CmdDebug = *
   lda #$00
   sta $d030
   lda $d011
   ora #$10
   sta $d011
   ldx #0
   lda #0
-  sta $f800,x
   inx
   bne -
   jsr MvLineToMp
   jsr FetchLine
   ldx #headLength-1
-  lda headBuffer,x
   sta $f800,x
   dex
   bpl -
   ldx #0
-  lda line,x
   sta $f800+headLength,x
   inx
   cpx headLineLen
   bcc -
   ldx #3
-  lda mp,x
   sta $f900,x
   dex
   bpl -
   rts

FindIndent = *  ;( [mp]=lineInPara ) : .A=indentCount
   lda #0
   rts

SplitMpLine = *  ;( [mp]=line, .X=cutPoint ) : [mp]=newLine, .CS=allocFail
   lda headLineLen
   sta oldLen
   lda headFlags
   sta oldFlags
   lda headFlags
   and #$7f
   sta headFlags
   stx headLineLen
   jsr ReplaceMpLine
   bcc +
   rts
+  ldx headLineLen
   ldy #0
-  lda line,x
   sta line,y
   iny
   inx
   cpx oldLen
   bcc -
   sec
   lda oldLen
   sbc headLineLen
   sta headLineLen
   lda oldFlags
   and #$ff-$40
   sta headFlags
   jmp +
   InsertCR = *
   lda #0
   sta headLineLen
   lda #$80
   sta headFlags
+  ldx #3
-  lda mp,x
   sta headPrev,x
   ;headNext stays as-is
   dex
   bpl -
   jsr StashLine
   bcc +
   rts
+  jsr LinkPrevNextToMp
   lda #1
   jsr AddToLineCount
   rts

CmdReturn = *
   lda #$00
   sta retFlag
   jsr MvLineToMp
   jsr FindIndent
   sta temp
   jsr MvLineToMp
   jsr FetchLine
   bit headFlags
   bvc ++
   lda #$ff
   sta retFlag
   jsr CmdLeftIntern
   jsr MvLineToMp
   jsr FetchLine
   bit headFlags
   bvc +
   jsr InsertCR
   lda #1
   jsr AddToByteCount
   jsr SetModified
   jmp CmdEndDown
+  lda headLineLen
   sta scrCol
+  ldx scrCol
   jsr SplitMpLine
   jsr MvLineToMp
   jsr FetchHead
   lda headFlags
   ora #$80
   sta headFlags
   ldx #headBuffer
   ldy #headLength
   jsr aceMemZpstore
   lda headLineLen
   sta scrCol
+  jsr Slosh
   jsr CmdRightIntern
   jsr Slosh
   bit retFlag
   bpl +
   jsr CmdRightIntern
+  jsr RedisplayScreen
   jsr CheckColBounds
   lda #1
   jsr AddToByteCount
   jmp SetModified
   retFlag: .buf 1

CmdRecall = *
   lda keyshift
   and #$01
   beq +
   jmp RecallChar
+  jmp NotImplemented
   
RecallChar = *
   lda sameKeyCount
   jsr FetchFromRubBuffer
   bcc +
   lda #<recallCharMsg
   ldy #>recallCharMsg
   jsr DisplayMessage
   jmp Buzz
+  sta keychar
   lda #$00
   sta keyshift
   jmp CmdRegularInput
   jmp CmdLeft
   recallCharMsg: .byte "No more characters in Rub-buffer",0

RUB_BUFFER_SIZE = 50
rubBufPtr:  .buf 1
rubBufSize: .buf 1

InitRubBuffer = *
   ldx #0
   stx rubBufPtr
   stx rubBufSize
   lda #$00
-  sta rubBuffer,x
   inx
   cpx #RUB_BUFFER_SIZE
   bcc -
   rts

AddToRubBuffer = *  ;( .A=char )
   ldx rubBufPtr
   sta rubBuffer,x
   inx
   cpx #RUB_BUFFER_SIZE
   bcc +
   ldx #0
+  stx rubBufPtr
   lda rubBufSize
   cmp #RUB_BUFFER_SIZE
   bcs +
   inc rubBufSize
+  rts

FetchFromRubBuffer = *  ;( .A=back_index ) : .A=char
   cmp rubBufSize
   beq +
   bcc +
   sec
   rts
+  sta temp
   sec
   lda rubBufPtr
   sbc temp
   bcs +
   adc #RUB_BUFFER_SIZE
+  tax
   lda rubBuffer,x
   clc
   rts

MvTailLineToMp = *
   ldx #3
-  lda headLinePtr,x
   sta mp,x
   dex
   bpl -
   rts

writeFd: .buf 1
stopCountdown: .byte 0

WriteDoc = *  ;( [mp]=starting line ) : .CS=error
-  jsr FetchLine
   bit headFlags
   bvc +
   rts
+  lda #<line
   ldy #>line
   sta zp+0
   sty zp+1
   bit headFlags
   bpl +
   ldx headLineLen
   lda #chrCR
   sta line,x
   inx
   txa
   jmp ++
+  lda headLineLen
+  ldy #0
   ldx writeFd
   jsr write
   ldx #3
-  lda headNext,x
   sta mp,x
   dex
   bpl -
   inc stopCountdown
   lda stopCountdown
   and #7
   bne +
   jsr aceConStopkey
   bcc +
   rts
+  jmp --

CmdSave = *
   ;** open file
   lda #<saveMsg1
   ldy #>saveMsg1
   jsr DisplayMessage
   lda #<docbufFilename
   ldy #>docbufFilename
   sta zp+0
   sty zp+1
   lda #"w"
   jsr open
   bcc saveMain
   lda errno
   cmp #aceErrFileExists
   bne saveError

   ;** scratch existing file if necessary
   jsr UndisplayMessage
   lda #<saveMsg2
   ldy #>saveMsg2
   jsr DisplayMessage
   jsr aceFileRemove
   bcs saveError
   jsr UndisplayMessage
   lda #<saveMsg3
   ldy #>saveMsg3
   jsr DisplayMessage
   lda #"w"
   jsr open
   bcs saveError

   ;** save the file
   saveMain = *
   sta writeFd
   jsr UndisplayMessage
   lda #<saveMsg4
   ldy #>saveMsg4
   jsr DisplayMessage
   jsr MvTailLineToMp
   jsr FetchHead
   jsr MvHeadNextToMp
   jsr WriteDoc
   bcs saveErrorClose

   ;** close the file
   lda writeFd
   jsr close
   bcs saveError
   lda #$00
   sta modified
   lda #$20
   jsr SetUpdateStat
   jsr UndisplayMessage
   lda #<saveMsg5
   ldy #>saveMsg5
   jsr DisplayMessage
   rts

   ;** handle error
   saveErrorClose = *
   lda writeFd
   jsr close
   saveError = *
   jsr UndisplayMessage
   lda #<saveErr1
   ldy #>saveErr1
   jsr DisplayMessage
   rts

   saveMsg1: .byte "Opening file for writing",0
   saveMsg2: .byte "Scratching old file",0
   saveMsg3: .byte "Reopening file for writing",0
   saveMsg4: .byte "Saving...",0
   saveMsg5: .byte "Saved",0
   saveErr1: .byte "Error: could not write file",0

CmdScrTop = *
   lda #$80
   jsr SetUpdateStat
   jsr MvLineToMp
   sec
   lda scrRow
   sbc #2
   jsr ScanUp
   jsr SubtractFromLineNum
   jsr MvMpToLine
   lda #2
   sta scrRow
   lda #0
   sta scrCol
   jsr CheckColBounds
   rts

CmdHome = *
   lda sameKeyCount
   cmp #2
   bcs +
   rts
+  and #$01
   bne +
   jmp CmdScrBottom
+  jmp CmdScrMiddle

CmdScrMiddle = *
   jsr CmdScrTop
   sec
   lda scrRows
   sbc #2
   lsr
   jmp +

CmdScrBottom = *
   lda #$80
   jsr SetUpdateStat
   jsr MvLineToMp
   sec
   lda scrRows
   sbc #1
   sec
   sbc scrRow
+  jsr ScanDown
   pha
   jsr AddToLineNum
   pla
   clc
   adc scrRow
   sta scrRow
   jsr MvMpToLine
   lda #0
   sta scrCol
   jsr CheckColBounds
   rts

CmdInst = *
   lda modeFlags
   pha
   lda #" "
   sta keychar
   jsr CmdRegularInput
   jsr CmdLeft
   pla
   sta modeFlags
   rts

wordState = work2+0  ;$ff=inword, $00=inWhitespace

CmdWordRight = *
   lda #$ff
   sta wordState
   lda #$40
   jsr SetUpdateStat
-  jsr MvLineToMp
   jsr FetchLine
   bit headFlags
   bvc +
   jsr CheckColBounds
   rts
+  ldy scrCol
   dey
-  iny
   cpy headLineLen
   bcs ++
   lda line,y
   cmp #" "
   beq +
   cmp #chrTAB
   beq +
   lda wordState
   bmi -
   sty scrCol
   jsr CheckColBounds
   rts
+  lda #$00
   sta wordState
   jmp -
+  bit headFlags
   bpl +
   lda #$00
   sta wordState
+  lda #0
   sta scrCol
   jsr CmdDown
   jmp --

CmdWordLeft = *
   jsr CmdLeft
   lda #$00
   sta wordState
   lda #$40
   jsr SetUpdateStat
-  jsr MvLineToMp
   jsr FetchLine
   bit headFlags
   bvc +
   jsr CheckColBounds
   rts
+  ldy scrCol
   iny
-  dey
   cpy #255
   beq +++
   cpy headLineLen
   bcc +
   bit headFlags
   bpl -
   jmp ++
+  lda line,y
   cmp #" "
   beq +
   cmp #chrTAB
   beq +
   lda #$ff
   sta wordState
   jmp -
+  bit wordState
   bpl -
   sty scrCol
   jmp CmdRight
+  jsr CmdUp
   bcs +
   ldy headLineLen
   sty scrCol
   jmp --
+  lda #0
   sta scrCol
   jmp CheckColBounds

CmdStatusMargin = *
   clc
   lda statusMargin
   adc scrCols
   sta statusMargin
   cmp #80
   bcc +
   lda #0
   sta statusMargin
   jmp ++
+  clc
   adc scrCols
   cmp #80
   bcc +
   sec
   lda #80
   sbc scrCols
   sta statusMargin
+  jsr DisplayStatus
   lda #<messageBuffer
   ldy #>messageBuffer
   jsr DisplayMessage
   rts

CmdNewName = *
   ldx #0
-  lda tempNewName,x
   sta docbufFilename,x
   beq +
   inx
   bne -
+  stx docbufFilenameLen
   lda #$02
   jmp SetUpdateStat
   rts
   tempNewName: .byte "tempjunk",0

UpdateIoLines = *  ;( (.AY)=title, [work2+8]=lines, [work2+12]=bytes )
   rts
   ioLinesMsg1 : .byte "  Lines=",0
   ioLinesMsg2 : .byte "    Bytes=",0

StrCat = *  ;( (.AY)=string ) : cat to stringbuf, .Y=catstrlen, .X=strbuflen
   sta temp+0
   sty temp+1
   ldx #$ff
-  inx
   lda stringbuf,x
   bne -
   ldy #0
-  lda (temp),y
   sta stringbuf,x
   beq +
   inx
   iny
   bne -
+  rts

;===bss===

bss             = *
linebuf         = bss+0             ;(256)
line            = linebuf+headLength;(241)
stringbuf       = linebuf+256       ;(256)
filebuf         = stringbuf+0       ;(256)
documentBuf     = filebuf+256       ;(256)
  docbufNext    = documentBuf+0     ;(4)
  docbufPrev    = documentBuf+4     ;(4)
  docbufInfo    = documentBuf+8     ;(23)
  docbufFilenameLen = documentBuf+31;(1)
  docbufFilename= documentBuf+32    ;($e0)
tpaFreemap      = documentBuf+256   ;(256)
rubBuffer       = tpaFreemap+256    ;(RUB_BUFFER_SIZE)
messageBuffer   = rubBuffer+RUB_BUFFER_SIZE  ;(81)
bssEnd          = messageBuffer+81
