;*** ACEterm 1.10 - by Craig Bruce - started 04-Mar-1995

;this file is in ACE-assembler format

;usage: term

.include "acehead.s"

.org aceAppAddress

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

modemFd       =  2  ;(1)  ;fd for modem
trptr         =  4  ;(2)  ;pointer to bytes to be translated
trcount       =  6  ;(2)  ;number of bytes to be translated
troutptr      =  8  ;(2)  ;pointer to output bytes from translation
troutcount    = 10  ;(2)  ;number of bytes that have been translated
keycode       = 12  ;(1)  ;keycode of last key struck
escState      = 13  ;(1)  ;current state in ESC sequence
escParm       = 14  ;(1)  ;current parameter index of parm data
escQuesFlag   = 15  ;(1)  ;flag for question-mark char used in ESC seq
statlineType  = 16  ;(1)  ;status line type: $80=used, $c0=rvs, $00=unused
keyshift      = 17  ;(1)  ;shift pattern of last key struck
statlineAddr  = 18  ;(2)  ;address of status line on screen
stringkeyPtr  = 20  ;(2)  ;pointer to current string key data
stringkeyCount= 22  ;(1)  ;number of characters remaining to take from stringkey
captureActive = 23  ;(1)  ;flag to indicate whether capture buffer is active
captureMasterPtr=24 ;(4)  ;far pointer to master-control storage for buf
captureBufferPtr=28 ;(4)  ;far pointer to current cap-buf page
capCount      = 32  ;(2)  ;number of bytes to stash into capture buffer
capPtr        = 34  ;(2)  ;pointer to data to stash into capture buffer
capLenSave    = 36  ;(2)  ;save for the read buffer length
keypadMode    = 38  ;(1)  ;$00=normal, $ff=application
cursorMode    = 39  ;(1)  ;$00=normal, $ff=application
linefeedMode  = 40  ;(1)  ;$00=linefeed, $ff=newline
screenMode    = 41  ;(1)  ;$00=normal, $ff=reversed
autowrapMode  = 42  ;(1)  ;$00=off, $ff=on
extentMode    = 43  ;(1)  ;$00=scroll_region, $ff=full_screen
attribMode    = 44  ;(1)  ;current attrib of cursor
                          ;  ($80=rvs,$40=underline,$20=blink,$10=intensity)
cursorDispMode= 45  ;(1)  ;$00=disable, $ff=enable
cursorSavePos = 46  ;(2)  ;(ACE) row and column of saved cursor
cursorSaveAttr= 48  ;(1)  ;saved attribute of cursor
exitFlag      = 49  ;(1)  ;whether to exit the terminal immediately or not
emulateMode   = 50  ;(1)  ;0=literal,1=glasstty,2=vt100
restrVersFlag = 51  ;(1)  ;flag to restore version # on next keystroke
escChar       = 52  ;(1)  ;current char in esc sequence
charColor     = 53  ;(1)  ;color of characters
cursorSaveColor = 54 ;(1) ;saved color of characters
work          = 112 ;(16) ;lowest-level temporary work area
statOverrun   = 112 ;(4)  ;number of bytes dropped to overrun
statOverflow  = 116 ;(4)  ;number of bytes dropped to buffer overflow
statBytRecvd  = 120 ;(4)  ;number of bytes received
statBytSent   = 124 ;(4)  ;number of bytes sent

escParmData:  .buf 24     ;accept up to 24 parameters for ESC sequences

readbufLenMax = 512     ;maximum size in bytes
readbufLen:   .word readbufLenMax

TRUE  = $ff
FALSE = $00

;===main===

main = *
   ;** check for a large-enough TPA
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
   .byte "usage: term\n",0

mainInit = *
   ;** initialize variables
   lda #$00
   sta escState
   lda #FALSE
   sta keypadMode
   sta cursorMode
   sta linefeedMode
   sta screenMode
   sta extentMode
   sta cursorSavePos+0
   sta cursorSavePos+1
   sta cursorSaveAttr
   sta attribMode
   sta exitFlag
   sta captureActive
   sta restrVersFlag
   lda #TRUE
   sta autowrapMode
   sta cursorDispMode
   lda defEmulate
   sta emulateMode
   ldx #7
   lda #$00
-  sta stringkeyLens,x
   dex
   bpl -
   sta stringkeyCount
   jsr userkeyInit
   jsr captureInit
   jsr modemOpen
   jsr ConfirmBaud
   ldx #0
   jsr RestoreVersion
   jsr ConsoleInit
   jmp term

modemOpen = *
   ;** open modem file
   lda #<modemFilename
   ldy #>modemFilename
   sta zp+0
   sty zp+1
   lda #"w"
   jsr open
   bcc +
   lda #<modemFilenameErr
   ldy #>modemFilenameErr
   jsr eputs
   jmp die
+  sta modemFd
   rts

   modemFilename:    .byte "x:",0
   modemFilenameErr: .byte 'Cannot open "x:" file (modem)\n',0

modemClose = *
   lda modemFd
   jmp close

conColors:     .buf 8
winRows:       .buf 1  ;current "scroll region"
winCols:       .buf 1
winStartRow:   .buf 1
winStartCol:   .buf 1
scrRows:       .buf 1  ;vt-100 screen
scrCols:       .buf 1
scrStartRow:   .buf 1
scrStartCol:   .buf 1
useRows:       .buf 1  ;window to use (25-27 rows)
useCols:       .buf 1
useStartRow:   .buf 1
useStartCol:   .buf 1
origRows:      .buf 1  ;original window when term started
origCols:      .buf 1
origStartRow:  .buf 1
origStartCol:  .buf 1
origConColors: .buf 6

;use 24 rows: no status
;use 25 rows: rvs status on top
;use 26 rows: separator and status on top
;use 27 rows: separator and status on top, separator on bottom

ConsoleInit = *
   ;** set window parameters
   jsr ConsoleWinInit1
   cmp #28
   bcc +
   sbc #27
   lsr
   clc
   adc origStartRow
   sta useStartRow
   lda #27
   sta useRows
+  cpx #81
   bcc +
   txa
   sbc #80
   lsr
   clc
   adc origStartCol
   sta useStartCol
   lda #80
   sta useCols
+  lda #24
   jsr ConsoleWinInit2
   ;** set up screen display
   jsr ConsoleRepaint
   ;** set console controls
+  ldx #1
   lda #$e0
   sec
   jsr aceConOption ;console attribute enable
   ldx #12
   lda #99
   sec
   jsr aceConOption ;prescroll override
   ldx #8
   lda #$ff
   sec
   jsr aceConOption ;ignore shifts in scrolling
   ldx #3
   lda #$00
   sta attribMode
   sec
   jsr aceConOption ;reset attributes
   rts

   conInitErr: .byte "term: window is too small for emulator "
               .byte "(must be at least 25 rows by 40 cols)\n",0

ConsoleWinInit1 = *
   ;** fetch color palette
   jsr aceWinPalette
   ldx #7
-  lda syswork,x
   sta conColors,x
   dex
   bpl -
   ;** enable attributes
   ldx #1
-  stx work
   clc
   jsr aceConOption
   ldx work
   sta origConColors-1,x
   inx
   cpx #7
   bcc -
   lda conColors+0
   sta charColor
   ;** window stuff
   jsr aceWinSize
   sta origRows
   stx origCols
   sta useRows
   stx useCols
   cmp #25
   bcs +
-  lda #<conInitErr
   ldy #>conInitErr
   jsr eputs
   jmp die
+  cpx #40
   bcc -
   ldy syswork+0
   sty origStartRow
   sty useStartRow
   ldy syswork+1
   sty origStartCol
   sty useStartCol
   rts

ConsoleWinInit2 = *  ;( .A=scrRows )
   sta scrRows
   sta winRows
   lda useCols
   sta scrCols
   sta winCols
   lda useStartCol
   sta scrStartCol
   sta winStartCol
   lda useRows
   ldy useStartRow
   ldx #$00
   cmp #25
   bcc +
   iny
   ldx #$c0
+  cmp #26
   bcc +
   iny
   ldx #$80
+  sty scrStartRow
   sty winStartRow
   stx statlineType
   rts

ConsoleRepaint = *
   lda #$c0
   ldx #$20
   ldy conColors+0
   jsr aceWinCls
   ;** assert "use" window
   lda useStartRow
   ldx useStartCol
   sta syswork+0
   stx syswork+1
   lda useRows
   ldx useCols
   jsr aceWinSet
   jsr aceWinSize
   lda syswork+2
   ldy syswork+3
   sta statlineAddr+0
   sty statlineAddr+1
   jsr updateStatline
   lda useRows
   cmp #26
   bcc +
   lda #1
   ldx #"-"
   jsr ConInitSeparator
+  lda useRows
   cmp #27
   bcc +
   sbc #1
   ldx #"-"
   jsr ConInitSeparator
+  lda winStartRow
   jsr assertTermWin
   rts

assertTermWin = *
   lda winStartRow
   ldx winStartCol
   sta syswork+0
   stx syswork+1
   lda winRows
   ldx winCols
   jmp aceWinSet

assertOrigWin = *
   lda origStartRow
   ldx origStartCol
   sta syswork+0
   stx syswork+1
   lda origRows
   ldx origCols
   jmp aceWinSet

updateStatline = *
   ;** update status and display status line
   ;** baud rate
   ldx #$00
   jsr aceModemParms
   sta work+0
   and #$0f
   sta work+1
   asl
   asl
   adc work+1
   tax
   ldy #20
-  lda statlineBaud,x
   sta statlineMsg,y
   inx
   iny
   cpy #25
   bcc -
   ;** communication parameters
   lda work+0
   and #$80
   asl
   rol
   asl
   asl
   tax
   ldy #26
-  lda statlineParm,x
   sta statlineMsg,y
   inx
   iny
   cpy #29
   bcc -
   ;** terminal type
   lda emulateMode
   asl
   asl
   asl
   tax
   ldy #37
-  lda statlineTerm,x
   sta statlineMsg,y
   inx
   iny
   cpy #43
   bcc -
   ;** capture-buffer modified/active
   lda #" "
   bit capmasFlags
   bpl +
   lda #"*"
+  sta statlineMsg+45
   bit captureActive
   ldx #0
   bit captureActive
   bpl +
   ldx #3
+  ldy #0
-  lda statlineBufA,x
   sta statlineMsg+46,y
   inx
   iny
   cpy #3
   bcc -
   ;** capture-buffer size
   ldx #7
   lda #" "
-  sta statlineMsg+50,x
   dex
   bpl -
   lda #"M"
   sta statlineMsg+58
   lda #"e"
   sta statlineMsg+59
   ldx #3
-  lda capmasBytes,x
   jmp ++
+  lda #$00
+  sta work,x
   dex
   bpl -
   ldx #work
   ldy #50
   jsr updateStatPutnum
   ;** free memory
   ldx #8
   lda #" "
-  sta statlineMsg+62,x
   dex
   bpl -
   ldx #work
   jsr aceMemStat
   ldx #work
   ldy #62
   jsr updateStatPutnum
   jmp dispStatline

   updateStatPutnum = *  ;( .X=32-bit#, .Y=outpos )
   tya
   ldy #>statlineMsg
   clc
   adc #<statlineMsg
   bcc +
   iny
+  sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   lda #" "
   sta (zp),y
   rts

   statlineMsg: .byte "xxxxxxxxxxxx   Link:-----/---   Term:---"
                .byte "---  -buf:------  Mem:-------- Co-I=Help",0,"for Luise",0
                    ;  0123456789012345678901234567890123456789
                    ;  0/4       1/5       2/6       3/7
   statlinePrev: .buf 81
   statlineBaud: .byte "   50  110134.5  300  600 1200 2400 4800"
                 .byte " 9600192003840057600 115k 230k 460k 921k"
                     ;  ^    ^    ^    ^    ^    ^    ^    ^    ^
   statlineParm: .byte "8N1 7E1 "
   statlineTerm:
      .byte "Verbat  Glass   VT-100  ANSI    XTERM   VT-52   ADM3A   CGTERM  "
          ;  ^       ^       ^       ^       ^       ^       ^       ^       ^
   statlineBufA: .byte "bufBUF"

diffFirst: .buf 1
diffLast:  .buf 1
diffLen:   .buf 1

diffStatline = *  ;( .A=diffFirst=start, .X=diffLen=len )
   lda #$ff
   sta diffFirst
   sta diffLast
   ldx #0
-  lda statlineMsg,x
   cmp statlinePrev,x
   sta statlinePrev,x
   beq +
   stx diffLast
   bit diffFirst
   bpl +
   stx diffFirst
+  inx
   cpx #80
   bcc -
   bit diffFirst
   bmi +
   inc diffLast
+  sec
   lda diffLast
   sbc diffFirst
   tax
   stx diffLen
   lda diffFirst
   rts

dispStatline = *
   bit statlineType
   bmi +
   rts
+  jsr diffStatline
   ldy statlineAddr+1
   clc
   adc statlineAddr+0
   bcc +
   iny
+  sta syswork+0
   sty syswork+1
   lda #<statlineMsg
   ldy #>statlineMsg
   clc
   adc diffFirst
   bcc +
   iny
+  sta syswork+2
   sty syswork+3
   lda statlineType
   and #$40
   sta syswork+6
   clc
   lda diffFirst
   adc diffLen
   sec
   sbc useCols
   beq +
   bcc +
   sta work+0
   sec
   lda diffLen
   sbc work+0
   tax
   bcs +
   rts
+  stx syswork+5
   ldy conColors+2
   lda #$e0
   jsr aceWinPut
   rts

InvalidateStatline = *
   ldx #79
   lda #$00
-  sta statlinePrev,x
   dex
   bpl -
   rts

ConInitSeparator = *  ;( .A=row to display separator on, .X=char )
   stx syswork+4
   ldx #0
   jsr aceWinPos
   lda scrCols
   sta syswork+5
   ldy conColors+3
   lda #$c0
   ldx #0
   jmp aceWinPut

ConsoleShutdown = *
   ;** disable some control controls
   ldx #12
   lda #$00
   sec
   jsr aceConOption ;prescroll override disable
   ldx #8
   lda #$00
   sec
   jsr aceConOption ;allow shifts in scrolling
   ldx #3
   lda #$00
   sec
   jsr aceConOption
   ;** restore original window
   jsr assertOrigWin
   ldx origRows
   dex
   txa
   ldx #" "
   jsr ConInitSeparator
+  lda #$40
   ldy conColors+0
   jsr aceWinCls
   sec
   lda origRows
   sbc #1
   ldx #0
   jsr aceConPos
   ;** restore original attributes
   ldx #1
   lda #$80
   sec
   jsr aceConOption
   ldx #2
   lda conColors+0
   sec
   jsr aceConOption
   ldx #3
   lda #$00
   sec
   jsr aceConOption
   rts

checkStop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   jmp die
   stoppedMsg: .byte "<Stopped>\n",0

petToAscTable = *   ;$ff=ignore, $fe=special

          ;0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
   .byte $ff,$ff,$ff,$fe,$fe,$ff,$ff,$fe,$ff,$09,$fe,$ff,$ff,$0d,$ff,$ff ;0
   .byte $ff,$fe,$fe,$fe,$fe,$ff,$ff,$ff,$ff,$ff,$ff,$1b,$fe,$fe,$ff,$ff ;1
   .byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f ;2
   .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f ;3
   .byte $40,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f ;4
   .byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$5b,$5c,$5d,$5e,$fe ;5
   .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ;6
   .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ;7
   .byte $ff,$fe,$ff,$fe,$ff,$fe,$fe,$fe,$fe,$ff,$ff,$ff,$ff,$0d,$ff,$ff ;8
   .byte $fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$ff,$fe,$ff,$ff ;9
   .byte $fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe ;a
   .byte $fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe ;b
   .byte $60,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ;c
   .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$7b,$7c,$7d,$7e,$5f ;d
   .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f ;e
   .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f ;f

ascToPetTable = *
          ;0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
   .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$07,$14,$09,$fe,$fe,$fe,$0a,$ff,$ff ;0
   .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$ff,$ff,$ff,$ff ;1
   .byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f ;2
   .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f ;3
   .byte $40,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf ;4
   .byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$5b,$5c,$5d,$5e,$5f ;5
   .byte $c0,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ;6
   .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$db,$dc,$dd,$de,$ff ;7
   .byte $ff,$ff,$ff,$ff,$fe,$fe,$ff,$ff,$fe,$ff,$ff,$ff,$ff,$fe,$ff,$ff ;8
   .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$ff,$ff,$ff,$ff ;9
   .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af ;a
   .byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf ;b
   .byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f ;c
   .byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f ;d
   .byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef ;e
   .byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$fe ;f

term = *
   jsr cursorOn
termLoop = *
   jsr aceModemCheck
   beq +
   lda #<readbuf
   ldy #>readbuf
   sta zp+0
   sty zp+1
   lda readbufLen+0
   ldy readbufLen+1
   ldx modemFd
   jsr read
   beq +
   jsr cursorOff
   jsr captureStash
   jsr PrintReceivedData
   jsr cursorOn
+  lda stringkeyCount
   beq +
   dec stringkeyCount
   ldx #0
   ldy #0
   lda (stringkeyPtr),y
   inc stringkeyPtr+0
   bne +++
   inc stringkeyPtr+1
   jmp +++
+  jsr aceConKeyAvail
   bcs termLoop
   bit restrVersFlag
   bpl +
   ldx #0
   jsr RestoreVersion
   jsr updateStatline
+  jsr aceConGetkey
+  sta keycode
   stx keyshift
   jsr checkKeypadKey
   tax
   lda petToAscTable,x
   cmp #$ff
   beq ++
   cmp #$fe
   bne +
   jsr cursorOff
   jsr Hotkey
   jsr cursorOn
   jmp ++
+  sta writeChar
   lda #<writeChar
   ldy #>writeChar
   ldx #1
   jsr modemSend
+  bit exitFlag
   bmi +
   jmp termLoop
+  jsr cursorOff
   jsr ConsoleShutdown
   rts
   writeChar: .byte 1

modemSend = *  ;( .AY=dataptr, .X=len )
   sta zp+0
   sty zp+1
   txa
   ldy #0
   ldx modemFd
   jsr write
   rts

cursorFlag: .buf 1

cursorOn = *
   lda #$ff
   sta cursorFlag
   jsr aceConGetpos
   cpx winCols
   bcc +
   dex
   ldy #$fa
   sty cursorFlag
+  jsr aceWinPos
   lda cursorFlag
   ldy conColors+1
   jsr aceWinCursor
   rts

cursorOff = *
   lda #$00
   jsr aceWinCursor
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

puthex = *  ;( .A=hex )
   pha
   lsr
   lsr
   lsr
   lsr
   jsr +
   pla
   and #$0f
+  ora #$30
   cmp #$3a
   bcc +
   adc #6
+  jmp putchar

putnum = *
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   lda #0
   sta numbuf,y
   lda zp+0
   ldy zp+1
   jmp puts
   numbuf: .buf 13

;=== print received data ===

PrintReceivedData = *  ;( readbuf, (zw)=count )
   lda emulateMode
   cmp #0
   bne +
   jmp PrintWriteLiteral
+  sec
   lda #0
   sbc zw+0
   sta trcount+0  ;makes this incrementable
   lda #0
   sbc zw+1
   sta trcount+1
   lda #<readbuf
   ldy #>readbuf
   sta trptr+0
   sty trptr+1

   printNext = *
   lda trcount+0
   ora trcount+1
   bne +
   rts

   ;** handle esc sequences
+- lda escState
   beq ++
   ldy #0
   lda (trptr),y
   tax
   lda ascToPetTable,x
   prEscEntry = *
   jsr EscProcess
   inc trptr+0
   bne +
   inc trptr+1
+  inc trcount+0
   bne -
   inc trcount+1
   bne -
   rts

   ;** handle regular characters
+  lda #<readbuf
   ldy #>readbuf
   sta troutptr+0
   sty troutptr+1
   lda #0
   sta troutcount+0
   sta troutcount+1
   lda trcount+0
   ora trcount+1
   bne +
   rts
+  ldy #0
-  lda (trptr),y
   tax
   lda ascToPetTable,x
   cmp #$fe
   beq prSpecial
   cmp #$ff
   beq prCont
   prTroutPut = *
   sta (troutptr),y
   inc troutptr+0
   bne +
   inc troutptr+1
+  inc troutcount+0
   bne +
   inc troutcount+1
   prCont = *
+  inc trptr+0
   bne +
   inc trptr+1
+  inc trcount+0
   bne -
   inc trcount+1
   bne -
   jsr PrintFlush
   rts

   prSpecial = *  ;process special character in translation (lf/esc)
   txa
   cmp #$ff
   bne +
   jmp prTroutPut
+  and #$ff ;$7f?
   cmp #$1b  ;ESC
   bne ++
-  ldx emulateMode
   cpx #1
   beq +
   pha
   jsr PrintFlush
   pla
   jmp prEscEntry
+  jmp prTroutPut
+  cmp #$9b  ;VT-CSI
   beq -
   cmp #$84  ;VT-IND
   beq -
   cmp #$8d  ;VT-RI
   beq -
   cmp #$85  ;VT-NEL
   beq -
   cmp #$88  ;VT-HTS
   beq -
   cmp #$0a  ;LF
   beq +
   cmp #$0b  ;VT
   beq +
   cmp #$0c  ;FF
   beq +
   lda #$fe
   jmp prTroutPut
+  lda troutcount+0
   ora troutcount+1
   bne +
   jsr aceConGetpos
   cpx #0
   bne prSpecialLfLit
-  ldy #0
   lda #chrCR
   jmp prTroutPut
+  ldy troutptr+1
   ldx troutptr+0
   bne +
   dey
+  dex
   stx work+0
   sty work+1
   ldy #0
   lda (work+0),y  ;check prev char
   cmp #$0a
   beq +
   cmp #chrCR
   beq -
   jmp prSpecialLfLit
+  lda #chrCR
   sta (work+0),y  ;modify prev char
   jmp prCont

   prSpecialLfLit = * ;literal lf -> crsr down
   jsr PrintFlush
   lda #chrVT
   sta readbuf+0
   lda #1
   ldy #0
   sta troutcount+0
   sty troutcount+1
   jsr PrintFlush
   inc trptr+0
   bne +
   inc trptr+1
+  inc trcount+0
   bne +
   inc trcount+1
+  jmp printNext

PrintFlush = *  ;flush trout buffer
   lda troutcount+0
   ora troutcount+1
   beq +
   lda #<readbuf
   ldy #>readbuf
   sta zp+0
   sty zp+1
   lda troutcount+0
   ldy troutcount+1
   ldx #stdout
   jsr write
   ;lda #$b9
   ;jsr putchar
+  rts

PrintWriteLiteral = *  ;( readbuf, (zw)=count )
   lda zw+0
   ldy zw+1
   sta writelen+0
   sty writelen+1
   lda #<readbuf
   ldy #>readbuf
   sta zp+0
   sty zp+1
-  lda writelen+0
   ora writelen+1
   bne +
   rts
+  ldy #0
   lda (zp),y
   tax
   and #$7f
   cmp #$7f
   beq +
   cmp #$20
   bcs ++
+  txa
   jmp ++
+  lda ascToPetTable,x
+  jsr aceConPutlit
   inc zp+0
   bne +
   inc zp+1
+  sec
   lda writelen+0
   sbc #1
   sta writelen+0
   bcs +
   dec writelen+1
+  jmp -
   writelen: .buf 2

;=== "hotkey" routines ===

Hotkey = *  ;( keycode )
   lda keycode
   cmp #$a0
   bcc +
   cmp #$bf+1
   bcs +
   and #$1f
   asl
   tax
   lda hotkeyCommDispatch+0,x
   sta work+0
   lda hotkeyCommDispatch+1,x
   sta work+1
   jmp (work+0)
+  ldx #hotkeyDispatch-hotkeyCodes-1
-  cmp hotkeyCodes,x
   beq +
   dex
   bpl -
   rts
+  txa
   asl
   tax
   lda hotkeyDispatch+0,x
   sta work+0
   lda hotkeyDispatch+1,x
   sta work+1
   ldx #0
   jmp (work+0)

   hotkeyCommDispatch = *
   .word HotAt,HotA,HotB,HotC,HotD,HotE,HotF,HotG,HotH,HotI,HotJ,HotK,HotL
   .word HotM,HotN,HotO,HotP,HotQ,HotR,HotS,HotT,HotU,HotV,HotW,HotX
   .word HotY,HotZ,HotLbracket,HotBslash,HotRbracket,HotUparrow,HotCommBackarrow
   hotkeyCodes = *
   .byte $91,$11,$9d,$1d,$85,$86,$87,$88
   .byte $00,$00,$93,$83,$13,$14,$5f
   .byte $04,$0a,$12,$92,$94,$03,$1c,$90
   .byte $81,$95,$96,$97,$98,$99,$9a,$9b
   hotkeyDispatch = *
   .word HotUp,HotDown,HotLeft,HotRight,HotPF1,HotPF2,HotPF3,HotPF4
   .word HotCoPlus,HotCoMinus,HotClr,HotRun,HotHome,HotDel,HotBackarrow
   .word HotI,HotRun,HotRvs,HotRvsOff,HotDel,HotStop,HotCtrl3,HotCtrl1
   .word HotCo1,HotCo2,HotCo3,HotCo4,HotCo5,HotCo6,HotCo7,HotCo8

HotAt = *
   rts

HotA = *
   jsr HotAWork
   jmp updateStatline

ConfirmBaud = *
   ldx #$00
   jsr aceModemParms
   ldx #$ff
   jsr aceModemParms
   bcc +
   jsr HotAWork
+  rts

HotAWork = *
   ldx #$00
   jsr aceModemParms
   sta work+0
   and #$f0
   sta work+1
-  dec work+0
   lda work+0
   and #$0f
   ora work+1
   ldx #$ff
   jsr aceModemParms
   bcs -
   rts

HotB = *
   ldx #$00
   jsr aceModemParms
   sta work+0
   and #$f0
   sta work+1
-  inc work+0
   lda work+0
   and #$0f
   ora work+1
   ldx #$ff
   jsr aceModemParms
   bcs -
   jmp updateStatline

HotC = *
HotD = *
   rts

HotE = *
   jsr HotS
   bit capmasFlags
   bmi +
   lda #TRUE
   sta exitFlag
+  rts

HotF = *
HotG = *
HotH = *
   rts

HotI = *
   lda #<helpMsg
   ldy #>helpMsg
   sta zp+0
   sty zp+1
   lda #<helpMsgEnd-helpMsg
   ldy #>helpMsgEnd-helpMsg
   ldx #stdout
   jmp write

helpMsg = *
   .byte "\nThe following 'hot keys' are supported ('Co'==Commodore):"
   .byte "\n\n"
   .byte "Co-A : decrease baud rate\n"
   .byte "Co-B : increase baud rate\n"
   .byte "Co-E : exit and save capture buffer if modified\n"
   .byte "Co-I : help information (also HELP)\n"
   .byte "Co-N : name the capture buffer\n"
   .byte "Co-O : show operating status of modem driver\n"
   .byte "Co-P : change parity: 8N1/7E1\n"
   .byte "Co-Q : quit (won't work if buffer is modified)-+\n"
   .byte "Co-S : save capture buffer                     |\n"
   .byte "Co-T : terminal-emulation type                 |\n"
   .byte "Co-Z : pause terminal flow                     v\n"
   .byte "Co-\\ : toggle capture-buffer modification status\n"
   .byte "Co-_ : compose ISO-8859-1 character\n"
   .byte "Co + and Co - : enable(+)/disable(-) capture buffer\n"
   .byte "Ct-1 : clear the capture buffer\n"
   .byte "Ct-3 : enter hexadecimal ASCII character code\n"
   .byte "F1,F3,F5,F7,Ct-@ to Ct-_ : PF1,PF2,PF3,PF4, ASCII Ctrl codes\n"
   .byte "CLR  : clear screen (nothing sent to modem)\n"
   .byte "DEL,HOME,BACKARROW : BS,DEL,_(128) / BS,DEL,ESC(64), "
     .byte "Co-X=swap HOME/DEL\n"
   .byte "RVS,RVSOFF : reverse screen\n"
   .byte "STOP : nothing(128) / send TAB character(64)\n"
helpMsgEnd = *

HotJ = *
HotK = *
HotL = *
HotM = *
   rts

HotN = *
   jsr SetBufferFilename
   rts

HotO = *  ;operating status
   ldx #statOverrun
   jsr aceModemStat
   sta doMoTemp
   lda #<doMoMsg1
   ldy #>doMoMsg1
   jsr puts
   ldx #statOverrun
   jsr putnum
   lda #<doMoMsg2
   ldy #>doMoMsg2
   jsr puts
   ldx #statOverflow
   jsr putnum
   lda #<doMoMsg3
   ldy #>doMoMsg3
   jsr puts
   lda doMoTemp
   jsr puthex
   lda #chrCR
   jsr putchar
   lda #<doMoMsg4
   ldy #>doMoMsg4
   jsr puts
   ldx #statBytRecvd
   jsr putnum
   lda #<doMoMsg5
   ldy #>doMoMsg5
   jsr puts
   ldx #statBytSent
   jsr putnum
   lda #chrCR
   jsr putchar
   lda #chrCR
   jsr putchar
   rts

   doMoTemp: .buf 1
   doMoMsg1: .byte "\nModem Stats: hardware overrun=",0
   doMoMsg2: .byte ", buffer overflow=",0
   doMoMsg3: .byte ", stat=$",0
   doMoMsg4: .byte "  bytes received=",0
   doMoMsg5: .byte ", bytes sent=",0

HotP = *
   ldx #$00
   jsr aceModemParms
   eor #$80
   ldx #$ff
   jsr aceModemParms
   jmp updateStatline

HotQ = *  ;quit
   bit capmasFlags
   bmi +
   lda #TRUE
   sta exitFlag
   rts
+  ldx #bufModError-version
   jsr RestoreVersion
   jsr updateStatline
   jsr Buzz
   rts

HotR = *
   nop
   rts

HotS = *
   jsr modemClose
   ldx #savingMsg-version
   jsr RestoreVersion
   jsr updateStatline
   jsr captureSave
   bcc +
   ldx #fileErrorMsg-version
   jsr RestoreVersion
   jsr updateStatline
   jsr Buzz
   jmp ++
+  ldx #savedMsg-version
   jsr RestoreVersion
   jsr updateStatline
+  jsr modemOpen
   rts

HotT = *
   inc emulateMode
   lda emulateMode
   cmp #2+1
   bcc +
   lda #0
   sta emulateMode
+  jmp updateStatline

HotU = *
HotV = *
HotW = *
   rts

HotX = *
   lda txDel
   ldx txHome
   stx txDel
   sta txHome
   rts

HotY = *
   rts

HotZ = *
   ldx #pauseMsg-version
   jsr RestoreVersion
   jsr updateStatline
   jsr CursorGreen
   jsr aceConGetkey
   jsr cursorOff
   ldx #0
   jsr RestoreVersion
   jsr updateStatline
   rts

HotLbracket = *
   rts

HotBslash = *
   lda capmasFlags
   eor #$80
   sta capmasFlags
   jmp updateStatline

HotRbracket = *
HotUparrow = *
   rts

HotCommBackarrow = *
   lda #12
   jsr Compose
   rts

HotCtrl1 = *
   bit capmasFlags
   bpl +
   ldx #bufModError-version
   jsr RestoreVersion
   jsr updateStatline
   jsr Buzz
   rts
+  jsr modemClose
   jsr captureKill
   jsr modemOpen
   jsr updateStatline
   rts

HotCtrl3 = *
   lda #0
   jsr Compose
   rts

HotPF4 = *
   inx ;7
HotPF3 = *
   inx ;6
HotPF2 = *
   inx ;5
HotPF1 = *
   inx ;4
HotRight = *
   inx ;3
HotLeft = *
   inx ;2
HotDown = *
   inx ;1
HotUp = *
   nop ;0
   txa
   cmp #4
   bcs +
   bit cursorMode
   bpl +
   clc
   adc #8
+  asl
   asl
   ldy #>cursorPfCodes
   clc
   adc #<cursorPfCodes
   bcc +
   iny
+  ldx #3
   jmp modemSend

   cursorPfCodes = *
   .byte "\e[a_\e[b_\e[d_\e[c_\eop_\eoq_\eor_\eos_\eoa_\eob_\eod_\eoc_"

HotCoPlus = *
   lda #$ff
   sta captureActive
   jsr updateStatline
   rts

HotCoMinus = *
   lda #$00
   sta captureActive
   jsr updateStatline
   rts

HotClr = *
   lda #chrCLS
   jmp putchar

HotRun = *
   nop
   rts

HotRvs = *
   ldx #5
   lda #$ff
   sec
   jmp aceWinOption

HotRvsOff = *
   ldx #5
   lda #$00
   sec
   jmp aceWinOption

HotHome = *
   lda #<txHome
   ldy #>txHome
   ldx #1
   jmp modemSend
   rts

HotDel = *
   lda #<txDel
   ldy #>txDel
   ldx #1
   jmp modemSend
   rts

HotBackarrow = *
   lda #<txBackarrow
   ldy #>txBackarrow
   ldx #1
   jmp modemSend

HotStop = *
   lda txStop
   bne +
   rts
+  lda #<txStop
   ldy #>txStop
   ldx #1
   jmp modemSend

HotUserFunc = *
   ldx #0
   cmp #$95
   bcc +
   sbc #$94
   tax
+  txa
   clc
   adc #>stringkeys
   tay
   lda #<stringkeys
lda #<testMsg
ldy #>testMsg
   sta stringkeyPtr+0
   sty stringkeyPtr+1
   lda stringkeyLens,x
lda #testMsgEnd-testMsg
   sta stringkeyCount
   rts
   testMsg:  .byte "xx this is a test message for testing your modem "
             .byte "to see how fast it will operate and other things",chrCR
   testMsgEnd = *
   stringkeyLens: .buf 8

checkKeypadKey = *  ;( .A=keychar, .X=shift ) : .A=keychar
   ;** check for C= +, C= -, keypad
   cmp #"+"
   bne +
   txa
   and #$0f
   cmp #2
   bne +
   jsr HotCoPlus
   lda #$00
   rts
+  cmp #"-"
   bne +
   txa
   and #$0f
   cmp #2
   bne +
   jsr HotCoMinus
   lda #$00
   rts
+  txa
   and #$20
   bne +
-  lda keycode
   rts
+  bit keypadMode
   bpl -
   ldx #13
-  lda keycode
   cmp keypadKeys,x
   beq +
   dex
   bpl -
   lda keycode
   rts
+  txa
   sta work
   asl
   adc work
   ldy #>keypadCodes
   clc
   adc #<keypadCodes
   bcc +
   iny
+  ldx #3
   jsr modemSend
   lda #$00
   rts
   keypadKeys:   .byte "0123456789-+.",chrCR
   keypadCodes:  .byte "\eo",$70,"\eo",$71,"\eo",$72,"\eo",$73,"\eo",$74
                 .byte "\eo",$75,"\eo",$76,"\eo",$77,"\eo",$78,"\eo",$79
                 .byte "\eo",$6d,"\eo",$6c,"\eo",$6e,"\eom"

;===catpure-buffer stuff===

captureInit = *
   ldx #captureDefaultNameEnd-captureDefaultName-1
-  lda captureDefaultName,x
   sta capmasName,x
   dex
   bpl -
captureReinit = *
   ldx #3
-  lda #aceMemNull
   sta captureMasterPtr,x
   sta captureBufferPtr,x
   sta capmasNext,x
   sta capmasPrev,x
   sta capmasHead,x
   sta capmasTail,x
   sta capbufNext,x
   sta capbufPrev,x
   lda #$00
   sta capmasBytes,x
   dex
   bpl -
   lda #$00
   sta capmasFlags
   sta capbufLen
   sta capbufFlags
   rts
   captureDefaultName: .byte "capturebuf",0
   captureDefaultNameEnd = *

captureStash = *  ;( readbuf, (zw)=count ) : (zw)=count
   bit captureActive
   bmi +
   rts
+  lda zw+0
   ldy zw+1
   sta capCount+0
   sty capCount+1
   sta capLenSave+0
   sty capLenSave+1
   lda #<readbuf
   ldy #>readbuf
   sta capPtr+0
   sty capPtr+1
-  lda capCount+0
   ora capCount+1
   beq captureStashFinish
   lda capCount+0
   ldx capCount+1
   beq +
   lda #255
+  jsr capStashChunk
   sta work+0
   bcc +
   lda #$00
   sta captureActive
   jmp captureStashFinish
+  sec
   lda capCount+0
   sbc work+0
   sta capCount+0
   bcs +
   dec capCount+1
+  clc
   lda capPtr+0
   adc work+0
   sta capPtr+0
   bcc +
   inc capPtr+1
+  clc
   lda capmasBytes+0
   adc work+0
   sta capmasBytes+0
   bcc +
   inc capmasBytes+1
   bne +
   inc capmasBytes+2
   bne +
   inc capmasBytes+3
+  lda capmasFlags
   ora #$80
   sta capmasFlags
   jmp -
   captureStashFinish = *
   jsr updateStatline
   lda capLenSave+0
   ldy capLenSave+1
   sta zw+0
   sty zw+1
   rts

capStashChunkBytes: .buf 1

capStashChunk = *  ;( .A=maxBufBytes ) : .A=actualBytes, .CS=err
   sta capStashChunkBytes
   ldx capbufLen
   cpx #245
   bcc +
   jsr capStashFlush
   bcc +
   rts
+  sec
   lda #245
   sbc capbufLen
   cmp capStashChunkBytes
   bcc +
   lda capStashChunkBytes
+  sta capStashChunkBytes
   ldy #0
   ldx capbufLen
-  lda (capPtr),y
   sta capbufData,x
   inx
   iny
   cpy capStashChunkBytes
   bcc -
   clc
   lda capbufLen
   adc capStashChunkBytes
   sta capbufLen
   lda capStashChunkBytes
   clc
   rts

capStashFlush = *
   jsr modemClose
   jsr capStashFlushWork
   php
   jsr modemOpen
   plp
   rts

capStashFlushWork = *
   ;;lda #$06
   ;;jsr putchar
   ;** check if there are no user bytes in the capturebuf page
   lda capbufLen
   bne +
   rts
   ;** get a page to store the data
+  ldx #0
   ldy #255
   lda #1
   jsr aceMemAlloc
   bcc +
   sec
   rts
+  ldx #3
-  lda mp,x
   sta captureBufferPtr,x
   lda #aceMemNull
   sta capbufNext,x
   lda capmasTail,x
   sta capbufPrev,x
   dex
   bpl -
   ;** store the new page of data
   lda #<captureBufferPage
   ldy #>captureBufferPage
   sta zp+0
   sty zp+1
   lda #0
   ldy #1
   jsr aceMemStash
   ;** make previous buffer page point to this one
   ldx #3
-  lda capmasTail,x
   sta mp,x
   lda captureBufferPtr,x
   sta capmasTail,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   bne +
   ;first block--write list head
   ldx #3
-  lda captureBufferPtr,x
   sta capmasHead,x
   dex
   bpl -
   jmp ++
   ;normal block--update next ptr of prev page
+  ldx #captureBufferPtr
   ldy #4
   jsr aceMemZpstore
+  nop
   ;** update the 
   ;** finish
   lda #0
   sta capbufLen
   sta capbufFlags
   clc
   rts

capSaveFd: .buf 1

captureSave = *  ;( ) : .CS=err
   ;** flush current page
   jsr capStashFlushWork
   ;** open file
   lda #<capmasName
   ldy #>capmasName
   sta zp+0
   sty zp+1
   lda #"W"
   jsr open
   bcc +
   ;;lda errno
   ;;sta $10f9
   rts
+  sta capSaveFd
   ;** get list head
   ldx #3
-  lda capmasHead,x
   sta mp,x
   dex
   bpl -
   ;** scan list
   captureSaveNext = *
   lda mp+3
   cmp #aceMemNull
   bne +
   ;** close and exit if finished
   lda capSaveFd
   jsr close
   lda capmasFlags
   and #$7f
   sta capmasFlags
   lda #0
   sta capbufLen
   clc
   rts
   ;** write page
+  lda #<captureBufferPage
   ldy #>captureBufferPage
   sta zp+0
   sty zp+1
   lda #0
   ldy #1
   jsr aceMemFetch
   lda #<capbufData
   ldy #>capbufData
   sta zp+0
   sty zp+1
   lda capbufLen
   ldy #0
   ldx capSaveFd
   jsr write
   ldx #3
-  lda capbufNext,x
   sta mp,x
   dex
   bpl -
   jsr aceConStopkey
   bcs +
   jmp captureSaveNext
+  lda capSaveFd
   jsr close
   sec
   rts

captureKill = *  ;( ) : .CS=err
   ;** flush current page
   jsr capStashFlushWork
   ;** get list head
   ldx #3
-  lda capmasHead,x
   sta mp,x
   dex
   bpl -
   ;** scan list
   captureKillNext = *
   lda mp+3
   cmp #aceMemNull
   bne +
   ;** reinit and exit if finished
   jsr captureReinit
   rts
   ;** write page
+  ldx #work+0
   ldy #4
   jsr aceMemZpload
   lda #1
   jsr aceMemFree
   ldx #3
-  lda work,x
   sta mp,x
   dex
   bpl -
   jmp captureKillNext

;===character composition===

composeType:  .buf 1
composeChars: .buf 2
composeLen:   .buf 1
composeCode:  .buf 1

Compose = *  ;( .A=0:hex/12:iso8859-1 )
   sta composeType
   lda #0
   sta composeLen
   jsr CursorGreen
   jsr ComposePrompt
   jsr aceConGetkey
   sta composeChars+0
   ldx composeType
   bne +
   jsr ComposeCheckHex
   bcs composeErrorExit
+  inc composeLen
   jsr ComposePrompt
   jsr aceConGetkey
   sta composeChars+1
   inc composeLen
   ldx composeType
   bne +
   jsr ComposeHex
   jmp ++
+  jsr ComposeIso8859_1
+  bcs composeErrorExit
   jsr ComposePrompt
   jsr cursorOff
   lda #TRUE
   sta restrVersFlag
   lda #<composeCode
   ldy #>composeCode
   ldx #1
   jsr modemSend
   rts

   composeErrorExit = *
   jsr cursorOff
   ldx #composeError-version
   jsr RestoreVersion
   jsr updateStatline
   jsr Buzz
   rts

ComposePrompt = *  ;( composeLen, composeType )
   ldx #0
   ldy composeType
-  lda hexInPrompt,y
   sta statlineMsg,x
   iny
   inx
   cpx #12
   bcc -
   ldx #3
   lda #" "
-  sta statlineMsg+8,x
   dex
   bpl -
   ldx #8
   ldy composeLen
   beq ++
   lda composeChars+0
   sta statlineMsg+8
   inx
   cpy #1
   beq ++
   lda composeChars+1
   sta statlineMsg+9
   lda #":"
   sta statlineMsg+$a
   lda composeCode
   and #$7f
   cmp #$20
   php
   lda composeCode
   plp
   bcc +
   cmp #$ff
   beq +
   ldx composeCode
   lda ascToPetTable,x
+  sta statlineMsg+$b
   jmp ++
+  lda #"_"
   sta statlineMsg,x
+  jsr updateStatline
   rts

   hexInPrompt:     .byte "HexASC:$01:x"
   composeInPrompt: .byte "Compose:xx:x"
   version:         .byte "ACEterm 1.10"
   composeError:    .byte "InvalidCode!"
   bufModError:     .byte "BufMod: Co-\\"
   fileErrorMsg:    .byte "File Error!!"
   savingMsg:       .byte "Saving...   "
   savedMsg:        .byte "Buffer saved"
   pauseMsg:        .byte "Flow paused "
                         ; 0123456789ab

CursorGreen = *
   jsr aceConGetpos
   cpx winCols
   bcc +
   dex
+  jsr aceWinPos
   lda #$ff
   ldy conColors+2
   jsr aceWinCursor
   rts

ComposeHex = *  ;( composeChars ) : composeCode, .CS=err
   lda composeChars+0
   jsr ComposeCheckHex
   bcs +
   asl
   asl
   asl
   asl
   sta composeCode
   lda composeChars+1
   jsr ComposeCheckHex
   bcs +
   ora composeCode
   sta composeCode
   clc
+  rts

ComposeCheckHex = *  ;( .A=char ) : .A=binValue, .CS=err
   cmp #"0"
   bcc ++
   cmp #"9"+1
   bcc +
   and #$7f
   cmp #"a"
   bcc ++
   cmp #"f"+1
   bcs ++
   sbc #6
+  and #$0f
   clc
   rts
+  sec
   rts

RestoreVersion = *  ;( .X=offset )
   ldy #0
-  lda version,x
   sta statlineMsg,y
   inx
   iny
   cpy #12
   bcc -
   lda #FALSE
   cpx #12
   beq +
   lda #TRUE
+  sta restrVersFlag
   rts

ComposeIso8859_1 = *  ;( composeChars ) : composeCode, .CS=err
   jsr ComposeCheckIso
   bcc +
   jsr ComposeSwapChars
   jsr ComposeCheckIso
   bcc +
   jsr ComposeSwapChars
   jsr ComposeAlterCases
   jsr ComposeCheckIso
   bcc +
   jsr ComposeSwapChars
   jsr ComposeCheckIso
+  rts

ComposeCheckIso = *  ;( composeChars ) : composeCode, .CS=not found
   ldx #0
-  lda composeChars+0
   cmp iso8859_1CompCodes+0,x
   bne +
   lda composeChars+1
   cmp iso8859_1CompCodes+1,x
   beq ++
+  inx
   inx
   cpx #iso8859_1ExtraCodeValues-iso8859_1CompCodes
   bcc -
   sec
   rts
+  txa
   lsr
   adc #$a0
   sta composeCode
   bcs +
   rts
+  tax
   lda iso8859_1ExtraCodeValues,x
   sta composeCode
   clc
   rts

iso8859_1CompCodes = *
   .byte "  !!C/L-XOY-||SO\"\"COA_<<~~--RO__"  ;ASCII $a0--$af
   .byte "0^+-2^3^''/UP!.^,,1^O_>>141234??"    ;ASCII $b0--$bf
   .byte "`A'A^A~A\"A*AAEC,`E'E^E\"E`I'I^I\"I" ;ASCII $c0--$cf
   .byte "D-~N`O'O^O~O\"O**O/`U'U^U\"U'YPPss"  ;ASCII $d0--$df
   .byte "`a'a^a~a\"a*aaec,`e'e^e\"e`i'i^i\"i" ;ASCII $e0--$ef
   .byte "%o~n`o'o^o~o\"o//o/`u'u^u\"u'ypp\"y" ;ASCII $f0--$ff
   .byte "C|L=X0Y S!S0C0R0"                    ;Extra codes
iso8859_1ExtraCodeValues = *
   .byte $a2,$a3,$a4,$a5,$a7,$a7,$a9,$ae

ComposeSwapChars = *
   lda composeChars+0
   ldx composeChars+1
   sta composeChars+1
   stx composeChars+0
   rts

ComposeAlterCases = *
   ldx #1
-  lda composeChars,x
   and #$7f
   cmp #"a"
   bcc +
   cmp #"z"+1
   bcs +
   lda composeChars,x
   eor #$80
   sta composeChars,x
+  dex
   bpl -
   rts

Buzz = *
   lda #chrBEL
   jsr putchar
   rts

;===buffer filename input===

posSave: .buf 2

SetBufferFilename = *
   jsr aceConGetpos
   sta posSave+0
   stx posSave+1
   lda useStartRow
   ldx useStartCol
   sta syswork+0
   stx syswork+1
   lda #1
   ldx useCols
   jsr aceWinSet
   ldx #2
   lda conColors+2
   sec
   jsr aceConOption
   lda #<namePrompt
   ldy #>namePrompt
   jsr puts
   lda #<readbuf
   ldy #>readbuf
   sta zp+0
   sty zp+1
   ldy #0
-  lda capmasName,y
   sta readbuf,y
   beq +
   iny
   bne -
+  jsr aceConInput
   bcs +
   cpy #0
   beq +
   sty work+0
   ldy #0
-  lda readbuf,y
   sta capmasName,y
   beq +
   iny
   cpy #230
   bcc -
   lda #$00
   sta capmasName,y
+  jsr assertTermWin
   lda posSave+0
   ldx posSave+1
   jsr aceConPos
   jsr InvalidateStatline
   jsr updateStatline
   jsr escAssertAttrib
   rts

   namePrompt: .byte "\fBufName: ",0

;===user-configurable options===

txBackarrow: .byte $5f  ;underscore
txHome:      .byte $7f  ;del
txDel:       .byte $08  ;backspace
txStop:      .byte $00  ;nothing
defEmulate:  .byte 2    ;vt100

userkeyInit = *
   jsr aceConKeyAvail
   cpy #$00
   bne +
   lda #$1b
   sta txBackarrow
   lda #$09
   sta txStop
+  rts

;===new stuff===

HotCo1 = *  ;full screen
   jsr ConfirmBaud
   jsr aceWinMax
   jsr ConsoleWinInit1
   lda useRows
   ldx #3
-  cmp #25
   bcc +
   sbc #1
+  dex
   bne -
   jsr ConsoleWinInit2
   jsr ConsoleRepaint
   lda #$00
   sta statlinePrev+0
   sta statlinePrev+79
   jsr dispStatline
   rts
HotCo2 = *  ;24-rows
   jsr assertOrigWin
   lda #$93
   jsr putchar
   lda #$00
   sta statlinePrev+0
   sta statlinePrev+79
   jmp ConsoleInit
HotCo3 = *  ;norm screen
   lda #0
   ldx #0
   HotCoDoScreen = *
   jsr aceWinScreen
   jmp HotCo1
HotCo4 = *  ;40-cols
   lda #0
   ldx #40
   jmp HotCoDoScreen
HotCo5 = *  ;max rows
   lda #255
   ldx #0
   jmp HotCoDoScreen
HotCo6 = *
   rts
HotCo7 = *
   rts
HotCo8 = *  ;80-cols
   lda #0
   ldx #80
   jmp HotCoDoScreen

;=== escape sequence control ===

EscProcess = *  ;( .A=char ) ...finite-state machine
   sta escChar
   ;** ANSI-ish interpreter
   ldx escState
   beq +
   jmp escNext
+  ldx #$01
   stx escState
   ldx #0
   stx escParm
   stx escQuesFlag
   stx escParmData+0
   stx escParmData+1
   cmp #$1b  ;ESC
   bne +
   rts
+  cmp #$9b  ;CSI
   bne +
   ldx #$02
   stx escState
   rts
+  cmp #$90  ;DCS
   bne +
   ldx #$03
   sta escState
   ;** command terminators
+  ldx #0
-  cmp escAnsiRawChar,x
   beq +
   inx
   cpx #escAnsiRawDispatch-escAnsiRawChar
   bcc -
   jmp escFinish
+  txa
   asl
   tax
   lda escAnsiRawDispatch+0,x
   sta syswork+0
   lda escAnsiRawDispatch+1,x
   sta syswork+1
   jsr +
   jmp escFinish
+  jmp (syswork+0)

escAnsiRawChar:
   .byte $00,$05,$07,$08  ;NUL,ENQ,BEL, BS  ;(1)
   .byte $09,$0a,$0b,$0c  ; HT, LF, VT, FF  ;(2)
   .byte $0d,$0e,$0f,$11  ; CR, SO, SI,XON  ;(3)
   .byte $13,$18,$1a,$7f ;XOFF,CAN,SUB,DEL  ;(4)
   .byte $84,$85,$88,$8d  ;IND,NEL,HTS, RI  ;(5)
   .byte $8e,$8f,$90,$9c  ;SS2,SS3          ;(6)
escAnsiRawDispatch:
   .word ActNull,ActEnquire,ActBell,ActBackspace    ;(1)
   .word ActTab,ActLinefeed,ActLinefeed,ActLinefeed ;(2)
   .word ActCr,ActSetG1toGL,ActSetG0toGL,ActXon     ;(3)
   .word ActXoff,ActNull,ActRvsQuestion,ActNull     ;(4)
   .word ActIndex,ActNewline,ActTabSet,ActRvsIndex  ;(5)
   .word ActSetG2toGL,ActSetG3toGL                  ;(6)

   escNext = *
   ldx escState
   cpx #$02
   beq escCsi
   cmp #"["
   bne +
   ldx #$02
   stx escState
   rts
+  cmp #"("
   bne +
-  sta escQuesFlag
   rts
+  cmp #")"
   beq -
   cmp #"*"
   beq -
   cmp #"+"
   beq -
   ;** command terminators
+  cmp #"="
   bne +
   jmp escKeypadApp
+  cmp #">"
   bne +
   jmp escKeypadNorm
+  cmp #"D"
   bne +
   jmp escCursorDownScroll
+  cmp #"M"
   bne +
   jmp escCursorUpScroll
+  cmp #"E"
   bne +
   jmp escNewline
+  cmp #"H"
   bne +
   jmp escTabSet
+  cmp #"7"
   bne +
   jmp escCursorSave
+  cmp #"8"
   bne +
   jmp escCursorRestore
+  cmp #"Z"
   bne +
   jmp escDeviceId
+  cmp #"c"
   bne +
   jmp escHardReset
+  jmp escMalformed

   escCsi = *
   cmp #"?"
   bne +
-  sta escQuesFlag
   rts
+  cmp #">"
   beq -
   cmp #"!"
   beq -
+  cmp #"0"
   bcc +
   cmp #"9"+1
   bcs +
   jsr escHandleDigit
   rts
+  cmp #";"
   bne ++
   inc escParm
   ldx escParm
   cpx #23
   bcc +
   ldx #23
   stx escParm
+  lda #0
   sta escParmData,x
   rts
   ;** command terminators
+  inc escParm
   cmp #"A"
   bne +
   jmp escCursorUp
+  cmp #"B"
   bne +
   jmp escCursorDown
+  cmp #"C"
   bne +
   jmp escCursorRight
+  cmp #"D"
   bne +
   jmp escCursorLeft
+  cmp #"H"
   bne +
   jmp escCursorPos
+  cmp #"f"
   bne +
   jmp escCursorPos
+  cmp #"g"
   bne +
   jmp escTabClear
+  cmp #"m"
   bne +
   jmp escAttrib
+  cmp #"h"
   bne +
   jmp escTermModeSet
+  cmp #"l"
   bne +
   jmp escTermModeClear
+  cmp #"L"
   bne +
   jmp escInsertLine
+  cmp #"M"
   bne +
   jmp escDeleteLine
+  cmp #"@"
   bne +
   jmp escInsertChar
+  cmp #"P"
   bne +
   jmp escDeleteChar
+  cmp #"X"
   bne +
   jmp escEraseChar
+  cmp #"K"
   bne +
   jmp escEraseLine
+  cmp #"J"
   bne +
   jmp escEraseScreen
+  cmp #"r"
   bne +
   jmp escScrollRegion
+  cmp #"i"
   bne +
   jmp escPrinterControl
+  cmp #"n"
   bne +
   jmp escDeviceStatus
+  cmp #"c"
   bne +
   jmp escDeviceAttr
+  cmp #"p"
   bne +
   jmp escSoftReset
+  jmp escMalformed

   escMalformed = *
   lda #chrBEL
   jsr putchar

   escFinish = *
   lda #$00
   sta escState
   rts

escHandleDigit = *  ;( .A=digit )
   and #$0f
   sta work+0
   ldx escParm
   lda escParmData,x
   asl
   bcs +
   asl
   bcs +
   clc
   adc escParmData,x
   bcs +
   asl
   bcs +
   clc
   adc work+0
   bcc ++
+  lda #255
+  sta escParmData,x
   rts

;=== escape sequence action routines, VT-220 annotations ===

ActNull = *  ;do nothing
   rts

ActEnquire = *  ;send answerback message
   nop  ;&&&
   rts

ActBell = *  ;ring bell
   lda #chrBEL
   jmp putchar

ActBackspace = *  ;backspace
   lda #chrBS
   jmp putchar

ActTab = *   ;perform tab
   nop ;&&& check if tabstops are 8 or custom
   lda #chrTAB
   jmp putchar

ActLinefeed = *  ;perform linefeed/newline
   nop ;&&& check if chrCR should be used
   nop ;&&& check if top line needs to be saved
   lda #chrVT
   jmp putchar

ActNewline = *  ;perform newline
   lda #chrCR
   jmp putchar

ActCr = *    ;perform carriage return only
   lda #chrBOL
   jmp putchar

ActSetG1toGL = *  ;set G1 into GL
ActSetG0toGL = *  ;set G0 into GL
ActSetG2toGL = *  ;set G2 into GL
ActSetG3toGL = *  ;set G3 into GL
ActXon = *  ;enable keyboard-input transmission
ActXoff = *  ;disable keyboard-input transmission
ActRvsQuestion = *  ;display a reverse-question error indicator
ActTabSet = *  ;set tab stop
   nop  ;&&&
   rts

ActIndex = *  ;cursor down and scroll screen if necessary
   jmp escCursorDownScroll

ActRvsIndex = *  ;cursor up and scroll screen if necessary
   jmp escCursorUpScroll

escCursorPos = *  ;ESC [ row ; col H    //   ESC [ row ; col f
   ;** get coordinates
   ldx #1
-  lda escParmData,x
   bne +
   lda #1
+  sec
   sbc #1
   cmp scrRows,x
   bcc +
   lda scrRows,x
   sbc #1
+  sta escParmData,x
   dex
   bpl -
   ;** determine if location is inside of current scroll window
   sec
   lda winStartRow
   sbc scrStartRow
   sta work+0         ;start col of scroll window in full term window
   lda escParmData+0
   cmp work+0
   bcc +
   clc
   lda work+0
   adc winRows
   cmp escParmData+0
   beq +
   bcs ++
   ;** if not within window, make window full-screen--approximation of vt100
+  lda scrStartRow
   ldx scrStartCol
   sta winStartRow
   stx winStartCol
   sta syswork+0
   stx syswork+1
   lda scrRows
   ldx scrCols
   sta winRows
   stx winCols
   jsr aceWinSet
   lda #0
   sta work+0
   ;** if within window, move cursor
+  sec
   lda escParmData+0
   sbc work+0
   ldx escParmData+1
   jsr aceConPos
   jmp escFinish

escCursorUpScroll = *
escCursorUp = *   ;ESC [ count A   //   ESC M
   lda #$91
   escCursorRep = *
   sta escCursorChar
   lda escParmData+0
   bne +
   inc escParmData+0
+- lda escCursorChar
   jsr aceConPutctrl
   dec escParmData+0
   bne -
   jmp escFinish
   escCursorChar: .buf 1

escCursorDownScroll
escCursorDown = *  ;ESC [ count B  //   ESC D
   lda #$11
   jmp escCursorRep

escCursorRight = *  ;ESC [ count C
   lda #$1d
   jmp escCursorRep

escCursorLeft = *  ;ESC [ count D
   lda #$9d
   jmp escCursorRep

escCursorSave = *  ;ESC 7
   sec
   lda winStartRow
   sbc scrStartRow
   sta cursorSavePos+0
   jsr aceConGetpos
   sec ;sic
   adc cursorSavePos+0
   sta cursorSavePos+0
   inx
   stx cursorSavePos+1
   lda attribMode
   sta cursorSaveAttr
   lda charColor
   sta cursorSaveColor
   jmp escFinish

escCursorRestore = *  ;ESC 8
   lda cursorSaveAttr
   sta attribMode
   lda cursorSaveColor
   sta charColor
   jsr escAssertAttrib
   lda cursorSavePos+0
   ldx cursorSavePos+1
   sta escParmData+0
   stx escParmData+1
   jmp escCursorPos

escNewline = *  ;ESC E
   lda #"\n"
   jsr putchar
   jmp escFinish

escEraseLine = *  ;ESC [ cmd K
   lda #$f1
   ldx escParmData+0
   beq +
   lda #$f0
   dec escParmData+0
   beq +
   lda #$f8
+  ldx #1
   stx escParmData
   jmp escCursorRep

escEraseScreen = *  ;ESC [ cmd J
   lda #$e0
   ldx escParmData+0
   beq +
   lda #$ef
   dec escParmData+0
   bne ++
+  ldx #1
   stx escParmData
   jmp escCursorRep
+  lda #$c0
   ldx #" "
   ldy conColors+0
   jsr aceWinCls
   jmp escFinish

escKeypadApp = *  ;ESC =
   lda #$ff
   sta keypadMode
   jmp escFinish

escKeypadNorm = *  ;ESC >
   lda #$00
   sta keypadMode
   jmp escFinish

escScrollRegion = *  ;ESC [ top bottom r
   lda escParmData+0
   beq +
   sec
   sbc #1
   cmp scrRows
   bcc +
   lda scrRows
   sbc #1
+  clc
   adc scrStartRow
   sta syswork+0
   ldx scrStartCol
   stx syswork+1
   lda escParmData+1
   bne +
   lda scrRows
+  cmp scrRows
   beq +
   bcc +
   lda scrRows
+  clc
   adc scrStartRow
   sec
   sbc syswork+0
   ldx scrCols
   jsr aceWinSet
   jsr aceWinSize
   sta winRows
   stx winCols
   lda syswork+0
   ldx syswork+1
   sta winStartRow
   stx winStartCol
   jmp escFinish

escTabSet = *  ;ESC H
   nop
   jmp escFinish

escTabClear = *  ;ESC command g
   nop
   jmp escFinish

escInsertLine = *  ;ESC [ count L
   lda #$e9
   jmp escCursorRep

escDeleteLine = *  ;ESC [ count M
   lda #$e4
   jmp escCursorRep

escInsertChar = *  ;ESC [ count @
   lda #$94
   jmp escCursorRep

escDeleteChar = *  ;ESC [ count P
   lda #$08
   jmp escCursorRep

escAttrib = *  ;ESC [ mode m
   ldx #0
-  lda escParmData,x
   bne +
   ldy conColors+0
   sty charColor
   jmp +++
+  tay
   lda #$01
   cpy #1
   beq +
   lda #$20
   cpy #4
   beq +
   lda #$10
   cpy #5
   beq +
   lda #$40
   cpy #7
   beq +
   jsr escAttribExtra
   jmp +++
+  ora attribMode
+  sta attribMode
+  inx
   cpx escParm
   bcc -
   jsr escAssertAttrib
   jmp escFinish

   escAttribExtra = *
   lda #$ff-$01
   cpy #22
   beq +
   cpy #21
   beq +
   lda #$ff-$20
   cpy #24
   beq +
   lda #$ff-$10
   cpy #25
   beq +
   lda #$ff-$40
   cpy #27
   bne ++
+  and attribMode
   sta attribMode
-  rts
+  cpy #30
   bcc -
   cpy #38
   bcs +
   tya
   sec
   sbc #30
   tay
   lda charColor
   and #$f0
   ora escAttribColors,y
   sta charColor
   rts
+  cpy #40
   bcc -
   cpy #48
   bcs -
   tya
   sec
   sbc #40
   tay
   lda escAttribColors,y
   asl
   asl
   asl
   asl
   sta work
   lda charColor
   and #$0f
   ora work
   sta charColor
   rts
   escAttribColors : .byte $0,$8,$4,$d,$2,$a,$7,$e

   escAssertAttrib = *
   ldx #3
   lda attribMode
   and #$f0
   sec
   jsr aceConOption
   lda attribMode
   and #$01
   sta work
   ldx #2
   lda charColor
   eor work
   sec
   jmp aceConOption

escTermModeSet = *  ;ESC [ type h   //   ESC [ ? type h
   lda escParmData+0
   cmp #1
   bne +
   lda escQuesFlag
   cmp #"?"
   bne +
   lda #$ff
   sta cursorMode
   jmp escFinish
+  cmp #3
   bne +
   lda #chrCLS
   jsr putchar
   jmp escFinish
+  jmp escFinish

escTermModeClear = *  ;ESC [ type l   //   ESC [ ? type l
   lda escParmData+0
   cmp #1
   bne +
   lda escQuesFlag
   cmp #"?"
   bne +
   lda #$00
   sta cursorMode
   jmp escFinish
+  cmp #3
   bne +
   lda #chrCLS
   jsr putchar
   jmp escFinish
+  jmp escFinish

escEraseChar = *  ;ESC [ count X
   lda #<escEraseCharMsg
   ldy #>escEraseCharMsg
   jsr puts
   jmp escFinish
   escEraseCharMsg: .byte "{erase_char}",0

escPrinterControl = *  ;ESC [ command i   //   ESC [ ? command i
   nop
   jmp escFinish

escDeviceStatus = *  ;ESC type n
   lda escParmData+0
   cmp #6
   beq +
   nop
   jmp escFinish
+  lda #$00
   sta work+1
   sta work+2
   sta work+3
   sec
   lda winStartRow
   sbc scrStartRow
   sta work+0
   jsr aceConGetpos
   sec ;sic
   adc work+0
   sta work+0
   inx
   stx work+5
   ldx #2
   stx escDevLen
   jsr escDevPutnum
   ldx escDevLen
   lda #";"
   sta escDevStatReply,x
   inc escDevLen
   lda work+5
   sta work+0
   jsr escDevPutnum
   ldx escDevLen
   lda #$52
   sta escDevStatReply,x
   inx
   lda #<escDevStatReply
   ldy #>escDevStatReply
   jsr modemSend
   jmp escFinish
   escDevStatReply: .byte "\e[24;80r",0,0,0   ;note: taken as ASCII
   escDevLen: .buf 1

   escDevPutnum = * ;( [work+0]=num )
   clc
   lda #<escDevStatReply
   ldy #>escDevStatReply
   adc escDevLen
   bcc +
   iny
+  sta zp+0
   sty zp+1
   lda #1
   ldx #work+0
   jsr aceMiscUtoa
   tya
   clc
   adc escDevLen
   sta escDevLen
   rts

escDeviceAttr = *  ;ESC [ command c  //  ESC [ ? command c  //  ESC [ > cmd c
   lda escParmData+0
   cmp #0
   beq escDeviceId
   nop
   jmp escFinish

escDeviceId = *  ;ESC Z
   lda #<escDeviceIdMsg
   ldy #>escDeviceIdMsg
   ldx #7
   jsr modemSend
   jmp escFinish
   escDeviceIdMsg: .byte "\e[?1;2",$63,0

escSoftReset = *  ;ESC [ ! p
   nop
   jmp escFinish

escHardReset = *  ;ESC c
   nop
   jmp escFinish

;===bss===

bss        = *
readbuf    = bss+0
captureMasterPage = readbuf+readbufLenMax
capmasNext     = captureMasterPage+0  ;(4)
capmasPrev     = captureMasterPage+4  ;(4)
capmasHead     = captureMasterPage+8  ;(4)
capmasTail     = captureMasterPage+12 ;(4)
capmasBytes    = captureMasterPage+16 ;(4)
capmasFlags    = captureMasterPage+20 ;(1) ;$80=modified
capmasName     = captureMasterPage+21 ;(233+1)
captureBufferPage = captureMasterPage+256
capbufNext     = captureBufferPage+0  ;(4)
capbufPrev     = captureBufferPage+4  ;(4)
capbufFlags    = captureBufferPage+8  ;(1) ;unused
capbufLen      = captureBufferPage+9  ;(1)
capbufData     = captureBufferPage+10 ;(245)
stringkeys = captureBufferPage+256  ;256 bytes each, eight strings
screenSave = stringkeys+0  ;(0)
bssEnd     = screenSave+0
