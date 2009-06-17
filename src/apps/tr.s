;*** translation program - by Craig Bruce, started 10-Jul-93

!src "../system/acehead.s"
!to "../../build/tr", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $40

chrQuote = $22
maxLineLen = 2049
true = $ff
false = $00

trPetscii     = 0
trAsciiCrLf   = 1
trAsciiLf     = 2
trAsciiCr     = 3
trSpeedscript = 4

arg        = 2  ;(2)
name       = 4  ;(2)
string     = 8  ;(2)
bufPtr     = 10 ;(2)
bufCount   = 12 ;(2)
infile     = 14 ;(1)
inBufLen   = 16 ;(2)
readTrTab  = 18 ;(2)
writeTrTab = 20 ;(2)
outBufCount = 22 ;(1)
outfile    = 23 ;(1)
trFrom     = 24 ;(1)
trTo       = 25 ;(1)
trInPlace  = 26 ;(1)

;******** standard library ********
puts = *
   ldx #stdout
fputs = *
   sta zp
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
   ldx #stdout
putc = *
   sta putcBuffer
   lda #<putcBuffer
   ldy #>putcBuffer
   sta zp
   sty zp+1
   lda #1
   ldy #0
   jmp write
   putcBuffer = *
      !fill 1

getarg = *
   sty zp+1
   asl
   sta zp
   rol zp+1
   clc
   lda aceArgv
   adc zp
   sta zp
   lda aceArgv+1
   adc zp+1
   sta zp+1
   ldy #0
   lda (zp),y
   tax
   iny
   lda (zp),y
   stx zp
   sta zp+1
   rts

;===main===

main = *
   lda #trAsciiCrLf
   sta trFrom
   lda #trPetscii
   sta trTo
   lda #false
   sta trInPlace
   lda #stdout
   sta outfile
   jsr setInPlacePath
   ;** check argument count
   lda aceArgc+1
   bne enoughArgs
   lda aceArgc+0
   cmp #2
   bcs enoughArgs
   jmp usage

enoughArgs = *
   lda #1
   ldy #0
   sta arg+0
   sty arg+1
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne fileArg
   inc arg
   iny
   lda (zp),y
   cmp #"i"
   bne +
   lda #true
   sta trInPlace
   lda arg+0
   ldy arg+1
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne fileArg
   inc arg
   iny
+  jsr getTrType
   stx trFrom
   iny
   jsr getTrType
   stx trTo

   fileArg = *
-  jsr aceConStopkey
   bcc +
   jmp stopped
+  lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   ora zp+1
   beq mainExit
   jsr trFile
   bcc +
   jsr error
+  inc arg+0
   bne +
   inc arg+1
+  jmp -

mainExit = *
   rts

getTrType = *  ;( (zp)+.y=str ) : .X=type
   lda (zp),y
   iny
   cmp #"i"
   bne +
   lda #true
   sta trInPlace
   jmp getTrType
+  ldx #trPetscii
   cmp #"p"
   beq getTrRet
   ldx #trSpeedscript
   cmp #"s"
   beq getTrRet
   ldx #trAsciiLf
   cmp #"u"
   beq getTrRet
   ldx #trAsciiCrLf
   cmp #"m"
   beq getTrRet
   ldx #trPetscii
   cmp #"c"
   beq getTrRet
   cmp #"a"
   bne usage
   lda (zp),y
   iny
   ldx #trAsciiLf
   cmp #"l"
   beq getTrRet
   ldx #trAsciiCr
   cmp #"c"
   beq getTrRet
   cmp #"r"
   beq getTrRet
   dey
   ldx #trAsciiCrLf
   getTrRet = *
   clc
   rts

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   lda #1
   jmp aceProcExit

usageMsg = *
   !pet "usage: tr [-i] [-fromset2toset] file ..."
   !byte chrCR
   !pet "where fromset/toset=p,a,al,ac,s,c,u,m"
   !byte chrCR
   !pet "(petscii,asc-crlf,asc-lf,asc-cr,"
   !byte chrCR
   !pet " speedscript,commodore,unix,ms-dos)"
   !byte chrCR
   !pet "also, the 'i' flag means 'in place'"
   !byte chrCR,0

stopped = *
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   lda #1
   jmp aceProcExit
   stoppedMsg = *
   !pet "<Stopped>"
   !byte chrCR,0

error = *
   lda #<errorMsg1
   ldy #>errorMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<errorMsg2
   ldy #>errorMsg2
   jmp eputs
   errorMsg1 = *
   !pet "Error reading file "
   !byte chrQuote,0
   errorMsg2 = *
   !byte chrQuote,chrCR,0

trFile = *
   lda trInPlace
   beq +
   jsr inPlaceEcho
+  jsr initTr
   jsr initInBuf
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   lda trInPlace
   beq ++
   lda #<inPlacePathname
   ldy #>inPlacePathname
   sta zp+0
   sty zp+1
   lda #"W"
   jsr open
   bcc +
   lda infile
   jsr close
   rts
+  sta outfile
++ jsr trBody
   lda infile
   jsr close
   lda trInPlace
   beq +
   lda outfile
   jsr close
   jsr inPlaceCopyBack
+  clc
   rts

trBody = *
   jsr initOutBuf
-  jsr getByte
   bcs bodyExit
   ldx trFrom
   beq bodyWrite
   cmp #13
   bne +
   cpx #trAsciiCrLf
   beq -
   cpx #trAsciiCr
   beq bodyWrite
+  tay
   lda (readTrTab),y

   bodyWrite = *
   ldx trTo
   beq bodyRep
   cmp #13
   bne +
   cpx #trSpeedscript
   beq +
   cpx #trAsciiCr
   beq bodyRep
   cpx #trAsciiLf
   beq +
   jsr putByte
   lda #13
+  tay
   lda (writeTrTab),y

   bodyRep = *
   jsr putByte
   jmp -

   bodyExit = *
   jsr flushOutBuf
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
   jsr aceConStopkey
   bcc +
   jsr stopped
+  lda #<inBuf
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
   jmp getByte
+  sec
   rts

initInBuf = *
   sec
   lda aceMemTop+0
   sbc #<inBuf
   sta inBufLen+0
   lda aceMemTop+1
   sbc #>inBuf
   sta inBufLen+1
   lda #0
   sta bufCount+0
   sta bufCount+1
   ldx outfile
   jsr aceFileInfo
   cmp #0
   bne +
   lda #<254
   ldy #>254
   sta inBufLen+0
   sty inBufLen+1
+  rts

putByte = *
   ldx outBufCount
   cpx #254
   bcc +
   pha
   jsr flushOutBuf
   pla
   ldx outBufCount
+  sta outBuf,x
   inc outBufCount
   rts

flushOutBuf = *
   jsr aceConStopkey
   bcc +
   jmp stopped
+  lda #<outBuf
   ldy #>outBuf
   sta zp+0
   sty zp+1
   lda outBufCount
   beq +
   ldy #0
   ldx outfile
   jsr write
   lda #0
   sta outBufCount
+  rts

initOutBuf = *
   lda #0
   sta outBufCount
   rts

inPlaceFilename = *
   !pet "0tmp-tr-inplace"
   !byte 0

setInPlacePath = *
   lda #<inPlacePathname
   ldy #>inPlacePathname
   sta zp+0
   sty zp+1
   lda #4
   jsr aceDirName
   ldx #0
-  lda inPlaceFilename,x
   sta inPlacePathname,y
   beq +
   iny
   inx
   bne -
+  rts

inPlaceCopyBack = *
   lda #<inPlacePathname
   ldy #>inPlacePathname
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"W"
   jsr open
   bcc +
   rts
+  jsr initInBuf
   lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
-  lda inBufLen+0
   ldy inBufLen+1
   ldx infile
   jsr read
   beq +
   ldx outfile
   jsr write
   jmp -
+  lda infile
   jsr close
   lda outfile
   jsr close
   lda #<inPlacePathname
   ldy #>inPlacePathname
   sta zp+0
   sty zp+1
   jsr aceFileRemove
   rts

inPlaceEcho = *
   lda #<ipEchoMsg1
   ldy #>ipEchoMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<ipEchoMsg2
   ldy #>ipEchoMsg2
   jmp eputs
   ipEchoMsg1 = *
      !pet "translating file "
      !byte chrQuote,0
   ipEchoMsg2 = *
      !byte chrQuote,chrCR,0

initTr = *
   lda trFrom
   asl
   tax
   lda readTr,x
   sta readTrTab+0
   lda readTr+1,x
   sta readTrTab+1
   lda trTo
   asl
   tax
   lda writeTr,x
   sta writeTrTab+0
   lda writeTr+1,x
   sta writeTrTab+1
   rts

readTr  = *
   !word 0,ascToPet,ascToPet,ascToPet,spdToPet
writeTr = *
   !word 0,petToAsc,petToAsc,petToAsc,petToSpd

ascToPet = *
       ;0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
!byte $00,$01,$02,$03,$04,$05,$06,$07,$14,$09,$0d,$11,$93,$0a,$0e,$0f ;0
!byte $10,$0b,$12,$13,$08,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f ;1
!byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f ;2
!byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f ;3
!byte $40,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf ;4
!byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$5b,$5c,$5d,$5e,$5f ;5
!byte $c0,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ;6
!byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$db,$dc,$dd,$de,$df ;7
!byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f ;8
!byte $90,$91,$92,$0c,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f ;9
!byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af ;a
!byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf ;b
!byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f ;c
!byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f ;d
!byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef ;e
!byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff ;f

spdToPet = *
       ;0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
!byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ;0
!byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$0d ;1
!byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f ;2
!byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f ;3
!byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf ;4
!byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df ;5
!byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af ;6
!byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf ;7
!byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$5f,$0e,$0f ;8
!byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f ;9
!byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef ;a
!byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff ;b
!byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f ;c
!byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f ;d
!byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f ;e
!byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f ;f

petToAsc = *
       ;0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
!byte $00,$01,$02,$03,$04,$05,$06,$07,$14,$09,$0d,$11,$93,$0a,$0e,$0f ;0
!byte $10,$0b,$12,$13,$08,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f ;1
!byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f ;2
!byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f ;3
!byte $40,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f ;4
!byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$5b,$5c,$5d,$5e,$5f ;5
!byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf ;6
!byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df ;7
!byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f ;8
!byte $90,$91,$92,$0c,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f ;9
!byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af ;a
!byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf ;b
!byte $60,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ;c
!byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$7b,$7c,$7d,$7e,$7f ;d
!byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef ;e
!byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff ;f

petToSpd = *
       ;0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
!byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$1f,$8e,$8f ;0
!byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f ;1
!byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f ;2
!byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f ;3
!byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f ;4
!byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$8d ;5
!byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef ;6
!byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff ;7
!byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf ;8
!byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df ;9
!byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f ;a
!byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f ;b
!byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ;c
!byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f ;d
!byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af ;e
!byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf ;f

;===bss===

bss     = *
outBuf  = bss+0
inPlacePathname = outBuf+256
inBuf   = inPlacePathname+64
