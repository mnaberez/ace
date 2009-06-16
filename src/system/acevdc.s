;ACE-128/64 kernel VDC 80-column screen driver code

;vdc memory layout: $0000=char,$1000=color,$2000=charset,$3000=altcharset

vdcCharAddr = $0000
vdcColorAddr = $1000
vdcCharsetAddr = $2000
vdcSelect = $d600
vdcStatus = $d600
vdcData = $d601
vdcRowInc = 80

vdcScrRows   .byte 25
vdcScrCols   .byte 80
vdcRegNum    .buf 1
vdcCursorLoc .buf 2

vdcStartup = *
   jsr vdcHardwareReset
   jsr vdcFillMode
   jmp +
   vdcReloadInit = *
   ;** charset
   jsr vdcFillMode
   lda #$00
   ldx #0
   jsr vdcLoadCharset
   ;** init hardware
+  jmp vdcWarmReset

vdcHardwareReset = *
   ldx #0
-  lda vdcHardData,x
   cpx #$1e
   beq +
   cpx #$1f
   beq +
   jsr vdcWrite
+  inx
   cpx #$25
   bcc -
   lda vdcStatus ;vdc version
   and #$07
   ldx #$40
   cmp #1
   bcc +
   ldx #$47
+  txa
   ldx #$19
   jsr vdcWrite
   rts
   vdcHardData .byte $7e,$50,$66,$49,$20,$e0,$19,$1d  ;regs $00-$07
               .byte $fc,$e7,$a0,$e7,$00,$00,$00,$00  ;regs $08-$0f
               .byte $00,$00,$00,$00,$10,$00,$78,$e8  ;regs $10-$17
               .byte $20,$47,$00,$00,$2f,$e7,$4f,$20  ;regs $18-$1f
               .byte $09,$60,$7d,$64,$f5              ;regs $20-$24

vdcWarmReset = *
   ldx #5
   clc
   jsr vdcWinOption
   pha
   ldx #1
   clc
   jsr vdcWinOption
   pha
   jsr vdcHardwareReset
   pla
   ldx #1
   sec
   jsr vdcWinOption
   pla
   ldx #5
   sec
   jsr vdcWinOption
   ;** set attributes address
   lda #<vdcColorAddr
   ldy #>vdcColorAddr
   ldx #$14
   jsr vdcWrite16
   ;** cursor height
   lda #8
   ldx #$0b
   jsr vdcWrite
   jsr vdcFillMode
   rts

vdcShutdown = *
   lda #25
   jsr vdcSetRows
   ;** restore charsets
   lda #<$d000
   ldy #>$d000
   jsr vdcGetRomCharset
   lda #$00
   ldx #0
   jsr vdcLoadCharset
   lda #<$d800
   ldy #>$d800
   jsr vdcGetRomCharset
   lda #<$3000
   ldy #>$3000
   ldx #0
   jsr vdcLoadSpecCharset
   ;** restore attributes
   .if useC128
   lda #<$800
   ldy #>$800
   ldx #$14
   jsr vdcWrite16
   .ife
   ;** restore cursor height
   lda #7
   ldx #$0b
   jsr vdcWrite
   rts

vdcGetRomCharset = *  ;( .AY=romChrAddr )
   sta syswork+0
   sty syswork+1
   lda #$00
   ldy aceCharSetPage
   sta syswork+2
   sty syswork+3
   php
   sei
   lda #bkCharset
   sta bkSelect
   ldx #8
   ldy #0
-  lda (syswork+0),y
   sta (syswork+2),y
   iny
   bne -
   inc syswork+1
   inc syswork+3
   dex
   bne -
   lda #bkACE
   sta bkSelect
   plp
   rts

chsSource = syswork+0 ;(2)
chsCount  = syswork+2 ;(1)

vdcLoadCharset = *  ;( .A=startChar, .X=charCount )
   stx chsCount
   pha
   lda #<vdcCharsetAddr
   ldy #>vdcCharsetAddr
   jsr vdcAddrWrite16
   pla
   jmp +
   vdcLoadSpecCharset = *
   jsr vdcAddrWrite16
   lda #0
   ldx #0
   stx chsCount
+  ldy #$00
   sty chsSource+1
   ldx #3
-  asl
   rol chsSource+1
   dex
   bne -
   sta chsSource+0
   clc
   lda chsSource+1
   adc aceCharSetPage
   sta chsSource+1
   lda #bkRam0io
   sta bkSelect

   charLoop = *
   lda #$1f
   sta vdcRegNum
   sta vdcSelect
   ldy #0
-  lda (chsSource),y
-  bit vdcStatus
   bpl -
   sta vdcData
   iny
   cpy #8
   bcc --
   lda #$00
   jsr vdcRamWrite
   ldx #$1e
   lda #7
   jsr vdcWrite
   clc
   lda chsSource+0
   adc #8
   sta chsSource+0
   bcc +
   inc chsSource+1
+  dec chsCount
   bne charLoop
   lda #bkACE
   sta bkSelect
   rts

vdcFillMode = *  ;( )
   ldx #$18
   jsr vdcRead
   and #$7f
   jsr vdcWrite
   rts

vdcCopyMode = *  ;( )
   ldx #$18
   jsr vdcRead
   ora #$80
   jsr vdcWrite
   rts
   
vdcRamWrite = *  ;( .A=value )
   ldx #$1f

vdcWrite = *  ;( .X=register, .A=value )
   stx vdcRegNum
   stx vdcSelect
-  bit vdcStatus
   bpl -
   sta vdcData
   rts

vdcAddrWrite16 = *  ;( .AY=value )
   ldx #$12

vdcWrite16 = *  ;( .X=hiRegister, .AY=value )
   stx vdcRegNum
   stx vdcSelect
-  bit vdcStatus
   bpl -
   sty vdcData
   inx
   stx vdcRegNum
   stx vdcSelect
-  bit vdcStatus
   bpl -
   sta vdcData
   rts

vdcRamRead = *  ;( ) : .A=value
   ldx #$1f

vdcRead = *  ;( .X=register ) : .A=value
   stx vdcRegNum
   stx vdcSelect
-  bit vdcStatus
   bpl -
   lda vdcData
   rts

vdcActivate = *  ;( .A=rows, .X=cols )
   sta vdcScrRows
   sta winRows
   sta winMaxRows
   ldx vdcScrCols
   stx winCols
   stx winMaxCols
   stx winRowInc
   jsr vdcSetRows
   lda #<vdcCharAddr
   ldy #>vdcCharAddr
   sta winCharAddr+0
   sty winCharAddr+1
   sta winStartAddr+0
   sty winStartAddr+1
   lda #0
   sta winStartRow
   sta winStartCol
   ldx #7
-  lda configBuf+$b0,x
   sta winPalette,x
   dex
   bpl -
   rts

vdcSetRows = *  ;( .A=rows )
   cmp #25+1
   bcc +
   cmp configBuf+$cf
   bcs vdcVerticalCrossover
+  cmp #30
   bcc +
   lda #30
+  pha
   jsr vdcWarmReset
   pla
   ldx #6
   jsr vdcWrite
   cmp #25+1
   bcc +
   sec
   sbc #26
   tay
   ldx #7
   jsr vdcRead
   clc
   adc vdcVert7Vals,y
   jsr vdcWrite
   lda vdcVert5Vals,y
   ldx #5
   jsr vdcWrite
+  rts
   vdcVert7Vals .byte 1,1,2,2,2
   vdcVert5Vals .byte 6,4,6,5,1

vdcVerticalCrossover = *  ;( .A=rows ) : .A=vdcReg5, .Y=vdcReg7
   cmp #51
   bcc +
   lda #51
+  pha
   ldy #6
-  lda vdcRegSaveIndex,y
   tax
   lda vdcRegFiftyRows,y
   jsr vdcWrite
   dey
   bpl -
   pla
   ldx #6
   jsr vdcWrite
   tay
   clc
   adc #1
   lsr
   and #%11111110
   clc
   adc #27
   cpy #50
   bcc +
   lda #53
+  ldx #7
   jsr vdcWrite
   rts
   vdcRegFiftyRows .byte $80,$38,$ff,$e8,51,$06,$35
   vdcRegSaveIndex .byte 0,4,8,9,6,5,7

vdcWinPos = *
   jsr vdcMult80
   clc
   lda syswork+0
   adc winStartAddr+0
   sta syswork+0
   lda syswork+1
   adc winStartAddr+1
   sta syswork+1
   rts

vdcMult80 = *  ;( .A=row:0-255, .X=col ) : (sw+0)=row*80+col, .X:unch
   sta syswork+0
   ldy #0
   sty syswork+1
   asl
   rol syswork+1
   asl
   rol syswork+1
   adc syswork+0
   bcc +
   inc syswork+1
+  asl
   rol syswork+1
   asl
   rol syswork+1
   asl
   rol syswork+1
   asl
   rol syswork+1
   stx syswork+0
   clc
   adc syswork+0
   bcc +
   inc syswork+1
+  sta syswork+0
   rts

vdcPutWhich .buf 1
vdcPutColor .buf 1
vdcPutLen   .buf 1

vdcWinPut = *
   sta vdcPutWhich
   sty vdcFillByte
   stx vdcPutLen
   bit vdcPutWhich
   bpl vdcWinPutColor
   lda syswork+0
   ldy syswork+1
   jsr vdcAddrWrite16
   ldy #0
   cpy vdcPutLen
   beq +
   lda #$1f
   sta vdcRegNum
   sta vdcSelect
-  lda (syswork+2),y 
-  bit vdcStatus
   bpl -
   sta vdcData
   iny
   cpy vdcPutLen
   bcc --
+  sec
   lda syswork+5
   sbc vdcPutLen
   beq vdcWinPutColor
   tay
   lda syswork+4
   jsr vdcRamWrite
   dey
   beq vdcWinPutColor
   tya
   ldx #$1e
   jsr vdcWrite

   vdcWinPutColor = *
   bit vdcPutWhich
   bvs +
   clc
   rts
+  lda vdcFillByte
   and #$0f
   sta vdcFillByte
   lda vdcPutWhich
   and #$20
   beq +
   lda syswork+6
   and #$f0
   ora vdcFillByte
   sta vdcFillByte
+  lda syswork+1
   clc
   adc #>vdcColorAddr
   tay
   lda syswork+0
   jsr vdcAddrWrite16
   lda syswork+5
   sta vdcFillCols
   jmp vdcFillGotAddr

vdcFillByte .buf 1
vdcFillCols .buf 1

vdcFill = * ;( (sw+0)=addr, vdcFillByte, vdcFillCols )
   lda syswork+0
   ldy syswork+1
   jsr vdcAddrWrite16
   vdcFillGotAddr = *
   lda vdcFillCols
   beq +
   lda vdcFillByte
   jsr vdcRamWrite
   ldx vdcFillCols
   dex
   beq +
   txa
   ldx #$1e
   jsr vdcWrite
+  clc
   rts

vdcWinGet = *
   brk
   ;%%%

vdcWinCopyInit = *
   jmp vdcCopyMode

vdcWinCopyRow = *
   bit winScrollMask
   bvc +
   clc
   lda winScrollDest+1
   adc #>vdcColorAddr
   tay
   lda winScrollDest+0
   jsr vdcAddrWrite16
   clc
   lda winScrollSource+1
   adc #>vdcColorAddr
   tay
   lda winScrollSource+0
   jsr vdcWinCopyDo
+  bit winScrollMask
   bpl +
   lda winScrollDest+0
   ldy winScrollDest+1
   jsr vdcAddrWrite16
   lda winScrollSource+0
   ldy winScrollSource+1
   vdcWinCopyDo = *
   ldx #$20
   jsr vdcWrite16
   lda winCols
   ldx #$1e
   jmp vdcWrite
+  rts

vdcWinCopyFinish = *
   jmp vdcFillMode

vdcFastScroll = *
   sec
   rts

vdcCursorSave  .buf 1
vdcCursorColor .buf 1

vdcWinCursor = *
   cmp #0
   beq vdcCursorOff
   sta vdcCursorSave
   sty vdcCursorColor
   lda syswork+0
   ldy syswork+1
   sta vdcCursorLoc+0
   sty vdcCursorLoc+1
   ldx #$0e
   jsr vdcWrite16
   ldx #$0a
   jsr vdcRead
   and #$1f
   ldy vdcCursorSave
   cpy #$fa
   bne +
   ora #$40
   jmp ++
+  ora #$60
+  jsr vdcWrite
   jsr vdcSetColorAddr
   jsr vdcRamRead
   sta vdcCursorSave
   jsr vdcSetColorAddr
   lda vdcCursorSave
   and #$f0
   ora vdcCursorColor
   jsr vdcRamWrite
   rts

vdcCursorOff = *
   lda vdcCursorLoc+0
   ldy vdcCursorLoc+1
   sta syswork+0
   sty syswork+1
   ldx #$0a
   jsr vdcRead
   and #$1f
   ora #$20
   jsr vdcWrite
   jsr vdcSetColorAddr
   lda vdcCursorSave
   jsr vdcRamWrite
   rts

vdcSetColorAddr = *  ;( (sw+0)=addr )
   clc
   lda syswork+1
   adc #>vdcColorAddr
   tay
   lda syswork+0
   jmp vdcAddrWrite16

vdcWinOption = *
   ;** 1.screen color
   dex
   bne vdcOptBorder
   php
   sei
   bcc ++
   bit vdcSsActive
   bpl +
   sta vdcSsColor
   jmp ++
+  ldx #$1a
   jsr vdcWrite
+  bit vdcSsActive
   bpl +
   lda vdcSsColor
   jmp ++
+  ldx #$1a
   jsr vdcRead
+  plp
   clc
   rts
   ;** 2.border color
   vdcOptBorder = *
+  dex
   bne +
   lda #$00
   clc
   rts
   ;** 3.cursor style
+  dex
   bne ++
   bcc +
   nop
+  nop
   clc
   rts
   ;** 4.cursor-blink speed
+  dex
   bne ++
   bcc +
   nop
+  nop
   clc
   rts
   ;** 5.screen rvs
+  dex
   bne vdcOptCpu
   bcc +++
   tay
   ldx #$18
   jsr vdcRead
   cpy #0
   beq +
   ora #%01000000
   jmp ++
+  and #%10111111
+  jsr vdcWrite
+  ldx #$18
   jsr vdcRead
   and #$40
   beq +
   lda #$ff
+  clc
   rts
   ;** 6.cpu speed (ignore)
   vdcOptCpu = *
   dex
   bne +
   jmp notImp
   ;** 7.color palette
+  dex
   bne ++
   bcc +
   nop
+  nop
   clc
   rts
+  jmp notImp

vdcIrqCursor = *
   ;** do nothing
   rts

vdcSsColor  .buf 1
vdcSsMode   .buf 1
vdcSsActive .byte $00

vdcScreenSave = *
-  bit vdcStatus
   bpl -
   lda vdcRegNum
   pha
   ldx #$19
   jsr vdcRead
   sta vdcSsMode
   ldx #$1a
   jsr vdcRead
   sta vdcSsColor
   lda #$ff
   sta vdcSsActive
   ldx #$19
   lda vdcSsMode
   and #%10111111
   jsr vdcWrite
   ldx #$1a
   lda #$00
   jsr vdcWrite
   pla
   sta vdcRegNum
   sta vdcSelect
-  bit vdcStatus
   bpl -
   rts

vdcScreenUnsave = *
-  bit vdcStatus
   bpl -
   lda vdcRegNum
   pha
   ldx #$19
   lda vdcSsMode
   jsr vdcWrite
   ldx #$1a
   lda vdcSsColor
   jsr vdcWrite
   lda #$00
   sta vdcSsActive
   pla
   sta vdcRegNum
   sta vdcSelect
-  bit vdcStatus
   bpl -
   rts

;=== hires stuff ===

;vdc register values courtesy of Fred Bowen of Commodore

;vdc memory: 00000-21359=even frame, 21360-42719=odd frame

vdcEvenFrame  = 0
vdcOddFrame   = 21360
vdcGrMode     .buf 1  ;$00=lores, $ff=hires
vdcGrColor    .buf 1

vdcGrScreen = *  ; ( .A=grType, .X=borderColor, .Y=BgFgColor )
   pha           ;               : .A=cols8, (sw+0)=rows, .X=xAspect
   sty vdcGrColor
   tya
   ldx #4
-  asl
   lsr vdcGrColor
   dex
   bne -
   ora vdcGrColor
   sta vdcGrColor
   ldx #26
   jsr vdcWrite
   lda #<640
   ldy #>640
   sta conMouseMaxX+0
   sty conMouseMaxX+1
   pla
   cmp #1
   beq +
   lda configBuf+$aa
   cmp #64
   bcc +
   jmp aceGrScreenHires
+  lda #$00      
   sta vdcGrMode
   ;** re-select 25 rows
   lda #25
   jsr vdcSetRows
   ldx #25
   jsr vdcRead
   and #%00001111
   ora #%10000000
   jsr vdcWrite
   lda #$00
   jsr vdcGrFill
   lda #<200
   ldy #>200
   sta syswork+0
   sty syswork+1
   sta conMouseMaxY+0
   sty conMouseMaxY+1
   jsr conMouseBounds
   lda #80
   ldx #2
   clc
   rts

   aceGrScreenHires = *
   lda #$ff
   sta vdcGrMode
   ldy #0
-  ldx vdcInitRegs,y
   lda vdcInitVals,y
   cpx #25
   bne +
   jsr vdcRead
   and #$0f
   ora vdcInitVals,y
+  jsr vdcWrite
   iny
   cpy #vdcInitVals-vdcInitRegs
   bcc -
   lda #$00
   jsr vdcGrFill
   lda #<491
   ldy #>491
   sta syswork+0
   sty syswork+1
   sta conMouseMaxY+0
   sty conMouseMaxY+1
   jsr conMouseBounds
   lda #80
   ldx #1
   clc
   rts

vdcInitRegs = *
   .byte 000,001,002,004,005,006,007,008,009,024,025,027,028,020,021
   .byte 012,013,026,012,013
vdcInitVals = *            
   .byte $7e,$50,$66,$4c,006,$4c,$47,003,006,000,$80,000,016,166,224
   .byte 000,000,224,0,0    ;last two = >evenFrame,<evenFrame

vdcGrExit = *  ;( )
   jsr vdcWarmReset
   lda #$00
   ldx #0
   jsr vdcLoadCharset
   ;** init hardware
   lda winRows
   ldx winCols
   jsr kernWinScreen
   rts

vdcGrFill = *  ;( .A=fillValue )
   pha
   lda #>vdcEvenFrame
   ldy #<vdcEvenFrame
   jsr vdcAddrWrite16
   ldx #31
   pla
   jsr vdcWrite
   ldy #161
   bit vdcGrMode
   bmi +
   ldy #63
+  nop
-  ldx #30
   lda #$00
   jsr vdcWrite
   dey
   bne -
   bit vdcGrMode
   bpl +
   lda #<vdcOddFrame+19600
   ldy #>vdcOddFrame+19600
   jsr vdcAddrWrite16
   lda #$00
   jsr vdcRamWrite
   lda #79
   ldx #$1e
   jsr vdcWrite
+  clc
   rts

vdcBmFrame .buf 1
vdcBmRows  .buf 1
vdcBmCols  .buf 1
vdcBmBuffer = stringBuffer
vdcGrOpFlags = syswork+15
vdcGrOpFlagsIn .buf 1

vdcGrOp = *  ;( .A=opflags, .X=X, (sw+0)=Y, .Y=cols, (sw+2)=rows, sw+4=interlv,
   ;**           sw+5=fillval, (sw+6)=sPtr, (sw+8)=dPtr, (sw+10)=mPtr )
   ;**           <all syswork arguments can change>
   ;** opflags: $80=get, $40=put, $20=copy, $10=fill,$8=mask,$4=and,$2=xor,$1=or
   sta vdcGrOpFlags
   sta vdcGrOpFlagsIn
   sty vdcBmCols
   and #$0f
   beq +
   bit vdcGrOpFlags
   bmi +
   lda #<vdcBmBuffer
   ldy #>vdcBmBuffer
   sta syswork+8
   sty syswork+9
   lda vdcGrOpFlags
   ora #$80
   sta vdcGrOpFlags
+  clc
   lda vdcBmCols
   adc syswork+4
   sta syswork+4
   bit vdcGrMode
   bmi +
   lda syswork+0
   ldy syswork+1
   jsr vdcMult80
   jmp vdcGrOpLoop
+  lsr syswork+1
   lda syswork+0
   sta vdcBmFrame
   ror
   jsr vdcMult80
   lda vdcBmFrame
   and #$01
   beq vdcGrOpLoop
   clc
   lda syswork+0
   adc #<vdcOddFrame
   sta syswork+0
   lda syswork+1
   adc #>vdcOddFrame
   sta syswork+1
vdcGrOpLoop = *
   ldy #0
   cpy vdcBmCols
   bne vdcGrOpGet
   jmp vdcGrOpContinue
vdcGrOpGet = *
   bit vdcGrOpFlags
   bpl vdcGrOpPut
   lda syswork+0
   ldy syswork+1
   jsr vdcAddrWrite16
   lda #$1f
   sta vdcRegNum
   sta vdcSelect
   ldy #0
-  bit vdcStatus
   bpl -
   lda vdcData
   sta (syswork+8),y
   iny
   cpy vdcBmCols
   bcc -
vdcGrOpPut = *
   bit vdcGrOpFlags
   bvc vdcGrOpCopy
   lda syswork+0
   ldy syswork+1
   jsr vdcAddrWrite16
   lda #$1f
   sta vdcRegNum
   sta vdcSelect
   ldy #0
-  lda vdcGrOpFlags
   and #$0f
   bne +
-  lda (syswork+6),y
   jmp vdcGrPut
+  and #$08
   bne +
   lda (syswork+8),y
   jmp ++
+  lda (syswork+10),y
   eor #$ff
   and (syswork+8),y
+  ldx vdcGrOpFlags
   stx syswork+14
   lsr syswork+14
   bcc +
   ora (syswork+6),y
   jmp vdcGrPut
+  lsr syswork+14
   bcc +
   eor (syswork+6),y
   jmp vdcGrPut
+  lsr syswork+14
   bcc -
   sta syswork+14
   lda (syswork+6),y
   eor #$ff
   and syswork+14

   vdcGrPut = *
-  bit vdcStatus
   bpl -
   sta vdcData
   iny
   cpy vdcBmCols
   bcc ---
vdcGrOpCopy = *
   lda vdcGrOpFlags
   and #$20
   beq vdcGrOpFill
   ldx #$20
   lda syswork+0
   ldy syswork+1
   jsr vdcWrite16
   lda #$00  ;xx get real address
   ldy #$00
   jsr vdcAddrWrite16
   ldx #$1e
   lda vdcBmCols
   jsr vdcWrite
vdcGrOpFill = *
   lda vdcGrOpFlags
   and #$10
   beq vdcGrOpContinue
   lda syswork+0
   ldy syswork+1
   jsr vdcAddrWrite16
   lda syswork+5
   jsr vdcRamWrite
   ldx vdcBmCols
   dex
   beq vdcGrOpContinue
   txa
   ldx #$1e
   jsr vdcWrite
vdcGrOpContinue = *
   lda syswork+2+0
   bne +
   dec syswork+2+1
+  dec syswork+2+0
   lda syswork+2+0
   ora syswork+2+1
   bne +
   clc
   rts
+  bit vdcGrOpFlagsIn
   bmi +
   clc
   lda syswork+8+0
   adc syswork+4
   sta syswork+8+0
   bcc +
   inc syswork+8+1
+  bit vdcGrOpFlags
   bvc +
   clc
   lda syswork+6+0
   adc syswork+4
   sta syswork+6+0
   bcc +
   inc syswork+6+1
+  lda vdcGrOpFlags
   and #$08
   beq +
   clc
   lda syswork+10+0
   adc syswork+4
   sta syswork+10+0
   bcc +
   inc syswork+10+1
+  bit vdcGrMode
   bmi +
   lda #<80
   ldy #>80
   jmp +++
+  lda vdcBmFrame
   inc vdcBmFrame
   and #$01
   bne +
   lda #<vdcOddFrame
   ldy #>vdcOddFrame
   jmp ++
   vdcOddToEvenDiff = 65535-vdcOddFrame+1+80
+  lda #<vdcOddToEvenDiff
   ldy #>vdcOddToEvenDiff
+  clc
   adc syswork+0
   sta syswork+0
   tya
   adc syswork+1
   sta syswork+1
   jmp vdcGrOpLoop

;the end + blank line

