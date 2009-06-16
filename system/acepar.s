;ACE-128/64 kernel parallel port driver, started 18-Sep-1994.

;This device driver uses the parallel port network cable described in Issue #8
;of C= Hacking net magazine.  If you don't have such a cable, then don't
;include this device in your device configuration.  This device driver has
;also been extended to work with the geoCable parallel printer cable.

parPA         = $dd00
parData       = $dd01
parDDR        = $dd03
parFlag       = $dd0d
parFlagVal    = $10
parLastOp     .buf 1  ;$00=read, $01=write, $ff=no last op
parReady      .buf 1
parReadCount  .buf 2
parProtocol   .buf 1  ;$00=raw-PC, $40=use-PA2, $80=packet-oriented, $c0=packPA2
parPa2Neutral .buf 1
parPa2Assert  .buf 1

parInit = *
   lda #$00
   sta parReady
   ldx #0
-  lda configBuf+0,x
   cmp #5
   beq +
   txa
   clc
   adc #4
   tax
   cpx #$80
   bcc -
   clc
   rts
   ;** got a live one!
+  lda configBuf+3,x
   sta parProtocol
   lda #$00
   sta parDDR
   lda parDDR-1
   ora #$04
   sta parDDR-1
   sta parReadCount+0
   sta parReadCount+1
   lda #$ff
   sta parLastOp
   sta parReady
   lda parPA
   ora #$04
   sta parPA
   clc
   rts

parGetPa2Parms = *
   lda parPA
   ora #$04
   sta parPa2Neutral
   eor #$04
   sta parPa2Assert
   rts

parOpen = *
   lda openFcb
   clc
   rts

parClose = *
   jsr parGetPa2Parms
   lda parLastOp
   cmp #$01
   bne +
   bit parProtocol
   bpl +
   lda #$e6          ;write EOF packet if we were last writer
   jsr parWriteByte
   lda #$00
   jsr parWriteByte
   lda #$00
   jsr parWriteByte
+  clc
   jmp closeFdEntry

parRead = *
   jsr parGetPa2Parms
   lda parLastOp
   cmp #$01
   bne +
   lda #$00
   sta parDDR
   lda parData
   bit parProtocol
   bvc +
   ldx parPa2Assert
   stx parPA
   ldx parPa2Neutral
   stx parPA
+  lda #$00
   sta parLastOp

   parReadHeader = *
   lda parReadCount+0
   ora parReadCount+1
   bne ++
   bit parProtocol
   bmi +
   lda readMaxLen+0
   ldy readMaxLen+1
   sta parReadCount+0
   sty parReadCount+1
   jmp ++
+  jsr parReadByte
   cmp #$e6
   ;xx should check it here
   jsr parReadByte
   sta parReadCount+0
   jsr parReadByte
   sta parReadCount+1
   ora parReadCount+0
   bne +
   jsr parReadEnd  ;eof
   ldx #$00
   rts
+  lda parReadCount+0
   cmp readMaxLen+0
   lda parReadCount+1
   sbc readMaxLen+1
   bcs +
   lda parReadCount+0
   ldy parReadCount+1
   sta readMaxLen+0
   sty readMaxLen+1
+  sec
   lda parReadCount+0  ;set count for next time
   sbc readMaxLen+0
   sta parReadCount+0
   lda parReadCount+1
   sbc readMaxLen+1
   sta parReadCount+1

   parReadNext = *
   lda readLength+0
   cmp readMaxLen+0
   lda readLength+1
   sbc readMaxLen+1
   bcc +
   parReadEnd = *
   lda readLength+0
   ldy readLength+1
   sta zw+0
   sty zw+1
   clc
   ldx #$ff
   rts
+  ldy #0
   lda #parFlagVal
-  bit parFlag
   beq -
   lda parData
   bit parProtocol
   bvc +
   ldx parPa2Assert
   stx parPA
   ldx parPa2Neutral
   stx parPA
+  sta (readPtr),y
   inc readPtr+0
   bne +
   inc readPtr+1
+  inc readLength+0
   bne +
   inc readLength+1
+  jmp parReadNext

parReadByte = *
   lda #parFlagVal
-  bit parFlag
   beq -
   lda parData
   bit parProtocol
   bvc +
   ldx parPa2Assert
   stx parPA
   ldx parPa2Neutral
   stx parPA
+  rts

parWrite = *
   jsr parGetPa2Parms
   lda parLastOp
   cmp #$01
   beq parWriteHeader
   ldx #$ff
   stx parDDR
   cmp #$00
   bne +
   lda #parFlagVal
-  bit parFlag
   beq -
+  lda #$01
   sta parLastOp

   parWriteHeader = *
   lda writeLength+0
   ora writeLength+1
   bne +
   clc
   rts
+  bit parProtocol
   bpl parWriteNext
   lda #$e6
   jsr parWriteByte
   lda writeLength+0
   jsr parWriteByte
   lda writeLength+1
   jsr parWriteByte

   parWriteNext = *
   lda writeLength+0
   ora writeLength+1
   bne +
   clc
   rts
+  ldy #0
   lda (writePtr),y
   bit parFlag   ;** preventative
   sta parData
   bit parProtocol
   bvc +
   ldx parPa2Assert
   stx parPA
   ldx parPa2Neutral
   stx parPA
+  inc writePtr+0
   bne +
   inc writePtr+1
+  lda writeLength+0
   bne +
   dec writeLength+1
+  dec writeLength+0
   lda #parFlagVal
-  bit parFlag
   beq -
   jmp parWriteNext

parWriteByte = *
   bit parFlag   ;** preventative
   sta parData
   bit parProtocol
   bvc +
   ldx parPa2Assert
   stx parPA
   ldx parPa2Neutral
   stx parPA
+  lda #parFlagVal
-  bit parFlag
   beq -
   rts

;the end + blank line

