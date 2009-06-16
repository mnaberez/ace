;ACE-128/64 kernel SwiftLink serial device driver, started 03-Mar-1995.

;This device driver uses the SwiftLink RS-232 Serial Cartridge, available from
;Creative Micro Designs, Inc, and also supports the extensions of the Turbo232
;Serial Cartridge.  Both devices are based on the 6551 ACIA chip.

; baud rates:           baud rates:          format:          crystal:
; -----------           -----------          -------          ------------
; $00=50                $08=9600             $00=8N1          $00=2x(norm)
; $01=110               $09=19200            $80=7E1          $40=1x
; $02=134.5             $0a=38400
; $03=300               $0b=57600
; $04=600               $0c=115200
; $05=1200              $0d=230400
; $06=2400              $0e=future
; $07=4800              $0f=future

;Note: this device driver is organized so that it will not "touch" the SL
;registers until the first Open call is executed.  This is to prevent bad
;things from happening if the cartridge is "not present" when ACE
;initializes.  Then, the device will be active until ACE shuts down,
;regardless of Open and Close calls.

;Note also: this device driver is rather complicated because of all kinds of
;race conditions and funky hardware mis-features.  Interrupts are used for
;receive interrupts, with a user-defined maximum buffer size, and there are
;no transmit interrups, as data transmission is done with polling.  The
;device driver also implements hardware flow control.  Since the 6551 requires
;that the transmitter be disabled in order to drop the RTS signal (ready to
;accept data from modem), data to be transmitted when the transmitter is off
;is buffered until flow can be restore.  The transmit buffer has a maximum
;capacity of 255 bytes, and if this is exceeded, transmit bytes are
;unceremoneously dropped on the floor.

slDataOff    = $0
slStatusOff  = $1
slCommandOff = $2
slControlOff = $3
slTurboOff   = $7
slReady            .buf 1  ;$00=not initialized, $ff=ready
slOpened           .buf 1
slRecvFlowStopped  .buf 1  ;$00=normal, $ff=RTS flow signal asserted (no Tx)
slSendHead         .buf 1
slSendTail         .buf 1
slSendCount        .buf 1
slOverrunBytes     .buf 4  ;pulled from "leanOverruns"
slBufOverflowBytes = leanDropCnt
slSentBytes        .buf 4
slReceivedBytes    .buf 4
slTurboFlag        .buf 1  ;flag: turbo-232 or regular swiftlink

;;The following variables are imported from the rest of the kernel
;;aceModemSendPage  = aceStatB+105 ;(1)   ;page of send buffer
;;aceModemRecvPage  = aceStatB+106 ;(1)   ;start page of recevie buffer
;;aceModemRecvHigh  = aceStatB+107 ;(1)   ;high page + 1
;;aceModemType      = aceStatB+108 ;(1)   ;$ff=swifty,$40=user-port,$00=none
;;aceModemIoPage    = aceStatB+109 ;(1)   ;page for SwiftLink registers
;;aceModemConfig    = aceStatB+116 ;(1)   ;$0x=baudrate, $80=0:8N1/1:7E1,
                                          ;$40=0:doubleCrystal/1:singleCrystal
.if useC128
   slIoPage    = $b2
   slSendPage  = $b4
.else
   slIoPage    = $b2
   slSendPage  = $b4
.ife

slInit = *
   lda #$00
   sta slReady
   sta slOpened
   sta slTurboFlag
   lda aceModemType
   cmp #$ff
   bne +
   lda #$00
   ldy aceModemSendPage
   sta slSendPage+0
   sty slSendPage+1
   sta slSendHead
   sta slSendTail
   sta slSendCount
   ldx #3
   lda #0
-  sta slOverrunBytes,x
   sta slBufOverflowBytes,x
   sta slSentBytes,x
   sta slReceivedBytes,x
   dex
   bpl -
   lda #$00
   ldy aceModemIoPage
   sta slIoPage+0
   sty slIoPage+1
   jsr slInitLeanReceive
+  clc
   rts

slInitLeanReceive = *
   lda #$ff
   sta leanFreeCnt
   lda #$00
   sta leanHead
   sta leanTail
   sta leanStopped
   sta leanOverruns
   lda #%00000001
   bit aceModemConfig
   bpl +
   lda #%01100001
+  sta leanRtsOff
   lda slIoPage+1
   sta leanPr1
   sta leanPr2
   sta leanPr3
   sta leanPr4

   ldy #slCommandOff
   lda (slIoPage),y
   and #%11110011
   ora #%00001000
   sta (slIoPage),y
   rts

slShutdown = *
   ;** make sure that interrupts are turned off before exiting from ACE
   lda slReady
   bpl +
   ldy #slCommandOff
   lda (slIoPage),y
   and #%11100001
   ora #%00000010
   sta (slIoPage),y
   lda #$00
   sta slReady
+  clc
   rts

slParmsTemp .buf 1

kernModemParms = * ;( .A=baudrate|crystal|format, .X=change ) : .A=baud|c|format
   cpx #$00
   beq +
   ldy aceModemConfig
   sty slParmsTemp
   sta aceModemConfig
   bit slReady
   clc
   bpl +
   jsr slSetParms
   bcc +
   ldy slParmsTemp
   sty aceModemConfig
+  lda aceModemConfig
   rts

slInitHardware = *
   lda #$ff
   sta slReady
   lda #$00
   ldy #slControlOff
   sta (slIoPage),y
   ldy #slTurboOff
   ldx #$00
   lda (slIoPage),y
   beq +
   ldx #$ff
+  stx slTurboFlag
   lda #<nmiLeanSwiftLink  ;should probably save the old vector
   ldy #>nmiLeanSwiftLink
   sta nmiRedirect+0
   sty nmiRedirect+1
   lda #%00011000
   ldy #slControlOff
   sta (slIoPage),y
   lda #%00001001
   ldy #slCommandOff
   sta (slIoPage),y
   ldy #slDataOff
   lda (slIoPage),y
   ;xx fall through

slSetParms = * ;( aceModemConfig ) : .CS=error
   ;** translate baud
   bit slTurboFlag
   bpl +
   lda aceModemConfig
   and #%10111111
   sta aceModemConfig
+  lda aceModemConfig
   and #$0f
   jsr slBaudTranslate
   bcc +
   rts
   ;** control: baud + wordlen
+  ora #%00010000
   bit aceModemConfig
   bpl +
   ora #%00100000
+  ldy #slControlOff
   sta (slIoPage),y
   cpx #$80
   bcc +
   txa
   and #$03
   ldy #slTurboOff
   sta (slIoPage),y
   ;** command: parity
+  ldy #slCommandOff
   lda (slIoPage),y
   and #%00011111
   bit aceModemConfig
   bpl +
   ora #%01100000
+  sta (slIoPage),y
   clc
   rts

slBaudTranslate = * ;( .A=baudCode ) : .CS=notSupported, .A=ctrlBits, .X=turbo
   sta slTemp
   jsr slMaxSoftBaud
   cmp slTemp
   bcs +
   sec
   rts
+  lda slTemp
   and #$0f
   tay
   lda slBaudLookup,y
   bit aceModemConfig
   bvs +
   and #$0f
   jmp ++
+  lsr
   lsr
   lsr
   lsr
+  cmp #$02
   bne +
-  sec
   rts
+  ldx #$00
   cmp #$00
   beq +
-  clc
   rts
+  bit aceModemConfig
   bvs -
   bit slTurboFlag
   bpl --
   cpy #$0b
   bne +
   ldx #$82
   clc
   rts
+  cpy #$0c
   bne +
   ldx #$81
   clc
   rts
+  ldx #$80
   clc
   rts

slTemp .buf 1
slBaudLookup = *
   .byte $12,$32,$42,$65,$76,$87,$a8,$ca,$ec,$fe,$2f,$20,$00,$20,$22,$22
;          0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f

slMaxSoftBaud = * ;( ) : .A=maxbaudCode
   ;** slowMax=57.6k=$0b, fastMax=115.2k=$0c, scpuMax=230.4k=$0d
   bit aceSuperCpuFlag
   bpl ++
   bit $d0b8
   bvs +
   lda #$0d ;scpu 20MHz
   rts
+  lda #$0b ;scpu 1MHz
   rts
+  nop
.if useC64
   lda #$0b ;c64 1MHz
   rts
.ife
   lda $d030
   and #$01
   bne +
   lda #$0b ;c128 1MHz
   rts
+  lda #$0c ;c128 2MHz
   rts

kernModemCheck = * ;( ) : .X=outstSendBytes, .A=sendBytesFree,
   ldx slSendCount      ; .Y=sendFlowStoppedFlag, .Z=oRecvB==0
   stx slTemp
   sec
   lda #0
   sbc slTemp
   sta slTemp
   lda leanFreeCnt
   cmp #$ff
   php
   lda slTemp
   ldy leanStopped
   beq +
   ldy #$ff
+  plp
   clc
   rts

kernModemStat = * ;( .X=zpStatBuf):.A=slStatus,[zSB+0]=ovrChrs,[zSB+4]=bovfChrs,
   lda leanOverruns                ;[zSB+8]=receivedBytes, [zSB+12]=sentBytes
   tay
   cmp slOverrunBytes+0
   bcs +
   inc slOverrunBytes+1
   bne +
   inc slOverrunBytes+2
   bne +
   inc slOverrunBytes+3
+  tya
   sta slOverrunBytes+0
   ldy #0
-  lda slOverrunBytes,y
   sta 0,x
   lda slBufOverflowBytes,y
   sta 4,x
   lda slReceivedBytes,y
   sta 8,x
   lda slSentBytes,y
   sta 12,x
   inx
   iny
   cpy #4
   bcc -
   ldy #slStatusOff
   lda (slIoPage),y
   pha
   ldy #slDataOff
   lda (slIoPage),y  ;bug-recovery hack
   tax
   pla
   clc
   rts

slVic80ReuScrollSave .buf 1  ;xx horrible hack!

slOpen = *
   bit slReady
   bmi +
   jsr slInitHardware
+  lda slOpened
   bne +
   lda winScrollReuWork+3
   sta slVic80ReuScrollSave
   lda #aceMemNull
   sta winScrollReuWork+3
+  php
   sei
   jsr slPollReceive  ;get character received slightly before flow stopped
   inc slOpened
   ldy #slCommandOff
   lda (slIoPage),y
   and #%11110001
   ora #%00001000
   ldx leanStopped
   beq +
   and #%11110011
+  sta (slIoPage),y
   jsr slPollReceive ;make sure we don't receive a char in race condition
   plp
   lda openFcb
   clc
   rts

slPollReceive = *
   ldy #slStatusOff
   lda #$08
   and (slIoPage),y
   beq +
   and (slIoPage),y
   beq +
   ldy #slDataOff
   lda (slIoPage),y
   ldx leanFreeCnt
   beq +
   dec leanFreeCnt
   ldx leanTail
   sta leanBuffer,x
   inc leanTail
+  rts

slCloseWait .buf 1

slClose = *
   lda slOpened
   beq +
   dec slOpened
   bne +
   php
   sei
   ldy #slCommandOff
   lda (slIoPage),y
   and #%11110001
   sta (slIoPage),y
   pha
   ldx #20    ;wait for edge conditions after
-  dex        ;dropping RTS, before disabling receive interrupts
   bne -
   pla
   ora #%00000010
   sta (slIoPage),y
   ;here, we are waiting for the modem to believe the RTS drop.
   ;  The value used is a fudge factor that may have to change
   ;  for different modems.
   lda configBuf+$87
   sta slCloseWait
-  jsr slPollReceive  ;try receiving polled char
   dec slCloseWait
   bne -
   plp
   lda slVic80ReuScrollSave
   sta winScrollReuWork+3
+  clc
   jmp closeFdEntry

slReadBytesLeft .buf 2
slReadChunkLen  .buf 1

slRead = *
   slReadNext = *
   sec
   lda readMaxLen+0
   sbc readLength+0
   sta slReadBytesLeft+0
   lda readMaxLen+1
   sbc readLength+1
   sta slReadBytesLeft+1
   ora slReadBytesLeft+0
   beq slReadEnd
   bcs ++
   slReadEnd = *
   lda slSendCount
   beq +
   jsr slTryToSend
+  lda readLength+0
   ldy readLength+1
   sta zw+0
   sty zw+1
   clc
   ldx #$ff
   rts
+  lda #128
   ldx slReadBytesLeft+1
   bne +
   ldx slReadBytesLeft+0
   cpx #128
   bcs +
   txa
+  jsr slReadBytes
   bcc +
   lda readLength+0
   ora readLength+1
   beq slReadNext    ;** don't return on zero bytes read
   jmp slReadEnd
+  sty slReadChunkLen
   clc
   tya
   adc readPtr+0
   sta readPtr+0
   bcc +
   inc readPtr+1
+  clc
   lda readLength+0
   adc slReadChunkLen
   sta readLength+0
   bcc +
   inc readLength+1
+  clc
   lda slReceivedBytes+0
   adc slReadChunkLen
   sta slReceivedBytes+0
   bcc +
   inc slReceivedBytes+1
   bne +
   inc slReceivedBytes+2
   bne +
   inc slReceivedBytes+3
+  jmp slReadNext

slReadBytes = *  ;( .A=maxBytes, readPtr ) : .Y=actualBytes, .CS=empty
   sta slReadChunkLen
   lda leanFreeCnt
   cmp #$ff
   bne +
   sec
   rts
+  php
   sei
   sec
   lda #$ff
   sbc leanFreeCnt
   cmp slReadChunkLen
   bcs +
   sta slReadChunkLen
+  ldy #0
   ldx leanHead
-  lda leanBuffer,x
   sta (readPtr),y
   inx
   iny
   inc leanFreeCnt
   cpy slReadChunkLen
   bcc -
+  stx leanHead
   ldx leanStopped
   beq +
   ldx leanFreeCnt
   cpx #64
   bcc +
   sty slReadChunkLen
   ldy #slCommandOff
   ldx #$00
   lda (slIoPage),y
   and #%11110011
   ora #%00001000
   stx leanStopped
   sta (slIoPage),y
   ldy slReadChunkLen
+  plp
   clc
   rts

slTryToSend = *
   ;** send as many buffered transmit bytes as possible, until either the
   ;** send buffer is empty or flow control is re-asserted
   lda slSendCount
   bne +
-  rts
+  ldy #slStatusOff
-  lda leanStopped
   bne -
   lda #%00010000
   and (slIoPage),y
   beq -
   ldy slSendHead
   ldx #bkRam0
   stx bkSelect
   lda (slSendPage),y
   ldx #bkACE
   stx bkSelect
   ;** because the 6551 works semi-intelligently, the race condition here
   ;** between detecting that it is okay to send and the possibility of
   ;** flow stoppage being re-asserted in the meantime, is non-critical:
   ;** the 6551 will automatically send the byte when data flow is reactivated
   ldy #slDataOff
   sta (slIoPage),y
   inc slSendHead
   dec slSendCount
   jmp slTryToSend

slWrite = *
   slWriteNext = *
   lda writeLength+0
   ora writeLength+1
   bne +
   clc
   rts
+  ldy #0
   lda (writePtr),y
   jsr slWriteByte
   inc writePtr+0
   bne +
   inc writePtr+1
+  lda writeLength+0
   bne +
   dec writeLength+1
+  dec writeLength+0
   jmp slWriteNext

slWriteByte = *  ;( .A=byte ) : .CS=couldn't do(flow stopped), .A=char
   tax
   ldy #slStatusOff
-  lda leanStopped
   bne slWriteStopped
   lda #%00010000
   and (slIoPage),y
   beq -
   txa
   ldy #slDataOff
   sta (slIoPage),y
   inc slSentBytes+0
   bne +
   inc slSentBytes+1
   bne +
   inc slSentBytes+2
   bne +
   inc slSentBytes+3
+  clc
   rts

   slWriteStopped = *
   txa
   ;** send flow is disabled--put the char into the send buffer
   ;** note that if flow is disabled, it will not be re-enabled until after
   ;** the next "read" call
   ldy slSendCount
   cpy #255
   bcc + 
   ;** send buffer is full--return with error
   sec
   rts
+  ldy slSendTail
   ldx #bkRam0
   stx bkSelect
   sta (slSendPage),y
   ldx #bkACE
   stx bkSelect
   inc slSendTail
   inc slSendCount
   clc
   rts

;the end + blank line

