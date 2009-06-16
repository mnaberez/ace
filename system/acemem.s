;ACE-128/64 kernel Dynamic Memory & Process routines, by Craig Bruce.

;*** memory routines ***

.if computer-64
   comCodeBuffer = $200
.ife
temp1 = $93

bkSelectRam0 = $ff01
reu = $df00
rl = $de00
;rlRegs = $e0a9  ;old
;rlExec = $fe1e  ;old
rlActivate   = $df7e
rlDeactivate = $df7f
rlSram       = $dfc0
rlPageSelect = $dfa0
rlPageActivate = $dfc1
rlPageData   = $de00

;***startup

initMemory = *
.if computer-64
   ldx #0
-  lda comCodeStart,x
   sta comCodeBuffer,x
   inx
   cpx #comCodeEnd-comCodeStart
   bcc -
.ife
   rts

shutdownMemory = *
.if computer-64
   ldx #0
   lda #0
-  sta comCodeBuffer,x
   inx
   cpx #comCodeEnd-comCodeStart
   bne -
.ife
   rts

internBankConfigs = *
.if computer-64
   .byte $3f,$7f,$bf,$ff,$bf,$ff,$bf,$ff
   .byte $3f,$7f,$bf,$ff,$bf,$ff,$bf,$00
.else
   .byte $70,$70
.ife

internBankGroups = *
.if computer-64
   .byte $04,$04,$04,$04,$14,$14,$24,$24
   .byte $04,$04,$04,$04,$14,$14,$24,$04
.ife

;***common code

comCodeStart = *
.if computer-64
.else
   comCodeBuffer = comCodeStart
.ife

comZpLoad = *
   sty temp1
   ldy mp+2
.if computer-64
   lda internBankGroups,y
   sta $d506
.ife
   lda internBankConfigs,y
   sta bkSelect
   ldy #0
-  lda (mp),y
   sta 0,x
   inx
   iny
   cpy temp1
   bcc -
   lda #bkACE
   sta bkSelect
   clc
   rts

comZpStore = *
   sty temp1
   ldy mp+2
.if computer-64
   lda internBankGroups,y
   sta $d506
.ife
   lda internBankConfigs,y
   sta bkSelect
   ldy #0
-  lda 0,x
   sta (mp),y
   inx
   iny
   cpy temp1
   bcc -
   lda #bkACE
   sta bkSelect
   clc
   rts

comCopyToRam0 = *
   ldx mp+2
.if computer-64
   lda internBankGroups,x
   sta $d506
   lda internBankConfigs,x
   tax
.else
   lda internBankConfigs,x
   sta bkSelect
.ife
   dey
   beq +
.if computer-64
-  stx bkSelect
   lda (mp),y
   sta bkSelectRam0
.else
-  lda (mp),y
.ife
   sta (zp),y
   dey
   bne -
.if computer-64
+  stx bkSelect
   lda (mp),y
   sta bkSelectRam0
.else
+  lda (mp),y
.ife
   sta (zp),y
   lda #bkACE
   sta bkSelect
   clc
   rts

comCopyFromRam0 = *
   ldx mp+2
.if computer-64
   lda internBankGroups,x
   sta $d506
   lda internBankConfigs,x
   tax
.else
   lda internBankConfigs,x
   sta bkSelect
.ife
   dey
   beq +
.if computer-64
-  sta bkSelectRam0
   lda (zp),y
   stx bkSelect
.else
-  lda (zp),y
.ife
   sta (mp),y
   dey
   bne -
.if computer-64
+  sta bkSelectRam0
   lda (zp),y
   stx bkSelect
.else
+  lda (zp),y
.ife
   sta (mp),y
   lda #bkACE
   sta bkSelect
   clc
   rts
comCodeEnd = *

;*** aceMemZpload( [mp]=Source, .X=ZpDest, .Y=Length ) : .CS=err

kernMemZpload = *
   lda mp+3
   beq nullPtrError
   cmp #aceMemRL
   bcc +
   beq zpLoadRL
   jmp comZpLoad-comCodeStart+comCodeBuffer
+  tya
   ldy #$91

zeroPageReuOp = *
   bit aceRamlinkAccess
   bpl +
   php
   sei
   sta rlActivate
+  sta reu+7
   lda mp+2
   sta reu+6
   stx reu+2
   lda #0
   sta reu+3
   sta reu+8
   lda mp+0
   sta reu+4
   lda mp+1
   sta reu+5
.if useFastClock
   lda vic+$30
   ldx #$00
   stx vic+$30
.ife
   sty reu+1
.if useFastClock
   sta vic+$30
.ife
   bit aceRamlinkAccess
   bpl +
   sta rlSram
   sta rlDeactivate
   plp
+  clc
   rts

nullPtrError = *
   lda #aceErrNullPointer
   sta errno
   sec
   rts

zpLoadRL = *
   lda #$91
zeroPageRlOp = *  ;( .A=opcode, .X=zpaddr, .Y=len, [mp]=farPtr )
   sta ramlinkOpcode
   lda #0
   sty ramlinkLength+0
   sta ramlinkLength+1
   stx ramlinkNearPtr+0
   sta ramlinkNearPtr+1
   jmp ramlinkOp

ramlinkOpcode .buf 1
ramlinkLength .buf 2
ramlinkNearPtr .buf 2
ramlinkMpSave .buf 3
ramlinkZpSave .buf 2

ramlinkOp = *  ;( [mp]=farPtr, ramlinkNearPtr, ramlinkLength, ramlinkOpcode )
  ;jmp ramlinkOpOld
   lda mp+0
   ldy mp+1
   ldx mp+2
   sta ramlinkMpSave+0
   sty ramlinkMpSave+1
   stx ramlinkMpSave+2
   lda zp+0
   ldy zp+1
   sta ramlinkZpSave+0
   sty ramlinkZpSave+1
   lda ramlinkNearPtr+0
   ldy ramlinkNearPtr+1
   sta zp+0
   sty zp+1
   clc
   lda mp+1
   adc aceRamlinkStart+0
   sta mp+1
   lda mp+2
   adc aceRamlinkStart+1
   sta mp+2
-  lda ramlinkLength+0
   ora ramlinkLength+1
   beq +
   jsr rlTransferChunk
   jmp -
+  lda ramlinkMpSave+0
   ldy ramlinkMpSave+1
   ldx ramlinkMpSave+2
   sta mp+0
   sty mp+1
   stx mp+2
   lda ramlinkZpSave+0
   ldy ramlinkZpSave+1
   sta zp+0
   sty zp+1
   clc
   rts

   rlTrSize .buf 1

   rlTransferChunk = *  ;( [mp]=rlmem, (zp)=nearmem, rlLength, rlOpcode )
   ;** figure maximum page operation
   lda ramlinkLength+1
   beq +
   lda #0
   ldx mp+0
   beq rlTrDo
   sec
   sbc mp+0
   jmp rlTrDo
+  lda mp+0
   beq +
   lda #0
   sec
   sbc mp+0
   cmp ramlinkLength+0
   bcc rlTrDo
+  lda ramlinkLength+0

   ;** do the transfer
   rlTrDo = *
   tay
   sty rlTrSize
   jsr rlPageOp

   ;** update the pointers and remaining length
   clc
   lda rlTrSize
   bne +
   inc mp+1
   inc zp+1
   dec ramlinkLength+1
   rts
+  adc mp+0
   sta mp+0
   bcc +
   inc mp+1
+  clc
   lda zp+0
   adc rlTrSize
   sta zp+0
   bcc +
   inc zp+1
+  sec
   lda ramlinkLength+0
   sbc rlTrSize
   sta ramlinkLength+0
   bcs +
   dec ramlinkLength+1
+  rts

   rlPageOp = *  ;( [mp]=rlmem, (zp)=nearmem, .Y=bytes, ramlinkOpcode )
   php
   sei
   sta rlActivate
   lda mp+1
   sta rlPageSelect+0
   lda mp+2
   sta rlPageSelect+1
   sta rlPageActivate
   lda aceReuRlSpeedPage+3
   bne rlPageOpReu  ;xxx dependency on aceMemNull==0
   rlPageOpNonReu = *
   tya
   clc
   adc mp+0
   tax

   lda ramlinkOpcode
   cmp #$91
   bne rlPageOpWrite
   dex
   dey
   beq +
-  lda rlPageData,x
   sta (zp),y
   dex
   dey
   bne -
+  lda rlPageData,x
   sta (zp),y
   jmp rlPageOpContinue

   rlPageOpWrite = *
   dex
   dey
   beq +
-  lda (zp),y
   sta rlPageData,x
   dex
   dey
   bne -
+  lda (zp),y
   sta rlPageData,x

   rlPageOpContinue = *
   sta rlSram
   sta rlDeactivate
   plp
   rts

   rlPageOpReu = * ;( [mp]=rlmem, (zp)=nearmem, .Y=bytes, ramlinkOpcode )
   ;** ramlink hardware already switched in
   ldx #1
   tya
   beq +
   ldx #0
   cmp #0  ;xx cut-off value
   bcc rlPageOpNonReu
+  ldy ramlinkOpcode
   cpy #$90
   beq +
   ldy #$90            ;rl->reu->intern
   jsr rlPageOpReuRl
   ldy #$91
   jsr rlPageOpReuIntern
   jmp ++
+  ldy #$90            ;intern->reu->rl
   jsr rlPageOpReuIntern
   ldy #$91
   jsr rlPageOpReuRl
+  sta rlSram
   sta rlDeactivate
   plp
   rts

   rlPageOpReuIntern = *  ;( .AX=bytes, .Y=op )
   sta reu+7  ;len
   stx reu+8
   sty temp1
   pha
   lda zp+0
   ldy zp+1
   sta reu+2
   sty reu+3
   lda aceReuRlSpeedPage+0
   ldy aceReuRlSpeedPage+1
   sta reu+4
   sty reu+5
   lda aceReuRlSpeedPage+2
   sta reu+6
.if useFastClock
   ldy vic+$30
   lda #0
   sta vic+$30
.ife
   lda temp1
   sta reu+1
.if useFastClock
   sty vic+$30
.ife
   pla
   rts

   rlPageOpReuRl = *  ;( .AX=bytes, .Y=op )
   sta reu+7  ;len
   stx reu+8
   sty temp1
   pha
   lda mp+0
   ldy #>rlPageData
   sta reu+2
   sty reu+3
   lda aceReuRlSpeedPage+0
   ldy aceReuRlSpeedPage+1
   sta reu+4
   sty reu+5
   lda aceReuRlSpeedPage+2
   sta reu+6
.if useFastClock
   ldy vic+$30
   lda #0
   sta vic+$30
.ife
   lda temp1
   sta reu+1
.if useFastClock
   sty vic+$30
.ife
   pla
   rts

;ramlinkOpOld = *  ;( [mp]=farPtr, ramlinkNearPtr, ramlinkLength, ramlinkOpcode)
;   jsr rlRegs
;   lda ramlinkOpcode
;   sta rl+1
;   lda ramlinkNearPtr+0
;   ldy ramlinkNearPtr+1
;   sta rl+2
;   sty rl+3
;   ldx #0
;   lda mp+3
;   cmp #aceMemRL
;   beq +
;   ldx #4
;+  lda mp+0
;   sta rl+4
;   clc
;   lda aceRamlinkStart+0,x
;   adc mp+1
;   sta rl+5
;   lda aceRamlinkStart+1,x
;   adc mp+2
;clc
;adc #8
;   sta rl+6
;   lda ramlinkLength+0
;   ldy ramlinkLength+1
;   sta rl+7
;   sty rl+8
;   lda #0
;   sta rl+10
;   sta rl+16
;   jsr rlExec
;   clc
;   rts

;*** aceMemZpstore( .X=ZpSource, [mp]=Dest, .Y=Length ) : .CS=err

kernMemZpstore = *
zpstore = *   ;;internally called operation
   lda mp+3
   bne +
   jmp nullPtrError
+  cmp #aceMemRL
   bcc +
   beq zpStoreRL
   jmp comZpStore-comCodeStart+comCodeBuffer
+  tya
   ldy #$90
   jmp zeroPageReuOp

zpStoreRL = *
   lda #$90
   jmp zeroPageRlOp

;*** aceMemFetch( [mp]=FarSource, (zp)=Ram0Dest, .AY=Length )

fetchLength     .buf 2
fetchSaveSource .buf 1
fetchSaveDest   .buf 1

kernMemFetch = *
fetch = *    ;;internally called operation
   ldx mp+3
   beq fetchNullPtrError
   cpx #aceMemRL
   bcs +
   ldx #$91
   jmp doReu
+  beq fetchRL
   cpy #0
   bne fetchLong
   tay
   bne fetchPage
   clc
   rts

   fetchNullPtrError = *
   jmp nullPtrError

   fetchPage = *  ;( [mp]=from, (zp)=to, .Y=len(0=256) )
   ldx mp+2
   cpx #0
   beq +
   ;xx don't have to worry about the 64 having more than one bank yet
   jmp comCopyToRam0-comCodeStart+comCodeBuffer
.if computer-64
+  stx bkSelectRam0
.else
+  ldx #bkRam0
   stx bkSelect
.ife
   dey
   beq +
-  lda (mp),y
   sta (zp),y
   dey
   bne -
+  lda (mp),y
   sta (zp),y
   lda #bkACE
   sta bkSelect
   clc
   rts

   fetchLong = *
   sta fetchLength
   sty fetchLength+1
   lda mp+1
   sta fetchSaveSource
   lda zp+1
   sta fetchSaveDest
   lda fetchLength+1
   beq fetchLongExit
-  ldy #0
   jsr fetchPage
   inc mp+1
   inc zp+1
   dec fetchLength+1
   bne -

   fetchLongExit = *
   ldy fetchLength
   beq +
   jsr fetchPage
+  lda fetchSaveSource
   sta mp+1
   lda fetchSaveDest
   sta zp+1
   clc
   rts

fetchRL = *
   ldx #$91
largeRlOp = *
   stx ramlinkOpcode
   sta ramlinkLength+0
   sty ramlinkLength+1
   lda zp+0
   ldy zp+1
   sta ramlinkNearPtr+0
   sty ramlinkNearPtr+1
   jmp ramlinkOp

;*** aceMemStash( (zp)=Ram0Source, [mp]=FarDest, .AY=length )

stashLength     .buf 2
stashSaveSource .buf 1
stashSaveDest   .buf 1

kernMemStash = *
stash = *        ;;internally called operation
   ldx mp+3
   beq stashNullPtrError
   cpx #aceMemRL
   bcs +
   ldx #$90
   jmp doReu
+  beq stashRL
   cpy #0
   bne stashLong
   tay
   bne stashPage
   clc
   rts

   stashNullPtrError = *
   jmp nullPtrError

   stashPage = *
   ldx mp+2
   cpx #0
   beq +
   ;xx don't have to worry about the 64 having more than one bank yet
   jmp comCopyFromRam0-comCodeStart+comCodeBuffer
.if computer-64
+  stx bkSelectRam0
.else
+  ldx #bkRam0
   stx bkSelect
.ife
   dey
   beq +
-  lda (zp),y
   sta (mp),y
   dey
   bne -
+  lda (zp),y
   sta (mp),y
   lda #bkACE
   sta bkSelect
   clc
   rts

   stashLong = *
   sta stashLength
   sty stashLength+1
   lda zp+1
   sta stashSaveSource
   lda mp+1
   sta stashSaveDest
   lda stashLength+1
   beq stashLongExit
-  ldy #0
   jsr stashPage
   inc mp+1
   inc zp+1
   dec stashLength+1
   bne -

   stashLongExit = *
   ldy stashLength
   beq +
   ldx mp+2
   jsr stashPage
+  lda stashSaveSource
   sta zp+1
   lda stashSaveDest
   sta mp+1
   clc
   rts

stashRL = *
   ldx #$90
   jmp largeRlOp

;*** ram0 load/store(.X) expansion memory [mp] <- -> (zp) for .AY bytes

doReu = *
   bit aceRamlinkAccess
   bpl +
   php
   sei
   sta rlActivate
+  sta reu+7
   sty reu+8
   lda zp+0
   ldy zp+1
   sta reu+2
   sty reu+3
   lda mp+0
   ldy mp+1
   sta reu+4
   sty reu+5
   lda mp+2
   sta reu+6
.if useFastClock
   ldy vic+$30
   lda #0
   sta vic+$30
.ife
   stx reu+1
.if useFastClock
   sty vic+$30
.ife
   bit aceRamlinkAccess
   bpl +
   sta rlSram
   sta rlDeactivate
   plp
+  clc
   rts

;*** memory-allocation routines

freemapBank     .buf 2
freemapDirty    .buf 1
freemapPage     .buf 1
searchMinFail   .buf aceMemTypes

initMemoryAlloc = *
   ldx #0
   ldy #0
   stx freemapPage
   stx freemapDirty
-  lda ram0FreeMap,x
   sta freemap,x
   bne +
   iny
+  inx
   bne -
   lda #0
   ldy #aceMemInternal
   sta freemapBank+0
   sty freemapBank+1
   lda #$00
   ldx #0
-  sta searchMinFail,x
   inx
   cpx #aceMemTypes
   bcc -
   jsr memReuRlSpeedInit
   clc
   rts

memReuRlSpeedInit = *  ;allocate reu page for ramlink-accessing speedup
   bit aceReuRlSpeedTry
   bmi +
   rts
+  lda #$ff  ;owned by malloc
   sta allocProcID
   lda #1
   ldx #aceMemREU
   ldy #aceMemREU
   jsr kernPageAlloc
   ldx #3
-  lda mp,x
   sta aceReuRlSpeedPage,x
   dex
   bpl -
   rts

freemapBankSave .buf 2

getFreemap = *  ;( .AY=bank )
   cmp freemapBank+0
   bne +
   cpy freemapBank+1
   bne +
   rts

   ;** save old freemap
+  sta freemapBankSave+0
   sty freemapBankSave+1
   lda freemapDirty
   beq +
   lda freemapBank+0
   ldy freemapBank+1
   jsr locateBankFreemap
   jsr setZpFreemap
   jsr stash

   ;** load new freemap
+  lda freemapBankSave+0
   ldy freemapBankSave+1
   sta freemapBank+0
   sty freemapBank+1
   jsr locateBankFreemap
   jsr setZpFreemap
   jsr fetch
   lda #0
   sta freemapDirty
   sta freemapPage
   rts

   setZpFreemap = *  ;() : (zp)=#freemap, .AY=#256
   lda #<freemap
   ldy #>freemap
   sta zp+0
   sty zp+1
   lda #<256
   ldy #>256
   rts

locateBankFreemap = *  ;( .AY=bank ) : [mp]
   sta mp+2
   sty mp+3
   lda #<$ff00
   ldx #>$ff00
   sta mp+0
   stx mp+1
   cpy #aceMemInternal
   beq +
   rts
+  lda mp+2
   bne +
   ;** ram0
   lda aceRam0Freemap+0
   ldy aceRam0Freemap+1
-  sta mp+0
   sty mp+1
   rts
+  cmp #1
   bne +
   ;** ram1
   lda #0
   ldy aceRam1Freemap
   jmp -
   ;** exp.int.ram
+  lda #<$0400
   ldy #>$0400
   jmp -

searchTypeStart .buf 1
searchTypeStop  .buf 1
searchSize      .buf 1
allocProcID     .buf 1
searchTypeJmp   .word 0,pageAllocREU,pageAllocRL,pageAllocInternal,0,0,0,0
realMemTypes    .byte aceMemNull,aceMemREU,aceMemRL,aceMemInternal
                .byte aceMemREU,aceMemRL,aceMemInternal

;kernel procids for pages: $00=free,$01=kernel,$ff=malloc,$fe=tpa,$fd=ramdisk,
;                          $fc=devices,$fb=reservedRamdisk

kernMemAlloc = *  ;( .A=pages, .X=stType, .Y=endType ) : [mp]=farPtr
   pha
   lda aceProcessID
   sta allocProcID
   pla
kernPageAlloc = *
   sta searchSize
   cmp #0
   bne +
   jsr pageAllocFail
   clc
   rts
+  cpx #aceMemREU
   bcs +
   ldx #aceMemREU
+  cpy #aceMemInternal
   beq +
   bcc +
   ldy #aceMemInternal
+  stx searchTypeStart
   sty searchTypeStop
-  lda searchTypeStart
   cmp searchTypeStop
   beq +
   bcs pageAllocFail
+  ldx searchTypeStart
   lda searchMinFail,x
   beq +
   cmp searchSize
   beq pageAllocNext
   bcc pageAllocNext
+  lda searchTypeStart
   asl
   tax
   lda searchTypeJmp+0,x
   sta mp+0
   lda searchTypeJmp+1,x
   beq pageAllocNext
   sta mp+1
   jsr pageAllocDispatch
   bcc ++
   ldx searchTypeStart
   lda searchMinFail,x
   beq +
   cmp searchSize
   bcc pageAllocNext
+  lda searchSize
   sta searchMinFail,x

   pageAllocNext = *
   inc searchTypeStart
   jmp -
+  ldx mp+3
   lda mp+2
   cmp minUsedBank,x
   bcs +
   sta minUsedBank,x
+  cmp maxUsedBank,x
   bcc +
   sta maxUsedBank,x
+  clc
   rts

   pageAllocDispatch = *
   jmp (mp)

   pageAllocFail = *
   lda #aceErrInsufficientMemory
   sta errno
   lda #$00
   sta mp+0
   sta mp+1
   sta mp+2
   sta mp+3
   sec
   rts

pageAllocREU = *  ;( ) : .X=page, freemapBank, .CC=ok
   lda #aceMemREU
   sta mp+3
   lda aceReuCur
   ldx aceReuStart
   ldy aceReuBanks
   jsr searchType
   sta aceReuCur
   rts

pageAllocInternal = *
   lda #aceMemInternal
   sta mp+3
   lda aceInternalCur
   ldx #$00
   ldy aceInternalBanks
   jsr searchType
   sta aceInternalCur
   rts

pageAllocRL = *
   lda #aceMemRL
   sta mp+3
   lda aceRamlinkCur
   ldx #$00
   ldy aceRamlinkBanks
   jsr searchType
   sta aceRamlinkCur
   rts

searchCurrent .buf 1
searchStart   .buf 1
searchStop    .buf 1

searchType = *  ;( mp+3=type, .A=current, .X=start, .Y=stop ):[mp],.CC,.A=cur,.X
   sta searchCurrent
   sta mp+2
   stx searchStart
   sty searchStop
   cpx searchStop
   bcc +
   rts
/  lda mp+2
   ldy mp+3
   jsr getFreemap
   ldy searchSize
   jsr searchFreemap
   bcs +
   lda #0
   sta mp+0
   stx mp+1
   lda mp+2
   clc
   rts
+  inc mp+2
   lda mp+2
   cmp searchStop
   bcc +
   lda searchStart
   sta mp+2
+  lda mp+2
   cmp searchCurrent
   bne -
   sec
   rts

searchPages .buf 1
newmax      .buf 1

searchFreemap = *  ;( .Y=pages ) : .CC=found, .X=firstPg
   ;** first free
   ldx freemapPage
   lda freemap,x
   beq +
-  inx
   beq freemapFull
   lda freemap,x
   bne -
   stx freemapPage
   jmp +
   freemapFull = *
   sec
   rts

   ;** search
+  sty searchPages
   cpx #0
   beq +
   dex
-  ldy searchPages
-  inx
   beq freemapFull
+  lda freemap,x
   bne --
   dey
   bne -

   ;** allocate
   stx newmax
   ldy searchPages
   lda allocProcID
-  sta freemap,x
   dex
   dey
   bne -
   inx
   cpx freemapPage
   bne +
   ldy newmax
   iny
   sty freemapPage
+  lda #$ff
   sta freemapDirty
   sec
   lda aceFreeMemory+1
   sbc searchPages
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   bcs +
   dec aceFreeMemory+3
+  clc
   rts

freePage .buf 1
freeLen  .buf 1

kernMemFree = *  ;( [mp]=FarPtr, .A=pages )
   ldx aceProcessID
   stx allocProcID
kernPageFree = *
   sta freeLen
   cmp #0
   bne +
   jmp pageFreeExit
+  lda mp+3
   cmp #aceMemNull
   bne +
   lda #aceErrNullPointer
   jmp pageFreeFail
+  lda #aceErrInvalidFreeParms
   ldx mp+0
   bne pageFreeFail
   lda mp+1
   sta freePage
   clc
   adc freeLen
   bcc +
   lda #aceErrInvalidFreeParms
   jmp pageFreeFail
+  lda mp+2
   ldy mp+3
   jsr getFreemap
   lda allocProcID
   ldx freePage
   ldy freeLen
-  cmp freemap,x
   beq +
   lda #aceErrFreeNotOwned
   jmp pageFreeFail
+  inx
   dey
   bne -
   ldx freePage
   ldy freeLen
   lda #$00
-  sta freemap,x
   inx
   dey
   bne -
   lda #$ff
   sta freemapDirty
   lda freePage
   cmp freemapPage
   bcs +
   sta freemapPage
   ;** assume 2*(min-1)+len+1 new min
+  ldx mp+3
   lda searchMinFail,x
   beq ++
   sec
   sbc #1
   asl
   bcs +
   sec
   adc freeLen
   bcc ++
+  lda #0
+  sta searchMinFail,x

   clc
   lda aceFreeMemory+1
   adc freeLen
   sta aceFreeMemory+1
   bcc pageFreeExit
   inc aceFreeMemory+2
   bne pageFreeExit
   inc aceFreeMemory+3

   pageFreeExit = *
   clc
   rts

   pageFreeFail = *
   sta errno
   sec
   rts

kernMemStat = *
   ldy #0
-  lda aceFreeMemory,y
   sta 0,x
   lda aceTotalMemory,y
   sta 4,x
   inx
   iny
   cpy #4
   bcc -
   lda aceProcessID
   clc
   rts

reclaimMemType .buf 1

reclaimProcMemory = *
   ldx #0
-  lda minUsedBank,x
   cmp maxUsedBank,x
   beq +
   bcs ++
+  stx reclaimMemType
   lda minUsedBank,x
   ldy maxUsedBank,x
   tax
   lda reclaimMemType
   jsr reclaimProcType
   ldx reclaimMemType
+  inx
   cpx #aceMemTypes
   bcc -
   rts

rpBank  .buf 2
rpEnd   .buf 1

reclaimProcType = *  ;( .A=type, .X=startBank, .Y=endBank )
   stx rpBank+0
   sta rpBank+1
   sty rpEnd
-  lda rpBank+0
   ldy rpBank+1
   cmp rpEnd
   beq +
   bcs ++
+  jsr getFreemap
   jsr reclaimProcFreemap
   inc rpBank+0
   bne -
+  rts

reclaimProcFreemap = *  ;( ) : .Y=pagesRemoved
   ldy #0
   ldx #0
   lda aceProcessID
   jmp +
-  inx
   beq ++
+  cmp freemap,x
   bne -
   lda #0
   sta freemap,x
   iny
   lda aceProcessID
   jmp -
+  cpy #0
   beq +
   lda #0
   sta freemapPage
   ldx freemapBank+1
   sta searchMinFail,x
   lda #$ff
   sta freemapDirty
   tya
   clc
   adc aceFreeMemory+1
   sta aceFreeMemory+1
   bcc +
   inc aceFreeMemory+2
   bne +
   inc aceFreeMemory+3
+  clc
   rts

minUsedBank .buf aceMemTypes
maxUsedBank .buf aceMemTypes  ;plus 1

clearMemoryInfo = *
   ldx #aceMemTypes-1
-  lda #$ff
   sta minUsedBank,x
   lda #$00
   sta maxUsedBank,x
   dex
   bpl -
   rts

;*** process primitives

reclaimSave .buf 1

reclaimOpenFiles = *
   jsr kernelClrchn
   ldx #0
-  lda lftable,x
   cmp #lfnull
   beq +
   lda pidtable,x
   cmp aceProcessID
   bne +
   stx reclaimSave
   txa
   jsr close
   ldx reclaimSave
+  inx
   cpx #fcbCount
   bcc -
   rts

execArgc      .buf 2
execFrame     .buf 44+4
execStackNeed .buf 1
execAddr      .buf 2
execWork      .buf 2
execErrExit   .buf 1
reloadFlag    .buf 1

kernProcExec = *
internProcExec = *
   sta execArgc+0
   sty execArgc+1
   ;** load executable
   lda #<aceAppAddress
   ldy #>aceAppAddress
   sta execAddr+0
   sty execAddr+1
   jsr execLoadExternal
   bcc ++
   lda errno
   cmp #aceErrFileNotFound
   beq +
   pha
   jsr execReloadProg
   pla
   sta errno
+  sec
   rts
+  ldy #$ff
-  iny
   lda (zp),y
   sta stringBuffer,y
   bne -
   lda #$80
   sta reloadFlag
   jmp execCommon

kernProcExecSub = *
   sta execArgc+0
   sty execArgc+1
   lda #0  ;null reload name means execsub
   sta stringBuffer+0
   lda #10
   sta execStackNeed
   lda zp+0
   ldy zp+1
   sta execAddr+0
   sty execAddr+1
   lda #$00
   sta reloadFlag

   execCommon = *
   ;** put in filename
   ldx #$ff
-  inx
   lda stringBuffer,x
   bne -
   inx
   stx syswork+0
   sec
   lda zw+0
   sbc syswork+0
   sta syswork+0
   lda zw+1
   sbc #0
   sta syswork+1
   ldy #0
-  lda stringBuffer,y
   sta (syswork),y
   beq +
   iny
   bne -

   ;** set up new frame info
+  ldx #1
-  lda aceFramePtr,x
   sta execFrame+0,x
   lda aceArgc,x
   sta execFrame+2,x
   lda aceArgv,x
   sta execFrame+4,x
   dex
   bpl -
   ldx #3
-  lda mp,x
   sta execFrame+14,x
   lda #0
   sta execFrame+18,x
   lda #aceMemNull
   sta execFrame+24,x
   dex
   bpl -
   tsx
   stx execFrame+22
   lda reloadFlag
   sta execFrame+23
   ldx #7
-  lda minUsedBank,x
   sta execFrame+28,x
   lda maxUsedBank,x
   sta execFrame+36,x
   dex
   bpl -

   ;** store new frame info
   sec
   lda syswork+0
   sbc #44
   sta syswork+0
   bcs +
   dec syswork+1
+  ldy #43
-  lda execFrame,y
   sta (syswork),y
   dey
   bpl -

   ;** set up globals for new process
   ldx #1
-  lda syswork+0,x
   sta aceFramePtr,x
   sta aceMemTop,x
   lda execArgc,x
   sta aceArgc,x
   lda zw,x
   sta aceArgv,x
   dex
   bpl -
   jsr clearMemoryInfo

   ;** call the new program
   inc aceProcessID
   lda execAddr+0
   ldy execAddr+1
   sta zp+0
   sty zp+1
   lda #0
   tax
   tay
   pha
   plp
   jsr aceEnter
   lda #0
   ldx #0
   jmp internExit

   aceEnter = *
   jmp (zp)

exitCodeSave .buf 2
exitArgc     .buf 2
exitArgv     .buf 2

kernProcExit = *
internExit = *
   sta exitCodeSave+0
   stx exitCodeSave+1
   lda aceFramePtr+0
   ldy aceFramePtr+1
   sta syswork+0
   sty syswork+1
   ldy #43
-  lda (syswork),y
   sta execFrame,y
   dey
   bpl -
   ldx execFrame+22
   txs
   ldx #1
-  lda aceArgc,x
   sta exitArgc,x
   lda aceArgv,x
   sta exitArgv,x
   lda execFrame+2,x
   sta aceArgc,x
   lda execFrame+4,x
   sta aceArgv,x
   lda execFrame+0,x
   sta aceMemTop,x
   sta aceFramePtr,x
   dex
   bpl -
   lda execFrame+23
   sta reloadFlag

   jsr reclaimOpenFiles
   jsr reclaimProcMemory
   dec aceProcessID
   ldx #7
-  lda execFrame+28,x
   sta minUsedBank,x
   lda execFrame+36,x
   sta maxUsedBank,x
   dex
   bpl -

   ;** reload previous program if necessary
   ;xx note: currently, a process that was "execsub"ed cannot "exec" another
   ;xx process or else the "execsub"ed process will not be reloaded, since I
   ;xx only check the reactivated frame and I don't go all the way back up
   ;xx looking for a program to reload
   bit reloadFlag
   bpl +
   jsr execReloadProg
+  nop

   ;** prepare exit parameters
   ldx #3
-  lda execFrame+14,x
   sta mp,x
   dex
   bpl -
   ldx #1
-  lda exitArgc,x
   sta zp,x
   lda exitArgv,x
   sta zw,x
   dex
   bpl -
   lda exitCodeSave+0
   ldx exitCodeSave+1
   ldy #0
   clc
   rts

execReloadProg = *
   lda aceFramePtr+0
   ldy aceFramePtr+1
   clc
   adc #44
   bcc +
   iny
+  sta zp+0
   sty zp+1
   lda zp+1
   cmp aceStackTop+1
   bcs +
   ldy #0
   lda (zp),y
   beq +
   lda aceFramePtr+0
   ldy aceFramePtr+1
   sta zw+0
   sty zw+1
   jsr execLoadExternal
   bcc +
   lda #<execReloadErrMsg
   ldy #>execReloadErrMsg
   sta zp+0
   sty zp+1
   lda #<execReloadErrMsgEnd-execReloadErrMsg
   ldy #>execReloadErrMsgEnd-execReloadErrMsg
   ldx #stderr
   jsr write
   lda #255
   ldx #0
   jmp internExit
+  clc
   rts

execReloadErrMsg = *
   .asc "acekernel: Error attempting to reload program"
   .byte chrCR
execReloadErrMsgEnd = *

;** load external file into transient program area
loadPathPos .buf 1
loadGiveUp  .buf 1
loadZpSave  .buf 2

execLoadExternal = * ;( (zp)=given program name, (zw)=high load address ) : (zp)
   lda #0
   sta aceAppAddress+3
   lda #0
   sta loadPathPos
   sta loadGiveUp
   lda zp+0
   ldy zp+1
   sta loadZpSave+0
   sty loadZpSave+1

   execTryLoadAgain = *
   ldy loadPathPos
   lda configBuf+$140,y
   beq execCmdNotFound
   lda loadGiveUp
   bne execCmdNotFound
   jsr getloadRestoreZp
   jsr getLoadPathname
   lda #<aceAppAddress
   ldy #>aceAppAddress
   jsr internBload
   jsr getloadRestoreZp
   bcs execLoadError
   lda aceAppAddress+3
   cmp #aceID1
   bne execBadProg
   lda aceAppAddress+4
   cmp #aceID2
   bne execBadProg
   lda aceAppAddress+5
   cmp #aceID3
   bne execBadProg
   clc
   rts

   execLoadError = *
   lda errno
   cmp #aceErrFileNotFound
   beq execTryLoadAgain
   cmp #aceErrDeviceNotPresent
   beq execTryLoadAgain
   cmp #aceErrIllegalDevice
   beq execTryLoadAgain
   cmp #aceErrDiskOnlyOperation
   beq execTryLoadAgain
   sec
   rts

   execBadProg = *
   lda #aceErrBadProgFormat
   sta errno
   sec
   rts

   execCmdNotFound = *
   lda #aceErrFileNotFound
   sta errno
   sec
   rts

getloadRestoreZp = *
   lda loadZpSave+0
   ldy loadZpSave+1
   sta zp+0
   sty zp+1
   rts

getLoadPathname = *  ;( (zp)=filename, aceShellPath, loadPathPos ) : (zp)=lname
   ldy loadPathPos
   ldx #0
-  lda configBuf+$140,y
   beq +
   sta stringBuffer,x
   iny
   inx
   bne -
+  iny
   sty loadPathPos
   ldy #1
   lda (zp),y
   cmp #":"
   beq +
   dey
   lda (zp),y
   cmp #"/"
   bne getPathReally
+  sta loadGiveUp
   ldx #0

   getPathReally = *
   ldy #0
-  lda (zp),y
   sta stringBuffer,x
   beq +
   inx
   iny
   bne -
+  lda #<stringBuffer
   ldy #>stringBuffer
   sta zp+0
   sty zp+1
   rts

;blank line at end

