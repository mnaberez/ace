;*** Configuration Program ***

.seq "acehead.s"
.seq "kernhead.s"
.org aceAppAddress
.obj "@0:config"

jmp main
.byte "c","F",aceID3
.byte 64,0

kernelSetlfs  = $ffba
kernelSetnam  = $ffbd
kernelOpen    = $ffc0
kernelClose   = $ffc3
kernelChkin   = $ffc6
kernelChkout  = $ffc9
kernelClrchn  = $ffcc
kernelChrin   = $ffcf
kernelLoad    = $ffd5
kernelSwapper = $ff5f
st = $90
totalBanks .buf 2
chrQuote = 34

configBuf    = 2   ;(2)
sysName      = 4   ;(2)
ram0FreeMap  = 6   ;(2)
aceEndPage   = 8   ;(1)
sysType      = 9   ;(1)
charset4bitP = 10  ;(2)
keymapAddr   = 12  ;(2)
scrDrivers   = 14  ;(1)
memRead      = 16  ;(2)
memWrite     = 18  ;(2)
banks        = 20  ;(1)
bankLimit    = 21  ;(1)
save0        = 22  ;(2)
save2        = 24  ;(2)
saveN        = 26  ;(2)
cpfrom       = $60 ;(2)
cpto         = $62 ;(2)
indirectReuBanks = $64 ;(1)
loadCharsetNameP = $66 ;(2)
work             = $68 ;(8)
titlePtr     = $70 ;(2)
keymapName   = $72 ;(2)

main = *
   lda #0
   sta aceTotalMemory+0
   sta aceTotalMemory+1
   sta totalBanks+0
   sta totalBanks+1
   sta aceReuRlSpeedTry
   lda #aceMemNull
   sta aceReuRlSpeedPage+3
   lda #<title
   ldy #>title
   sta titlePtr+0
   sty titlePtr+1
   jsr loadConfig
   bcs +
   jsr loadCharset
   bcs +
   jsr loadKeymap
   bcs +
   jsr screenInit
   jsr setDate
   jsr displayDate
   jsr getRamlinkParms
   jsr internalMemory
   jsr reuMemory
   jsr rlMemory
   lda totalBanks+0
   ldy totalBanks+1
   sta aceTotalMemory+2
   sty aceTotalMemory+3
   jsr totalMemory
   ldx #3
-  lda aceFreeMemory,x
   sta aceTotalMemory,x
   dex
   bpl -
   clc
   lda #<title
   ldy #>title
   sta 2
   sty 3
   sec
   lda titlePtr+0
   sbc #<title
   sta 4
   lda titlePtr+1
   sbc #>title
   sta 5
   clc
+  rts

testMemoryType = *  ;( .A=type, .X=bankLimit ) : .A=bankCount
   sta mp+3
   stx bankLimit
   lda #$00
   ldy #$80  ;** page to use for testing ram
   ldx #$00
   sta mp+0
   sty mp+1
   stx mp+2
   lda #0
   sta banks

   nextBank = *
   lda banks
   sta mp+2
   jsr saveBank
   lda #$ff-$cb
   sta memWrite
   lda mp+2
   sta memWrite+1
   ldx #memWrite
   ldy #2
   jsr aceMemZpstore
   lda #$ff-$cb
   ldx mp+2
   jsr testBank
   bcs bankFail
   lda #$cb
   sta memWrite
   ldx #memWrite
   ldy #2
   jsr aceMemZpstore
   lda #$cb
   ldx mp+2
   jsr testBank
   bcs bankFail
   lda #$cb
   ldx #0
   jsr testBank
   bcs bankFail
   lda mp+2
   cmp #2
   bcc +
   lda #$cb
   ldx #2
   jsr testBank
   bcs bankFail
+  jsr restoreBank
   inc banks
   lda banks
   cmp bankLimit
   bcc nextBank

   bankFail = *
   jsr restoreWrapBanks
   lda banks
   rts

saveBank = *  ;()
   ldx #saveN
   ldy #2
   lda mp+2
   cmp #0
   bne +
   ldx #save0
+  cmp #2
   bne +
   ldx #save2
+  jsr aceMemZpload
   rts

restoreBank = *  ;()
   lda mp+2
   cmp #0
   beq +
   cmp #2
   beq +
   ldx #saveN
   ldy #2
   jsr aceMemZpstore
+  rts

restoreWrapBanks = *  ;()
   lda banks
   cmp #3
   bcc +
+  lda #2
   sta mp+2
   ldx #save2
   ldy #2
   jsr aceMemZpstore
   lda banks
   cmp #1
   bcc +
   lda #0
   sta mp+2
   ldx #save0
   ldy #2
   jsr aceMemZpstore
+  rts

rdVal  = 10  ;(1)
rdBank = 11  ;(1)

testBank = *  ;( .A=data, .X=bank ) : .CS=err
   sta rdVal
   lda mp+2
   sta rdBank
   stx mp+2
   lda #$ff
   sta memRead
   sta memRead+1
   ldx #memRead
   ldy #2
   jsr aceMemZpload
   lda memRead
   cmp rdVal
   bne +
   lda memRead+1
   cmp mp+2
   bne +
   lda rdBank
   sta mp+2
   clc 
   rts
+  lda rdBank
   sta mp+2
   sec
   rts

puts = *
   sta $40
   sty $41
   ldy #0
-  lda ($40),y
   beq +
   jsr chrout
   iny
   bne -
   inc $41
   bne -
+  rts

loadConfig = *
   lda #0
   ldx 186
   ldy #0
   jsr kernelSetlfs
   lda 1023
   cmp #1
   beq +
   ora #$30
   sta loadConfigName+6
   lda #7
   jmp ++
+  lda #6
+  ldx #<loadConfigName
   ldy #>loadConfigName
   jsr kernelSetnam
   lda #0
   ldx configBuf+0
   ldy configBuf+1
   jsr kernelLoad
   bcs +
   clc
   rts
+  lda #<badConfig
   ldy #>badConfig
   jsr puts
   sec
   rts

   loadConfigName = *
   .asc ".acerc0"
   badConfig = *
   .asc "Error attempting to b-load "
   .byte chrQuote
   .asc ".acerc"
   .byte chrQuote
   .asc ", aborting."
   .byte chrCR,0

loadCharset = *
   lda configBuf+0
   ldy configBuf+1
   iny
   clc
   adc #$e0
   bcc +
   iny
+  sta loadCharsetNameP+0
   sty loadCharsetNameP+1
   lda #0
   ldx 186
   ldy #0
   jsr kernelSetlfs
   ldy #$ff
-  iny
   lda (loadCharsetNameP),y
   bne -
   tya
   ldx loadCharsetNameP+0
   ldy loadCharsetNameP+1
   jsr kernelSetnam
   lda #0
   ldx #<charsetBuf
   ldy #>charsetBuf
   jsr kernelLoad
   bcc +
   lda #<badCharset
   ldy #>badCharset
   jsr puts
   lda loadCharsetNameP+0
   ldy loadCharsetNameP+1
   jsr puts
   lda #<badCharset2
   ldy #>badCharset2
   jsr puts
   sec
+  rts

   loadCharsetName = *
   .asc "acechr-commodore"
   .byte 0
   badCharset = *
   .asc "Error attempting to b-load "
   .byte chrQuote,0
   badCharset2 = *
   .byte chrQuote
   .asc ", aborting."
   .byte chrCR,0

loadKeymap = *
   lda #0
   ldx 186
   ldy #0
   jsr kernelSetlfs
   lda configBuf+0
   ldy configBuf+1
   iny
   clc
   adc #$20
   bcc +
   iny
+  sta keymapName+0
   sty keymapName+1
   ldy #$ff
-  iny
   lda (keymapName),y
   bne -
   tya
   bne +
   rts
+  ldx keymapName+0
   ldy keymapName+1
   jsr kernelSetnam
   lda #0
   ldx keymapAddr+0
   ldy keymapAddr+1
   jsr kernelLoad
   bcs +
   clc
   rts
+  lda #<badKeymap
   ldy #>badKeymap
   jsr puts
   sec
   rts

   badKeymap = *
   .asc "Error attempting to b-load keymap file"
   .asc ", aborting."
   .byte chrCR,0

screenInit = *
   lda #147
   jsr $ffd2
   lda sysName+0
   ldy sysName+1
   jsr puts
   rts

displayDate = *
   lda #<dateBuf
   ldy #>dateBuf
   jsr aceTimeGetDate
   ;** year
   lda dateBuf+0
   ldx #11
   jsr putDigits
   lda dateBuf+1
   ldx #13
   jsr putDigits
   ;** month
   lda dateBuf+2
   cmp #$10
   bcc +
   sec
   sbc #$10-10
+  tax
   lda monthStr+0,x
   sta dateStr+7
   lda monthStr+13,x
   sta dateStr+8
   lda monthStr+26,x
   sta dateStr+9
   ;** day
   lda dateBuf+3
   ldx #4
   jsr putDigits
   ;** hour
   lda dateBuf+4
   ldx #"a"
   cmp #$00
   bne +
   lda #$12
   jmp putHour
+  cmp #$12
   bcc putHour
   ldx #"p"
   cmp #$12
   beq putHour
   sei
   sed
   sec
   sbc #$12
   cld
   cli
   putHour = *
   stx dateStr+26
   ldx #17
   jsr putDigits
   ;** minute
   lda dateBuf+5
   ldx #20
   jsr putDigits
   ;** second
   lda dateBuf+6
   ldx #23
   jsr putDigits
   ;** day of week
   lda dateBuf+7
   and #$07
   tax
   lda dowStr+0,x
   sta dateStr+0
   lda dowStr+8,x
   sta dateStr+1
   lda dowStr+16,x
   sta dateStr+2
   lda #<dateStr
   ldy #>dateStr
   jsr puts
   rts

   putDigits = *  ;( .A=num, .X=offset )
   pha
   lsr
   lsr
   lsr
   lsr
   ora #$30
   sta dateStr,x
   pla
   and #$0f
   ora #$30
   sta dateStr+1,x
   rts
 
   dateStr = *
        ;0123456789012345678901234567
   .asc "Tue-05-May-1993  11:34:12 pm"
   .byte 13,13,0
   dateBuf .buf 16
   dowStr = *
   .asc "SMTWTFSX"
   .asc "uouehrax"
   .asc "nneduitx"
   monthStr = *
   .asc "XJFMAMJJASOND"
   .asc "xaeapauuuecoe"
   .asc "xnbrrynlgptvc"

tryDate = 10

setDate = *
   ldy #$82
-  sty tryDate
   lda (configBuf),y
   cmp #$ff
   beq dateQueryNext
   cmp #$fe
   bne +
   jmp dateTrySmartWatch
+  jsr cmdOpen
   bcs dateQueryNext
   lda #<queryDateStr
   ldy #>queryDateStr
   jsr cmdSend
   bcs queryError
   lda #<dateBuf
   ldy #>dateBuf
   ldx #9
   jsr cmdData
   bcs queryError
   jsr cmdClose
   jmp convertCmdDate

   queryError = *
   jsr cmdClose
   dateQueryNext = *
   inc tryDate
   ldy tryDate
   cpy #$86
   bcc -
   jmp setDefaultDate

   queryDateStr = *
   .asc "t-rb"
   .byte 13,0

convertCmdDate = *
   lda dateBuf+4
   ldx dateBuf+7
   beq +
   sei
   sed
   clc
   adc #$12
   cld
   cli
+  cmp #$12
   bne +
   lda #$00
+  cmp #$24
   bne +
   lda #$12
+  sta dateBuf+4
   clc
   lda dateBuf+0
   adc #$50
   sta dateBuf+7
   ldx #$19
   lda dateBuf+1
   cmp #$70
   bcs +
   ldx #$20
+  stx dateBuf+0
   lda #<dateBuf
   ldy #>dateBuf
   jsr aceTimeSetDate
   rts

setDefaultDate = *
   ldy #$90
   ldx #0
-  lda (configBuf),y
   sta dateBuf,x
   inx
   iny
   cpy #$9c
   bcc -
   lda dateBuf+7
   and #$f0
   sta dateBuf+7
   lda dateBuf+8
   and #$0f
   ora dateBuf+7
   sta dateBuf+7
   lda #<dateBuf
   ldy #>dateBuf
   jmp aceTimeSetDate

cmdOpen = *  ;( .A=device ) : .CS=err
   tax
   lda #6
   ldy #15
   jsr kernelSetlfs
   lda #0
   jsr kernelSetnam
   jsr kernelOpen
   rts

cmdClose = *  ;()
   clc
   lda #6
   jsr kernelClose
   rts

cmdSend = *  ;( (.AY)=cmdStrZ ) : .CS=err
   sta $40
   sty $41
   ldx #6
   jsr kernelChkout
   bcc +
   rts
+  ldy #0
-  lda ($40),y
   beq +
   jsr $ffd2
   iny
   bne -
+  jsr kernelClrchn
   clc
   rts

cmdData = *  ;( (.AY)=cmdBuf, .X=len ) : .CS=err
   sta $40
   sty $41
   stx $42
   ldx #6
   jsr kernelChkin
   bcc +
   rts
+  ldx #0
   jsr kernelChrin
   cmp #"0"
   bcc ++
   cmp #"9"+1
   bcs ++
   badData = *
-  jsr kernelChrin
   bcs +
   bit st
   bvs +
   cmp #13
   bne -
+  jsr kernelClrchn
   sec
   rts
+  ldy #0
   sta ($40),y
   iny
-  jsr kernelChrin
   sta ($40),y 
   iny
   cpy $42
   bcc -
   cmp #13
   bne badData
   jsr kernelClrchn
   clc
   rts

getRlParmReuBanks  .buf 1

getRamlinkParms = *
   ldy #$80
   lda (configBuf),y
   jsr cmdOpen
   bcs rlParmsError

   ;** ramlink ram access
   ldy #$e0
   lda (configBuf),y
   sta partRlCmd+3
   lda #<partRlCmd
   ldy #>partRlCmd
   jsr cmdSend
   bcs rlParmsError
   ldy #$e1
   jsr checkPartition
   bcs +
   sta aceRamlinkStart+0
   sty aceRamlinkStart+1
   stx aceRamlinkBanks
   jsr rlReuPhysSize  ;returns .A=reu banks in ram-port
   sta aceReuRlSpeedTry
   cmp #0
   beq +
   sta getRlParmReuBanks
   sec
   lda aceRamlinkStart+1
   sbc getRlParmReuBanks
   sta aceRamlinkStart+1
   lda #$ff
   sta aceReuRlSpeedTry

   ;** indirect reu access
+  ldy #$ee
   lda (configBuf),y
   sta partRlreuCmd+3
   lda #<partRlreuCmd
   ldy #>partRlreuCmd
   jsr cmdSend
   bcs rlParmsError
   ldy #$ef
   jsr checkPartition
   bcs rlParmsError
   stx indirectReuBanks

   rlParmsError = *
   jsr cmdClose
   rts

checkPartition = *  ;( .Y=nameOff ) : .CS=err, .AY=start, .X=banks
   sty $44
   lda #<partitionBuf
   ldy #>partitionBuf
   ldx #31
   jsr cmdData
   bcs checkErrExit
   lda partitionBuf+0
   cmp #7
   bne checkErrExit
   ldy $44
   ldx #0
-  lda (configBuf),y
   beq +
   cmp partitionBuf+3,x
   bne checkErrExit
   inx
   iny
   bne -
+  cpx #16
   bcs +
   lda partitionBuf+3,x
   cmp #$a0
   bne checkErrExit
+  lda partitionBuf+21
   ldy partitionBuf+20
   ldx partitionBuf+28
   clc
   rts

   checkErrExit = *
   sec
   rts

partRlCmd = *
   .asc "g-p"
   .byte 5,0
partRlreuCmd = *
   .asc "g-p"
   .byte 5,0
partitionBuf .buf 35

putnum = *  ;( [$44]=num, .A=width )
   pha
   lda #<outchar
   ldy #>outchar
   sta zp+0
   sty zp+1
   ldx #$44
   pla
   jsr aceMiscUtoa
   jsr putcomma
   ldx #0
-  lda outchar,x
   beq +
   jsr chrout
   inx
   bne -
+  rts
   outchar .buf 16

putcomma = *  ;( outchar )
   ldx #$ff
-  inx
   lda outchar,x
   bne -
-  dex
   dex
   dex
   dex
   cpx #$80
   bcc +
   rts
+  cpx #0
   bcs +
   rts
+  lda outchar,x
   cmp #" "
   bne +
   rts
+  stx $44
   ldy #0
-  lda outchar+1,y
   sta outchar,y
   iny
   cpy $44
   bcc -
   lda #"_"
   sta outchar,y
   jmp --
   rts

displayAvail = *  ;( (.AY)=name, (.X,$45)=banks )
   sta $40
   sty $41
   stx $44
   ldy #0
-  lda ($40),y
   jsr chrout
   iny
   cpy #6
   bcc -
   lda #":"
   jsr chrout
   txa
   clc
   adc totalBanks
   sta totalBanks
   lda $45
   adc totalBanks+1
   sta totalBanks+1
   lda #0
   sta $46
   sta $47
   ldx #6
-  asl $44
   rol $45
   rol $46
   rol $47
   dex
   bne -
   lda #7
   jsr putnum
   lda #"K"
   jsr chrout
   rts

addToFree = *  ;( [$44]=bytes )
   clc
   lda $44
   adc aceFreeMemory+0
   sta aceFreeMemory+0
   lda $45
   adc aceFreeMemory+1
   sta aceFreeMemory+1
   lda $46
   adc aceFreeMemory+2
   sta aceFreeMemory+2
   bcc +
   inc aceFreeMemory+3
+  rts

displayFree = *  ;( [$44]=bytes )
   lda #<freeMsg
   ldy #>freeMsg
   jsr puts
   lda #11
   jsr putnum
   lda #13
   jsr chrout
   rts
   freeMsg = *
   .asc "   free:"
   .byte 0

resetFree = *
   lda #0
   ldx #3
-  sta $44,x
   dex
   bpl -
   rts

internalMemory = *
   lda #aceMemInternal
   ldx #255
   sei
   jsr testMemoryType
   cli
   sta aceInternalBanks
   pha
   jsr installInternVectors
   pla
   tax
   lda #0
   sta $45
   lda #<internalName
   ldy #>internalName
   jsr displayAvail
   jsr resetFree

   ;** ram0
   lda #aceMemInternal
   sta mp+3
   lda #0
   sta aceInternalCur
   lda ram0FreeMap+0
   ldy ram0FreeMap+1
   sta aceRam0Freemap+0
   sty aceRam0Freemap+1
   ldx #0
   sta mp+0
   sty mp+1
   stx mp+2
   ldy #$a3
   bit sysType
   bmi +
   ldy #$c1
+  lda (configBuf),y
   tay
   lda #1
   ldx #>aceAppAddress
   jsr initBanks
   jsr freeRam0AfterKernel

   ;** ram1
   bit sysType
   bpl expInternal64
   lda #$00
   sta mp+0
   ldy #$a0
   lda (configBuf),y
   sta mp+1
   sta aceRam1Freemap
   lda #1
   sta mp+2
   ldy #$a1
   lda (configBuf),y
   tay
   lda #2
   ldx mp+1
   inx
   jsr initBanks

   ;** ram2-7 c128
   expInternal128 = *
   lda #2
   sta mp+2
   lda #$00
   ldy #$04
   sta mp+0
   sty mp+1
   ldy #$a5
   lda (configBuf),y
   ldx aceInternalBanks
   jsr min
   sta aceInternalBanks
   ldx #$05
   ldy #$ff
   jsr initBanks
   jsr addToFree
   jsr displayFree
   rts

   ;** ram1-3 c64
   expInternal64 = *
   lda #1
   sta aceInternalBanks
   jsr addToFree
   jsr displayFree
   rts

   internalName = *
   .asc "intern"

freeRam0AfterKernel = *
   ;** free end.kernel->st.app
   ldy aceEndPage
   cpy #>aceAppAddress
   bcs +
   lda #$00
-  sta (ram0FreeMap),y
   iny
   cpy #>aceAppAddress
   bcc -
+  sec
   lda #>aceAppAddress
   sbc aceEndPage
   sta $40
   bit sysType
   bvc +
   clc
   adc #3
   sta $40
   lda #$00
   ldy #$0b
   sta (ram0FreeMap),y
   ldy #$0c
   sta (ram0FreeMap),y
   ldy #$12
   sta (ram0FreeMap),y
+  clc
   lda $45
   adc $40
   sta $45
   bcc +
   inc $46
   bne +
   inc $47
+  rts

installInternVectors = *
   bit sysType
   bpl installVectors64
   lda aceInternalBanks
   cmp #2
   bcs +
   rts
+  sei
   lda #2
   ldy #aceMemInternal
   sta mp+2
   sty mp+3
-  lda #$05
   ldy #$ff
   sta mp+0
   sty mp+1
   sta zp+0
   sty zp+1
   lda #<251
   ldy #>251
   jsr aceMemStash
   inc mp+2
   lda mp+2
   cmp aceInternalBanks
   bcc -
   cli
   rts

installVectors64 = *
   ;xx copy to exp banks
   rts

ram0HiMemPtr .buf 1

reserveRam0HiMem = *
   lda #$ff
   sta ram0HiMemPtr
   jsr reserveVic80
   lda ram0HiMemPtr
   cmp #$ff
   beq +
   jsr reserveCharSet
   jsr reserveVic40
   jsr reserveBack80
   jsr reserveModem
   jmp ++
+  jsr reserveDymem
   jsr reserveVic40
   jsr reserveCharSet
   jsr reserveModem
+  nop
   rts

reserveVic80 = *
   lda #$00
   sta aceSoft80Allocated
   bit sysType
   bvs +
-  rts
+  ldy #$c0
   lda (configBuf),y
   bpl -
   lda scrDrivers
   and #$20
   beq -
   lda #$fc
   ldy #$d8
-  sta (ram0FreeMap),y
   iny
   cpy #$ff
   bcc -
   sec
   lda aceFreeMemory+1
   sbc #$ff-$d8
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   lda aceFreeMemory+3
   sbc #0
   sta aceFreeMemory+3
   lda #$d8
   sta ram0HiMemPtr
   lda #$ff
   sta aceSoft80Allocated
   rts

reserveCharSet = *
   sec
   lda ram0HiMemPtr
   sbc #>2048
   tax
   tay
   sta aceCharSetPage
   lda #$fc
-  sta (ram0FreeMap),y
   iny
   cpy ram0HiMemPtr
   bcc -
   stx ram0HiMemPtr
   sec
   lda aceFreeMemory+1
   sbc #>2048
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   lda aceFreeMemory+3
   sbc #0
   sta aceFreeMemory+3
   rts

reserveVic40 = *
   sec
   lda ram0HiMemPtr
   sbc #>1024
   tax
   tay
   sta aceVic40Page
   lda #$fc
-  sta (ram0FreeMap),y
   iny
   cpy ram0HiMemPtr
   bcc -
   stx ram0HiMemPtr
   sec
   lda aceFreeMemory+1
   sbc #>1024
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   lda aceFreeMemory+3
   sbc #0
   sta aceFreeMemory+3
   rts

reserveBack80 = *
   bit aceSoft80Allocated
   bmi +
   rts
+  sec
   lda ram0HiMemPtr
   sbc #>2048
   tax
   tay
   lda #$fc
-  sta (ram0FreeMap),y
   iny
   cpy ram0HiMemPtr
   bcc -
   stx ram0HiMemPtr
   sec
   lda aceFreeMemory+1
   sbc #>2048
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   lda aceFreeMemory+3
   sbc #0
   sta aceFreeMemory+3
   rts

reserveModem = *
   ;aceModemSendPage  = aceStatB+105 ;(1)
   ;aceModemRecvPage  = aceStatB+106 ;(1)
   ;aceModemRecvHigh  = aceStatB+107 ;(1)
   ;aceModemType      = aceStatB+108 ;(1)  ;$ff=swifty,$40=user-port,$00=none
   ;aceModemIoPage    = aceStatB+109 ;(1)
   ;aceModemConfig    = aceStatB+113 ;(1)
   lda #$00
   sta aceModemType
   ldy #0
-  lda (configBuf),y
   cmp #6
   beq +
   tya
   clc
   adc #4
   tay
   cpy #$80
   bcc -
   rts
+  lda #$ff
   sta aceModemType
   iny
   lda (configBuf),y
   sta aceModemIoPage
   iny
   iny
   lda (configBuf),y
   sta aceModemConfig
   dec ram0HiMemPtr
   ldy ram0HiMemPtr
   sty aceModemSendPage
   sty aceModemRecvHigh
   lda #$fc
   sta (ram0FreeMap),y
   sec
   lda aceFreeMemory+1
   sbc #1
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   lda aceFreeMemory+3
   sbc #0
   sta aceFreeMemory+3
   rts

reserveDymem = *
   sec
   lda ram0HiMemPtr
   sbc #>768
   sta ram0HiMemPtr
   rts

reserveTPA = *
   ldy #$a8
   ldx #$c0
   bit sysType
   bmi +
   ldy #$c6
   ldx #$d0
+  lda (configBuf),y
   cmp ram0HiMemPtr
   bcc +
   lda ram0HiMemPtr
+  stx $40
   cmp $40
   bcc +
   lda $40
+  sta $40
   sta aceTpaLimit
   ldy #>aceAppAddress
   lda #$fe
   cpy $40
   bcs +
-  sta (ram0FreeMap),y
   iny
   cpy $40
   bcc -
+  sec
   lda $40
   sbc #>aceAppAddress
   sta $40
   sec
   lda aceFreeMemory+1
   sbc $40
   sta aceFreeMemory+1
   lda aceFreeMemory+2
   sbc #0
   sta aceFreeMemory+2
   lda aceFreeMemory+3
   sbc #0
   sta aceFreeMemory+3
   rts

reuBleedSaveBuf .buf 16

reuBleedSave = *
   bit sysType
   bpl +
   lda #$3f
   sta $ff00
   jmp ++
+  lda #$30
   sta $01
+  nop
   ldx #15
-  lda $df00,x
   sta reuBleedSaveBuf,x
   dex
   bpl -
   bit sysType
   bpl +
   lda #$0e
   sta $ff00
   jmp ++
+  lda #$36
   sta $01
+  nop
   rts

reuBleedRestore = *
   bit sysType
   bpl +
   lda #$3f
   sta $ff00
   jmp ++
+  lda #$30
   sta $01
+  ldx #15
-  lda reuBleedSaveBuf,x
   sta $df00,x
   dex
   bpl -
   bit sysType
   bpl +
   lda #$0e
   sta $ff00
   jmp ++
+  lda #$36
   sta $01
+  nop
   rts

reuMemory = *
   lda #$00
   sta aceRamlinkAccess
   jsr reuBleedSave
   lda #aceMemREU
   ldx #255
   jsr testMemoryType
   pha
   jsr reuBleedRestore
   lda #0
   sta $45
   pla
   sta aceReuBanks
   tax
   bne +
   lda indirectReuBanks
   beq +
   sta aceReuBanks
   tax
   lda #$ff
   sta aceRamlinkAccess
   lda #<rlreuName
   ldy #>rlreuName
   jmp ++
+  lda #<reuName
   ldy #>reuName
+  jsr displayAvail
   jsr resetFree
   lda #aceMemREU
   sta mp+3
   ldy #$a6
   bit sysType
   bmi +
   ldy #$c2
+  lda (configBuf),y
   sta aceReuStart
   sta aceReuCur
   ldy #$a7
   bit sysType
   bmi +
   ldy #$c3
+  lda (configBuf),y
   ldx aceReuBanks
   jsr min
   sta aceReuBanks
   lda #$00
   ldy #$ff
   ldx aceReuStart
   sta mp+0
   sty mp+1
   stx mp+2
   lda aceReuBanks
   ldx #$00
   ldy #$ff
   jsr initBanks
   jsr addToFree
   jsr displayFree
   rts
   reuName = *
   .asc "reu   "
   rlreuName = *
   .asc "rl-reu"

rlMemory = *
   ldx aceRamlinkBanks
   lda #0
   sta $45
   lda #<rlName
   ldy #>rlName
   jsr displayAvail
   jsr resetFree
   ldy #$81
   lda (configBuf),y
   ldx aceRamlinkBanks
   jsr min
   sta aceRamlinkBanks
   lda #aceMemRL
   sta mp+3
   lda #$00
   ldy #$ff
   ldx #0
   stx aceRamlinkCur
   sta mp+0
   sty mp+1
   stx mp+2
   lda aceRamlinkBanks
   ldx #$00
   ldy #$ff
   jsr initBanks
   jsr addToFree
   jsr displayFree
   rts
   rlName = *
   .asc "rl-ram"

totalMemory = *
   ldx totalBanks
   lda totalBanks+1
   sta $45
   lda #<totalName
   ldy #>totalName
   jsr displayAvail
   ldx #3
-  lda aceFreeMemory,x
   sta $44,x
   dex
   bpl -
   jsr displayFree
   lda #13
   jsr chrout
   jsr reserveRam0HiMem
   jsr reserveTPA
   rts
   totalName = *
   .asc "total "
   rts

endBank   = 10  ;(1)
startFree = 11  ;(1)
endFree   = 12  ;(1)

initBanks = *  ;( [mp]=firstFreemap, .A=endBank+1, .X=startFree, .Y=endFree+1 )
   sta endBank
   stx startFree
   sty endFree
   lda #<freemap
   ldy #>freemap
   sta zp+0
   sty zp+1
   ldx #0
   lda #$ff
-  sta freemap,x
   inx
   bne -
   ldx startFree
   cpx endFree
   bcs freeNextBank
   lda #$00
-  sta freemap,x
   inx
   cpx endFree
   bcc -

   freeNextBank = *
   lda mp+2
   cmp endBank
   bcs +
   lda #<256
   ldy #>256
   jsr aceMemStash
   inc mp+2
   sec
   lda endFree
   sbc startFree
   clc
   adc $45
   sta $45
   bcc freeNextBank
   inc $46
   bne freeNextBank
   inc $47
   jmp freeNextBank
+  rts

min = *  ;( .A=num1, .X=num2 ) : .A=min
   stx $40
   cmp $40
   bcc +
   lda $40
+  rts

chroutSave .buf 1

chrout = *
   jsr $ffd2
   sty chroutSave
   ldy #0
   sta (titlePtr),y
   inc titlePtr+0
   bne +
   inc titlePtr+1
+  ldy chroutSave
   clc
   rts

rlReuPhysSize = *  ;( ) : .A=reu banks in ram-port
   sei
   lda #$ff
   sta aceRamlinkAccess
   jsr reuBleedSave
   lda #aceMemREU
   ldx #255
   jsr testMemoryType
   pha
   jsr reuBleedRestore
   pla
   sta 1022
   ldx #$00
   stx aceRamlinkAccess
   cli
   rts

;=== smart-watch stuff ===

;*** adapted from:
;***
;*** swread v1.00: SmartWatch reader
;*** by Randy Weems  -  20-Mar-1995
;*** 
;*** Read SmartWatch connected to joystick port 1
;*** and set system date/time.

dateTrySmartWatch = *
   jsr swMain
   bcs +
   rts
+  jmp dateQueryNext

swByte        = work+0 ;(1)  ;swap byte during read/write
swPRASave     = work+1 ;(1)  ;save for port A
swDDRASave    = work+2 ;(1)  ;save for port A DDR

swCIA1PRA     = $dc00  ;(1)  ;CIA 1 port A
swCIA1DDRA    = $dc02  ;(1)  ;CIA 1 Data Direction Register port A

;*** sw main routine ***
swMain = *
   php
   sei
   ;** save CIA 1's registers
   lda swCIA1PRA
   sta swPRASave
   lda swCIA1DDRA
   sta swDDRASave

   ;** setup CIA 1 port A
   lda #%00001111
   sta swCIA1DDRA
   sta swCIA1PRA

   ;** start SmartWatch read sequence and get date/time
   jsr swInitRead
   ldx #7
-  jsr swReadByte
   sta vbDTBuffer,x
   dex
   bpl -

   ;** restore CIA1's ports
   lda swDDRASave
   sta swCIA1DDRA
   lda swPRASave
   sta swCIA1PRA

   ;** rearrange results from smartwatch to
   ;** to match setdate's format

   ;only need lower 6 bits of hour
   lda vbDTBuffer+4
   and #%00111111
   sta vbDTBuffer+4
  
   ;upper nibble is tenths of second, and 
   ;lower nibble is day of week
   lda vbDTBuffer+7
   asl
   asl
   asl
   asl
   sta vbDTBuffer+7
   lda vbDTBuffer+3
   and #$0f
   ;smartwatch returns 0-6 for Sun-Sat, ACE uses same (now)
   ora vbDTBuffer+7
   sta vbDTBuffer+7

   ;shift year, month, and day up by one byte
   ldx #2
 - lda vbDTBuffer+0,x
   sta vbDTBuffer+1,x
   jsr swIsValidBCD
   bcs swBadRead
   dex
   bpl -

   ; ** determine century
   ;lda vbDTBuffer+1    ;not necessary, left over from above
   cmp #$80             ;if the year's < '80 assume it's 20xx
   lda #$19
   bcs +
   lda #$20
+  sta vbDTBuffer+0

   ;**display buffer in hex; for debugging only
;   ldy #0
;-  sty swByte
;   lda vbDTBuffer,y
;   jsr puthex
;   lda #' '
;   jsr chrout
;   ldy swByte
;   iny
;   cpy #8
;   bne -
;   lda #chrCR
;   jsr chrout

   lda #<vbDTBuffer
   ldy #>vbDTBuffer
   jsr aceTimeSetDate 

swExit = *
   plp
   clc
   rts

swBadRead = *
   plp
   sec
   rts

swInitRead = *
   lda #%00001111
   sta swCIA1DDRA

   lda #%00001110
   ldx #%00000010
   sta swCIA1PRA
   stx swCIA1PRA
   sta swCIA1PRA

   ldx #7
-  lda swReadSeq,x
   jsr swSendByte
   dex
   bpl -
   rts

swReadSeq = *
   .byte $5c,$a3,$3a,$c5,$5c,$a3,$3a,$c5

swSendByte = *
   sta swByte
   lda #%00001111
   sta swCIA1DDRA

   ldy #8
   lda #%00001100
   sta swCIA1PRA
-  lda swByte
   and #%00000001
   sta swCIA1PRA
   ora #%00001100
   sta swCIA1PRA
   lsr swByte
   dey
   bne -
   rts

swReadByte = *
   lda #%00001110
   sta swCIA1DDRA

   ldy #8
 - lda #%00001110
   sta swCIA1PRA
   lda #%00000010
   sta swCIA1PRA
   lda swCIA1PRA
   lsr
   ror swByte
   dey
   bne -
   lda swByte
   rts

;*** end of sw program ***

;*** sw utility routines ***

swIsValidBCD = *
; test upper and lower nibble of .A for valid BCD digits
; returns with carry flag clear for valid packed BCD, set otherwise
   pha
   and #%00001111
   cmp #$0a
   pla
   bcs +
   cmp #$a0
 + rts

;puthex = *  ;( .A=value )
;   pha
;   lsr
;   lsr
;   lsr
;   lsr
;   tax
;   lda puthexChars,x
;   jsr chrout
;   pla
;   and #$0f
;   tax
;   lda puthexChars,x
;   jsr chrout
;   rts
;   puthexChars = *
;   .byte "0123456789abcdef"

;** uninitialzed data
vbDTBuffer .buf 8  ;(8)   ;buffer for date/time - vector of bytes

;=== bss ===

bss = *
freemap = bss+0
title = freemap+256
charsetBuf = title+512
bssEnd = charsetBuf+4300
