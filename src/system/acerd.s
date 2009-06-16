;ACE-128/64 kernel ramdisk driver, started 18-Apr-1994.

rdDirentLength  = 48
rdDirentBuf     .buf rdDirentLength
rdDirentBytes   = aceDirentBytes-aceDirentBuffer+rdDirentBuf
rdDirentDate    = aceDirentDate-aceDirentBuffer+rdDirentBuf
rdDirentType    = aceDirentType-aceDirentBuffer+rdDirentBuf
rdDirentFlags   = aceDirentFlags-aceDirentBuffer+rdDirentBuf
rdDirentUsage   = aceDirentUsage-aceDirentBuffer+rdDirentBuf
rdDirentNameLen = aceDirentNameLen-aceDirentBuffer+rdDirentBuf
rdDirentName    = aceDirentName-aceDirentBuffer+rdDirentBuf
rdDirentHeadPtr = rdDirentBuf+$24
rdDirentTailPtr = rdDirentBuf+$28
rdDirentReserved = rdDirentBuf+$2c
rdDcbOpenMode   = fcbTable1+0  ;(1)
rdDcbStatus     = fcbTable1+1  ;(1)
rdDcbNextDirent = fcbTable1+2  ;(4)
rdFcbrOpenMode  = fcbTable1+0  ;(1)
rdFcbrNextBlk   = fcbTable1+1  ;(3)
rdFcbrFilePtr   = fcbTable1+4  ;(4)
rdFcbrBlkBytes  = fcbTable1+8  ;(2)

rdFcbwOpenMode  = fcbTable1+0  ;(1)
rdFcbwBlkPage   = fcbTable1+1  ;(1)
rdFcbwRemBytes  = fcbTable1+2  ;(2)
rdFcbwFilePtr   = fcbTable1+4  ;(4)
rdFcbwFileSize  = fcbTable1+8  ;(4)
rdFcbwDirent    = fcbTable1+$c ;(4)

rdFcbwBuf .buf 18
rdFcbwBufOpenMode = rdFcbwOpenMode-rdFcbwOpenMode+rdFcbwBuf
rdFcbwBufBlkPage  = rdFcbwBlkPage-rdFcbwOpenMode+rdFcbwBuf
rdFcbwBufRemBytes = rdFcbwRemBytes-rdFcbwOpenMode+rdFcbwBuf
rdFcbwBufFilePtr  = rdFcbwFilePtr-rdFcbwOpenMode+rdFcbwBuf
rdFcbwBufFileSize = rdFcbwFileSize-rdFcbwOpenMode+rdFcbwBuf
rdFcbwBufDirent   = rdFcbwDirent-rdFcbwOpenMode+rdFcbwBuf
rdInitIndex .buf 1

rdInit = *
   ldx #0
   stx rdInitIndex
-  ldx rdInitIndex
   lda configBuf+0,x
   cmp #4
   bne +
   jsr rdInitDrive
+  clc
   lda rdInitIndex
   adc #4
   sta rdInitIndex
   cmp #$80
   bcc -
   rts

rdMallocFlags .buf 1  ;xx not used yet

rdVarMalloc = *  ;( .A=pagesRequested ) : .CS=err, [mp]=mem, .A=pagesGotten
   jsr rdMalloc
   bcc +
   lda errno
   cmp #aceErrInsufficientMemory
   beq ++
   sec
+  rts
+  lda #1
   ;xx fall through

rdMalloc = *  ;( .A=pagesRequired ) : .CS=err, [mp]=mem, .A=pagesGotten
   pha
   pha
   lda #$fd
   sta allocProcID
   pla
   ldx #$00
   ldy #$ff
   jsr kernPageAlloc
   pla
   rts

rdFree = *  ;( [mp]=memPtr, .A=pages ) : .CS=err
   pha
   lda #$fd
   sta allocProcID
   pla
   jsr kernPageFree
   rts

rdInitDrive = *  ;( rdInitIndex )
   ;** alloc root dirent, root dir contents
   lda #2
   jsr rdMalloc
   bcc +
   rts

   ;** init device table entry: root dir, current dir
+  lda rdInitIndex
   asl
   tay
   lda rdDirentOffsetTab+0
   sta deviceTable+0,y
   sta deviceTable+4,y
   iny
   ldx #1
-  lda mp,x
   sta deviceTable+0,y
   sta deviceTable+4,y
   iny
   inx
   cpx #4
   bcc -

   ;** init root dirent
   jsr rdInitDirBlockStrbuf
   ldx #rdDirentLength-1
   lda #0
-  sta rdDirentBuf,x
   dex
   bpl -
   lda #1
   sta rdDirentBytes+1
   lda #<rdDirentDate
   ldy #>rdDirentDate
   jsr internGetDate
   ldx #3
-  lda rdRootType,x
   sta rdDirentType,x
   dex
   bpl -
   lda #%11110100
   sta rdDirentFlags
   lda #13
   sta rdDirentNameLen
   ldx #16
-  lda rdRootName,x
   sta rdDirentName,x
   dex
   bpl -
   lda rdInitIndex
   lsr
   lsr
   clc
   adc #"@"
   sta rdDirentName+12
   lda #%10000000
   sta rdDirentUsage
   ldx #3
-  lda mp,x
   sta rdDirentHeadPtr,x
   sta rdDirentTailPtr,x
   dex
   bpl -
   inc rdDirentHeadPtr+1
   inc rdDirentTailPtr+1
   ldx #rdDirentLength-1
-  lda rdDirentBuf,x
   sta workbuf+$10,x
   dex
   bpl -
   lda #<workbuf
   ldy #>workbuf
   sta zp+0
   sty zp+1
   lda #<256
   ldy #>256
   jsr stash

   ;** init root dir contents
   ;** "." entry
   lda #0
   sta rdDirentBytes+1
   lda #%01100100
   sta rdDirentFlags
   ldx #3
-  lda rdDotType,x
   sta rdDirentType,x
   dex
   bpl -
   lda #1
   sta rdDirentNameLen
   ldx #16
   lda #0
-  sta rdDirentName,x
   dex
   bpl -
   lda #"."
   sta rdDirentName+0
   lda #%11010000
   sta rdDirentUsage
   lda #$10
   sta rdDirentHeadPtr+0
   dec rdDirentHeadPtr+1
   ldx #rdDirentLength-1
-  lda rdDirentBuf,x
   sta workbuf+$10,x
   dex
   bpl -
   ;** ".." entry
   lda #2
   sta rdDirentNameLen
   lda #"."
   sta rdDirentName+1
   ldx #rdDirentLength-1
-  lda rdDirentBuf,x
   sta workbuf+$40,x
   dex
   bpl -
   ;** store dir block
   inc mp+1
   lda #<256
   ldy #>256
   jsr stash
   rts

rdRootName = *
   .asc "ace-ramdisk-a"
   .byte 0,0,0,0
rdRootType = *
   .asc "dir"
   .byte 0
rdDotType = *
   .asc "hln"
   .byte 0
rdDirentOffsetTab = *
   .byte $10,$40,$70,$a0,$d0

rdInitDirBlockStrbuf = *
   ldx #0
   lda #0
-  sta workbuf,x
   inx
   bne -
   lda #1
   sta workbuf+$00
   sta workbuf+$05
   lda #aceMemNull
   sta workbuf+$03
   rts

rdDcbOffset .buf 1
rdFcbOffset = rdDcbOffset
rdDevOffset .buf 1
rdMpSave .buf 4
rdZpSave .buf 2
rdCount .buf 1
rdRegSave .buf 3

rdSaveMpZp = *
   ldx #3
-  lda mp,x
   sta rdMpSave,x
   dex
   bpl -
   lda zp+0
   ldy zp+1
   sta rdZpSave+0
   sty rdZpSave+1
   rts

rdRestoreMpZp = *
   ldx #3
-  lda rdMpSave,x
   sta mp,x
   dex
   bpl -
   lda rdZpSave+0
   ldy rdZpSave+1
   sta zp+0
   sty zp+1
   rts

rdDirOpen = *
rdOpenPreamble = *
   lda openDevice
   asl
   sta rdDevOffset
   lda openFcb
   asl
   asl
   asl
   asl
   sta rdDcbOffset
   tax
   lda #"d"
   sta rdDcbOpenMode,x
   inx
   ldy #15
   lda #0
-  sta rdDcbOpenMode,x
   inx
   dey
   bne -
   ldx rdDcbOffset
   lda #$00
   sta rdDcbStatus,x
   ldy rdDevOffset
   iny
   iny
   iny
   iny
   lda #0
   sta rdCount
   ldx rdDcbOffset
-  lda deviceTable,y
   sta rdDcbNextDirent,x
   stx rdRegSave+1
   ldx rdCount
   sta rdWorkDir,x
   ldx rdRegSave+1
   inx
   iny
   inc rdCount
   lda rdCount
   cmp #4
   bcc -
   lda openFcb
   clc
   rts

rdDirClose = *
   jmp closeFdEntry

;*** dirread( .X=dcb, .Y=devCfgOff ) : .Z=eof, aceDirentBuffer=data

rdDcbNum .buf 1

rdDirRead = *
   stx rdDcbNum
   tya
   asl
   sta rdDevOffset
   lda rdDcbNum
   asl
   asl
   asl
   asl
   sta rdDcbOffset
   jsr rdSaveMpZp
   ldx rdDcbOffset
   lda rdDcbOpenMode,x
   cmp #"d"
   beq +
   lda #aceErrFileNotInput
   jmp rdDirReadError
+  lda rdDcbStatus,x
   cmp #$00
   beq rdDirReadTitle
   cmp #$01
   bne +
   jmp rdDirReadDirent
+  lda #0
   sta aceDirentNameLen
   sta aceDirentName+0
   clc
   ;** fall through
rdDirReadExit = *
   php
   jsr rdRestoreMpZp
   plp
   rts
rdDirReadError = *
   sta errno
   sec
   lda #0
   jmp rdDirReadExit

rdDirReadTitle = *
   ldx rdDcbOffset
   lda #$01
   sta rdDcbStatus,x
   ldy #0
-  lda rdDcbNextDirent,x
   sta mp,y
   inx
   iny
   cpy #4
   bcc -
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr fetch
   ldx #aceDirentLength-1
-  lda rdDirentBuf,x
   sta aceDirentBuffer,x
   dex
   bpl -
   ldx rdDcbOffset
   ldy #0
-  lda rdDirentHeadPtr,y
   sta rdDcbNextDirent,x
   inx
   iny
   cpy #4
   bcc -
   ldx rdDcbOffset
   lda #$10
   sta rdDcbNextDirent+0,x
   lda #$ff
   clc
   jmp rdDirReadExit

rdDirReadDirent = *
   ldx rdDcbOffset
   ldy #0
-  lda rdDcbNextDirent,x
   sta mp,y
   inx
   iny
   cpy #4
   bcc -
   lda mp+0
   cmp #$ff
   beq rdDirReadNextBlock
   ldx rdDcbOffset
   clc
   adc #rdDirentLength
   bcc +
   lda #$ff
+  sta rdDcbNextDirent+0,x
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr fetch
   lda rdDirentUsage
   bpl rdDirReadDirent
   ldx #aceDirentLength-1
-  lda rdDirentBuf,x
   sta aceDirentBuffer,x
   dex
   bpl -
   lda #$ff
   clc
   jmp rdDirReadExit

rdBlockHead .buf 6
rdBhPages   = rdBlockHead+0
rdBhNextPg  = rdBlockHead+1
rdBhBytes   = rdBlockHead+4

   rdDirReadNextBlock = *
   lda #$00
   sta mp+0
   lda #<rdBlockHead
   ldy #>rdBlockHead
   sta zp+0
   sty zp+1
   lda #6
   ldy #0
   jsr fetch
   lda rdBhNextPg+2
   cmp #aceMemNull
   beq rdDirReadFree
   ldx rdDcbOffset
   lda #$10
   sta rdDcbNextDirent+0,x
   inx
   ldy #0
-  lda rdBhNextPg,y
   sta rdDcbNextDirent,x
   inx
   iny
   cpy #3
   bcc -
   jmp rdDirReadDirent

rdDirReadFree = *
   ldx rdDcbOffset
   lda #$ff
   sta rdDcbStatus,x
   lda #0
   ldx #aceDirentLength-1
-  sta aceDirentBuffer,x
   dex
   bpl -
   ldx #3
-  lda aceFreeMemory,x
   sta aceDirentBytes,x
   dex
   bpl -
   lda #%01000100
   sta aceDirentFlags
   lda #%00000000
   sta aceDirentUsage
   jsr rdRestoreMpZp
   lda #$ff
   clc
   rts

rdChDir = *
   lda #0
   sta stringBuffer+2
   jmp chdirSetName

rdFileType .buf 1

rdOpen = *
   jsr rdOpenPreamble
   jsr rdSaveMpZp
   ldy openNameScan
   ldx #0
-  lda (zp),y
   sta stringBuffer,x
   beq +
   inx
   iny
   bne -
+  lda #"s"
   sta rdFileType
   lda stringBuffer-2,x
   cmp #","
   bne +
   lda stringBuffer-1,x
   sta rdFileType
   lda #0
   sta stringBuffer-2,x
+  ldx rdFcbOffset
   lda openMode
   sta rdFcbrOpenMode,x
   cmp #"r"
   beq +
   jmp rdOpenWrite

   ;** open for read
+  jsr rdScanDir
   bcc +
   lda errno
   jmp rdOpenError
+  ldx rdFcbOffset
   lda #0
   sta rdFcbrBlkBytes+0,x
   sta rdFcbrBlkBytes+1,x
   ldy #0
-  lda rdDirentHeadPtr+1,y
   sta rdFcbrNextBlk,x
   inx
   iny
   cpy #3
   bcc -
   jsr rdRestoreMpZp
   lda openFcb
   clc
   rts

   rdOpenWrite = *
   cmp #"w"
   beq +
   jmp rdOpenAppend
+  jsr rdScanDir
   bcs +
   lda #aceErrFileExists
   rdOpenError = *
   pha
   jsr rdRestoreMpZp
   pla
   sta errno
   ldx openFcb
   lda #lfnull
   sta lftable,x
   sec
   lda #fcbNull
   rts
+  lda rdFreeDirent+3
   cmp #aceMemNull
   bne +
   jsr rdAddDirBlock
   bcc +
   lda #aceErrDiskFull
   jmp rdOpenError
   ;** initialize directory entry
+  ldx #rdDirentLength-1
   lda #0
-  sta rdDirentBuf,x
   dex
   bpl -
   lda #$00
   sta stringBuffer+16
   ldx #$ff
-  inx
   lda stringBuffer,x
   sta rdDirentName,x
   bne -
   stx rdDirentNameLen
   lda #<rdDirentDate
   ldy #>rdDirentDate
   jsr internGetDate
   ldy #0
   lda rdFileType
   cmp #"p"
   bne +
   ldy #4
+  ldx #0
-  lda rdOpenWriteTypes,y
   sta rdDirentType,x
   iny
   inx
   cpx #4
   bcc -
   lda #%01101100
   sta rdDirentFlags
   lda #%10000000
   sta rdDirentUsage
   ldx #3
-  lda #aceMemNull
   sta rdDirentHeadPtr,x
   sta rdDirentTailPtr,x
   dex
   bpl -
   ;** initialize fcb buffer
   ldx #15
   lda #$00  ;xx assume null=$00
-  sta rdFcbwBufOpenMode,x
   dex
   bpl -
   lda #"w"
   sta rdFcbwBufOpenMode
   ldx #3
-  lda rdFreeDirent,x
   sta rdFcbwBufDirent,x
   sta mp,x
   dex
   bpl -
   ;** write directory entry
   rdOpenWriteFlush = *
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr stash
   ;** write fcb entry
   ldx #0
   ldy rdFcbOffset
-  lda rdFcbwBufOpenMode,x
   sta rdFcbwOpenMode,y
   iny
   inx
   cpx #16
   bcc -
   jsr rdRestoreMpZp
   lda openFcb
   clc
   rts

rdOpenWriteTypes = *
   .asc "seq"
   .byte $00
   .asc "prg"
   .byte $00

   rdOpenAppend = *
   cmp #"a"
   beq +
   lda #aceErrInvalidFileMode
   jmp rdOpenError
+  jsr rdScanDir
   bcc +
   lda errno
   jmp rdOpenError
+  lda rdDirentFlags
   bpl +
   lda #aceErrOpenDirectory
   jmp rdOpenError
+  and #%00100000
   bne +
   lda #aceErrPermissionDenied
   jmp rdOpenError
+  lda rdDirentFlags
   ora #%00001000
   sta rdDirentFlags
   lda #<rdDirentDate
   ldy #>rdDirentDate
   jsr internGetDate
   lda #"w"
   sta rdFcbwBufOpenMode
   ldx #3
-  lda rdDirentTailPtr,x
   sta rdFcbwBufFilePtr,x
   sta mp,x
   lda rdDirentBytes,x
   sta rdFcbwBufFileSize,x
   lda rdDirentPtr,x
   sta rdFcbwBufDirent,x
   dex
   bpl -
   lda #$06
   sta rdFcbwBufFilePtr+0
   lda rdFcbwBufFilePtr+1
   sta rdFcbwBufBlkPage   ;%%%
   lda #<rdOpenBlkHdr
   ldy #>rdOpenBlkHdr
   sta zp+0
   sty zp+1
   lda #6
   ldy #0
   jsr fetch
   clc
   lda rdFcbwBufFilePtr+0
   adc rdOpenBlkHdr+4
   sta rdFcbwBufFilePtr+0
   lda rdFcbwBufFilePtr+1
   adc rdOpenBlkHdr+5
   sta rdFcbwBufFilePtr+1
   sec
   lda #250
   sbc rdOpenBlkHdr+4
   sta rdFcbwBufRemBytes+0
   ldx rdOpenBlkHdr+0
   dex
   txa
   sbc rdOpenBlkHdr+5
   sta rdFcbwBufRemBytes+1
   ldx #3
-  lda rdDirentPtr,x
   sta mp,x
   dex
   bpl -
   jmp rdOpenWriteFlush

rdOpenBlkHdr .buf 6

rdWorkDir       .buf 4
rdDirentPtr     .buf 4
rdFreeDirent    .buf 4
rdScanDirOffset .buf 1

rdScanDir = *  ;( stringBuffer=filename, [rdWorkDir] ) : [rdDirentPtr]=dirent
   ldx #3
-  lda rdWorkDir,x
   sta mp,x
   lda #aceMemNull
   sta rdFreeDirent,x
   dex
   bpl -
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr fetch
   ldx #3
-  lda rdDirentHeadPtr,x
   sta mp,x
   dex
   bpl -
   lda #<workbuf
   ldy #>workbuf
   sta zp+0
   sty zp+1

   rdScanDirNextBlock = *
   lda mp+3
   cmp #aceMemNull
   bne +
   lda #aceErrFileNotFound
   sta errno
   sec
   rts
+  lda #<256
   ldy #>256
   jsr fetch
   lda #$10
   sta rdScanDirOffset

   rdScanDirNextDirent = *
   ldx rdScanDirOffset
   lda rdDirentUsage-rdDirentBuf+workbuf,x
   bmi +
   lda rdFreeDirent+3
   cmp #aceMemNull
   bne rdScanDirContinue
   ldx #3
-  lda mp,x
   sta rdFreeDirent,x
   dex
   bpl -
   lda rdScanDirOffset
   sta rdFreeDirent+0
   jmp rdScanDirContinue
+  ldy #0
-  lda rdDirentName-rdDirentBuf+workbuf,x
   cmp stringBuffer,y
   bne rdScanDirContinue
   inx
   iny
   cmp #0
   bne -
   lda rdScanDirOffset
   sta mp+0
   ldx #3
-  lda mp,x
   sta rdDirentPtr,x
   dex
   bpl -
   ldx rdScanDirOffset
   ldy #0
-  lda workbuf,x
   sta rdDirentBuf,y
   inx
   iny
   cpy #rdDirentLength
   bcc -
   clc
   rts

   rdScanDirContinue = *
   clc
   lda rdScanDirOffset
   adc #rdDirentLength
   sta rdScanDirOffset
   bcc rdScanDirNextDirent
   ldx #3
-  lda workbuf,x
   sta mp,x
   dex
   bpl -
   lda #$00
   sta mp+0
   jmp rdScanDirNextBlock

rdAddDirBlock = *  ;( [rdWorkDir] ) : [rdFreeDirent], .CS=err
   ;** alloc new block
   lda #$00 ;xx
   sta rdMallocFlags
   lda #1
   jsr rdMalloc
   bcc +
   lda #aceErrDiskFull
   sta errno
   sec
   rts
   ;** init new block
+  jsr rdInitDirBlockStrbuf
   lda #<workbuf
   ldy #>workbuf
   sta zp+0
   sty zp+1
   lda #<256
   ldy #>256
   jsr stash
   ldx #3
-  lda mp,x
   sta rdFreeDirent,x
   dex
   bpl -

   ;** modify working directory dirent
   ldx #3
-  lda rdWorkDir,x
   sta mp,x
   dex
   bpl -
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr fetch
   ldx #3
-  lda rdDirentTailPtr,x
   sta rdPrevBlkPtr,x
   lda rdFreeDirent,x
   sta rdDirentTailPtr,x
   dex
   bpl -
   lda #rdDirentLength
   ldy #0
   jsr stash
   ;** link previous dir block
   ldx #3
-  lda rdPrevBlkPtr,x
   sta mp,x
   dex
   bpl -
   lda #$01
   sta mp+0
   lda #<rdDirentTailPtr+1
   ldy #>rdDirentTailPtr+1
   sta zp+0
   sty zp+1
   lda #3
   ldy #0
   jsr stash
   ;** clean up
   lda #$10
   sta rdFreeDirent+0
   clc
   rts

rdPrevBlkPtr .buf 4

rdReadChunk = syswork+8
rdFcbrBuf .buf 12
rdFcbrBufOpenMode = rdFcbrOpenMode-rdFcbrOpenMode+rdFcbrBuf
rdFcbrBufNextBlk  = rdFcbrNextBlk-rdFcbrOpenMode+rdFcbrBuf
rdFcbrBufFilePtr  = rdFcbrFilePtr-rdFcbrOpenMode+rdFcbrBuf
rdFcbrBufBlkBytes = rdFcbrBlkBytes-rdFcbrOpenMode+rdFcbrBuf

rdRead = *  ;( ) :.AY=(zw)=len,.Z=eof
   lda readFcb
   asl
   asl
   asl
   asl
   sta rdFcbOffset
   jsr rdSaveMpZp
   ldx rdFcbOffset
   ldy #0
-  lda rdFcbrOpenMode,x
   sta rdFcbrBuf,y
   inx
   iny
   cpy #10
   bcc -
   lda rdFcbrBufOpenMode
   cmp #"r"
   beq rdReadMore
   lda #aceErrFileNotInput

   rdReadError = *
   sta errno
   lda #0
   ldy #0
   sec
   rdReadExit = *
   php
   sta zw+0
   sty zw+1
   ldy #0
   ldx rdFcbOffset
-  lda rdFcbrBuf,y
   sta rdFcbrOpenMode,x
   inx
   iny
   cpy #10
   bcc -
   jsr rdRestoreMpZp
   lda zw+0
   ldy zw+1
   plp
   rts

   rdReadMore = *
   lda readMaxLen+0
   ora readMaxLen+1
   bne +
-  lda readLength+0
   ora readLength+1
   php
   lda readLength+0
   ldy readLength+1
   plp
   clc
   jmp rdReadExit
+  lda rdFcbrBufBlkBytes+0
   ldy rdFcbrBufBlkBytes+1
   sta rdReadChunk+0
   sty rdReadChunk+1
   ora rdReadChunk+1
   bne +
   lda #$06
   sta rdFcbrBufFilePtr+0
   lda #$00
   sta mp+0
   ldx #2
-  lda rdFcbrBufNextBlk,x
   sta rdFcbrBufFilePtr+1,x
   sta mp+1,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   beq --
   lda #<workbuf
   ldy #>workbuf
   sta zp+0
   sty zp+1
   lda #6
   ldy #0
   jsr fetch
   lda workbuf+$04
   ldy workbuf+$05
   sta rdFcbrBufBlkBytes+0
   sty rdFcbrBufBlkBytes+1
   ldx #2
-  lda workbuf+$01,x
   sta rdFcbrBufNextBlk,x
   dex
   bpl -
   jmp rdReadMore

+  lda rdReadChunk+0
   cmp readMaxLen+0
   lda rdReadChunk+1
   sbc readMaxLen+1
   bcc +
   lda readMaxLen+0
   ldy readMaxLen+1
   sta rdReadChunk+0
   sty rdReadChunk+1
+  ldx #3
-  lda rdFcbrBufFilePtr,x
   sta mp,x
   dex
   bpl -
   lda readPtr+0
   ldy readPtr+1
   sta zp+0
   sty zp+1
   lda rdReadChunk+0
   ldy rdReadChunk+1
   jsr fetch

   clc
   lda readPtr+0
   adc rdReadChunk+0
   sta readPtr+0
   lda readPtr+1
   adc rdReadChunk+1
   sta readPtr+1
   clc
   lda readLength+0
   adc rdReadChunk+0
   sta readLength+0
   lda readLength+1
   adc rdReadChunk+1
   sta readLength+1
   clc
   lda rdFcbrBufFilePtr+0
   adc rdReadChunk+0
   sta rdFcbrBufFilePtr+0
   lda rdFcbrBufFilePtr+1
   adc rdReadChunk+1
   sta rdFcbrBufFilePtr+1
   sec
   lda readMaxLen+0
   sbc rdReadChunk+0
   sta readMaxLen+0
   lda readMaxLen+1
   sbc rdReadChunk+1
   sta readMaxLen+1
   sec
   lda rdFcbrBufBlkBytes+0
   sbc rdReadChunk+0
   sta rdFcbrBufBlkBytes+0
   lda rdFcbrBufBlkBytes+1
   sbc rdReadChunk+1
   sta rdFcbrBufBlkBytes+1
   jmp rdReadMore

rdCfgOffset .buf 1
rdWriteChunk = syswork+4

rdWrite = *  ;( writeLength, writePtr ) : (zp)=dataStart
   stx rdCfgOffset
   lda configBuf+$03,x
   sta rdMallocFlags
   lda regsave+1  ;fd
   asl
   asl
   asl
   asl
   sta rdFcbOffset
   jsr rdSaveMpZp
   ldx rdFcbOffset
   ldy #0
-  lda rdFcbwOpenMode,x
   sta rdFcbwBuf,y
   inx
   iny
   cpy #16
   bcc -
   lda rdFcbwBufOpenMode
   cmp #"w"
   beq rdWriteMore
   lda #aceErrFileNotOutput

   rdWriteError = *
   sta errno
   lda #0
   ldy #0
   sec
   rdWriteExit = *
   php
   ldy #0
   ldx rdFcbOffset
-  lda rdFcbwBuf,y
   sta rdFcbwOpenMode,x
   inx
   iny
   cpy #16
   bcc -
   jsr rdRestoreMpZp
   plp
   rts

   rdWriteMore = *
   lda writeLength+0
   ora writeLength+1
   bne +
   clc
   jmp rdWriteExit
   ;** get new block if old one full
+  lda rdFcbwBufRemBytes+0
   ora rdFcbwBufRemBytes+1
   beq +
   jmp rdWriteCopy
   ;** get new block
+  ldx writeLength+1
   lda writeLength+0
   beq +
   inx
+  txa
   jsr rdVarMalloc
   bcc +
   lda #aceErrDiskFull
   jmp rdWriteError
+  tax
   lda #0
   sec
   sbc #6
   sta rdFcbwBufRemBytes+0
   txa
   sbc #0
   sta rdFcbwBufRemBytes+1
   ;** initialize new block
   stx rdNewBlkData+0
   lda #<rdNewBlkData
   ldy #>rdNewBlkData
   sta zp+0
   sty zp+1
   lda #6
   ldy #0
   jsr stash  ;necessary?
   ;** link in block, init fcb
   sec
   lda rdFcbwBufFilePtr+1
   sbc rdFcbwBufBlkPage
   sec
   sbc #1
   sta rdOldBlkData+5
   lda #250
   sta rdOldBlkData+4
   ldx #3
-  lda mp,x
   ldy rdFcbwBufFilePtr,x
   sta rdFcbwBufFilePtr,x
   sta rdOldBlkData,x
   sty mp,x
   dex
   bpl -
   lda rdFcbwBufBlkPage
   sta mp+1
   lda #$01
   sta mp+0
   lda rdFcbwBufFilePtr+1
   sta rdFcbwBufBlkPage
   lda #<rdOldBlkData+1
   ldy #>rdOldBlkData+1
   sta zp+0
   sty zp+1
   lda #5
   ldx mp+3
   cpx #aceMemNull
   bne +
   ldx #3
-  lda rdFcbwBufDirent,x
   sta mp,x
   dex
   bpl -
   clc
   lda mp+0
   adc #rdDirentHeadPtr-rdDirentBuf+1
   sta mp+0
   lda #3
+  ldy #0
   jsr stash
   lda #$06
   sta rdFcbwBufFilePtr+0

   ;** copy data into block
   rdWriteCopy = *
   ldx writeLength+0
   ldy writeLength+1
   cpx rdFcbwBufRemBytes+0
   tya
   sbc rdFcbwBufRemBytes+1
   bcc +
   ldx rdFcbwBufRemBytes+0
   ldy rdFcbwBufRemBytes+1
+  stx rdWriteChunk+0
   sty rdWriteChunk+1
   ldx #3
-  lda rdFcbwBufFilePtr,x
   sta mp,x
   dex
   bpl -
   lda writePtr+0
   ldy writePtr+1
   sta zp+0
   sty zp+1
   lda rdWriteChunk+0
   ldy rdWriteChunk+1
   jsr stash

   ;** update control variables
   clc
   lda rdFcbwBufFilePtr+0
   adc rdWriteChunk+0
   sta rdFcbwBufFilePtr+0
   lda rdFcbwBufFilePtr+1
   adc rdWriteChunk+1
   sta rdFcbwBufFilePtr+1
   sec
   lda rdFcbwBufRemBytes+0
   sbc rdWriteChunk+0
   sta rdFcbwBufRemBytes+0
   lda rdFcbwBufRemBytes+1
   sbc rdWriteChunk+1
   sta rdFcbwBufRemBytes+1
   clc
   lda writePtr+0
   adc rdWriteChunk+0
   sta writePtr+0
   lda writePtr+1
   adc rdWriteChunk+1
   sta writePtr+1
   sec
   lda writeLength+0
   sbc rdWriteChunk+0
   sta writeLength+0
   lda writeLength+1
   sbc rdWriteChunk+1
   sta writeLength+1
   clc
   lda rdFcbwBufFileSize+0
   adc rdWriteChunk+0
   sta rdFcbwBufFileSize+0
   lda rdFcbwBufFileSize+1
   adc rdWriteChunk+1
   sta rdFcbwBufFileSize+1
   bcc +
   inc rdFcbwBufFileSize+2
   bne +
   inc rdFcbwBufFileSize+3
+  jmp rdWriteMore

rdNewBlkData = * ;gets modified
   .byte $00
   .byte aceMemNull,aceMemNull,aceMemNull
   .byte $00,$00
rdOldBlkData = * ;gets modified
   .byte $00
   .byte aceMemNull,aceMemNull,aceMemNull
   .byte 250,$00

rdClose = *  ;( closeFd )
   lda closeFd
   asl
   asl
   asl
   asl
   sta rdFcbOffset
   tax
   lda rdFcbrOpenMode,x
   cmp #"w"
   beq +
   rts
+  ldy #0
-  lda rdFcbwOpenMode,x
   sta rdFcbwBuf,y
   inx
   iny
   cpy #16
   bcc -
   jsr rdSaveMpZp
   ;** tidy up last block control info
   ldx #3
-  lda rdFcbwBufFilePtr,x
   sta mp,x
   dex
   bpl -
   lda rdFcbwBufBlkPage
   sta mp+1
   lda #$01
   sta mp+0
   sec
   lda rdFcbwBufFilePtr+0
   sbc #$06
   sta rdOldBlkData+4
   lda rdFcbwBufFilePtr+1
   sbc rdFcbwBufBlkPage
   sta rdOldBlkData+5
   lda #aceMemNull
   sta rdOldBlkData+1
   sta rdOldBlkData+2
   sta rdOldBlkData+3
   lda #<rdOldBlkData+1
   ldy #>rdOldBlkData+1
   sta zp+0
   sty zp+1
   lda #5
   ldy #0
   jsr stash
   ;** tidy up directory entry
   ldx #3
-  lda mp,x
   sta rdFcbwBufFilePtr,x
   lda rdFcbwBufDirent,x
   sta mp,x
   dex
   bpl -
   lda #$00
   sta rdFcbwBufFilePtr+0
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr fetch
   lda rdDirentFlags
   and #%11110111
   sta rdDirentFlags
   ldx #3
-  lda rdFcbwBufFilePtr,x
   sta rdDirentTailPtr,x
   lda rdFcbwBufFileSize,x
   sta rdDirentBytes,x
   dex
   bpl -
   lda #rdDirentLength
   ldy #0
   jsr stash
   jsr rdRestoreMpZp
   rts

rdBloadAddr  .buf 2
rdBloadTop   .buf 2
rdBloadLen   .buf 2
rdBloadFd    .buf 1
rdBloadBytes .buf 2
rdBloadName  .buf 2
rdBloadEnd   .buf 2

rdBload = *
   ;** binary-load a file
   ;** this routine is fairly high-level and calls the regular file system
   ;** calls; make sure one fcb is free
   ldx #1
-  lda bloadAddress,x
   sta rdBloadAddr,x
   lda zp,x
   sta rdBloadName,x
   dex
   bpl -
   sec
   lda zw+0
   sta rdBloadTop+0
   sbc bloadAddress+0
   sta rdBloadLen+0
   lda zw+1
   sta rdBloadTop+1
   sbc bloadAddress+1
   sta rdBloadLen+1
   ;** (zp) already contains filename
   lda #"r"
   jsr open
   sta rdBloadFd
   bcc +
   rts
+  lda #<rdBloadBytes
   ldy #>rdBloadBytes
   sta zp+0
   sty zp+1
   lda #2
   ldy #0
   ldx rdBloadFd
   jsr read
   bcs rdBloadErrClose
   lda rdBloadAddr+0
   ldy rdBloadAddr+1
   sta zp+0
   sty zp+1
   lda rdBloadLen+0
   ldy rdBloadLen+1
   ldx rdBloadFd
   jsr read
   bcc +
   rdBloadErrClose = *
   lda errno
   pha
   lda rdBloadFd
   jsr close
   pla
   sta errno
   jsr rdBloadCleanup
   sec
   rts

+  clc
   adc rdBloadAddr+0
   sta rdBloadEnd+0
   tya
   adc rdBloadAddr+1
   sta rdBloadEnd+1
   lda #<rdBloadBytes
   ldy #>rdBloadBytes
   sta zp+0
   sty zp+1
   lda #1
   ldy #0
   ldx rdBloadFd
   jsr read
   beq +

   lda #aceErrBloadTruncated
   sta errno
   jmp rdBloadErrClose

+  lda rdBloadFd
   jsr close
   rdBloadCleanup = *
   ldx #1
-  lda rdBloadName,x
   sta zp,x
   lda rdBloadTop,x
   sta zw,x
   dex
   bpl -
   lda rdBloadEnd+0
   ldy rdBloadEnd+1
   clc
   rts

rdExtractFiletype = *  ;( stringBuffer, .X=strlen ) : .A=filetype, strBuf
   lda #"s"
   cpx #2
   bcs +
-  lda #"s"
   sec
   rts
+  lda stringBuffer-2,x
   cmp #","
   bne -
   lda #0
   sta stringBuffer-2,x
   lda stringBuffer-1,x
   clc
   rts

rdRemoveDevice .buf 1
rdRemoveBlkHdr .buf 7
rdummy .buf 1

rdRemove = *  ;( .A=device, .Y=scanPos ) : .CC=ok
   sta rdRemoveDevice
   ;** fetch filename string for dir-scan
   ldx #0
-  lda (zp),y
   sta stringBuffer,x
   beq +
   inx
   iny
   bne -
+  jsr rdExtractFiletype
   jsr rdSaveMpZp
   ;** get working directory for dir-scan
   lda rdRemoveDevice
   asl
   tay
   iny
   iny
   iny
   iny
   ldx #0
-  lda deviceTable,y
   sta rdWorkDir,x
   iny
   inx
   cpx #4
   bcc -
   ;** scan directory, get file entry
   jsr rdScanDir
   bcc +
   jsr rdRestoreMpZp
   sec
   rts
   ;** remove all blocks of file
+  ldx #3
-  lda rdDirentHeadPtr,x
   sta mp,x
   dex
   bpl -
   rdRemoveNextBlock = *
   lda mp+3
   cmp #aceMemNull
   beq rdRemoveDirent
   ;*** fetch block header
   lda #<rdRemoveBlkHdr
   ldy #>rdRemoveBlkHdr
   sta zp+0
   sty zp+1
   lda #6
   ldy #0
   jsr fetch
   ;*** free block
   lda rdRemoveBlkHdr+0
   jsr rdFree
   ;*** go to next block
   ldx #2
-  lda rdRemoveBlkHdr+1,x
   sta mp+1,x
   dex
   bpl -
   lda #0
   sta mp+0
   jmp rdRemoveNextBlock
   
   ;** mark directory entry deleted
   rdRemoveDirent = *
   lda rdDirentUsage
   and #%01111111
   sta rdDirentUsage
   ;** write out updated directory entry
   ldx #3
-  lda rdDirentPtr,x
   sta mp,x
   dex
   bpl -
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr stash
   ;** tidy up and exit
   jsr rdRestoreMpZp
   clc
   rts

rdRenameDevice .buf 1
rdRenameScan   .buf 1
rdRenameType   .buf 1
rdRenameNewName .buf 17

rdRename = *  ;( .A=device, .Y=scanPos ) : .CC=ok
   sta rdRenameDevice
   sty rdRenameScan
   ;** scan for new name (should not exist)
   ldy #0
   ldx #0
-  lda (zw),y
   sta stringBuffer,x
   beq +
   inx
   iny
   bne -
+  jsr rdExtractFiletype
   bcc +
   lda #0
+  sta rdRenameType
   lda #0
   sta stringBuffer+16
   ldx #16
-  lda stringBuffer,x
   sta rdRenameNewName,x
   dex
   bpl -
   jsr rdSaveMpZp
   ;** get working directory for new-name-dir-scan
   lda rdRenameDevice
   asl
   tay
   iny
   iny
   iny
   iny
   ldx #0
-  lda deviceTable,y
   sta rdWorkDir,x
   iny
   inx
   cpx #4
   bcc -
   ;** scan directory, get file entry
   jsr rdScanDir
   bcs +
   jsr rdRestoreMpZp
   lda #aceErrFileExists
   sta errno
   sec
   rts
   ;** scan for dirent to modify
+  lda rdZpSave+0
   ldy rdZpSave+1
   sta zp+0
   sty zp+1
   ldy rdRenameScan
   ldx #0
-  lda (zp),y
   sta stringBuffer,x
   beq +
   inx
   iny
   bne -
+  jsr rdExtractFiletype
   jsr rdSaveMpZp
   ;** scan directory, get file entry
   jsr rdScanDir
   bcc +
   jsr rdRestoreMpZp
   sec
   rts
+  ldx #0
-  lda rdRenameNewName,x
   sta rdDirentName,x
   beq +
   inx
   bne -
+  stx rdDirentNameLen
   ldx #2
-  lda rdRenameKnownTypes1,x
   cmp rdRenameType
   beq +
   dex
   bpl -
   jmp ++
+  sta rdDirentType+0
   lda rdRenameKnownTypes2,x
   sta rdDirentType+1
   lda rdRenameKnownTypes3,x
   sta rdDirentType+2
   ;** write out updated directory entry
+  ldx #3
-  lda rdDirentPtr,x
   sta mp,x
   dex
   bpl -
   lda #<rdDirentBuf
   ldy #>rdDirentBuf
   sta zp+0
   sty zp+1
   lda #rdDirentLength
   ldy #0
   jsr stash
   ;** tidy up and exit
   jsr rdRestoreMpZp
   clc
   rts

rdRenameKnownTypes1 .asc "spu"
rdRenameKnownTypes2 .asc "ers"
rdRenameKnownTypes3 .asc "qgr"

;the end + blank line

