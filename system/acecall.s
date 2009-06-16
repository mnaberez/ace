;*** ACE Kernel main file/dir/other system calls

;====== file calls ======

;*** open( zp=filenameZ, .A=mode["r","w","a","W","A"] ) : .A=fcb

aceOpenOverwrite = *
   lda #"w"
   jsr open
   bcs +
   rts
+  ldy errno
   cpy #aceErrFileExists
   beq +
   sec
   rts
+  jsr internRemove
   bcs +
   lda #"w"
   jsr open
+  rts

aceOpenForceAppend = *
   lda #"a"
   jsr open
   bcs +
   rts
+  ldy errno
   cpy #aceErrFileNotFound
   beq +
   sec
   rts
+  lda #"w"
   jsr open
   rts

openFcb      = syswork+0
openNameScan = syswork+1
openMode     = syswork+2
openNameLength = syswork+3
openDevice   = syswork+4
checkStat .buf 1

kernFileOpen = *
internOpen = *
   sta openMode
   cmp #"W"
   bne +
   jmp aceOpenOverwrite
+  cmp #"A"
   bne +
   jmp aceOpenForceAppend
+  lda #true
   sta checkStat
   jsr getLfAndFcb
   bcc +
   rts
+  sta lftable,x
   lda #$00
   sta eoftable,x
   stx openFcb
   jsr getDevice
   sty openNameScan
   ldx openFcb
   sta devtable,x
   sta openDevice
   tax

   ;get sa here
   lda configBuf+0,x
   cmp #0
   bne +
   ldy configBuf+2,x
   jmp nonDiskSa
+  ldy #0
   cmp #1
   beq openDiskSa
   ;** check console
   cmp #2
   bne +
-  lda openFcb
   clc
   rts
   ;** check null device
+  cmp #3
   beq -
   ;** check ramdisk device
   cmp #4
   bne +
   jmp rdOpen
   ;** check parallel port device
+  cmp #5
   bne +
   jmp parOpen
   ;** check SwiftLink modem device
+  cmp #6
   bne +
   jmp slOpen
   ;** illegal device
+  lda #aceErrIllegalDevice
   sta errno
   sec
   rts

   openDiskSa = *
   ldy #2
   diskSaSearch = *
   ldx #fcbCount-1
-  lda lftable,x
   bmi +
   lda devtable,x
   cmp openDevice
   bne +
   tya
   cmp satable,x
   bne +
   iny
   bne diskSaSearch
+  dex
   bpl -

   nonDiskSa = *
   ldx openFcb
   tya
   sta satable,x

   ;set the name
   ldx #0
   ldy openNameScan
-  lda (zp),y
   sta stringBuffer,x
   beq +
   iny
   inx
   bne -
+  ldy openDevice
   lda configBuf+0,y
   cmp #1
   bne nonDiskOpen
   ;** stick the mode for disk files
   cpx #0
   bne +
   lda #aceErrOpenDirectory
   sec
   rts
+  lda #","
   sta stringBuffer,x
   inx
   lda openMode
   sta stringBuffer,x
   inx
   jmp openGotName

   ;** get rid of the filename for non-disks
   nonDiskOpen = *
   ldx #0

   openGotName = *
   ;** dispatch here for non-kernel devices
   txa
   ldx #<stringBuffer
   ldy #>stringBuffer
   jsr kernelSetnam

   ;set lfs
   ldx openFcb
   lda lftable,x
   pha
   lda satable,x
   tay
   lda devtable,x
   tax
   lda configBuf+1,x
   tax
   pla
   jsr kernelSetlfs

   ;do the open
   jsr kernelOpen
   bcs openError
+  ldx openDevice
   lda configBuf+0,x
   cmp #1
   bne +
   lda checkStat
   beq +
   txa
   jsr openDiskStatus
   bcc +

   openError = *
   sta errno
   ldx openFcb
   lda lftable,x
   clc
   jsr kernelClose
   ldx openFcb
   lda #lfnull
   sta lftable,x
   sec
   lda #fcbNull
   rts
+  lda openFcb
   clc
   rts

openDiskStatus = *  ;( .A=device ) : errno=.A=errcode, .CS=errflag
   jsr cmdchOpen
   bcs +
   jsr checkDiskStatus
   php
   pha
   jsr cmdchClose
   pla
   plp
+  rts

cmdchOpen = *  ;( .A=device )
   pha
   jsr cmdchClose
   pla
   tax
   lda configBuf+1,x
   tax
   ldy #15
   lda #cmdlf
   jsr kernelSetlfs
   lda #0
   jsr kernelSetnam
   jsr kernelOpen
   bcc +
   sta errno
+  rts

cmdchClose = *
   sec
   lda #cmdlf
   jsr kernelClose
   bcc +
   sta errno
+  rts

cmdchSend = *  ;( stringBuffer )
   ldx #cmdlf
   jsr kernelChkout
   bcs cmdchErr
   ldx #0
-  lda stringBuffer,x
   beq +
   jsr kernelChrout
   bcs cmdchErr
   inx
   bne -
+  jsr kernelClrchn
   clc
   rts

   cmdchErr = *
   sta errno
   pha
   jsr kernelClrchn
   pla
   sec
   rts

checkDiskStatusCode .buf 1

checkDiskStatus = *
   ldx #cmdlf
   jsr kernelChkin
   bcs cmdchErr
   jsr kernelChrin
   bcs cmdchErr
   and #$0f
   sta checkDiskStatusCode
   asl
   asl
   adc checkDiskStatusCode
   asl
   sta checkDiskStatusCode
   jsr kernelChrin
   bcs cmdchErr
   and #$0f
   clc
   adc checkDiskStatusCode
   sta checkDiskStatusCode
-  jsr kernelReadst
   and #$80
   beq +
   lda #aceErrDeviceNotPresent
   sec
   bcs cmdchErr
+  jsr kernelChrin
   bcs cmdchErr
   cmp #chrCR
   bne -
   jsr kernelClrchn
   lda checkDiskStatusCode
   cmp #62
   bne +
   lda #aceErrFileNotFound
   sta errno
   sec
   rts
+  cmp #20
   bcc +
   sta errno
+  rts

;*** close( .A=fcb )

closeFd .buf 1

kernFileClose = *
aceClose = *
internClose = *
   tax
   lda pidtable,x
   cmp aceProcessID
   beq +
   clc
   rts
+  ldy devtable,x
   stx closeFd
   lda configBuf+0,y
   cmp #2
   bne +
   jmp closeFdEntry
+  cmp #3
   bne +
   jmp closeFdEntry
+  cmp #4
   bne +
   jsr rdClose
   jmp closeFdEntry
+  cmp #5
   bne +
   jmp parClose
+  cmp #6
   bne +
   jmp slClose
+  ldx closeFd
   lda lftable,x
   clc
   jsr kernelClose

   closeFdEntry = *
   ldx closeFd
   lda #lfnull
   sta lftable,x
   clc
   rts

;*** read( .X=fcb, (zp)=data, .AY=maxLength ) : .AY=(zw)=length, .Z=eof

readMaxLen     = syswork+0
readPtr        = syswork+2
readLength     = syswork+4
readFcb        = syswork+6
readDeviceDisk = syswork+7

kernFileRead = *
   sta readMaxLen+0
   sty readMaxLen+1
   stx readFcb
   lda zp+0
   ldy zp+1
   sta readPtr+0
   sty readPtr+1
   lda #0
   sta readLength+0
   sta readLength+1
   lda eoftable,x
   beq +
   jmp readEofExit
+  ldy #0
   lda devtable,x
   tax
   lda configBuf+0,x
   cmp #4
   bne +
   jmp rdRead
+  cmp #5
   bne +
   jmp parRead
+  cmp #6
   bne +
   jmp slRead
+  cmp #2
   bne +
   lda readMaxLen+0
   ldy readMaxLen+1
   ldx readFcb
   jmp conRead
+  cmp #3
   bne +
   lda #0
   ldy #0
   sta zw+0
   sty zw+1
   clc
   rts
+  cmp #1
   bne +
   ldy #$ff
+  ldx readFcb
   sty readDeviceDisk
   lda lftable,x
   tax
   jsr kernelChkin
   bcc readByte
   sta errno
   rts
   
   readByte = *
   lda readLength+0
   cmp readMaxLen+0
   lda readLength+1
   sbc readMaxLen+1
   bcs readExit
   jsr kernelChrin
   ldy #0
   sta (readPtr),y
   inc readPtr+0
   bne +
   inc readPtr+1
+  inc readLength+0
   bne +
   inc readLength+1
+  bit readDeviceDisk
   bpl readByte
   lda st
   and #$40
   beq readByte
   ldx readFcb
   sta eoftable,x

   readExit = *
   jsr kernelClrchn
   readExitNoclr = *
   lda readLength+0
   ldy readLength+1
   sta zw+0
   sty zw+1
   ldx #$ff
   clc
   rts

   readEofExit = *
   lda #0
   ldy #0
+  sta zw+0
   sty zw+1
   clc
   rts

;*** write( .X=fcb, (zp)=data, .AY=length )

writeLength = syswork+0
writePtr    = syswork+2

kernFileWrite = *
internWrite = *
   sta writeLength+0
   sty writeLength+1
   lda zp+0
   ldy zp+1
   sta writePtr+0
   sty writePtr+1
   stx regsave+1
   lda devtable,x
   tax
   lda configBuf+0,x
   ;** check ramdisk
   cmp #4
   bne +
   jmp rdWrite
   ;** check parallel port
+  cmp #5
   bne +
   jmp parWrite
   ;** check SwiftLink modem
+  cmp #6
   bne +
   jmp slWrite
   ;** check console
+  cmp #2
   bne +
   lda writeLength+0
   ldy writeLength+1
   ldx regsave+1
   jmp conWrite
   ;** check null device
+  cmp #3
   bne +
   clc
   rts
+  ldx regsave+1
   lda lftable,x
   tax
   jsr kernelChkout
   bcc writeByte
   rts

   writeByte = *
   lda writeLength+0
   ora writeLength+1
   beq writeFinish
   ldy #0
   lda (writePtr),y
   jsr kernelChrout
   bcc +
   sta errno
   jsr kernelClrchn
   sec
   rts
+  inc writePtr+0
   bne +
   inc writePtr+1
+  lda writeLength+0
   bne +
   dec writeLength+1
+  dec writeLength+0
   jmp writeByte
   
   writeFinish = *
   jsr kernelClrchn
   clc
   rts

;*** aceFileLseek( ... ) : ...

kernFileLseek = *
   lda #aceErrNotImplemented
   sta errno
   sec
   rts

;*** aceFileRemove( (zp)=Name )

removeDevice = syswork

kernFileRemove = *
internRemove = *
   jsr getDiskDevice
   bcc +
   rts
+  sta removeDevice
   cpx #4
   bne +
   jmp rdRemove
+  lda #"s"
   sta stringBuffer
   lda #":"
   sta stringBuffer+1
   ldx #1
   lda (zp),y
   cmp #"/"
   beq +
   ldx #2
/  lda (zp),y
   sta stringBuffer,x
   beq +
   iny
   inx
   bne -
+  lda #0
   sta stringBuffer,x
   lda removeDevice
   jsr cmdchOpen
   bcs ++
   jsr cmdchSend
   bcs +
   jsr checkDiskStatus
+  php
   jsr cmdchClose
   plp
+  rts

;*** aceFileRename( (zp)=OldName, (zw)=NewName )
;*** don't even think about renaming files outside the current directory

renameDevice = syswork+0
renameScan   = syswork+1

kernFileRename = *
   jsr getDiskDevice
   bcc +
   rts
+  sta renameDevice
   cpx #4
   bne +
   jmp rdRename
+  sty renameScan
   lda #"r"
   sta stringBuffer+0
   lda #":"
   sta stringBuffer+1
   ;** copy new name
   ldy #0
   ldx #2
-  lda (zw),y
   sta stringBuffer,x
   beq +
   iny
   inx
   bne -
+  lda #"="
   sta stringBuffer,x
   inx
   ;** copy old name
   ldy renameScan
-  lda (zp),y
   sta stringBuffer,x
   beq +
   inx
   iny
   bne -
+  lda renameDevice
   jsr cmdchOpen
   bcs ++
   jsr cmdchSend
   bcs +
   jsr checkDiskStatus
+  php
   jsr cmdchClose
   plp
+  rts

;*** aceFileBload( (zp)=Name, .AY=Address, (zw)=Limit+1 ) : .AY=End+1

bloadAddress = syswork
bloadFilename = syswork+2
bloadDevice = syswork+4

kernFileBload = *
internBload = *
   sta bloadAddress+0
   sty bloadAddress+1
   jsr getDevice
   sta bloadDevice
   tax
   clc
   tya
   adc zp+0
   sta bloadFilename+0
   lda zp+1
   adc #0
   sta bloadFilename+1
   lda configBuf+0,x
   cmp #4
   bne +
   jmp rdBload
+  cmp #1
   beq +
   lda #aceErrIllegalDevice
   sta errno
   sec
   rts
+  lda configBuf+1,x
   tax
   lda #0
   ldy #0
   jsr kernelSetlfs
   ldy #0
-  lda (bloadFilename),y
   beq +
   iny
   bne -
+  tya
   ldx bloadFilename+0
   ldy bloadFilename+1
   jsr kernelSetnam
   lda #0
   ldx bloadAddress+0
   ldy bloadAddress+1
   jsr kernelLoad
   stx bloadAddress+0
   sty bloadAddress+1
   bcc bloadOk
   pha
   cmp #aceErrDeviceNotPresent
   beq +
   ldx bloadDevice
   lda configBuf+0,x
   cmp #1
   bne +
   txa
   jsr openDiskStatus
+  pla
-  sta errno
   lda #0
   ldx #0
   ldy #0
   sec
   rts

   bloadOk = *
   ldx bloadDevice
   lda configBuf+0,x
   cmp #1
   bne +
   txa
   jsr openDiskStatus
   bcs -
+  lda bloadAddress+0
   ldy bloadAddress+1
   rts

;*** aceFileInfo( .X=Fcb ) : .A=DevType(0=con,1=char,2=disk), .X=Cols, .Y=Rows

kernFileInfo = *
   lda devtable,x
   tax
   lda configBuf+0,x
   cmp #2
   bne +
   jsr kernWinSize
   tay
   lda #0
   rts

+  ldx #80
   ldy #66
   cmp #1
   beq +
   cmp #4
   beq +
   lda #1
   rts
+  lda #2
   rts

;*** aceFileStat( ... ) : ...

kernFileStat = *
   lda #aceErrNotImplemented
   sta errno
   sec
   rts

;*** aceFileIoctl( ... ) : ...

kernFileIoctl = *
   lda #aceErrNotImplemented
   sta errno
   sec
   rts

;*** aceFileFdswap( .X=Fcb1, .Y=Fcb2 )

kernFileFdswap = *
   lda lftable,x
   pha
   lda lftable,y
   sta lftable,x
   pla
   sta lftable,y
   lda devtable,x
   pha
   lda devtable,y
   sta devtable,x
   pla
   sta devtable,y
   lda satable,x
   pha
   lda satable,y
   sta satable,x
   pla
   sta satable,y
   lda eoftable,x
   pha
   lda eoftable,y
   sta eoftable,x
   pla
   sta eoftable,y
   lda pidtable,x
   pha
   lda pidtable,y
   sta pidtable,x
   pla
   sta pidtable,y
   txa
   asl
   asl
   asl
   asl
   tax
   tya
   asl
   asl
   asl
   asl
   tay
   lda #16
   sta syswork+15
-  lda fcbTable1,x
   pha
   lda fcbTable1,y
   sta fcbTable1,x
   pla
   sta fcbTable1,y
   inx
   iny
   dec syswork+15
   bne -
   clc
   rts

;====== directory calls ======

;*** aceDirOpen( (zp)=deviceName ) : .A=fcb

kernDirOpen = *
   lda #true
   sta checkStat
   jsr getDiskDevice
   bcc +
   rts
+  sta openDevice
   sty openNameScan
   jsr getLfAndFcb
   bcc +
   rts
+  sta lftable,x
   lda openDevice
   sta devtable,x
   lda #0
   sta eoftable,x
   lda #0
   sta satable,x
   stx openFcb
   ldx openDevice
   lda configBuf+0,x
   cmp #4
   bne +
   jmp rdDirOpen
+  lda #"$"
   sta stringBuffer+0
   lda #"0"
   sta stringBuffer+1
   lda #0
   sta stringBuffer+2
   ldx #2
   jsr dirOpenSetName
   jsr openGotName
   bcc +
   rts
+  ldx openFcb
   lda lftable,x
   tax
   jsr kernelChkin
   jsr kernelChrin
   jsr kernelChrin
   jsr kernelClrchn
   lda openFcb
   clc
   rts

   dirOpenSetName = *
   ldy openDevice
   lda configBuf+3,y
   bmi dirNameDate
   ldy openNameScan
   lda (zp),y
   bne +
   rts
+  ldx #1

   dirNameNormal = *
-  lda (zp),y
   sta stringBuffer,x
   beq +
   iny
   inx
   bne -
+  dex
   lda #":"
   cmp stringBuffer,x
   beq +
   inx
   sta stringBuffer,x
+  inx
   lda #"*"
   sta stringBuffer,x
   inx
   lda #0
   sta stringBuffer,x
   rts

   dirNameDate = *
   lda #"="
   sta stringBuffer+1
   lda #"t"
   sta stringBuffer+2
   ldx #3
   ldy openNameScan
   jsr dirNameNormal
   lda #"="
   sta stringBuffer,x
   inx
   lda #"l"
   sta stringBuffer,x
   inx
   lda #0
   sta stringBuffer,x
   rts

;*** aceDirClose( ... ) : ...

kernDirClose = *
   tax
   lda pidtable,x
   cmp aceProcessID
   beq +
   clc
   rts
+  ldy devtable,x
   stx closeFd
   lda configBuf+0,y
   cmp #4
   beq +
   lda closeFd
   jmp aceClose
+  jmp rdDirClose

;*** aceDirRead( .X=fcb ) : .Z=eof, aceDirentBuffer=data

dirBlocks = syswork+0

kernDirRead = *
   ldy devtable,x
   lda configBuf+0,y
   cmp #4
   bne +
   jmp rdDirRead
+  lda lftable,x
   tax
   jsr kernelChkin
   bcc +
   lda #0
   rts
   ;** read the link
+  jsr kernelChrin
   sta syswork+4
   jsr kernelReadst
   and #$40
   bne dirreadEofExit
   jsr kernelChrin
   ora syswork+4
   bne +

   dirreadEofExit = *
   jsr kernelClrchn
   ldx #0
   rts
   dirreadErrExit = *
   sta errno
   jsr kernelClrchn
   ldx #0
   sec
   rts

   ;** read the block count
+  jsr kernelChrin
   sta dirBlocks
   sta aceDirentBytes+1
   jsr kernelChrin
   sta dirBlocks+1
   sta aceDirentBytes+2
   asl dirBlocks
   rol dirBlocks+1
   lda #0
   rol
   sta dirBlocks+2
   sec
   lda #0
   sbc dirBlocks
   sta aceDirentBytes+0
   lda aceDirentBytes+1
   sbc dirBlocks+1
   sta aceDirentBytes+1
   lda aceDirentBytes+2
   sbc dirBlocks+2
   sta aceDirentBytes+2
   ;** read the filename
   lda #0
   sta aceDirentName
   sta aceDirentNameLen
-  jsr kernelChrin
   bcs dirreadErrExit
   bit st
   bvs dirreadErrExit
   cmp #" "
   beq -
   cmp #18
   beq -
   cmp #$22
   bne dirreadExit
   ldx #0
-  jsr kernelChrin
   bcs dirreadErrExit
   bit st
   bvs dirreadErrExit
   cmp #$22
   beq +
   sta aceDirentName,x
   inx
   bne -
+  lda #0
   sta aceDirentName,x
   stx aceDirentNameLen
-  jsr kernelChrin
   cmp #" "
   beq -
   ;** read type and flags
   ldx #%01100000
   stx aceDirentFlags
   ldx #%10000000
   stx aceDirentUsage
   cmp #"*"
   bne +
   lda aceDirentFlags
   ora #%00001000
   sta aceDirentFlags
   jsr kernelChrin
+  ldx #3
   ldy #0
   jmp dirTypeFirst
-  jsr kernelChrin
   dirTypeFirst = *
   sta aceDirentType,y
   iny
   dex
   bne -
   lda #0
   sta aceDirentType+3
   lda aceDirentType
   cmp #"d"
   bne +
   lda aceDirentFlags
   ora #%10010000
   sta aceDirentFlags
   jmp dirreadExit
+  cmp #"p"
   bne dirreadExit
   lda aceDirentFlags
   ora #%00010000
   sta aceDirentFlags
   jmp dirreadExit

   dirreadExit = *
   jsr kernelChrin
   cmp #0
   bne +
   jmp dirreadRealExit
+  cmp #"<"
   bne +
   lda aceDirentFlags
   and #%11011111
   sta aceDirentFlags
+  ldx #7
   lda #0
-  sta aceDirentDate,x
   dex
   bpl -
-  jsr kernelChrin
   cmp #0
   beq dirreadRealExit
   cmp #"0"
   bcc -
   cmp #"9"+1
   bcs -

   dirreadDate = *
   jsr dirGetNumGot
   bcs dirreadRealExit
   sta aceDirentDate+2
   jsr dirGetNum
   bcs dirreadRealExit
   sta aceDirentDate+3
   jsr dirGetNum
   bcs dirreadRealExit
   sta aceDirentDate+1
   ldx #$19
   cmp #$70
   bcs +
   ldx #$20
+  stx aceDirentDate+0  ;century
   jsr dirGetNum
   bcs dirreadRealExit
   sta aceDirentDate+4
   jsr dirGetNum
   bcs dirreadRealExit
   sta aceDirentDate+5
   jsr kernelChrin
   and #$ff
   beq dirreadRealExit
   jsr kernelChrin
   and #$ff
   beq dirreadRealExit
   cmp #"a"
   bne dirreadPM

   dirreadAM = *
   lda aceDirentDate+4
   cmp #$12
   bne +
   lda #$00
   sta aceDirentDate+4
   jmp +

   dirreadPM = *
   lda aceDirentDate+4
   cmp #$12
   beq +
   clc
   sei
   sed
   adc #$12
   cld
   cli
   sta aceDirentDate+4

/  jsr kernelChrin
   cmp #0
   bne -

   dirreadRealExit = *
   jsr kernelClrchn
   ldx #$ff
   clc
   rts

   dirGetNum = *
-  jsr kernelChrin
   dirGetNumGot = *
   cmp #0
   beq +
   cmp #"0"
   bcc -
   cmp #"9"+1
   bcs -
   asl
   asl
   asl
   asl
   sta syswork+6
   jsr kernelChrin
   cmp #0
   beq +
   and #$0f
   ora syswork+6
   clc
+  rts

;*** aceDirIsdir( (zp)=FilenameZ ) : .A=Dev, .X=isDisk, .Y=isDir

kernDirIsdir = *
   jsr getDevice
   pha
   tax
   lda configBuf+0,x
   cmp #1
   beq +
   cmp #4
   beq +
   ldx #false
   ldy #false
   jmp isDirExit
   ldx #true
+  ldy #255
-  iny
   lda (zp),y
   bne -
   dey
   lda (zp),y
   ldy #true
   cmp #":"
   beq isDirExit
   ldy #false

   isDirExit = *
   pla
   rts

;*** aceDirChange( (zp)=DirName, .A=flags($80=home) )

chdirDevice = syswork+0
chdirScan   = syswork+1
chdirNameScan = syswork+2
chdirCharSave = syswork+3

kernDirChange = *
internDirChange = *
   cmp #$80
   bcc +
   lda #<configBuf+$100
   ldy #>configBuf+$100
   sta zp+0
   sty zp+1
+  jsr getDiskDevice
   bcc +
   rts
+  sty chdirNameScan
   sta chdirDevice
   cpx #4
   bne +
   jmp rdChDir
+  lda #"c"
   sta stringBuffer+0
   lda #"d"
   sta stringBuffer+1
   ldx #2
-  lda (zp),y
   sta stringBuffer,x
   beq +
   cmp #":"
   beq +
   iny
   inx
   bne -
+  lda #0
   sta stringBuffer,x
   cpx #2
   beq chdirSetName
   lda #"/"
   cmp stringBuffer-1,x
   beq +
   sta stringBuffer,x
   inx
   lda #0
   sta stringBuffer,x
+  lda chdirDevice
   jsr cmdchOpen
   bcc +
   rts
+  jsr cmdchSend
   bcs chdirAbort
   jsr checkDiskStatus
   bcs chdirAbort

   lda #"p"
   sta stringBuffer+1
   ldx #0
-  lda stringBuffer+2,x
   cmp #"0"
   bcc +
   cmp #"9"+1
   bcs +
   inx
   bne -
+  cpx #0
   beq +
   cmp #"/"
   bne +
   sta chdirCharSave
   stx chdirNameScan
   lda #0
   sta stringBuffer+2,x
   jsr cmdchSend
   bcs chdirAbort
   ldx chdirNameScan
   lda chdirCharSave
   sta stringBuffer+2,x
   jsr checkDiskStatus
   bcs chdirAbort
+  jsr cmdchClose
   lda chdirDevice
   sta aceCurrentDevice

   chdirSetName = *
   lda chdirDevice
   sta aceCurrentDevice
   lsr
   lsr
   ora #$40
   sta aceCurDirName+0
   lda #":"
   sta aceCurDirName+1
   ldx #0
-  lda stringBuffer+2,x
   sta aceCurDirName+2,x
   beq +
   inx
   bne -
+  lda aceCurDirName+1,x
   cmp #":"
   beq +
   lda #":"
   sta aceCurDirName+2,x
   inx
+  lda #0
   sta aceCurDirName+2,x
   clc
   rts

   chdirAbort = *
   jsr cmdchClose
   sec
   rts

;*** aceDirMake( (zp)=Name, .AY=minimumEntries )

mkdirDevice = syswork

kernDirMake = *
   jsr getDiskDevice
   bcc +
   rts
+  sta mkdirDevice
   lda #"m"
   sta stringBuffer+0
   lda #"d"
   sta stringBuffer+1
   lda #":"
   sta stringBuffer+2
   ldx #3
-  lda (zp),y
   sta stringBuffer,x
   beq +
   iny
   inx
   bne -
+  lda mkdirDevice
   jsr cmdchOpen
   bcs ++
   jsr cmdchSend
   bcs +
   jsr checkDiskStatus
+  php
   jsr cmdchClose
   plp
+  rts

;*** aceDirRemove( (zp)=Name )

rmdirDevice = syswork

kernDirRemove = *
   jsr getDiskDevice
   bcc +
   rts
+  sta rmdirDevice
   lda #"r"
   sta stringBuffer+0
   lda #"d"
   sta stringBuffer+1
   lda #":"
   sta stringBuffer+2
   ldx #3
-  lda (zp),y
   sta stringBuffer,x
   beq +
   iny
   inx
   bne -
+  lda rmdirDevice
   jsr cmdchOpen
   bcs ++
   jsr cmdchSend
   bcs +
   jsr checkDiskStatus
+  php
   jsr cmdchClose
   plp
+  rts

;*** aceDirName( .A=sysdir, (zp)=buf, .Y=assignLen ) : buf, .Y=len
;***   .A : 0=curDir, 1=homedir, 2=execSearchPath, 3=configSearchPath, 4=tempDir
;***   .A : $80+above: assign directory

dirnamePath   .buf 1
dirnameSet    .buf 1
dirnameSetLen .buf 1

kernDirName = *
   ldx #$00
   cmp #$80
   bcc +
   sty dirnameSetLen
   ldx #$ff
+  stx dirnameSet
   and #$07
   ldx #$ff
   cmp #2
   beq +
   cmp #3
   beq +
   ldx #$00
+  stx dirnamePath
   ldx #<aceCurDirName
   ldy #>aceCurDirName
   cmp #0
   beq +
   sec
   sbc #1
   lsr
   ror
   ror
   and #$c0
   ldy #>configBuf
   iny
   clc
   adc #<configBuf
   tax
   bcc +
   iny
+  stx syswork+0
   sty syswork+1
   bit dirnameSet
   bmi dirnameSetCopy
   ldy #0
-  lda (syswork+0),y
   sta (zp),y
   beq +
   iny
   bne -
+  bit dirnamePath
   bpl +
   iny 
   lda (syswork+0),y
   bne -
   sta (zp),y
+  rts

   dirnameSetCopy = *
   ldy #0
-  lda (zp),y
   sta (syswork+0),y
   iny
   cpy dirnameSetLen
   bcc -
   rts

;====== time calls ======

;*** aceTimeGetDate( (.AY)=dateString )  fmt:YY:YY:MM:DD:HH:MM:SS:TW
;                                             0  1  2  3  4  5  6  7

prevHour .buf 1

kernTimeGetDate = *
internGetDate = *
   php
   sei
   sta syswork+$e
   sty syswork+$f
   ldy #3
-  lda aceDate,y
   sta (syswork+$e),y
   dey
   bpl -
   ldy #4
   lda cia1+$b
   bpl +
   and #$1f
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
+  sta (syswork+$e),y
   iny
   lda cia1+$a
   sta (syswork+$e),y
   iny
   lda cia1+$9
   sta (syswork+$e),y
   iny
   lda cia1+$8
   asl
   asl
   asl
   asl
   ora aceDOW
   sta (syswork+$e),y
   ;** check for increment date
   ldy #4
   lda (syswork+$e),y
   cmp prevHour
   sta prevHour
   bcs +
   ldy #3
   lda aceDate,y
   sed
   clc
   adc #$01
   cld
   sta aceDate,y
   sta (syswork+$e),y
   ;** exit
+  plp
   clc
   rts

;*** aceTimeSetDate( (.AY)=dateString )

kernTimeSetDate = *
   sta syswork+0
   sty syswork+1
   ldy #3
-  lda (syswork),y
   sta aceDate,y
   dey
   bpl -
   ldy #4
   lda (syswork),y
   sta prevHour
   cmp #$13
   bcc +
   sei
   sed
   sec
   sbc #$12
   cld
   cli
   ora #$80
+  sta cia1+$b
   iny
   lda (syswork),y
   sta cia1+$a
   iny
   lda (syswork),y
   sta cia1+$9
   iny
   lda (syswork),y
   lsr
   lsr
   lsr
   lsr
   sta cia1+$8
   lda (syswork),y
   and #$07
   sta aceDOW
   rts

;*** aceTimeJif( .X=zpJifBuf ) : [zpJB]=jiffyCount

kernTimeJif = *
   php
   sei
   ldy #0
-  lda jiffyCount,y
   sta 0,x
   inx
   iny
   cpy #4
   bcc -
   plp
   clc
   rts

;====== miscellaneous calls ======

;*** aceMiscUtoa( $0+X=value32, (zp)=buf, .A=minLen ) : buf, .Y=len

utoaBin = syswork+2     ;(4)
utoaBcd = syswork+6     ;(5)
utoaFlag = syswork+11   ;(1)
utoaLen = syswork+12    ;(1)
utoaPos = syswork+13    ;(1)
utoaInitOff .buf 1      ;(1)

kernMiscUtoa = *
   ldy #0
   sty utoaInitOff
   cmp #0
   bne +
   lda #1
+  cmp #11
   bcc +
   sec
   sbc #10
   sta utoaInitOff
   ;.y == 0
   lda #" "
-  sta (zp),y
   iny
   cpy utoaInitOff
   bcc -
   lda #10
+  sta utoaLen
   sec
   lda #10
   sbc utoaLen
   sta utoaLen
   ldy #0
-  lda 0,x
   sta utoaBin,y
   inx
   iny
   cpy #4
   bcc - 
   ldx #4
   lda #0
-  sta utoaBcd,x
   dex
   bpl -
   sta utoaFlag
   ldy #32
   sei
   sed

   utoaNextBit = *
   asl utoaBin+0
   rol utoaBin+1
   rol utoaBin+2
   rol utoaBin+3
   ldx #4
-  lda utoaBcd,x
   adc utoaBcd,x
   sta utoaBcd,x
   dex
   bpl -
   dey
   bne utoaNextBit
   cld
   cli

   lda #10
   sta utoaPos
   ldx #0
   ldy utoaInitOff
-  lda utoaBcd,x
   jsr utoaPutHex
   inx
   cpx #5
   bcc -
   lda #0
   sta (zp),y
   rts

   utoaPutHex = *
   pha
   lsr
   lsr
   lsr
   lsr
   jsr utoaPutDigit
   pla
   and #$0f

   utoaPutDigit = *
   dec utoaPos
   beq utoaForceDigit
   cmp utoaFlag
   bne utoaForceDigit
   dec utoaLen
   bmi +
   rts
+  lda #$20
   bne utoaPoke
   utoaForceDigit = *
   ora #$30
   sta utoaFlag
   
   utoaPoke = *
   sta (zp),y
   iny
   rts

;*** aceMiscIoPeek( (zw)=ioaddr, .Y=offset ) : .A=data

kernMiscIoPeek = *
   lda #bkKernel
   sta bkSelect
   lda (zw),y
   pha
   lda #bkApp
   sta bkSelect
   pla
   rts

;*** aceMiscIoPoke( (zw)=ioaddr, .Y=offset, .A=data )

kernMiscIoPoke = *
   pha
   lda #bkKernel
   sta bkSelect
   pla
   sta (zw),y
   pha
   lda #bkApp
   sta bkSelect
   pla
   rts

;*** aceMiscCmdOpen( (zp)=DevName ) : .A=Fcb

kernMiscCmdOpen = *
   jsr getDiskDevice
   bcc +
   rts
+  sta openDevice
   jsr getLfAndFcb
   bcc +
   rts
+  sta lftable,x
   stx openFcb
   lda openDevice
   sta devtable,x
   lda #0
   sta eoftable,x
   lda #15
   sta satable,x
   stx openFcb
   lda #0
   sta stringBuffer
   lda #false
   sta checkStat
   ldx #0
   jsr openGotName
   bcc +
   rts
+  lda st
   and #$80
   beq +
   lda #aceErrDeviceNotPresent
   sta errno
   sec
   rts
+  lda openFcb
   rts

;*** aceMiscCmdClose( .A=fcb )

kernMiscCmdClose = *
   tax
   lda lftable,x
   pha
   lda #lfnull
   sta lftable,x
   pla
   sec
   jmp kernelClose

;*** aceMiscCmdSend( .X=Fcb, .AY=Cmd )

kernMiscCmdSend = *
   sta syswork+0
   sty syswork+1
   lda lftable,x
   tax
   jsr kernelChkout
   bcc +
   sta errno
   rts
+  ldy #0
-  lda (syswork),y
   beq +
   jsr kernelChrout
   bcs ++
   iny
   bne -
+  jsr kernelClrchn
   clc
   rts
+  sta errno
   jsr kernelClrchn
   sec
   rts

;*** aceMiscCmdStatus( .X=Fcb, .AY=StatusBuf ) : StatusBuf, .A=statusCode

kernMiscCmdStatus = *
   sta syswork+0
   sty syswork+1
   lda lftable,x
   tax
   jsr kernelChkin
   bcc +
-  sta errno
   jsr kernelClrchn
   sec
   rts
+  ldy #0
-  jsr kernelChrin
   bcs --
   cmp #$0d
   beq +
   sta (syswork),y
   iny
   jsr kernelReadst
   and #$40
   beq -
+  lda #0
   sta (syswork),y
   jsr kernelClrchn
   ldy #0
   lda (syswork),y
   and #$0f
   asl
   asl
   adc (syswork),y
   asl
   sta syswork+3
   iny
   lda (syswork),y
   and #$0f
   adc syswork+3
   clc
   rts

;====== support functions ======

;*** getDevice( zp=filenameZ ) : .A=device, .Y=scanPos

getDevice = *
   ldy #0
   lda (zp),y
   beq useDefault
   ldy #1
   lda (zp),y
   cmp #":"
   bne useDefault
   ldy #0
   lda (zp),y
   ldy #2
   cmp #"."
   bne +
   lda aceCurrentDevice
   jmp gotDev
+  and #$1f
   asl
   asl
   jmp gotDev
   
   useDefault = *
   lda aceCurrentDevice
   ldy #0

   gotDev = *
   rts
   
getLfAndFcb = * ;() : .X=fcb, .A=lf
   ldx #0
-  lda lftable,x
   bmi +
   inx
   cpx #fcbCount
   bcc -
   lda #aceErrTooManyFiles
   sta errno
   sec
   rts
   openLfSearch = *
+  inc newlf
   lda newlf
   and #$3f
   clc
   adc #1
   ldy #fcbCount-1
-  cmp lftable,y
   beq openLfSearch
   dey
   bpl -
   tay
   lda aceProcessID
   sta pidtable,x
   tya
   clc
   rts

devTableInit = *
   ldx #0
   lda #aceMemNull
-  sta deviceTable,x
   inx
   bne -
   rts

getDiskDevice = *  ;( (zp)=devname ) : .A=device, .Y=scan, .X=dev_t, .CC=isDisk
   jsr getDevice
   pha
   tax
   lda configBuf+0,x
   cmp #1
   bne +
-  tax
   pla
   clc
   rts
+  cmp #4
   beq -
   pla
   lda #aceErrDiskOnlyOperation
   sta errno
   sec
   rts
   
;====== experimental stuff ======

;*** aceTest( (zp)=filename ) : .A=dev#(0-31), (zw)=devRelName

testCurDir .asc "/acedev"
           .byte 0
testHomeDir .asc "/csbruce"
            .byte 0
fnameScan .buf 1
fnameSkip = fnameScan

kernTest = *
;;   lda #"/"
;;   sta stringBuffer+0
;;
;;   ;** fetch prefix+name into stringBuffer
;;   ldy #0
;;   lda (zp),y
;;   ldx #1
;;   cmp #"/"
;;   beq +++
;;   cmp #"~"
;;   bne +
;;   iny
;;-  lda testHomeDir,x
;;   sta stringBuffer,x
;;   beq ++
;;   inx
;;   bne -
;;+  nop
;;-  lda testCurDir,x
;;   sta stringBuffer,x
;;   beq +
;;   inx
;;   bne -
;;+  lda #"/"
;;   sta stringBuffer,x
;;   inx
;;+  nop
;;-  lda (zp),y
;;   sta stringBuffer,x
;;   beq +
;;   inx
;;   iny
;;   bne -
;;
;;   ;** scan for extra slashes, ".", and ".."
;;+  ldx #1
;;   ldy #1
;;   scanNext = *
;;   lda stringBuffer,x
;;   cmp #"/"
;;   bne scanCont
;;   stx fnameScan
;;-  inx
;;   lda stringBuffer,x
;;   cmp #"."
;;   beq -
;;   cmp #"/"
;;   beq +
;;   cmp #$00
;;   bne ++
;;+  clc ;sic
;;   txa
;;   sbc fnameScan
;;   beq +
;;   cmp #3
;;   bcs +
;;   cmp #1
;;   beq scanNext
;;   cpy #2
;;   bcc scanNext
;;   dey
;;-  dey
;;   lda stringBuffer,y
;;   cmp #"/"
;;   bne -
;;   iny
;;   jmp scanNext
;;+  ldx fnameScan 
;;   lda #"/"
;;   scanCont = *
;;   cmp #"/"
;;   bne +
;;   cmp stringBuffer-1,y
;;   bne +
;;   dey
;;+  sta stringBuffer,y
;;   cmp #$00
;;   beq +
;;   iny
;;   inx
;;   bne scanNext
;;+  cpy #2
;;   bcc +
;;   lda stringBuffer-1,y
;;   cmp #"/"
;;   bne +
;;   lda #$00
;;   sta stringBuffer-1,y
;;
;;   ;** search for longest match among the mounted device names
;;+  lda #<configBuf+512
;;   ldy #>configBuf+512
;;   sta syswork+14
;;   sty syswork+15
;;-  ldy #2
;;-  iny
;;   lda (syswork+14),y
;;   beq +
;;   cmp stringBuffer-3,y
;;   beq -
;;-  clc
;;   ldy #0
;;   lda (syswork+14),y
;;   adc syswork+14
;;   sta syswork+14
;;   bcc ---
;;   inc syswork+15
;;   jmp ---
;;+  lda stringBuffer-3,y
;;   cmp #$00
;;   beq +
;;   cmp #"/"
;;   beq ++
;;   cpy #5
;;   bcs -
;;+  dey
;;   lda #"/"
;;   sta stringBuffer-3,y
;;+  sec
;;   tya
;;   sbc #3
;;   tax
;;   ldy #1
;;   lda (syswork+14),y
;;   ora #$40
;;   ;** return
;;   pha
;;   ldy #0
;;-  lda stringBuffer,y
;;   sta (zw),y
;;   beq +
;;   iny
;;   bne -
;;+  pla
;;   clc
;;   rts

;blank line

