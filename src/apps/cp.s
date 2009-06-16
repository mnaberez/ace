;*** cp program ***

!src "../system/acehead.s"
!to "../../build/cp", cbm
!convtab pet

*= aceAppAddress

jmp copymain
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $60
chrQuote = $22
overwriteAllFlag = *
   !fill 1
abortFlag = *
   !fill 1

copyBufferPtr    = 2 ;(2)
copyBufferLength = 4 ;(2)
copyInFile       = 6 ;(1)
copyOutFile      = 7 ;(1)
scanPos          = 8 ;(1)
copyInName       = 10 ;(2)
copyOutName      = 12 ;(2)
copyOpenName     = 14 ;(2)
copyArg          = 16 ;(2)
lastArg          = 18 ;(2)
baseArg          = 20 ;(1)
cpErrno          = 22 ;(4)

;===copy===
copymain = *
   lda #0
   sta overwriteAllFlag
   sta abortFlag
   jsr getBufferParms
   ;** check for at least three arguments
   lda aceArgc+1
   bne +
   lda aceArgc
   cmp #3
   bcc copyUsageError
   ;** check for first argument being "-f"
+  lda #1
   sta baseArg
   lda #1
   ldy #0
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   iny
   lda (zp),y
   cmp #"f"
   bne +
   iny
   lda (zp),y
   cmp #0
   bne +
   lda #$ff
   sta overwriteAllFlag
   inc baseArg
   ;** check if destination is a directory
+  jsr getLastArg
   jsr aceDirIsdir
   cpy #0
   beq +
   jmp copyToDir
   ;** check for exactly three parameters
+  lda aceArgc+1
   bne copyUsageError
   sec
   lda aceArgc
   sbc baseArg
   cmp #2
   bne copyUsageError
   ;** get buffer parameters
   lda baseArg
   ldy #0
   jsr getarg
   lda zp
   ldy zp+1
   sta copyInName
   sty copyInName+1
   inc baseArg
   lda baseArg
   ldy #0
   jsr getarg
   lda zp
   ldy zp+1
   sta copyOutName
   sty copyOutName+1
   jsr copyfile
   rts

copyUsageError = *
   lda #<copyUsageErrorMsg
   ldy #>copyUsageErrorMsg
   ldx #stderr
   jsr fputs
   rts
   copyUsageErrorMsg = *
   !pet "usage: cp fromfile tofile"
   !byte chrCR
   !pet "       cp fromfile1 from2 ...fromN todir"
   !byte chrCR,0

copyfile = *
   ;** open files
   lda copyInName
   ldy copyInName+1
   sta zp
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   lda copyInName
   ldy copyInName+1
   jmp copyOpenError
+  sta copyInFile
copyfileOutput = *
   lda copyOutName
   ldy copyOutName+1
   sta zp
   sty zp+1
   lda #"w"
   jsr open
   bcc copyWriteOk
   lda errno
   cmp #aceErrFileExists
   beq +
-  lda copyInFile
   jsr close
   lda copyOutName
   ldy copyOutName+1
   jmp copyOpenError
+  jsr copyAskOverwrite
   beq +
   lda copyInFile
   jsr close
   sec
   rts
+  jsr copyRemoveOutfile
   jmp copyfileOutput

   copyWriteOk = *
   sta copyOutFile
   jsr copyFileContents
   lda copyOutFile
   jsr close
   lda copyInFile
   jsr close
   rts

copyAskOverwrite = *  ;() : .CS=quit, .EQ=yes, .NE=no
   lda overwriteAllFlag
   beq .cao1
   lda #0
   rts
.cao1:
   lda #<copyAskOverwriteMsg
   ldy #>copyAskOverwriteMsg
   jsr puts
   lda copyOutName
   ldy copyOutName+1
   jsr puts
   lda #<copyAskOverwriteMsg2
   ldy #>copyAskOverwriteMsg2
   jsr puts
   jsr getchar
   cmp #chrCR
   beq .cao1
   pha
-  jsr getchar
   cmp #chrCR
   bne -
   pla
   cmp #"q"
   bne +
-  lda #$ff
   sta abortFlag
   sec
   rts
+  cmp #"Q"
   beq -
   cmp #"a"
   bne +
-  lda #$ff
   sta overwriteAllFlag
   lda #"y"
+  cmp #"A"
   beq -
   cmp #"y"
   beq +
   cmp #"Y"
+  clc
   rts
   copyAskOverwriteMsg = *
   !pet "Overwrite "
   !byte chrQuote,0
   copyAskOverwriteMsg2 = *
   !byte chrQuote
   !pet " (y/n/a/q)? "
   !byte 0

copyRemoveOutfile = *
   lda copyOutName
   ldy copyOutName+1
   sta zp
   sty zp+1
   jsr aceFileRemove
   rts

copyFileContents = *
   ;** copy file contents
   lda copyBufferPtr
   ldy copyBufferPtr+1
   sta zp
   sty zp+1
-  jsr checkstop
   lda copyBufferLength
   ldy copyBufferLength+1
   ldx copyInFile
   jsr read
   bcs .cfc1
   beq +
   pha
   tya
   pha
   jsr checkstop
   pla
   tay
   pla
   ldx copyOutFile
   jsr write
   bcc -
   bcs copyFileError
+  rts
.cfc1:
+  jmp copyFileError
   
   copyOpenError = *
   ldx errno
   stx cpErrno+0
   sta copyOpenName
   sty copyOpenName+1
   lda #<copyOpenErrorMsg1
   ldy #>copyOpenErrorMsg1
   ldx #stderr
   jsr fputs
   lda copyOpenName
   ldy copyOpenName+1
   ldx #stderr
   jsr fputs
   lda #<copyOpenErrorMsg2
   ldy #>copyOpenErrorMsg2
   ldx #stderr
   jsr fputs
   lda #0
   sta cpErrno+1
   sta cpErrno+2
   sta cpErrno+3
   lda #<cpNumbuf
   ldy #>cpNumbuf
   sta zp+0
   sty zp+1
   ldx #cpErrno
   lda #1
   jsr aceMiscUtoa
   lda #<cpNumbuf
   ldy #>cpNumbuf
   ldx #stderr
   jsr fputs
   lda #<copyOpenErrorMsg3
   ldy #>copyOpenErrorMsg3
   ldx #stderr
   jsr fputs
   rts

   cpNumbuf = *
      !fill 12
   copyOpenErrorMsg1 = *
      !pet "Error opening file "
      !byte chrQuote
      !byte 0
   copyOpenErrorMsg2 = *
      !byte chrQuote
      !pet ", code "
      !byte 0
   copyOpenErrorMsg3 = *
      !byte chrCR
      !byte 0

   copyFileError = *
   lda #<copyFileErrorMsg
   ldy #>copyFileErrorMsg
   ldx #stderr
   jmp fputs
   copyFileErrorMsg = *
      !pet "File data error!"
      !byte chrCR
      !byte 0

copyToDir = *
   lda baseArg
   ldy #0
   sta copyArg+0
   sty copyArg+1
-  lda aceArgc+0
   ldy aceArgc+1
   sec
   sbc #1
   bcs +
   dey
+  cmp copyArg+0
   bne +
   cpy copyArg+1
   beq copyToDirExit
+  jsr checkstop
   lda copyArg+0
   ldy copyArg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta copyInName+0
   sty copyInName+1
   jsr copyFileToDir
   lda abortFlag
   bne copyToDirStopped
   inc copyArg+0
   bne +
   inc copyArg+1
+  jmp -

copyToDirExit = *
   rts

checkstop = *
   jsr aceConStopkey
   bcs +
   rts
copyToDirStopped = *
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit

   stoppedMsg = *
   !pet "<Stopped>"
   !byte chrCR,0

copyFileToDir = *
   ;** generate output file name
   jsr getLastArg
   ldy #0
-  lda (zp),y
   beq +
   sta copyNameBuf,y
   iny
   bne -
+  tya
   tax
   ;** extract basename
   ldy #0
   sty scanPos
-  lda (copyInName),y
   beq +
   cmp #":"
   bne basenameNext
   iny
   sty scanPos
   dey
   basenameNext = *
   iny
   bne -

+  ldy scanPos
-  lda (copyInName),y
   sta copyNameBuf,x
   beq +
   inx
   iny
   bne -
   ;** copy file
+  lda #<copyNameBuf
   ldy #>copyNameBuf
   sta copyOutName+0
   sty copyOutName+1
   jsr copyToDirStatus
   jsr copyfile
   rts

nameSpace = *
   !fill 1

copyToDirStatus = *
   lda copyInName+0
   ldy copyInName+1
   jsr puts

   ldy #255
-  iny
   lda (copyInName),y
   bne -
   tya
-  sec
   sbc #10
   bcs -
   adc #10
   sta nameSpace
   sta nameSpace
   sec
   lda #10
   sbc nameSpace
   sta nameSpace

-  lda #" "
   jsr putchar
   dec nameSpace
   bne -

   lda copyOutName+0
   ldy copyOutName+1
   jsr puts
   lda #chrCR
   jsr putchar
   rts

;******** standard library ********
eputs = *
   ldx #stderr
   jmp fputs
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

getchar = *
   ldx #stdin
getc = *
   lda #<getcBuffer
   ldy #>getcBuffer
   sta zp
   sty zp+1
   lda #1
   ldy #0
   jsr read
   beq +
   lda getcBuffer
   rts
+  sec
   rts
   getcBuffer = *
      !fill 1

;===copy library===
getBufferParms = *
   lda #<cpEnd
   ldy #>cpEnd
   sta copyBufferPtr+0
   sty copyBufferPtr+1
   sec
   lda aceMemTop+0
   sbc copyBufferPtr+0
   sta copyBufferLength+0
   lda aceMemTop+1
   sbc copyBufferPtr+1
   sta copyBufferLength+1
   bcc +
   rts
+  lda #"!"
   jmp putchar

getarg = *
   sty zp+1
   asl
   rol zp+1
   clc
   adc aceArgv+0
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
   rts

getLastArg = *
   lda aceArgc+0
   ldy aceArgc+1
   sec
   sbc #1
   bcs +
   dey
+  jmp getarg

;===the end===
cpBss = *
copyNameBuf = cpBss+0
cpEnd = cpBss+256
