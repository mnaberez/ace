;ACE command shell by Craig Bruce -- now an external program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:sh"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0 ;*stack,reserved

libwork = $60
chrQuote = 34
true = $ff
false = $00

;** these five fields need to be saved and
;** restored between exec calls, into mp
shellRedirectStdin  .buf 1
shellRedirectStdout .buf 1
shellRedirectStderr .buf 1
inputFd             .buf 1
suppressPromptFlag  .buf 1

stackPtr = $60
name     = $62
parseArgc .buf 2
parseArgv .buf 2
shellExitFlag .buf 1
shellPromptFlag .buf 1
checkPromptFlag .buf 1
abortCommandFlag .buf 1
regsave .buf 3

;******** shell ********

main = *
   lda #stdin
   sta inputFd
   lda #1
   ldy #0
   jsr getarg
   bne +
   jmp shell
   ;** check for flags
+  ldy #0
   lda (zp),y
   cmp #"-"
   beq +
   lda #1
   ldy #0
   jmp mainCont
   ;** check for "-v" flag
+  ldy #0
-  iny
   lda (zp),y
   cmp #"v"
   bne +
   tya
   pha
   lda #<shellTitle
   ldy #>shellTitle
   jsr eputs
   lda #1
   ldy #0
   jsr getarg
   pla
   tay
   jmp -
   ;** check for "-i" flag
+  cmp #"i"
   bne +
   tya
   pha
   jsr ashrc
   lda #1
   ldy #0
   jsr getarg
   pla
   tay
   jmp -
+  lda #2
   ldy #0
   ;xx shift the arguments when you get that far
   ;** try to get filename to execute
   mainCont = *
   ldx #stdin
   stx inputFd
   jsr getarg
   bne +
   jmp shell
+  lda #"r"
   jsr open
   bcs +
   sta inputFd
   ;xx shift the arguments when you get that far
   ;** file will close when shell exits
   jmp shell
   ;** error opening shell script--abort
+  ldx #stderr
   jsr zpputs
   lda #<scriptOpenError
   ldy #>scriptOpenError
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit

shellTitle .byte "ACE Shell vers 0.92 - by CSB 17-Dec-95",chrCR,chrCR,0
scriptOpenError .byte ": cannot open shell script for execution",chrCR,0

ashrc = *
   ;xx get home-directory name
   lda #<cmdBuffer
   ldy #>cmdBuffer
   sta zp+0
   sty zp+1
   nop ;jsr gethomedir
   ldy #0 ;xx
   ;** append ".ashrc" filename
   ldx #0
-  lda ashrcName,x
   sta cmdBuffer,y
   beq +
   iny
   inx
   bne -
   ;** open file
+  lda #"r"
   jsr open
   bcs +
   sta inputFd
   ;** execute shell as same process
   jsr shell
   ;** close and return
   lda inputFd
   jmp close
   ;** handle error
+  ldx #stderr
   jsr zpputs
   lda #<scriptOpenError
   ldy #>scriptOpenError
   jmp eputs

   ashrcName .asc ".ashrc"
             .byte 0
   ashrcOpenError .asc ": cannot open "
             .byte chrQuote
             .asc ".ashrc"
             .byte chrQuote
             .asc "script for execution"
             .byte chrCR,0

shell = *
   lda #$ff
   sta checkPromptFlag
   sta shellRedirectStdin
   sta shellRedirectStdout
   sta shellRedirectStderr
   lda #0
   sta suppressPromptFlag

   getCommand = *
   lda #0
   sta abortCommandFlag
   lda checkPromptFlag
   beq +
   jsr shellCheckPromptability
+  lda shellPromptFlag
   beq +
   lda suppressPromptFlag
   bne +
   lda #<argBuffer
   ldy #>argBuffer
   sta zp+0
   sty zp+1
   lda #0
   jsr aceDirName
   lda #"["
   ldx #stderr
   jsr putc
   lda #<argBuffer
   ldy #>argBuffer
   jsr eputs
   lda #<shellReady2
   ldy #>shellReady2
   jsr eputs
+  lda #0
   sta suppressPromptFlag
   sta shellExitFlag
   lda aceMemTop+0
   ldy aceMemTop+1
   sta stackPtr+0
   sty stackPtr+1
   jsr shellGetArgs
   bcs shellFinish
   lda parseArgc+0
   ora parseArgc+1
   beq +
   lda abortCommandFlag
   bne +
   jsr setupRedirects
   jsr shellConstructFrame
   jsr shellExecCommand
   lda #$ff
   sta checkPromptFlag
   jsr shellRemoveFrame
   jsr unsetRedirects
+  jsr closeRedirects
   bit shellExitFlag
   bmi shellFinish
   jmp getCommand

   shellFinish = *
   rts

shellReady2 .asc "] "
            .byte 0

shellCheckPromptability = *
   ldx inputFd
   jsr aceFileInfo
   ldx #$ff
   cmp #0
   beq +
   ldx #0
+  stx shellPromptFlag
   lda #0
   sta checkPromptFlag
   rts

;=== command parsing ===

argPtr = $02
argQuote = $03
argWasQuoted = $04

shellGetArgChar = *
   ldx inputFd
   jmp getc

shellGetArgs = *
   lda #0
   sta parseArgc+0
   sta parseArgc+1

   newarg = *
   jsr shellGetArgChar
   bcc +
   jmp argEof
+  cmp #" "
   beq newarg
   cmp #chrTAB
   beq newarg
   cmp #chrCR
   bne +
   jmp argEndOfLine
+  cmp #";"
   bne +
   lda #$ff
   sta suppressPromptFlag
   jmp argEndOfLine
+  cmp #"#"
   bne ++
-  jsr shellGetArgChar
   bcc +
   jmp argEof
+  cmp #chrCR
   bne -
   jmp argEndOfLine

+  ldx #0
   stx argPtr
   stx argWasQuoted
+  cmp #"\"
   bne ++
   jsr shellGetArgChar
   bcc +
   jmp argEof
+  cmp #chrCR
   beq newarg
   jmp ++

+  nop

   argNewQuote = *
   ldx #0
   stx argQuote
   cmp #$22
   beq argStartQuote
   cmp #"'"
   bne +
   argStartQuote = *
   sta argQuote
   sta argWasQuoted
   jmp argNextChar

+  ldx argPtr
   sta argBuffer,x
   inc argPtr

   argNextChar = *
   jsr shellGetArgChar
   bcs argEof
   ldx argQuote
   bne argQuoteMode
   cmp #" "
   beq argProcess
   cmp #chrTAB
   beq argProcess
-  cmp #";"
   bne +
   ldx argWasQuoted
   bne +
   lda #$ff
   sta suppressPromptFlag
   lda #chrCR
+  cmp #chrCR
   beq argProcess
   ldx argPtr
   sta argBuffer,x
   inc argPtr
   jmp argNextChar

   argQuoteMode = *
   cmp #0
   beq -
   cmp argQuote
   bne -
   jsr shellGetArgChar
   bcs argEof
   cmp #" "
   beq argProcess
   cmp #chrTAB
   beq argProcess
   cmp #chrCR
   beq argProcess
   jmp argNewQuote

   argProcess = *
   pha
   ldx argPtr
   lda #0
   sta argBuffer,x
   jsr shellHandleArg
   pla
   cmp #chrCR
   beq argEndOfLine
   jmp newarg
   argEndOfLine = *
   clc
   argEof = *
   rts

shellHandleArg = *
   lda abortCommandFlag
   beq +
   rts
+  lda argWasQuoted
   bne +
   ldx #stdin
   ldy #"r"
   lda argBuffer
   cmp #"<"
   beq shellHandleRedirect
   ldx #stdout
   ldy #"W"
   cmp #">"
   beq shellHandleRedirect
   jsr checkWildcards
   bcc +
   rts
+  jsr shellStoreArg
   rts

shellStoreArg = *
   lda stackPtr+0
   ldy stackPtr+1
   clc
   sbc argPtr
   bcs +
   dey
+  sta stackPtr+0
   sty stackPtr+1
   sta zp+0
   sty zp+1
   ldy #0
-  lda argBuffer,y
   sta (zp),y
   beq +
   iny
   bne -
+  lda parseArgc+1
   sta zp+1
   lda parseArgc+0
   asl
   rol zp+1
   clc
   adc #<argArgvBuffer
   sta zp+0
   lda zp+1
   adc #>argArgvBuffer
   sta zp+1
   ldy #0
   lda stackPtr+0
   sta (zp),y
   iny
   lda stackPtr+1
   sta (zp),y
   inc parseArgc+0
   bne +
   inc parseArgc+1
+  rts

shellHandleRedirect = *   ;( .X=fd, .Y=mode )
   lda #<argBuffer+1
   sta zp+0
   lda #>argBuffer+1
   sta zp+1
   lda argBuffer+1
   cmp #">"
   bne +
   jsr shellRedirInc
   ldy #"A"
   lda argBuffer+2
+  cmp #"!"
   bne +
-  ldx #stderr
   jsr shellRedirInc
   lda #0
+  cmp #"&"
   beq -
   lda shellRedirectStdin,x
   cmp #255
   bne redirectMultiError
   tya
   stx cmdBuffer
   sta regsave
   jsr open
   bcs redirectError
   ldx cmdBuffer
   sta shellRedirectStdin,x
   rts

redirectError = *
   lda #<redirectErrorMsg
   ldy #>redirectErrorMsg
redirectErrorWmsg = *
   pha
   tya
   pha
   lda #$ff
   sta abortCommandFlag
   lda zp+0
   ldy zp+1
   jsr eputs
   pla
   tay
   pla
   jsr eputs
   rts

   redirectErrorMsg = *
   .asc ": Error opening redirection file."
   .byte chrCR,0

redirectMultiError = *
   lda #<redirectMultiErrorMsg
   ldy #>redirectMultiErrorMsg
   jmp redirectErrorWmsg

   redirectMultiErrorMsg = *
   .asc ": Error - Multiple redirections of same stream."
   .byte chrCR,0

shellRedirInc = *
   inc zp+0
   bne +
   inc zp+1
+  rts

shellSetupRed = 2

setupRedirects = *
unsetRedirects = *
   ldx #0
   stx shellSetupRed
-  lda shellRedirectStdin,x
   cmp #255
   beq +
   tay
   jsr aceFileFdswap
+  inc shellSetupRed
   ldx shellSetupRed
   cpx #3
   bcc -
   rts

shellCloseRed = 2

closeRedirects = *
   ldx #0
   stx shellCloseRed
-  lda shellRedirectStdin,x
   cmp #$ff
   beq +
   jsr close
   ldx shellCloseRed
   lda #$ff
   sta shellRedirectStdin,x
+  inc shellCloseRed
   ldx shellCloseRed
   cpx #3
   bcc -
   rts

wildPrefix = 10
wildSuffix = 11

checkWildcards = *
   lda #255
   sta wildPrefix
   sta wildSuffix
   ldx argPtr
-  dex
   cpx #255
   beq +
   lda argBuffer,x
   cmp #":"
   beq +
   cmp #"*"
   bne -
   ldy wildSuffix
   cpy #255
   bne -
   stx wildSuffix
   inc wildSuffix
   jmp -
+  inx
   stx wildPrefix
   lda wildSuffix
   cmp #255
   bne +
   clc
   rts
+  jsr handleWildcards
   sec
   rts

wildLength = 12
wildSuffixLength = 13
wildFcb = 14
wildMatch = 15

handleWildcards = *
   lda #0
   sta wildMatch
   ldx argPtr
   inx
-  dex
   lda argBuffer,x
   sta cmdBuffer+1,x
   cpx wildPrefix
   bne -
   lda #0
   sta cmdBuffer,x
   sta argBuffer,x
   ldx wildSuffix
   sta cmdBuffer,x
   inc wildPrefix
   inc wildSuffix
   ldx #0
-  lda argBuffer,x
   beq +
   sta cmdBuffer,x
   inx
   bne -
+  sec
   lda argPtr
   sbc wildSuffix
   sta wildSuffixLength
   inc wildSuffixLength
   sec
   lda argPtr
   sbc wildPrefix
   sta wildLength

   lda #<cmdBuffer
   ldy #>cmdBuffer
   sta zp+0
   sty zp+1
   jsr aceDirOpen
   bcs noMatch
   sta wildFcb
   ldx wildFcb
   jsr aceDirRead
   bcs +
   beq +
   jsr scanWildcard
+  lda wildFcb
   jsr aceDirClose
   lda wildMatch
   bne +
   noMatch = *
   lda #$ff
   sta abortCommandFlag
   lda #<noMatchMsg
   ldy #>noMatchMsg
   jsr eputs
+  rts

noMatchMsg = *
   .asc "No match for wildcard"
   .byte chrCR,0

scanWildcard = *
   ldx wildFcb
   jsr aceDirRead
   bcs +
   bne ++
+  rts
+  lda aceDirentName
   bne +
   rts
+  lda aceDirentUsage
   and #%00010000
   bne scanWildcard
   lda aceDirentNameLen
   cmp wildLength
   bcc scanWildcard
   ldx wildPrefix
   ldy #0
   jsr substrCmp
   bcs scanWildcard
   ldx wildSuffix
   sec
   lda aceDirentNameLen
   sbc wildSuffixLength
   tay
   jsr substrCmp
   bcs scanWildcard

   ldx #0
-  lda cmdBuffer,x
   beq +
   sta argBuffer,x
   inx
   bne -
+  ldy #0
-  lda aceDirentName,y
   sta argBuffer,x
   beq +
   inx
   iny
   bne -
+  lda aceDirentType
   cmp #"s"
   beq +
   sta argBuffer+1,x
   lda #","
   sta argBuffer,x
   inx
   inx
   lda #0
   sta argBuffer,x
+  stx argPtr
   jsr shellStoreArg
   lda #$ff
   sta wildMatch
   jmp scanWildcard

substrCmp = *  ;( .X=cmdbufOff, .Y=direntNameOff ) : .CC=match
-  lda cmdBuffer,x
   bne +
   clc
   rts
+  cmp aceDirentName,y
   bne +
   iny
   inx
   bne -
+  sec
   rts

;=== stack management ===

frameArgvSource = $02
frameArgvDest = $04
frameArgvBytes = $06

shellConstructFrame = *
   ;** push the ZERO trailer argv
   sec
   lda stackPtr+0
   sbc #2
   sta stackPtr+0
   bcs +
   dec stackPtr+1
+  ldy #0
   lda #0
   sta (stackPtr),y
   iny
   sta (stackPtr),y

   ;** push argv[] array here
   lda parseArgc+0
   ldy parseArgc+1
   sty frameArgvBytes+1
   asl
   sta frameArgvBytes+0
   rol frameArgvBytes+1
   sec
   lda stackPtr+0
   sbc frameArgvBytes+0
   sta stackPtr+0
   sta frameArgvDest+0
   lda stackPtr+1
   sbc frameArgvBytes+1
   sta stackPtr+1
   sta frameArgvDest+1
   lda #<argArgvBuffer
   ldy #>argArgvBuffer
   sta frameArgvSource+0
   sty frameArgvSource+1
-  lda frameArgvBytes+0
   ora frameArgvBytes+1
   beq frameSetArgvPtr
   ldy #0
   lda (frameArgvSource),y
   sta (frameArgvDest),y
   inc frameArgvSource+0
   bne +
   inc frameArgvSource+1
+  inc frameArgvDest+0
   bne +
   inc frameArgvDest+1
+  lda frameArgvBytes+0
   bne +
   dec frameArgvBytes+1
+  dec frameArgvBytes+0
   jmp -

   ;** set argv pointer
   frameSetArgvPtr = *
   lda stackPtr+0
   ldy stackPtr+1
   sta parseArgv+0
   sty parseArgv+1
   ;** put un-redirection info into [mp]
   lda shellRedirectStdin
   ldx shellRedirectStdout
   ldy shellRedirectStderr
   sta mp+0
   stx mp+1
   sty mp+2
   lda inputFd
   sta mp+3
   rts

shellRemoveFrame = *
   ;** recover un-redirection info from [mp]
   lda mp+0
   ldx mp+1
   ldy mp+2
   sta shellRedirectStdin
   stx shellRedirectStdout
   sty shellRedirectStderr
   lda mp+3
   sta inputFd
   rts

;=== dispatch ===

dispArgv = $02
dispArgPtr = $04
dispVector = $02

shellExecCommand = *
   ;** fetch the command name
   lda parseArgv+0
   ldy parseArgv+1
   sta dispArgv+0
   sty dispArgv+1
   ldy #1
-  lda (dispArgv),y
   sta dispArgPtr,y
   sta name,y
   dey
   bpl -
   ldy #0
-  lda (dispArgPtr),y
   sta argBuffer,y
   beq +
   iny
   bne -

   ;** search internal dispatch table for name
+  ldy #0
   dispCmpCommand = *
   lda dispTable,y
   beq shellLoadExternal
   ldx #0
-  lda argBuffer,x
   cmp dispTable,y
   bne +
   cmp #0
   beq dispMatch
   inx
   iny
   bne -
   brk
+  dey
-  iny
   lda dispTable,y
   bne -
   iny
   iny
   iny
   jmp dispCmpCommand

   dispMatch = *
   lda suppressPromptFlag
   pha
   lda dispTable+1,y
   pha
   lda dispTable+2,y
   tay
   pla
   jsr dispSetup
   jsr aceProcExecSub
   pla
   sta suppressPromptFlag
   rts

   dispSetup = *  ;( (.AY)=zp contents ) : zp, zw, .AY=argc
   sta zp+0
   sty zp+1
   lda parseArgv+0
   ldy parseArgv+1
   sta zw+0
   sty zw+1
   lda parseArgc+0
   ldy parseArgc+1
   rts

;** load external file into transient program area
loadPath = 2
loadPathPos = 4
loadGiveUp = 7

shellLoadExternal = *
   lda suppressPromptFlag
   pha
   lda name+0
   ldy name+1
   jsr dispSetup
   jsr aceProcExec
   bcs +
   pla
   sta suppressPromptFlag
   rts
+  pla
   sta suppressPromptFlag
   lda errno
   pha
   lda name+0
   ldy name+1
   jsr eputs
   pla
   cmp #aceErrFileNotFound
   beq dispCmdNotFound
   cmp #aceErrBadProgFormat
   beq dispBadProg
   lda #<dispLoadErrorMsg1
   ldy #>dispLoadErrorMsg1
   jmp eputs

   dispBadProg = *
   lda #<dispBadProgMsg
   ldy #>dispBadProgMsg
   jmp eputs

   dispCmdNotFound = *
   lda #<dispLoadErrorMsg2
   ldy #>dispLoadErrorMsg2
   jmp eputs
   dispLoadErrorMsg1 = *
   .asc ": External-program load error"
   .byte chrCR,0
   dispLoadErrorMsg2 = *
   .asc ": Command not found"
   .byte chrCR,0
   dispBadProgMsg = *
   .asc ": Bad external-program format"
   .byte chrCR,0

;===internal command name and dispatch table===

dispTable = *
.asc "echo"
.byte 0
.word echo
;xx .asc "sh"
;xx .byte 0
;xx .word shellCmd  ;this must be an external call for the time being
.asc "cd"
.byte 0
.word cd
.asc "cat"
.byte 0
.word cat
.asc "cls"
.byte 0
.word cls
.asc "clear"
.byte 0
.word cls
.asc "exit"
.byte 0
.word shellExit
.asc "x"
.byte 0
.word shellExit
.asc "@"
.byte 0
.word dos
.asc "dos"
.byte 0
.word dos
.asc "path"
.byte 0
.word path
.asc "ls"
.byte 0
.word dirMain
.asc "clsl"
.byte 0
.word dirCmdClsl
.asc "dir"
.byte 0
.word dirCmdDir
.asc "d"
.byte 0
.word dirCmdD

.byte 0

;===echo===

echoArgv = $02
echoSpace = $04
echoTemp = $06

echo = *
   lda #0
   sta echoSpace
   lda aceArgv+0
   ldy aceArgv+1

   echoNewArg = *
   clc
   adc #2
   bcc +
   iny
+  sta echoArgv+0
   sty echoArgv+1
   ldy #0
   lda (echoArgv),y
   sta echoTemp+0
   iny
   lda (echoArgv),y
   sta echoTemp+1
   ora echoTemp+0
   beq echoExit
   lda #" "
   cmp echoSpace
   bne +
   jsr putchar
+  lda #" "
   sta echoSpace
   lda echoTemp+0
   ldy echoTemp+1
   jsr puts
   lda echoArgv+0
   ldy echoArgv+1
   jmp echoNewArg
   
   echoExit = *
   lda #chrCR
   jsr putchar
   rts

;===copy parameters===

copyBufferPtr = $02
copyBufferLength = $04

getBufferParms = *
   lda #<copyBuffer
   ldy #>copyBuffer
   sta copyBufferPtr+0
   sty copyBufferPtr+1
   sec
   lda aceMemTop+0
   sbc copyBufferPtr+0
   sta copyBufferLength+0
   lda aceMemTop+1
   sbc copyBufferPtr+1
   sta copyBufferLength+1
   rts

;===cd===

cdScanSave .buf 1

cd = *
   lda aceArgc+0
   cmp #2
   lda aceArgc+1
   sbc #0
   bcs +
   lda #$80
   jsr aceDirChange
   rts
+  lda #1
   ldy #0
   jsr getarg

   cdSetDevice = *
   ldx #2
   ldy #0
   lda (zp),y
   sta argBuffer+0
   iny
   lda (zp),y
   sta argBuffer+1
   iny
   cmp #":"
   bne +
   lda argBuffer+0
   cmp #$40
   bcc +
   cmp #$60
   bcc cdCheckPath
+  ldx #0
   ldy #0

   cdCheckPath = *
   sty cdScanSave
-  lda (zp),y
   cmp #"0"
   bcc +
   cmp #"9"+1
   bcs +
   iny
   bne -
+  sta argBuffer,x
   cmp #0
   bne +
   cpy cdScanSave
   beq cdOkay
+  ldy cdScanSave
   cmp #"/"
   beq cdPathOkay
   cmp #":"
   beq cdPathOkay
+  lda #"/"
   sta argBuffer,x
   inx

   cdPathOkay = *
-  lda (zp),y
   sta argBuffer,x
   beq cdCheckEnd
   inx
   iny
   bne -

   cdCheckEnd = *
   dex
   lda argBuffer,x
   cmp #":"
   beq +
   inx
   lda #":"
   sta argBuffer,x
   lda #0
   sta argBuffer+1,x
+  dex
   lda argBuffer,x
   cmp #"/"
   beq cdOkay
   lda #"/"
   sta argBuffer+1,x
   lda #":"
   sta argBuffer+2,x
   lda #0
   sta argBuffer+3,x

   cdOkay = *
   lda #<argBuffer
   ldy #>argBuffer
   sta zp+0
   sty zp+1
   lda #$00
   jsr aceDirChange
   bcs +
   rts
+  lda #<cdErrMsg
   ldy #>cdErrMsg
   jmp eputs

cdErrMsg = *
   .asc "Error changing directory"
   .byte chrCR,0

;===cat===

catBufferPtr = $02
catBufferLength = $04
catArg = $06
catFcb = $08
catAbort = 10

cat = *
   lda #0
   sta catAbort
   jsr getBufferParms
   ldx #stdout
   jsr aceFileInfo
   cmp #0
   bne +
   lda catBufferLength+1
   beq +
   lda #<254
   ldy #>254
   sta catBufferLength+0
   sty catBufferLength+1
+  lda #1
   ldy #0
   sta catArg+0
   sty catArg+1
   lda aceArgc+0
   cmp #2
   lda aceArgc+1
   sbc #0
   bcs catFiles
   lda #0
   sta catFcb
   jmp catFile

   catFiles = *
   lda catArg+0
   ldy catArg+1
   jsr getarg
   lda #"r"
   jsr open
   bcc +
   lda zp+0
   ldy zp+1
   jsr eputs
   lda #<catErrMsg
   ldy #>catErrMsg
   jsr eputs
   jmp ++
+  sta catFcb
   jsr catFile
   lda catFcb
   jsr close
+  inc catArg
   bne +
   inc catArg+1
+  lda catAbort
   bne +
   lda catArg
   cmp aceArgc
   lda catArg+1
   sbc aceArgc+1
   bcc catFiles
+  rts

catErrMsg = *
   .asc ": cannot open"
   .byte chrCR,0

catFile = *
   lda catBufferPtr
   ldy catBufferPtr+1
   sta zp
   sty zp+1
-  lda catBufferLength
   ldy catBufferLength+1
   ldx catFcb
   jsr read
   beq +
   bcs +
   ldx #1
   jsr write
   bcs +
   jsr aceConStopkey
   bcs printStoppedMsg
   jmp -
+  rts

printStoppedMsg = *
   lda #$ff
   sta catAbort
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jmp eputs
   stoppedMsg = *
   .asc "<Stopped>"
   .byte chrCR,0

;===exit===

shellExit = *
   lda #$ff
   sta shellExitFlag
   rts

;===dos===

dosFcb = $02

dos = *
   ;** open command channel
   lda #<dosCurDevice
   ldy #>dosCurDevice
   sta zp
   sty zp+1
   jsr aceMiscCmdOpen
   bcc +
   rts
+  sta dosFcb

   ;** send command
   lda #1
   ldy #0
   jsr getarg
   lda zp
   ora zp+1
   beq dosStatusOnly
   lda zp
   ldy zp+1
   ldx dosFcb
   jsr aceMiscCmdSend
   bcs +

   ;** read status
   dosStatusOnly = *
   lda #<cmdBuffer
   ldy #>cmdBuffer
   ldx dosFcb
   jsr aceMiscCmdStatus
   bcs +
   lda #<cmdBuffer
   ldy #>cmdBuffer
   jsr puts
   lda #chrCR
   jsr putchar

   ;** close command channel
+  lda dosFcb
   jmp aceMiscCmdClose
   
dosCurDevice .asc ".:"
             .byte 0

;===shell===

shellCmd = *
   jsr main
   lda #0
   sta shellExitFlag
   lda #255
   sta checkPromptFlag
   rts

;===path===

pathPos = 4
pathArg = 6
pathSourcePos = 7

path = *
   lda #0
   sta pathPos
   lda aceArgc+1
   beq +
   rts
+  lda aceArgc
   cmp #2
   bcs pathSet
   lda #<pathMsg
   ldy #>pathMsg
   jsr puts
   lda #$00
   sta argBuffer+0
   sta argBuffer+1
   lda #<argBuffer
   ldy #>argBuffer
   sta zp+0
   sty zp+1
   lda #2
   jsr aceDirName
   
   displayPath = *
   ldy pathPos
   lda argBuffer,y
   bne +
   lda #chrCR
   jsr putchar
   rts
+  lda #chrQuote
   sta cmdBuffer
   ldx #1
-  lda argBuffer,y
   sta cmdBuffer,x
   beq +
   iny
   inx
   bne -
+  iny
   sty pathPos
   lda #chrQuote
   sta cmdBuffer,x
   inx
   lda #" "
   sta cmdBuffer,x
   inx
   lda #<cmdBuffer
   ldy #>cmdBuffer
   sta zp
   sty zp+1
   txa
   ldy #0
   ldx #1
   jsr write
   jmp displayPath

   pathMsg = *
   .asc "path "
   .byte 0

pathSet = *
   ldy #0
   sty pathPos
   lda #1
   sta pathArg

   pathNextArg = *
   lda pathArg
   ldy #0
   jsr getarg
   lda zp
   ora zp+1
   bne +
   lda #0
   ldy pathPos
   sta argBuffer,y
   iny
   lda #<argBuffer
   ldx #>argBuffer
   sta zp+0
   stx zp+1
   lda #$82
   jsr aceDirName
   rts
+  ldy #0
   ldx pathPos
-  lda (zp),y
   sta argBuffer,x
   beq +
   inx
   iny
   bne -
+  inx
   stx pathPos
   inc pathArg
   jmp pathNextArg

;===dir===

;*** ls 1.10: directory-lister program - by Craig Bruce

; This file is in the BUDDY-assembler format

; ls [-lcf] [[directory_name] [filename] ...]
;
; -l : long-form directory listing
; -c : clear screen
; -f : give number of files, bytes used, and bytes free at end of listing
; -help : give usage information

;*** global declarations

dirArg     = 2
dirName    = 4
dirString  = 8
dirFcb     = 16
dirColumns = 17
dirCurCol  = 18
dirLong    = 19
dirSpaces  = 20
dirlineLen = 21
dirChCols  = 22
dirPaged   = 23
dirShown   = 24
dirCls     = 25
dirFiles   = 32
dirBytes   = 36
dirFree    = 40
dirFileSum = 44
dirCheckFi = 45
dirWork    = 64

dirCmdDir = *
   lda #true
   sta dirCls
   sta dirLong
   lda #false
   sta dirFileSum
   jmp dirMainEntry

dirCmdD = *
   lda #false
   sta dirCls
   lda #true
   sta dirLong
   sta dirFileSum
   jmp dirMainEntry

dirCmdClsl = *
   lda #true
   sta dirCls
   lda #false
   sta dirLong
   sta dirFileSum
   jmp dirMainEntry

dirMain = *
   lda #false
   sta dirLong
   sta dirCls
   sta dirFileSum
   dirMainEntry = *
   lda #false
   sta dirPaged
   sta dirShown
   lda #true
   sta dirCheckFi

   lda #0
   ldy #0
   sta dirArg+0
   sty dirArg+1

   dirNextArg = *
   jsr aceConStopkey
   bcc +
   jmp dirStopped
+  inc dirArg+0
   bne +
   inc dirArg+1
+  lda dirArg+0
   ldy dirArg+1
   jsr getarg
   lda zp+0
   ora zp+1
   beq dirMainExit
   ldy #0
   lda (zp),y
   cmp #"-"
   bne dirNameArg
   jsr dirHandleOption
   jmp dirNextArg

   dirNameArg = *
   lda zp+0
   ldy zp+1
   sta dirName+0
   sty dirName+1
   jsr dir
   lda #true
   sta dirShown
   jmp dirNextArg

dirMainExit = *
   lda dirShown
   bne +
   lda #false
   sta dirCheckFi
   lda #<dirDefaultDir
   ldy #>dirDefaultDir
   sta dirName+0
   sty dirName+1
   jsr dir
+  rts

   dirDefaultDir = *
   .byte ".",":",0

dirHandleOption = *
   ldy #0
   sty dirWork+2
   lda zp+0
   ldy zp+1
   sta dirWork+0
   sty dirWork+1

   dirNextOption = *
   inc dirWork+2
   ldy dirWork+2
   lda (dirWork),y
   bne +
   rts
+  cmp #"f"
   bne +
   lda #true
   sta dirFileSum
   jmp dirNextOption
+  cmp #"l"
   bne +
   lda #true
   sta dirLong
   jmp dirNextOption
+  cmp #"c"
   bne +
   lda #true
   sta dirCls
   jmp dirNextOption
+  lda #<dirUsageMsg
   ldy #>dirUsageMsg
   jsr eputs
   lda #0
   ldx #0
   jmp aceProcExit

dirUsageMsg = *
   .asc  "ls [-lcf] [[directory_name] [filename] ...]"
   .byte chrCR,chrCR
   .asc  "    -l : long-form directory listing"
   .byte chrCR
   .asc  "    -c : clear screen before listing"
   .byte chrCR
   .asc  "    -f : give number of files, bytes used, and bytes free at "
   .asc  "end of listing"
   .byte chrCR
   .asc  " -help : give this help information"
   .byte chrCR,0

dirStopped = *
   lda #<dirStoppedMsg
   ldy #>dirStoppedMsg
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit
   dirStoppedMsg = *
   .asc "<Stopped>"
   .byte chrCR,0

dirError = *
   lda #<dirErrorMsg1
   ldy #>dirErrorMsg1
   jsr eputs
   lda dirName+0
   ldy dirName+1
   jsr eputs
   lda #<dirErrorMsg2
   ldy #>dirErrorMsg2
   jmp eputs

   dirErrorMsg1 = *
   .asc "Error reading file/directory "
   .byte chrQuote,0
   dirErrorMsg2 = *
   .byte chrQuote,chrCR,0

dir = *
   bit dirCheckFi
   bpl +
   lda #<dirName
   ldy #>dirName
   jsr aceDirIsdir
   cpy #0
   bne +
   jmp dirFile
+  lda dirCls
   beq +
   lda #chrCLS
   jsr putchar
+  lda dirLong
   bne dirLsLong

dirLsShort = *
   ldx #1
   jsr aceFileInfo
   stx dirChCols
   cmp #0
   bne +
   txa
   ldx #$ff
-  inx
   sbc #20
   bcs -
   txa
   bne ++
+  lda #1
+  sta dirColumns
   jmp dirCommon

dirLsLong = *
   ldx #1
   jsr aceFileInfo
   stx dirChCols
   lda #1
   sta dirColumns

dirCommon = *
   lda #0
   sta dirCurCol
   ldx #3
-  sta dirBytes,x
   sta dirFiles,x
   dex
   bpl -

   dirGotName = *
   lda dirName+0
   ldy dirName+1
   sta zp+0
   sty zp+1
   jsr aceDirOpen
   bcc +
   jmp dirError
+  sta dirFcb
   ldx dirFcb
   jsr aceDirRead
   bcs dirExit
   beq dirExit
   jsr aceConStopkey
   bcc +
   jmp dirStopped
+  lda dirLong
   bpl dirNext
   jsr dirDisplayHeading

   dirNext = *
   ldx dirFcb 
   jsr aceDirRead
   bcs dirExit
   beq dirExit
   jsr aceConStopkey
   bcc +
   jsr dirExit
   jmp dirStopped
+  lda aceDirentName+0
   beq dirTrailerExit
   lda aceDirentUsage
   and #%00010000
   bne dirNext
   jsr dirDisplay
   jmp dirNext

   dirTrailerExit = *
   lda dirLong
   bpl dirExit
   jsr dirDisplayTrailer
   jmp dirExit

   dirExit = *
   lda dirCurCol
   beq +
   lda #chrCR
   jsr putchar
+  lda dirFcb
   jmp aceDirClose

dirDisplay = *
   bit aceDirentFlags
   bmi ++
   inc dirFiles+0
   bne +
   inc dirFiles+1
   bne +
   inc dirFiles+2
   bne +
   inc dirFiles+3
+  ldx #0
   ldy #4
   clc
-  lda dirBytes,x
   adc aceDirentBytes,x
   sta dirBytes,x
   inx
   dey
   bne -
+  bit dirLong
   bmi +
   jmp dirDisplayShort
+  jsr dirSetupDirline
   lda #<dirline
   ldy #>dirline
   sta zp+0
   sty zp+1
   lda dirlineLen
   ldy #0
   ldx #stdout
   jmp write

;*            000000000011111111112222222222333333333344444444445555555555
;*       pos: 012345678901234567890123456789012345678901234567890123456789
dirline .asc "drwx*e-t  00-Xxx-00  12:00a 12345678 *SEQ  1234567890123456\n"
        .byte 0
dirFlagNames .asc "drwx*e-t"
dirDateStr   .asc "  00-Xxx-00  12:00a "
dirDateEnd = *

dirSetupDirline = *
   ;** flags
   ldx #0
   lda aceDirentFlags
-  asl
   pha
   lda #"-"
   bcc +
   lda dirFlagNames,x
+  sta dirline+0,x
   pla
   inx
   cpx #8
   bcc -

   ;** date
   jsr dirPutInDate
   ldx #dirDateEnd-dirDateStr-1
-  lda dirDateStr,x
   sta dirline+8,x
   dex
   bpl -

   ;** bytes
   ldx #3
-  lda aceDirentBytes,x
   sta dirFree,x
   dex
   bpl -
   lda #<dirUtoaNumber
   ldy #>dirUtoaNumber
   sta zp+0
   sty zp+1
   lda #8
   ldx #dirFree
   jsr aceMiscUtoa
   ldy #28
   lda dirChCols
   cmp #60
   bcs +
   ldy #8
+  ldx #0
-  lda dirUtoaNumber,x
   sta dirline,y
   iny
   inx
   cpx #8
   bcc -
   lda #" "
   sta dirline,y
   iny

   ;** unclosed flag
   lda dirline+4
   cmp #"-"
   bne +
   lda #" "
+  sta dirline,y
   iny

   ;** filetype
   ldx #0
-  lda aceDirentType,x
   ora #$80
   sta dirline,y
   iny
   inx
   cpx #3
   bcc -
   lda #" "
   sta dirline,y
   iny
   sta dirline,y
   iny

   ;** filename
   ldx #0
-  lda aceDirentName,x
   beq +
   sta dirline,y
   iny
   inx
   bne -
+  lda #chrCR
   sta dirline,y
   iny
   lda #0
   sta dirline,y
   sty dirlineLen
   rts

dirDisplayShort = *
   lda #<aceDirentName
   ldy #>aceDirentName
   jsr puts
   inc dirCurCol
   lda dirCurCol
   cmp dirColumns
   bcc +
   lda #0
   sta dirCurCol
   lda #chrCR
   jmp putchar
+  ldy #$ff
-  iny
   lda aceDirentName,y
   bne -
   sty dirSpaces
   lda #20
   sbc dirSpaces
   sta dirSpaces
-  lda #" "
   jsr putchar
   dec dirSpaces
   bne -
   rts

dirDisplayHeading = *
   lda #<dirHeadingMsg
   ldy #>dirHeadingMsg
   jsr puts
   lda #<aceDirentName
   ldy #>aceDirentName
   jsr puts
   lda #chrCR
   jsr putchar
   rts

   dirHeadingMsg = *
   .asc "Dir: "
   .byte 0

dirDisplayTrailer = *
   ldx #3
-  lda aceDirentBytes,x
   sta dirFree,x
   dex
   bpl -
   ldx dirFileSum
   beq dirDisplayShortTrailer
   ldx #0
   ldy #0
-  lda dirTrailingMsg,x
   beq +
   cmp #4
   bcc dirStoreNum
   sta dirTrailBuf,y
   inx
   iny
   bne -
+  lda #<dirTrailBuf
   ldx #>dirTrailBuf
   sta zp+0
   stx zp+1
   tya
   ldy #0
   ldx #stdout
   jmp write

   dirDisplayShortTrailer = *
   lda #<dirUtoaNumber
   ldy #>dirUtoaNumber
   sta zp+0
   sty zp+1
   ldx #dirFree
   lda #0
   jsr aceMiscUtoa
   lda #<dirUtoaNumber
   ldy #>dirUtoaNumber
   jsr puts
   lda #<dirTrailShMsg
   ldy #>dirTrailShMsg
   jmp puts

   dirTrailShMsg = *
   .asc " bytes free"
   .byte chrCR,0

   dirStoreNum = *
   stx dirWork+0
   sty dirWork+1
   sec
   sbc #1
   asl
   asl
   adc #dirFiles
   tax
   lda #<dirUtoaNumber
   ldy #>dirUtoaNumber
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   ldx #0
   ldy dirWork+1
-  lda dirUtoaNumber,x
   beq +
   sta dirTrailBuf,y
   inx
   iny
   bne -
+  ldx dirWork+0
   inx
   jmp --

   dirTrailingMsg = *
   .asc "files="
   .byte 1
   .asc "  bytes="
   .byte 2
   .asc "  free="
   .byte 3,chrCR,0

   dirTrailBuf .buf 64

dirPutInDate = *
   ;** year
   lda aceDirentDate+1
   ldx #9
   jsr dirPutDigits
   ;** month
   lda aceDirentDate+2
   cmp #$10
   bcc +
   sec
   sbc #$10-10
+  tax
   lda dirMonthStr+0,x
   sta dirDateStr+5
   lda dirMonthStr+13,x
   sta dirDateStr+6
   lda dirMonthStr+26,x
   sta dirDateStr+7
   ;** day
   lda aceDirentDate+3
   ldx #2
   jsr dirPutDigits
   ;** hour
   lda aceDirentDate+4
   ldx #"a"
   cmp #$00
   bne +
   lda #$12
   jmp dirPutHour
+  cmp #$12
   bcc dirPutHour
   ldx #"p"
   cmp #$12
   beq dirPutHour
   sei
   sed
   sec
   sbc #$12
   cld
   cli
   dirPutHour = *
   stx dirDateStr+18
   ldx #13
   jsr dirPutDigits
   ;** minute
   lda aceDirentDate+5
   ldx #16
   jsr dirPutDigits
   rts

   dirPutDigits = *  ;( .A=num, .X=offset )
   pha
   lsr
   lsr
   lsr
   lsr
   ora #$30
   sta dirDateStr,x
   pla
   and #$0f
   ora #$30
   sta dirDateStr+1,x
   rts
 
   dirMonthStr = *
   .asc "XJFMAMJJASOND"
   .asc "xaeapauuuecoe"
   .asc "xnbrrynlgptvc"

dirUtoaNumber .buf 11

dirFile = *
   ldx #stdout
   jsr aceFileInfo
   cpx #60
   bcc +
   lda #<dirFileLongMsg
   ldy #>dirFileLongMsg
   jmp ++
+  lda #<dirFileShortMsg
   ldy #>dirFileShortMsg
+  jsr puts
   lda dirName+0
   ldy dirName+1
   jsr puts
   lda #chrCR
   jsr putchar
   rts

   dirFileLongMsg = *
   .asc "*argument is a file--option not supported: "
   .byte 0
   dirFileShortMsg = *
   .asc "*argument is a file-n: "
   .byte 0

;******** standard library ********

eputs = *
   ldx #stderr
   jmp fputs
puts = *
   ldx #stdout
fputs = *
   sta zp+0
   sty zp+1
zpputs = *
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
   sta zp+0
   sty zp+1
   lda #1
   ldy #0
   jmp write
   putcBuffer .buf 1

getchar = *
   ldx #stdin
getc = *
   lda #<getcBuffer
   ldy #>getcBuffer
   sta zp+0
   sty zp+1
   lda #1
   ldy #0
   jsr read
   beq +
   lda getcBuffer
   rts
+  sec
   rts
   getcBuffer .buf 1

cls = *
   lda #chrCLS
   jmp putchar

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

;===bss===

bss           = *
cmdBuffer     = bss+0
copyBuffer    = bss+0
argBuffer     = cmdBuffer+256
argArgvBuffer = argBuffer+256
