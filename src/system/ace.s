;ACE-128/64 kernel by Craig Bruce, started 04-Mar-1992.

computer = 128

;* system zero-page memory usage:
;*   $02-$7f = application work area
;*   $80-$8f = system work area
;*   $f8-$ff = system parameter area

;* regular RAM0 organization
;*   $0100-$01ff = processor stack (0.25K)
;*   $0200-$0eff = system storage (3.25K)
;*   $0f00-$0fff = kernel-interface variables (0.25K)
;*   $1000-$12ff = system storage / free on the C64 (0.75K)
;*   $1300-$6fff = ACE kernel and device drivers (23.25K)
;*   $7000-$bfff = application area & stack (20K / configurable size)
;*   $c000-$eeff = free memory (11.25K)
;*   $ef00-$efff = modem transmit buffer (0.25K)
;*   $f000-$f7ff = regular character set (2K)
;*   $f800-$fbff = vic 40-column screen (1K)
;*   $fc00-$feff = free memory (0.75K)
;*   $ff00-$ffff = system storage (0.25K)

;* high-RAM0 organization for C64 with soft-80 screen configured:
;*   $c000-$c2ff = free memory (0.75K)
;*   $c300-$c3ff = modem transmit buffer (0.25K)
;*   $c400-$cbff = soft-80 char storage (2K)
;*   $cc00-$cfff = vic 40-column screen (1K)
;*   $d000-$d7ff = regular character set (2K)
;*   $d800-$dfff = soft-80 4-bit character set (2K)
;*   $e000-$ff3f = bitmapped screen (7.81K)
;*   $ff40-$ffff = system storage (0.19K)

.seq acehead.s
.seq kernhead.s
.org $1300

.if computer-64
   .obj "@0:ace128"
   useC128 = 1
   useC64  = 0
   useVdc  = 1
   useVic  = 1
   useSoft80 = 0  ;;can't use on C128
   useExtKeyboard = 1
   useFastClock = 1
.else
   .obj "@0:ace64"
   useC64  = 1
   useC128 = 0 ;no
   useVdc  = 0
   useVic  = 1
   useSoft80 = 1
   useExtKeyboard = 0
   useFastClock = 0 ;don't use--crashes on C64
.ife

jmp entryPoint

;***jump table

jmp kernFileOpen
jmp kernFileClose
jmp kernFileRead
jmp kernFileWrite
jmp kernFileLseek
jmp kernFileBload
jmp kernFileRemove
jmp kernFileRename
jmp kernFileInfo
jmp kernFileIoctl
jmp notImp  ;kernFileSelect
jmp notImp  ;kernFileBlock

jmp kernDirOpen
jmp kernDirClose
jmp kernDirRead
jmp kernDirIsdir
jmp kernDirChange
jmp kernDirMake
jmp kernDirRemove
jmp kernDirName

jmp kernWinScreen
jmp kernWinMax
jmp kernWinSet
jmp kernWinSize
jmp kernWinCls
jmp kernWinPos
jmp kernWinPut
jmp kernWinGet
jmp kernWinScroll
jmp kernWinCursor
jmp kernWinPalette
jmp kernWinChrset
jmp kernWinOption

jmp kernConWrite
jmp kernConPutlit
jmp kernConPos
jmp kernConGetpos
jmp kernConInput
jmp kernConStopkey
jmp kernConGetkey
jmp kernConKeyAvail
jmp kernConKeyMat
jmp kernConMouse
jmp kernConJoystick
jmp kernConOption

jmp kernGrScreen
jmp kernGrExit
jmp kernGrFill
jmp kernGrOp

jmp kernProcExec
jmp kernProcExecSub
jmp kernProcExit

jmp kernMemZpload
jmp kernMemZpstore
jmp kernMemFetch
jmp kernMemStash
jmp kernMemAlloc
jmp kernMemFree
jmp kernMemStat

jmp kernTimeGetDate
jmp kernTimeSetDate
jmp kernTimeJif

jmp kernMiscUtoa
jmp kernMiscIoPeek
jmp kernMiscIoPoke

jmp kernFileFdswap
jmp kernConRead
jmp kernConPutchar
jmp kernConPutctrl
jmp kernMiscCmdOpen
jmp kernMiscCmdClose
jmp kernMiscCmdSend
jmp kernMiscCmdStatus
jmp kernModemCheck
jmp kernModemParms
jmp kernModemStat
jmp kernTest

.byte $ff,$fe,$3c,$e2,$fc

;***global declarations

maxZpUse     = $90
stringBuffer = $400  ;(256 bytes)
keylineBuf   = $500  ;(256 bytes)
fcbTable1    = $600  ;(256 bytes)
freemap      = $800  ;(256 bytes)
ram0FreeMap  = $900  ;(256 bytes)
configBuf    = $d00  ;(512 bytes)

.if useC128
   pageC0Save   = $c00  ;(256 bytes)  ;a little redundant
   workbuf      = $b00  ;(256 bytes)
   maxPage = $c0
   bkACE = $0e
   bkApp = $0e
   bkRam0 = $3f
   bkRam0io = $3e
   bkKernel = $00
   bkCharset = $0f
   bkSelect = $ff00
   kernelIrqHandler = $fa65
   kernelBrkHandler = $b003
   kernelNmiHandler = $fa40
   kernelStopHandler = $f66e
   nmiRedirect = $318
   nmiExit = $ff33
.else
   basicZpSave  = $a00  ;("maxZpUse" bytes ($90))
   deviceTable  = $1000 ;(256 bytes)
   workbuf      = $1100 ;(256 bytes)
   unusedMem1   = $1200 ;unused (256 bytes) ;modify config program first
   unusedMem2   = $0b00 ;unused (256 bytes)
   unusedMem3   = $0c00 ;unused (256 bytes)
   maxPage = $d0
   bkSelect = $01
   bkACE = $36
   bkApp = $36
   bkRam0 = $30
   bkRam0io = $35
   bkKernel = $37
   bkCharset = bkRam0
   kernelIrqHandler = $ea31
   kernelBrkHandler = $fe66
   kernelNmiHandler = $fe47
   kernelStopHandler = $f6ed
.ife

vic   = $d000
vdc   = $d600
sid   = $d400
cia1  = $dc00
cia2  = $dd00
st    = $90
true  = $ff
false = $00
chrQuote = $22
scpuHwOn  = $d07e
scpuHwOff = $d07f
scpuMrMode = $d0b4
scpuMrAll = $d077 ;mirror all
scpuMrOff = $d076 ;mirror only BASIC screen

fcbCount = 16
lftable   .buf fcbCount
devtable  .buf fcbCount
satable   .buf fcbCount
eoftable  .buf fcbCount
pidtable  .buf fcbCount
lfnull = $ff
cmdlf  = 66
fcbNull = $ff
minDisk = 8
regsave .buf 3
jiffyCount .buf 4

newlf .buf 1

kernelClall  = $ffe7
kernelSetbnk = $ff68
kernelSetmsg = $ff90
kernelReadst = $ffb7
kernelSetlfs = $ffba
kernelSetnam = $ffbd
kernelOpen   = $ffc0
.if computer-64
kernelClose  = $ffc3
.ife
kernelChkin  = $ffc6
kernelChkout = $ffc9
kernelClrchn = $ffcc
kernelChrin  = $ffcf
kernelChrout = $ffd2
kernelLoad   = $ffd5
kernelStop   = $ffe1
kernelGetin  = $ffe4
kernelScrorg = $ffed
kernelSwapper = $ff5f

notImp = *
   lda #aceErrNotImplemented
   sta errno
   sec
   rts

.if useC64
;*** kernel close with pseudo-close for disk command channel for the 64
kernelClose = *
   bcs +
   jmp $ffc3
+  ldx $98
-  dex
   bmi kernelCloseExit
   cmp $259,x
   bne -
   beq +
   brk
   ;** found entry; copy last entry on top if it
+  ldy $98
   dey
   lda $259,y   ;move lfn
   sta $259,x
   lda $263,y   ;move dev num
   sta $263,x
   lda $26d,y   ;move sec addr
   sta $26d,x
   dec $98
   kernelCloseExit = *
   clc
   rts
.ife

;*** entrypoint()

entryPoint = *
   lda #0
   pha
   plp
   lda #bkACE
   sta bkSelect
   jmp main

;*** startup()

aceBootstrap = *
   php
   sei
   ldx #2
-  lda $00,x
   sta basicZpSave,x
   lda #0
   sta $00,x
   inx
   cpx #maxZpUse
   bcc -
.if useC128
   ldx #0
-  lda $c00,x
   sta pageC0Save,x
   inx
   bne -
.ife
   lda #%01111111
   sta $dc0d
   lda #%01111111
   sta $dd0d
   bit $dc0d
   bit $dd0d
   lda #%00000000
   sta vic+$1a
   lda #<irqHandler
   ldy #>irqHandler
   sta $314
   sty $315
   lda #<brkHandler
   ldy #>brkHandler
   sta $316
   sty $317
   lda #<nmiHandler
   ldy #>nmiHandler
   sta $318
   sty $319
   lda #<stopHandler
   ldy #>stopHandler
   sta $328
   sty $329
   lda #<nmiContinue
   ldy #>nmiContinue
   sta nmiRedirect+0   ;redundant on C128
   sty nmiRedirect+1
   lda aceSuperCpuFlag
   pha
   ldx #127
   lda #0
-  sta errno,x
   dex
   bpl -
   pla
   sta aceSuperCpuFlag
   lda #$04  ;"a:"
   sta aceCurrentDevice
   lda #0
   jsr kernelSetmsg
.if computer-64
   lda #0
   ldx #0
   jsr kernelSetbnk
.ife
   jsr kernelClall
   lda vic+$20
   sta colorSave+0
   lda vic+$21
   sta colorSave+1
   plp
   rts
   colorSave .buf 4

aceConfig = *
   lda #0
   ldx bootDevice
   ldy #0
   jsr kernelSetlfs
   lda #6
   ldx #<aceConfigName
   ldy #>aceConfigName
   jsr kernelSetnam
   lda #0
   ldx #<aceAppAddress
   ldy #>aceAppAddress
   jsr kernelLoad
   bcc +
   rts
+  lda #<configBuf
   ldy #>configBuf
   sta 2
   sty 3
   lda #<aceStartupMsg
   ldy #>aceStartupMsg
   sta 4
   sty 5
   lda #<ram0FreeMap
   ldy #>ram0FreeMap
   sta 6
   sty 7
   ldx #>aceBssEnd
   lda #<aceBssEnd
   beq +
   inx
+  stx 8
   .if computer-64
   lda #128
   .else
   lda #64
   .ife
   sta 9
   lda #<charset4bit
   ldy #>charset4bit
   sta 10
   sty 11
   lda #<conKeymapNormal
   ldy #>conKeymapNormal
   sta 12
   sty 13
   lda #$00
.if useVdc
   ora #$80
.ife
.if useVic
   ora #$40
.ife
.if useSoft80
   ora #$20
.ife
   sta 14
   jsr aceAppAddress
   rts
   aceConfigName = *
   .asc "config"

aceStartup = *
   ldx #fcbCount-1
   lda #lfnull
-  sta lftable,x
   dex
   bpl -
   lda #0
   sta newlf
   jsr initStack
   lda aceCurrentDevice
   lsr
   lsr
   ora #$40
   sta aceCurDirName+0
   lda #":"
   sta aceCurDirName+1
   lda #0
   sta aceCurDirName+2
   ldx #57
-  lda configBuf+$140,x
   sta aceShellPath,x
   dex
   bpl -
   lda #1
   sta aceProcessID
   rts

initStack = *
   lda #0
   ldy aceTpaLimit
   sta aceStackTop+0
   sty aceStackTop+1
   sta aceMemTop+0
   sty aceMemTop+1
   sta aceFramePtr+0
   sty aceFramePtr+1
   rts

;*** shutdown()

aceShutdown = *
   lda #bkACE
   sta bkSelect
   ldx #2
-  lda basicZpSave,x
   sta $00,x
   inx
   cpx #maxZpUse
   bcc -
.if computer-64
   ldx #0
-  lda pageC0Save,x
   sta $c00,x
   inx
   bne -
.ife
   php
   sei
   lda #<kernelIrqHandler
   ldy #>kernelIrqHandler
   sta $314
   sty $315
   lda #<kernelBrkHandler
   ldy #>kernelBrkHandler
   sta $316
   sty $317
   lda #<kernelNmiHandler
   ldy #>kernelNmiHandler
   sta $318
   sty $319
   lda #<kernelStopHandler
   ldy #>kernelStopHandler
   sta $328
   sty $329
.if computer-64
   lda #%01111111
   sta $dc0d
   lda #%01111111
   sta $dd0d
   bit $dc0d
   bit $dd0d
   lda #%00000001
   sta vic+$1a
.else
   lda #%10000001
   sta $dc0d
   lda #%01111111
   sta $dd0d
   bit $dc0d
   bit $dd0d
   lda #%00000000
   sta vic+$1a
.ife
   lda colorSave+0
   sta vic+$20
   lda colorSave+1
   sta vic+$21
   plp
   rts

aceShutdownBasic = *
   bit exitScr
   bpl shutBasicScrDone
   ;** shut down the screens
.if useC128
   jsr winShutdown
   lda winDriver
   eor $d7
   bpl +
   jsr kernelSwapper
+  nop
.else
   lda #25
   ldx #40
   ;xx check config for screen
   jsr kernWinScreen
   jsr winShutdown
.ife
   ldx #0
   lda #$20
-  sta $400+000,x
   sta $400+250,x
   sta $400+500,x
   sta $400+750,x
   inx
   cpx #250
   bcc -
   shutBasicScrDone = *
   ;** shutdown system
   jsr aceShutdown
   ;** return to basic
   bit exitScr
   bpl +
   lda #147
   jsr kernelChrout
+  lda #bkKernel
   sta bkSelect
   cld
   cli
.if useC128
   lda #0
   sta $f8
   sta $f9
   lda #$16
   sta 2604
   sta $d018
   lda #0
   sta $1c00
   sta 208
   jsr $51d6
   jmp $4db7
.else
   lda #0
   sta $800
   sta 198
   jsr $a642
   jmp $a474
.ife
   brk

resetIntDispatch = *  ;for C64 only, RAM0
   ldx #$ff
   sei
   txs
   cld
   lda #bkKernel
   sta bkSelect
   jmp $fce2

nmiIntDispatch = *  ;for C64 only, RAM0, replica of ROM
   sei
   jmp ($318)

nmiHandler = *
.if useC64
   cld            ;This code gives the C64 and C128 the same
   pha            ;semantics and the same number of cycles
   txa            ;of NMI dispatching overhead (including
   pha            ;the return).  Although, the C128 is in
   tya            ;its Kernal bank when entering, and the
   pha            ;C64 is in bkACE.
   lda bkSelect
   pha
   lda #bkACE
   sta bkSelect
   nmiRedirect = *+1
   jmp nmiContinue
.ife
nmiContinue = *
   cld
.if useC128
   lda #$7f
   sta $dd0d
   ldy $dd0d
   bmi +
   lda #$7f
   sta $dc00
   lda $ff
   sta $d02f
-  lda $dc01
   cmp $dc01
   bne -
   and #$80
   bmi +
   jsr aceShutdown
   ;jsr shutdownMemory
   ;lda #147
   ;jsr kernelChrout
   ;jmp aceShutdownBasic
   jsr $e056
   jsr $e109
   jsr $c000
   lda #0
   sta $1c00
   lda #bkKernel
   sta bkSelect
   jsr $51d6
   jmp ($0a00)
.else
   lda #$7f
   sta $dd0d
   ldy $dd0d
   bmi +
   lda #$7f
   sta $dc00
-  lda $dc01
   cmp $dc01
   bne -
   and #$80
   bmi +
   jsr aceShutdown
   jsr $fd15
   jsr $fda3
   jsr $e518
   lda #0
   sta $800
   jsr $a642
   jmp ($a002)
.ife
+  jmp nmiExit

.if useC64
nmiExit = *
   pla
   sta bkSelect
   pla
   tay
   pla
   tax
   pla
   rti
.ife

;C128 NMI overhead=76 cycles: int=7, maxLatency=6, ROMenter=33, ROMexit=30
;C64  NMI overhead=76 cycles: int=7, maxLatency=6, ROMenter=34, ROMexit=29

;*** extra-lean SwiftLink interrupt handler; the NMI redirect vector points here

.if useC128
   leanHead     = $c8  ;buffer head pointer
   leanTail     = $c9  ;buffer tail pointer
   leanFreeCnt  = $ca  ;bytes free in buffer
   leanStopped  = $cb  ;flow stopped flag
   leanRtsOff   = $ce  ;must be non-zero (DTR,ints active)
   leanOverruns = $cf  ;number of bytes lost to overruns
.else
   leanHead     = $a8  ;buffer head pointer
   leanTail     = $a9  ;buffer tail pointer
   leanFreeCnt  = $aa  ;bytes free in buffer
   leanStopped  = $ab  ;flow stopped flag
   leanRtsOff   = $ac  ;must be non-zero (DTR,ints active)
   leanOverruns = $ad  ;number of bytes lost to overruns
.ife
leanDropCnt .buf 4
leanBuffer  .buf 256 ;this needs to be on the Commodore Kernal bank

nmiLeanSwiftLink = *
   leanPr1 = *+2
   lda $de00+$1      ;(4) ;status
   and #$08          ;(2)
   beq leanNot       ;(2*)
   leanPr2 = *+2
   lda $de00+$1       ;(4) opt ;status
   and #$04           ;(2) opt
   beq +              ;(3*)opt
   inc leanOverruns   ;(5^)opt
   leanPr3 = *+2
+  lda $de00+$0      ;(4) ;data
   ldy leanTail      ;(3)
   ldx leanFreeCnt   ;(3)
   beq leanDrop      ;(2*)
   sta leanBuffer,y  ;(5)
   inc leanTail      ;(5)
   dec leanFreeCnt   ;(5)
   cpx #33           ;(2)
   bcs +             ;(3*)
   lda leanRtsOff    ;(3)
   leanPr4 = *+2
   sta $de00+$2      ;(4) ;command
   sta leanStopped   ;(3)
+  jmp nmiExit       ;(3)

;timing: normal=76+43+9=128 cycles, assertFlow=76+52+9=137 cycles
;
;C128 @ 115.2k: 177 cycles avail (fast)
;C64  @  57.6k: 177 cycles avail, worstAvail=177-43? = 134
;SCPU @ 230.4k: 868 cycles avail: for a joke!

leanDrop = *
   inc leanDropCnt+0 ;non-critical
   bne +
   inc leanDropCnt+1
   bne +
   inc leanDropCnt+2
   bne +
   inc leanDropCnt+3
+  jmp nmiExit

leanNot = *
   jmp nmiContinue

aceIrqInit = *
   php
   sei
.if useC64
   ldx #5
-  lda c64IntVecs,x
   sta $fffa,x
   dex
   bpl -
.ife
   lda #<irqHandler
   ldy #>irqHandler
   sta $314
   sty $315
.if useC128
   ;xx on the C128, we must use the VIC raster interrupt as the timer
   lda vic+$11
   and #$7f
   sta vic+$11
   lda #252
   sta vic+$12
.else
   ;** use CIA1 timer A to generate 60-Hz IRQ on the C64
   lda #<17045  ;1_022_727 / 60 for NTSC
   ldy #>17045
   ldx configBuf+$c7
   beq +
   lda #<16421  ;985_248 / 60 for PAL
   ldy #>16421
+  sta $dc04
   sty $dc05
   lda $dc0e
   and #%10000000
   ora #%00010001
   sta $dc0e
.ife
   ldx #3
-  lda #$00
   sta jiffyCount,x
   dex
   bpl -
   plp
   rts

c64IntVecs = *
   .word nmiIntDispatch,resetIntDispatch,irqIntDispatch

irqIntDispatch = *  ;for C64 only, RAM0
   pha
   txa
   pha
   tya
   pha
   lda bkSelect
   pha
   lda #bkKernel
   sta bkSelect
   cld
   tsx
   lda $0105,x
   and #$10
   beq irqHandlerInt64
   jmp brkHandler

irqHandler = *  ;(.AXY already saved, 128 bank)
   cld
.if computer-64
.else
   lda bkSelect
   pha
.ife
   irqHandlerInt64 = *
   lda #bkACE
   sta bkSelect
.if computer-64
   lda vic+$19
   bpl +
   and #$01
   beq +
   sta vic+$19
   jmp sixty
.else
   lda $dc0d
   bpl +
   and #$01
   bne sixty
.ife
+  jmp irqExit

sixty = *
   inc jiffyCount+0
   bne +
   inc jiffyCount+1
   bne +
   inc jiffyCount+2
   bne +
   inc jiffyCount+3
+  jsr conScreenSave
   jsr winIrqCursor
   jsr conIrqKeyscan
   jmp irqExit

.if computer-64
irqExit = $ff33
.else
irqExit = *
   pla
   sta bkSelect
   pla
   tay
   pla
   tax
   pla
   rti
.ife

brkHandler = *
   cld
   ldx #0
-  lda $00,x
   sta $0400,x
   dex
   bne -
   jsr aceShutdown
.if computer-64
   lda #0
   sta $1c00
   ;jsr $51d6
.else
   lda #0
   sta $800
   jsr $a642
.ife
   jmp kernelBrkHandler

stopHandler = *
   lda #$ff
   rts

.seq acecall.s
.seq acemem.s
.seq acewin.s
.if useVdc
   .seq acevdc.s
.ife
.if useVic
   aceVicColorOff .byte $b8
   .seq acevic.s
.ife
   charset4bit  = $d800
.if useSoft80
   .seq acesoft80.s
.ife
.seq acecon.s
.seq acerd.s
.seq aceswift.s
.seq acepar.s

;*** main()

bootDevice .buf 1
titlePtr   .buf 2
titleLen   .buf 2
exitScr    .byte 0

main = *
   lda 186
   sta bootDevice
   lda #147
   jsr kernelChrout
   lda #14
   jsr kernelChrout
   ldx #$00
   lda $d0bc
   and #$80
   bne +
   ldx #$ff
+  stx aceSuperCpuFlag
   bit aceSuperCpuFlag
   bpl +
   sta scpuHwOn
   sta $d07b ;select 20 MHz
   sta scpuMrAll
   sta scpuHwOff
.if useC64
+  bit aceSuperCpuFlag
   bpl +
   lda #"S"
   sta aceStartupMsg+4
   lda #"6"
   sta aceStartupMsg+5
   lda #"4"
   sta aceStartupMsg+6
+  nop
.ife
   ldx #0
-  lda aceStartupMsg,x
   beq +
   jsr kernelChrout
   inx
   bne -
+  sei
   jsr aceBootstrap
   jsr initMemory
   jsr aceConfig
   bcc +
   jmp configErrMainExit
+  lda 2
   ldy 3
   sta titlePtr+0
   sty titlePtr+1
   lda 4
   ldy 5
   sta titleLen+0
   sty titleLen+1
   jsr aceIrqInit
   jsr aceStartup
   bit aceSuperCpuFlag
   bpl +
   sta scpuHwOn
   sta scpuMrOff
   sta scpuHwOff
+  jsr initMemoryAlloc
   sei
   jsr winStartup
   jsr conInit
   lda #$ff
   sta exitScr
   jsr devTableInit
   jsr rdInit
   jsr parInit
   jsr slInit
.if computer-64
   lda #$01
   sta vic+$1a     ;enable VIC raster IRQ on C128
.else
   bit $dc0d
   lda #%10000001  ;enable 60-Hz IRQ on C64
   sta $dc0d
.ife
   cli
   lda titlePtr+0
   ldx titlePtr+1
   inx
   inx
   sta syswork+0
   stx syswork+1
   ldy #5
   lda (syswork+0),y
   tay
   clc
   lda syswork+0
   adc #8
   bcc +
   inx
+  sta syswork+0
   stx syswork+1
   lda #%11100000
   cpy #$00
   beq +
   ora #%00010000
+  ldx #$00
   ldy #40
   jsr kernWinChrset
   clc
   lda syswork+0
   adc #40
   sta syswork+0
   bcc +
   inc syswork+1
+  lda #%10001010
   ldx #$00
   ldy #0
   jsr kernWinChrset
   clc
   lda syswork+1
   adc #>2048
   sta syswork+1
   lda #%10000110
   ldx #$00
   ldy #0
   jsr kernWinChrset

   ;** open std files
   lda #<configBuf+$100
   ldy #>configBuf+$100
   sta zp+0
   sty zp+1
   lda #$00
   jsr internDirChange
   lda #<stdinName
   ldy #>stdinName
   sta zp+0
   sty zp+1
   lda #"r"
   jsr internOpen  ;fcb=0
   lda #<stdoutName
   ldy #>stdoutName
   sta zp
   sty zp+1
   lda #"w"
   jsr internOpen   ;fcb=1
   lda #"w"
   jsr internOpen   ;fcb=2
   lda titlePtr+0
   ldy titlePtr+1
   sta zp+0
   sty zp+1
   lda titleLen+0
   ldy titleLen+1
   ldx #stdout
   jsr internWrite
   cli

   ;** call shell
   lda aceMemTop+0
   ldy aceMemTop+1
   sec
   sbc #13
   bcs +
   dey
+  sta zw+0
   sty zw+1
   ldy #12
-  lda startupArgs,y 
   sta (zw),y
   dey
   bpl -
   ldy #0
   clc
   lda zw+0
   adc #6
   sta (zw),y
   iny
   lda zw+1
   adc #0
   sta (zw),y
   iny
   clc
   lda zw+0
   adc #9
   sta (zw),y
   iny
   lda zw+1
   adc #0
   sta (zw),y
   lda #<shellName
   ldy #>shellName
   sta zp+0
   sty zp+1
   lda #2
   ldy #0
   jsr internProcExec
   lda #$00
   bcc +
   lda errno
+  sta 1021

   ;** exit
   lda #stdin
   jsr internClose
   lda #stdout
   jsr internClose
   lda #stderr
   jsr internClose
   lda #chrCLS
   jsr conPutchar
   configErrMainExit = *
   lda bootDevice
   sta 186
   jsr slShutdown
   jsr shutdownMemory
   jmp aceShutdownBasic

stdoutName .asc "s:"
           .byte 0
stdinName  .asc "k:"
           .byte 0

startupArgs .byte 0,0,0,0,0,0,"s","h",0,"-","v","i",0
shellName .byte "s","h",0
   
aceStartupMsg = *
.if computer-64
   .asc "ACE-128 "
.else
   .asc "ACE-64  "
.ife
   .asc "Kernel 0.99 - by CSB 09-Feb-97"
   .byte chrCR,chrCR,0

;*** bss: c128=400 bytes, c64=0 bytes

aceBss = *
.if computer-64
   basicZpSave  = aceBss+0        ;("maxZpUse" bytes)
   deviceTable  = basicZpSave+maxZpUse  ;(256 bytes)
   aceBssEnd    = deviceTable+256
.else
   aceBssEnd    = aceBss+0
.ife
