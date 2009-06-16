;*** sort program - by Craig Bruce, started 13-Jun-93

.seq "acehead.s"
.org aceAppAddress
.obj "@0:sort"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $40

chrQuote = $22
maxLineLen = 2049
true = $ff
false = $00

reverseFlag     .buf 1
ignoreCaseFlag  .buf 1
keyPosition     .buf 1

;******** standard library ********
puts = *
   ldx #stdout
fputs = *
   sta zp+0
   sty zp+1
   ldy #$ff
-  iny
   lda (zp),y
   bne -
   tya
   ldy #0
   jmp write
eputs = *
   ldx #stderr
   jmp fputs

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

getarg = *
   sty zp+1
   asl
   sta zp+0
   rol zp+1
   clc
   lda aceArgv
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
   rts

;===main===
arg = 2
name = 4
string = 8

main = *
   ;** check argument count
   lda #0
   sta reverseFlag
   sta ignoreCaseFlag
   lda #1
   sta keyPosition
   lda aceArgc+1
   bne enoughArgs
   lda aceArgc
   cmp #2
   bcs enoughArgs

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jmp eputs

usageMsg = *
   .asc "usage: sort [-[i][v]] [+column] file ..."
   .byte chrCR,0

enoughArgs = *
   ;** main loop
   lda #1
   ldy #0
   sta arg+0
   sty arg+1
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"-"
   bne keyArg
   inc arg+0
-  iny
   lda (zp),y
   beq keyArg
   cmp #"i"
   bne +
   lda #true
   sta ignoreCaseFlag
+  cmp #"v"
   bne +
   lda #true
   sta reverseFlag
+  jmp -

   keyArg = *
   lda arg
   ldy #0
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"+"
   bne fileArg
   jsr getKeyPosition
   inc arg

   fileArg = *
   jsr sortInit
-  jsr aceConStopkey
   bcs stopped
   lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   ora zp+1
   beq mainExit
   jsr sortRead
   bcc +
   jsr error
+  inc arg+0
   bne +
   inc arg+1
+  jmp -

mainExit = *
   jsr sortReal
   jsr sortPrint
   jsr sortClean
   rts

keyDigit = $71

getKeyPosition = *
   lda #0
   sta keyPosition
   iny
-  lda (zp),y
   cmp #"0"
   bcc +
   cmp #"9"+1
   bcs +
   and #$0f
   sta keyDigit
   lda keyPosition
   asl
   asl
   clc
   adc keyPosition
   asl
   clc
   adc keyDigit
   sta keyPosition
   iny
   bne -
+  rts

stopped = *
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   lda #1
   jmp aceProcExit
   stoppedMsg = *
   .asc "<Stopped>"
   .byte chrCR,0

error = *
   lda #<errorMsg1
   ldy #>errorMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<errorMsg2
   ldy #>errorMsg2
   jmp eputs
   errorMsg1 = *
   .asc "Error reading file "
   .byte chrQuote,0
   errorMsg2 = *
   .byte chrQuote,chrCR,0

bufPtr = 10   ;(1)
bufCount = 12 ;(1)
infile = 14   ;(1)

sortRead = *
   ;** open file
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   jsr readBody
   lda infile
   jsr close
   rts

;===sort routines===

sorthead = 30  ;(4)
sortP    = 34  ;(4)
sortQ    = 38  ;(4)
header   = 42  ;(5)

sortInit = *
   lda #"s"
   jsr echoStatus
   jsr mallocInit
   lda keyPosition
   cmp #1
   bcc +
   sbc #1
+  sta keyPosition
   ldx #3
   lda #aceMemNull
-  sta sorthead,x
   dex
   bpl -
   rts

readBody = *
   lda #"r"
   jsr echoStatus
   lda #0
   sta bufCount
-  jsr getline
   bcs +
   jsr positionLine
   jsr storeline
   bcs +
   lda #"."
   jsr echoStatus
   jmp -
+  rts

sortReal = *
   lda reverseFlag
   bmi +
   lda #"v"
   jsr echoStatus
   jsr reverseFile
+  rts

sortPrint = *
   lda #"w"
   jsr echoStatus
   lda #13
   jsr echoStatus
   jsr writefile
   rts

sortClean = *
   ;lda #"f"
   ;jsr echoStatus
   ;jsr freefile
   ;lda #13
   ;jsr echoStatus
   rts

;*** fetchline( sortP=LinePtr, .AY=Ram0buf )

fetchline = *
   sta zp+0
   sty zp+1
   ldx #3
-  lda sortP,x
   sta mp,x
   dex
   bpl -
   ldx #header
   ldy #5
   jsr aceMemZpload
   lda header+4
   ldy #0
   jmp aceMemFetch

;*** sortGTcmp( sortline, cmpline ) : .C={sortline >= cmpline}

sortGTcmp = *
   ldx #0
   clc
   lda keyPosition
   adc #6
   cmp sortbuflen
   bcc +
   inx
+  cmp cmpbuflen
   bcc +
   inx
   inx
+  txa
   beq doCompare
   cmp #2
   rts

   doCompare = *
   bit ignoreCaseFlag
   bmi doCaselessCompare
   ldy keyPosition
-  lda sortline,y
   cmp cmpline,y
   bne +
   cmp #0
   beq +
   iny
   bne -
+  rts

   caselessChar = $72

   doCaselessCompare = *
   ldy keyPosition
-  lda sortline,y
   jsr lowcase
   pha
   lda cmpline,y
   jsr lowcase
   sta caselessChar
   pla
   cmp caselessChar
   bne +
   cmp #0
   beq +
   iny
   bne -
+  rts

lowcase = *
   cmp #"A"
   bcc +
   cmp #"Z"+1
   bcs +
   and #$7f
+  rts

;*** positionLine( sortline ) : sortQ=prev, sortP=next

positionLine = *
   ldx #3
-  lda #aceMemNull
   sta sortQ,x
   lda sorthead,x
   sta sortP,x
   dex
   bpl -

   positionSearch = *
   lda sortP+3
   cmp #aceMemNull
   beq positionExit
   lda #<cmpbuf
   ldy #>cmpbuf
   jsr fetchline
   jsr sortGTcmp
   bcs positionExit    ;** controls sort order
   ldx #3
-  lda sortP,x
   sta sortQ,x
   lda cmpbuf,x
   sta sortP,x
   dex
   bpl -
   bmi positionSearch

   positionExit = *
   rts

;*** storeline( sortline )    {between sortQ and sortP}

storeline = *
   lda sortbuflen
   ldy #0
   jsr malloc
   bcc +
   rts
+  ldx #3
-  lda sortP,x
   sta sortbuf,x
   dex
   bpl -
   lda #<sortbuf
   ldy #>sortbuf
   sta zp+0
   sty zp+1
   lda sortbuflen
   ldy #0
   jsr aceMemStash
   lda sortQ+3
   cmp #aceMemNull
   beq storelineFirst
   ldx #3
-  lda mp,x
   ldy sortQ,x
   sta sortQ,x
   sty mp,x
   dex
   bpl -
   ldx #sortQ
   ldy #4
   jsr aceMemZpstore
   clc
   rts

   storelineFirst = *
   ldx #3
-  lda mp,x
   sta sorthead,x
   dex
   bpl -
   clc
   rts

;*** writefile()

writefile = *
   ldx #3
-  lda sorthead,x
   sta sortP,x
   dex
   bpl -

   writeLine = *
   lda sortP+3
   cmp #aceMemNull
   beq writeExit
   lda #<sortbuf
   ldy #>sortbuf
   jsr fetchline
   jsr putline
   ldx #3
-  lda sortbuf,x
   sta sortP,x
   dex
   bpl -
   jmp writeLine

   writeExit = *
   rts

;*** reverseList()

reverseFile = *
   ldx #3
-  lda sorthead,x
   sta mp,x
   lda #aceMemNull
   sta sorthead,x
   dex
   bpl -

   reverseLine = *
   lda mp+3
   cmp #aceMemNull
   beq reverseExit
   ldx #sortP
   ldy #4
   jsr aceMemZpload
   ldx #sorthead
   ldy #4
   jsr aceMemZpstore
   ldx #3
-  lda mp,x
   sta sorthead,x
   lda sortP,x
   sta mp,x
   dex
   bpl -
   bmi reverseLine

   reverseExit = *
   rts

;*** freefile()

freefile = *
   ldx #3
-  lda sorthead,x
   sta mp,x
   dex
   bpl -

   freeLine = *
   lda mp+3
   cmp #aceMemNull
   bne +
   rts
+  ldx #header
   ldy #5
   jsr aceMemZpload
   lda header+4
   ldy #0
   jsr free
   ldx #3
-  lda header,x
   sta mp,x
   dex
   bpl -
   jmp freeLine

echoStatus = *
   ldx #stderr
   jmp putc

;===dynamic memory routines===

mallocWork = $60

mallocHead   .buf 4
tpaFreeFirst .buf 1
tpaFreeMin   .buf 1
tpaFreePages .buf 1
tpaAreaStart .buf 1
tpaAreaEnd   .buf 1

;*** mallocInit()

mallocInit = *
   lda #aceMemNull
   sta mallocHead+3
   ldx #0
   lda #$ff
-  sta tpaFreemap,x
   inx
   bne -
   ldx #>bssEnd
   lda #<bssEnd
   beq +
   inx
+  stx tpaFreeFirst
   stx tpaAreaStart
   ldx aceMemTop+1
   stx mallocWork
   stx tpaAreaEnd
   txa
   sec
   sbc tpaFreeFirst
   bcs +
   lda #0
+  sta tpaFreePages
   clc
   adc #1
   sta tpaFreeMin
   ldx tpaFreeFirst
   cpx mallocWork
   bcs +
   lda #$00
-  sta tpaFreemap,x
   inx
   cpx mallocWork
   bcc -
+  rts

libPages .buf 1

libPageAlloc = *  ;( .A=pages ) : [mp]
   sta libPages
   ldx #$00
   ldy #aceMemInternal-1
   jsr aceMemAlloc
   bcs +
   rts
+  jsr tpaPageAlloc
   bcs +
   rts
+  lda libPages
   ldx #aceMemInternal
   ldy #$ff
   jsr aceMemAlloc
   bcs +
   rts
+  lda #<nomemMsg
   ldy #>nomemMsg
   jsr eputs
   lda #1
   jmp aceProcExit

   nomemMsg = *
   .byte chrCR
   .asc "Insufficient memory, aborting."
   .byte chrCR,0

newmax   .buf 1

tpaPageAlloc = *  ;( libPages ) : [mp]
   lda libPages
   cmp tpaFreeMin
   bcs tpaFreemapFull
   ;** first free
   ldx tpaFreeFirst
   lda tpaFreemap,x
   beq ++
-  inx
   beq tpaFreemapFull
   lda tpaFreemap,x
   bne -
   stx tpaFreeFirst
   jmp ++
   tpaFreemapFull = *
   lda libPages
   cmp tpaFreeMin
   bcs +
   sta tpaFreeMin
+  sec
   rts

   ;** search
+  dex
-  ldy libPages
-  inx
   beq tpaFreemapFull
   lda tpaFreemap,x
   bne --
   dey
   bne -

   ;** allocate
   stx newmax
   ldy libPages
   lda #$41
-  sta tpaFreemap,x
   dex
   dey
   bne -
   inx
   cpx tpaFreeFirst
   bne +
   ldy newmax
   iny
   sty tpaFreeFirst
+  sec
   lda tpaFreePages
   sbc libPages
   sta tpaFreePages
   lda #0
   ldy #aceMemInternal
   sta mp+0
   stx mp+1
   sta mp+2
   sty mp+3
   clc
   rts

mallocLenSave .buf 3

malloc = *
   sta mallocLenSave+0
   sty mallocLenSave+1
   jsr libMalloc
   bcs +
   rts
+  ldx mallocLenSave+1
   lda mallocLenSave+0
   beq +
   inx
+  txa
   cpx #>1024
   bcs +
   ldx #>1024
+  txa
   sta mallocLenSave+2
   jsr libPageAlloc
   bcc +
   rts
+  lda #0
   ldy mallocLenSave+2
   jsr free
   lda mallocLenSave+0
   ldy mallocLenSave+1
   jmp malloc

;*** malloc( .AY=Bytes ) : [mp]=FarPointer

mallocMemNextPtr = mallocWork+0 ;(4)
mallocMemLength  = mallocWork+4 ;(2)
mallocLength     = mallocWork+6 ;(2)
mallocQ          = mallocWork+8 ;(4)

libMalloc = *
   clc
   adc #7
   bcc +
   iny
+  and #$f8
   sta mallocLength
   sty mallocLength+1
   ldx #3
-  lda mallocHead,x
   sta mp,x
   lda #aceMemNull
   sta mallocQ,x
   dex
   bpl -

   mallocLook = *
   lda mp+3
   cmp #aceMemNull
   bne +

   mallocErrorExit = *
   lda #aceMemNull
   sta mp+3
   lda #aceErrInsufficientMemory
   sta errno
   sec
   rts

+  ldx #mallocMemNextPtr
   ldy #6
   jsr aceMemZpload
   lda mallocMemLength
   cmp mallocLength
   lda mallocMemLength+1
   sbc mallocLength+1
   bcs mallocGotBlock
   ldx #3
-  lda mp,x
   sta mallocQ,x
   lda mallocMemNextPtr,x
   sta mp,x
   dex
   bpl -
   jmp mallocLook

   mallocGotBlock = *
   lda mallocMemLength
   cmp mallocLength
   bne +
   lda mallocMemLength+1
   sbc mallocLength+1
   beq mallocTakeWholeBlock
+  sec
   lda mallocMemLength
   sbc mallocLength
   sta mallocMemLength
   lda mallocMemLength+1
   sbc mallocLength+1
   sta mallocMemLength+1
   ldx #mallocMemNextPtr
   ldy #6
   jsr aceMemZpstore
   clc
   lda mp+0
   adc mallocMemLength
   sta mp+0
   lda mp+1
   adc mallocMemLength+1
   sta mp+1
   clc
   rts

   mallocTakeWholeBlock = *
   lda mallocQ+3
   cmp #aceMemNull
   bne +
   ldx #3
-  lda mallocMemNextPtr,x
   sta mallocHead,x
   dex
   bpl -
   clc
   rts
+  ldx #3
-  lda mp,x
   ldy mallocQ,x
   sta mallocQ,x
   sty mp,x
   dex
   bpl -
   ldx #mallocMemNextPtr
   ldy #4
   jsr aceMemZpstore
   ldx #3
-  lda mallocQ,x
   sta mp,x
   dex
   bpl -
   clc
   rts

;*** free( [mp]=FarPointer, .AY=Length )  {alters [mp]}

freeMemNextPtr = mallocWork+0  ;(4)
freeMemLength  = mallocWork+4  ;(2)
freeLength     = mallocWork+6  ;(2)
freeNewPtr     = mallocWork+8  ;(4)
freeQ          = mallocWork+12 ;(4)

free = *
   clc
   adc #7
   bcc +
   iny
+  and #$f8
   sta freeLength+0
   sty freeLength+1
   ldx #3
-  lda mp,x
   sta freeNewPtr,x
   lda mallocHead,x
   sta mp,x
   lda #aceMemNull
   sta freeQ,x
   dex
   bpl -

   freeSearchLoop = *
   lda mp+3
   cmp #aceMemNull
   beq freeCoalesceQandNew
   lda mp+0
   cmp freeNewPtr+0
   lda mp+1
   sbc freeNewPtr+1
   lda mp+2
   sbc freeNewPtr+2
   lda mp+3
   sbc freeNewPtr+3
   bcs freeCoalesceQandNew
+  ldx #freeMemNextPtr
   ldy #4
   jsr aceMemZpload
   ldx #3
-  lda mp,x
   sta freeQ,x
   lda freeMemNextPtr,x
   sta mp,x
   dex
   bpl -
   bmi freeSearchLoop

   freeCoalesceQandNew = *
   ldx #3
-  lda freeQ,x
   sta mp,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   bne +
   ;** prev is head
   ldx #3
-  lda mallocHead,x
   sta freeMemNextPtr,x
   lda freeNewPtr,x
   sta mallocHead,x
   dex
   bpl -
   lda freeLength+0
   ldy freeLength+1
   sta freeMemLength+0
   sty freeMemLength+1
   jmp freeCoalesceNewAndP

   ;** prev is real
+  ldx #freeMemNextPtr
   ldy #6
   jsr aceMemZpload
   lda mp+3
   cmp freeNewPtr+3
   bne +
   lda mp+2
   cmp freeNewPtr+2
   bne +
   clc
   lda mp+0
   adc freeMemLength
   tax
   lda mp+1
   adc freeMemLength+1
   cmp freeNewPtr+1
   bne +
   cpx freeNewPtr
   bne +
   ;** prev does coalesce
   clc
   lda freeMemLength
   adc freeLength
   sta freeMemLength
   lda freeMemLength+1
   adc freeLength+1
   sta freeMemLength+1
   ldx #3
-  lda freeQ,x
   sta freeNewPtr,x
   dex
   bpl -
   bmi freeCoalesceNewAndP

   ;** prev does not coalesce
+  ldx #freeNewPtr
   ldy #4
   jsr aceMemZpstore
   lda freeLength+0
   ldy freeLength+1
   sta freeMemLength+0
   sty freeMemLength+1

   freeCoalesceNewAndP = *
   lda freeNewPtr+3
   cmp freeMemNextPtr+3
   bne +
   lda freeNewPtr+2
   cmp freeMemNextPtr+2
   bne +
   clc
   lda freeNewPtr
   adc freeMemLength
   tax
   lda freeNewPtr+1
   adc freeMemLength+1
   cmp freeMemNextPtr+1
   bne +
   cpx freeMemNextPtr
   bne +

   ;** new and next coalesce
   ldx #3
-  lda freeMemNextPtr,x
   sta mp,x
   dex
   bpl -
   lda freeMemLength+1
   pha
   lda freeMemLength+0
   pha
   ldx #freeMemNextPtr
   ldy #6
   jsr aceMemZpload
   clc
   pla
   adc freeMemLength+0
   sta freeMemLength+0
   pla
   adc freeMemLength+1
   sta freeMemLength+1

+  ldx #3
-  lda freeNewPtr,x
   sta mp,x
   dex
   bpl -
   ldx #freeMemNextPtr
   ldy #6
   jsr aceMemZpstore
   clc
   rts

;=== line I/O routines ===

ysave = $70

getline = *  ;( infile ) : sortline, .CS=eof
   ldy #0
-  sty ysave
   jsr getByte
   ldy ysave
   bcs +
   sta sortline,y
   iny
   cpy #240
   bcs getlineExit
   cmp #13
   bne -
   dey

   getlineExit = *
   lda #0
   sta sortline,y
   clc
   tya
   adc #6
   sta sortbuflen
   clc
+  rts

putline = *  ;( sortbuf )
   jsr aceConStopkey
   bcc +
   jsr stopped
+  ldy sortbuflen
   lda #13
   sta sortbuf-1,y
   lda #<sortline
   ldy #>sortline
   sta zp+0
   sty zp+1
   sec
   lda sortbuflen
   sbc #5
   ldy #0
   ldx #stdout
   jsr write
   ldy sortbuflen
   lda #0
   sta sortbuf-1,y
   rts

getByte = *
   lda bufCount
   beq getByteFillBuf
   ldy bufPtr
   lda inBuf,y
   inc bufPtr
   dec bufCount
   clc
   rts

getByteFillBuf = *
   jsr aceConStopkey
   bcc +
   jsr stopped
+  lda #<inBuf
   ldy #>inBuf
   sta zp+0
   sty zp+1
   lda #254
   ldy #0
   sty bufPtr
   ldx infile
   jsr read
   beq +
   bcs +
   sta bufCount
   jmp getByte
+  sec
   rts

;===bss===

bss        = *
sortbuf    = bss+0
sortbuflen = sortbuf+4
sortline   = sortbuf+5
cmpbuf     = sortbuf+256
cmpbuflen  = cmpbuf+4
cmpline    = cmpbuf+5
inBuf      = cmpbuf+256
tpaFreemap = inBuf+256
bssEnd     = tpaFreemap+256
