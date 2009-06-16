;*** window program - by Craig Bruce - 21-Feb-94

.seq "acehead.s"
.org aceAppAddress
.obj "@0:window"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

scanVal = $20
rows .buf 1
cols .buf 1
startRow .buf 1
startCol .buf 1

;===window===

main = *
   jsr aceWinSize
   sta rows
   stx cols
   lda syswork+0
   ldx syswork+1
   sta startRow
   stx startCol
   ;** check argument count
   lda aceArgc+1
   bne enoughArgs
   lda aceArgc+0
   cmp #2
   bcs enoughArgs

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp winParms

usageMsg = *
   .asc "usage: window columns [rows [startColumn [startRow]]]"
   .byte chrCR,0

enoughArgs = *
   lda #1
   ldy #0
   jsr getarg
   beq mainFinish
   jsr scanNum
   lda scanVal+0
   sta cols

   lda #2
   ldy #0
   jsr getarg
   beq mainFinish
   jsr scanNum
   lda scanVal+0
   sta rows

   lda #3
   ldy #0
   jsr getarg
   beq mainFinish
   jsr scanNum
   lda scanVal+0
   sta startCol

   lda #4
   ldy #0
   jsr getarg
   beq mainFinish
   jsr scanNum
   lda scanVal+0
   sta startRow

mainFinish = *
   lda startRow
   ldx startCol
   sta syswork+0
   stx syswork+1
   lda rows
   ldx cols
   jsr aceWinSet
   bcs mainError
   lda #$80
   ldx #$20
   jsr aceWinCls

   winParms = *
   lda cols
   jsr putnum
   lda #","
   jsr putchar
   lda rows
   jsr putnum
   lda #","
   jsr putchar
   lda startCol
   jsr putnum
   lda #","
   jsr putchar
   lda startRow
   jsr putnum
   lda #chrCR
   jsr putchar
   rts

mainError = *
   lda #<mainErrorMsg
   ldy #>mainErrorMsg
   jmp eputs
mainErrorMsg = *
   .asc "winset: invalid window parameters"
   .byte chrCR,0

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

numbuf .buf 11

putnum = *
   sta $30
   lda #0
   sta $31
   sta $32
   sta $33
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   ldx #$30
   jsr aceMiscUtoa
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   jsr puts
   rts

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
   ora zp+0
   rts

scanDigit .buf 1
scanSave .buf 4
scanTemp .buf 1
scanIndex .buf 1
scanAnything .buf 1

scanNum = *  ;( (zp)=numStr ) : .Y=scan, [scanVal]=num, .CS=err
   ldy #0
   ldx #3
   lda #0
-  sta scanVal,x
   dex
   bpl -
   lda #0
   sta scanAnything
-  lda (zp),y
   cmp #" "
   bne scanNumNext
   iny
   bne -
   sec
   rts

   scanNumNext = *
   lda (zp),y
   cmp #"0"
   bcc +
   cmp #"9"+1
   bcc ++
+  lda scanAnything
   beq scanError
   clc
   rts
+  and #$0f
   sta scanDigit
   lda #$ff
   sta scanAnything
   ;** times ten
   sty scanTemp
   ldx #3
-  lda scanVal,x
   sta scanSave,x
   dex
   bpl -
   lda #2
   sta scanIndex
-  clc
   ldy #4
   ldx #0
-  rol scanVal,x
   inx
   dey
   bne -
   bcs scanError
   dec scanIndex
   bne --
   clc
   ldy #4
   ldx #0
-  lda scanVal,x
   adc scanSave,x
   sta scanVal,x
   inx
   dey
   bne -
   bcs scanError
   clc
   ldy #4
   ldx #0
-  rol scanVal,x
   inx
   dey
   bne -
   bcs scanError
   clc
   ldy #4
   ldx #0
   lda scanDigit
-  adc scanVal,x
   sta scanVal,x
   lda #0
   inx
   dey
   bne -
   bcs scanError

   ldy scanTemp
   iny
   beq scanError
   jmp scanNumNext

   scanError = *
   sec
   rts

;===the end===
bss = *
