;*** memory status program

.seq "acehead.s"
.org aceAppAddress
.obj "@:mem"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

freeMemory  = 4  ;(4)
totalMemory = 8  ;(4)
processId   = 12 ;(4)
tpaMemory   = 16 ;(4)
memaddr     = 20 ;(4)
memcnt      = 24 ;(4)
memoff      = 28 ;(1)
xsave       = 29 ;(1)

main = *
   lda aceArgc+1
   bne +
   lda aceArgc+0
   cmp #1
   beq ++
+  jmp memArg
+  lda #0
   ldx #7
-  sta processId,x
   dex
   bpl -
   ldx #freeMemory
   jsr aceMemStat
   sta processId
   sec
   lda aceMemTop+0
   sbc #<aceAppAddress
   sta tpaMemory+0
   lda aceMemTop+1
   sbc #>aceAppAddress
   sta tpaMemory+1
   
   lda #<processMsg
   ldy #>processMsg
   jsr puts
   ldx #processId
   jsr putnum

   lda #<totalMsg
   ldy #>totalMsg
   jsr puts
   ldx #totalMemory
   jsr putnum

   lda #<freeMsg
   ldy #>freeMsg
   jsr puts
   ldx #freeMemory
   jsr putnum

   lda #<tpaMsg
   ldy #>tpaMsg
   jsr puts
   ldx #tpaMemory
   jsr putnum
   rts

processMsg = *
   .asc "ProcessID    ="
   .byte 0
totalMsg = *
   .asc "Total Memory ="
   .byte 0
freeMsg = *
   .asc "Dynamic Free ="
   .byte 0
tpaMsg = *
   .asc "Program Free ="
   .byte 0

numbuf .buf 13

putnum = *
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #10
   jsr aceMiscUtoa
   lda #13
   sta numbuf+0,y
   lda #0
   sta numbuf+1,y
   lda zp+0
   ldy zp+1
   jmp puts

;=== standard library ===

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

eputchar = *
   ldx #stderr
   jmp putc
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

;=== new stuff ===

puthex = *  ;( .A=hex )
   pha
   lsr
   lsr
   lsr
   lsr
   jsr +
   pla
   and #$0f
+  ora #$30
   cmp #$3a
   bcc +
   adc #6
+  jmp putchar

scanhex = *
   lda #0
   ldx #3
-  sta memaddr,x
   dex
   bpl -
   ldy #0
-  lda (zp),y
   bne +
   rts
+  and #$7f
   cmp #$41
   bcc +
   sbc #7
+  and #$0f
   pha
   ldx #4
-  asl memaddr+0
   rol memaddr+1
   rol memaddr+2
   rol memaddr+3
   dex
   bne -
   pla
   ora memaddr+0
   sta memaddr+0
   iny
   jmp --
   
memArg = *
   lda #1
   ldy #0
   jsr getarg
   jsr scanhex
   ldx #3
-  lda memaddr,x
   sta mp,x
   dex
   bpl -
   lda aceArgc+0
   cmp #3
   bne +
   lda aceArgc+1
   cmp #0
   bne +
   jmp memTestBandwidth
+  lda memaddr+0
   and #$f0
   sta memaddr+0
   lda #<memPage
   ldy #>memPage
   sta zp+0
   sty zp+1
   lda #<256
   ldy #>256
   jsr aceMemFetch
   lda #$00
   sta memoff
   lda #<headerMsg
   ldy #>headerMsg
   jsr puts

   memArgNext = *
   lda memaddr+3
   jsr puthex
   lda memaddr+2
   jsr puthex
   lda memaddr+1
   jsr puthex
   lda memaddr+0
   jsr puthex
   lda #":"
   jsr putchar
   lda #16
   sta memcnt
   ldx memoff
   stx xsave
-  lda #" "
   jsr putchar
   ldx xsave
   lda memPage,x
   jsr puthex
   inc xsave
   dec memcnt
   bne -
   lda #" "
   jsr putchar
   lda #";"
   jsr putchar
   lda #16
   sta memcnt
   ldx memoff
   stx xsave
-  ldx xsave
   lda memPage,x
   cmp #$20
   bcc dispDot
   cmp #$60
   bcc dispAsis
   cmp #$a0
   bcc dispDot
   cmp #$e0
   bcc dispAsis
   dispDot = *
   lda #"."
   dispAsis = *
   jsr putchar
   inc xsave
   dec memcnt
   bne -
   lda #chrCR
   jsr putchar
   clc
   lda memaddr+0
   adc #16
   sta memaddr+0
   bcc +
   inc memaddr+1
+  lda xsave
   sta memoff
   bne memArgNext
   rts

;format:
;12345678: 00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff ;0123456789abcdef

headerMsg = *
   .asc "addr\off:  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f"
   .byte chrCR,0

memTestBandwidth = *
   lda #2
   ldy #0
   jsr getarg
   ldy #0
   lda (zp),y
   cmp #"l"
   bne +
   jmp memTestLatency
+  lda #<memTestBandwidthMsg1
   ldy #>memTestBandwidthMsg1
   jsr puts
   lda #<memTest
   ldy #>memTest
   sta zp+0
   sty zp+1
   lda #>8192
   sta memcnt+1
   lda #0
   sta memcnt+0
-  lda #<16384
   ldy #>16384
   jsr aceMemFetch
   inc memcnt+0
   bne -
   dec memcnt+1
   bne -
   lda #<memTestBandwidthMsg2
   ldy #>memTestBandwidthMsg2
   jsr puts
   rts

memTestBandwidthMsg1 = *
   .asc "Bandwidth test: fetching 16384 bytes 8192 times..."
   .byte chrCR
   .asc "start"
   .byte 7,chrCR,0
memTestBandwidthMsg2 = *
   .byte 7
   .asc "finshed"
   .byte chrCR,0

memTestLatency = *
   lda #<memTestLatencyMsg1
   ldy #>memTestLatencyMsg1
   jsr puts
   lda #<memTest
   ldy #>memTest
   sta zp+0
   sty zp+1
   lda #16
   sta memcnt+2
   lda #0
   sta memcnt+1
   lda #0
   sta memcnt+0
-  lda #1
   ldy #0
   jsr aceMemFetch
   inc memcnt+0
   bne -
   dec memcnt+1
   bne -
   dec memcnt+2
   bne -
   lda #<memTestLatencyMsg2
   ldy #>memTestLatencyMsg2
   jsr puts
   rts

memTestLatencyMsg1 = *
   .asc "Latency test: fetching one byte 1_048_576 times..."
   .byte chrCR
   .asc "start"
   .byte 7,chrCR,0
memTestLatencyMsg2 = *
   .byte 7
   .asc "finshed"
   .byte chrCR,0

;===bss===

bss      = *
memPage  = bss+0
memTest  = memPage+256
bssEnd   = memTest+16384
