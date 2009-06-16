;*** uuencode program

.seq "acehead.s"
.org aceAppAddress
.obj "@0:uuencode"

jmp uuencodeMain
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $60
uueArg    = 2  ;(2)
uueName   = 4  ;(2)
inBufLen  = 6  ;(2)
bufPtr    = 8  ;(2)
bufCount  = 10 ;(2)
infile    = 12 ;(1)
datalen   = 13 ;(1)
outlinelen = 14 ;(1)
uutemp    = 15 ;(1)
uubuf     = 16 ;(3)
asciiFlag = 20 ;(1)
crFlag    = 21 ;(1)
lfFlag    = 22 ;(1)

chrQuote = $22

;===uuencode===

uuencodeMain = *
   ;** check argument count
   lda aceArgc+1
   bne uueEnoughArgs
   lda aceArgc
   cmp #2
   bcs uueEnoughArgs

uueUsage = *
   lda #<uueUsageMsg
   ldy #>uueUsageMsg
   jmp puts

uueUsageMsg = *
   .byte "Usage: uuencode [-aulc] file1 file2 ... fileN",chrCR
   .byte "       -a=asciiCrLf, -u=asciiLf, -l=asciiLf, -c=asciiCr",chrCR,0

uueEnoughArgs = *
   ;** get input buffer length
   sec
   lda aceMemTop+0
   sbc #<uueInBuf
   sta inBufLen
   lda aceMemTop+1
   sbc #>uueInBuf
   sta inBufLen+1
   lda #$00
   sta asciiFlag
   sta lfFlag
   lda #$ff
   sta crFlag
   ;** main loop
   lda #1
   ldy #0
   sta uueArg
   sty uueArg+1
-  jsr checkstop
   lda uueArg
   ldy uueArg+1
   jsr getarg
   lda zp
   ldy zp+1
   sta uueName
   sty uueName+1
   ora zp+1
   beq uueExit
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jsr handleFlags
   jmp ++
+  jsr uueEcho
   jsr uuencode
   bcc +
   jsr uueError
+  inc uueArg
   bne +
   inc uueArg+1
+  jmp -

uueExit = *
   rts

handleFlags = *
-  iny
   lda (zp),y
   bne +
   rts
+  ldx #$ff
   cmp #"a"
   bne +
   stx asciiFlag
   stx lfFlag
   stx crFlag
   jmp -
+  cmp #"u"
   bne +
-  stx asciiFlag
   stx lfFlag
   ldx #$00
   stx crFlag
   jmp --
+  cmp #"l"
   beq -
+  cmp #"c"
   bne +
   stx asciiFlag
   stx crFlag
   ldx #$00
   stx lfFlag
   jmp --
+  jmp uueUsage

uueError = *
   lda #<uueErrorMsg1
   ldy #>uueErrorMsg1
   jsr eputs
   lda uueName
   ldy uueName+1
   jsr eputs
   lda #<uueErrorMsg2
   ldy #>uueErrorMsg2
   jmp eputs

uueErrorMsg1 = *
   .asc "Error attempting to uuencode "
   .byte chrQuote
   .byte 0

uueErrorMsg2 = *
   .byte chrQuote
   .byte chrCR
   .byte 0

uueEcho = *
   lda #<uueEchoMsg1
   ldy #>uueEchoMsg1
   jsr eputs
   lda uueName
   ldy uueName+1
   jsr eputs
   lda #<uueEchoMsg2
   ldy #>uueEchoMsg2
   jmp eputs

uueEchoMsg1 = *
   .asc "Uuencoding file "
   .byte chrQuote
   .byte 0

uueEchoMsg2 = *
   .byte chrQuote
   .asc "..."
   .byte chrCR
   .byte 0

checkstop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   lda #1
   ldx #0
   jmp aceProcExit

   stoppedMsg = *
   .asc "<Stopped>"
   .byte chrCR,0

uuencode = *
   ;** open file
   lda uueName
   ldy uueName+1
   sta zp
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   rts
+  sta infile
   ;** encode file
   jsr uuencodeBody
   ;** close file
   lda infile
   jsr close
   rts

uuencodeBody = *
   lda #0
   sta bufCount+0
   sta bufCount+1
   ;** header line
   lda #<uueHeaderMsg
   ldy #>uueHeaderMsg
   bit asciiFlag
   bpl +
   lda #<uueAsciiHeaderMsg
   ldy #>uueAsciiHeaderMsg
+  jsr puts
   lda uueName+0
   ldy uueName+1
   sta zp+0
   sty zp+1
   jsr basename
   bit asciiFlag
   bpl +
   jsr outBufToAscii
+  lda #<uueOutBuf
   ldy #>uueOutBuf
   jsr puts
   jsr uueCrLf
   ;** loop
-  jsr uuLine
   bcc -
   ;** zero line
   lda #"`"
   bit asciiFlag
   bpl +
   lda #$60
+  jsr putchar
   jsr uueCrLf
   ;** end line
   lda #<uueEndMsg
   ldy #>uueEndMsg
   bit asciiFlag
   bpl +
   lda #<uueAsciiEndMsg
   ldy #>uueAsciiEndMsg
+  jsr puts
   jsr uueCrLf
   rts

uueCrLf = *
   bit crFlag
   bpl +
   lda #$0d
   jsr putchar
+  bit lfFlag
   bpl +
   lda #$0a
   jsr putchar
+  rts

uueHeaderMsg = *
   .byte "begin 640 ",0
uueAsciiHeaderMsg = *
   .byte $62,$65,$67,$69,$6e,$20,$36,$34,$30,$20,0

uueEndMsg = *
   .byte "end",0
uueAsciiEndMsg = *
   .byte $65,$6e,$64,0

getByte = *
   lda bufCount
   ora bufCount+1
   beq getByteFillBuf
   ldy #0
   lda (bufPtr),y
   inc bufPtr
   bne +
   inc bufPtr+1
+  ldx bufCount
   bne +
   dec bufCount+1
+  dec bufCount
   clc
   rts

getByteFillBuf = *
   jsr checkstop
   lda #<uueInBuf
   ldy #>uueInBuf
   sta zp
   sty zp+1
   sta bufPtr
   sty bufPtr+1
   lda inBufLen
   ldy inBufLen+1
   ldx infile
   jsr read
   beq +
   bcs +
   sta bufCount
   sty bufCount+1
   jmp getByte
+  sec
   rts

uuLine = *
   lda #0
   sta datalen
   lda #1
   sta outlinelen
   ;** get the line
-  jsr getFourChars
   bcs +
   lda datalen
   cmp #45
   bcc -
   clc
   ;** put the line
+  php
   ldy outlinelen
   bit crFlag
   bpl +
   lda #$0d
   sta uueOutBuf,y
   inc outlinelen
   iny
+  bit lfFlag
   bpl +
   lda #$0a
   sta uueOutBuf,y
   inc outlinelen
+  lda datalen
   cmp #0
   beq +
   jsr getUuchar
   sta uueOutBuf
   lda #<uueOutBuf
   ldy #>uueOutBuf
   sta zp
   sty zp+1
   lda outlinelen
   ldy #0
   ldx #1
   jsr write
+  plp
   rts

getFourChars = *
   ldx #2
   lda #0
-  sta uubuf,x
   dex
   bpl -
   ;** read the bytes
   ldx #0
-  stx uutemp
   jsr getByte
   bcs +
   ldx uutemp
   sta uubuf,x
   inc datalen
   inx
   cpx #3
   bcc -
   clc
   ;** put bytes into output line
;pos  76543210  76543210  76543210  76543210
;byt  xx111111  xx112222  xx222233  xx333333
;bit    765432    107654    321076    543210
+  php
   ldx uutemp
   cpx #0
   bne +
   plp
   rts
+  ldy outlinelen
   ;** first byte
   lda uubuf
   lsr
   lsr
   jsr getUuchar
   sta uueOutBuf,y
   iny

   ;** second byte
   lda uubuf
   asl
   asl
   asl
   asl
   sta uutemp
   lda uubuf+1
   lsr
   lsr
   lsr
   lsr
   ora uutemp
   jsr getUuchar
   sta uueOutBuf,y
   iny

   ;** third byte
   lda uubuf+1
   asl
   asl
   sta uutemp
   lda uubuf+2
   asl
   rol
   rol
   and #%00000011
   ora uutemp
   jsr getUuchar
   sta uueOutBuf,y
   iny

   ;** fourth byte
   lda uubuf+2
   jsr getUuchar
   sta uueOutBuf,y
   iny
   sty outlinelen
   plp
   rts

getUuchar = *
   bit asciiFlag
   bmi getUucharAscii
   and #%00111111
   bne +
   lda #"`"
   rts
+  clc
   adc #" "
   cmp #"a"
   bcs +
   rts
+  cmp #"["
   bcs +
   adc #128
+  rts

getUucharAscii = *
   and #%00111111
   bne +
   lda #$60
   rts
+  clc
   adc #$20
   rts

;******** standard library ********

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
eputs = *
   ldx #stderr
   jmp fputs

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
   putcBuffer .buf 1

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
   getcBuffer .buf 1

getarg = *
   sty zp+1
   asl
   sta zp
   rol zp+1
   clc
   lda aceArgv
   adc zp
   sta zp
   lda aceArgv+1
   adc zp+1
   sta zp+1
   ldy #0
   lda (zp),y
   tax
   iny
   lda (zp),y
   stx zp
   sta zp+1
   rts

basenameStart = libwork

basename = * ;( (zp)=inname ) : uueOutBuf=outname
   ldy #255
   sty basenameStart
-  iny
   lda (zp),y
   beq basenameDone
   cmp #":"
   beq +
   cmp #"/"
   bne -
+  sty basenameStart
   jmp -
   basenameDone = *
   ldy basenameStart
   ldx #255
-  iny
   inx
   lda (zp),y
   sta uueOutBuf,x
   bne -
   cpx #2
   bcc +
   lda uueOutBuf-2,x
   cmp #","
   bne +
   lda #0
   sta uueOutBuf-2,x
+  rts

outBufToAscii = * ;uueOutBuf=str
   ldy #0
-  lda uueOutBuf,y
   bne +
   rts
+  cmp #$20
   bcs +
-  lda #$5f
   jmp strAscNext
+  cmp #$41
   bcc strAscNext
   cmp #$5b
   bcs +
   adc #$20
   jmp strAscNext
+  cmp #$60
   bcc strAscNext
   cmp #$c0
   bcc -
   bne +
   lda #$60
   jmp strAscNext
+  cmp #$db
   bcs +
   and #$7f
   jmp strAscNext
+  cmp #$e0
   bcs -
   sec
   sbc #$60

   strAscNext = *
   sta uueOutBuf,y
   iny
   bne --
   rts

;===the end===

uueEnd = *
uueOutBuf = uueEnd
uueInBuf = uueOutBuf+80
