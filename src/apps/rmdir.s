;*** rmdir program

!src "../system/acehead.s"
!to "../../build/rmdir", cbm
!convtab pet

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

libwork = $60

chrQuote = $22

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
putcBuffer = *
   !fill 1

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
getcBuffer 
   !fill 1

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
   stx zp
   sta zp+1
   rts

;===remove===
arg = 2
name = 4

main = *
   ;** check argument count
   lda aceArgc+1
   bne enoughArgs
   lda aceArgc+0
   cmp #2
   bcs enoughArgs

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jmp puts

usageMsg = *
   !pet "Usage: rmdir flatDirName ..."
   !byte chrCR,0

enoughArgs = *
   lda #1
   ldy #0
   sta arg+0
   sty arg+1
-  lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   ora zp+1
   beq prgExit
   jsr aceConStopkey
   bcs stopped
   jsr echo
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   jsr aceDirRemove
   bcc +
   jsr error
+  inc arg+0
   bne +
   inc arg+1
+  jmp -

prgExit = *
   rts

stopped = *
   lda #<stoppedMsg
   ldy #>stoppedMsg
   jmp eputs
   stoppedMsg = *
   !pet "<stopped>"
   !byte chrCR,0

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
   !pet "Error attempting remove dir "
   !byte chrQuote,0
errorMsg2 = *
   !byte chrQuote,chrCR,0

echo = *
   lda #<echoMsg1
   ldy #>echoMsg1
   jsr eputs
   lda name+0
   ldy name+1
   jsr eputs
   lda #<echoMsg2
   ldy #>echoMsg2
   jmp eputs

echoMsg1 = *
   !pet "Removing directory "
   !byte chrQuote,0

echoMsg2 = *
   !byte chrQuote,chrCR,0

;===the end===
end = *
