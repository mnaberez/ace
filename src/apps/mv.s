;*** mv (rename) program

!src "../system/acehead.s"
!to "../../build/mv", cbm

*= aceAppAddress

jmp main
!byte aceID1,aceID2,aceID3
!byte 64,0  ;** stack,reserved

;*** global declarations

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
   getcBuffer = *
      !fill 1

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

;===rename===

arg = 2
oldname = 4
newname = 6

main = *
   ;** check argument count
   lda aceArgc+1
   bne enoughArgs
   lda aceArgc+0
   cmp #3
   bcs enoughArgs

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jmp puts

usageMsg = *
   !pet "Usage: mv oldname newname ..."
   !byte chrCR
   !pet "       where ... means repeat names in old/new pairs"
   !byte chrCR,0

enoughArgs = *
   lda #1
   ldy #0
   sta arg+0
   sty arg+1

   mainNext = *
   lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta oldname+0
   sty oldname+1
   ora zp+1
   beq mainExit
   inc arg+0
   bne +
   inc arg+1
+  lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ldy zp+1
   sta newname+0
   sty newname+1
   ora zp+1
   beq mainExit

   jsr checkstop
   jsr echo
   lda oldname+0
   ldy oldname+1
   sta zp+0
   sty zp+1
   lda newname+0
   ldy newname+1
   sta zw+0
   sty zw+1
   jsr aceFileRename
   bcc +
   jsr error
+  inc arg+0
   bne +
   inc arg+1
+  jmp mainNext

mainExit = *
   rts

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
   !pet "<stopped>"
   !byte chrCR,0

error = *
   lda #<errorMsg1
   ldy #>errorMsg1
   jsr eputs
   lda oldname+0
   ldy oldname+1
   jsr eputs
   lda #<errorMsg2
   ldy #>errorMsg2
   jmp eputs

errorMsg1 = *
   !pet "Error attempting to rename "
   !byte chrQuote,0

errorMsg2 = *
   !byte chrQuote,chrCR,0

echo = *
   lda #<echoMsg1
   ldy #>echoMsg1
   jsr eputs
   lda oldname+0
   ldy oldname+1
   jsr eputs
   lda #<echoMsg2
   ldy #>echoMsg2
   jsr eputs
   lda newname+0
   ldy newname+1
   jsr eputs
   lda #<echoMsg3
   ldy #>echoMsg3
   jsr eputs
   rts   

echoMsg1 = *
   !pet "Renaming file "
   !byte chrQuote,0

echoMsg2 = *
   !byte chrQuote
   !pet " to "
   !byte chrQuote,0

echoMsg3 = *
   !byte chrQuote,chrCR,0

;===the end===
bss    = *
bssEnd = bss+0
