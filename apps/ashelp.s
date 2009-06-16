;** assembler program utility file - by Craig Bruce - 26-Sep-1994

;======== assemble processor instruction ========

; num bkt ins     num bkt ins     num bkt ins     num bkt ins     num bkt ins
; 00. xx. xxx     12. 63. bvc     24. 22. eor     36. 20. pha     48. 27. sta
; 01. 12. adc     13. 15. bvs     25. 05. inc     37. 35. php     49. 50. stx
; 02. 63. and     14. 38. clc     26. 26. inx     38. 40. pla     50. 51. sty
; 03. 00. asl     15. 39. cld     27. 27. iny     39. 55. plp     51. 12. tax
; 04. 00. bcc     16. 44. cli     28. 06. jmp     40. 20. rol     52. 13. tay
; 05. 16. bcs     17. 57. clv     29. 38. jsr     41. 26. ror     53. 38. tsx
; 06. 24. beq     18. 24. cmp     30. 28. lda     42. 10. rti     54. 08. txa
; 07. 47. bit     19. 15. cpx     31. 51. ldx     43. 20. rts     55. 26. txs
; 08. 56. bmi     20. 16. cpy     32. 52. ldy     44. 03. sbc     56. 45. tya
; 09. 25. bne     21. 60. dec     33. 24. lsr     45. 50. sec
; 10. 42. bpl     22. 17. dex     34. 52. nop     46. 51. sed
; 11. 51. brk     23. 18. dey     35. 45. ora     47. 56. sei

instrNames = *  ;names of instruction, 3 chars each
   .asc "xxxadcandaslbccbcsbeqbitbmibnebplbrkbvcbvsclccldcliclvcmpcpxcpy"
   .asc "decdexdeyeorincinxinyjmpjsrldaldxldylsrnoporaphaphpplaplprolror"
   .asc "rtirtssbcsecsedseistastxstytaxtaytsxtxatxstya"

instrHashPtrs = *  ;pointers to instruction numbers or >=100=indirects
   ;bucketx0..x1..x2..x3..x4..x5..x6..x7..x8..x9
   .byte 100,000,000,044,000,025,028,000,054,000  ;buckets 00-09
   .byte 042,000,103,052,000,106,109,022,023,000  ;buckets 10-19
   .byte 112,000,024,000,116,009,120,124,030,000  ;buckets 20-29
   .byte 000,000,000,000,000,037,000,000,127,015  ;buckets 30-39
   .byte 038,000,010,000,016,131,000,007,000,000  ;buckets 40-49
   .byte 134,137,142,000,000,039,145,017,000,000  ;buckets 50-59
   .byte 021,000,000,148                          ;buckets 60-63

instrHashIndirects = *
   .byte 004,003,000     ;off=100, bucket=00, instrs=bcc,asl
   .byte 001,051,000     ;off=103, bucket=12, instrs=adc,tax
   .byte 019,013,000     ;off=106, bucket=15, instrs=cpx,bvs
   .byte 005,020,000     ;off=109, bucket=16, instrs=bcs,cpy
   .byte 043,036,040,000 ;off=112, bucket=20, instrs=rts,pha,rol
   .byte 006,018,033,000 ;off=116, bucket=24, instrs=beq,cmp,lsr
   .byte 026,041,055,000 ;off=120, bucket=26, instrs=inx,ror,txs
   .byte 048,027,000     ;off=124, bucket=27, instrs=sta,iny
   .byte 029,014,053,000 ;off=127, bucket=38, instrs=jsr,clc,tsx
   .byte 035,056,000     ;off=131, bucket=45, instrs=ora,tya
   .byte 049,045,000     ;off=134, bucket=50, instrs=stx,sec
   .byte 031,050,011,046,000;off=137,buck=51, instrs=ldx,sty,brk,sed
   .byte 032,034,000     ;off=142, bucket=52, instrs=ldy,nop
   .byte 008,047,000     ;off=145, bucket=56, instrs=bmi,sei
   .byte 002,012,000     ;off=148, bucket=63, instrs=and,bvc

parseIdCheckInstr = *  ;( .A=instrNumToCheck ) : .CS=notInstr, .A+.X:unch
   sta work
   asl
   adc work
   tay
   lda stringBuf+0
   and #$7f
   cmp instrNames+0,y
   bne +
   lda stringBuf+1
   and #$7f
   cmp instrNames+1,y
   bne +
   lda stringBuf+2
   and #$7f
   cmp instrNames+2,y
   bne +
   clc
   lda work
   rts
+  sec
   rts

instr = *  ;( .A=instrNum )
   ;** got instruction number
   sta instrNum
   bit originSet
   bmi +
   lda #errOriginNotSet
   jmp error
+  bit debug
   bpl +
   sta number+0
   lda #0
   sta number+1
   sta number+2
   sta number+3
   lda #<instrMsg
   ldy #>instrMsg
   jsr puts
   ldx #number
   jsr putnum
   lda #chrCR
   jsr putchar

   ;** get addressing mode and value
+  jsr instrGetAddrMode
   bit debug
   bpl +++
   lda instrAddrMode
   sta number+0
   lda #0
   sta number+1
   sta number+2
   sta number+3
   lda #<instrAddrMsg1
   ldy #>instrAddrMsg1
   jsr puts
   ldx #number
   jsr putnum
   lda #<instrAddrMsg2
   ldy #>instrAddrMsg2
   jsr puts
   lda instrValue+0
   ldy instrValue+1
   sta number+0
   sty number+1
   lda instrValueType
   bmi +
   ldx #number
   jsr putnum
   jmp ++
+  lda #"?"
   jsr putchar
+  lda #chrCR
   jsr putchar

   ;** get opcode for instr/addrmode
+  nop
-  lda instrNum
   ldx instrAddrMode
   jsr instrGetOpcode
   bcc +
   ldx instrAddrMode
   lda instrAdmodePromote,x
   sta instrAddrMode
   bne -
   lda #errWrongAdmode
   jmp error
+  bit debug
   bpl ++
   lda #<instrCodeMsg
   ldy #>instrCodeMsg
   jsr puts
   lda address+1
   jsr puthex
   lda address+0
   jsr puthex
   lda #":"
   jsr putchar
   lda #" "
   jsr putchar
   lda instrOpcode
   jsr puthex
   lda instrLen
   cmp #1
   beq +
   lda #" "
   jsr putchar
   lda instrValue+0
   jsr puthex
   lda instrLen
   cmp #2
   beq +
   lda #" "
   jsr putchar
   lda instrValue+1
   jsr puthex
+  lda #chrCR
   jsr putchar

   ;** store the instruction into memory
+  lda instrOpcode
   sta memPutVals+0
   lda instrValue+0
   ldy instrValue+1
   sta memPutVals+1
   sty memPutVals+2
   ldx address+0
   ldy address+1
   lda instrLen
   jsr memPut

   ;** add relocatable reference
   lda instrValueType
   beq ++
   cmp #$04
   bcs ++
   ldx address+0
   ldy address+1
   inx
   bne +
   iny
+  jsr recordRelocRef

   ;** increment address and finish
+  clc
   lda address+0
   adc instrLen
   sta address+0
   bcc +
   inc address+1
   bne +
   lda #errAddressWrap
   jmp error
+  jmp parseEnd

instrMsg = *
   .asc "got processor instruction="
   .byte 0
instrAddrMsg1 = *
   .asc "got address mode="
   .byte 0
instrAddrMsg2 = *
   .asc ", value="
   .byte 0
instrCodeMsg = *
   .asc "code = "
   .byte 0
instrAdmodePromote = *
   .byte $00,$00,$00,$00,$07,$08,$09,$00,$00,$00,$00,$00,$00

; num  name       gen  byt  tokens
; ---  ---------  ---  ---  -------
; 00.  <none>     00.    0  <none>
; 01.  implied    00.    1  CR
; 02.  immediate  00.    2  #     / exp8  / CR
; 03.  relative   00.    2  exp16 / CR
; 04.  zeropage   07.    2  exp8  / CR
; 05.  zp,x       08.    2  exp8  / ,     / x   / CR
; 06.  zp,y       09.    2  exp8  / ,     / y   / CR
; 07.  absolute   00.    3  exp16 / CR
; 08.  abs,x      00.    3  exp16 / ,     / x   / CR
; 09.  abs,y      00.    3  exp16 / ,     / y   / CR
; 10.  indirect   00.    3  (     / exp16 / )   / CR
; 11.  (ind,x)    00.    2  (     / exp8  / ,   / x   / )  / CR
; 12.  (ind),y    00.    2  (     / exp8  / )   / ,   / y  / CR
;
; All zp modes can be promoted to abs except for zp,y for stx and zp,x for sty.
; Indy mode causes some special problems--I'll have to patch the hole.

instrGetAddrMode = *  ;( address, instrNum ) : instrAddrMode, instrValue,
   lda #$00             ;                        instrValueType
   sta instrValue+0
   sta instrValue+1
   sta instrValueType
   lda tokenNextChar
   cmp #"#"
   beq admodeImmediate
   cmp #"("
   beq admodeIndirect
   cmp #chrCR
   bne +
   jsr getToken  ;get the CR
   lda #01       ;implied
   sta instrAddrMode
   rts
+  lda instrNum
   cmp #14
   bcs +
   cmp #4
   bcc +
   cmp #7
   beq +
   cmp #11
   beq +
   jmp admodeRelative
   ;** zp or abs, straight or indexed
+  lda #0
   jsr admodeHandleExpr
   ldy #04  ;zeropage mode
   cpx #1
   beq +
   ldy #07  ;absolute mode
+  sty instrAddrMode
   cmp #chrCR
   bne +
   rts
+  cmp #","
   beq +
-  jmp syntaxError
+  jsr admodeGetXY
   inc instrAddrMode
   cpx #0
   beq +
   inc instrAddrMode
+  cmp #chrCR
   bne -
   rts

admodeImmediate = *
   jsr getToken  ;get the "#"
   lda #1
   jsr admodeHandleExpr
   cmp #chrCR
   beq +
   jmp syntaxError
+  lda #02
   sta instrAddrMode
   rts

admodeRelative = *
   lda #2
   jsr admodeHandleExpr
   cmp #chrCR
   beq +
   jmp syntaxError
+  lda #03
   sta instrAddrMode
   rts

admodeIndirect = *
   jsr getToken  ;get the "("
   lda #0
   jsr parseExpression
   cmp #","
   beq admodeIndX
   cmp #")"
   beq +
-  jmp syntaxError
+  jsr getToken
   ldx tokenType
   cpx #tokenSpecial
   bne -
   lda tokenChar
   cmp #","
   beq admodeIndY
   cmp #chrCR
   bne -
   lda #0
   sta admodeMustBe
   jsr admodeHandleExprIn
   lda #10
   sta instrAddrMode
   rts

   admodeIndX = *
   lda #1
   sta admodeMustBe
   jsr admodeHandleExprIn
   jsr admodeGetXY
   cpx #0
   beq +
-  jmp syntaxError
+  cmp #")"
   bne -
   jsr getToken
   cpx #tokenSpecial
   bne -
   cmp #chrCR
   bne -
   lda #11
   sta instrAddrMode
   rts

   admodeIndY = *
   lda #1
   sta admodeMustBe
   jsr admodeHandleExprIn
   jsr admodeGetXY
   cpx #1
   beq +
-  jmp syntaxError
+  cmp #chrCR
   bne -
   lda #12
   sta instrAddrMode
   rts

admodeGetXY = *  ;( .X=tokenType, token ) : .A=specialChar, .X=0:x/1:y
   jsr getToken
   ldx tokenType
   cpx #tokenIdentifier
   beq +
-  jmp syntaxError
+  lda stringLen
   cmp #1
   bne -
   lda stringBuf+0
   and #$7f
   ldy #0
   cmp #"x"
   beq +
   iny
   cmp #"y"
   bne -
+  tya
   pha
   jsr getToken
   ldx tokenType
   cpx #tokenSpecial
   bne -
   pla
   tax
   lda tokenChar
   rts

admodeMustBe .buf 1
admodeChar   .buf 1
admodeHole   .buf 1

admodeHandleExpr = *  ;( .A=1:byte+2:rel, address ) : .A=nextChar, .X=valBytes,
   sta admodeMustBe   ;                               instrValue, instrValueType
   lda #0
   jsr parseExpression
   sta admodeChar

   admodeHandleExprIn = *
   jsr evaluateExpression
   lda idVal+0
   ldy idVal+1
   sta instrValue+0
   sty instrValue+1
   lda idType
   sta instrValueType
   bcs admodeExprHole
   lda idVal+2
   ora idVal+3
   beq +
-  lda #errValueTooLarge
   jmp error
+  lda admodeMustBe
   bne ++
   ldx #2
   lda instrValue+1
   bne +
-  ldx #1
+  lda admodeChar
   rts
+  cmp #1  ;must be byte
   bne +
   lda instrValue+1
   bne --
   beq -
+  clc     ;must be relative
   lda address+0
   adc #2
   sta work+14
   lda address+1
   adc #0
   sta work+15
   sec
   lda instrValue+0
   sbc work+14
   sta instrValue+0
   tax
   lda instrValue+1
   sbc work+15
   sta instrValue+1
   cmp #0
   bne +
   cpx #128
   bcc ++
-  lda #errBranchTooFar
   jmp error
+  cmp #$ff
   bne -
   cpx #128
   bcc -
+  lda #0
   sta instrValue+1
   ldx #1
   lda admodeChar
   rts

   admodeExprHole = *
   ;** get presumed hole type
   lda #$02    ;holeType=word
   ldx admodeMustBe
   beq +
   lda #$01    ;holeType=byte
   cpx #1
   beq +
   lda #$40      ;holeType=branch
   ;** check for special cases of "stx zp,y" and "sty zp,x" which must be zp
+  cmp #$02
   bne +
   ldx instrNum
   cpx #49       ;instr.49==stx
   bcc +
   cpx #50+1     ;instr.50==sty
   bcs +
   ldx admodeChar
   cpx #","
   bne +
   lda #$01      ;is one of specials, so hole must be 8-bits
   ;** record the hole
+  pha
   tax
   lda #1
   jsr addMemoryHole
   ;** get the return values
   pla
   cmp #$40
   bne +
   lda #1
+  tax
   ldy #$80
   sty instrValueType
   ldy #$00
   sty instrValue+0
   sty instrValue+1
   lda admodeChar
   rts

;taken from Mighty Mon 4.0: instr# + addrMode -> opcode conversion + length

opcodeBase      = work+14
opcodeAdmodeNum = work+15

instrGetOpcode = *  ;( .A=instr#, .X=addrMode ) : instrOpcode, instrLen, .CS=inv
   tay
   lda opcodeBaseTab,y
   sta opcodeBase
   stx opcodeAdmodeNum
   lda opcodeAdmodeTab,y
   bmi ++
   cmp opcodeAdmodeNum
   bne +
   lda opcodeLenTab,x
   tax
   lda opcodeBase
   sta instrOpcode
   stx instrLen
   clc
   rts
+  nop
-  sec
   rts

+  and #$7f
   tay
   lda opcodeOrOffTab,y
   clc
   adc opcodeAdmodeNum
   tay
   dey
   lda opcodeOrMaskTab,y
   bmi -
   ora opcodeBase
   tay
   lda opcodeLenTab,x
   tax
   tya
   sta instrOpcode
   stx instrLen
   clc
   rts

opcodeBaseTab = *
   .byte $00,$61,$21,$02,$90,$b0,$f0,$24,$30,$d0,$10,$00,$50,$70,$18,$d8
   .byte $58,$b8,$c1,$e0,$c0,$c6,$ca,$88,$41,$e6,$e8,$c8,$4c,$20,$a1,$a2
   .byte $a0,$42,$ea,$01,$48,$08,$68,$28,$22,$62,$40,$60,$e1,$38,$f8,$78
   .byte $81,$86,$84,$aa,$a8,$ba,$8a,$9a,$98

opcodeAdmodeTab = *
   .byte $00,$81,$81,$82,$03,$03,$03,$83,$03,$03,$03,$01,$03,$03,$01,$01
   .byte $01,$01,$81,$84,$84,$85,$01,$01,$81,$85,$01,$01,$86,$07,$81,$87
   .byte $88,$82,$01,$81,$01,$01,$01,$01,$82,$82,$01,$01,$81,$01,$01,$01
   .byte $89,$8a,$8b,$01,$01,$01,$01,$01,$01

opcodeLenTab = *
   .byte $00,$01,$02,$02,$02,$02,$02,$03,$03,$03,$03,$02,$02

opcodeOrOffTab = *
   .byte 0,0,12,24,36,48,60,72,84,96,108,120

opcodeOrMaskTab = *
   ;      01  02  03  04  05  06  07  08  09  10  11  12 : addrMode
   ;      --  --  --  --  --  --  --  --  --  --  --  --     <if compressed>
   .byte $80,$08,$80,$04,$14,$80,$0c,$1c,$18,$80,$00,$10  ;[trim:1+0=1] [add=2]
   .byte $08,$80,$80,$04,$14,$80,$0c,$1c,$80,$80,$80,$80  ;[trim:0+4=4] [add=2]
   .byte $80,$80,$80,$04,$80,$80,$0c,$80,$80,$80,$80,$80  ;[trim:3+5=8] [add=2]
   .byte $80,$00,$80,$04,$80,$80,$0c,$80,$80,$80,$80,$80  ;[trim:1+5=6] [add=2]
   .byte $80,$80,$80,$04,$14,$80,$0c,$1c,$80,$80,$80,$80  ;[trim:3+4=7] [add=2]
   .byte $80,$80,$80,$80,$80,$80,$00,$80,$80,$20,$80,$80  ;[trim:6+2=8] [add=2]
   .byte $80,$00,$80,$04,$80,$14,$0c,$80,$1c,$80,$80,$80  ;[trim:1+3=4] [add=2]
   .byte $80,$00,$80,$04,$14,$80,$0c,$1c,$80,$80,$80,$80  ;[trim:1+4=5] [add=2]
   .byte $80,$80,$80,$04,$14,$80,$0c,$1c,$18,$80,$00,$10  ;[trim:3+0=3] [add=2]
   .byte $80,$80,$80,$04,$80,$14,$0c,$80,$80,$80,$80,$80  ;[trim:3+5=8] [add=2]
   .byte $80,$80,$80,$04,$14,$80,$0c,$80,$80,$80,$80,$80  ;[trim:3+5=8] [add=2]

;======== expression handling ========

parseExprType .buf 1

parseExpression = *  ;( .A=type(0=num,1=either) ) : .A=nextSpecialChar,.X=type
   sta parseExprType
   ldx #16
   stx expOffset
   lda #"+"
   sta expOp+0,x
   ldx #3
-  lda #0
   sta expHoleType,x
   lda #aceMemNull
   sta expHoleAddr,x
   lda sourceLine,x
   sta expSrcLine,x
   lda filePtr,x
   sta expSrcFile,x
   dex
   bpl -
   lda sourceCol
   sta expSrcCol
   bit debug
   bpl expGetOperand
   lda #<parseExprMsg
   ldy #>parseExprMsg
   jsr puts

   ;** expecting operand
   expGetOperand = *
   lda #0
   sta expPlusCount
   sta expMinusCount
   sta expLessCount
   sta expGreaterCount
   expGetOperandCont = *
   ldx expOffset
   lda #0
   sta expReserved,x
   jsr getToken
   cpx #tokenSpecial
   bne expNotSpecial
   lda tokenChar
   cmp #"+"
   bne +
   inc expPlusCount
   jmp expGetOperandCont
+  cmp #"-"
   bne +
   inc expMinusCount
   jmp expGetOperandCont
+  cmp #">"
   bne +
   inc expGreaterCount
   jmp expGetOperandCont
+  cmp #"<"
   bne +
   inc expLessCount
   jmp expGetOperandCont
+  cmp #"*"
   bne +
   jmp expOpnStar
+  jmp expOpnRelative

   expNotSpecial = *
   ldx expOffset
   lda expMinusCount
   and #$01
   lsr
   ror
   sta expSign,x
   jsr expFigureHiLo
   sta expHiLo,x
   lda #$00
   sta expType,x
   ldx tokenType
   cpx #tokenIdentifier
   bne +
   lda #0
   jmp expOpnIdentifier
+  cpx #tokenNumber
   bne +
   jmp expOpnNumber
+  cpx #tokenString
   bne +
   jmp expOpnString
+  jmp syntaxError
   parseExprMsg = *
   .asc "...must parse an expression..."
   .byte chrCR,0

;"Asteroids do not concern me, Admiral.  I want that ship, not excuses!"
;"Mudhole!?  Slimy!!?  My home, this is!"

expFigureHiLoWork .buf 1

expFigureHiLo = * ; ( ) : .A=HiLo_value ;.X:unchanged
   lda expGreaterCount
   cmp #16
   bcc +
   lda #15
+  asl
   asl
   asl
   asl
   sta expFigureHiLoWork
   lda expLessCount
   cmp #2
   bcc +
   lda #1
+  ora expFigureHiLoWork
   rts

expOpnRelExit .buf 1  ;whether to exit exp after relative ref.

expOpnRelative = *
   ldx #0
   cmp #":"
   beq +
   ldx #$ff
+  stx expOpnRelExit
   lda expPlusCount  ;there must be some pluses or minuses
   ora expMinusCount
   bne +
   jmp syntaxError
+  jsr expFigureHiLo
   ldx expOffset
   sta expHiLo,x
   lda #0
   sta expSign,x
   lda #"+"
   ldy expPlusCount
   beq +
   ldx expMinusCount
   beq ++
   jmp syntaxError
+  lda #"-"
   ldy expMinusCount
   bne +
   jmp syntaxError
+  dey
   jsr genRelLabel
   lda expOpnRelExit
   ;** fall through

expOpnIdentifier = *
   pha
   jsr findSymbol
   ldx expOffset
   lda idType
   cmp #$81
   bcc +
   cmp #$ff
   beq +
   lda #errNonNumIdExpr
   jmp error
+  sta expType,x
   ldy #0
   cmp #$80
   bcs +
-  lda idVal,y
   sta expValue,x
   inx
   iny
   cpy #4
   bcc -
   pla
   jmp expGetOperator
+  inc expUnresCnt
-  lda idPtr,y
   sta expValue,x
   inx
   iny
   cpy #4
   bcc -
   pla
   jmp expGetOperator

expOpnStar = *
   bit originSet
   bmi +
   lda #errOriginNotSet
   jmp error
+  ldx expOffset
   lda expMinusCount
   and #$01
   lsr
   ror
   sta expSign,x
   jsr expFigureHiLo
   sta expHiLo,x
   lda #$01
   sta expType,x
   lda address+0
   sta expValue+0,x
   lda address+1
   sta expValue+1,x
   lda #0
   sta expValue+2,x
   sta expValue+3,x
   lda #0
   jmp expGetOperator

expOpnNumber = *
   ldx expOffset
   lda #$00
   sta expType,x
   ldy #0
-  lda number,y
   sta expValue,x
   inx
   iny
   cpy #4
   bcc -
   lda #0
   jmp expGetOperator

expOpnString = *
   lda stringLen
   cmp #1
   bne +
   ;** interpret string as number
   ldx expOffset
   lda #$00
   sta expType,x
   lda stringBuf+0
   sta expValue+0,x
   lda #0
   sta expValue+1,x
   sta expValue+2,x
   sta expValue+3,x
   lda #0
   jmp expGetOperator

   ;** interpret string as actual string
+  lda expPlusCount
   ora expMinusCount
   ora expLessCount
   ora expGreaterCount
   beq +
-  lda #errInvalStrOpers
   jmp error
+  lda expOffset
   cmp #16
   bne -
   lda parseExprType
   bne +
-  jmp syntaxError
+  jsr getToken
   cpx #tokenSpecial
   bne -
+  ldx #$80
   rts

expGetOperator = *  ;(.A=exitFlag)
   pha
   clc
   lda expOffset
   adc #14
   sta expOffset
   bcc +
   lda #errTooManyOperands
   jmp error
+  pla
   cmp #0
   beq +
-  ldx expOffset
   txa
   sta expLength
   lda sourceCol
   sta expSrcCol
   lda tokenChar
   ldx #0
   rts
+  jsr getToken
   ldx tokenType
   cpx #tokenSpecial
   beq +
   jmp syntaxError
+  lda tokenChar
   cmp #"+"
   beq +
   cmp #"-"
   beq +
   cmp #"*"
   beq +
   cmp #"/"
   beq +
   cmp #"!"
   beq +
   cmp #"&"
   beq +
   cmp #"|"
   beq +
   cmp #"^"
   bne -
+  ldx expOffset
   sta expOp,x
   jmp expGetOperand

opnType .buf 1

evaluateExpression = *  ;( expTable ) : .CS=unresolved, idVal, idType
   ldx #3
   lda #0
-  sta idVal,x
   dex
   bpl -
   sta idType
   lda expUnresCnt
   beq +
   lda #$80
   sta idType
   sec
   rts
 + ldx #16
   stx expOffset

   evalNext = *
   ldx expOffset
   lda expType,x
   sta opnType
   ldy #$00
-  lda expValue,x
   sta number,y
   inx
   iny
   cpy #4
   bcc -
   ldx expOffset
   lda expSign,x
   bpl +
   jsr evalNegate
+  ldx expOffset
   cpx #16
   beq +
   lda expHiLo,x
   beq +
   jsr evalHiLo
+  ldx expOffset
   lda expOp,x
   cmp #"+"
   bne +
   jmp evalAdd
+  cmp #"-"
   bne +
   jmp evalSubtract
+  cmp #"*"
   bne +
   jmp evalMultiply
+  cmp #"/"
   bne +
   jmp evalDivide
+  cmp #"!"
   bne +
   jmp evalModulus
+  cmp #"&"
   bne +
   jmp evalAnd
+  cmp #"|"
   bne +
   jmp evalOr
+  cmp #"^"
   bne +
   jmp evalXor
+  lda #errInternError
   jmp error
   ;** go onto next operation
   evalCont = *
   clc
   lda expOffset
   adc #14
   sta expOffset
   cmp expLength
   bcs +
   jmp evalNext
   ;** check global hi-lo
+  lda expHiLo+16
   beq +
   ldx #3
-  lda idVal,x
   sta number,x
   dex
   bpl -
   lda idType
   sta opnType
   lda expHiLo+16
   jsr evalHiLo
   ldx #3
-  lda number,x
   sta idVal,x
   dex
   bpl -
   lda opnType
   sta idType
+  clc
   bit debug
   bmi +
   rts
+  lda #<evalMsg
   ldy #>evalMsg
   jsr puts
   ldx #idVal
   jsr putnum
   lda #","
   jsr putchar
   lda #" "
   jsr putchar
   ldx idType
   lda evalTypeNames,x
   jsr putchar
   lda #chrCR
   jsr putchar
   clc
   rts
   evalMsg = *
   .asc "evaluate: result="
   .byte 0
   evalTypeNames .asc "valhg"

evalNegate = *  ;( number, opnType ) : -number, opnType
   sec
   ldy #4
   ldx #0
-  lda #0
   sbc number,x
   sta number,x
   inx
   dey
   bne -
   rts

evalHiLoCnt .buf 1

evalHiLo = *  ;( number, opnType ) : number, opnType
   ;value $10 will extract high byte of addr, $01 will extract low byte
   pha
   lsr
   lsr
   lsr
   lsr
   tax
   beq ++
   ldy opnType
   lda hiOpnType1,y
   cpx #1
   beq +
   lda hiOpnType2,y
+  sta opnType
-  lda number+1
   sta number+0
   lda number+2
   sta number+1
   lda number+3
   sta number+2
   lda #$00
   sta number+3
   dex
   bne -
+  pla
   and #$0f
   beq +
   ldx #0
   stx number+3
   stx number+2
   stx number+1
   ldx opnType
   lda loOpnType,x
   sta opnType
+  rts
   hiOpnType1 .byte 0,3,0,0,4
   hiOpnType2 .byte 0,0,0,0,4
   loOpnType  .byte 0,2,2,3,4

evalSubtract = *  ;(idVal,idType, number,opnType) : idVal, idType
   jsr evalNegate
evalAdd = *  ;(idVal,idType, number,opnType) : idVal, idType
   clc
   ldy #4
   ldx #0
-  lda idVal,x
   adc number,x
   sta idVal,x
   inx
   dey
   bne -
   lda opnType  ;if either type is garbage, then the result is garbage
   ora idType
   cmp #4
   bcc +
-  lda #4
   sta idType
   jmp evalCont
+  lda idType   ;if both operands are the same type, we're okay
   cmp opnType
   beq +
   lda idType   ;if operands are different and one isn't "value", then garbage
   and opnType
   bne -
+  lda idType
   eor opnType
   sta idType
   jmp evalCont

evalMultiply = *  ;(idVal,idType, number,opnType) : idVal, idType
   ldx #3
-  lda idVal,x
   sta syswork+0,x
   lda number,x
   sta syswork+4,x
   dex
   bpl -
   jsr multiply32
   ldx #3
-  lda syswork+8,x
   sta idVal,x
   dex
   bpl -
   jmp evalLogicType

evalDivide = *  ;(idVal,idType, number,opnType) : idVal, idType
   ldx #3
-  lda idVal,x
   sta syswork+0,x
   lda number,x
   sta syswork+4,x
   dex
   bpl -
   jsr divide32
   ldx #3
-  lda syswork+8,x
   sta idVal,x
   dex
   bpl -
   jmp evalLogicType

evalModulus = *  ;(idVal,idType, number,opnType) : idVal, idType
   ldx #3
-  lda idVal,x
   sta syswork+0,x
   lda number,x
   sta syswork+4,x
   dex
   bpl -
   jsr divide32
   ldx #3
-  lda syswork+12,x
   sta idVal,x
   dex
   bpl -
   jmp evalLogicType

evalAnd = *  ;(idVal,idType, number,opnType) : idVal, idType
   ldx #3
-  lda idVal,x
   and number,x
   sta idVal,x
   dex
   bpl -
   evalLogicType = *
   lda idType
   ora opnType
   beq +
   lda #4
   sta idType
+  jmp evalCont

evalOr = *  ;(idVal,idType, number,opnType) : idVal, idType
   ldx #3
-  lda idVal,x
   ora number,x
   sta idVal,x
   dex
   bpl -
   jmp evalLogicType

evalXor = *  ;(idVal,idType, number,opnType) : idVal, idType
   ldx #3
-  lda idVal,x
   eor number,x
   sta idVal,x
   dex
   bpl -
   jmp evalLogicType

;======== symbol table management ========

findSymbol = *  ;( stringBuf, stringLen ) : .A=code,idPtr,idVal
   ;** ret: code .A=(symFound,symUnresolved,symNew)
   jsr hash
   ldy #3
-  lda (hashPtr),y
   sta mp,y
   dey
   bpl -
   lda hashVal+0
   cmp symHash+0
   bne findSymNext
   lda hashVal+1
   cmp symHash+1
   bne findSymNext
   ora hashVal+0
   beq findSymNext
   ldx #0
-  lda stringBuf,x
   beq +
   cmp symName,x
   bne findSymNext
   inx
   bne -
+  ldx #3
-  lda symPtr,x
   sta mp,x
   dex
   bpl -
   jmp findSymMatches
   ;%%%
   findSymNext = *
   lda mp+3
   cmp #aceMemNull
   bne +
   jmp findSymCreate
+  lda #<symBuf
   ldy #>symBuf
   sta zp+0
   sty zp+1
   lda #12+8+1
   ldy #0
   jsr aceMemFetch
   lda symNameLen
   cmp stringLen
   bne findSymCont
   cmp #8+1
   bcc +
   clc
   adc #12+1
   ldy #0
   jsr aceMemFetch
+  ldx #0
-  lda stringBuf,x
   beq +
   cmp symName,x
   bne findSymCont
   inx
   bne -
   findSymMatches = *
+  ldx #3
-  lda mp,x
   sta idPtr,x
   sta symPtr,x
   lda symValue,x
   sta idVal,x
   dex
   bpl -
   lda hashVal+0
   ldy hashVal+1
   sta symHash+0
   sty symHash+1
   lda symType
   sta idType
   lda #symFound
   bit symType
   bpl +
   lda #symUnresolved
+  rts

   findSymCont = *
   ldx #3
-  lda symNext,x
   sta mp,x
   dex
   bpl -
   jmp findSymNext

findSymCreate = *
   lda #$80
   sta symType
   sta idType
   lda #$00
   sta symClass
   sta symUnresOpnd
   lda stringLen
   sta symNameLen
   ldx #0
-  lda stringBuf,x
   sta symName,x
   beq +
   inx
   bne -
+  clc
   lda #12+1
   adc symNameLen
   ldy #0
   jsr malloc
   bcc +
   lda #errInsufficientMemory
   jmp error
+  ldy #3
-  lda #aceMemNull
   sta symValue,y
   lda #0
   sta idVal,y
   lda (hashPtr),y
   sta symNext,y
   lda mp,y
   sta idPtr,y
   sta symPtr,y
   sta (hashPtr),y
   dey
   bpl -
   lda hashVal+0
   ldy hashVal+1
   sta symHash+0
   sty symHash+1
   jsr stashSymbol
   lda #symNew
   rts

fetchSymbol = *  ;( [mp] ) : symBuf
   lda #<symBuf
   ldy #>symBuf
   sta zp+0
   sty zp+1
   lda #12+8+1
   ldy #0
   jsr aceMemFetch
   lda symNameLen
   cmp #8+1
   bcc +
   clc
   adc #12+1
   ldy #0
   jsr aceMemFetch
+  rts

fetchSymbolHeader = *  ;( [mp] ) : symBuf (header)
   lda #<symBuf
   ldy #>symBuf
   sta zp+0
   sty zp+1
   lda #12
   ldy #0
   jsr aceMemFetch
   rts

stashSymbol = *  ;( symBuf, [mp] )
   lda #<symBuf
   ldy #>symBuf
   sta zp+0
   sty zp+1
   clc
   lda #12+1
   adc symNameLen
   ldy #0
   jsr aceMemStash
   rts

stashSymbolHeader = *  ;( symBuf(header), [mp] )
   lda #<symBuf
   ldy #>symBuf
   sta zp+0
   sty zp+1
   lda #12
   ldy #0
   jsr aceMemStash
   rts

initSymbolTable = *
   lda #<identHashTable
   ldy #>identHashTable
   ldx #hashTablePages
   ;** fall through

fillNull = *
   sta work+0
   sty work+1
   ldy #0
   lda #aceMemNull
-  sta (work),y
   iny
   bne -
   inc work+1
   dex
   bne -
   rts

findUnresSymbol = *
   ;** fetches symbol and reference
   lda #$80
   sta dumpSymOpt
   jmp dumpSymbolTableIn
   foundUnresSymbol = *
   ldx #3
-  lda symValue,x
   sta expPtr,x
   dex
   bpl -
   jsr fetchExp
   rts

dumpSymEntries .buf 2
dumpSymOpt .buf 1
dumpCount = idVal

dumpSymbolTable = *
   lda #$00
   sta dumpSymOpt
   ldx #3
-  sta dumpCount,x
   dex
   bpl -
   bit symDump
   bmi +
   rts
+  lda #<dumpSymMsg1
   ldy #>dumpSymMsg1
   jsr puts

   dumpSymbolTableIn = *
   lda #<identHashTable
   ldy #>identHashTable
   sta hashPtr+0
   sty hashPtr+1
   lda #0
   ldy #hashTableEntriesHigh
   sta dumpSymEntries+0
   sty dumpSymEntries+1
-  jsr dumpSymBucket
   clc
   lda hashPtr+0
   adc #4
   sta hashPtr+0
   bcc +
   inc hashPtr+1
+  inc dumpSymEntries+0
   bne -
   dec dumpSymEntries+1
   bne -
   lda #<dumpSymMsg2
   ldy #>dumpSymMsg2
   jsr puts
   ldx #dumpCount
   jsr putnum
   lda #chrCR
   jsr putchar
   rts

   dumpSymBucket = *
   ldy #3
-  lda (hashPtr),y
   sta mp,y
   dey
   bpl -
-  lda mp+3
   cmp #aceMemNull
   bne +
   rts
+  jsr fetchSymbol
   jsr dumpSymbol
   ldx #3
-  lda symNext,x
   sta mp,x
   dex
   bpl -
   jmp --

   dumpSymbol = *
   bit dumpSymOpt
   bpl +++
   lda symType
   cmp #$80
   beq +
   cmp #$ff
   bne ++
+  pla ;** pop call from dumpSymBucket
   pla
   pla ;** pop call from dumpSymbolTable
   pla
   jmp foundUnresSymbol
+  rts
+  ldx #dumpCount
   jsr incLong
   lda #0
   sta number+2
   sta number+3
   sec
   lda hashPtr+0
   sbc #<identHashTable
   sta number+0
   lda hashPtr+1
   sbc #>identHashTable
   sta number+1
   lsr number+1
   ror number+0
   lsr number+1
   ror number+0
   ldx #number
   ldy #4
   jsr wputnum
   lda #2
   jsr space
   ldx #3
-  lda symValue,x
   sta number,x
   dex
   bpl -
   lda number+3
   jsr puthex
   lda number+2
   jsr puthex
   lda number+1
   jsr puthex
   lda number+0
   jsr puthex
   lda #1
   jsr space
   ldx #number
   ldy #10
   jsr wputnum
   lda #2
   jsr space
   lda #"?"
   bit symType
   bmi +
   ldx symType
   lda evalTypeNames,x
+  jsr putchar
   lda #2
   jsr space
   lda #<symName
   ldy #>symName
   jsr puts
   lda #chrCR
   jsr putchar
   jsr checkStop
   rts

   spaceCount .buf 1

   space = *
   sta spaceCount
-  lda #" "
   jsr putchar
   dec spaceCount
   bne -
   rts

   dumpSymMsg1 = *
   .asc "HASH  HEXVALUE    DECIMAL  T  NAME"
   .byte chrCR
   .asc "----  -------- ----------  -  -----"
   .byte chrCR,0
   dumpSymMsg2 = *
   .asc "--"
   .byte chrCR
   .asc "Number of symbols: "
   .byte 0
;1234567890123456789012345678901234567890

hashWork .buf 2

hash = *
   lda #$aa
   sta hashVal+0
   sta hashVal+1
   ldx #0
-  lda stringBuf,x
   bne +
   lda hashVal+0
   sta hashPtr+0
   lda hashVal+1
   and #hashTableMask
   asl hashPtr+0
   rol
   asl hashPtr+0
   rol
   sta hashPtr+1
   clc
   lda hashPtr+0
   adc #<identHashTable
   sta hashPtr+0
   lda hashPtr+1
   adc #>identHashTable
   sta hashPtr+1
   rts
   ;** hashVal := hashVal * 37;
+  lda hashVal+0
   ldy hashVal+1
   sta hashWork+0
   sty hashWork+1
   asl              ;times 2
   rol hashVal+1
   asl              ;times 4
   rol hashVal+1
   asl              ;times 8
   rol hashVal+1
   jsr hashAddWork  ;times 9
   asl              ;times 18
   rol hashVal+1
   asl              ;times 36
   rol hashVal+1
   jsr hashAddWork  ;times 37
   adc stringBuf,x
   sta hashVal+0
   bcc +
   inc hashVal+1
+  inx
   bne -

hashAddWork = *
   clc
   adc hashWork+0
   sta hashVal+0
   lda hashVal+1
   adc hashWork+1
   sta hashVal+1
   lda hashVal+0
   rts

;======== symbols, expressions, and holes: symbol definition support ========

parseDefineVar = *
   jsr findSymbol
   cmp #symNew
   beq +
   lda symType
   cmp #$80
   beq +
   lda #errRedefinedSymbol
   jmp error
+  lda #$ff
   sta symType
   jsr stashSymbol
   ldx #3
-  lda idPtr,x
   sta varPtr,x
   dex
   bpl -
   rts

addMemoryHole = *  ;( .X=holeType, address+.A )
   ;** put in hole information
   stx expHoleType
   clc
   adc address+0
   sta expHoleAddr+0
   lda address+1
   adc #0
   sta expHoleAddr+1
   lda #0
   sta expHoleAddr+2
   sta expHoleAddr+3
   jmp addHoleIn

addVariableHole = *  ;( varPtr, expTable ) : ...
   ;** put in hole information
   lda #$80
   sta expHoleType
   ldx #3
-  lda varPtr,x
   sta expHoleAddr,x
   dex
   bpl -

   addHoleIn = *
   ;** keep a count of the global number of unresolved references
   ldx #holeCount
   jsr incLong
   ;** keep a separate count of the unresolved relative labels:they must be resd
   lda stringLen
   cmp #2
   bcc +
   lda stringBuf+1
   cmp #"+"
   beq +
   cmp #"-"
   bne ++
+  ldx #relHoleCount
   jsr incLong
   ;** allocate storage for expression descriptor
+  lda expLength
   ldy #0
   jsr malloc
   bcc +
   lda #errInsufficientMemory
   jmp error
+  ldx #3
-  lda mp,x
   sta expPtr,x
   dex
   bpl -
   ;** scan for pointers to unresolved labels, refs into corresp.reference lists
   ldx #16
   stx expOffset

   addHoleNext = *
   ldx expOffset
   lda expType,x
   bpl addHoleCont
   ldy #0
-  lda expValue,x
   sta mp,y
   inx
   iny
   cpy #4
   bcc -
   jsr fetchSymbolHeader
   ldx expOffset
   ldy #0
-  lda symValue,y
   sta expNextUnres,x
   lda expPtr,y
   sta symValue,y
   inx
   iny
   cpy #4
   bcc -
   ldx expOffset
   lda symUnresOpnd
   sta expNextOpnd,x
   stx symUnresOpnd
   jsr stashSymbolHeader

   addHoleCont = *
   clc
   lda expOffset
   adc #14
   sta expOffset
   cmp expLength
   bcc addHoleNext

   ;** store expression descriptor
   jmp stashExp

plugStackHead .buf 4
plugRecBuf    = *
plugRecNext   .buf 4
plugRecVarPtr .buf 4
plugRecIdVal  .buf 4
plugRecIdType .buf 1

plugStackInit = *
   lda #aceMemNull
   sta plugStackHead+3
   rts

plugStackPush = *  ;( varPtr, idVal, idType )
   lda #13
   ldy #0
   jsr malloc
   bcc +
   lda #errInsufficientMemory
   jsr error
+  ldx #3
-  lda plugStackHead,x
   sta plugRecNext,x
   lda mp,x
   sta plugStackHead,x
   lda varPtr,x
   sta plugRecVarPtr,x
   lda idVal,x
   sta plugRecIdVal,x
   dex
   bpl -
   lda idType
   sta plugRecIdType
   lda #<plugRecBuf
   ldy #>plugRecBuf
   sta zp+0
   sty zp+1
   lda #13
   ldy #0
   jsr aceMemStash
   rts

plugStackPop = *  ;( ) : varPtr, idVal, idType, .CS=empty
   ldx #3
-  lda plugStackHead,x
   sta mp,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   bne +
   sec
   rts
+  lda #<plugRecBuf
   ldy #>plugRecBuf
   sta zp+0
   sty zp+1
   lda #13
   ldy #0
   jsr aceMemFetch
   lda #13
   ldy #0
   jsr free
   ldx #3
-  lda plugRecNext,x
   sta plugStackHead,x
   lda plugRecVarPtr,x
   sta varPtr,x
   lda plugRecIdVal,x
   sta idVal,x
   dex
   bpl -
   lda plugRecIdType
   sta idType
   rts

assignVarSave .buf 10

assignVariable = *  ;( varPtr, idVal, idType ) : ... ;changes idPtr,varPtr,etc.
   jsr plugStackInit
   ;** assign value to variable
   assignVarBody = *
   ldx #3
-  lda varPtr,x
   sta mp,x
   dex
   bpl -
   jsr fetchSymbolHeader
   lda symType
   cmp #$ff
   beq +
   lda #errInternError
   jmp error
+  ldx #3
-  lda symValue,x
   sta expPtr,x
   lda idVal,x
   sta symValue,x
   dex
   bpl -
   lda symUnresOpnd
   sta expOffset
   lda idType
   sta symType
   jsr stashSymbolHeader

   ;** cascade changes if necessary
   ;Run through reference list, filling in value of id, unlinking from this id.
   ;If we resolve an expression, evaluate it, and if the hole is in memory, fill
   ;it.  If the filled hole is another variable, push the <varPtr,idVal,idType>
   ;values onto the plug stack and continue with reference list for the current
   ;variable.  When we are done with the current label, pop a variable off the
   ;plug stack and continue with it.  Otherwise, exit.
   assignVarCascade = *
   lda expPtr+3
   cmp #aceMemNull
   bne +
   jsr plugStackPop
   bcc assignVarBody
   rts
   ;** fetch unresolved expression
+  jsr fetchExp
   ;** fill in value
   ldx expOffset
   lda idType
   sta expType,x
   ldy #0
-  lda idVal,y
   sta expValue,x
   inx
   iny
   cpy #4
   bcc -
   dec expUnresCnt
   bne assignVarUnresolved
   jmp assignVarResolved

   ;** still unresolved--stash modified expression
   assignVarUnresolved = *
   jsr stashExp
   jmp assignVarCascadeCont

   ;** resolved--evaluate expression, fill in hole
   assignVarResolved = *
   ldx #3
-  lda expPtr,x
   sta mp,x
   dex
   bpl -
   lda expLength
   ldy #0
   jsr free
   ;** save varPtr,idVal,idType,expOffset
   ldx #3
-  lda varPtr,x
   sta assignVarSave+0,x
   lda expHoleAddr,x
   sta varPtr,x
   lda idVal,x
   sta assignVarSave+4,x
   dex
   bpl -
   lda idType
   sta assignVarSave+8
   lda expOffset
   sta assignVarSave+9
   ldx #holeCount
   jsr decLong
   lda symClass
   cmp #$01
   beq +
   ldx #relHoleCount
   jsr decLong
   ;** evaluate new expression
+  jsr evaluateExpression
   ;** fill hole--id hole: push hole plug--do something else for memory hole
   lda expHoleType
   cmp #$80
   beq +
   jsr fillMemoryHole
   jmp ++
+  jsr plugStackPush
   ;** restore varPtr,idVal,idType,expOffset
+  ldx #3
-  lda assignVarSave+0,x
   sta varPtr,x
   lda assignVarSave+4,x
   sta idVal,x
   dex
   bpl -
   lda assignVarSave+8
   sta idType
   lda assignVarSave+9
   sta expOffset

   ;** go onto next unresolved expression
   assignVarCascadeCont = *
   ldx expOffset
   lda expNextOpnd,x
   sta expOffset
   ldy #0
-  lda expNextUnres,x
   sta expPtr,y
   inx
   iny
   cpy #4
   bcc -
   jmp assignVarCascade

fetchExp = *  ;( [expPtr] ) : expTable, mp=expPtr
   ldx #3
-  lda expPtr,x
   sta mp,x
   dex
   bpl -
   lda #<expTable
   ldy #>expTable
   sta zp+0
   sty zp+1
   lda #16+14
   ldy #0
   jsr aceMemFetch
   lda expLength
   cmp #16+14+1
   bcc +
   lda expLength
   ldy #0
   jsr aceMemFetch
+  rts

stashExp = *  ;( [expPtr], expTable ) : mp=expPtr
   ldx #3
-  lda expPtr,x
   sta mp,x
   dex
   bpl -
   lda #<expTable
   ldy #>expTable
   sta zp+0
   sty zp+1
   lda expLength
   ldy #0
   jsr aceMemStash
   rts

fillMhType .buf 1

fillMemoryHole = *  ;( .A=holeType, varPtr, idVal, idType )
   sta fillMhType
   bit debug
   bpl fillMhCont
   lda #<fillMhMsg1
   ldy #>fillMhMsg1
   jsr puts
   lda varPtr+1
   jsr puthex
   lda varPtr+0
   jsr puthex
   lda #<fillMhMsg2
   ldy #>fillMhMsg2
   jsr puts
   ldx #idVal
   jsr putnum
   lda #" "
   jsr putchar
   lda #"("
   jsr putchar
   lda #"v"
   ldx idType
   beq +
   lda #"a"
+  jsr putchar
   lda #<fillMhMsg3
   ldy #>fillMhMsg3
   jsr puts
   lda fillMhType
   jsr puthex
   lda #chrCR
   jsr putchar
   ;%%%

   fillMhCont = *
   lda fillMhType
   cmp #$40
   beq fillMhBranch
   tax
   dex
-  lda idVal,x
   sta memPutVals,x
   dex
   bpl -
   ldx fillMhType
   dex
-  inx
   cpx #4
   bcs +
   lda idVal,x
   beq -
   lda #errValueTooLarge
   jmp errorRef
+  lda fillMhType
   ldx varPtr+0
   ldy varPtr+1
   jsr memPut
   ;** add relocatable reference
   lda idType
   beq +
   cmp #$04
   bcs +
   ldx varPtr+0
   ldy varPtr+1
   jsr recordRelocRef
+  rts

   fillMhBranch = *
   lda idVal+2
   ora idVal+3
   beq +
   lda #errValueTooLarge
   jmp errorRef
+  clc
   lda varPtr+0
   adc #1
   sta memPutVals+0
   lda varPtr+1
   adc #0
   sta memPutVals+1
   sec
   lda idVal+0
   sbc memPutVals+0
   sta memPutVals+0
   tax
   lda idVal+1
   sbc memPutVals+1
   sta memPutVals+1
   cmp #0
   bne +
   cpx #128
   bcc ++
-  lda #errBranchTooFar
   jmp error
+  cmp #$ff
   bne -
   cpx #128
   bcc -
+  lda #1
   ldx varPtr+0
   ldy varPtr+1
   jsr memPut
   rts

fillMhMsg1 = *
   .asc "fill memory hole: address=$"
   .byte 0
fillMhMsg2 = *
   .asc ", value="
   .byte 0
fillMhMsg3 = *
   .asc "), holeType=$"
   .byte 0

;======== program memory storage ========

memInit = *  ;( )
   lda #aceMemNull
   sta memBufPtr+3
   ldx #0
   lda #aceMemNull
-  sta memPtrTable,x
   inx
   bne -
   rts

memPutSave .buf 2

memPut = *  ;( .XY=addr,.A=byteCount, memPutVals )
   sta memPutCount
   sty memPutPage
   ldy #0

   memPutNext = *
   lda memBufPtr+3
   cmp #aceMemNull
   beq memPutMiss
   lda memPutPage
   cmp memBufPage
   bne memPutMiss
   lda memPutVals,y
   sta memBuf,x
   inx
   bne +
   inc memPutPage
+  iny
   cpy memPutCount
   bcc memPutNext
   rts

   memPutMiss = *
   stx memPutSave+0
   sty memPutSave+1
   jsr memFlushBuf
   jsr memFetchBuf
   ldx memPutSave+0
   ldy memPutSave+1
   jmp memPutNext

memFlushBuf = *
   lda #<memBuf
   ldy #>memBuf
   sta zp+0
   sty zp+1
   ;** flush old page
   ldx #3
-  lda memBufPtr,x
   sta mp,x
   dex
   bpl -
   lda mp+3
   cmp #aceMemNull
   beq +
   lda memBufPage
   and #$03
   clc
   adc mp+1
   sta mp+1
   lda #0
   ldy #1
   jsr aceMemStash
+  rts

memFetchBuf = *  ;( memPutPage )
   lda #<memBuf
   ldy #>memBuf
   sta zp+0
   sty zp+1
   lda memPutPage
   sta memBufPage
   and #%11111100
   tay
   ldx #0
-  lda memPtrTable,y
   sta memBufPtr,x
   sta mp,x
   iny
   inx
   cpx #4
   bcc -
   lda mp+3
   cmp #aceMemNull
   beq memFetchBufNew
   lda memPutPage
   and #$03
   clc
   adc mp+1
   sta mp+1
   lda #0
   ldy #1
   jsr aceMemFetch
   rts
   
   memFetchBufNew = *
   lda #0
   ldy #4
   jsr malloc
   lda memBufPage
   and #%11111100
   tay
   ldx #0
-  lda mp,x
   sta memPtrTable,y
   sta memBufPtr,x
   iny
   inx
   cpx #4
   bcc -
   rts

memSaveAddr .buf 2
memSaveFd   .buf 1
memSaveLen  .buf 1  ;0==256

memSave = *  ;( .A=fd, .XY=from, address=to)
   sta memSaveFd
   stx memSaveAddr+0
   sty memSaveAddr+1
   jsr memFlushBuf

   memSaveNext = *
   lda memSaveAddr+0
   cmp address+0
   lda memSaveAddr+1
   sbc address+1
   bcc +
   rts
+  lda memSaveAddr+1
   sta memPutPage
   jsr memFetchBuf   ;sets (zp)
   lda #0
   sta memSaveLen
   ;** set bottom
   lda memSaveAddr+0
   beq ++
   clc
   lda zp+0
   adc memSaveAddr+0
   sta zp+0
   bcc +
   inc zp+1
+  sec
   lda memSaveLen
   sbc memSaveAddr+0
   sta memSaveLen
   ;** set top
+  lda memSaveAddr+1
   cmp address+1
   bcc +
   sec
   lda #0
   sbc address+0
   tax
   lda memSaveLen
   stx memSaveLen
   sec
   sbc memSaveLen
   sta memSaveLen
   ;** save the page
+  ldy #0
   lda memSaveLen
   bne +
   iny
+  ldx memSaveFd
   jsr write
   lda memSaveLen
   beq +
   clc
   adc memSaveAddr+0
   sta memSaveAddr+0
   bcc ++
+  inc memSaveAddr+1
+  jmp memSaveNext

recordRelocRef = *  ;( .XY=refAddress, .A=valueType )
   rts

;======== tokenizer ========

;* fill entire buffer, align with top; .CS=eof
fillbuf = *
   lda #<sourceBuf
   ldy #>sourceBuf
   sta zp+0
   sty zp+1
   lda #0
   ldy #1
   sta bufptr
   ldx sourceFcb
   jsr read
   bne +
   sec
   rts
+  cpy #1
   bcc +
   clc
   rts
+  sta bufptr
   tax
   ldy #0
-  dex
   dey
   lda sourceBuf,x
   sta sourceBuf,y
   cpx #0
   bne -
   sec
   lda #0
   sbc bufptr
   sta bufptr
   clc
   rts

getNextChar = *
   inc bufptr
   beq +
-  ldy bufptr
   lda sourceBuf,y
   rts
+  jsr fillbuf
   bcc -
   lda #chrEOF
   rts

eatWhitespace = *  ;() : .A=NextChar
   lda prevChar
   cmp #" "
   beq +
   cmp #chrTAB
   beq +
   rts
+  ldy bufptr
-  iny
   beq +
   eatWhCont = *
   lda sourceBuf,y
   cmp #" "
   beq -
   cmp #chrTAB
   beq -
   sty bufptr
   rts
+  jsr fillbuf
   bcs +
   ldy bufptr
   jmp eatWhCont
+  lda #chrEOF
   rts

;*** token dispatch ***
;ret: .X=tokenIdentifier, .A=nextChar, .Y=strlen, stringLen, stringBuf
;     .X=tokenNumber,     .Y=numlen, number
;     .X=tokenString,     .A=firstChar,.Y=strlen, stringLen, stringBuf
;     .X=tokenSpecial,    .A=char

tokDebugSave .buf 3

getToken = *
   lda newCol
   sta sourceCol
   lda newLine+0
   sta sourceLine+0
   lda newLine+1
   sta sourceLine+1
   lda newLine+2
   sta sourceLine+2
   lda newLine+3
   sta sourceLine+3
   bit debug
   bpl getTokenReal
   jsr getTokenReal
   sta tokDebugSave+0
   stx tokDebugSave+1
   sty tokDebugSave+2
   jsr dispToken
   lda tokDebugSave+0
   ldx tokDebugSave+1
   ldy tokDebugSave+2
   rts

getTokenReal = *
   lda prevChar
   cmp #" "
   bne +
-  jsr eatWhitespace
+  cmp #chrTAB
   beq -
   cmp #"@"
   bcc +
-  jmp getIdentifier
+  cmp #"."
   beq -
   cmp #"'"
   bcc cmpMore1
   bne +
   jmp getString
+  cmp #"0"
   bcc tokSpecial
   cmp #":"
   bcs +

   tokNum = *
   jmp getNumber

+  cmp #";"
   bne +
   jmp eatComment

   tokSpecial = *
   jmp getSpecialToken

   cmpMore1 = *
   cmp #"$"
   bcc +
   beq tokNum
   cmp #"%"
   beq tokNum
   jmp getSpecialToken

+  cmp #chrQuote
   bne tokSpecial
   jmp getString

;*** comment ***

eatComment = *
   ldy bufptr
-  iny
   beq +

   commentChar = *
   lda sourceBuf,y
   cmp #chrCR
   bne -
   sty bufptr
   jmp getSpecialToken
+  jsr fillbuf
   bcs +
   ldy bufptr
   jmp commentChar
+  lda #chrEOF
   jmp getSpecialToken

;*** special ***

getSpecialToken = *
   cmp #chrCR
   bne +
   inc newLine+0
   bne +
   inc newLine+1
   bne +
   inc newLine+2
   bne +
   inc newLine+3
+  cmp #chrEOF
   bne +
   sta prevChar
   sta tokenChar
   ldx #tokenEOF
   stx tokenType
   rts
+  pha
   jsr getNextChar
   sta prevChar
   sta tokenNextChar
   pla
   ldx #tokenSpecial
   stx tokenType
   sta tokenChar
   rts

;*** identifier ***

getIdentifier = *
   cmp #"["
   bcc +
   cmp #"^"+1
   bcs +
   jmp getSpecialToken
+  cmp #"{"
   bcc +
   cmp #"~"+1
   bcs +
   jmp getSpecialToken
+  sta stringBuf
   ldy #1
   sty stringLen
-  jsr getNextChar
   cmp #"@"
   bcc identExit

   identGoodChar = *
   ldy stringLen
   sta stringBuf,y
   inc stringLen
   cpy #240
   bcc -
   sta prevChar
   lda #errIdentTooLong
   jmp error

   identExit = *
   cmp #"."
   beq identGoodChar
   cmp #"_"
   beq identGoodChar
   cmp #"0"
   bcc +
   cmp #":"
   bcc identGoodChar
+  cmp #" "
   bne +
-  sta prevChar
   jsr eatWhitespace
+  cmp #chrTAB
   beq -
   sta prevChar
   lda #0
   ldy stringLen
   sta stringBuf,y
   lda prevChar
   ldy stringLen
   ldx #tokenIdentifier
   stx tokenType
   cmp #";"
   bne +
   lda #chrCR
+  sta tokenNextChar
   rts

;*** string ***

getString = *
   sta strDelimit
   lda #0
   sta stringLen
-  jsr getNextChar
   sta prevChar
   cmp #chrEOF
   beq strEof
   cmp strDelimit
   beq strExit
   cmp #chrCR
   beq strEof
   bit ignoreBackslash
   bmi getStrPut
   cmp #"\"
   beq strEsc
   getStrPut = *
   ldy stringLen
   sta stringBuf,y
   inc stringLen
   lda stringLen
   cmp #241
   bcc -
   sta prevChar
   lda #errStringTooLong
   jmp error

strEsc = *
   jsr getNextChar
   cmp #chrCR
   beq strEof
   cmp #chrEOF
   beq strEof
   ldx #strEscCharEnd-strEscChar-1
-  cmp strEscChar,x
   beq +
   dex
   bpl -
   jmp getStrPut
+  lda strEscTrans,x
   jmp getStrPut

strEscChar = *
   .asc "\nbtraz'e0qfv"
   .byte chrQuote
   strEscCharEnd = *
strEscTrans = *
   .byte 92,13,20,9,10,7,0,39,27,0,34,147,17,34

strEof = *
   lda #errNoCloseQuote
   jmp error

strExit = *
   jsr getNextChar
   sta prevChar
   lda #0  ;but may contain \0
   ldy stringLen
   sta stringBuf,y
   lda stringBuf+0
   ldx #tokenString
   stx tokenType
   sta tokenChar
   rts

getNumber = *
   pha
   ldx #3
   lda #0
-  sta number,x
   dex
   bpl -
   pla
   ldx #16
   cmp #"$"
   beq +
   ldx #2
   cmp #"%"
   beq +
   ldx #10
   stx numBase
   bne gotNextDigit
+  stx numBase
   jsr getNextChar
   sta prevChar
   jsr checkDigit
   bcc +
   lda #errBadNumber
   jmp error
+  txa
   jmp gotNextDigit

nextDigit = *
   jsr getNextChar
   sta prevChar
   cmp #"_"
   beq nextDigit
gotNextDigit = *
   jsr checkDigit
   bcs getNumExit
   pha
   jsr shiftNumber
   bcs overflowExitPla
   pla
   clc
   adc number
   sta number
   bcc +
   inc number+1
   bne +
   inc number+2
   bne +
   inc number+3
   beq overflowExit
+  jmp nextDigit

overflowExitPla = *
   pla
overflowExit = *
   lda #errNumOverflow
   jmp error

getNumExit = *
   ldy #3
-  lda number,y
   beq +
   dey
   bpl -
   iny
+  iny
   sty tokenNumBytes
   ldx #tokenNumber
   stx tokenType
   rts

checkDigit = *  ;( .A=asciiDigit ) : .A=binDigit, .X=asciiDigit, .CC=ok
   tax
   cmp #"0"
   bcc checkBad
   cmp #"9"+1
   bcc checkAnd
   cmp #"a"
   bcc checkBad
   cmp #"f"+1
   bcc +
   cmp #"A"
   bcc checkBad
   cmp #"F"+1
   bcs checkBad
+  sec
   sbc #7
   checkAnd = *
   and #$0f
   cmp numBase
   rts
   checkBad = *
   sec
   rts

shiftNumber = *
   lda numBase
   cmp #10
   bne +
   ldx #3
-  lda number,x
   sta numSave,x
   dex
   bpl -
   ldx #2
   jsr rollNumber
   jsr addNumber
   ldx #1
   jsr rollNumber
   rts
+  ldx #1
   cmp #16
   bne +
   ldx #4
+  jsr rollNumber
   rts

   rollNumber = *  ;( .X=times )
   asl number
   rol number+1
   rol number+2
   rol number+3
   bcs +
   dex
   bne rollNumber
   rts
+  pla
   pla
   sec
   rts

   addNumber = *
   ldx #0
   clc
-  lda number,x
   adc numSave,x
   sta number,x
   inx
   txa
   and #$03
   bne -
   bcs +
   rts
+  pla
   pla
   sec
   rts

;======== debugging token display routines ========

dispToken = *  ;( .tokenType )
   ldx tokenType
   cpx #tokenIdentifier
   beq dispIdentifier
   cpx #tokenString
   beq dispString
   cpx #tokenSpecial
   bne +
   jmp dispSpecial
+  cpx #tokenNumber
   bne +
   jmp dispNumber
+  rts

dispIdentifier = *
   lda #"i"
   jsr putchar
   lda #":"
   jsr putchar
   jsr showStr
   lda #","
   jsr putchar
   jsr hash
   lda #0
   sta number+2
   sta number+3
   lda hashVal+0
   ldy hashVal+1
   sta number+0
   sty number+1
   ldx #number
   jsr putnum
   lda #","
   jsr putchar
   sec
   lda hashPtr+0
   sbc #<identHashTable
   sta number+0
   lda hashPtr+1
   sbc #>identHashTable
   sta number+1
   lsr number+1
   ror number+0
   lsr number+1
   ror number+0
   ldx #number
   jsr putnum
   lda #","
   jsr putchar
   lda tokenNextChar
   showChar = *
   cmp #chrCR
   bne +
   lda #"\"
   jsr putchar
   lda #"n"
+  jsr putchar
   showCR = *
   lda #chrCR
   jsr putchar
   rts

dispString = *
   lda #"s"
   jsr putchar
   lda #":"
   jsr putchar
   jsr showStr
   jmp showCR

showStr = *
   lda #<stringBuf
   ldy #>stringBuf
   sta zp+0
   sty zp+1
   lda stringLen
   ldy #0
   ldx #stdout
   jsr write
   rts

dispSpecial = *
   lda #"c"
   jsr putchar
   lda #":"
   jsr putchar
   lda tokenChar
   jmp showChar

dispNumber = *
   lda #"n"
   jsr putchar
   lda #":"
   jsr putchar
   lda #<stringBuf
   ldy #>stringBuf
   sta zp+0
   sty zp+1
   ldx #number
   lda #1
   jsr aceMiscUtoa
   sty stringLen
   jsr showStr
   jmp showCR

;=== dynamic memory routines ===

mallocWork = memWork ;(16)  ;required work area; defined earlier

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
+  sec
   ;rts
   ;lda #<nomemMsg
   ;ldy #>nomemMsg
   ;jsr eputs
   lda #errInsufficientMemory
   jmp error

   ;nomemMsg = *
   ;.byte chrCR
   ;.asc "Insufficient memory, aborting."
   ;.byte chrCR,0

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
quickMalloc = *
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

;======== standard library ========

eputs = *
   ldx #stderr
   jmp fputs
puts = *
   ldx #stdout
fputs = *
   sta zp+0
   sty zp+1
fputsZp = *
   ldy #$ff
-  iny
   lda (zp),y
   bne -
   tya
   ldy #0
   jmp write

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
   lda zp+0
   ldy zp+1
   rts

multiply32 = *  ;( [sw+0]=multiplicand, [sw+4]=multiplier ) : [sw+8]=result
   lda #$00
   sta syswork+8+0
   sta syswork+8+1
   sta syswork+8+2
   sta syswork+8+3
-  lsr syswork+4+3
   ror syswork+4+2
   ror syswork+4+1
   ror syswork+4+0
   bcc +
   ldx #0
   ldy #4
   clc
-  lda syswork+8,x
   adc syswork+0,x
   sta syswork+8,x
   inx
   dey
   bne -
+  lda syswork+4+0
   bne +
   ora syswork+4+1
   ora syswork+4+2
   ora syswork+4+3
   bne +
   rts
+  asl syswork+0+0
   rol syswork+0+1
   rol syswork+0+2
   rol syswork+0+3
   jmp --

divide32 = *  ;( [sw+0]=topnum, [sw+4]=botnum ): [sw+8]=result,[sw+12]=remainder
   ldx #7
   lda #$00
-  sta syswork+8,x
   dex
   bpl -
   lda #32
   sta div32count

   div32loop = *
   asl syswork+0+0
   rol syswork+0+1
   rol syswork+0+2
   rol syswork+0+3
   rol syswork+12+0
   rol syswork+12+1
   rol syswork+12+2
   rol syswork+12+3
   sec
   ldy #4
   ldx #0
-  lda syswork+12,x
   sbc syswork+4,x
   inx
   dey
   bne -
   php
   rol syswork+8+0
   rol syswork+8+1
   rol syswork+8+2
   rol syswork+8+3
   plp
   bcc +
   sec
   ldy #4
   ldx #0
-  lda syswork+12,x
   sbc syswork+4,x
   sta syswork+12,x
   inx
   dey
   bne -
+  dec div32count
   bne div32loop
   rts
   div32count .buf 1

;end of file + blank line

