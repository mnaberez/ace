;*** as v1.20 program: one-pass assembler  -  by Craig Bruce  -  15-Oct-1994

; This is the Buddy-assembler version of the program

;todo: -implement storage classes: $00=internal, $01=rel.label, $80=exported
;      -implement source column, make line:col point to start of cur token
;      -make it so you can use a "\<CR>" to continue a line

.seq "acehead.s"
.org aceAppAddress
.obj "@0:as"

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;======== global declarations ========

sourceFcb     =  2  ;(1)   ;file control block of current input file
bufptr        =  3  ;(1)   ;byte offset inside of source buffer
sourceLine    =  4  ;(4)   ;source file current line
number        =  8  ;(4)   ;token: 32-bit number value
stringLen     = 12  ;(1)   ;length of string in string buffer
prevChar      = 13  ;(1)   ;previous character scanned, next read (1-too-far)
tokenType     = 14  ;(1)   ;token: type 
tokenNextChar = 15  ;(1)   ;token: next char after token
tokenNumBytes = 16  ;(1)   ;token: number length (1-4 bytes)
tokenChar     = 17  ;(1)   ;token: character
name          = 18  ;(2)   ;pointer to source filename
sourceCol     = 20  ;(1)   ;source file column within line
nextPlusLab   = 22  ;(4)   ;number for next "+" label
prevMinusLab  = 26  ;(4)   ;number for previous "-" label
address       = 30  ;(4)   ;current memory address for code assembly
hashVal       = 34  ;(2)   ;16-bit hash value result
hashPtr       = 36  ;(2)   ;pointer to selected entry in identifier hash table
idPtr         = 38  ;(4)   ;pointer to current identifier descriptor
idVal         = 42  ;(4)   ;value of current identifier
varPtr        = 46  ;(4)   ;pointer to current identifier to be defined
filePtr       = 50  ;(4)   ;pointer to current file info descriptor
idType        = 54  ;(1)   ;type of current identifier (value,address,unres)
originSet     = 55  ;(1)   ;flag indicating whether origin has been set
expPlusCount  = 56  ;(1)   ;number of plus monadic operators of an operand
expMinusCount = 57  ;(1)   ;number of minus monadic operators of an operand
expLessCount  = 58  ;(1)   ;number of low-byte monadic operators of an operand
expGreaterCount = 59 ;(1)  ;number of high-byte monadic operators of an operand
expOffset     = 60  ;(1)   ;offset of cur operand in an expression descriptor
expPtr        = 62  ;(4)   ;pointer to expression descriptor in far memory
addressOrigin = 66  ;(4)   ;origin as set by org directive
holeCount     = 70  ;(4)   ;number of holes (unresolved references)
relHoleCount  = 74  ;(4)   ;number of holes for relative (internal) references
instrNum      = 78  ;(1)   ;assem instr: instruction number (1-56)
instrAddrMode = 79  ;(1)   ;assem instr: addressing mode for instr (1-12)
instrValue    = 80  ;(2)   ;assem instr: operand value for instr (8/16 bit)
instrValueType = 82 ;(1)   ;assem instr: operand value type
instrOpcode   = 83  ;(1)   ;assem instr: opcode of instr/addrmode
instrLen      = 84  ;(1)   ;assem instr: length of instruction
memBufPage    = 85  ;(1)   ;mem put: page of buffer
memBufPtr     = 86  ;(4)   ;mem put: pointer to 1K memory-buffer block
memPutVals    = 90  ;(4)   ;mem put: values to put into memory
memPutCount   = 94  ;(1)   ;mem put: number of values to put into memory
memPutPage    = 95  ;(1)   ;mem put: current page for values
newLine       = 96  ;(4)   ;current input line
newCol        = 100 ;(1)   ;current input column
arg           = 102 ;(2)   ;current command line argument
reserved      = 104
memWork       = 112 ;(16)  ;work area for the memory routines/low-level work
strDelimit    = 112 ;(1)   ;string tokenize: delimiter--overlap
numBase       = 113 ;(1)   ;number tokenize: base--overlap
numSave       = 114 ;(4)   ;number tokenize: save--overlap
work          = 112 ;(16)  ;miscellaneous work--low-level--overlap

hashTablePages = 16
hashTableEntriesHigh = 4
hashTableMask  = $03
symNew = $81
symUnresolved = $80
symFound = 0

tokenIdentifier = 0
tokenNumber  = 1
tokenString  = 2
tokenSpecial = 3
tokenEOF     = 4

chrQuote = $22
chrEOF = $00

errOk            = 0       ;ok
errIdentTooLong  = 1       ;an identifier token exceeds 240 chars in length
errStringTooLong = 2       ;a string literal exceeds 240 chars in length
errNoCloseQuote  = 3       ;ran into a CR before end of string literal
errBadNumber     = 4       ;invalid numeric literal
errNumOverflow   = 5       ;numeric literal value overflows 32-bits
errSyntax        = 6       ;syntax error
errInvalStrOpers = 7       ;attempt to perform numeric operators on a string
errTooManyOperands = 8     ;expression has more than 17 operands
errInsufficientMemory = 9  ;ran out of memory during compilation process
errRedefinedSymbol = 10    ;attempt to redefine a symbol
errOriginNotSet  = 11      ;attempt to assemble code with code origin not set
errInternError   = 12      ;internal error: You should never see this
errNonNumIdExpr  = 13      ;non-numeric symbol in a numeric expression
errExpectOperator = 14     ;expecting an operator
errExpectOperand = 15      ;expecting an operand
errExpectCommand = 16      ;expecting a command
errValueTooLarge = 17      ;value is too large for operation (or is negative)
errBranchTooFar  = 18      ;branch out of range
errNotImplemented = 19     ;feature is not (yet) implemented
errWrongAdmode   = 20      ;instruction does not support given address mode
errAddressWrap   = 21      ;address wraped around 64K code address space
errObjectFile    = 22      ;error trying to write output object file
errNotResolvedExpr = 23    ;directive requires resolved expression
errOriginAlreadySet = 24   ;code origin already set; you can't set it twice
errUnresdReferences = 25   ;unresolved reference
errExpectString  = 26      ;expecting a string-literal filename
errSourceFile    = 27      ;cannot open source file

debug      .buf 1  ;display debug information: $ff=yes, $00=no
symDump    .buf 1  ;display symbol table dump: $ff=yes, $00=no
outputType .buf 1  ;type of output module: $00=binary, $01=reloc, $80=link

;======== main routine ========

main = *
   ;** check for large enough TPA
   sec
   lda #<bssEnd
   cmp aceMemTop+0
   lda #>bssEnd
   sbc aceMemTop+1
   bcs +
   jmp mainCont
+  lda #<tpaMsg
   ldy #>tpaMsg
   jsr eputs
die = *
   lda #1
   ldx #0
   jmp aceProcExit

tpaMsg = *
   .byte "as: Insufficient program space to run in",chrCR,0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   sta zp+0
   sty zp+1
   lda #<usageMsgEnd-usageMsg
   ldy #>usageMsgEnd-usageMsg
   ldx #stderr
   jsr write
   jmp die

usageMsg = *
   .byte "usage: as [-help] [-s] [-d] [-q] [file ...]",chrCR,chrCR
   .byte "       -help : produce this information, don't run",chrCR
   .byte "          -s : produce symbol table dump at end",chrCR
   .byte "          -d : provide debugging information (LOTS)",chrCR
   .byte "          -q : take quoted text literally (ignore backslashes)"
   .byte chrCR
   usageMsgEnd = *

defaultInput = *
   .byte "-stdin-",0

filenameUsed .buf 1

mainCont = *
   ;** init globals
   lda #$00
   sta debug           ;default off
   sta symDump         ;default none
   sta outputType      ;default binary
   sta ignoreBackslash ;default off
   lda #$00
   sta filenameUsed
   lda #0
   sta arg+0
   sta arg+1
   jsr asInit
   ldx #7
-  lda writeDefaultName,x
   sta writeName,x
   dex
   bpl -

   mainNext = *
   jsr checkStop
   inc arg+0
   bne +
   inc arg+1
+  lda arg+0
   ldy arg+1
   jsr getarg
   lda zp+0
   ora zp+1
   beq mainExit
   lda zp+0
   ldy zp+1
   sta name+0
   sty name+1
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jsr handleFlags
   jmp mainNext
+  jsr mainFile
   jmp mainNext

mainExit = *
   bit filenameUsed
   bmi +
   lda #<defaultInput
   ldy #>defaultInput
   ldx #$01
   jsr fileOpen
   jsr asDriver  ;it closes the file
+  ldx #3
-  lda prevFilePtr,x
   sta filePtr,x
   dex
   bpl -
   lda #$00
   ldx #3
-  ora holeCount
   dex
   bpl -
   cmp #$00
   beq +
   jsr findUnresSymbol
   lda #errUnresdReferences
   jmp errorRef
+  jsr writeObjectFile
   jsr dumpSymbolTable
   rts

handleFlags = *
   iny
   lda (zp),y
   bne +
   rts
+  cmp #"s"
   beq flagS
   cmp #"d"
   beq flagD
   cmp #"q"
   beq flagQ
   jmp usage

flagS = *
   lda #$ff
   sta symDump
   jmp handleFlags

flagD = *
   lda #$ff
   sta debug
   jmp handleFlags

flagQ = *
   lda #$ff
   sta ignoreBackslash
   jmp handleFlags
   ignoreBackslash .buf 1

echo = *
   lda #<echoMsg1
   ldy #>echoMsg1
   jsr eputs
   jsr echoName
   lda #<echoMsg2
   ldy #>echoMsg2
   jmp eputs
echoName = *
   lda name+0
   ldy name+1
   jmp eputs

echoMsg1 = *
   .byte "assembling source file ",chrQuote,0
echoMsg2 = *
   .byte chrQuote,chrCR,0

mainFile = *
   bit filenameUsed
   bmi +
   jsr mainExtractObj
+  lda name+0
   ldy name+1
   ldx #$00
   jsr fileOpen
   bcc +
   jsr echoName
   lda #<mainFileErr
   ldy #>mainFileErr
   jsr eputs
   jmp die
+  jsr asDriver  ;it closes the file
   lda #$ff
   sta filenameUsed
   rts
mainFileErr = *
   .byte ": Cannot open file",chrCR,0

fileOpenType .buf 1
prevFilePtr  .buf 4

fileOpen = *  ;( (.AY)=filename,.X=type) : sourceFcb, <file identifier>, .CS=err
   sta name+0
   sty name+1
   stx fileOpenType
   jsr echo
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda fileOpenType
   cmp #$01
   bne +
   lda #stdin
   jmp ++
+  lda #"r"
   jsr open
   bcc +
   rts
+  sta sourceFcb
   ldx #25
   lda #$00
-  sta workBuf,x
   dex
   bpl -
   ldy #0
-  lda (name),y
   sta workBuf+25,y
   beq +
   iny
   bne -
+  lda fileOpenType
   sta workBuf+12
   iny
   tya
   clc
   adc #25
   sta workBuf+24
   ldy #0
   jsr malloc
   bcc +
   jmp die
   rts
+  ldx #3
-  lda filePtr,x
   sta workBuf+0,x
   lda mp,x
   sta filePtr,x
   sta prevFilePtr,x
   lda #$00
   sta newLine,x
   dex
   bpl -
   sta newCol
   lda #1
   sta newLine+0
   lda #<workBuf
   ldy #>workBuf
   sta zp+0
   sty zp+1
   lda workBuf+24
   ldy #0
   jsr aceMemStash
   lda #"@"
   sta workBuf+25
   sta workBuf+26
   lda #$ff
   sta bufptr
   lda #" "
   sta prevChar
   clc
   rts

fileInfoSave = *
   jsr fileInfoSetup
   jsr aceMemFetch
   lda #0
   ldy #1
   jsr malloc
   ldx #3
-  lda mp,x
   sta workBuf+16,x
   lda newLine,x
   sta workBuf+4,x
   dex
   bpl -
   lda #<sourceBuf
   ldy #>sourceBuf
   sta zp+0
   sty zp+1
   lda #0
   ldy #1
   jsr aceMemStash
   lda bufptr
   sta workBuf+15
   lda prevChar
   sta workBuf+14
   lda sourceFcb
   sta workBuf+13
   lda newCol
   sta workBuf+8
   jsr fileInfoSetup
   jsr aceMemStash
   rts

fileInfoRestore = *
   jsr fileInfoSetup
   jsr aceMemFetch
   ldx #3
-  lda workBuf+16,x
   sta mp,x
   lda workBuf+4,x
   sta newLine,x
   dex
   bpl -
   lda #<sourceBuf
   ldy #>sourceBuf
   sta zp+0
   sty zp+1
   lda #0
   ldy #1
   jsr aceMemFetch
   lda #0
   ldy #1
   jsr free
   lda workBuf+15
   sta bufptr
   lda workBuf+14
   sta prevChar
   lda workBuf+13
   sta sourceFcb
   lda workBuf+8
   sta newCol
   rts

fileInfoSetup = *
   ldx #3
-  lda filePtr,x
   sta mp,x
   dex
   bpl -
   lda #<workBuf
   ldy #>workBuf
   sta zp+0
   sty zp+1
   lda #24
   ldy #0
   rts

fileClose = *
   lda sourceFcb
   cmp #stdin
   beq +
   jsr close
+  ldx #3
-  lda filePtr,x
   sta mp,x
   sta prevFilePtr,x
   dex
   bpl -
   ldx #filePtr
   ldy #4
   jsr aceMemZpload
   rts

mainExtractObj = *
   ldx #0 ;copy-start
   ldy #0
-  lda (name),y
   beq +++
   cmp #":"
   beq +
   cmp #"/"
   bne ++
+  tya
   tax
   inx
+  iny
   bne -
+  txa
   tay
   ldx #0
-  lda (name),y
   sta stringBuf,x
   beq +
   iny
   inx
   bne -
+  nop
-  cpx #3
   bcs +
   rts
+  lda stringBuf-2,x
   cmp #","
   bne +
   lda #0
   dex
   dex
   sta stringBuf,x
   jmp -
+  cmp #"."
   beq +
   rts
+  lda stringBuf-1,x
   cmp #"s"
   beq +
   rts
+  cpx #18
   bcc +
   ldx #18
+  lda #","
   sta stringBuf-2,x
   lda #"p"
   sta stringBuf-1,x
   lda #0
   sta stringBuf,x
-  lda stringBuf,x
   sta writeName,x
   dex
   bpl -
   rts

asInit = *  ;initialize variables for assembly
   ldx #3
   lda #0
-  sta filePtr,x
   sta nextPlusLab,x
   sta prevMinusLab,x
   sta address,x
   sta holeCount,x
   sta relHoleCount,x
   dex
   bpl -
   lda #1
   sta newLine+0
   sta nextPlusLab+0
   lda #$00
   sta originSet
   sta symHash+0
   sta symHash+1
   lda #<$1300
   ldy #>$1300
   sta address+0
   sty address+1
   sta addressOrigin+0
   sty addressOrigin+1
   jsr mallocInit
   jsr initSymbolTable
   jsr memInit
   rts

;======== driver ========

forceEofFlag .buf 1

asDriver = *
   lda #$00
   sta forceEofFlag
-  bit forceEofFlag
   bmi +
   jsr parse
   bcc -
+  jsr fileClose
   lda filePtr+3
   cmp #aceMemNull
   beq +
   jsr fileInfoRestore
   jsr fileReturnEcho
   jmp asDriver
+  rts

fileReturnEcho = *
   jsr fileInfoSetup
   lda #0
   ldy #1
   jsr aceMemFetch
   lda #<fileRetMsg
   ldy #>fileRetMsg
   jsr eputs
   lda #<workBuf+25
   ldy #>workBuf+25
   jsr eputs
   lda #chrQuote
   jsr eputchar
   lda #chrCR
   jsr eputchar
   rts
   fileRetMsg .byte "resuming source file ",chrQuote,0

writeObTry .buf 1
writeObFd  .buf 1

writeObjectFile = *
   lda #<writeMsg
   ldy #>writeMsg
   jsr eputs
   lda #<writeName
   ldy #>writeName
   jsr eputs
   lda #chrQuote
   ldx #stderr
   jsr putc
   ldx #stderr
   lda #chrCR
   jsr putc
   bit originSet
   bmi +
   lda #errOriginNotSet
   jmp error
+  lda #2
   sta writeObTry
   lda #<writeName
   ldy #>writeName
   sta zp+0
   sty zp+1

   ;** open object for writing
-  lda #"W"
   jsr open
   bcc +
   lda #errObjectFile
   jmp error
+  sta writeObFd

   ;** write object data
   lda addressOrigin+0
   ldx writeObFd
   jsr putc
   lda addressOrigin+1
   ldx writeObFd
   jsr putc
   lda writeObFd
   ldx addressOrigin+0
   ldy addressOrigin+1
   jsr memSave

   ;** close object
   lda writeObFd
   jsr close
   bcc +
   lda #errObjectFile
   jmp error
+  lda #stderr
   sta writeObFd
   lda #<writeRangeMsg1
   ldy #>writeRangeMsg1
   jsr eputs
   lda addressOrigin+1
   ldx #stderr
   jsr fputhex
   lda addressOrigin+0
   ldx #stderr
   jsr fputhex
   lda #<writeRangeMsg2
   ldy #>writeRangeMsg2
   jsr eputs
   sec 
   lda address+0
   sbc #1
   pha
   lda address+1
   sbc #0
   ldx #stderr
   jsr fputhex
   pla
   ldx #stderr
   jsr fputhex
   lda #<writeRangeMsg3
   ldy #>writeRangeMsg3
   jsr eputs
   sec
   lda address+0
   sbc addressOrigin+0
   sta work+0
   lda address+1
   sbc addressOrigin+1
   sta work+1
   lda #0
   sta work+2
   sta work+3
   ldx #work
   jsr eputnum
   lda #<writeRangeMsg4
   ldy #>writeRangeMsg4
   jsr eputs
   rts

writeDefaultName = *
   .byte "a.out,p",0
writeMsg = *
   .byte "writing object file ",chrQuote,0
writeRangeMsg1 = *
   .byte "code range: $",0
writeRangeMsg2 = *
   .byte "--$",0
writeRangeMsg3 = *
   .byte " (",0
writeRangeMsg4 = *
   .byte " bytes)",chrCR,0

;======== parser ========

parse = *
   ;** at beginning of command
   jsr checkStop
   jsr getToken
   lda tokenType
   cmp #tokenEOF
   bne +
   sec
   rts
+  cmp #tokenIdentifier
   bne parseNotId
   lda tokenNextChar
   cmp #"="
   bne +
   jmp parseEquate
+  cmp #":"
   bne +
   jmp parseAddress
+  jmp parseIdentifier

parseNotId = *
   cmp #tokenSpecial
   beq +
   syntaxError = *
   lda #errSyntax
   jmp error
+  lda tokenChar
   cmp #chrCR
   bne +
   jmp parseEnd
+  cmp #"+"
   bne +
   jmp parsePlus
+  cmp #"-"
   bne +
   jmp parseMinus
+  cmp #"#"
   bne +
   jmp parseEnd
+  cmp #"."
   bne +
   jmp parseEnd
+  cmp #"/"
   bne +
   jsr parsePlusSub
   jmp parseMinus
+  jmp syntaxError

;ret: .X=tokenIdentifier, .A=nextChar, .Y=strlen, stringLen, stringBuf
;     .X=tokenNumber,     .Y=numlen, number
;     .X=tokenString,     .A=firstChar,.Y=strlen, stringLen, stringBuf
;     .X=tokenSpecial,    .A=char

parseEquate = *
   jsr parseDefineVar
   jsr getToken
   cpx #tokenSpecial
   beq +
-  jmp syntaxError
+  cmp #"="
   bne -
   lda #0
   jsr parseExpression
   cmp #chrCR
   bne -
   jsr evaluateExpression
   bcs +
   jsr assignVariable
   jmp ++
+  jsr addVariableHole
+  bit debug
   bpl +
   lda #<parseEquateMsg
   ldy #>parseEquateMsg
   jsr puts
+  jmp parseEnd
   parseEquateMsg = *
   .byte "parseEquate: assign parsed expression to variable.",chrCR,0

parseAddress = *
   jsr parseDefineVar
   jsr getToken
   cpx #tokenSpecial
   beq +
-  jmp syntaxError
+  cmp #":"
   bne -
-  jsr parseAssignAddress
   bit debug
   bpl +
   lda #<parseAddressMsg
   ldy #>parseAddressMsg
   jsr puts
+  jmp parseEnd
   parseAddressMsg = *
   .byte "parseAddress: assign current address to variable.",chrCR,0

parseAddressNoColon = *
   jsr parseDefineVar
   jmp -

parseAssignAddress = *
   bit originSet
   bmi +
   lda #errOriginNotSet
   jmp error
+  ldx #3
-  lda address,x
   sta idVal,x
   dex
   bpl -
   lda #$01
   sta idType
   jmp assignVariable

parsePlusExitFlag .buf 1

parsePlusSub = *
   ldx #$ff
   stx parsePlusExitFlag
   jmp +
parsePlus = *
   ldx #$00
   stx parsePlusExitFlag
+  lda #"+"
   ldy #0
   jsr genRelLabel
   jsr parseDefineVar
   jsr parseAssignAddress
   bit debug
   bpl +
   lda #<parsePlusMsg
   ldy #>parsePlusMsg
   jsr puts
   ldx #nextPlusLab
   jsr putnum
   lda #chrCR
   jsr putchar
+  inc nextPlusLab+0
   bne +
   inc nextPlusLab+1
   bne +
   inc nextPlusLab+2
   bne +
   inc nextPlusLab+3
+  bit parsePlusExitFlag
   bmi +
   jmp parseEnd
+  rts
   parsePlusMsg = *
   .byte "parsePlus: assign current address to a syntheic '+' variable:",0

parseMinus = *
   inc prevMinusLab+0
   bne +
   inc prevMinusLab+1
   bne +
   inc prevMinusLab+2
   bne +
   inc prevMinusLab+3
+  lda #"-"
   ldy #0
   jsr genRelLabel
   jsr parseDefineVar
   jsr parseAssignAddress
   bit debug
   bpl +
   lda #<parseMinusMsg
   ldy #>parseMinusMsg
   jsr puts
   ldx #prevMinusLab
   jsr putnum
   lda #chrCR
   jsr putchar
+  jmp parseEnd
   parseMinusMsg = *
   .byte "parseMinus: assign current address to a synthetic '-' variable:",0

genRelLabel = *  ;( .A=type, .Y=relative )
   sta stringBuf+1
   cmp #"+"
   bne +
   clc
   tya
   adc nextPlusLab
   sta number+0
   ldy #3
   ldx #1
-  lda nextPlusLab,x
   adc #0
   sta number,x
   inx
   dey
   bne -
   jmp ++
+  sec
   sty number+0
   lda prevMinusLab+0
   sbc number+0
   sta number+0
   ldy #3
   ldx #1
-  lda prevMinusLab,x
   sbc #0
   sta number,x
   inx
   dey
   bne -
+  lda #"L"
   sta stringBuf+0
   lda #<stringBuf+2
   ldy #>stringBuf+2
   sta zp+0
   sty zp+1
   ldx #number
   lda #1
   jsr aceMiscUtoa
   iny
   iny
   lda #"c"
   sta stringBuf,y
   iny
   lda #0
   sta stringBuf,y
   sty stringLen
   rts

parseEnd = *
   bit debug
   bpl +
   lda #<parseEndMsg
   ldy #>parseEndMsg
   jsr puts
+  clc
   rts
   parseEndMsg = *
   .byte "-----=-----",chrCR,chrCR,0

parseTemp .buf 3

parseIdentifier = *  ;line starting with an identifier
   ;** check if identifier is a processor instruction
   lda stringLen
   cmp #3
   bne parseIdNotInstr
   ldx #2
-  lda stringBuf,x
   sta parseTemp,x
   and #$7f
   sta stringBuf,x
   dex
   bpl -
   jsr hash
   ldx #2
-  lda parseTemp,x
   sta stringBuf,x
   dex
   bpl -
   lda hashVal+0
   and #63
   tax
   lda instrHashPtrs,x
   cmp #100
   bcs +
   jsr parseIdCheckInstr
   bcs parseIdNotInstr
   jmp instr
+  sec
   sbc #100
   tax
-  lda instrHashIndirects,x
   beq parseIdNotInstr
   jsr parseIdCheckInstr
   bcs +
   jmp instr
+  inx
   bne -

   ;** check if identifier is a directive
   parseIdNotInstr = *
   jsr checkIfDirective
   bcs parseIdNotDirective
   jmp directive

   ;** check if identifier is a macro or old-fashioned label
   parseIdNotDirective = *
   jsr hash
   jmp parseAddressNoColon
   lda #errExpectCommand
   jmp error

;======== handle assembler directives ========

direcNum .buf 1

checkIfDirective = *  ;( stringBuf, stringLen ) : .CS=no, .A=dirNum
   lda #0
   sta direcNum

   direcCheckNext = *
   ldx direcNum
   ldy direcOffsets,x
   ldx #0
-  lda stringBuf,x
   and #$7f
   cmp direcNames,y
   bne direcCheckCont
   cmp #$00
   bne +
   lda direcNum
   clc
   rts
+  iny
   inx
   bne -

   direcCheckCont = *
   inc direcNum
   lda direcNum
   cmp #27
   bcc direcCheckNext
   lda #0
   sec
   rts

direcOffsets = *
   .byte 00,06,11,16,22,30,36,41,50,55,60,64,71,77,84,89,96,106,114,119,125
   .byte 131,137,142,147,155,160,164

direcNames = *
   .byte ".byte",0     ;dn=00.  off=00
   .byte ".buf",0      ;dn=01.  off=06
   .byte ".asc",0      ;dn=02.  off=11  ;syn .byte
   .byte ".word",0     ;dn=03.  off=16
   .byte ".triple",0   ;dn=04.  off=22
   .byte ".long",0     ;dn=05.  off=30
   .byte ".org",0      ;dn=06.  off=36
   .byte ".include",0  ;dn=07.  off=41
   .byte ".seq",0      ;dn=08.  off=50  ;syn .include
   .byte ".obj",0      ;dn=09.  off=55
   .byte ".if",0       ;dn=10.  off=60
   .byte ".elsif",0    ;dn=11.  off=64
   .byte ".else",0     ;dn=12.  off=71
   .byte ".endif",0    ;dn=13.  off=77
   .byte ".ife",0      ;dn=14.  off=84  ;syn .endif
   .byte ".macro",0    ;dn=15.  off=89
   .byte ".endmacro",0 ;dn=16.  off=96
   .byte ".global",0   ;dn=17.  off=106
   .byte ".end",0      ;dn=18.  off=114
   .byte ".text",0     ;dn=19.  off=119 ;not implemented
   .byte ".data",0     ;dn=20.  off=125 ;not implemented
   .byte ".comm",0     ;dn=21.  off=131 ;not implemented
   .byte ".bss",0      ;dn=22.  off=137 ;not implemented
   .byte ".scr",0      ;dn=23.  off=142 ;not implemented
   .byte ".tascii",0   ;dn=24.  off=147 ;not implemented
   .byte ".equ",0      ;dn=25.  off=155 ;not implemented
   .byte "equ",0       ;dn=26.  off=160 ;not implemented
   .byte ".byt",0      ;dn=27.  off=164 ;syn .byte

direcVectors = *
   .word direcByte,direcBuf,direcByte,direcWord,direcTriple,direcLong,direcOrg
   .word direcInclude,direcInclude,direcComment
   .word direcIf,direcElsif,direcElse,direcEndif,direcEndif
   .word direcMacro,direcEndmacro,direcGlobal,direcEnd
   .word direcText,direcData,direcComm,direcBss
   .word direcScr,direcTascii,direcEqu,direcEqu,direcByte

directive = *  ;( .A=dirNum )
   bit debug
   bpl +
   pha
   lda #<direcMsg
   ldy #>direcMsg
   jsr puts
   pla
+  asl
   tay
   lda direcVectors+0,y
   sta work+14
   lda direcVectors+1,y
   sta work+15
   jsr +
   jmp parseEnd
+  jmp (work+14)
   direcMsg = *
   .byte "must parse a directive",chrCR,0

direcOrg = *
   bit originSet
   bpl +
   lda #errOriginAlreadySet
   jmp error
+  lda #0
   jsr parseExpression
   cmp #chrCR
   beq +
   jmp syntaxError
+  jsr evaluateExpression
   bcc +
   lda #errNotResolvedExpr
   jmp error
+  lda idVal+2
   ora idVal+3
   beq +
   lda #errValueTooLarge
   jmp error
+  lda idVal+0
   ldy idVal+1
   sta address+0
   sty address+1
   sta addressOrigin+0
   sty addressOrigin+1
   lda #$ff
   sta originSet
   bit debug
   bpl +
   lda #<direcOrgMsg
   ldy #>direcOrgMsg
   jsr puts
   lda idVal+1
   jsr puthex
   lda idVal+0
   jsr puthex
   lda #chrCR
   jsr putchar
+  rts
   direcOrgMsg = *
   .byte "setting code origin to $",0

direcDfSize .buf 1
direcDfChar .buf 1

direcByte = *
   lda #1
   bne direcDlIn
direcWord = *
   lda #2
   bne direcDlIn
direcTriple = *
   lda #3
   bne direcDlIn
direcLong = *
   lda #4
   direcDlIn = *
   sta direcDfSize
   bit originSet
   bmi direcDfNext
   lda #errOriginNotSet
   jmp error

   direcDfNext = *
   lda #1
   jsr parseExpression
   sta direcDfChar
   cpx #0
   beq +
   jsr direcDfString
   jmp direcDfCont
+  jsr evaluateExpression
   bcs ++
   ldx direcDfSize
-  cpx #4
   bcs ++
   lda idVal,x
   bne +
   inx
   bne -
+  lda #errValueTooLarge
   jmp error
+  lda #1
   ldy #0
   sta direcDfCount+0
   sty direcDfCount+1
   jsr direcDfPut

   direcDfCont = *
   lda direcDfChar
   cmp #","
   beq direcDfNext
   cmp #chrCR
   beq +
   jmp syntaxError
+  rts

direcDfStrPos .buf 1

direcDfString = *
   lda #0
   sta direcDfStrPos
   sta idType
   sta idVal+1
   sta idVal+2
   sta idVal+3
-  ldx direcDfStrPos
   cpx stringLen
   bcc +
   rts
+  lda stringBuf,x
   sta idVal+0
   lda #1
   ldy #0
   sta direcDfCount+0
   sty direcDfCount+1
   jsr direcDfPut
   inc direcDfStrPos
   jmp -

direcDfCount .buf 2

direcDfPut = *  ;(address++, idVal, idType, direcDfSize, direcDfCount--, expBuf)
   lda direcDfCount+0
   ora direcDfCount+1
   bne +
   rts

   ;** debug information
+  bit debug
   bpl +++
   lda #<direcDfPutMsg1
   ldy #>direcDfPutMsg1
   jsr puts
   lda address+1
   jsr puthex
   lda address+0
   jsr puthex
   lda #<direcDfPutMsg2
   ldy #>direcDfPutMsg2
   jsr puts
   bit idType
   bpl +
   lda #"?"
   jsr putchar
   jmp ++
+  ldx #idVal
   jsr putnum
+  lda #chrCR
   jsr putchar

   ;** handle unresolved reference
+  bit idType
   bpl +
   ldx direcDfSize
   lda #0
   jsr addMemoryHole

   ;** handle resolved reference
+  ldx #3
-  lda idVal,x
   sta memPutVals,x
   dex
   bpl -
   lda direcDfSize
   ldx address+0
   ldy address+1
   jsr memPut
   ;** add relocatable reference
   lda idType
   beq +
   cmp #$04
   bcs +
   ldx address+0
   ldy address+1
   jsr recordRelocRef

   ;** go onto next put
+  clc
   lda address+0
   adc direcDfSize
   sta address+0
   bcc +
   inc address+1
   bne +
   lda #errAddressWrap
   jmp error
+  lda direcDfCount+0
   bne +
   dec direcDfCount+1
+  dec direcDfCount+0
   jmp direcDfPut

direcDfPutMsg1 = *
   .byte "define-put: address=$",0
direcDfPutMsg2 = *
   .byte ", value=",0

direcBuf = *
   bit originSet
   bmi +
   lda #errOriginNotSet
   jmp error
+  lda #0
   jsr parseExpression
   cmp #chrCR
   beq +
   jmp syntaxError
+  jsr evaluateExpression
   bcc +
   lda #errNotResolvedExpr
   jmp error
+  lda idVal+2
   ora idVal+3
   beq +
   lda #errValueTooLarge
   jmp error
+  lda idVal+0
   ldy idVal+1
   sta direcDfCount+0
   sty direcDfCount+1
+  lda #0
   sta idType
   sta idVal+0
   sta idVal+1
   sta idVal+2
   sta idVal+3
   lda #1
   sta direcDfSize
   jsr direcDfPut
   rts

direcComment = *
-  jsr getToken
   cpx #tokenSpecial
   bne -
   cmp #chrCR
   beq +
   cmp #chrEOF
   bne -
+  rts

direcInclude = *
   jsr getToken
   cpx #tokenString
   beq +
   lda #errExpectString
   jmp error
+  jsr getToken
   cpx #tokenSpecial
   beq +
-  jmp syntaxError
+  cmp #chrCR
   bne -
   jsr fileInfoSave
   lda #<stringBuf
   ldy #>stringBuf
   ldx #$00
   jsr fileOpen
   bcs +
   rts
+  lda #errSourceFile
   jmp error

direcEnd = *
   lda #$ff
   sta forceEofFlag
   rts

direcIf = *
direcElsif = *
direcElse = *
direcEndif = *
direcMacro = *
direcEndmacro = *
direcGlobal = *
direcText = *
direcData = *
direcComm = *
direcBss = *
direcScr = *
direcTascii = *
direcEqu = *
   lda #errNotImplemented
   jmp error

;======== error handler ========

errorCode      .buf 1
errorPos       .buf 1
errorExitCode  = aceExitData+0    ;$fc8a09cb:editor-repos
errorExitLine  = aceExitData+4
errorExitCol   = aceExitData+8
errorExitRepos = aceExitData+12
errorExitFilep = aceExitData+13
errorExitReser = aceExitData+14
errorBuf       = aceExitData+15

errorRef = *
   jsr errorPreramble
   lda #<errorMsg2
   ldy #>errorMsg2
   jsr errorCat
   ldx #3
-  lda expSrcLine,x
   sta work+0,x
   lda expSrcFile,x
   sta work+4,x
   dex
   bpl -
   lda expSrcCol
   ldx #work+0
   ldy #work+4
   jsr errorDispFile
   jmp errorIn

errorPreramble = *
   sta errorCode
   ldx #14
-  lda errorExitHead,x
   sta errorExitCode,x
   dex
   bpl -
   lda #0
   sta errorPos
   lda #<errorMsg1
   ldy #>errorMsg1
   jsr errorCat
   ldx #sourceLine
   ldy #filePtr
   lda sourceCol
   jmp errorDispFile

error = *
   jsr errorPreramble
   errorIn = *
   ldx errorPos
   lda #" "
   sta errorBuf,x
   inc errorPos
   lda errorCode
   asl
   tax
   lda errDesc+1,x
   tay
   lda errDesc+0,x
   jsr errorCat
   lda errorCode
   cmp #errUnresdReferences
   bne +
   lda #<symName
   ldy #>symName
   jsr errorCat
+  lda #<errorBuf
   ldy #>errorBuf
   jsr eputs
   lda #chrCR
   ldx #stderr
   jsr putc
   lda #1
   ldx #0
   jmp aceProcExit

errDispFiFp  .buf 1
errDispFiCol .buf 1
errDispFiLin .buf 1

errorDispFile = *  ;( .X=zpLineOff, .A=col, .Y=zpFilePtr, errorPos ) : errorPos
   ;** produces output of the form: ("filename":1234:12)
   sta errDispFiCol
   stx errDispFiLin
   sty errDispFiFp
   ldx errorPos
   lda #"("
   sta errorBuf,x
   inx
   lda #chrQuote
   sta errorBuf,x
   inx
   stx errorPos
   ldx errDispFiFp
   ldy #0
-  lda 0,x
   sta mp,y
   inx
   iny
   cpy #4
   bne -
   lda #<workBuf
   ldy #>workBuf
   sta zp+0
   sty zp+1
   lda #25+20
   ldy #0
   jsr aceMemFetch
   lda workBuf+24
   cmp #25+20
   bcc +
   ldy #0
   jsr aceMemFetch
+  lda #<workBuf+25
   ldy #>workBuf+25
   jsr errorCat
   lda #chrQuote
   sta errorBuf,x
   inx
   lda #":"
   sta errorBuf,x
   inx
   stx errorPos
   ldx errDispFiLin
   jsr errorDispFileApnum
   lda #":"
   sta errorBuf,x
   inx
   stx errorPos
   lda errDispFiCol
   sta number+0
   lda #0
   sta number+1
   sta number+2
   sta number+3
   ldx #number
   jsr errorDispFileApnum
   lda #")"
   sta errorBuf,x
   inx
   stx errorPos
   rts

errorDispFileApnum = *  ;( .X=zpoff, errorPos ) : .X=errorPos
   lda #<putnumNum
   ldy #>putnumNum
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   lda #<putnumNum
   ldy #>putnumNum
   ;** fall through

errorCat = *  ;( .AY=str, errorPos ) : .X=errorPos
   sta work+14
   sty work+15
   ldx errorPos
   ldy #0
-  lda (work+14),y
   sta errorBuf,x
   beq +
   inx
   iny
   bne -
+  stx errorPos
   rts

errorExitHead = *
   .byte $fc,$8a,$09,$cb,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$06,$00
errorMsg1 = *
   .byte "err ",0
errorMsg2 = *
   .byte ", ref",0

errDesc .word err00,err01,err02,err03,err04,err05,err06,err07,err08,err09
        .word err10,err11,err12,err13,err14,err15,err16,err17,err18,err19
        .word err20,err21,err22,err23,err24,err25,err26,err27

err00 .byte "Ok--no error",0
err01 .byte "An identifier token exceeds 240 chars in length",0
err02 .byte "A string literal exceeds 240 chars in length",0
err03 .byte "Ran into a CR before end of string literal",0
err04 .byte "Invalid numeric literal",0
err05 .byte "Numeric literal value overflows 32-bits",0
err06 .byte "Syntax error",0
err07 .byte "Attempt to perform numeric operators on a string",0
err08 .byte "Expression has more than 17 operands",0
err09 .byte "Ran out of memory during compilation process",0
err10 .byte "Attempt to redefine a symbol",0
err11 .byte "Attempt to assemble code with code origin not set",0
err12 .byte "Internal error... You should not be seeing this!",0
err13 .byte "Non-numeric symbol in a numeric expression",0
err14 .byte "Expecting an operator",0
err15 .byte "Expecting an operand",0
err16 .byte "Expecting a command",0
err17 .byte "Value is too large or negative",0
err18 .byte "Branch out of range",0
err19 .byte "Feature is not (yet) implemented",0
err20 .byte "Instruction does not support given address mode",0
err21 .byte "Address wraped around 64K code address space",0
err22 .byte "Error trying to write output object file",0
err23 .byte "Directive requires resolved expression",0
err24 .byte "Code origin already set; you can't set it twice",0
err25 .byte "Unresolved symbol: ",0
err26 .byte "Expecting a string-literal filename",0
err27 .byte "Cannot open source file for reading",0

wputnum = *
   lda #stdout
   sta putnumFd
   jmp putnumIn
eputnum = *
   lda #stderr
   jmp fputnum
putnum = *
   lda #stdout
fputnum = *
   sta putnumFd
   ldy #1
putnumIn = *
   lda #<putnumNum
   sta zp+0
   lda #>putnumNum
   sta zp+1
   tya
   jsr aceMiscUtoa
   ldx putnumFd
   jmp fputsZp

putnumFd  .buf 1
putnumNum .buf 11

puthex = *  ;( .A=value )
   ldx #stdout
fputhex = *  ;( .A=value, .X=fd )
   stx putnumFd
   pha
   lsr
   lsr
   lsr
   lsr
   tax
   lda puthexChars,x
   ldx putnumFd
   jsr putc
   pla
   and #$0f
   tax
   lda puthexChars,x
   ldx putnumFd
   jsr putc
   rts
   puthexChars = *
   .byte "0123456789abcdef"

checkStop = *
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
   .byte "<Stopped>",chrCR,0

;======== utility functions ========

incLong = *  ;( .X=zpOffset )
   inc 0,x
   bne +
   inc 1,x
   bne +
   inc 2,x
   bne +
   inc 3,x
+  rts

decLong = *  ;( .X=zpOffset )
   sec
   lda 0,x
   sbc #1
   sta 0,x
   inx
   ldy #3
-  lda 0,x
   sbc #0
   sta 0,x
   inx
   dey
   bne -
   rts

;======== included files ========

.seq "ashelp.s"

;======== bss ========

bss = *
sourceBuf    = bss+0                  ;( 256 bytes)
stringBuf    = sourceBuf+256          ;( 256 bytes)
symBuf       = stringBuf+256          ;( 256 bytes)
symNext      = symBuf+0
symValue     = symBuf+4
symUnresOpnd = symBuf+8
symType      = symBuf+9
symClass     = symBuf+10
symNameLen   = symBuf+11
symName      = symBuf+12
symHash      = symBuf+256            ;( 2 bytes: symbol-caching feature)
symPtr       = symHash+2             ;( 4 bytes: symbol-caching feature)
expTable     = symPtr+4              ;( 256 bytes == 15 entries)
expHoleType  = expTable+0
expLength    = expTable+1
expUnresCnt  = expTable+2
expSrcCol    = expTable+3
expHoleAddr  = expTable+4
expSrcLine   = expTable+8
expSrcFile   = expTable+12
expField     = expTable+0
expOp        = expField+0
expType      = expField+1
expSign      = expField+2
expHiLo      = expField+3
expValue     = expField+4
expNextUnres = expField+8
expNextOpnd  = expField+12
expReserved  = expField+13
identHashTable = expTable+256      ;(4096 bytes == 1024 entries (ref:hTabPag)
memPtrTable  = identHashTable+4096 ;(256 bytes == 64 entries)
memBuf       = memPtrTable+256    ;( 256 bytes)
writeName    = memBuf+256         ;(  20 bytes)
tpaFreemap   = writeName+20       ;( 256 bytes)
workBuf      = tpaFreemap+256     ;( 256 bytes)
bssEnd       = workBuf+256
