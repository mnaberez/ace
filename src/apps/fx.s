;*  Craig's File-eXchange Protocol (FX), Client program.
;*
;*  ACE-6502-Assembler version 1.00
;*
;*  Started 1995/05/03, written by Craig Bruce.
;*
;*  Written for use with the ACE operating system.

;*  usage: fx [-78k] [[-b] file ...] [-t file ...]
;*
;*  -7 : use 7-bit link
;*  -8 : use 8-bit link (default)
;*  -k : use 1K packet sizes
;*  -t : announce text files
;*  -b : announce binary files

.include "acehead.s"

.org aceAppAddress

jmp main
.byte aceID1,aceID2,aceID3
.byte 64,0  ;** stack,reserved

;*** global declarations

TRUE          = $ff
FALSE         = $00
CHR_START     = $01
CHR_END       = $19
CHR_ESC       = $05
CHR_ABORT     = $18
CHR_XON       = $11
CHR_XOFF      = $13
CHR_QUOTE8    = $14

REQ_CONNECT         = "C" - $80
ACK_CONNECT         = "c" + $20
REQ_DISCONNECT      = "Q" - $80
ACK_DISCONNECT      = "q" + $20
REQ_DOWNLOAD_OPEN   = "D" - $80
ACK_DOWNLOAD_OPEN   = "d" + $20
REQ_DOWNLOAD_PACKET = "S" - $80
ACK_DOWNLOAD_PACKET = "s" + $20
REQ_DOWNLOAD_CLOSE  = "E" - $80
ACK_DOWNLOAD_CLOSE  = "e" + $20
REQ_UPLOAD_OPEN     = "U" - $80
ACK_UPLOAD_OPEN     = "u" + $20
REQ_UPLOAD_PACKET   = "R" - $80
ACK_UPLOAD_PACKET   = "r" + $20
REQ_UPLOAD_CLOSE    = "V" - $80
ACK_UPLOAD_CLOSE    = "v" + $20
PROTOCOL_VERSION    = $01

DEFAULT_TIMEOUT     = 300  ;jiffies: 5 seconds

modemFd       = 2  ;(1)  ;fd for modem
uploadFd      = 3  ;(1)  ;fd for current file being uploaded
downloadFd    = 4  ;(1)  ;fd for current file being downloaded
debugMode     = 5  ;(1)  ;flag for debug mode active
oldterm       = 6  ;(1)  ;old ACE modem parameters
newterm       = 7  ;(1)  ;new ACE modem parameters
arg           = 8  ;(2)  ;current argument number
name          = 10 ;(2)  ;name of current file argument
progname      = 12 ;(2)  ;given name of program
filenameUsed  = 14 ;(1)  ;flag: if a filename has been encountered on cmd line
linkInited    = 15 ;(1)  ;flag: link initialized
crc           = 16 ;(4)  ;current crc accumulator
filelen       = 20 ;(4)  ;length of file being sent
getdatalen    = 24 ;(2)  ;data length
bufptr        = 26 ;(2)  ;buffer pointer
buflen        = 28 ;(2)  ;buffer length
blocklen      = 30 ;(2)  ;length of data block
uploadTextSize= 32 ;(2)  ;data size to use for uploading text files
uploadBinarySize=34;(2)  ;data size to use for uploading binary files
downloadTextSize=36;(2)  ;data size to use for downloading text files
downloadBinarySize=38;(2);data size to use for downloading binary files
uploadSeq     = 40 ;(1)  ;sequence number for uploading
downloadSeq   = 41 ;(1)  ;sequence number for downloading
textMode      = 42 ;(1)  ;text/binary mode for uploading
logFd         = 43 ;(1)  ;file descriptor of log file
logFlag       = 44 ;(1)  ;flag for log file active
downloadTextFlag=45;(1)  ;flag for downloading
putdatalen    = 46 ;(2)  ;length for PutPacket
modbufCount   = 48 ;(1)  ;number of active bytes in the modem-receive buffer
modbufPtr     = 49 ;(1)  ;pointer into modem-receive buffer
bufptrCrc     = 50 ;(2)  ;pointer to received data, minus 4
bufpCrcCdn    = 52 ;(1)  ;countdown to start using
timeoutCount  = 53 ;(1)  ;number of successive timeouts on current packet
jifTimeout    = 54 ;(4)  ;jiffy time to timeout
jifToPeriod   = 58 ;(2)  ;timeout period
channelWidth8 = 60 ;(1)  ;flag: $00=7 bits, $ff=8 bits
highBit       = 61 ;(1)  ;high bit for quoting 7-bit characters
statMsg       = 62 ;(2)  ;pointer to status message for RPC
ackPtr        = 64 ;(2)  ;ack pointer for RPC
kbFlag        = 66 ;(1)  ;flag to use 1K packet sizes
connected     = 67 ;(1)  ;whether we are connected to the server
work          = 112;(16) ;misc work area

;===main===

main = *
   ;** check for large enough TPA
   lda #$00
   sta linkInited
   sec
   lda #<bssEnd
   cmp aceMemTop+0
   lda #>bssEnd
   sbc aceMemTop+1
   bcs +
   jmp mainInit
+  lda #<tpaMsg
   ldy #>tpaMsg
   jsr eputs
die = *
   jsr LinkRestore
   lda #1
   ldx #0
   jmp aceProcExit

tpaMsg: .byte "fx: Insufficient program space to run program\n",0

usage = *
   lda #<usageMsg
   ldy #>usageMsg
   jsr eputs
   jmp die

usageMsg: .byte "usage: fx [-78k] [[-b] file ...] [-t file ...]\n\n"
          .byte "-7 : use 7-bit link\n"
          .byte "-8 : use 8-bit link (default)\n"
          .byte "-k : use 1K packet sizes\n"
          .byte "-t : announce text files\n"
          .byte "-b : announce binary files\n",0

mainInit = *
   ;** set globals
   lda #TRUE
   sta debugMode
   sta channelWidth8
   lda #FALSE
   sta textMode
   sta logFlag
   sta kbFlag
   sta connected
   lda #0
   ldy #0
   jsr getarg
   sta progname+0
   sty progname+1
   lda #FALSE
   sta filenameUsed
   lda #<DEFAULT_TIMEOUT
   ldy #>DEFAULT_TIMEOUT
   sta jifToPeriod+0
   sty jifToPeriod+1
   jsr GenPetAndAsc
   jsr CrcGen
   lda #0
   sta arg+0
   sta arg+1
   jsr ModemGetInit
   jsr LinkInit
   lda #<titleMsg
   ldy #>titleMsg
   jsr puts
   jmp mainNext
   titleMsg: .byte "Craig's File eXchange Protocol Client\n\n",0

   mainNext = *
   jsr CheckStop
   inc arg+0
   bne +
   inc arg+1
+  lda arg+0
   ldy arg+1
   jsr getarg
   beq mainExit
   sta name+0
   sty name+1
   ldy #0
   lda (zp),y
   cmp #"-"
   bne +
   jmp handleFlags
+  bit connected
   bmi +
   jsr ConnectToServer
   lda #TRUE
   sta connected
+  jsr UploadFile
   jmp mainNext

mainExit = *
   bit filenameUsed
   bmi +
   nop
+  bit connected
   bmi +
   jsr ConnectToServer
+  jsr DownloadFiles
   jsr DisconnectFromServer
   jsr LinkRestore
   rts

handleFlags = *
   iny
   lda (zp),y
   bne +
   jmp mainNext
+  cmp #"d"
   bne +
   lda #TRUE
   sta debugMode
   jmp handleFlags
+  cmp #"t"
   bne +
   lda #TRUE
   sta textMode
   jmp handleFlags
+  cmp #"b"
   bne +
   lda #FALSE
   sta textMode
   jmp handleFlags
+  cmp #"7"
   bne +
   lda #FALSE
   sta channelWidth8
   jmp handleFlags
+  cmp #"8"
   bne +
   lda #TRUE
   sta channelWidth8
   jmp handleFlags
+  cmp #"k"
   bne +
   lda #TRUE
   sta kbFlag
   jmp handleFlags
+  cmp #"l"
   beq +
   jmp usage
+  lda #TRUE
   sta logFlag
   ;xx open log file, preserving arg,Y
   jmp handleFlags

;===file-exchange level===

ackConnect = packet+3

ConnectToServer = *
   sec
   lda aceMemTop+0
   sbc #<packet+32
   bit kbFlag
   bpl +
   lda #<1024
+  sta uploadTextSize+0
   sta uploadBinarySize+0
   sta downloadTextSize+0
   sta downloadBinarySize+0
   lda aceMemTop+1
   sbc #>packet+32
   bit kbFlag
   bpl +
   lda #>1024
+  sta uploadTextSize+1
   sta uploadBinarySize+1
   sta downloadTextSize+1
   sta downloadBinarySize+1
   jsr ModemOpen
   lda #REQ_CONNECT
   sta packet+0
   lda #PROTOCOL_VERSION
   sta packet+1
   lda #"8"
   bit channelWidth8
   bmi +
   lda #"7"
+  sta packet+2
   lda #FALSE
   sta channelWidth8
   lda #<connectMsg1
   ldy #>connectMsg1
   sta statMsg+0
   sty statMsg+1
   lda #3
   ldy #0
   ldx #ACK_CONNECT
   jsr RpcSend
   ldx #FALSE
   lda ackConnect+3
   cmp #"7"
   beq +
   jsr LinkMake8
   ldx #TRUE
+  stx channelWidth8
   ldx #4
-  lda ackConnect+0,x
   ora ackConnect+1,x
   beq +
   lda #$00
   sta ackConnect+0,x
   sta ackConnect+1,x
   lda #$ff
   sta ackConnect+2,x
   sta ackConnect+3,x
+  inx
   inx
   inx
   inx
   cpx #20
   bcc -
   ldx ackConnect+4+3
   lda ackConnect+4+2
   cpx uploadTextSize+0
   sbc uploadTextSize+1
   bcs +
   stx uploadTextSize+0
   lda ackConnect+4+2
   sta uploadTextSize+1
+  ldx ackConnect+8+3
   lda ackConnect+8+2
   cpx uploadBinarySize+0
   sbc uploadBinarySize+1
   bcs +
   stx uploadBinarySize+0
   lda ackConnect+8+2
   sta uploadBinarySize+1
+  ldx ackConnect+12+3
   lda ackConnect+12+2
   cpx downloadTextSize+0
   sbc downloadTextSize+1
   bcs +
   stx downloadTextSize+0
   lda ackConnect+12+2
   sta downloadTextSize+1
+  ldx ackConnect+16+3
   lda ackConnect+16+2
   cpx downloadBinarySize+0
   sbc downloadBinarySize+1
   bcs +
   stx downloadBinarySize+0
   lda ackConnect+16+2
   sta downloadBinarySize+1
+  lda #<connectMsg2
   ldy #>connectMsg2
   jsr puts
   jsr ModemClose
   lda #$00
   sta work+2
   sta work+3
   lda #<connectMsg3
   ldy #>connectMsg3
   jsr puts
   lda uploadTextSize+0
   ldy uploadTextSize+1
   sta work+0
   sty work+1
   ldx #work+0
   jsr putnum
   lda #<connectMsg4
   ldy #>connectMsg4
   jsr puts
   lda uploadBinarySize+0
   ldy uploadBinarySize+1
   sta work+0
   sty work+1
   ldx #work+0
   jsr putnum
   lda #<connectMsg5
   ldy #>connectMsg5
   jsr puts
   lda downloadTextSize+0
   ldy downloadTextSize+1
   sta work+0
   sty work+1
   ldx #work+0
   jsr putnum
   lda #<connectMsg6
   ldy #>connectMsg6
   jsr puts
   lda downloadBinarySize+0
   ldy downloadBinarySize+1
   sta work+0
   sty work+1
   ldx #work+0
   jsr putnum
   lda #chrCR
   jsr putchar
   lda #<connectMsg7
   ldy #>connectMsg7
   bit channelWidth8
   bpl +
   lda #<connectMsg8
   ldy #>connectMsg8
+  jsr puts
   rts
   connectMsg1: .byte "Connecting to server...",0
   connectMsg2: .byte " connected\n",0
   connectMsg3: .byte "Packet sizes: ulTxt=",0
   connectMsg4: .byte ", ulBin=",0
   connectMsg5: .byte ", dlText=",0
   connectMsg6: .byte ", dlBin=",0
   connectMsg7: .byte "Using a 7-bit channel\n",0
   connectMsg8: .byte "Using an 8-bit channel\n",0

DisconnectFromServer = *
   jsr ModemOpen
   lda #REQ_DISCONNECT
   sta packet+0
   lda #<disconnectMsg1
   ldy #>disconnectMsg1
   sta statMsg+0
   sty statMsg+1
   lda #1
   ldy #0
   ldx #ACK_DISCONNECT
   jsr RpcSend
   bcs +
   lda #<disconnectMsg2
   ldy #>disconnectMsg2
   jsr puts
+  jsr ModemClose
   rts
   disconnectMsg1: .byte "Disconnecting from server...",0
   disconnectMsg2: .byte " disconnected\n",0
   rts

UploadFile = *  ;( (name)=nameStrPtr )
   lda #<uploadMsg1
   ldy #>uploadMsg1
   jsr puts
   lda name+0
   ldy name+1
   jsr puts
   lda #<uploadMsg2
   ldy #>uploadMsg2
   bit textMode
   bpl +
   lda #<uploadMsg3
   ldy #>uploadMsg3
+  jsr puts
   lda name+0
   ldy name+1
   sta zp+0
   sty zp+1
   lda #"r"
   jsr open
   bcc +
   lda errno
   sta work+0
   lda #0
   sta work+1
   sta work+2
   sta work+3
   lda #<uploadOpenErrMsg
   ldy #>uploadOpenErrMsg
   jsr puts
   ldx #work+0
   jsr putnum
   lda #chrCR
   jsr putchar
   rts
+  sta uploadFd
   ldx #3
   lda #0
-  sta filelen,x
   dex
   bpl -
   jsr UploadOpen
   lda #$ff
   sta uploadSeq
   jsr UploadFileBody
   jsr ClearStatLine
   jsr UploadClose
   lda uploadFd
   jsr close
   rts
   uploadMsg1: .byte 'Uploading file "',0
   uploadMsg2: .byte '" (binary)\n',0
   uploadMsg3: .byte '" (text)\n',0
   uploadOpenErrMsg: .byte "Could not open file, skipping: err=",0

ClearStatLine = *
   lda #<clearMsg
   ldy #>clearMsg
   jmp puts
   clearMsg: .byte "\r                   \r",0

UploadOpen = *
   jsr ModemOpen
   lda #REQ_UPLOAD_OPEN
   sta packet+0
   bit textMode
   bmi +
   lda #"b"+$20
   sta packet+1
   jmp ++
+  lda #"t"+$20
   sta packet+1
+  lda #$00
   ldx #2
-  sta packet+0,x
   inx
   cpx #20
   bcc -
   ldy #0
-  lda (name),y
   php
   tax
   lda petToAsc,x
   sta packet+20,y
   plp
   beq +
   iny
   bne -
+  cpy #2
   bcc +
   lda packet+20-2,y
   cmp #","
   bne +
   lda #0
   sta packet+20-2,y
   dey
   dey
+  iny
   tya
   clc
   adc #20
   ldy #0
   ldx #<upOpMsg1
   stx statMsg+0
   ldx #>upOpMsg1
   stx statMsg+1
   ldx #ACK_UPLOAD_OPEN
   jsr RpcSend
   jsr ClearStatLine
   jsr SetBytes
   jsr puts
   ldy #1
   lda (ackPtr),y
   cmp #"y"+$20
   bne +
   jsr ModemClose
   clc
   rts
+  lda #<upOpMsg2
   ldy #>upOpMsg2
   jsr puts
   jsr ModemClose
   sec
   rts
   upOpMsg1: .byte "\ropening remote file",0
   upOpMsg2: .byte "Could not open remote file, skipping.\n",0

UploadClose = *
   jsr ModemOpen
   lda #REQ_UPLOAD_CLOSE
   sta packet+0
   lda #<upClMsg1
   ldy #>upClMsg1
   sta statMsg+0
   sty statMsg+1
   lda #1
   ldy #0
   ldx #ACK_UPLOAD_CLOSE
   jsr RpcSend
   jsr ClearStatLine
   jsr ModemClose
   rts
   upClMsg1: .byte "\rclosing remote file",0

UploadFileBody = *
   inc uploadSeq
   lda #<packet+6
   ldy #>packet+6
   sta zp+0
   sty zp+1
   bit textMode
   bpl +
   lda uploadTextSize+0
   ldy uploadTextSize+1
   jmp ++
+  lda uploadBinarySize+0
   ldy uploadBinarySize+1
+  ldx uploadFd
   jsr read
   bne +
   jsr SetBytes
   jsr puts
   rts
+  sta blocklen+0
   sty blocklen+1
   sta buflen+0
   sty buflen+1
   ldx #$00
   stx packet+2  ;h/m
   stx packet+3
   sty packet+4  ;m/l
   sta packet+5
   clc
   adc #6
   bcc +
   iny
+  sta putdatalen+0
   sty putdatalen+1
   lda #REQ_UPLOAD_PACKET
   sta packet+0
   lda uploadSeq
   sta packet+1
   jsr ModemOpen
   bit textMode
   bpl +
   lda #<packet+6
   ldy #>packet+6
   sta bufptr+0
   sty bufptr+1
   lda #$00
   jsr BufTrText
+- jsr SetBytes
   sta statMsg+0
   sty statMsg+1
   lda putdatalen+0
   ldy putdatalen+1
   ldx #ACK_UPLOAD_PACKET
   jsr RpcSend
   ldy #1
   lda (ackPtr),y
   cmp uploadSeq
   bne -
   jsr ModemClose
   lda blocklen+0
   ldy blocklen+1
   jsr AddToFileLen
   jmp UploadFileBody

SetBytes = *
   lda #<uploadMsgN
   ldy #>uploadMsgN
   sta zp+0
   sty zp+1
   lda #1
   ldx #filelen
   jsr aceMiscUtoa
   lda #<uploadMsgS
   ldy #>uploadMsgS
   rts
   uploadMsgS: .byte "\rbytes="
   uploadMsgN: .buf 11

AddToFileLen = *
   clc
   adc filelen+0
   sta filelen+0
   tya
   adc filelen+1
   sta filelen+1
   bcc +
   inc filelen+2
   bne +
   inc filelen+3
+  rts

DownloadFiles = *
   lda #<downloadMsg1
   ldy #>downloadMsg1
   jsr puts
   jsr DownloadOpen
   bcc +
   lda #<downloadMsg2
   ldy #>downloadMsg2
   jsr puts
   rts
+  pha
   lda #<downloadMsg3
   ldy #>downloadMsg3
   jsr puts
   pla
   downloadNext = *
   cmp #$00
   beq downloadOkay
   pha
   lda #<downloadMsgE0
   ldy #>downloadMsgE0
   jsr puts
   lda name+0
   ldy name+1
   jsr puts
   pla
   cmp #$01
   bne +
   lda #<downloadMsgE1
   ldy #>downloadMsgE1
   jsr puts
   jmp downloadClose
+  cmp #$02
   bne +
   lda #<downloadMsgE2
   ldy #>downloadMsgE2
   jsr puts
   jmp downloadClose
+  cmp #$03
   bne downloadOkay
   lda #<downloadMsgE3
   ldy #>downloadMsgE3
   jsr puts
   jmp downloadClose

   downloadOkay = *
   lda #<downloadMsg4
   ldy #>downloadMsg4
   jsr puts
   lda name+0
   ldy name+1
   jsr puts
   lda #<downloadMsg5
   ldy #>downloadMsg5
   jsr puts
   ldx #3
   ldy #0
-  lda dlOpenAck+2,y
   sta work+0,x
   lda #$00
   sta filelen,x
   iny
   dex
   bpl -
   ldx #work+0
   jsr putnum
   lda #<downloadMsg6
   ldy #>downloadMsg6
   jsr puts
   lda #<downloadMsg7
   ldy #>downloadMsg7
   bit downloadTextFlag
   bpl +
   lda #<downloadMsg8
   ldy #>downloadMsg8
+  jsr puts
   jsr DownloadBody

   downloadClose = *
   jsr ClearStatLine
   jsr DownloadClose
   jsr ClearStatLine
   jsr DownloadOpen
   bcs +
   jmp downloadNext
+  lda #<downloadMsg9
   ldy #>downloadMsg9
   jsr puts
   rts
   downloadMsg1:  .byte "Checking for files to download...",0
   downloadMsg2:  .byte " none\n",0
   downloadMsg3:  .byte " present\n",0
   downloadMsg4:  .byte 'Downloading "',0
   downloadMsg5:  .byte '" (',0
   downloadMsg6:  .byte " bytes, ",0
   downloadMsg7:  .byte "binary)\n",0
   downloadMsg8:  .byte "text)\n",0
   downloadMsg9:  .byte "Finished downloading\n",0
   downloadMsgE0: .byte 'Cannot download "',0
   downloadMsgE1: .byte '": error opening remote file\n',0
   downloadMsgE2: .byte '": name refers to a directory\n',0
   downloadMsgE3: .byte '": error opening local file\n',0
   nullStr:       .byte 0

dlOpenAck = packet+1

DownloadOpen = *  ;( ) : .CS=noMore, .A=errcode, (name)=tempFilename
   ;** errcode: $00=ok, $01=remoteError, $02=directory, $03=localError
   lda #$ff
   sta downloadSeq
   sta downloadFd
   jsr ModemOpen
   lda #REQ_DOWNLOAD_OPEN
   sta packet+0
   lda #<nullStr
   ldy #>nullStr
   sta statMsg+0
   sty statMsg+1
   lda #1
   ldy #0
   ldx #ACK_DOWNLOAD_OPEN
   jsr RpcSend
   jsr ModemClose
   lda dlOpenAck+1
   cmp #"0"
   bne +
   sec
   rts
+  ldx #FALSE
   cmp #"b"+$20
   beq +
   ldx #TRUE
+  stx downloadTextFlag
   ldy #0
-  lda dlOpenAck+20,y
   tax
   lda ascToPet,x
   sta dlOpenAck+20,y
   beq +
   iny
   bne -
+  bit downloadTextFlag
   bmi +
   lda #","
   sta dlOpenAck+20+0,y
   lda #"p"
   sta dlOpenAck+20+1,y
   lda #0
   sta dlOpenAck+20+2,y
+  lda #<dlOpenAck+20
   ldy #>dlOpenAck+20
   sta zp+0
   sty zp+1
   sta name+0
   sty name+1
   lda dlOpenAck+1
   cmp #"e"+$20
   bne +
   lda #$01
   clc
   rts
+  cmp #"d"+$20
   bne +
   lda #$02
   clc
   rts
+  lda #"W"
   jsr open
   bcc +
   lda #$03
   clc
   rts
+  sta downloadFd
   lda #$00
   clc
   rts

dlCloseAck = packet+1

DownloadClose = *
   jsr ModemOpen
   lda #REQ_DOWNLOAD_CLOSE
   sta packet+0
   lda #<dlCloseMsg
   ldy #>dlCloseMsg
   sta statMsg+0
   sty statMsg+1
   lda #1
   ldy #0
   ldx #ACK_DOWNLOAD_CLOSE
   jsr RpcSend
   jsr ModemClose
   lda downloadFd
   cmp #$ff
   beq +
   jsr close
+  jsr ClearStatLine
   rts
   dlCloseMsg: .byte "\rclosing files",0

dlPacketAck = packet+6

DownloadBody = *
   inc downloadSeq
   jsr ModemOpen
   lda #REQ_DOWNLOAD_PACKET
   sta packet+0
   lda downloadSeq
   sta packet+1
   lda downloadTextSize+0
   ldy downloadTextSize+1
   bit downloadTextFlag
   bmi +
   lda downloadBinarySize+0
   ldy downloadBinarySize+1
+  sta packet+5
   sty packet+4
   ldx #$00
   stx packet+2
   stx packet+3
   jsr SetBytes
   sta statMsg+0
   sty statMsg+1
   lda #6
   ldy #0
   ldx #ACK_DOWNLOAD_PACKET
   jsr RpcSend
   jsr ModemClose
   lda dlPacketAck+4
   ora dlPacketAck+5
   bne +
   rts
+  lda dlPacketAck+5
   ldy dlPacketAck+4
   sta blocklen+0
   sty blocklen+1
   sta buflen+0
   sty buflen+1
   bit downloadTextFlag
   bpl +
   lda #<dlPacketAck+6
   ldy #>dlPacketAck+6
   sta bufptr+0
   sty bufptr+1
   lda #$ff
   jsr BufTrText
+  lda #<dlPacketAck+6
   ldy #>dlPacketAck+6
   sta zp+0
   sty zp+1
   lda blocklen+0
   ldy blocklen+1
   ldx downloadFd
   jsr write
   lda blocklen+0
   ldy blocklen+1
   jsr AddToFileLen
   jmp DownloadBody

;===charset translation===

bufTrType: .buf 1

BufTrText = *  ;( (bufptr)++=text, (buflen)--=len, .A=trType:$00=pet2asc )
   sta bufTrType
   lda buflen+1
   beq +
-  ldy #0
   jsr BufTrPage
   inc bufptr+1
   dec buflen+1
   bne -
+  ldy buflen+0
   beq +
   jsr BufTrPage
+  rts

BufTrPage = *  ;( (bufptr)=text, .Y=len )
   sty work+0
   ldy #0
   bit bufTrType
   bmi +
-  lda (bufptr),y
   tax
   lda petToAsc,x
   sta (bufptr),y
   iny
   cpy work+0
   bne -
   rts
+- lda (bufptr),y
   tax
   lda ascToPet,x
   sta (bufptr),y
   iny
   cpy work+0
   bne -
   rts

GenPetAndAsc = *
   ldy #0
   sty work+0
-  lda petToAscGenTable,y
   iny
   ldx petToAscGenTable,y
   beq +
   iny
   sty work+1
   ldy work+0
-  sta petToAsc,y
   clc
   adc #1
   iny
   dex
   bne -
   sty work+0
   ldy work+1
   jmp --
+  ldy #0
-  ldx petToAsc,y
   tya
   sta ascToPet,x
   iny
   bne -
   rts

petToAscGenTable = *
   .byte $00,8, $14,1, $09,1, $0d,1, $11,1, $93,1, $0a,1, $0e,3, $0b,1
   .byte $12,2, $08,1, $15,44,$61,26,$5b,5, $c0,32,$80,19,$0c,1, $94,44
   .byte $60,1, $41,26,$7b,5, $e0,32,$00,0
   
;===transport layer===

rpcAckType: .buf 1

RpcSend = *  ;( packet, .AY=len, .X=ackType ) : ack, .CS=okay, blocklen
   ;** also arg: statMsg
   sta putdatalen+0
   sty putdatalen+1
   stx rpcAckType
   lda #0
   sta timeoutCount
   clc
   lda #<packet
   adc putdatalen+0
   sta work+0
   lda #>packet
   adc putdatalen+1
   tay
   lda work+0
   sta ackPtr+0
   sty ackPtr+1

   rpcAgain = *
   lda statMsg+0
   ldy statMsg+1
   jsr puts
   jsr PutPacket
-  lda ackPtr+0
   ldy ackPtr+1
   jsr GetPacket
   bcs +
   ldy #0
   lda (ackPtr),y
   cmp rpcAckType
   bne -
   clc
   rts
+  inc timeoutCount
   lda rpcAckType
   cmp #ACK_DISCONNECT
   beq discFail
   lda #<rpcToMsg
   ldy #>rpcToMsg
   jsr puts
   lda timeoutCount
   sta work+0
   lda #$00
   ldx #2
-  sta work+1,x
   dex
   bpl -
   ldx #work+0
   jsr putnum
   lda #chrCR
   jsr putchar
   lda timeoutCount
   cmp #20
   bcs +
   jmp rpcAgain
+  lda #<rpcToMsg2
   ldy #>rpcToMsg2
   jsr puts
   jmp die

   discFail = *
   lda #<rpcDiscFail
   ldy #>rpcDiscFail
   jsr puts
   sec
   rts

   rpcToMsg:   .byte " -- timeout #",0
   rpcToMsg2:  .byte "Too many timeouts in a row: aborting.\n",0
   rpcDiscFail:.byte " -- timeout:\n"
               .byte "Disconnect failed, send Ctrl-X's to abort server\n",0

PutPacket = *  ;( packet, putdatalen )
   jsr CrcInit
   lda #<packet
   ldy #>packet
   sta bufptr+0
   sty bufptr+1
   lda putdatalen+0
   ldy putdatalen+1
   sta buflen+0
   sty buflen+1
   lda #CHR_START
   jsr PutByteLiteral

   putPackNext = *
   lda buflen+0
   ora buflen+1
   beq putPackFinish
   ldy #0
   lda (bufptr),y
   jsr PutByte
   ldy #0
   lda (bufptr),y
   jsr CrcByte
jmp +
lda #"."
jsr putchar
ldy #0
lda (bufptr),y
jsr puthex
+
   inc bufptr+0
   bne +
   inc bufptr+1
+  lda buflen+0
   bne +
   dec buflen+1
+  dec buflen+0
   jmp putPackNext

   putPackFinish = *
   jsr CrcFinish
   lda crc+3
   jsr PutByte
   lda crc+2
   jsr PutByte
   lda crc+1
   jsr PutByte
   lda crc+0
   jsr PutByte
   lda #CHR_END
   jsr PutByteLiteral
jmp +
lda #":"
jsr putchar
lda crc+3
jsr puthex
lda #","
jsr putchar
lda crc+2
jsr puthex
lda #","
jsr putchar
lda crc+1
jsr puthex
lda #","
jsr putchar
lda crc+0
jsr puthex
jsr CrcVerify
+
   rts

PutByte = *  ;( .A=byte )
   bit channelWidth8
   bmi +
   cmp #$80
   bcc +
   pha
   lda #CHR_QUOTE8
   jsr PutByteLiteral
   pla
   and #$7f
+  cmp #CHR_START
   beq putByteEsc
   cmp #CHR_END
   beq putByteEsc
   cmp #CHR_ESC
   beq putByteEsc
   cmp #CHR_ABORT
   beq putByteEsc
   cmp #CHR_XON
   beq putByteEsc
   cmp #CHR_XOFF
   beq putByteEsc
   cmp #CHR_QUOTE8
   beq putByteEsc
   jmp PutByteLiteral
   putByteEsc = *
   pha
   lda #CHR_ESC
   jsr PutByteLiteral
   pla
   ora #$40
   jsr PutByteLiteral
   rts

PutByteLiteral = *  ;( .A=byte )
   sta putByteVal
   lda #<putByteVal
   ldy #>putByteVal
   sta zp+0
   sty zp+1
   lda #1
   ldy #0
   ldx modemFd
   jmp write
   putByteVal: .buf 1

getPackPtr: .buf 2

GetPacket = *  ; ( .AY=ackptr ) : .CS=timeout
   sta getPackPtr+0
   sty getPackPtr+1
   getNewPacket = *
-  jsr GetByte
   bcc +
   rts
+  cmp #CHR_START
   bne -
   getNewPacketIn = *
   lda #0
   sta getdatalen+0
   sta getdatalen+1
   lda getPackPtr+0
   ldy getPackPtr+1
   sta bufptr+0
   sty bufptr+1
   sta bufptrCrc+0
   sty bufptrCrc+1
   lda #4
   sta bufpCrcCdn
   jsr CrcInit

   getMoreBytes = *
   lda #$00
   sta highBit
   jsr GetByte
   bcc +
   rts
+  cmp #CHR_QUOTE8
   bne +
   lda #$80
   sta highBit
   jsr GetByte
   bcc +
   rts
+  cmp #CHR_START
   bne +
   jmp getNewPacketIn
+  cmp #CHR_END
   bne +
   jmp getPacketEnd
+  cmp #CHR_ESC
   bne +++
   jsr GetByte
   bcc +
   rts
+  cmp #$3f
   bne +
   lda #$7f
   jmp ++
+  cmp #$40
   bcc getNewPacket
   cmp #$5f+1
   bcs getNewPacket
   and #$1f
+  ldy #0
   ora highBit
   sta (bufptr),y
   inc bufptr+0
   bne +
   inc bufptr+1
+  inc getdatalen+0
   bne +
   inc getdatalen+1
+  lda bufpCrcCdn
   beq +
   dec bufpCrcCdn
   jmp getMoreBytes
+  lda (bufptrCrc),y
   jsr CrcByte
   inc bufptrCrc+0
   bne +
   inc bufptrCrc+1
+  jmp getMoreBytes

   getPacketEnd = *
   jsr CrcFinish
   lda getdatalen+1
   bne +
   lda getdatalen+0
   cmp #4
   bcs +
   jmp getNewPacket
   ;** cut the CRC-32 value off of what was received
+  ldx #4
-  lda bufptr+0
   bne +
   dec bufptr+1
+  dec bufptr+0
   lda getdatalen+0
   bne +
   dec getdatalen+1
+  dec getdatalen+0
   dex
   bne -
   ;** compare the CRC-32 values
   ldy #0
   ldx #3
-  lda crc,x
   cmp (bufptr),y
   bne +
   iny
   dex
   bpl -
   clc
   rts

GetByte = *  ;( ) : .A=data
   jsr CheckStop
   jsr ModemGetByte
   rts

;** crc = 0xFFFFFFFF;
;** while( (c=getc(fp)) != EOF ) {
;**     crc = (crc>>8) & 0x00FFFFFF ^ crcTable[ (crc^c) & 0xFF ];
;** }
;** return( crc^0xFFFFFFFF );

CrcInit = *
   ldx #3
-  lda #$ff
   sta crc,x
   dex
   bpl -
   rts

CrcByte = *  ;( .A=byte )
   eor crc+0            ;.X = (crc^c) & 0xFF
   tax
   lda crc+1            ;crc = (crc>>8) & 0x00FFFFFF ^ crcTable[ .X ]
   eor crcTable0,x
   sta crc+0
   lda crc+2
   eor crcTable1,x
   sta crc+1
   lda crc+3
   eor crcTable2,x
   sta crc+2
   lda crcTable3,x
   sta crc+3
   rts

CrcFinish = *
   ldx #3
-  lda crc,x
   eor #$ff
   sta crc,x
   dex
   bpl -
   rts

;** poly = 0xEDB88320L;
;** for (i=0; i<256; i++) {
;**     crc = i;
;**     for (j=8; j>0; j--) {
;**         if (crc&1) {
;**             crc = (crc >> 1) ^ poly;
;**         } else {
;**             crc >>= 1;
;**         }
;**     }
;**     crcTable[i] = crc;
;** }

CrcGen = *
   ;** generate CRC table at runtime
   ldy #0
-  ldx #0
   sty crc+0
   stx crc+1
   stx crc+2
   stx crc+3

   ldx #8
-  lsr crc+3
   ror crc+2
   ror crc+1
   ror crc+0
   bcc +
   lda crc+0
   eor #$20
   sta crc+0
   lda crc+1
   eor #$83
   sta crc+1
   lda crc+2
   eor #$B8
   sta crc+2
   lda crc+3
   eor #$ED
   sta crc+3
+  dex
   bne -

   lda crc+0
   sta crcTable0,y
   lda crc+1
   sta crcTable1,y
   lda crc+2
   sta crcTable2,y
   lda crc+3
   sta crcTable3,y
   iny
   bne --
   rts

CrcVerify = *
   ;** verify correctness of CRC table at runtime
   ldy #0
-  ldx #0
   sty crc+0
   stx crc+1
   stx crc+2
   stx crc+3

   ldx #8
-  lsr crc+3
   ror crc+2
   ror crc+1
   ror crc+0
   bcc +
   lda crc+0
   eor #$20
   sta crc+0
   lda crc+1
   eor #$83
   sta crc+1
   lda crc+2
   eor #$B8
   sta crc+2
   lda crc+3
   eor #$ED
   sta crc+3
+  dex
   bne -

   lda crc+0
   cmp crcTable0,y
   bne +
   lda crc+1
   cmp crcTable1,y
   bne +
   lda crc+2
   cmp crcTable2,y
   bne +
   lda crc+3
   cmp crcTable3,y
   bne +
   iny
   bne --
   rts
+  lda #"V"
   jsr putchar
   jmp aceProcExit

LinkInit = *
   ldx #$00
   jsr aceModemParms
   sta oldterm
   rts

LinkMake8 = *
   lda oldterm
   and #$7f
   ldx #$ff
   jmp aceModemParms

LinkRestore = *
   ldx #$ff
   lda oldterm
   jsr aceModemParms
   rts

ModemOpen = *
   ;** open modem file
   lda #<modemFilename
   ldy #>modemFilename
   sta zp+0
   sty zp+1
   lda #"w"
   jsr open
   bcc +
   lda #<modemFilenameErr
   ldy #>modemFilenameErr
   jsr eputs
   jmp die
+  sta modemFd
   rts

   modemFilename: .byte "x:",0
   modemFilenameErr: .byte 'Cannot open "x:" file (modem)\n',0

ModemClose = *
   lda modemFd
   jmp close

ModemGetInit = *
   lda #0
   sta modbufCount
   sta modbufPtr
   rts

ModemGetByte = *  ;.CC=okay, .CS=timed_out
   lda modbufCount
   beq +
   ldx modbufPtr
   lda readbuf,x
   dec modbufCount
   inc modbufPtr
   clc
   rts
+  ldx #jifTimeout
   jsr aceTimeJif
   clc
   lda jifTimeout+0
   adc jifToPeriod+0
   sta jifTimeout+0
   lda jifTimeout+1
   adc jifToPeriod+1
   sta jifTimeout+1
   bcc +
   inc jifTimeout+2
   bne +
   inc jifTimeout+3
+- jsr CheckStop
   ldx #work+0
   jsr aceTimeJif
   sec
   ldx #0
   ldy #4
-  lda work+0,x
   sbc jifTimeout,x
   inx
   dey
   bne -
   bcc +
   sec
   rts
+  jsr aceModemCheck
   beq --
   lda #<readbuf
   ldy #>readbuf
   sta zp+0
   sty zp+1
   lda #255
   ldy #0
   ldx modemFd
   jsr read
   sta modbufCount
   lda #0
   sta modbufPtr
   jmp ModemGetByte

CheckStop = *
   jsr aceConStopkey
   bcs +
   rts
+  lda #<stoppedMsg
   ldy #>stoppedMsg
   jsr eputs
   jmp die
   stoppedMsg: .byte "<Stopped>\n",0

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
   putcBuffer: .buf 1

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
   php
   lda zp+0
   ldy zp+1
   plp
   rts

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

putnum = *
   lda #<numbuf
   ldy #>numbuf
   sta zp+0
   sty zp+1
   lda #1
   jsr aceMiscUtoa
   lda #0
   sta numbuf,y
   lda zp+0
   ldy zp+1
   jmp puts
   numbuf: .buf 13

;===bss===

bss        = *
ascToPet   = bss+0
petToAsc   = ascToPet+256
crcTable0  = petToAsc+256
crcTable1  = crcTable0+256
crcTable2  = crcTable1+256
crcTable3  = crcTable2+256
readbuf    = crcTable3+256
packet     = readbuf+256
bssEnd     = packet+32+1024
