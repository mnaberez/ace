;===ace system-interface declarations===

aceStatB      = $f00   ;(256)
aceCallB      = $1303  ;(225)
aceExitData   = $700   ;(256)
aceAppAddress = $6d00

zp      = $f8  ;(2)
zw      = $fa  ;(2)
mp      = $fc  ;(4)
syswork = $80  ;(16)

errno            = aceStatB+0          ;(1)
aceArgc          = aceStatB+4          ;(2)
aceArgv          = aceStatB+6          ;(2)
aceMemTop        = aceStatB+8          ;(2)
aceDirentBuffer  = aceStatB+10         ;(aceDirentLength)
aceDirentBytes   = aceDirentBuffer+0   ;(4)
aceDirentDate    = aceDirentBuffer+4   ;(8) = YY:YY:MM:DD:HH:MM:SS:TW
aceDirentType    = aceDirentBuffer+12  ;(4)
aceDirentFlags   = aceDirentBuffer+16  ;(1) = drwx*e-t
aceDirentUsage   = aceDirentBuffer+17  ;(1) = ulshb---
aceDirentNameLen = aceDirentBuffer+18  ;(1)
aceDirentName    = aceDirentBuffer+19  ;(17)
aceDirentLength  = 36

open          = aceCallB+0   ;( (zp)=name, .A=mode[rwaWA] ) : .A=fd
close         = aceCallB+3   ;( .A=fd )
read          = aceCallB+6   ;( .X=fd, (zp)=buf, .AY=len ) : .AY=(zw)=len, .Z
write         = aceCallB+9   ;( .X=fd, (zp)=buf, .AY=len )
aceFileLseek  = aceCallB+12  ;( .X=fd, .A=origin, [sw+0]=pos ) : [sw+0]=newpos
aceFileBload  = aceCallB+15  ;( (zp)=name, .AY=loadAddr, (zw)=limit+1):.AY=end+1
aceFileRemove = aceCallB+18  ;( (zp)=name )
aceFileRename = aceCallB+21  ;( (zp)=oldName, (zw)=newName )
aceFileInfo   = aceCallB+24  ;( .X=fd, .A=flags ) : .A=devType, .X=cols, .Y=rows
aceFileIoctl  = aceCallB+27  ;( .Y=opcode, ... ) : ...
aceFileSelect = aceCallB+30  ;( .A=conFlags, .X=fdCount, sw=... ) : .A=fl,.X=fd
aceFileBlock  = aceCallB+33  ;( .A=cmd, [sw+0]=blockNum, (zp)=dataPtr )

aceDirOpen    = aceCallB+36  ;( (zp)=dirName ) : .A=fd
aceDirClose   = aceCallB+39  ;( .A=fd )
aceDirRead    = aceCallB+42  ;( .X=fd ) : direntBuffer, .Z=eof
aceDirIsdir   = aceCallB+45  ;( (zp)=name ) : .A=dev, .X=isDisk, .Y=isDir
aceDirChange  = aceCallB+48  ;( (zp)=dirName, .A=flags($80=home) )
aceDirMake    = aceCallB+51  ;( (zp)=newDirName, .AY=suggestedEntries )
aceDirRemove  = aceCallB+54  ;( (zp)=dirName )
aceDirName    = aceCallB+57  ;( .A=sysdir, (zp)=buf ) : buf, .Y=len
                             ; .A:0=curDir, 1=homeDir, 2=execSearchPath,
                             ;    3=configSearchPath, 4=tempDir

aceWinScreen = aceCallB+60  ;( .A=MinRows, .X=MinCols )
aceWinMax    = aceCallB+63  ;( )
aceWinSet    = aceCallB+66  ;( .A=rows, .X=cols, sw+0=scrRow, sw+1=scrCol )
aceWinSize   = aceCallB+69  ;( ) : <above>+ ,(sw+2)=addr,(sw+4)=rowinc
aceWinCls    = aceCallB+72  ;( .A=char/color/attrFlags, .X=char, .Y=color )
aceWinPos    = aceCallB+75  ;( .A=row, .X=col ) : (sw+0)=addr
aceWinPut    = aceCallB+78  ;( .A=attr,.Y=color,.X=len,(sw+0)=addr,(sw+2)=chPtr,
                            ;  sw+4=fillChar, sw+5=fieldLen, sw+6=extattr )
aceWinGet    = aceCallB+81  ;( .A=attr, .X=len, (sw+0)=scr, (sw+2)=charPtr,
                            ;  (sw+4)=colorPtr, (sw+6)=attrPtr )
aceWinScroll = aceCallB+84  ;( .A=attr+$08:up+$04:dn,.X=rows,sw+4=chr,.Y=color)
aceWinCursor = aceCallB+87  ;( (sw+0)=addr, .Y=color, .A=$ff:on/$00:off)
aceWinPalette = aceCallB+90 ;( ) : sw+0...sw+7=palette [8 colors]
aceWinChrset = aceCallB+93  ;( (sw+0)=addr,.A=flags,.X=start,.Y=len):.A=flags
aceWinOption = aceCallB+96  ;( .X=op, .A=arg, .CS=set ) : .A=return

aceConWrite    = aceCallB+99  ;( (zp)=Buf, .AY=Len, .X=prescroll ) : .X=scroll
aceConPutlit   = aceCallB+102 ;( .A=char )
aceConPos      = aceCallB+105 ;( .A=row, .X=col )
aceConGetpos   = aceCallB+108 ;( ) : .A=rowOfCursor, .X=colOfCursor
aceConInput    = aceCallB+111 ;( (zp)=buf/initstr,.Y=initStrLen):.Y=len,.CS=excp
aceConStopkey  = aceCallB+114 ;( ) : .CC=notPressed
aceConGetkey   = aceCallB+117 ;( ) : .A=key
aceConKeyAvail = aceCallB+120 ;( ) : .CC=keyIsAvailable, .A=keyboardType
aceConKeyMat   = aceCallB+123 ;( (zp)=keymatrixPtr )
aceConMouse    = aceCallB+126 ;( ) : .A=buttons:l/r:128/64, (sw+0)=X, (sw+2)=Y
aceConJoystick = aceCallB+129 ;( ) : .A=joy1, .X=joy2
aceConOption   = aceCallB+132 ;( .X=op, .A=arg, .CS=set ) : .A=return

aceGrScreen = aceCallB+135 ;( .A=grType, .X=borderColor, .Y=BgFgColor )
                           ;  : .A=cols8, (sw+0)=rows, .X=xAspect
aceGrExit   = aceCallB+138 ;( )
aceGrFill   = aceCallB+141 ;( .A=fillValue )
aceGrOp     = aceCallB+144 ;( .A=flags, .X=X8, (sw+0)=Y, .Y=cols8, (sw+2)=rows,
                           ; (sw+4)=sPtr/val,(sw+6)=dPtr,(sw+8)=mPtr,sw+a=intl8)

aceProcExec = aceCallB+147 ;( (zp)=execName, (zw)=argv,.AY=argCnt,[mp]=saveArea,
                           ; .X=reftch):.A=exitCode,.X=exitDataLen,[mp]=saveArea
aceProcExecSub = aceCallB+150 ;( (zp)=execAddr, ...) rest same as aceProcExec
                              ;  : .A=exitCode, .X=exitDataLen, [mp]=saveArea
aceProcExit    = aceCallB+153 ;( .A=exitCode, .X=exitBufDataLen, exitData )

aceMemZpload  = aceCallB+156 ;( [mp]=Source, .X=ZpDest, .Y=Length )
aceMemZpstore = aceCallB+159 ;( .X=ZpSource, [mp]=Dest, .Y=Length )
aceMemFetch   = aceCallB+162 ;( [mp]=FarSource, (zp)=Ram0Dest, .AY=Length )
aceMemStash   = aceCallB+165 ;( (zp)=Ram0Source, [mp]=FarDest, .AY=length )
aceMemAlloc   = aceCallB+168 ;( .A=PageCount, .X=StartTyp,.Y=EndTyp):[mp]=FarPtr
aceMemFree    = aceCallB+171 ;( [mp]=FarPointer, .A=PageCount )
aceMemStat    = aceCallB+174 ;( .X=zpOff) : .A=procID, [.X+0]=free, [.X+4]=total

aceTimeGetDate = aceCallB+177 ;( (.AY)=dateString ) : dateString
aceTimeSetDate = aceCallB+180 ;( (.AY)=dateString )
aceTimeJif     = aceCallB+183 ;( .X=zpJifBuf ) : [zJB]=jiffies

aceMiscUtoa      = aceCallB+186 ;( $0+X=value32,(sw+0)=buf,.A=minLen):buf,.Y=len
aceMiscIoPeek    = aceCallB+189 ;( (zw)=ioaddr, .Y=offset ) : .A=data
aceMiscIoPoke    = aceCallB+192 ;( (zw)=ioaddr, .Y=offset, .A=data )

;*** discontinued ***

aceFileFdswap    = aceCallB+195 ;( .X=fd1, .Y=fd2 )
aceConRead       = aceCallB+198 ;( (zp)=Buf, .AY=Len ) : .AY=(zw)=Len, .Z
aceConPutchar    = aceCallB+201 ;( .A=char )
aceConPutctrl    = aceCallB+204 ;( .A=char, .X=aux )
aceMiscCmdOpen   = aceCallB+207 ;( (zp)=DevName ) : .A=fd
aceMiscCmdClose  = aceCallB+210 ;( .A=fd )
aceMiscCmdSend   = aceCallB+213 ;( .X=fd, (zp)=cmdString, .Y=cmdLen )
aceMiscCmdStatus = aceCallB+216 ;( .X=fd, (zp)=statBufPtr):statBuf,.A=statusCode
aceModemCheck    = aceCallB+219 ;( ):.AY=outstRecvBytes,.X=outstSendBytes,
                                ; .Z=oRecvB==0
aceModemParms    = aceCallB+222 ;( .A=baudrate|format )
aceModemStat     = aceCallB+225 ;( .X=zpStatBuf ):.A=slSt,[zSB+0]=ovrChr,
                                ; [zSB+4]=bovfChr
aceTest          = aceCallB+228 ;( (zp)=filename ) : .A=dev#, (zw)=devRelName

aceID1 = $cb
aceID2 = $06
aceID3 = 16

aceMemNull     = $00
aceMemREU      = $01
aceMemRL       = $02
aceMemInternal = $03
aceMemRdREU    = $04
aceMemRdRL     = $05
aceMemRdInternal = $06

aceErrStopped = 0
aceErrTooManyFiles = 1
aceErrFileOpen = 2
aceErrFileNotOpen = 3
aceErrFileNotFound = 4
aceErrDeviceNotPresent = 5
aceErrFileNotInput = 6
aceErrFileNotOutput = 7
aceErrMissingFilename = 8
aceErrIllegalDevice = 9
aceErrWriteProtect = 26
aceErrFileExists = 63
aceErrFileTypeMismatch = 64
aceErrNoChannel = 70
aceErrDiskFull = 72
aceErrInsufficientMemory = 128
aceErrOpenDirectory = 129
aceErrDiskOnlyOperation = 131
aceErrNullPointer = 132
aceErrInvalidFreeParms = 133
aceErrFreeNotOwned = 134
aceErrInvalidWindowParms = 135
aceErrInvalidConParms = 136
aceErrInvalidFileMode = 137
aceErrNotImplemented = 138
aceErrBloadTruncated = 139
aceErrPermissionDenied = 140
aceErrNoGraphicsSpace = 141
aceErrBadProgFormat = 142

chrBEL = $07  ;bell
chrTAB = $09  ;tab
chrBOL = $0a  ;beginning of line (return)
chrCR  = $0d  ;carriage return (newline)
chrVT  = $11  ;vertical tab (down, linefeed)
chrBS  = $14  ;backspace (del)
chrCLS = $93  ;clear screen (form feed)

stdin  = 0
stdout = 1
stderr = 2
;===end of ace interface declarations===
