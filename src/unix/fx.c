/*  Craig's File-eXchange (FX) Protocol Server, ANSI-C Unix
**
**  v1.04 written 1995/07/18 by Craig Bruce, rev. 1995/12/09.
**
**  This is a simple file upload/download protocol that uses between 1-byte
**  and (64K-1)-byte packets for transferring data.
**
**  Potential for portability problems: (1) Some compilers may already have
**  definitions for types "uchar", "ulong", and "bool".  Simply uncomment
**  the #defines near the front of this file.  (2) Some Unix environments
**  may not have a "tm_gmtoff" field in their "struct tm" structure in
**  "time.h".  If your environment doesn't have this, the comment out the
**  define for GOT_GMT_OFFSET.  (3) Some Unix environments don't have a
**  "termio.h" file and only have a "termios.h" file instead.  If this
**  is the case with your system, then comment out the GOT_TERMIO_FILE define.
**  It might be better to just comment it out anyway (don't know).
**
**  This program assumes that the stdin and stdout file descriptors are both
**  connected to the terminal line that goes to the client machine.  The
**  program further assumes that its host uses ASCII characters and that
**  the connection to the client is 8-bit clean, except that the protocol
**  does not use the Ctrl-Q and Ctrl-S (Xon/Xoff) characters, in order to
**  avoid potential problems with intermediate devices.
**
**  usage:
**  fx [-dlv78] [-m maximums] [-f argfile] [[-b] binfile ...] [-t textfile ...]
**
**  -d = debug mode
**  -l = write to log file ("fx.log")
**  -v = verbose log/debug mode
**  -7 = use 7-bit channel
**  -8 = use 8-bit channel
**  -m = set maximum packet sizes; maximums = ulbin/ultxt/dlbin/dltxt (bytes)
**  -f = take arguments one-per-line from given argfile
**  -b = binary files prefix
**  -t = text files prefix
**  -help = help
*/

/* #define GOT_ULONG_ALREADY  */
/* #define GOT_UCHAR_ALREADY  */
/* #define GOT_BOOL_ALREADY   */
/* #define GOT_GMT_OFFSET     */
/* #define GOT_TERMIO_FILE    */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#ifdef GOT_TERMIO_FILE
#  include <termio.h>
#else
#  include <termios.h>
#  include <unistd.h>
#  include <sys/termios.h>
#endif

#ifndef GOT_UCHAR_ALREADY
    typedef unsigned char uchar;
#endif
#ifndef GOT_ULONG_ALREADY
    typedef unsigned long ulong;
#endif
#ifndef GOT_BOOL_ALREADY
    typedef int bool;
#endif
#define TRUE 1
#define FALSE 0

/* this is the number of hours and minutes of local time zone from GMT */
/* this is only used if GOT_GMT_OFFSET is not defined */
#define STATIC_GMT_OFFSET_HOURS   99 /*99==unknown*/
#define STATIC_GMT_OFFSET_MINUTES 0  /*negative if hours is negative*/
/* this is the accuracy of the local time, expressed in BCD */
/* the accuracy is 2^NN, NN=BCDvalue, milliseconds.  0x10 == 1.024 sec */
#define LOCAL_TIME_ACCURACY           0x10
#define MAX_PACKET                    65567
#define MAX_DATA                      65535
#define DEFAULT_UPLOAD_TEXT_SIZE      1024
#define DEFAULT_UPLOAD_BINARY_SIZE    1024
#define DEFAULT_DOWNLOAD_TEXT_SIZE    MAX_DATA
#define DEFAULT_DOWNLOAD_BINARY_SIZE  MAX_DATA
#define DEFAULT_CHANNEL_WIDTH_8       TRUE

#define VERSION             "ANSI-C Unix version 1.04"
#define CHR_START           0x01
#define CHR_END             0x19
#define CHR_ESC             0x05
#define CHR_ABORT           0x18
#define CHR_XON             0x11
#define CHR_XOFF            0x13
#define CHR_QUOTE8          0x14

#define REQ_CONNECT         'C'
#define ACK_CONNECT         'c'
#define REQ_DISCONNECT      'Q'
#define ACK_DISCONNECT      'q'
#define REQ_DOWNLOAD_OPEN   'D'
#define ACK_DOWNLOAD_OPEN   'd'
#define REQ_DOWNLOAD_PACKET 'S'
#define ACK_DOWNLOAD_PACKET 's'
#define REQ_DOWNLOAD_CLOSE  'E'
#define ACK_DOWNLOAD_CLOSE  'e'
#define REQ_UPLOAD_OPEN     'U'
#define ACK_UPLOAD_OPEN     'u'
#define REQ_UPLOAD_PACKET   'R'
#define ACK_UPLOAD_PACKET   'r'
#define REQ_UPLOAD_CLOSE    'V'
#define ACK_UPLOAD_CLOSE    'v'
#define PROTOCOL_VERSION    0x01
#define TX_FD               1
#define RX_FD               0

int     main( int argc, char *argv[] );
void    HandleConnect( uchar packet[], ulong len );
void    HandleDisconnect( uchar packet[], ulong len );
void    HandleUploadOpen( uchar packet[], ulong len );
void    HandleUploadPacket( uchar packet[], ulong len );
void    HandleUploadClose( uchar packet[], ulong len );
void    HandleDownloadOpen( uchar packet[], ulong len );
void    HandleDownloadPacket( uchar packet[], ulong len );
void    HandleDownloadClose( uchar packet[], ulong len );
ulong   ExtractWord32( uchar buf[] );
void    InsertWord32( ulong val, uchar buf[] );
char   *FindArgFilename( void );
void    IncrementArg( void );
void    CloseFiles( void );
void    FillInFileStat( char *filename, uchar ack[] );
void    LinkInit( void );
void    LinkMake8( void );
void    LinkRestore( void );
ulong   GetPacket( uchar buf[] );
void    ProtoStat( char *s );
int     GetByte( void );
void    PutPacket( uchar buf[], ulong len );
void    PutByte( int byte );
void    PutByteLiteral( int byte );
void    PutByteInit( void );
void    PutByteFlush( void );
void    MakeCharPrintable( char *outstr, uchar lframe, uchar c, uchar rframe );
ulong   CrcCalcBuf( uchar buf[], ulong len );
void    CrcGenerateTable( void );

uchar   packet[MAX_PACKET];
uchar   txBuf[MAX_PACKET+MAX_PACKET+MAX_PACKET+64];
long    txBufCount;
#ifdef GOT_TERMIO_FILE
    struct termio  oldterm, newterm;
#else
    struct termios oldterm, newterm;
#endif
bool    debugMode;
FILE   *downloadFile;
FILE   *uploadFile;
FILE   *logFile;
FILE   *argFile;
char    argFileLine[1001];
unsigned uploadTextSize;
unsigned uploadBinarySize;
unsigned downloadTextSize;
unsigned downloadBinarySize;
int     uploadSeq;
ulong   uploadFileLen;
int     downloadSeq;
ulong   downloadFileLen;
bool    downloadFileOpenError;
uchar   downloadPacket[MAX_PACKET];
ulong   crcTable[256];
int     argNum;
bool    textMode;
int     globalArgc;
char  **globalArgv;
bool    verboseMode;
bool    extremelyVerboseMode;
bool    channelWidth8;
bool    linkInited;

/****************************************************************************/
int main( int argc, char *argv[] )
{
    int     i;
    char    out[80];
    ulong   len;
    uchar   c;

    printf("Craig's File eXchange Protocol Server, %s\n", VERSION);
    
    linkInited   = FALSE;
    debugMode    = FALSE;
    downloadFile = NULL;
    uploadFile   = NULL;
    logFile      = NULL;
    argFile      = NULL;
    textMode     = FALSE;
    verboseMode  = FALSE;
    extremelyVerboseMode = FALSE;
    uploadTextSize    = DEFAULT_UPLOAD_TEXT_SIZE;
    uploadBinarySize  = DEFAULT_UPLOAD_BINARY_SIZE;
    downloadTextSize  = DEFAULT_DOWNLOAD_TEXT_SIZE;
    downloadBinarySize= DEFAULT_DOWNLOAD_BINARY_SIZE;
    channelWidth8 = DEFAULT_CHANNEL_WIDTH_8;
    downloadSeq = 257;
    uploadSeq = 257;
    downloadFileLen = 0;
    downloadFileOpenError = FALSE;
    uploadFileLen = 0;
    globalArgc = argc;
    globalArgv = argv;
    argNum     = 1;
    CrcGenerateTable();
    FindArgFilename();
    printf("To abort, type three Ctrl-X's in a row.\n");
    printf("Server Ready.\n");
    fflush(stdout);
    LinkInit();

    while (TRUE) {
        len = GetPacket( packet );
        if (len>0) {
            c = packet[0];
            switch( c ) {
            case REQ_CONNECT:
                HandleConnect( packet, len );
                break;
            case REQ_DISCONNECT:
                HandleDisconnect( packet, len );
                sleep( 1 );
                LinkRestore();
                CloseFiles();
                exit( 0 );
                break;
            case REQ_DOWNLOAD_OPEN:
                HandleDownloadOpen( packet, len );
                break;
            case REQ_DOWNLOAD_PACKET:
                HandleDownloadPacket( packet, len );
                break;
            case REQ_DOWNLOAD_CLOSE:
                HandleDownloadClose( packet, len );
                break;
            case REQ_UPLOAD_OPEN:
                HandleUploadOpen( packet, len );
                break;
            case REQ_UPLOAD_PACKET:
                HandleUploadPacket( packet, len );
                break;
            case REQ_UPLOAD_CLOSE:
                HandleUploadClose( packet, len );
                break;
            default:
                break;
            }
        }
    }
    LinkRestore();
    CloseFiles();
    return( 0 );
}

/****************************************************************************/
void HandleConnect( uchar packet[], ulong len )
{
    uchar   ack[20];
    bool    clientWidth8;

    ProtoStat("Connection request");
    if (len<3) return;
    clientWidth8 = (packet[2] == '8');
    ack[0] = ACK_CONNECT;
    ack[1] = PROTOCOL_VERSION;
    ack[2] = (channelWidth8) ? '8' : '7';
    if (!clientWidth8 || !channelWidth8) {
        channelWidth8 = FALSE;
    }
    ack[3] = (channelWidth8) ? '8' : '7';
    InsertWord32( uploadTextSize, &ack[4] );
    InsertWord32( uploadBinarySize, &ack[8] );
    InsertWord32( downloadTextSize, &ack[12] );
    InsertWord32( downloadBinarySize, &ack[16] );
    PutPacket( ack, 20 );
    if (channelWidth8) {
        LinkMake8();
    }
}

/****************************************************************************/
void HandleDisconnect( uchar packet[], ulong len )
{
    uchar   ack[1];

    ProtoStat("Disconnection request");
    ack[0] = ACK_DISCONNECT;
    PutPacket( ack, 1 );
}

/****************************************************************************/
void HandleUploadOpen( uchar packet[], ulong len )
{
    char   *filename;
    int     textFile;
    uchar   ack[20];
    
    ProtoStat("Upload-Open request");
    if (len<20+2) return;
    filename = &packet[20];
    packet[len] = '\0';
    textFile = (packet[1] == 't');
    ack[0] = ACK_UPLOAD_OPEN;
    ack[1] = 'y';
    if (uploadFile == NULL) {
        if (textFile) {
            uploadFile = fopen(filename, "w");
        } else {
            uploadFile = fopen(filename, "wb");
        }
        if (uploadFile == NULL) ack[1] = 'n';
    }
    PutPacket( ack, 2 );
    uploadSeq = 257;
    uploadFileLen = 0;
}

/****************************************************************************/
void HandleUploadPacket( uchar packet[], ulong len )
{
    ulong   bytes;
    uchar   ack[20];

    ProtoStat("Upload-Packet request");
    if (len<6) return;
    if ( uploadSeq != packet[1] ) {
        bytes = ExtractWord32( &packet[2] );
        if (uploadFile != NULL) {
            bytes = fwrite( &packet[6], 1, bytes, uploadFile );
            fflush( uploadFile );
        } else {
            bytes = 0;
        }
        uploadSeq = packet[1];
        uploadFileLen += bytes;
    }
    ack[0] = ACK_UPLOAD_PACKET;
    ack[1] = packet[1];
    PutPacket( ack, 2 );
}

/****************************************************************************/
void HandleUploadClose( uchar packet[], ulong len )
{
    uchar   ack[20];

    ProtoStat("Upload-Close request");
    if (uploadFile != NULL) {
        fclose( uploadFile );
        uploadFile = NULL;
    }
    ack[0] = ACK_UPLOAD_CLOSE;
    InsertWord32( uploadFileLen, &ack[1] );
    PutPacket( ack, 5 );
}

/****************************************************************************/
void HandleDownloadOpen( uchar packet[], ulong len )
{
    char   *filename;
    uchar   ack[1024];
    int     i;

    ProtoStat("Download-Open request");
    filename = FindArgFilename();
    ack[0] = ACK_DOWNLOAD_OPEN;
    if (filename == NULL) {
        ack[1] = '0';
        PutPacket( ack, 2 );
        return;
    }
    ack[1] = (textMode) ? 't' : 'b';
    if (uploadFile == NULL) {
        if (textMode) {
            downloadFile = fopen( filename, "r");
        } else {
            downloadFile = fopen( filename, "rb");
        }
        if (downloadFile==NULL) {
            ack[1] = 'e';
            downloadFileOpenError = TRUE;
        } else {
            downloadFileOpenError = FALSE;
        }
    }
    for (i=2; i<20; i++) ack[i] = 0x00;
    strcpy( &ack[20], filename );
    FillInFileStat( filename, ack );
    downloadSeq = 257;
    downloadFileLen = 0;
    PutPacket( ack, 20+strlen(filename)+1 );
}

/****************************************************************************/
void FillInFileStat( char *filename, uchar ack[] )
{
    int     err;
    struct stat statbuf;
    struct tm *tmbuf;
    int     century, year, month;
    long    gmtOff, gmtOffHours, gmtOffMinutes, gmtSeconds;
    bool    gmtWest;

    err = stat( filename, &statbuf );
    InsertWord32( statbuf.st_size, &ack[2] );
    if (S_ISDIR(statbuf.st_mode)) {
        ack[1] = 'd';
    }
    ack[6]  = ((statbuf.st_mode & S_ISUID) ? 0x04 : 0x00) |
              ((statbuf.st_mode & S_ISGID) ? 0x02 : 0x00) |
              ((statbuf.st_mode & S_IWUSR) ? 0x01 : 0x00);
    ack[7]  = ((statbuf.st_mode & S_IWUSR) ? 0x80 : 0x00) |
              ((statbuf.st_mode & S_IXUSR) ? 0x40 : 0x00) |
              ((statbuf.st_mode & S_IRGRP) ? 0x20 : 0x00) |
              ((statbuf.st_mode & S_IWGRP) ? 0x10 : 0x00) |
              ((statbuf.st_mode & S_IXGRP) ? 0x08 : 0x00) |
              ((statbuf.st_mode & S_IROTH) ? 0x04 : 0x00) |
              ((statbuf.st_mode & S_IWOTH) ? 0x02 : 0x00) |
              ((statbuf.st_mode & S_IXOTH) ? 0x01 : 0x00);
    tmbuf   = localtime( &statbuf.st_mtime );
    year    = (tmbuf->tm_year + 1900) % 100;
    century = (tmbuf->tm_year + 1900) / 100;
    month   = (tmbuf->tm_mon) + 1;
    ack[8]  = ((century/10)<<4)        | (century%10);         /* century */
    ack[9]  = ((year/10)<<4)           | (year%10);            /* year */
    ack[10] = ((month/10)<<4)          | (month%10);           /* month */
    ack[11] = ((tmbuf->tm_mday/10)<<4) | (tmbuf->tm_mday%10);  /* day */
    ack[12] = ((tmbuf->tm_hour/10)<<4) | (tmbuf->tm_hour%10);  /* hour */
    ack[13] = ((tmbuf->tm_min/10)<<4)  | (tmbuf->tm_min%10);   /* minute */
    ack[14] = ((tmbuf->tm_sec/10)<<4)  | (tmbuf->tm_sec%10);   /* second */
    ack[15] = 0x00;                                            /* hundredths */
    ack[16] = 0x00 | (tmbuf->tm_wday);                         /* msec + dow */
#   ifdef GOT_GMT_OFFSET
    gmtOff = tmbuf->tm_gmtoff;
#   else
    gmtOff = STATIC_GMT_OFFSET_HOURS * 3600 + STATIC_GMT_OFFSET_MINUTES * 60;
#   endif
    gmtWest = (gmtOff < 0);
    gmtSeconds = (gmtWest) ? -gmtOff : gmtOff;
    gmtOffHours = gmtSeconds / 3600;
    gmtOffMinutes = gmtSeconds - gmtOffHours*3600;
    ack[17] = ((gmtOffHours/10)<<4) | (gmtOffHours%10);     /* GMT-off hours */
    if (gmtWest) {
        ack[17] |= 0x80;
    }
    ack[18] = ((gmtOffMinutes/10)<<4) | (gmtOffMinutes%10);/* GMT-off minutes*/
    ack[19] = LOCAL_TIME_ACCURACY;
}

/****************************************************************************/
void HandleDownloadPacket( uchar packet[], ulong len )
{
    ulong   maxData;
    char    out[1000];

    ProtoStat("Download-Packet request");
    if (len != 6) return;
    maxData = ExtractWord32( &packet[2] );
    if (textMode && maxData > downloadTextSize) maxData = downloadTextSize;
    if (!textMode && maxData>downloadBinarySize) maxData = downloadBinarySize;
    if (packet[1] != downloadSeq) {
        /* new request--read more file data */
        downloadPacket[0] = ACK_DOWNLOAD_PACKET;
        downloadPacket[1] = packet[1];
        if (downloadFile != NULL) {
            maxData = fread( &downloadPacket[6], 1, maxData, downloadFile );
        } else {
            maxData = 0;
        }
        InsertWord32( maxData, &downloadPacket[2] );
        downloadSeq = packet[1];
        downloadFileLen += maxData;
    }
    PutPacket( downloadPacket, ExtractWord32(&downloadPacket[2])+6 );
}

/****************************************************************************/
void HandleDownloadClose( uchar packet[], ulong len )
{
    uchar   ack[20];

    ProtoStat("Download-Close request");
    if (downloadFileOpenError) {
        downloadFileOpenError = FALSE;
        IncrementArg();
    }
    if (downloadFile != NULL) {
        fclose( downloadFile );
        downloadFile = NULL;
        IncrementArg();
    }
    ack[0] = ACK_DOWNLOAD_CLOSE;
    InsertWord32( downloadFileLen, &ack[1] );
    PutPacket( ack, 5 );
}

/****************************************************************************/
ulong ExtractWord32( uchar buf[] )
{
    return( (buf[0]<<24) | (buf[1]<<16) | (buf[2]<<8) | (buf[3]) );
}

/****************************************************************************/
void InsertWord32( ulong val, uchar buf[] )
{
    buf[0] = (val >> 24) & 0xFF;
    buf[1] = (val >> 16) & 0xFF;
    buf[2] = (val >>  8) & 0xFF;
    buf[3] = (val >>  0) & 0xFF;
}

/****************************************************************************/
char *FindArgFilename( void )
{
    char   *ap, *strp;
    bool    keepScanning;
    int     j, scanc;
    ulong   ulTxt, ulBin, dlTxt, dlBin;

    keepScanning = TRUE;
    while (keepScanning) {
        if (argFile==NULL) {
            ap = globalArgv[ argNum ];
        } else {
            ap = argFileLine;
        }
        keepScanning = FALSE;
        if (ap != NULL) {
            if (ap[0] == '-') {
                keepScanning = TRUE;
                for (j=0; ap[j]!='\0'; j++) {
                    switch( ap[j] ) {
                    case 'd':
                        debugMode = TRUE;
                        ProtoStat("Debug mode activated");
                        break;
                    case 'l':
                        logFile = fopen("fx.log", "w");
                        ProtoStat("Log file activated");
                        break;
                    case 't':
                        textMode = TRUE;
                        ProtoStat("Text mode activated for sending files");
                        break;
                    case 'b':
                        textMode = FALSE;
                        ProtoStat(
                            "Binary mode activated for sending files");
                        break;
                    case 'v':
                        verboseMode = TRUE;
                        ProtoStat("Verbose mode activated");
                        break;
                    case 'V':
                        verboseMode = TRUE;
                        extremelyVerboseMode = TRUE;
                        ProtoStat("Extremely Verbose mode activated");
                        break;
                    case '7':
                        channelWidth8 = FALSE;
                        ProtoStat("7-bit channel width selected");
                        break;
                    case '8':
                        channelWidth8 = TRUE;
                        ProtoStat("8-bit channel width selected");
                        break;
                    case 'k':
                        uploadTextSize     = 1024;
                        uploadBinarySize   = 1024;
                        downloadTextSize   = 1024;
                        downloadBinarySize = 1024;
                        break;
                    case 'h':
                        LinkRestore();
                        CloseFiles();
                        fprintf(stderr,"\nusage:\n  fx [-dlv78k] [-m maximu");
                        fprintf(stderr,"ms] [-f argfile] [[-b] binfile ...] ");
                        fprintf(stderr,"[-t txtfile ...]\n\n");
                        fprintf(stderr,"-d = debug mode\n");
                        fprintf(stderr,"-l = write to log file (\"fx.log\")");
                        fprintf(stderr,"\n-v = verbose log/debug mode\n");
                        fprintf(stderr,"-7 = set channel width to 7 bits\n");
                        fprintf(stderr,"-8 = set channel width to 8 bits\n");
                        fprintf(stderr,"-k = use 1K packet sizes\n");
                        fprintf(stderr,"-m = set maximum packet sizes; ");
                        fprintf(stderr,"maximums = ulbin/ultxt/dlbin/dltxt ");
                        fprintf(stderr,"(bytes)\n");
                        fprintf(stderr,"-f = take arguments one-per-line ");
                        fprintf(stderr,"from given argfile\n");
                        fprintf(stderr,"-b = binary files prefix\n");
                        fprintf(stderr,"-t = text files prefix\n");
                        fprintf(stderr,"-help = help\n");
                        exit( 0 );
                    case 'm':
                        IncrementArg();
                        if (argFile==NULL) {
                            strp = globalArgv[ argNum ];
                        } else {
                            strp = argFileLine;
                        }
                        scanc = sscanf(strp, "%ld/%ld/%ld/%ld", &ulBin, &ulTxt,
                            &dlBin, &dlTxt);
                        if (scanc == 4) {
                            if (ulTxt>=1 && ulTxt <= MAX_DATA) {
                                uploadTextSize = ulTxt;
                            }
                            if (ulBin>=1 && ulBin <= MAX_DATA) {
                                uploadBinarySize = ulBin;
                            }
                            if (dlTxt>=1 && dlTxt <= MAX_DATA) {
                                downloadTextSize = dlTxt;
                            }
                            if (dlBin>=1 && dlBin <= MAX_DATA) {
                                downloadBinarySize = dlBin;
                            }
                        }
                        break;
                    case 'f':
                        IncrementArg();
                        if (argFile == NULL) {
                            strp = globalArgv[ argNum ];
                            argFile = fopen(strp, "r");
                            if (argFile != NULL) {
                                ;
                            }
                        }
                        break;
                    default:
                        break;
                    }
                }
                IncrementArg();
            }
        }
    }
    return( ap );
}

/****************************************************************************/
void IncrementArg( void )
{
    char   *ap;

    if (argFile != NULL) {
        ap = fgets(argFileLine, 1000, argFile);
        if (ap != NULL) {
            argFileLine[strlen(argFileLine)-1] = '\0';
            return;
        }
        fclose(argFile);
        argFile = NULL;
    }
    ap = globalArgv[ argNum ];
    if (ap != NULL) {
        argNum += 1;
    }
}

/****************************************************************************/
void CloseFiles( void )
{
    if (downloadFile != NULL) {
        fclose( downloadFile );
    }
    if (uploadFile != NULL) {
        fclose( uploadFile );
    }
    if (logFile != NULL) {
        fclose( logFile );
    }
    if (argFile != NULL) {
        fclose( argFile );
    }
}

/****************************************************************************/
void LinkInit( void )
{
    /* turn input/output special processing off */
#   ifdef GOT_TERMIO_FILE
        ioctl( RX_FD, TCGETA, &oldterm );
#   else
        tcgetattr( RX_FD, &oldterm );
#   endif
    newterm = oldterm;
    newterm.c_iflag &= ~( IXON );       /* disable processing of XON char */
    newterm.c_iflag &= ~( IXOFF );      /* disable processing of XOFF char */
    newterm.c_iflag &= ~( INPCK );      /* disable input parity checking */
    newterm.c_iflag &= ~( ISTRIP );     /* disable striping of high bit */
    newterm.c_iflag &= ~( INLCR );      /* disable mapping of NL to CR */
    newterm.c_iflag &= ~( IGNCR );      /* disable ignoring of CR */
    newterm.c_iflag &= ~( ICRNL );      /* disable mapping of CR to NL */
    newterm.c_oflag &= ~( OPOST );      /* disable all output processing */
    newterm.c_lflag &= ~( ECHO );       /* disable echoing */
    newterm.c_lflag &= ~( ICANON );     /* disable line-input processing */
    newterm.c_lflag &= ~( ISIG );       /* disable keyboard signals */
    newterm.c_cc[VMIN] = 1;             /* minimum input size = 1 char */
    newterm.c_cc[VTIME] = 0;            /* zero delay in accepting char */
#   ifdef GOT_TERMIO_FILE
        ioctl( RX_FD, TCSETA, &newterm );
#   else
        tcsetattr( RX_FD, TCSADRAIN, &newterm );
#   endif
    linkInited = TRUE;
}

/****************************************************************************/
void LinkMake8( void )
{
    newterm.c_cflag &= ~( CSIZE );  /* clear word size */
    newterm.c_cflag |= CS8;         /* select 8-bit bytes */
    newterm.c_cflag &= ~( PARENB ); /* disable parity checking */
#   ifdef GOT_TERMIO_FILE
        ioctl( RX_FD, TCSETA, &newterm );
#   else
        tcsetattr( RX_FD, TCSADRAIN, &newterm );
#   endif
}

/****************************************************************************/
void LinkRestore( void )
{
    if (linkInited) {
#       ifdef GOT_TERMIO_FILE
            ioctl( RX_FD, TCSETA, &oldterm );
#       else
            tcsetattr( RX_FD, TCSANOW, &oldterm );
#       endif
    }
}

/****************************************************************************/
ulong GetPacket( uchar buf[] )
{
    ulong   i;
    int     c;
    bool    getNewPacket;
    bool    getMoreBytes;
    ulong   inErrCheck;
    ulong   calcErrCheck;
    char    out[100];

    getNewPacket = TRUE;
    while (getNewPacket) {
        /* get start of packet */
        while (GetByte() != CHR_START) ;
        if (verboseMode) ProtoStat("Start");
        i = 0;
        getNewPacket = FALSE;
        getMoreBytes = TRUE;
        while (getMoreBytes) {
            c = GetByte();
            switch( c ) {
            case CHR_START:
                /* mal-formed packet: drop it and receive a new one */
                ProtoStat("Re-Start:ignored");
                getMoreBytes = FALSE;
                getNewPacket = TRUE;
                break;
            case CHR_END:
                /* current packet is finished */
                if (verboseMode) ProtoStat("End");
                getMoreBytes = FALSE;
                if (i>=4) {
                    inErrCheck = (buf[i-4]<<24) + (buf[i-3]<<16)
                        + (buf[i-2]<<8) + buf[i-1];
                    i -= 4;
                    calcErrCheck = CrcCalcBuf( buf, i );
                    if (verboseMode) {
                        sprintf(out,"inCRC=%lx,calcCRC=%lx",inErrCheck,
                            calcErrCheck);
                        ProtoStat(out);
                    }
                    if (inErrCheck!=calcErrCheck && !debugMode) {
                        ProtoStat("Bad error check!");
                        getNewPacket = TRUE;
                    }
                } else {
                    /* too few characters in packet: drop it */
                    ProtoStat("Packet too short");
                    getNewPacket = TRUE;
                }
                break;
            case CHR_ESC:
                if (verboseMode) ProtoStat("Escape");
                c = GetByte();
                if (c>=0x40 && c<=0x5f) {
                    /* convert into control character */
                    c = c & 0x1F;
                } else if (c == 0x3f) {
                    /* convert to DEL character */
                    c = 0x7f;
                } else {
                    /* mal-formed escape sequence: abort packet */
                    ProtoStat("Mal-formed Escape");
                    getMoreBytes = FALSE;
                    getNewPacket = TRUE;
                }
                break;
            case CHR_QUOTE8:
                if (verboseMode) ProtoStat("Quote8");
                c = GetByte();
                if (c==CHR_ESC) {
                    c = GetByte();
                    if (c>=0x40 && c<=0x5f) {
                        c = c & 0x1F;
                    } else if (c == 0x3f) {
                        c = 0x7f;
                    } else {
                        ProtoStat("Mal-formed Quote8ed Escape");
                        getMoreBytes = FALSE;
                        getNewPacket = TRUE;
                    }
                }
                c |= 0x80;
                break;
            default:
                break;
            }
            if (getMoreBytes && i<MAX_PACKET) {
                buf[i] = c;
                i += 1;
            }
            if (!getNewPacket && extremelyVerboseMode) {
                sprintf(out, "len=%d", i);
                ProtoStat(out);
            }
        }
    }
    buf[i] = '\0';  /* useful for debugging */
    return( i );
}

/****************************************************************************/
void ProtoStat( char *s )
{
    char    out[100];

    if (debugMode) {
        sprintf(out, "[%s]", s);
        write( TX_FD, out, strlen(out) );
    }
    if (logFile != NULL) {
        fprintf(logFile, "%s\n", s);
        fflush( logFile );
    }
}

/****************************************************************************/
int GetByte( void )
{
    unsigned char inC;
    int     c;
    static int abortCount = 0;
    char    out[100];

    /* take care of Ctrl-X in here */
    c = CHR_ABORT;
    while( c == CHR_ABORT ) {
        read( RX_FD, &inC, 1 );
        c = inC;
        if (c==CHR_ABORT) {
            abortCount++;
            if (abortCount >= 3) {
                ProtoStat("Cancel-Cancel-Cancel received");
                CloseFiles();
                LinkRestore();
                exit( 1 );
            }
        } else {
            abortCount = 0;
        }
    }
    if (debugMode) {
        MakeCharPrintable( out, '<', c, '>' );
        write( TX_FD, out, strlen(out) );
    }
    return( c );
}

/****************************************************************************/
void PutPacket( uchar buf[], ulong len )
{
    ulong   i;
    ulong   outErrCheck;

    PutByteInit();
    outErrCheck = 0;
    PutByteLiteral( CHR_START );
    for (i=0; i<len; i++) {
        PutByte( buf[i] );
    }
    outErrCheck = CrcCalcBuf( buf, len );
    PutByte( (outErrCheck>>24) & 0xFF );
    PutByte( (outErrCheck>>16) & 0xFF );
    PutByte( (outErrCheck>> 8) & 0xFF );
    PutByte( (outErrCheck    ) & 0xFF );
    PutByteLiteral( CHR_END );
    PutByteFlush();
}

/****************************************************************************/
void PutByte( int inByte )
{
    int     byte;

    byte = inByte;
    if ( !channelWidth8 && byte > 0x80 ) {
        PutByteLiteral( CHR_QUOTE8 );
        byte &= 0x7F;
    }
    switch( byte ) {
    case CHR_START:
    case CHR_END:
    case CHR_ESC:
    case CHR_ABORT:
    case CHR_XON:
    case CHR_XOFF:
    case CHR_QUOTE8:
        PutByteLiteral( CHR_ESC );
        PutByteLiteral( byte | 0x40 );
        break;
    default:
        PutByteLiteral( byte );
        break;
    }
}

/****************************************************************************/
void PutByteLiteral( int byte )
{
    uchar   c;
    char    out[100];
    
    c = byte;
    if (debugMode) {
        MakeCharPrintable( out, '{', c, '}' );
        write( TX_FD, out, strlen(out) );
    } else {
        txBuf[txBufCount] = c;
        txBufCount++;
    }
}

/****************************************************************************/
void PutByteInit( void )
{
    txBufCount = 0;
}

/****************************************************************************/
void PutByteFlush( void )
{
    if (txBufCount > 0) {
        write( TX_FD, txBuf, txBufCount );
        txBufCount = 0;
    }
}

/****************************************************************************/
void MakeCharPrintable( char *outstr, uchar lframe, uchar c, uchar rframe )
{
    if (c>=' ' && c<='~') {
        sprintf(outstr, "%c%c%c", lframe, c, rframe);
    } else if (c<=0x1F) {
        sprintf(outstr, "%c^%c%c", lframe, (c&0x1F)+0x40, rframe);
    } else if (c==0x7F) {
        sprintf(outstr, "%c^%c%c", lframe, '?', rframe);
    } else {
        sprintf(outstr, "%c0x%02X%c", lframe, c, rframe);
    }
}

/****************************************************************************/
ulong CrcCalcBuf( uchar buf[], ulong len )
{
    register unsigned long crc;
    int     c;
    ulong   i;

    crc = 0xFFFFFFFF;
    for (i=0; i<len; i++) {
        crc = ((crc>>8) & 0x00FFFFFF) ^ crcTable[ (crc^buf[i]) & 0xFF ];
    }
    return( crc^0xFFFFFFFF );
}

/****************************************************************************/
void CrcGenerateTable( void )
{
    unsigned long crc, poly;
    int     i, j;

    poly = 0xEDB88320L;
    for (i=0; i<256; i++) {
        crc = i;
        for (j=8; j>0; j--) {
            if (crc&1) {
                crc = (crc >> 1) ^ poly;
            } else {
                crc >>= 1;
            }
        }
        crcTable[i] = crc;
    }
}
