/*  UNBCODE: bcode decoder program version 2.02 by Craig Bruce, 28-Feb-1995.
**
**  This program decodes binary data from BCODE, NUCODE, UUCODE, and
**  HEXCODE formats.
**
**  usage: unbcode [-ivdnf] [-p prefix] [filename ...]
**
**  -i : "informative" mode: tells you when files are recombined
**  -v : verbose mode: tells you what it is up to
**  -d : debugging mode: tells you about the file fragments it is juggling
**  -n : spits out names (only) of files extracted and recombined
**  -f : force: ignores decoding errors
**  -p : give filename prefix for temporary files
**  -help : gives usage information
**
**  CRC-32 code based on "File Verification Using CRC" by Mark R. Nelson in
**  Dr. Dobb's Journal, May 1992, pp. 64-67.  Note that CRC-32 values ARE the
**  same as in ZMODEM and PKZIP.
**
**  v1.00: Original release, in K&R C.  07-Oct-1993.
**  v1.01: Fixed some printf formats.  09-Oct-1993.
**  v1.02: Added code for remove and rename.  To activate, add -DNO_REMOVE
**         and/or -DNO_RENAME as needed.  15-Oct-1993, done by B.J.S.
**  v1.03: Fixed some more printf formats.  09-Nov-1993.
**  v1.04: Replaced CRC constants table by generating function.  30-Jan-1994.
**  v1.05: Included functionality of "nu-decode", improved uudecoder.
**  v1.06: Added flag for ignoring errors, recovers fully from partial data
**         of invalid decodes.
**  v1.07: Added support for "hexcode" encoding format.  29-Apr-1994.
**  v1.08: Fixed bug of not scanning first line after a uuencoded segment's
**         "end" token.  26-May-1994.
**  v1.09: Made it accept segment separators of the form "-bcode-begin"
**         (with one hyphen) for compatibility with anonymous mail/post
**         servers which erase everything following a line that starts with
**         two hyphens.  Also accepts string "nocrcxxx" in place of a
**         hexadecimal crc-32 value to make fewer requirements on bcode
**         encoder/filter programs.  16-Nov-1994.
**  v1.10: Added a "prefix" option to allow you to keep the intermediate
**         temporary files generated during decoding in a separate directory.
**         Note that this is a prefix rather than a directory name, so you
**         will have to include the final slash character in the prefix
**         name on a Unix system.  Note that using this option means that the
**         final intermediate reassembled file will have to be copied rather
**         than renamed to the final filename.  Also made the program accept
**         the segment length of zero as meaning "don't care" to make fewer
**         requirements on filter programs.  Also converted program back to
**         ANSI C.  05-Dec-1994.
**  v2.00: Tested and made some cosmetic changes, prepared for big release.
**         05-Dec-1994.
**  v2.01: Arrggh!  I found out that sscanf on a "%s" stops scanning at the
**         first whitespace character: filenames with spaces were not handled
**         correctly; fixed.  27-Feb-1995.
**  v2.02: D'ooh!  v2.01 introduced a bug into decoding regular uuencoded
**         files; fixed.  Fixed a couple of printf formats too.  28-Feb-1995.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
/*#include <sysent.h>*/ /*use this plus -DNO_RENAME -DNO_REMOVE some systems*/

#define VERSION         "2.02"
#define MAX_CHUNK       64
#define MAX_LINE        90
#define MAX_FILENAME    127
#define MAX_TEMPNAME    20
#define MAX_FRAG        1024
#define COPY_BUF_SIZE   4096
#define BCODE           1
#define UUCODE          2
#define HEXCODE         3

#ifdef NO_REMOVE
#define remove(x) unlink(x)
#endif

typedef unsigned char   BYTE;
typedef char            SBYTE;
typedef unsigned long   ULONG;
typedef int             BOOL;
#define TRUE            1
#define FALSE           0

typedef struct {
    ULONG   fromSeg;
    ULONG   toSeg;
    BOOL    isEnd;
    ULONG   tempFileName;
    ULONG   validLength;
    char   *filename;
} FRAGREC;

int     main( int argc, char *argv[] );
void    DecodeFile( FILE *fin );
ULONG   FindHeader( FILE *fin, char *filename );
int     GetNthToken( char *destStr, char line[], int tokenNumber );
void    BuildDecodeTable( void );
BOOL    DecodeSeg( FILE *fin, FILE *fout, ULONG segnum, char *filename,
            BOOL *isEnd, ULONG *outSeglen );
int     DecodeBcodeLine( char line[], int lineLen, BYTE buf[],
            ULONG *totalCrc );
int     DecodeUucodeLine( char line[], int lineLen, BYTE buf[],
            ULONG *totalCrc );
int     DecodeHexcodeLine( char line[], int lineLen, BYTE buf[],
            ULONG *totalCrc );
void    CrcTableGen( void );
void    LoadStatusFile( void );
void    SaveStatusFile( void );
void    WriteStatusData( FILE *fout );
FILE   *GetTempFile( char *filename, ULONG segnum, ULONG *fragRec );
ULONG   GetFreeTempName( void );
void    GetTempNameStr( ULONG tempName, char *s );
void    ErrorPreramble( char *s );
ULONG   InsertFragRec( ULONG segnum, ULONG tempName, char *filename );
void    CheckCoalesce( ULONG fragRec );
void    CheckComplete( ULONG fragRec );
void    RemoveFragRec( ULONG rec );
void    DiscardSegment( ULONG fragRec );
int     ReadLine( FILE *fin, char *buf );
#ifdef NO_RENAME
int     rename( char *oldname, char *newname );
#endif

char   *progname;
BOOL    informative;
BOOL    verbose;
BOOL    debug;
BOOL    force;          /* force decoding - ignore errors */
BOOL    spitNames;      /* spit decoded file names out stdout */
char   *readFilename;
ULONG   readLineNum;

FRAGREC frags[MAX_FRAG];
ULONG   fragCount;
int     format;
BOOL    statusFileExists;
ULONG   nextTempName;
ULONG   hexcodeFilepos;
char    pushedHeader[MAX_LINE];
char   *tempPrefix;
BOOL    prefixUsed;
BYTE    copyBuf[COPY_BUF_SIZE];
SBYTE   decodeTable[ 256 ];
ULONG   crcTable[ 256 ];

char    base64Char[64] = {
        'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
        'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
        'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
        'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
};
char    uucodeChar[64] = {
        '`','!','"','#','$','%','&','\'','(',')','*','+',',', '-','.','/',
        '0','1','2','3','4','5','6','7', '8','9',':',';','<', '=','>','?',
        '@','A','B','C','D','E','F','G', 'H','I','J','K','L', 'M','N','O',
        'P','Q','R','S','T','U','V','W', 'X','Y','Z','[','\\',']','^','_'
};

/****************************************************************************/
int main( int argc, char *argv[] )
{
    FILE   *fin;
    int     i, j, dasharg;
    BOOL    filenameUsed;

    progname     = argv[0];
    filenameUsed = FALSE;
    informative  = FALSE;
    verbose      = FALSE;
    debug        = FALSE;
    force        = FALSE;
    spitNames    = FALSE;
    prefixUsed   = FALSE;
    tempPrefix = "";
    strcpy( pushedHeader, "Yo" );
    CrcTableGen();
    LoadStatusFile();

    i = 1;
    while (i<argc) {
        if (argv[i][0] == '-') {
            dasharg = i;
            for (j=1; argv[dasharg][j] != '\0'; j++) {
                switch (argv[dasharg][j]) {
                case 'i':
                    informative = TRUE;
                    break;
                case 'd':
                    verbose = TRUE;
                    informative = TRUE;
                    debug = TRUE;
                    fprintf(stderr, "unbcode version %s\n", VERSION );
                    fprintf(stderr, "debugging mode activated\n");
                    fprintf(stderr, "---Current-File-Segments---\n");
                    WriteStatusData( stderr );
                    fprintf(stderr, "--End-of-Current-Segments--\n");
                    break;
                case 'v':
                    verbose = TRUE;
                    informative = TRUE;
                    fprintf(stderr, "unbcode version %s\n", VERSION);
                    break;
                case 'f':
                    force = TRUE;
                    break;
                case 'n':
                    spitNames = TRUE;
                    break;
                case 'p':
                    i++;
                    if (i>=argc) {
                        fprintf(stderr, "%s: no argument for -p flag\n",
                            progname);
                        exit( 1 );
                    }
                    tempPrefix = argv[i];
                    prefixUsed = TRUE;
                    break;
                default:
                    fprintf(stderr, "unbcode version %s\n\n", VERSION);
                    fprintf(stderr, "usage: %s [-ivdnf] [-p pr",progname);
                    fprintf(stderr, "efix] [filename ...]\n\n");
                    fprintf(stderr, "-i : \"informative\" mode: tells yo");
                    fprintf(stderr, "u when files are recombined\n");
                    fprintf(stderr, "-v : verbose mode: tells you what it");
                    fprintf(stderr, " is up to\n");
                    fprintf(stderr, "-d : debugging mode: tells you about");
                    fprintf(stderr, " the file fragments it is juggling\n");
                    fprintf(stderr, "-n : spits out names (only) of files");
                    fprintf(stderr, " extracted and recombined\n");
                    fprintf(stderr, "-f : force: ignores decoding errors\n");
                    fprintf(stderr, "-p : give filename prefix for temporary");
                    fprintf(stderr, " files\n");
                    fprintf(stderr, "-help : gives usage information\n");
                    exit( 1 );
                    break;
                }
            }
        } else {
            filenameUsed = TRUE;
            if (verbose) {
                fprintf(stderr, "%s: decoding file \"%s\"\n",progname,argv[i]);
            }
            if( (fin = fopen(argv[i], "r")) == NULL) {
                fprintf(stderr, "%s: error opening \"%s\"\n",progname,argv[i]);
            } else {
                readFilename = argv[i];
                readLineNum = 0;
                DecodeFile( fin );
                fclose( fin );
            }
        }
        i++;
    }
    if (!filenameUsed) {
        if (verbose) {
            fprintf(stderr, "%s: decoding from standard input\n", progname);
        }
        readFilename = "<stdin>";
        readLineNum = 0;
        DecodeFile( stdin );
    }
    SaveStatusFile();
    return( 0 );
}

/****************************************************************************/
void DecodeFile( FILE *fin )
{
    char    filename[MAX_FILENAME];
    ULONG   segnum, seglen;
    FILE   *fout;
    BOOL    err, isEnd;
    ULONG   fragRec;

    while (TRUE) {
        segnum = FindHeader( fin, filename );
        if (segnum == 0) return;
        if (verbose) {
            fprintf(stderr, "%s: decoding segment %lu of file %s\n", progname,
                segnum, filename);
        }
        BuildDecodeTable();
        fout = GetTempFile( filename, segnum, &fragRec );
        if (fout == NULL) continue;
        err = DecodeSeg( fin, fout, segnum, filename, &isEnd, &seglen );
        fclose( fout );
        if (err) {
            fprintf(stderr, "%s: error decoding segment ", progname);
            fprintf(stderr, "#%lu of file \"%s\", discarding it\n",
                segnum, filename);
            DiscardSegment( fragRec );
        } else {
            frags[fragRec].isEnd = isEnd;
            frags[fragRec].validLength += seglen;
            CheckCoalesce( fragRec );
            CheckComplete( fragRec );
        }
        if (debug) {
            fprintf(stderr, "---File-Fragments---\n");
            WriteStatusData( stderr );
            fprintf(stderr, "--End-of-Fragments--\n");
        }
    }
}

/****************************************************************************/
ULONG FindHeader( FILE *fin, char *filename )
{
    char    line[MAX_LINE];
    int     len, scan, pos;
    ULONG   segnum;
    char    c1, c2, c3, dummyc;

    strcpy( line, pushedHeader );
    len = strlen( line );
    strcpy( pushedHeader, "nothing" );
    while( TRUE ) {
        if (len == -1) return( 0 );
        if (line[0]=='-') {
            for (pos=0; pos<=1; pos++) {
                scan = sscanf(&line[pos], "-bcode-begin %lu %c", &segnum,
                    &dummyc);
                if (scan == 2 && segnum >= 1) {
                    GetNthToken( filename, line, 3 );
                    format = BCODE;
                    return( segnum );
                }
                scan = sscanf(&line[pos], "-nucode-begin %lu %c", &segnum,
                    &dummyc);
                if (scan == 2 && segnum >= 1) {
                    GetNthToken( filename, line, 3 );
                    format = UUCODE;
                    return( segnum );
                }
                scan = sscanf(&line[pos], "-hexcode-begin %lu %c", &segnum,
                    &dummyc);
                if (scan == 2 && segnum >= 1) {
                    GetNthToken( filename, line, 3 );
                    format = HEXCODE;
                    return( segnum );
                }
            }
        } else if (line[0]=='b' && line[1]=='e') {
            scan = sscanf(line, "begin %c%c%c %c", &c1, &c2, &c3, &dummyc);
            if (scan == 4 && segnum >= 1 && (c1>='0'&&c1<='7')
                    && (c2>='0'&&c2<='7') && (c3>='0'&&c3<='7') ) {
                GetNthToken( filename, line, 3 );
                format = UUCODE;
                segnum = 1;
                return( segnum );
            }
        }
        len = ReadLine( fin, line );
    }
}

/****************************************************************************/
int GetNthToken( char *destStr, char line[], int tokenNumber )
{
    int     i, tok;

    i = 0;
    tok = 1;
    while (line[i]!='\0') {
        /* skip whitespace before token */
        while (line[i]==' ' || line[i]=='\t' || line[i]=='\n') i++;
        /* check if we are done */
        if (tok >= tokenNumber) {
            strcpy( destStr, &line[i] );
            return( i );
        }
        tok++;
        /* skip intermediate token characters */
        while (line[i]!='\0' && line[i]!=' ' &&line[i]!='\t' &&line[i]!='\n'){
            i++;
        }
    }
    return( -1 );
}

/****************************************************************************/
void BuildDecodeTable( void )
{
    int     i, v;

    for (i=0; i<=255; i++) decodeTable[i] = -1;

    v = 0;
    switch( format ) {
    case BCODE:
        for (i=0; i<64; i++) {
            decodeTable[ base64Char[i] ] = v++;
        }
        decodeTable['='] = 0;
        break;
    case UUCODE:
        for (i=0; i<64; i++) {
            decodeTable[ uucodeChar[i] ] = v++;
        }
        decodeTable[' '] = 0;
        break;
    case HEXCODE:
        for (i='0'; i<='9'; i++) {
            decodeTable[ i ] = v++;
        }
        for (i='a'; i<='f'; i++) {
            decodeTable[ i ] = v++;
        }
        break;
    }
}

/****************************************************************************/
BOOL DecodeSeg( FILE *fin, FILE *fout, ULONG segnum, char *filename,
    BOOL *isEnd, ULONG *outSeglen )
{
    ULONG   seglen, segcrc;
    ULONG   statNum, statLen, statCrc;
    int     len, scan, off, pos;
    BYTE    buf[MAX_CHUNK+3];
    char    line[MAX_LINE];
    char    chkstr[MAX_FILENAME];
    char    c1, c2, c3;

    seglen = 0;
    segcrc = 0xFFFFFFFF;
    *isEnd = TRUE;
    *outSeglen = 0;
    hexcodeFilepos = 0;
    while( TRUE ) {
        len = ReadLine( fin, (char*)line );
        if (len == -1) {
            ErrorPreramble("unexpected EOF encountered\n");
            return( TRUE );
        }
        if (format==BCODE && line[0]=='-') break;
        if (format==UUCODE && (line[0]=='`' || line[0]==' '
            || (line[0]=='-' && line[1]=='-' && line[2]=='n')
            || (line[0]=='-' && line[1]=='n')) ) break;
        if (format==HEXCODE && line[0]=='-') break;

        switch( format ) {
        case BCODE:
            len = DecodeBcodeLine( line, len, buf, &segcrc );
            break;
        case HEXCODE:
            len = DecodeHexcodeLine( line, len, buf, &segcrc );
            break;
        case UUCODE:
            if (line[0]=='b') {
                scan = sscanf(line, "begin %c%c%c %s", &c1, &c2, &c3,
                    chkstr);
                if (scan == 4 && (c1>='0'&&c1<='7') && (c2>='0'&&c2<='7')
                        && (c3>='0'&&c3<='7') ) {
                    /* ignore duplicate begin line */
                    GetNthToken( chkstr, line, 3 );
                    if (strcmp(filename, chkstr)!=0 ) {
                        ErrorPreramble(  /*indent next line*/
                    "Ignoring inconsistent filename on uucode \"begin\" line\n"
                        );
                    }
                    len = 0;
                } else {
                    len = DecodeUucodeLine( line, len, buf, &segcrc );
                }
            } else {
                len = DecodeUucodeLine( line, len, buf, &segcrc );
            }
            break;
        }
        if (len == -1) {
            if (force) {
                /* ignore the error line */
                len = 0;
                fprintf(stderr,
                    "%s: force mode in effect: ignoring error line\n",
                    progname);
            } else {
                /* report invalid segment */
                return( TRUE );
            }
        }
        fwrite( buf, sizeof(BYTE), len, fout );
        seglen += len;
    }
    segcrc ^= 0xFFFFFFFF;
    *outSeglen = seglen;

    switch( format ) {
    case BCODE:
        off = 0;
        for (pos=0; pos<=1; pos++) {
            if( strncmp("-bcode-end ", &line[pos], 11)==0 ) {
                off = 11 + pos;
                *isEnd = TRUE;
                break;
            } else if( strncmp("-bcode-continued ", &line[pos], 17)==0 ) {
                off = 17 + pos;
                *isEnd = FALSE;
                break;
            }
        }
        if (off==0) {
            ErrorPreramble("Invalid BCODE control token\n");
            return( !force );
        }
        break;
    case HEXCODE:
        off = 0;
        for (pos=0; pos<=1; pos++) {
            if( strncmp("-hexcode-end ", &line[pos], 13)==0 ) {
                off = 13 + pos;
                *isEnd = TRUE;
                break;
            } else if( strncmp("-hexcode-continued ", &line[pos], 19)==0 ) {
                off = 19 + pos;
                *isEnd = FALSE;
                break;
            }
        }
        if (off==0) {
            ErrorPreramble("Invalid HEXCODE control token\n");
            return( !force );
        }
        break;
    case UUCODE:
        if( line[0]=='`' || line[0]==' ' ) {
            off = -1;
            /* scan ahead for the "end" and maybe "--nucode-" or "-nucode"*/
            len = ReadLine( fin, line );
            if ( len>1 && strcmp(line, "end")==0 ) {
                /* we have scanned the "end" line, try for the "nucode" line */
                len = ReadLine( fin, line );
                if (strncmp("-nucode-end ", line, 12)==0
                        || strncmp("--nucode-end ", line, 13)==0
                        || strncmp("-nucode-continued ", line, 18)==0
                        || strncmp("--nucode-continued ", line, 19)==0 ) {
                    /* got the "nucode" ending line; use it below */
                    off = 0;
                }
                strcpy( pushedHeader, line );
            } else {
                /* couldn't get the "end" line; push what we got, return err */
                strcpy( pushedHeader, line );
                ErrorPreramble(
                    "Uucode end marker not followed by \"end\" line\n");
                return( !force );
            }
        } else {
            off = 0;
        }
        if (off==0) {
            for (pos=0; pos<=1; pos++) {
                if( strncmp("-nucode-end ", &line[pos], 12)==0 ) {
                    off = 12 + pos;
                    *isEnd = TRUE;
                    break;
                } else if( strncmp("-nucode-continued ", &line[pos], 18)==0 ) {
                    off = 18 + pos;
                    *isEnd = FALSE;
                    break;
                }
            }
            if (off==0) {
                ErrorPreramble("Invalid NUCODE control token\n");
                return( !force );
            }
        }
        break;
    }

    if (off > 0) {
        scan = sscanf(&line[off], "%lu %lu %08lx", &statNum, &statLen,
            &statCrc);
        if (scan != 3) {
            scan = sscanf(&line[off], "%lu %lu %s", &statNum, &statLen,chkstr);
            if (scan==3 && strcmp(chkstr,"nocrcxxx")==0) {
                statCrc = segcrc;
            } else {
                scan = 0;
            }
        }
    } else {
        scan = 3;
        statNum = segnum;
        statLen = seglen;
        statCrc = segcrc;
    }
    if (scan != 3) {
        ErrorPreramble("Invalid control line format\n");
        return( !force );
    }
    if (statNum != segnum) {
        ErrorPreramble("Mismatching segment numbers\n");
        return( !force );
    }
    if (statLen != seglen && statLen != 0) {
        ErrorPreramble("Mismatching segment lengths\n");
        return( !force );
    }
    if (statCrc != segcrc) {
        ErrorPreramble("Mismatching CRC-32 values\n");
        return( !force );
    }
    return( FALSE );
}

/****************************************************************************/
int DecodeBcodeLine( char line[], int lineLen, BYTE buf[], ULONG *totalCrc )
{
    register int    c0,c1,c2,c3;
    register ULONG  crc;
    register int    bufpos, linepos, len;

    crc = *totalCrc;
    if (lineLen%4 != 0) {
        ErrorPreramble("Invalid BCODE line length\n");
        return( -1 );
    }
    if (lineLen == 0) return( 0 );
    for (bufpos = 0, linepos = 0; linepos < lineLen; bufpos+=3, linepos+=4) {
        c0 = decodeTable[ line[linepos  ] ];
        c1 = decodeTable[ line[linepos+1] ];
        c2 = decodeTable[ line[linepos+2] ];
        c3 = decodeTable[ line[linepos+3] ];
        if (c0==-1 || c1==-1 || c2==-1 || c3==-1) {
            ErrorPreramble("Invalid character in BCODE data\n");
            return( -1 );
        }
        buf[bufpos  ] = (c0<<2) | ((c1&0x30)>>4);
        buf[bufpos+1] = ((c1&0x0F)<<4) | ((c2&0x3C)>>2);
        buf[bufpos+2] = ((c2&0x03)<<6) | c3;
    }

    /* fix non-integral-length (last) line */
    if (line[lineLen-1] == '=') bufpos--;
    if (line[lineLen-2] == '=') bufpos--;
    len = bufpos;

    /* take crc of binary data */
    for( bufpos=0; bufpos < len; bufpos++ ) {
        crc = ((crc>>8) & 0x00FFFFFF) ^ crcTable[ (crc^buf[bufpos]) & 0xFF];
    }
    *totalCrc = crc;
    return( len );
}

/****************************************************************************/
int DecodeUucodeLine( char line[], int lineLen, BYTE buf[], ULONG *totalCrc )
{
    register int    c0,c1,c2,c3;
    register ULONG  crc;
    register int    bufpos, linepos, len;

    crc = *totalCrc;
    if (lineLen == 0) {
        ErrorPreramble("Blank line found in uucode data\n");
        return( -1 );
    }
    len = decodeTable[ line[0] ];
    if (len<0) {
        ErrorPreramble("Invalid character for uucode line length\n");
        return( -1 );
    }
    if ( 1+((len+2)/3)*4 > lineLen ) {
        ErrorPreramble("Uucode line length longer than actual line\n");
        return( -1 );
    }
    for (bufpos = 0, linepos = 1; bufpos<len; bufpos+=3, linepos+=4) {
        c0 = decodeTable[ line[linepos  ] ];
        c1 = decodeTable[ line[linepos+1] ];
        c2 = decodeTable[ line[linepos+2] ];
        c3 = decodeTable[ line[linepos+3] ];
        if (c0==-1 || c1==-1 || c2==-1 || c3==-1) {
            ErrorPreramble("Invalid character in uucode data");
            fprintf(stderr, ": one of \"%c%c%c%c\"\n", line[linepos],
                line[linepos+1], line[linepos+2], line[linepos+3]);
            return( -1 );
        }
        buf[bufpos  ] = (c0<<2) | ((c1&0x30)>>4);
        buf[bufpos+1] = ((c1&0x0F)<<4) | ((c2&0x3C)>>2);
        buf[bufpos+2] = ((c2&0x03)<<6) | c3;
    }

    /* take crc of binary data */
    for( bufpos=0; bufpos < len; bufpos++ ) {
        crc = ((crc>>8) & 0x00FFFFFF) ^ crcTable[ (crc^buf[bufpos]) & 0xFF];
    }
    *totalCrc = crc;
    return( len );
}

/****************************************************************************/
int DecodeHexcodeLine( char line[], int lineLen, BYTE buf[], ULONG *totalCrc )
{
    register int    c0, c1, b0;
    register ULONG  crc, cksum;
    register int    bufpos, linepos, len;
    ULONG           lineAddr, lineCksum;
    int             lineDataStart, lineDataEnd, i;

    crc = *totalCrc;
    /* handle line address */
    lineAddr = 0;
    for (i=0; i<lineLen; i++) {
        c0 = decodeTable[ line[i] ];
        if (c0 == -1) break;
        lineAddr = lineAddr*16 + c0;
    }
    if (i<1 || i>=lineLen || line[i]!=':') {
        ErrorPreramble("Invalid HEXCODE line address format\n");
        return( -1 );
    }
    if (hexcodeFilepos==0) {
        hexcodeFilepos = lineAddr;
    }
    if (lineAddr != hexcodeFilepos) {
        ErrorPreramble("Invalid HEXCODE line address\n");
        hexcodeFilepos = lineAddr;
        return( -1 );
    }
    lineDataStart = i+1;

    /* fetch line checksum */
    if (lineLen>=3) {
        c0 = decodeTable[ line[ lineLen-2 ] ];
        c1 = decodeTable[ line[ lineLen-1 ] ];
    }
    if (lineLen<3 || c0==-1 || c1==-1 || line[lineLen-3]!=':') {
        ErrorPreramble("Invalid HEXCODE line checksum format\n");
        return( -1 );
    }
    lineCksum = c0 * 16 + c1;
    lineDataEnd = lineLen-3;

    /* decode line */
    bufpos = 0;
    cksum = 0;
    for (linepos=lineDataStart; linepos<lineDataEnd; bufpos++, linepos+=2) {
        c0 = decodeTable[ line[linepos  ] ];
        c1 = decodeTable[ line[linepos+1] ];
        if (c0==-1 || c1==-1) {
            if (linepos==lineDataEnd-1) {
                ErrorPreramble("Odd number of HEXCODE data chars\n");
            } else {
                ErrorPreramble("Invalid character in HEXCODE data\n");
            }
            return( -1 );
        }
        b0 = (c0<<4) | c1;
        buf[bufpos] = b0;
        cksum += b0;
    }

    len = bufpos;
    hexcodeFilepos += len;
    if ( (cksum & 0xFF) != lineCksum) {
        ErrorPreramble("HEXCODE line checksum mismatch\n");
        return( -1 );
    }

    /* take crc of binary data */
    for( bufpos=0; bufpos < len; bufpos++ ) {
        crc = ((crc>>8) & 0x00FFFFFF) ^ crcTable[ (crc^buf[bufpos]) & 0xFF];
    }
    *totalCrc = crc;
    return( len );
}

/****************************************************************************/
void CrcTableGen( void )
{
    unsigned long   crc, poly;
    int             i, j;

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

/****************************************************************************/
void LoadStatusFile( void )
{
    char    line[MAX_LINE];
    char    filename[MAX_FILENAME];
    FILE   *fin;
    int     scan;
    FRAGREC*h;
    char    interpret[4], dummyc;

    fragCount = 0;
    statusFileExists = FALSE;
    nextTempName = 1;
    sprintf(filename, "%s0BC-STAT", tempPrefix);
    if( (fin = fopen(filename, "r")) == NULL) return;
    statusFileExists = TRUE;
    while ( ReadLine( fin, line ) != -1 ) {
        h = & frags[fragCount];
        scan = sscanf(line, "%05lu-%05lu  %c%c%c  %010lu  0BC%05lx  %c",
            &h->fromSeg, &h->toSeg, &interpret[0], &interpret[1],
            &interpret[2], &h->validLength, &h->tempFileName, &dummyc);
        if (scan != 8) {
            fprintf(stderr, "Bad Status Line: %s\n", line);
        } else {
            GetNthToken( filename, line, 5 );
            h->isEnd = (interpret[0] == 'e');
            h->filename = (char*)malloc( strlen(filename)+1 );
            strcpy( h->filename, filename );
            fragCount++;
            if (fragCount>=MAX_FRAG) {
                fprintf(stderr, "too many status fragments, FATAL\n");
                exit( 1 );
            }
        }
    }
    fclose( fin );
}

/****************************************************************************/
void SaveStatusFile( void )
{
    char    filename[MAX_FILENAME];
    FILE   *fout;
    int     err;

    sprintf(filename, "%s0BC-STAT", tempPrefix);
    if (fragCount == 0) {
        if (statusFileExists) {
            err = remove( filename );
            if (err) {
                fprintf(stderr,"cannot remove %s\n", filename);
            }
        }
        return;
    }
    if( (fout = fopen(filename, "w")) == NULL) {
        fprintf(stderr, "cannot open %s for saving status\n",filename);
        return;
    }
    WriteStatusData( fout );
    fclose( fout );
}

/****************************************************************************/
void WriteStatusData( FILE *fout )
{
    ULONG   i;
    FRAGREC *h;
    char    interpret[4];

    for (i=0; i<fragCount; i++) {
        h = & frags[i];
        if (h->isEnd) {
            strcpy( interpret, "end" );
        } else {
            if (h->fromSeg == 1) {
                strcpy( interpret, "beg" );
            } else {
                strcpy( interpret, "mid" );
            }
        }
        fprintf(fout, "%05lu-%05lu  %s  %010lu  0BC%05lu  %s\n", h->fromSeg,
            h->toSeg, interpret, h->validLength, h->tempFileName, h->filename);
    }
}

/****************************************************************************/
FILE *GetTempFile( char *filename, ULONG segnum, ULONG *fragRec )
{
    FRAGREC*h;
    ULONG   i, rec, tempName;
    char    tempname[MAX_FILENAME];
    char    tempmode[4];
    BOOL    append;
    FILE   *fout;

    /* search to append to existing file */
    append = FALSE;
    for (i=0; i<fragCount; i++) {
        h = & frags[i];
        if (strcmp(filename, h->filename)==0) {
            if (h->fromSeg<=segnum && segnum<=h->toSeg) {
                ErrorPreramble("Ignoring duplicate segment");
                fprintf(stderr, " %lu of \"%s\"\n", segnum, filename);
                return( NULL );
            }
            if (segnum == h->toSeg + 1) {
                /* append */
                tempName = h->tempFileName;
                strcpy( tempmode, "ab" );
                append = TRUE;
                h->toSeg = segnum;
                h->isEnd = FALSE;
                rec = i;
                break;
            }
        }
    }

    if (!append) {
        /* find new tempname */
        tempName = GetFreeTempName();
        /* create new frag record */
        rec = InsertFragRec( segnum, tempName, filename );
        strcpy(tempmode, "wb");
    }

    /* open the temporary file */
    GetTempNameStr( tempName, tempname );
    if( (fout = fopen(tempname, tempmode)) == NULL) {
        fprintf(stderr, "%s: error opening \"%s\" for write, FATAL!\n",
                progname, filename);
        exit( 1 );
    }
    *fragRec = rec;
    return( fout );
}

/****************************************************************************/
ULONG GetFreeTempName( void )
{
    long    i, j;
    ULONG   tempName;

    for (i=0; i<100000; i++) {
        tempName = nextTempName;
        nextTempName = (nextTempName + 1) % 100000;
        for (j=0; j<fragCount; j++) {
            if (frags[j].tempFileName == tempName) {
                j = -1;
                break;
            }
        }
        if (j != -1) break;
        if (i==100000-1) {
            fprintf(stderr, "cannot allocate temp name!!!  FATAL!\n");
            exit( 1 );
        }
    }
    return( tempName );
}

/****************************************************************************/
void GetTempNameStr( ULONG tempName, char *s )
{
    sprintf(s, "%s0BC%05lu", tempPrefix, tempName);
}

/****************************************************************************/
void ErrorPreramble( char *s )
{
    fprintf(stderr, "%s: (%s:%lu) %s", progname, readFilename, readLineNum, s);
}

/****************************************************************************/
ULONG InsertFragRec( ULONG segnum, ULONG tempName, char *filename )
{
    ULONG   rec;
    long    i;
    FRAGREC*h;
    int     cmp;

    rec = fragCount;
    fragCount++;
    if (fragCount >= MAX_FRAG) {
        fragCount--;
        fprintf(stderr, "input data too fragmented; FATAL!\n");
        exit( 1 );
    }

    /* insertion sort new entry into place */
    for (i=rec-1; i>=0; i--) {
        cmp = strcmp(filename, frags[i].filename);
        if (cmp > 0) {
            break;
        } else if (cmp==0 && segnum>frags[i].fromSeg) {
            break;
        }
        frags[i+1] = frags[i];
    }

    /* initialize new record */
    rec = i+1;
    h = & frags[rec];
    h->fromSeg = segnum;
    h->toSeg = segnum;
    h->isEnd = FALSE;
    h->tempFileName = tempName;
    h->validLength = 0;
    h->filename = (char*)malloc( strlen(filename)+1 );
    strcpy( h->filename, filename );
    return( rec );
}

/****************************************************************************/
void CheckCoalesce( ULONG fragRec )
{
    FRAGREC *p, *q;
    char    tempname[MAX_FILENAME];
    int     err;
    FILE   *fin, *fout;
    int     bytes;

    if (fragRec >= fragCount-1) return;
    p = & frags[fragRec];
    q = & frags[fragRec+1];
    if (strcmp(p->filename, q->filename)!=0 || p->toSeg+1 != q->fromSeg) {
        return;
    }

    /* coalesce */
    if (debug) {
        fprintf(stderr,"Coalescing segs %lu-%lu and segs %lu-%lu of file %s\n",
            p->fromSeg, p->toSeg, q->fromSeg, q->toSeg, p->filename);
    }

    /* copy file contents */
    GetTempNameStr( q->tempFileName, tempname );
    if( (fin = fopen(tempname, "rb")) == NULL) {
        fprintf(stderr, "%s: error opening \"%s\", FATAL\n",progname,tempname);
        exit( 1 );
    }
    GetTempNameStr( p->tempFileName, tempname );
    if( (fout = fopen(tempname, "ab")) == NULL) {
        fprintf(stderr, "%s: error opening \"%s\", FATAL\n",progname,tempname);
        exit( 1 );
    }
    while ( (bytes=fread( copyBuf, sizeof(BYTE), COPY_BUF_SIZE, fin)) >0) {
        fwrite( copyBuf, sizeof(BYTE), bytes, fout );
    }
    fclose( fout );
    fclose( fin );

    /* remove old record and temp file */
    p->toSeg = q->toSeg;
    p->isEnd = q->isEnd;
    p->validLength += q->validLength;
    GetTempNameStr( q->tempFileName, tempname );
    err = remove( tempname );
    if (err) {
        fprintf(stderr, "cannot remove %s, continuing.\n", tempname);
    }
    RemoveFragRec( fragRec+1 );
}

/****************************************************************************/
void CheckComplete( ULONG fragRec )
{
    char    tempname[MAX_FILENAME];
    int     err, bytes;
    FRAGREC*h;
    FILE   *fin, *fout;

    h = & frags[fragRec];
    if (!h->isEnd || h->fromSeg!=1) return;

    GetTempNameStr( h->tempFileName, tempname );
    if (prefixUsed) {
        /* must copy file contents--may cross directory/device */
        if( (fin = fopen(tempname, "rb")) == NULL) {
            fprintf(stderr, "%s: error opening \"%s\", FATAL\n", progname,
                tempname);
            exit( 1 );
        }
        if( (fout = fopen(h->filename, "wb")) == NULL) {
            fprintf(stderr, "%s: error opening \"%s\", continuing\n", progname,
                h->filename);
            fclose( fin );
        }
        if (fout != NULL) {
            while ( (bytes=fread(copyBuf,sizeof(BYTE),COPY_BUF_SIZE,fin)) >0) {
                fwrite( copyBuf, sizeof(BYTE), bytes, fout );
            }
            fclose( fout );
            fclose( fin );
            err = remove( tempname );
            if (err) {
                fprintf(stderr, "%s: cannot remove \"%s\", continuing\n",
                    progname, tempname);
            }
        }
    } else {
        /* rename temp file to final filename--in same directory */
        err = rename( tempname, h->filename );
        if (err) {
            fprintf(stderr, "cannot rename %s to %s, continuing.\n", tempname,
                h->filename );
        }
    }
    if (informative) {
        fprintf(stderr, "%s: extracted \"%s\"\n", progname, h->filename );
    }
    if (spitNames) {
        printf("%s\n", h->filename);
    }
    RemoveFragRec( fragRec );
}

/****************************************************************************/
void RemoveFragRec( ULONG rec )
{
    long    i;

    /* free dynamic memory of filename */
    free( frags[rec].filename );

    /* pull all higher entries back one position */
    for (i=rec; i<fragCount-1; i++) {
        frags[i] = frags[i+1];
    }

    fragCount--;
}

/****************************************************************************/
void DiscardSegment( ULONG fragRec )
{
    FRAGREC*f;
    int     err;
    char    filename[MAX_FILENAME];
    ULONG   newtemp;
    long    remlen;
    int     bytes, buflen;
    FILE   *fin, *fout;

    f = &frags[fragRec];
    if (f->fromSeg == f->toSeg) {
        /* discard entire fragment--easy */
        GetTempNameStr( f->tempFileName, filename );
        err = remove( filename );
        if (err) {
            fprintf(stderr, "%s: cannot remove \"%s\", continuing\n", progname,
                filename);
        }
        RemoveFragRec( fragRec );
    } else {
        /* discard last segment of a fragment--hard */
        newtemp = GetFreeTempName();

        /* copy valid contents into new temporary file */
        GetTempNameStr( f->tempFileName, filename );
        if( (fin = fopen(filename, "rb")) == NULL) {
            fprintf(stderr, "%s: error opening \"%s\", FATAL!\n", progname,
                filename);
            exit( 1 );
        }
        GetTempNameStr( newtemp, filename );
        if( (fout = fopen(filename, "wb")) == NULL) {
            fprintf(stderr, "%s: error opening \"%s\", FATAL!\n", progname,
                filename);
            exit( 1 );
        }
        remlen = f->validLength;
        while( TRUE ) {
            if (remlen <= 0) break;
            buflen = (remlen<COPY_BUF_SIZE) ? (int)remlen : COPY_BUF_SIZE;
            bytes = fread( copyBuf, sizeof(BYTE), buflen, fin);
            if (bytes <= 0) break;
            fwrite( copyBuf, sizeof(BYTE), bytes, fout );
            remlen -= bytes;
        }
        GetTempNameStr( f->tempFileName, filename );
        if (remlen>0) {
            fprintf(stderr, "%s: unexpectedly short file \"%s\", FATAL!\n",
                progname, filename);
            exit( 1 );
        }
        fclose( fout );
        fclose( fin );
    
        /* remove old temporary file */
        err = remove( filename );
        if (err) {
            fprintf(stderr, "%s: cannot remove \"%s\", continuing\n", progname,
                filename);
        }
        /* update fragment record */
        (f->toSeg)--;
        f->tempFileName = newtemp;
    }
}

/****************************************************************************/
int ReadLine( FILE *fin, char *buf )
{
    char   *r;
    char    junkline[MAX_LINE];
    int     len;

    r = fgets( buf, MAX_LINE, fin );
    if (r==NULL) return( -1 );
    len = strlen(buf)-1;
    if (buf[len] == '\n') {
        buf[len] = '\0';
    } else {
        len++;
        do {
            r = fgets( junkline, MAX_LINE, fin );
        } while (r!=NULL && junkline[strlen(junkline)-1]!='\n');
    }
    readLineNum++;
    return( len );
}

/****************************************************************************/
#ifdef NO_RENAME
int rename( char *oldname, char *newname )
{
    if ( link( oldname, newname ))
        return( -1 );
    else
        return( unlink( oldname ));
}
#endif

/******************************************************************--END--***/
