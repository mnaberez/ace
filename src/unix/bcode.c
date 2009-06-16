/*  BCODE: bcode encoder program version 2.02 by Craig Bruce, 28-Feb-1995.
**
**  This program encodes binary data into BCODE, NUCODE (improved UUCODE), and
**  HEXCODE formats.
**
** usage: bcode [-vbuh12] [-l max_lines] [-p pref] [[[filename][-a alias]] ...]
**
**  -v : verbose mode
**  -b : encode into BCODE format
**  -u : encode into NUCODE format (upward/downward compatible with UUCODE)
**  -h : encode into HEXCODE format
**  -1 : use one hyphen as the token prefix
**  -2 : use two hyphens as the token prefix
**  -l : set maximum line-count per encoded segment
**  -a : use alias instead of given filename in encoded data
**  -p : specify a filename prefix to use for the output files
**  -help : show usage info
**
**  CRC-32 code based on "File Verification Using CRC" by Mark R. Nelson in
**  Dr. Dobb's Journal, May 1992, pp. 64-67.  Note that CRC-32 values ARE the
**  same as in ZMODEM and PKZIP.
**
**  v1.00: Original release of bcode, of course, in K&R C.
**  v1.01: Fixed some bad print formats.
**  v1.02: Fixed some more bad print formats.
**  v1.03: Changed the bcoding format to use 72 chars/line, rather than 76.
**  v1.04: Replaced CRC constants table by generating function.
**  v1.05: Optionally generates MS-DOS-compatible filenames of the form
**         "bcNNN.bco" or "uuNNN.uue" in segmentation mode if you "-DMESS_DOS"
**         on the compile line.
**  v1.06: Included functionality of "nuencode", improved uuencoding scheme.
**         Added definition "DEFAULT_BCODE" to select default of encoding
**         scheme.
**  v1.07: Added "hex" format, a very simple hexadecimal encoding format.
**         Mess-DOS name: "hexNNN.hex".  Changed "DEFAULT_FORMAT".
**  v1.08: Added option of generating control tokens with either one or two
**         hyphens, since lines beginning with two hyphens can be mishandled
**         by anonymous mail/news servers.  Also made option interpreter
**         handle grouped control flags.  Also converted code *back* to ANSI
**         standard C (really, GCC).  Using one hyphen will be the new
**         standard and using two will be relegated to the status of an
**         "hysterical raisin".  29-Nov-1994.
**  v1.09: Added a "prefix" option to allow you to send the encoder output to
**         a different directory if you want to.  Note that this is a prefix
**         and not a directory name, so on a Unix system, you would have to
**         say something like "-p /tmp/".  04-Dec-1994.
**  v2.00: Tested and made some cosmetic changes, prepared for big release.
**         05-Dec-1994.
**  v2.02: Extended to use only the basenames of given pathnames for files
**         to encode, implying that output will go to the current directory.
**         To find the basename, the name is scanned from the end backwards
**         until a "/", "\", or ":" is found.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define VERSION         "2.02"
#define MAX_CHUNK       54
#define MAX_LINE        90
#define MAX_FILENAME    127
#define BCODE           1
#define UUCODE          2
#define HEXCODE         3
#define DEFAULT_FORMAT  UUCODE      /* set this to your preferred default */
#define DEFAULT_TOKHYPH 1           /* set this to your preferred default */

/* #define MESS_DOS        1 */     /* define for ms-dos format filenames */

typedef unsigned char   BYTE;
typedef unsigned long   ULONG;
typedef int             BOOL;
#define TRUE            1
#define FALSE           0

int     main( int argc, char *argv[] );
void    EncodeFile( FILE *fin, char *filename, char *alias, ULONG maxlines );
void    ExtractBasename( char *filename, char pathname[] );
BOOL    EncodeSeg( char *filename, ULONG segnum, ULONG maxlines, FILE *fin,
            FILE *fout );
ULONG   EncodeLine( FILE *fin, FILE *fout, ULONG *totalCrc );
int     ReadChunk( FILE *fin, BYTE *buf );
void    CrcTableGen( void );

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
char    hexcodeChar[16] = {
        '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
};
ULONG crcTable[256];

char   *progname;
char   *prefix;
BOOL    verbose;
ULONG   jobsegnum;
ULONG   filepos;
int     format;
char    tokhyph[3];
int     maxChunk;

/****************************************************************************/
int main( int argc, char *argv[] )
{
    FILE   *fin;
    int     i, j, dasharg;
    ULONG   maxlines;
    BOOL    filenameUsed;
    char   *alias;
    char    filename[MAX_FILENAME];

    progname = argv[0];
    filenameUsed = FALSE;
    verbose = FALSE;
    alias = "stdin";
    prefix = "";
    maxlines = 0;
    jobsegnum = 1;
    format = DEFAULT_FORMAT;
    switch (format) {
    case BCODE:   maxChunk = 54;  break;
    case UUCODE:  maxChunk = 45;  break;
    case HEXCODE: maxChunk = 32;  break;
    }
    if (DEFAULT_TOKHYPH==1) {
        strcpy(tokhyph,"-");
    } else {
        strcpy(tokhyph,"--");
    }
    CrcTableGen();

    i = 1;
    while (i<argc) {
        if (argv[i][0] == '-') {
            dasharg = i;
            for (j=1; argv[dasharg][j] != '\0'; j++) {
                switch (argv[dasharg][j]) {
                case 'l':
                    i++;
                    if (i>=argc) {
                        fprintf(stderr, "%s: no argument for -l flag\n",
                            progname);
                        exit( 1 );
                    }
                    maxlines = atoi( argv[i] );
                    break;
                case 'a':
                    i++;
                    if (i>=argc) {
                        fprintf(stderr, "%s: no argument for -a flag\n",
                            progname);
                        exit( 1 );
                    }
                    alias = argv[i];
                    break;
                case 'p':
                    i++;
                    if (i>=argc) {
                        fprintf(stderr, "%s: no argument for -p flag\n",
                            progname);
                        exit( 1 );
                    }
                    prefix = argv[i];
                    break;
                case 'v':
                    verbose = TRUE;
                    fprintf(stderr, "bcode encoder version %s\n", VERSION);
                    break;
                case 'b':
                    format = BCODE;
                    maxChunk = 54;
                    break;
                case 'u':
                    format = UUCODE;
                    maxChunk = 45;
                    break;
                case 'h':
                    format = HEXCODE;
                    maxChunk = 32;
                    break;
                case '1':
                    strcpy(tokhyph,"-");
                    break;
                case '2':
                    strcpy(tokhyph,"--");
                    break;
                default:
                    fprintf(stderr, "bcode encoder version %s\n\n", VERSION);
                    fprintf(stderr, "usage: %s [-vbuh12] [-l max_l", progname);
                    fprintf(stderr, "ines] [-p pref] [[[filename][-a alias]");
                    fprintf(stderr, "] ...]\n\n");
                    fprintf(stderr, "-v : verbose mode\n");
                    fprintf(stderr, "-b : encode into BCODE format\n");
                    fprintf(stderr, "-u : encode into NUCODE format (upward/");
                    fprintf(stderr, "downward compatible with UUCODE)\n");
                    fprintf(stderr, "-h : encode into HEXCODE format\n");
                    fprintf(stderr, "-1 : use one hyphen as the token ");
                    fprintf(stderr, "prefix (new standard)\n");
                    fprintf(stderr, "-2 : use two hyphens as the token ");
                    fprintf(stderr, "prefix\n");
                    fprintf(stderr, "-l : set maximum line-count per encoded");
                    fprintf(stderr, " segment\n");
                    fprintf(stderr, "-a : use alias instead of given filenam");
                    fprintf(stderr, "e in encoded data\n");
                    fprintf(stderr, "-p : specify a filename prefix to use ");
                    fprintf(stderr, "for the output files\n");
                    fprintf(stderr, "-help : produce this help info\n");
                    exit( 1 );
                    break;
                }
            }
        } else {
            filenameUsed = TRUE;
            ExtractBasename( filename, argv[i] );
            if (verbose) {
                fprintf(stderr, "%s: encoding file \"%s\"\n",progname,argv[i]);
            }
            if( (fin = fopen(argv[i], "rb")) == NULL) {
                fprintf(stderr, "%s: error opening \"%s\"\n",progname,argv[i]);
            } else {
                if (i+2<argc && argv[i+1][0]=='-' && argv[i+1][1]=='a') {
                    EncodeFile( fin, filename, argv[i+2], maxlines );
                    i += 2;
                } else {
                    EncodeFile( fin, filename, filename, maxlines );
                }
                fclose( fin );
            }
        }
        i++;
    }
    if (!filenameUsed) {
        if (verbose) {
            fprintf(stderr, "%s: encoding from standard input\n", progname);
        }
        EncodeFile( stdin, "stdin", alias, maxlines );
    }
    return( 0 );
}

/****************************************************************************/
void ExtractBasename( char *filename, char pathname[] )
{
    int i;

    for (i=0; pathname[i]!=0; i++) ;
    while (i>=0 && pathname[i]!='/' && pathname[i]!='\\' && pathname[i]!=':') {
        i--;
    }
    strcpy( filename, &pathname[i+1] );
}

/****************************************************************************/
void EncodeFile( FILE *fin, char *filename, char *alias, ULONG maxlines )
{
    FILE   *fout;
    ULONG   segnum;
    BOOL    moreSegs;
    char    outname[MAX_FILENAME];

    filepos = 0;
    for( segnum = 1; ; segnum++ ) {
        if (maxlines>0) {
#           ifdef MESS_DOS
            switch (format) {
            case BCODE:
                sprintf(outname, "%sbc%03lu.bco", prefix, jobsegnum);
                break;
            case UUCODE:
                sprintf(outname, "%suu%03lu.uue", prefix, jobsegnum);
                break;
            case HEXCODE:
                sprintf(outname, "%shex%03lu.hex", prefix, jobsegnum);
                break;
            }
#           else
            switch (format) {
            case BCODE:
                sprintf(outname, "%s%s.b%02lu", prefix, filename, segnum);
                break;
            case UUCODE:
                sprintf(outname, "%s%s.u%02lu", prefix, filename, segnum);
                break;
            case HEXCODE:
                sprintf(outname, "%s%s.h%02lu", prefix, filename, segnum);
                break;
            }
#           endif
            jobsegnum++;
            if (verbose) {
                fprintf(stderr, "%s: outputting to file \"%s\"\n", progname,
                    outname);
            }
            fout = fopen(outname, "w");
            if (fout == NULL) {
                fprintf(stderr, "%s: error opening \"%s\" for writing\n",
                    progname, outname);
                return;
            }
        } else {
            fout = stdout;
        }
        moreSegs = EncodeSeg(alias, segnum, maxlines, fin, fout);
        if (!moreSegs) break;
        if (maxlines>0) {
            fclose( fout );
        }
    }
}

/****************************************************************************/
BOOL EncodeSeg( char *filename, ULONG segnum, ULONG maxlines, FILE *fin,
    FILE *fout )
{
    ULONG   segcrc, seglen, len;
    ULONG   linenum;
    char   *ends;

    switch (format) {
    case BCODE:
        fprintf(fout,"%sbcode-begin %lu %s\n", tokhyph, segnum, filename);
        break;
    case UUCODE:
        fprintf(fout,"%snucode-begin %lu %s\n", tokhyph, segnum, filename);
        if (segnum==1) {
            fprintf(fout, "begin 640 %s\n", filename);
        }
        break;
    case HEXCODE:
        fprintf(fout,"%shexcode-begin %lu %s\n", tokhyph, segnum, filename);
        break;
    }

    seglen = 0;
    segcrc = 0xFFFFFFFF;
    linenum = 1;
    while (!maxlines || linenum<=maxlines) {
        len = EncodeLine( fin, fout, &segcrc );
        seglen += len;
        if (len <= 0) break;
        linenum++;
    }
    segcrc ^= 0xFFFFFFFF;

    if (linenum>maxlines && maxlines) {
        ends = "continued";
    } else {
        if (format==UUCODE) {
            fprintf(fout, "`\n");
            fprintf(fout, "end\n");
        }
        ends = "end";
    }
    switch (format) {
    case BCODE:
        fprintf(fout, "%sbcode-%s %lu %lu %08lx\n", tokhyph, ends, segnum,
            seglen, segcrc);
        break;
    case UUCODE:
        fprintf(fout, "%snucode-%s %lu %lu %08lx\n", tokhyph, ends, segnum,
            seglen, segcrc);
        break;
    case HEXCODE:
        fprintf(fout, "%shexcode-%s %lu %lu %08lx\n", tokhyph, ends, segnum,
            seglen, segcrc);
        break;
    }
    return( len );
}

/****************************************************************************/
ULONG EncodeLine( FILE *fin, FILE *fout, ULONG *totalCrc )
{
    register int    b0, b1, b2;
    register ULONG  crc;
    register int    bufpos, len, linepos, slp, chksum;
    BYTE            buf[MAX_CHUNK+3];
    char            line[MAX_LINE];

    crc = *totalCrc;
    len = ReadChunk( fin, buf );
    if (len > 0) {
        buf[len] = 0;
        buf[len+1] = 0;
        buf[len+2] = 0;

        /* convert hexcode line */
        if (format==HEXCODE) {
            sprintf(line, "%06x:", filepos);
            linepos = strlen( line );
            chksum = 0;
            for (bufpos=0; bufpos<len; bufpos++, linepos+=2) {
                b0 = buf[bufpos];
                line[linepos  ] = hexcodeChar[b0 >> 4];
                line[linepos+1] = hexcodeChar[b0 & 0x0f];
                chksum += b0;
            }
            line[linepos  ] = ':';
            line[linepos+1] = hexcodeChar[(chksum >> 4) & 0x0f];
            line[linepos+2] = hexcodeChar[chksum & 0x0f];
            line[linepos+3] = '\n';
            line[linepos+4] = '\0';
            fputs(line, fout);

            for( bufpos=0; bufpos < len; bufpos++ ) {
                crc = ((crc>>8) & 0x00FFFFFF) ^ crcTable[ (crc^buf[bufpos])
                    &0xFF];
            }
            *totalCrc = crc;
            filepos += len;
            return( len );
        }
            
        /* convert bcode/uucode line */
        if (format==BCODE) {
            slp = 0;
        } else {
            line[0] = uucodeChar[ len ];
            slp = 1;
        }
        for (bufpos = 0, linepos = slp; bufpos < len; bufpos+=3, linepos+=4) {
            b0 = buf[bufpos];
            b1 = buf[bufpos+1];
            b2 = buf[bufpos+2];
            if (format==BCODE) {
                line[linepos  ] = base64Char[ b0 >> 2 ];
                line[linepos+1] = base64Char[ (b0&0x03)<<4 | (b1&0xF0)>>4 ];
                line[linepos+2] = base64Char[ (b1&0x0F)<<2 | (b2&0xC0)>>6 ];
                line[linepos+3] = base64Char[ b2 & 0x3F ];
            } else {
                line[linepos  ] = uucodeChar[ b0 >> 2 ];
                line[linepos+1] = uucodeChar[ (b0&0x03)<<4 | (b1&0xF0)>>4 ];
                line[linepos+2] = uucodeChar[ (b1&0x0F)<<2 | (b2&0xC0)>>6 ];
                line[linepos+3] = uucodeChar[ b2 & 0x3F ];
            }
        }

        if (format==BCODE) {
            /* fix non-integral-length (last) line */
            switch( len%3 ) {
                case 1: line[linepos-2] = '='; /* fall though */
                case 2: line[linepos-1] = '='; /* fall though */
                case 0: break;
            }
        }
        line[linepos] = '\n';
        line[linepos+1] = '\0';
        fputs(line, fout);

        /* take crc of line data */
        for( bufpos=0; bufpos < len; bufpos++ ) {
            crc = ((crc>>8) & 0x00FFFFFF) ^ crcTable[ (crc^buf[bufpos]) &0xFF];
        }
    }
    *totalCrc = crc;
    return( len );
}

/****************************************************************************/
int ReadChunk( FILE *fin, BYTE *buf )
{
    int rem, readLen;

    rem = maxChunk;
    while( rem>0 && (readLen=fread( buf, sizeof(char), rem, fin)) > 0) {
        buf += readLen;
        rem -= readLen;
    }
    return( maxChunk - rem );
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

/******************************************************************--END--***/
