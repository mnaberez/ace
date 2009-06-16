/*  pbmtovbm version 1.99  -  by Craig Bruce  -  24-April-1995.
**
**  This program will convert PBM, PBM-Raw, and XBM bitmap graphic files into
**  the VBM format, which is used with ACE-128/64.  This program is written
**  in ANSI C and intended to be run on a Unix system, although it may work
**  on a DOS machine.
**
**  usage: pbmtovbm [-23ucpd] [filename ...]
**
**  -2 : use version #2 VBM format
**  -3 : use version #3 VBM format (default)
**  -u : use uncompressed encoding
**  -c : use RLE-compressed encoding (default)
**  -p : output in PBM-ascii format instead of VBM format
**  -d : you are encoding files into a different directory--input and output
**       files may have the same basename
**  -help : show this help information
*/

#include <stdio.h>
#include <string.h>

#define VERSION               "1.99"
#define TRUE                  1
#define FALSE                 0
#define CHR_CR                0x0d
#define CHR_LF                0x0a
#define CHR_TAB               0x09
#define CHR_SPACE             0x20
#define VBMFMT_VERSION2       0x02
#define VBMFMT_VERSION3       0x03
#define VBMFMT_UNCOMPRESSED   0x00
#define VBMFMT_RLE_COMPRESSED 0x01
#define VBMFMT_REP_CODE       0x31
#define VBMFMT_ZERO_CODE      0x8C
#define VBMFMT_FF_CODE        0xCC
#define VBMFMT_DBL_ZERO_CODE  0x39
#define VBMFMT_DBL_FF_CODE    0x9C
#define OUTFMT_VBM            0x01
#define OUTFMT_PBM            0x02

void    main ( int argc, char *argv[] );
void    ConvertFile( char *inname );
FILE   *GetOutputFile( char *inname );
void    ExtractBasename( char *filename, char pathname[] );
void    StripExtension( char *filename, char *extension );
void    XbmConvertFile( FILE *fin, char *inname );
void    XbmConvertBody( FILE *fin, FILE *fout, int width, int height );
int     ScanWord( FILE *fin, char word[] );
void    PbmConvertFile( FILE *fin, char *inname );
void    PbmConvertBody( FILE *fin, FILE *fout, int width, int height );
void    PbmRawConvertFile( FILE *fin, char *inname );
void    PbmRawConvertBody( FILE *fin, FILE *fout, int width, int height,
            int firstChar );
int     PbmScanHeader( FILE *fin, int *width, int *height );
int     GetNextSigChar( FILE *fin );
void    VbmConvertFile( FILE *fin, char *inname );
int     VbmScanHeader( FILE *fin, int *width, int *height, char fmtstr[] );
void    VbmConvertBody( FILE *fin, FILE *fout, int width, int height );
int     VbmConvertGetByte( FILE *fin );
void    VbmPutHeader( FILE *fout, int width, int height );
void    VbmPutByte( FILE *fout, int byte, int significantBits );
void    PbmPutByte( FILE *fout, int byte, int significantBits );
void    VbmFlushCompressedByte( FILE *fout );
void    VbmPutFinish( FILE *fout );

/*** defaults ***/
int outFormat = OUTFMT_VBM;
int vbmFormat = VBMFMT_VERSION3;
int vbmEncoding = VBMFMT_RLE_COMPRESSED;

int bitvalue[] = { 1, 2, 4, 8, 16, 32, 64, 128 };  /*xbm format*/
char *progname;
int vbmRepCode     = VBMFMT_REP_CODE;
int vbmZeroCode    = VBMFMT_ZERO_CODE;
int vbmFfCode      = VBMFMT_FF_CODE;
int vbmDblZeroCode = VBMFMT_DBL_ZERO_CODE;
int vbmDblFfCode   = VBMFMT_DBL_FF_CODE;
int vbmPrevByte;
int vbmPrevCount;
int pbmOutPos;
int vbmInFormat;
int vbmInEncoding;
int vbmInRepCode;
int vbmInZeroCode;
int vbmInFfCode;
int vbmInDblZeroCode;
int vbmInDblFfCode;
int vbmInExpandByte;
int vbmInExpandCount;
int reusableFilenames;

/*****************************************************************************/
void main ( int argc, char *argv[] )
{
    int     i, j;
    int     filenameUsed;

    progname = argv[0];
    filenameUsed = FALSE;
    reusableFilenames = FALSE;
    for (i=1; i<argc; i++) {
        if (argv[i][0] == '-') {
            for (j=1; argv[i][j] != '\0'; j++) {
                switch (argv[i][j]) {
                case '2':
                    outFormat = OUTFMT_VBM;
                    vbmFormat = VBMFMT_VERSION2;
                    vbmEncoding = VBMFMT_UNCOMPRESSED;
                    break;
                case '3':
                    outFormat = OUTFMT_VBM;
                    vbmFormat = VBMFMT_VERSION3;
                    vbmEncoding = VBMFMT_RLE_COMPRESSED;
                    break;
                case 'u':
                    vbmEncoding = VBMFMT_UNCOMPRESSED;
                    break;
                case 'c':
                    outFormat = OUTFMT_VBM;
                    vbmFormat = VBMFMT_VERSION3;
                    vbmEncoding = VBMFMT_RLE_COMPRESSED;
                    break;
                case 'p':
                    outFormat = OUTFMT_PBM;
                    vbmEncoding = VBMFMT_UNCOMPRESSED;
                    break;
                case 'd':
                    reusableFilenames = TRUE;
                    break;
                default:
                    fprintf(stderr, "PBM to VBM conversion utility version ");
                    fprintf(stderr, "%s\n\n", VERSION);
                    fprintf(stderr, "usage: %s [-23ucp] ", progname);
                    fprintf(stderr, "[filename ...]\n\n", progname);
                    fprintf(stderr, "-2 : use version #2 VBM format\n");
                    fprintf(stderr, "-3 : use version #3 VBM format\n");
                    fprintf(stderr, "-u : use uncompressed encoding\n");
                    fprintf(stderr, "-c : use RLE-compressed encoding\n");
                    fprintf(stderr, "-p : output in PBM-ascii format instead");
                    fprintf(stderr, " of VBM format\n");
                    fprintf(stderr, "-d : you are encoding files into a ");
                    fprintf(stderr, "different directory--input and output\n");
                    fprintf(stderr, "     files may have the same basename\n");
                    fprintf(stderr, "-help : show this help information\n");
                    exit( 1 );
                    break;
                }
            }
        } else {
            ConvertFile( argv[i] );
            filenameUsed = TRUE;
        }
    }
    if (!filenameUsed) {
        ConvertFile( "" );
    }
}

/*****************************************************************************/
void ConvertFile( char *inname )
{
    FILE   *fin;
    int     width, height;
    char    c1, c2;

    fprintf(stderr, "converting" );
    if (strlen(inname)==0) {
        fprintf(stderr, " <stdin> to ");
        fin = stdin;
    } else {
        fprintf( stderr, " \"%s\" to ", inname );
        fin = fopen( inname, "rb" );
        if( fin == NULL ) {
            fprintf(stderr,"\n%s: Can't open input file: %s\n", progname,
                inname );
            exit( 1 );
        }
    }
    c1 = getc( fin );
    c2 = getc( fin );
    if (c1=='P' && c2=='1') {
        PbmConvertFile( fin, inname );
    } else if (c1=='P' && c2=='4') {
        PbmRawConvertFile( fin, inname );
    } else if (c1=='B' && c2=='M') {
        VbmConvertFile( fin, inname );
    } else {
        XbmConvertFile( fin, inname );
    }
    fclose( fin );
}

/*****************************************************************************/
FILE *GetOutputFile( char *inname )
{
    FILE   *fout;
    char    outname[80];

    if (strlen(inname)==0) {
        fprintf(stderr, "<stdout>");
        fout = stdout;
    } else {
        ExtractBasename( outname, inname );
        StripExtension( outname, ".xbm" );
        StripExtension( outname, ".XBM" );
        StripExtension( outname, ".bm" );
        StripExtension( outname, ".BM" );
        if (outFormat == OUTFMT_VBM || reusableFilenames) {
            StripExtension( outname, ".pbm" );
            StripExtension( outname, ".PBM" );
        }
        if (outFormat == OUTFMT_PBM || reusableFilenames) {
            StripExtension( outname, ".vbm" );
            StripExtension( outname, ".VBM" );
        }
        if (outFormat == OUTFMT_VBM) {
            strcat( outname, ".vbm" );
        } else {
            strcat( outname, ".pbm" );
        }
        fprintf( stderr, "\"%s\"", outname );

        fout = fopen( outname, "wb" );
        if( fout == NULL ) {
            fprintf(stderr,"%s: Can't open output file: %s\n", progname,
                outname );
            exit( 1 );
        }
    }
    return( fout );
}

/****************************************************************************/
void ExtractBasename( char *filename, char pathname[] )
{
    int i;

    i = strlen( pathname ) - 1;
    while (i>=0 && pathname[i]!='/' && pathname[i]!='\\' && pathname[i]!=':') {
        i--;
    }
    strcpy( filename, &pathname[i+1] );
}

/*****************************************************************************/
void StripExtension( char *filename, char *extension )
{
    int flen, elen;

    flen = strlen( filename );
    elen = strlen( extension );
    if (flen>elen && strcmp(&filename[flen-elen],extension)==0){
        filename[flen-elen] = '\0';
    }
}

/*****************************************************************************/
void XbmConvertFile( FILE *fin, char *inname )
{
    int     width, height, eof;
    FILE   *fout;

    width = 0;
    height = 0;
    eof = ScanWord( fin, "_width" );
    if (eof==EOF) {
        fprintf(stderr, "\n%s: invalid input XBM format!\n", progname);
        return;
    }
    fscanf( fin, "%d", &width );
    ScanWord( fin, "_height" );
    fscanf( fin, "%d", &height );
    ScanWord( fin, "static char" );
    eof = ScanWord( fin, "[]" );
    if (eof==EOF) {
        fprintf(stderr, "\n%s: invalid input XBM format!\n", progname);
        return;
    }
    fout = GetOutputFile( inname );
    fprintf(stderr, " : XBM, width=%d, height=%d\n", width, height );
    XbmConvertBody( fin, fout, width, height );
    fclose( fout );
}

/*****************************************************************************/
void XbmConvertBody( FILE *fin, FILE *fout, int width, int height )
{
    int     hexhi, hexlo;
    int     binhi, binlo, binval;
    int     row, col;
    int     eof;

    VbmPutHeader( fout, width, height );
    for (row=0; row<height; row++) {
        for (col=0; col<width; col+=8) {
            eof = ScanWord( fin, "0x" );
            if (eof==EOF) {
                fprintf(stderr, "\n%s: invalid input XBM format!\n", progname);
                return;
            }
            hexhi = getc( fin );
            hexlo = getc( fin );
            binhi = hexhi<='9' ? hexhi-'0' : (hexhi - 7) & 0x0F;
            binlo = hexlo<='9' ? hexlo-'0' : (hexlo - 7) & 0x0F;
            binval = binhi * 16 + binlo;
            if (col+8 >= width) {
                /* last byte of row, may have insignificant bits */
                VbmPutByte( fout, binval, (width&7)==0 ? 8 : (width&7) );
            } else {
                /* all bits are significant */
                VbmPutByte( fout, binval, 8 );
            }
        }
    }
    VbmPutFinish( fout );
}

/*****************************************************************************/
int ScanWord( FILE *fin, char word[] )
{
    int i, c;

    i = 1;
    while( word[i] != '\0' ) {
        c = getc( fin );
        if( c == EOF ) return( EOF );
        if( c == word[i] ) {
            i++;
        } else {
            i = 1;
        }
    }
    return( 'a' );
}

/*****************************************************************************/
void PbmConvertFile( FILE *fin, char *inname )
{
    int     width, height, eof;
    FILE   *fout;

    eof = PbmScanHeader( fin, &width, &height );
    if (eof==EOF) {
        fprintf(stderr, "\n%s: invalid input PBM format!\n", progname);
    } else {
        fout = GetOutputFile( inname );
        fprintf( stderr, " : PBM, width=%d, height=%d\n", width, height );
        PbmConvertBody( fin, fout, width, height );
        fclose( fout );
    }
}

/*****************************************************************************/
void PbmConvertBody( FILE *fin, FILE *fout, int width, int height )
{
    int binval;
    int row, col, bitpos, c;

    VbmPutHeader( fout, width, height );
    for (row=0; row<height; row++) {
        binval = 0x00;
        for (col=0; col<width; col++) {
            bitpos = (col & 7);  /* 01234567 */
            if (bitpos==0 && col!=0) {
                /* flush */
                VbmPutByte( fout, binval, 8 );
                binval = 0x00;
            }
            c = GetNextSigChar( fin );
            if (c==EOF) {
                fprintf(stderr, "%s: invalid input PBM format!\n", progname);
                return;
            }
            if (c=='1') {
                binval |= bitvalue[ bitpos ];
            }
        }
        /* flush the last partial byte of a column */
        VbmPutByte( fout, binval, (width&7)==0 ? 8 : (width&7) );
    }
    VbmPutFinish( fout );
}

/*****************************************************************************/
void PbmRawConvertFile( FILE *fin, char *inname )
{
    int     width, height, firstChar;
    FILE   *fout;

    firstChar = PbmScanHeader( fin, &width, &height );
    if (firstChar==EOF) {
        fprintf(stderr, "\n%s: invalid input PBM-Raw format!\n", progname);
    } else {
        fout = GetOutputFile( inname );
        fprintf(stderr, " : PBM-Raw, width=%d, height=%d\n", width, height );
        PbmRawConvertBody( fin, fout, width, height, firstChar );
        fclose( fout );
    }
}

/*****************************************************************************/
void PbmRawConvertBody( FILE *fin, FILE *fout, int width, int height,
    int firstChar )
{
    int     hexhi, hexlo;
    int     binhi, binlo, binval;
    int     row, col, i;
    int     byte, rv;
    int     useFirstCharFlag;

    /* this firstChar garbage is to handle a weakness in the pbm-raw spec */
    if (firstChar==CHR_SPACE || firstChar==CHR_TAB || firstChar==CHR_CR
            || firstChar==CHR_LF) {
        useFirstCharFlag = FALSE;
    } else {
        useFirstCharFlag = TRUE;
    }
    VbmPutHeader( fout, width, height );
    for (row=0; row<height; row++) {
        for (col=0; col<width; col+=8) {
            /* get the byte */
            if (useFirstCharFlag) {
                byte = firstChar;
                useFirstCharFlag = FALSE;
            } else {
                byte = getc( fin );
            }
            if (byte==EOF) {
                fprintf(stderr,"%s: invalid input PBM-Raw format!\n",progname);
                return;
            }

            /* reverse the order of the bits so I can reverse them back later*/
            /* (yes, you heard me correctly) */
            rv = 0;
            for( i=0; i<8; i++ ) {
                if( byte & bitvalue[i] ) {
                    rv |= bitvalue[7 - i];
                }
            }
            byte = rv;

            /* write out the byte */
            if (col+8 >= width) {
                /* last byte of row, may have insignificant bits */
                VbmPutByte( fout, byte, (width&7)==0 ? 8 : (width&7) );
            } else {
                /* all bits are significant */
                VbmPutByte( fout, byte, 8 );
            }
        }
    }
    VbmPutFinish( fout );
}

/*****************************************************************************/
int PbmScanHeader( FILE *fin, int *width, int *height )
{
    int c, i, parm;
    char digits[21];

    /* get width and height*/
    *width = 0;
    *height = 0;
    for (parm=1; parm<=2; parm++) {
        c = GetNextSigChar( fin );
        digits[0] = c;
        for (i=1; i<20; i++) {
            c = getc( fin );
            if (c<'0'||c>'9') break;
            digits[i] = c;
        }
        digits[i] = '\0';
        if (c==EOF) return( EOF );
        if (parm==1) {
            *width = atoi( digits );
        } else {
            *height = atoi( digits );
        }
    }
    return( c );
}

/*****************************************************************************/
int GetNextSigChar( FILE *fin )
{
    int c, c2;

    while (TRUE) {
        c = getc( fin );
        if (c=='#') {
            while (TRUE) {
                c = getc( fin );
                if (c==EOF || c==CHR_CR || c==CHR_LF) break;
            }
        }
        if (c==EOF) return (EOF);
        if (c!=CHR_CR && c!=CHR_LF && c!=CHR_TAB && c!=CHR_SPACE) break;
    }
    return( c );
}

/*****************************************************************************/
void VbmConvertFile( FILE *fin, char *inname )
{
    int     width, height, eof;
    char    fmtstr[4];
    FILE   *fout;

    eof = VbmScanHeader( fin, &width, &height, fmtstr );
    if (eof==EOF) {
        fprintf(stderr, "\n%s: invalid input VBM format!\n", progname);
    } else {
        fout = GetOutputFile( inname );
        fprintf(stderr, " : VBM-%s, width=%d, height=%d\n", fmtstr, width,
            height );
        VbmConvertBody( fin, fout, width, height );
        fclose( fout );
    }
}

/*****************************************************************************/
int VbmScanHeader( FILE *fin, int *width, int *height, char fmtstr[] )
{
    int c, ch, cl, i;

    *width = 0;
    *height = 0;
    strcpy( fmtstr, "Err" );
    c = getc( fin );
    if (c != 0xCB) return( EOF );
    c = getc( fin );
    if (c != VBMFMT_VERSION2 && c != VBMFMT_VERSION3) {
        fprintf(stderr,"\n%s: unrecognized VBM-file version number!",progname);
        return( EOF );
    }
    vbmInFormat = c;
    ch = getc( fin );
    cl = getc( fin );
    *width = ch * 256 + cl;
    ch = getc( fin );
    cl = getc( fin );
    *height = ch * 256 + cl;
    if (vbmInFormat == VBMFMT_VERSION2) {
        vbmInEncoding = VBMFMT_UNCOMPRESSED;
    } else {
        c = getc( fin ); /* encoding type */
        if (c!=VBMFMT_UNCOMPRESSED && c!=VBMFMT_RLE_COMPRESSED) return(EOF);
        vbmInEncoding = c;
        vbmInRepCode = getc( fin ); /* rle bytes / reserved */
        vbmInZeroCode = getc( fin );
        vbmInFfCode = getc( fin );
        vbmInDblZeroCode = getc( fin );
        vbmInDblFfCode = getc( fin );
        getc( fin ); /* reserved */
        getc( fin );
        ch = getc( fin );  /* comment length */
        cl = getc( fin );
        for (i=1; i<=ch*256+cl; i++) {
            getc( fin );  /* throw away file comment */
        }
    }
    sprintf(fmtstr, "%d%c", vbmInFormat,
        (vbmInEncoding==VBMFMT_UNCOMPRESSED) ? 'u' : 'c');
}

/*****************************************************************************/
void VbmConvertBody( FILE *fin, FILE *fout, int width, int height )
{
    int     binhi, binlo, binval;
    int     row, col, i;
    int     byte, rv;
    int     useFirstCharFlag;

    VbmPutHeader( fout, width, height );
    vbmInExpandByte = 0x00;
    vbmInExpandCount = 0;
    for (row=0; row<height; row++) {
        for (col=0; col<width; col+=8) {
            /* get the byte */
            byte = VbmConvertGetByte( fin );
            if (byte==EOF) {
                fprintf(stderr,"%s: invalid input VBM format!\n",progname);
                return;
            }

            /* reverse the order of the bits so I can reverse them back later*/
            /* (yes, you heard me correctly) */
            rv = 0;
            for( i=0; i<8; i++ ) {
                if( byte & bitvalue[i] ) {
                    rv |= bitvalue[7 - i];
                }
            }
            byte = rv;

            /* write out the byte */
            if (col+8 >= width) {
                /* last byte of row, may have insignificant bits */
                VbmPutByte( fout, byte, (width&7)==0 ? 8 : (width&7) );
            } else {
                /* all bits are significant */
                VbmPutByte( fout, byte, 8 );
            }
        }
    }
    VbmPutFinish( fout );
}

/*****************************************************************************/
int VbmConvertGetByte( FILE *fin )
{
    int c;

    if (vbmInExpandCount == 0) {
        c = getc( fin );
        vbmInExpandByte = c;
        vbmInExpandCount = 1;
        if (vbmInEncoding == VBMFMT_RLE_COMPRESSED) {
            if (c == vbmInRepCode) {
                vbmInExpandByte = getc( fin );
                vbmInExpandCount = getc( fin );
            } else if (c == vbmInZeroCode) {
                vbmInExpandByte = 0x00;
                vbmInExpandCount = getc( fin );
            } else if (c == vbmInFfCode) {
                vbmInExpandByte = 0xFF;
                vbmInExpandCount = getc( fin );
            } else if (c == vbmInDblZeroCode) {
                vbmInExpandByte = 0x00;
                vbmInExpandCount = 2;
            } else if (c == vbmInDblFfCode) {
                vbmInExpandByte = 0xFF;
                vbmInExpandCount = 2;
            }
        }
    }
    vbmInExpandCount -= 1;
    if (vbmInFormat == VBMFMT_VERSION2 || vbmInExpandByte == EOF) {
        return( vbmInExpandByte );
    } else {
        return( vbmInExpandByte^0xFF );
    }
}

/*****************************************************************************/
void VbmPutHeader( FILE *fout, int width, int height )
{
    int v;

    /* check if output format is PBM */
    if (outFormat == OUTFMT_PBM) {
        fprintf(fout,"P1\n# CREATOR: pbmtovbm version %s\n%d %d\n", VERSION,
            width, height);
        pbmOutPos = 0;
        return;
    }

    /* output format is VBM */
    putc( 'B', fout );                    /* file type identification */
    putc( 'M', fout );
    putc( 0xCB, fout );
    putc( vbmFormat, fout );              /* VBM format version */
    v = width / 256;    putc( v, fout );  /* width and height of image */
    v = width % 256;    putc( v, fout );  /*  in 16-bit H/L numbers */
    v = height / 256;   putc( v, fout );
    v = height % 256;   putc( v, fout );
    if (vbmFormat!=VBMFMT_VERSION2) {
        putc( vbmEncoding, fout );
        if (vbmEncoding==VBMFMT_RLE_COMPRESSED) {
            putc( vbmRepCode, fout );     /* code for repetitions */
            putc( vbmZeroCode, fout );    /* code for repeated zeroes */
            putc( vbmFfCode, fout );      /* code for repeated ffs */
            putc( vbmDblZeroCode, fout ); /* code for double zeroes */
            putc( vbmDblFfCode, fout );   /* code for double ffs */
        } else {
            putc( 0x00, fout );           /* reserved for uncompressed fmt */
            putc( 0x00, fout );
            putc( 0x00, fout );
            putc( 0x00, fout );
            putc( 0x00, fout );
        }
        putc( 0x00, fout );               /* 16 bits reserved */
        putc( 0x00, fout );
        putc( 0x00, fout );               /* 16-bit comment length==0 */
        putc( 0x00, fout );
        vbmPrevByte = 0x00;
        vbmPrevCount = 0;
    }
}

/*****************************************************************************/
void VbmPutByte( FILE *fout, int byte, int significantBits )
{
    int     outbyte, v;
    int     i;

    /* check if output should actually be in PBM format */
    if (outFormat==OUTFMT_PBM) {
        PbmPutByte( fout, byte, significantBits );
        return;
    }

    /* the byte is passed in in standard XBM format (i.e., backwards) */
    /* and '1' bits == black */

    /* make non-significant bits vbm-black */
    v = byte;
    if (significantBits != 8) {
        v |= 0xFF - (bitvalue[ significantBits ] - 1);
    }

    /* reverse the order of the bits to make them Commodore compatible */
    outbyte = 0x00;
    for( i=0; i<8; i++ ) {
        if( v & bitvalue[i] ) {
            outbyte |= bitvalue[7 - i];
        }
    }

    /* reverse the meanings of the bit values if not version two */
    if (vbmFormat != VBMFMT_VERSION2) {
        outbyte ^= 0xFF;
    }

    /* write the byte directly and return if uncompressed output */
    if (vbmEncoding == VBMFMT_UNCOMPRESSED) {
        putc( outbyte, fout );
        return;
    }

    /* check for RLE compression */
    if (outbyte == vbmPrevByte && vbmPrevCount<=254) {
        vbmPrevCount++;
    } else {
        VbmFlushCompressedByte( fout );
        vbmPrevByte = outbyte;
        vbmPrevCount = 1;
    }
}

/*****************************************************************************/
void PbmPutByte( FILE *fout, int byte, int significantBits )
{
    int i, c;

    for (i=0; i<significantBits; i++) {
        if( byte & bitvalue[i] ) {
            c = '1';
        } else {
            c = '0';
        }
        if (pbmOutPos >= 68) {
            fprintf(fout, "\n");
            pbmOutPos = 0;
        }
        fprintf(fout, "%c", c);
        pbmOutPos += 1;
    }
}

/*****************************************************************************/
void VbmPutFinish( FILE *fout )
{
    if (outFormat == OUTFMT_PBM) {
        fprintf(fout, "\n");
        return;
    }
    if (vbmEncoding == VBMFMT_RLE_COMPRESSED) {
        VbmFlushCompressedByte( fout );
    }
}

/*****************************************************************************/
void VbmFlushCompressedByte( FILE *fout )
{
    int i;

    if (vbmPrevCount==0) {
        return;
    }
    if ( ((vbmPrevCount==1 && (vbmPrevByte==0x00 || vbmPrevByte==0xFF))
            || (vbmPrevCount<=3 && vbmPrevByte!=0x00 && vbmPrevByte!=0xFF))
           && vbmPrevByte!=vbmRepCode && vbmPrevByte!=vbmZeroCode
            && vbmPrevByte!=vbmFfCode && vbmPrevByte!=vbmDblZeroCode
            && vbmPrevByte!=vbmDblFfCode) {
        for (i=1; i<=vbmPrevCount; i++) {
            putc( vbmPrevByte, fout );
        }
    } else {
        if (vbmPrevByte == 0x00) {
            if (vbmPrevCount == 2) {
                putc( vbmDblZeroCode, fout );
            } else {
                putc( vbmZeroCode, fout );
                putc( vbmPrevCount, fout );
            }
        } else if (vbmPrevByte == 0xFF) {
            if (vbmPrevCount == 2) {
                putc( vbmDblFfCode, fout );
            } else {
                putc( vbmFfCode, fout );
                putc( vbmPrevCount, fout );
            }
        } else {
            putc( vbmRepCode, fout );
            putc( vbmPrevByte, fout );
            putc( vbmPrevCount, fout );
        }
    }
    vbmPrevByte = 0x00;
    vbmPrevCount = 0;
}
