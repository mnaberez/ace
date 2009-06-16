#include <stdio.h>

int getline();
int atoi();

main(argc, argv)
	int argc;
	char *argv[];
{
	FILE *fin, *fout;
	char line[1000];
	int nfiles, nlines, ifile, iline;

	if (argc != 2) {
		fprintf(stderr, "usage: %s filename\n", argv[0]);
		exit( 1 );
	}
	fin = fopen( argv[1], "r" );
	if (fin == NULL) {
		fprintf(stderr, "Error opening file \"%s\".\n", argv[1]);
		exit( 1 );
	}
	getline( fin, line );
	nfiles = atoi( line );
	printf("Files to extract = %d\n", nfiles);
	for (ifile=1; ifile <= nfiles; ifile++) {
		getline( fin, line );
		printf("%d. Extracting \"%s\"", ifile, line);
		fout = fopen( line, "w" );
		if (fout == NULL) {
			fprintf(stderr, "\nUnable to open \"%s\" for output\n",
				line);
			exit(1);
		}
		getline( fin, line );
		nlines = atoi( line );
		printf(", lines=%d\n", nlines);
		for (iline=1; iline <= nlines; iline++) {
			getline( fin, line );
			fprintf(fout, "%s\n", line);
		}
		fclose( fout );
	}
}

int getline( file, line )
	register FILE *file;
	register char *line;
{
	register int c;

	while ((c=getc(file)) != '\n') {
		*line++ = c;
	}
	*line = '\0';
}
