#include <stdio.h>
#include <dos.h>

#define BLKSIZE 512
#define FMSIZE 64

char in_fname[FMSIZE], out_fname[FMSIZE]; /* used by all so make global */


/********/
/* main */
/********/

main(argc, argv)
int argc;
char *argv[];
	{
	FILE *in_fp, *out_fp, *fopen();
	long maxsize, length1, length2, addr1, addr2, addr3;
	int i;

	if (argc < 5)
		{
		fprintf(stderr, "\nusage: spl <filename> <start addr> <filler addr> <2nd start addr>.\n");
		exit(1);
		}

	if ((in_fp = fopen(argv[1], "rb")) == NULL)
		{
		fprintf(stderr, "\nError opening <%s>\n", argv[1]);
		exit(2);
		}

	sscanf(argv[2], "%ld", &addr1);
	sscanf(argv[3], "%ld", &addr2);
	sscanf(argv[4], "%ld", &addr3);

	if (addr1 > addr2 || addr1 > addr3 || addr2 > addr3)
		{
		fprintf(stderr, "\nIllegal addresses, must be addr1 < addr2 < addr3.\n");
		exit(3);
		}

	length1 = addr2 - addr1;
	length2 = addr3 - addr2;

	for (i = 0; in_fname[i] = argv[1][i]; ++i)  /* strip off type */
		if (in_fname[i] == '.')
			break;
	in_fname[i] = 0;

	sprintf(out_fname, "%s.bin", in_fname);
	printf("Creating <%s>\n", out_fname);

	if ((out_fp = fopen(out_fname, "wb")) == NULL)
		{
		fprintf(stderr, "\nError opening <%s>\n", out_fname);
		exit(4);
		}

	copy  (in_fp, out_fp, length1);
	skip (in_fp, length2);
	copyrest (in_fp, out_fp);

	fclose(out_fp);
	fclose(in_fp);
	printf("Ok, all done.\n");
	exit(0);
	}


/********/
/* copy */
/********/

copy(in_fp, out_fp, maxsize)
FILE *in_fp, *out_fp;
long maxsize;
{
	long cursize;
	int bytes, i;
	char buffer[BLKSIZE];

    cursize = 0;

    do
	{
	if ((maxsize - cursize) <= (long) BLKSIZE)
		bytes = (int) (maxsize - cursize);
	else
		bytes = BLKSIZE;

	if ((i = fread(buffer, 1, bytes, in_fp)) == 0)
		{
		fprintf(stderr, "\nError reading file <%s>\n", in_fname);
		exit(6);
		}

	if (fwrite(buffer, 1, i, out_fp) != i)
		{
		fprintf(stderr, "\nError writing file <%s>\n", out_fname);
		exit(5);
		}
	}
    while ((cursize += (long) i) < maxsize);	/* can't be greater than */
}


/********/
/* skip */
/********/

skip (in_fp, length2)
FILE *in_fp;
long length2;
{
	long i;

    for (i = 0; i < length2; ++i)
	if (fgetc (in_fp) == EOF)
		{
		fprintf(stderr, "\nError, file <%s> not as large as discard length.\n", in_fname);
		exit(7);
		}
}


/************/
/* copyrest */
/************/

copyrest(in_fp, out_fp)
FILE *in_fp, *out_fp;
{
	int bytes, i;
	char buffer[BLKSIZE];

    bytes = BLKSIZE;

    while (feof(in_fp) == 0)	/* not eof */
	{

	if ((i = fread(buffer, 1, bytes, in_fp)) == 0)
		{
		fprintf(stderr, "\nError reading file <%s>\n", in_fname);
		exit(6);
		}

	if (fwrite(buffer, 1, i, out_fp) != i)
		{
		fprintf(stderr, "\nError writing file <%s>\n", out_fname);
		exit(5);
		}
	}
}
