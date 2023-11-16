#include <stdio.h>
#include <dos.h>

#define BLKSIZE 512

main(argc, argv)
int argc;
char *argv[];
	{
	FILE *in_fp, *out_fp, *fopen();
	char in_fname[FMSIZE], out_fname[FMSIZE], buffer[BLKSIZE];
	long maxsize, cursize;
	int i, fnum, bytes;

	if (argc < 3 || argc > 4)
		{
		fprintf(stderr, "\nusage: split <filename> <max-number-bytes> [<term-char(s)>].\n");
		exit(1);
		}

	if ((in_fp = fopen(argv[1], "rb")) == NULL)
		{
		fprintf(stderr, "\nError opening <%s>\n", argv[1]);
		exit(2);
		}

	sscanf(argv[2], "%ld", &maxsize);

	if (maxsize == 0)
		{
		fprintf(stderr, "\nIllegal <max-number-bytes> %ld.\n", maxsize);
		exit(3);
		}

	for (i = 0; in_fname[i] = argv[1][i]; ++i)
		if (in_fname[i] == '.')
			break;

	in_fname[i] = 0;

	for (fnum = 0, out_fp = NULL; feof(in_fp) == 0;)
		{
		if (out_fp == NULL)
			{
			++fnum;
			sprintf(out_fname, "%s.%03d", in_fname, fnum);
			printf("Creating <%s>\n", out_fname);

			if ((out_fp = fopen(out_fname, "wb")) == NULL)
				{
				fprintf(stderr, "\nError opening <%s>\n", out_fname);
				exit(4);
				}

			cursize = 0;
			}

		if ((maxsize - cursize) <= (long) BLKSIZE)
			bytes = (int) (maxsize - cursize);
		else
			bytes = BLKSIZE;

		i = fread(buffer, 1, bytes, in_fp);

		if (fwrite(buffer, 1, i, out_fp) != i)
			{
			fprintf(stderr, "\nError writing file <%s>\n", out_fname);
			exit(5);
			}

		if ((cursize += (long) i) >= maxsize)	/* can't be greater than */
			{
			if (argc >= 4)			/* any final char? */
				fwrite(argv[3], 1, 1, out_fp);
			fclose(out_fp);
			out_fp = NULL;
			}
		}

	fclose(in_fp);
	printf("File <%s> split into %d files.\n", argv[1], fnum);

	exit(0);
	}
