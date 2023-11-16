#include "lattice/stdio.h"
#include "libraries/dos.h"

#define FMSIZE  100	/* Amiga: not in above includes */
#define BLKSIZE 512

FILE *fopen();		/* extern */
FILE *in_fp, *out_fp;

main(argc, argv)
int argc; char *argv[];
{
	char in_fname[FMSIZE], out_fname[FMSIZE], buffer[BLKSIZE];
	int i, runlen;

	if (argc != 3)
		{
		fprintf(stderr, "\nusage: sift <filename> <n> -- \
copy to filename.out, discarding every n+1 byte\n");
		cleanup(1);
		}

	if ((in_fp = fopen(argv[1], "rb")) == NULL)
		{
		fprintf(stderr, "\nError opening <%s>\n", argv[1]);
		cleanup(2);
		}

	sscanf(argv[2], "%d", &runlen);
	if ((runlen < 1) || (runlen > BLKSIZE))
		{
		fprintf(stderr, "\nrun length out of range\n");
		cleanup(3);
		}

	strcpy (out_fname, argv[1]);
	strcat (out_fname, ".out");

	printf("Creating <%s>\n", out_fname);
	if ((out_fp = fopen(out_fname, "wb")) == NULL)
		{
		fprintf(stderr, "\nError opening <%s>\n", out_fname);
		cleanup(4);
		}

	while (!feof(in_fp)) {
		i = fread (buffer, 1, runlen, in_fp);

		if (fwrite(buffer, 1, i, out_fp) != i)
			{
			fprintf(stderr, "\nError writing file <%s>\n", out_fname);
			cleanup(5);
			}

		i = fread (buffer, 1, 1, in_fp);	/* skip this byte */
		}

	cleanup(0);
}

cleanup (exitval)
int exitval;
{
	if (in_fp)  fclose(in_fp);
	if (out_fp)  fclose(out_fp);

	exit (exitval);
}
