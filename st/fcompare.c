#include <stdio.h>
#include <fcntl.h>

#define BLKSIZE 512
#define IOERR -1

int file1, file2;

main(argc, argv)
int argc; char *argv[];
	{
	if (argc != 3)
		{
		printf("\nusage: fcompare <file1> <file2>\n");
		exit(1);
		}

	if ((file1 = open(argv[1], O_RDONLY | O_RAW)) == IOERR)
		{
		printf("\nError opening <%s>\n", argv[1]);
		exit(2);
		}

	if ((file2 = open(argv[2], O_RDONLY | O_RAW)) == IOERR)
		{
		printf("\nError opening <%s>\n", argv[2]);
		exit(3);
		}

	filecomp();

	printf("  Done\n");
	emergex(0);
	}

filecomp()	/* compare the files, a block at a time */
{
	int i, j, minlen;
	char buf1[BLKSIZE], buf2[BLKSIZE];
	int blockn = 0;

	do {
		if ((i = read(file1, buf1, BLKSIZE)) == IOERR)
			{
			printf(stderr,"\nError reading 1st file\n");
			emergex();
			}

		if ((j = read(file2, buf2, BLKSIZE)) == IOERR)
			{
			printf(stderr,"\nError reading 2nd file\n");
			emergex();
			}

		if (i < j) minlen = i;
		else minlen = j;

		if (minlen)
			bcomp(blockn, minlen, buf1, buf2);

		blockn++;
		}
	while (minlen == BLKSIZE);
}

bcomp(blockn, blockl, p1, p2)
int blockn, blockl;		/* block number and length */
register char *p1, *p2;
{
	int i, misses = 0;

	for (i=0; i<blockl; i++) {
		if (*p1++ != *p2++)  misses++;
		}
	if (misses)
		printf("Block %d has %d misses\n", blockn, misses);
}

emergex(exitid)		/* close up any open files */
int exitid;
	{
	if (close(file1) == IOERR)
		{
		printf("\nError closing 1st file\n");
		}

	if (close(file2) == IOERR)
		{
		printf("\nError closing 2nd file\n");
		}

	exit(exitid);
	}
