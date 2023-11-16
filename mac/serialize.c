
/* Serialize.c		Duncan Blanchard, 9/1/88

   This utility patches a one-byte serial number into an Infocom game file.  
   The number serves as an anti-piracy device on disks sent to outside testers.
   Two possible techniques:
   
       - update serial number, and add (new - old) to lowcore checksum.
   This is the current implementation.
   
       - update serial number, and subtract (new - old) from an adjacent 
   dummy byte (or even from the high-order byte).  This would simplify
   the patching of multi-disk games.
*/

#include	<types.h>
#include	<stdio.h>
#include	<ErrMgr.h>
#include	<Errors.h>
#include	<CursorCtl.h>

/* #include <dos.h> */

#define UWORD unsigned short		/* 16 bits */
#define CHKOFF 28			/* checksum offset in game file */

main(argc, argv)
int argc;
char *argv[];
	{
	FILE *fp, *fopen();
	int i, srloff;
	UWORD srlnum, oldsrlnum, chksum;

	if (argc != 4)
		{
		fprintf(stderr, "\nusage: Serialize <filename> <srlnum (2 bytes) offset> <srlnum val (0-255)>.\n");
		exit(1);
		}

	if ((fp = fopen(argv[1], "r+")) == NULL)	/* mode = read/write */
		{
		fprintf(stderr, "\nError opening file <%s>\n", argv[1]);
		exit(2);
		}

	sscanf(argv[2], "%ld", &srloff);
	sscanf(argv[3], "%hd", &srlnum);

	/* reject new values >1 byte (complicates checksum adjustment) */
	if (srlnum > 255)
		{
		fprintf(stderr, "\nError, serial number can't exceed 255\n");
		exit(3);
		}
	readword(fp, srloff, (char *) &oldsrlnum);

	/* old value should normally be 1 or 0 -- if it's NOT, user probably
	   gave us a bad offset.  Catch error before file is written to. */
	if (oldsrlnum > 1 /*255*/ )
		{
		fprintf(stderr, "\nError, old serial number exceeds 1");
		fprintf(stderr, "\nThe 'srlnum offset' you supplied may be wrong!\n");
		exit(4);
		}
	writeword(fp, srloff, (char *) &srlnum);	/* patch in new serial number */

	readword(fp, CHKOFF, (char *) &chksum);
	chksum += (srlnum - oldsrlnum);
	writeword(fp, CHKOFF, (char *) &chksum);	/* and adjust checksum */

	fclose(fp);
	printf("Done\n");
	exit(0);
	}

readword(fp, off, bufp)
FILE *fp;
int off;
char *bufp;
	{
	if (fseek(fp, off, 0) != 0)
		{
		fprintf(stderr, "\nRSeek %d error\n", off);
		exit(11);
		}
	if (fread(bufp, 1, 2, fp) != 2)
		{
		fprintf(stderr, "\nError reading file\n");
		exit(12);
		}
	}

writeword(fp, off, bufp)
FILE *fp;
int off;
char *bufp;
	{
	if (fseek(fp, off, 0) != 0)
		{
		fprintf(stderr, "\nWSeek %d error\n", off);
		exit(13);
		}
	if (fwrite(bufp, 1, 2, fp) != 2)
		{
		fprintf(stderr, "\nError writing file\n");
		exit(14);
		}
	}

