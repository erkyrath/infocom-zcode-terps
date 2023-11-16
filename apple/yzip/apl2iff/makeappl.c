/*
  piccvt.c
  Copyright (c) 1988, Infocom, Inc.

  Convert a picture from Magnetic Scrolls format (as modified) to 8-bit
  pixels, with 2-255 colors.  The palette is extracted from the picture,
  and it is then (possibly) adjusted to the specified number of colors,
  defaultly 14.

  There are three methods to reduce the palette:
  1) The least-frequently-used color is replaced by the color closest to it.
  2) The two least-frequently-used colors are replaced by their weighted
     average.
  3) If the palette size is currently n, and the desired size is m, the
     least-frequently-used 2*(n-m) colors are identified.  The least-frequently-used
     color and the color closest to it in the set are replaced by their weighted
     average.  (Experimental).

  Pictures may be coerced into a supplied palette, from a Magnetic Scrolls-format
  palette file, by using the -p switch, followed by the name of the palette file.

  The palette size may be set by using the -s switch, followed by a number;
  the -g switch may also be followed by the palette size.

  -a causes conversion to an Apple II picture, which uses ~graphics/apple.pal.
  If -t is supplied before -a, various stipples used to produce light and dark gray
  are disabled.  If -h is used instead of -a, it should be followed by a small
  fix identifying an alternate method for determining the best match in the palette
  for a given color.  Currently defined are:
  0:  sum of absolute values of differences
  1:  sum of absolute values of differences, weighted by input values
  2:  sum of squares of differences, weighted by input values
  ...
  The default if -h is not supplied is just the sum of the squares of the
  differences.
  -d followed by a number will cause the picture (Apple only) to be dithered
  before conversion.  This is a failed experiment. */

#include <fcntl.h>
#include <stdio.h>

#ifdef __MSDOS__
#include <alloc.h>
#include <dos.h>
#endif

#include "picfile.h"
#include "iff-ilbm.h"

typedef struct cv_pixel {
  short r,g,b;
} cv_pixel;

#ifdef __MSDOS__
unsigned char huge *FileBuff=0L;		/* pointer to file data */
unsigned char huge *FileOffset=0L;	/* dynamic pointer to file data */
#define _fmalloc farmalloc
#define _free farfree
#else

#define far
#define _fmalloc malloc
#define _free free
#define SEEK_END 2

unsigned char *FileBuff = 0;
unsigned char *FileOffset = 0;
#endif

unsigned getWORD();
unsigned long getLONG();
unsigned char getBYTE();


palette pal;
palette applepal;
int counts[maxpal];		/* Number of uses of this color */
int ids[maxpal];		/* What to translate this color to */

int palsize = 14;
#define PALETTE_CLOSEST 0
#define PALETTE_WEIGHTED 1
#define PALETTE_FUNNY 2
int palette_method = PALETTE_CLOSEST;

int palette_trans_colors = 2;
int palette_trans_defined = 0;
pixel palette_base;
pixel palette_base_ext;
int outfile;
unsigned char far *picdat;
unsigned char far *cpd;
ColourRegister *palreg;

int apple_use_all = 0;
int apple_trans_other = 0;
int reduce_x = 1;
int apple_hack = -1;

init_palettes()
{
  int i;
  pal.size = 0;
  if (palette_trans_defined) {
    pal.size = 2;
    pal.paldata[0].r = palette_base.r;
    pal.paldata[0].g = palette_base.g;
    pal.paldata[0].b = palette_base.b;
    if (palette_trans_defined == 2) {
      pal.paldata[1].r = palette_base_ext.r;
      pal.paldata[1].g = palette_base_ext.g;
      pal.paldata[1].b = palette_base_ext.b; }
    else {
      pal.paldata[1].r = palette_base.r;
      pal.paldata[1].g = palette_base.g;
      pal.paldata[1].b = palette_base.b; } }
  else if (palette_trans_colors) {
    pal.size = 2;
    pal.paldata[0].r = 0;
    pal.paldata[0].g = 0;
    pal.paldata[0].b = 0;
    pal.paldata[1].r = 255;
    pal.paldata[1].g = 255;
    pal.paldata[1].b = 255; }
  for (i = 0; i < maxpal; i++) {
    counts[i] = 0;
    ids[i] = i; }
}

main(argc, argv)
int argc;
char **argv;
{
  char *str;
  int temp, i1, i2, i3, i;
  int extpars[4];
  extpars[0] = 0;
  extpars[1] = 0;
  extpars[2] = 0;
  extpars[3] = 0;
  _fmode = O_BINARY;      /* set the default open mode to NOT TEXT */

  if (argc >= 2) {
    argv++;			/* Skip the program name */
    argc--;
    printf("Generic Apple conversion.\n");
    init_palettes();
    load_apple_palette(&applepal, 1);
    while (argc > 0) {
      str = *argv++;
      if (str[0] == '-') {	/* A switch */
	switch (str[1]) {
	case 'K':
	case 'k':
	  printf("Colors 0 and 1 are not transparent.\n");
	  apple_trans_other = 1;
	  palette_trans_colors = 0;
	  break;
	case 'W':
	case 'w':
	  printf("Using whole apple palette.\n");
	  apple_use_all = 1;
	  break;
	case 'B':
	case 'b':
	  reduce_x = 0;
	  printf("Not reducing width.\n");
	  break;
	case 'E':
	case 'e':
	  for (i = 0; i < 4; i++) {
	    sscanf(*argv++, "%d", &extpars[i]);
	    argc--; }
	  printf("Extracting:  %d, %d, %d, %d.\n", extpars[0], extpars[1],
		 extpars[2], extpars[3]);
	  break; }  }
      else {
	do_iff_cvt(str, &pal, &extpars[0]); }
      argc--; } } }

int linebase, ap_bit, bank, ap_offset, section, apple_ct = 0;
char screen[2][0x2000];		/* main, aux */
char section_screen[0x4000];

void writebit(b)
int b;
{
  if (section) {
    if (b)
      section_screen[apple_ct] |= ap_bit;
    else
      section_screen[apple_ct] &= ap_bit ^ -1; }
  else {
    if (b)
      screen[bank][linebase+ap_offset] |= ap_bit;
    else
      screen[bank][linebase+ap_offset] &= ap_bit ^ -1; }
  ap_bit <<= 1;
  if (ap_bit == 128) {
    ap_bit = 1;
    if (bank || section) {
      bank = 0;
      apple_ct++;
      ap_offset++; }
    else
      bank = 1; } }

char stip_base[] = {0, 15, 11, 1, 5, 2, 12, 6, 3, 13, 7, 4, 14, 8, 9, 10};

write_short(outfile, num)
int outfile, num;
{
  char foo[2];
  foo[0] = (num >> 8) & 0377;
  foo[1] = num & 0377;
  write(outfile, foo, 2); }

BitMapHeader header;
int *pal_counts;
char *scanline;

do_iff_cvt(fname, rpal, extpars)
char *fname;
palette *rpal;
int *extpars;
{
  	FILE *fp, *get_iff();
 	cv_pixel *picpix, *cpp;
  	char *newname;
  	int original_palsize;
  	int n, w, pid;
  	int xmin, ymin, xmax, ymax, width, height;
  	register sbit, dbit, i, j, x, y, planes, k, cbit;
  	ColourRegister *pp;
	
  	fp = get_iff( fname );
  	if (fp < 0)
	{
    	perror(fname);
    	endit(1);
	}
  	n = getLONG(fp);
  	if (n != ID('F','O','R','M'))
	{
    	printf("not an IFF file\n");
    	endit(-1);
	}
  	n = getLONG(fp);		/* number of bytes */
  	n = getLONG(fp);
  	if (n != ID('I','L','B','M'))
	{
    	printf("not an Interleaved Bitmap file\n");
    	endit(-1);
	}
  	for (;;)
	{
    	n = getLONG(fp);
    	switch(n)
		{
    	case ID('B','M','H','D') :			/* read in the bit map header */
			n = getLONG(fp);
      	header.w = getWORD(fp);
      	header.h = getWORD(fp);
      	header.x = getWORD(fp);
      	header.y = getWORD(fp);
      	header.nPlanes = getBYTE(fp);
      	header.masking = getBYTE(fp);
      	header.compression = getBYTE(fp);
      	header.pad1 = getBYTE(fp);
      	header.transparentColour = getWORD(fp);
      	header.xAspect = getBYTE(fp);
      	header.yAspect = getBYTE(fp);
      	header.pageWidth = getWORD(fp);
      	header.pageHeight = getWORD(fp);
      	break;
    	case ID('C','M','A','P'):			/* palette */
      	n = getLONG(fp);	/* no of colours * 3 */
      	original_palsize = n / 3;
      	n = original_palsize * sizeof(ColourRegister);
      	pal_counts = (int *)malloc((n / 3) * sizeof(int));
      	for (i = 0; i < original_palsize; i++)
				pal_counts[i] = 0;
      	palreg = (ColourRegister *)malloc(n);
      	for (i = 0, pp = palreg; i < n/sizeof(ColourRegister); i++, pp++)
			{
				pp->red = getBYTE(fp);
				pp->green = getBYTE(fp);
				pp->blue = getBYTE(fp);
			}
      	if (n & 1)
				getBYTE(fp);	/* lose pad byte */
      	break;
    	case ID('B','O','D','Y'):			/* the bit maps */
      	n = getLONG(fp);
      	scanline = (char *)malloc(header.w);
      	xmin = extpars[0];
      	ymin = extpars[1];
      	xmax = (extpars[2] > 0)?extpars[2]:header.w+extpars[2]-1;
      	ymax = (extpars[3] > 0)?extpars[3]:header.h+extpars[3]-1;
      	width = 1 + xmax - xmin;
			if ( width%2 )
			{ /* don't want odd width, thank you */
				error("Width is odd - adding one to make even.\n");
				width++;
			}
      	height = 1 + ymax - ymin;
      	if (reduce_x)
				picdat = (unsigned char far *)_fmalloc((width / 2) * height);
      	else
				picdat = (unsigned char far *)_fmalloc(width * height);
      	cpd = picdat;
      	for (y = 0; y <= ymax; y++)
			{ /* For each line in picture */
				dbit = 1;	/* destination scanline bit */
				for (x = 0; x < header.w; x++)
	  				scanline[x] = 0; /* Zero the scanline array */
				for (planes=0; planes<header.nPlanes; planes++)
				{ /* For each plane */
	  				sbit = 128;
	  				for (x = 0; x < header.w; x++)
					{ /* Get a byte (maybe from RLE) */
	    				if (sbit == 128)
							w = decom(fp);
	    				if (w & sbit)
							scanline[x] |= dbit;
	    				sbit /= 2;
	    				if (sbit == 0)
							sbit = 128;
					}
	  				dbit *= 2;
				}
				if ((y >= ymin) && (y <= ymax))
				{
	  				for (x = xmin; x <= xmax; x++)
					{
	    				pid = scanline[x];
	    				if (reduce_x)
						{
	      				if (x % 2)
							{
								*cpd++ = pid;
								pal_counts[pid]++;
							}
						}
	    				else
						{
	      				*cpd++ = pid;
	      				pal_counts[pid]++;
						}
					}
				}
			}
      	/* pal_counts has the number of uses for each color in the palette.
	 			picdat has the actual data.  width and height are the dimensions
	 			of the data in picdat.
			*/
      	if (reduce_x)
				width = width / 2;
      	if ((width > 140) || (height > 192))
			{
				printf("Picture too big for apple.(%dx%d)\n",width,height);
				endit(1);
			}
      	for (i = 0; i < maxpal; i++)
				counts[i] = 0;
      	if (!apple_trans_other)
				palette_trans_colors = 2;
      	rpal->size = 2;
      	ids[0] = 0;
      	ids[1] = 0;
      	/* Make the actual palette.  ids[] contains the translations of
				the existing numbers to what will actually happen in the output.
			*/
      	for (i = palette_trans_colors; i < original_palsize; i++)
			{
				if (pal_counts[i] > 0)
				{
	  				palette_base.r = palreg[i].red*255/240;
	  				palette_base.g = palreg[i].green*255/240;
	  				palette_base.b = palreg[i].blue*255/240;
	  				j = cvt_addnew(rpal, &palette_base, palette_trans_colors);
	  				counts[j] += pal_counts[i];
	  				ids[i] = j;
				}
			}
      	generate_apple_ids();

	      /* Now translate the picture data */
   	   for (i = 0; i < width * height; i++)
				picdat[i] = ids[picdat[i]];

	      /* At this point, rpal contains the actual palette used.
		 		counts contains the counts for each color.
	 			picdat matches the palette used; color 1 is never used, color 0
				is background only.
			*/
   	   if (apple_use_all)
				newname = make_pic_name(fname, ".whl");
	      else
				newname = make_pic_name(fname, ".apl");
      	outfile = open(newname, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, 0666);
      	if (outfile < 0)
			{
				outfile = creat(newname, 0666);
				if (outfile < 0)
				{
		  			error("Can't open output file %s", newname);
		  			endit(1);
				}
			}
   	   for (i = 0; i < 2; i++)
			{
				for (j = 0; j < 0x2000; j++)
				{
	  				screen[i][j] = 0;
				}
			}
   	   fprintf( stderr, "Filename %s is a ", newname );
      	if ( height < 140 )
	      {
   	   	fprintf( stderr, "SECTION" );
				section = 1;
				write_char(outfile, width);
				write_char(outfile, height); 
   	   }
      	else
       		fprintf( stderr, "PICTURE" );

	      fprintf( stderr, ".\n");
   	   cpd = picdat;
      	apple_ct = 0;
	      ap_bit = 1;
   	   for (i = 0; i < height; i++)
			{
				linebase = 1024 * (i & 7) + 128 * ((i / 8) & 7) + 40 * (i / 64);
				if (ap_bit != 1)
		  			apple_ct++;
				ap_bit = 1;
				bank = 0;
				ap_offset = 0;
				for (j = 0; j < width; j++)
				{
	  				k = *cpd++;
	  				for (cbit = 8; cbit > 0; cbit >>= 1)
					{
		    			if (k & cbit)
							writebit(1);
	    				else
							writebit(0);
					}
				}
			}
	      if (section)
			{
				if (ap_bit != 1)
	  				apple_ct++;
				write(outfile, &section_screen[0], apple_ct);
			}
      	else
				write(outfile, &screen[0][0], 0x4000);

	      endit(0);
		case ID('C','R','N','G') :
      	getLONG(fp);
	      getLONG(fp);
   	   getLONG(fp);
      	break;
	 	case ID('D','P','P','V'):
   	case ID('C','A','M','G'):
     		n = getLONG(fp);	/* Number of bytes, not including these */
	      myseek(fp, (long)n, 1);
   	   break;
	   default:
      	printf("Weird section %c%c%c%c\n", n>>24,n>>16,n>>8,n);
      	endit(-1);
		}
	}
}

#define NORMAL 1
#define LITERAL 2
#define REPLICATE 3

int state = NORMAL;
int n, byte;

decom(fp)
int fp;
{
  register c;
  switch (header.compression)  {
  case cmpNone :
    return getBYTE(fp);
  case cmpByteRun1 :
    switch (state) {
    case NORMAL :
      c = getBYTE(fp);
      if (c & 0x80) c |= 0xffffff00;	/* sign extend */
      if (c >= 0 && c <= 127) {
	n = c+1;
	state = LITERAL;
	return decom(fp); }
      else if (c >= -127 && c <= -1) {
	n = -c + 1;
	state = REPLICATE;
	byte = getBYTE(fp);	/* byte to replicate */
	return decom(fp); }
      else	/* noop */
	return decom(fp);
    case LITERAL :
      byte = getBYTE(fp);
    case REPLICATE :
      n--;
      if (!n)
      	state = NORMAL;
      return byte;
    default :
      printf("Unknown state %d\n", state); }
    /*NOTREACHED*/
    default :
      printf("Weird decompression algorithm %d\n", header.compression); }
}

unsigned getWORD(fp)
FILE *fp;
{
  unsigned char buf[2];
  unsigned int retval;
  
  if ( FileOffset )
  {
  	retval = (*FileOffset++) << 8;
	return( retval | (*FileOffset++) );	
  }
  else
  {
  	fread( buf, 1, 2, fp );
  	return( ((buf[0] << 8) | buf[1] ) );
  }
}
unsigned long getLONG(fp)
FILE *fp;
{
  unsigned char buf[4];
  
  if ( FileOffset )
  {
	FileOffset += 2;
  	return( getWORD(NULL) );
  }
  else
  {
  	fread( buf, 1, 4, fp );
  	return( ((buf[2] << 8) | buf[3] ));
  }
}
unsigned char getBYTE(fp)
FILE *fp;
{
  char buf[1];
  if ( FileOffset )
  	return( *FileOffset++ );
  else
  {
  	fread( buf, 1, 1, fp );
  	return( buf[0] );
  }	
}


/* Generate ids[] to map colors in the input palette into the apple palette.
   If apple_use_all is set, use as many colors from the apple palette as possible;
   this preserves detail at the cost of color fidelity. */
generate_apple_ids()
{
  int i, j, k;
  int nids[32];
  int apids[16];
  int oids[16];
  int distance[16][16];
  int used_colors = 0;
  int bestdist, bestcol, unassigned_colors, free_apple_colors;
  for (i = 0; i < palette_trans_colors; i++)
    nids[i] = 0;
  if (!apple_use_all || ((pal.size - palette_trans_colors) > 16)) {
    for (i = palette_trans_colors; i < pal.size; i++) {
      if (apple_hack >= 0)
	k = apple_bestcol(&applepal, &pal.paldata[i], apple_hack);
      else
	k = cvt_bestcol(&applepal, &pal.paldata[i], 0);
      used_colors |= 1 << k;
      nids[i] = k; } }
  else {
    for (i = palette_trans_colors; i < pal.size; i++) {
      for (j = 0; j < 16; j++) {
	distance[i][j] = df(&pal.paldata[i],&applepal.paldata[j]); } }
    /* dist[i][j] is the distance between color i in the source and color j
       in the apple palette. */
    for (i = 0; i < 16; i++)
      nids[i] = -1;
    /* First find any colors that are really close matches, and assign them. */
    for (j = 0; j < 16; j++) {
      bestcol = -1;
      bestdist = 999999999;
      for (i = palette_trans_colors; i < pal.size; i++) {
	if (nids[i] >= 0) continue; /* Color already assigned */
	if (distance[i][j] < bestdist) {
	  bestdist = distance[i][j];
	  if (bestdist < 3000)
	    bestcol = i; } }	/* Best color in source for apple color j */
      if (bestcol >= 0) {
	nids[bestcol] = j;
	used_colors |= 1 << j; } }
    unassigned_colors = 0;
    free_apple_colors = 0;
    /* Now store in oids[] all the source colors that haven't been given mates
       yet.  unassigned_colors is the count. */
    for (i = palette_trans_colors; i < pal.size; i++) {
      if (nids[i] < 0) {
	oids[unassigned_colors++] = i; } }
    if (unassigned_colors) {
      /* Sort these colors in order of luminosity. */
      for (i = 0; i < (unassigned_colors - 1); i++) {
	for (j = 0; j < (unassigned_colors - i - 1); j++) {
	  if (lum(pal.paldata[oids[j]]) > lum(pal.paldata[oids[j+1]])) {
	    k = oids[j+1];
	    oids[j+1] = oids[j];
	    oids[j] = k; } } }
      /* Now do same for apple colors that haven't been assigned. */
      for (i = 0; i < 16; i++) {
	if ((used_colors & (1 << i)) == 0) {
	  apids[free_apple_colors++] = i; } }
      for (i = 0; i < free_apple_colors - 1; i++) {
	for (j = 0; j < free_apple_colors - i - 1; j++) {
	  if (lum(pal.paldata[apids[j]])>lum(pal.paldata[apids[j+1]])) {
	    k = apids[j+1];
	    apids[j+1] = apids[j];
	    apids[j] = k; } } }
      j = 0;
      k = free_apple_colors - 1;
      for (i = 0; i < unassigned_colors; i++) {
	if ((i % 2) == 0) {
	  used_colors  |= 1 << apids[j];
	  nids[oids[i / 2]] = apids[j++]; }
	else {
	  used_colors |= 1 << apids[k];
	  nids[oids[unassigned_colors - 1 - (i / 2)]] =
	    apids[k--]; } } } }
  /* nids contains the mapping between colors in the input palette and colors
     in the apple palette; ids contains the mapping between the source palette
     and the input palette. */
  for (i = 0; i < 16; i++)
    oids[i] = nids[ids[i]];
  if (palette_trans_colors && ((pal_counts[0] + pal_counts[1]) > 0)) {
    for (i = 0; i < 16; i++) {
      if ((used_colors & (1 << i)) == 0) {
	printf("Transparencies mapped to %d.\n", i);
	for (j = 0; j < palette_trans_colors; j++) {
	  ids[j] = i; }
	break; } } }
  for (i = palette_trans_colors; i < 16; i++)
    ids[i] = oids[i]; }
/*
*  read in the file into a file buffer
*/
FILE *get_iff( name )
  char *name;
{
  FILE *fp;
  long size, count;
  unsigned int handle, read_val;
#ifdef __MSDOS__
  union REGS regs, oregs;
  struct SREGS sregs;

  fp = fopen( name, "rb" );
#else
  fp = fopen(name, "r");
#endif
  fseek( fp, 0L, SEEK_END );
  size = ftell( fp );
#ifdef __MSDOS__  

  if ( (FileBuff = (char huge * ) farmalloc( size )) == NULL )
   { /* problems getting memory */
  	fprintf( stderr, "Unable to allocate FileBuff .  . .\n" );
	fprintf( stderr, "Using disk reads . . .\n" );
	return fp;
  }
/***
* Use DOS functions to open the file and read it all in, cuz we have
* a FAR pointer and fread don't like it.
***/
  if ( (handle = _open( name, O_RDONLY )) < 0 )
  {
	perror( "Unable to open file." );
	endit(1);
  }
  regs.h.ah = 0x3f;                 /* READ */
  regs.x.bx = handle;
  regs.x.cx = 0xffff;               /* in 64kb */

  for ( count = 0; count < size; count += 0xffff )
  { /* read in 64K chunks */
   regs.x.dx = FP_OFF( (FileBuff+count) );
   sregs.ds = FP_SEG( (FileBuff+count) );
  	intdosx( &regs, &oregs, &sregs );
	if ( oregs.x.cflag )
	{ /* oops, some problems */
		fprintf( stderr, "Problems reading file . . .\n");
		fprintf( stderr, "Using disk reads . . .\n" );
		farfree( (char far *)FileBuff );
		return fp;
	}
  }
  /* close up the files */
  _close( handle );
#else
  /* On Mac, just open the file, read the stuff in */
  FileBuff = (char *)malloc(size);
  if ((handle = open(name, O_RDONLY)) < 0) {
    perror(name);
    exit(1); }
  read(handle, FileBuff, size);
  close(handle);
#endif
  fclose( fp );
  
  FileOffset = FileBuff;
  return fp;
}

endit( code )
 int code;
{
	   int test;

      close(outfile);
      free(scanline);
      free(pal_counts);
		free(palreg);
      _free(picdat);
#ifdef __MSDOS__
		_free( (char far *)FileBuff );
#endif
     exit( code );
}
myseek( fp, offset, seekn )
	FILE *fp;
	long offset;
	int seekn;
{

	if ( FileOffset )
		FileOffset += offset;
	else
		lseek( fp, offset, seekn );
}

