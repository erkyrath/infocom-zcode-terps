#include "iff-ilbm.h"
#include "picfile.h"
#include <stdio.h>
#include <fcntl.h>

BitMapHeader header;

palette pal;

dump_apple_iff(newdat, fname, width, height)
unsigned char *newdat;
char *fname;
int width, height;
{
  int n, i, y, x, dbit, col, sbit;
  long int sav_ptr, len, osav_ptr, olen;
  FILE *out;
  int rwidth;
  char *newname;
  unsigned char *planes[4];
  load_apple_palette(&pal, 0);	/* Get the palette set up */
  rwidth = width * 2;
  header.w = 320;
  header.h = 200;
  header.pageWidth = 320;
  header.pageHeight = 200;
  header.x = 0;
  header.y = 0;
  header.yAspect = 11;
  header.xAspect = 10;
  header.nPlanes = 4;
  n = 4 + 8 + 20 + 8;
  n += 3 * 16;
  if (n & 1) n++;
/*  n += 8 + ((header.w + 7) / 8) * header.h * header.nPlanes; */
  newname = make_pic_name(fname, ".iff");
  out = fopen(newname, "wb+");
  fwrite("FORM", 1, 4, out);
  sav_ptr = ftell(out);		/* Save location of file size */
  putLONG( 0L, out);
  fwrite("ILBM", 1, 4, out);
  fwrite("BMHD", 1, 4, out);
  header.compression = 1;
  header.masking = mskNone;
  header.pad1 = 0;
  header.transparentColour = 0;
  putLONG( 20L, out);
  putWORD(header.w, out);
  putWORD(header.h, out);
  putWORD(header.x, out);
  putWORD(header.y, out);
  putc(header.nPlanes, out);
  putc(header.masking, out);
  putc(header.compression, out);
  putc(header.pad1, out);
  putWORD(header.transparentColour, out);
  putc(header.xAspect, out);
  putc(header.yAspect, out);
  putWORD(header.pageWidth, out);
  putWORD(header.pageHeight, out);
  fwrite("CMAP", 1, 4, out);
  osav_ptr = ftell(out);
  putLONG(0L, out);
  for (i = 0; i < pal.size; i++) {
    putc(pal.paldata[i].r*240/255, out);
    putc(pal.paldata[i].g*240/255, out);
    putc(pal.paldata[i].b*240/255, out); }
  olen = ftell(out) - osav_ptr - 4L;
  if (((3 * pal.size) & 1) != 0)
    putc(0, out);
  fseek(out, osav_ptr, 0);
  putLONG(olen, out);
  fseek(out, 0L, 2);
  for (i = 0; i < header.nPlanes; i++)
    planes[i] = (unsigned char *)malloc((header.w + 7) / 8);
  fwrite("BODY", 1, 4, out);
  osav_ptr = ftell(out);
  putLONG(0L, out);		/* For length of body */
  for (y = 0; y < height; y++) {
    for (x = 0; x < ((header.w + 7) / 8); x++) {
      for (i = 0; i < header.nPlanes; i++) {
	planes[i][x] = 0; } }
    for (x = 0, dbit = (128 | 64) ; x < rwidth; x += 2, dbit /= 4) {
      if (dbit == 0) dbit = (128 | 64);
      col = *newdat++;
      /* Do each pixel twice, to double the width */
      for (i = 0, sbit = 1; i < header.nPlanes; i++, sbit *= 2)
	if (col & sbit)
	  planes[i][x / 8] |= dbit; }
    for (i = 0; i < header.nPlanes; i++)
      write_line(planes[i], header.w, out); }
  for (x = 0; x < ((header.w + 7) / 8); x++) {
    for (i = 0; i < header.nPlanes; i++) {
      planes[i][x] = 0; } }
  for (; y < 200; y++) {
    for (i = 0; i < header.nPlanes; i++)
      write_line(planes[i], header.w, out); }
  olen = ftell(out) - osav_ptr - 4L;
  if ((olen & 1) != 0)		/* Pad to even # bytes */
    putc(0, out);
  len = ftell(out) - sav_ptr - 4L;
  fseek(out, osav_ptr, 0);
  putLONG(olen, out);
  fseek(out, sav_ptr, 0);
  putLONG(len, out);	/* Write out file length */
  fclose(out); }

write_line(plane, width, out)
unsigned char *plane;
int width;
FILE *out;
{
  int i = 0, max = ((width + 7) / 8) - 1, j;
  int lastout = -1;
  int run = 0;
  int cval, lastval, beg_run = 0;
  while (1) {
    cval = plane[i];		/* Current byte */
    if (i == 0) {		/* If first on line, just initialize stuff */
      lastval = cval;
      run = 1; }
    else {
      if (cval == lastval) {	/* Same as last one */
	run++;
	if ((run > 2) && (lastout != (beg_run - 1))) {
	  /* Dump some literals if we now have a run longer than 2 and
	     the next byte to dump isn't the beginning of the run. */
	  putc(beg_run - lastout - 2, out); /* Literal */
	  for (j = lastout + 1; j < beg_run; j++)
	    putc(plane[j], out);
	  lastout = beg_run - 1; } }
      else if (run > 2) {
	/* We have a run that just ended, so dump it */
	putc(-(run - 1), out);
	putc(lastval, out);
	lastval = cval;		/* And start a new one */
	run = 1;
	lastout = i - 1;
	beg_run = i; }
      else {
	run = 1;
	lastval = cval;
	beg_run = i; } }
    if (i == max) {
      /* We're looking at the last byte */
      if (lastout != i) {
	/* And it hasn't been dumped yet */
	if (run > 2) {
	  /* If there's a run, dump it */
	  putc(-(run - 1), out);
	  putc(lastval, out); }
	else {
	  /* Otherwise, dump some literals */
	  putc(i - lastout - 1, out);
	  for (j = lastout + 1; j <= i; j++)
	    putc(plane[j], out); } }
      break; }
    i++; } }

unsigned char input_buf[16384];

unsigned char *do_apple_cvt(fname, section)
char *fname;
int section;
{
  int fp, ilen;
  unsigned char *picdat;
  int iwidth = 140;
  int iheight = 192;
  int width, height, xmin, xmax, ymin, ymax;
  fp = open(fname, O_RDONLY);
  if (fp < 0) {
    perror(fname);
    exit(1); }
  ilen = read(fp, &input_buf[0], 16384);
  close(fp);
  if (section) {
    iwidth = input_buf[0];
    iheight = input_buf[1]; }
  xmin = 0;
  ymin = 0;
  xmax = iwidth - 1;
  ymax = iheight - 1;
  width = 1 + xmax - xmin;
  height = 1 + ymax - ymin;
  picdat = (unsigned char *)malloc(width * height);
  if (section)
    parse_apple_file(&input_buf[2], picdat, iwidth, iheight, xmin, ymin, xmax, ymax);
  else
    parse_apple_file(&input_buf[0], picdat, 0, 0, xmin, ymin, xmax, ymax);
  dump_apple_iff(picdat, fname, width, height);
  free( picdat );
  }

main(argc, argv)
int argc;
char **argv;
{
  char *str;
  int section = 1;
  argv++;
  _fmode = O_BINARY;      /* set the default open mode to NOT TEXT */
  while ( --argc )
  {
    str = *argv++;
    if (str[0] == '-')
		{
      if ((str[1] == 'p') || (str[1] == 'P'))
      {
	section = 0;
	printf("Picture.\n"); 
      }
}
    else
    {
    	printf("Making %s %s an IFF file.\n", (section?"SECTION":"PICTURE"),
    		str );
    	do_apple_cvt(str, section);
    }
  }
}  	

putWORD(n, out)
FILE *out;
{
  putc(n>>8, out);
  putc(n, out);
}

putLONG(n, out)
long int n;
FILE *out;
{
  int i, shft;
  for (i = 0, shft = 24; i < 4; i++, shft -= 8) {
    putc(n >> shft, out); } }
