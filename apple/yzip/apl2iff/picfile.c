/***************************************************************
picfile.c
Mon Mar  7 15:35:22 WET 1988

	(C) 1988 Magnetic Scrolls Ltd

***************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include "picfile.h"

extern char *malloc();

char toolname[40];

write_char(fil, chr)
int fil;
char chr;
{
  char buf[1];
  buf[0] = chr;
  write(fil, buf, 1); }

int get_char(fil)
int fil;
{
  unsigned char buf[1];
  read(fil, buf, 1);
  return(buf[0]); }

int get_number(fil)
int fil;
{
  unsigned char buf[2];
  read(fil, buf, 2);
  return((buf[0]<<8) + buf[1]); }

/* Call after reading 'PI' at beginning; fill in pf from info in header. */
void read_pf_header(pf)
picfile *pf;
{
  unsigned char buf[2];
  char *oname;
  int fil;
  int hsize;
  fil = pf->fp;
  read(fil, buf, 1);
  oname = malloc(buf[0] + 1);
  oname[buf[0]] = 0;
  read(fil, oname, buf[0]);
  pf->source = oname;
  if ((buf[0] + 1) & 1)
    read(fil, buf, 1);		/* Move to even byte boundary */
  hsize = get_number(fil);	/* Number of words left in the header */
  pf->x = 0;
  pf->y = 0;
  pf->origin_x = 0;
  pf->origin_y = 0;
  pf->origin_width = 0;
  pf->origin_height = 0;
  pf->origin_palette = 0;
  pf->bordered = 0;
  pf->dither_factor = 0;
  pf->written = 1;
  if (hsize--) {
    pf->is_palette = get_number(fil);
    if (hsize--) {
      pf->x = get_number(fil);
      if (hsize--) {
	pf->y = get_number(fil);
	if (hsize--) {
	  pf->palette_size = get_number(fil);
	  if (hsize--) {
	    pf->origin_x = get_number(fil);
	    if (hsize--) {
	      pf->origin_y = get_number(fil);
	      if (hsize--) {
		pf->origin_width = get_number(fil);
		if (hsize--) {
		  pf->origin_height = get_number(fil);
		  if (hsize--) {
		    pf->origin_palette = get_number(fil);
		    if (hsize--) {
		      pf->bordered = get_number(fil);
		      if (hsize--) {
			pf->dither_factor = get_number(fil);
			/* Maybe some extra crap to skip in header */
			if (hsize) {
			  lseek(fil, 2*hsize, 1); } } } } } } } } } } } } }

/* TAA 3/5/88.  Base argument is picfile structure for input file for
   this transform; not used when opening for read.  Origin_name is passed
   when producing a picfile from some other format (neo comes to mind);
   base must be zero for us to look for it, and we must be opening for
   writing.
   On open for read, the returned picfile is filled in from the file header;
   on open for write, the header is written just before the first pixel is
   written, to allow the caller to fill information in based on the transform
   he's performing. */
picfile *picfopen(name,mode,xsize,ysize,base,origin_name)
char *name,*mode,*origin_name;
picfile *base;     
int xsize,ysize;
{
  picfile *pf;
  int fil;
  int rfile;
  int idx = 0;
  int ptr;
  int fmode;
  char chr;
  char buf[7];
  char temp[14];
  char *newname;
  if (mode[0] != 'r') {
    error("Can't open %s in mode %s", name, mode);
    return(NULL); }
  if (mode[0] == 'r')
    fmode = O_RDONLY;
  pf = (picfile *)malloc(sizeof(picfile));
  pf->tbuf = malloc(default_buflen);
  pf->buf = pf->tbuf;
  pf->buflen = default_buflen;
  pf->in_buf = 0;
  pf->cur_palette = 0;
  pf->cur_stipple = 0;
  fil = open(name,fmode,0666);
  if (fil >= 0) {
    pf->fp=fil;
    switch (mode[0]) {
    case 'r':
      read(fil, buf, 2);
      if ((buf[0] != 'P') || (buf[1] != 'I')) {
	error("Not a picture file: %s\n", name);
	exit(1); }
      pf->written = 0;
      pf->pictype = 0;
      read_pf_header(pf);
      break; } }
  return (pf);
}

int picfclose(pf)
picfile *pf;
{
  int result;
  if (pf->fp < 0)
    result = 0;
  else
    result = (close(pf->fp));
  if (pf->buflen)
    free((char *)pf->tbuf);
  if (pf->cur_palette)
    free((char *)pf->cur_palette);
  if (pf->cur_stipple)
    free((char *)pf->cur_stipple);
  free((char *) pf);
  return(result);
}

set_picfile_buffer(pf, size)
picfile *pf;
int size;
{
  if (pf->buflen) {
    if ((pf->fp < 0) || (pf->buflen == size))
      return;
    free(pf->tbuf); }
  pf->in_buf = 0;
  pf->buf = 0;
  pf->buflen = size;
  if (size > 0) {
    pf->tbuf = (char *)malloc(size);
    pf->buf = pf->tbuf; } }

getpixel(pf,p)
picfile* pf;
pixel* p;
{
  char *cbuf;
  palette *pal;
  pixel *p1;
  int i, j, ib;
  char obuf[3];
  char k;
  if (pf->buflen) {
    cbuf = pf->buf;
    ib = pf->in_buf;
    if (pal = pf->cur_palette) {
      if (ib == 0) {
	j = read(pf->fp, pf->tbuf, pf->buflen);
	pf->in_buf = j;
	ib = j;
	cbuf = pf->tbuf; }
      if (pf->apple_pic == 3) {
	k = *cbuf;
	i = pf->buflen - ib;
	if ((i % 2) == 0)
	  k = (k >> 4) & 017;
	else {
	  k = k & 017;
	  cbuf++; } }
      else
	k = *cbuf++;
      ib--;
      p1 = &(pal->paldata[k]);
      p->r = p1->r;
      p->g = p1->g;
      p->b = p1->b; }
    else {
      for (i = 0; i < 3; i++) {
	if (ib == 0) {
	  j = read(pf->fp, pf->tbuf, pf->buflen);
	  pf->in_buf = j;
	  ib = j;
	  cbuf = pf->tbuf; }
	k = *cbuf++;
	ib--;
	switch (i) {
	case 0:
	  p->r = k;
	  break;
	case 1:
	  p->g = k;
	  break;
	case 2:
	  p->b = k; } } }
    pf->buf = cbuf;
    pf->in_buf = ib; }
  else {
    if (pal = pf->cur_palette) {
      read(pf->fp, obuf, 1);
      p1 = &(pal->paldata[obuf[0]]);
      p->r = p1->r;
      p->g = p1->g;
      p->b = p1->b; }
    else {
      read(pf->fp, obuf, 3);
      p->r = obuf[0];
      p->g = obuf[1];
      p->b = obuf[2]; } }
}

getline(pf,ln)
picfile *pf;
pixel *ln;
{
int i;
  for(i=0;i<pf->x;i++)
    getpixel(pf,&ln[i]);
}

error(s,a1,a2,a3,a4,a5,a6)
char s[];
int a1,a2,a3,a4,a5,a6;
{
  fprintf(stderr,"%s : ",toolname);
  fprintf(stderr,s,a1,a2,a3,a4,a5,a6);
  fprintf(stderr,"\n");
}

char *make_pic_name(old, app)
char *old, *app;
{
  int foo;
  char *new, *look;
  foo = strcspn(old, ".") + 5;
  
  if ( isdigit( (int)*old ) )
  { /* ProDOS name can't start with number */
  	foo++;
  }
  new = (char *)malloc( foo );

  look = new;
  if ( isdigit( (int)*old ) )
  { /* can't start with digit, add 'X' in front */
  	*look++ = 'X';
  }
  for ( ; *old && (*old != '.'); old++ )
  { /* look for bad ProDOS chars */
  	if ( isalnum( (int)(*old) ) )
  	{ /* good char, so copy in */
  		*look++ = *old;
	}
  }
  *look = '\0';
  strcat( new, app );
  return(new);
}
