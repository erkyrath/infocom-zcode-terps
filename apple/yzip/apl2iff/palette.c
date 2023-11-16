/***************************************************************
palette.c
Mon Mar  7 15:35:16 WET 1988

	(C) 1988 Magnetic Scrolls Ltd

***************************************************************/
#include <stdio.h>

#include "picfile.h"

int apple_bestcol(pl,p,which)
register palette *pl;
register pixel *p;
int which;
{
  int best;
  register int i;
  unsigned long d,bestdist,tmp;
  pixel *p2;

  best = 0;
  bestdist = 9999999L;
  p2 = pl->paldata;
  for (i = 0; i < pl->size; i++, p2++) {
    switch (which) {
    case 0:
      d = 0;
      tmp = p->r - p2->r;
      if (tmp < 0)
	d -= tmp;
      else
	d += tmp;
      tmp = p->g - p2->g;
      if (tmp < 0)
	d -= tmp;
      else
	d +=tmp;
      tmp = p->b - p2->b;
      if (tmp < 0)
	d -= tmp;
      else
	d += tmp;
      break;
    case 1:
      d = 0;
      tmp = p->r - p2->r;
      if (tmp < 0)
	tmp = 0 - tmp;
      d += p->r * tmp;
      tmp = p->g - p2->g;
      if (tmp < 0)
	tmp = 0 - tmp;
      d += p->g * tmp;
      tmp = p->b - p2->b;
      if (tmp < 0)
	tmp = 0 - tmp;
      d += p->b * tmp;
      break;
    case 2:
      d = (sq(p->r - p2->r) * p->r) + (sq(p->g - p2->g) * p->g) +
        (sq(p->b - p2->b) * p->b);
      break; }
    if (d < bestdist) {
      best = i;
      bestdist = d; } }
  return(best); }

int cvt_bestcol(pl,p,start)
register palette *pl;
register pixel *p;
int start;
{
  int best;
  register int i;
  unsigned long d,bestdist;
  pixel *p2;
  best = 0;
  bestdist = 9999999L;
  p2 = &pl->paldata[start];
  for(i = start; i < pl->size; i++, p2++)
  {
    d = df(p,p2);
    if (d < bestdist)
    {
      best = i;
      bestdist = d;
    }
  }
  return(best); }

int cvt_inpal(pal,col,start)
register palette *pal;
register pixel *col;
int start;
{
  register int i;
  for (i = (pal->size - 1);(i >= start) && (!pixelcmp(col,&pal->paldata[i])); --i);
  if (i < start) return(-1); else return(i);
}

int cvt_addnew(pal,col,start)
palette *pal;
pixel *col;
int start;
{
  int i;
  if ((i = cvt_inpal(pal,col,start)) >=0) return (i);
  else 
  {
    addcol(pal,col);
    return(pal->size - 1);
  }
}
  
addcol(pal,col)
palette *pal;
pixel *col;
{
  if (pal->size > maxpal)
  {
    error("Palette too large!\n");
    exit(-1);
  }
  pal->paldata[pal->size].r = col->r;
  pal->paldata[pal->size].g = col->g;
  pal->paldata[pal->size++].b = col->b;
}

loadpalette_preserve(name, pal, preserve)
char *name;
palette *pal;
{
  int i, sz;
  picfile *pfp;
  pixel *paldat;
  paldat = pal->paldata;
  sz = pal->size;
  if ((pfp = picfopen(name, "r")) != NULL) {
    i = pfp->x;
    while (i--) {
      getpixel(pfp, &paldat[sz]);
      sz++; }
    pal->size = sz; } }

loadpalette(name,pal)
char name[];
palette *pal;

{
picfile *pf;

  if ((pf = picfopen(name,"r")) !=NULL)
  {
    pal->size = pf->x;
    getline(pf,pal->paldata);
    picfclose(pf);
  } else {
    error("Can't load palette %s\n",name);
    exit(-1);
  }
}

load_apple_palette(pal, preserve)
palette *pal;
int preserve;
{
  char *str; 
  char palnam[200];
  strcpy(palnam, "apple.pal");
  if (preserve)
    return(loadpalette_preserve(palnam, pal));
  else
    return(loadpalette(palnam, pal)); }
