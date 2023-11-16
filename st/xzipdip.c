
#include "ZIP.C"

#include <stdio.h>	/* getchar() macro */

#include "XZIP.H"

#define PFNAME "PICT.DAT"
#define NOSEEK -1
#define MAX_ICONS 60		/* make XZIPTEST fit in one screen */

#define ERROR -1	/* [move into *.h] */
#define O_RDONLY 0	/* [move into *.h] */
#define O_RAW 0

/************************************************************************
*									*
*	G R A P H I C S   G L O B A L S					*
*									*
************************************************************************/

int pfile;			/* picture file refnum */
char *pbuffer;			/* preloaded picture data */
int pbuflen;			/* length */

struct pheader {
    ZIPINT len;		/* picture file length (words) */
    ZIPINT endlod;	/* start of data (word ptr) */
    ZIPINT cksum;	/* picture file checksum */
    ZIPBYT nbsets;	/* #blocksets in I-file */
    ZIPBYT nicons;	/* #icons in I-file */
    } ph;

DIPADDR bset_table;	/* base of blockset addrs (byte offset) */
DIPADDR icon_table;

ZIPINT rev_table[256];	/* lookup table for block display */

ZIPINT bsaddr;		/* base of current blockset (word ptr) */
ZIPBYT dblock[GBLEN];	/* current block data */

ZIPBYT p_getbyt();
ZIPINT p_getwrd();
ZIPINT gs_bsaddr();

/*------------------------------*/
/*	init_pic		*/
/*------------------------------*/

init_pic()
{
    char *p;

    if ((pfile = open(PFNAME, O_RDONLY | O_RAW)) == ERROR)
	emergex("pfile open error");

    read_pfile(&ph, 0, sizeof(struct pheader));

/* Allocation of space for picture data ought, in some way, to take into 
   account the size of the code file (preload and total).  
   E.g., if everything won't fit, load only the picture header.
*/
    if (p = zalloc(ph.len * 2)) {
	pbuffer = p;
	pbuflen = ph.len * 2;
	}

/** else if (p = zalloc(ph.endlod * 2)) {	THIS CASE UNIMPLEMENTED
	}
**/
    else emergex("pfile allocation error");

    read_pfile(pbuffer, 0, pbuflen);		/* read some/all data */

    bset_table = sizeof(struct pheader);
    icon_table = bset_table + (ph.nbsets << 1);
}

/************************************************************************
*									*
*	G R A P H I C S   O P E R A T I O N S				*
*									*
************************************************************************/

/*------------------------------*/
/*	op_picinf		*/
/*------------------------------*/

op_picinf(pict, dest)
int pict;		/* number of a valid picture, or zero */
ZIPINT *dest;		/* leave results here; absolute ptr to word table */
{
    iconinfo ic1;

    if (pict > ph.nicons) {	/* out of range, return error */
	*dest++ = 0;
	*dest = 0;
	}
    else if (pict == 0) {	/* return highest picture # */
	if (ph.nicons < MAX_ICONS)
	    *dest = ph.nicons;
	else
	    *dest = MAX_ICONS;	/* [limit for XZIPTEST]
	}
    else {			/* valid, return size info */
	gs_iconinfo(pict, &ic1);

	*dest++ = ic1.width;	/* SHOULD be in pixels */
	*dest = ic1.height;
	}
}

/*------------------------------*/
/*	op_dclear		*/
/*------------------------------*/

op_dclear(		)
{

}

/*------------------------------*/
/*	op_display		*/
/*------------------------------*/

op_display(icon, locx, locy)
int icon, locx, locy;		/* icon #, location to show */
{
    iconinfo ic1;
    DIPADDR rowaddr;		/* sub-icon row address */
    int y;

    locx--;			/* XZIP: convert to zero-origin */
    locy--;

    gs_iconinfo(icon, &ic1);		/* get icon header info */
    bsaddr = gs_bsaddr(ic1.bset);	/* get blockset addr (word ptr) */

/* display the icon, looping once for each row */

    for (y=0; y<ic1.height; y++) {
      rowaddr = ic1.addr + (ic1.width * y);
      showrow(rowaddr, ic1.width, locx, locy + y);
      }
}

/*------------------------------*/
/*	showrow			*/
/*------------------------------*/

/* display a single row of icon blocks */

showrow(rowaddr, len, dxloc, dyloc)
DIPADDR rowaddr;		/* byte ptr to row data for icon */
short len,			/* row length */
    dxloc, dyloc;		/* screen position */
{
    ZIPBYT irow[SCRNX2];	/* icon row image, maximum width */
    short i;

#if ETRAP
    if (len + dxloc > SCRNX2)
      emergex("row too wide in showrow()");
#endif

/* The block ids within a row are contiguous bytes.  It's desirable to
   fetch them all at once, before we start calling gs_getblk (since it
   fetches paged data too).
*/
    for (i=0; i<len; i++)
      irow[i] = p_getbyt(rowaddr + i);	/* get icon block ids */

    for (i=0; i<len; i++) {
      gs_getblk(irow[i], dblock);	/* get block data */
      md_drawblock(dxloc + i, dyloc);	/* and display it */
      }
}

/************************************************************************
*									*
*	G R A P H I C S   S U P P O R T					*
*									*
************************************************************************/

/*------------------------------*/
/*	p_getbyt		*/
/*------------------------------*/

/* Get a byte from the picture file.  This routine may operate in as many 
   as three data environments --
     [] Full preload, if space permits.  Determined at runtime.
     [] Paged disk read.  Tricky since picture and code pages probably must
	mix.
     [] Absolute disk read.  Note that the present drawing strategy is to 
	fetch row data then blockset data alternately; may need to change 
	strategy to avoid excessive disk head motion.
*/

ZIPBYT p_getbyt(addr)
DIPADDR addr;		/* byte offset */
{
    if (addr >= pbuflen)
	emergex("out-of-range in p_getbyt");

    return((ZIPBYT)pbuffer[addr]);
}

/*------------------------------*/
/*	p_getwrd		*/
/*------------------------------*/

ZIPINT p_getwrd(addr)
DIPADDR addr;		/* byte offset */
{
    ZIPINT val;

    val = p_getbyt(addr) << 8;
    val |= p_getbyt(addr + 1);

    return(val);
}

/*------------------------------*/
/*	gs_iconinfo		*/
/*------------------------------*/

#define ICHEAD 4		/* length of icon header (bytes) */

/* pick up an icon's header information */

gs_iconinfo(icon, ic)
int icon;			/* icon number */
iconinfo *ic;			/* leave the header info here */
{
    DIPADDR loc;			/* byte ptr to table entry */
    loc = icon_table + ((icon-1) << 1);	/* index is 1-origin */

    ic->addr = loc + ICHEAD;		/* byte ptr to icon data */
    ic->bset = p_getbyt(loc);		/* blockset */
    ic->iters = p_getbyt(loc + 1);	/* number of iterations */
    ic->width = p_getbyt(loc + 2);	/* icon size */
    ic->height = p_getbyt(loc + 3);
}

/*------------------------------*/
/*	gs_bsaddr		*/
/*------------------------------*/

#define BSHEAD 1		/* length (words!) of blockset header */

/* Lookup address of given blockset.  Note that the size of each table entry
   is one word. */

ZIPINT gs_bsaddr(bset)
ZIPBYT bset;
{
    DIPADDR loc;		/* (relative) ptr to table entry */

    loc = bset_table + ((bset-1) << 1);    /* index is 1-origin */
    return(p_getwrd(loc) + BSHEAD);	/* get [word] ptr, skip header */
}

/*------------------------------*/
/*	gs_getblk		*/
/*------------------------------*/

/* Get data corresponding to the given icon-block.
   Uses global bsaddr, a word ptr to (base of) current blockset. */

gs_getblk(blk, buffer)
ZIPBYT blk,		/* block id, 1-255 */
    *buffer;		/* data or mask buffer ptr */
{
    short i;
    DIPADDR addr;

    addr = (bsaddr << 1) + (blk * GBLEN);	/* block addr (byte ptr) */

    for (i=0; i<GBLEN; i++)		/* move data to buffer */
      *buffer++ = p_getbyt(addr + i);
}

/************************************************************************
*									*
*	U T I L I T Y   R O U T I N E S					*
*									*
************************************************************************/

/*------------------------------*/
/*	rev_byte		*/
/*------------------------------*/

/* Swap bits 1-8, 2-7, etc.  At the same time double each bit, so a zero
   becomes two zeros and a one, two ones.  On the AT&T PC the ultimate
   effect is to widen the 40-column DIP to fill the 80-column display.
*/
ZIPINT rev_byte(val)
ZIPBYT val;
{
    short i;
    register ZIPINT oldval = val << 8;
    register ZIPINT newval = 0;

    for (i=0; i<8; i++) {
      newval >>= 1;
      newval |= oldval & 0x8000;	/* transfer high bit */
      newval >>= 1;
      newval |= oldval & 0x8000;	/* replicated high bit */
      oldval <<= 1;
      }
    return(newval);
}

/*------------------------------*/
/*	emergex			*/
/*------------------------------*/

emergex(str)
char *str;
{
    printf("Emergex: ", str);

    /* jump to 68K exit routine */
    while (1)
	;
}

/************************************************************************
*									*
*	G R A P H I C S,   M A C H I N E   D E P E N D E N T		*
*									*
************************************************************************/

/*------------------------------*/
/*	read_pfile		*/
/*------------------------------*/

read_pfile (dest, off, len)	/* Read raw picture file data */
char *dest; int off, len;
{
    if (off != NOSEEK) {
	if (lseek(pfile, off, 0) != off)
	    emergex("pfile seek error");
	}

    if (read(pfile, dest, len) != len)
	emergex("pfile read error");
}

/*------------------------------*/
/*	md_screen_update	*/
/*------------------------------*/

/* A DIP icon is drawn by repeated calls to ms_drawblock.  To improve
   speed on machines with complex and slow screen access, ms_drawblock may
   be drawn to a full-size alternate screen bitmap, instead of directly
   to the real screen.

   When the icon is complete, this routine is called to display the 
   changed portion of the alternate screen all at once.
*/

md_screen_update(locx, locy, width, height)
unsigned short locx, locy, width, height;
{
}

/*------------------------------*/
/*	md_drawblock		*/
/*------------------------------*/

#define RS_REPLACE 3

/* Draw the block in dblock[].  */

md_drawblock(locx, locy)
unsigned short locx, locy;	/* display coordinates */
{
    MFDB db_mfdb;
    GRECT src_area, 
	dst_area;
    ZIPINT xdblock[GBLEN];	/* expanded block data */

    register int i;
    register ZIPBYT *p1, *p2;

#if ETRAP
    if ((locx >= SCRNX2) || (locy >= SCRNY2))
      emergex("md_drawblock position out of range");
#endif

/* Atari ST graphics suggest two transformations on each byte displayed,
   depending on the current res, if pictures are to use the full screen.
     (1) expand each byte to a short (medium & high res), and
     (2) double each byte vertically (high res)
   For speed, a transformation lookup table is used.  
*/
    p2 = (ZIPBYT *) &xdblock;
    p1 = (ZIPBYT *) &dblock;
    for (i=0; i<GBLEN; i++) {
	*p2++ = *p1++;		/* left-align each byte in word field */
	*p2++ = 0;
	}

    db_mfdb.fwp = 16;		/* pixels */
    db_mfdb.fww = 1;
    db_mfdb.fh  = 8;		/* pixels */
    db_mfdb.np  = 1;		/* planes in source */
    db_mfdb.ff  = 0;		/* format is Atari-specific */
    db_mfdb.mp  = (long) &xdblock;

    src_area.g_x = 0;	src_area.g_w = 8;
    src_area.g_y = 0;	src_area.g_h = 8;

    dst_area.g_x = locx * x_char;	dst_area.g_w = 8;
    dst_area.g_y = locy * y_char;	dst_area.g_h = 8;

    rast_op(RS_REPLACE, &src_area, &db_mfdb,
			&dst_area, &scr_mfdb);
}

/*------------------------------*/
/*	draw_test		*/
/*------------------------------*/

draw_test()
{
    int i;

    init_pic();

    for (i=1; i<20; i++) {
	while (getchar() != 0x0D)
	    ;
	op_display(i, 1,1);
	}
}