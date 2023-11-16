
/************************************************************************

	Amiga YZIP Graphics Routines

	July 5, 1988	version 1.0 by Clarence K. Din
	Aug 29, 1988	GR.C working with DRIVE.C to display pictures
			from the Shogun picture file... Revision 7
	Sept 26, 1988	GR.C memory error crashes fixed... Revision 8
			(very slow picture rendering; IFF will be used)

	1-23-89		version 2.0 -- dbb
	4-13-89		added gh.scale, ge.picXD, qpr.flags
			in ShowGFX, relaxed draw_offscrn criteria 

*************************************************************************/

#include "yzip.h"

extern	WORD	first_row, last_row,	/* bounds of current YZIP window */
		first_col, last_col;
extern	WORD	cur_col, cur_row;

extern	struct	windowRec *curwp;
extern	int	Hirez;

extern	struct	Remember  *RememberKey;
extern	struct	Screen	  *ZScreen;
extern	struct	RastPort  *ZRastPort;


#define NIL		0
#define NOERR		0	/* our result convention for YZIP */

/* typedef unsigned short	*UWptr;  */
/* typedef unsigned char	*UBptr;  */

#define MAXGFXCOLORS	16	/* current max number of colors */
#define GFXPLANES	4	/* # bitplanes to support MAXGFXCOLORS */

#define GHEADLEN	16	/* length of master header */
#define LBYTES		3	/* #length bytes at each dataOff */

/* Header flags... */
#define HF_GDIR		1	/* file has a global directory */
#define HF_EHUFF	2	/* dir entries include Huff ptrs (may be 0) */
#define HF_GHUFF	4	/* file header includes global Huff ptr */
#define HF_NOPAL	8	/* no palettes exist (Apple II) */

/* Entry flags... */
#define EF_TRANS	1	/* picture uses color 0 (transparent) */
#define EF_PHUFF	2	/* picture is huffman-coded */
#define EF_XOR2		4	/* picture was XORed on alternate lines */
#define EF_MONO		8	/* two-color picture */

#define EF_IFF		32	/* picture is IFFed */

/* PICSET stuff */
/* Note: could instead get picset buflen from gfx_Header (adjusted for each 
   game), OR could set it arbitrarily large, based on available memory. */

#define	PSMAXLEN	5*1024	/* default buflen */
#define	MAXITEMLEN	1024	/* prevent loading of "too big" pics */
#define	PSCRITLEN	256	/* abort picset loading if out of space */
    

struct gHeader {	/* master header info */
	UWORD	fileID,
		flags;
	ULONG	huffOff;
	UWORD	nPics,		/* #pic entries in this file */
		ngPics,		/* #pic entries in global file */
		dirEntryLen;
	BOOL	scale;		/* if set, Amiga gfx scaled 2x horizontally */
	WORD	initSuccess;	/* flag set after inits */
};

struct gEntry {		/* directory info for current pic */
	UWORD	picID,
		picX,		/* unscaled (320 max) */
		picY,
		eFlags;
	ULONG	dataOff,
		palOff,
		huffOff;
/* additional fields for Amiga: */
	UWORD	picXD,		/* optionally scaled for display (640 max) */
		picYD;		/* (same as picY) */
};

struct gPalette {
	ULONG	palOff;			/* the one currently installed */
/**	int	palSize;	**/	/* (these now locals) */
/**	char	palData[16];	**/
};

struct gHuffTree {
	ULONG	huffOff;		/* the one currently loaded */
	char	huffTree[256];
};

struct gData {
	ULONG	minSize,	/* fully compressed size */
		midSize,	/* after dehuff, before derun */
		maxSize;	/* fully decompressed size (expected) */
	char	*pMin;		/* (points into either gd or ps buffer) */

/* for the Amiga, a separate buffer is pre-allocated, big enough for the 
   entire (compressed) image, and with enough space leftover for
   incremental decompression.  */

	char	*buffer;
	ULONG	bufLen;
};

struct gPicset {	/* picture buffering vars */
	char *basePtr;		/* overall ptr; id stuff starts here */
	LONG baseLen;		/* overall (allocated) len */
	WORD goodCount;		/* #pics currently cached */

	char *p;		/* search results returned here */
	LONG len;
	};

/*------------------------------*/
/*	gfx globals		*/
/*------------------------------*/

int	gchan;		/* gfx file handle */
UBYTE	*pDir;		/* gfx directory, in core */

/* this is a small buffer in Chip memory, for an incremental off-screen 
   bitmap (move into gData?)  */

UBYTE	*altPtr;
ULONG	altLen;

struct gHeader	gh;
struct gEntry	ge;
struct gPalette gp;
struct gHuffTree gt;
struct gData	gd;
struct gPicset	ps;


/*------------------------------*/
/*	utility calls		*/
/*------------------------------*/

/* A handy routine to seek/read within the GFX file. 
   Return 0 if no error. */

int doRead (off, len, p)	/* off == -1 means skip the seek */
int off, len; char *p;
{
	int n;

	if (off != -1) {
		n = Seek(gchan, off, -1);	/* under 4.0, LSeek? */
		if (n < 0)  return (-1);
		}

	n = Read(gchan, p, len);
	if (n != len)  return (-1);

	return (0);				/* no error */
}

int BAND (x,y)		/* do a simple bitwise AND, Pascal-style */
int x, y;
{
	return (x & y);
}

int RoundUp (n)
int n;
{
	if (n & 1)
		return(n+1);
	else	return(n);
}

int RoundDown (n)
int n;
{
	if (n & 1)
		return(n-1);
	else	return(n);
}

/*------------------------------*/
/*	InitGFX			*/
/*------------------------------*/

#define PICFILE "Pic.Data"
#define MINBUFLEN 25*1024;	/* biggest pic ~20K?; extra for decomp */
#define ALTBUFLEN  2*1024;	/* 160 bytes per full 16-color line */

int InitGFX (start)
int start;
{
	ULONG dirLen;
	UBYTE temp1[4];		/* buffer for reading Byte val(s) */
	UWORD temp2[4];		/* buffer for reading Word val(s) */

/**	gh.initSuccess = 0;
	pDir = 0;  **/		/* (automatic) */

	if (start)
	{
	/**	gh.nPics = 0;	**/		/* (default for PICINF) */

		gchan = Open (PICFILE, MODE_OLDFILE);
		if (!gchan)
			return (-1);

		if (doRead (-1, 2, &temp1))	/* get id/flags */
			return (-1);
	/**	gh.fileID = temp1[0];  **/	/* (not used on Amiga) */
		gh.flags = temp1[1];

		if (doRead (-1, 2, &temp2))	/* get global hoff, if any */
			return (-1);
		gh.huffOff = 2 * temp2[0];	/* make byte offset */

		if (doRead (-1, 4, &temp2))	/* get loc/ext pic count */
			return (-1);
		gh.nPics = temp2[0];
	/**	gh.ngPics = temp2[1]; **/	/* (not used on Amiga) */


		if (doRead (-1, 1, &temp1))	/* get dir entry size */
			return (-1);
		gh.dirEntryLen = temp1[0];

	/* alloc space for the directory, and read it in */
	/* (make sure c_getmem/RememberKey has been inited) */

		dirLen = gh.nPics * gh.dirEntryLen;
		pDir = (unsigned char *) c_getmem (dirLen);
		if (!pDir)
			return (-1);
		if (doRead (GHEADLEN, dirLen, pDir))
			return (-1);

	/* alloc space for reading picfile (and incremental decompression) */

		gd.bufLen = MINBUFLEN;
		gd.buffer = (unsigned char *) c_getmem (gd.bufLen);
		if (!gd.buffer)
			return (-1);

	/* alloc small amount of Chip mem for off-screen bitmap */

		altLen = ALTBUFLEN;
		altPtr = (unsigned char *) AllocRemember (
			&RememberKey, altLen, MEMF_CHIP);
		if (!altPtr)
			return (-1);

	/**	gp.palOff = 0;
		gt.huffOff = 0; **/		/* (automatic) */

		if (Hirez)  gh.scale = TRUE;	/* scale gfx 2x */
		gh.initSuccess = TRUE;		/* "vital" inits completed */

	/* show a splash screen if it exists (NOT IN SPEC) */
	/**	opDisplay (1, 0, 0, 0);		**/

	/* and init picset stuff (a failure here is not fatal) */
	/* if error, just return basePtr NULL	*/

		ps.baseLen = PSMAXLEN;
		ps.basePtr = (char *) c_getmem (ps.baseLen);
		ps.goodCount = 0;
	}
	else  /* cleanup */
	{
		if (gchan) {
			Close(gchan);		
			gchan = 0;
		}

	/* all mem (pDir etc) now freed thru RememberKey ... */
	}
	return(0);
}

/*------------------------------*/
/*	binary_search		*/
/*------------------------------*/

/* search a directory for a given id  [first word of each entry],
   return ptr, or null if not found */

UWORD *binary_search (dir, e_count, e_size /*words*/, id)
UWORD *dir;
int e_count, e_size, id;
{
	register int top, bot, cur;
	register int cid;
	register UWORD *p;

	bot = 0;
	top = e_count;
	while (top > bot) {
		cur = bot + ((top - bot) >> 1);	  /* DIV 2, round DOWN */
		p = &dir[cur * e_size];
		
		if ((cid = *p) == id)
			return (p);
			
		if (cid < id)
			bot = cur + 1;
		else	top = cur;
		}
	return (0);
}

/*------------------------------*/
/*	ReadGFXEntry		*/
/*------------------------------*/

void doCopy (p1, p2, n)		/* copy n bytes */
register char *p1, *p2; int n;
{
	while (n-- > 0)  *p2++ = *p1++;
}

int ReadGFXEntry (id)
int id;
{
	UBYTE *pEnt;

	if (!gh.initSuccess)
		return(-1);	/* make sure picfile opened okay */

	if (id == ge.picID)
		return (0);	/* same id as last time, done! */

	pEnt = (UBYTE *)binary_search(pDir, gh.nPics, gh.dirEntryLen/2, id);
	if (!pEnt)
		return(-1);	/* bad id */

/* Extract data from the directory buffer */

	doCopy (pEnt, &ge.picID, 8);	pEnt += 8;

/**	doCopy (pEnt, &ge.picID, 2);	pEnt += 2;
	doCopy (pEnt, &ge.picX, 2);	pEnt += 2;
	doCopy (pEnt, &ge.picY, 2);	pEnt += 2;
	doCopy (pEnt, &ge.eFlags, 2);	pEnt += 2;  **/

/* copy dataOff, palOff into 4-byte fields */

	ge.dataOff = 0;
	doCopy (pEnt, (UBYTE *)(&ge.dataOff) + 1, 3);	pEnt += 3;

	ge.palOff = 0;
	doCopy (pEnt, (UBYTE *)(&ge.palOff) + 1, 3);	pEnt += 3;

	if (BAND (gh.flags, HF_EHUFF+HF_GHUFF) == HF_EHUFF)
		{
		ge.huffOff = 0;
		doCopy (pEnt, (UBYTE *)(&ge.huffOff) + 2, 2);	pEnt += 2;
		ge.huffOff *= 2;
		}
	else	ge.huffOff = gh.huffOff;

	if (gh.scale)
		ge.picXD = ge.picX * 2;		/* scaled up */
	else	ge.picXD = ge.picX;
	ge.picYD = ge.picY;

	return(0);
}

/*------------------------------*/
/*	ReadGFXPalette		*/
/*------------------------------*/

int ReadGFXPalette ()
{
	UWORD	palSize;
	UBYTE	palData[3*MAXGFXCOLORS], temp1[4];
	int	c, n;
	int	firstId, lastId;	/* color range to install */

	if (ge.palOff && (ge.palOff != gp.palOff))
	{
		if (doRead (ge.palOff, 1, &temp1))	/* read size byte */
			return (-1);
		palSize = temp1[0];		/* (coerce) */
 
	/* since gfx never uses colors 0-1, actual max size is 14 */
		if (palSize > MAXGFXCOLORS)
			return (-1);		/* should never happen */

		if (doRead (-1, 3*palSize, &palData))	/* read in palette */
			return (-1);

		firstId = 2;  			/* gfx always skips 0-1 */
		lastId = firstId + palSize;	/* (actually last+1) */
		if (lastId > MAXGFXCOLORS)
			lastId = MAXGFXCOLORS;

/**		LoadRGB4(vp,&colortable[0],16);
		SetAPen(ZRastPort,1); **/	/* ?? */

	/* Install new colors.  Note that Amiga takes only 4 bits each 
	   for R-G-B.  The picture file has 8 bits for each, so we use the 
	   most-significant 4 (the low 4 are currently zeroes anyway).  */

		c = 0;
		for (n=firstId; n<lastId; n++)
		{
			SetRGB4(&(ZScreen->ViewPort), n,
				palData[c+0] >> 4,	/* pass 4 bits only */
				palData[c+1] >> 4,
				palData[c+2] >> 4);
			c+=3;
		}
		gp.palOff = ge.palOff;  /* remember which palette this is */
	}

/* also read new hufftree, if necessary */
  
	if (BAND(ge.eFlags, EF_PHUFF))		/* is /this/ pic huffed? */
		if (gt.huffOff != ge.huffOff)	/* tree already loaded? */
		{
			if (doRead (ge.huffOff, 256, &gt.huffTree))
				return (-1);

			gt.huffOff = ge.huffOff;
		}

	return (0);	/* all okay */
}

/*------------------------------*/
/*	ReadGFXData		*/
/*------------------------------*/

/* Read in (or find in PICSET) the data (compressed) for one picture.
   We currently use a single large buffer, to avoid multiple disk hits.  
   It's preallocated, to guarantee we won't fail due to memory
   fragmentation, etc.  (On the Amiga, this buffer is not shared with the 
   alternate BitMap, since that must be "Chip" memory.) */

int ReadGFXData ()
{
	if (searchPicset (ge.picID)) {		/* first, check cache */
		gd.minSize = ps.len;
		gd.pMin = ps.p;
		}
	else
		{
	/* read raw length bytes */

		gd.minSize = 0;
		if (doRead (ge.dataOff, LBYTES, ((UBYTE *)&gd.minSize)+1))
			return (-1);

		if (gd.minSize > gd.bufLen)
			return (-2);		/* buffer too small ... */

	/* read in the compressed data ... */

	/**	gd.pMin = altPtr + (altLen - RoundUp (gd.minSize));  **/
	/**	gd.pMin = altPtr;  **/		/* [Amiga - altPtr for BM only] */
		gd.pMin = gd.buffer;		/* use "home" buffer */

		if (doRead (-1, gd.minSize, gd.pMin))
			return (-4);
		}

/* extract intermediate length bytes (present only for huffed data, which
   we currently assume it always to be).   These bytes /are/ included in 
   minSize. */

	gd.midSize = 0;
	doCopy (gd.pMin, ((UBYTE *) &gd.midSize)+1, LBYTES);

/**	gd.minSize -= LBYTES;	**/	/* DON'T hack this */
	gd.pMin += LBYTES;		/* hack the ptr */

	return(0);
}

/*------------------------------*/
/*	ShowGFX			*/
/*------------------------------*/

/* incrementally decompress/display a picture */

struct uncompRec {	/* this block passed to UncompH */
	UBYTE	*inbuf, 
		*outbuf, 
		*huffTree;
	ULONG	picX,		/* pixel width, unscaled [320 = full] */
		midSize,	/* after unhuff, before derun */
		outLen;		/* #valid output bytes to return this pass */
	UBYTE	firstCall,	/* raised by caller, initially */
		lastCall;	/* raised by callee, when done */
	LONG	saveRegs[16];	/* private storage for callee */
};

struct quadpicRec {	/* this block passed to QuadPic */
	UWORD	srcX,		/* color ids  (2 dup pixels/byte) */
		srcY;
	UWORD	flags;		/* 1=trans, 2=bumpDest, 4=scale */
	UWORD	dstRB;		/* rowBytes, pre plane */
	UBYTE	*pSrc,
		*pDst1,		/* [updated ptrs returned here] */
		*pDst2,
		*pDst3,
		*pDst4;
};

struct BlitPicRec {	/* this block passed to BlitPic */
	struct BitMap *altBM, 
		*screenBM;
	int	sizeX,		/* src-dst sizes always equ (unlike Mac!) */
		sizeY,
		offX,		/* offset within (screen) bitmap */
		offY;
};


int ShowGFX (ypos, xpos)
int ypos, xpos;			/* absolutized */
{
	struct BitMap *Zbm;
/* unroll vars */
	UWORD subRows, sr1, sr2;	/* #rows handled per loop */
	UWORD picRB;		/* rowBytes, per plane (Amiga: scaled 2x) */
	int draw_offscrn;
/* UncompH vars */
	struct uncompRec ucr;
	ULONG uncompRet, max2;
	int transFlag;
/* QuadPic vars */
	struct quadpicRec qpr;
/* BlitPic vars */
	struct BitMap bm;
	struct BlitPicRec bpr;
	ULONG planeBytes;	/* total bytes, per plane */

/* For now we've stopped supporting the not-huffed case.  If this becomes 
   a problem, need only create an UNCOMPNH based on UncompH.  */

	if (BAND (ge.eFlags, EF_PHUFF) == 0)
		return (-1);

	if (BAND (ge.eFlags, EF_TRANS))
		transFlag = 1;
	else	transFlag = 0;

/* Note: decompH prefers an even-aligned output buffer AND an even number
   of rows.  QuadPic might, for speed, require even-aligned input/output 
   buffers.  The blitter, I think, doesn't care. */

/* SETUP FOR LOOP: */

	Zbm = ZRastPort->BitMap;

/* If we can draw directly into screen memory, we completely do away with 
   one (or two) blitter calls per increment.  QuadPic can do so if no 
   shifting or clipping is required.  (In practice, the only important case 
   of this is full screen pics.)  */

	draw_offscrn = TRUE;
/* [xpos is relative to ZWindow, whose Amiga x offset is always zero] */
	if (xpos & 0x0007 == 0)			/* no (bit) shifting? */
	  if (ge.picXD & 0x0007 == 0)		/* no (bit) clipping? */
	   if (xpos >= 0)
	    if (xpos + ge.picXD <= last_col)	/* no game clipping? */
	     if (ypos >= 0)
	      if (ypos + ge.picYD <= last_row)
	       if (Zbm->Depth == GFXPLANES)	/* correct depth? */
		draw_offscrn = FALSE;

/* (a) calc unroll increment */

/* calc decompH max output rows, in the unused tail end of gd.buffer.
   (notice if data is in picset buf, gd.buffer is actually /all/ unused).
   Allow 1 extra row for XOR, and up to 128 bytes for overruns. */

	sr1 = (((gd.bufLen - gd.minSize) - 128) / ge.picX) - 1;	
	subRows = sr1;

	if (draw_offscrn) {

	/* calc QuadPic max output rows (rowbytes even, in case QuadPic 
	   requires).
	   >>> compiler check -- be sure 2x mult not optimized away! <<< */

		picRB = ((ge.picXD + 15) / 16) * 2;	/* single plane */
		sr2 = altLen / (picRB * GFXPLANES);

		if (sr2 < sr1)			/* use the smaller */
			subRows = sr2;
		}

	if (subRows < 2)		/* not enough space */
		return (-1);

	if (subRows >= ge.picY) {	/* "small pic", do one-fell-swoop */
		subRows = ge.picY;

	/* if subRows > X, or gd.maxSize > (10K?), force to be incremental
	   (not now necessary, since gd.buflen is relatively small) */
		}
	else {				/* even, for UncompH */
	/**	if (subRows >= 4 && !draw_offscrn)
		/-* even this case looks a bit jerky *-/
			subRows = 4;
		else			**/
			subRows = 2;

	/* sr > 2 is faster, but less smooth ...  If blitting, also want to
	   minimize flashing ... */
	/**	subRows = RoundDown (subRows);  **/
		}

/* (b) setup uncompRec */

	ucr.inbuf	= gd.pMin;
	ucr.outbuf	= gd.buffer + RoundUp (gd.minSize);  /* /tail/ end */
	ucr.huffTree	= gt.huffTree;
	ucr.picX	= ge.picX;	/* pixel width [320 = full] */
	ucr.midSize	= gd.midSize;	/* after unhuff, before derun */
	ucr.outLen	= ge.picX * subRows;	/* "interrupt count" */
	ucr.firstCall	= 1;		/* raised by caller, initially */
	ucr.lastCall	= 0;		/* raised by callee, when done */

/* (c) setup our bitmap, and BlitPicRec */

	if (draw_offscrn) {
		planeBytes = picRB * subRows;

		bm.BytesPerRow	= picRB;
		bm.Rows		= subRows;
		bm.Flags	= 0;		/* ? */
		bm.Depth	= GFXPLANES;
		bm.pad		= 0;		/* (in case ever defined) */
		bm.Planes[0]	= altPtr;
		bm.Planes[1]	= bm.Planes[0] + planeBytes;
		bm.Planes[2]	= bm.Planes[1] + planeBytes;
		bm.Planes[3]	= bm.Planes[2] + planeBytes;

		bpr.altBM	= &bm;
		bpr.screenBM	= Zbm;
		bpr.sizeX	= ge.picXD;	/* sizeX is scaled */
		bpr.sizeY	= subRows;
		bpr.offX	= xpos;
		bpr.offY	= ypos;		/* /initial/ offY */
		}

/* (d) setup QuadPicRec */

	qpr.srcX	= ge.picX;	/* color ids  (2 dup pixels/byte) */
	qpr.srcY	= subRows;

	qpr.flags	= 0;
	if (transFlag)	qpr.flags |= 0x0001;	/* id 0 is transparent */
	if (gh.scale)	qpr.flags |= 0x0004;	/* scale 2x */

	qpr.dstRB	= picRB;		/* rowBytes, per plane */
	qpr.pSrc	= ucr.outbuf + ge.picX;	/* skipping XOR row */

/* Notice that because of the way QuadPic unpacks bits from the high end,
   we pass the bitplane ptrs in reversed order. */

	qpr.pDst1	= bm.Planes[3];
	qpr.pDst2	= bm.Planes[2];
	qpr.pDst3	= bm.Planes[1];
	qpr.pDst4	= bm.Planes[0];

	if (!draw_offscrn) {
		qpr.flags	|= 0x0002;	/* bump Dst ptrs */
		qpr.dstRB	= Zbm->BytesPerRow;
		qpr.pDst1	= Zbm->Planes[3];
		qpr.pDst2	= Zbm->Planes[2];
		qpr.pDst3	= Zbm->Planes[1];
		qpr.pDst4	= Zbm->Planes[0];
		}

/* TOP OF LOOP: */

	max2 = 0;
	while (!ucr.lastCall) {

/* (1) decompress a few rows into (unused sect of) gd.buffer */

		uncompRet = UncompH (&ucr);
		max2 += uncompRet;

		if (ucr.lastCall)	/* final chunk may have fewer rows */
		if (subRows < ge.picY) {	/* unless only 1 pass */
			subRows = uncompRet / ge.picX;

		/* update derivative vars */
			qpr.srcY = subRows;
		/**	bm.Rows	= subRows;  **/
			bpr.sizeY = subRows;
			}

/* (2a) if in transparent mode, first copy screen to off-screen .. */

		if (draw_offscrn && transFlag) {
			if (BlitPic (&bpr, 0))	/* NOT toSBM */
				return;		/* fell off screen, ABORT */

		/* since the blitter is asynchronous and writes into the 
		   same buffers that QuadPic will immediately read from,
		   must ensure that it finishes first! */

			WaitBlit ();
			}

/* (2) unpack the rows (into off-screen bitmap, or directly to screen) */

		QuadPic (&qpr);

/* (3) blit the rows to the main screen */

		if (draw_offscrn) {
			if (BlitPic (&bpr, 1))	/* toSBM */
				return;		/* fell off screen, ABORT */

			bpr.offY += subRows;	/* next increment */
			}
	}
/** end of loop **/

/**	if (max2 != ge.picX * ge.picY)  **/
		/* (error, but ignore -- nothing more to be done!) */

	return(0);
}

/*------------------------------*/
/*	BlitPic			*/
/*------------------------------*/

/* This routine uses the Amiga's hardware blitter to copy bits to OR from
   the screen.  Return an error if /entire/ image got clipped.

   The blitter does two things that would be painstaking for us to do 
   (in QuadPic), and probably slower too: 
	- moves an image to an arbitrary pixel alignment
	- clips an image to an arbitrary pixel width

   In certain situations that don't require the above capabilities (such as
   when drawing a full-screen pic), QuadPic can output directly to screen
   memory, completely bypassing the alternate bitmap and this blitter
   call (1 or 2 per increment).  */

int BlitPic (bp, toSBM)
register struct BlitPicRec *bp; 
int toSBM;			/* if FALSE, to altBM */
{
	int maxX, maxY;		/* space between offset and window bounds */
	int sX, sY, oX, oY;	/* local copies, for clipping & hacking */

/* DON'T hack the vertical offset to take into account the screen's 
   Title bar.  */

	oX = bp->offX;
	oY = (bp->offY) /* + Peek1*/ ;		/* 12 or whatever */
	sX = bp->sizeX;
	sY = bp->sizeY;

/* Since BltBitMap internally performs NO bounds checking, we must 
   clip carefully (we check the screen rect only; our alternate-screen 
   rect should always be valid).  
   We actually clip to the current YZIP window (per the spec).  This 
   /should/ always lie within the physical screen/window (the kernel
   ensures this). */

	maxX = last_col - (bp->offX);
	maxY = last_row - (bp->offY);		/* (note: no Peek1 ...) */

	if (sX > maxX)	sX = maxX;
	if (sY > maxY)	sY = maxY;

	if (sX <= 0 || sY <= 0)
		return (-1);	/* don't pass BltBitMap neg OR 0 */
	
	if (toSBM)
	BltBitMap (		/* (return val not useful; ignore) */
		bp->altBM, 0, 0,
		bp->screenBM, oX, oY,
		sX, sY, 
		0x00C0, 	/* logic function = "std copy" */
		0x00FF,		/* "all bitplanes" */
		altPtr		/* >>> "dummy" ptr to Chip mem <<< */
		);
	else		/* just reverse first 6 args */
	BltBitMap (
		bp->screenBM, oX, oY,
		bp->altBM, 0, 0,
		sX, sY, 
		0x00C0, 	/* logic function = "std copy" */
		0x00FF,		/* "all bitplanes" */
		altPtr		/* >>> "dummy" ptr to Chip mem <<< */
		);

	return (0);

/***	if (BAND (ge.eFlags, EF_TRANS))
		mode = 0x60;		/-* "transparent mode"  (??) *-/
	else	mode = 0xC0;

	ClipBlit (offrp, 0, 0,
		ZRastPort, xpos, ypos,
		ge.picXD, ge.picYD, mode);  ***/
}

/*------------------------------*/
/*	opDisplay		*/
/*------------------------------*/

/* position given in pixel units, 0-origin, relative to current window */
/* (result for debugging only, not used by 68K) */

int /*void*/ opDisplay (id, ypos, xpos, erase)
WORD id, ypos, xpos, erase;
{
	WORD gc;
	WORD y2, x2;	/* for Erase */

/* -2 (after 0-origining) means only read palette (NOT IMPLEMENTED) */

/* if we get a negative coordinate, don't suppress the pic, just display
   it at 0,0 (clipped if necessary) */

    /*	if (ypos < -1 || xpos < -1) 
		return (-1);		*/
	if (ypos < -1)  ypos = 0;
	if (xpos < -1)  xpos = 0;

/* -1 (after 0-origining) means use cursor position */

	if (ypos == -1)
		ypos = cur_row;
	else	ypos += first_row;

	if (xpos == -1)
		xpos = cur_col;
	else	xpos += first_col;

/* call this BEFORE (Erase or) any dy/dx references */

	if (ReadGFXEntry (id) != NOERR)
		return (-2);

/* this Erase call is used in YZT and Arthur */

	if (erase)
	{
	/**	EraseGFX (ypos, xpos);  **/

		y2 = ypos + ge.picYD;
		x2 = xpos + ge.picXD;	/* scaled */

		if (y2 > last_row)  y2 = last_row;	/* clip to cur wind */
		if (x2 > last_col)  x2 = last_col;

		erase_rect (ypos, xpos, y2, x2);
		return (0);
	}

	if (ReadGFXPalette () != NOERR)
		return (-3);
	else {
		if (ReadGFXData () != NOERR)
			return (-4);
		else {
			ShowGFX (ypos, xpos);

		/* if drawn successfully and in a scrolling window,
		   store a "gfx scroll-away count" */

			if (curwp)
			if (curwp->attr & WFSCRL) {
				gc = ypos + ge.picYD;	/* absolute bot */
				if (gc > last_row)
					gc = last_row;	/* [clipped] */
				gc -= first_row;	/* re-relativize */

			/* avoid storing if another gfx already below us! */
				if (curwp->gcnt < gc)
					curwp->gcnt = gc;
				}
			}

		}

	return(0);
}

/*------------------------------*/
/*	opPicInf		*/
/*------------------------------*/

/* Fill in the given table [2 WORDS] with size (in pixels) of the given 
   picture.  If invalid id, return a negative result. */

int opPicInf (id, tbl)
WORD id, *tbl;
{
	if (id == 0)
	{
		tbl[0] = gh.nPics;	/* highest (?) id */
		return(0);
	}

	if (ReadGFXEntry (id) == NOERR)
	{
		tbl[0] = ge.picYD;
		tbl[1] = ge.picXD;	/* [Amiga - always scaled] */
		return (0);
	}
	else  /* failed */
	{
		tbl[0] = 16;	/* but, return some defaults */
		tbl[1] = 16;	/*   for games that don't trap */
		return (-1);
	}
}

/************************************************************************
	Picset support
************************************************************************/

/*------------------------------*/
/* psRead			*/
/*------------------------------*/

/* read data for given id into buffer at p, if enough space available.
   return data length, or -1 if error, -2 if space critically low  */

LONG psRead (id, p, avail)
WORD id; char *p; LONG avail;
{
	LONG ilen;

/* get dataOff (write new routine? this one does some extra work) */
	if (ReadGFXEntry (id))
		goto PSRERR;

/* read length bytes for the (compressed) data */
	ilen = 0;
	if (doRead (ge.dataOff, LBYTES, ((UBYTE *) &ilen) + 1))
		goto PSRERR;

/* since our buffer is relatively small, we reserve it for smaller pics */
	if (ilen > MAXITEMLEN)
		goto PSRERR;

	if (ilen > avail) {		/* not enough room */
		if (avail > PSCRITLEN)
			goto PSRERR;	/* (but do keep trying) */

	/* space is critically low; since no more pics are likely to fit,
	   avoid further disk hits */
	/**	goto PSRERR2; **/
		return (-2);
		}

/* read (compressed) data into the picset buf */
	if (doRead (-1, ilen, p))
		goto PSRERR;
		
	return (ilen);		/* all okay */
PSRERR:
	return (-1);
}

/*------------------------------*/
/* getPicset			*/
/*------------------------------*/

/* Preload the requested set of pics (or as many as will fit)

   Note: PICSET currently caches /compressed/ picture data, trading off 
   space against display speed (would reverse this to do animation, 
   for example).  Also, palette data is not touched by the caching.  This 
   is okay for small pics that share someone else's palette.

   The format of the info at basePtr is as follows (every psEntry may not
   be used):
	- an array of psEntry's
	- raw data
*/

struct psEntry {	/* one entry for each pic in the set */
	WORD id;
	char *data;	/* (absolute) start of data */
	LONG len;	/* data length (in bytes) */
	};

VOID getPicset (tbl, count)
WORD *tbl; WORD count;		/* #pics in table */
{
	register struct psEntry *curEntry;
	char *curBase;
	WORD curId;
	LONG headerLen, curSize, avail;

	if (!ps.basePtr)			/* valid buffer? */
		return;
	ps.goodCount = 0;
	curEntry = (struct psEntry *) ps.basePtr;	/* initial entry */

	headerLen = sizeof(struct psEntry) * count;
	curBase = ps.basePtr + headerLen;

	avail = ps.baseLen - headerLen;		/* space remaining at curBase */
	if (avail <= 0)
		return;

	while (count-- > 0) {
		/* get next id in set */
		curId = *tbl++;		/* now guaranteed even-aligned... */

		curSize = psRead (curId, curBase, avail);
		if (curSize >= 0) {
			curEntry->id = curId;
			curEntry->data = curBase;		/* data ptr */
			curEntry->len = curSize;		/* length (bytes) */

		/* keep curBase even aligned, for max I/O speed */
			curSize = RoundUp (curSize);
			curBase += curSize;
			avail -= curSize;

			ps.goodCount++;
			curEntry += 1;		/* skip to next entry */
			}
		else
		/* if error, nothing written into header */
			{
			/* if (debug) [beep]; */

			if (curSize == -2)
				count = 0;	/* abort loop */
			}
		}
}

/*------------------------------*/
/* searchPicset			*/
/*------------------------------*/

/* Check the current picset for the given id; 
   return True if found (and return ptr/len in globals) */
  
int /*BOOL*/ searchPicset (targId)
WORD targId;
{
	register struct psEntry *curEntry;
	WORD n;

	if (!ps.basePtr)
		return (FALSE);
		
	n = ps.goodCount;			/* # cached pics */
	curEntry = (struct psEntry *) ps.basePtr;

	while (n-- > 0) {
		if (curEntry->id == targId)	/* found it, return info */
			{
			ps.p = curEntry->data;
			ps.len = curEntry->len;
			
			return (TRUE);			/* success */
			}
		else
			curEntry += 1;		/* skip to next entry */
		}
	return (FALSE);
}
