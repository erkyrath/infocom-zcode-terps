/***************************************************************
iff.ilbm.h
Mon Mar  7 15:37:32 WET 1988

	(C) 1988 Magnetic Scrolls Ltd

***************************************************************/
/* IFF ILBM format graphics */

typedef short WORD;
typedef unsigned short UWORD;
typedef unsigned char UBYTE;

#define ID(c1,c2,c3,c4)	((c3<<8) + c4 )

/* Bit Map HeaDer */

typedef UBYTE Masking;
#define mskNone		0
#define mskHasMask	1
#define mskHasTransparentColor	2
#define mskLasso	3

typedef UBYTE Compression;
#define cmpNone		0
#define cmpByteRun1	1

typedef struct {
	UWORD	w, h;		/* width & height in pixels */
	WORD	x, y;		/* pixel position for image */
	UBYTE	nPlanes;	/* # source bitplanes */
	Masking	masking;
	Compression compression;
	UBYTE	pad1;
	UWORD	transparentColour;
	UBYTE	xAspect, yAspect;	/* aspect ratio */
	WORD	pageWidth, pageHeight;	/* source page size in pixels */
} BitMapHeader;

/* Colour MAP */

typedef struct {
	UBYTE red, green, blue, foo;
} ColourRegister;

/* GRAB */

typedef struct {
	WORD x, y;
} Point2D;

/* DEST */

typedef struct {
	UBYTE depth;
	UBYTE pad1;
	UBYTE planePick;
	UBYTE planeOnOff;
	UBYTE planeMask;
} DestMerge;

/* SPRT */

typedef UWORD SpritePrecedence;


