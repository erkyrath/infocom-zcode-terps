
/*------------------------------------------------------*/
/*	ATARI ST XZIP, INTERFACE ROUTINES		*/
/*------------------------------------------------------*/

/*  Modification history
    05 May 87	dbb	added mouse button handling
    02 Jun 87	dbb	changed color lru method, added _exist_file()
    24 Jun 87	dbb	fixed _char_out to handle CRs in any font
			added keypad "function keys"
    13 Jul 87	dbb	tweak_color() blinks the newly-selected color
			added delay() 
    17 Sep 87	dbb	added check for, load of Neochrome bootscreen
			FROZEN Version A
*/

#include "portab.h"
#include "obdefs.h"
#include "osbind.h"

/**			(not used by Lattice)
#include "define.h"
#include "vdibind.h"
#include "gemdefs.h" 
**/

#define VOID /**/
#define DEADCODE 1

extern _mov_mem();	/* char *p1, *p2; LONG len; */
extern _clr_mem();	/* char *p1; LONG len; */
extern _ZSTART();

/*	now use gemdos(), xbios(), etc, in osbind.h ...
EXTERN	LONG	_trap1 ();	GEMDOS interface
EXTERN	LONG	_trap2 ();	DOS extensions (GEM, GSX)
EXTERN	LONG	_trap13 ();	BIOS
EXTERN	LONG	_trap14 ();	Atari BIOS extensions
*/

/* <LATTICE NOTES>

   Naming convention: names of variables and functions which are referenced
   from the 68K section begin with "_".  Some compilers (Alcyon) automatically
   add this prefix, but Lattice does not.

   Also, names of library (OS) calls are truncated to 8 chars.  This is
   because we must compile with the "-n" option (31 char symbols instead of 8)
   to be compatible with the Lattice assembler (always 30 (?!) chars),
   but must also be compatible with the link libraries, which were apparently 
   compiled without "-n".  Aarghh.
*/

/************************************************************************/
/*	Equates and Globals						*/
/************************************************************************/

#define NOKEY 0

#define B1DOWN 0x0001	/* masks for left button */
#define B1UP 0x0000

#define ONECLICK 254	/* ZIL "function key" codes for mouse clicks */
#define TWOCLICK 253
#define NOCLICK  0
#define MIDCLICK 1	/* got one click, waiting for (potential) second */

/* Maximum interval (in 200ths sec) for double click.  The control panel DA
   has a setting for this;  where can we find it? */

#define DOUBLEWAIT 50	/* 1/4 sec, a moderate to fast value */

/**				/-* (now defined in Lattice) *-/
WORD	contrl[16];
WORD	intin[256];		/-* must be big (>160) for v_gtext *-/
WORD	ptsin[256];
WORD	intout[256];
WORD	ptsout[256];	**/

WORD	handle;			/* VDI workstation handle */
WORD	w_handle;		/* a window handle */

WORD	_wind1 = 0;		/* in Window 0 (bottom) or 1? */
WORD	_w0font = 1;		/* default fonts */
WORD	_w1font = 1;
WORD	_marg_left = 0;		/* margin widths (not location) */
WORD	_marg_right = 0;

WORD	y_res, y_char;		/* rows = y_res / y_char */
WORD	x_res, x_char;

WORD	_rows, _cur_row;	/* cur_row in 0 .. rows-1 */
WORD	_columns, _cur_column;
WORD	_split_row;		/* first row in scrolling area */

WORD	_v_inverse = 0;		/* inverse video flag */
WORD	_v_italic = 0;		/* flag for italics kludge */

MFDB	scr_mfdb, bak_mfdb;
MFDB	*scr_ptr, *bak_ptr;	/* current screen MFDB, backup MFBD */

char	pathspec[64];		/* buffers for file select dialog */
char	_filename[16];
char	_fullname[80];		/* combined buffer */

char	pathback[64];		/* buffers for old default names */
char	fileback[16];

WORD	mouse;			/* set when mouse pointer is visible */
LONG	click1_time;		/* click1 reference time (200ths), or zero */
WORD	bstatus;		/* last detected mouse button state */
WORD	_xmouse, _ymouse;	/* mouse position (char units; zero origin) */

BOOL	curs_vis = 1;		/* off to suppress our cursor (not mouse) */
BOOL	boot_vis = 0;		/* on if special boot screen was drawn */
WORD	boot_rez;		/* 0 low, 2 hi, 1 med */


/************************************************************************/
/*	Memory Routines							*/
/************************************************************************/

/*------------------------------*/
/*	md_meminit		*/
/*------------------------------*/

md_meminit()	/* should precede all allocs */
{
    allmem();		/* Lattice: call once in lifetime of program */
}

/*------------------------------*/
/*	_zalloc			*/
/*------------------------------*/

char *_zalloc(len)	/* ZIP memory allocator -- for C and 68K */
LONG len;
{
/*  Return zero if failed.  If len = -1, return available memory. */

    if (len != -1) {
	return ((char *) malloc(len));	/* compiler library call ... */
    /*  return ((char *) gemdos(0x48, len));  */	/* OS call */
	}
    else
	return ((char *) sizmem());
}

/************************************************************************/
/*	Color Routines							*/
/************************************************************************/

#define RGBLEN 3	/* length (words) of RGB info for each color index */

#define USE_DEF  1	/* use default ST color */
#define DEF_FORE 2	/* default ST foreground id = black */
#define DEF_BACK 9	/* default ST background id = white */

#define BASEID	2	/* first XZIP color id */
#define LASTID	9	/* last XZIP color id */

#define GETC 1		/* md_color: read */
#define SETC 0		/* md_color: write */

/* Note: XZIP color RGB settings use the same values as the corresponding
   1040 defaults.  The numbers below are "requested" settings from vq_color().
   "Realized" settings are (currently) less detailed (8/8 and 0/8). */

WORD color_table[8*RGBLEN] =	/* XZIP ST color settings */
	{
/*	0x003E, 0x003E, 0x003E,	*/	/* id 2 = black  (0.5 / 8) */
	0x0000, 0x0000, 0x0000, 	/*   play it safe for mono setup */
	0x03A9, 0x003E, 0x003E,		/* id 3 = red */
	0x003E, 0x032C, 0x003E,		/* id 4 = green */
	0x03A9, 0x03A9, 0x003E, 	/* id 5 = yellow */
	0x003E, 0x003E, 0x03A9,		/* id 6 = blue */
	0x03A9, 0x003E, 0x03A9,		/* id 7 = magenta */
	0x003E, 0x03A9, 0x03A9, 	/* id 8 = cyan */
	0x03A9, 0x03A9, 0x03A9	 	/* id 9 = white  (7.5 / 8) */
	};

WORD fore_ix = 1,	/* default */
     back_ix = 0;	/* this one never changes */

WORD max_index,		/* # color indices in current res (really max+1) */
     _mono = 0;

/* Map each ST color index to an XZIP color id.  Defaults are white, black,
   red, green.  (Allow for low res (16 indices max), though XZIP normally uses
   only medium (4).) */

WORD ixid_map[16] = {		/* XZIP requests */
	9, 2, 3, 4, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0 };
WORD ixus_map[16] = {		/* separate map for user choices */
	9, 2, 3, 4, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0 };

/*------------------------------*/
/*	init_colors		*/
/*------------------------------*/

WORD color_saves[16*RGBLEN];	/* desktop color settings (4 max XZIP) */

/* Save or restore the initial desktop color settings.  (In XZIP we no longer
   use them ourselves.)  */

init_colors (read)
WORD read;		/* read from OS, or write back */
{
    register WORD ix, id;
    WORD *p, *p2;

    for (ix=0; ix<max_index; ix++) {
	p = &color_saves[ix * RGBLEN];	/* saved settings for this ix */
	if (read) {
	    id = ixid_map[ix];
	    p2 = &color_table[(id-BASEID) * RGBLEN];    /* default for XZIP */

	    md_color (ix, p, GETC);		/* read old */
	    md_color (ix, p2, SETC);		/* and set ours */
	    }
	else
	    md_color (ix, p, SETC);
	}
}

/*------------------------------*/
/*	_op_color		*/
/*------------------------------*/

/* Medium resolution on the ST allows at most four different simultaneous
   colors.  We allot three for foreground (with indices 1 to 3), and the
   special index 0 for background. 

   This call will work for mono too.  Anything that's not black is mapped
   (by us, otherwise by the VDI) to white. */

WORD _op_color(id1, id2)
register WORD id1, id2;		/* foreground and background */
{
    register WORD ix;
    WORD fore_color(), back_color();

    if (id1 == USE_DEF) id1 = DEF_FORE;
    if (id2 == USE_DEF) id2 = DEF_BACK;

    if (_mono) {
	if (id1 > 2) id1 = 9;	/* map non-black to white */
	if (id2 > 2) id2 = 9;
	}
    if (id1) {
	ix = fore_color(id1);	/* setup color, get index */
	if (ix != -1) {
	    fore_ix = ix;	/* update global */
	    vst_colo/*r*/ (handle, ix);    /* and use new index for output */
	    }
	}
    if (id2) {
	ix = back_color(id2);	/* setup color (index always 0) */
	}
    return ((DEF_BACK << 8) | DEF_FORE);   /* (used by 68K init) */
}

/*------------------------------*/
/*	fore_color		*/
/*------------------------------*/

/* We use a simple lru method to recycle indices, giving XZIP "step through"
   use of /any/ number of colors.  (Works for high res too, a trivial case.)  
   Should be expanded into a full lru scheme if colors are changed often.

   Index 1, by the way, is used by the ST for the mouse pointer, and also
   for various dialog and alert boxes.  Changing the color it's associated 
   with can create problems (blending into background, odd colors).  To be
   unambiguous, we will use it to satisfy the /first/ request for a new color.
*/

WORD fore_color(id)	/* setup new color, return its index */
register WORD id;	/* XZIP id of new color */
{
    static WORD lru_index = 1;	/* least recently used index */
    register WORD ix;

    if (id < BASEID || id > LASTID) return (-1);	/* out of range */

    for (ix=1; ix<max_index; ix++)	/* desired color currently in use? */
	if (ixid_map[ix] == id) break;	/* yes, return the index */

    if (ix == max_index) {		/* no, grab an index */
	ix = lru_index;
	if (++lru_index == max_index)	/* update lru */
	    lru_index = 1;		/* and wrap if needed */

	ixid_map[ix] = id;		/* remember the new id */
	set_index(ix, id);
	}
    ixus_map[ix] = id;		/* user map "follows" XZIP map */
    return (ix);
}

/*------------------------------*/
/*	back_color		*/
/*------------------------------*/

WORD back_color(id)	/* set up new background color, return its index */
register WORD id;	/* XZIP id of new color */
{
    register WORD ix = 0;	/* always use index 0 for background (ST) */

/* Note: The VDI isn't very flexible about having more than one background
   color.  Although the format of screen memory imposes no restrictions
   at all, many of the VDI routines that draw to the screen (lines, curves,
   text) take only one color arg, and are hardwired to use index 0 for
   the background (the routine vrt_cpyfm is a notable exception).  Since
   we are working with only four colors anyway, this is probably OK.
*/
    if (id < BASEID || id > LASTID) return (-1);	/* out of range */

    if (ixid_map[ix] != id) {
	ixid_map[ix] = id;	/* remember the new id */
	set_index(ix, id);
	}
    ixus_map[ix] = id;		/* make user map "follow" XZIP map */
    return (ix);
}

/*------------------------------*/
/*	set_index		*/
/*------------------------------*/

set_index(ix, id)	/* remap a color index to a new XZIP id */
WORD ix, id;
{
    md_color (ix, &color_table[(id - BASEID) * RGBLEN],  SETC);
}

/*------------------------------*/
/*	md_color		*/
/*------------------------------*/

/* Read/write an RGB value for a given VDI color index */

md_color (ix, p, read)
WORD ix, *p;	/* 3-word RGB array */
BOOL read;
{
    WORD actual = 0;		/* return "requested" color setting */
    WORD temp[RGBLEN+1];	/* bug workaround */

    if (read) {

/* A nasty bug appears in the Lattice library call to vq_color.
   It returns a surprise copy of the index number, preceding the three
   RGB settings.  Messes up the expected alignment. */

	vq_color (handle, ix, actual, &temp);	/* read current */

	*p++ = temp[1];		/* bug: skip over slot 0 */
	*p++ = temp[2];
	*p++ = temp[3];
	}
    else
	vs_color (handle, ix, p);		/* write new */
}

/*------------------------------*/
/*	mapRGB			*/
/*------------------------------*/

/* Given a set of ST RGB values (0..7), return its GEM equivalent (0..1000).
   The formula used by Atari seems to be:
	GEM = (125 * ST) + 62	[i.e. 1/16 max .. 15/16 max]  */

mapRGB (sv, gv)
WORD sv;		/* 4 nybbles, "xRGB" */
WORD *gv;		/* 3 words */
{
    int i;
    for (i=2; i>=0; i--) {
	gv[i] = (125 * (sv & 0x000F)) + 62;
	sv >>= 4;
	}
}

/*------------------------------*/
/*	tweak_color		*/
/*------------------------------*/

WORD user_ix = 0;

/* Allows user to change a displayed color.  Shifted left-right keys cycle
   index, up-down change value.  If mono display, exchange fore and back.  */

tweak_color (code)
int code;
{
    register WORD ix, id;

    if (max_index == 2)		/* if mono, ignore arg, just swap ids */
	{
	id = ixus_map[fore_ix];			/* 1 or 0 */
	ixus_map[fore_ix] = ixus_map[back_ix];	/* 0 or 1 */
	ixus_map[back_ix] = id;

	set_index(fore_ix, ixus_map[fore_ix]);
	set_index(back_ix, ixus_map[back_ix]);
	}
    else		/* color, adjust accordingly */
	{
	if (code & 0x02) {	/* bit2 means alter a color */
	    ix = user_ix;		/* USER's current index */
	    id = ixus_map[ix];		/* and color */

	    if (code & 0x01) id--;
	    else id++;

	    if (id > LASTID) id = BASEID;	/* wrap if necessary */
	    if (id < BASEID) id = LASTID;
	
	    ixus_map[ix] = id;	/* save user's new setting */
	    set_index(ix, id);
	    }
	else {			/* otherwise just adjust the index */
	    ix = user_ix;		/* USER's current index */
	    if (code & 0x01) ix--;
	    else ix++;

	    if (ix > max_index-1) ix = 0;	/* wrap if necessary */
	    if (ix < 0) ix = max_index-1;
	    user_ix = ix;	/* save it */

/* The next lines are intended as feedback for the user.  Blink the newly-
   selected color ix, to identify it */

	    tweak_color (2);	/* (recurse) */
	    delay (20);		/* 1/10 sec */
	    tweak_color (3);
	    }
	}
}

/************************************************************************/
/*	Raster Operations						*/
/************************************************************************/

/*------------------------------*/
/*	grect_to_array		*/
/*------------------------------*/

grect_to_array (area, array)    /* convert x,y,w,h to x1,y1,x2,y2 */
GRECT	*area;
WORD	*array;
{
    array[0] = area->g_x;
    array[1] = area->g_y;
    array[2] = area->g_x + area->g_w - 1;
    array[3] = area->g_y + area->g_h - 1;
}

/*------------------------------*/
/*	scan_move		*/
/*------------------------------*/

/*  OPTIMIZED SCROLLING ROUTINE, FOR ATARI ST

  THIS ROUTINE PERFORMS A BLOCKMOVE OF SCREEN MEMORY FOR THE ATARI ST.
  ASSUMPTIONS ARE:
	() AREA MOVED MUST COVER THE FULL WIDTH OF THE SCREEN.
	() MOVE MUST BE FROM LOW TO HIGH MEMORY (SCROLL DOWN), IF IT OVERLAPS

  A BLOCKMOVE, UNLIKE THE GEM RASTER FUNCTION, TAKES ADVANTAGE OF THE
  INTERLEAVING OF COLOR PLANES IN THE ST.  IT IS MUCH FASTER, AND AVOIDS THE
  UGLY "COLOR FLASHING" EFFECT THAT OCCURS WITH GEM (MOSTLY IN LOW RES).
*/

scan_move (first, last, delta, m1, m2)
WORD first, last, delta;	/* scanline offsets */
MFDB *m1, *m2;			/* src and dest; may be the same */
{
    char *p1, *p2;
    WORD scan_len; LONG len;

    scan_len = (m1->fww << 1) * (m1->np);	/* bytes per scanline */
    len = scan_len * (last - first);		/* total bytes to move */

    p1 = (char *) m1->mp + (scan_len * first);
    p2 = (char *) m2->mp + (scan_len * (first + delta));

    if (mouse)			/* if allowed to show, mouse image "smears" */
	show_mouse(0);
    _mov_mem (p1, p2, len);	/* fast 68K blockmove */
/*  movmem (p1, p2, len); */	/* Lattice call is terribly slow .. */
    if (mouse)			/* restore the mouse pointer */
	show_mouse(1);
}

/*------------------------------*/
/*	scan_clear		*/
/*------------------------------*/

scan_clear (first, last, m2)
WORD first, last;		/* scanline offsets */
MFDB *m2;			/* target mfdb */
{
    char *p2;
    WORD scan_len; LONG len;
/*  LONG repval = 0; */

    scan_len = (m2->fww << 1) * (m2->np);	/* bytes per scanline */
    len = scan_len * (last - first);		/* total bytes to CLEAR */
    p2 = (char *) m2->mp + (scan_len * first);

    if (mouse)			/* if allowed to show, will cause garbage */
	show_mouse(0);
    _clr_mem (p2, len);				/* fast 68K clear */
/*  repmem (p2, &repval, 4, len>>2); */		/* Lattice: again, too slow */
    if (mouse)			/* restore the mouse pointer */
	show_mouse(1);
}

/*------------------------------*/
/*	rast_op			*/
/*------------------------------*/

/* This VDI call handles cases not handled by the calls above */

rast_op (mode, s_area, s_mfdb, d_area, d_mfdb)	/* bit block transfer */

WORD	mode;
GRECT	*s_area, *d_area;
MFDB	*s_mfdb, *d_mfdb;
{
	WORD	pxy[8];

	grect_to_array (s_area, &pxy[0]);
	grect_to_array (d_area, &pxy[4]);

	vro_cpyf/*m*/ (handle, mode, pxy, s_mfdb, d_mfdb);
}

/*------------------------------*/
/*	copy_screen		*/
/*------------------------------*/

copy_screen (save)
BOOL save;
{
    if (save)
	scan_move (0, y_res, 0, scr_ptr, bak_ptr);	/* to altscreen */
    else
	scan_move (0, y_res, 0, bak_ptr, scr_ptr);	/* from altscreen */
}

/************************************************************************/
/*	Screen Handling							*/
/************************************************************************/

/*------------------------------*/
/*	_clear_chars		*/
/*------------------------------*/

_clear_chars (row1, row2, column1, column2, dst_mfdb)	/* clear given area */

WORD	row1, row2, column1, column2;
MFDB	*dst_mfdb;
{
	GRECT	clear_area;
	WORD	mode;

	clear_area.g_x = column1 * x_char;
	clear_area.g_w = (column2 - column1) * x_char;

	clear_area.g_y = row1 * y_char;
	clear_area.g_h = (row2 - row1) * y_char;

	if (_v_inverse) 
		mode = 15;	/* fill with foreground color */
	else mode = 0;		/* fill with background color */

	rast_op (mode, &clear_area, dst_mfdb,
			&clear_area, dst_mfdb);
}

/*------------------------------*/
/*	_clear_eol		*/
/*------------------------------*/

_clear_eol ()
{
	_clear_chars (_cur_row, _cur_row + 1,
			_cur_column, _columns, scr_ptr);
}

/*------------------------------*/
/*	_clear_lines		*/
/*------------------------------*/

/* We can use our 68K routine for this one; it's faster than the VDI and
   avoids the ugly "color flashing" (most visibly when clearing large areas).
*/

_clear_lines (row1, row2)
WORD row1, row2;
{
	scan_clear (row1 * y_char, row2 * y_char, scr_ptr);
}

/*------------------------------*/
/*	_clear_screen		*/
/*------------------------------*/

_clear_screen ()
{
	_clear_lines (0, _rows);
}

/*------------------------------*/
/*	line_scroll		*/
/*------------------------------*/

line_scroll ()
{
	WORD	first, last;	/* actually last +1 */
	WORD	delta;		/* delta is always one line up */

	first = y_char * (_split_row + 1);
	last  = y_char * _rows;
	delta = y_char;

/* (for smoother scrolling, could decrease delta and loop) */

	scan_move (first, last, -delta, scr_ptr, scr_ptr);
	scan_clear (last - delta, last, scr_ptr);
}

/*------------------------------*/
/*	_line_out		*/
/*------------------------------*/

/* Draw a string at the current position.  Check length, clip if required.
   (no folding, must be handled elsewhere) */

_line_out (string, len)
char	*string;
WORD	len;
{
	char	old_endmark;
	WORD	x_align, y_align;
	WORD	font, clip_len;

	if (_wind1)
		font = _w1font;		/* font/picture set to use */
	else	font = _w0font;

	if (font == 2)	{		/* draw pics (in the output stream) */
	    pic_line_out(string, len);
	    return(0);
	    }

	if (font == 3) {		/* use character graphics set */
	    ac_line_out(string, len);
	    return(0);
	    }

/* If ST italics in column 0, must kluge some extra room, or v_gtext() clips 
   off the first char completely.  [The 68K screen folding routine routinely
   uses one less column than is available, for this reason.]
*/
	if (_v_italic && (!_cur_column))	/* start drawing in column 1 */
		_cur_column = 1;

/* If the entire string won't fit at the current position, for any reason,
   clip it down to size.
*/
	clip_len = _columns - _cur_column;	/* max allowed string length */
	if (len < clip_len)
		clip_len = len;			/* length to actually draw */

/* Make the string ASCIZ.  [Buffer output is not given in this form, and 
   in fact may be immediately followed by a character that will ultimately 
   appear at the start of the next line.]
*/
	old_endmark = string[clip_len];		/* save this byte */
	string[clip_len] = 0;			/* null-terminate the string */

	x_align = x_char * _cur_column;
	y_align = y_char * _cur_row;

	if (_v_inverse)		/* draw string, then background ... */
		{
		vswr_mod/*e*/ (handle, 2);		/* transparent mode */
		vst_colo/*r*/ (handle, back_ix);	/* background color */
		v_gtext (handle, x_align, y_align, string);

		vswr_mod/*e*/ (handle, 4);		/* rev transpar mode */
		vst_colo/*r*/ (handle, fore_ix);	/* foreground color */
		v_gtext (handle, x_align, y_align, string);

		vswr_mod/*e*/ (handle, 1);		/* replace mode */
		}
	else			/* draw normal string */
		v_gtext (handle, x_align, y_align, string);

	_cur_column += clip_len;
	string[clip_len] = old_endmark;		/* restore this byte */
}

/*------------------------------*/
/*	_char_out		*/
/*------------------------------*/

/* Draw a single character.  Handle CR and BS as special cases */

_char_out (c)
char c;
{
	char	char_buffer[2];
	WORD	font;

	if (_wind1)
		font = _w1font;		/* font/picture set to use */
	else	font = _w0font;

/* Originally, the next line was meant to avoid mistaking alternate
   character/picture codes for screen control codes.  However, the use of
   #13 as the newline code is firmly embedded in the 68K kernel and can
   pop up unexpectedly (like during long strings of font3 output).

   The simplest solution is to make its meaning /not/ be context (font)
   dependent, and to require that picture sets, like alternate char sets,
   have ids starting from some offset (like 32 or 16). */

/**	if (font > 1) goto fontx; **/	/* (don't) skip special handling */

	switch (c)
	{
	case 13:
		CR_out ();
		break;
	case 8:
		BS_out ();
		break;
	default:	/* first check col; next line if required */
fontx:		if (_cur_column >= _columns - _marg_right)
			CR_out ();

		char_buffer[0] = c;
		_line_out (&char_buffer, 1);	/* draw the char */
		break;				/* (updates col too) */
	}
}

CR_out ()	/* go to next row, leftmost column */
{
	_cur_column = _marg_left;
	_cur_row++;
	if (_cur_row == _rows) {    /* if off bottom, scroll */
		line_scroll ();
		_cur_row--;
		}
}

BS_out ()	/* dec col, write a space, dec col */
{
	if (_cur_column <= _marg_left) {
		_cur_column = _columns - _marg_right;
		_cur_row--;
		}
	_cur_column--;
	_char_out (32);		/* (one level of recursion) */

/**	if (_cur_column <= _marg_left)
		{} **/		/* (impossible case!) */
	_cur_column--;
}

/*------------------------------*/
/*	(op)_highlight		*/
/*------------------------------*/

_highlight (mode)
WORD	mode;
{
	switch (mode)
	{
	case 0:			/* normal text */
		_v_inverse = 0;
		_v_italic = 0;
		vst_effe/*cts*/ (handle, 0);
		break;
	case 1:			/* inverse video */
		_v_inverse = 1;
		break;
	case 2:			/* bold */
		vst_effe/*cts*/ (handle, 1);
		break;
	case 4:			/* italic (underline) */
		_v_italic = 1;
		vst_effe/*cts*/ (handle, 4);
		break;
	}
}

/************************************************************************/
/*	Input Routines							*/
/************************************************************************/

/*------------------------------*/
/*	delay			*/
/*------------------------------*/

delay (count)
LONG count;		/* in 200ths */
{
    LONG done;

    done = _time200() + count;
    while (_time200() < done);		/* loop */
}
	
/*------------------------------*/
/*	_time200		*/
/*------------------------------*/

LONG _time200()		/* return system time, in 1/200 secs */
{
/*  static LONG *hz_200 = 0x04BA;	Lattice error */
    LONG moment, *hz_200;

    super_mode (1);	/* must get into Supervisor mode */
    hz_200 = (LONG *) 0x04BA;
    moment = *hz_200;	/* read the raw 200hz system timer */
    super_mode (0);
    return (moment);
}

/*------------------------------*/
/*	super_mode		*/
/*------------------------------*/

super_mode (enter)	/* enter/exit Supervisor mode */
int enter;
{
    static LONG old_ssp;

    if (enter)
	old_ssp = Super(0L);
    else
	Super(old_ssp);		/* must have previously Entered */
}

/*------------------------------*/
/*	cycle_cursor		*/
/*------------------------------*/

#define ONTIME 132	/* cursor duty cycle is 66% ... */
#define CYCLETIME 200	/* ... of one full sec */

/* This routine must be called once before and after event_in loops, as well
   as repeatedly during event_in itself, to show a cursor correctly. */

cycle_cursor (arg)	/* 1= starting input, -1= ending input, 0= waiting */
int arg;
{
    static LONG basetime;
    static int curs_on;		/* true when cursor showing */
    int elapsed;

    if (!curs_vis)		/* but suppress cursor if indicated */
	return(0);

    if (arg == 1) {		/* STARTING INPUT */
	basetime = _time200();	/* get & save a reference time */
	curs_on = 0;		/* cursor initially off */
	}

    elapsed = _time200() - basetime;	/* current time */
    if (elapsed >= CYCLETIME) {		/* too big, wraparound */
	basetime += CYCLETIME;
	elapsed -= CYCLETIME;
	}

    if (arg == -1) {		/* ENDING INPUT */
	elapsed = ONTIME;	/* make sure we leave with cursor off */
	curs_on = 1;		/* force an erase in case of mouse garbage */
	}

/* A subtle problem:

   The blinking cursor can be visually interfered with by the (moving) mouse 
   pointer.  Sequence is (1) mouse moves, and partly covers cursor (2) cursor
   changes state (3) mouse moves again.  Left behind is a fragmented cursor.

   The reason is that, whenever the mouse moves over a given spot, it saves
   the screen under that spot.  When it moves off that spot, it restores it.
   The ST mouse driver, unfortunately, doesn't notice that the screen has
   /changed/ in the meantime.  So the restored section is wrong.

   One fix would be to "hide" the mouse during all cursor state changes.
   But this causes random visible flickering of the mouse pointer.  
   Synchronizing the hide with the vertical blank interrupt (xbios 37)
   seems to make the degree of flicker depend on the absolute screen location
   of the mouse (none in bottom third, slight in middle, greatest at top;
   the map in BZ for mouse activity is near the top).

   A partial fix is to allow "temporary" fragmentation only.  The cursor 
   cycle is one sec and is "self correcting" anyway.  Prevent any
   "permanent" garbage by making sure the spot is clean when the blinking
   is turned off (which usually occurs prior to the cursor being moved).  

     () hide the mouse BEFORE calling us to end cursor blink, then
     () do the final "remove cursor" EVEN IF curs_on indicates it is currently
	       erased, in case garbage is showing.
*/

    if ((elapsed >= ONTIME) && curs_on)	/* Phase 2: remove the cursor */
	{
	_char_out (32); _char_out (8);
	curs_on = 0;
	}

    if ((elapsed < ONTIME) && !curs_on)	/* Phase 1: show a cursor */
	{
	_char_out (32);
	if (_v_inverse)	_v_inverse = 0;
	else	_v_inverse = 1;

	_char_out (8);			/* ST: reverse video for the BS */
	if (_v_inverse)	_v_inverse = 0;
	else	_v_inverse = 1;

	curs_on = 1;
	}
}

/*------------------------------*/
/*	_do_input		*/
/*------------------------------*/

_do_input(start)	/* handle any special tasks surrounding input */
int start;
{
    if (start) {
	cycle_cursor(1);		/* start up the cursor */

	show_mouse(1);			/* show the mouse pointer */
	mouse = 1;
	bstatus = B1DOWN;		/* "reset" mouse button status */
	click1_time = 0;		/* and double-click timer */
	}
    else {
	show_mouse(0);			/* hide the mouse */
	mouse = 0;
	cycle_cursor(-1);		/* THEN shut down the cursor */
	}
}

/*------------------------------*/
/*	q_clickdown		*/
/*------------------------------*/

/* Check for an up->down transition of mouse button (left button only,
   to be consistent with GEM desktop; ignore upclicks).

   This check should be called in a fast loop, since we compare the current
   state to the last detected state.  When the loop is broken, prior to
   starting the next loop, the button status global should be reset to DOWN
   to avoid detecting a "premature" click; we want to detect only "real-time" 
   clicks.  */

BOOL
q_clickdown()
{
    WORD cbstatus, x, y;	/* current button status, mouse position */
    BOOL val = FALSE;

    vq_mouse( handle, &cbstatus, &x, &y);
    cbstatus &= B1DOWN;		/* mask off other button flags */

    if (!bstatus && cbstatus) {	  /* up->down transition? */
	val = TRUE;
	_xmouse = x / x_char;	/* store position, in char units */
	_ymouse = y / y_char;
	}

    bstatus = cbstatus;		/* update button status global */
    return (val);
}

/*------------------------------*/
/*	mouse_in		*/
/*------------------------------*/
UWORD
mouse_in()	/* check for a mouse click (or two) */
{
    UWORD val;

    if (click1_time) {		/* click detected previously? */

	if (q_clickdown()) {	/* yes, second click? */
	    val = TWOCLICK;	/* yes */
	    click1_time = 0;
	    }
	else if (_time200() > click1_time + DOUBLEWAIT) {  /* keep waiting? */
	    val = ONECLICK;	/* no, call it a single */
	    click1_time = 0;
	    }
	else
	    val = MIDCLICK;	/* yes, keep waiting */
	}
    else  {			/* click not detected previously */

	if (q_clickdown()) {	/* first click? */
	    val = MIDCLICK;	/* yes, start waiting */
	    click1_time = _time200();
	    }
	else
	    val = NOCLICK;	/* nothing */
	}
    return (val);
}

/*------------------------------*/
/*	_char_in		*/
/*------------------------------*/
WORD
_char_in ()	/* Check for a key, return a zero (immediately) if none */
{
    register LONG rawc;		/* scan code and character from keyboard */
    register WORD c;		/* ascii code */
    register WORD *tp;		/* ptr to table of special keys	 */

    static WORD transtab[] =	/* key translations for the Atari ST: */
	{
	0x4800,	129,		/* up arrow */
	0x5000,	130,		/* down arrow */
	0x4B00,	131,		/* left arrow */
	0x4D00,	132,		/* right arrow */
	0x3B00,	133,		/* F1 */
	0x3C00,	134,		/* F2 */
	0x3D00, 135,		/* F3 */
	0x3E00,	136,		/* F4 */
	0x3F00, 137,		/* F5 */
	0x4000,	138,		/* F6 */
	0x4100,	139,		/* F7 */
	0x4200,	140,		/* F8 */
	0x4300,	141,		/* F9 */
	0x4400,	142,		/* F10 */
	/* no F11, F12 */
	0x7030, 145,		/* K0 (numeric keypad) */
	0x6D31, 146,		/* K1 */
	0x6E32, 147,		/* K2 */
	0x6F33, 148,		/* K3 */
	0x6A34, 149,		/* K4 */
	0x6B35, 150,		/* K5 */
	0x6C36, 151,		/* K6 */
	0x6737, 152,		/* K7 */
	0x6838, 153,		/* K8 */
	0x6939, 154,		/* K9 */
	0, 0 };			/* zero marks table end */

    static WORD colorkeys[] =	/* keys for (internal) color control */
	{
	0x4D36, 0,		/* shift right arrow */
	0x4B34, 1,		/* shift left arrow */
	0x4838, 2,		/* shift up arrow */
	0x5032, 3,		/* shift down arrow */
	0, 0 };

    rawc = gemdos(0x06, 0xFF);	/* read the keyboard */
    rawc = ((rawc>>8) & 0xFF00)	/* scancode/ascii is arranged -S-A, */
	  | (rawc & 0x00FF);	/* move to --SA */
    if (rawc == 0)		/* exit immediately if nothing */
	return (NOKEY);

    tp = transtab;
    while (*tp != 0)		/* check table for non-ascii key */
	{
	if (*tp++ == rawc)	/* found it? */
	    return (*tp);	/* yes, return translated value */
	tp++;
	}

    tp = colorkeys;
    while (*tp != 0)		/* check table for color-control key */
	{
	if (*tp++ == rawc) {	/* found it? */
	    tweak_color (*tp);	/* yes, go adjust the color */
	    return (NOKEY);
	    }
	tp++;
	}

    c = rawc & 0xFF;		/* not in tables, extract ascii code */
    if (c < 0x20 || c > 0x7F) {
	if (c != 0x08 && c != 0x0D)	/* special key, pass it along? */
	    c = NOKEY;			/* strange key, discard it */
	}
    return (c);
}

/*------------------------------*/
/*	_event_in		*/
/*------------------------------*/

/* Must "bracket" loops that call this routine with two calls to _do_input */

UWORD
_event_in ()	/* check for a key or mouse event */
{
    register UWORD val;

    cycle_cursor(0);		/* maintain a blinking cursor */
    val = mouse_in();		/* Check first for a mouse click (or two) */

    if (val != NOCLICK) {	/* got something ... */
	if (val == MIDCLICK)	/* "waiting", but don't tell yet */
	    val = NOCLICK;	/* just keep looping so cursor/timeouts work */
	}
    else val = _char_in();	/* Check for a key */

    return (val);
}

#ifndef DEADCODE  /* skip this section */

/* Serious problems exist with evnt_multi() and timer events, particularly 
   with short (1 msec) timeout values:
	() many keystrokes go unreported.
	() type-ahead and key repeat don't work.
   Also, mouse button events aren't debounced.
*/

UWORD
BAD_event_in ()	/* check for a key or mouse event, timeout if none */
{
    UWORD	ev_mwhich, ev_mmox, ev_mmoy, ev_mkreturn;
    UWORD	junk, result;

    ev_mwhich = evnt_mul/*ti*/ (
	1 + 2 + 0x20,		/* wait for key, button or timer event */
	1,			/* just one click will do */
	1,			/* left button only (? can't wait for both) */
	1,			/* looking for a down button */
	0,0,0,0,0,		/* [to detect mouse motion without a click] */
	0,0,0,0,0,
	&junk,			/* [to get message events -- careful, LONG] */
	150, 0,			/* msecs [LONG] to wait before timing out */
    /* returned values: */
	&ev_mmox,		/* mouse position */
	&ev_mmoy,
	&junk,			/*   [state of button] */
	&junk,			/*   [state of Shift/Ctrl/Alt keys] */
	&ev_mkreturn,		/* key */
	&junk );		/*   [# button clicks] */

    if (ev_mwhich == 0x20)	/* timed out, return a null */
	result = 0;

    else if (ev_mwhich == 1)	/* raw key, return value */
	result = ev_mkreturn;

    else if (ev_mwhich == 2)	/* button */
	result = '@';		/* TESTING ... TESTING ... TESTING ... */
    
    return (result);
}
#endif

/*------------------------------*/
/*	_event_wait		*/
/*------------------------------*/

UWORD _event_wait()	/* don't return until key (or button) is pressed */
{
    UWORD c;

    _do_input(1);
    while (!(c = _event_in()));
    _do_input(0);
    return (c);
}

/*------------------------------*/
/*	show_mouse		*/
/*------------------------------*/

/* Mouse pointer must be visible during () input (XZIP) and () disk i/o (in 
   case an error dialog box pops up; this is an OS oversight).  Mouse pointer 
   must be hidden during () scrolling and () all drawing to screen. */

show_mouse(visible)
BOOL visible;
{
    if (visible)
	v_show_c (handle, 0);
    else {
	xbios (37);		/* wait for next vbi, to reduce flickering */
	v_hide_c (handle);
	}
}

/************************************************************************/
/*	File Selection							*/
/************************************************************************/

/*------------------------------*/
/*	get_path		*/
/*------------------------------*/

get_path (init_path)		/* return default directory pathname */

char	*init_path;
{
	WORD	init_drive;

	init_drive = gemdos(0x19);	/* Dgetdrv() */

	init_path[0] = init_drive + 'A';
	init_path[1] = ':';
/*	init_path[2] = '\0';	*/

	gemdos(0x47, &init_path[2], init_drive + 1);	/* Dgetpath */
	strcat (init_path, "\\");
}

/*------------------------------*/
/*	back_search		*/
/*------------------------------*/
WORD
back_search (string, target_char)	/* return length of string through
					     given char, zero if not found */
char	*string;
char	target_char;
{
	WORD	index;

	index = strlen (string);
	while (index && (string[index-1] != target_char))
		{
		index--;
		}
	return (index);
}

/*------------------------------*/
/*	add_fname		*/
/*------------------------------*/

add_fname (pathspec, filename)

char	*pathspec, *filename;
{
	WORD	index1, index2;

	index1 = back_search (pathspec, '\\');	/* any subdirectories? */
	index2 = back_search (pathspec, '\:');

	if (index1 > index2)			/* chop off any filespec */
		pathspec[index1] = '\0';
	else	pathspec[index2] = '\0';

	strcat (pathspec, filename);		/* append this filename */
}

/*------------------------------*/
/*	add_ftype		*/
/*------------------------------*/

add_ftype (fullname, filetype)

char	*fullname, *filetype;
{
	WORD	index;

	index = back_search (fullname, '\.');	/* any filetype? */
	if (index)
		fullname[index-1] = '\0';	/* if so, chop it off */
	strcat (fullname, filetype);		/* append our filetype */
}

/*------------------------------*/
/*	_file_select		*/
/*------------------------------*/

#define STYPE ".SAV"	/* normal savefile type */
#define PTYPE ".INF"	/* a distinct "partial" savefile type */

WORD
_file_select (partial, save)	/* get file and pathspec from user */
WORD partial;	/* "partial", otherwise normal */
WORD save;	/* save, otherwise restore */
{
    WORD button, error;
    char *ftype;

    if (partial)  ftype = PTYPE;	/* different, so fsel filter works */
    else  ftype = STYPE;

    _filename[8] = '\0';		/* ST: max length is 8 chars */
    add_ftype (pathspec, ftype);	/* force a standard type */
    if (_filename[0])			/* but only if a default name exists */
	add_ftype (_filename, ftype);

    if (!partial && save)		/* special case, no default: */
	_filename[0] = '\0';		/* type in new name OR click on old */

    copy_screen (1);			/* backup screen */
    if (!mouse)
	show_mouse(1);			/* make sure mouse is up temporarily */
    error = fsel_inp/*ut*/ (pathspec, _filename, &button);
    if (!mouse)
	show_mouse(0);			/* mouse off */
    copy_screen (0);			/* restore screen */

    if (((error == 0) || (button == 0)) || (strlen (_filename) == 0))
	return (1);			/* failed */

    add_ftype (_filename, ftype);	/* again, force standard types */
/*  add_ftype (pathspec, ftype); */	/* (this one is redundant) */

    strcpy (_fullname, pathspec);
    add_fname (_fullname, _filename);	/* create full name */

/* if about to SAVE ...  (this now done in 68K)
	() check if this file already exists
	() if so, ask user for Abort or Proceed		*/

    return (0);		/* okay */
}

/*------------------------------*/
/*	_new_default		*/
/*------------------------------*/

_new_default (okay)	/* called at end of each SAVE/RESTORE */

WORD	okay;
{
	if (okay)	/* dest <- source */
		{
		strcpy (pathback, pathspec);	/* update old defaults */
		strcpy (fileback, _filename);
		}
	else
		{
		strcpy (pathspec, pathback);	/* retrieve old defaults */
		strcpy (_filename, fileback);
		}
}

/************************************************************************/
/*	Disk I/O							*/
/************************************************************************/

#define	 dos_create	0x3C
#define	 dos_open	0x3D

#define	 dos_lseek	0x42
#define	 dos_read	0x3F
#define	 dos_write	0x40

#define	 dos_close	0x3E
#define	 dos_delete	0x41	/* "unlink" */
#define  dos_exist	0x4E

/*------------------------------*/
/*	_read_file		*/
/*------------------------------*/

LONG
_read_file (channel, offset, length, buffer)

WORD	channel;
LONG	offset, length;
char	*buffer;
{
	LONG	result;

	show_mouse(1);
	result = gemdos (dos_lseek, offset, channel, 0);

	if (result != offset)		/* seek error */ 
		result = -99;
	else	result = gemdos (dos_read, channel, length, buffer);
	show_mouse(0);

	if (result < 0) 
		return (result);	/* read error (ignore EOF error) */
	else	return (0);		/* otherwise no error */
}

/*------------------------------*/
/*	_write_file		*/
/*------------------------------*/

LONG
_write_file (channel, offset, length, buffer)

WORD	channel;
LONG	offset, length;
char	*buffer;
{
	LONG	result;

	show_mouse(1);
	result = gemdos (dos_lseek, offset, channel, 0);

	if (result != offset) 		/* seek error */
		result = -99;
	else	result = gemdos (dos_write, channel, length, buffer);

	show_mouse(0);
	if (result != length)
		return (result);	/* write error, positive means EOF */
	else	return (0);		/* otherwise no error */
}

/*------------------------------*/
/*	_create_file		*/
/*------------------------------*/

LONG
_create_file ()		/* create AND OPEN file, return channel */
{
	LONG result;	/* handle if success, negative if error */

	show_mouse(1);
	result = gemdos (dos_create, _fullname, 0);
	show_mouse(0);

	return (result);
}

/*------------------------------*/
/*	_open_file		*/
/*------------------------------*/

LONG
_open_file ()
{
	LONG result;	/* handle if success, negative if error */

	show_mouse(1);
	result = gemdos (dos_open, _fullname, 2);	/* READ/WRITE */
	show_mouse(0);

	return (result);
}

/*------------------------------*/
/*	_close_file		*/
/*------------------------------*/

LONG
_close_file (channel)

WORD	channel;
{
	LONG	result;		/* zero if success, negative if error */

	show_mouse(1);
	result = gemdos (dos_close, channel);
	show_mouse(0);

	return (result);
}

/*------------------------------*/
/*	_delete_file		*/
/*------------------------------*/

LONG
_delete_file ()
{
	LONG	result;		/* zero if success, nonzero if error */

	show_mouse(1);
	result = gemdos (dos_delete, _fullname);
	show_mouse(0);

	return (result);
}

/*------------------------------*/
/*	_exist_file		*/
/*------------------------------*/

/* check if _fullname conflicts with an existing name */

#define FNATTR  0x16	/* SEARCH INCLUDES HIDDEN, SYSTEM, SUBDIRECS */

LONG
_exist_file ()
{
	register LONG result;		/* return nonzero if conflict */

	show_mouse(1);
	result = gemdos (dos_exist, _fullname, FNATTR);
	show_mouse(0);

	if (result) result = 0;		/* GEM returns /zero/ if conflict */
	else result = 1;
	return (result);
}

/*------------------------------*/
/*	_open_game		*/
/*------------------------------*/

#define GAMEFILE "story.dat"

LONG
_open_game()
{
	LONG	f_handle;

	strcpy (_fullname, GAMEFILE);
	f_handle = _open_file ();

	_fullname[0] = 0;
	return (f_handle);
}

/*------------------------------*/
/*	_close_game		*/
/*------------------------------*/

LONG
_close_game (f_handle)

WORD	f_handle;
{
	return (_close_file (f_handle));
}

/************************************************************************/
/*	Picture display							*/
/************************************************************************/

#define GF_TITLE 0x01	/* title screen bit */

struct picinfo {
    UWORD gcount;	/* number of pic definitions in file */
    UWORD goff;
    UWORD unused;
    UWORD gflags;
    } pi;

UWORD *psiz_tab;	/* picture sizes, pixels (x/y) */
ULONG *poff_tab;	/* data offsets (in file) */

MFDB pic_mfdb;		/* raster structs for pictures */
WORD pic_xy[8];

int pfchn;		/* picfile channel */

/*------------------------------*/
/*	pic_init		*/
/*------------------------------*/

#define PICFILE "FONT2.DAT"
#define PHDLEN sizeof(struct picinfo)

pic_init()
{
    int i, n, tablen;
    ULONG picbase;
    UWORD xpic, ypic;

    strcpy (_fullname, PICFILE);
    if ((pfchn = _open_file()) < 0) {	/* open picture file */
	pi.gcount = 0;			/* error, zero pic count and exit */
	return(1);
	}

    n = _read_file(pfchn, 0, PHDLEN, &pi);	/* read in header */
    if (n) {					/* zero means "ok" */

	/*** if error (bad read or odd count), return ***/
	pi.gcount = 0;			/* error, zero pic count and exit */
	return(1);
	}

/* allocate and setup the size table */

    tablen = pi.gcount * 4;

    psiz_tab = (UWORD *) _zalloc(tablen);	/* sizes: 2 words per item */
    poff_tab = (ULONG *) _zalloc(tablen);	/* offsets: 1 long per item */
    if (!psiz_tab | !poff_tab)
	init_exit ("Out of memory");

    n = _read_file(pfchn, 32, tablen, psiz_tab);    /* read in size info */
    if (n) {					    /* zero means "ok" */
	pi.gcount = 0;			/* error, zero pic count and exit */
	return(1);
	}
/* and build the offset table ... */

    picbase = 32 + tablen + (pi.gcount * 12);	/* skip hdr, sizes, names */
    for (i=0; i<pi.gcount; i++) {

	poff_tab[i] = picbase;		/* offset of current pic */

	xpic = psiz_tab[i*2];		/* width (pixels) */
	if (xpic & 15) xpic += 16;	/* round up to nearest word */
	xpic >>= 4;			/* width (words) */

	ypic = psiz_tab[(i*2)+1];
	picbase += (xpic * 2) * ypic;		/* next pic starts here */
	}

/* Space for pic data itself should perhaps be allocated and preloaded, but
   ONLY if there's enough memory to preload the whole game too.

   Otherwise, keep file open for reading.  Close only at end of program (or
   just fudge) */

/**    _close_file(pfchn); **/

/* display a title screen, before game loads?  NOW A SEPARATE CALL */
}

/*------------------------------*/
/*	pic_line_out		*/
/*------------------------------*/

/* Output a string of "picture chars"  */

pic_line_out(s, len)
UBYTE *s; int len;	/* "len" is number of pics, not physical length */
{
    WORD tbl[2];
    char c;
    int i, n;

    for (i=0; i<len; i++) {
	c = *s++;

	if (_op_picinf(c, &tbl) == 0) {
	    n = tbl[0];		/* width in char units, rounded up */

	    if (_cur_column + n <= _columns) {	/* make sure there's room */
		_op_display (c, _cur_column, _cur_row, 0);
		_cur_column += n;
		}
	    }
	}
}

/*------------------------------*/
/*	_op_picinf		*/
/*------------------------------*/

int _op_picinf(n, tbl)	/* return nonzero if error */
UWORD n; WORD *tbl;	/* return (in table) size of given picture */
{
    register int n2, val;
    int err = 0;

    if (!n) {			/* special arg */
	tbl[0] = pi.gcount;	/* return highest pic id (1 origin) */
	return (0);
	}

    --n;			/* make id 0-origin */
    n2 = n << 1;

    if (n >= pi.gcount)		/* check for out-of-range */
	err = 1;
    else {		/* return size (in "char units" for ST) ... */

	val = psiz_tab[n2];
	if (val % x_char) val += x_char;	/* round up */
	tbl[0] = val / x_char;

	val = psiz_tab[n2+1];
	if (val % y_char) val += y_char;	/* round up */
	tbl[1] = val / y_char;
	}
    return(err);		/* done */
}

/*------------------------------*/
/*	_op_display		*/
/*------------------------------*/

_op_display (n, xpos, ypos, clr)    /* display [clear] a picture */
UWORD n, xpos, ypos, clr;	    /* id, position (in char units), flag */
{
    int xwpic, xpic, ypic;	/* size */
    int n2, x, y;

    --n;			/* make id 0-origin */
    if (n >= pi.gcount)		/* check for out-of-range (1 origin) */
	return(1);
    n2 = n << 1;

    xpic = xwpic = psiz_tab[n2];	/* width (pixels) */
    if (xwpic & 15) xwpic += 16;
    xwpic >>= 4;			/* round up: width (words) */
    ypic = psiz_tab[n2+1];		/* height (pixels) */

/* get picture into alternate screen buffer [or just clear buffer] */

    if (pic_read (n, xwpic, ypic, clr))
	return(1);			/* read error */

    if (y_char < 16)		/* medium res: ignore extra lines */
	ypic >>= 1;

/* then draw the buffer (quickly) on the main screen, with a raster call */

    pic_mfdb.mp  = (long) bak_mfdb.mp;

    pic_mfdb.fwp = xwpic << 4;	/* pixel width of transfer block */
    pic_mfdb.fww = xwpic;	/* word width */
    pic_mfdb.fh  = ypic;	/* pixel height */
    pic_mfdb.np  = 1;		/* planes in source */
    pic_mfdb.ff  = 0;		/* format is Atari-specific */

    x = xpos * x_char;
    y = ypos * y_char;

    pic_xy[0] = 0;		/* source <left>, <top> */
    pic_xy[1] = 0;
    pic_xy[2] = xpic - 1;	/* source <right>, <bottom> */
    pic_xy[3] = ypic - 1;

    pic_xy[4] = x;			/* display position, left */
    pic_xy[5] = y;			/* top */
    pic_xy[6] = x + (xpic-1);		/* right */
    pic_xy[7] = y + (ypic-1);		/* bottom */

    pic_draw();
}

/*------------------------------*/
/*	pic_read		*/
/*------------------------------*/

/* Read pic data from disk into the alternate screen buffer, starting 
   from the base, and perform any resolution-dependent munging required.
   [Or just zero the buffer, if flag indicates.]  */

int pic_read (n, xwpic, ypic, clr)
WORD n, xwpic, ypic, clr;	/* id, size, flag */
{
    int i, len, off;
    WORD *p1, *p2;

    len = (xwpic * 2) * ypic;		/* in bytes */
    p1 = p2 = (WORD *) bak_mfdb.mp;	/* use the alternate screen buffer */

    if (clr) {
	_clr_mem(p1, len);	/* zero the buffer */
	return (0);		/* done */
	}

    off = poff_tab[n];
    n = _read_file(pfchn, off, len, p1);	/* read in pic */
    if (n)
	return(1);				/* nonzero, error, exit */

    if (y_char < 16) {		/* munge hi-res pic into medium-res */
	for (i=0; i<ypic; i++) {
	    _mov_mem(p2, p1, (xwpic * 2));	/* by squeezing it down */
	    p1 += xwpic;
	    p2 += (xwpic * 2);
	    }
	}
    return(0);		/* okay */
}

/*------------------------------*/
/*	pic_draw		*/
/*------------------------------*/

#define PIC_REPLACE 1	/* writing mode, not logic op */

/* Draw a picture, medium OR high res */

pic_draw()
{
    LONG fb_color = (fore_ix << 16) | back_ix;

    vrt_cpyf/*m*/ (handle, PIC_REPLACE, &pic_xy,
		     &pic_mfdb, &scr_mfdb, &fb_color);
}

/************************************************************************/
/*	Alternate Character Set display					*/
/************************************************************************/

struct acinfo {
    UWORD gcount;	/* number of alt char definitions in file */
    UWORD goff;		/* refnum of initial alt char */
    UBYTE xchar;	/* size */
    UBYTE ychar;
    UWORD gflags;
    } ai;

UBYTE *acdata;		/* base of alt char definitions */

MFDB ac_mfdb;		/* raster structs for altchars */
WORD ac_xy[8];

/*------------------------------*/
/*	ac_init			*/
/*------------------------------*/

#define ACFILE "FONT3.DAT"
#define HDLEN sizeof(struct acinfo)

#define AC_EXPAND 4 	/* ST "data expansion" factor */
#define AC_SIZE 8	/* our standard ac width, height */

ac_init()
{
    register UBYTE *bp, *mp;
    register int i,j;
    int chn, n, readlen, bufflen;
    UBYTE block[AC_SIZE];

    strcpy (_fullname, ACFILE);
    if ((chn = _open_file()) < 0) {	/* open charset file */
	acdata = 0;			/* error (or none), just skip */
	return(1);	
/**	char msg[80];
	strcpy (msg, "Couldn't find file ");
	strcat (msg, _fullname);
	init_exit(msg);		**/
	}

    _read_file(chn, 0, HDLEN, &ai);		/* read in header */
    if (ai.xchar != AC_SIZE || ai.ychar != AC_SIZE)
	init_exit("Bad char size");		/* something's wrong */

/* Allocate buffer and read in data.  Alternate char data should always be
   preloaded. */

    readlen = ai.gcount * AC_SIZE;	/* length of data */
    bufflen = readlen * AC_EXPAND;	/* allow room for munging ST format */

    acdata = (UBYTE *) _zalloc(bufflen);
    if (!acdata)
	init_exit ("Out of memory");

    n = _read_file(chn, 32, readlen, acdata);	/* read in rest of file */
    if (n)					/* zero means "ok" */
	init_exit("Error reading font file");

    _close_file(chn);

/* The standard width of our alternate chars is a byte.  The ST raster ops
   require form widths to be exact multiples of a word (2 bytes, 16 pixels),
   so we arrange the ac data (during initializations, for speed) into a 
   word-width transfer area.
*/
    for (j = ai.gcount-1; j >= 0; j--) {	/* work back from last def */

	mp = (UBYTE *) acdata + (j * AC_SIZE);
	bp = (UBYTE *) &block;

	for (i=0; i<AC_SIZE; i++)	/* copy to temp buffer */
	    *bp++ = *mp++;

	mp = (UBYTE *) acdata + (j * (AC_SIZE * AC_EXPAND));
	bp = (UBYTE *) &block;

/* For hi-res, each byte is "stretched" into two so the char is 16 bits high.
   Otherwise the extra space is unused (but facilitates faster drawing). */

	for (i=0; i<AC_SIZE; i++) {
	    *mp++ = *bp;		/* left-align byte in word */
	    *mp++ = 0;
	    if (y_char >= 16) {
		*mp++ = *bp;		/* stretch it */
		*mp++ = 0;
		}
	    bp++;
	    }
	}
/*** set up these vars too, once only ***/

    ac_mfdb.fwp = 16;		/* pixel width of transfer block */
    ac_mfdb.fww = 1;		/* word width */
    ac_mfdb.fh  = y_char;	/* pixel height */
    ac_mfdb.np  = 1;		/* planes in source */
    ac_mfdb.ff  = 0;		/* format is Atari-specific */

    ac_xy[0] = 0;		/* source <left>, <top> */
    ac_xy[1] = 0;
    ac_xy[2] = x_char - 1;	/* source <right>, <bottom> */
    ac_xy[3] = y_char - 1;
}

/*------------------------------*/
/*	ac_line_out		*/
/*------------------------------*/

ac_line_out(s, len)	/* output a string of alt chars */
UBYTE *s; int len;
{
    register int off, i, x, y;
    UBYTE c;

    if (!acdata) return(1);	/* no alt chars available */

    x = _cur_column * x_char;
    y = _cur_row * y_char;

    ac_xy[5] = y;		/* display position, top */
    ac_xy[7] = y + (y_char-1);	/* display position, bottom */

    for (i=0; i<len; i++) {
	c = *s++;
	off = (c - ai.goff) * (AC_SIZE * AC_EXPAND);
	ac_mfdb.mp  = (long) acdata + off;

	ac_xy[4] = x;			/* display position, left */
	ac_xy[6] = x + (x_char-1);	/* display position, right */
	ac_draw();

	x += x_char;
	_cur_column++;
	}
}

/*------------------------------*/
/*	ac_draw			*/
/*------------------------------*/

#define VRT_REPLACE 1	/* writing mode, not logic op */
#define VRO_REPLACE 3	/* this one is a logic op */

/* Draw an alternate character, medium OR high res */

ac_draw()
{
    LONG fb_color = (fore_ix << 16) | back_ix;

    vrt_cpyf/*m*/ (handle, VRT_REPLACE, &ac_xy,
		     &ac_mfdb, &scr_mfdb, &fb_color);

/* Draw an alternate character, high-res only -- DEAD */

/** vro_cpyfm (handle, VRO_REPLACE, &ac_xy, &ac_mfdb, &scr_mfdb); **/
}

/************************************************************************/
/*	Initializations							*/
/************************************************************************/

/*------------------------------*/
/*	z_init			*/
/*------------------------------*/

static int old_rez;		/* startup (desktop) resolution */

WORD
z_init ()
{
	WORD	work_in[20], work_out[100], extnd_out[100];
	WORD	attrib[10], gr_cell[4];
	WORD	hor_out, vert_out;	/* dummy */
	WORD	apid, i, planes;
	LONG	buff_size, buff_loc;

/* This call doesn't work quite right.  The screen does go into medium res
   but the value for x_res isn't updated in the OS.  Also v_gtext() seems to
   draw lots of garbage (parallel lines).
   Can it be fixed?  We currently depend on the user for the correct res. */

/**	old_rez = Getrez();
	if (old_rez == 0)		/-* ST: started in low res? *-/
	    Setscreen(-1,-1, 1);	/-* change to medium for XZIP *-/
**/
	md_meminit();	/* must precede all allocs */

/* Set the system up to do AES calls */

	apid = appl_ini/*t*/ ();	/* returns AES handle, IGNORED */
	if (apid == -1)
		return (4);

/* Get the VDI handle for the currently opened workstation */

	handle = graf_han/*dle*/ (&gr_cell[0],&gr_cell[1],
			      &gr_cell[2],&gr_cell[3]);

	x_char = gr_cell[0];	/* initialize char cell sizes */
	y_char = gr_cell[1];

/* Open the VIRTUAL SCREEN workstation, get new handle */

	for (i=0; i<10; i++) {
		work_in[i] = 1;
		}
	work_in[10] = 2;	/* use RC units -- raster */

	v_opnvwk(work_in, &handle, work_out);
	x_res = work_out[0] + 1;
	y_res = work_out[1] + 1;

	graf_mou/*se*/ (0);

/**	/-* Window functions, NOT USED *-/
	WORD	wind_type, wi[4], wo[4];

/-* request size of desktop window, calc size of work area  *-/
	wind_get (0,4, &wi[0],&wi[1],&wi[2],&wi[3]);
	wind_type = 0x002b;		/-* name, size, move, close *-/
	wind_calc (1,wind_type,wi[0],wi[1],wi[2],wi[3],
				&wo[0],&wo[1],&wo[2],&wo[3]);
/-* make a window of the max size *-/
	w_handle = wind_create (wind_type,wi[0],wi[1],wi[2],wi[3]);
	wind_set (w_handle,2,"SAMPLE",0,0);
	wind_open (w_handle,,wi[0],wi[1],wi[2],wi[3]);
**/
	vq_extnd (handle, 1, extnd_out);	/* get extra info */
	planes = extnd_out[4];			/* 1, 2, or 4 */
	if (planes == 1)
		_mono = 1;			/* monochrome */
	max_index = 1;
	for (i=0; i<planes; i++)
		max_index = max_index<<1;	/* 2, 4, or 16 */

/* set up the primary MFDB */

	scr_mfdb.fwp = x_res;
	scr_mfdb.fww = x_res>>4;	/* word width */
	scr_mfdb.fh = y_res;
	scr_mfdb.np = planes;
	scr_mfdb.ff = 0;		/* form format is ATARI specific */

	scr_mfdb.mp = xbios(2);		/* get screen phys base addr */
/*	scr_mfdb.mp = 0x0L; */
	scr_ptr = &scr_mfdb;

/* Set up the secondary MFDB, and allocate an alternate screen buffer. 
   To be used as the main ST screen, must begin on a multiple of 256 bytes. */

	bak_mfdb.fwp = x_res;
	bak_mfdb.fww = x_res>>4;	/* word width */
	bak_mfdb.fh = y_res;
	bak_mfdb.np = planes;
	bak_mfdb.ff = 0;		/* form format is ATARI specific */

	buff_size = (LONG)(x_res>>3) * (LONG)(y_res) * (LONG)(planes);
	buff_size += 256;		/* ST: room for rounding */
	buff_loc = (LONG) _zalloc(buff_size);
	if (buff_loc == 0)
		return (2);
	buff_loc = (buff_loc & 0xFFFFFF00) + 256;    /* ST: round up */

	bak_mfdb.mp = buff_loc;
	bak_ptr = &bak_mfdb;

	vqt_attr/*ibutes*/ (handle, attrib);
/**	x_char = attrib[8];		/-* char cell width is 6: WRONG! *-/
	y_char = attrib[9];	**/

	_columns = x_res / x_char;	/* compute our screen size */
	_rows = y_res / y_char;	

	_cur_column = 0; 
	_cur_row = _rows - 1;
	_split_row = 0;		/* no status line yet */

/* set graphic text alignment -- to left and top */

	vst_alig/*nment*/ (handle, 0, 5, &hor_out, &vert_out);

	show_mouse(0);		/* hide the cursor */
	_clear_screen ();	/*   THEN clear the screen */

	get_path (pathspec);		/* get user's default path name */
	strcat (pathspec, "*.*");	/* add a generic file spec */

	_filename[0] = 0;
	_fullname[0] = 0;
	_new_default (1);	/* initialize backups, too */

	init_colors (1);	/* save original desktop colors */

	if (_columns < 80)	/* XZIP (this replaces check in 68K) */
	    init_exit("Screen resolution must be Medium");
	return (0);
}	

/*------------------------------*/
/*	z_exit			*/
/*------------------------------*/

z_exit()
{
	_clear_screen ();	/* clear screen, THEN ... */
	init_colors (0);	/* restore original desktop colors */

/**	wind_close (w_handle);
	wind_delete (w_handle); **/

	show_mouse(1);
	v_clsvwk (handle);	/* close VIRTUAL work station */

	appl_exi/*t*/ ();		/* clean up AES environment */

/**	if (old_rez == 0)		/-* were we low res at startup? *-/
	    Setscreen(-1,-1, 0);	/-* change back *-/	**/

/**	_exit (0);	**/	/* fall through */
}

/*------------------------------*/
/*	init_exit		*/
/*------------------------------*/

/* For aborts before jump into 68K segment (after jump, call FATAL).
   (Careful: our screen handling must have been set up, for the out/wait calls 
   to work.) */

init_exit(msg)
char *msg;
{
    char *s = "Press any key to exit ";

    _line_out (msg, strlen(msg));
    _char_out (13);
    _line_out (s, strlen(s));
    _event_wait();

    z_exit();		/* remember to clean up */
    _exit(0);		/* escape (Lattice) */
}

WORD palsys [16*RGBLEN];	/* system default palette (lo-rez) */
WORD palraw [16];		/* raw (ST format) Neochrome palette */

/*------------------------------*/
/*	title_screen		*/
/*------------------------------*/

/* Read and display a special front screen, if the file exists, 
   before we begin loading the main game.  Caveats:
	(1) the file is assumed to be in 16-color Neochrome format
   (unlike the XZIP graphics routines elsewhere in this program)
	(2) we must be using a color display
   Must do setups in z_init (MFDB stuff) before calling here.
*/

#define FSFILE "SCREEN.DAT"
#define FSPALETTE 4		/* base of Neochrome palette */
#define FSPLEN 32
#define FSDATA 128		/* base of Neochrome screen dump */
#define FSDLEN 32000		/* standard ST screen len */

WORD read_title()	/* returns zero if okay */
{
    int chn;

    strcpy (_fullname, FSFILE);
    if ((chn = _open_file()) < 0)	/* open data file */
	return (1);			/* none, exit */

/* read the color info from the file header */

    if (_read_file(chn, FSPALETTE, FSPLEN, &palraw) < 0)
	return (1);		/* error, exit */

/* read data into our alternate screen buffer */

    if (_read_file(chn, FSDATA, FSDLEN, bak_mfdb.mp) < 0)
	return (1);		/* error, exit */

    _close_file(chn);
    return (0);			/* success */
}

title_screen ()
{
    static WORD magic[16] = {	/* magic index map (see VDI raster chap) */
	0, 2, 3, 6, 4, 7, 5, 8,
	9,10,11,14,12,15,13, 1
	};
    int i, j;
    WORD palpic [RGBLEN];

/* If an XZIP graphics file exists, check the "front-screen" bit first.
   If set, just display the first defined picture as a title screen.  (Though
   defined in mono, this format gets adjusted for any display.)
*/
    if (pi.gflags & GF_TITLE) {		/* got it? */
	_op_display (1, 0, 0, 0);	
	boot_vis = 1;			/* remember it's showing */
	return(0);
	}

/* Otherwise, start looking for a Neochrome screen ... */
    if (_mono)
	return(0);		/* but must have color */
    if (read_title())
	return(0);		/* couldn't find it */

/* OK, now must make sure screen is (temporarily) in lo-res.
   [My original use of Setscreen, to permanently switch a game that 
   had been /booted/ in lo-res into medium res, didn't work quite 
   right.  The screen DID go into medium res but the value for x_res 
   wasn't updated in the OS.  Also v_gtext() seemed to draw lots of
   garbage (parallel lines).  Is it fixable?  Currently we depend on 
   the user to boot in the correct res.] */

    boot_rez = Getrez();
    if (boot_rez == 1)		/* started in medium res? [should be!] */
	Setscreen(-1,-1, 0);	/* change to low for Neochrome */

/* Save away the current system palette, and set up the bootscreen's. */

    for (i=0; i<16; i++) {
	mapRGB (palraw [i], &palpic);		/* map new to GEM format */
	j = magic[i];
	md_color (j, &palsys [j*RGBLEN], GETC);	/* read old */
	md_color (j, &palpic, SETC);		/* and write new */
	}

/* Blit picture to the front.  This call uses our own 68K blockmove
   (not a GEM raster op) and should be safe across a rez change. */

    copy_screen (0);
    boot_vis = 1;		/* remember it's up */
}

/*------------------------------*/
/*	_end_title		*/
/*------------------------------*/

/* Come here after game has loaded, when a title screen was drawn, and wait
   for a key.  (Also called from the 68K FATAL, in case of an init error.) */

_end_title()
{
    int i;
    if (boot_vis) {		/* if no bootscreen up, skip out */

/* suppress our (blinking) cursor temporarily, for aesthetic reasons 
   and because of the above problem with v_gtext() */

	curs_vis = 0;
	_event_wait();		/* let them admire us */
	curs_vis = 1;		/* re-enabled */

	if (!_mono) {		/* if mono, palette/rez can't have changed */

/* We should probably erase the screen before changing palette or rez again,
   to avoid odd mutations (even though can create a "flash", as game goes
   on to init its own screen colors). */

	    _clear_screen();

/* Restore the (lo-rez) system palette (really necessary?) */

	    for (i=0; i<16; i++) {
		md_color (i, &palsys [i*RGBLEN], SETC);	/* write back old */
		}

/* Make sure screen is back to original rez. */

	    if (boot_rez != Getrez())
	        Setscreen(-1,-1, 1);	/* [for color XZIP, should be med] */
	    }
	boot_vis = 0;	/* all cleaned up */
	}
}

/************************************************************************/
/*	Program start							*/
/************************************************************************/

/*------------------------------*/
/*	main			*/
/*------------------------------*/

main()
{
    if (z_init() == 0)		/* do machine dependent setup */
	{
	ac_init();		/* set up alternate chars */
	pic_init();		/* and pictures */

	title_screen(); 	/* display it if present */
	_ZSTART();		/* enter 68K kernel */
	}
    z_exit();			/* here after 68K exit; clean up */
}
