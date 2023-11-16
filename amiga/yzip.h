
/*------------------------------------------------------*/
/*	General header file for YZIP			*/
/*------------------------------------------------------*/

#ifndef YZIP_H
#define YZIP_H		/* else (been here before,) ignore it all */

/*------------------------------*/
/*	system includes 	*/
/*------------------------------*/

#include "exec/types.h"
#include "exec/exec.h"
#include "exec/libraries.h"		/* for Library.lib_Version */
#include "libraries/dos.h"
#include "libraries/dosextens.h"

#include "intuition/intuition.h"	/* for AllocRemember */
#include "intuition/intuitionbase.h"

#include "workbench/icon.h"
#include "workbench/workbench.h"	/* for disk objects */

#include "graphics/text.h"		/* SetSoftStyle flags */
#include "graphics/rastport.h"		/* SetDrMode flags */
#include "graphics/gfxbase.h"		/* for screen size vars */

#include "hardware/intbits.h"		/* for interrupt flag */

/*------------------------------*/
/*	YZIP typedefs		*/
/*------------------------------*/

#define VOID	int		/* "void" requires many forward declares */
#define GLOBAL	/**/

/**				/-* already defined in exec/types.h) *-/
typedef long            LONG;	/-* 32 bits *-/
typedef unsigned long   ULONG;
typedef short           WORD;   /-* 16 bits *-/
typedef unsigned short  UWORD;
typedef char            BYTE;	/-* 8 bits *-/
typedef unsigned char   UBYTE;

typedef unsigned char   *STRPTR;
typedef STRPTR          *APTR;
typedef short           BOOL;
typedef unsigned char   TEXT;

#define TRUE      1
#define FALSE     0
#define NULL      0
**/

typedef	unsigned char	CHAR;	/* always unsigned for us */
typedef	unsigned char	UCHAR;

#define ERR 1
#define NOERR 0

/*------------------------------*/
/*	YZIP switches		*/
/*------------------------------*/

/* This switch controls conditional ZIP/EZIP compilation */
/*   (WAS defined in batch file, where compiler is invoked) */
/*   (For XZIP must always be true) */

#define	    EZIP		1	/* set for EZIP, otherwise classic */

#define     ZDEBUG		0	/* set for debugging */
#define     bug_DiskFull	0	/* set if crashes system - FIXED */
#define     DEADCODE		0	/* set to re-include */

/*------------------------------*/
/*	color stuff		*/
/*------------------------------*/

#define MAXCOLORS 11 /*8*/	/* total YZIP color ids (w/3 grays) */
#define BASE_ID 2
#define LAST_ID  BASE_ID+MAXCOLORS-1  /*9*/

#define DEF_FORE 9	/* default Amiga foreground = white */
#define DEF_BACK 11 /*6*/	/* default Amiga background = med gray */

/*------------------------------*/
/*	window stuff		*/
/*------------------------------*/

#define AM_YSIZ 200		/* hardwired for YZIP */
#define AM_XSIZ 640

#if EZIP

/** #define Peek1 0 **/		/* (for YZIP, now a var) */
		/*8+4*/ /*8+3*/	/* ZScreen scanlines showing behind ZWindow */
#define Peek2 0     /*8+5*/	/* WBScreen scanlines showing behind ZScreen */

/* The first margin is needed for the ZScreen slider and show/hide boxes.
   Without these functions, task switching is crippled.

   The second margin is needed because of an Intuition problem that causes
   all system alert boxes (e.g. insert disk X, disk X full, etc) to appear in 
   the WB screen, normally hidden by EZIP.  This space permits the words
   "System Alert" to show through to the user.  >>> FIXED IN WB 1.2 <<<
*/
#else
/** #define Peek1 0 **/
#define Peek2 0
#endif

/* The size of the hidden titlebar is a compromise: big enough to grab
   easily, but not enough to interfere with game (e.g. Z0 compass). */

#define HIDDEN_TBAR_HEIGHT 4



#endif		/* ... end of YZIP_H conditional */

