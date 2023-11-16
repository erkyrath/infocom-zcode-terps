
/************************************************************************
*									*
*									*
*		    TITLE:  DIP INTERPRETIVE PROGRAM			*
*									*
*			     FOR UNIX IN C				*
*									*
*		Adapted from Unix ZIP by Duncan Blanchard		*
*									*
*			      April 1986				*
*									*
*									*
************************************************************************/

#define _DEBUG 0		/* non-zero to activate */

/********************   ZIP EDIT HISTORY   ******************************

EDIT	WHO	DESCRIPTION
----	---	---------------------------------------------------------
A	PHG	Initial implementation.  Known paging bug, rarely occurs 
		and seems to be tied to printi.  Current version loads entire 
		game to solve bug.  To test bug, run with -k64 on command line.

	dbb	Rewrote paging -- getpag, newzpc, getbyt, nxtbyt, bsplit, 
		  printi, etc.
		New routines for verify, flagptr
		Added macros, GTVBYT vs GTABYT, etc.
		Changed GTxWRD and PTxWRD complex macros to routines 
		Rewrote memory and preload inits (memini())
		Added new type, ZIPOBJ vs ZIPBYT (useful for EZIP)
		Fixed restart bug, was reloading one too few blocks
		Fixed restore, now saves user flags (SCRIPT etc)
		Fixed uninitialized tloc bug in mtime()
		Added a system call for memavail in memini().

TO DO		Change paging inits to allocate table space on the fly.
		Make combined PUTCHR routine for variable-width screen and
		  script output buffering, takes args in a parameter block.


/********************   DIP EDIT HISTORY   ******************************

EDIT	WHO	DESCRIPTION
----	---	---------------------------------------------------------
A	dbb	Initial Implementation.  Includes an alternate screen buffer
		to increase graphics speed.

TO DO		Remove printf's from sav_res() and verify().  Extend verify
		to checksum the picture file too.

**************************  PROGRAMMER'S  NOTES  ************************

	C ZIP is a generic zip that should run on any machine with a 
	decent C compiler.  The symbol names loosely follow the conven-
	tions that were begun with the PDP-11 sources.  There have been
	several departures since this source was a direct translation of
	the IBM/MS 8086 source.  

	The interpreter should be easily upgradeable to EZIP by substituting
	the proper values in the "zipdefs.h" header file.  Constants were
	used wherever necessary.

	The memory model used was a 64K machine with character oriented 
	addressing.  There are three most common data types that recur in
	the code:

		ZIPINT - which is defined as UNSIGNED SHORT;
		ZIPBYT - which is defined as UNSIGNED CHAR;
		char * - which is the basic pointer type used.

	The following are the important global data structures that are kept
	around in the ZIP:

		zpc1, zpc2 -- current program instruction, block and offset;
		zlocs -- offset to locals from z-stack bottom;
		dataspace -- pointer to start of z-data (code);
		argblk[] -- array of arguments for last opcode;
		endlod -- block number of first non-preload block;
		pagedesc[] -- structure for page mapping.

	In the interest of having a general ZIP paging scheme that is faster
	than the time stamping scheme used in the PDP-11 version, a linked
	list LRU scheme was implemented.  The LRU chain is a doubly linked
	list of blkdesc structures.  Each structure contains a previous and
	next pointer, a char ptr to the buffer and a virtual page number for
	the page currently residing in the buffer.  The global variable MRU
	is a pointer to the structure corresponding to the most recently used
	page; consequently, mru->prev points to the LRU buffer.

	The routine Getpag takes a block number and returns a char pointer
	to the start of the corresponding block.  It also performs all the
	necessary pointer manipulation and paging to make that pointer valid.

	The main loop consists of a call to the procedure NXTINS which fetches,
	decodes, and executes the instruction at the current zpc.  NXTINS 
	merely decodes the opcode to determine if it is a 2-op, 1-op, 0-op or
	extended-op and then drops into a case statement.  Part of the decoding
	process fills ARGBLK with the appropiate arguments.  Some opcodes are
	separated from the case statement as procedures because they serve work
	in conjunction with other opcodes.  The main loop is exited when the
	variable QUIT is set to true.


	Completion notes.

	There are a few things that should or must be changed before this
	interpreter could be shipped.  Currently scripting is done to a
	file cleverly named "script".  It should be changed to be a pipe
	that is queued when scripting is turned off and the file (pipe) is
	closed.  

	The second change that must be done is windowing support.  Although
	TAM.H is included below, the support routines have not yet been 
	added.  The signal for a window change must be trapped.  The 
	procedure SCRWID() should be changed to calculate terminal 
	characteristics from the structure holding this information defined
	in TAM.  Certain screen setups will make screen splitting un-
	available (used only in Seastalker).  The procedure STATUSLN will
	need to be made more flexible.  It is currently hard wired for 80
	columns.


	A note about macros.  

	A number of macros are used throughout the zip.
	
		GTVWRD, GTVBYT, PTVWRD, PTVBYT - get and put bytes and words
		into virtual dataspace that will always be preloaded and below
		the 64K boundary.  The offsets used with them are never more
		than 16 bit quantities.

		PRED(conditional) - calls the routine ppred with evaluation
		of the condition as its argument.  Predicate jumps are involved
		in a large number of ZIL instructions.

		PUSH, POP, PUSHZ, POPZ - do the appropriate manipulation
		of the system and z stacks and their stack pointers.


	Conversion to EZIP -- notes.

	Converting this ZIP to ezip should not be to great a task.  A
	file EZIPDEFS.H should be included somewhere after the file
	ZIPDEFS.H since it redefines the critical equates that make the
	difference between zip and ezip.  Some routines will have to be
	modified anyway to provide for the differences.  Searching for
	the text string "EZIP" will bring you to areas of the ZIP that 
	still need to be modified in order to make ezip.

	There are areas not flagged because the group of ops is spread
	out.  All of the property and object operations may have to 
	be altered with respect to picking up objects and properties.
	Properties have a new variable length feature and objects are
	all word quantities rather than byte quantities.  Some of the
	changes must be made to the body of the opcodes and other changes
	to the supportive routines for flags, property offsets, and
	next property routines.


	Create notes.

	Don't forget to turn off the _DEBUG switch at the top of the code.
	Otherwise some serious debugging code will be left in the zip.
	Creating versions of zip requires only the renaming of the 
	executable file to the proper game name WITH NO EXTENSION.
	
************************************************************************/

#include <stdio.h> 
#include <sys/signal.h>
#include <sys/termio.h>
#include <ctype.h>
#include <tam.h>		/* contains AT&T 7300 window support */

#include "dipdefs.h"

#include <wind.h>		/* W_POPUP */
#include <sys/window.h>

#include <fcntl.h>		/* O_RDWR, O_CREAT, etc */
#include <sys/types.h>
#include <sys/stat.h>


extern open();
extern read();
extern fstat();
extern ioctl();


/************************************************************************
*									*
*	G R A P H I C S   G L O B A L S					*
*									*
************************************************************************/

/*  I N I T I A L I Z A T I O N S  */

ZIPINT btable,		/* blockset index table (word ptr) */
    itable;		/* icon index table (word ptr) */
ZIPBYT nbsets,		/* number of blocksets in I-file */
    nicons;		/* number of icons in I-file */

ZIPINT ibase;		/* word ptr to image file */
ZIPINT rev_table[256];	/* lookup table for block display */

unsigned short *altscreen;	/* alternate screen for AT&T PC */

/*  C U R R E N T   B L O C K  */

ZIPINT bsaddr;		/* base of current blockset (word ptr) */

ZIPBYT dblock[GBLEN],	/* current block data */
    mblock[GBLEN];	/* current mask block data */
ZIPBYT negate = 0,	/* set to DO_NEGATE when highlighting active */
    maskflag = 0;	/* nonzero when active */

/*  W I N D O W S  */

short clipx1 = SCRNX1,	/* clip window coordinates (init full) */
    clipy1 = SCRNY1,
    clipx2 = SCRNX2,
    clipy2 = SCRNY2;

/************************************************************************
*									*
*	O T H E R  G L O B A L S					*
*									*
************************************************************************/

/*  M E M O R Y   R E L A T E D  */

char *dataspace;		/* data space pointer; where code lives */ 
ZIPINT memreq;			/* total dataspace requested, in blocks */

/*  F I L E   C H A N N E L S   A N D   D E S C R I P T O R S  */

int gamechn, savechn;		/* file channel storage */
FILE *scrptfd;			/* file for scripting */

char *gamfile, 
     gamfbuf[PATHSIZ],
     savfile[PATHSIZ];

/*  I / O  B U F F E R S  */

char *chrptr, *endbuf,		/* output buffer pointers */
     *p_chrptr, *p_endbuf,	/* pipe output buffer pointers */
     p_outbuf[PBUFSIZ],		/* pipe output buffer */ 
     outbuf[OBUFSIZ],		/* maximum output buffer */
     inbuf[IBUFSIZ];		/* maximum input buffer */

/*  F L A G S  */

char scripting = 0,		/* scripting flag */
     scrchk = 0,		/* flag to check for script bit */
     slflg = 1,			/* status line in place (EZIP) */
     scroll = 1,		/* windowed scrolling */
     toplin,			/* top line of screen 0 */
     screen,			/* current screen for output */
     splitable = 0,		/* ablity to support split screen */
     spltflg = 0,		/* screen split flag */
     quit = 0;			/* game op flag for quit */

/*  T T Y   */

int ttyfd,			/* file descriptor for stdio */
    ttysav1, ttysav2;		/* storage of startup tty conditions */

int winlen = 22,		/* window length */
    linecnt;			/* line count for MORE */

/*  T A B L E   P O I N T E R S  */

ZIPINT zorkid,			/* game id */
    endlod,			/* endlod pointer */
    glotab,			/* global table ptr */
    purbot;			/* pure load ptr */

/*  R A N D O M  */

int rseed;			/* seed for random numbers */
int delaytimer = DELAYINIT;	/* user-controllable game timer */

/*  P A G I N G  */

short zpc1,			/* z program counter block number */
    zpc2,			/* z program counter byte offset */
    zblk,			/* roving zpointer (bsplit/getbyt/putstr), */
    zoff,			/*   usually different than zpc */
    curblk = -1,		/* current block (same as last zpc1) */
    curpag = -1;		/* last page gotten (from getpag) */

char *curblkloc,		/* pointer to curblk block */
    *curpagloc;			/* pointer to curpag block */

struct blkdesc {
    struct blkdesc *next,	/* next descriptor ptr */
		   *prev;	/* previous descriptor ptr  */
    char *loc;			/* page pointer */
    short vpage;		/* page number */
    } pagedesc[MAXBLKS];	/* one descriptor for each virtual page */

struct blkdesc *mru,		/* most recently used blkdesc */
    *pagemap[MAXBLKS];		/* one mapping for each virtual page */

/*  Z - S T A C K   A N D   S Y S T E M   S T A C K  */

ZIPINT zstack[LSTACK],		/* z stack and stack pointer */
    *zsp;
int sstack[STKLEN],		/* system stack and stack pointer */
    *ssp;

ZIPINT zlocs;			/* pointer to z local variables */
short argblk[MAXARGS];		/* argument block (for all opcodes) */

/*  D E B U G G I N G  */

#if _DEBUG
char debug = 0;			/* debug flag */
struct ops {
    char brkflg,
         *opstr;
    } ins_tbl[256];
char opstrs[256][8];		/* opcode names */
int skipcnt = 0;		/* skip n instructions */
int bfunc;
short z1, z2;			/* for setting breakpoints at ZPCn */
struct history_list {
   short z1, z2,
         argblk[MAXARGS],
         opcode;
   char *opstring;
} op_hist[16];
int last_ins = 0;
#endif

/*  C O M M O N   R O U T I N E S   */

/* Declared here instead of in the header of every routine that uses them */

ZIPINT GTAWRD(), GTVWRD();
char lc();

ZIPINT op_copyicon(), gs_bsaddr();
ZIPBYT op_input(), md_input(), md_joystick();

/************************************************************************
*									*
*	M A I N   P R O G R A M   A N D   I N I T S			*
*									*
************************************************************************/

main(argc,argv)  
int argc; char **argv; 
{
    char *datname, *init();

    md_setup();		/* do machine dependent inits */

    if (datname = init(argc,argv)) THEN	/* get command line stuff */
      gamfile = datname;

    sysini();		/* do system initialization */
    memini();		/* setup memory, preload, and paging */
    zipini();		/* setup zip header info and table pointers */

    do {		/* main loop */
#if _DEBUG
      if (debug) THEN debugger();
      else
#endif
	nxtins();
    }
    while (quit == 0);	/* (not optimal speedwise, but structured) */

    z_exit();
}

char *init(argc,argv) 
int argc;
char **argv;
{  /* Init processes command line parameters, figures the dat file name to use,
      and sets up the debugger if requested */

    char *prog, *s, *datfile = 0, *ext = ".dat";
    short i, locmem = 0;
#if _DEBUG
    char str[10], *tstr;
    int op;
    FILE *opchnfp; 
#endif

    prog = argv[0];
    while (--argc) {
      if ((*++argv)[0] == '-') {
	for (s = &((*argv)[1]); *s; s++) {
	  switch (lc(*s)) {
#if _DEBUG
	    case 'd': {			/* turn on debugger */
		debug = ON | VERBOSE; 
		for (i = 0; i <= 255; i++) {
		  ins_tbl[i].brkflg = 0;
		  ins_tbl[i].opstr = opstrs[0];
		  }
		opchnfp = fopen("ops.dat", "r");
		while (fscanf(opchnfp, "%s %d", str, &op) != EOF) {
		  i = 0;
		  tstr = str;
		  while (*tstr) 
		    opstrs[op][i++] = *tstr++;
		  ins_tbl[op].opstr = opstrs[op];
		  }
		fclose(opchnfp);
		break;
	   	} 
#endif
	    case 'k': {			/* max dataspace request, in K */
		s++;
		while (*s) {
		  locmem *= 10;		/* make a decimal number */
		  locmem += *s - '0';	/* asciify string */
		  s++;			/* advance pointer */
		  }
		s--;			/* back up one */
		break;
		}
	    case 'g': {			/* game file to use */
		datfile = (s+1);
		while (*(s+1)) s++;	/* skip rest of arg */
		break;
		}
	    default : printf("\nUnknown switch: %c\n", lc(*s)); break;
	    }					/* end of switch */
	  }					/* end of for loop */
	}					/* end of if loop */
      }						/* end of while loop */
    if (locmem) THEN
      memreq = locmem << 1;		/* convert k to blocks */
    else memreq = 0;			/* otherwise use default */

    if (datfile == 0) THEN {
      s = prog;				/* get program name */
      i = 0;
      while (*s) 
	gamfbuf[i++] = *s++;
      s = ext;
      while (*s)
	gamfbuf[i++] = *s++;		/* add on ".dat" */
      datfile = gamfbuf;
      }
    return(datfile);
}

sysini()
{   /*  Sysini opens the data file, saves away the name as the default save
	name, and determines total available memory.
    */
    char *d, *s, *ext = ".sav";
    ZIPINT memavail;

    if ((gamechn = open(gamfile, RDONLY)) < 0) THEN {
      printf("Failed to open game file -- %s", gamfile);
      fatal("Sysini");
      }
    s = gamfile;
    d = savfile;
    while (*s != PERIOD)
      *d++ = *s++;			/* copy game file name */
    s = ext;
    while (*d++ = *s++);		/* add .SAV extension */

/* adjust memreq so it doesn't exceed memory available */

    memavail = md_avail() >> CVTBLK;	/* blocks available */

    if (memreq > memavail) THEN		/* user wanted too much, limit it */
      memreq = memavail;
    if (!memreq) THEN			/* default, ask for memavail */
      memreq = memavail;
}

memini()
{ /*	This routine compares memreq with ENDLOD and PLENTH (for combined
	game/picture files).  It determines how much dataspace to allocate,
	and does so.  It determines how much data to preload, and does so.
	It initializes paging in the space remaining.
  */
    ZIPINT maxlod,
      iendld,		/* word ptr to image endlod (relative to ibase) */
      ilenth;		/* length of image file (in words) */
    short i;
    char buffer[BLKSIZ],	/* temp space for a block */
      *md_alloc();

/*  Read the first game block into a temporary buffer, then the first
    picture block.  We temporarily set dataspace to point to this buffer, 
    so that the GTV macros work.  
*/
    dataspace = buffer;
    getblk(0, buffer);			/* get first game block */

    ibase = GTVWRD(PLENTH) + IFUDGE;	/* picture file starts here */
    bsplit(ibase);
    getblk(zblk, buffer);		/* get first picture block */

/*  (IENDLD) may be set incorrectly to zero in some test programs.
    It could be reconstructed by adding up IHEAD + 2(IBSETS) + 2(IICONS).
    In other words, must preload through the two tables which get 
    patched later.

    >>> Careful, may cross a block boundary? <<<
*/
    iendld = GTVWRD(zoff + IENDLD);
    ilenth = GTVWRD(zoff + ILENTH);

/*  To simplify virtual memory management, the entire code file is always
    preloaded.  The picture file (generally much bigger) is preloaded 
    entirely if it fits, or through iendld otherwise.
*/
    endlod = ibase + iendld;		/* total preload, in words */
    if (endlod & 0xFF) THEN
      endlod += BLKSIZ/2;		/* round up to next block */
    endlod >>= CVTBLK-1;		/* convert to blocks */

    maxlod = ibase + ilenth;		/* total length, in words */
    if (maxlod & 0xFF) THEN
      maxlod += BLKSIZ/2;		/* round up to next block */
    maxlod >>= CVTBLK-1;		/* convert to blocks */

/*  Note that our paging scheme normally requires a minimum of 2 pages in 
    the chain, one for the current code page (always locked), and a second 
    for roving pointers.

    Exceptions: When all pages are preloaded, paging is never called and 
    no chain at all is required.  In the freak case where only one page is 
    not preloaded, the "chain" also needs only one page.  Thus, an array 
    of exactly MAXBLKS paging structures is the most ever required.
*/
    if (memreq < endlod + 2) THEN
      fatal("Insufficient memory for preload");

    if (memreq >= maxlod) THEN {  /* mucho memory, take advantage */
      endlod = maxlod;		/* hack endlod to force total preload */
      memreq = maxlod;		/* reduce memreq to max needed */
      }

/*  Allocate all needed memory, re-init dataspace, and load preload */

    if ((dataspace = md_alloc(memreq * BLKSIZ)) == NULL) THEN {
      printf("Unable to allocate %d", memreq, "blocks");
      fatal("Memory allocation error");
      }
    getpre(0, endlod);			/* read in preload data */

/*  Currently, an array of blkdescs and a pagemap are declared statically
    [0..255].  Should allocate space dynamically for [endlod..memreq-1] only 
    (number of physical buffers), and a pagemap array for [0..maxlod-1] only
    (number of actual pages).

    IDEA:  call getpre(endlod, memreq) to "prime" the page buffers, and 
    mark each pagedesc and pagemap appropriately.
*/

    if (endlod < maxlod) THEN {		/* if total preload, just skip */

      for (i = 0; i < MAXBLKS; i++)
        pagemap[i] = NOT_IN_CORE;	/* no paged pages in core, yet */

      for (i = endlod; i < memreq; i++) {
        pagedesc[i].next = &pagedesc[i+1];	/* setup pointer chain */
        pagedesc[i].prev = &pagedesc[i-1];
        pagedesc[i].loc = ((char *)(dataspace + (i * BLKSIZ)));
        pagedesc[i].vpage = NO_PAGE;
        }
      i = memreq - 1;
      pagedesc[i].next = &pagedesc[endlod];	/* make the list circular */
      pagedesc[endlod].prev = &pagedesc[i];	/* excluding pre and extra */
      mru = &pagedesc[i];			/* init mru to last page */
      }
}

zipini()
{ /*	ZIPINI initializes the ZIL world's link to the interpreter.  Pointers
	to global tables are setup.  Interpreter capabilities are setup also.

	(EZIP -- set interpreter id and version)
  */
    char *ptr;
    int i;
    ZIPINT rev_byte();

    if (GTVBYT(PVERS1) != ZMVERS) THEN	/* check z-machine */
      fatal("Wrong Z-Machine version");
    if (GTVBYT(PVERS2) & 1) THEN	/* check for byte swapped file */
      fatal("Byte swapped game file");
    
    zorkid = GTVWRD(PZRKID);		/* get game id */
    glotab = GTVWRD(PGLOTB) << 1;	/* get globals base, make byte ptr */

/*  Impure code, for save/restore purposes, is ONLY in the code file. 
    The picture tables need only be patched once at startup, below.
*/
    purbot = GTVWRD(PPURBT) << 1;	/* get purbot base, make byte ptr */
    if (purbot & BYTEBITS) THEN 
      purbot += BLKSIZ;			/* round up to next block */
    purbot >>= CVTBLK;			/* convert to blocks */
    
/*  Calculate the values (but wait until restart() to set them) for the 
    first four DIP globals, using the Picture File header data, which was
    already preloaded.
*/
    nbsets = GTVBYT((ibase << 1) + IBSETS);
    nicons = GTVBYT((ibase << 1) + IICONS);
    btable = ibase + (IHEAD/2);		/* bset table, word ptr */
    itable = btable + nbsets;		/* icon table, word ptr */

/* Patch (relativize) all entries in the two Picture File tables. */

    ptr = (btable << 1) + dataspace;
    for (i=0; i<nbsets; i++) {
      PTAWRD(ptr, GTAWRD(ptr) + ibase);
      ptr += 2;
      }

    ptr = (itable << 1) + dataspace;
    for (i=0; i<nicons; i++) {
      PTAWRD(ptr, GTAWRD(ptr) + ibase);
      ptr += 2;
      }

/* initialize the reversed-byte lookup table */

    for (i=0; i<256; i++)
      rev_table[i] = rev_byte(i);

    mtime();				/* set up random seeds */
    restart(ZFALSE);			/* continue ... */
}

restart(midgame)
int midgame;				/* FALSE if called from zipini() */
{
  /*	Restart (also called by ZIPINI) reloads preload code, saves any flags
	that would be wiped out by the reload, and jumps to the game entry
	point.  (EZIP add in appropriate low memory settings.)
  */

    if (midgame) THEN			/* reload preload, jim */
      getpre(0, endlod);

/* Initialize the first four DIP globals (Picture File info) */

    putvar(16, nbsets);
    putvar(17, btable);
    putvar(18, nicons);
    putvar(19, itable);

    cls();
    locate(25,1);

    zsp = zstack + LSTACK;	/* setup stack pointers */
    ssp = sstack + STKLEN;
    zlocs = zsp - zstack;	/* make a locals pointer */
    zlocs--;	 		/* to next stack slot */

    bsplit(GTVWRD(PSTART));	/* get starting address (word ptr) */
    zpc1 = zblk;
    zpc2 = zoff + 1;		/* but always start on next ODD byte */
    newzpc();
}

/************************************************************************
*									*
*	I N S T R U C T I O N   D I S P A T C H				*
*									*
************************************************************************/

nxtins()
{ /*	NXTINS is the heart of ZIP.  It picks up the next instruction
	using NXTBYT, decodes it, gathers it arguments, and performs
	the selected operation.  The loop consists of an initial opcode
	fetch and four case statements.  Each of the four types of opcodes
	have their own argument decoding schemes (as documented in the
	ZIP DOC).  There is one label used, EXTENT (for "EXTended op ENTry).
	2-ops can also be encoded as extended ops; consequently, the 2-op
	case statement must be entered a second time for this case.

	Note that if debugging is on, a call to the debugger is made before
	each instruction is executed (thus the ZPC will reflect that number
	of arguments picked up as well as the size of the opcode.)
	(EZIP add in new opcodes)
   */

    ZIPBYT opcode, operand, ix, jx, t1, t2, id,
	 adrmode,			/* addressing mode bits */
	 nxtbyt(), getbyt();
    ZIPOBJ objx;
    char number[7],			/* scratch pad for OPPRNN */
	 *ptr,				/* scratch pointer variable */
	 *objptr, *objptr2,
	 *flagptr(), *objloc();
    short argcount, 
	 ts1, getvar();			/* temp for signed operations */
    ZIPINT temp,
	   nxtwrd(), getarg(), getwrd(), 
	   firstprp(), nxtprp();
    DIPADDR table;			/* byte tables MAY exceed 64K */

    opcode = nxtbyt();			/* get next opcode byte */
    if (opcode) THEN {
      if (opcode < ONE_OP) THEN {	/* it's a two op */

	/*  D E C O D E   F O R   2  O P   */

	operand = opcode & TWOMSK;	/* isolate operand */
	adrmode = 1;			/* addressing mode immediate */
	if (opcode & TWOMOD1) THEN	/* check for variable arg */
	  adrmode++;
	argblk[1] = getarg(adrmode);	/* get argument by adrmode */
	adrmode = 1;			/* reset to immediate */
	if (opcode & TWOMOD2) THEN	/* check for variable arg */
	  adrmode++;
	argblk[2] = getarg(adrmode);	/* get second arg by adrmode */
extent:					/* entry point for ext-encoded 2-op */
#if _DEBUG
	if (debug) THEN
	  dinfo(2, operand);		/* display opcode information */
#endif
	switch (operand) {		/* find two op */
	  case OPADD : {
	      putval(argblk[1] + argblk[2]);
	      return;
	      }
	  case OPSUB : {
	      putval(argblk[1] - argblk[2]);
	      return;
	      }
	  case OPMUL : {
	      putval(argblk[1] * argblk[2]);
	      return;
	      }
	  case OPDIV : {
	      putval(argblk[1] / argblk[2]);
	      return;
	      }
	  case OPMOD : {
	      putval(argblk[1] % argblk[2]);
	      return;
	      }
	  case OPBAND : {		
	      putval(argblk[1] & argblk[2]);
	      return;
	      }
	  case OPBOR : {		
	      putval(argblk[1] | argblk[2]);
	      return;
	      }
	  case OPBXOR : {		
	      putval(argblk[1] ^ argblk[2]);
	      return;
	      }
	  case OPBTST : {		
	      PRED((~argblk[1] & argblk[2]) == 0);
	      return;
	      }
	  case OPQEQU : {		/* EQUAL? */
	      PRED(argblk[1] == argblk[2]);
	      return;
	      }
	  case OPQLES : {		/* LESS? */
	      PRED(argblk[1] < argblk[2]);
	      return;
	      }
	  case OPQDLE : {		/* DECREMENT LESS? */
	      ts1 = getvar(argblk[1]);	/* get variable */
	      putvar(argblk[1],--ts1);	/* set dec'ed var */
	      PRED(ts1 < argblk[2]);
	      return;
	      }
	  case OPQGRT : {		/* GREATER? */
	      PRED(argblk[1] > argblk[2]);
	      return;
	      }
	  case OPQIGR : {		/* INCREMENT GREATER? */
	      ts1 = getvar(argblk[1]);
	      putvar(argblk[1],++ts1);
	      PRED(ts1 > argblk[2]);
	      return;
	      }
	  case OPSET : {		
	      putvar(argblk[1],argblk[2]); 
	      return;
	      }
	  case OPGET : {
	      table = ((ZIPINT) argblk[1]) << 1;  /* unsigned! */
	      table += argblk[2] << 1;		/* make an index into table */
	      dspltb(table);			/* split into zblk, zoff */
	      putval(getwrd());			/* return the word */
	      return;
	      }
	  case OPGETB : {
	      table = ((ZIPINT) argblk[1]) << 1;  /* unsigned! */
	      table += argblk[2];		/* make an index into table */
	      dspltb(table);			/* split into zblk, zoff */
	      bytval(getbyt());			/* return the byte */
	      return;
	      }
	  default : fatal("Undefined 2-op");	
	  }			/* end of switch statement */
	}			/* end of if op > ONE_OP */
      else
	
	/*  D E C O D E   F O R   1  O P   */

	if (opcode < ZERO_OP) THEN {
	  operand = (opcode & ONEMODE) >> 4;	/* isolate mode bits */
	  opcode = (opcode & ONEMSK) + ONE_OP;	/* isolate operator bits */
	  argblk[1] = getarg(operand);		/* get one arg */
#if _DEBUG
	  if (debug) THEN 
	    dinfo(1, opcode);		/* display opcode information */
#endif
	  switch (opcode) {
	    case OPPUSH : {
		PUSHZ(argblk[1]); 
		return;
		}
	    case OPPOP : {
		putvar(argblk[1], POPZ());
		return;
		}
	    case OPVALU : {			/* VALUE */
		putval(getvar(argblk[1]));
		return;
		}
	    case OPINC : {
		putvar(argblk[1], (getvar(argblk[1]) + 1));
		return;
		}
	    case OPDEC : {
		putvar(argblk[1], (getvar(argblk[1]) - 1));
		return;
		}
	    case OPQZER : {		/* ZERO? */
		PRED(argblk[1] == 0);
		return;
		}
	    case OPBCOM : {			/* COMPLEMENT */
		putval(~argblk[1]);
		return;
		}
	    case OPJUMP : {
		zpc2 += argblk[1] - 2; 		/* offset - normalize */
		newzpc();
		return;
		}	
	    case OPRETU : {			/* RETURN */
		zret(argblk[1]);
		return;
		}
	    default : fatal("Undefined 1-op");
	    }				/* end of switch statement */
	  }				/* end of if ONE_OP ... */
	else

	/*  D E C O D E   F O R   0  O P   */

	if (opcode < EXT_OP) THEN {
	  opcode = (opcode & ZEROMSK) + ZERO_OP; /* mask off operator bits */
#if _DEBUG
	  if (debug) THEN 
	    dinfo(0, opcode);		/* display opcode information */
#endif
	  switch (opcode) {
	    case OPNOOP : return;
	    case OPRTRU : {		/* RTRUE */
		zret(ZTRUE);
		return;
		}
	    case OPRFAL : {		/* RFALSE */
		zret(ZFALSE);
		return;
		}
	    case OPRSTA : {			/* RETURN VALUE ON STACK */
		zret(POPZ());
		return;
		}
	    case OPFSTA : {			/* FLUSH STACK */
		POPZ(); 
		return;
		}
	    case OPQUIT : {
		quit = 1;			/* break out of main loop */
		return;
		}
	    case OPCOPY : {
		ppred(ZTRUE);			/* always a "legal" copy */
		return;
		}
	    case OPVERI : {
		PRED(verify());
		return;
		}
	    default : fatal("Undefined 0-op");
	    }				/* end of switch statement */
	  }				/* end of if ZERO_OP ... */
	else {

	/*  D E C O D E   F O R   E X T  O P   */

	  opcode = (opcode & EXTMSK) + EXT_OP;	/* figure opcode */
	  if (opcode == OPCALL) THEN {		/* handle this one specially */

/* The OPCALL instruction (for DIP) has a special format:
	OPCALL, MODEBYTE1, ..., MODEBYTEn, ARG1, ..., ARGn

   There can be up to 4 mode bytes and up to 16 arguments.  Each mode
   byte contains 4 argument specifiers, which are defined the same way as
   those for "normal" extops.

   Note that the argument specifier for "no more args" occurs only if
   there are less than 16 arguments.  [Since the maximum number of locals
   is 15, this ought to be the maximum number of arguments too, right?]
*/
	    argcount = 0;			/* init arg count */
	    for (jx = 1; jx <= 4; jx++) {
	      adrmode = nxtbyt();		/* get a mode byte */
	      for (ix = 1; ix <= 4; ix++) {
		temp = adrmode & BITS87;	/* extract high two bits */
		if (temp == BITS87) THEN 
		  goto nomore;			/* last arg, exit both loops */
		argcount++;
		temp >>= 6;			/* move mode to two low bits */
		argblk[argcount] = temp;	/* save mode bits here */
		adrmode <<= 2;			/* next two mode bits */
		}
	      }
nomore:	    argblk[0] = argcount;
	    for (ix = 1; ix <= argcount; ix++) {  /* decode args in order */
	      argblk[ix] = getarg(argblk[ix]);	/* get arg and store */
	      }
#if _DEBUG
	    if (debug) THEN 
	      dinfo(3, opcode);		/* display opcode information */
#endif
	    if (argblk[1] != 0) THEN {
	      PUSHZ(zpc1);			/* save return location */
	      PUSHZ(zpc2);
	      PUSHZ(zlocs);			/* save locals */
	      bsplit(argblk[1]);		/* split new code ptr */
	      zpc1 = zblk;
	      zpc2 = zoff;
	      newzpc();				/* update the zpc */
	      zlocs = zsp - zstack;		/* make a locals pointer */
	      zlocs--;		 		/* to next stack slot*/

	      ix = nxtbyt();			/* get num locals byte */
	      argblk[0]--;			/* arg[0] has locs to init */
	      jx = 2;				/* index to first opt arg */
	      while (ix-- != 0) {		/* set optional args */
	        temp = nxtwrd();		/* get next default */
	        if (argblk[0] < 1) THEN 	/* use default */
	          PUSHZ(temp);
	        else {
	          PUSHZ(argblk[jx]);		/* save arg */
	          jx++;
	          argblk[0]--;			/* dec count of init vals */
	          }				/* end of if optional */
	        }				/* end of while */
	      return;				/* end of "real" call */
	      }
	    else {
	      putval(ZFALSE);		/* return a false */
	      return;
	      }
	    }				/* end of if OPCALL */

/* Here for all extops, except OPCALL */

	  argblk[0] = 0;			/* init arg count */
	  adrmode = nxtbyt();			/* get (single) mode byte */
	  for (ix = 1; ix <= 4; ix++) {
	    PUSH(adrmode);			/* save low two bits */
	    adrmode >>= 2;			/* get next two bits */
	    }
	  for (ix = 1; ix <= 4; ix++) {		/* get args in order */
	    adrmode = POP() & 3;		/* isolate mode bits */
	    if (adrmode == 3) THEN 
	      break;				/* no more */
	    argblk[ix] = getarg(adrmode);	/* get arg and store */
	    argblk[0]++;			/* increment counter */
	    }
	  for (ix++; ix <= 4; ix++) POP();	/* flush remaining modes */
#if _DEBUG
	  if (debug) THEN 
	    dinfo(3, opcode);		/* display opcode information */
#endif
	  switch (opcode) {
	    case XQEQU : {			/* EXTENDED EQUAL? */
		for (ix = 2; ix <= argblk[0]; ix++) {
		  if (argblk[1] == argblk[ix]) THEN {
		    ppred(ZTRUE);		/* a match!!!! */
		    return;
		    }
		  }
		ppred(ZFALSE);			/* no match found */
		return;
		}
	    case OPPUT : {	/* table must be in preload, unlike OPPUT */
		table = ((ZIPINT) argblk[1]) << 1;  /* unsigned */
		table += argblk[2] << 1;	/* make an index into table */
		PTVWRD(table, argblk[3]);	/* put word arg3 @offset */
		return;
		}
	    case OPPUTB : {
		table = ((ZIPINT) argblk[1]) << 1;  /* unsigned */
		table += argblk[2];		/* make an index into table */
		PTVBYT(table, BYTARG(3));	/* return byte */
		return;
		}
	    case OPINPUT : {
		bytval(op_input());
		return;
		}
	    case OPSHOWI : {			/* show positive icon */
		op_showicon(OPSHOWI, 1);	/* always 1st iteration */
		return;
		}
	    case OPSETI : {
		putval(op_copyicon(OPSETI));
		return;
		}
	    case OPSWAPI : {
		putval(op_copyicon(OPSWAPI));
		return;
		}
	    case OPSOUND : {
						/* [machine dependant] */
		return;
		}
	    case OPRAND : {			/* RANDOM (EZIP) */
		argblk[1] &= BYTEMSK;		/* use bottom 8 */
		temp = rand();			/* get a random number */
		putval((temp % argblk[1])+1);	/* return the remainder */
		return;
		}
	    case OPCLEAR : {
		op_clear();
		return;
		}
	    case OPSHOWN : {			/* show negative icon */
		op_showicon(OPSHOWN, 1);	/* always 1st iteration */
		return;
		}
	    case OPWIND : {
		op_clipwind();
		return;
		}
	    case OPITER : {
		op_iterinit();	/* initialize an iteration table */
		return;
		}
	    case OPLOAD : {
		putval(ZFALSE);			/* [not implemented] */
		return;
		}
	    case OPDUMP : {
		putval(ZFALSE);			/* [not implemented] */
		return;
		}
	    case OPREST : {
		PRED(sav_res(OPREST));
		return;
		}
	    case OPSAVE : {		/* (EZIP make into VAL's) */
		PRED(sav_res(OPSAVE));
		return;
		}
	    default : {
		if ((operand = opcode - EXT_OP) <= LAST_TWO_OP) THEN
		  goto extent;
		else		
		  fatal("Undefined Ext-Op");
		}				/* end of default */
	    }					/* end of switch statement */
	  }					/* end of else not ZERO */
	}					/* end of if (opcode) ... */
    else {
#if _DEBUG
      if (debug) THEN
	dinfo(-1, opcode);
#endif
      fatal("Undefined operation");
      }
}						/* end of nxtins */

/************************************************************************
*									*
*	S H A R E D   O P C O D E S 					*
*									*
************************************************************************/

zret(rtval) 		/* zret does a OPRETU with value rtval */
ZIPINT rtval;
{
    zsp = zstack + zlocs;	/* restore old top of stack */
    POPZ();		/* dummy pop */
    zlocs = POPZ();	/* restore locals */
    zpc2 = POPZ() & BYTEBITS;	/* restore caller's offset, block */
    zpc1 = POPZ();
    newzpc();		/* update the pc */
    putval(rtval);	/* and return the indicated value */
    return;
}

/************************************************************************
*									*
*	G A M E   C O M M A N D S					*
*									*
************************************************************************/

sav_res(fcn)
int fcn;
{  /*	Save and restore routines.  This routine receives the OP as a
	parameter to distinguish whether it is reading or writing.  It
	first obtains a filename (after informing of the default).  It
	then pushes all vitals on the zstack (including the zsp) and
	r/w the zstack to disk.  Then it r/w's all the impure code to 
	disk.  It only informs the user of failure because the high-level
	indicate success with "Ok."

	The save file name is a global so that it may be retained between
	save/restores.

	(EZIP.  Modify to return values (see doc) rather than predicates)
  */
    char *fptr, filename[PATHSIZ], *s, *d,
         *entstr = "Enter save file name."; 
    int i, errcode;
    ZIPINT oldflags;

    printf("%s", entstr);		/* print enter string */
    mcrlf();				/* windowed scroll */
    printf("(Default is %s): ", savfile);	/* print last save file */
    if (md_getl(filename, PATHSIZ) == 0) THEN	/* get a line of input */
      fptr = savfile;				/* use default on crlf */
    else
      fptr = filename; 			/* otherwise use entered name */
    if (scripting) THEN			/* script save file name if nec */
      fprintf(scrptfd, "%s\n(Default is %s): %s\n", entstr, savfile, fptr);
    if (fcn == OPSAVE) THEN		/* create or open accordingly */
      savechn = creat(fptr, FMODE);
    else
      savechn = open(fptr, RDONLY);
    if (savechn != -1) THEN {		/* if sucessful, save stack */
      PUSHZ(zpc1);			/* save vitals */
      PUSHZ(zpc2);
      PUSHZ(zlocs);
      PUSHZ(zorkid);
      zstack[0] = zsp - zstack;		/* relativize stack pointer */
      if (fcn == OPSAVE)		/* r/w stack */
	errcode = wrtbyts(zstack, LSTACK*2);
      else
	errcode = rdbyts(zstack, LSTACK*2);
     zsp = zstack + zstack[0];		/* unrelativize stack pointer */
     if (*zsp == zorkid) THEN 		/* check version */
      if (errcode != ZFALSE) THEN {
	POPZ();				/* throw away copy of zorkid */
	zlocs = POPZ();			/* restore vitals */
	zpc2 = POPZ() & BYTEBITS;	
	zpc1 = POPZ();
	if (fcn == OPSAVE) THEN		/* r/w impure code accordingly */
	  errcode = wrtbyts(dataspace, GTVWRD(PPURBT));
	else {

	  errcode = rdbyts(dataspace, GTVWRD(PPURBT));

	  }
	close(savechn);			/* close the file */
	if (errcode != ZFALSE) THEN {
	  s = fptr;			/* save the save file name */
	  d = savfile;
	  while (*d++ = *s++);
	  newzpc ();
	  return(ZTRUE);		/* return success */
	  }
	else
	  if (fcn == OPREST) THEN 	/* restore failure is fatal */
	    fatal("Partial read on restore");
	}				/* end of if errcode <> -1 */
      else
	for (i = 1; i <= 4; i++) POPZ();	/* flush vitals */
     else
       fatal("Wrong game or version");		/* zorkid's didn't match */
     }					/* end of if savechn */
    else {
      printf("Invalid save file");
      mcrlf();
      if (scripting) THEN
	fprintf(scrptfd, "Invalid save file\n");
      }
    return(ZFALSE);
}

verify()
{  /*   Verify computes a checksum on the entire data file, less the header.
	All pages are brought in from disk.  The checksum is then compared 
	to the checksum stored in the header. (EZIP - Remove annoucing printf)
   */
    ZIPINT chksum = 0, blocksum();
    short i, lastblk, lastoff;

    printf("Unix Interpreter Version A");	/* version */
    mcrlf();					/* windowed scroll */

    bsplit(GTVWRD(PLENTH));	/* get length of game file */
    lastblk = zblk;
    lastoff = zoff;

    chksum += blocksum(0, HDRSIZ, BLKSIZ);	/* skip the header bytes */
    for (i=1; i<lastblk; i++)
      chksum += blocksum(i, 0, BLKSIZ);
    chksum += blocksum(lastblk, 0, lastoff);	/* sum the final bytes */

    if (chksum == GTVWRD(PCHKSM)) THEN		/* desired checksum */
      return(ZTRUE);
    else 
      return(ZFALSE);
}

ZIPINT blocksum (block, off1, off2)	/* checksum a block */
short block, off1, off2;
{
    ZIPBYT buffer[BLKSIZ];
    register ZIPINT sum = 0, i;

    getblk(block, buffer);		/* read block from disk */
    for (i=off1; i<off2; i++)		/* sum between given offsets */
      sum += buffer[i];
    return(sum);
}

/*  FORMER VERIFY HACK, USED GETBYT(), NOW DEAD.

    Setting endlod to 0 tells the paging routines that there's no preload,
    i.e., search for each page in the page buffers.  If it's not there
    (as preload pages normally would not be), then it's paged in from disk.
*/

/************************************************************************
*									*
*	G R A P H I C S   O P E R A T I O N S				*
*									*
************************************************************************/

/*------------------------------*/
/*	op_showicon		*/
/*------------------------------*/

op_showicon(mode, iter)		/* mode is OPSHOWI or OPSHOWN */
int mode, iter;			/* iteration # is usually 1 */
{
    iconinfo ic1, ic2;		/* icon and mask info */
    short locx1, locy1,		/* icon display coords, before clipping */
      locx2, locy2,
      subx1, suby1,		/* sub-icon display coords, after clipping */
      subx2, suby2;
    DIPADDR addr1, addr2;	/* sub-icon, sub-mask row addresses */
    short width, yn,		/* clipped row width, row counter */
      ixoff, iyoff,		/* row offset (relative to full icon) */
      height;			/* clipped icon height */

    if (mode == OPSHOWN) THEN negate = DO_NEGATE;
    else negate = NO_NEGATE;

    if (argblk[0] == 4) THEN maskflag = 1;	/* 4th arg means mask icon */
    else maskflag = 0;

    gs_iconinfo(argblk[3], &ic1);	/* get icon header info */
    bsaddr = gs_bsaddr(ic1.bset);	/* get blockset addr (word ptr) */

    if (iter > 1) THEN			/* adjust base addr of icon data */
      ic1.addr += (ic1.width * ic1.height) * (iter - 1);

    if (maskflag) THEN {
      gs_iconinfo(argblk[4], &ic2);	/* get mask header info */

/* The DIP spec suggests handling unequal icon and mask sizes by clipping
   extra icon blocks (and ignoring extra mask blocks).  PC DIP, however,
   just returns with no action taken. */

/*    if ((ic1.width != ic2.width) || (ic1.height != ic2.height)) THEN
	return;
*/
      if (ic1.bset != ic2.bset) THEN		/* [really an error?] */
        fatal("bad mask blockset in op_showicon()");
      }

    locx1 = argblk[1];		locy1 = argblk[2];
    locx2 = locx1 + ic1.width;	locy2 = locy1 + ic1.height;

/* Compute intersection with clip region */

    subx1 = locx1;		suby1 = locy1;
    subx2 = locx2;		suby2 = locy2;

    if (subx1 < clipx1) THEN subx1 = clipx1;	/* clip left */
    if (suby1 < clipy1) THEN suby1 = clipy1;	/* clip top */
    if (subx2 > clipx2) THEN subx2 = clipx2;	/* clip right */
    if (suby2 > clipy2) THEN suby2 = clipy2;	/* clip bottom */

/* display the clipped (optionally masked) icon, looping once for each row */

    ixoff = subx1 - locx1;	/* clipped x offset, same for every row */
    width = subx2 - subx1;	/* clipped width, same for every row too */

    for (yn=suby1; yn<suby2; yn++) {
      iyoff = yn - locy1;
      addr1 = ic1.addr + (ic1.width * iyoff) + ixoff;
      if (maskflag) THEN
        addr2 = ic2.addr + (ic2.width * iyoff) + ixoff;
      showrow(addr1, addr2, width, subx1, yn);
      }

/* icon done, update the real screen from the alternate screen */

    height = suby2 - suby1;	/* clipped height */
    md_screen_update(subx1, suby1, width, height);
}

/*------------------------------*/
/*	op_copyicon		*/
/*------------------------------*/

#define TOO_NARROW (ic1.width + ixoff > ic2.width)
#define TOO_SHORT (ic1.height + iyoff > ic2.height)

ZIPINT op_copyicon(mode)	/* return must be PUTVALed */
int mode;			/* mode is OPSETI or OPSWAPI */
{
    iconinfo ic1, ic2;
    short ixoff, iyoff, row;
    DIPADDR addr1, addr2;

    gs_iconinfo(argblk[3], &ic1);	/* get icon header info */
    gs_iconinfo(argblk[4], &ic2);
    ixoff = argblk[1];			/* sub-icon offset in icon2 */
    iyoff = argblk[2];

    if (ic1.bset != ic2.bset) THEN	/* [really a fatal error?] */
      fatal("blockset mismatch in op_copyicon()");

    if (TOO_NARROW || TOO_SHORT) THEN {	  /* bad fit, return error */
      return(ZFALSE);
      }

    for (row=0; row<ic1.height; row++) {
      addr1 = ic1.addr + (ic1.width * row);
      addr2 = ic2.addr + (ic2.width * (row + iyoff)) + ixoff;
      if (mode == OPSETI) THEN
        copyrow(addr1, addr2, ic1.width);
      else
        swaprow(addr1, addr2, ic1.width);
      }
    return(ZTRUE);
}

/*------------------------------*/
/*	op_clipwind		*/
/*------------------------------*/

op_clipwind()		/* set clip window globals */
{
    register short locx1, locy1, locx2, locy2;
    short temp;

    locx1 = argblk[1];		locy1 = argblk[2];
    locx2 = argblk[3] + 1;	locy2 = argblk[4] + 1;

    if (locx1 < SCRNX1) THEN locx1 = SCRNX1;	/* minimum coords */
    if (locy1 < SCRNY1) THEN locy1 = SCRNY1;
    if (locx2 < SCRNX1) THEN locx2 = SCRNX1;
    if (locy2 < SCRNY1) THEN locy2 = SCRNY1;

    if (locx1 > SCRNX2) THEN locx1 = SCRNX2;	/* maximum coords */
    if (locy1 > SCRNY2) THEN locy1 = SCRNY2;
    if (locx2 > SCRNX2) THEN locx2 = SCRNX2;
    if (locy2 > SCRNY2) THEN locy2 = SCRNY2;

    if (locx1 > locx2) THEN {	/* exchange if necessary */
      temp = locx1;
      locx1 = locx2;
      locx2 = temp;
      }

    if (locy1 > locy2) THEN {	/* exchange if necessary */
      temp = locy1;
      locy1 = locy2;
      locy2 = temp;
      }

    clipx1 = locx1;		clipy1 = locy1;	    /* save the coords */
    clipx2 = locx2;		clipy2 = locy2;
}

/*------------------------------*/
/*	op_iterinit		*/
/*------------------------------*/

/* The first three slots in each table entry are initialized by the game; 
   the remaining slots are initialized by this routine. */

op_iterinit()		/* word ptr to Active Icon Table in argblk[1] */
{
    iconinfo ic;
    short count;
    ZIPBYT *ptr;	/* absolute ptr into Active Icon Table */
    ZIPINT addr;

    ptr = (argblk[1] << 1) + (ZIPBYT *) dataspace;
    count = GTABYT(ptr++);	/* get number of entries in table */
    PTABYT(ptr++, 1);		/* set current entry to the first */

/* [ETRAP -- check that ptr (initial & final) is within impure preload] */

    while (count--) {		/* loop once for each table entry */
      addr = GTAWRD(ptr);	/* icon addr */
      gs_iconinfo(addr, &ic);	/* get header info for this icon */

      ptr += AI_NEGATE;		/* skip over addr and position slots */
      PTABYT(ptr++, NO_NEGATE);	/* set mode to positive */
      PTABYT(ptr++, 1);		/* set current iteration to the first */

      PTABYT(ptr++, ic.bset);	/* and store icon header info */
      PTABYT(ptr++, ic.iters);
      PTABYT(ptr++, ic.width);
      PTABYT(ptr++, ic.height);
      }
}

/*------------------------------*/
/*	op_input		*/
/*------------------------------*/

/*  ARG1, delay time, is interpreted as follows:
	+int : poll the keyboard and joystick for int/60 sec. or until 
		input is detected
	   0 : poll the keyboard and joystick once, immediately
	-int : pause for [-int]/60 sec.

    ARG2 [optional] is a word pointer to an active icon table.  
    Cycle once through the table, starting with the "current" icon.
    After each icon is iterated (until the last), check for input
    (if requested).  If it's detected, halt the cycle immediately.

    The returned value is one of the following:
	+int : 7-bit ascii character
	-int : joystick position and state (see DIP spec for codes)
    NO_INPUT : no input detected (joystick is centered)
*/

ZIPBYT op_input()		/* return value needs to be PUTVALed */
{
    short delay = argblk[1];
    ZIPINT table;
    ZIPBYT temp, key;

    if (argblk[0] == 2) THEN {		/* TABLE GIVEN, ITERATE IT */

/* Note that all contents of argblk must be saved before calling iterate(),
   since that routine mungs argblk.
*/
      table = argblk[2];
      while (iterate(table)) {		/* do current entry (any more?) */

	if (delay >= 0) THEN
	  if ((key = md_input()) != NO_INPUT) THEN    /* got one, abort */
	    return(key);	    
        }
      }				/* [fall through when iterate done] */

    if (delay >= 0) THEN {	/* GET KEY WITH DELAY */

      while (delay > 0) {
        if ((key = md_input()) != NO_INPUT) THEN    /* got one, abort */
	  return(key);
	md_delay(1);		/* count down one tick */
	delay--;
	}
      key = md_input();		/* no (more) delay, return immediately */
      return(key);
      }

    else {			/* DELAY, THEN GET KEY */
      md_delay(-delay);

/* Check for input accumulated during long pause.  Return the first,
   if any, and throw away the remainder, if any.
*/
      key = md_input();
      while ((temp = md_input()) != NO_INPUT) ;
      return(key);
      }
}

/*------------------------------*/
/*	op_clear		*/
/*------------------------------*/

/* Clear the screen, black if arg1 is 0, 
     white (negative) if arg1 is -1.
*/
op_clear()
{
    short i, x, y;

    for (i=0; i<GBLEN; i++)
      dblock[i] = argblk[1];

    maskflag = 0;
    negate = 0;

    for (y=SCRNY1; y<SCRNY2; y++)
      for (x=SCRNX1; x<SCRNX2; x++)
        md_drawblock(x, y);		/* [quick and dirty version] */

/* The alternate screen has been updated, now update the real screen */

    md_screen_update(0, 0, SCRNX2, SCRNY2);
}

/************************************************************************
*									*
*	G R A P H I C S   S U B - O P E R A T I O N S			*
*									*
************************************************************************/

/*------------------------------*/
/*	showrow			*/
/*------------------------------*/

/* display a single row of icon blocks, with (optional) mask */

showrow(addr1, addr2, len, dxloc, dyloc)
DIPADDR addr1, addr2;		/* byte ptr to row data for icon, mask */
short len,			/* row length */
    dxloc, dyloc;		/* screen position */
{
    ZIPBYT irow[SCRNX2],	/* icon row image, maximum width */
      mrow[SCRNX2];		/* mask row image, maximum width */
    short i;

#if ETRAP
    if (len + dxloc > SCRNX2) THEN
      fatal("row too wide in showrow()");
#endif

/* The block ids within a row are contiguous bytes.  It's desirable to
   fetch them all at once, before we start calling gs_getblk (since it
   fetches paged data too).
*/
    dspltb(addr1);
    for (i=0; i<len; i++)
      irow[i] = getbyt();	/* get icon block ids */

    if (maskflag) THEN {
      dspltb(addr2);
      for (i=0; i<len; i++)
        mrow[i] = getbyt();	/* get mask block ids */
      }

    for (i=0; i<len; i++) {
      gs_getblk(irow[i], dblock, negate);  /* get block data (optional neg) */

      if (maskflag) THEN
        gs_getblk(mrow[i], mblock, NO_NEGATE);	/* get mask data (never neg) */

      md_drawblock(dxloc + i, dyloc);	/* and display it */
      }
}

/*------------------------------*/
/*	copyrow			*/
/*------------------------------*/

/* Copy a single row of icon blocks to another place. */

copyrow(addr1, addr2, len)
DIPADDR addr1, addr2;		/* byte ptrs to icon row data */
short len;
{
    register ZIPBYT *ptr;
    short i;

/* [ETRAP - check that addr2 is within impure preload] */

    dspltb(addr1);
    ptr = addr2 + (ZIPBYT *) dataspace;	/* absolutize the target ptr */

/* Since the block ids within the target row are PRELOADED here, it's 
   not necessary to fetch and store in separate sequences.
*/
    for (i=0; i<len; i++)
      PTABYT(ptr++, getbyt());		/* move the block ids */
}

/*------------------------------*/
/*	swaprow			*/
/*------------------------------*/

/* Swap two single rows of icon blocks. */

swaprow(addr1, addr2, len)
DIPADDR addr1, addr2;		/* byte ptrs to icon row data */
short len;
{
    ZIPBYT temp;
    register ZIPBYT *ptr1, *ptr2;
    short i;

/* [ETRAP -- check that both addresses are within impure preload] */

    ptr1 = addr1 + (ZIPBYT *) dataspace;
    ptr2 = addr2 + (ZIPBYT *) dataspace;

/* Since the block ids within both rows are PRELOADED here, it's 
   not necessary to fetch and store in separate sequences.
*/
    for (i=0; i<len; i++) {
      temp = GTABYT(ptr2);
      PTABYT(ptr2++, GTABYT(ptr1));
      PTABYT(ptr1++, temp);
      }
}

/*------------------------------*/
/*	iterate			*/
/*------------------------------*/

/* Iterate the "current" entry in the given Active Icon table, and 
   update the table.  Return zero if it was the last entry.

   This routine overwrites argblk[] and calls op_showicon().
*/

int iterate(table)
ZIPINT table;		/* word ptr to Active Icon Table */
{
    ZIPBYT mode,
      aitot, aicur,	/* total & current Active Icons */
      itot, icur,	/* total & current iteration of current AI */ 
      *ptr, *entr;	/* absolute ptrs into the table */
     
/* [careful using GTABYT macro with ptr++ and ptr+n arguments] */

    ptr = (table << 1) + (ZIPBYT *) dataspace;  /* point to head of table */
    aitot = GTABYT(ptr++);		/* number of entries in table */
    aicur = GTABYT(ptr++);		/* current entry */

    if (aicur > 1) THEN			/* point to current entry */
      entr = ptr + ((aicur - 1) * AI_ENTRY);
    else entr = ptr;

    argblk[3] = GTAWRD(entr + AI_ADDR);	/* get icon addr */
    argblk[1] = GTAWRD(entr + AI_LOCX);	/* get icon position */
    argblk[2] = GTAWRD(entr + AI_LOCY);
    argblk[0] = 3;			/* no fourth (mask) arg */

    if (GTABYT(entr + AI_NEGATE)) THEN	/* check negate flag */
      mode = OPSHOWN;
    else mode = OPSHOWI;

    icur = GTABYT(entr + AI_ICUR);	/* get AI's current iteration */
    itot = GTABYT(entr + AI_ITOT);	/* get AI's total iterations */

/* Calling the top-level op_showicon() leads to a bit of unnecessary work, 
   since it calls gs_iconinfo() for size and blockset info.  We have that
   already in the AI table, but aren't using it.
*/
    op_showicon(mode, icur);	/* display AI's current iteration */

    if (icur == itot) THEN
      icur = 0;				/* no more iterations */
    PTABYT(entr + AI_ICUR, icur + 1);	/* update AI's current iteration */

    if (aicur == aitot) THEN
      aicur = 0;			/* no more active icons */
    PTABYT(ptr - 1, aicur + 1);		/* update current AI */

    return(aicur);	/* nonzero if any more active icons */
}

/************************************************************************
*									*
*	G R A P H I C S   S U P P O R T					*
*									*
************************************************************************/

/*------------------------------*/
/*	gs_iconinfo		*/
/*------------------------------*/

#define ICHEAD 4		/* length of icon header (bytes) */

/* pick up an icon's header information */

gs_iconinfo(headaddr, ic)
ZIPINT headaddr;		/* word pointer to icon header */
iconinfo *ic;			/* leave the header info here */
{
    ZIPBYT getbyt();

    ic->addr = (headaddr << 1) + ICHEAD;    /* byte ptr to icon data */
    bsplit(headaddr);

    ic->bset = getbyt();	/* blockset */
    ic->iters = getbyt();	/* number of iterations */
    ic->width = getbyt();	/* icon size */
    ic->height = getbyt();
}

/*------------------------------*/
/*	gs_bsaddr		*/
/*------------------------------*/

#define BSHEAD 1		/* length of blockset header (words) */

/* Lookup address of given blockset.  Note that the size of each table entry
   is one word. */

ZIPINT gs_bsaddr(bset)
ZIPBYT bset;
{
    ZIPINT addr;		/* word ptr to table entry */

    addr = btable + (bset-1);	/* index into table, 1-origin */
    bsplit(addr);
    return(getwrd() + BSHEAD);	/* get word ptr, skip header */

/* Could access the table directly, if blockset index is preloaded --

    addr = btable + (bset-1);		(index into table, 1-origin)
    addr <<= 1;				(convert to byte address)
    return(GTVWRD(addr) + BSHEAD);	(get word ptr, skip header)
*/
}

/*------------------------------*/
/*	gs_getblk		*/
/*------------------------------*/

/* Get data corresponding to the given graphics block.
   Uses global bsaddr, a word ptr to (base of) current blockset. */

gs_getblk(blk, buffer, neg)
ZIPBYT blk,		/* block id, 1-255 */
    *buffer,		/* data or mask buffer ptr */
    neg;		/* negate flag (actually XOR pattern) */
{
    short i;
    ZIPINT addr;
    ZIPBYT getbyt();

    addr = bsaddr + (blk * GBLEN/2);	/* block's address (word ptr) */
    bsplit(addr);

    for (i=0; i<GBLEN; i++)		/* move data to buffer */
      *buffer++ = getbyt() ^ neg;	/*   and negate it if requested */
}

/************************************************************************
*									*
*	U T I L I T Y   R O U T I N E S					*
*									*
************************************************************************/

/*------------------------------*/
/*	dspltb			*/
/*------------------------------*/

dspltb(bytaddr)
DIPADDR bytaddr;
{  /*	Dspltb takes a (virtual) byte pointer, separates it into block
	and byte offsets, and returns them in the zblk and zoff globals.
	[In the DIP image file only, the pointer may exceed 64K.]
   */
    zblk = bytaddr >> CVTBLK;		/* extract block bits */
    zoff = bytaddr & BYTEBITS;		/* extract byte offset bits */
}

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

/************************************************************************
*									*
*	V A R I A B L E   A N D   P R E D I C A T E   S U P P O R T 	*
*									*
************************************************************************/

ZIPINT getarg(mode)
char mode;
{  /*   Getarg is a general routine called by NXTINS to get arguments for
	an opcode.  It is called with the addressing mode as an parameter
	to determine if the argument should be retrieved as from the stack,
	as immediate data (long or short), global variable or local.
   */
    ZIPBYT result, nxtbyt();
    ZIPINT nxtwrd();

    switch (mode) {
      case 0 : return(nxtwrd());	/* long immediate */
      case 1 : return(nxtbyt());	/* short immediate */
      case 2 :
	if (result = nxtbyt()) THEN	/* variable (type detrmd by getvar) */
	  return(getvar(result));
	else
	  return(POPZ());		/* stack */
      default : fatal("Undefined address mode");
      }
}

short getvar(var)
ZIPBYT var;
{  /* 	Getvar retrieves a variable value as dictated by var.  0 indicates
	return tos, 1-15 are local variables referenced through zlocs, and
	16-255 are global.
   */
    ZIPINT global;

    if (var) THEN		/* not a stack variable */
      if (var >= LOCAL) THEN {			/* not local, thus global */
	global = ((var - 16) << 1) + glotab;	/* basify, num*2 + offset */
	return(GTVWRD(global));			/* get the global value */
	}
      else {			/* get a local value */
	--var;
	return(GETLOC(var));
	}
    else
      return(*zsp);		/* return value on top of stack (don't pop) */
}

putvar(var, value)
ZIPBYT var;
short value;
{  /* Sets variable (var) to value.  See above for description of variables */
    ZIPINT global;

    if (var) THEN 		/* not a stack variable */
      if (var >= LOCAL) THEN {			/* not local, thus global */
	global = ((var - 16) << 1) + glotab;	/* basify, num*2 + offset */
	PTVWRD(global, value);			/* set the variable */
	return;
	}
      else {			/* set a local value */
	--var;
	SETLOC(var, value);
	}
    else 
      *zsp = var;		/* update top-of-stack (don't push) */
    return;
}

putval(value)
short value;
{  /*  	Many opcodes return a value.  Putval uses an immediate byte of data
	to determine to what location the value is returned.
   */
    ZIPBYT loc, nxtbyt();	/* location to put value */

    loc = nxtbyt();		/* get location indicator */
    loc ? putvar(loc, value) : PUSHZ(value);
    return;
}

bytval(value)
ZIPBYT value;
{  /*  Bytval performs a putval but assures that high bits are off. */
    putval(value & 255);
    return;
}

ppred(truth)		/* do a predicate jump */
ZIPINT truth;
{  /*  	Ppred performs a predicate jump based on truth and immediate values.
	An immediate byte is picked up to determine if the jump is long or
	short or if a return true or false should be done.
   */
    ZIPBYT jump1, jump2, nxtbyt(); 	/* predicate jump values */
    short offset;

    jump1 = nxtbyt();		/* get jump value */
    if (jump1 & BACKWARD) THEN	/* test polarity */
      truth++;			/* increment flag */
    if (jump1 & JMPLNTH) THEN 	/* one byte jump offset? */
      offset = jump1 & PREDMSK;	/* mask off special bits */
    else {			/* nope, one byte jump */
      jump1 &= PREDMSK;		/* clear out special bits */
      jump2 = nxtbyt();		/* get low order byte */
      offset = jump1;		/* get high order bits */
      offset = (offset << 8) + jump2;	/* make a word from bytes */
      if (offset & BIT14) THEN	/* is it a 14 bit 2's comp number */
	offset |= COMP16;	/* make into a 16 bit 2's comp */
      }
    if ((truth - 1) != 0) THEN {/* jump according to truth */
      if (offset != 0) THEN 	/* do jump if there is an offset */
	if (--offset) THEN {	/* do a jump */
	  offset--;		/* adjust offset */
	  zpc2 += offset;	/* add it to pc +++ */
	  newzpc(); 
	  return;
	  }
	else {			/* just return a true */
	  zret(ZTRUE);
	  return;
	  }
      else {
	zret(ZFALSE);		/* just do a return false */
	return;
	}
      }
    return;			/* no jump required */
}
	  
/************************************************************************
*									*
*	V I R T U A L   M E M O R Y   R O U T I N E S			*
*									*
************************************************************************/

ZIPINT GTAWRD(ptr)		/* get a word from preload */
ZIPBYT *ptr;
{
    return ((*ptr << 8) | *(ptr + 1));
}

ZIPINT GTVWRD(offset) 
ZIPINT offset;
{
    register ZIPBYT *ptr;

    ptr = (ZIPBYT *) dataspace + offset;
    return ((*ptr << 8) | *(ptr + 1));
}

PTAWRD(ptr, value)		/* set a word in preload */
ZIPBYT *ptr;
ZIPINT value;
{
    *ptr = value >> 8;
    *(ptr + 1) = value;
}

PTVWRD(offset, value) 
ZIPINT offset, value;
{
    register ZIPBYT *ptr;

    ptr = (ZIPBYT *) dataspace + offset;
    *ptr = value >> 8;
    *(ptr + 1) = value;
}

/*  Nxtbyt and getbyt provide the only general way of retrieving data 
    from virtual memory.  (The macros GTVBYT, PTVBYT, etc, are used only
    to access data known to be pre-loaded.)
*/

ZIPBYT nxtbyt()		/* return byte at the zpc, advance zpc */
{
    register ZIPBYT value;

    value = *(curblkloc+zpc2);	/* get the byte */
    zpc2++;			/* increment byte pointer */
    if (zpc2 >= BLKSIZ) THEN	
      newzpc();			/* assure next byte is ready */
    return(value);
}

ZIPINT nxtwrd()
{
    register ZIPINT high;	/* must EXPLICITLY get high byte first */

    high = nxtbyt() << 8;
    return (high | (nxtbyt() & 255));
}

ZIPBYT getbyt()
{  /*  	This routine takes its arguments in the globals zblk and zoff.
	It returns the indicated byte and updates the globals.
   */
    register ZIPBYT value;
    char *page, *getpag();

/*  Testing for preload here avoids unneeded calls to getpag */

    if (zblk < endlod) THEN
      page = dataspace + (zblk << CVTBLK);
    else page = getpag(zblk);

    value = *(page+zoff);	/* get the byte */

    zoff++;			/* update byte and block pointers */
    if (zoff == BLKSIZ) THEN {
      zoff = 0;
      zblk++;
      }
    return(value);		/* and return the byte */
}

ZIPINT getwrd()
{
    register ZIPINT high;	/* must EXPLICITLY get high byte first */

    high = getbyt() << 8;
    return (high | (getbyt() & 255));
}

newzpc()
{  /* 	Newzpc is called whenever the zpc might have crossed a block boundary.
	Normalize the zpc and get new page (if needed).
   */

    zpc1 += (zpc2 >> CVTBLK);	/* normalize (works if negative too) */
    zpc2 &= BYTEBITS;

    if (zpc1 != curblk) THEN {	/* crossed into new page? */
      curblk = zpc1;		/* update the zpc globals */
      curblkloc = getpag(zpc1);
      }
}
    
char *getpag(blk)	/* return a pointer to the requested page */
short blk;
{
   /*	This is the heart of the paging scheme.  It manages a doubly-linked
	list of block descriptors.  Preloaded pages are not included in this
	list so they cannot be paged out.  If the page requested is preloaded,
	a valid pointer is returned immediately.  

	Otherwise, the block is removed from the linked list, spliced into
	the front of the list and made mru.  There are two subroutines, unlink
	and relink, that manage the linked list.

	If the block is not in core, the current mru's->previous (or lru
	block) buffer is used to page in the requested block.  Then the
	information in the corresponding block descriptors is filled to
	indicate the absence of the lru and the presence of the new.  The mru
	pointer is then pointed at this block.
   */

    struct blkdesc *lru;
    short deadpage;

#if _DEBUG
    if (blk >= MAXBLKS) THEN		/* valid block request? */
      fatal("Virtual page number out of range");
#endif

    if (blk == curpag) THEN		/* same page as last time */
      return(curpagloc);		/* return immediately */

    if (blk < endlod) THEN {		/* preloaded, expand the pointer */
      curpag = blk;
      curpagloc = dataspace + (blk << CVTBLK);
      return(curpagloc); 
      }

    if (pagemap[blk] == NOT_IN_CORE) THEN {

/* When choosing the (lru) page to discard, make sure it's not the 
   current code page (where the zpc is), otherwise the newzpc buffer pointer 
   becomes invalid! -- DBB */

      lru = mru->prev;			/* get oldest page */
      deadpage = lru->vpage;
      if (deadpage == curblk) THEN {	/* but avoid using the zpc page */
        lru = lru->prev;		/* get next oldest page */
	deadpage = lru->vpage;
	}
      getblk(blk, lru->loc);		/* read new page over lru page */
      if (deadpage != NO_PAGE) THEN	/* mark old page as gone */
        pagemap[deadpage] = NOT_IN_CORE;
      pagemap[blk] = lru;		/* update map for new page */
      lru->vpage = blk;			/* update desc for new page */
      mru = lru;			/* update mru */
      }

    else				/* page is resident */
      if (pagemap[blk] != mru) THEN {	/* if already mru, do nothing */
	unlinkb(blk);			/* unsplice from wherever it is */
	relinkb(blk);			/* link it in as new mru */
	}

    curpag = blk;			/* update page globals */
    curpagloc = mru->loc;
    return(curpagloc);			/* return pointer */
}

unlinkb(block)
short block;
{  /* 	Unlink removes a block descriptor from the lru chain.
   */
    struct blkdesc *t1, *t2;

    t1 = pagemap[block]->prev;		/* get pointer to one end */
    t2 = pagemap[block]->next;		/* and the other */
    t1->next = t2;			/* swap pointers */
    t2->prev = t1;
}

relinkb(block)
short block;
{  /*  Splice a block back into the lru chain (becomes the new mru).
   */
    struct blkdesc *newblk, *lru;

    newblk = pagemap[block];		/* pointer to the splice block */
    lru = mru->prev;			/* lru pointer */
    newblk->next = mru;			/* update new desc's prev and next */
    newblk->prev = lru;
    mru->prev = newblk;			/* update mru and lru descs */
    lru->next = newblk;
    mru = newblk;			/* new mru */
}

bspltb(bytaddr)
ZIPINT bytaddr;
{  /*	Bspltb takes a (virtual) byte pointer, separates it into byte and 
	block pointers and returns them in the zblk and zoff globals.
   */
    zblk = bytaddr >> CVTBLK;		/* extract block bits */
    zoff = bytaddr & BYTEBITS;		/* extract byte offset bits */
}

bsplit(wrdaddr)
ZIPINT wrdaddr;
{  /* 	Bsplit takes a word aligned pointer, breaks it into a byte and
	block pointer, and returns them in zblk and zoff.
   */
    zblk = wrdaddr >> 8;		/* isolate block bits */
    zoff = (wrdaddr << 1) & BYTEBITS;	/* convert word offset to byte */
}

/************************************************************************
*									*
*	D I S K   I / O							*
*									*
************************************************************************/

getpre(first, final)	/* preload first through (final-1) */
short first, final;
{  /* 	Getpre is used by ZIPINI and RESTART to read in blocks.  The blocks
	are read to an offset in dataspace based on the virtual page number,
	as opposed to an "available" paging slot.
   */
    short i;
   
    for (i = first; i < final; i++) {
      getblk(i, (dataspace + (i << CVTBLK)));
      }
    return;
}

getblk(block, loc)
short block;
char *loc;
{  /*	Getblk reads in a virtual block into absolute location loc.
   */
    long offset;

    offset = block << CVTBLK;		/* calculate seek offset */
#if _DEBUG
    if (debug & VERBOSE) THEN 
      printf("\nGetting block %d(%xh) at offset %xh\n", block,block, offset);
#endif
    lseek(gamechn, offset, 0);		/* first seek to block */
    if (read(gamechn, loc, BLKSIZ) == 0) THEN 		/* so the read */
      fatal("Get block failed");	/* die on failed read */
    return;
}

wrtbyts(loc, numbyts)
char *loc;
int numbyts;
{  /*  	This routine is used by SAV_RES to write numbyts from loc to the
	save file.
   */
    if (write(savechn, loc, numbyts) != numbyts) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
}

rdbyts(loc, numbyts)
char *loc;
int numbyts;
{  /*  Rdbyts is used to read numbyts bytes into loc from the save file.
   */
    if (read(savechn, loc, numbyts) != numbyts) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
}

/************************************************************************
*									*
*	E R R O R   H A N D L I N G 					*
*									*
************************************************************************/

fatal(message)
char *message;
{  /*	Fatal gives the standard zip fatal error message (message passed)
	by caller and then performs clean up through Z_EXIT.
   */
#if _DEBUG
    int i, j, k;
#endif

    printf("\nFatal error: %s\n", message);
#if _DEBUG
    if (debug) THEN {
      printf("\nStrike any key to get history list");
      while (getchar() == -1)
        ;
      printf("\nZPC1:ZPC2   Opcode   Args");
      j = ++last_ins & 15;
      for (i = 0; i <= 15; i++) {
	printf("\n%04.4x:%04.4x   %s   ", op_hist[j].z1, op_hist[j].z2, op_hist[j].opstring);
	for (k = 0; k < MAXARGS; k++)
	  printf("%04.4x ", op_hist[j].argblk[k]);
	j = ++j & 15;
	}
      printf("\n");
      }
#endif
    z_exit();			/* exit after clean up */
}

z_exit()
{  /*	Z_exit reset the tty before exit.  If the tty is not reset, the
	user will be logged out on exit.
   */
    md_ttyres(ttyfd);			/* reset the tty please! */
    exit();
}

/************************************************************************
*									*
*	S Y S T E M   D E P E N D E N T 				*
*									*
************************************************************************/

md_setup()
{  /* 	Setup performs any system dependent initialization that must be
	done only once.
   */
    char *md_alloc();

    signal(SIGINT, z_exit);	/* handle errors without user logout */
    signal(SIGQUIT, z_exit);

    ttyfd = md_ttyini(); 	/* turn off echo and unbuffer input */

/* Allocate an alternate screen buffer for graphics.  Takes about 15.3 Kbytes.
   This should happen before memini() runs and starts to check on memory
   available for paging, etc.
*/
    altscreen = (unsigned short *) md_alloc((SCRNX2 * 2) * (SCRNY2 * GBLEN));
}

mtime()
{  /* mtime get the machine time for setting the random seed.
   */
    long time(), tloc = 0;

    rseed = time(tloc);		/* get system time */
    srand(rseed);		/* get a random seed based on time */
    return;
}

char *md_alloc(nbytes)
int nbytes;
{  /*	Allocate a segment of size nbytes and return a pointer to it.
	If an error occurs, return a null pointer.
   */
    char *malloc();

    return(malloc(nbytes));		/* NULL if error */
}

int md_avail()		/* return number of free (contiguous) bytes */
{
/* The AT&T operating system supports virtual paging, so pretend available
   memory is unlimited.
*/
    return(2<<(20-1));			/* 1Mb */
/*  return(MAXBLKS * BLKSIZ); */	/* max actually used by DIP */
}

cls()
{  /*	Cls resets the line count, clears the screen, and positions the
	cursor at the bottom of the screen.
   */
    linecnt = 0;
    printf("\033[2J\033[H");		/* clear the screen vt100 style */
    return;
}

locate(row, col)
short row,col;
{  /*	Uses ansi calls to position the cursor.
   */
    char control[10];

    sprintf(control, "\033[%1d;%1dH", row, col);
    printf("%s", control);
}

hilite(attrib)
int attrib;
{  /*	Given attribute attrib, set vt100 style attribute.
	(EZIP may want to use this code style to implement the
	hilite opcode.  It must be expand to include other attribs.)
   */
    int num;

    switch (attrib) {
      case NORMAL: {
	num = 0;
	break;
	}
      case REVERSE: {
	num = 7;
	break;
	}
      default: num = 0;
      }
    printf("\033[%1dm", num);
    return;
}

scrwid()
{  /*	Get screen width and determine splittability of screen. 
   */
    splitable = 1;			/* for now, we're always splitable */
    return(RM - LM);
}

mcrlf()
{  /*	Machine dependent (actually vt100) method for doing windowed scrolling.
   */
    if (screen == 0) THEN {		/* do work for screen 0 */
      locate(toplin,1);
      printf("\033[M");
      locate(25,1);
      linecnt++;
      if (linecnt >= winlen) THEN {
        printf("[MORE]");
        while (getchar() == -1)
	  ;
        printf("\033[6D\033[K");
        linecnt = 1;
        }
      }
    else				/* screen 1 requires no scroll */
      printf("\n");
    return;
}

int md_getl(buf, cnt)
char *buf;
int cnt;
{  /*	Machine (or OS) dependent line read.  Md_getl reads chars up to cnt.
	All unprintables or escape sequences are thrown away.  When the
	cnt'th char is typed, it echoes, disappears, and the terminal beeps.
	Backspaces are handled by backing up, printing a space and backing 
	up again.  The number of chars actually read is returned.  

	(EZIP.  This will have to be fixed to allow for internal call on
	timeout.   It also should be able to take input from alternate
	channels.)
   */
    int i = 0, c;

    if (cnt > scrwid()) THEN		/* don't allow hardware scroll */
      cnt = scrwid() - 1;
    while ((c = getchar()) != EOL) {	/* loop until char or crlf */
      if (i < cnt) THEN 		/* if enough room */
	if (c != BKSPC) THEN 		/* handle backspace specially */
	  if (isprint(c)) THEN {	/* printable? */
	    *(buf + i) = c;		/* fill buffer */
	    i++;			/* inc counter */
	    md_putc(c);			/* echo the character */
	    }
	  else 			/* not a printable char */
	    switch (c) {		/* special case chars */
	      case 3: {
#if _DEBUG
		if (debug) THEN		/* drop into debugger */
		  skipcnt = 0;
		else
#endif
		  z_exit();		/* otherwise, allow exit */
	 	break;
	  	}
	      case ESC: {
		while (getchar() != -1)	/* throw away escape sequences */
		  ;
		printf(FEEP);		/* and beep once */
		break;
		}
	      default: 
		if (c != -1) THEN	/* beep for all else */
		  printf(FEEP);
	      }				/* end of switch */
	else 				/* handle a backspace */
	  if (i) THEN {			
	    i--;		
	    *(buf + i) = NULL;		/* wipe out last char in buffer */
	    printf("\b \b");		/* rubout the last char typed */
	    }
	  else  			/* no room for backspace */
	    printf(FEEP);		/* left margin, so beep */
      else {				/* buffer is full */
	i--;				/* blank out last char and beep */
	*(buf + i) = NULL;		/* too many chars typed */
	printf("\b \b");
	printf(FEEP);
	}
      }					/* end of while loop */
    *(buf + i) = NULL;			/* make an end string */
    mcrlf();				/* and do a windowed scroll */
    return(i);
}

md_putc(byte)
char byte;
{  /* 	Machine dependent write of a character.
	(EZIP will require multiple channels for output.)
   */
    putchar(byte);
    return;
}

int md_ttyini()
{  /* 	This routine performs Unix tty magic.  It sets the input buffer
	length to 0, and turns off canonization and echo. 
   */
    struct termio ttyinfo;
    int fd, err;

    fd = fileno(stdin);		/* get the stdin file descriptor */

    err = ioctl(fd, TCGETA, &ttyinfo);
    if (err == -1) THEN
      printf("\nIOCTL(TCGETA) failed");

    ttysav1 = ttyinfo.c_lflag;
    ttysav2 = ttyinfo.c_cc[VMIN];

    ttyinfo.c_lflag &= ~ICANON;
    ttyinfo.c_lflag &= ~ECHO;	
    ttyinfo.c_cc[VMIN] = 0;

    err = ioctl(fd, TCSETA, &ttyinfo);
    if (err == -1) THEN
      printf("\nIOCTL(TCSETA) failed");    
    return (fd);
}

md_ttyres(fd)
int fd;
{  /* 	This undoes the above magic.
   */
    struct termio ttyinfo;
    int err;

    err = ioctl(fd, TCGETA, &ttyinfo);
    ttyinfo.c_lflag = ttysav1;
    ttyinfo.c_cc[VMIN] = ttysav2;
    err = ioctl(fd, TCSETA, &ttyinfo);

/*  close(fd);	*/
}

/************************************************************************
*									*
*	D E B U G G E R 						*
*									*
************************************************************************/
#if _DEBUG

debugger()
{
    char c;
    short i;

    if (skipcnt == 0) THEN {
      dump();
      printf("\nZIPDDT>");
      while ((c = getchar()) != 'q') {
	if (c != NULL_INPUT) THEN 
	  if (dbgcmd(lc(c))) THEN
	    break;
	  else
	    printf("\nZIPDDT>");
        }				/* end of while */
      }					/* end of if skipcnt */
    skipcnt--;				/* decrement our skip count */
    nxtins();				/* execute the instruction */
}

dump()
{
    if (debug & VERBOSE) THEN {
      printf("\nZPC1 : ZPC2\n");
      printf("%4.4x   %4.4x\n",zpc1 & 0xffff,zpc2);
      }
    return;
}

dinfo(optype, opcode)
ZIPINT optype, opcode;
{
    short i;
    char c;

    last_ins = ++last_ins & 15;		/* continue history list */
    op_hist[last_ins].z1 = zpc1;
    op_hist[last_ins].z2 = zpc2;
    for (i = 0; i < MAXARGS; i++) 
      op_hist[last_ins].argblk[i] = argblk[i];
    op_hist[last_ins].opcode = opcode;
    op_hist[last_ins].opstring = ins_tbl[opcode].opstr;
    PUSH(debug);
    if ((opcode == OPCALL) && (ins_tbl[OPCALL].brkflg == 2) && (argblk[1] == bfunc)) THEN {
      ins_tbl[OPCALL].brkflg |= 1;
      debug = POP();
      debug |= VERBOSE;
      PUSH(debug);
      printf("\nBreakpoint at function at %x", bfunc);
      skipcnt = 0;
      } 
    if ((ins_tbl[opcode].brkflg == 1) || ((z1 == zpc1) && (z2 == zpc2))) THEN {
      debug |= VERBOSE;
      printf("\nBreakpoint at instruction %s\n", ins_tbl[opcode].opstr);
      skipcnt = 0;
      }					/* end of if break point */
    if (debug & VERBOSE) THEN {
      switch (optype) {
	case 0: {
	  printf("\nZero-Op:  %s (%x)", ins_tbl[opcode].opstr, opcode);
	  break;
	  }
	case 1: {
	  printf("\nOne-op: %s (%x) Argument: %x", ins_tbl[opcode].opstr, opcode, argblk[1] & 0xffff);
	  break;
	  }
	case 2: {
	  printf("\nTwo-op: %s (%x) Args: %x  %x", ins_tbl[opcode].opstr, opcode, argblk[1] & 0xffff, argblk[2] & 0xffff);
	  break;
	  }
	case 3: {
	  printf("\nExt-op: %s (%x)\nArgs: ", ins_tbl[opcode].opstr, opcode);
	  for (i = 1; i <= argblk[0]; i++)
	    printf("%x ",argblk[i] & 0xffff);
	  break;
	  }
	default: {
	  printf("\nUndefined opcode %x", opcode);
	  break;
	  }
	}
      dump();
      }
    debug = POP();
    if (ins_tbl[opcode].brkflg & 1) THEN {
      printf("\nZIPDDT>");
      while ((c = getchar()) != 'q') 
	if (c != NULL_INPUT) 
	  if (dbgcmd(lc(c))) THEN
	    break;
	  else
	    printf("\nZIPDDT>");
      }
    return;
}

dbgcmd(c)
char c;
{
    char brkflg = 0;
    int brkins, i, j;

    switch (c) {
      case SPACE: {
	debug = (debug & SKIPS) | STEP;
	skipcnt = 1;
	brkflg = 1;
	break;
	}
      case 'h' : {
	printf("\n<SPACE>\t- Single step one instruction"); 
	printf("\na\t- Dump absolute page number"); 
	printf("\nb\t- Set a break point");
	printf("\nc\t- Clear break point at instruction");
	printf("\nd\t- Disable all break points"); 
	printf("\ne\t- Enable all break points"); 
	printf("\nf\t- Turn off debugger");
	printf("\ng\t- Go (proceed until break point)");
	printf("\nh\t- This message");
	printf("\nj\t- Set breakpoint at function)"); 
	printf("\nl\t- Look at Local or Global variable"); 
	printf("\nm\t- Set a memory location");
	printf("\no\t- Show input and output buffers"); 
	printf("\ns\t- Skip n instructions");
	printf("\nv\t- Toggle verbosity flag");
	printf("\nw\t- Set a Local or Global variable"); 
	printf("\nx\t- Dump a virtual block of data");
	printf("\nz\t- Set breakpoint at ZPC1:ZPC2");
	printf("\n^C\t- To exit program from debugger");
	break;
	}
      case 'j': {
 	printf("\nSet breakpoint at function (word pointer): ");
	bfunc = getnum(HEX);	
	if (bfunc) THEN
	  ins_tbl[OPCALL].brkflg = 2;
	else
	  ins_tbl[OPCALL].brkflg = 0;
	break;
	}
      case 'v': {
	debug ^= VERBOSE;
	if (debug & VERBOSE) THEN
	  printf("\nVerbosity turned ON.");
	else
	  printf("\nVerbosity turned OFF.");
	break;
	}
      case 'b': {
	printf("\nSet breakpoint at instruction: ");
 	brkins = getnum(DEC);
	ins_tbl[brkins].brkflg = 1;
	printf("Breakpoint set at instruction %s", ins_tbl[brkins].opstr);
	break;
	}
      case 'c': {
	printf("\nClear breakpoint at instruction: ");
	brkins = getnum(DEC);
	if (brkins) THEN 
	  ins_tbl[brkins].brkflg = 0;
	else {
	  for (i = 0; i <= 255; i++) 
	    ins_tbl[i].brkflg = 0;
	  }
	break;
	}
      case 'd': {
	debug &= ~BRKPT;
	break;
	}
      case 'e': {
	debug |= BRKPT;
	break;
	}
      case 'f': {
	debug = OFF;		/* turn off debugger */
	brkflg = 1;
	break;
	}
      case 'g': {
	skipcnt = -1;
	brkflg = 1;
	break;
	}
      case 'l': {
	printf("\nLook at value of Global/Local: ");
	i = getnum(HEX);
	printf("Word value is: %x", getvar(i));
	break;
	}
      case 'm': {
	printf("\nSet memory location: ");
	i = getnum(HEX);
	printf("Word value is: %x", GTVWRD(i));
	printf("\nSet to value: ");
	j = getnum(HEX);
	PTVWRD(i, j);
	break;
	}
      case 'o': {
	printf("\nThe current output buffer is:\n%s", outbuf);
	printf("\nThe character pointer starts at:\n%s", chrptr);
	printf("\n\The input buffer is:\n%s", inbuf);
	break;
	}
      case 's': {
	printf("\nExecute the next n instructions: ");
        skipcnt = getnum(DEC);
	if (skipcnt == 0) THEN
	  skipcnt = 1;
	brkflg = 1;
	break;
	}
      case 'a': {
	printf("\nDump absolute block number: ");
 	i = getnum(HEX);	
	adump(i);
	break;
	}
      case 'w': {
	printf("\nLocal/Global to set: ");
	i = getnum(HEX);
  	printf("Current value is: %x", getvar(i));
	printf("\nSet variable to value: ");
	j = getnum(HEX);
	putvar(i, j);
	break;
	}
      case 'x': {
	printf("\nDump virtual block number: ");
	i = getnum(HEX);
	vdump(i);
	break;
	}
      case 'z': {
	printf("\nSet breakpoint at ZPC1: ");
	z1 = getnum(HEX);
	printf("Set breakpoint at ZPC2: ");
	z2 = getnum(HEX);
	debug |= BRKPT;
	break;
	}
      case 3: z_exit();		/* ^C to return to UNIX */
      default: printf("\nUndefined debug command\n");
      }				/* end of switch */
    return(brkflg);
}

getnum(radix)
int radix;
{
    char control[3], numstr[10];
    int num;

    md_getl(numstr, 10);
    sprintf(control, "%%%c", radix);
    sscanf(numstr, control, &num);
    return(num);
}

vdump(blknum)		/* dump a block of data formatted */
short blknum;
{
    char *loc;

printf("\nBlock number goes in as %d", blknum);
    if (blknum < endlod) THEN
      loc = (dataspace + (blknum << CVTBLK));
    else 
      loc = pagemap[blknum]->loc;
    if (loc >= dataspace) THEN
      ddump(loc);
    else
      printf("\nLocation %d for block number %d is not valid",loc,blknum);
    return;
} 

adump(blknum)
short blknum;
{
    if ((blknum >= 0) && (blknum <= MAXBLKS)) THEN {
      printf("\nTrying to dump block %d at %d",blknum,dataspace+(blknum<<CVTBLK));
      ddump(dataspace + (blknum << CVTBLK));
      }
    else
      printf("\Invalid block number %d", blknum);
    return;
}

ddump(loc)
char *loc;
{
    short i , j;
    char *tmp;
  
    printf("\nBase: %6.6x", loc); 
    for (i = 0; i < BLKSIZ; i++) {
      if (((i % 8) == 0) && (i % 16)) THEN
	printf(" -");
      if (i % 16) THEN 
	printf(" ");
      else {
	printf("\n[%3.3x] ", i);
	tmp = loc + i;
	}
      printf("%-2.2x",*(loc+i) & 255);
      if ((i % 16) == 15) THEN { 
	printf("    [");
	for (j = 0; j <= 15; j++) 
	  if ((*(tmp+j) >= SPACE) && (*(tmp+j) <= 127)) THEN
	    printf("%c", *(tmp+j));
	  else
	    printf(".");
	printf("]");
	}
      }
    return;
} 
#endif

char lc(c)			/* lower casify */
char c;
{
    if ((c <= 'Z') && (c >= 'A')) THEN
      c += 'a' - 'A';
    return(c & 255);
}

/************************************************************************
*									*
*	M A C H I N E   D E P E N D E N T   G R A P H I C S		*
*									*
************************************************************************/

/*------------------------------*/
/*	md_screen_update	*/
/*------------------------------*/

/* A DIP icon is drawn by repeated calls to ms_drawblock.  To improve
   speed on machines with complex and slow screen access, ms_drawblock
   draws to a full-size alternate screen bitmap, instead of directly
   to the real screen.

   When the icon is complete, this routine is called to display the 
   changed portion of the alternate screen all at once.
*/

md_screen_update(locx, locy, width, height)
unsigned short locx, locy, width, height;
{
    struct urdata ur;

/* It seems that ur_srcwidth and ur_dstwidth, if used, must always be
   in multiples of two bytes.  A value of one causes an "address fault"
   hardware crash.
*/
    ur.ur_srcbase = altscreen;
    ur.ur_srcwidth = SCRNX2 * 2;	/* in bytes */
    ur.ur_dstbase = 0;
/*  ur.ur_dstwidth = 0;	*/		/* [not needed for real screen] */

    ur.ur_srcx = locx * GBLEN * 2;	/* in pixels */
    ur.ur_srcy = locy * GBLEN;
    ur.ur_dstx = ur.ur_srcx;		/* [same as for alternate screen] */
    ur.ur_dsty = ur.ur_srcy;

    ur.ur_width = width * GBLEN * 2;	/* in pixels */
    ur.ur_height = height * GBLEN;

    ur.ur_srcop = SRCSRC;
    ur.ur_dstop = DSTSRC;
    ur.ur_pattern = 0;

    ioctl(ttyfd, WIOCRASTOP, &ur);	/* use our window */
}

/*------------------------------*/
/*	md_drawblock		*/
/*------------------------------*/

/* Draw the block in dblock[].  

[  If masking is active, use the mask in mblock[].  Wherever a mask bit is
   1, the screen shows through unchanged.  Elsewhere the block is displayed.
   The second function below is used in several other DIPs, and here. ]

   Straight-forward masking function:  S' = (S AND M) OR (B AND ~M)
   Equivalent, non-obvious masking function:  S' = ((S XOR B) AND M) XOR B
*/

md_drawblock(locx, locy)
unsigned short locx, locy;	/* display coordinates */
{
    int i;
    unsigned short *altptr;	/* pointer into alternate screen */

/* AT&T PC graphics require two transformations on each byte displayed, 
   (1) reverse the bits, and (2) expand the byte to a short.  The latter
   is needed because of an even byte restriction in the screen raster 
   call, and incidentally helps to fill out the 80+ column display.  For 
   speed, a transformation lookup table is used.  
*/

#if ETRAP
    if ((locx >= SCRNX2) || (locy >= SCRNY2)) THEN
      fatal("md_drawblock position out of range");
#endif

/* 'C' note - since altscreen and altptr are short pointers and an AT&T
   short is two bytes, added arithmetic values are automatically doubled.
*/
    altptr = altscreen + (locy * SCRNX2 * GBLEN) + locx;

    for (i=0; i<GBLEN; i++) {
      if (maskflag) THEN {
	*altptr ^= rev_table[dblock[i]];
	*altptr &= rev_table[mblock[i]];    /* could skip if mblock is $FF */
	*altptr ^= rev_table[dblock[i]];
	}
      else
	*altptr = rev_table[dblock[i]];

      altptr += SCRNX2;
      }
}

/*------------------------------*/
/*	md_delay		*/
/*------------------------------*/

md_delay(ticks)		/* one tick = 1/60 second */
int ticks;
{  /*   This should be implemented with a system call rather than a
	software loop if possible, so the timing isn't hardware	dependent.
	Also a sophisticated OS can give the time to somebody else.
   */
    int i, j;

    for (i=0; i<ticks; i++) {
      j = delaytimer;		/* user-specified value */
      while (j--);
      }
}

/*------------------------------*/
/*	md_user_delay		*/
/*------------------------------*/

md_user_delay(key)	/* > and < keys control the timing loop */
ZIPBYT key;
{
    if (key == '>') THEN {	/* speed up */
      delaytimer -= DELAYDELTA;
      if (delaytimer < DELAYMIN) THEN delaytimer = DELAYMIN;
      }
    if (key == '<') THEN {	/* slow down */
      delaytimer += DELAYDELTA;
      if (delaytimer > DELAYMAX) THEN delaytimer = DELAYMAX;
      }
}

/*------------------------------*/
/*	md_input		*/
/*------------------------------*/

ZIPBYT md_input()
{  /*   Poll keyboard and joystick, if no input return immediately 
	with NO_INPUT.
   */
    ZIPBYT key;

    key = getchar();			/* [echo off, immed return] */
    if (key == 0xFF) THEN		/* no input available */
      key = NO_INPUT;
    else {
      if (key == 0x0A) THEN		/* map Unix LF to CR for DIP */
        key = 0x0D;
      md_user_delay(key);		/* check for a timer command */
      key = md_joystick(key);		/* check for joystick equivalents */
      }
    return(key);
}

/*------------------------------*/
/*	md_joystick		*/
/*------------------------------*/

/* This routine checks the state of the joystick and/or alternate keys.
   If input is detected, it's mapped into one of the joystick interface 
   values defined by DIP.

   [For the AT&T PC, number keys and cursor keys are mapped into joystick 
   values in the order suggested by their arrangement in the numeric keypad.
   The "Home" key (5) maps to the button.]
*/

ZIPBYT md_joystick(userchar)
ZIPBYT userchar;
{
    ZIPBYT mapchar;

    switch (userchar) {
    case '5':	mapchar = 128+16+15;	break;	/* button */
    case '-':	mapchar = 128+11;	break;	/* W */
    case '.':	mapchar = 128+7;	break;	/* E */
    case '2':	mapchar = 128+14;	break;	/* N */
    case '0':	mapchar = 128+13;	break;	/* S */
    case '4':	mapchar = 128+10;	break;	/* NW */
    case '6':	mapchar = 128+6;	break;	/* NE */
    case '1':	mapchar = 128+9;	break;	/* SW */
    case '3':	mapchar = 128+5;	break;	/* SE */
    default:	mapchar = userchar;	break;	/* not a joystick key */
    }
    return(mapchar);
}

