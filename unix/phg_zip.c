/************************************************************************
*									*
*									*
*		TITLE:	Z-LANGUAGE INTERPRETIVE PROGRAM			*
*									*
*			      FOR UNIX IN C				*
*									*
*		 Copyright Infocom, Inc. (c)(p), 1985			*
*									*
*			Written by Paul H. Gross			*
*			   on 5-October-1985				*
*									*
*									*
************************************************************************/
#define _DEBUG 1
/********************   ZIP EDIT HISTORY   ******************************

EDIT	INITIALS	DESCRIPTION
----	--------	-------------------------------------------------
A	PHG		Initial implementation.  Known paging bug, rarely
			occurs and seems to be tied to printi.  Current
			version loads entire game to solve bug.  To test
			bug, run with -k64 on command line.

					
/*************************  PROGRAMMER'S  NOTES  ************************

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
		ZOBJECT - which is defined as UNSIGNED CHAR;
		char * - which is the basic pointer type used.

	The following are the important global data structures that are kept
	around in the ZIP:

		zpc -- pointer to current program instruction;
		zlocs -- offset to locals from z-stack bottom;
		dataspace -- pointer to start of z-data (code);
		argblk[] -- array of arguments for last opcode;
		preload -- pointer to first byte of non-preload;
		endlod -- block number of first non-preload block;
		pagdesc[] -- structure for page mapping.

	In the interest of having a general ZIP paging scheme that is cleaner
	than the time stamping scheme used in the PDP-11 version, a linked
	list LRU scheme was implemented.  The LRU chain is a doubly linked
	list of blkdesc structures.  Each structure contains a previous and
	next pointer, a char ptr to the buffer and a virtual page number for
	the page currently residing in the buffer.  The global variable MRU
	is a pointer to the structure corresponding to the most recently used
	page; consequently, mru->prev points to the LRU buffer.  The routine
	MAKEPTR take a block number and a byte pointer as arguments and returns
	a char pointer to the corresponding byte.  It also performs all the
	necessary pointer manipulation and paging to make that pointer valid.

	The main loop consists of a call to the procedure NXTINS which fetches,
	decodes, and executes the instruction at the current zpc.  NXTINS 
	merely decodes the opcode to determine if it is a 2-op, 1-op, 0-op or
	extended-op and then drops into a case statement.  Part of the decoding
	process fills ARGBLK with the appropiate arguments.  Some opcodes are
	separated from the case statement as procedures because they serve work
	in conjunction with other opcodes.  The main loop is exited when the
	variable QUIT is set to the value ZQUIT.


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
	
		GTAWRD, GTABYT, PTAWRD, PTABYT - get and put bytes and words
		into dataspace that will always be preloaded and below the
		the 64K boundary.  The offsets used with them are never more
		than 16 bit quantities.

		PRED(conditional) - calls the routine ppred with evaluation
		of the condition as its argument.  Predicate jumps are involved
		in a large number of ZIL instructions.

		PUSH, POP, PUSHZ, POPZ - do the appropriate manipulation
		of the system and z stacks and their stack pointers.


	Conversion to EZIP notes.

	Converting this ZIP to ezip should not be to great a task.  The
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
#include <tam.h>			/* contains AT&T 7300 window support */
#include "zipdefs.h"
/* EZIP should include ezipdefs.h */

/* 	      G L O B A L S 		*/

char *gamfile, gamfbuf[PATHSIZ],
     savfile[PATHSIZ];

/* 	M E M O R Y   R E L A T E D  	*/
char *dataspace;		/* data space pointer where code lives */ 
short memreq;			/* number of bytes requested */


/*  F I L E   C H A N N E L S   A N D   D E S C R I P T O R S  */

int gamechn, savechn;		/* file channel storage */
FILE *scrptfd;			/* file for scripting */

/*  I / O  B U F F E R S  */

char *chrptr, *endbuf,		/* output buffer pointers */
     *p_chrptr, *p_endbuf,	/* pipe output buffer pointers */
     p_outbuf[PBUFSIZ],		/* pipe output buffer */ 
     outbuf[OBUFSIZ],		/* maximum output buffer */
     inbuf[IBUFSIZ];		/* maximum input buffer */

/*  F L A G S  */

char scripting = 0,		/* scripting flag */
     scrchk,			/* flag to check for script bit */
     slflg = 1,			/* status line in place (EZIP) */
     scroll = 1,		/* windowed scrolling */
     toplin,			/* toplin of screen 0 */
     screen,			/* current screen for output */
     splitable = 0,		/* ablity to support split screen */
     spltflg = 0,		/* screen split flag */
     quit = 0;			/* game op flag for quit and restart */

/*  T T Y   */

int ttyfd,			/* file descriptor for stdio */
    ttysav;			/* storage of a startup tty condition */
FILE *ttyfp;			/* for setting up terminal i/o */
int winlen = 22,		/* window length */
    linecnt;			/* line count for MORE */


/*  	T A B L E   P O I N T E R S 	  */

ZIPINT timemd,			/* time/score mode */
    zorkid,			/* game id */
    endlod,			/* endlod pointer */
    voctab,			/* vocabulary table ptr */
    objtab,			/* object table ptr */
    glotab,			/* global table ptr */
    wrdtab,			/* word table ptr */
    wrdoff,			/* offset to current wrdtab */
    purbot,			/* pure load ptr */
    vwlen,			/* number of bytes in vocab word entry */
    vwords,			/* number of word entries in vocab table */
    vocbeg;			/* beginning of actual vocabulary */

/*  O P R E A D   G L O B A L S  */ 

char *curword,			/* ascii input word pointer */
     *lastbrk,			/* pointer to last ascii break char */
     *nxttok,			/* pointer to next input word */
     *rdbos,			/* beginning of read string (table 1) */
     *esibrks,			/* end of self-inserting break chars */
     rbrks[32],			/* string of read break chars */
     irbrks[] = {SPACE,TAB,CR,FF,PERIOD,COMMA,QMARK,NULL},
				/* ^^ initial read break chars */
     zchars[] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ  0123456789.,!?_#\'\"/\\-:()";

short rdwstr[CHRS_PER_ZWORD/3],	/* vocabulary word repository during lookup */
      rdeos,			/* read end of string */
      curoff;			/* current offset in ascii input buffer */

/* R A N D O M */

int rseed;			/* seed for random numbers */

/*  P A G I N G   */

char *zpc,			/* real zpc pointer */
     *zpage,			/* beginning of current pc page */
     *zendpg,			/* end of current pc page */
     *preload,			/* real memory endlod pointer */
     *curpage,			/* get byte current page */
     *curpend,			/* end of current page */
     pagfault;			/* flag to indicate a call to getpag */

ZIPINT zlocs;			/* pointer to z local variables */

short zpc1,			/* z-program counter block number */
    zpc2,			/* z-pc byte offset in block */
    curblk;			/* current block, usually the same as zpc1 */

struct blkdesc {
    struct blkdesc *next,	/* next descriptor ptr */
		   *prev;	/* previous descriptor ptr  */
    char *loc;			/* page pointer */
    short vpage;		/* page number */
} pagdesc[MAXBLKS], *mru;	/* 1 descriptor for each page */

struct blkdesc *vpagemap[MAXBLKS];  /* mapping for each virtual page */


/*  Z - S T A C K   A N D   S Y S T E M   S T A C K  */

ZIPINT zstack[LSTACK];		/* z stack and system stack */
int    sstack[STKLEN];		
ZIPINT *zsp;
int    *ssp;			/* stack pointers */

short argblk[MAXARGS];		/* argument block for variable length ops */


/* procedures */

char *init();


/************************************************************************
*									*
*			D E B U G G I N G 				*
*									*
************************************************************************/
#ifdef _DEBUG
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
/************************************************************************
*									*
*									*
*			M A I N   P R O G R A M				*
*									*
*									*
************************************************************************/


main(argc,argv)  
int argc; char **argv; 
{
    char *datname;

    if (datname = init(argc,argv)) THEN	/* get command line stuff */
      gamfile = datname;
    sysini();				/* do system initialization */
    zipbgn();				/* read header, preload, and
					++ setup table pointers */
    do {			/* main loop */
      while (quit == 0) 
#ifdef _DEBUG
	if (debug) THEN
	  debugger();
	else
#endif
	  nxtins();
      if (quit == ZRESTART) THEN 
	restart();
      }
    while (quit != ZQUIT);
    z_exit();
	
}


char *init(argc,argv) 
int argc;
char **argv;
{  /* Init processes command line parameters, figures the dat file name to use,
      and sets up the debugger if requested */

    char *prog, *s, *datfile = 0, *tstr, str[10], *ext = ".dat";
    short locmem = 0, i;
    int op;
    FILE *opchnfp; 

    prog = argv[0];
    while (--argc) {
      if ((*++argv)[0] == '-') {
	for (s = &((*argv)[1]); *s; s++) {
	  switch (lc(*s)) {
#ifdef _DEBUG
	    case 'd': {
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
		break;		/* turn on debugger */
	   	} 
	    case 'k': {
		s++;
		while (*s) {
		  locmem *= 10;		/* make a decimal number */
		  locmem += *s - '0';	/* asciify string */
		  s++;			/* advance pointer */
		  }
		s--;			/* back up one */
		break;
		}
#endif
	    case 'g': {
		datfile = (s+1);		/* change data file */
		while (*(s+1)) s++;	/* skip rest of arg */
		break;
		}
	    default : printf("\nUnknown switch: %c\n", lc(*s)); break;
	    }					/* end of switch */
	  }					/* end of for loop */
	}					/* end of if loop */
      }						/* end of while loop */
    if (locmem) THEN
      memreq = locmem;				/* convert k to bytes */
    else
      memreq = DATASIZ;				/* use predetermined size */
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
}				/* end of init */

sysini()
{   /*  Sysini opens the data file, saves away the name as the default save
	name, allocates the dataspace, initializes paging, and traps signals */

    short i;
    char *d, *s, *ext = ".sav", *malloc();

    if ((gamechn = open(gamfile, RDONLY)) < 0) THEN {
      printf("Failed to open game file -- %s", gamfile);
      fatal("Sysini");
      }
    s = gamfile;
    d = savfile;
    while (*s != PERIOD)
      *d++ = *s++;			/* copy game file name */
    s = ext;
    while (*d++ = *s++);		/* get the extension */
    if ((dataspace = malloc(memreq * 1024)) == NULL) THEN {
      printf("Unable to allocate %d", memreq);
      fatal("Insufficient memory");
      }
    else
    if (debug) THEN
      printf("\nDataspace begins at: %x\n", dataspace);
    for (i = 1; i < MAXBLKS; i++) {	/* initialize paging stuff */
      vpagemap[i] = NOT_IN_CORE;	/* no pages in core, yet */
      pagdesc[i].vpage = NOT_IN_CORE;	
      pagdesc[i].next = &pagdesc[i+1];	/* setup pointers */
      pagdesc[i].prev = &pagdesc[i-1];
      pagdesc[i].loc = ((char *)(dataspace + (i * BLKSIZ)));/* and loc ptrs */
      }
    pagfault = 0;
    signal(SIGINT, z_exit);		/* handle errors without logout */
    signal(SIGQUIT, z_exit);
    setup();				/* setup screen characteristics */
}

zipbgn()
{ /*	ZIPBGN initializes the ZIL world's link to the interpreter.  Pointers
	to each of the tables referenced are set up.  Type of status line, and
	interpreter capabilities are set up (split screen).  Break characters
	for opread are setup as well.  All this initialization requires the
	loading of the first game block which contains a 64 byte header of
	game file information.  (EZIP -- set interpreter id and version)
  */

    short i, first, last, source;
    char *dest, *src2;
    ZIPINT temp;
    
    getpre(0, 1);			/* get first preload block */
    if (GTABYT(PVERS1) != ZMVERS) THEN	/* check z-machine */
      fatal("Wrong Z-Machine version");
    if (GTABYT(PVERS2) & 1) THEN	/* check for byte swapped file */
      fatal("Byte swapped game file");
    if (GTABYT(PVERS2) & 2) THEN	/* check for status line mode */
      timemd++;				/* indicate time mode requested */
    if (slflg == 0) THEN		/* status line available? */
      PTABYT(PVERS2, (GTABYT(PVERS2) | ~STATBIT));/* set status line state */
    
    zorkid = GTAWRD(PZRKID);		/* get zork id */
    endlod = GTAWRD(PENDLD);		/* get endlod pointer */
    if (endlod & BYTEBITS) THEN {
      endlod >>= CVTBLK;		/* convert endlod to blocks */
      endlod++;				/* add in an extra block */
      }
    else 
      endlod >>= CVTBLK;		/* convert endlod to blocks */

/* Load in entire game for now */

    if (memreq == DATASIZ) THEN {
      endlod = GTAWRD(PLENTH);		/* length of program in words */
      endlod >>= (CVTBLK - 1);		/* convert to number of blocks */
      }

    preload = (char *)(dataspace + (endlod << CVTBLK));/* keep a real pointer */
    if (preload > ((char *)(dataspace + (memreq << TO_K)))) THEN 
					/* enough room for pre */
      fatal("Preload exceeds available memory");
    first = endlod;			/* first available block */
    last = (memreq << 1) - 1;		/* last available block */
    pagdesc[first].prev = &pagdesc[last];	/* rearrange circular list */
    pagdesc[last].next = &pagdesc[first];	/* to exclude pre & extra */
    mru = &pagdesc[last];		/* init mru to last page */
    for (i = 0; i < endlod; i++) 
      pagdesc[i].vpage = i; 
    getpre(1, endlod - 1);		/* read in the rest of preload */
    

    voctab = GTAWRD(PVOCTB);		/* set up vocab pointer */
    objtab = GTAWRD(POBJTB);		/* and the object table ptr */
    glotab = GTAWRD(PGLOTB);		/* and the globals table */
    wrdtab = GTAWRD(PWRDTB);		/* and the fwords table */
    purbot = GTAWRD(PPURBT);		/* make a purebot pointer */
    if (purbot & BYTEBITS) THEN 
      purbot += BLKSIZ;			/* round up to next block */
    purbot >>= CVTBLK;			/* convert to blocks */
    purbot &= 127;			/* clear off unwanted bits */
    
    temp = GTABYT(voctab);		/* initialize source and dest */
    source = voctab + 1;		/* source is an offset type ptr */
    dest = rbrks;
    for (i = 1; i <= temp; i++) 	/* transfer break chars */
      *dest++ = GTABYT(source++);	/* first byte is number to trans */
    esibrks = dest;
    src2 = irbrks;			/* end list with initial breaks */
    do {
      *dest++ = *src2++;
      }
    while (*(src2 - 1) != 0);		/* transfer up to and inc a null */
    vwlen = GTABYT(source++);		/* get vocab entry length */
    vwords = GTAWRD(source);		/* save number of words in vocab */
    vocbeg = source + 2;		/* set starting point of vocab tbl */
    mtime();				/* set up random seeds */
    restart();				/* use restart procedure */

}

restart()
{ /*	Restart (also called by ZIPBGN) reloads preload code, sets any flags
	that would be wiped out by the reload, and jumps to the games entry
	point.  (EZIP addin appropriate low memory settings)
  */
    char *bspltb();

    if (quit == ZRESTART) THEN {	/* reload preload, jim */
      getpre(0, endlod - 1);
      quit = 0;
      }
    if (scripting) THEN			/* reset scripting flag */
      PTAWRD(PFLAGS, (GTAWRD(PFLAGS) | SCRIPTBIT));
    else
      PTAWRD(PFLAGS, (GTAWRD(PFLAGS) & ~SCRIPTBIT));
      
    if (scroll && splitable) THEN	/* reset split flag */
      PTABYT(PVERS2, (GTABYT(PVERS2) | SPLTBIT));
    else 
      PTABYT(PVERS2, (GTABYT(PVERS2) & ~SPLTBIT));
    toplin = STATLEN + 1;		/* reset screen parameters */
    spltflg = 0;
    screen = 0;
    linecnt = 0;
    winlen = 22;
    cls();
    locate(25,1);
    zsp = zstack + LSTACK;		/* setup stack pointers */
    ssp = sstack + STKLEN;
    chrptr = outbuf;			/* reset output buffer pointers */
    if (scripting) THEN			/* reset script output buffer */
      p_chrptr = p_outbuf;
    zlocs = zsp - zstack;		/* make a locals pointer */
    zlocs--;	 			/* to next stack slot*/
    zpc = bspltb(GTAWRD(PSTART));	/* get starting address */
    fixzpc();
    return;
}


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

    ZOBJECT i, j, opcode, oprnd, nxtbyt(), getbyt();
    char *ill2op = "Undefined 2-op",
	 *ill1op = "Undefined 1-op",
	 *ill0op = "Undefined 0-op",
         number[7],			/* scratch pad for OPPRNN */
	 *ptr,				/* scratch pointer variable */
	 adrmode,			/* addressing mode bits value */
	 *objloc(),*bspltb(),*bsplit(), *makeptr(), *putstr(),
	 *objptr, *objptr2;
    short ts1, getvar();		/* temp for signed operations */
    ZIPINT temp, t1, t2, flagptr(),
	   nxtwrd(), getarg(), getwrd(), nxtprp(), gprpoff();

    opcode = nxtbyt();			/* get next opcode byte */
    if (opcode) THEN {			/* legal operation */
      if (opcode < ONE_OP) THEN {	/* it's a two op */
	if (oprnd = (opcode & TWOMSK)) THEN {	/* isolate operand */
	  adrmode = 1;			/* addressing mode immediate */
	  if (opcode & TWOMOD1) THEN	/* check for variable arg */
	    adrmode++;
	  argblk[1] = getarg(adrmode);	/* get argument by adrmode */
	  adrmode = 1;			/* reset to immediate */
	  if (opcode & TWOMOD2) THEN	/* check for variable arg */
	    adrmode++;
	  argblk[2] = getarg(adrmode);  /* get second arg by adrmode */
extent:					/* entry point for ext encoded 2 */
#ifdef _DEBUG
	  if (debug) THEN
	    dinfo(2, oprnd);		/* display opcode information */
#endif
	  switch (oprnd) {		/* find two op */
	    case OPQEQU : {		/* EQUAL? */
		PRED(argblk[1] == argblk[2]);
		return;
		}
	    case OPQLES : {		/* LESS? */
		PRED(argblk[1] < argblk[2]);
		return;
		}
	    case OPQGRT : {		/* GREATER? */
		PRED(argblk[1] > argblk[2]);
		return;
		}
	    case OPQDLE : {		/* DECREMENT LESS? */
		ts1 = getvar(argblk[1]);	/* get variable */
		putvar(argblk[1],--ts1);	/* set dec'ed var */
		PRED(ts1 < argblk[2]);
		return;
		}
	    case OPQIGR : {		/* INCREMENT GREATER? */
		ts1 = getvar(argblk[1]);
		putvar(argblk[1],++ts1);
		PRED(ts1 > argblk[2]);
		return;
		}
	    case OPQIN : {		/* IN? */
		objptr = objloc(argblk[1]);
		PRED(*(objptr+PARENT) == BYTARG(2));
		return;
		}
	    case OPBTST : {		
		PRED((~argblk[1] & argblk[2]) == 0);
		return;
		}
	    case OPBOR : {		
		putval(argblk[1] | argblk[2]);
		scrchk = 1;		/* instruction used to toggle script */
		return;
		}
	    case OPBAND : {		
		putval(argblk[1] & argblk[2]);
		scrchk = 1;		/* check scripting bit */
		return;
		}
	    case OPQFSE : {		/* FlagSET? */
		temp = flagptr(argblk[1],&argblk[2]);
		t1 = BIT16;			/* bit to shift */
		PRED(GTAWRD(temp) & (t1 >> argblk[2]));
		return;
		}
	    case OPFSET : {		/* FlagSET */
    		temp = flagptr(argblk[1],&argblk[2]);
		t1 = BIT16;			/* isolating bit */
		t2 = GTAWRD(temp) | (t1 >> argblk[2]);
		PTAWRD(temp, t2);
		return;
		}
	    case OPFCLE : {		/* FCLEAR */
    		temp = flagptr(argblk[1],&argblk[2]);
		t1 = BIT16 - 1;		/* clearing bit */
		t1 = rotate(t1, argblk[2]);
		t2 = GTAWRD(temp);
		PTAWRD(temp, (t1 & t2));
		return;
		}
	    case OPSET : {		
		putvar(argblk[1],argblk[2]); 
		return;
		}
	    case OPMOVE : {			/* (EZIP - remove BYTARGS) */
		zremove(argblk[1]);		/* remove object from cont */
		objptr = objloc(argblk[1]);	/* find loc of obj2 */
		objptr2 = objloc(argblk[2]);	/* and that of obj1 */
		*(objptr+PARENT) = BYTARG(2); 	/* obj2 into obj1's loc*/
		temp = *(objptr2+CHILD1);	/* get contents of 2's 1st */
		*(objptr2+CHILD1) = BYTARG(1);	/* obj1 now 1st in obj2*/
		if (temp != EMPTY) THEN 	/* chain into sibling */
		  *(objptr+SIBLING) = temp; 	/* yes, chain into 1's sib */
		return;
		}
	    case OPGET : {
		argblk[1] += argblk[2] << 1;	/* make an index into table*/
		objptr = bspltb(argblk[1]);	/* make a memory pointer */
		putval(getwrd(&objptr));	/* putval the word @objptr */
		return;
		}
	    case OPGETB : {
		argblk[1] += argblk[2];		/* make an index into table*/
		objptr = bspltb(argblk[1]);	/* make a memory pointer */
		bytval(getbyt(&objptr));	/* putval the word @objptr */
		return;
		}
	    case OPGETP : {			/* (EZIP remove BYTARGS) */
		temp = gprpoff(argblk[1]);	/* get property pointer */
		do {
		  i = GTABYT(temp);		/* get property id */
		  i &= PNUMSK;			/* isolate id bits */
		  if (i < BYTARG(2)) THEN {
		    temp = ((--argblk[2]) << 1) + objtab;
		    break;
		    }
		  if (i == BYTARG(2)) THEN {	/* got it */
		    i = (GTABYT(temp++)) & PSZMSK;	/* now find length */
		    if ((i >>= PROPSIZE) == 0)	THEN {/* make them low bits */
		      bytval(GTABYT(temp));
		      return;
		      }
		    else break;
		    }				/* end of if (i==BYT) */
		  temp = nxtprp(temp);		/* get next property */
		  }
		while (ZTRUE);			/* loop until prop found */
		ptr = dataspace + temp;		/* make a real pointer */
		temp = getwrd(&ptr);		/* get word at ptr */
		putval(temp);			/* return the word */
		return;
		}
	    case OPGTPT : {			/* GET PROPERTY TABLE */
	  	temp = gprpoff(argblk[1]);	/* get property pointer */
		do {
		  i = GTABYT(temp);		/* get property id */
		  i &= PNUMSK;			/* isolate id bits */
		  if (i == BYTARG(2)) THEN {	/* got it */
		    temp++;
		    break;
		    }
		  if (i < BYTARG(2)) THEN {	/* no such prop */
		    temp = 0;
		    break;
		    }
		  temp = nxtprp(temp);		/* get next property */
		  }
		while (ZTRUE);			/* loop until prop found */
		putval(temp);		/* and return the property pointer */
		return;
		}
	    case OPNEXT : {
		temp = gprpoff(argblk[1]);	/* get property pointer */
		if (argblk[2]) THEN {		/* if not a zero prop */
		  do {				/* find next property */
		    i = GTABYT(temp);		/* get prop id */
		    i &= PNUMSK;		/* mask out size bits */
		    if (i == BYTARG(2)) THEN break;
		    if (i < BYTARG(2)) THEN
		      fatal("Property not found");
		    temp = nxtprp(temp);	/* get next property */
		    }
		  while (ZTRUE);		/* do until found */
		  temp = nxtprp(temp);		/* get next prop */
	 	  }				/* end of if non-zero prop */
		putval((GTABYT(temp) & PNUMSK));/* return the property */
		return;
		}
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
	    default : fatal(ill2op);	
	    }				/* end of switch statement */
	  }				/* end of if (oprnd = opcode */
	else 				/* else it's not a valid TWO */
	  fatal(ill2op);		/* end of if(oprnd...) */
	}				/* end of if op > ONE_OP */
      else
	
	/*  D E C O D E   F O R   1  O P   */

	if (opcode < ZERO_OP) THEN {
	  oprnd = (opcode & ONEMODE) >> 4;	/* isolate mode bits */
	  opcode = (opcode & ONEMSK) + ONE_OP;	/* isolate operator bits */
	  argblk[1] = getarg(oprnd);		/* get one arg */
#ifdef _DEBUG
	  if (debug) THEN 
	    dinfo(1, opcode);		/* display opcode information */
#endif
	  switch (opcode) {
	    case OPQZER : {		/* ZERO? */
		PRED(argblk[1] == 0);
		return;
		}
	    case OPQNEX : {		/* NEXT? */
		objptr = objloc(argblk[1]);	/* get obj's location */
		i = *(objptr + SIBLING);	/* get sib slot */
		bytval(i);			/* return the byte value */
		PRED(i);			/* predicate return */
		return;
		}
	    case OPQFIR : {		/* FIRST? */
		objptr = objloc(argblk[1]);	/* get obj's location */
		i = *(objptr + CHILD1);		/* get child slot */
		bytval(i);			/* return the byte value */
		PRED(i);			/* predicate return */
		return;
		}
	    case OPLOC : {
		objptr = objloc(argblk[1]);	/* get obj's location */
		bytval(*(objptr + PARENT));	/* return byte value */
		return;
		}
	    case OPPTSI : {			/* PROPERTY TBL SIZE */
		i = GTABYT(argblk[1]-1);	/* get property id */
		i &= PSZMSK;			/* mask off id bits */
		i >>= PROPSIZE;			/* right justify bits */
		i++;				/* zero based so add 1 */
		bytval(i);			/* return byte value */
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
	    case OPPRNB : {			/* PRINT BYTE ALIGNED STRING */
		ptr = bspltb(argblk[1]);	/* get string pointer */
		putstr(&ptr);
		return;
		}
	    case OPREMO : {			/* REMOVE */
		zremove(argblk[1]);
		return;
		}
	    case OPPRND : {			/* PRINT OBJ'S DESCRIPTION */
		printd(argblk[1]);		/* print short desc */
		return;				/* and go away */
		}
	    case OPRETU : {			/* RETURN */
		zret(argblk[1]);
		return;
		}
	    case OPJUMP : {
		zpc2 += argblk[1] - 2; 		/* offset - normalize */
		if (BLOCK_CROSSED) THEN 
		  newzpc();
		else 
		  zpc += argblk[1] - 2;
		return;
		}	
	    case OPPRIN : {			/* PRINT WORD ALIGNED STRING */
		ptr = bsplit(argblk[1]);	/* get a string pointer */
		putstr(&ptr);
		return;
		}
	    case OPVALU : {			/* VALUE */
		putval(getvar(argblk[1]));
		return;
		}
	    case OPBCOM : {			/* COMPLEMENT */
		putval(~argblk[1]);
		return;
		}
	    default : fatal(ill1op);
	    }				/* end of switch statement */
	  }				/* end of if ONE_OP ... */
	else

	/*  D E C O D E   F O R   0  O P   */

	if (opcode < EXT_OP) THEN {
	  opcode = (opcode & ZEROMSK) + ZERO_OP; /* mask off operator bits */
#ifdef _DEBUG
	  if (debug) THEN 
	    dinfo(0, opcode);		/* display opcode information */
#endif
	  switch (opcode) {
	    case OPRTRU : {		/* RTRUE */
		zret(ZTRUE);
		return;
		}
	    case OPRFAL : {		/* RFALSE */
		zret(ZFALSE);
		return;
		}
	    case OPPRNI : {		/* PRINT IMMEDIATE STRING */
		printi();
		return;
		}
	    case OPPRNR : {		/* PRINT WITH CRLF */
		printi();		/* print immediate string */
		newlin();		/* with a carriage return */
		zret(ZTRUE);		/* and return a true */
		return;
		}
	    case OPNOOP : return;
	    case OPSAVE : {		/* (EZIP make into VAL's) */
		PRED(sav_res(OPSAVE));
		return;
		}
	    case OPREST : {
		PRED(sav_res(OPREST));
		return;
		}
	    case OPRSTT : {			/* RESTART */
		*chrptr = NULL;			/* end of line */
		dumpbuf();			/* and flush that output */
		if (scripting) THEN {
		  *p_chrptr = NULL;
		  dmp_pbuf();
		  }
		quit = ZRESTART;
		return;
		}
	    case OPRSTA : {			/* RETURN STACK TRUE */
		zret(POPZ());
		return;
		}
	    case OPFSTA : {			/* RETURN STACK FALSE */
		POPZ(); 
		return;
		}
	    case OPQUIT : {
		quit = ZQUIT;
		return;
		}
	    case OPCRLF : {
		newlin();
		return;
		}
	    case OPUSL : {
		statusln();
		return;		/* STATUS LINE */
		}
	    case OPVERI : {
		PRED(verify());
		return;
		}
	    default : fatal(ill0op);
	    }				/* end of switch statement */
	  }				/* end of if ZERO_OP ... */
	else {

	/*  D E C O D E   F O R   E X T  O P   */

	  opcode = (opcode & EXTMSK) + EXT_OP;	/* figure opcode */
	  adrmode = nxtbyt();			/* get mode bits byte */
	  argblk[0] = 0;			/* argument counter */
	  for (i = 1; i <= 4; i++) {		/* isolate all mode bits */
	    PUSH(adrmode);			/* save for low bits */
	    adrmode >>= 2;			/* get next two bits */
	    }
	  for (i = 1; i <= 4; i++) {		/* get args in order */
	    adrmode = POP();			/* get a byte */
	    if ((adrmode &= 3) == 3) THEN break;	/* no more */
	    argblk[i] = getarg(adrmode);	/* get arg and store */
	    argblk[0]++;			/* increment counter */
	    }
	  for (i++; i <= 4; i++) POP();		/* flush remaining modes */
#ifdef _DEBUG
	  if (debug) THEN 
	    dinfo(3, opcode);		/* display opcode information */
#endif
	  switch (opcode) {
	    case XQEQU : {			/* EXTENDED EQUAL? */
		temp = ZFALSE;			/* guilty until ... */
		for (i = 2; i <= argblk[0]; i++) { /* ?equal for each arg */
		  if (argblk[1] == argblk[i]) THEN {
		    PRED(ZTRUE);		/* a match!!!! */
		    return;
		    }
		  }
		PRED(ZFALSE);			/* no match found */
		return;
		}
	    case OPCALL : {
		if (argblk[1] != 0) THEN {
		  PUSHZ(zpc1);			/* save return location */
		  PUSHZ(zpc2);
		  PUSHZ(zlocs);			/* save locals */
		  zpc = bsplit(argblk[1]);	/* get code ptr */
		  fixzpc();			/* make zpc reflect change */
		  zlocs = zsp - zstack;		/* make a locals pointer */
		  zlocs--;		 	/* to next stack slot*/
		  i = nxtbyt();			/* get num locals byte */
		  argblk[0]--;			/* arg[0] has locs to init */
		  j = 2;			/* index to first opt arg */
		  while (i-- != 0) {		/* set optional args */
		    temp = nxtwrd();		/* get next default */
		    if (argblk[0] < 1) THEN 	/* use default */
		      PUSHZ(temp);
		    else {
		      PUSHZ(argblk[j]);		/* save arg */
		      j++;
		      argblk[0]--;		/* dec count of init vals */
		      }				/* end of if optional */
		    }				/* end of while */
		  return;			/* end of real call */
		  }
		else {
		  putval(ZFALSE);		/* return a false */
		  return;
		  }
		}				/* end of opcall */
	    case OPPUT : {
		argblk[2] <<= 1;		/* convert words to bytes */
		argblk[1] += argblk[2];		/* add in offset */
		PTAWRD(argblk[1],argblk[3]);	/* put word arg3 @offset */
		if (scrchk) THEN		/* (EZIP remove these) */
		  chkscript();
		return;
		}
	    case OPPUTB : {
		argblk[1] += argblk[2];		/* figure index */
		PTABYT(argblk[1],BYTARG(3));	/* return byte */
		return;
		}
	    case OPPUTP : {
		temp = gprpoff(argblk[1]);	/* get property pointer */
		do {
		  i = GTABYT(temp);		/* get property id */
		  i &= PNUMSK;			/* isolate id bits */
		  if (i < BYTARG(2)) THEN 
		    fatal("Property not found");/* bye-bye ... */
		  if (i == BYTARG(2)) THEN {	/* got it */
		    i = (GTABYT(temp++)) & PSZMSK;	/* now find length */
		    if ((i >>= PROPSIZE) == 0) THEN {	/* make them low bits */
		      PTABYT(temp,BYTARG(3));	/* and store byte */
		      return;
		      }
		    else break;
		    }
		  temp = nxtprp(temp);		/* get next property */
		  }
		while (ZTRUE);			/* loop until prop found */
		PTAWRD(temp, argblk[3]);	/* and return the prop */
		return;
		}
	    case OPREAD : {
		zread();
		return;
		}
	    case OPPRNC : {			/* PRINT CHAR */
		putchr(argblk[1]);
		return;
		}
	    case OPPRNN : {			/* PRINT NUMBER */
		sprintf(number, "%d", argblk[1]);
		i = 0;
		while (number[i]) {
		  putchr(number[i]);
		  i++;
		  }
		return;
		}
	    case OPRAND : {			/* RANDOM (EZIP) */
		argblk[1] &= BYTEMSK;		/* use bottom 8 */
		temp = rand();			/* get a random number */
		putval((temp % argblk[1])+1);	/* return the remainder */
		return;
		}
	    case OPPUSH : {
		PUSHZ(argblk[1]); 
		return;
		}
	    case OPPOP : {
		putvar(argblk[1], POPZ());
		return;
		}
	    case OPSPLT : {			/* SPLIT (EZIP) */
		if (splitable) THEN
		  if (argblk[1]) THEN {		/* size of window 1 */
		    toplin = STATLEN + argblk[1] + 1;
		    winlen -= argblk[1];
		    spltflg = 1;
		    for (i = 1 + STATLEN; i < toplin; i++) {
		      locate(i, 1);
		      EREOL;
		      }
		    locate(STATLEN+1, 1);
		    }
		  else {
		    toplin = STATLEN + 1;
		    winlen = 22;
		    spltflg = 0;
		    } 
		return;
		}
	    case OPSCRN : {			/* SCREEN (EZIP) */
		if (spltflg) THEN {		
		  if (argblk[1]) THEN
		    locate(STATLEN+1, 1);
		  else
		    locate(25,1);
		  screen = argblk[1];
		  }
		return;
		}
	    default : {
		if ((oprnd = opcode - EXT_OP) <= LAST_TWO_OP) THEN
		  goto extent;
		else		
		  fatal("Undefined Ext-Op");
		}				/* end of default */
	    }					/* end of switch statement */
	  }					/* end of else not ZERO */
	}					/* end of if (opcode) ... */
    else {
#ifdef _DEBUG
      if (debug) THEN
	dinfo(-1, opcode);
#endif
      fatal("Undefined operation");
      }
}						/* end of nxtins */


/************************************************************************
*									*
*		     S H A R E D   O P C O D E S 			*
*									*
************************************************************************/



zremove(obj) 		/* OPREMO separated for use by OPMOVE */
ZOBJECT obj;
{  /*  (EZIP must modify byte transfers to be word transfers) 
   */
    char *objptr, *objptr2;
    ZOBJECT i, j;

    objptr = objloc(obj);		/* get obj's loc */
    if (i = (*(objptr + PARENT))) THEN { /* if there is a parent */
      objptr2 = objloc(i);
      j = (*(objptr2 + CHILD1));	/* get parent's first */
      if (j == obj) THEN  		/* change to obj's sib */
	*(objptr2 + CHILD1) = *(objptr + SIBLING);
      else {				/* get next sib in change */
	do {
	  objptr2 = objloc(j);		/* get obj's loc */
	  j = *(objptr2 + SIBLING);	/* get sib number */
	  if (j == obj) THEN
	    break;
	  }				/* end of do loop */
	while (ZTRUE);			/* while until obj found */
	*(objptr2 + SIBLING) = *(objptr + SIBLING); /* change */
	}				/* end of else */
      *(objptr + PARENT) = 0;		/* set no parent or sib */
      *(objptr + SIBLING) = 0;
      }					/* end of parent */
    return;
}



printi()		/* OPPRNI prints an immediate string */
{ /*	Printi is the cause of some significant strife.  The problem is
	that putstr uses GETWRD to advance its pointer yet the string's 
	pointer is acquired from the current zpc.  When we return from
	putstr, zpc (zpc1 & zpc2) must be updated to reflect the advance.
	Two globals are employed for this task: pagfault and curblk.  
	Pagfault is set only by the routine GETPAG which is called when
	a non-preloaded boundary is crossed in GETBYT or NXTBYT.  CURBLK
	is set to the current z-block number (zpc1) and is 0 at all other
	times.  Thus CURBLK serves as a flag to GETPAG for figuring which
	virtual block the pointer just crossed out of.  GETPAG then gets
	the next block and increments CURBLK for restoring zpc1 on return.

  */

    char *makeptr(), *putstr();
    
    pagfault = 0;			/* ignore other page faults */
    zpc = makeptr(zpc1, zpc2);		/* update curpage boundaries */
    curblk = zpc1;			/* set flag that we're in printi */
    zpc = putstr(&zpc);			/* print string at current zpc */
    if (pagfault) THEN {		/* if print paged, fix boundaries */
      zpc1 = curblk;			/* restore virtual block number */
      zpc2 = (zpc - curpage) & BYTEBITS;/* get byte offset */
      zpc = makeptr(zpc1, zpc2);	/* validate the pointer for safety */
      fixzpc();				/* update zpage boundaries */
      pagfault = 0;			/* reset pagfault flag */
      }
    else {				/* no page fault (outside preload) */
      zpc2 = (zpc - dataspace) & BYTEBITS;	/* isolate byte pointer */
      if (zpc < preload) THEN 			/* set block accordingly */
	zpc1 = (zpc - dataspace) >> CVTBLK;	/* offset if in preload */
      else
	zpc1 = curblk;				/* curblk otherwise */
      }
    curblk = 0;				/* reset curblk flag */
    return;
}

printd(obj)
ZOBJECT obj;
{ /*	Printd prints an objects short description from the property
	table.
  */
    char *ptr, *objptr;
    ZIPINT temp;

    objptr = objloc(obj);	/* get obj's location */
    objptr += PROPS;		/* point to prop pointer */
    temp = getwrd(&objptr); 	/* get address of props */
    temp++;			/* point to description */
    ptr = bspltb(temp);		/* get a real pointer */
    putstr(&ptr);		/* print the string */
    return;			/* and go away */
}

zret(rtval) 		/* zret does a OPRETU with value rtval */
ZIPINT rtval;
{
    char *makeptr();

    zsp = zstack + zlocs;	/* restore old top of stack */
    POPZ();		/* dummy pop */
    zlocs = POPZ();	/* restore locals */
    zpc2 = POPZ() & BYTEBITS;	/* restore return location */
    zpc1 = POPZ();	/* current block */
    zpc = makeptr(zpc1, zpc2);	/* get the return page */
    fixzpc();		/* update page boundaries */
    putval(rtval);	/* and return the value */
    return;
}

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
	  errcode = wrtbyts(dataspace, GTAWRD(PPURBT));
	else
	  errcode = rdbyts(dataspace, GTAWRD(PPURBT));
	close(savechn);			/* close the file */
	if (errcode != ZFALSE) THEN {
	  s = fptr;			/* save the save file name */
	  d = savfile;
	  while (*d++ = *s++);
	  zpc = makeptr(zpc1, zpc2);	/* get page */
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
    

/************************************************************************
*									*
*	O B J E C T   O P E R A T I O N S   S U P P O R T 		*
*									*
************************************************************************/

ZIPINT flagptr(obj, flagnum)	/* return a pointer to a flagword in obj */
ZIPINT *flagnum;
ZOBJECT obj;
{  /*	Flagptr is a common routine to the three flag operations that
	returns a pointer to the correct flag word (containing flagnum) for
	obj.	(EZIP.  Modify to support three flag words.)
   */
    char *ptr;

    ptr = objloc(obj);
    if ((*flagnum) >= 16) THEN { 	/* using second flag word */
      *flagnum = (*flagnum) - 16;	/* basify */
      ptr += 2;				/* point to second word */
      }
    return(ptr - dataspace);		/* return updated pointer */
}


ZIPINT gprpoff(obj)	/* Get property pointer for obj objnum */
ZOBJECT obj;
{  /* 	GetPRoPertyOFFset might explain this procedure name.  This routine
	is shared by the property operations and returns the offset from
	the start of the dataspace for a given object's properties.
	(EZIP.  Modify for new property definitions.)
   */
    ZIPINT offset, temp;
    char *objptr, *objloc();

    objptr = objloc(obj) + PROPS;	/* get objs location */
    offset = getwrd(&objptr);		/* get prop tbl location */
    temp = GTABYT(offset) << 1;		/* get byte at offset temp */
    offset += temp + 1;			/* skip over short descrip */
    return(offset);			/* return the offset */
}

char *objloc(obj)
ZOBJECT obj;
{  /*   Return a char pointer of the object obj.  DEF_PROP must be changed
	for EZIP.
   */
    ZIPINT offset;

    offset = obj * OBJLEN;		/* get offset to object */
    offset += objtab + DEF_PROP_TBL_LEN;	/* skip default prop table */
    return(dataspace + offset);		/* return the real pointer */
}


ZIPINT nxtprp(prpptr)
ZIPINT prpptr;
{  /*	Next property.  This routine take a property offset and wades thru
	the property table to the next property.
   */
    ZOBJECT id;

    id = (*(dataspace + prpptr));	/* get property id */
    id &= PSZMSK;		/* mask off id bits */
    id >>= PROPSIZE;		/* right justify size bits */
    return(prpptr + id + 2);	/* skip extra length byte */
}

rotate(word, cnt)
ZIPINT word, cnt;
{  /*	This routine emulates a rotate instruction for use in isolate bits
	in flag instructions.
   */
    short i;

    for (i = 1; i <= cnt; i++)
      if (word & 1) THEN
	word = (word >> 1) | BIT16;
      else
	word >>= 1;
    return(word);
}

/************************************************************************
*									*
*	V A R I A B L E   A N D   P R E D I C A T E   S U P P O R T 	*
*									*
************************************************************************/

ZIPINT getarg(var)
char var;
{  /*   Getarg is a general routine called by NXTINS to get arguments for
	an opcode.  It is called with the addressing mode as an argument
	to determine if the argument should be retrieved as from the stack,
	as immediate data (long or short), global variable or local.
   */
    ZOBJECT result, nxtbyt();
    ZIPINT nxtwrd();

    switch (var) {
      case 0 : return(nxtwrd());	/* long immediate */
      case 1 : return(nxtbyt());	/* short immediate */
      case 2 :			/* variable (type deterbmined by getvar) */
	if (result = nxtbyt()) THEN 
	  return(getvar(result));
	else 
	  return(POPZ());		/* stack */
      default : fatal("Undefined address mode");
      }
}

short getvar(var)
ZOBJECT var;
{  /* 	Getvar retrieves a variable value as dictated by var.  0 indicates
	return tos, 1-15 are local variables referenced through zlocs, and
	16-255 are global.
   */
    ZIPINT global, getloc();

    if (var) THEN 		/* not a stack variable */
      if (var >= LOCAL) THEN {	/* no, get global */
	global = ((var - 16) << 1) + glotab;	/* basify, num*2 + offset */
	return(GTAWRD(global));		/* get the global value */
	}
      else 			/* get a local value */
	return(GETLOC(--var));	/* get the value */
    else
      return(*zsp);		/* return value on top of the stack */
}

putvar(var, value)
ZOBJECT var;
short value;
{  /* Sets variable (var) to value.  See above for description of variables */
    ZIPINT global;

    if (var) THEN 		/* not a stack variable */
      if (var >= LOCAL) THEN {	/* no, get global */
	global = ((var - 16) << 1) + glotab;	/* basify, num*2 + offset */
	PTAWRD(global, value);		/* set the variable */
	return;
	}
      else   			/* get a local value */
	SETLOC(--var, value);
    else 
      *zsp = var;		/* save var on the stack */
    return;
}


putval(value)
short value;
{  /*  	Many opcodes return a value.  Putval uses an immediate byte of data
	to determine to what location the value is returned.
   */
    ZOBJECT loc, nxtbyt();	/* location to put value */

    loc = nxtbyt();		/* get location indicator */
    loc ? putvar(loc, value) : PUSHZ(value);
    return;
}

bytval(value)
ZOBJECT value;
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
    ZOBJECT jump1, jump2, nxtbyt(); 	/* predicate jump values */
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
	  zpc += offset;	/* update our real pc */
	  if (BLOCK_CROSSED) THEN 
	    newzpc(); 
	  return;
	  }
	else {			/* just to a return true */
	  zret(ZTRUE);
	  return;
	  }
      else {
	zret(ZFALSE);		/* just to a return false */
	return;
	}
      }
    return;			/* no jump required */
}
	  

char *bspltb(byteptr)
ZIPINT byteptr;
{  /*	Bspltb takes a word that is a byte pointer, separates it into byte
	and block pointers and returns a valid char pointer (and assures
	a valid page).
   */
    short blk, byt;
    
    byt = byteptr & BYTEBITS;	/* extract byte offset bits */
    blk = byteptr >> CVTBLK;	/* extract block bits */
    blk &= BLOCKBITS;		/* clear unwanted bits */
    return(makeptr(blk, byt));	/* make a real memory pointer */
}

char *bsplit(wrdptr)
ZIPINT wrdptr;
{  /* 	Bsplit takes a word aligned pointer and breaks it into a byte and
	block pointer and then returns a valid char pointer (and page).
   */
    short blk, byt;

    blk = wrdptr >> 8;			/* isolate block bits */
    byt = (wrdptr << 1) & BYTEBITS;	/* convert word offset to byte */
    return(makeptr(blk,byt));	/* return a real memory pointer */
}


/************************************************************************
*									*
*	V I R T U A L   M E M O R Y  R O U T I N E S			*
*									*
************************************************************************/


ZOBJECT getbyt(ptr)
char **ptr;
{  /*  	This routine takes a pointer to a pointer and returns the associated
	byte and increments the pointer.  If the pointer is within preload
	it is assumed to be valid, otherwise it is compared to block boundaries
	and a new page is retrieved if the pointer does not lie between.
	Curpage and Curpend are global pointers that are updated with each
	pointer created (or validated).  
   */
    ZOBJECT value;
    char *getpag();

    if (*ptr >= preload) THEN 		/* pointer valid? */
      if ((*ptr < curpage) || (*ptr >= curpend)) THEN 
	*ptr = getpag(*ptr, curpage); 
    value = **ptr;		/* get the byte */
    (*ptr)++;			/* auto increment the pointer */
    return((ZOBJECT)(value));		/* and return the byte */
}

ZIPINT getwrd(ptr)
char **ptr;
{  /*	Getwrd simply retrieves a word by successive calls to getbyt */
    ZIPINT value;

    value = getbyt(ptr);		/* get first byte */
    value = (value << 8) + getbyt(ptr);	/* get second and add in */
    return(value);
}

ZOBJECT nxtbyt()
{  /* 	Nxtbyt, along with getbyt, provide the only ways of retrieving non-
	preloaded bytes from virtual memory.  It too validates the pointer
	if it is not within the current boundaries for the zpc (zpage and
	zendpg).  
   */
    char *getpag();
    ZOBJECT value;

    if (zpc >= preload) THEN 		/* pointer valid? */
      if ((zpc < zpage) || (zpc >= zendpg)) THEN {
	zpc = getpag(zpc, zpage);	/* get the page that ptr points to */
	fixzpc();			/* update page boundaries */
	}
    zpc2++;			/* increment byte pointer too */
    value = (ZOBJECT)(*zpc++);	/* get the byte */
    if (BLOCK_CROSSED) THEN	
      newzpc();			/* assure block is in memory */
    return(value);
}

ZIPINT nxtwrd()
{  /* Nxtwrd makes two successive calles to nxtbyt. */
    ZIPINT value;
    ZOBJECT nxtbyt();

    value = nxtbyt();			/* get the high byte */
    value = (value << 8) + nxtbyt();	/* shift and get the low byte */
    return(value);
}

newzpc()
{  /* 	Newzpc is called when a z-block boundary has been crossed.  It
	calls MAKEPTR to assure that the page is in memory. 
   */

    zpc1 += (zpc2 >> CVTBLK);	/* add in extra blocks */
    zpc2 &= BYTEBITS;		/* shave excess */
    zpc = makeptr(zpc1, zpc2);	/* get the page */
    fixzpc();
    return;
}


fixzpc()
{  /* 	Fixzpc is called after a called to MAKEPTR for a new zpc.  This routine
	updates the zpage boundaries with the most recently set curpage
	boundaries.  It also assures that zpc1 and zpc2 are in sync with zpc.
	This routine is only valid if (zpc = makeptr(blk, byt) was executed.
   */
    zpage = curpage;		/* set current page for zpc */
    zendpg = curpend;		/* set z end of page marker */
    zpc2 = (zpc - curpage) & BYTEBITS;	/* isolate byte offset in real ptr */
    if (zpc <= preload) THEN 
      zpc1 = (zpc - dataspace) >> CVTBLK;	/* convert addr to blocks */
    else 			/* not preloaded, so look up page */
      zpc1 = mru->vpage;	/* get virtual page number from structure */
    return;
}

char *getpag(ptr, page)
char *ptr, *page;
{
/*  Getpag is called when a page boundary is crossed in nxtbyt or getbyt.
    Note that the page is never preloaded page.  Curblk is set by printi
    to indicate that that procedure is currently invoked; consequently, we
    know that the next page to be gotten is curblk + 1 (ZPC1).  Otherwise
    the next block is determined by looking at the virtual page number of
    the page we just left. 
*/
 
    short blk, byt, oldblk;
    char *makeptr();

    pagfault = 1;			/* set flag */
    byt = (ptr - dataspace) & BYTEBITS; /* isolate byte offset in block */
    if (curblk) THEN {			/* in print immediate, so use */
      blk = curblk + 1;			/* curblk to find page */
      curblk++;				/* and increment it */
      }
    else
      blk = nxtblk(ptr, page);		/* get block offset from last */
    ptr = makeptr(blk, byt);	/* get page and pointer for this pair */
    return(ptr);
}

blockn(page)
char *page;
{  /* 	Blockn returns the virtual page corresponding to a given absolute
	memory page.  This value is found in the pagdesc structure.
   */
    int blk;

    blk = (page - dataspace) >> CVTBLK;	/* get last block number */
    blk = pagdesc[blk].vpage;		/* and corresponding vpage */
    return(blk);
}

nxtblk(ptr, page)
char *ptr, *page;
{  /*	Nxtblk returns the next sequential virtual block after the block
	corresponding to page.
   */
    int blk;

    blk = ptr - page;			/* get block offset from last */
    blk >>= CVTBLK;			/* convert to number of blks */ 
    if (page < preload) THEN 		/* did we just cross a boundary */
      blk = endlod + blk - 1;		/* endlod has first non pre blk */
    else 
      blk = blockn(page) + blk; 	/* find vpage in that block */
    return(blk);
}
    
char *makeptr(blk, byt)
short blk, byt;
{  /*	Makeptr is the heart of the paging scheme.  It manages a doubly-linked
	list of block descriptors.  Preloaded pages are not included in this
	list so they cannot be paged out.  If the page requested is preloaded,
	a valid pointer is returned immediately.  Otherwise, the block is 
	searched for in the linked list, spliced into the front of the list 
	and made mru.  If the block is not in core, the current mru's->previous
	(or lru block) buffer is used to page in the requested block.  Then
	the information in the corresponding block descriptors is filled to
	indicate the absence of the lru and the presence of the new.  The mru
	pointer is that pointed at this block.

	There are two subroutines, unlink and relink, that are used to 
	accomplish the splicing into the front of the chain a block still in
	core.
   */

    struct blkdesc *lru;

    if (blk < endlod) THEN {		/* preloaded, expand the pointer */
      curpage = dataspace + (blk << CVTBLK);
      curpend = curpage + BLKSIZ;
      return(curpage + byt); 
      }
    if (blk <= MAXBLKS) THEN {		/* valid block request? */
      if (vpagemap[blk] == NOT_IN_CORE) THEN { /* null pointer ? */
	lru = mru->prev;		/* get previous page number */
        getblk(blk, lru->loc);		/* read page over lru page */
	vpagemap[lru->vpage] = NOT_IN_CORE;	/* set old page as out */
	vpagemap[blk] = lru;		/* update map to have a core ptr */
	lru->vpage = blk;		/* associate vpage with desc */
	mru = lru;			/* update mru */
	}
      else 			/* page is in core */
	if (vpagemap[blk] != mru) THEN {/* in core, but not mru */
	  unlink(blk);			/* unsplice soon to be mru */
	  relink(blk);			/* link it in to lru spot */
	  mru = mru->prev;		/* update mru */
	  }
      curpage = mru->loc;		/* update page boundaries */
      curpend = curpage + BLKSIZ;
      return(curpage + byt);		/* return a pointer */
      }
    else
      fatal("Virtual page number out of range");
}

unlink(block)
short block;
{  /* 	Unlink removes a block descriptor from the lru chain.
   */
    struct blkdesc *t1, *t2;

    t1 = vpagemap[block]->prev;		/* get pointer to one end */
    t2 = vpagemap[block]->next;		/* and the other */
    t1->next = t2;			/* swap pointers */
    t2->prev = t1;
}

relink(block)
short block;
{  /*  Relink splices a block into the lru chain at mru.
   */
    struct blkdesc *t1;

    t1 = mru->prev;			/* lru pointer */
    mru->prev = vpagemap[block];	/* splice in new descriptor */
    vpagemap[block]->next = mru;	/* update new desc's prev and next */
    vpagemap[block]->prev = t1;
    t1->next = vpagemap[block];		/* and update lru's pointers */
}

/************************************************************************
*									*
*		P A G I N G   A N D   D I S K   I / O			*
*									*
************************************************************************/

getpre(block, count)
short block;
ZIPINT count;
{  /* 	Getpre is used by ZIPBGN and RESTART to read in blocks.  The blocks
	are read in the location based on the virtual page number as opposed
	to an "available" slot.
   */
    short i, blk;
   
    for (i = 1; i <= count; i++) { 	/* figure block num and offset */
      blk = block + i - 1;		/* calculate block number */
      getblk(blk, (dataspace + (blk << CVTBLK)));
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
#ifdef _DEBUG
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
*			E R R O R   H A N D L I N G 			*
*									*
************************************************************************/


fatal(message)
char *message;
{  /*	Fatal gives the standard zip fatal error message (message passed)
	by caller and then performs clean up through Z_EXIT.
   */
    int i, j, k;

    printf("\nFatal error: %s\n", message);
#ifdef _DEBUG
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
    md_ttyres();			/* reset the tty please! */
    exit();
}

/************************************************************************
*									*
*		S T R I N G   O U T P U T   S U P P O R T 		*
*									*
************************************************************************/

char *putstr(strptr)
char **strptr;
{  /* 	To understand fully what this procedure does, you must be familiar
	with zstrings as presented in the ZIP document.  Briefly, a pointer
	to a pointer to zstring is passed to this routine.  A word contains
	3 5-bit characters.  There are three character sets, shift chars,
	and a funny thing called fwords.  Fwords, indicated by the value
	three (3), are an offset into the table pointed to by wrdtab which
	contains a pointer to another zstring.  Fword stands for Frequently
	used word.  So this routine is re-entrant.  It is guaranteed that
	an fword will not contain another fword.
   */
    ZIPINT word, bytsav; 	/* a word string */
    short i, j, k, tempcs = 0, permcs = 0;
    char *tstr, mode = 0, asciflg = 0;

    do {
      word = getwrd(strptr);	/* get a word @strptr */
      PUSH(word);		/* save word and pointer */
      PUSH(*strptr - dataspace);/* save the offset */
      for (i = 1; i <= 3; i++) {	/* three bytes to a word */
        PUSH(word);		/* save current low order bits */
        word >>= ZCHRLEN;	/* shift down to next byte */
        }
      for (i = 1; i <= 3; i++) {
        word = POP();		/* get next byte */
        word &= ZCHRMSK;	/* isolate byte bits */
	if (mode == BYTEMODE) THEN
	  if ((word < 6) && (asciflg == 0)) THEN /* special character? */
	    switch (word) {	/* do special case for each */
	      case 0: {
		putchr(SPACE);	/* print a space char */
		RESET_CS;	/* reset the character set */
		break;		/* return to top of loop */
		}
	      case 1:
	      case 2: 		/* calculate fword offset */
	      case 3: {
		word--;		/* use as a multiplicand */
		word <<= 6;	/* blocks are 64 bytes (or 32 words) */
		wrdoff = word;	/* setup offset */
		mode = 1;	/* indicate word mode */
		break;		/* and loop */
		}		/* next pass will invoke a word */
	      case 4:
	      case 5: {		/* handle char set shifts */
		switch (tempcs) {	/* change cs according to current */
		  case 0: {
		      tempcs = word - 3;	/* tempcs becomes 1 or 2 */
		      break;			/* go back to the top */
		      }
		  case 1:	/* cs's 1 + 2 both set permcs */
		  case 2: {
		      if (tempcs != word - 3) THEN	/* not current cs */
			tempcs = 0;		/* reset to 0 */
		      permcs = tempcs;		/* perm too */
		      break;
		      }
		  default: fatal("Undefined shift state - putstr");
		  }				/* end of switch (tempcs) */
		break;
		}				/* end of case 5 */
	      default: fatal("Illegal special code - putstr");
	      }			/* end of switch (word) for special char */
	  else 			/* word is not special char */
	    switch(tempcs) {	/* so process it according to char set */
	      case 0:
	      case 1: {
		putchr(getzchr(word, tempcs));	/* translate char in word */
		RESET_CS;
		break;				/* and go to top of loop */
		}
	      case 2: {				/* see if it is ascii */
		if (asciflg) THEN {
		  asciflg++;			/* byte count */
		  if (asciflg == 2) THEN 
		    bytsav = word << ZCHRLEN;
		  else {
		    word |= bytsav;		/* or in high byte */
		    putchr(word);
		    asciflg = 0;		/* reset ascii mode flag */
		    RESET_CS;			/* reset char set */
		    }
		  }
		else				/* process non-ascii */
		  switch (word) {	/* cs 2 has specials for 6 & 7 */
		    case 6: {
		      asciflg = 1;	/* set flag to indicate ascii mode */
		      break;
		      }
		    case 7: {
		      newlin();			/* print a crlf */
		      RESET_CS;			/* reset char set */
		      break;
		      }
		    default: {		/* either a char or ascii */
		      putchr(getzchr(word, tempcs));
		      RESET_CS;
		      }			/* end of default for cs 2 */
		    }			/* end of switch (word) cs 2 */
		  }			/* end of case 2 */
	      }				/* end of switch (tempcs) non-spec */
	else { 				/* we're in WORD mode */
	  word <<= 1;	/* multiply it by 2 */
	  word += wrdtab + wrdoff;	/* index into fword tbl */
	  PUSH(curpage - dataspace);	/* save the current page ptr */
	  *strptr = bsplit(GTAWRD(word));	/* get a word */
	  *strptr = putstr(strptr);	/* and print it recursively */
	  curpage = dataspace + POP();	/* restore the page pointer */
	  curpend = curpage + BLKSIZ;	/* restore current p-end boundary */
	  RESET_CS;			/* and reset charset */
	  mode = 0;			/* turn off word mode */
	  }				/* end off else word processing */
	}				/* end of for (decoding) loop */
      *strptr = (POP() + dataspace);	/* add in the offset */ 
      word = POP();			/* get next character */
      }					/* end of do loop */
    while ((word & BIT16) == 0);	/* do until word is negative */
    return(*strptr);
}

getzchr(zchr, charset)
ZIPINT zchr, charset;
{  /*	Given a 5 bit code in zchr and a character set number in charset,
	this routine returns the ASCII value of the char.
   */

    charset *= CSETLEN;			/* get offset into char set */
    zchr += charset - 6;		/* add in offset to char */
    return(*(zchars+zchr));		/* lookup the character */
}

putchr(letter)
ZIPINT letter;
{  /*	Putchr takes an ASCII character and queues it for output.  If it
	fills the buffer, a search backwards for a space is conducted to
	break the line.  The end of the buffer is determined by endbuf
	which should be set whenever the screen size is changed to reflect
	the difference between the right and left margins.
   */
    char *tailptr, *dest;
    char brkflg = 0;
   
    if (scripting) THEN
      p_putchr(letter); 
    PUSH(scripting);			/* save state of scripting */
    scripting = 0;			/* turn off for duration */
    if (chrptr != endbuf) THEN 		/* if there is room */
      *chrptr++ = letter;		/* put the char in the buffer */
    else {
      tailptr = endbuf;			/* init tail to end of buffer */
      while (*--tailptr != SPACE) {	/* search backwards for a " " */
	if (tailptr == outbuf) THEN {	/* not a space to be found! */
	  letter = dumpfix(letter, CONSOLE);	/* dump buffer and add crlf */
	  brkflg = 1;			/* indicate reason for loop exit */
	  break;
	  }				/* end of while searching for ' '*/
	}
      if (*tailptr == SPACE) THEN 	/* space found, rearrange buffer */
	if ((tailptr == outbuf) && (brkflg != 1)) THEN
	  letter = dumpfix(letter, CONSOLE);
	else {
	  *tailptr = NULL;		/* make space into end of line */
	  dumpbuf();			/* now print the buffer */
	  dest = outbuf;		/* now move remainder of line <-- */
	  while (++tailptr < endbuf) 	/* from space+1 to the beginning */
	    *dest++ = *tailptr;		/* of the output buffer */
	  chrptr = dest;		/* reset the character pointer */
	  }				/* end of rearranging of buffer */
      if (letter) THEN			/* if no crlf */
        *chrptr++ = letter;		/* put the char in the buffer */
      }					/* end of first else */
    scripting = POP();			/* restore scripting state */
    return;
}


dumpfix(letter, caller)
int caller;
ZIPINT letter;
{  /*	Dumpfix is called after putchr has searched backwards for a space
	character.  It prints the line and determines when a crlf should
	follow.
   */
    if (caller == CONSOLE) THEN
      dumpbuf();		/* print the line */
    else
      dmp_pbuf();		/* print the line on pipe */
    if (letter == SPACE) THEN 	/* print a crlf in place of a ' '*/
      return(NULL);		/* NULL is interpreter as crlf */
    else
      return(letter);
}

p_putchr(letter)		/* put char to pipe (usually printer) */
ZIPINT letter;
{  /* 	P_putchr is a fixed width version of putchr for writing to a pipe
	(for scripting).  It function is the same with the exception that
	p_endbuf is fixed at 79.
   */
    char *tailptr, *dest;
    char brkflg = 0;
   
    if (p_chrptr != p_endbuf) THEN 	/* if there is room */
      *p_chrptr++ = letter;		/* put the char in the buffer */
    else {
      tailptr = p_endbuf;		/* init tail to end of buffer */
      while (*--tailptr != SPACE) {	/* search backwards for a " " */
	if (tailptr == p_outbuf) THEN {	/* not a space to be found! */
	  letter = dumpfix(letter, PIPE);	/* dump buffer and add crlf */
	  brkflg = 1;			/* indicate reason for loop exit */
	  break;
	  }				/* end of while searching for ' '*/
	}
      if (*tailptr == SPACE) THEN 	/* space found, rearrange buffer */
	if ((tailptr == p_outbuf) && (brkflg != 1)) THEN
	  letter = dumpfix(letter, PIPE);
	else {
	  *tailptr = NULL;		/* make space into end of line */
	  dmp_pbuf();			/* now print the buffer */
	  dest = p_outbuf;		/* now move remainder of line <-- */
	  while (++tailptr < p_endbuf) 	/* from space+1 to the beginning */
	    *dest++ = *tailptr;		/* of the output buffer */
	  p_chrptr = dest;		/* reset the character pointer */
	  }				/* end of rearranging of buffer */
      if (letter) THEN			/* if no crlf */
        *p_chrptr++ = letter;		/* put the char in the buffer */
      }					/* end of first else */
    return;
}

newlin()
{  /* 	Newlin is called when a CRLF is desired on output of a zstring.
	It flushes the buffer and resets the buffer character pointer.
   */
    *chrptr = NULL;		/* indicate end of line */
    if (scripting) THEN
      *p_chrptr = NULL;
    dumpbuf();
}

dumpbuf()
{  /*	Dumpbuf flushes the existing buffer to the screen.
   */
    if (scripting) THEN {
      dmp_pbuf();
      p_chrptr = p_outbuf;
      }
    mprnt(outbuf);
    chrptr = outbuf;		/* reset buffer pointer */
    
}

dmp_pbuf()
{  /* 	Dmp_pbuf flushes the existing output buffer for the pipe to the
	pipe.
   */
    char *bufptr;

    bufptr = p_outbuf;			/* get another pointer */
    while ((*bufptr) && (*bufptr != Z_EOL))
      bufptr++;				/* search for end of string */
    if (*bufptr == Z_EOL) THEN {	/* drop in end char */
      *bufptr = NULL;
      fprintf(scrptfd, "%s", p_outbuf);	/* write string */
      }
    else
      fprintf(scrptfd, "%s\n",p_outbuf);/* with crlf if necessary */
}

mprnt(buf)
char *buf;
{  /*	Mprnt prints a string assuming that Z_EOL indicates end of line
	without crlf and null requests a crlf.
	(EZIP.  May have to be modified to support the printing of
	attributes.)
   */
    while ((*buf) && (*buf != Z_EOL)) 	/* search for line terminator */
      md_putc(*buf++);			/* print character */
    if (*buf == NULL) THEN
      mcrlf();				/* windowed scroll on ending null */
    return;
}

chkscript()
{  /* 	Chkscript is called when the flag scrchk has been set.
	Scrchk flag is set in BOR and BAND to indicate that a possible change
	in state of scripting has occurred.  The flag should be set by OPDIROUT
	in EZIP.
   */	
    ZIPINT temp;

    scrchk = 0;				/* reset flag */
    temp = GTAWRD(PFLAGS);		/* get status word */
    if (scripting) THEN {		/* set according to current state */
      if ((temp & SCRIPTBIT) == 0) THEN {
	fclose(scrptfd);		/* close if turned off */
	scripting = 0;			/* and reset flag */
	}
      }
    else
      if (temp & SCRIPTBIT) THEN {
        scripting = 1;			/* turn on flag and open */
	p_chrptr = p_outbuf;		/* reset scripting buffer */
        if ((scrptfd = fopen("script", "w")) < 0) THEN 
	  scripting = 0;		/* turn off flag if open fails */
        }
    return;
}

verify()
{  /*   Verify will perform a checksum on the entire data file less the
	header.  All pages are brought in from disk.  The checksum is
	then compared to the checksum stored in the header. 
	(EZIP.  Remove annoucing printf)
   */
    char buffer[16*1024];
    ZIPINT chksum, pchk, length, offset;
    int i, j, nreads;

    printf("Unix Interpreter Version A");	/* version */
    mcrlf();				/* windowed scroll */
    offset = 32;			/* skip header */
    chksum = 0;				/* initialize */
    length = GTAWRD(PLENTH);		/* get length of dat file */
    nreads = length/(16*512);		/* it will take len/bufsiz reads */ 
    lseek(gamechn, 64, 0);		/* skip header */
    for (i = 0; i <= nreads; i++) {
      read(gamechn, buffer, 16*1024);	/* read in 16K */
      for (j = 0; j < 16*1024; j++) {
	if (offset >= length) THEN
	  break;
	chksum += (*(buffer + j++) & 255);	/* add in both bytes */
 	chksum += (*(buffer + j) & 255);	
	offset++;				/* and update the offset */
	}
      }
    pchk = GTAWRD(PCHKSM);		/* get checksum from the header */
    if (chksum == pchk) THEN
      return(ZTRUE);
    else 
      return(ZFALSE);
}

/************************************************************************
*									*
*	R E A D   A N D   S U P P O R T I N G   R O U T I N E S 	*
*									*
************************************************************************/

#define SPACE_LEFT (rdnwds < maxtoks)

zread()
{  /*	Read is probably the most complex zip opcode.  It not only reads
	a line of input but it also performs some basic parsing and table
	lookup.  The arguments to read are an input buffer and a token
	table to store the results of the lookups.  The input buffer's
	first byte is a read only bytes to indicate the maximum number
	of input characters.  The first two bytes of the token table are
	also special.  The first is supposed to be the maximum number of
	tokens.  Unfortunately it always puts twice that number in the
	first slot.  This problem has been an ongoing argument between
	(technical) imps and the micro group.  The easiest solution is
	to set it to 59 if it is ever above that.  Otherwise table 2 will
	overflow into someother table and break things.  The second byte
	of the token table is filled by the interpreter to indicate the
	number of tokens actually parsed.  Tokens begin at the third byte
	(offset 2) and are 4 bytes each.  The format of each is as follows:

		2 bytes -- pointer to word in vocabulary table 
		1 byte  -- length of ascii input
		1 byte  -- offset into input buffer for corresponding text.
	
	If the token buffer is filled, any other input is flushed.

	The input buffer is only changed in that all input should be
	lowercasified.  If it is filled, it beeps.

	Read has a number of subroutines for processing the input string.
	The status line is always updated before taking input.  There are
	a number of global pointers for read declared for use by many of
	the subroutines.  See the variable declarations for read for an
	explanation of each.

	The basic flow is as follows: A line is read in and lowercasified.
	Getasc gets a single word and determines its length.  Nxtasc finds
	the next token to be used.  Zword is used to convert an ascii word
	to zstring format.  The zword (two 16 bit words in zip) is then
	looked up in the vocabulary table.  A token value of zero is stored
	if no match is found.  The process continues until the token table
	is filled or all the ascii input has been processed.

	Keep in mind the concept of a self-inserting break character.  This
	characters, such as "period" and "comma," are treated as words,
	converted to zwords and looked up in the vocab table.  SI breaks
	are initially downloaded from the datafile during initialization in
	ZIPBGN.  

	(EZIP.  The basic read routine will be unchanged.  Support for
	incall, timeout, and input will have to be added.  Statusln will
	be removed.  Timeout routines must be built into getlin which
	will require a special version of md_getl.)
   */
    short numread, 		/* number of bytes read in */
	  maxtoks;		/* number of tokens allowed in tbl 2 */
    ZIPINT rdret,		/* pointer to table 2 where tokens go */
	   rdnwds,		/* token counter */
	   wordent,		/* current entry ptr in table 2 */
	   lookup();
    
	   
    statusln();				/* status line update */
    putchr(Z_EOL);			/* flush output buffer */
    dumpbuf();				/* without a crlf */
    linecnt = 0;			/* reset MORE line counter */
    numread = getlin(argblk[1]);	/* get a line of input */
    if (scripting) THEN
      fprintf(scrptfd, "%s\n", inbuf);
    rdbos = (char *)(argblk[1] + dataspace + 1); /* table 1 pointer */ 
    rdeos = argblk[1] + numread + 1;	/* make a pointer to eostring */
    rdret = argblk[2] + 1;		/* table 2 token count reposit */ 
    rdnwds = 0;				/* no words parsed, ...yet */
    maxtoks = GTABYT(argblk[2]);	/* max tokens in input buffer */
    if (maxtoks > TOKEN_TBL_LEN) THEN	/* fix ZAP bug which allows too */
      maxtoks = TOKEN_TBL_LEN;
    wordent = argblk[2] + 2; 		/* words begin after count bytes */
    curword = rdbos;
    while (notbrkc(*curword) == ZFALSE)
      curword++;			/* scan of leading spaces */
    curoff = curword - rdbos + 1;
 
    while (WORDS_LEFT && SPACE_LEFT) {
      getasc();				/* get ascii word */
      makezwrd();			/* make a zword of it */
      PTAWRD(wordent, lookup());	/* store 16 bit offset in table 2 */
      PTABYT(wordent+2, POP());		/* fill in length value */
      PTABYT(wordent+3, curoff);	/* fill in offset value */
      rdnwds++;				/* increment number of words read */
      wordent += 4;			/* skip to next word entry */
      curword = nxttok;			/* get pointer to next ascii word */
      curoff = curword - rdbos + 1;	/* update pointer */
      }
    if (WORDS_LEFT) THEN
      flushwrds();
    PTABYT(rdret, rdnwds);
    return;
}

getasc()
{   /* 	Get the next ascii word, advance pointers, and return the length 
	and offset on the stack.
    */

    short len = 1;			/* initialize */
    char *curbyt, *wordend, *nxtasc();

    curbyt = curword;			/* get current offset */
    if (notbrkc(*curbyt)) THEN
      while (notbrkc(*(curbyt+1))) {	/* if char is not a break */
        len++;				/* then bump our pointers */
        curbyt++;
        }
    wordend = curword + len;		/* pointer to end of ascii word */
    nxttok = nxtasc(wordend);		/* advance nxttok to next token */
    PUSH(len);				/* and length of ascii string */
    return;
}

char *nxtasc(wordend)
char *wordend;
{  /*	Nxtasc advances the pointer to the next letter or self-inserting
	break character; consequently the pointer is left pointing at the
	next thing to be tokenized.
   */
    if ((lastbrk) && (lastbrk < esibrks)) THEN
      lastbrk = esibrks;		/* force thru loop one for si brk */
    while ((lastbrk) && (lastbrk >= esibrks)) 	/* look for a break char */
      if (notbrkc(*wordend)) THEN 	/* look for next non break or si */
	break;
      else
	if (lastbrk >= esibrks) THEN
	  wordend++;			/* check next char */
    return(wordend);
}

notbrkc(c)
char c;
{  /* 	Returns true if character c is not a break character. 
   */
    char *ptr = rbrks;

    lastbrk = 0;		/* initialize pointer to last break char */
    if (c) THEN {
      while (*ptr) 			/* while not at end of list */
        if (*ptr == c) THEN		/* break found */
	  break;
        else
	  ptr++;			/* no break, check next char */
      if (*ptr) THEN { 			/* if match, then c is a break */
        lastbrk = ptr;			/* save ptr to last break char */
        return(ZFALSE);
        }
      else
        return(ZTRUE); 
      }
    else
      return(ZFALSE);
}

makezwrd()
{  /*	Makezwrd takes the maximum length of the ascii input word and
	passes it to zword to convert the word into a zstring.
	(EZIP.  Storage space in zascii should have used an equate.
	I apologize.  It should be expanded to 9 for EZIP.)
   */
    short i, len;
    char zascii[7];			/* ascii storage */

    len = POP();			/* get length from stack */
    PUSH(len);				/* put value back on stack */
    for (i = 0; i <= CHRS_PER_ZWORD; i++) 
      if (i >= len) THEN
	zascii[i] = 0;
      else
	zascii[i] = *(curword + i);
    zword(zascii);
    return;
}
 
zword(ptr)	
char *ptr;
{  /*	Make a zword out of the ascii word pointed to by ptr and return 
    	results in global rdwstr.
   */
    short cs, i, j;
    ZOBJECT zbyte, chrbyt();
    char zchrs[CHRS_PER_ZWORD];		/* repository during conversion */

    for (i = 0; i < CHRS_PER_ZWORD; i++) {	/* for each char */
      if (*ptr) THEN {			/* if it is a char */
	cs = char_cs(*ptr);		/* figure character set */
	if (cs) THEN {			/* if cs other than 0 */
	  cs += 3;			/* calculate a temporary shift */
	  zchrs[i] = cs;		/* save it */
 	  if (++i == CHRS_PER_ZWORD) THEN
	    break;
	  }
	zbyte = chrbyt(*ptr++);		/* get zchar byte value */	
        if (zbyte) THEN 		/* found, so save char */ 
	  zchrs[i] = zbyte; 
	else {				/* char not found, use ASCII */
	  zchrs[i] = 6;			/* cs 2 indicator of ASCII */
	  if (++i != CHRS_PER_ZWORD) THEN {/* if not last char then save */
	    zchrs[i] = *(ptr-1) >> ZCHRLEN;	/* save hi bits */
	    if (++i != CHRS_PER_ZWORD) THEN
	      zchrs[i] = *(ptr-1) & ZCHRMSK;	/* save low bits */
	    else
	      break;
	    }
	  else
	    break;
	  }
	}				/* end if (*ptr)  */
      else
	zchrs[i] = PADCHR;		/* save a pad character */
      }					/* end of for loop */
    for (i = 0; i < (CHRS_PER_ZWORD / 3); i++)
      rdwstr[i] = 0;			/* initialize to zero */
    j = -1;
    for (i = 0; i < CHRS_PER_ZWORD; i++) {	/* fill string */
      if ((i % 3) == 0) THEN		/* change words every three bytes */
	j++; 
      rdwstr[j] = (rdwstr[j] << ZCHRLEN) | (zchrs[i] & ZCHRMSK);
      }
    rdwstr[CHRS_PER_ZWORD / 3 - 1] |= BIT16;	/* turn on end of string bit */
    return;
}

char_cs(c)
char c;
{  /*	Given a character c, return its character set.
   */
    if (c) THEN				/* if char is not null */
      if (islower(c)) THEN
	return(0);			/* it is either lower or...*/
      else
	return(2);			/* 3rd or ascii */
    else
      return(3);			/* nope, null */
}

ZOBJECT chrbyt(c)
char c;
{  /*	Given a character c, return its zchar value.
   */
    char *zptr;

    switch (char_cs(c)) {
      case 0: return(c - 'a' + 6);	/* first char is 6 */
      case 2: {				/* never upper case! */
	zptr = zchars + 51;
	while (*++zptr) {
	  if (*zptr == c) THEN
	    return((zptr - zchars - 52) + 6);
	  }
	return(0);			/* return failure */
	}
      default: return(0);
      }
}

ZIPINT lookup()
{  /*	Lookup performs a binary search on the vocabulary with the target
	being in rdwstr.  A zero is returned if the word is not found and
	otherwise the offset into the vocab table is returned.
	(EZIP.  The matchings in the binary search must look at the third
	zword that has been created.)
   */
    short vocptr, vocend, index, hi, lo;

    vocend = ((vwords - 1) * vwlen) + vocbeg; /* point to end of voc */
    vocptr = vwords;			/* pointer to vocabulary */
    index = vwlen;			/* index into vocab */
    vocptr >>= 1;
    do {
      index <<= 1;			/* binary search */
      vocptr >>= 1;			/* find middle of vocab table */ 
      }
    while (vocptr);
    vocptr = vocbeg + index;		/* make point to middle */ 
    vocptr -= vwlen;			/* back up to avoid bug */
   
   do { 		/* B I N A R Y   S E A R C H  */

    index >>= 1;			/* index will be half */
    if ((hi = GTAWRD(vocptr)) == rdwstr[0]) THEN
      if ((lo = GTAWRD(vocptr+2)) == rdwstr[1]) THEN
	return(vocptr);
      else
	if (lo < rdwstr[1]) THEN
	  vocptr += index;		/* move up the table */
	else
	  vocptr -= index;		/* back up in table */ 
    else
      if (hi < rdwstr[0]) THEN
	vocptr += index;		/* move up the table */
      else
	vocptr -= index;		/* move down table */
    if (vocptr > vocend) THEN		/* don't go past end of table */
      vocptr = vocend;
    }
    while (index >= vwlen);		/* loop until found or not */
    return(ZFALSE);			/* return that no word found */ 
}

getlin(buffer)
short buffer;
{  /*	Getlin takes a buffer that has first byte (read-only) containing
	the maximum number of input characters.  After the line is read
	in, it is lower casified.  The number of bytes read is returned.
   */
    short cnt, maxchars;
    char *ptr;

    maxchars = GTABYT(buffer);		/* first byte has length */
    ptr = dataspace + buffer + 1;	/* get pointer to entry point */
    md_getl(inbuf, maxchars);		/* get it the dependent way */
    for (cnt = 0; cnt < maxchars; cnt++)
      if ((inbuf[cnt] == 0) || (inbuf[cnt] == 26)) THEN
	break;
      else 
	*ptr++ = lc(inbuf[cnt]);	/* lowercasify input */
    *ptr = NULL;
    return(cnt);
}

flushwrds(stroff)
short stroff;
{  /* 	Flushwrds is called when the token table is filled and there is
	more input to be parsed.  The remainder of the ascii buffer is
	flushed and the characters thrown away are reported to the user.
   */
    printf("Too many words typed, flushing: ");
    PTABYT(rdeos, NULL);	/* put a null in at eos */
    mprnt(rdbos + curoff);	/* print flushed string */
    return;
}


setup()
{  /* 	Setup performs any system dependent initialization that must be
	done only once.
   */
    md_initty(); 		/* turn off echo and unbuffer input */
    endbuf = outbuf + scrwid();	/* determine output buffer width */
    p_endbuf = p_outbuf + PIPEWIDTH;	/* and pipe width */
    return;
}

statusln()
{  /*	Status line updates the status line.  It finds the current room
	description, time, score and moves and displays them.  The first
	global variable in ZIL is called here and in the object number where
	the player is.  Printd for this object will print the room description.
	The next two globals are either time or score and moves.  The timemd
	variable set during ZIPBGN determines the type of status line to 
	use.  This routine will have to modified to support a windowed 
	environment.
	(EZIP.  Please scrap this routine for ezip.)
   */
    char *chrsav, line[81], *meridian = "am";
    ZIPINT score, moves, hour, minutes;
    int i;

    for (i = 0; i < 80; i++) line[i] = SPACE;	/* clean out line */
    line[80] = NULL;			/* end char */
    chrsav = chrptr;			/* save ptr for out buf */
    chrptr = &line[1];			/* indent one space in string */
    PUSH(scripting);			/* save state of this */
    scripting = 0;			/* turn off scripting */
    printd(getvar(G_HERE));		/* print room's short desc */
    chrptr = chrsav;			/* restore char pointer */
    scripting = POP();			/* restore state of scripting */
    if (timemd) THEN {			/* just print time */
      hour = getvar(G_HOURS);		/* get hours passed */
      if (hour >= 12) THEN
	*meridian = 'p';		/* make it pm */
	if (hour > 12) THEN
	  hour -= 12;			/* round off */
      minutes = getvar(G_MINS);		/* get minutes */
      sprintf(&line[60],"Time:  %1d:%02.2d %s", hour, minutes, meridian);
      }
    else {				/* do score / moves status */
      score = getvar(G_SCORE);		/* get current score */
      moves = getvar(G_MOVES); 		/* get current number of moves */
      sprintf(&line[50],"Score: %1d", score);
      sprintf(&line[66],"Moves: %1d", moves);
      }
    for (i = 0; i < 80; i++) 
      if (line[i] == NULL) THEN
	line[i] = SPACE;		/* remove nulls from sprintf */
    locate(STATLEN, 1);			/* go to upper left */
    hilite(REVERSE);			/* turn on reverse video */
    printf("%s", line);			/* print the status line */
    hilite(NORMAL);			/* restore normal video */
    locate(25,1);
    return;
}

/************************************************************************
*									*
*		S Y S T E M   D E P E N D E N T 			*
*									*
************************************************************************/

mtime()
{  /* mtime get the machine time for setting the random seed.
   */
    long time(), tloc;

    rseed = time(tloc);		/* get system time */
    srand(rseed);		/* get a random seed based on time */
    return;
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
        printf("** MORE **");
        while (getchar() == -1)
	  ;
        printf("\033[10D\033[K");
        linecnt = 1;
        }
      }
    else				/* screen 1 requires no scroll */
      printf("\n");
    return;
}

md_getl(buf, cnt)
char *buf;
{  /*	Machine (or OS) dependent line read.  Md_getl reads chars upto cnt.
	All unprintables or escape sequences are thrown away.  When the
	cnt'th char is typed, it echoes, disappears, and the terminal beeps.
	The nuber of chars actually read are returned.  Backspaces are
	handled by backing up, printing a space and backing up again.
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
#ifdef _DEBUG
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

#define O_RDWR 2
md_initty()
{  /* 	This routine performs Unix tty magic.  It sets the input buffer
	length to 0, and turns off canonization and echo. 
   */
    struct termio ttyinfo;

    ttyfd = fileno(stdin);		/* get a file descriptor */
    if (ioctl(ttyfd, TCGETA, &ttyinfo) == -1) THEN
      printf("\nIOCTL - TCGETA failed");
    ttyinfo.c_lflag &= ~ICANON;
    ttyinfo.c_lflag &= ~ECHO;	
    ttysav = ttyinfo.c_cc[VMIN];
    ttyinfo.c_cc[VMIN] = 0;
    if (ioctl(ttyfd, TCSETA, &ttyinfo) == -1) THEN
      printf("\nIOCTL - TCSETA failed");
    
}

md_ttyres()
{  /* 	This undoes the above magic.
   */
    struct termio ttyinfo;

    ioctl(ttyfd, TCGETA, &ttyinfo);
    ttyinfo.c_lflag |= ICANON;
    ttyinfo.c_lflag |= ECHO;
    ttyinfo.c_cc[VMIN] = ttysav;
    ioctl(ttyfd, TCSETA, &ttyinfo);
    close(ttyfd); 
}

/************************************************************************
*									*
*			    D E B U G G E R 				*
*									*
************************************************************************/
#ifdef _DEBUG

debugger()
{
    char c;
    short i;

    if (skipcnt == 0) THEN {
      dump();
      printf("\nZIPDDT>");
      while ((c = getchar()) != 'q') {
	if (c != NO_INPUT) THEN 
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
	if (c != NO_INPUT) 
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
	printf("Word value is: %x", GTAWRD(i));
	printf("\nSet to value: ");
	j = getnum(HEX);
	PTAWRD(i, j);
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

lc(c)
int c;
{
    if (c <= 'Z' && c >= 'A') THEN
      c = c - 'A' + 'a';
    return(c);
}

vdump(blknum)		/* dump a block of data formatted */
short blknum;
{
    char *loc;

printf("\nBlock number goes in as %d", blknum);
    if (blknum < endlod) THEN
      loc = (dataspace + (blknum << CVTBLK));
    else 
      loc = vpagemap[blknum]->loc;
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

