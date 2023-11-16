
/*------------------------------------------------------*/
/*	AMIGA XZIP INTERFACE				*/
/*------------------------------------------------------*/

/* XZIP MODIFICATION HISTORY

	11 Nov 87	ported from EZIP code
			stash_get now called from event_in NOT read_console
			  (to avoid infinite loop w/ get_control_seq)
			begin_mark must be /unsigned/ (UCHAR);
			  read_cursor_pos() & get_control_seq now work

   ZIP/EZIP MODIFICATION HISTORY

	25 OCT 85	init_script () uses PRT: file.  Close_Device was
			crashing when two games run simultaneously.

	29 OCT 85	drive_default() strips savename of leading spaces
	29 OCT 85	get_control_seq() ignores garbage seqs

	04 NOV 85	write_cursor_pos() takes no params (same as read)
	04 NOV 85	extra error checking in make_icon_file()

	22 JUL 87	changed scripting EOL seq from Esc-E to CRLF,
			  wasn't working under AmigaDOS 1.2
			added unhide_screen for Alertbox problem
			set NOCAREREFRESH flag to avoid border flicker
			added stash_put(), stash_get() for typeahead;
			  read_cursor_pos() still NOT working as advertised
	26 AUG 87	added volume control to md_sound (in SOUND.C)
	01 Nov 87	switched to Native development
*/

#define VOID	int		/* "void" would require forward declares */
#define GLOBAL	/**/

/*	/-* already defined in exec/types.h) *-/
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
*/

typedef	unsigned char	CHAR;	/* always unsigned for us */
typedef	unsigned char	UCHAR;

/* This switch controls conditional ZIP/EZIP compilation */
/*   (WAS defined in batch file, where compiler is invoked) */
/*   (For XZIP must always be true) */

#define	    EZIP		1	/* set for EZIP, otherwise classic */

#define     ZDEBUG		0	/* set for debugging */
#define     bug_DiskFull	0	/* set if crashes system - FIXED */
#define     DEADCODE		0	/* set to re-include */

#define ERR 1
#define NOERR 0

/* color equates */

#define MINCOLORS 2	/* default blue/white */
#define MAXCOLORS 8	/* total XZIP color ids */
#define BASE_ID 2
#define LAST_ID 9

#define DEF_FORE 9	/* default Amiga foreground = white */
#define DEF_BACK 6	/* default Amiga background = blue */

/************************************************************************/
/*	Global Variables						*/
/************************************************************************/

#if EZIP
struct  Screen	*ZScreen;
struct	TextFont *sysfont, *altfont;
#endif

/* Use altered IOStdReq structure, "data" field type APTR changed to STRPTR.
   Otherwise the compiler forces string pointers to be even-aligned.	
   >>> CHANGED BACK FOR NATIVE DEVELOPMENT  - dbb  <<< */

struct	IOStdReq/*_MUNGED*/    ZIOStdReq;	/* for Console device */
/**  (now DEAD)
struct	IOStdReq/-*_MUNGED*-/  ZRDStdReq;	/-* for queued reads *-/
struct	MsgPort     ZMsgPort;		**/

struct	Window	   *ZWindow;
struct	RastPort   *ZRastPort;

struct	Device	   *ConsoleDevice;	/* for library vector */

/* scripting variables -- two methods possible */

	LONG	print_file;		/* scripting refnum */
/**
struct	IOStdReq_MUNGED    SIOStdReq;	/-* not used *-/
struct	MsgPort     SMsgPort;	**/

	WORD	prt_inited = 0;		/* set when printer opened */
	WORD	prt_active = 0;		/* set when scripting is active */

/* cursor variables */

	WORD	y_res, y_char;		/* rows = y_res / y_char */
	WORD	x_res, x_char;

GLOBAL	WORD	rows, cur_row;		/* cur_row in 0 .. rows-1 */
GLOBAL	WORD	columns, cur_column;
GLOBAL	WORD	marg_left = 0, marg_right = 0;

GLOBAL	WORD	split_row;		/* first row in scrolling area */
GLOBAL	WORD	wind1;			/* 1=TOP, 0=BOTTOM */

/* This buffer is accessed by the kernel. Careful about changing its size */

GLOBAL	CHAR	savename[80];	/* buffer for file_select dialog */
GLOBAL	CHAR	saveback[80];	/* buffer for default name */

GLOBAL  LONG	game_ref;
GLOBAL  LONG	save_ref;

	CHAR	*gamename = "Story.Data";	/* (on default drive) */
	CHAR	*saveinit = "Story.Save";	/* (add default prefix) */
	CHAR	*execname = NULL;		/* zip name, get from OS */

GLOBAL	LONG	actlen = 0;		/* for reporting r/w transfer len */

/* These vars are used to avoid unnecessary Seeking (e.g. when reading
   sequential blocks during $verify).  AmigaDOS Seeking causes head
   movement even when it shouldn't and thrashes the disk badly.  */

	LONG	old_offset = -1;	/* init to invalid vals */
	LONG	old_channel = -1;

/* memory usage can be controlled by user, by supplying a free byte
   count in the CLI command line.  Useful if another task will be launched
   subsequently.  Otherwise game takes as much memory as it can use. */

	LONG	mem_freereq;		/* set during init */

	LONG	IntuitionBase;
	LONG	GfxBase;		/* for ZRastPort calls */
/**	LONG	DiskfontBase;  **/	/* for OpenDiskFont -- DEAD */

	LONG	IconBase;
struct	DiskObject	*diskobj;	/* SAVE file icon info */

struct	Remember	*RememberKey;

/* DEAD, SEE bug_DiskFull

   If a NEW save file is being created, check disk free space before
   each write call.  Prevents a useless Retry/Abort dialog (and in the
   case of make_icon_file, a crash).
*/
	BOOL	disk_newsave;
	LONG	disk_free;

GLOBAL	WORD	xmouse, ymouse;		/* 1-origin */

/* Amiga RGB values for the 8 XZIP colors, ordered so that the first three
   correspond to the usual Workbench defaults.  The values were determined
   empirically. */

	UWORD colortable[] = {
    0x005A, 0x0FFF, 0x0000, 0x00C0,	/* blue, white, black, green */
    0x0E00, 0x0EE0, 0x0F0F, 0x00EE	/* red, yellow, magenta, cyan */
    };

/* This table relates XZIP color ordering to the above Amiga ordering */
	
	UBYTE colormap[] = {
    -1, -1,		/* (placeholders -- never read) */
    2, 4, 3, 5,		/* black, red, green, yellow */
    0, 6, 7, 1		/* blue, magenta, cyan, white */
    };

	BOOL color = FALSE;		/* default */

	CHAR *cno = "Couldn't open: ";	/* prefix for fatalmsg() */
	CHAR *cnc = "Couldn't create ";

	UBYTE *soundbuf;	/* preallocate to avoid fragmentation */

/************************************************************************/
/*	Externals							*/
/************************************************************************/

/*
extern LONG trap1 ();
extern VOID START ();
*/

/************************************************************************/
/*	Misc Functions							*/
/************************************************************************/

/*------------------------------*/
/*	delay			*/
/*------------------------------*/

delay (n)	/* in 1/10 secs */
WORD n;
{
	Delay (n * 5);		/* AmigaDOS function, in 1/50 secs */

/**	ULONG secs, mics, sfinal, mfinal;
	BOOL done = FALSE;

	CurrentTime (&secs, &mics);
	sfinal = secs + n;  mfinal = mics;	/-* n in whole secs *-/
	while (!done) {
		CurrentTime (&secs, &mics);
		if (secs == sfinal && mics >= mfinal)
			done = TRUE;
		if (secs > sfinal)
			done = TRUE;
		}		**/
}

/*------------------------------*/
/*	unhide_screen		*/
/*------------------------------*/

/* [EZIP only]  Fixes a bug under AmigaDOS 1.2.  When a System Alert box
   occurs, the Workbench screen pops to the front [?why?] and the box appears 
   in it.  But after the user responds to the alert, the Workbench screen 
   does not automatically unhide the game screen.  This is likely to make 
   users think the game has gone away.  Ugh. */

unhide_screen()		/* [call just before start of input] */
{
#if EZIP
	ScreenToFront (ZScreen);	/* NOP if already at front */
#endif
}

/*------------------------------*/
/*	bell_sound		*/
/*------------------------------*/
VOID
bell_sound (tone)

WORD	tone;		/* 1 for beep, 2 for boop */
{
/* no internal bell, just ignore beep; flash screen for boop */

	if (tone == 2)
		DisplayBeep (ZWindow->WScreen);	/* (a system call) */
}	

/*------------------------------*/
/*	random_seed		*/
/*------------------------------*/
ULONG
random_seed ()
{
	ULONG	secs, mics;

	CurrentTime (&secs, &mics);

	/* low 20 bits (significant in mics) are the most random */
	return (secs + mics);
}

/*------------------------------*/
/*	window_resized		*/
/*------------------------------*/
WORD
window_resized ()	/*   update globals, return window column width */

/* this routine is called -
	[] during init
	[] (CZIP) after a RDLINE input
	[] (CZIP) after every line output (this takes care of MOREs)
*/

{
/* determine size of current font */

	/* (DONE ONCE ONLY, DURING INIT; SETS Y_CHAR AND X_CHAR) */

/* determine usable size of window */

	y_res = ZWindow->Height - ZWindow->BorderTop - ZWindow->BorderBottom;
	x_res = ZWindow->Width - ZWindow->BorderLeft - ZWindow->BorderRight;

/* Calculate rows and columns.  To prevent undesired AMIGA auto wrapping, 
   (CZIP only) we pretend that the column width is one less than it really is.
*/
	rows	= y_res / y_char;
	columns	= (x_res / x_char) /**-1**/;

/**	if (cur_row >= rows)  cur_row = rows - 1;	/-* CZIP *-/
	if (cur_column >= columns)  cur_column = columns - 1;
**/
	return (columns);	/* updates kernel folding routines */
}

/************************************************************************/
/*	Memory Management						*/
/************************************************************************/

/* memory fudge factor -- leave this much free for transient use,
   (unless over-ridden by the F/n command switch).  Resizing of windows 
   especially triggers large dynamic allocation requests.  Also, opening 
   a script channel uses some 25K.  (We now pre-allocate a main sound 
   buffer.)
*/

#define	MEM_SYSTEM	64*1024		/* normal XZIP */

/**  #if EZIP		
#define	MEM_SYSTEM	32*1024		/-* allow loading on 256K machine *-/
#else	/-* if ZIP *-/
#define	MEM_SYSTEM	64*1024		/-* enough for several windows *-/
#endif  **/

#define	MEM_DEFAULT    -1	/* user didn't specify memory use */

/*------------------------------*/
/*	c_maxmem		*/
/*------------------------------*/

/* Return size of available memory pool, may be "doctored" (up or down) 
   upon user request.  This routine should be called after every
   significant allocation has been made except for the page buffers. */

LONG
c_maxmem (game_total)	
LONG	game_total;	/* max memory (for buffers) we can use */
{
	LONG	game_used;
	LONG	mem_avail, mem_return;

	CHAR	message[64];
	LONG	len;

	mem_avail = (AvailMem (MEMF_PUBLIC));

	if (mem_freereq == MEM_DEFAULT)
		mem_return = mem_avail - MEM_SYSTEM;	/* adjust it */
	else
		{
/* got a request, use it */

		if (mem_freereq < 0)		mem_freereq = 0;
		if (mem_freereq > mem_avail)	mem_freereq = mem_avail;

		mem_return = mem_avail - mem_freereq;	/* "doctor" it */

/* Request was made, so report our status to user.  Want to show
   the actual amount we will use here (if less than returned value). */

		if (game_total > mem_return)
			game_used = mem_return;
		else
			game_used = game_total;

		stcu_d (&message[0], game_used, 32);
		strcat (&message, " bytes used");

	/* if free will be more than requested, show why */

		if (game_used == game_total)
			strcat (&message, " (maximum)");
		strcat (&message, ", ");

		len = strlen (&message);
		stcu_d (&message[len], (mem_avail - game_used), 32);
		strcat (&message, " bytes free");

		len = strlen (&message);
		line_out (&message, len);
		char_out (13);
		char_out (13);
		}

	return (mem_return);
}

/*------------------------------*/
/*	c_getmem		*/
/*------------------------------*/

LONG c_getmem (nbytes)		/* return zero if failed */
LONG nbytes;
{
/**	return (AllocMem (nbytes, MEMF_PUBLIC));  **/

/* This routine calls the AllocMem function, and also allocates a "link node,"
   which allows automatic de-allocation when we terminate.
   We don't normally require any particular memory type (e.g., public or chip).
*/
	return (AllocRemember (&RememberKey, nbytes, MEMF_CLEAR));
}

/************************************************************************/
/*	Initializations							*/
/************************************************************************/

#if EZIP

#define Peek1 (8+4) /*8+3*/	/* ZScreen scanlines showing behind ZWindow */
#define Peek2 0     /*8+5*/	/* WBScreen scanlines showing behind ZScreen */

/* The first margin is needed for the ZScreen slider and show/hide boxes.
   Without these functions, task switching is crippled.

   The second margin is needed because of an Intuition problem that causes
   all system alert boxes (e.g. insert disk X, disk X full, etc) to appear in 
   the WB screen, normally hidden by EZIP.  This space permits the words
   "System Alert" to show through to the user.  >>> FIXED IN WB 1.2 <<<
*/
#else
#define Peek1 0
#define Peek2 0
#endif

#define NOTYET 0	/* to be defined at runtime */

/*------------------------------*/
/*	window_init		*/
/*------------------------------*/

WORD set_color ();	/* forward */

struct  TextAttr  sysTextAttr = {
	"topaz.font", TOPAZ_EIGHTY, FS_NORMAL, FPF_ROMFONT
	};
/***
struct  TextAttr  altTextAttr = {
	"BZ Font.font", TOPAZ_EIGHTY, FS_NORMAL, FPF_DISKFONT
	};
***/	/* no longer used */

WORD window_init (open, wcolor)
BOOL open, wcolor;		/* close if false */
{
static struct NewScreen ns = {
	0, 0 + Peek2,		/* LeftEdge, TopEdge */
	640, 200 - Peek2,	/* Width (high-res), Height  >> defaults << */
	1,			/* Depth (default = 2 colors) */
	0, 1,			/* DetailPen, BlockPen */
	HIRES,			/* ViewModes */
	CUSTOMSCREEN,		/* Type */
	&sysTextAttr,		/* Font (topaz 80) */
	NULL,			/* DefaultTitle */
	NULL,			/* Gadgets */
	NULL			/* CustomBitMap */
	};

static struct NewWindow nw = {
	0, 0 + Peek1,				/* LeftEdge, TopEdge */
	NOTYET, NOTYET,   /* derive from ns */	/* Width, Height */
/**	640, (200 - Peek2) - Peek1,	**/
	0, 1,					/* DetailPen, BlockPen */
	RAWKEY | MOUSEBUTTONS | INTUITICKS,	/* IDCMPFlags */
	BORDERLESS | SMART_REFRESH |
		ACTIVATE | NOCAREREFRESH,	/* Flags */
	NULL,					/* FirstGadget */
	NULL,					/* CheckMark */
	NULL,					/* Title */
	NOTYET,					/* Screen */
	NULL,					/* BitMap */
	0, 0, 0, 0,				/* (min+max sizes, unused) */
	CUSTOMSCREEN				/* Type */
	};

	struct GfxBase *gb = (struct GfxBase *) GfxBase;

	if (open)
		{
	/* first create a custom screen ... */

		if (gb->LibNode.lib_Version >= 33) {
		/* must be running under system 1.2+ to use size fields */

			ns.Height = gb->NormalDisplayRows - Peek2;
			ns.Width = gb->NormalDisplayColumns;	/* (pixels) */
			}
		else {
		/* under system 1.1, deduce size from this flag */

			if ((gb->DisplayFlags) & PAL)
				ns.Height = 256 - Peek2;
		/**	else	ns.Height = 200 - Peek2;  **/	/* default */
		/**	ns.Width = 640;  **/			/* default */
			}

		if (wcolor)  ns.Depth = 3;		/* support 8 colors */
		ns.DefaultTitle = (UBYTE *) execname;	/* null, or our name */

		ZScreen = (struct Screen *) OpenScreen (&ns);
		if (ZScreen == NULL) {
			fatalmsg (cnc);  fatalmsg ("Screen");
			return (ERR);
			}

	/* then create a window (taking size from screen) ... */

		nw.Height = ns.Height - Peek1;
		nw.Width = ns.Width;
		nw.Screen = ZScreen;

		ZWindow = (struct Window *) OpenWindow (&nw);
		if (ZWindow == NULL) {
			fatalmsg (cnc);  fatalmsg ("Window");
			return (ERR);
			}

		ZRastPort = ZWindow->RPort;
		SetDrMd (ZRastPort, JAM2);	/* always draw fore AND back */

		if (wcolor) {
			/* install our colors in the screen's table */
			LoadRGB4 (&(ZScreen->ViewPort), colortable, MAXCOLORS);

			/* make sure defaults are showing  (fixes a glitch
			   in LoadRGB4; must call /after/ ZRastPort inited */
			set_color (1, 1);
			}
		}

	else		/* closing, clean up */
		{
		if (ZWindow)
			CloseWindow (ZWindow);
		if (ZScreen)
			CloseScreen (ZScreen);
		}

	return (NOERR);
}

/*------------------------------*/
/*	z_init			*/
/*------------------------------*/

#define ZHLEN 64		/* file header len (bytes) */
#define PFLAGS 8		/* flags word (in header) */
#define FCOLO 64		/* color-requested bit */
#define FSOUN 128		/* sound-requested bit */

LONG open_game ();		/* forward */
LONG close_game ();
LONG read_file ();

WORD z_init ()
{
	WORD	err;
	CHAR	cur_prefs[1];
	CHAR	old_YSize;
	UWORD	zheader[ZHLEN/2];

	CHAR *il = "intuition.library";
	CHAR *gl = "graphics.library";
/**	CHAR *dl = "diskfont.library";  **/
	CHAR *icl = "icon.library";
	CHAR *cd = "console.device";

/* open the libraries we'll need */

	IntuitionBase = (LONG) OpenLibrary (il, 0);
	if (IntuitionBase == 0) {
		fatalmsg (cno);  fatalmsg (il);
		return (ERR);
		}

	GfxBase = (LONG) OpenLibrary (gl, 0);
	if (GfxBase == 0) {
		fatalmsg (cno);  fatalmsg (gl);
		return (ERR);
		}

/**	DiskfontBase = (LONG) OpenLibrary (dl, 0);
	if (DiskfontBase == 0) {
		fatalmsg (cno);  fatalmsg (dl);
		return (ERR);
		}		**/

/* could close the WorkBench screen, to free up memory */
/**	CloseWorkBench ();	**/	/* rude for user though */

/* check/adjust the default font */

	GetPrefs (&cur_prefs, 1);	/* get user's default size */	
	old_YSize = cur_prefs[0];	/* 9 or 8 */

#if EZIP

/* temporarily adjust Prefs, if necessary, to ensure 80 columns
   (seems that this must be done before creating the Custom screen) */

	if (old_YSize != TOPAZ_EIGHTY)
		{
		cur_prefs[0] = TOPAZ_EIGHTY;
		SetPrefs (&cur_prefs, 1, FALSE);	/* force to 8 */
		}
#endif

/* read the game file header and see if color is desired */

	err = open_game();
	if (err == 0) {
		err = read_file (game_ref, 0, ZHLEN, &zheader);
		close_game();
		}
	if (err) {
		fatalmsg (cno);  fatalmsg (gamename);
		return (ERR);
		}

	color = zheader[PFLAGS] & FCOLO;	/* nonzero if bit set */

/* create and open a game window (and custom screen, for EZIP) */

	err = window_init (TRUE, color);
	if (err)
		return (ERR);	/* (fatalmsg already called) */

/**	sysfont = OpenDiskFont (&sysTextAttr);	/-* 2nd time, actually *-/
	altfont = OpenDiskFont (&altTextAttr);	/-* ZERO if error *-/
**/
	setup_fonts ();		/* our replacement for OpenDiskFont */

/* don't CREATE a console device, just get its library vector */

	if (OpenDevice (cd, -1, &ZIOStdReq, 0) != 0) {
		fatalmsg (cno);  fatalmsg (cd);
		return (ERR);
		}
	ConsoleDevice = ZIOStdReq.io_Device;

/* create a console device, attaching it to our window */
/**	ZIOStdReq.io_Data = (LONG) ZWindow;
	if (OpenDevice (cd, 0, &ZIOStdReq, 0) != 0)
		return (ERR);
**/

#if EZIP
/* undo the temporary font setting, if it was changed above */

	if (old_YSize != TOPAZ_EIGHTY)
		{
		cur_prefs[0] = old_YSize;
		SetPrefs (&cur_prefs, 1, FALSE);    /* restore user's choice */
		}
#endif

/* initialize our internal text size variables */

#if EZIP
	y_char = TOPAZ_EIGHTY;		/* always 8 (80 columns) */
	x_char = 8;

#else		/* use the default fontsize */

	y_char = old_YSize;		/* either 8 or 9 */

	if (old_YSize != TOPAZ_EIGHTY)
		x_char = 10;		/* can only be Topaz 60 */
	else	x_char = 8;
#endif

	window_resized ();		/* set up rows/columns globals */

/* set up the message port in the I/O request -- DEAD */

/**	ZMsgPort.mp_Node.ln_Type	= NT_MSGPORT;
	ZMsgPort.mp_Flags		= 0;
	ZMsgPort.mp_SigBit		= AllocSignal (-1);
	ZMsgPort.mp_SigTask		= (struct Task *) FindTask (NULL);

	AddPort (&ZMsgPort);
	ZIOStdReq.io_Message.mn_ReplyPort = &ZMsgPort;  **/

/* and start reading -- DEAD */

/**	ZRDStdReq = ZIOStdReq;		/-* copy the whole structure *-/
	queue_read ();		**/

/* etcetera */

#if EZIP
#else
	split_row = 1;		/* default for ZIP (0 for EZIP) */
#endif
/**	cur_row = 0;	cur_column = 0;  **/	/* (automatic) */
/**	write_cursor_pos ();  **/

/* default path+name for initial save */

	getcwd (saveback);
	strcat (saveback, saveinit);

/**	RememberKey = NULL;  **/		/* (automatic) */

/* Get the Save Icon info during init.  This avoids having to access the game
   disk later, when it may have been removed from the drive */

	IconBase = (LONG) OpenLibrary (icl, 0);
	if (IconBase == NULL)
		diskobj = NULL;		/* (not a fatal error) */
	else
		{ 
		diskobj = (struct DiskObject *) GetDiskObject ("Icon.Data");

		/* if info file not found, just return diskobj NULL */
		}

/***	init_interrupt (1);	***/	/* (sound etc) */

	if (zheader[PFLAGS] & FSOUN) {

/* pre-allocate a 64K buffer for our "standard" single channel of sound data.
   We would prefer to allocate sound buffers dynamically and free them when 
   not in use, but doing so risks failure.  One reason is that Amiga free
   memory tends to fragment, making it impossible later on to get the large 
   blocks our sound requires.  */

		soundbuf = (UBYTE *)AllocRemember    /* auto cleanup */
			(&RememberKey, 64*1024, (MEMF_CHIP | MEMF_CLEAR));
		}
	return (0);
}	

/*------------------------------*/
/*	setup_fonts		*/
/*------------------------------*/

/* This routine inits the sysfont and altfont vars.  For the system font,
we just pick up the pointer from our RastPort.  Don't need to call
Open[Disk]Font a second time; it was called implicitly when our custom
screen/window was created.

   Setup for our alternate font is trickier.  We do /not/ want to use
OpenDiskFont because of the assumptions it makes about where to find 
the file (a system-wide FONTS: directory), and also about the existence
of a name file and a separate data subdirectory.  Instead, we simulate 
that call by reading the alternate data file into memory ourself,
and patching up a few pointers.
*/

#define ALTNAME "Graphic.Data"
/* don't call it "Font.Data" since user might be lead to needlessly copy
   it into font directory */

VOID
setup_fonts ()
{
	LONG fd, len, actual_len, *lp;
	WORD *wp;
	register char *p, *segbase;

	sysfont = ZRastPort->Font;

/* if an alternate font doesn't exist, just leave altfont NULL */

	fd = Open (ALTNAME, MODE_OLDFILE);
	if (!fd)  return (1);			/* if error, just exit */

	Seek (fd, 0, OFFSET_END);
	len = Seek (fd, 0, OFFSET_BEGINNING);	/* get length */

/* Allocate memory for font data; deallocation will be handled automatically
   (by FreeRemember) upon program exit. */

	p = (char *) c_getmem (len);
	if (p) {
		actual_len = Read (fd, p, len);
		if (actual_len == len) {

			segbase = p + 0x20;	/* skip seg header stuff */
			altfont = (struct TextFont *) (segbase + 0x3A);

/* absolutize some ptrs, all relative to segbase */

			lp = (LONG *) (segbase + 0x44);
			*lp += (LONG) segbase;	/* font name string (NULL) */

			lp = (LONG *) (segbase + 0x5C);
			*lp += (LONG) segbase;	/* packed char data */

			lp = (LONG *) (segbase + 0x62);
			*lp += (LONG) segbase;	/* char offsets array */

/* also patch this one; not sure what it is (disk vs ROM font?) */

			wp = (WORD *) (segbase + 0x58);
			*wp = 0x0001;
			}
		}
	Close (fd);
}

#if DEADCODE		/* currently require no true interrupts */

/*------------------------------*/
/*	VertBServer		*/
/*------------------------------*/

/* This routine runs at the interrupt level.  Happily, registers D0-D1/
   A0-A1 are scratch in interrupt protocol as well as the Lattice compiler.
   Also, references to global vars are OK since in Lattice they are absolute, 
   rather than keyed off any special register.  System calls are OK (unless 
   otherwise advertised), since standard "glue" code saves register A6 then 
   loads a dispatch vector from an absolute location, or from one of the 
   call's parameters in some cases (CheckIO?).
*/

extern void int_sound();

LONG VertBServer()		/* run our interrupt routines */
{
	int_sound();
	int_key();
	return (0);		/* and continue down server chain */
}

/*------------------------------*/
/*	init_interrupt		*/
/*------------------------------*/

/* Install our interrupt(s) on the vertical-blank interrupt chain (one of
   the simpler ways to do Amiga interrupts).  Executes up to 60 times/sec, 
   low priority.  */

init_interrupt (start)
int start;
{
	static BOOL inited = 0;
	static struct Interrupt zir;		/* should be MEMF_PUBLIC? */
	register struct Interrupt *p = &zir;	/* (tighten up code) */

	if (start) {
		p->is_Node.ln_Type = NT_INTERRUPT;
		p->is_Node.ln_Pri = -60;	/* not time-critical */
		p->is_Node.ln_Name = "zir";
	/**	p->is_Data = 0;  **/		/* currently unused */
		p->is_Code = (void *) VertBServer;

	/* put the new interrupt server into action */
		AddIntServer (INTB_VERTB, p);
		inited = 1;
		}
	else {
		if (inited)
			RemIntServer (INTB_VERTB, p);
		}
}
#endif /* DEADCODE */

/*------------------------------*/
/*	z_exit			*/
/*------------------------------*/

/* Clean up everything, in reverse order of creation.  Any memory allocated
   but not freed will be lost to eternity.
*/

VOID
z_exit ()
{
/**	init_interrupt (0);	**/		/* remove our interrupt */

/** shut down scripting, if no one else did **/
/**	if (prt_inited)
		script_init (FALSE);  **/	/* now in 68K */

/* free the save file icon stuff, if it existed */
	if (diskobj)
		FreeDiskObject (diskobj);
	if (IconBase)
		CloseLibrary (IconBase);

/* de-allocate all memory claimed by 68K kernel initializations */
	if (RememberKey)
		FreeRemember (&RememberKey, TRUE);

/* close the various Console Device structures */

/**	RemPort (&ZMsgPort);		  **/
/**	FreeSignal (ZMsgPort.mp_SigBit);  **/	/* DEAD */

	if (ConsoleDevice)
		CloseDevice (&ZIOStdReq);	/* looks odd, but correct */

/* close our window (and screen, for EZIP) */

/**	if (altfont) {			/-* DEAD, see setup_fonts *-/
		CloseFont (altfont);
	/-*	RemFont (altfont);  *-/ /-* only if AvailFonts/AddFont used *-/
		}
	if (sysfont)
		CloseFont (sysfont);	    /-* necessary? opened twice *-/
**/
	window_init (FALSE, color);

/* try to re-open WorkBench, if we closed it */

#if EZIP
/**	OpenWorkBench ();	**/
#endif

/* lastly, close the libraries we opened */

/**	if (DiskfontBase)  CloseLibrary (DiskfontBase);	**/
	if (GfxBase)  CloseLibrary (GfxBase);
	if (IntuitionBase)  CloseLibrary (IntuitionBase);
}

/*------------------------------*/
/*	fatalmsg		*/
/*------------------------------*/

/* Display a fatal error message.  For initialization errors, we don't want
   to assume that our normal output window is operational, so we send the
   message to the window of our controlling process -- either (1) the CLI,
   or (2) the temporary window opened by our Workbench startup code.

   Just before we exit, pause long enough so that (in case 2) the message
   can be read before the window is closed.  Don't try to getc() a key;
   might hang if temp window not opened, and inappropriate in case 1.  */

fatalmsg (s)
char *s;
{
	if (s)
		printf (s);
	else {
		printf ("\n");
		delay (20);	/* two secs */
		}
}

/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	parse_args		*/
/*------------------------------*/

parse_args (argc, argv)
LONG	argc;
CHAR	*argv[];	/* an array of pointers */
{
	CHAR	*argx;
	CHAR	char1, char2;
	int i;

/* The only valid command line argument is F/n, where n is the minimum
   number of bytes to leave in the free memory pool.

   If the argument is missing, or not a number, or if the game is run from
   WorkBench, leave the default (MEM_SYSTEM, defined in c_maxmem).

   The game takes as much memory as it can use, within the above limit.
*/

	mem_freereq = MEM_DEFAULT;		/* assume no memory arg */

/* A general problem: the name of an executable file containing spaces
   (more than one word) is not collected into one string!  */

/**	if ((argc >= 1) && (argv != 0))
		execname = (char *) argv[0];	/-* (used in window title) *-/
**/
	if ((argc > 1) && (argv != 0))		/* got one */
	    for (i=1; i<argc; i++)		/* (skip initial arg) */
		{
		argx = (CHAR *) argv[i];

		char1 = *argx++;
		char2 = *argx++;

	/* string should start with "F/" */

		if (
		   ((char1 == 'F') || (char1 == 'f')) 
				   && (char2 == '/')
		   )

			{
		/* okay, convert string to a number */

			if (stcd_i (argx, &mem_freereq ) == 0)

		/* error, it's not a number */

				mem_freereq = MEM_DEFAULT;
			}
		}
}

/*------------------------------*/
/*	main			*/
/*------------------------------*/

main (argc, argv)
LONG	argc;
CHAR	*argv[];	/* an array of pointers */
{
	WORD	err;

	parse_args (argc, argv);

	err = z_init ();	/* set up most machine-dep stuff */
	if (!err)
		ZSTART ();	/* enter the ZIP kernel */

	z_exit ();		/* clean up */
	if (err)
		fatalmsg (0);	/* pause, so can read message */
}

