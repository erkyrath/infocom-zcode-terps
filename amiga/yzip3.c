
#define TOPAZ8_BASE 6		/* 80-col font baseline */

/************************************************************************/
/*	Low Level Console Device I/O					*/
/************************************************************************/

#if DEADCODE

/*------------------------------*/
/*	write_console		*/
/*------------------------------*/
VOID
write_console (str, len)	/* send raw string to console device */

CHAR	*str;
WORD	len;
{
	/** DEAD **/
}

/*------------------------------*/
/*	queue_read		*/
/*------------------------------*/

VOID
queue_read ()
{
	/** DEAD **/
}

/*------------------------------*/
/*	read_console		*/
/*------------------------------*/

VOID /*CHAR*/
read_console ()			/* get a char from console device */
{
	/** DEAD **/
}

/*------------------------------*/
/*	send_control_seq	*/
/*------------------------------*/

#define	begin_mark 0x009B	/* one unsigned byte, was '\233' */

VOID
send_control_seq (str)	/* write it, prefixed with control char */
CHAR	*str;
{
	/** DEAD **/
}

/*------------------------------*/
/*	get_control_seq		*/	/* read chars into buffer */
/*------------------------------*/

VOID
get_control_seq (buffer, end_mark)
UCHAR	*buffer;
UCHAR	end_mark;
{
	/** DEAD **/
}

/*------------------------------*/
/*	stash_put		*/
/*------------------------------*/

/**
#define STASH_SIZE 64		/-* max size *-/

CHAR	stash[STASH_SIZE];	/-* typeahead saved here *-/
WORD	stash_len = 0;	    **/

/* The next two calls are used to maintain a typeahead buffer, saving
keys that would otherwise be thrown away.  This situation occurs only
because of the awful way the cursor position is read -- a special escape
sequence is sent to the console device and the position is reported
back via the input stream, mixed with any typeahead. */

stash_put (c)		/* DEAD -- not needed */
CHAR c;
{
/**	if (stash_len < STASH_SIZE) {
		stash[stash_len] = c;
		stash_len++;
		}		**/
}

/*------------------------------*/
/*	stash_get		*/
/*------------------------------*/

CHAR stash_get ()	/* return a char, or NULL if none */
{
/**	CHAR c = 0;
	WORD i;

	if (stash_len) {		/-* any saved typeahead? *-/
		c = stash[0];		/-* yes, return it FIFO *-/
		stash_len--;
		for (i=0; i<stash_len; i++)	/-* and slide up the rest *-/
			stash[i] = stash[i+1];
		}
	return (c);	**/
}

/************************************************************************/
/*	Cursor Positioning						*/
/************************************************************************/

/* ALL absolute cursor positioning should go through these two routines.
   Since the kernel uses "cur_row" and "cur_col" to track cursor position,
   always use those globals instead of passing parameters.

   Note that the interpreter defines the cursor home position as (0,0) 
   while the console device (as well as the ZIP spec) defines it as (1,1).
*/

/*------------------------------*/
/*	read_cursor_pos		*/
/*------------------------------*/

VOID
read_cursor_pos ()	/* read, unparse, and digitize cursor position, 
			     store in globals */
{
	/* dead, using Move/Text */
}

/*------------------------------*/
/*	write_cursor_pos	*/
/*------------------------------*/

VOID
write_cursor_pos ()	/* set the cursor position */
{
	/* dead, using Move/Text */
}

#endif  /* DEADCODE */

/************************************************************************/
/*	Screen Handling							*/
/************************************************************************/

/*------------------------------*/
/*	ZScrlRast		*/
/*------------------------------*/

/* This is a YZIP "front end" to the Amiga's ScrollRaster call.
   We hack the vertical boundary points to take into account the way
   YZIP steals the title bar area. */

VOID ZScrlRast (dx, dy, xmin, ymin, xmax, ymax)
int dx, dy, xmin, ymin, xmax, ymax;
{
	ScrollRaster (ZRastPort, dx, dy, 
		xmin, ymin-Peek1, 
		xmax, ymax-Peek1 );
}

/*------------------------------*/
/*	amiga_scroll		*/
/*------------------------------*/

VOID
amiga_scroll (up_pix)		/* scroll (the current YZIP window) */
int up_pix;
{
/* Note: the rect passed to ScrollRaster /includes/ the area to be covered 
   over.  Also, the rect should define the "actual perimeter" of the area, 
   not p+1.  */

	ZScrlRast (0, up_pix,			/* dx, dy */
		first_col, first_row,		/* xmin, ymin */
		last_col-1, last_row-1);	/* xmax, ymax */
}

/*------------------------------*/
/*	erase_line		*/
/*------------------------------*/

/* erase within the current line of the (current) screen --
   default bounds are current column and the right edge of the screen */

VOID
erase_line (start_col /*absolute*/, len)	/* was erase_eol */
WORD start_col, len;
{
	WORD end_col;	 /* also absolute */

	if (start_col == -1)  start_col = cur_col;

	if (len == -1)  end_col = last_col;
	else {
		end_col = start_col + len;
		if (end_col > last_col)  end_col = last_col;
		}

/* YZIP note: could use RectFill here, but its modes are confusing, as well
   as its attempt to draw boundary lines (!) (Mortimer p279).  A simple
   hack for erasing to the background color is to just scroll the offending
   bits into oblivion. */

	ZScrlRast (0, y_char,			/* dx, dy */
		start_col, cur_row,		/* xmin, ymin */
		end_col-1, cur_row + y_char-1);	/* xmax, ymax */

/**	Move (ZRastPort, x_align, y_align);
	ClearEOL (ZRastPort);		**/

/**	send_control_seq ("K");	 **/	/* clear to EOL */
}

/*------------------------------*/
/*	clear_window		*/
/*------------------------------*/
VOID
clear_window (arg)	/* was clear_lines */
WORD	arg;
{
	WORD top, left, bot, right;

	if (arg == -1) {	/* clear entire screen */
		top = 0;		left = 0;
		bot = tot_rows;		right = tot_cols;
		}
	else {
		top = first_row;	left = first_col;
		bot = last_row;		right = last_col;
		}

/* As before, we accomplish the RectFill with a scroll. */

	ZScrlRast (0, bot-top,			/* dx, dy */
		left, top,			/* xmin, ymin */
		right-1, bot-1);		/* xmax, ymax */
}

/*------------------------------*/
/*	line_out		*/
/*------------------------------*/

VOID
line_out (str, len)	/* draw a string at current position, */
			/*   first check length and clip if required */
CHAR	*str;
WORD	len;		/* (in chars) */
{
	WORD	x_align, y_align;
	WORD	pix_len, clip_len;
/**	CHAR	old_endmark;  **/

	clip_len = (last_col /*columns*/ - cur_col) / x_char;
	if (len > clip_len)
		len = clip_len;
	pix_len = len * x_char;

/**	old_endmark = str[len];
	str[len] = 0;  **/		/* make it asciz */

	x_align = cur_col /* * x_char */;
	y_align = (cur_row /* *y_char*/) + TOPAZ8_BASE;  /* allow for baseline */

/* for YZIP, hack the vertical pos */

	Move (ZRastPort, x_align, y_align - Peek1);
	Text (ZRastPort, str, len);

	cur_col += pix_len;
/**	str[len] = old_endmark;  **/
}

/*------------------------------*/
/*	char_out		*/
/*------------------------------*/

VOID
char_out (c)	/* if CR or BS, special case, else draw the char */
CHAR	c;
{
	CHAR	temp[2];	/* even-aligned */

	if (c == 13)
		{
		cur_col = first_col + marg_left;
		cur_row += y_char;
		if (last_row - cur_row >= y_char)
			return;		/* not yet at bottom */
		else {
			cur_row -= y_char;
			amiga_scroll (y_char);
			}
		}

	else if (c == 8)
		{
		BS_cursor();
		char_out (32);
		BS_cursor();
		}
	else		/* normal char, draw it */
		{
		if (cur_col >= last_col /*columns*/)	/* first goto next line */
			char_out (13);
		temp[0] = c;
		line_out (&temp, 1);
		}
}

/*------------------------------*/
/*	BS_cursor		*/
/*------------------------------*/
VOID 
BS_cursor ()
{
	if (cur_col >= first_col + marg_left + x_char)
		cur_col -= x_char;		    /* normal BS */
	else if (cur_row >= first_row + y_char ) {

/* YZIP: need a static global for backups across lines ... */
		cur_col = (last_col /*columns*/ - marg_right) - x_char;
		cur_row -= y_char;		    /* backup a line */
		}		
}

/************************************************************************/
/*	Input handling							*/
/************************************************************************/

/* Maintain a blinking cursor.  This routine is called from setup_input()
   and from intuitick(), every 1/10 sec while we await input */

#define ONTICK   0
#define OFFTICK  7	/* duty cycle is 70% */
#define MAXTICK 10	/* ... of one second */

/*------------------------------*/
/*	blink_cursor		*/
/*------------------------------*/

VOID blink_cursor (arg)		/* 1= starting, -1= ending, 0= waiting */
WORD arg;
{
	static WORD ticks;
	static BOOL curs_on = FALSE;	/* initially off */
	int old_hl;			/* save current here */
/**	static BOOL blinky = FALSE; **/	/* Amiga (CLI) default is NO blink */

	if (!curs_vis)
		return;		/* cursor suppressed, avoid displaying */

	switch (arg) {
	case 1:
		ticks = ONTICK;
		break;
	case -1:
		ticks = OFFTICK;
		break;
/**	case 0:			    /-* not handled *-/
		if (blinky) {	    /-* should make this a global option *-/
			ticks++;
			if (ticks >= MAXTICK)	/-* wrap around *-/
				ticks = ONTICK;
			}
		break;		**/
	}

/* Since our cursor is currently always non-blinking, for now we'll call 
   hack_wsize() from setup_input() only, to make things simpler. */

	if ((ticks == ONTICK) && !curs_on) {	/* show cursor */
	/**	hack_wsize (0);	**/	/* normalize for output! */
		old_hl = highlight (-1);
		highlight (1);		/* inverse vid */
		char_out (32);

		highlight (old_hl);
	/**	hack_wsize (1);	**/
		curs_on = TRUE;
		}
	if ((ticks == OFFTICK) && curs_on) {	/* erase cursor */
	/**	hack_wsize (0);	**/	/* normalize for output! */
		char_out (8);
	/**	hack_wsize (1);	**/
		curs_on = FALSE;
		}
}

/*------------------------------*/
/*	setup_input		*/
/*------------------------------*/

VOID setup_input (start, single)	/* show/hide cursor */
BOOL start, single;		/* if single, don't allow "menu strings" */
{
	if (start) {
		unhide_screen ();	/* just in case ... */
		blink_cursor (1);	/* for now, OUTSIDE hack_wsize() */
		hack_wsize (1);
		}
	else {
		hack_wsize (0);
		blink_cursor (-1);
		}
}

/*------------------------------*/
/*	hack_wsize		*/
/*------------------------------*/

/* To allow YZIP to use the entire screen (i.e. draw into the titlebar area)
   and at the same time to allow the titlebar gadgets to work in the usual 
   way, this routine hacks the window's vertical size to include/exclude 
   the titlebar (or a portion thereof).

   This routine is called from setup_input(); could be temporarily undone 
   from blink_cursor() if necessary.  We hope it causes no strange 
   side effects.

   The altered state currently exists /only/ during input.  This can cause 
   problems when running a debugger.  It may also be seen by users as
   having slightly strange responsiveness.  IDEA: could call from output 
   bottlenecks instead, but would cut more into performance.

   Ideally, we'd like to do a one-time hack to some piece of the 
   window/rastport/layer structure to effect this behavior.  Possible? 
*/

hack_wsize (start_input)
BOOL start_input;
{
	int y_htb;
	struct Layer *lp;

	if (start_input)
		y_htb = HIDDEN_TBAR_HEIGHT;
	else	y_htb = -HIDDEN_TBAR_HEIGHT;

/* could replace all of the following with MoveWindow/SizeWindow, EXCEPT
   that those calls don't take effect immediately!  */

	ZWindow->TopEdge += y_htb;
	ZWindow->Height -= y_htb;

	ZWindow->MinHeight -= y_htb;	/* do these for good measure */
	ZWindow->MaxHeight -= y_htb;
	ZWindow->GZZHeight -= y_htb;

/* apparently we must also hack some layer stuff too.  Careful ... */

	if (lp = ZWindow->RPort->Layer)
		{
		lp->bounds.MinY		  += y_htb;	/* (top edge) */
		lp->ClipRect->bounds.MinY += y_htb;

	/* other ClipRect ptrs?  ClipRegion? */
		}
}

/*------------------------------*/
/*	german_convert		*/
/*------------------------------*/

/* Convert a German extension char from/to its native Amiga code to/from
   the common XZIP code.  If the given char is not in the German set,
   return NULL.  */

UBYTE german_convert (c, from_native)
UBYTE c; 
BOOL from_native;	/* true for input, false for output */
{
    register UBYTE *p, *p2;
    UBYTE c2 = 0;

    static UBYTE gtab[] =	/* German conversion table for the Amiga */
	{
	0xE4,	155,		/* ae (umlaute a) */
	0xF6,	156,		/* oe (umlaute o) */
	0xFC,	157,		/* ue (umlaute u) */
	0xC4,	158,		/* AE */
	0xD6,	159,		/* OE */
	0xDC,	160,		/* UE */
	0xDF,	161,		/* ss */
	0xBB,	162,		/* >> (left quote) */
	0xAB,	163,		/* << (right quote) */
	0, 0 };			/* zero marks end of table */

    if (from_native) {
	p = &gtab[0];	p2 = &gtab[1];
	}
    else {
	p2 = &gtab[0];	p = &gtab[1];
	}

    while (*p != 0) {
	if (*p == c) {		/* found it? */
	    c2 = *p2;		/* yes, return corresponding value */
	    break;
	    }
	p += 2;
	p2 += 2;
	}
    return (c2);
}

/*------------------------------*/
/*	map_fkey		*/
/*------------------------------*/

/* map an FKey to its XZIP value.  Return NULL if not found.  */

UBYTE map_fkey (raw)
UBYTE raw;
{
    register UBYTE *p;
    UBYTE c = 0;

/* FKey translation table for the Amiga.  The values on the left are raw
   keycodes which (for the following groups of keys) I believe are invariant
   across the various international Amiga keyboards.  */

    static UBYTE keytab[] =
	{
	0x4C,	129,		/* up arrow */
	0x4D,	130,		/* down arrow */
	0x4F,	131,		/* left arrow */
	0x4E,	132,		/* right arrow */
	0x50,	133,		/* F1 */
	0x51,	134,		/* F2 */
	0x52,	135,		/* F3 */
	0x53,	136,		/* F4 */
	0x54,	137,		/* F5 */
	0x55,	138,		/* F6 */
	0x56,	139,		/* F7 */
	0x57,	140,		/* F8 */
	0x58,	141,		/* F9 */
	0x59,	142,		/* F10 */
	/* no F11, F12 */
	0, 0 };			/* zero marks end of table */

    p = keytab;

    while (*p != 0) {
	if (*p++ == raw) {	/* found it? */
	    c = *p;		/* yes, pick up XZIP value */
	    break;
	    }
	p++;
	}
    return (c);
}

/*------------------------------*/
/*	raw_key			*/
/*------------------------------*/

/* Use the system KeyMap to convert a raw keycode to the correct ascii 
   value (or for certain keys, a console-device-style escape sequence).  
   (see Enhancer1.2 booklet p66).  Then adjust as required any value that
   should fall into the extended (128+) XZIP character set. */

#define KBLEN 32	/* (known max is 4) */

UBYTE raw_key (msg)
struct IntuiMessage *msg;
{
	UBYTE keycode, c, kbuffer[KBLEN];
	static struct InputEvent iev = {NULL, IECLASS_RAWKEY, 0,0,0};
	int len;

/* copy input event info */

	keycode = msg->Code;
	iev.ie_Code = keycode;
	iev.ie_Qualifier = msg->Qualifier;

/* To support deadkeys (1.2 only), get previous codes from location
   pointed to by IAddress.  (This pointer not valid after IntuiMessage
   is replied ?)  */

	iev.ie_position.ie_addr = *((APTR *) msg->IAddress);

/* convert key to ascii (must have previously opened console device)  */

	len = RawKeyConvert (&iev, &kbuffer, KBLEN, NULL);

/* translate special keys ... */

	if (len <= 0)  return (0);	/* key-up event, deadkey, etc */

	if (len > 1) {			/* FKey, cursor key, (Help) */
		c = map_fkey (keycode);
		return (c);
		}

	c = kbuffer[0];		/* length is 1 exactly */

/* Check if c is a number; if so, THEN check if it came from the keypad
   (some international keyboards assign special chars to shifted keypads) */

	if ((c >= '0') && (c <= '9'))
		if (keycode >= 0x0F) {		/* keypad '0' */
			c += 145 - '0';
			return (c);
			}

/* Check if c is a "post-deadkey" (or an actual international key) */

	if (c >= 128) {
		c = german_convert (c, TRUE);	/* 0 if not */
/**		return (c);  **/
		}

	return (c);
}

/*------------------------------*/
/*	mouse_buttons		*/
/*------------------------------*/

/* this routine monitors only the mouse "selection" button (the left one) */

UBYTE mouse_buttons (im)
struct IntuiMessage *im;
{
	UBYTE retval = 0;	/* default val */
	ULONG cursecs, curmics;
static	ULONG oldsecs = 0, oldmics = 0;

	switch (im->Code) {

	case SELECTDOWN:		/* button just pressed */
		xmouse = (im->MouseX /* /x_char */) + 1;
		ymouse = (im->MouseY /* /y_char */) + 1
	/* since we're doing input, must include this fudge factor */
			+ HIDDEN_TBAR_HEIGHT;

		cursecs = im->Seconds;
		curmics = im->Micros;

	/* use a system call to check if it's a double */

		if (DoubleClick (oldsecs, oldmics, cursecs, curmics)) {
			oldsecs = 0;	oldmics = 0;
			retval = 253;	/* double click */
			}
		else {
			oldsecs = cursecs;	oldmics = curmics;
			retval = 254;	/* single click */
			}
		break;

/**	case SELECTUP:			/-* button just released *-/
		retval = 0;
		break;		**/
	}
	return (retval);
}

/*------------------------------*/
/*	intuiticks		*/
/*------------------------------*/

/* this routine called every 1/10 sec (approx) */

UBYTE intuiticks (im)
struct IntuiMessage *im;
{
	blink_cursor (0);	/* maintain the cursor */
	return (255);		/* return a special code (for timeouts) */
}

/*------------------------------*/
/*	event_multi		*/
/*------------------------------*/

/* Note: returns 0 for a null event, 255 for a timer event */

UBYTE event_multi ()	/* wait for an input event */
{
	struct IntuiMessage *msg, localmsg;
	int i;
	UBYTE *s, *d, val;

/* must be sure that a task's port is empty before the task can be put
   to sleep (Peck p202) */

	msg = (struct IntuiMessage *) GetMsg (ZWindow->UserPort);
	if (msg == NULL) {
		WaitPort (ZWindow->UserPort);		/* sleep */
		msg = (struct IntuiMessage *) GetMsg (ZWindow->UserPort);
		}

/* copy the contents of the message to our own area, so we can reply it
   as quickly as possible  (Peck p155) */

	s = (UBYTE *) msg;
	d = (UBYTE *) &localmsg;
	for (i=0; i<sizeof(struct IntuiMessage); i++)
		*d++ = *s++;

/* and reply it (unless it's of type RAWKEY -- in supporting deadkeys,
   I think we may have to hold the block until after it's processed */

	if (msg->Class != RAWKEY) {
		ReplyMsg (msg);		/* return block to system */
		msg = NULL;
		}

	switch (localmsg.Class) {

	case RAWKEY:
		val = raw_key (&localmsg);
		break;
	case MOUSEBUTTONS:
		val = mouse_buttons (&localmsg);
		break;
	case INTUITICKS:
		val = intuiticks (&localmsg);
		break;
	case DISKINSERTED:
	/*	val = disk_inserted (&localmsg); */
		fsel_diskevent ();	/* keep FileIO happy */
		val = 0;
		break;

/**	case SIZEVERIFY:			/-* unused cases *-/
		val = size_verify (&localmsg);
		break;
	case NEWSIZE:
		val = new_size (&localmsg);
		break;
	case REFRESHWINDOW:
		val = refresh_window (&localmsg);
		break;
	case MOUSEMOVE:
		val = mouse_move (&localmsg);
		break;
	case GADGETDOWN:
		val = gadget_down (&localmsg);
		break;
	case GADGETUP:
		val = gadget_up (&localmsg);
		break;
	case REQSET:
		val = req_set (&localmsg);
		break;
	case MENUPICK:
		val = menu_pick (&localmsg);
		break;
	case CLOSEWINDOW:
		val = close_window (&localmsg);
		break;
	case REQVERIFY:
		val = req_verify (&localmsg);
		break;
	case REQCLEAR:
		val = req_clear (&localmsg);
		break;
	case MENUVERIFY:
		val = menu_verify (&localmsg);
		break;
	case NEWPREFS:
		val = new_prefs (&localmsg);
		break;
	case DISKREMOVED:
		val = disk_removed (&localmsg);
		break;
	case WBENCHMESSAGE:
		val = wbench_message (&localmsg);
		break;
	case ACTIVEWINDOW:
		val = active_window (&localmsg);
		break;
	case INACTIVEWINDOW:
		val = inactive_window (&localmsg);
		break;
	case DELTAMOVE:
		val = delta_move (&localmsg);
		break;
	case VANILLAKEY:
		val = vanilla_key (&localmsg);
		break;
**/					/* end of unused cases */
	default:
		val = 0;
		break;
	}

	if (msg)  ReplyMsg (msg);	/* reply now, if not done earlier */

	return (val);
}

/*------------------------------*/
/*	event_in		*/
/*------------------------------*/

UBYTE event_in ()	/* wait for keyboard (or mouse) input */
{
	UBYTE	c = 0;

/***	c = stash_get();		/-* any saved typeahead? *-/
	if (!c)
		c = read_console ();	/-* no, get a live key *-/
***/
	while (c == 0)			/* return only when something happens */
		c = event_multi ();

	if (c == 255)  c = 0;		/* timer event, return as NULL */
	return (c);
}

/*------------------------------*/
/*	int_key			*/
/*------------------------------*/

/* Should this run as a machine interrupt or a game pseudo-interrupt?
   Some ops ($VERIFY) take a long time.  */

int_key()
{
/* On the Amiga, if event messages (i.e. typed-ahead keys) aren't replied 
   quickly by the application and are allowed to accumulate, the OS
   allocates extra buffers which /never/ get released (Peck p155).

   This can cause nasty fragmentation of free memory and break OPSOUND
   (since it dynamically allocates and releases large blocks).

   One fix: reply every message immediately, and store type-ahead in our
   own small queue.  Should also properly decode FKey and mouse events.

   (If /currently/ awaiting input, just exit) */
}

/************************************************************************/
/*	More Output Routines						*/
/************************************************************************/

/*------------------------------*/
/*	highlight		*/
/*------------------------------*/

int
highlight (mode)	/* if mode = -1, just return current mode */
WORD	mode;
{
static	WORD	old_mode = 0;		/* (in kernel format) */
static	WORD	sd_old = 0;		/* initial SetDrMode */
static	WORD	ss_old = FS_NORMAL;	/* initial SetSoftStyle */

	WORD	sd_mode = 0;
	WORD	ss_mode = FS_NORMAL;

	if (mode == -1)  return (old_mode);
	else old_mode = mode;		/* remember it */

	switch (mode)
	{
	case 0:				/* normal text, no effects */
		break;
	case 1:				/* inverse video */
		sd_mode = INVERSVID;
		break;
	case 2:				/* bold */
		ss_mode = FSF_BOLD;
		break;
	case 4:				/* underlined (italic) */
		ss_mode = FSF_UNDERLINED;
	/**	ss_mode = FSF_ITALIC;	  **/	/* causes spacing problem) */
		break;
	}

	if (sd_mode != sd_old) {
		SetDrMd (ZRastPort, JAM2 + sd_mode);
		sd_old = sd_mode;	/* remember current */
		}

	if (ss_mode != ss_old) {
		SetSoftStyle (ZRastPort, ss_mode, 0xFF);  /* only 1 at a time */
		ss_old = ss_mode;
		}
}

/*------------------------------*/
/*	zfont			*/
/*------------------------------*/

#define ZSYSFONT 1
#define ZALTFONT 3

int
zfont (new_font)	/* if -1, just return current font */
WORD	new_font;
{
static	WORD	old_font = ZSYSFONT;	/* (initially, topaz 80) */
struct	TextFont *tf;

	if (new_font == -1)  return (old_font);

	if (new_font != old_font) {

		if (new_font == ZALTFONT)  tf = altfont;
		else  tf = sysfont;

		if (tf)  SetFont (ZRastPort, tf);
		old_font = new_font;		/* remember it */
		}
}

/*------------------------------*/
/*	set_color		*/
/*------------------------------*/

/* Under YZIP, the various fore/back text colors are now supported using 
   only the first two "pens" (all other pens are used for graphics).
   Therefore, the 2 text colors are now "global", meaning they /can't/ be 
   changed for a single word on the screen, or for a certain window.

   As a hack to support the above, for YZIP we now get a current-window arg.
   We allow text colors to be changed only in window 0, and ignore 
   requests in other windows (except for the special case of bg = -1), 
   since it only succeeds in making the screen flash.  */

WORD set_color (fore, back, curwind)	/* return system defaults */
WORD fore, back, curwind;
{
	/* ignore if display doesn't support colors */
	if (tty_color) {
		if (curwind == 0) {
			set_back (back);  /* first; may have side effect! */
			set_fore (fore);
			}
		else if (back == -1)	/* special case */
			set_back (back);
		}

	return ((DEF_BACK << 8) | DEF_FORE);
}

/*------------------------------*/
/*	set_fore		*/
/*------------------------------*/

static WORD of_id = 0;	/*DEF_FORE*/
static WORD ob_id = 0;	/*DEF_BACK*/	/* (might NOT init be YZIP default) */

set_fore (id)
register WORD id;
{
	register WORD pen, foreRGB;
	int seq[4];

	if (id == 1)
		id = DEF_FORE;			/* use default */

	if (id != of_id)			/* changed? */
	if (id >= BASE_ID && id <= LAST_ID) {
		of_id = id;			/* remember new one */
		pen = colormap[id];		/* convert XZIP id to pen # */

	/* For YZIP, this routine now changes Color Register #1 (as opposed
	   to Pen A, which can be set to any register, but is now always 
	   implicitly set to register 1).
	   Might now make sense to get rid of colorMap (?).  */

		foreRGB = colortable[pen];
		unpack_RGB (foreRGB, &seq);
		SetRGB4 (&(ZScreen->ViewPort), 1, seq[1], seq[2], seq[3]);

	/**	SetAPen (ZRastPort, 1); **/	/* (once, in init) */
		}
}

/*------------------------------*/
/*	set_back		*/
/*------------------------------*/

/* Note: In general, we want the edges of a screen to be the same color as
   our background (otherwise it looks bad).  Since the edges are apparently
   always rendered using pen 0 (changable?), we will always use that pen
   for our background (as opposed to "BPen"), and change its RGB definition
   as required.  
   
   Consequently, like on the ST, there can be only one "primary" 
   background color at a time (although "secondary" backgrounds would be 
   easy enough to support if XZIP recognized the concept).  */

set_back (id)
register WORD id;
{
	register WORD pen, backRGB;
	int seq[4];

	if (id == 1)
		id = DEF_BACK;

	if (id >= BASE_ID && id <= LAST_ID) {

		if (id == ob_id)		/* same, done */
			return (0);
		ob_id = id;			/* remember new one */

		pen = colormap[id];		/* convert XZIP id to pen # */
		backRGB = colortable[pen];

		unpack_RGB (backRGB, &seq);
		SetRGB4 (&(ZScreen->ViewPort), 0, seq[1], seq[2], seq[3]);

		SetBPen (ZRastPort, 0);
		}

	if (id == -1) {		/* "set to current screen bg" */
		pen = ReadPixel (ZRastPort, 
			cur_col, cur_row /* +Peek1? */ );

	/* "secondary background" -- Use one of 14 gfx colors, not 2
	   text colors.  Set the background /pen/ to match the desired pen,
	   so that these background bits will remain undisturbed on screen 
	   after primary background (pen 0) changes to something else. */

		SetBPen (ZRastPort, pen);

	/**	backRGB = GetRGB4 (ZScreen->ViewPort.ColorMap, pen);
		if (backRGB == -1)
			return (-1);		/-* error *-/
		unpack_RGB (backRGB, &seq);
		SetRGB4 (&(ZScreen->ViewPort), 0, seq[1], seq[2], seq[3]);
	**/

		ob_id = -1;			/* invalidate it */
		}
}

unpack_RGB (val, p)	/* unpack 4 nybbles into array at p */
register int val, *p;
{
	register int i;

	for (i=3; i>=0; i--) {
		p[i] = val & 0x000F;	/* rightmost nybble */
		val >>= 4;
		}
}
