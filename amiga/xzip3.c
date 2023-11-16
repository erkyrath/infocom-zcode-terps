
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
   Since the kernel uses "cur_row" and "cur_column" to track cursor position,
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
/*	amiga_scroll		*/
/*------------------------------*/

VOID
amiga_scroll ()		/* scroll the screen (at split_row) */
{
	ScrollRaster (ZRastPort, 0, y_char,		/* dx, dy */
		0, split_row * y_char,			/* xmin, ymin */
		columns * x_char, rows * y_char);	/* xmax, ymax */

/* Note: ymin /includes/ the line being covered, and so is not sr+1 */
/* The last two args could be pre-computed in globals for more speed */
}

/*------------------------------*/
/*	erase_eol		*/
/*------------------------------*/
VOID
erase_eol ()
{
	WORD x_align, y_align;

	x_align = x_char * cur_column;
	y_align = (y_char * cur_row) + TOPAZ8_BASE;  /* allow for baseline */

	Move (ZRastPort, x_align, y_align);
	ClearEOL (ZRastPort);

/**	send_control_seq ("K");	 **/	/* clear to EOL */
}

/*------------------------------*/
/*	clear_lines		*/
/*------------------------------*/
VOID
clear_lines (row1, row2)	/* EZIP, also CZIP (OPSPLIT) */

WORD	row1, row2;
{
	WORD	old_row, old_column;
	WORD	i;

/**	read_cursor_pos ();  **/
	old_row = cur_row;	old_column = cur_column;

	for (i=row1; i<row2; i++)
		{
		cur_row = i; 	cur_column = 0;
		erase_eol ();

		/**	write_cursor_pos ();
		send_control_seq ("K");	 **/	/* clear to EOL */
		}

	cur_row = old_row;	cur_column = old_column;
/**	write_cursor_pos ();  **/
}

/*------------------------------*/
/*	clear_screen		*/
/*------------------------------*/

/**  Not currently used  **/

/*------------------------------*/
/*	line_out		*/
/*------------------------------*/

VOID
line_out (str, len)	/* draw a string at current position, */
			/*   first check length and clip if required */
CHAR	*str;
WORD	len;
{
	WORD	x_align, y_align;
	WORD	clip_len;
/**	CHAR	old_endmark;  **/

	clip_len = columns - cur_column;
	if (len > clip_len)
		len = clip_len;

/**	old_endmark = str[len];
	str[len] = 0;  **/		/* make it asciz */

	x_align = x_char * cur_column;
	y_align = (y_char * cur_row) + TOPAZ8_BASE;  /* allow for baseline */

	Move (ZRastPort, x_align, y_align);
	Text (ZRastPort, str, len);

	cur_column += len;
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
		cur_column = marg_left;
		if (cur_row < rows - 1)
			cur_row++;
		else
			amiga_scroll ();
		}

	else if (c == 8)
		{
		BS_cursor();
		char_out (32);
		BS_cursor();
		}
	else		/* normal char, draw it */
		{
		if (cur_column >= columns)	/* first goto next line */
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
	if (cur_column > marg_left)
		cur_column--;		    /* normal BS */
	else if (cur_row > 0) {
		cur_column = (columns - marg_right) - 1;
		cur_row--;		    /* backup a line */
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

	if ((ticks == ONTICK) && !curs_on) {	/* show cursor */
		old_hl = highlight (-1);
		highlight (1);		/* inverse vid */
		char_out (32);
		highlight (old_hl);
		curs_on = TRUE;
		}
	if ((ticks == OFFTICK) && curs_on) {	/* erase cursor */
		char_out (8);
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
		blink_cursor (1);
		}
	else
		blink_cursor (-1);
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
    register UBYTE *p;
    register int off1, off2;
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

    if (from_native)
	{ off1 = 0; off2 = 1; }
    else
	{ off1 = 1; off2 = 0; }

    p = gtab;

    while (*p != 0) {
	if (p[off1] == c) {	/* found it? */
	    c2 = p[off2];	/* yes, return corresponding value */
	    break;
	    }
	p += 2;
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
		xmouse = (im->MouseX / x_char) + 1;
		ymouse = (im->MouseY / y_char) + 1;

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
	case DISKINSERTED:
		val = disk_inserted (&localmsg);
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

int	hl_mode = 0;		/* current mode (in kernel format) */

/*------------------------------*/
/*	highlight		*/
/*------------------------------*/
int
highlight (mode)	/* if mode = -1, return current mode */
WORD	mode;
{
static	WORD	sd_cur = 0;		/* initial SetDrMode */
static	WORD	ss_cur = FS_NORMAL;	/* initial SetSoftStyle */
static	WORD	font_cur = 0;		/* topaz 80 */

	WORD	sd_mode = 0;
	WORD	ss_mode = FS_NORMAL;
	WORD	font_mode = 0;
struct	TextFont *tf;

	if (mode == -1)  return (hl_mode);
	else hl_mode = mode;		/* remember it */

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
	case 4:				/* underlineD (italic) */
		ss_mode = FSF_UNDERLINED;
	/**	ss_mode = FSF_ITALIC;	  **/	/* causes spacing problem) */
		break;
	case 128:			/* use alt charset */
		font_mode = 1;		
		break;
	}

	if (sd_mode != sd_cur) {
		SetDrMd (ZRastPort, JAM2 + sd_mode);
		sd_cur = sd_mode;	/* remember current */
		}

	if (ss_mode != ss_cur) {
		SetSoftStyle (ZRastPort, ss_mode, 0xFF);  /* only 1 at a time */
		ss_cur = ss_mode;
		}

	if (font_mode != font_cur) {

		if (font_mode)  tf = altfont;
		else  tf = sysfont;

		if (tf)  SetFont (ZRastPort, tf);
		font_cur = font_mode;
		}
}

/*------------------------------*/
/*	set_color		*/
/*------------------------------*/

WORD set_color (fore, back)	/* return system defaults */
WORD fore, back;
{
	/* ignore if display doesn't support colors */
	if (color) {
		set_back (back);	/* first, since may have side effect! */
		set_fore (fore);
		}
	
	return ((DEF_BACK << 8) | DEF_FORE);
}

/*------------------------------*/
/*	set_fore		*/
/*------------------------------*/

set_fore (id)
register WORD id;
{
	register WORD pen;

	if (id == 1)
		id = DEF_FORE;			/* use default */

	if (id >= BASE_ID && id <= LAST_ID) {
		pen = colormap[id];		/* convert XZIP id to pen # */
		SetAPen (ZRastPort, pen);	/* and reset foreground pen */
		}
}

/*------------------------------*/
/*	set_back		*/
/*------------------------------*/

/* Note: In general, we want the edges of a screen to be the same color
   as our background (otherwise it looks bad).  Since the edges are rendered
   using pen 0, we will always use pen 0 for our background, and change 
   its RGB definition as required.  This means that, like the ST, there can
   be only one "primary" background color at a time (although "secondary"
   backgrounds would be easy enough to support if XZIP recognized the
   concept).  */

set_back (id)
register WORD id;
{
	register WORD pen, i2, oRGB, nRGB;
	int seq[4];

	if (id == 1)
		id = DEF_BACK;

	if (id < BASE_ID || id > LAST_ID)
		return (0);		/* (no change) */

	pen = colormap[id];		/* convert XZIP id to pen number */

	if (pen != 0) {
	/* find current id of pen 0 */
		for (i2=BASE_ID; i2<=LAST_ID; i2++) {
			if (colormap[i2] == 0) break;
			}

	/* swap id entries, RGB entries */
		colormap[i2] = colormap[id];
		colormap[id] = 0;
			
		oRGB = colortable[0];
		nRGB = colortable[pen];
		colortable[0] = nRGB;
		colortable[pen] = oRGB;

	/* and install new colors (would be simpler to reload whole table, 
	   but that seems to have the side effect of changing the foreground 
	   color to the last entry in the table (??)) */
	/**	LoadRGB4 (&(ZScreen->ViewPort), colortable, MAXCOLORS); **/

		unpack_RGB (nRGB, &seq);
		SetRGB4 (&(ZScreen->ViewPort), 0, seq[1], seq[2], seq[3]);

		unpack_RGB (oRGB, &seq);
		SetRGB4 (&(ZScreen->ViewPort), pen, seq[1], seq[2], seq[3]);
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
