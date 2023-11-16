
/*------------------------------------------------------*/
/*	AMIGA ZIP/EZIP INTERFACE			*/
/*------------------------------------------------------*/

/* MODIFICATION HISTORY

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
			added stash_put(), stash_get() for typeahead,
			  used by get_control_seq() and read_console(),
			  read_cursor_pos() still NOT working as advertised
	26 AUG 87	added volume control to md_sound (in SOUND.C)
*/

#include "exec\types.h"
#include "exec\exec.h"

#include "libraries\dos.h"
#include "libraries\dosextens.h"

#include "intuition\intuition.h"	/* for AllocRemember */
#include "intuition\intuibas.h"

#include "workbench\icon.h"
#include "workbench\workbench.h"	/* for disk objects */

#define CHAR	unsigned char
#define	INT	short		/* must be 16 bits for 68K compatibility */
#define LONG	long
#define BOOL	INT

#define VOID	int
#define GLOBAL	/**/
#define FOREVER	for(;;)

/* This switch controls conditional ZIP/EZIP compilation */
/*   (now defined in batch file, where compiler is invoked) */

/* #define  EZIP	0	*/	/* set for EZIP, otherwise classic */

#define     ZDEBUG		0	/* set for debugging */
#define     bug_DiskFull	0	/* set if crashes system - FIXED */

/************************************************************************/
/*	Global Variables						*/
/************************************************************************/

#if EZIP

struct  Screen	*ZScreen;
struct  TextAttr    ZTextAttr;		/* defined in \graphics\text.h */

#endif

/* Use altered IOStdReq structure, "data" field type APTR changed to STRPTR.
   Otherwise the compiler forces string pointers to be even-aligned.	*/

struct	IOStdReq_MUNGED    ZIOStdReq;		/* for general console I/O */
struct	IOStdReq_MUNGED    ZRDStdReq;		/* for queued reads */

struct	MsgPort     ZMsgPort;		/* for console I/O */
struct	Window	*ZWindow;

	CHAR	single_char[2];		/* for queued reads */

/* scripting variables -- two methods possible */

	LONG	print_file;		/* scripting refnum */
/*
struct	IOStdReq_MUNGED    SIOStdReq;
struct	MsgPort     SMsgPort;
*/
	INT	prt_inited = 0;		/* set when printer opened */
	INT	prt_active = 0;		/* set when scripting is active */

/* cursor variables */

	INT	y_res, y_char;		/* rows = y_res / y_char */
	INT	x_res, x_char;

GLOBAL	INT	rows, cur_row;		/* cur_row in 0 .. rows-1 */
GLOBAL	INT	columns, cur_column;

GLOBAL	INT	split_row;		/* first row in scrolling area */

/* This buffer is accessed by the kernel. Careful about changing its size */

GLOBAL	CHAR	savename[64];	/* buffer for file_select dialog */
GLOBAL	CHAR	saveback[64];	/* buffer for default name */

	INT	max_index;	/* number of available color indices */
	INT	v_fore = 1;	/* text color */
	INT	v_back = 0;	/* all other colors */
	INT	reversed = 0;	/* reversed video? (MONO ONLY) */


GLOBAL	INT	ms_tick;	/* millisecs per timer tick */
GLOBAL	INT	ms_total;	/* millisecs since cursor was turned on */
	INT	curs_on;	/* set if currently showing */

/* These vars are used to pass results back to a kernal routine, */
/*   since function returns are limited to an error code (by convention) */

	LONG	temp_channel;
	LONG	temp_memory;

/* These vars increase the speed of disk reads (by avoiding extra seeks) */

	LONG	old_offset = -1;	/* initialize to invalid values */
	LONG	old_channel = -1;

/* memory usage can be controlled by user, by supplying a free byte
   count in the CLI command line.  Useful if another task will be launched
   subsequently.  Otherwise game takes as much memory as it can use. */

	LONG	mem_freereq;		/* set during init */

struct	IntuitionBase	*IntuitionBase;

struct	IconBase	*IconBase;
struct	DiskObject	*diskobj;	/* SAVE file icon info */

struct	Remember	*RememberKey;

/* set when console-device forces a scroll, unknown to kernel (happens
   when last column is filled) */

	INT	auto_CR = 0;		/* DEAD, SEE window_resized () */

/* DEAD, SEE bug_DiskFull

   If a NEW save file is being created, check disk free space before
   each write call.  Prevents a useless Retry/Abort dialog (and in the
   case of make_icon_file, a crash).
*/
	BOOL	disk_newsave;
	LONG	disk_free;

/************************************************************************/
/*	Externals							*/
/************************************************************************/

/*
extern LONG trap1 ();
extern VOID START ();
*/

CHAR stash_get ();

/************************************************************************/
/*	Low Level Console Device I/O					*/
/************************************************************************/

/*------------------------------*/
/*	write_console		*/
/*------------------------------*/
VOID
write_console (string, len)	/* send raw string to console device */

CHAR	*string;
INT	len;
{
	ZIOStdReq.io_Data = (STRPTR) string;
	ZIOStdReq.io_Length = len;

	ZIOStdReq.io_Command = CMD_WRITE;
	DoIO (&ZIOStdReq);		/* works OK for writes */
}

/*------------------------------*/
/*	queue_read		*/
/*------------------------------*/

VOID
queue_read ()
{
	ZRDStdReq.io_Command	= CMD_READ;
	ZRDStdReq.io_Data	= &single_char[0];	/* must be aligned */
	ZRDStdReq.io_Length	= 1;

	SendIO (&ZRDStdReq);		/* due to bug in DoIO */
}


/*------------------------------*/
/*	read_console		*/
/*------------------------------*/

CHAR
read_console ()			/* get a char from console device */
{
	CHAR	c;

/**	while (GetMsg (&ZMsgPort) == NULL);	**/  /* busy loop */

	c = stash_get();		/* any saved typeahead? */
	if (!c) {
		WaitPort (&ZMsgPort);	/* no, sleep tastefully til keydown */
		GetMsg (&ZMsgPort);	/* got one, remove msg from queue */
		c = single_char[0];	/* value was left here */
		queue_read ();		/* start the next read */
		}
	return (c);
}


/*------------------------------*/
/*	send_control_seq	*/
/*------------------------------*/

#define	begin_mark	'\233'		/* 0x9B (one byte only) */

VOID
send_control_seq (string)	/* write it, prefixed with control char */

CHAR	*string;
{
	CHAR	temp[64];	/* string must be even aligned */

	temp[0] = begin_mark;
	temp[1] = 0;

	strcat (&temp, string);
	write_console (temp, strlen (temp));
}

/*------------------------------*/
/*	get_control_seq		*/	/* read chars into buffer */
/*------------------------------*/

VOID
get_control_seq (buffer, end_mark)

CHAR	*buffer;
CHAR	end_mark;
{
	CHAR	c;
	INT	i, j;

/* We are waiting for a certain sequence, which always begins with $9B and
   ends with the given end_mark.  Must ignore (1) other sequences which may be
   generated at any time by pressing special keys, and (2) normal keys.

   Other seqs also start with $9B but (hopefully) don't contain end_mark.
   Known examples are cursor keys (A-D), functions (0~-9~), and Help key (?~).
*/

	while ((c = read_console ()) != begin_mark) {
		stash_put(c);		/* not what we want -- but save it! */
		}
	i = 0;				/* new sequence starting */
	while ((c = read_console ()) != end_mark) {
		if (c != begin_mark) {
			buffer[i] = c;
			i++;
			}
		else {			/* some other seq, ignore */
			for (j=0; j<i; j++)
				stash_put(buffer[j]);	/* but save chars */
			i = 0;		/* new sequence starting */
			}
		}
	buffer[i] = end_mark;		/* end with the ending mark */
	buffer[i+1] = 0;		/* make it ASCIZ */
}

/*------------------------------*/
/*	stash_put		*/
/*------------------------------*/

#define STASH_SIZE 64		/* max size */

CHAR	stash[STASH_SIZE];	/* typeahead saved here */
INT	stash_len = 0;

/* The next two calls are used to maintain a typeahead buffer, saving
keys that would otherwise be thrown away.  This situation occurs only
because of the awful way the cursor position is read -- a special escape
sequence is sent to the console device and the position is reported
back via the input stream, mixed with any typeahead. */

stash_put (c)
CHAR c;
{
	if (stash_len < STASH_SIZE) {
		stash[stash_len] = c;
		stash_len++;
		}
}

/*------------------------------*/
/*	stash_get		*/
/*------------------------------*/

CHAR stash_get ()	/* return a char, or NULL if none */
{
	CHAR c = 0;
	INT i;

	if (stash_len) {		/* any saved typeahead? */
		c = stash[0];		/* yes, return it FIFO */
		stash_len--;
		for (i=0; i<stash_len; i++)	/* and slide up the rest */
			stash[i] = stash[i+1];
		}
	return (c);
}

/************************************************************************/
/*	Misc Functions							*/
/************************************************************************/

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

INT	tone;		/* 1 for beep, 2 for boop */
{
/* no internal bell, just ignore beep; flash screen for boop */

	if (tone == 2)
		DisplayBeep (ZWindow->WScreen);	/* (a system call) */
}	

/*------------------------------*/
/*	random_seed		*/
/*------------------------------*/
LONG
random_seed ()
{
	LONG	seconds, micros;

	CurrentTime (&seconds, &micros);

	return (micros);
}


/*------------------------------*/
/*	highlight		*/
/*------------------------------*/
VOID
highlight (mode)

INT	mode;
{
	switch (mode)
	{
	case 0:			/* normal text, no effects */

		send_control_seq ("0m");
		break;
	case 1:			/* inverse video */

		send_control_seq ("7m");
		break;
	case 2:			/* bold */

		send_control_seq ("1m");
		break;
	case 4:			/* italic (underline) */

		send_control_seq ("4m");
		break;
	}
}

/*------------------------------*/
/*	get_size_msg		*/
/*------------------------------*/

/* DEAD -- seems to prevent console-device from getting size update.
   Replaced by "window_resized" routine following.

   The message is not needed anyway, since the ZWindow data structure is 
   always updated as a side-effect of sizing events.

   To use this routine, must have initialized NewZWindow.IDCMPFlags to NEWSIZE.
*/

VOID
get_size_msg ()		/* get (any pending) IDCMP message */
{
/*	struct	Message	*sizeMsg;

	sizeMsg = (struct Message *) GetMsg (ZWindow->UserPort);

	if (sizeMsg)
		{
		*** here we refresh window and update related vars ***

		ReplyMsg (sizeMsg);		*** release the message ***
		}
*/
}

/*------------------------------*/
/*	window_resized		*/
/*------------------------------*/
INT
window_resized ()	/*   update globals, return window column width */

/* this routine is called -
	[] during init
	[] after every line output (this takes care of MOREs)
	[] after a RDLINE input
*/

{

/* determine size of current font */

	/* (DONE ONCE ONLY DURING INIT, SETS Y_CHAR AND X_CHAR) */

/* determine usable size of window */

	y_res = ZWindow->Height - ZWindow->BorderTop - ZWindow->BorderBottom;
	x_res = ZWindow->Width - ZWindow->BorderLeft - ZWindow->BorderRight;

/* Calculate rows and columns.  To prevent undesired AMIGA auto wrapping, we 
   pretend that the column width is one less than it really is.
*/
	rows	= y_res / y_char;
	columns	= (x_res / x_char) - 1;

	if (cur_row >= rows)		cur_row = rows - 1;
	if (cur_column >= columns)	cur_column = columns - 1;

	return (columns);	/* updates kernel folding routines */
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
	CHAR	cursor_info[64];

	CHAR	*startx,*endx;		/* string limits */
	CHAR	*starty,*endy;

	int	x_value,y_value;	/* careful of type here */

/* Fix for type-ahead problem: get_control_seq() stores extra chars
   in a special buffer, and read_console() always checks that buffer.

   Old fix: avoid all reading from the console device [messes up
   type-ahead, and seems sometimes to be itself screwed up by normal 
   user input].  Assume cursor is always in "normal" position prior 
   to special operations (when cursor is read).
*/
	cur_row = rows - 1;	cur_column = 0;
	return (0);		/* STILL FAKE IT; REST IS HANGING */

/* read in string representing cursor position -- format is <row>;<col>R */

	send_control_seq ("6n");
	get_control_seq (&cursor_info, 'R');

/* setup pointers to x and y strings, and make them asciz */

	endy = (char *) strchr (&cursor_info, ';');
	endx = (char *) strchr (&cursor_info, 'R');

	starty = (char *) &cursor_info;		endy[0] = 0;
	startx = (char *) &endy[1];		endx[0] = 0;

/* convert decimal strings to integers, normalize (cur_row in 0 .. rows-1),
   leave results in globals (global type may be different -- INT) */

	stcd_i (starty, &y_value);	cur_row = y_value - 1;
	stcd_i (startx, &x_value);	cur_column = x_value - 1;
}

/*------------------------------*/
/*	write_cursor_pos	*/
/*------------------------------*/

VOID
write_cursor_pos ()	/* set the cursor position */
{
	int	x_new, y_new;		/* correct type for library call */

	CHAR	cursor_info[64];
	CHAR	x_str[32];
	CHAR	y_str[32];

/* normalize co-ords (1-origin), convert integers to strings */

	y_new = cur_row + 1;		/* these are values to set */
	x_new = cur_column + 1;

	stcu_d (&y_str, y_new, 32);
	stcu_d (&x_str, x_new, 32);

/* combine and transmit the strings */

	cursor_info[0] = 0;	/* asciz */

	strcat (&cursor_info, &y_str);
	strcat (&cursor_info, ";");
	strcat (&cursor_info, &x_str);
	strcat (&cursor_info, "H");

	send_control_seq (&cursor_info);
}


/*------------------------------*/
/*	amiga_scroll		*/
/*------------------------------*/

VOID
amiga_scroll ()		/* method depends on cursor position */
{
	CHAR	char_seq[2];

	read_cursor_pos ();	/* get cursor pos into globals */

	if (cur_row < rows - 1)		/* not at bottom of screen? */
		{
		char_seq[0] = 13;
		char_seq[1] = 10;		/* CR, tack on a LF */
		write_console (&char_seq, 2);
		}
	else			/* at bottom, delete split line to scroll */
		{
		cur_row = split_row;	cur_column = 0;
		write_cursor_pos ();

		send_control_seq ("M");

		cur_row = rows - 1;
		write_cursor_pos ();
		}
}

/************************************************************************/
/*	Screen Handling							*/
/************************************************************************/

/*------------------------------*/
/*	clear_eol		*/
/*------------------------------*/
VOID
clear_eol ()
{
	send_control_seq ("K");		/* clear to EOL */
}

/*------------------------------*/
/*	clear_lines		*/
/*------------------------------*/
VOID
clear_lines (row1, row2)	/* EZIP, also CZIP (OPSPLIT) */

INT	row1, row2;
{
	INT	old_row, old_column;
	INT	i;

	read_cursor_pos ();
	old_row = cur_row;	old_column = cur_column;

	for (i=row1; i<row2; i++)
		{
		cur_row = i; 	cur_column = 0;
		write_cursor_pos ();
		send_control_seq ("K");		/* clear to EOL */
		}

	cur_row = old_row;	cur_column = old_column;
	write_cursor_pos ();
}

/*------------------------------*/
/*	clear_screen		*/
/*------------------------------*/
VOID
clear_screen ()
{
	cur_row = 0;	cur_column = 0;
	write_cursor_pos ();

	send_control_seq ("J");		/* clear to end of display */

	cur_row = rows - 1;
	write_cursor_pos ();		/* restore normal cursor position */
}

/*------------------------------*/
/*	line_out		*/
/*------------------------------*/

VOID
line_out (string, len)	/* draw a string at current position, */
			/*   first check length and clip if required */
CHAR	*string;
INT	len;
{
/*	CHAR	old_endmark;
	INT	x_align, y_align;
	INT	clip_len;

	clip_len = columns - cur_column;
	if (len < clip_len)		clip_len = len;

	old_endmark = string[clip_len];
	string[clip_len] = 0;

	x_align = x_char * cur_column;
	y_align = y_char * cur_row;
*/
	write_console (string, len);		/* draw the string */

/*	cur_column = cur_column + clip_len;
	string[clip_len] = old_endmark;
*/

}

/*------------------------------*/
/*	char_out		*/
/*------------------------------*/

VOID
char_out (the_char)	/* if CR or BS, special case, else draw the char */

CHAR	the_char;
{
	CHAR	char_seq[4];

	if (the_char == 13)
		{
		amiga_scroll ();
		}

	else if (the_char == 8)
		{
		char_seq[0] = 8;
		char_seq[1] = 32;	/* erase the current char */
		char_seq[2] = 8;
		write_console (&char_seq, 3);
		}
	else
		{
		char_seq[0] = the_char;
		write_console (&char_seq, 1);
		}
}

/*------------------------------*/
/*	blink_cursor		*/
/*------------------------------*/
VOID 
blink_cursor ()		/* duty cycle is 66% */

{
	/* Amiga -- cursor controlled by console device */
}

/*------------------------------*/
/*	char_in			*/
/*------------------------------*/
CHAR
char_in ()	/* wait for keyboard input */
{
	CHAR	the_char;

	the_char = read_console ();

/* don't echo begin_mark! */	/* NOW FILTERED OUT IN KERNEL */

/*	if (the_char == begin_mark)
		{

		while ((the_char = read_console ()) != end_mark)
			{
			}

		the_char = 'X';
		}
*/

	return (the_char);
}

/************************************************************************/
/*	Special Output Routines						*/
/************************************************************************/

/*------------------------------*/
/*	show_status		*/
/*------------------------------*/
VOID 
show_status (location, loc_len, score, score_len)	/* ZIP only */

CHAR	*location;
CHAR	*score;
INT	loc_len, score_len;
{
#if EZIP
	/* unused in EZIP, retain only the external function def */
#else
	INT	old_row, old_column;
	INT	middle_len, middle_column;	/* blank area in middle */
	INT	i;
	CHAR	blanks[128];

/* store current cursor position, move cursor to home position */

	read_cursor_pos ();
	old_row = cur_row;	old_column = cur_column;
	cur_row = 0;		cur_column = 0;
	write_cursor_pos ();

/* switch temporarily to inverse video */

	highlight (1);

/* draw first string */

	line_out (location, loc_len);

	char_out (32);		/* one padding space */

/* clear the middle area */

	middle_len = columns - loc_len - score_len - 2;

	if (middle_len < 0)	/* strings overlap, backup cursor */
		{
		middle_column = loc_len + 1 + middle_len;

		if (middle_column < 0)
			middle_column = 0;

		cur_row = 0;	cur_column = middle_column;
		write_cursor_pos ();
		}
	else
		{
		highlight (0);		/* middle is normal video */

		for (i=0; i<middle_len; i++)
			blanks[i] = 32;

		line_out (&blanks, middle_len);

		highlight (1);
		}

/* draw second string */

	char_out (32);		/* another padding space */

	line_out (score, score_len);

/* turn off inverse video, also erase possible garbage char in rightmost 
   column (unused column, due to console device auto-scrolling)
*/
	highlight (0);		char_out(32);

/* restore former cursor position */

	cur_row = old_row;	cur_column = old_column;
	write_cursor_pos ();

#endif
}

/*------------------------------*/
/*	show_more		*/
/*------------------------------*/

#define	msg_length	6

VOID 
show_more ()
{
	CHAR	*message;

/* Display a [MORE] message, wait for a key, then back up cursor */

	message = "[MORE]";
	line_out (message, msg_length);

	char_in ();

	send_control_seq ("6D");	/* backup six spaces */

/* Erase the [MORE] message, then back up cursor again */

	message = "      ";
	line_out (message, msg_length);

	send_control_seq ("6D");	/* backup six spaces */
}

/************************************************************************/
/*	File Selection							*/
/************************************************************************/


/*------------------------------*/
/*	file_select		*/
/*------------------------------*/
INT
file_select (opsave, drive)	/* get file and pathspec from user, */
				/*   combine and leave in SAVENAME  */

INT	opsave;		/* zero if restore, otherwise save */
CHAR	drive;		/* drive to use, zero for default */
{
	/* AMIGA LACKS AN OS ROUTINE - IMPLEMENTED IN KERNEL */
}

/*------------------------------*/
/*	new_default		*/
/*------------------------------*/

VOID
new_default (okay)	/* called at end of each SAVE/RESTORE */

BOOL	okay;
{
	if (okay)	/* dest <- source */
		{
		strcpy (&saveback, &savename);	/* update old defaults */
		}
	else
		{
		strcpy (&savename, &saveback);	/* retrieve old defaults */
		}
}

/*------------------------------*/
/*	strip_spaces		*/
/*------------------------------*/

/* Remove any leading and/or trailing spaces from savenames, before
   the file is created.  AmigaDOS permits them, but they lead to confusing
   RESTORE errors.

   Note that we continue to allow embedded spaces.
*/

VOID
strip_spaces (filename)

CHAR	*filename;
{
	CHAR	*leading;
	INT	len;

	leading = filename;
	while (*leading++ == 32)	/* skip over a leading space */
		{
		strcpy (filename, leading);	/* move the string up */
		--leading;
		}

	len = strlen (filename);
	while (len && (filename[len-1] == 32))
		{
		filename[len-1] = 0;	/* chop off any trailing spaces */
		--len;
		}
}

/*------------------------------*/
/*	divide_pathname		*/
/*------------------------------*/

/* Check whether a given filename is prefixed with a drive or directory name.
   If so, return a pointer to the bare filename, otherwise return NULL.

   Path syntax for Amiga is "DFn:PATH1/PATH2/PATHn/filename".
*/

CHAR *
divide_pathname (pathname)

CHAR	*pathname;
{
	CHAR	*special;	/* <:> and </> are the special chars */

/* if one or more directories were given, find the last one */

	special = (CHAR *) strrchr (pathname, '/');

/* otherwise, see if a drive was given */

	if (special == NULL)
		special = (CHAR *) stpchr (pathname, ':');

	if (special != NULL)	/* found one, skip over the delimiter */
		special++;

	return (special);	/* pointer to bare filename, or NULL */
}

/*------------------------------*/
/*	drive_default		*/
/*------------------------------*/

/* If savename lacks a drive/directory spec but saveback includes one 
   (the default), prefix it to savename.

   Also, take this opportunity to remove any leading/trailing spaces from
   savename.
*/

VOID
drive_default ()
{
	CHAR	*barename;
	CHAR	temp[64];

/* check whether savename includes a drive/directory */

	barename = (CHAR *) divide_pathname (&savename);

	if (barename == NULL)		/* Nope */
		{
		strip_spaces (&savename);

/* check whether a default drive/directory exists */

		strcpy (&temp, &saveback);

		barename = (CHAR *) divide_pathname (&temp);

		if (barename != NULL)		/* Yep */
			{
/* and prefix the default drive/directory onto savename */

			*barename = 0;		/* chop off default filename */
			strcat (&temp, &savename);
			strcpy (&savename, &temp);
			}
		}

/* user DID supply a drive/directory, just check for spaces in filename */

	else
		{
		strip_spaces (barename);
		}
}

#if bug_DiskFull	/* BUG GONE, THIS IS DEAD CODE */

/*------------------------------*/
/*	disk_bytes_avail	*/
/*------------------------------*/

LONG
disk_bytes_avail ()	/* returns number of available bytes on save disk, 
			   -1 if error. Must call AFTER save file is created */
{
	LONG	filelock;
	LONG	avail;

	INT	pad1;
struct	InfoData    ZInfoData;		/* information about the save disk */
	INT	pad2;

struct	InfoData    *ZInfoDataPtr;	/* pointer to the above */
	LONG	ptr;


/* Must first obtain a "lock" for a valid file on the desired disk.
   We conveniently use the save file (thus it must already exist). */

	filelock = Lock (&savename, ACCESS_READ);

	if (!filelock)			/* if error, exit now */
		return (-1);

/* The InfoData structure must be longword aligned (for AmigaDOS).  
   Adjust alignment with the pad bytes, if necessary. */

	pad1 = pad2 = 0;	/* make the Compiler stop whining */

	ptr = (LONG) &ZInfoData;
	ptr = ptr>>2;		ptr = ptr<<2;
	ZInfoDataPtr = (struct InfoData *) ptr;

/* Determine how much space remains on the save disk. */

	if (Info (filelock, ZInfoDataPtr))
		{
		avail = 
			(ZInfoDataPtr->id_NumBlocks
			- ZInfoDataPtr->id_NumBlocksUsed)	/* free blks */
			* ZInfoDataPtr->id_BytesPerBlock;
		}
	else	avail = -1;	/* something wrong, fail */

	UnLock (filelock);	/* make sure to undo this lock */

	return (avail);
}

#endif	/* bug_DiskFull */

/************************************************************************/
/*	Disk I/O							*/
/************************************************************************/


/*------------------------------*/
/*	read_file		*/
/*------------------------------*/

LONG
read_file (channel, offset, length, buffer)

LONG	channel;
LONG	offset, length;
CHAR	*buffer;
{
	LONG	former_pos;
	LONG	actual_len;
	LONG	error;

/* disk access speed is increased, if unnecessary seeks are eliminated */

	if ((channel != old_channel) || (offset != old_offset))
		{
		former_pos = Seek (channel, offset, -1);	/* AmigaDOS */
		if (former_pos == -1)
			{
			old_offset = -1;	/* force seek next time */
			return (IoErr ());
			}
		}

	old_channel = channel;
	old_offset = offset + length;

	actual_len = Read (channel, buffer, length);
	if (actual_len == -1)
		{
		old_offset = -1;	/* force seek next time */
		error = IoErr ();
		}

/* Check the number of bytes actually read.  This will detect certain 
   invalid save files (for example, files where Amiga crashed during their
   creation, for whatever reason).

   We allow for a special case of actual length being 256 less than expected.
   The kernel does paging in 512 byte blocks, while the Amiga TFTP utility 
   transfers data files in 256 byte blocks.
*/
	else if ((actual_len == length) || (actual_len == (length - 256)))

		error = 0;		/* zero means no error */

	else
		{
		old_offset = -1;
		error = 1;		/* length error */
		}

	return (error);
}


/*------------------------------*/
/*	write_file		*/
/*------------------------------*/

LONG
write_file (channel, offset, length, buffer)

LONG	channel;
LONG	offset, length;
CHAR	*buffer;
{
	LONG	error;

/* Errors: zero means none, 999 means disk full, otherwise IOErr () */

#if bug_DiskFull

	if (disk_newsave)		/* FALSE if writing over old file */
		{
		disk_free = disk_free - length;
		if (disk_free < 0)
			return (999);		/* not enough room for write */
		}
#endif
	error = Seek (channel, offset, -1);
	if (error == -1)
		return IoErr ();

	error = Write (channel, buffer, length);
	if (error == -1)
		return IoErr ();

	else if (error != length)	/* (should have been caught above) */
		return (999);

	else	return (0);		/* no error */
}


/*------------------------------*/
/*	create_file		*/
/*------------------------------*/

LONG
create_file ()		/* create (if needed), and open, a SAVE file */
{
/* two ways to enter this routine:  if disk_newsave is TRUE, we are creating
   a new save file, otherwise we are writing over an old one */

	temp_channel = Open (&savename, MODE_NEWFILE);

#if bug_DiskFull	/* avoid a write error */

	if  ((temp_channel) && (disk_newsave))
		{
/* we just created the file, but must close it momentarily to get disk info.
   Otherwise the Lock call fails */

		Close (temp_channel);

/* determine how much free space is on the disk, so file_write can fail
   gracefully if necessary */

		disk_free = disk_bytes_avail ();

		temp_channel = Open (&savename, MODE_NEWFILE);
		}
#endif	/* bug_DiskFull */

	if (!temp_channel)
		return (IoErr ());
	else
		return (0);		/* zero means OK */
}


/*------------------------------*/
/*	open_file		*/
/*------------------------------*/

LONG
open_file ()		/* open an file to RESTORE from */
{
	temp_channel = Open (&savename, MODE_OLDFILE);

	if (!temp_channel)
		return (IoErr ());
	else
		return (0);		/* zero means OK */
}

/*------------------------------*/
/*	close_file		*/
/*------------------------------*/

LONG
close_file (channel)

LONG	channel;
{
	Close (channel);

	return (0);		/* no error codes? */
}


/*------------------------------*/
/*	delete_file		*/
/*------------------------------*/

LONG
delete_file ()
{
	LONG	success;	/* actually BOOLEAN */

	success = DeleteFile (&savename);

	if (success)
		return (0);
	else	return (1);
}


/*------------------------------*/
/*	exist_file		*/
/*------------------------------*/

LONG
exist_file ()		/* called before every SAVE */
{
	LONG	filelock;
	LONG	exists;

	filelock = Lock (&savename, ACCESS_WRITE);
	exists = filelock;		/* non-zero if already exists */

	if (exists)
		{
		disk_newsave = FALSE;
		UnLock (filelock);	/* if does exist, undo the lock */
		}
	else	disk_newsave = TRUE;	/* set the global */

	return (exists);
}

/*------------------------------*/
/*	open_game		*/
/*------------------------------*/

LONG
open_game()
{
	CHAR	*gamename;

	gamename = "Story.Data";	/* always call it this */

	temp_channel = Open (gamename, MODE_OLDFILE);

	if (temp_channel == 0)
		return (IoErr ());
	else	return (0);		/* zero means OK */
}

/*------------------------------*/
/*	close_game		*/
/*------------------------------*/

LONG
close_game (channel)

LONG	channel;
{
	Close (channel);

	return (0);		/* no errors in AmigaDOS ? */
}


/*------------------------------*/
/*	make_icon_file		*/
/*------------------------------*/

LONG
make_icon_file ()	/* create a new save file icon, */
			/*   called only after a successful SAVE */
{
	LONG	error;
	LONG	object;
/*	INT	len;	*/

/* diskobj (should have been) read into memory during init */

	if (diskobj == NULL)
		error = 1;

	else
		{

#if bug_DiskFull

/* PROBLEM: PutDiskObj is crashing the machine when a disk-full error occurs.
     First, make sure we have enough room for a new icon file.
*/

#define     MaxIconSize     1024	/* actually ours is under 400 */

		if (disk_newsave)	/* if OLD save, space not an issue */
			{
			disk_free = disk_bytes_avail ();
			if (disk_free < MaxIconSize)	return (1);
			}

/* (Alternate fix.  The preceeding code fixes this problem better.)

   Do some preliminary error checking:  Attempt to create a file with the 
   desired name, and put 1K of random data into it.
*/
		strcat (&savename, ".info");	/* desired name */
		error = create_file ();		/* --> temp_channel */

		if (!error)
			{
			error = write_file (temp_channel, 0, 1024, &savename);

			close_file (temp_channel);	/* (no error codes) */

			if (!error)
				error = delete_file ();    /* <-- savename */
			else		delete_file ();
			}

		len = strlen (&savename);
		savename[len-5] = 0;		/* chop off the ".info" */

		if (error)			/* if ANY error, fail now */
			return (error);

#endif	/* bug_diskfull */


/* Everything seems OK, go ahead and make the Save icon */

		diskobj->do_CurrentX = NO_ICON_POSITION;
		diskobj->do_CurrentY = NO_ICON_POSITION;

	/* write icon file via same path as save file */

		object = PutDiskObject (&savename, diskobj);

		if (object == NULL)
			error = IoErr ();
		else	error = 0;
		}

	return (error);
}

/************************************************************************/
/*	Scripting							*/
/************************************************************************/

/*------------------------------*/
/*	raw_script		*/
/*------------------------------*/

INT
raw_script (buffer, length)	/* send data to the printer device */

LONG	buffer, length;
{
	INT	error;
	LONG	seconds1, micros1;
	LONG	seconds2, micros2;
	LONG	actual_len;

/* A problem:  AmigaDOS (rev 1.0) isn't passing back printer error codes.
   Each attempt to print when the printer isn't ready causes a 30 second
   timeout.  A series of these effectively hangs the game.

   To fix, use the delay to infer an error.  If no error code is returned
   but the delay exceeds 30 seconds, consider the printer not ready.
*/

	CurrentTime (&seconds1, &micros1);	/* starting time */

	actual_len = Write (print_file, buffer, length);
	if (actual_len == length)
		error = 0;
	else	error = 1;

/*	SIOStdReq.io_Data = (APTR) buffer;
	SIOStdReq.io_Length = length;

	SIOStdReq.io_Command = CMD_WRITE;
	DoIO (&SIOStdReq);
	error = (SIOStdReq.io_Error);
*/
	CurrentTime (&seconds2, &micros2);

	if (!error)	/* says no error, but is it lying? */
		{
		if ((seconds2 - seconds1) >= (30 - 1))
			error = 99;
		}

	return (error);			/* error code, zero if OK */
}

/*------------------------------*/
/*	script_init		*/
/*------------------------------*/

/*
  Two approaches to printing were tried; neither returns errors.  The hairy 
  one crashes (CloseDevice) when multi-tasked, so use the simple one.
*/

LONG
script_init (open)	/* initialize/uninitialize the printer */

INT	open;
{
	LONG	error;

	if (open)
		{
		print_file = Open ("PRT:", MODE_OLDFILE);

		if (print_file)		/* zero means error */
			error = 0;
		else	error = 1;

/*		OpenDevice ("printer.device", 0, &SIOStdReq, 0);
*/
	/* set up the message port in the I/O request */

/*		SMsgPort.mp_Node.ln_Type	= NT_MSGPORT;
		SMsgPort.mp_Flags		= 0;
		SMsgPort.mp_SigBit		= AllocSignal (-1);
		SMsgPort.mp_SigTask	= (struct Task *) FindTask (NULL);

		AddPort (&SMsgPort);
		SIOStdReq.io_Message.mn_ReplyPort = &SMsgPort;
*/		}

	else
		{
		if (print_file)			/* call only if once opened */
			Close (print_file);

		error = 0;			/* no close errors? */

	/* clean up the various structures we created */

/*		RemPort (&SMsgPort);

		FreeSignal (SMsgPort.mp_SigBit);

		CloseDevice (&SIOStdReq);
*/		}

	return (error);		/* zero if none */
}

/*------------------------------*/
/*	script_open		*/
/*------------------------------*/

INT
script_open (start)	/* kernel issued request to start/stop scripting,
			   return status, either active or inactive  */
INT	start;
{
	CHAR	reset_seq[2];
	LONG	error;

/* starting, may need to open/reset the printer */

	if (start)
		{
		if (!prt_inited)	/* device never opened, do so now */
			{
			error = script_init (TRUE);

			if (!error)
				prt_inited = TRUE;	/* okay, opened */
			}

		if (prt_inited && !prt_active)	/* new start, reset printer */
			{

			reset_seq[0] = '\33';	/* ESC ($1B) */
			reset_seq[1] = 'c';	/* 'c' means reset */

			error = raw_script (&reset_seq, 2);

			if (!error)
				prt_active = TRUE;	/* okay, active */
			}
		}

	else			/* if stop, flag it and shut down */
		{
	 	prt_active = FALSE;

		if (prt_inited)
			{
			script_init (FALSE);	prt_inited = FALSE;
			}
		}

	return (prt_active);
}

/*------------------------------*/
/*	script_line		*/
/*------------------------------*/

INT
script_line (buffer, length)	/* script a line, tack on an EOL,
				     return error code (zero if none) */
LONG	buffer, length;
{
	CHAR	eol_seq[2];
	INT	error;

	if (length > 0)
		error = raw_script (buffer, length);
	else	error = 0;

/* The EOL seq used to be an "ISO standard" Esc-E (1B45), but under
   AmigaDOS 1.2 this seems to have no effect.  Changing to CRLF (0D0A)
   works under both 1.1 and 1.2 except that the spacing is too wide 
   (resembles triple, used to resemble double).  It appears that AmigaDOS 
   (/not/ the printer) automatically makes the initial CR a CRLF.  So,
   we will output only the CR and hope all printers are happy.

   By the way, a way to check script spacing is to compare it to what
   AmigaDOS does after typing "TYPE textfile TO PRT:".  Also, some
   printer behavior can be controlled from User Preferences, like
   margins and 6- or 8-line spacing.
*/
	if (!error)
		{
		eol_seq[0] = 0x0D;
/**		eol_seq[1] = 0x0A;	**/

		error = raw_script (&eol_seq, 1);
		}
	return (error);
}

/************************************************************************/
/*	Memory Management						*/
/************************************************************************/

/* memory fudge factor -- leave this much free for transient use,
   unless over-ridden by user.  Resizing of windows especially triggers
   large dynamic allocation requests.  */

#if EZIP	/* must be < 64K for 256K machine to load EZIP at all */
#define	mem_system	32*1024		/* enough for one other window */

#else
#define	mem_system	64*1024		/* enough for several windows */
#endif

#define	mem_default    -1	/* user didn't specify memory use */

/*------------------------------*/
/*	c_maxmem		*/
/*------------------------------*/

LONG
c_maxmem (game_total)	/* return size of available memory pool */
			/*  may be "doctored" (up or down) upon user request */

LONG	game_total;	/* maximum (buffered) memory we can use */
{
	LONG	game_used;
	LONG	mem_avail, mem_return;

	CHAR	message[64];
	LONG	len;

	mem_avail = (AvailMem (MEMF_PUBLIC));

	if (mem_freereq == mem_default)
		mem_return = mem_avail - mem_system;	/* adjust it */
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

		cur_row = 0;	cur_column = 0;
		write_cursor_pos ();		/* top line for special msg */

		len = strlen (&message);
		line_out (&message, len);

		cur_row = rows - 1;		/* reset cursor */
		write_cursor_pos ();
		}

	return (mem_return);
}

/*------------------------------*/
/*	c_getmem		*/
/*------------------------------*/

LONG
c_getmem (nbytes)

LONG	nbytes;
{
/*	return (AllocMem (nbytes, MEMF_PUBLIC));	*/ /* zero if failed */

/* This routine calls the AllocMem function, and also allocates a LINK NODE,
   which saves the data needed for automatic de-allocation when we terminate.
*/

	return (AllocRemember (&RememberKey, nbytes, MEMF_PUBLIC));
}

/************************************************************************/
/*	Initializations							*/
/************************************************************************/

#if EZIP

#define	    Peek1	(8+3)	/* ZScreen scanlines showing behind ZWindow */
#define	    Peek2	(8+5)	/* WBScreen scanlines showing behind ZScreen */

/* The first margin is needed for the ZScreen slider and show/hide boxes.
   Without these functions, task switching is crippled.

   The second margin is needed because of an Intuition problem that causes
   all system alert boxes (e.g. insert disk X, disk X full, etc) to appear in 
   the WB screen, normally hidden by EZIP.  This space permits the words
   "System Alert" to show through to the user.
*/

#else

#define	    Peek1	0
#define	    Peek2	0

#endif

/*------------------------------*/
/*	window_init		*/
/*------------------------------*/
INT
window_init (open)

BOOL	open;		/* close if FALSE */
{

#if EZIP
struct	NewScreen	NewZScreen;
#endif

struct	NewWindow	NewZWindow;

	if (open)
		{
		
#if EZIP    /* first create a custom screen */

		ZTextAttr.ta_Name	= "topaz.font";
		ZTextAttr.ta_YSize	= TOPAZ_EIGHTY;		/* EZIP */
		ZTextAttr.ta_Style	= FS_NORMAL;
		ZTextAttr.ta_Flags	= FPF_ROMFONT;

		NewZScreen.LeftEdge	= 0;		/* required by Amiga */
		NewZScreen.Width	= 640;		/* high-res */

		NewZScreen.TopEdge	= 0 + Peek2;
		NewZScreen.Height	= 200 - Peek2;
		NewZScreen.Depth	= 1;		/* 2 colors only */

		NewZScreen.DetailPen	= 0;
		NewZScreen.BlockPen	= 1;
		NewZScreen.ViewModes	= HIRES;

		NewZScreen.Type		= CUSTOMSCREEN;
		NewZScreen.Font		= &ZTextAttr;
		NewZScreen.DefaultTitle = NULL;
		NewZScreen.Gadgets	= NULL;
		NewZScreen.CustomBitMap = NULL;

		ZScreen = (struct Screen *) OpenScreen (&NewZScreen);

		if (ZScreen == NULL)
			return (1);	/* nonzero means error, exit */
#endif
	/* create a window */

		NewZWindow.LeftEdge	= 0;	/* full width initially */
		NewZWindow.Width	= 640;

		NewZWindow.TopEdge	= 0 + Peek1;		/* (careful) */
		NewZWindow.Height 	= 200 - Peek2 - Peek1;

		NewZWindow.DetailPen 	= 0;
		NewZWindow.BlockPen	= 1;
		NewZWindow.Title	= NULL;
#if EZIP
		NewZWindow.Flags	=
		  BORDERLESS | SMART_REFRESH | ACTIVATE |
		  NOCAREREFRESH;

		NewZWindow.Type 	= CUSTOMSCREEN;
		NewZWindow.Screen	= ZScreen;
#else
		NewZWindow.Flags	=
		  WINDOWSIZING | WINDOWDRAG | WINDOWDEPTH | 
		  SMART_REFRESH | ACTIVATE | NOCAREREFRESH;

		NewZWindow.Type 	= WBENCHSCREEN;
		NewZWindow.Screen	= NULL;
#endif

		NewZWindow.IDCMPFlags 	= NULL;
		NewZWindow.FirstGadget	= NULL;
		NewZWindow.CheckMark	= NULL;
		NewZWindow.BitMap	= NULL;

#if EZIP
	/* sizing ignored in EZIP */
#else
		NewZWindow.MinWidth	= 200;
		NewZWindow.MinHeight	= 50;  /* 4 lines for MORE algorithm */

		NewZWindow.MaxWidth	= 640;
		NewZWindow.MaxHeight	= 200 - Peek2 - Peek1;
#endif
		ZWindow = (struct Window *) OpenWindow (&NewZWindow);

		if (ZWindow == NULL)
			return (2);		/* nonzero means error, exit */
		}

	else		/* closing, clean up */
		{
		if (ZWindow)
			CloseWindow (ZWindow);
#if EZIP
		if (ZScreen)
			CloseScreen (ZScreen);
#endif
		}

	return (0);
}

/*------------------------------*/
/*	z_init			*/
/*------------------------------*/
INT
z_init ()
{
	INT	error;

	CHAR	cur_prefs[1];
	CHAR	old_YSize;

/* open the Intuition Library */

	IntuitionBase = (struct IntuitionBase *)
		OpenLibrary ("intuition.library", 0);

	if (IntuitionBase == 0)
		return (103);

/* close the WorkBench screen, to free up memory */

#if EZIP			/* (ZIP uses it, can't close it) */
/*	CloseWorkBench ();	*/
#endif

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

/* create and open a game window (and custom screen, for EZIP) */

	error = window_init (TRUE);
	if (error)
		return (104);

/* create a console device, attaching it to our window */

	ZIOStdReq.io_Data = (CHAR *) ZWindow;

	if (OpenDevice ("console.device", 0, &ZIOStdReq, 0) != 0)
		return (105);

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

/* set up the message port in the I/O request */

	ZMsgPort.mp_Node.ln_Type	= NT_MSGPORT;
	ZMsgPort.mp_Flags		= 0;
	ZMsgPort.mp_SigBit		= AllocSignal (-1);
	ZMsgPort.mp_SigTask		= (struct Task *) FindTask (NULL);

	AddPort (&ZMsgPort);
	ZIOStdReq.io_Message.mn_ReplyPort = &ZMsgPort;

/* and start reading ... */

	ZRDStdReq = ZIOStdReq;	/* copy the whole structure */
	queue_read ();

/* etcetera */

	split_row = 1;		/* default for ZIP */

	cur_row = rows - 1;	cur_column = 0;
	write_cursor_pos ();

	strcpy (saveback, "Story.Save");	/* initial default */

	RememberKey = NULL;	/* set up for memory allocation */

/* Get the Save Icon info during init.  This avoids having to access the game
   disk later, when it may have been removed from the drive */

	IconBase = (struct IconBase *) OpenLibrary ("icon.library", 0);

	if (IconBase == NULL)
		diskobj = NULL;
	else
		{ 
		/* if info file not found, just return diskobj NULL */

		diskobj = (struct DiskObject *) GetDiskObject ("Icon.Data");
		}

#if EZIP

/* Prevent a bug that puts garbage in the input line.  Switching between 
   screens (EZIP and WB) seems to trigger unrequested "Raw Input Event" 
   reports of event types 17/18 (undocumented types).  

   Disk event messages (type 16) were also appearing infrequently.
*/

send_control_seq ("17}");	/* "reset raw events" */
send_control_seq ("18}");
send_control_seq ("16}");

#endif
	return (0);
}	

/*------------------------------*/
/*	z_exit			*/
/*------------------------------*/

/* Clean up everything, in reverse order of creation.  Any memory allocated
   but not freed will be lost to eternity.
*/

VOID
z_exit ()
{

/* be sure to shut down scripting, if indeed no one else did */

	if (prt_inited)
		script_init (FALSE);

/* free the save file icon stuff, if it existed */

	if (diskobj != NULL)
		FreeDiskObject (diskobj);

	if (IconBase != NULL)
		CloseLibrary (IconBase);

/* de-allocate all memory claimed by 68K kernel initializations */

	if (RememberKey != NULL)
		FreeRemember (&RememberKey, TRUE);

/* clean up the various console device structures */

	RemPort (&ZMsgPort);

	FreeSignal (ZMsgPort.mp_SigBit);

	CloseDevice (&ZIOStdReq);

/* close our window (and screen, for EZIP) */

	window_init (FALSE);

/* try to re-open WorkBench, if we closed it */

#if EZIP
/*	OpenWorkBench ();	*/
#endif

/* lastly close the Intuition library */

	CloseLibrary (IntuitionBase);
}

/************************************************************************/
/*	Diagnostics (Temporary)						*/
/************************************************************************/

#if ZDEBUG

/*------------------------------*/
/*	z_test			*/
/*------------------------------*/

z_test()
{
	CHAR	mychr;
	CHAR	mystr[32];

	c_maxmem ();	c_getmem (16);	/* test memory functions */
	c_getmem (16);	c_maxmem ();

	FOREVER		/* do input, output, scroll, until 'quit' */
	{
		mychr = char_in ();

		if (mychr == 'q') break;	/* leave the loop */

		else if (mychr == 'z')		/* print a string */
		{
		        strcpy (mystr, "xyz");
			line_out (mystr, strlen (mystr));

			char_out (13);
		}
		else char_out (mychr);		/* echo the char */
	}
}
#endif

/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	main			*/
/*------------------------------*/

INT
main (argc, argv)

LONG	argc;
CHAR	*argv[];	/* an array of pointers */
{
	INT	error;
	CHAR	*arg1;
	CHAR	char1, char2;

/* The only valid command line argument is F/n, where n is the minimum
   number of bytes to leave in the free memory pool.

   If the argument is missing, or not a number, or if the game is run from
   WorkBench, leave the default (mem_system, defined in c_maxmem).

   The game takes as much memory as it can use, within the above limit.
*/

	mem_freereq = mem_default;		/* assume no memory arg */

	if ((argc > 1) && (argv != 0))		/* got one */
		{
		arg1 = (CHAR *) argv[1];

		char1 = *arg1++;
		char2 = *arg1++;

	/* string should start with "F/" */

		if (
		   ((char1 == 'F') || (char1 == 'f')) 
				   && (char2 == '/')
		   )

			{
		/* okay, convert string to a number */

			if (stcd_i (arg1, &mem_freereq ) == 0)

		/* error, it's not a number */

				mem_freereq = mem_default;
			}
		}

/* set up the world */

	error = z_init ();	/* returns zero if OK */

	if (!error)
		{
#if ZDEBUG
		z_test ();		/* quick I/O test */
#endif
		START ();		/* enter the ZIP kernel */
		}

	z_exit ();		/* clean up the world */

	return (error);		/* fall back into controlling process */
}

