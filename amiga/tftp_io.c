
/*------------------------------------------------------*/
/*	AMIGA GENERIC INTERFACE	 (TFTP)			*/
/*------------------------------------------------------*/

#include "exec\types.h"
#include "exec\exec.h"

#include "libraries\dos.h"
#include "libraries\dosextens.h"

#include "intuition\intuition.h"	/* for AllocRemember */
#include "intuition\intuibase.h"

#include "devices\serial.h"		/* for serial port I/O */

#define CHAR	unsigned char
#define	INT	short		/* 16 bits for 68K compatibility */
#define LONG	long

#define VOID	int
#define GLOBAL	/**/

#define FOREVER	for(;;)


/************************************************************************/
/*	Global Variables						*/
/************************************************************************/

struct	IOExtSer    XIOExtSer;		/* for serial device, IOStdReq+ */
struct	MsgPort     XMsgPort;

	LONG	con_file;		/* console file refnum */
/*	LONG	ser_file;	*/	/* serial file refnum */

/* This slot returns file refnums to caller (function returns an error code).
     Better solution -- return refnum through a VAR type parameter */

	LONG	temp_channel;

	INT	columns, cur_column;	/* cursor info */
	INT	rows, cur_row;		/* cur_row in 0 .. rows-1 */

	INT	split_row;		/* first row in scrolling area */

	CHAR	savename[64];		/* buffer for file_select dialog */
	CHAR	saveback[64];		/* buffer for default name */

/* These vars increase the speed of disk reads */

	LONG	old_channel = -1;
	LONG	old_offset = -1;

struct	IntuitionBase	*IntuitionBase;
struct	Remember	*RememberKey;

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
	Write (con_file, string, len);		/* AmigaDOS write */
}

/*------------------------------*/
/*	read_console		*/
/*------------------------------*/
CHAR
read_console ()			/* get a char from console device */
{
	CHAR	the_char;

/* RAW read always completes after a single character */

	Read (con_file, &the_char, 1);
	return (the_char);
}

/*------------------------------*/
/*	send_control_seq	*/
/*------------------------------*/

#define	begin_mark	'\233'		/* 0x9B (one byte only) */

VOID
send_control_seq (string)	/* write it, prefixed with control char */

CHAR	*string;
{
	CHAR	temp;

	temp = begin_mark;

	write_console (&temp, 1);
	write_console (string, strlen (string));
}

/*------------------------------*/
/*	get_control_seq		*/	/* read chars into buffer */
/*------------------------------*/

VOID
get_control_seq (buffer, end_mark)

CHAR	*buffer;
CHAR	end_mark;
{
	CHAR	the_char;
	INT	i;

	while ((the_char = read_console ()) != begin_mark)
		{
			/* ignore any prior chars, also starting mark */
		}

	i = 0;
	while ((the_char = read_console ()) != end_mark)
		{
		buffer[i] = the_char;
		i++;
		}

	buffer[i] = end_mark;	/* end with the ending mark */
	buffer[i+1] = 0;	/* make it ASCIZ */
}

/************************************************************************/
/*	Low Level Functions						*/
/************************************************************************/



/************************************************************************/
/*	Cursor Positioning						*/
/************************************************************************/

/* ALL absolute cursor positioning should go through these two routines */

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

/* read in string representing cursor position -- format is <row>;<col>R */

	send_control_seq ("6n");
	get_control_seq (&cursor_info, 'R');

/* setup pointers to x and y strings, and make them asciz */

	endy = (CHAR *) strchr (&cursor_info, ';');
	endx = (CHAR *) strchr (&cursor_info, 'R');

	starty = (CHAR *) &cursor_info;		endy[0] = 0;
	startx = endy + 1;			endx[0] = 0;

/* convert decimal strings to integers, 
   normalize (cur_row in 0 .. rows - 1),
   leave results in globals (global type may be different) */

	stcd_i (starty, &y_value);	cur_row = y_value - 1;
	stcd_i (startx, &x_value);	cur_column = x_value - 1;
}

/*------------------------------*/
/*	write_cursor_pos	*/
/*------------------------------*/

VOID
write_cursor_pos (x_new, y_new)		/* reset the cursor position */

INT	x_new,y_new;
{
	CHAR	cursor_info[64];
	CHAR	x_str[32];
	CHAR	y_str[32];

/* normalize co-ords (y_new in 0 .. rows - 1), convert integers to strings */

	stcu_d (&x_str, x_new + 1, 32);
	stcu_d (&y_str, y_new + 1, 32);

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

	if (cur_row < rows)	/* not at bottom of screen? */
		{
		char_seq[0] = 13;
		char_seq[1] = 10;		/* CR, tack on a LF */
		write_console (&char_seq, 2);
		}
	else			/* at bottom, delete top line to scroll */
		{
		write_cursor_pos (0, split_row);
		send_control_seq ("M");
		write_cursor_pos (0, cur_row);
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
/*	clear_screen		*/
/*------------------------------*/
VOID
clear_screen ()
{
	write_cursor_pos (0,0);

	send_control_seq ("J");

	write_cursor_pos (0,1);		/* move cursor to second row */
}

/*------------------------------*/
/*	line_out		*/
/*------------------------------*/

VOID
line_out (string, len)		/* draw a string at current position, */

CHAR	*string;
INT	len;
{
	write_console (string, len);
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

	else	write_console (&the_char, 1);
}

/*------------------------------*/
/*	char_in			*/
/*------------------------------*/
CHAR
char_in ()	/* wait for keyboard input */
{
	CHAR	the_char;

	the_char = read_console ();

	if (the_char == begin_mark)
		{
		the_char = 'X';		/* don't echo begin_mark! */
		}

	return (the_char);
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


}

/*------------------------------*/
/*	new_default		*/
/*------------------------------*/

VOID
new_default (okay)	/* called at end of each SAVE/RESTORE */

INT	okay;
{
	if (okay)	/* dest <- source */
		{
		strcpy (saveback, savename);	/* update old defaults */
		}
	else
		{
		strcpy (savename, saveback);	/* retrieve old defaults */
		}
}

/************************************************************************/
/*	Serial Port I/O							*/
/************************************************************************/

/*------------------------------*/
/*	init_serial		*/
/*------------------------------*/

VOID
init_serial (open)	/* initialize/uninitialize the serial port */

INT	open;
{
	if (open)
		{
		if (OpenDevice ("serial.device", 0, &XIOExtSer, 0) != 0)
			{
				/* error */
			}
	/* set up the message port in the I/O request */

		XMsgPort.mp_Node.ln_Type	= NT_MSGPORT;
		XMsgPort.mp_Flags		= 0;
		XMsgPort.mp_SigBit		= AllocSignal (-1);
		XMsgPort.mp_SigTask	= (struct Task *) FindTask (NULL);

		AddPort (&XMsgPort);
		XIOExtSer.IOSer.io_Message.mn_ReplyPort = &XMsgPort;

	/* configure the port to read 8 data bits */
		XIOExtSer.io_ReadLen 		= 8;

	/* disable xON-xOFF  (causes loss of valid data) */
		XIOExtSer.io_SerFlags 		|= SERF_XDISABLED;

/*		XIOExtSer.io_WBufLen		= 2;	(was zero)  */
		XIOExtSer.io_BrkTime		= (30 * 1000 * 1000);

		XIOExtSer.IOSer.io_Command	= SDCMD_SETPARAMS;
		DoIO (&XIOExtSer);
		}

	else		/* clean up the various structures we created */
		{
		RemPort (&XMsgPort);

		FreeSignal (XMsgPort.mp_SigBit);

		CloseDevice (&XIOExtSer);
		}
}

/*------------------------------*/
/*	read_serial		*/
/*------------------------------*/

LONG
read_serial (buffer, length)

CHAR	*buffer;
LONG	length;
{
/*	LONG	result;

	result = Read (ser_file, buffer, length);

	if (result < 0)
		return (result);
	else	return (0);
*/
	XIOExtSer.IOSer.io_Data = (APTR) buffer;
	XIOExtSer.IOSer.io_Length = length;

	XIOExtSer.IOSer.io_Command = CMD_READ;
	DoIO (&XIOExtSer);

/*	SendIO (&XIOExtSer);	*/	/* due to bug in DoIO */
}

/*------------------------------*/
/*	write_serial		*/
/*------------------------------*/

LONG
write_serial (buffer, length)

CHAR	*buffer;
LONG	length;
{
/*	if (Write (ser_file, buffer, length) == -1)
		return IoErr ();
	else	return (0);
*/
	XIOExtSer.IOSer.io_Data = (APTR) buffer;
	XIOExtSer.IOSer.io_Length = length;

	XIOExtSer.IOSer.io_Command = CMD_WRITE;
	DoIO (&XIOExtSer);
}



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

/* disk access speed is increased, if unnecessary seeks are eliminated */

	if (! ((channel == old_channel) & (offset == old_offset)))
		{
		if (Seek (channel, offset, -1) == -1)	/* AmigaDOS */
			{
			old_offset = -1;	/* force seek next time */
			return IoErr ();
			}
		}

	old_channel = channel;
	old_offset = offset + length;

	if (Read (channel, buffer, length) == -1)
		return IoErr ();
	else	return (0);		/* zero means no error */
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
	if (Seek (channel, offset, -1) == -1)	/* AmigaDOS */
		return IoErr ();

	if (Write (channel, buffer, length) == -1)
		return IoErr ();
	else	return (0);		/* zero means no error */
}


/*------------------------------*/
/*	create_file		*/
/*------------------------------*/

LONG
create_file ()		/* create AND OPEN a new file,  */
			/*   leave channel in temporary var */
{
	temp_channel = Open (savename, MODE_NEWFILE);

	if (temp_channel == 0)
		return (IoErr ());
	else	return (0);		/* zero means OK */
}


/*------------------------------*/
/*	open_file		*/
/*------------------------------*/

LONG
open_file ()		/* open an existing file, return channel */
{
	temp_channel = Open (savename, MODE_OLDFILE);

	if (temp_channel == 0)
		return (IoErr ());
	else	return (0);		/* zero means OK */
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

	success = DeleteFile (savename);

	if (success)
		return (0);
	else	return (1);
}


/*------------------------------*/
/*	exist_file		*/
/*------------------------------*/

LONG
exist_file ()
{
	LONG	filelock;

	filelock = Lock (savename, ACCESS_WRITE);

	if (filelock == 0)	/* file does not exist */
		return (0);

	UnLock (filelock);	/* exists, undo lock */

	return (1);		/* and indicate duplicate file */
}

/*------------------------------*/
/*	open_game		*/
/*------------------------------*/

LONG
open_game (gamename, channel)

CHAR	*gamename;
LONG	*channel;	/* return file refnum through this ptr */
{
	*channel = Open (gamename, MODE_NEWFILE);

	if (*channel == 0)
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
/*	write_game		*/
/*------------------------------*/

LONG
write_game (channel, length, buffer)	/* no Seeking for TFTP */

LONG	channel;
LONG	length;
CHAR	*buffer;
{
/*	if (Seek (channel, offset, -1) == -1)
		return IoErr ();
*/
	if (Write (channel, buffer, length) != length)
		return IoErr ();
	else	return (0);		/* zero means no error */
}



/************************************************************************/
/*	Memory Management						*/
/************************************************************************/

/*------------------------------*/
/*	c_maxmem		*/
/*------------------------------*/

LONG
c_maxmem ()	/* return size of available memory pool */
{
	return (AvailMem (MEMF_PUBLIC));
}

/*------------------------------*/
/*	c_getmem		*/
/*------------------------------*/

LONG
c_getmem (nbytes)

LONG	nbytes;
{
/*	return (AllocMem (nbytes, MEMF_PUBLIC));	*/ /* zero if failed */

/* This routine calls the AllocMem function, but also allocates a LINK NODE
   and saves the data needed for automatic de-allocation when the program ends.
*/

	return (AllocRemember (&RememberKey, nbytes, MEMF_PUBLIC));
}

/************************************************************************/
/*	Initializations							*/
/************************************************************************/

/*------------------------------*/
/*	z_init			*/
/*------------------------------*/
INT
z_init ()
{
	CHAR *name;

/* open the console device */

	name = "raw:0/0/640/200/";	/* untitled */

	if ((con_file = Open (name, MODE_NEWFILE )) == 0)
		{
		return (101);
		}
   
/* and the serial device */

/*	ser_file = Open ("SER:", MODE_OLDFILE);	*/

/* and the Intuition Library */

	IntuitionBase = (struct IntuitionBase *)
		OpenLibrary ("intuition.library", 0);

	if (IntuitionBase == 0)
		return (103);

	RememberKey = 0;	/* set up for memory allocation */

	strcpy (saveback, "Story.Save");	/* initial default */

	rows = 100;	/* force normal scrolling always */

	return (0);
}	

/*------------------------------*/
/*	z_exit			*/
/*------------------------------*/

VOID
z_exit ()
{

/* De-allocate the various pieces of memory we allocated */

	FreeRemember (&RememberKey, TRUE);

	CloseLibrary (IntuitionBase);

/*	Close (ser_file);	*/
	Close (con_file);
}

/************************************************************************/
/*	Diagnostics (Temporary)						*/
/************************************************************************/

/*------------------------------*/
/*	z_test			*/
/*------------------------------*/

z_test()
{
	CHAR	mychr;

	c_maxmem ();	c_getmem (16);	/* test memory functions */
	c_getmem (16);	c_maxmem ();

	FOREVER		/* do input, output, scroll, until 'quit' */
	{
		mychr = char_in ();

		if (mychr == 'q') break;	/* leave the loop */

		else if (mychr == 'z')		/* print a string */
			{
			line_out ("xyz", 3);
			char_out (13);
			}

		else char_out (mychr);		/* echo the char */
	}
}

/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	dummy_main		*/
/*------------------------------*/
INT
dummy_main ()
{
	INT	error;

	error = z_init ();		/* zero if OK */

/*	if (!error)		*/
/*		z_test ();	*/
/*		START ();	*/

	z_exit ();			/* clean up */

	return (error);
}

