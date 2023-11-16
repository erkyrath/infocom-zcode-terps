
/*------------------------------------------------------*/
/*	File transfer program using TFTP format		*/
/*------------------------------------------------------*/

#define CHAR	unsigned char
#define	INT	short		/* 16 bits for 68K compatibility */
#define LONG	long

#define BOOL	short		/* 1 or 0 */
#define TRUE	1
#define FALSE	0

#define VOID	int
#define GLOBAL	/**/

#define FOREVER	for(;;)

#define SHOW (val, fmt) printf ("%s=fmt\n", "val", val)

/*  SHOW (ch, %02x);	*/	/* prints "ch = [hex value of ch]" */

/************************************************************************/
/*	Global Variables						*/
/************************************************************************/

	LONG	game_channel;	/* refnum */

	unsigned short		checksum1;	/* given */
	unsigned short		checksum2;	/* computed */

/************************************************************************/
/*	Error Handling							*/
/************************************************************************/

/*------------------------------*/
/*	show_msg		*/
/*------------------------------*/
VOID
show_msg (str)	/* write to console window */

CHAR	*str;
{
	line_out (str, strlen (str));
}

VOID
show_msgCR (str)	/* adding a CR */

CHAR	*str;
{
	line_out (str, strlen (str));
	char_out (13);
}

VOID
show_CR ()	/* CR only */
{
	char_out (13);
}

/************************************************************************/
/*	Low Level Functions						*/
/************************************************************************/

#define     CHCMD	126
#define     CHDTA	0
#define     CHEOF	4

#define     CHACK	2
#define     CHNAK	5


/*------------------------------*/
/*	SendCmd			*/
/*------------------------------*/

VOID
SendCmd (ch)

CHAR	ch;	/* ACK or NAK */
{
	CHAR	buff[2];

	buff[0] = CHCMD;
	buff[1] = ch;

	write_serial (&buff, 2);
}

/*------------------------------*/
/*	SendAck			*/
/*------------------------------*/
VOID
SendAck ()

{
	char_out ('A');

	SendCmd (CHACK);	/* do last, triggers next transmission */
}

/*------------------------------*/
/*	SendNak			*/
/*------------------------------*/
VOID
SendNak (code)

CHAR	code;	/* 1 = CMD error, 2 = HEADER error, 3 = checksum error */
{
	CHAR	string1[64], string2[64];

	char_out ('N');

	if (code != 3)			/* echo the error code */
		char_out (code + '0');
	else				/* show the two sums */
		{
		stci_d (&string1, checksum1, 64);
		stci_d (&string2, checksum2, 64);

		strcat (&string1, "/");
		strcat (&string1, &string2);
		strcat (&string1, "/");

		show_msg (&string1);
		}

	SendCmd (CHNAK);	/* do last, triggers next transmission */
}

/*------------------------------*/
/*	GetCh			*/
/*------------------------------*/
CHAR
GetCh ()	/* read a generic char from port */

{
	CHAR	buff[2];

/*	CHAR	hex1, hex2;	*/

	read_serial (&buff, 1);

/* for debugging, echo char (high + low nibble) to screen */

/*	hex1 = buff[0];    hex1 = hex1 >> 4;
	hex2 = buff[0];    hex2 = hex2 & 15;

	char_out (hex1 + '0');
	char_out (hex2 + '0');
	char_out (',');
*/
	return (buff[0]);
}

/*------------------------------*/
/*	GetCmd			*/
/*------------------------------*/
CHAR
GetCmd ()

{
	while (GetCh () != CHCMD)	/* wait for header code */
		{
		}

	return (GetCh ());	/* DATA or EOF */
}

/*------------------------------*/
/*	GetHdr			*/
/*------------------------------*/
BOOL
GetHdr ()	/* called after DATA command is received */

{
	if (GetCh () != 1)	return (FALSE);
	if (GetCh () != 0)	return (FALSE);
	if (GetCh () != 254)	return (FALSE);
	if (GetCh () != 255)	return (FALSE);

	return (TRUE);
}

/*--------------------------------------*/
/*	GetBlock  (formerly RECV)	*/
/*--------------------------------------*/
BOOL
GetBlock (buffer)  /* get block (256 bytes) plus checksum, verify it */

CHAR	*buffer;
{
	INT	i;
	CHAR	*temp;

/*	temp = buffer;
	for (i=0; i<256; i++)
		*temp++ = GetCh ();
*/
	read_serial (buffer, 256);	/* read block in one gulp */

/*	temp = (CHAR *) &checksum1;
	for (i=0; i<2; i++)
		*temp++ = GetCh ();
*/
	read_serial (&checksum1, 2);	/* read checksum */

	checksum2 = 0;
	temp = buffer;

	for (i=0; i<256; i++)
		checksum2 += *temp++;	/* compute our checksum */

	if (checksum1 == checksum2)
		return (TRUE);
	else
		return (FALSE);
}

/*------------------------------*/
/*	GetFile			*/
/*------------------------------*/
INT
GetFile ()	/* return zero for success */

{
	CHAR	command;
	CHAR	block[256];

	while ((command = GetCmd ()) != CHEOF)
		{
		if (command != CHDTA)
			SendNak (1);

		else if (! (GetHdr () ))    /* data coming, suck up header */
			SendNak (2);

		else if (! (GetBlock (&block) ))
			SendNak (3);		/* must be checksum error */

		else		/* write block to disk, exit if error */
			{
			if (write_game (game_channel, 256, &block) != 0)
				{
				show_msgCR ("Disk write error");
				return (1);
				}
			SendAck ();	/* okay, loop back for next */
			}
		}

	SendAck ();	/* acknowledge the EOF */
	return (0);
}


/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	TFTP			*/
/*------------------------------*/
VOID
TFTP ()
{
	INT	error;
	CHAR	*gamename;

	show_msgCR ("    ****** TFTP for Commodore Amiga ******");
	show_CR ();

	init_serial (TRUE);	/* open serial channel */

	gamename = "df1:Story.Data";
	error = open_game (gamename, &game_channel);

	if (error)
		{
		show_msg ("File open error: ");
		show_msgCR (gamename);
		}
	else
		{
		show_msg ("Writing to file ");
		show_msg (gamename);
		show_msg (", strike any key to begin ...");

		char_in ();	char_out (13);

		error = GetFile ();
		close_game (game_channel);	
		}

	init_serial (FALSE);		/* close the serial port */

	show_CR ();
	if (!error)	show_msg ("Transfer complete. ");

	show_msg ("Strike any key to exit ... ");
	char_in ();
}

/*------------------------------*/
/*	main			*/
/*------------------------------*/
INT
main ()
{
	INT	error;

	error = z_init ();	/* zero if OK */

	if (!error)
		TFTP ();	/* perform the file transfer */

	z_exit ();		/* clean up */

	return (error);
}

