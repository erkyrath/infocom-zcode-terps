
/*------------------------------------------------------*/
/*	TFTP test program				*/
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


/************************************************************************/
/*	Error Handling							*/
/************************************************************************/

/*------------------------------*/
/*	show_message		*/
/*------------------------------*/
VOID
show_message (str)	/* display in console window */

CHAR	*str;
{
	line_out (str, strlen (str));

	char_out (13);		/* add the CR */
}

/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	main
/*------------------------------*/
VOID
main ()
{
	CHAR	c[4];

	z_init ();
	init_serial (TRUE);

	show_message ("TFTP Test Program, strike any key to begin ... ");

	char_in ();

	show_message ("Reading SERIAL.DEVICE: ");

/*	LONG	file;

	file = open ("SER:", 2);
	if (file == -1)
		show_message (" Serial device open error\n");
	else
*/		
		FOREVER
			{
			read_serial (&c, 3);

			c[3] = 0;		/* asciz */
			show_message (&c);

			if (c[0] == 'Q')	break;
			}

	show_message ("Transfer complete, strike any key to exit ...");

	char_in ();

	init_serial (FALSE);

	z_exit ();
}
