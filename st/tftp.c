
/*----------------------------------------------------------------------*/
/*	Atari ST file transfer program using TFTP format		*/
/*----------------------------------------------------------------------*/

#include <stdio.h>
#include <fcntl.h>		/* defines O_RAW, etc */

#define BYTE	unsigned char
#define WORD	unsigned short	/* this must always be two bytes */
#define BOOL	int		/* TRUE or FALSE */

#define TRUE	1
#define FALSE	0

/*
#define SHOW(val, fmt) printf("%s=fmt\n", "val", val)
	SHOW(ch, %02x);		prints "ch = [hex value of ch]"
*/

/************************************************************************/
/*	Global Variables						*/
/************************************************************************/

int	game_channel,		/* refnum */
	packcount = 0;		/* counter for ACK/NAK display folding */

WORD	cksum1,		/* received checksum */
	cksum2;		/* computed checksum */


/************************************************************************/
/*	Display Routines						*/
/************************************************************************/

/*------------------------------*/
/*	show_msg		*/
/*------------------------------*/

show_msg(str)		/* write to console window */
char	*str;
{
	printf(str);
}

show_CR()		/* CR only */
{
	printf("\n");
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
/*	WritePort		*/
/*------------------------------*/

WritePort(ch)
BYTE	ch;	/* CMD, ACK, or NAK */
{
	bios(3, 1, ch);		/* bconout, device 1 is RS232 port */
}

/*------------------------------*/
/*	SendAck			*/
/*------------------------------*/

SendAck()
{
	putchar('A');

	packcount++;
	if (packcount == 40) {	/* fold screen display if needed */
		show_CR();
		packcount = 0;
		}

	WritePort(CHCMD);	/* last out, triggers next transmission */
	WritePort(CHACK);
}

/*------------------------------*/
/*	SendNak			*/
/*------------------------------*/

SendNak(code)
BYTE	code;	/* 1 = CMD error, 2 = HEADER error, 3 = checksum error */
{
	char	str[64];		/* checksum string */

	putchar('N');
	putchar(code + '0');		/* show error code */
/*	packcount += 2;		*/

	if (code == 3)			/* also show the two checksums */
		{
		putchar(',');
		stci_d(str, cksum1, 64);
		show_msg(str);
		putchar(',');

		stci_d(str, cksum2, 64);
		show_msg(str);
		putchar(',');
		}

	WritePort(CHCMD);	/* last out, triggers next transmission */
	WritePort(CHNAK);
}

/*------------------------------*/
/*	ReadPort		*/
/*------------------------------*/

BYTE
ReadPort()	/* read next character from serial port */
{
	BYTE	ch;
/*	BYTE	hex1, hex2;  */

	ch = bios(2, 1);	/* bconin, device 1 is RS232 port */

/* for debugging, echo char to screen */

/*	hex1 = ch >> 4;		[high nibble]
	hex2 = ch & 15;		[low nibble]

	putchar(hex1 + '0');
	putchar(hex2 + '0');
	putchar(',');
*/
	return(ch);
}

/*------------------------------*/
/*	GetCmd			*/
/*------------------------------*/

BYTE
GetCmd()
{
	while (ReadPort() != CHCMD);	/* wait for header byte */

	return(ReadPort());		/* next is either DATA or EOF */
}

/*------------------------------*/
/*	GetHdr			*/
/*------------------------------*/

BOOL
GetHdr()	/* called after DATA command is received */
{
	if (ReadPort() != 1)	return(FALSE);
	if (ReadPort() != 0)	return(FALSE);
	if (ReadPort() != 254)	return(FALSE);
	if (ReadPort() != 255)	return(FALSE);

	return(TRUE);
}

/*--------------------------------------*/
/*	GetBlock  (aka RECV)		*/
/*--------------------------------------*/

BOOL
GetBlock(buffer)  /* get 256 bytes plus checksum, verify it */

BYTE	*buffer;
{
	int	i;
	BYTE	*ptr;

	ptr = buffer;
	for (i=0; i<256; i++)		/* get block */
		*ptr++ = ReadPort();

	ptr = (BYTE *) &cksum1;
	for (i=0; i<2; i++)		/* get checksum */
		*ptr++ = ReadPort();

	ptr = buffer;
	cksum2 = 0;
	for (i=0; i<256; i++)		/* compute checksum */
		cksum2 += *ptr++;

	if (cksum1 == cksum2)
		return(TRUE);
	else
		return(FALSE);
}

/*------------------------------*/
/*	GetFile			*/
/*------------------------------*/

int
GetFile()	/* return zero if success */
{
	BYTE	command;
	BYTE	block[256];

	while ((command = GetCmd()) != CHEOF)
		{
		if (command != CHDTA)
			SendNak(1);

		else if (! (GetHdr() ))	    /* data coming, suck up header */
			SendNak(2);

		else if (! (GetBlock(&block) ))
			SendNak(3);	/* must be a checksum error */

		else  /* write block to disk, exit if error */
			{
			if (write(game_channel, &block, 256) != 256)
				{
				show_msg(" Disk write error");
				return(-1);
				}
			SendAck();	/* okay, loop back for next */
			}
		}
	SendAck();	/* acknowledge the EOF */
	return(0);
}

/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	TFTP			*/
/*------------------------------*/

#define CRLF 13

TFTP()
{
	int	error;
	char	*gamename;

	show_CR();
	show_msg("****** TFTP for Atari ST ******");	show_CR();
	show_CR();

	gamename = "STORY.DAT";
	game_channel = creat(gamename, O_RAW);

	if (game_channel == -1)
		{
		show_msg("Open error for file ");
		show_msg(gamename);
		show_CR();
		}
	else
		{
		show_msg("Writing to file ");
		show_msg(gamename);
		show_CR();
		show_msg("Press RETURN to begin ... ");

		while (getch() != CRLF);	/* pause (1 char, no echo) */
		show_msg("Listening ...");
		show_CR();

		error = GetFile();		/* do the transfer */
		show_CR();
		if (!error)
			show_msg("Transfer complete. ");
		else	show_msg("Transfer not complete. ");

		if (close(game_channel) != 0)
			show_msg("File close error");
		}

	show_msg("Press RETURN to exit ... ");
	while (getch() != CRLF);		/* pause */
}

/*------------------------------*/
/*	main			*/
/*------------------------------*/

main()
{
    /*	init_serial(TRUE);	*/	/* [ST ports always open] */

	TFTP();

    /*	init_serial(FALSE);	*/
}

