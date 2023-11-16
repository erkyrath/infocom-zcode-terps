TO DO		Make combined PUTCHR routine for variable-width screen and
		script output buffering, takes args in a parameter block.


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

int ttyfd,			/* for setting up terminal i/o */
    ttysav;			/* storage of a startup tty condition */
int winlen = 22,		/* window length */
    linecnt;			/* line count for MORE */


*--------------------------------------------------------------------------
* GENERALIZED OUTPUT FOLDING ROUTINE
*--------------------------------------------------------------------------

*    THE QUECHR ROUTINE IS CALLED WITH A POINTER TO A STRUCTURE CONTAINING
* THE VARIABLES BELOW.  THIS ARRANGEMENT ALLOWS FOR THE EXISTANCE OF MORE THAN
* ONE STRUCTURE, USEFUL IF DISPLAYED TEXT AND SCRIPTED TEXT ARE TO FOLD AT
* DIFFERENT POINTS.

*    THE STRUCTURE IDENTIFIES TWO ACTION ROUTINES.  THE OUTPUT FUNCTION
* DUMPS THE BUFFER WHEN IT BECOMES FULL.  THE SIZE FUNCTION ALLOWS FOR THE 
* HANDLING OF PROPORTIONALLY SPACED TEXT.

*    THE LAST TWO VARIABLES ARE CURRENTLY IGNORED, BUT MIGHT BE USEFUL
* FOR IMPLEMENTING RECALCULATION OF THE FOLD POINTS FOR A PARAGRAPH OF TEXT,
* AS WHEN A WINDOW CHANGES SIZE.

/* data structure for queue-parameters block */

struct queparams {
    char *bufptr;	/* start of buffer */
    WORD bufsiz;	/* maximum units in buffer */

    char *nxtptr;	/* current position within buffer */
    WORD cursiz;	/* current units in buffer */

    XXX sizfun;		/* given a char, returns unit size */
    XXX outfun;		/* given bufptr & endptr, dumps buffer, adds cr */
    XXX retfun;		/* (unused -- this should add the cr) */

    BYTE dumped;	/* was buffer emptied (without cr) before full? */
    BYTE autocr;	/* append a cr to each buffer dump?	yes */
    BYTE keepsp;	/* don't discard trailing space?	no */
    }
    dque, sque;

#define qplen sizeof(struct queparams)


/************************************************************************
*									*
*	O U T P U T   B U F F E R I N G					*
*									*
************************************************************************/

putchr(letter, qp)
char letter; struct queparams *qp;
{  /*	Putchr takes an ASCII character and queues it for output.  If it
	fills the buffer, a search backwards for a space is conducted to
	break the line.  The end of the buffer is determined by endbuf
	which should be set whenever the screen size is changed to reflect
	the difference between the right and left margins.
   */
    char *tailptr, *dest;
    char nospace = 0;
   
    if (chrptr != endbuf)  		/* any room left? */
      *chrptr++ = letter;		/* store the char */
    else {
      tailptr = endbuf;
      while (*--tailptr != SPACE) {	/* search backwards for a space */
	if (tailptr == outbuf)  {	/* not a space to be found! */
	  letter = dumpfix(letter, CONSOLE);	/* dump buffer and add crlf */
	  nospace = 1;			/* indicate reason for loop exit */
	  break;
	  }				/* end of while searching for ' ' */
	}
      if (*tailptr == SPACE)		/* space found, rearrange buffer */
	if ((tailptr == outbuf) && (nospace != 1)) 
	  letter = dumpfix(letter, CONSOLE);
	else {
	  *tailptr = NULL;		/* end the line at the space */
	  dumpbuf();
	  dest = outbuf;		/* shift-left remainder of line */
	  while (++tailptr < endbuf) 	/* from space+1 to buffer start */
	    *dest++ = *tailptr;
	  chrptr = dest;		/* reset the character pointer */
	  }				/* end of rearranging of buffer */
      if (letter) 			/* if no crlf */
        *chrptr++ = letter;		/* put the char in the buffer */
      }
}


dumpfix(letter, caller)
int caller;
ZIPINT letter;
{  /*	Dumpfix is called after putchr has searched backwards for a space
	character.  It prints the line and determines when a crlf should
	follow.
   */
    if (caller == CONSOLE) 
      dumpbuf();		/* print the line */
    else
      dmp_pbuf();		/* print the line on pipe */
    if (letter == SPACE)  	/* print a crlf in place of a ' '*/
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
    char nospace = 0;
   
    if (p_chrptr != p_endbuf)  	/* if there is room */
      *p_chrptr++ = letter;		/* put the char in the buffer */
    else {
      tailptr = p_endbuf;		/* init tail to end of buffer */
      while (*--tailptr != SPACE) {	/* search backwards for a " " */
	if (tailptr == p_outbuf)  {	/* not a space to be found! */
/**/	  letter = dumpfix(letter, PIPE);	/* dump buffer and add crlf */
	  nospace = 1;			/* indicate reason for loop exit */
	  break;
	  }				/* end of while searching for ' '*/
	}
      if (*tailptr == SPACE)  	/* space found, rearrange buffer */
	if ((tailptr == p_outbuf) && (nospace != 1)) 
/**/	  letter = dumpfix(letter, PIPE);
	else {
	  *tailptr = NULL;		/* make space into end of line */
/**/	  dmp_pbuf();			/* now print the buffer */
	  dest = p_outbuf;		/* now move remainder of line <-- */
	  while (++tailptr < p_endbuf) 	/* from space+1 to the beginning */
	    *dest++ = *tailptr;		/* of the output buffer */
	  p_chrptr = dest;		/* reset the character pointer */
	  }				/* end of rearranging of buffer */
      if (letter) 			/* if no crlf */
        *p_chrptr++ = letter;		/* put the char in the buffer */
      }					/* end of first else */
    return;
}

newlin()
{  /* 	Newlin is called when a CRLF is desired on output of a zstring.
	It flushes the buffer and resets the buffer character pointer.
   */
    *chrptr = NULL;		/* indicate end of line */
    if (scripting) 
      *p_chrptr = NULL;
    dumpbuf();
}

dumpbuf()
{  /*	Dumpbuf flushes the existing buffer to the screen.
   */
    if (scripting)  {
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
    if (*bufptr == Z_EOL)  {	/* drop in end char */
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
    if (*buf == NULL) 
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
    temp = GTVWRD(PFLAGS);		/* get status word */
    if (scripting)  {		/* set according to current state */
      if ((temp & SCRIPTBIT) == 0)  {
	fclose(scrptfd);		/* close if turned off */
	scripting = 0;			/* and reset flag */
	}
      }
    else
      if (temp & SCRIPTBIT)  {
        scripting = 1;			/* turn on flag and open */
	p_chrptr = p_outbuf;		/* reset scripting buffer */
        if ((scrptfd = fopen("script", "w")) < 0)  
	  scripting = 0;		/* turn off flag if open fails */
        }
    return;
}



* ALLOCATE AND INITIALIZE A QUEUE PARAMETER BLOCK,
*   D1 IS MAXIMUM SIZE OF BUFFER (IN BYTES), D2 IS INITIAL UNIT SIZE
*   A1 POINTS TO LINE OUTPUT FUNCTION, A2 POINTS TO UNIT SIZE FUNCTION
*   RETURN BLOCK POINTER IN A0

INITQP	MOVE.W	D1,D0
	ADD.W	#QPLEN,D0	* TOTAL SPACE TO ALLOCATE
	EXT.L	D0
	BSR	GETMEM		* GET IT

	MOVE.L	A0,D0		* PARAMETERS BLOCK WILL START HERE
	ADD.L	#QPLEN,D0
	MOVE.L	D0,BUFPTR(A0)	* LINE BUFFER STARTS HERE
	MOVE.L	D0,NXTPTR(A0)	* ALSO CURRENT POINTER

	MOVE.W	D2,BUFSIZ(A0)	* UNIT CAPACITY OF BUFFER
	CLR.W	CURSIZ(A0)	* ALWAYS EMPTY INITIALLY

	MOVE.L	A1,OUTFUN(A0)	* INITIALIZE LINE OUTPUT FUNCTION
	MOVE.L	A2,SIZFUN(A0)	* INITIALIZE CHAR SIZE FUNCTION

	CLR.B	DUMPED(A0)	* THIS ONE IS SET ONLY BY PUTLIN
	RTS			* RETURN THE POINTER IN A0


* QUEUE THE CHAR IN D0 FOR OUTPUT, A0 POINTS TO QUEUE-PARAMETERS BLOCK

QUECHR	MOVEM.L	D4/A1-A4,-(SP)
	MOVE.W	D0,D4
	MOVE.L	A0,A4

	MOVE.W	CURSIZ(A4),D0
	CMP.W	BUFSIZ(A4),D0	* BUFFER FULL YET?
	BLT	QCX8		* NO
***	BGT	QCX0		* OVERFULL (DEQUE THE LAST CHAR AND REENTER)

	CMPI.B  #32,D4		* YES, BUT DID A SPACE CAUSE OVERFLOW?
	BNE.B	QCX1		* NO

	MOVE.L  NXTPTR(A4),D0	* YES, JUST PRINT THE WHOLE BUFFER
	MOVE.L	BUFPTR(A4),A0

	MOVE.L	OUTFUN(A4),A3	
	JSR	(A3)

	CLR.W	CURSIZ(A4)		* RESET LENGTH COUNTER
	MOVE.L  BUFPTR(A4),NXTPTR(A4)	* RESET CURRENT CHAR POINTER
	CLR.B	DUMPED(A4)		* STARTING A FRESH LINE
	BRA.B	QCX9			* EXIT, IGNORING SPACE

* FOLDING ROUTINE, SEARCH FOR MOST-RECENT SPACE ...

QCX1	MOVE.L  BUFPTR(A4),A1	* BEGINNING OF BUFFER
	MOVE.L  NXTPTR(A4),A2	* END OF BUFFER (+1)
	BRA.B	QCX2A		* ALLOW FOR EMPTY [DUMPED] BUFFER 

QCX2	CMPI.B  #32,-(A2)	* SEARCH FOR SPACE BACKWARDS FROM END
	BEQ.B	QCX4		* FOUND ONE
QCX2A	CMPA.L  A1,A2		* REACHED BEGINNING OF BUFFER?
	BGT	QCX2		* NOT YET

* NO SPACES FOUND, DUMP WHOLE BUFFER ...

QCX3	TST.B	DUMPED(A4)	* BUT WAS THIS BUFFER ALREADY PARTLY EMPTIED?
	BNE	QCX5		* YES, CARRY EVERYTHING OVER TO NEXT LINE

	MOVE.L	NXTPTR(A4),A2	* OTHERWISE, OUTPUT EVERYTHING
	BRA	QCX5

* SPACE WAS FOUND, DUMP THE BUFFER (THROUGH SPACE) ...

QCX4	MOVE.L	A2,A0		* POINTER TO THE SPACE
	ADDQ.L	#1,A2		* POINTER PAST THE SPACE

	CMPA.L  A1,A0		* DEGENERATE CASE WITH SPACE AT BUFPTR?
	BEQ	QCX3		* YES, OUTPUT WHOLE LINE

QCX5	MOVE.L  A2,D0		* LAST CHAR TO PRINT (+1)
	MOVE.L	A1,A0		* START OF BUFFER

	MOVE.L	OUTFUN(A4),A3	* GO DUMP IT, ADDING A CR
	JSR	(A3)

* SHIFT ANY REMAINING CHARS TO FRONT OF BUFFER ...

	CLR.W	CURSIZ(A4)	* ZERO THE UNIT COUNT
	CLR.B	DUMPED(A4)	* START WITH A FRESH BUFFER
	BRA	QCX7

QCX6	MOVE.B  (A2)+,D0
	MOVE.B	D0,(A1)+	* COPY NEXT CHAR TO BEGINNING OF BUF

	MOVE.L	SIZFUN(A4),A0	* CHAR STILL IN D0
	JSR	(A0)
	ADD.W	D0,CURSIZ(A4)	* UPDATE THE UNIT COUNT

QCX7	CMPA.L  NXTPTR(A4),A2	* ANY MORE CHARS AFTER SPACE?
	BLT	QCX6		* YES
	MOVE.L  A1,NXTPTR(A4)	* NO, STORE NEW CURRENT POINTER HERE

* FINALLY, STORE THE NEW CHAR AND EXIT ...

QCX8	MOVE.L	NXTPTR(A4),A0
	MOVE.B  D4,(A0)+	* STORE THE NEW CHARACTER IN BUFFER
	MOVE.L  A0,NXTPTR(A4)	* AND UPDATE POINTER

	MOVE.W	D4,D0
	MOVE.L	SIZFUN(A4),A0
	JSR	(A0)		* GET UNIT SIZE OF NEW CHAR
	ADD.W	D0,CURSIZ(A4)	* AND UPDATE COUNTER

QCX9	MOVEM.L (SP)+,D4/A1-A4
	RTS


*--------------------------------------------------------------------------
* MAIN OUTPUT HANDLER
*--------------------------------------------------------------------------

* !ALL! OUTPUT GENERATED BY THE GAME (AND THE USER) SHOULD BE CHANNELED 
*   THROUGH THE FOLLOWING TWO ROUTINES, WHICH REDIRECT IT APPROPRIATELY.


* OUTPUT A NEWLINE

PUTNEW	MOVEQ	#13,D0		* JUST FALL THROUGH WITH A CR

* OUTPUT THE CHAR IN D0 (TO THE REQUESTED DEVICES)

PUTCHR	CMPI.B	#9,D0		* TAB? (OLD ZORK BUG, DISPLAYS GARBAGE)
	BNE.S	PCX1
	MOVEQ	#32,D0		* YES, MAP TO A SPACE
PCX1	NOP			* <ST ASSEMBLER>

*** STATUS LINE OUTPUT (ZIP INTERNAL FUNCTION ONLY) ...

    IFEQ CZIP
	TST.W	VOSTAT(A6)	* SPECIAL OUTPUT TO SL HANDLER?
	BNE	PUTSL		* YES (ABORT PUTCHR)
    ENDC

*** TABLE OUTPUT ...

    IFEQ EZIP
	TST.W	VOTABL(A6)	* TABLE OUTPUT?
	BNE	TABCHR		* YES (ABORT PUTCHR PER "DIROUT" SPEC)
    ENDC

	MOVE.W	D0,-(SP)	* OTHERWISE, SAVE THE CHAR HERE

*** SCRIPT (BEFORE SCREEN, SO "OPEN" ERROR DISPLAYS IN CORRECT SEQUENCE)

	TST.W	WIND1(A6)	* BUT ARE WE IN WINDOW 1?
	BNE.S	PCX2		* YES, TEMPORARILY AVOID SCRIPTING

    IFEQ EZIP
	TST.W	VOPRNT(A6)	* SCRIPTING ACTIVE?
    ENDC
    IFEQ CZIP
	BSR	TSTSCR		* CHECK FOR SCRIPTING REQUEST -- ACTIVE?
    ENDC
	BEQ.S	PCX2		* NO

	MOVE.W	(SP),D0		* SCRIPT THIS CHAR
	BSR	SCRCHR

*** SCREEN DISPLAY ...

PCX2	TST.W	VOCONS(A6)	* CONSOLE OUTPUT ACTIVE?
	BEQ.S	PCX3		* NO

	MOVE.W	(SP),D0		* YES, DISPLAY THE CHAR
	BSR	QDCHR

*** FILE OUTPUT ...

PCX3	NOP			* NOT IMPLEMENTED

	TST.W	(SP)+		* FLUSH CHAR FROM STACK
	RTS

