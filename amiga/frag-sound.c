
/============================= in xzip1 =================================/

	init_interrupt (1);
	init_interrupt (0);

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
	static struct Interrupt zir;
	register struct Interrupt *p = &zir;	/* (tighten up code) */

	if (start) {
		p->is_Node.ln_Type = NT_INTERRUPT;
		p->is_Node.ln_Pri = -60;	/* not time-critical */
		p->is_Node.ln_Name = "zir";
	/**	p->is_Data = 0;  **/		/* currently unused */
		p->is_Code = VertBServer;

	/* put the new interrupt server into action */
		AddIntServer (INTB_VERTB, p);
		inited = 1;
		}
	else {
		if (inited)
			RemIntServer (INTB_VERTB, p);
		}
}

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
/*	int_key			*/
/*------------------------------*/

/* Should this run as a machine interrupt or a game pseudo-interrupt?
   Some ops ($VERIFY) take a long time.  */

void int_key()
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


/============================= in sound.c =================================/

/* Use of sound synchronization flags:
	(1) audion - sets start, clears stop
	(2) machine (true) interrupt - if ON-OFF and done, sets stop
	(3) game interrupt - if ON-ON, clears both

   In routines (1) and (3), the two flags are best changed simultaneously,
   to avoid the chance of an in-between interrupt by (2).
*/

struct SyncFlag {
	UBYTE start;
	UBYTE stop;
	} sf[SCHANS] = {0};


/*** in audion() ***/

	UWORD *p;

			curIOA->ioa_Request.io_Command = CMD_WRITE;
 			curIOA->ioa_Request.io_Flags = ADIOF_PERVOL |IOF_QUICK;
			BeginIO((struct IORequest *)curIOA);

		/* write to both flags simultaneously */
			p = (UWORD *) &sf[m2chn];
			*p = 0x0100;		/* start=ON, stop=OFF */


/*------------------------------*/
/*	do_Finish		*/
/*------------------------------*/

/* send ADCMD_FINISH message (separate routine since multiple calls)  */

do_Finish (curIOA, chan)
struct IOAudio *curIOA;
UBYTE chan;
{
	UWORD *p = (UWORD *) &sf[chan];
	*p = 0;		/* turn off both flags simultaneously */

	curIOA->ioa_Request.io_Flags = ADIOF_PERVOL | IOF_QUICK;
	curIOA->ioa_Request.io_Command = ADCMD_FINISH;
	BeginIO ((struct IORequest *)curIOA);
}

/*------------------------------*/
/*	md_sound		*/
/*------------------------------*/

/* id=1-n or 0(mru), action=1-4, 
   volume=0-64 or -1(midi), repeat=1-n, 0(infinite) or -1(midi) */

	WORD i, err;

	[remove vol2, rep2 refs]


/*------------------------------*/
/*	int_sound		*/
/*------------------------------*/

/* Machine interrupt - check for end of sound.
   This interrupt routine is active at all times, for simplicity.

   If a sound has been started (on any channel) and we have NOT previously
   detected its completion, we check now.  If we detect its completion,
   we set the appropriate global "stop" flag.  
*/

void int_sound()
{
	register UBYTE *p;			/* (tighten up code) */
	register int i;
/*	struct IORequest *result;  */

	p = (UBYTE *) &sf[0];
	for (i=0; i<SCHANS; i++) {
	    if (*p++) {				/* FIRST see if started */
		if (!*p)			/* THEN see if NOT stopped */
		    if (CheckIO (IOA[i]))	/* check status */
			*p = 1;			/* valid result means done */
		}
	    p++;				/* next flag set */
	    }
}

/*------------------------------*/
/*	end_sound		*/
/*------------------------------*/

/* This "pseudo-interrupt" is called from the 68K kernel and returns TRUE
   to report the end of a sound (on ANY channel...) */

BOOL end_sound ()
{
	register UBYTE *p;
	register int i;
	BOOL result = FALSE;

	p = (UBYTE *) &sf[0];
	for (i=0; i<SCHANS; i++) {
	    if (*p++)				/* started? */
		if (*p) {			/* stopped? */
		    *((UWORD *)(p-1)) = 0;	/* turn off both flags */
		    result = TRUE;
		    }
	    p++;				/* next flag set */
	    }
	return (result);
}


/============================= in 68K =================================/

    XREF	_end_sound	* END-OF-SOUND CHECK
    XREF	_int_key

SFUNC	EQU	x-2		* FUNCTION TO CALL UPON SOUND-END
SCOUNT	EQU	x-2		* #OPS TO WAIT BEFORE NEXT SOUND-END CHECK

NXTINS	BSR	GAMINT		* CHECK FOR GAME INTERRUPT (SOUND, ETC)
ITTYIN	BSR	GAMINT		* CHECK FOR GAME INTERRUPT (SOUND, ETC)

* ----------------------
* GAMINT, GAMINT1
* ----------------------

SKPCNT	EQU	50	* 2500 ops/sec (max) ==> 50 ints/sec (max)

* ENTRY POINT FROM MAIN LOOP
* To avoid cutting into interpreter performance too much, we keep a
* simple counter, and only occasionally perform the (somewhat costlier) 
* check for a sound interrupt.

GAMINT	SUBQ.W	#1,SCOUNT(A6)	* TIME FOR A SOUND CHECK?
	BLE.S	GAMIX1
	RTS			* NO, QUICK EXIT
GAMIX1	MOVE.W	#SKPCNT,SCOUNT(A6)    * YES, RESET COUNTER AND FALL THRU

* ENTRY POINT FROM INPUT-WAIT LOOP
* While awaiting Amiga input, we awaken only 10 times/sec anyway, so check
* for sound every time.  Might actually prefer it be a bit more frequent.
* Note: called from OPREAD/OPINPUT; safe to call interrupt function?

GAMINT1	SAVEREGS

* (could check first for valid SFUNC(A6), skip end-check if none,
* but then end event would hang around unreported ...)

	RESWORD
	JSR	_end_sound	* END-OF-SOUND CHECK
	POPWORD			* NON-ZERO IF END DETECTED
	BEQ.S	GAMIX3

	MOVE.W	SFUNC(A6),D0	* SOUND-INTERRUPT FUNCTION ("START NEXT")
	BSR	INCALL		* CALL IT (INTERNALLY)
***	TST.W	D0		* (NO RETURN VAL)

GAMIX3
***	JSR	_int_key	* TYPE-AHEAD CHECK (here or from real int?)
	RESTREGS
	RTS

* ----------------------
* OPSOUND
* ----------------------

* MAKE A SOUND
*   ARG1 = ID: 1=BEEP, 2=BOOP, 3+ ARE SPECIAL, 0=MRU
*   [ARG2] = ACTION: 1=INIT, [2=START], 3=STOP, 4=CLEANUP
*   [ARG3] = COUNT (HIBYTE): -1=INFINITE, [0=USE MIDI COUNT], 1-254=FINITE
*	     VOL (LOBYTE): 0=MIN, 8=MAX, [-1=USE MIDI VOLUME]
*   [ARG4] = INTERRUPT FUNCTION

OPSOUND	NOP			* REQUEST AN ARGBLK
	LEA	DEFBLK(A6),A1	* USE A DEFAULT ARGBLK, TOO
	MOVE.W	#4,(A1)		* 4 ARGS MAX
	MOVE.W	#2,ARG2(A1)	* DEFAULT ACTION = "START"
	MOVE.W	#$00FF,ARG3(A1)	  * DEFAULT COUNT=0/VOL=-1 (USE MIDI DATA)
	CLR.W	ARG4(A1)	* DEFAULT INTERRUPT HANDLER = NONE
	BSR	SETDEF		* SET UP DEFAULTS

	MOVE.W	ARG4(A0),SFUNC(A6)  * SAVE INTERRUPT HERE
	MOVE.W	ARG3(A0),D2	* COUNT/VOL
	MOVE.W	D2,D3
	EXT.W	D2		* VOL (16 BITS)
	LSR.W	#8,D3		* COUNT (16 BITS, UNLESS -1)
	CMPI.B	#$FF,D3		* -1?
	BNE.S	OPSDX1
	EXT.W	D3		* YES, MAKE 16 BITS

OPSDX1	MOVE.W	ARG2(A0),D1	* ACTION
	MOVE.W	ARG1(A0),D0	* SOUND ID
	BRA	DOSOUND

* ----------------------
* DOSOUND
* ----------------------

* AMIGA: ADJUST ARGS
*   VOLUME: 0-64, -1=USE MIDI VOL
*   COUNT: 0=INFINITE, 1-254=FINITE, -1=USE MIDI COUNT

DOSOUND	TST.W	D2
	BLT.S	D0SDX1
	ASL.W	#3,D2		* MAP 0-8 TO 0-64

DOSDX1	TST.W	D3
	BGT.S	DOSDX2		* IF 1-254, JUST PASS ALONG
	ADDQ.W	#1,D3		* MAP -1 TO 0
	BEQ.S	DOSDX2
	MOVEQ	#-1,D3		* MAP 0 TO -1

DOSDX2	SAVEREGS
	MOVE.L	D3,-(SP)	* COUNT
	MOVE.L	D2,-(SP)	* VOLUME
	MOVE.L	D1,-(SP)	* ACTION
	MOVE.L	D0,-(SP)	* ID
	JSR	_md_sound
	FLUSH	16
	RESTREGS
	RTS

