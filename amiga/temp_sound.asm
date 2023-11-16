
*********************** ax1 ******************************

    XREF	_german_convert
    XREF	_end_sound	* END-OF-SOUND CHECK

*    16 FEB 88	DBB	ADDED SOUND, SOUND INTERRUPTS (GAMINT)
*			FIXED COPYT/PRINTT PAGING BUG
*    			XZIP 'B' FROZEN

SFUNC	EQU	TCOUNT-2	* FUNCTION TO CALL UPON SOUND-END
SCOUNT	EQU	SFUNC-2		* #OPS TO WAIT BEFORE NEXT SOUND-END CHECK


*********************** ax2 ******************************

**** OPINPUT ****

INPX4	BSR	GAMINT1		* CHECK FOR GAME INTERRUPT (SOUND, ETC)

	BSR	ITTYIN		* CHECK FOR A KEY  << DON'T WAIT! >>



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


*********************** ax3 ******************************

NXTINS	BSR	GAMINT		* CHECK FOR GAME INTERRUPT (SOUND, ETC)
	BSR	NXTBYT		* GET THE NEXT INSTRUCTION BYTE

*********************** ax4 ******************************

* ----------------------
* DOSOUND
* ----------------------

* AMIGA: ADJUST ARGS
*   VOLUME: 0-64, -1=USE MIDI VOL
*   COUNT: 0=INFINITE, 1-254=FINITE, -1=USE MIDI COUNT

DOSOUND	TST.W	D2
	BLT.S	DOSDX1
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

* ----------------------
* GAMINT, GAMINT1
* ----------------------

* PERIODICALLY CHECK FOR GAME INTERRUPTS

* NOTE: BECAUSE THE INTERPRETER AS A WHOLE IS /NOT/ RE-ENTRANT, THIS 
* ROUTINE CHOULD BE CALLED ONLY FROM THE TOP OF THE MAIN LOOP (NXTINS), AND
* (PERHAPS) FROM OPREAD/OPINPUT (SO SOUNDS CAN CHAIN BETWEEN MOVES).  
* BUT IT SHOULD /NOT/ BE CALLED FROM ITTYIN, SINCE THAT IS ALSO CALLED 
* FROM [MORE], AND THE INTERPRETER STATE IS UNDEFINED.

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
* Note: called from OPREAD/OPINPUT; probably safe to call interrupt function
* as long as it doesn't itself call OPREAD/OPINPUT.

GAMINT1	SAVEREGS

* (could check first for valid SFUNC(A6), skip end-check if none,
* but then end event would hang around unreported ...)

	RESWORD
	JSR	_end_sound	* END-OF-SOUND CHECK
	POPWORD			* NON-ZERO IF END DETECTED
	BEQ.S	GAMIX3

	MOVE.W	SFUNC(A6),D0	* SOUND-INTERRUPT FUNCTION ("START NEXT")
	BEQ.S	GAMIX3		* NONE
	BSR	INCALL		* CALL IT (INTERNALLY)
***	TST.W	D0		* (NO RETURN VAL)

GAMIX3
***	JSR	_int_key	* TYPE-AHEAD CHECK (here or from real int?)
	RESTREGS
	RTS

