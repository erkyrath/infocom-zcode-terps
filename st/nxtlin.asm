
[MORE] WAS BEING INVOKED TOO LATE WHEN THE [MORE] LINE BEGAN WITH
BOLDFACE TEXT.  HERE'S THE FIX.

-----------------------------------------------------------------------

OPREAD	...

	MOVE.W	#1,LINES(A6)	* RESET COUNTER, PREVENTS A DEGENERATE [MORE]
	BSR	PUTLIN		* THEN FORCE OUT ANY QUEUED TEXT (THE PROMPT)
	...

* CHECK FOR END-OF-PAGE CONDITION

NXTLIN	MOVE.W	_rows,D0
	SUB.W	_split_row,D0	* LINES AVAILABLE FOR DISPLAY
	SUBQ.W	#1,D0		* ALLOW SPACE FOR [MORE]

	CMP.W	LINES(A6),D0	* PAGE FULL YET?
	BNE	NXTLX2		* NO

    IFEQ CZIP
	MOVEM.L	D1-D7/A1-A5,-(SP)
	BSR	OPUSL			* YES, UPDATE STATUS LINE NOW
	MOVEM.L	(SP)+,D1-D7/A1-A5
    ENDC

	LEA	MSGMOR,A0	* DISPLAY MORE MESSAGE (NO CR)
	MOVEQ	#6,D0
	BSR	PRINT		* WRITE DIRECTLY TO SCREEN
    DATA
MSGMOR	DC.B	'[MORE]',0
    TEXT

	BSR	TTYIN		* WAIT FOR A KEY, NO ECHO
	SUBQ.W	#6,_cur_column	* BACKUP CURSOR

	LEA	MSGNOM,A0	* ERASE MORE MESSAGE (NO CR)
	MOVEQ	#6,D0
	BSR	PRINT		* WRITE DIRECTLY TO SCREEN
    DATA
MSGNOM	DC.B	'      ',0
    TEXT

	SUBQ.W	#6,_cur_column	* BACKUP CURSOR
	MOVE.W	#1,LINES(A6)	* RESET COUNTER, ALLOWING FOR ONE PREVIOUS LINE
NXTLX2	RTS

* OUTPUT THE LINE BUFFER, BEGINNING AND END IN A0, D0
*   FIRST CHECK FOR END-OF-SCREEN CONDITION (UNLESS IN WINDOW 1)

BUFOUT	TST.W	WIND1(A6)	* IN WINDOW 1?
	BNE	BUFUX1		* YES, SKIP THE SCREEN CHECK

	MOVEM.L	D0/A0,-(SP)
	BSR	NXTLIN		* PAUSE FOR [MORE] IF NECESSARY
	MOVEM.L	(SP)+,D0/A0

BUFUX1	SUB.L	A0,D0		* CURRENT LENGTH OF BUFFER IN D0.W
	BLE	BUFUX2		* EXIT IF ZERO
	BSR	PRINT		* OTHERWISE, DISPLAY IT
BUFUX2	RTS

* OUTPUT THE LINE BUFFER THEN ADD A CR, BEGINNING AND END IN A0, D0
*   ALSO UPDATE THE CUMULATIVE LINE COUNTER

LINOUT	BSR	BUFOUT		* DISPLAY IT
	MOVEQ	#13,D0
	BSR	OUTCHR		* AND TACK ON A CR

	TST.W	WIND1(A6)	* IN WINDOW 1?
	BNE	LINOX1		* YES, IGNORE COUNTER
	ADDQ.W	#1,LINES(A6)	* OTHERWISE UPDATE THE COUNTER
LINOX1	RTS
