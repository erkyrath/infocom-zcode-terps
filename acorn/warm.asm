	PAGE
	SBTTL "--- WARMSTART ROUTINE ---"

	; -------------
	; ZIP WARMSTART
	; -------------

WARM2:	LDA	#0		; CLEAR ALL Z-PAGE VARIABLES
	LDX	#ZEROPG
ST0:	STA	0,X
	INX
	CPX	#ZPGTOP
	BCC	ST0

	; INIT THE PAGING TABLE

	TAX			; = 0
	LDA	#$FF
ST1:	STA	PTABL,X
	STA	PTABH,X
	INX
	BNE	ST1

	INC	ZSP		; INIT Z-STACK POINTERS
	INC	OLDZSP		; TO "1"
	INC	SCRIPT		; ENABLE SCRIPTING

	; GRAB THE FIRST BLOCK OF PRELOAD

	LDA	#HIGH ZBEGIN	; MSB OF PRELOAD START ADDRESS
	STA	ZCODE		; FREEZE IT HERE
	STA	DBUFF+HI	; LSB IS ALWAYS ZERO
	JSR	GETDSK		; [DBLOCK] SET TO Z-BLOCK 0

	; EXTRACT GAME DATA FROM Z-CODE HEADER

	LDX	ZBEGIN+ZENDLD	; MSB OF ENDLOAD POINTER
	INX			; ADD 1 TO GET
	STX	ZPURE		; 1ST "PURE" PAGE OF Z-CODE

	TXA			; ADD START PAGE OF PRELOAD
	CLC			; TO CALC ABSOLUTE START ADDRESS
	ADC	ZCODE		; OF PAGING SPACE
	STA	PAGE0

	JSR	MEMTOP		; RETURNS TOP RAM PAGE IN [A]
	SEC
	SBC	PAGE0		; SUBTRACT ADDRESS OF PAGING SPACE
	BEQ	NORAM
	BCS	SETNP		; ERROR IF NOT ENOUGH RAM

	; *** ERROR #0 -- INSUFFICIENT RAM ***

NORAM:	LDA	#0
	JMP	ZERROR

SETNP:	STA	PMAX		; SET # SWAPPING PAGES

	LDA	ZBEGIN+ZMODE
	AND	#%00000010	; ISOLATE STATUS-FORMAT BIT
	STA	TIMEFL		; 0=SCORE, NZ=TIME

	LDA	ZBEGIN+ZGLOBA	; GET MSB OF GLOBAL TABLE ADDR
	CLC			; CONVERT TO
	ADC	ZCODE		; ABSOLUTE ADDRESS
	STA	GLOBAL+HI
	LDA	ZBEGIN+ZGLOBA+1	; LSB NEEDN'T CHANGE
	STA	GLOBAL+LO

	LDA	ZBEGIN+ZFWORD	; DO SAME FOR FWORDS TABLE
	CLC
	ADC	ZCODE
	STA	FWORDS+HI
	LDA	ZBEGIN+ZFWORD+1	; NO CHANGE FOR LSB
	STA	FWORDS+LO

	LDA	ZBEGIN+ZVOCAB	; NOW DO VOCABULARY TABLE
	CLC
	ADC	ZCODE
	STA	VOCAB+HI
	LDA	ZBEGIN+ZVOCAB+1	; LSB SAME
	STA	VOCAB+LO

	LDA	ZBEGIN+ZOBJEC	; NOT TO MENTION
	CLC			; THE OBJECT TABLE
	ADC	ZCODE
	STA	OBJTAB+HI
	LDA	ZBEGIN+ZOBJEC+1	; LSB SAME
	STA	OBJTAB+LO

	; FETCH THE REST OF THE PRELOAD

LDPRE:	LDA	DBLOCK+LO	; CHECK CURRENT BLOCK #
	CMP	ZPURE		; LOADED LAST PRELOAD PAGE YET?
	BCS	WARMEX		; YES, TIME TO PLAY!
	JSR	GETDSK		; ELSE GRAB NEXT Z-BLOCK
	JMP	LDPRE

WARMEX:	LDA	ZBEGIN+ZGO	; GET START ADDRESS OF Z-CODE
	STA	ZPCM		; MSB
	LDA	ZBEGIN+ZGO+1	; AND LSB
	STA	ZPCL		; HIGH BIT ALREADY ZEROED

	JSR	CLS		; CLEAR SCREEN, DISABLE SPLIT

	; ... AND FALL INTO MAIN LOOP

	END
