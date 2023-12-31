	PAGE	
	SBTTL "--- MACHINE-DEPENDENT I/O: APPLE II ---"


; ----------------------------
; FETCH ASCII KEYCODE INTO [A]
; ----------------------------
; EXIT: ASCII IN [A] & [IOCHAR]

GETKEY:	CLD		
	TXA		; SAVE [X] & [Y]
	PHA		
	TYA		
	PHA		
GKEY0:	JSR	RDKEY	;GET A CHAR

	;CHECK TO MAKE SURE KEY IS VALID, ONLY ACCEPT IT IF IT IS

	AND	#$7F	;SCREEN OUT SHIFTS
	CMP	#EOL	;CHECK FOR GOOD (BAD) CHAR
	BNE	GK0	
	JMP	OK	
GK0:	CMP	#BACKSP	
	BNE	GK1	
	JMP	OK	

	;CHECK FOR "ARROWS", CONVERT FOR USE (EZIP)
	;ALSO : CHANGE HIGH _LOW )@%^&*( TO ,-.0256789

GK1:	LDX	#ENDKEY	; GET LENGTH OF LIST
GK2:	CMP	HAVE,X	; CHECK AGAINST LIST OF UNWANTED KEYS
	BEQ	GK3	; FOUND IT
	DEX		
	BPL	GK2	; CHECK THSM ALL
	BMI	GK4	; NOT FOUND, CONTINUE OTHER CHECKS
GK3:	LDA	WANT,X	; GET KEY TO USE INSTEAD
	BNE	OK	; JMP

GK4:	CMP	#SPACE	; NO CTRL CHARS ACCEPTABLE
	BCC	BADKEY	; IF HIGH  SPACE, BAD

;
; 6/3/87 jdarnold -- please allow '+'
;
;	CMP	#$2B	
;	BEQ	BADKEY	

	CMP	#$3C	
	BCC	OK	
	CMP	#$3F	
	BEQ	OK	

MASK0:	CMP	#'z'+1	;PICK OUT LETTERS NOW
	BCS	BADKEY	;IF LOW  BAD
	CMP	#'a'	
	BCS	OK	;IF LOW  OK
	CMP	#'A'	
	BCC	BADKEY	
	CMP	#'Z'+1	
	BCC	OK	;IF HIGH  OK
BADKEY:	JSR	BEEP	;BAD KEY, GIVE WARNING NOISE
	JMP	GKEY0	;TRY AGAIN
OK:	STA	IOCHAR	;HOLD ON TO IT

	ADC	RNUM1	;FUTZ WITH RANDOM
	STA	RNUM1	
	EOR	RNUM2	
	STA	RNUM2	
	PLA		; RESTORE
	TAY		; EVERYTHING
	PLA		
	TAX		
	LDA	IOCHAR	; GET CHAR INTO [A]
	RTS		; AND RETURN IT

HAVE:	DB	$08,$15,$0B,$0A,$3C,$5F,$3E,$40,$25,$26,$5E
WANT:	DB	$0B,$07,$0E,$0D,$2C,$2D,$2E,$32,$35,$37,$0E
ENDKEY	EQU	$-WANT-1


; -----------------
; PRINT CHAR IN [A]
; -----------------

CHAR:	STA	IOCHAR	; SAVE HERE
	TXA		; SAVE [X] AND [Y]
	PHA		
	TYA		
	PHA		
	LDA	IOCHAR	

CHAR1:	JSR	MCOUT	
	PLA		; RESTORE [X] AND [Y]
	TAY		
	PLA		
	TAX		
	RTS		


; ---------------------
; FETCH A LINE OF INPUT
; ---------------------
; ENTRY: ABS ADDR OF READ BUFFER IN [ARG1]
; EXIT: # CHARS READ IN [A]

INPUT:	JSR	LINOUT	; FLUSH [LBUFF]
	LDA	#0	; RESET LINE COUNT
	STA	LENGTH	
	STA	CHRCNT	
	LDY	WTOP	
	STY	LINCNT	; RESET LINE COUNT
	DEC	NARGS	; CHECK IF TIME LIMIT (EZIP)
	DEC	NARGS	
	BEQ	INP2	; NO
	LDA	ARG3+LO	; GET DELAY WANTED
	STA	I+HI	
	LDA	#0	
	STA	J+HI	
	STA	J+LO	
	DEC	NARGS	; IS THERE A FCN?
	BEQ	INP5	; NO
	LDA	ARG4+LO	; YES, SET IT
	STA	J+LO	
	LDA	ARG4+HI	
	STA	J+HI	
INP5:	BIT	ANYKEY	; CLEAR STROBE
INP4:	LDA	I+HI	
	STA	I+LO	
INP0:	LDX	#10	; .1 SEC
INP1:	LDA	#$40	; .O1 SEC
	JSR	MWAIT	
	DEX		
	BNE	INP1	
	BIT	KBD	; CHECK FOR KEYSTROKE
	BMI	INP2	; OK, HE'S THERE, CONTINUE
	DEC	I+LO	; 10TH'S OF SECONDS TO WAIT
	BNE	INP0	; SOME TIME LEFT

	;TIME OUT, CHECK FOR A FCN

	LDA	J+LO	; IS THERE A FCN
	ORA	J+HI	
	BNE	INP3	
	JMP	LEXBAD	; NO FCN, LEAVE WITH NOTHING

INP3:	JSR	INTCLL	; INTERNALLY CALL THE FCN
	LDA	VALUE+LO	; CHECK RESULTS
	BEQ	INP4	; ELSE TRY AGAIN (END EZIP)
	JMP	LEXBAD	; ELSE ABORT
INP2:	LDY	#0	; AND CHAR COUNT

INLOOP:	JSR	GETKEY	; GET ASCII INTO [A] AND [IOCHAR]
	CMP	#14	
	BEQ	CBAD	; CLEAR OFF ARROWS (EZIP)
	CMP	#7	
	BEQ	CBAD	
	CMP	#EOL	; EOL?
	BEQ	ENDLIN	; LINE DONE IF SO
	CMP	#BACKSP	; BACKSPACE?
	BEQ	BACKUP	; SPECIAL HANDLING
	CMP	#11	; LEFT ARROW (BACKSPACE)?
	BEQ	BACKUP	
	STA	LBUFF,Y	; ELSE ADD CHAR TO INPUT BUFFER
	INY		; NEXT POSITION IN LINE
SHOWIT:	LDX	INVFLG	
	BPL	SHOW1	; INVERSE
	ORA	#%10000000	; ELSE MAKE IT NORMAL
SHOW1:	JSR	CHAR	; SEND TO SCREEN
	CPY	#77	; ALL THE CHARS ALLOWED?
	BCC	INLOOP	; NO, GET ANOTHER CHAR

	; HANDLE LINE OVERFLOW

NOMORE:	JSR	GETKEY	
	CMP	#EOL	; IF EOL,
	BEQ	ENDLIN	; WRAP UP THE LINE
	CMP	#BACKSP	; BACKSPACE
	BEQ	BACKUP	; IS OKAY TOO
	CMP	#11	; HIGH -
	BEQ	BACKUP	
	JSR	BEEP	; ELSE COMPLAIN
	JMP	NOMORE	; AND INSIST

	; HANDLE BACKSPACE

BACKUP:	DEY		; BACK UP THE POINTER
	BMI	BBAD	
	LDA	#$08	;BACKSP STRANGE SO DO 082008
	JSR	CHAR	
	LDA	#$A0	
	JSR	CHAR	
	LDA	#$08	
	BNE	SHOWIT	;JMP
BBAD:	LDY	#0	; RESET POINTER
CBAD:	JSR	BEEP	; ELSE SCREAM WITH PAIN
	JMP	INLOOP	; AND WAIT FOR SOMETHING BETTER

	; HANDLE END OF LINE

ENDLIN:	LDA	#$8D	; MUST BE THERE FOR PRINTER
	STA	LBUFF,Y	; SHIP EOL TO BUFFER
	INY		; UPDATE INDEX
	STY	LINLEN	; SAVE HERE FOR "READ"
	STY	PRLEN	; AND HERE FOR "PPRINT"
	JSR	CHAR	; AND SEND EOL TO SCREEN

	; MOVE [LBUFF] TO [ARG1] W/LC CONVERSION

LEX0:	LDA	LBUFF-1,Y	; GET A CHAR FROM [LBUFF]
	CMP	#'A'	; IF CHAR IS ALPHA,
	BCC	LEX2	; CONVERT TO LOWER CASE
	CMP	#'Z'+1	
	BCS	LEX2	
	ADC	#$20	
LEX2:	AND	#$7F	
	STA	(RDTBL1),Y	; MOVE CHAR TO INPUT BUFFER AT [ARG1]
	DEY		; LOOP TILL
	BNE	LEX0	; ALL CHARS MOVED
	JSR	PPRINT	; SCRIPT [LBUFF] IF ENABLED
	LDA	LINLEN	; RESTORE # CHARS
	RTS		; INTO [A]
LEXBAD:	LDA	#0	; TIME OUT OCCURRED (EZIP)
	RTS		; ZERO CHARS OBTAINED


; -----------------------
; DIRECT PRINT LINE [X/A]
; -----------------------
; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)
; STRING LENGTH IN [Y]

DLINE:	STX	STRING+LO	; DROP STRING ADDRESS
	STA	STRING+HI	; INTO DUMMY BYTES
	STY	J		; COUNTER
	LDX	#0		; INIT CHAR-FETCH INDEX
DOUT:	DB	$BD		; 6502 "LDA nnnn,X" OPCODE
STRING:	DW	$0000		; DUMMY OPERAND BYTES
	LDY	INVFLG		; INVERSE?
	BPL	DOUT1		; YES
	ORA	#%10000000	; ELSE MAKE IT NORMAL
DOUT1:	JSR	CHAR
	INX
	DEC	J		; LOOP TILL
	BNE	DOUT		; OUT OF CHARS
	RTS		


; -----------------------
; SEND [LBUFF] TO PRINTER
; -----------------------
; ENTRY: LENTH OF LINE IN [PRLEN]

PLEAV:	RTS		

PPRINT:	LDA	SCRIPT	; SCRIPTING INTERNALLY ENABLED?
	BEQ	PLEAV	; NO, SCRAM IMMEDIATELY
	LDA	SCRIPTF	; SCRIPTING ON?
	BEQ	PLEAV	; NO, EXIT
	LDA	CSW+LO	;SAVE NORMAL OUTPUT HOOK
	PHA		
	LDA	CSW+HI	
	PHA		
	LDA	CHZ	;SAVE CURRENT CURSOR POSITION
	PHA		
	LDA	EHZ	; IF 80 COL
	PHA		
	LDA	ALTCSW+LO	;LOAD SCRIPTING HOOK
	STA	CSW+LO	
	LDA	ALTCSW+HI	
	STA	CSW+HI	
	LDA	#0	
	STA	CHZ	
	STA	EHZ	
	LDY	#0	
PP5:	LDA	LBUFF,Y	;GET A CHAR TO SEND OUT
	JSR	MCOUT	
	INY		
	DEC	PRLEN	;LINE COUNT
	BNE	PP5	;PRINT WHOLE LINE

	;ALL DONE, RESET TO NORMAL AND LEAVE

	PLA	;RETRIEVE	CURRENT CURSOR POSITION
	STA	EHZ	
	PLA		
	STA	CHZ	
	PLA		
	STA	CSW+HI	
	PLA		
	STA	CSW+LO	
	RTS		

PSTAT:	DB	0	;SET TO CLEAR WHEN BOOT,
			;I PUT IT HERE SO RESTART WON'T ALTER
ALTCSW:	DB	0,0	;(WORD) PRINTER COUT

	; FIRST TIME USING PRINTER, INITIALIZE IT

PCHK:	LDX	#<SLOTM	;ASK WHICH SLOT PRINTER IS IN
	LDA	#>SLOTM	
	LDY	#SLOTML	
	JSR	DLINE	
	LDA	#0	;ACTUALLY SLOT 1
	JSR	DODEF	;DISPLAY IT AS DEFAULT
	JSR	GETKEY	
	CMP	#EOL	
	BEQ	PC1	;USE DEFAULT
	SEC		
	SBC	#'0'	
	CMP	#8	;1-7
	BCS	PCHK	;OOPS
	BCC	PC2	;SKIP AROUND DEFAULT
PC1:	LDA	#1	;WHICH IS 1
PC2:	CLC		
	ADC	#$C0	
	STA	ALTCSW+HI	
	JSR	PISSER	;SEND HIGH CRLOW  TO SCREEN FOR NEATNESS
	INC	PSTAT	;SET TO ON
	LDA	CSW+LO	;SAVE NORMAL OUTPUT HOOK
	PHA		
	LDA	CSW+HI	
	PHA		
	LDA	ALTCSW+LO	;LOAD SCRIPTING HOOK
	STA	CSW+LO	
	LDA	ALTCSW+HI	
	STA	CSW+HI	
	LDA	#$89	; OUTPUT PRINTER SETUP SEQUENCE
	JSR	MCOUT	; START WITH COMMAND CHAR HIGH CTRL-ILOW 
	LDA	#$B8	; 8 (80 COL WIDE)
	JSR	MCOUT	
	LDA	#$B0	; 0
	JSR	MCOUT	
	LDA	#$CE	; N (LF AFTER CR)
	JSR	MCOUT	
	LDA	CSW+LO	; SAVE REAL PRINTER OUTPUT
	STA	ALTCSW+LO	; LOC. FOR NEXT TIME
	LDA	CSW+HI	
	STA	ALTCSW+HI	
	PLA		; RESET NORMAL OUTPUT
	STA	CSW+HI	
	PLA		
	STA	CSW+LO	

; PRINTER SETUP ON THE IIc COPIES THE ROM INTERRUPT VECTOR
; FROM $FFFE IN ROM TO BOTH MAIN & AUX. RAM PAGES @ $FFFE
; THAT PAGE IN MAIN RAM CONTAINS THE MONITOR STUFF SO IS OK
; BUT THAT PAGE IN AUX. RAM IS BEING USED TO CONTAIN A PAGE
; OF GAME CODE. THE FOLLOWING RTN CAUSES THE NEXT USE OF THAT
; PAGE TO READ IT IN NEW FROM THE DISK SO THAT THE VALUES
; WILL ALL BE CORRECT.  (6/14/85 Le)
;
; THAT PAGE IS SECTOR $4F IN RAMDSK
; AND IT IS NOT USED
; ASK 85

	RTS		


; ------------
; SPLIT SCREEN
; ------------
; SPLIT SCREEN AT LINE [ARG1]
; DISABLE SPLIT IF [ARG1] = 0
; IGNORE IF SPLIT ALREADY ENABLED OR [ARG1] LOW = 20

ZSPLIT:	LDA	ZBEGIN+ZMODE	
	AND	#%00100000	;CHECK IF ENABLED (EZIP)
	BEQ	ZSPOUT	;NOT, LEAVE

	LDA	ARG1+LO	;GET # OF LINES FOR SCREEN
	BEQ	NORL	;IF 0 THEN RESTORE SCREEN
	CMP	#24	;IS SPLIT REALLY = WHOLE SCREEN
	BCS	ZSPOUT	;YES, IGNORE
	LDX	SPSTAT	;CHECK IF ALREADY ENABLED
	BEQ	ZSPL1	; NO
	LDA	WTOP	; BOTTOM OF SCREEN 1
	SEC		
	SBC	ARG1+LO	; COMPARE NEW SIZE, IF EXISTING SIZE
	BCS	ZSPL2	; LOW = DON'T CLEAR, SCREEN SHRINKING
ZSPL1:	LDA	ARG1+LO	; CLEAR LINES OF NEW, OR BEING ADDED
	STA	SPSTAT	;NON ZERO = SCREEN IS SPLIT
ZSPL2:	LDA	#$18	;RESTORE BOTTOM FOR SCROLL
	STA	WBOTM	
	LDA	ARG1+LO	; GET BOTTOM OF SPLIT SCREEN (AGAIN)
	STA	WTOP	;MAKE THAT THE TOP OF THE SECOND SCREEN
	CMP	LINCNT	; IS SCROLLING SCREEN NOW LESS THAN LINCNT?
	BCC	ZSPL3	; NO
	STA	LINCNT	; YES
ZSPL3:	LDA	#0	
	STA	CHZ	
	STA	EHZ	
	LDA	#$17	
	STA	CV	;RESTORE CURSOR AFTER HOME CALL
	JSR	BASCAL	;HIGH CRLOW  TO MAKE CV WORK
ZSPOUT:	RTS		

NORL:	;RESTORE SCREEN	TO FULL SCREEN MODE

	LDA	#0	;PUT CURSOR AT TOP OF SCREEN
	STA	WTOP	;RESTORE FULL SCREEN ALIGNMENT
	STA	LINCNT	
	STA	SPSTAT	;FLAG NOT SPLIT
	RTS		


; ------
; SCREEN
; ------
; GO TO TOP WINDOW (TOP OF SCREEN) IF [A] = 1
; GO TO BOTTOM OF SCREEN IF [A] = 0
; IGNORE IF SPLIT NOT ENABLED OR [A] HIGH LOW  0 OR 1
; FLAG SPLITF WILL BE SET FOR OUTPUT TO DETERMINE
; IF AND WHICH WINDOW TO DISPLAY TO
; (0=BOTTOM 1=TOP)

ZSCRN:	LDA	ZBEGIN+ZMODE	
	AND	#%00000001	;CHECK IF ENABLED (EZIP)
	BEQ	ZSPOUT	;NOT, LEAVE
	LDA	SPSTAT	;CHECK IF SCREEN IS SPLIT
	BEQ	ZSPOUT	;NO, SO JUST LEAVE
	LDA	ARG1+LO	;CHECK WHICH WINDOW
	BNE	SCRN1	;TOP SCREEN
	LDA	#$FF	; SCROLLING SCREEN SO
	STA	SCRIPT	; ALLOW SCRIPTING
	LDA	#0	
	STA	SPLITF	;SET FLAG TO SPLIT SCREEN (0)

	LDA	LENGTH	; get saved cursor horizontal
	STA	EHZ	; and use it
	STA	CHZ	; and use it

	LDA	#$17	; assume last line (?)
	STA	CV	

	JMP	SCRNP	;JMP TO HIGH CRLOW  RTN
SCRN1:	CMP	#01	
	BNE	ZSPOUT	;INVALID SCREEN ID
	STA	SPLITF	;SET FLAG TO UNSCROLLING SCREEN (1)
	LDA	#0	
	STA	SCRIPT	; SET SCRIPTNG OFF, NOT ALLOWED THIS SCREEN
	STA	CHZ	
	STA	EHZ	
	STA	CV	;ALIGN AT TOP OF SCREEN
SCRNP:	JMP	BASCAL	;SET CURSOR (+ LEAVE)


; ------------
; CLEAR SCREEN
; ------------

CLS	EQU	HOME	;CLEAR & HOME CURSOR

PISSER:	LDA	#$8D	
	JMP	MCOUT	


; -----
; SOUND
; -----
; ARG1 = BOOP (2) BEEP (1) ALL OTHERS INVALID
; (EZIP)

ZSOUND:	LDA	ZBEGIN+ZMODE	
	AND	#%00100000	
	BEQ	ZSOEX	; NOT ENABLED
	LDX	ARG1+LO	; GET SOUND WANTED
	DEX		
	BEQ	BEEP	
	DEX		
	BNE	ZSOEX	; INVALID
	LDY	#$FF	; DURATION ($C0 = .1 SEC)
BOOP:	LDA	#$10	; TONE ($0C = 1 KHZ)
	JSR	MWAIT	
	LDA	SPKR	; TOGGLE SPEAKER
	DEY		
	BNE	BOOP	
ZSOEX:	RTS		

BEEP:	JMP	BELL	; QUICK BEEP ( 1KHZ @ .1 SEC)


	; ------------------------
	; CHECK IF 128 K OF MEMORY
	; ------------------------

	; EXIT: CARRY SET IF NOT 128K, CLEAR IF THERE IS

MEMCHK:	LDA	#0	; START @ 0
	STA	TSTVAL	
MLOOP1:	LDY	#0	
MLOOP2:	STA	$1000,Y	; WRITE [TSTVAL] TO
	INY		; MAIN MEMORY
	BNE	MLOOP2	
	INC	TSTVAL	
	LDA	TSTVAL	
	STA	WRTBNK+AUX	; SET TO AUX MEMORY

MLOOP3:	STA	$1000,Y	; WRITE NEXT VALUE
	INY		; TO AUX MEMOYR
	BNE	MLOOP3	
	STA	WRTBNK+MAIN	; SET TO MAIN MEMORY
	DEC	TSTVAL	; RESET TO [TSTVAL] WRITTEN TO MAIN

MLOOP4:	LDA	$1000,Y	; CHECK IF WHAT WRITTEN
	CMP	TSTVAL	; 1ST TO MAIN MEM PG $10
	BNE	NO128	; IS STILL THERE
	INY		
	BNE	MLOOP4	
	INC	TSTVAL	
	STA	RDBNK+AUX	; & SET TO AUX MEMORY

MLOOP5:	LDA	$1000,Y	; & SEE IF WHAT WROTE
	CMP	TSTVAL	; TO AUX MEM IS STILL THERE
	BNE	NO128	
	INY		
	BNE	MLOOP5	
	STA	RDBNK+MAIN	; RESET TO MAIN MEM
	LDA	TSTVAL	; LAST THING WAS INC'D
	BNE	MLOOP1	; GO TRY W/ NEXT SET OF VALUES, DO 0,1 -LOW  FF,O
	CLC		
	RTS		; SIGNAL OK
NO128:	STA	RDBNK+MAIN	; RESET TO MAIN MEM
	SEC		
	RTS		; OOPS, ONLY 64K

	END

