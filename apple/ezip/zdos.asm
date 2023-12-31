	PAGE	
	SBTTL "--- Z-DOS: APPLE II ---"


; --------------------
; READ A VIRTUAL BLOCK
; --------------------
; ENTRY: V-BLOCK TO READ IN [DBLOCK]
; BUFFER ADDRESS IN [DBUFF]
; DSKBNK SAYS WHERE TO PUT PAGE (AUX OR MAIN)
; EXIT: DATA AT [DBUFF]

GETDSK:	LDA	#0		; CLEAR TRACK
	STA	DCBTRK		; USE AS COUNTER
	LDX	DBLOCK+HI	
	LDY	DBLOCK+LO	
	CPX	#>PSIDE1	;IS THE BLOCK ON SIDE 1 OR 2
	BCC	GETSD1	
	BNE	GETSD2	
	CPY	#<PSIDE1	;DONT KNOW YET
	BCC	GETSD1	

	; SUBTRACT # PAGES ON SIDE 1 TO
	; GET OFFSET INTO SIDE 2

GETSD2:	LDA	SIDEFLG		;ON SIDE 2?
	CMP	#2	
	BEQ	GETSDOK		;YES
	JSR	SIDE2		;NO, ASK FOR IT
	LDX	DBLOCK+HI	
	LDY	DBLOCK+LO	
GETSDOK:TYA		
	SEC		
	SBC	#<PSIDE1	;C=1
	TAY		
	TXA		
	SBC	#>PSIDE1	
	TAX		
	TYA		
	SEC		
SDL1:	SBC	#18		; DIVIDE BY SECTORS PER TRACK
	BCS	S1	
	DEX			; BORROW
	BMI	S2		; ALL GONE
	SEC		
S1:	INC	DCBTRK		; INCREMENT TRACK
	BCS	SDL1		; CONTINUE
S2:	CLC		
	ADC	#18		; ADD BACK INTO GET SECTOR #
	STA	DCBSEC	
	LDA	DCBTRK		; CHECK TRACK SIZE
	CMP	#ZTRKL		; ANYTHING ABOVE TRK 34
	BCC	OKA	
	JMP	TRKERR	
OKA:			
	LDA	#FREAD		; READ SIDE 2 INDICATOR
	BNE	GETSDGO		; GO VALIDATE TRACK

	; CONVERT FOR SIDE 1

GETSD1:	LDA	SIDEFLG		; ARE WE ON SIDE 1
	CMP	#1	
	BEQ	OK3	
	JSR	SIDE1	
	LDX	DBLOCK+HI	
	LDY	DBLOCK+LO	
OK3:	TYA			; GET LSB OF BLOCK ID
	AND	#%00001111	; MASK OFF TOP NIBBLE
	STA	DCBSEC		; TO FORM DCBSEC ID (0-15)
	TXA			; GET MSB OF BLOCK ID
	ASL	A		; SHIFT BOTTOM NIB INTO TOP NIB
	ASL	A		
	ASL	A		
	ASL	A		
	STA	DCBTRK		; SAVE HERE FOR A MOMENT
	TYA			; GET LSB
	LSR	A		; MOVE TOP TO BOTTOM
	LSR	A		
	LSR	A		
	LSR	A		
	ORA	DCBTRK		; SUPERIMPOSE TOP OF MSB
	CLC			; PRE-LOAD Z-BLOCKS START ON
	ADC	#ZTRKF		; TRACK 3 (EZIP)
	CMP	#ZTRKL		; ANYTHING ABOVE TRACK 34
	BCS	TRKERR	
	STA	DCBTRK		; THIS IS THE TRACK ID
SDL2:	LDA	#READ	
GETSDGO:			
	STA	RDBNK+MAIN	;SET TO READ FROM MAIN BANK

	;WHERE BUFFA AND BUFFB ARE

	JSR	DOS	
	BCS	DISKERR		;BAD READ

	; SOMEDAY DOS SHOULD TAKE CARE OF
	; READING/WRITING FROM EITHER BANK
	; IN THE 48K
	; JUST MAKE SHURE THERE ARE NO BUFFA
	; BUFFB CONFLICTS
	; DOS EVENTUALLY

	LDY	DSKBNK	
	STA	WRTBNK,Y	;SET TO WRITE TO DSKBNK
	LDY	#0		; MOVE DATA
SDLP3:	LDA	IOBUFF,Y	; IN [IOBUFF]
	STA	(DBUFF),Y	; TO [DBUFF]
	INY		
	BNE	SDLP3	

	; RESET TO WRITE TO MAIN BANK

	STA	WRTBNK+MAIN	

	INC	DBLOCK+LO	; POINT TO NEXT
	BNE	S5		; VIRTUAL BLOCK
	INC	DBLOCK+HI	
S5:	INC	DBUFF+HI	
	LDA	DBUFF+HI	
	CMP	#MAINEND+1	;PAST LAST MAIN RAM PAGE ?
	BCC	S6		;NO
	LDA	#AUXSTART	;RESET DBUFF TO FIRST AUX PAGE
	STA	DBUFF+HI	
	LDA	#AUX		;SET DSKBNK TO AUX
	STA	DSKBNK	
S6:	RTS		

	; DISKERR SHOULD FIRST ASK IF
	; SIDE [SIDEFLG] IS IN, THEN
	; IT SHOULD RETRY AND IF
	; IT FAILS AGAIN, THEN IT SHOULD
	; ERROR 14 OUT
	; BUT FOR RIGHT NOW STUPIDITY
	; SHALL REMAIN A FATAL FLAW

DISKERR:	LDA	#14	
	JMP	ZERROR		; DRIVE ACCESS ERROR

	; POINT TO NEXT SECTOR

NXTSEC:	INC	DCBSEC		; UPDATE SECTOR
	LDA	DCBSEC		; CHECK IT
	AND	#%00001111	; DID IT OVERFLOW?
	BNE	SECTOK		; NO, ALL'S WELL
	LDX	DCBTRK		; ELSE UPDATE
	INX			; TRACK ID
	CPX	#ZTRKL		; IF LOW  35,
	BCS	WRTERR		; SCRAM W/CARRY SET
	STX	DCBTRK		; ELSE SAVE NEW TRACK
SECTOK:	STA	DCBSEC		; AND SECTOR
	INC	DBUFF+HI	; POINT TO NEXT RAM PAGE
	CLC			; CLEAR CARRY FOR SUCCESS (WRITE ONLY)
	RTS		


; ----------------------
; WRITE [DBLOCK] TO DISK
; ----------------------
; ENTRY: TRACK,SECTOR,DRIVE,SLOT ALL SET ALREADY
; PAGE TO WRITE IN (DBUFF)
; EXIT: CARRY CLEAR IF OKAY, SET IF FAILED

PUTDSK:	LDY	#0	; MOVE DATA AT [DBUFF]
	STA	RDBNK+MAIN	;SELECT MAIN BANK
PDSK0:	LDA	(DBUFF),Y	; TO [IOBUFF]
	STA	IOBUFF,Y	; FOR WRITING
	INY		
	BNE	PDSK0	
	LDA	#WRITE	
	JSR	DOS	; DO IT!
	BCC	NXTSEC	; OKAY IF CARRY CLEAR
WRTERR:	RTS		; ELSE EXIT WITH CARRY SET

	; *** ERROR #12: DISK ADDRESS RANGE ***

TRKERR:	LDA	#12	
	JMP	ZERROR	

	; *** ERROR #14: DRIVE ACCESS ***

DSKER:	LDA	#14	
	JMP	ZERROR	


; ---------------------
; READ DBLOCK FROM DISK
; ---------------------
; CALLED BY RESTORE
; ENTER: (W/[DCBSEC/TRK] PRESET)

GETRES:	LDA	#READ	
	JSR	DOS	
	BCS	DSKER	; FATAL ERROR IF CARRY SET
	LDY	#0	; MOVE DATA
	STA	RDBNK+MAIN	
RES1:	LDA	IOBUFF,Y	; IN [IOBUFF]
	STA	(DBUFF),Y	; TO [DBUFF]
	INY		
	BNE	RES1	
	INC	DBLOCK+LO	; POINT TO NEXT
	BNE	RES2	; VIRTUAL BLOCK
	INC	DBLOCK+HI	

	; POINT TO NEXT SECTOR

RES2:	INC	DCBSEC	; UPDATE SECTOR
	LDA	DCBSEC	; CHECK IT
	AND	#%00001111	; DID IT OVERFLOW?
	BNE	RES3	; NO, ALL'S WELL
	LDX	DCBTRK	; ELSE UPDATE
	INX		; TRACK ID
	CPX	#ZTRKL	; IF LOW  35,
	BCS	WRTERR	; SCRAM W/CARRY SET
	STX	DCBTRK	; ELSE SAVE NEW TRACK
RES3:	STA	DCBSEC	; AND SECTOR
	INC	DBUFF+HI	; POINT TO NEXT RAM PAGE
	CLC		; CLEAR CARRY FOR SUCCESS (WRITE ONLY)
	RTS		


; -----------------------------
; SET UP SAVE & RESTORE SCREENS
; -----------------------------

SAVRES:	JSR	ZCRLF	; CLEAR THE LINE BUFFER
	LDA	#0	
	STA	SCRIPT	; DISABLE SCRIPTING
	RTS

;	JMP	HOME	; CLEAR THE SCREEN (LEAVE STATUS AS IS - EZIP)


; -----------------
; DISPLAY A DEFAULT
; -----------------
; ENTRY: DEFAULT (1-8) IN [A]

DEFAL:	DB	" (Default is "
DEFNUM:	DB	"*) >"
DEFALL	EQU	$-DEFAL	

DODEF:	CLC		
	ADC	#'1'	; CONVERT TO DB	II 0-7
	STA	DEFNUM	; INSERT IN STRING
	LDX	#<DEFAL	
	LDA	#>DEFAL	
	LDY	#DEFALL	
	JMP	DLINE	; PRINT THE STRING


; -----------------------------
; GET SAVE & RESTORE PARAMETERS
; -----------------------------

NUMSAV	DB	0		; HOLDS # SAVES AVAILABLE

POSIT:	DB	EOL	
	DB	"Position 1-"
SAVASC:	DB	"*"
POSITL:	EQU	$-POSIT	
WDRIV:	DB	EOL	
	DB	"Drive 1 or 2"
WDRIVL	EQU	$-WDRIV	
SLOT:	DB	EOL	
	DB	"Slot 1-7"
SLOTL	EQU	$-SLOT	
GSLOT:	DB	5	;START W/ DEFAULT SLOT 6 (YES 5 IS 6)
MIND:	DB	EOL	
	DB	EOL	
	DB	"Position "
MPOS:	DB	"*; Drive #"
MDRI:	DB	"*; Slot "
MSLT:	DB	"*."	
	DB	EOL	
	DB	"Are you sure? (Y/N) >"
MINDL	EQU	$-MIND	
INSM:	DB	EOL	
	DB	"Insert SAVE disk into Drive #"
SAVDRI:	DB	"*."	
INSML	EQU	$-INSM	
YES:	DB	"YES"
	DB	EOL	
YESL	EQU	$-YES	
NO:	DB	"NO"	
	DB	EOL	
NOL	EQU	$-NO	


PARAMS:	LDX	#<POSIT	
	LDA	#>POSIT	
	LDY	#POSITL	
	JSR	DLINE	; "POSITION (1-X)"

	; GET GAME SAVE POSITION

	LDA	GPOSIT	; SHOW THE CURRENT
	JSR	DODEF	; DEFAULT POSITION

GETPOS:	JSR	GETKEY	; WAIT FOR A KEY
	CMP	#EOL	; IF [RETURN],
	BEQ	POSSET	; USE DEFAULT
	SEC		
	SBC	#'1'	; ELSE CONVERT DB	II TO BINARY
	CMP	NUMSAV	; IF BELOW [NUMSAV]
	BCC	SETPOS	; MAKE IT THE NEW DEFAULT
	JSR	BEEP	; ELSE RAZZ
	JMP	GETPOS	; AND TRY AGAIN
POSSET:	LDA	GPOSIT	; USE DEFAULT
SETPOS:	STA	TPOSIT	; USE KEYPRESS
	CLC		
	ADC	#'1'	; CONVERT TO DB	II "1"-"5"
	STA	MPOS	; STORE IN TEMP STRING
	STA	SVPOS	
	STA	RSPOS	
	ORA	#%10000000	
	JSR	CHAR	; AND DISPLAY IT

	; GET DRIVE ID

	LDX	#<WDRIV	
	LDA	#>WDRIV	
	LDY	#WDRIVL	
	JSR	DLINE	; "DRIVE 1 OR 2"
	LDA	GDRIVE	; SHOW DEFAULT
	JSR	DODEF	

GETDRV:	JSR	GETKEY	; GET A KEYPRESS
	CMP	#EOL	; IF [RETURN],
	BEQ	DRVSET	; USE DEFAULT
	SEC		
	SBC	#'1'	; CONVERT TO BINARY 0 OR 1
	CMP	#2	; IF WITHIN RANGE,
	BCC	SETDRV	; SET NEW DEFAULT
	JSR	BEEP	
	JMP	GETDRV	; ELSE TRY AGAIN
DRVSET:	LDA	GDRIVE	; USE DEFAULT
SETDRV:	STA	TDRIVE	; USE [A]
	CLC		
	ADC	#'1'	; CONVERT TO DB	II 1 OR 2
	STA	SAVDRI	; STORE IN DRIVE STRING
	STA	MDRI	; AND IN TEMP STRING
	ORA	#%10000000	
	JSR	CHAR	; AND SHOW NEW SETTING

	;IF IIC SLOT IS 6 OTHERWISE ASK

	LDA	SIG	; CHECK IF IIc
	BNE	PREIIC	; IS NOT A IIC SO ASK WHICH SLOT
	LDA	#5	; SLOT 6
	BNE	SETSLT	; JMP
PREIIC:	LDX	#<SLOT	
	LDA	#>SLOT	
	LDY	#SLOTL	
	JSR	DLINE	; "SLOT 1-7"

	; GET DRIVE SLOT

	LDA	GSLOT	; SHOW THE CURRENT
	JSR	DODEF	; DEFAULT SLOT
GETSLT:	JSR	GETKEY	; WAIT FOR A KEY
	CMP	#EOL	; IF [RETURN],
	BEQ	SLTSET	; USE DEFAULT
	SEC		
	SBC	#'1'	; ELSE CONVERT DB	II TO BINARY
	CMP	#7	; IF "7" OR BELOW
	BCC	SETSLT	; MAKE IT THE NEW DEFAULT
BADSLT:	JSR	BEEP	; ELSE RAZZ
	JMP	GETSLT	; AND TRY AGAIN
SLTSET:	LDA	GSLOT	; USE DEFAULT
SETSLT:	STA	TSLOT	; USE KEYPRESS
	CLC		
	ADC	#'1'	; CONVERT TO DB	II "1"-"7"
	STA	MSLT	; STORE IN TEMP STRING
	LDX	SIG	; AND IF NOT IIC
	BEQ	DBLCHK	
	ORA	#%10000000	
	JSR	CHAR	; DISPLAY IT

DBLCHK:	LDX	#<MIND	; SHOW TEMPORARY SETTINGS
	LDA	#>MIND	
	LDY	#MINDL	
	JSR	DLINE	

	; VALIDATE RESPONSES

GETYN:	JSR	GETKEY	
	CMP	#'y'	; IF REPLY IS "Y"
	BEQ	ALLSET	; ACCEPT RESPONSES
	CMP	#'Y'	
	BEQ	ALLSET	
	CMP	#EOL	; EOL IS ALSO ACCEPTABLE
	BEQ	ALLSET	
	CMP	#'n'	; IF REPLY IS "N"
	BEQ	NOTSAT	; RESTATE PARAMETERS
	CMP	#'N'	
	BEQ	NOTSAT	
	JSR	BEEP	; ELSE BEEP
	JMP	GETYN	; INSIST ON Y OR N

NOTSAT:	LDX	#<NO	
	LDA	#>NO	
	LDY	#NOL	
	JSR	DLINE	; PRINT "NO"/EOL
	JMP	PARAMS	; AND TRY AGAIN

ALLSET:	LDX	#<YES	
	LDA	#>YES	
	LDY	#YESL	
	JSR	DLINE	; PRINT "YES"/EOL
	LDA	TDRIVE	; MAKE THE TEMPORARY DRIVE
	STA	DCBDRV	; AND SET [DRIVE] ACCORDINGLY
	INC	DCBDRV	; 1-ALIGN THE DRIVE ID
	LDX	TSLOT	; MAKE TEMP DRIVE SLOT
	INX		; 1-ALIGN
	TXA		
	ASL	A	; * 16 FOR # RWTS NEEDS
	ASL	A		
	ASL	A		
	ASL	A		
	STA	DCBSLT	; AND SET SLOT ACCORDINGLY

; CALC STARTING SECTOR & TRACK (BM 1/20/86)

	LDA	TPOSIT		; GET THE SAVE POSITION
	LDX	NUMSAV		; AND # SAVES AVAILABLE
	CPX	#3		; IF 3 SAVES,
	BEQ	GOOX		; NO OFFSET NECESSARY

	CLC			; ELSE JUMP INDEX
	ADC	#3		; OVER 1ST 3 ENTRIES

GOOX:	TAX			; USE MUNGED [TPOSIT] AS AN INDEX
	LDA	TRAX,X		; INTO TABLES
	STA	DCBTRK		; FOR 1ST TRACK
	LDA	SEX,X		; AND SECTOR
	STA	DCBSEC

	LDX	#<INSM	
	LDA	#>INSM	
	LDY	#INSML	
	JSR	DLINE	; "INSERT SAVE DISK IN DRIVE X."

; ---------------------
; "PRESS RETURN" PROMPT
; ---------------------

RETURN:	LDX	#<RTN	
	LDA	#>RTN	
	LDY	#RTNL	
	JSR	DLINE	; SHOW PROMPT

	; ENTRY FOR QUIT/RESTART

GETRET:	JSR	GETKEY	; WAIT FOR [RETURN]
	CMP	#EOL	
	BEQ	GRRTS	
	JSR	BEEP	; ACCEPT NO
	JMP	GETRET	; SUBSTITUTES!
GRRTS:	RTS		

RTN:	DB	EOL	
	DB	"Press [RETURN] to continue."
	DB	EOL	
	DB	EOL	
	DB	EOL	
RTNL	EQU	$-RTN

TRAX:	DB	0,11,23		; 1ST TRACK FOR 3-SAVE DISKS
	DB	0,8,17,25	; 1ST TRACK FOR 4-SAVE DISKS
SEX:	DB	0,8,0		; 1ST SECTOR FOR 3-SAVE DISKS
	DB	0,8,0,8		; 1ST SECTOR FOR 4-SAVE DISKS

; --------------------
; PROMPT FOR GAME DISK
; --------------------
; EZIP USES BOTH SIDES OF DISK

GAME:	DB	EOL	
	DB	"Insert Side "
DSIDE:	DB	"* of the STORY disk into Drive #1."
	DB	EOL	
	DB	EOL	
	DB	EOL	
GAMEL	EQU	$-GAME	

SIDE1:	LDA	#'1'	; ASK FOR SIDE 1
	STA	DSIDE	
	LDA	#1	;SET FOR SUCCESS
	STA	SIDEFLG	
SL1:	LDX	#<GAME	
	LDA	#>GAME	
	LDY	#GAMEL	
	JSR	DLINE	; "INSERT STORY DISK"
	JSR	RETURN	; "PRESS [RETURN] TO CONTINUE:"
	LDA	#0	; GO READ TRK 0, SEC 0, & SEE
	STA	DCBSEC	; IF THEY DID SWAP
	STA	DCBTRK	
	LDA	#1	; MAKE SURE WE'RE ON
	STA	DCBDRV	; THE BOOT DRIVE
	LDA	#READ	
	JSR	DOS	
	BCS	SL1	; DISK READ ERROR, WRONG FORMAT, SO: WRONG SIDE
	BCC	ASK2	; JUMP, GOOD
SIDE2:	LDA	#'2'	; ASK FOR SIDE 2
	STA	DSIDE	
	LDA	#2	
	STA	SIDEFLG	;SET FOR SUCCESS
	LDA	DCBDRV	; GET LAST DRIVE USED
	PHA		; HOLD IT A SEC
	LDA	#1	; MAKE SURE WE'RE ON
	STA	DCBDRV	; THE BOOT DRIVE
	PLA		; IF SAVED/RESTORED
	CMP	#2	; TO DRIVE 2, DON'T ASK
	BEQ	ASK2	; NOTE: THIS IS OK W/ VERIFY CAUSE

	; ASKS FOR SIDE 1 FIRST, RESETTING
	; DRIVE TO 1

SL2:	LDX	#<GAME	
	LDA	#>GAME	
	LDY	#GAMEL	
	JSR	DLINE	; "INSERT STORY DISK"
	JSR	RETURN	; "PRESS [RETURN] TO CONTINUE:"
	LDA	#0	; GO READ TRK 0, SEC 0, & SEE
	STA	DCBSEC	; IF THEY DID SWAP
	STA	DCBTRK	
	LDA	#FREAD	
	JSR	DOS	
	BCS	SL2	; DISK READ ERROR, WRONG FORMAT, SO: WRONG SIDE
ASK2:	LDA	#$FF	; RE-ENABLE
	STA	SCRIPT	; SCRIPTING
	RTS		

; ---------
; SAVE GAME
; ---------

SAV:	DB	"Save Position"
	DB	EOL	
SAVL	EQU	$-SAV	
SVING:	DB	EOL	
	DB	EOL	
	DB	"Saving position "
SVPOS:	DB	"* ..."
	DB	EOL	
	DB	EOL	
	DB	EOL	
SVINGL	EQU	$-SVING	

ZSAVE:	JSR	SAVRES		; SET UP SCREEN
	LDX	#<SAV	
	LDA	#>SAV	
	LDY	#SAVL	
	JSR	DLINE		; "SAVE POSITION"
	JSR	PARAMS		; GET PARAMETERS
	LDX	#<SVING	
	LDA	#>SVING	
	LDY	#SVINGL	
	JSR	DLINE		; "SAVING POSITION X ..."

	; SAVE GAME PARAMETERS IN [BUFSAV]

	LDA	ZBEGIN+ZID	; MOVE GAME ID
	STA	BUFSAV+0	; INTO 1ST 2 BYTES
	LDA	ZBEGIN+ZID+1	; OF THE AUX LINE BUFFER
	STA	BUFSAV+1	
	LDA	ZSP+LO		; MOVE [ZSP]
	STA	BUFSAV+2	
	LDA	ZSP+HI	
	STA	BUFSAV+3	
	LDA	OLDZSP+LO	
	STA	BUFSAV+4	
	LDA	OLDZSP+HI	; MOVE [OLDZSP]
	STA	BUFSAV+5	
	LDX	#2		; MOVE CONTENTS OF [ZPC]
ZSL1:	LDA	ZPC,X		; TO BYTES 7-9
	STA	BUFSAV+6,X	; OF [BUFSAV]
	DEX		
	BPL	ZSL1	

	; WRITE [LOCALS]/[BUFSAV] PAGE TO DISK

	LDA	#>LOCALS	
	STA	DBUFF+HI	; POINT TO THE PAGE
	JSR	PUTDSK	; AND WRITE IT OUT
	BCC	ZSOK	; IF SUCCEEDED, WRITE STACK

ZSBAD:	JSR	SIDE2	; ELSE REQUEST STORY DISK
;	JSR	CLS	
	LDA	#22	; SET CURSOR AT BOTTOM OF SCREEN
	STA	CV	
	JSR	BASCAL	
	JMP	RET0	; AND FAIL

	; WRITE CONTENTS OF Z-STACK TO DISK

ZSOK:	LDA	#>ZSTKBL	; POINT TO 1ST PAGE
	STA	DBUFF+HI	
	LDA	#4	; DO ALL 4 PAGES
	STA	L	; SET COUNTER
ZSOKLP:	JSR	PUTDSK	; WRITE THEM
	BCS	ZSBAD	
	DEC	L	
	BNE	ZSOKLP	

	; WRITE ENTIRE GAME PRELOAD TO DISK

	LDA	ZCODE		; POINT TO 1ST PAGE
	STA	DBUFF+HI	; OF PRELOAD
	LDX	ZBEGIN+ZPURBT	; GET # IMPURE PAGES
	INX			; USE FOR INDEXING
	STX	I+LO	
ZSL2:	JSR	PUTDSK	
	BCS	ZSBAD	
	DEC	I+LO	
	BNE	ZSL2	
	JSR	SIDE2		; PROMPT FOR GAME DISK
;	JSR	CLS	
	LDA	#22	
	STA	CV	
	JSR	BASCAL	
	LDA	TDRIVE	; IF SAVE SUCCESSFUL
	STA	GDRIVE	; SAVE PARAMS FOR
	LDA	TSLOT	; NEXT TIME
	STA	GSLOT	
	LDA	TPOSIT	
	STA	GPOSIT	
	LDA	#1	; SET TO MARK
	LDX	#0	
	JMP	PUTBYT	; SUCCESS


; ------------
; RESTORE GAME
; ------------

RES:	DB	"Restore Position"
	DB	EOL	
RESL	EQU	$-RES	
RSING:	DB	EOL	
	DB	EOL	
	DB	"Restoring position "
RSPOS:	DB	"* ..."
	DB	EOL	
	DB	EOL	
	DB	EOL	
RSINGL	EQU	$-RSING	

ZREST:	JSR	SAVRES
	LDX	#<RES
	LDA	#>RES
	LDY	#RESL
	JSR	DLINE		; "RESTORE POSITION"
	JSR	PARAMS		; GET PARAMETERS
	LDX	#<RSING
	LDA	#>RSING
	LDY	#RSINGL
	JSR	DLINE		; "RESTORING POSITION X ..."

	; SAVE LOCALS IN CASE OF ERROR

	LDX	#31	
ZRL1:	LDA	LOCALS,X	; COPY ALL LOCALS
	STA	$0100,X		; TO BOTTOM OF MACHINE STACK
	DEX		
	BPL	ZRL1	
	LDA	#>LOCALS	
	STA	DBUFF+HI	
	JSR	GETRES		; RETRIEVE 1ST BLOCK OF PRELOAD
	BCS	ZRBAD		; BAD DISK READ IF CARRY CLEAR
	LDA	BUFSAV+0	; DOES 1ST BYTE OF SAVED GAME ID
	CMP	ZBEGIN+ZID	; MATCH THE CURRENT ID?
	BNE	ZRBAD		; WRONG DISK IF NOT
	LDA	BUFSAV+1	; WHAT ABOUT THE 2ND BYTE?
	CMP	ZBEGIN+ZID+1	
	BEQ	ZROK		; CONTINUE IF BOTH BYTES MATCH

	; HANDLE RESTORE ERROR

ZRBAD:	LDX	#31		; RESTORE ALL SAVED LOCALS
ZRL2:	LDA	$0100,X	
	STA	LOCALS,X	
	DEX		
	BPL	ZRL2	
	JSR	SIDE2		; PROMPT FOR GAME DISK
;	JSR	CLS	
	LDA	#22		; SET CURSOR AT BOTTOM OF SCREEN
	STA	CV	
	JSR	BASCAL	
	JMP	RET0		; PREDICATE FAILS

	; CONTINUE RESTORE

ZROK:	LDA	ZBEGIN+ZSCRIP	; SAVE BOTH FLAG BYTES
	STA	I+LO	
	LDA	ZBEGIN+ZSCRIP+1	
	STA	I+HI	
	LDA	#>ZSTKBL	; RETRIEVE OLD CONTENTS OF
	STA	DBUFF+HI	; Z-STACK
	LDA	#4		; DO 4 PAGES
	STA	L		; SET COUNTER
ZROKLP:	JSR	GETRES		; GET 4 PAGES OF Z-STACK
	BCS	ZRBAD	
	DEC	L	
	BNE	ZROKLP	

	LDA	ZCODE	
	STA	DBUFF+HI	
	JSR	GETRES		; GET 1ST BLOCK OF PRELOAD
	BCS	ZRBAD	
	LDA	I+LO		; RESTORE THE STATE
	STA	ZBEGIN+ZSCRIP	; OF THE FLAG WORD
	LDA	I+HI	
	STA	ZBEGIN+ZSCRIP+1	
	LDA	ZBEGIN+ZPURBT	; GET # PAGES TO LOAD
	STA	I+LO	
ZRL3:	JSR	GETRES		; FETCH THE REMAINDER
	BCS	ZRBAD	
	DEC	I+LO		; OF THE PRELOAD
	BNE	ZRL3	

	; RESTORE THE STATE OF THE SAVED GAME

	LDA	BUFSAV+2	; RESTORE THE [ZSP]
	STA	ZSP+LO	
	LDA	BUFSAV+3	
	STA	ZSP+HI	
	LDA	BUFSAV+4	
	STA	OLDZSP+LO	
	LDA	BUFSAV+5	; AND THE [OLDZSP]
	STA	OLDZSP+HI	
	LDX	#2	; RESTORE THE [ZPC]
ZRL4:	LDA	BUFSAV+6,X	
	STA	ZPC,X	
	DEX		
	BPL	ZRL4	
	JSR	VLDZPC	;MAKE VALID
	JSR	SIDE2	; PROMPT FOR GAME DISK
;	JSR	CLS	

	LDA	#22	
	STA	CV	
	JSR	BASCAL	

	LDA	TDRIVE	; IF RESTORE SUCCESSFUL
	STA	GDRIVE	; SAVE PARAMS FOR
	LDA	TSLOT	; NEXT TIME
	STA	GSLOT	
	LDA	TPOSIT	
	STA	GPOSIT	
	LDA	#2	; SET TO
	LDX	#0	
	JMP	PUTBYT	; SUCCESS

	END
