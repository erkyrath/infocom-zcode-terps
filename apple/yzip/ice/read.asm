	TITLE	"Apple ][ YZIP (c)Infocom","READ HANDLER"
; ----
; READ
; ----
; READ LINE INTO TABLE [ARG1] ; PARSE INTO TABLE [ARG2] (IF ARG2 IS THERE)

MAXWORDS DB	0		; maximum number of words in table
WORDCNT	DB	0		; how many words read so far
WORDSTART DB	0		; table offset of word
SAVESPC	DS	3		; SPC that points to Word Count

ZREAD:
	lda	ARG1+HI		; MAKE THE TABLE ADDRESSES
	sta	RDTBL1+HI	; AND PLACE IT HERE TO USE
	lda	ARG1+LO
	sta	RDTBL1+LO	; LSBS NEED NOT CHANGE

	lda	#0		; TURN OFF FLAGS
	sta	PSVFLG		; FOR ZLEX
	sta	VOCFLG

	ldx	NARGS
	dex			; IF 2ND TBL ADDR 0 OR NOT THERE
	beq	ONLYRD		; JUST READ IN DON'T DO CONVERSION (X)
	ldx	#0		; JIC
	lda	ARG2+HI
	ora	ARG2+LO
	beq	ONLYRD

	lda	ARG2+HI
	sta	RDTBL2+HI
	lda	ARG2+LO
	sta	RDTBL2+LO

	ldx	#1		;  1 = DO IT ALL (X)
ONLYRD:
	stx	RDFLAG		; CHECK AGAIN AFTER READ IN WHAT TO DO
	jsr	INPUT		; READ LINE; RETURN LENGTH IN [RDTBL1],1

	lda	RDFLAG		; FLAG (X)
	beq	RDEX		; IF INPUT ONLY, LEAVE NOW
	jsr	DOREST
RDEX:
	lda	#$F0		; RETURN NOW ONLY WANTED READ PART
	sta	RDFLAG
	lda	BRKCHR		; GET BREAK CHAR
	ldx	#0
	jmp	PUTBYT		; RETURN IT 
;
; IF TIMEOUT, [A]=0 SO WILL QUIT W/NO RESULTS
;
DOREST:
	lda	RDTBL2+HI	; get max number of words
	jsr	SETPC		; tell me memory and bank
	sta	FPCH		; save page
	sty	FPCBNK		; and bank
	lda	RDTBL2+LO	; and for LSB
	sta	FPCL		; it is same
	jsr	FETCHB		; get max # of words in table
	beq	RDERR		; (5/14/85 - FORCE # WORDS TO
	cmp	#59		; BE BETWEEN 1 AND 59)
	bcc	RD0
RDERR:
	lda	#58		; (5/16/86 - MAKE IT 58, 59 LOST)
RD0:
	sta	MAXWORDS	; save max words
	lda	#0		; start at 0 words
	sta	WORDCNT		; save it
	sta	WRDLEN		; INIT # CHARS IN WORD COUNTER
	lda	#2		
	sta	SOURCE		; INIT SOURCE TABLE PNTR
;
; now futz with destination table a little
;
	jsr	NEXTFPC		; now we point to # words read
	lda	FPCBNK		; and save this pointer
	sta	SAVESPC		; for stashing at the end
	lda	FPCH
	sta	SAVESPC+1
	lda	FPCL
	sta	SAVESPC+2

	lda	#4		; offset to end of first entry
	jsr	ADDFPC		; and point to end of first entry
	jsr	FP2SP		; now put RDTBL2 into stash pointer
;
; now get source table
;	
	lda	RDTBL1+HI	; get page
	jsr	SETPC		; and tell me what mem page and bank
	sta	FPCH		; set up fetch counter
	sty	FPCBNK		; and bank
	lda	RDTBL1+LO	; and lo stays the same
	sta	FPCL		; and save it
	jsr	NEXTFPC		; get # of chars in buffer
	jsr	FETCHB		; and tell me about it
	sta	LINLEN		; SAVE # CHARS IN LINE
	jsr	NEXTFPC		; now point to first char in line
;
; MAIN LOOP STARTS HERE
;
READL:
	lda	MAXWORDS	; how we doin'?
	cmp	WORDCNT		; see if we have maxxed out
	bcc	RLEX		; all done, thank you

	lda	LINLEN
	ora	WRDLEN		; OUT OF CHARS AND WORDS?
	bne	RL2		; NOT YET
RLEX:
	lda	SAVESPC		; now set SPC to point to # words
	sta	SPCBNK		; read byte, as saved at the beginning
	lda	SAVESPC+1
	sta	SPCH
	lda	SAVESPC+2
	sta	SPCL
	lda	WORDCNT		; get word count
	jsr	STASHB		; and save it
	rts
RL2:
	lda	WRDLEN		; GET WORD LENGTH
	cmp	#9		; 9 CHARS DONE? (EZIP)
	bcc	RL3		; NO, KEEP GOING
	jsr	FLUSHW		; ELSE FLUSH REMAINDER OF WORD
RL3:
	lda	WRDLEN		; GET WORD LENGTH AGAIN
	bne	READL2		; CONTINUE IF NOT FIRST CHAR
;
; START A NEW WORD
;
	ldx	#8		; CLEAR Z-WORD INPUT BUFFER
RLL:	sta	IN,X		; [A] = 0
	dex
	bpl	RLL

	lda	SOURCE		; STORE THE START POS OF THE WORD
	sta	WORDSTART	; and save it for later
	jsr	FETCHB		; GET A CHAR FROM SOURCE BUFFER
	jsr	SIB		; IS IT A SELF-INSERTING BREAK?
	bcs	DOSIB		; YES IF CARRY WAS SET
	jsr	NORM		; IS IT A "NORMAL" BREAK?
	bcc	READL2		; NO, CONTINUE
	inc	SOURCE		; ELSE FLUSH THE STRANDED BREAK
	jsr	NEXTFPC		; and point to next char
	dec	LINLEN		; UPDATE # CHARS LEFT IN LINE
	jmp	READL		; AND LOOP
READL2:
	lda	LINLEN		; OUT OF CHARS YET?
	beq	READL3		; LOOKS THAT WAY
	jsr	FETCHB		; Grab the char
	jsr	BREAK		; IS IT A BREAK?
	bcs	READL3		; YES IF CARRY WAS SET
	ldx	WRDLEN		; ELSE STORE THE CHAR
	sta	IN,X		; INTO THE INPUT BUFFER
	dec	LINLEN		; ONE LESS CHAR IN LINE
	inc	WRDLEN		; ONE MORE IN WORD
	inc	SOURCE		; and update next source
	jsr	NEXTFPC		; POINT TO NEXT CHAR IN SOURCE
	jmp	READL		; AND LOOP BACK
;
; handle self-inserting breaks
DOSIB:
	sta	IN		; put the break into 1st word slot
	dec	LINLEN		; one less char in line
	inc	WRDLEN		; one more in word buffer
	inc	SOURCE		; and update next source
	jsr	NEXTFPC		; point to next source char
READL3:
	lda	WRDLEN		; ANY CHARS IN WORD YET?
	bne	READL31		; yup, so deal with word
	jmp	READL		; then go get next word
READL31:
	jsr	CONZST		; CONVERT ASCII IN [IN] TO Z-STRING
	jsr	FINDW		; AND LOOK IT UP IN VOCABULARY

	lda	WORDSTART	; get where it starts
	jsr	STASHB		; and save it
	jsr	PREVSPC		; step backwards to point to length
	lda	WRDLEN		; and get length
	jsr	STASHB		; and save it away
	jsr	PREVSPC		; and backwards to LSB of offset
	ldx	#6		; offset to point to end of next entry

	inc	WORDCNT		; increment # words read

	lda	PSVFLG		; IF SHOULD PRESERVE WHAT'S IN
	beq	READL4
	lda	VALUE+HI	; RDTBL2 AND NOT FOUND (VALUE = 0)
	ora	VALUE+LO
	beq	READL5		; JUST SKIP OVER
READL4:
	lda	VALUE+LO	; GET LSB OF VOCAB ENTRY ADDRESS
	jsr	STASHB		; and stash it away
	jsr	PREVSPC		; point to MSB part
	lda	VALUE+HI	; ALSO STORE MSB IN 2ND SLOT
	jsr	STASHB		; and send it out
	ldx	#7		; offset to point to end of next entry
READL5:
	lda	#0
	sta	WRDLEN		; CLEAR # CHARS IN WORD
	txa			; get offset
	jsr	ADDSPC		; and point to end of next entry
	jmp	READL		; AND LOOP BACK

; ---
; LEX
; ---
; DO PARSE OF TBL1 INTO TBL2 (2ND HALF OF READ)

ZLEX:
	LDA	ARG1+HI		; MAKE THE TABLE ADDRESSES
	STA	RDTBL1+HI	; AND PLACE IT HERE TO USE
	LDA	ARG1+LO
	STA	RDTBL1+LO	; LSBS NEED NOT CHANGE

	LDA	ARG2+HI
	STA	RDTBL2+HI
	LDA	ARG2+LO
	STA	RDTBL2+LO

	DEC	NARGS
	DEC	NARGS
	BEQ	NORMLEX		; USE NORMAL VOCAB TBL

	LDA	#1		; USE ARG3 VOCAB TBL
	STA	VOCFLG
	LDA	#0
	DEC	NARGS
	BEQ	NOSAVE		; ZERO UNFOUND WORDS
	LDA	#1		; PRESERVE UNFOUND WORD SLOT FLAG
NOSAVE:	STA	PSVFLG
	JMP	DOLEX

NORMLEX: LDA	#0
	STA	VOCFLG		; USE NORMAL VOCAB TBL
	STA	PSVFLG		; AND WILL BE NO PRESERVING

DOLEX:	JMP	DOREST		; GO DO LEXICAL CONVERSION AND JUST RETURN


; -----
; ZWSTR
; -----
; CONVERT A WORD TO A ZWORD, PLACE IN ARG4 TBL
ZWSTR:
	lda	ARG1+HI		; Make ARG1 be the FPC
	jsr	SETPC		; so get absolute mem bank/page
	sty	FPCBNK		; save bank and
	sta	FPCH		; page
	lda	ARG1+LO
	sta	FPCL		; LSBS NEED NOT CHANGE
;
; (IGNORE WORD LENGTH CAUSE CHECK FOR BREAK CHAR (9 CHAR MAX))
;
	lda	ARG3+LO		; ADD OFFSET INTO INBUF
	jsr	ADDFPC		; add it to the FPC

	lda	ARG4+HI		; now fix the SPC too
	jsr	SETPC		; get me bank and page
	sty	SPCBNK		; save bank
	sta	SPCH		; save page
	lda	ARG4+LO
	sta	SPCL		; LSB doesn't change
;
; START A NEW WORD
;
	lda	#9
	sta	LINLEN		; 1 WORD'S WORTH
	lda	#0
	sta	WRDLEN

	ldx	#8		; CLEAR Z-WORD INPUT BUFFER
WSTR1:	sta	IN,X		; [A] = 0
	dex
	bpl	WSTR1
;
; THIS LOOP READS FROM INBUF TIL BREAK OR 9 CHARS READ
;
WSTR2:
	jsr	FETCHB		; grab the next char
	jsr	BREAK		; IS IT A BREAK?
	bcs	WSTR3		; YES IF CARRY WAS SET
	ldx	WRDLEN		; ELSE STORE THE CHAR
	sta	IN,X		; INTO THE INPUT BUFFER
	inc	WRDLEN		; ONE MORE CHAR IN WORD
	dec	LINLEN		; ONE LESS IN LINE
	jsr	NEXTFPC		; point to next char
	bne	WSTR2		; AND LOOP BACK TIL DONE
WSTR3:
	lda	WRDLEN		; ANY CHARS IN WORD YET?
	beq	WOOPS		; APPARENTLY NOT, OOPS
	jsr	CONZST		; CONVERT ASCII IN [IN] TO Z-STRING

	ldx	#0		; MOVE FROM [OUT] TO RDTBL2
WSTR4:	lda	OUT,X
	jsr	STASHB		; and stash it into ZWORD table
	jsr	NEXTSPC		; and point to next byte
	inx
	cpx	#6		; max 6 word table
	bne	WSTR4		; not done yet
WOOPS:
	rts

; ----------
; FLUSH WORD
; ----------

FLUSHW:
	lda	LINLEN		; ANY CHARS LEFT IN LINE?
	beq	FLEX		; NO, SCRAM
	jsr	FETCHB		; GRAB A CHAR
	jsr	BREAK		; IS IT A BREAK?
	bcs	FLEX		; EXIT IF SO
	dec	LINLEN		; ELSE UPDATE CHAR COUNT
	inc	WRDLEN		; AND WORD-CHAR COUNT
	inc	SOURCE		; AND CHAR POINTER
	jsr	NEXTFPC		; and FPC pointer too
	jmp	FLUSHW		; AND LOOP BACK (ALWAYS)
FLEX:
	rts


; ---------------------------------
; IS CHAR IN [A] ANY TYPE OF BREAK?
; ---------------------------------
; ------------------
; NORMAL BREAK CHARS
; ------------------

BRKTBL:	DB	'!?,.'		; IN ORDER OF
	DB	$0D		; ASCII	ENDING FREQUENCY
	DB	SPACE		; SPACE CHAR IS TESTED FIRST FOR SPEED
	DB	0		; ZERO ADDED FOR ZWSTR (X)
NBRKS	EQU	$-BRKTBL	; # NORMAL BREAKS

BREAK:	JSR	SIB		; CHECK FOR A SIB FIRST
	BCS	FBRK		; EXIT NOW IF MATCHED

	; ELSE FALL THROUGH ...


; --------------------------------
; IS CHAR IN [A] A "NORMAL" BREAK?
; --------------------------------

NORM:	LDX	#NBRKS-1	; NUMBER OF "NORMAL" BREAKS
NBL:	CMP	BRKTBL,X	; MATCHED?
	BEQ	FBRK		; YES, EXIT
	DEX
	BPL	NBL		; NO, KEEP LOOKING
	CLC			; NO MATCH, CLEAR CARRY
	RTS			; AND RETURN


; ---------------------
; IS CHAR IN [A] A SIB?
; ---------------------

SIB:	STA	IOCHAR		; SAVE TEST CHAR
	lda	VOCAB+ABANK	; get bank
	sta	MPCBNK		; and save it
	lda	VOCAB+HI	; and hi part
	sta	MPCPNT+HI	; and save it
	lda	ZBEGIN+ZVOCAB+0 ; GET 1ST BYTE IN VOCAB TABLE
	LDY	ZBEGIN+ZVOCAB+1
	STA	MPCM
	STY	MPCL
	LDA	#0
	STA	MPCH		; now everything is set up
	JSR	GETBYT		; HAS # SIBS
	STA	J		; USE AS AN INDEX
SBL:	JSR	GETBYT		; GET NEXT SIB
	CMP	IOCHAR		; MATCHED?
	BEQ	FBRK0		; YES, REPORT IT
	DEC	J
	BNE	SBL		; ELSE KEEP LOOPING
	LDA	IOCHAR
	CLC			; NO MATCH, SO
	RTS			; EXIT WITH CARRY CLEAR
FBRK0:	LDA	IOCHAR
FBRK:	SEC			; EXIT WITH CARRY SET
	RTS			; IF MATCHED WITH A BREAK CHAR


; -----------------
; VOCABULARY SEARCH
; -----------------
; ENTRY: 6-BYTE TARGET Z-WORD IN [OUT]
; EXIT: VIRTUAL ENTRY ADDRESS IN [VALUE] IF FOUND ;
; OTHERWISE [VALUE] = 0

VWLEN	EQU	I		; **********
VWCUR	EQU	J+HI

FINDW:
	lda	VOCFLG		; USE WHAT VOCAB TBL?
	beq	FWL2		; NORMAL
	lda	ARG3+HI		; IF ALTERNATE VOCTBL
	ldy	ARG3+LO		; IT'S ADDR IS IN ARG3
	jmp	FWL3
FWL2:
	lda	DIDVTBL		; have we done default vocab table?
	beq	FWLNEW		; nope, so do it the first time
	ldx	#2		; restore pointers
FWRSTL:
	lda	VOCMPC,X	; get it
	sta	MPC,X		; save it
	lda	VCESVE,X	; save VOCEND too
	sta	VOCEND,X	; okay, we have
	lda	VWLSVE,X	; and starting length
	sta	VWLEN,X		; we have
	dex			; count
	bpl	FWRSTL		; okay, next one
	jmp	FWLOOP		; and go do it
FWLNEW:
	lda	#$FF		; show we are doing default table
	sta	DIDVTBL		; we shall

	lda	ZBEGIN+ZVOCAB	; GET VIRTUAL ADDR OF VOCAB TBL
	ldy	ZBEGIN+ZVOCAB+1
FWL3:
	STA	MPCM
	STY	MPCL
	LDA	#0
	STA	MPCH
	JSR	VLDMPC		; SET TO NEW PAGE
	JSR	GETBYT		; GET # SIBS
	CLC
	ADC	MPCL		; GET ACTUAL BASE ADDR OF VOCAB ENTRIES
	STA	MPCL
	BCC	FWL0
	INC	MPCM
FWL0:	JSR	VLDMPC		; SET TO NEW PAGE
	JSR	GETBYT		; GET # BYTES PER ENTRY (AND MOVE TO NEXT BYTE)
	STA	ESIZE		; SAVE IT HERE
	STA	VWLEN+0		; AND HERE
	LDA	#0		; CLEAR REST OF COUNTER
	STA	VWLEN+1
	STA	VWLEN+2

	JSR	GETBYT		; GET # OF ENTRIES IN TBL (MSB)
	STA	NENTS+HI	; AND STUFF IT IN [NENTS]
	JSR	GETBYT		; DON'T FORGET THE LSB!
	STA	NENTS+LO
	LDA	NENTS+HI
	BPL	SORTED
	JMP	UNSORTED	; VOCAB LIST IS UNSORTED, HANDLE DIFFERENTLY
SORTED:
	LDA	#0		; FIND SIZE OF VAOCAB TBL
	STA	VOCEND		; TO LOCATE THE END OF IT
	STA	VOCEND+1
	STA	VOCEND+2
	LDX	ESIZE
FWL1:
	CLC
	LDA	VOCEND		; (# OF ENTRIES) * (ENTRY SIZE)
	ADC	NENTS+LO
	STA	VOCEND
	LDA	VOCEND+1
	ADC	NENTS+HI
	STA	VOCEND+1
	bcc	FWL11
	inc	VOCEND+2
FWL11:
	DEX
	BNE	FWL1

	CLC
	LDA	VOCEND		; AND ADD LENGTH TO START OF TBL
	ADC	MPCL		; TO GET END OF TBL
	STA	VOCEND
	LDA	VOCEND+1
	ADC	MPCM
	STA	VOCEND+1
	LDA	VOCEND+2
	ADC	MPCH
	STA	VOCEND+2	; TO SAVE FOR TESTING IF PAST END

	LDA	VOCEND		; SUBTRACT [ESIZE] SO THAT
	SEC			; [VOCEND] POINTS TO REAL LAST ENTRY
	SBC	ESIZE
	STA	VOCEND
	LDA	VOCEND+1
	SBC	#0
	STA	VOCEND+1
;
; BEGIN THE SEARCH! [MPC] NOW POINTS TO 1ST ENTRY
;
	LSR	NENTS+HI	; 2 ALIGN # OF ENTRIES
	ROR	NENTS+LO	; 2 point to middle of table
FWCALC:	ASL	VWLEN+0		; CALCULATE INITIAL OFFSET FOR SEARCH
	ROL	VWLEN+1
	ROL	VWLEN+2
	LSR	NENTS+HI
	ROR	NENTS+LO
	BNE	FWCALC

	CLC			; ADD 1ST OFFSET INTO START OF VOCABULARL
	LDA	MPCL		; WHICH IS CURRENTLY IN MPC
	ADC	VWLEN+0
	STA	MPCL
	LDA	MPCM
	ADC	VWLEN+1
	STA	MPCM
	LDA	MPCH
	ADC	VWLEN+2
	STA	MPCH

	SEC			; AVOID FENCE-POST BUG FOR
	LDA	MPCL		; EXACT-POWER-OF-2 TBL (DUNCAN)
	SBC	ESIZE
	STA	MPCL
	BCS	FWSAVE
	LDA	MPCM
	SEC
	SBC	#1
	STA	MPCM
	BCS	FWSAVE
	LDA	MPCH
	SBC	#0
	STA	MPCH
FWSAVE:
	lda	DIDVTBL		; are we installing default table?
	bpl	FWLOOP		; already have?	
	ldx	#2		; save MPC
	stx	DIDVTBL		; show we have saved it
FWSVL:
	lda	MPC,X		; get it
	sta	VOCMPC,X	; save it
	lda	VOCEND,X	; save VOCEND too
	sta	VCESVE,X	; okay, we have
	lda	VWLEN,X		; and starting length
	sta	VWLSVE,X	; we have
	dex			; count
	bpl	FWSVL		; okay, next one
FWLOOP:
	lsr	VWLEN+2		; SET FOR NEXT OFFSET,
	ror	VWLEN+1		; WHICH IS HALF THIS ONE
	ror	VWLEN+0

	lda	MPCL		; HOLD START ADDR, MPC WILL BE A MESS
	sta	VWCUR+0
	lda	MPCM
	sta	VWCUR+1
	lda	MPCH
	sta	VWCUR+2

	jsr	VLDMPC		; SET TO NEW PAGE
	jsr	GETBYT		; GET 1ST BYTE OF ENTRY
	cmp	OUT		; MATCH 1ST BYTE OF TARGET?
	bcc	WNEXT		; LESS
	bne	FWPREV		; GREATER
	jsr	GETBYT	
	cmp	OUT+1		; 2ND BYTE MATCHED?
	bcc	WNEXT
	bne	FWPREV		; NOPE
	jsr	GETBYT
	cmp	OUT+2		; 3RD BYTE?
	bcc	WNEXT
	bne	FWPREV		; SORRY ...
	jsr	GETBYT
	cmp	OUT+3		; 4TH BYTE
	bcc	WNEXT
	BNE	FWPREV
	JSR	GETBYT
	CMP	OUT+4		; 5TH BYTE?
	BCC	WNEXT
	BNE	FWPREV		; SORRY ...
	JSR	GETBYT
	CMP	OUT+5		; LAST BYTE?
	BEQ	FWSUCC		; FOUND IT!
	BCS	FWPREV		; ELSE BACK UP ...
WNEXT:
	LDA	VWCUR+0		; TO MOVE UP, JUST ADD
	CLC			; OFFSET FROM START OF THIS
	ADC	VWLEN+0		; ENTRY
	STA	MPCL
	LDA	VWCUR+1
	ADC	VWLEN+1
	BCS	WNXT2		; SAVES CODE (?)

	STA	MPCM
	LDA	#0
	STA	MPCH
WNXT0:
	LDA	MPCM		; GONE PAST END?
	CMP	VOCEND+1	
	BEQ	WNXT1		; MAYBE
	BCS	WNXT2		; YES
	BCC	FWMORE		; NO
WNXT1:
	LDA	MPCL
	CMP	VOCEND
	BCC	FWMORE		; NO
	BEQ	FWMORE		; NO, EQUAL
WNXT2:
	LDA	VOCEND		; YES, SO POINT TO END OF TBL
	STA	MPCL
	LDA	VOCEND+1
	STA	MPCM
	LDA	VOCEND+2
	STA	MPCH
	JMP	FWMORE
FWPREV:
	LDA	VWCUR+0		; TO MOVE DOWN, JUST SUBTRACT
	SEC			; OFFSET FROM START OF THIS
	SBC	VWLEN+0		; ENTRY
	STA	MPCL
	LDA	VWCUR+1
	SBC	VWLEN+1
	STA	MPCM
	LDA	VWCUR+2
	SBC	VWLEN+2
	STA	MPCH
FWMORE:
	LDA	VWLEN+2		; IF OFFSET >GE< 1 WORD, CONTINUE
	BNE	FWM1
	LDA	VWLEN+1
	BNE	FWM1
	LDA	VWLEN+0
	CMP	ESIZE
	BCC	FWFAIL
FWM1:
	JMP	FWLOOP		; AND TRY AGAIN

FWSUCC:	LDA	VWCUR+0		; ENTRY MATCHED!  RETRIEVE START OF WORD
	STA	VALUE+LO
	LDA	VWCUR+1
	STA	VALUE+HI	; MUST BE 64K LIMIT AS ONLY
	RTS			; WORD VALUE RETURNABLE
FWFAIL:
	LDA	#0		; NOT FOUND
	STA	VALUE+LO
	STA	VALUE+HI
	RTS			; THEN RETURN WITH [VALUE] = 0
;
; DO UNSORTED SEARCH ON VOCAB TBL IN MPC
;
UNSORTED:
	LDA	#$FF		; 2'S COMPLEMENT LENGTH
	EOR	NENTS+HI	; TO GET REAL LENGTH
	STA	NENTS+HI	; WAS NEGATIVE TO SIGNIFY
	LDA	#$FF		; UNSORTED VOCAB TBL
	EOR	NENTS+LO
	STA	NENTS+LO
	INC	NENTS+LO	; 2'S CMPL
	BNE	UNSRT0
	INC	NENTS+HI
UNSRT0:
	LDA	MPCL		; HOLD START ADDR, MPC WILL BE A MESS
	STA	VWCUR+0
	LDA	MPCM
	STA	VWCUR+1
	LDA	MPCH
	STA	VWCUR+2

	JSR	GETBYT		; GET 1ST BYTE OF ENTRY
	CMP	OUT		; MATCH 1ST BYTE OF TARGET?
	BNE	FNEXT		; LESS
	JSR	GETBYT
	CMP	OUT+1		; 2ND BYTE MATCHED?
	BNE	FNEXT
	JSR	GETBYT
	CMP	OUT+2		; 3RD BYTE?
	BNE	FNEXT
	JSR	GETBYT
	CMP	OUT+3		; 4TH BYTE
	BNE	FNEXT
	JSR	GETBYT
	CMP	OUT+4		; 5TH BYTE?
	BNE	FNEXT
	JSR	GETBYT
	CMP	OUT+5		; LAST BYTE?
	BEQ	FWSUCC		; FOUND IT!

FNEXT:	LDA	VWCUR+LO	; TO MOVE UP, JUST ADD
	CLC			; OFFSET FROM START OF THIS
	ADC	ESIZE		; ENTRY
	STA	MPCL
	BCC	FNEXT0

	LDA	VWCUR+HI	; PICK UP CARRY
	ADC	#0
	STA	MPCM
	LDA	#0
	STA	MPCH
	JSR	VLDMPC		; CROSSED PAGE SO RE-VALIDATE

FNEXT0:	DEC	NENTS+LO	; CHECKED ALL ENTRIES?
	BNE	UNSRT0
	LDA	NENTS+HI
	BEQ	FWFAIL		; GO INDICATE NO FIND
	DEC	NENTS+HI	; OR DO NEXT 256 ENTRIES
	JMP	UNSRT0

	END
