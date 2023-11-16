	TITLE	"Apple ][ YZIP (c)Infocom","OBJECT & PROPERTY HANDLERS"

; ----------------------------------
; GET ABSOLUTE ADDRESS OF OBJECT [A]
; ----------------------------------
; ENTER: OBJECT IN A/X (LO/HI)
; EXIT: ADDRESS IN [FPC]
OBJLOC:
	STX	I+HI		; SAVE MSB FOR SHIFTING
	ASL	A		; MULTIPLY BY LENGTH OF AN ENTRY (14)
	STA	I+LO	
	ROL	I+HI	
	LDX	I+HI	
	ASL	A		
	ROL	I+HI	; *4
	ASL	A		
	ROL	I+HI	; *8
	ASL	A		
	ROL	I+HI	; *16
	SEC		
	SBC	I+LO	; -(*2)
	STA	I+LO	; SO IS *14 TOTAL
	LDA	I+HI	
	STX	I+HI	
	SBC	I+HI	
	STA	I+HI	
	LDA	I+LO	
	CLC		
	ADC	#112		; ADD OBJECT TABLE OFFSET
	BCC	OBJ3	
	INC	I+HI	
OBJ3:
	clc			; NEXT ADD THE relative ADDR
	adc	ZBEGIN+ZOBJEC+1	; OF THE OBJECT TABLE
	sta	FPCL		; save lo byte
	lda	I+HI	
	adc	ZBEGIN+ZOBJEC	; now work on page
	jsr	SETPC		; now get page/bank
	sta	FPCH		; this is hi part
	sty	FPCBNK		; and this is the bank
	rts
; -----------------------------
; GET ADDRESS OF PROPERTY TABLE
; -----------------------------
; EXIT: [FPC] HAS ABSOLUTE ADDR OF PROPERTY TABLE
;		including OFFSET TO START OF PROP IDS
PROPB:
	lda	ARG1+LO	
	ldx	ARG1+HI	; get address
	jsr	OBJLOC	; put table location into FPC
	lda	#12	; add 12 to get to beginning
	jsr	ADDFPC	; and add it to FPC
	jsr	FETCHB	; get MSB of P-TABLE Address
	pha		; and save it for a moment
	jsr	NEXTFPC	; to get LSB
	jsr	FETCHB	; get LSB of P-TABLE Address
	sta	FPCL	; and save lo part
	pla		; get page back
	jsr	SETPC	; and set up memory bank/page
	sta	FPCH	; save page
	sty	FPCBNK	; and bank
	jsr	FETCHB	; get length of short description
	asl	A	; WORD-ALIGN IT
	jsr	ADDFPC	; and add it to FPC
	jsr	NEXTFPC	; POINT JUST PAST THE DESCRIPTION
	rts


; -------------------
; FETCH A PROPERTY ID
; -------------------
; ENTRY: LIKE "PROPB" EXIT (i.e. - address in FPC)
;
PROPN:
	jsr	FETCHB		; get the byte
	and	#%00111111	; MASK OUT LENGTH BITS (EZIP)
	rts		

; -------------------------------
; FETCH # BYTES IN PROPERTY VALUE
; -------------------------------
; ENTRY: LIKE "PROPB" EXIT (i.e. - address in FPC)
;
PROPL:
	jsr	FETCHB		; CHECK LENGTH FLAGS
	tax			; save it
	bpl	SHORT		; OFF, SO 1 OR 2 BYTES
	jsr	NEXTFPC
	jsr	FETCHB		; NEXT BYTE HAS LENGTH
	and	#%00111111	; MASK OFF EXTRA BITS
	rts		
SHORT:
	and	#%01000000	; BIT 6
	beq	ONE	
	lda	#2		; BIT 6 = 1, LENGTH =2
	rts		
ONE:
	lda	#1		; BIT 6 = 0, LENGTH =1
	rts		

; ----------------------
; POINT TO NEXT PROPERTY
; ----------------------
; ENTRY: LIKE "PROPB" EXIT (i.e. - in [FPC])
;
PROPNX:
	jsr	PROPL	; GET LENGTH OF CURRENT PROP
	tay		; move one more for correct alignment
	iny		; okay, done it
	tya		; thanks
	jsr	ADDFPC	; add to [FPC]
	rts
;
; ----------------
; GET OBJECT FLAGS
; ----------------
; ENTRY: OBJECT # IN [ARG1], FLAG # IN [ARG2]
; EXIT: FLAG WORD IN [K], BIT ID IN [J],
; FLAG WORD ADDRESS IN [FPC]
;
FLAGSU:
	LDA	ARG1+LO	; get table offset
	LDX	ARG1+HI	
	JSR	OBJLOC	; GET OBJECT ADDR IN [FPC]
	LDA	ARG2+LO	; LOOK AT FLAG ID
	CMP	#$10	; FIRST SET OF FLAGS?
	BCC	FLS1	; YES, ADDR IN [FPC] IS CORRECT
	SBC	#16	; ELSE ZERO-ALIGN FLAG INDEX
	TAX		; SAVE IT HERE
	CMP	#$10	; CHECK IF IN 2ND WORD
	BCC	FLS	; YES, GO ALIGN FOR THAT
	SBC	#16	; ELSE ALIGN TO 3RD WORD
	TAX		
	lda	#4	; 3rd Flag word
	bne	FLSx	; and add it in
FLS:
	lda	#2	; 2nd Flag word
FLSx:
	jsr	ADDFPC
FLS0:
	TXA		; RESTORE INDEX
FLS1:
	STA	K+LO	; SAVE FLAG ID HERE
	LDX	#1	; INIT THE
	STX	J+LO	; FLAG WORD TO
	DEX		; $0001
	STX	J+HI	
	LDA	#15	; SUBTRACT THE BIT POSITION
	SEC		; FROM 15
	SBC	K+LO	; TO GET THE SHIFT LOOP
	TAX		; INDEX
	BEQ	FLS2	; EXIT NOW IF NO SHIFT NEEDED
FLSL:	ASL	J+LO	; SHIFT THE BIT
	ROL	J+HI	; INTO POSITION
	DEX		
	BNE	FLSL	
FLS2:
	jsr	FETCHB	; MOVE THE FLAG WORD
	sta	K+HI	; INTO [K] - FIRST THE MSB
	jsr	NEXTFPC	; and then get get LSB
	jsr	FETCHB
	sta	K+LO	; THEN THE LSB
	jmp	PREVFPC	; point back to flag word

	END
