	TITLE	"Apple ][ YZIP (c)Infocom","X-OPS"

	; ------
	; EQUAL?
	; ------
	; IS [ARG1] = [ARG2] (OR [ARG3] OR [ARG4])?

ZEQUAL:
	DEC	NARGS	; DOUBLE-CHECK # ARGS
	LDA	ARG1+LO	; FETCH LSB
	LDX	ARG1+HI	; AND MSB OF [ARG1]
	CMP	ARG2+LO	; TEST LSB OF [ARG2]
	BNE	TRY2	; NO GOOD, LOOK FOR ANOTHER ARG
	CPX	ARG2+HI	; ELSE TRY MSB OF [ARG2]
	BEQ	EQOK	; MATCHED!
TRY2:	DEC	NARGS	; OUT OF ARGS YET?
	BEQ	EQBAD	; YES, WE FAILED
	CMP	ARG3+LO	; TRY LSB OF [ARG3]
	BNE	TRY3	; NO GOOD, LOOK FOR ANOTHER ARG
	CPX	ARG3+HI	; HOW ABOUT MSB OF [ARG3]?
	BEQ	EQOK	; YAY!
TRY3:	DEC	NARGS	; OUT OF ARGS YET?
	BEQ	EQBAD	; IF NOT ...
	CMP	ARG4+LO	; TRY [ARG4]
	BNE	EQBAD	; SORRY, CHUM
	CPX	ARG4+HI	; MSB MATCHED?
	BNE	EQBAD	; TOO BAD

EQOK:	JMP	PREDS	; FINALLY MATCHED!

EQBAD:	JMP	PREDF	; FAILURE (SNIFF!)


; ----------------------------
; ICALL,ICALL1, ICALL2, IXCALL
; ----------------------------

ZICALL:
ZICLL1:
ZICLL2:
ZIXCLL:	LDA	#1	; SET FLAG FOR RETURNLESS CALL
	STA	IRET
	BNE	IENTR	; JMP OVER NORMAL SETTING


; -------------------
; XCALL, CALL1, CALL2
; -------------------

ZXCALL:			; DROP	THROUGH
ZCALL1:			; CALL	RTN HANDLES ALL 4 KINDS
ZCALL2:

; ----
; CALL
; ----
; BRANCH TO FUNCTION AT ([ARG1]*4), PASSING
; OPTIONAL PARAMETERS IN [ARG2]-[ARG4]
; ([ARG5]-[ARG8] FOR XCALL (EZIP))

ZCALL:	LDA	#0
	STA	IRET		; SET FLAG TO RETURN SOMETHING
IENTR:	LDA	ARG1+LO
	ORA	ARG1+HI		; IS CALL ADDRESS ZERO?
	BNE	DOCALL		; NO, CONTINUE
	LDA	IRET		; any ret value?
	BEQ	Ij		; yes, so return a zero

	RTS			; otherwise, just end
Ij:
	LDX	#0
	JMP	PUTBYT		; ELSE RETURN THE ZERO IN [A]
DOCALL:	LDX	OLDZSP+LO	; SAVE OLD STACK POINTER
	LDA	OLDZSP+HI
	JSR	PUSHXA
	LDA	ZPCL		; AND LSB OF [ZPC]
	LDX	IRET		; AND RETURN FLAG
	JSR	PUSHXA		; ON THE Z-STACK
	LDX	ZPCM		; SAVE MIDDLE 8 BITS
	LDA	ZPCH		; AND TOP BIT OF [ZPC]
	JSR	PUSHXA		; AS WELL

	; FORM 16-BIT ADDRESS FROM [ARG1]

	LDA	#0	
	ASL	ARG1+LO		; MULTIPLY [ARG1]
	ROL	ARG1+HI		; (BY 2)
	ROL	A		; >BIT INTO [A]
	ASL	ARG1+LO		; BY 4 (EZIP)
	ROL	ARG1+HI	
	ROL	A
	STA	ZPCH		; NEW >BIT OF [ZPC]
	LDA	ARG1+HI		; GET NEW <BYTES
	STA	ZPCM	
	LDA	ARG1+LO	
	STA	ZPCL	
;
; now add offset
;
	clc			; doing adding
	lda	ZPCL		; start at lo part
	adc	FOFFL		; add in lo part
	sta	ZPCL
	lda	ZPCM
	adc	FOFFM
	sta	ZPCM
	lda	ZPCH
	adc	FOFFH
	sta	ZPCH
	jsr	VLDZPC	
	jsr	NEXTPC		; FETCH # LOCALS TO PASS
	sta	J+LO		; SAVE HERE FOR COUNTING
	sta	J+HI		; AND HERE FOR LATER REFERENCE
	beq	ZCLL2		; SKIP IF NO LOCALS
	lda	#0	
	sta	I+LO		; ELSE INIT STORAGE INDEX
ZCLL1:
	LDY	I+LO	
	LDX	LOCALS+LO,Y	; GET LSB OF LOCAL INTO [X]
	LDA	LOCALS+HI,Y	; AND MSB INTO [A]
	JSR	PUSHXA		; PUSH LOCAL IN [X/A] ONTO Z-STACK
	LDY	I+LO		; RESTORE INDEX
	LDA	#0		; ZERO ALL LOCALS (X)
	STA	LOCALS+LO,Y
	STA	LOCALS+HI,Y
	INY		
	INY			; UPDATE
	STY	I+LO		; THE STORAGE INDEX
	DEC	J+LO		; ANY MORE LOCALS?
	BNE	ZCLL1		; YES, KEEP LOOPING
;
; MOVE UP TO 7 ARGUMENTS TO [LOCALS]
;
ZCLL2:
	LDA	ASSVLU		; get how many here
	JSR	PUSHXA		; save how many args for ASSIGNED?
	DEC	NARGS		; EXTRA ARGS IN THIS CALL?
	LDA	NARGS		; SAVE FOR ASSIGNED? OP
	STA	ASSVLU		; and save away
	BEQ	ZCALL3		; NO, CONTINUE
	LDA	ARG2+LO		; MOVE [ARG2] TO LOCAL #1
	STA	LOCALS+LO	
	LDA	ARG2+HI	
	STA	LOCALS+HI	
	DEC	NARGS		; ANY LEFT?
	BEQ	ZCALL3		; NO, SCRAM
	LDA	ARG3+LO		; MOVE [ARG3] TO LOCAL #2
	STA	LOCALS+LO+2	
	LDA	ARG3+HI	
	STA	LOCALS+HI+2	
	DEC	NARGS		; ANY LEFT?
	BEQ	ZCALL3		; NO, EXUENT
	LDA	ARG4+LO		; MOVE [ARG4] TO LOCAL #3
	STA	LOCALS+LO+4	
	LDA	ARG4+HI	
	STA	LOCALS+HI+4	
	DEC	NARGS		; MORE (THAT MEANS IT'S AN XCALL)
	BEQ	ZCALL3		; NO, JUST A CALL
	LDA	ARG5+LO		; MOVE [ARG5] TO LOCAL #4
	STA	LOCALS+LO+6	
	LDA	ARG5+HI	
	STA	LOCALS+HI+6	
	DEC	NARGS		; MORE?
	BEQ	ZCALL3		; NO
	LDA	ARG6+LO		; MOVE [ARG6] TO LOCAL #5
	STA	LOCALS+LO+8	
	LDA	ARG6+HI	
	STA	LOCALS+HI+8	
	DEC	NARGS		; MORE?
	BEQ	ZCALL3		; NO
	LDA	ARG7+LO		; MOVE [ARG7] TO LOCAL #6
	STA	LOCALS+LO+10	
	LDA	ARG7+HI	
	STA	LOCALS+HI+10	
	DEC	NARGS		; MORE?
	BEQ	ZCALL3		; NO
	LDA	ARG8+LO		; MOVE [ARG8] TO LOCAL #7
	STA	LOCALS+LO+12	
	LDA	ARG8+HI	
	STA	LOCALS+HI+12	
ZCALL3:	LDX	J+HI		; RETRIEVE # LOCALS
	TXA			; DUPE FOR NO GOOD REASON
	JSR	PUSHXA		; PUSH # LOCALS ONTO Z-STACK
	LDA	ZSP+HI		; REMEMBER WHERE
	STA	OLDZSP+HI	; WE CAME FROM
	LDA	ZSP+LO	
	STA	OLDZSP+LO	
	RTS			; WHEW!


; ---
; PUT
; ---
; SET ITEM [ARG2] IN WORD-TABLE [ARG1] EQUAL TO [ARG3]
ZPUT:
	asl	ARG2+LO	; WORD-ALIGN [ARG2]
	rol	ARG2+HI	
	jsr	PCALC	; GET ITEM ADDR INTO [SPC]
	lda	ARG3+HI	; STORE MSB OF [ARG3]
	jsr	STASHB	; and store it away
	jsr	NEXTSPC	; POINT TO LSB
	jmp	PUTLSB	; and put in lsb


; ----
; PUTB
; ----
; SET ITEM [ARG2] IN BYTE-TABLE [ARG1] EQUAL TO [ARG3]

ZPUTB:	JSR	PCALC	
;
; ENTRY FOR "PUT"
;
PUTLSB:
	LDA	ARG3+LO	; GET LSB OF [ARG3]
	jsr	STASHB	; and save it away
	RTS		


; ---------------------------
; CALC ITEM ADDRESS FOR "PUT"
; ---------------------------
; put the 3 byte address into SPC
;
PCALC:	LDA	ARG2+LO	; ADD ITEM OFFSET IN [ARG2]
	CLC		; TO TABLE ADDR IN [ARG1]
	ADC	ARG1+LO	; TO FORM A POINTER
	STA	SPCL	; in [SPC]
	LDA	ARG2+HI	; SAME FOR MSB
	ADC	ARG1+HI	
	jsr	SETPC	; and figger out where it is
	sta	SPCH	; set page
	sty	SPCBNK	; and bank
	rts

; ----
; PUTP
; ----
; SET PROPERTY [ARG2] IN OBJECT [ARG1] EQUAL TO [ARG3]

ZPUTP:
	jsr	PROPB		; GET PROP TBL ADDR
PUTP1:
	jsr	PROPN		; GET ID
	cmp	ARG2+LO		; is it the wanted one?
	beq	PUTP2		; ayyup
	bcc	PNERR		; ERROR IF LOWER
	jsr	PROPNX		; TRY NEXT PROPERTY, ALIGN [I] AT IT (EZIP)
	jmp	PUTP1		; and check again
PUTP2:
	jsr	PROPL		; GET PROPERTY LENGTH INTO [A]
	tax			; save length
	jsr	NEXTFPC		; and point to first prop. byte
	lda	FPCBNK		; and copy FPC to SPC for storage
	sta	SPCBNK
	lda	FPCH
	sta	SPCH
	lda	FPCL
	sta	SPCL		; saved it
	cpx	#1		; IF LENGTH = 1
	beq	PUTP3		; PUT A BYTE
	cpx	#2		; PUT A WORD IF [A] = 2
	bne	PLERR		; ELSE LENGTH IS BAD
	lda	ARG3+HI		; GET MSB OF PROPERTY
	jsr	STASHB		; and store in object
	jsr	NEXTSPC		; POINT TO LSB SLOT
PUTP3:
	lda	ARG3+LO		; FETCH LSB of property
	jsr	STASHB		; AND STORE IN OBJECT
	rts

	; *** ERROR #10: BAD PROPERTY NUMBER ***

PNERR:	LDA	#10	
	JMP	ZERROR	

	; *** ERROR #11: PUTP PROPERTY LENGTH ***

PLERR:	LDA	#11	
	JMP	ZERROR	


; ------
; PRINTC
; ------
; PRINT CHAR WITH ASCII VALUE IN [ARG1]

ZPRC:	LDA	ARG1+LO	; GRAB THE CHAR
	JMP	COUT	; AND SHIP IT OUT


; ------
; PRINTN
; ------
; PRINT VALUE OF [ARG1] AS A SIGNED INTEGER

ZPRN:	LDA	ARG1+LO	; MOVE [ARG1] TO [QUOT]
	STA	QUOT+LO	
	LDA	ARG1+HI	
	STA	QUOT+HI	

	; PRINT [QUOT]

NUMBER:	LDA	QUOT+HI	; IF VALUE IS POSITIVE
	BPL	DIGCNT	; CONTINUE
	LDA	#'-'	; ELSE START WITH A MINUS SIGN
	JSR	COUT	
	JSR	ABQUOT	; AND CALC ABS([QUOT])

	; COUNT # OF DECIMAL DIGITS

DIGCNT:	LDA	#0	; RESET
	STA	DIGITS	; DIGIT INDEX
DGC:	LDA	QUOT+LO	; IS QUOTIENT
	ORA	QUOT+HI	; ZERO YET?
	BEQ	PRNTN3	; YES, READY TO PRINT
	LDA	#10	; ELSE DIVIDE [QUOT]
	STA	REMAIN+LO	; BY 10 (LSB)
	LDA	#0	
	STA	REMAIN+HI	; 10 (MSB)
	JSR	UDIV	; UNSIGNED DIVIDE
	LDA	REMAIN+LO	; FETCH LSB OF REMAINDER (THE DIGIT)
	PHA		; SAVE IT ON STACK
	INC	DIGITS	; UPDATE DIGIT COUNT
	BNE	DGC	; LOOP TILL QUOTIENT=0
PRNTN3:	LDA	DIGITS	; IF DIGIT COUNT IS NZ
	BNE	PRNTN4	; CONTINUE
	LDA	#'0'	; ELSE PRINT "0"
	JMP	COUT	; AND RETURN
PRNTN4:	PLA		; PULL A DIGIT OFF THE STACK
	CLC		
	ADC	#'0'	; CONVERT TO DB	II
	JSR	COUT	; AND PRINT IT
	DEC	DIGITS	; OUT OF DIGITS YET?
	BNE	PRNTN4	; NO, KEEP LOOPING
	RTS		


; ------
; RANDOM
; ------
; RETURN A RANDOM VALUE BETWEEN 0 AND [ARG1]

ZRAND:	LDA	ARG1+LO		; IF VALUE IS ZERO
 	ORA	ARG1+HI
	BNE	ZRAND1
	STA	SRHOLD+LO	; RETURN TO RANDOM RANDOM
	STA	SRHOLD+HI	; CLEAR INDICATOR
	JMP	RET0		; AND RETURN THRU HERE SO ALIGNED
ZRAND1:	LDA	SRHOLD+LO	; ARE WE NONRAMDOM INCREMENTING? (EZIP)
	ORA	SRHOLD+HI
	BNE	ZRAND3		; YUP
	LDA	ARG1+HI
	BPL	ZRAND2		; GENERATE A RANDOM #
	EOR	#$FF		; SET UP TO INCREMENT FROM 1 THRU INT
	STA	SRHOLD+HI	; GET ABSOLUTE
	LDA	ARG1+LO
	EOR	#$FF
	STA	SRHOLD+LO
	INC	SRHOLD+LO
	LDA	#0		; W/ NO RAMDOMNESS
	STA	RAND1+LO
	STA	RAND1+HI
	BEQ	ZRAND3		; JMP  (END EZIP)
ZRAND2:	LDA	ARG1+LO		; MAKE [ARG1] THE DIVISOR
	STA	ARG2+LO
	LDA	ARG1+HI
	STA	ARG2+HI
	JSR	RANDOM		; GET RANDOM BYTES INTO [A] AND [X]
	STX	ARG1+LO		; MAKE THEM THE DIVIDEND
	AND	#$7F		; MAKE SURE MSB IS POSITIVE
	STA	ARG1+HI
	JSR	DIVIDE		; SIGNED DIVIDE, [ARG1] / [ARG2]
	LDA	REMAIN+LO	; MOVE REMAINDER
	CLC
	ADC	#1		; ADD 1
	STA	VALUE+LO	; INTO [VALUE]
	LDA	REMAIN+HI
	ADC	#0
	STA	VALUE+HI
	JMP	PUTVAL		; AND RETURN RESULT

	; NON RANDOM INCREMENTING

ZRAND3:
	LDA	RAND1+HI	; (EZIP)
	CMP	SRHOLD+HI
	BCC	ZRAND4
	LDA	RAND1+LO
	CMP	SRHOLD+LO
	BCC	ZRAND4
	BEQ	ZRAND4
	LDA	#1		; WENT THRU ALL
	STA	RAND1+LO	; START AGAIN
	LDA	#0
	STA	RAND1+HI
ZRAND4:	LDA	RAND1+LO
	STA	VALUE+LO
	LDA	RAND1+HI
	STA	VALUE+HI
	INC	RAND1+LO	; FOR NEXT TIME
	BNE	ZRAND5
	INC	RAND1+HI
ZRAND5:	JMP	PUTVAL		; (END EZIP)


; ----
; PUSH
; ----
; PUSH [ARG1] ONTO THE Z-STACK

ZPUSH:	LDX	ARG1+LO	
	LDA	ARG1+HI	
	JMP	PUSHXA	


; ---
; POP
; ---
; POP WORD OFF Z-STACK, STORE IN VARIABLE [ARG1]

ZPOP:
	lda	NARGS		; check whether it wants game or passed stack
	beq	ZPOP1		; must want from game stack

	lda	ARG1+LO		; get me the address of the LTABLE
	sta	FPCL		; for munging with
	lda	ARG1+HI		; this is page
	jsr	SETPC		; get me actual page/bank
	sta	FPCH		; set page
	sty	FPCBNK		; and bank

	jsr	FETCHB		; this is hi part of counter
	sta	J+HI		; save it
	jsr	NEXTFPC		; point to lo part
	jsr	FETCHB		; get it
	sta	J+LO		; save it
;
; now count popping this one and stash it into the table
;
	inc	J+LO		; count it
	bne	ZPOP2		; wrapped?
	inc	J+HI		; increment MSB
ZPOP2:
	jsr	FP2SP		; set up to stash back in beginning of LTABLE
	lda	J+LO		; LSB first
	jsr	STASHB		; saved it
	jsr	PREVSPC		; point to MSB
	lda	J+HI		; get it
	jsr	STASHB		; saved it
;
; finally, we can save the arg into the stack
;
	asl	J+LO		; make a word offset (*2)
	rol	J+HI		; pick up carry maybe
	lda	J+LO		; add in arg offset
	clc			; adding
	adc	ARG1+LO		; figger offset
	sta	FPCL		; this goes here for fetching
	lda	J+HI		; now page
	adc	ARG1+HI		; add in start of table
	jsr	SETPC		; get me memory page
	sta	FPCH		; page
	sty	FPCBNK		; and bank
	jsr	FETCHB		; get MSB
	sta	VALUE+HI	; save value
	jsr	NEXTFPC		; and point to LSB
	jsr	FETCHB		; get LSB
	sta	VALUE+LO	; and save
	jmp	ZPOP3		; and return it
ZPOP1:
	jsr	POPVAL		; VALUE INTO [VALUE]
ZPOP3:
	jmp	PUTVAL		; AND put the data away


; ------
; INTBL?
; ------

ZINTBL:
	LDA	ARG3+HI		; JIC COUNT IS 0,
	BMI	INTNF		; >0, just ignore!
	ORA	ARG3+LO
	BEQ	INTNF		; SAY NOT FOUND

	LDA	NARGS		; IS THERE A RECORD SPEC?
	CMP	#4
	BEQ	SET4
SETDEF:	LDA	#130		; NO, SET DEFAULT
	STA	ARG4+LO

SET4:	LDA	ARG4+LO
	BEQ	SETDEF		; GO BACK AND GET VALUE
	LDA	#0		; COMPARE BYTE OR WORD?
	ASL	ARG4+LO
	ROL	A		; PICK UP INDICATOR
	LSR	ARG4+LO		; CLEAR FROM RECORD LENGTH
	STA	TYPE 		; BYTE (0) OR WORD (1)
	LDA	TYPE		; SET FLAG
	BNE	SETTBL
	LDA	ARG1+LO		; IF ONLY BYTE, MOVE IT
	STA	ARG1+HI		; TO FIRST BYTE CHECKED
SETTBL:
	LDA	ARG2+LO		; PICK UP TBL ADDR
	STA	MPCL
	LDA	ARG2+HI
	STA	MPCM
	LDA	#0
	STA	MPCH		; ONLY A WORD ADDR, SO IN 1ST 64K
	JSR	VLDMPC

INTLP:	LDA	MPCL		; HOLD START ADDR, MPC WILL BE A MESS
	STA	VWCUR+0
	LDA	MPCM
	STA	VWCUR+1
	LDA	MPCH
	STA	VWCUR+2
	JSR	GETBYT		; GET 1ST BYTE
	CMP	ARG1+HI		; DOES IT = THE VALUE LOOKING FOR?
	BNE	INTNXT		; NO
	LDA	TYPE
	BEQ	INTFND		; ONLY COMPARING A BYTE SO FOUND!
	JSR	GETBYT
	CMP	ARG1+LO
	BEQ	INTFND		; YES, FOUND IT
INTNXT:
	LDA	VWCUR+0		; TO MOVE UP, JUST ADD
	CLC			; OFFSET FROM START OF THIS
	ADC	ARG4+LO		; ENTRY
	STA	MPCL
	BCC	INEXT0

	LDA	VWCUR+1		; PICK UP CARRY
	ADC	#0
	STA	MPCM
	LDA	VWCUR+2
	ADC	#0
	STA	MPCH
	JSR	VLDMPC		; CROSSED PAGE SO RE-VALIDATE

INEXT0:	DEC	ARG3+LO		; CHECKED ALL ENTRIES?
	BNE	INTLP
	LDA	ARG3+HI
	BEQ	INTNF
	DEC	ARG3+HI
	BNE	INTLP

INTNF:
	LDA	#0		; 0 = NOT FOUND
	STA	VALUE+LO
	STA	VALUE+HI
	JSR	PUTVAL
	JMP	PREDF		; FAILED!

INTFND:
	LDA	VWCUR+LO
	STA	VALUE+LO	; AND SET TO RETURN THE VALUE
	LDA	VWCUR+HI
	STA	VALUE+HI
	JSR	PUTVAL		; SEND IT BACK
	JMP	PREDS		; AND SCREEM SUCCESS

; ----
; BCOM
; ----
; COMPLEMENT [ARG1]

ZBCOM:	LDA	ARG1+LO
	EOR	#$FF
	STA	VALUE+LO
	LDA	ARG1+HI
	EOR	#$FF
	STA	VALUE+HI
	JMP	PUTVAL


; -----
; COPYT
; -----

ZCOPYT:
	LDA	ARG2+LO		; CHECK OUT WHAT'S TO BE DONE
	ORA	ARG2+HI
	BNE	ZC0
	JMP	CASE1		; ZERO LENGTH BYTES OF SOURCE
ZC0:
	LDA	ARG3+HI
	CMP	#$7F
	BCC	CASE2
	JMP	CASE3		; FORWARD COPY

	; CASE2 - CHECK IF FORWARD OR BACKWARD COPY

CASE2:	LDA	ARG1+HI		; IF SRC < DEST
	CMP	ARG2+HI
	BCC	CHK2
	BEQ	ZC1
	JMP	FRWRD		; NO
ZC1:	LDA	ARG1+LO
	CMP	ARG2+LO
	BEQ	CHK2
	BCS	FRWRD		; NO
CHK2:	LDA	ARG1+LO		; AND SRC + LENGTH > DEST
	CLC
	ADC	ARG3+LO
	STA	I+LO
	LDA	ARG1+HI
	ADC	ARG3+HI
	CMP	ARG2+HI
	BCC	FRWRD		; NO
	BNE	BKWRD		; YES
	LDA	I+LO
	CMP	ARG2+LO
	BEQ	FRWRD		; DEBUG, IF EQUAL REALLY LESS
	BCS	BKWRD		; OVERLAPS SO DO BACKWARD COPY

	; ELSE FALL THROUGH TO FORWARD COPY

FRWRD:	LDA	#0		; USE GETBYT CAUSE MAY  BE
	STA	MPCH		; BEYOND MAIN MEMORY
	LDA	ARG1+HI
	STA	MPCM
	LDA	ARG1+LO
	STA	MPCL
	JSR	VLDMPC		; AND ALIGN TO CORRECT PAGE
	LDA	ARG2+LO
	STA	SPCL
	LDA	ARG2+HI
	jsr	SETPC		; get memory spot
	sta	SPCH		; high part
	sty	SPCBNK		; and the bank part
	LDA	ARG3+LO
	STA	J+LO
	LDA	ARG3+HI
	STA	J+HI
FRLP:
	jsr	DECJ
	bcc	FRDUN		; CARRY CLEAR ON $FFFF
	jsr	GETBYT
	jsr	STASHB		; and save it
	jsr	NEXTSPC		; and point to next one
	jmp	FRLP
FRDUN:
	rts


BKWRD:	
	LDA	ARG3+LO		; DECR 1ST TO GET CORRECT OFFSET
	STA	J+LO
	LDA	ARG3+HI
	STA	J+HI
	JSR	DECJ
	LDA	ARG1+LO		; SET TO END OF SOURCE & DEST.
	CLC
	ADC	J+LO
	sta	FPCL		; set up fetch pointer
	LDA	ARG1+HI
	ADC	J+HI
	jsr	SETPC		; and get mem locations
	sta	FPCH
	sty	FPCBNK
	LDA	ARG2+LO
	CLC
	ADC	J+LO
	sta	SPCL		; and now set up stash pointer
	LDA	ARG2+HI
	ADC	J+HI
	jsr	SETPC		; and get me page/bank
	sta	SPCH
	sty	SPCBNK
BKLP:
	jsr	FETCHB		; get byte
	jsr	STASHB		; and save it
	jsr	PREVFPC		; going backwards
	jsr	PREVSPC		; and here too
	jsr	DECJ		; RETURNS CARRY CLEAR ON $FFFF
	bcs	BKLP
BKDUN:
	RTS

	; ZERO LENGTH # OF BYTES OF SOURCE

CASE1:	LDA	ARG1+LO
	STA	SPCL		; set stash pointer
	LDA	ARG1+HI
	jsr	SETPC		; get page/bank
	sta	SPCH
	sty	SPCBNK
	LDA	ARG3+LO		; SET UP COUNTER
	STA	J+LO
	LDA	ARG3+HI
	STA	J+HI
C1LP:
	jsr	DECJ		; CARRY CLEAR WHEN J = $FFFF
	bcc	C1DUN
	lda	#0
	jsr	STASHB		; and zero it
	jsr	NEXTSPC		; and point to next one
	jmp	C1LP
C1DUN:
	rts

	; 2'S COMPLEMENT LENGTH (XOR + 1) THEN DO FORWARD COPY

CASE3:
	LDA	ARG3+LO
	EOR	#$FF
	STA	ARG3+LO
	LDA	ARG3+HI
	EOR	#$FF
	STA	ARG3+HI
	INC	ARG3+LO
	BNE	GOFRWD
	INC	ARG3+HI
GOFRWD:	JMP	FRWRD


; ---------
; ASSIGNED?
; ---------


ZASSND:
	LDA	ARG1+LO		; COMPARE TO # OF OPTIONALS FROM LAST CALL
	CMP	ASSVLU
	BCC	DOYES		; IF LESS OR EQUAL, WAS ASSIGNED
	BEQ	DOYES
	JMP	PREDF
DOYES:
	JMP	PREDS


; -------------
; LOGICAL SHIFT
; -------------
; SHIFT ARG1, ARG2 BITS (LEFT IF ARG2 IS POS. RIGHT IF NEG.)

ZSHIFT:	LDA	ARG1+LO		; SET UP FOR SHIFT
	STA	VALUE+LO
	LDA	ARG1+HI
	STA	VALUE+HI
	LDA	ARG2+LO		; IF NEGATIVE, SHIFT RIGHT
	CMP	#$80
	BCS	SRIGHT

	; SHIFT LEFT

	TAY			; COUNT
SLP1:	ASL	VALUE+LO
	ROL	VALUE+HI
	DEY
	BNE	SLP1
	JMP	PUTVAL		; AND RETURN THE VALUE

SRIGHT:	EOR	#$FF		; COMPLEMENT
	TAY
SLP2:	LSR	VALUE+HI	; SHIFT
	ROR	VALUE+LO
	DEY
	BPL	SLP2
	JMP	PUTVAL


; ----------------
; ARITHMETIC SHIFT
; ----------------
; PROPAGATING SIGN BIT ON RIGHT SHIFT

ZASHFT:	LDA	ARG2+LO		; IF NEGATIVE, SHIFT RIGHT
	CMP	#$80
	BCC	ZSHIFT		; SAME AS LOGICAL SHIFT
	LDX	ARG1+LO		; SET UP FOR SHIFT
	STX	VALUE+LO
	LDX	ARG1+HI
	STX	VALUE+HI

	EOR	#$FF		; COMPLEMENT COUNT
	TAY
ASLP2:	LDA	ARG1+HI
	ASL	A		; GET SIGN BIT
	ROR	VALUE+HI	; SHIFT
	ROR	VALUE+LO
	DEY
	BPL	ASLP2
	JMP	PUTVAL
	
; --------
; XPUSH
; --------
ZXPUSH:
	lda	ARG2+LO		; get me the address of the LTABLE
	sta	FPCL		; for munging with
	lda	ARG2+HI		; this is page
	jsr	SETPC		; get me actual page/bank
	sta	FPCH		; set page
	sty	FPCBNK		; and bank

	jsr	FETCHB		; this is hi part of counter
	sta	J+HI		; save it
	jsr	NEXTFPC		; point to lo part
	jsr	FETCHB		; get it
	sta	J+LO		; thanx
	ora	J+HI		; check for zero elements left
	bne	ZXP0		; yes, there is room at the inn

	jmp	PREDF		; no room here!
ZXP0:
	jsr	FP2SP		; set up to stash back in beginning of LTABLE
	lda	J+HI		; now the MSB
	sta	K+HI		; and saved it
	lda	J+LO		; save this
	sta	K+LO		; save it
;
; now count this one and stash it into the table
;
	bne	ZXP1		; nope, dec okay
	dec	J+HI		; decrement MSB
ZXP1:
	dec	J+LO		; and the LSB
	lda	J+LO		; LSB first
	jsr	STASHB		; saved it
	jsr	PREVSPC		; point to MSB
	lda	J+HI		; get it
	jsr	STASHB		; saved it
;
; finally, we can save the arg into the stack
;
	asl	K+LO		; make a word offset (*2)
	rol	K+HI		; pick up carry maybe
	lda	K+LO		; add in arg offset
	clc			; adding
	adc	ARG2+LO		; figger offset
	sta	SPCL		; this goes here for stashing
	lda	K+HI		; now page
	adc	ARG2+HI		; add in start of table
	jsr	SETPC		; get me memory page
	sta	SPCH		; page
	sty	SPCBNK		; and bank

	lda	ARG1+HI		; push MSB
	jsr	STASHB		; saved
	jsr	NEXTSPC		; point to next one
	lda	ARG1+LO		; and now LSB 
	jsr	STASHB		; into the stack
	jmp	PREDS		; show we worked good
;---------
; ZFSTACK
;---------
ZFSTACK:
	dec	NARGS		; how many args there?
	bne	ZFS1		; flush ARG2 stack
;
; pop from system stack
;
	lda	ARG1+LO		; just add number to system counter
	clc			; adding
	adc	ZSP+LO		; added
	sta	ZSP+LO		; and saved
	lda	ARG1+HI		; get hi part
	adc	ZSP+HI		; add in hi part
	sta	ZSP+HI		; save hi part
	rts
ZFS1:
	lda	ARG2+LO		; get LTABLE we are interested in
	sta	FPCL		; set up FPC first
	lda	ARG2+HI		; and page
	jsr	SETPC		; tell me where
	sta	FPCH		; save me where
	sty	FPCBNK		; FPC all set
	jsr	FP2SP		; have SPC point to stack too

	jsr	FETCHB		; get MSB of counter
	sta	J+HI		; save MSB
	jsr	NEXTFPC		; point to LSB
	jsr	FETCHB		; get LSB
	sta	J+LO		; save LSB
	
	lda	J+LO		; get LSB back
	clc			; get ready for add
	adc	ARG1+LO		; add how many to get rid off
	sta	J+LO		; save new counter
	lda	J+HI		; get MSB
	adc	ARG1+HI		; add MSB

	jsr	STASHB		; save Msb of new counter
	jsr	NEXTSPC		; point to LSB
	lda	J+LO		; get lsb
	jsr	STASHB		; okay, reset the counter
	rts

;
; no mouse stuff yet
;
ZMINFO:
ZMLIMIT:
	rts
ZMENU:
	jmp	PREDF		; no menu stuff either

	END
