	PAGE			
	SBTTL "--- X-OPS ---"

	; ------
	; EQUAL?
	; ------
	; IS [ARG1] = [ARG2] (OR [ARG3] OR [ARG4])?

ZEQUAL:	DEC	NARGS	; DOUBLE-CHECK # ARGS
	BNE	DOEQ	; MUST BE AT LEAST TWO, OR ...

	; *** ERROR #9: NOT ENOUGH "EQUAL?" ARGS ***

	LDA	#9	
	JMP	ZERROR	
DOEQ:	LDA	ARG1+LO	; FETCH LSB
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


; -------------------
; XCALL, CALL1, CALL2
; -------------------

ZXCALL:		; DROP	THROUGH
ZCALL1:		; CALL	RTN HANDLES ALL 4 KINDS
ZCALL2:			


; ----
; CALL
; ----
; BRANCH TO FUNCTION AT ([ARG1]*4), PASSING
; OPTIONAL PARAMETERS IN [ARG2]-[ARG4]
; ([ARG5]-[ARG8] FOR XCALL (EZIP))

ZCALL:	LDA	ARG1+LO	
	ORA	ARG1+HI	; IS CALL ADDRESS ZERO?
	BNE	DOCALL	; NO, CONTINUE
	LDX	#0	
	JMP	PUTBYT	; ELSE RETURN THE ZERO IN [A]
DOCALL:	LDX	OLDZSP+LO	; SAVE OLD STACK POINTER
	LDA	OLDZSP+HI	
	JSR	PUSHXA	
	LDA	ZPCL	; AND LSB OF [ZPC]
	JSR	PUSHXA	; ON THE Z-STACK ([X] NOT USED HERE - EZIP)
	LDX	ZPCM	; SAVE MIDDLE 8 BITS
	LDA	ZPCH	; AND TOP BIT OF [ZPC]
	JSR	PUSHXA	; AS WELL

	; FORM 16-BIT ADDRESS FROM [ARG1]

	LDA	#0	
	ASL	ARG1+LO	; MULTIPLY [ARG1]
	ROL	ARG1+HI	; (BY 2)
	ROL	A	;	HIGH BIT INTO [A]
	STA	ZPCH	; NEW HIGH BIT OF [ZPC]
	ASL	ARG1+LO	; BY 4 (EZIP)
	ROL	ARG1+HI	
	ROL	ZPCH	
	LDA	ARG1+HI	; GET NEW LOW BYTES
	STA	ZPCM	
	LDA	ARG1+LO	
	STA	ZPCL	
	JSR	VLDZPC	
	JSR	NEXTPC	; FETCH # LOCALS TO PASS
	STA	J+LO	; SAVE HERE FOR COUNTING
	STA	J+HI	; AND HERE FOR LATER REFERENCE
	BEQ	ZCLL2	; SKIP IF NO LOCALS
	LDA	#0	
	STA	I+LO	; ELSE INIT STORAGE INDEX
ZCLL1:	LDY	I+LO	
	LDX	LOCALS+LO,Y	; GET LSB OF LOCAL INTO [X]
	LDA	LOCALS+HI,Y	; AND MSB INTO [A]
	JSR	PUSHXA	; PUSH LOCAL IN [X/A] ONTO Z-STACK
	JSR	NEXTPC	; GET MSB OF NEW LOCAL
	STA	I+HI	; SAVE IT HERE
	JSR	NEXTPC	; NOW GET LSB
	LDY	I+LO	; RESTORE INDEX
	STA	LOCALS+LO,Y	; STORE LSB INTO [LOCALS]
	LDA	I+HI	; RETRIEVE MSB
	STA	LOCALS+HI,Y	; STORE IT INTO [LOCALS]
	INY		
	INY		; UPDATE
	STY	I+LO	; THE STORAGE INDEX
	DEC	J+LO	; ANY MORE LOCALS?
	BNE	ZCLL1	; YES, KEEP LOOPING

	; MOVE UP TO 3 ARGUMENTS TO [LOCALS] ( 7 FOR XCALL (EZIP))

ZCLL2:	DEC	NARGS	; EXTRA ARGS IN THIS CALL?
	BEQ	ZCALL3	; NO, CONTINUE
	LDA	ARG2+LO	; MOVE [ARG2] TO LOCAL #1
	STA	LOCALS+LO	
	LDA	ARG2+HI	
	STA	LOCALS+HI	
	DEC	NARGS	; ANY LEFT?
	BEQ	ZCALL3	; NO, SCRAM
	LDA	ARG3+LO	; MOVE [ARG3] TO LOCAL #2
	STA	LOCALS+LO+2	
	LDA	ARG3+HI	
	STA	LOCALS+HI+2	
	DEC	NARGS	; ANY LEFT?
	BEQ	ZCALL3	; NO, EXUENT
	LDA	ARG4+LO	; MOVE [ARG4] TO LOCAL #3
	STA	LOCALS+LO+4	
	LDA	ARG4+HI	
	STA	LOCALS+HI+4	
	DEC	NARGS	; MORE (THAT MEANS IT'S AN XCALL)
	BEQ	ZCALL3	; NO, JUST A CALL
	LDA	ARG5+LO	; MOVE [ARG5] TO LOCAL #4
	STA	LOCALS+LO+6	
	LDA	ARG5+HI	
	STA	LOCALS+HI+6	
	DEC	NARGS	; MORE?
	BEQ	ZCALL3	; NO
	LDA	ARG6+LO	; MOVE [ARG6] TO LOCAL #5
	STA	LOCALS+LO+8	
	LDA	ARG6+HI	
	STA	LOCALS+HI+8	
	DEC	NARGS	; MORE?
	BEQ	ZCALL3	; NO
	LDA	ARG7+LO	; MOVE [ARG7] TO LOCAL #6
	STA	LOCALS+LO+10	
	LDA	ARG7+HI	
	STA	LOCALS+HI+10	
	DEC	NARGS	; MORE?
	BEQ	ZCALL3	; NO
	LDA	ARG8+LO	; MOVE [ARG8] TO LOCAL #7
	STA	LOCALS+LO+12	
	LDA	ARG8+HI	
	STA	LOCALS+HI+12	
ZCALL3:	LDX	J+HI	; RETRIEVE # LOCALS
	TXA		; DUPE FOR NO GOOD REASON
	JSR	PUSHXA	; PUSH # LOCALS ONTO Z-STACK
	LDA	ZSP+HI	; REMEMBER WHERE
	STA	OLDZSP+HI	; WE CAME FROM
	LDA	ZSP+LO	
	STA	OLDZSP+LO	
	RTS		; WHEW!


; ---
; PUT
; ---
; SET ITEM [ARG2] IN WORD-TABLE [ARG1] EQUAL TO [ARG3]

ZPUT:	ASL	ARG2+LO	; WORD-ALIGN [ARG2]
	ROL	ARG2+HI	
	JSR	PCALC	; GET ITEM ADDR INTO [I]
	LDA	ARG3+HI	; STORE MSB OF [ARG3]
	STA	(I),Y	; INTO MSB OF TABLE POSITION
	INY		; POINT TO LSB
	BNE	PUTLSB	; BRANCH ALWAYS


; ----
; PUTB
; ----
; SET ITEM [ARG2] IN BYTE-TABLE [ARG1] EQUAL TO [ARG3]

ZPUTB:	JSR	PCALC	

	; ENTRY FOR "PUT"

PUTLSB:	LDA	ARG3+LO	; GET LSB OF [ARG3]
	STA	(I),Y	; STORE IN TABLE AT [Y]
	RTS		


; ---------------------------
; CALC ITEM ADDRESS FOR "PUT"
; ---------------------------

PCALC:	LDA	ARG2+LO	; ADD ITEM OFFSET IN [ARG2]
	CLC		; TO TABLE ADDR IN [ARG1]
	ADC	ARG1+LO	; TO FORM A POINTER
	STA	I+LO	; IN [I]
	LDA	ARG2+HI	; SAME FOR MSB
	ADC	ARG1+HI	
	CLC		
	ADC	ZCODE	; MAKE IT ABSOLUTE
	STA	I+HI	
	LDY	#0	; ZERO FOR INDEXING
	RTS		


; ----
; PUTP
; ----
; SET PROPERTY [ARG2] IN OBJECT [ARG1] EQUAL TO [ARG3]

ZPUTP:	JSR	PROPB	; GET PROP TBL ADDR

PUTP1:	JSR	PROPN	; GET ID
	CMP	ARG2+LO	
	BEQ	PUTP2	
	BCC	PNERR	; ERROR IF LOWER
	JSR	NEXTPI	; TRY NEXT PROPERTY, ALIGN [I] AT IT (EZIP)
	JMP	PUTP1	
PUTP2:	JSR	PROPL	; GET PROPERTY LENGTH INTO [A]
	INY		; MAKE [Y] POINT TO 1ST PROPERTY BYTE
	CMP	#1	; IF LENGTH = 1
	BEQ	PUTP3	; PUT A BYTE
	CMP	#2	; PUT A WORD IF [A] = 2
	BNE	PLERR	; ELSE LENGTH IS BAD
	LDA	ARG3+HI	; GET MSB OF PROPERTY
	STA	(I),Y	; AND STORE IN OBJECT
	INY		; POINT TO LSB SLOT
PUTP3:	LDA	ARG3+LO	; FETCH LSB
	STA	(I),Y	; AND STORE IN OBJECT
	RTS		

	; *** ERROR #10: BAD PROPERTY NUMBER ***

PNERR:	LDA	#10	
	JMP	ZERROR	

	; *** ERROR #11: PUTP PROPERTY LENGTH ***

PLERR:	LDA	#11	
	JMP	ZERROR	


; ------
; PRINTC
; ------
; PRINT CHAR WITH DB	II VALUE IN [ARG1]

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
	LDA	#$2D	; ELSE START WITH A MINUS SIGN
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

ZRAND3:	LDA	SRHOLD+HI	; RESET TOP OF COUNT IF NECESSARY
	CMP	ARG1+HI		; SO ALWAYS W/IN RANGE OF CALL
	BCC	ZRAND7
	BNE	ZRAND6
	LDA	ARG1+LO
	CMP	SRHOLD+LO
	BCS	ZRAND7

ZRAND6:	LDA	ARG1+HI		; NEW LOWER, SO SUBSTITUTE
	STA	SRHOLD+HI
	LDA	ARG1+LO
	STA	SRHOLD+LO

ZRAND7:	LDA	RAND1+HI	; (EZIP)
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

ZPOP:	JSR	POPVAL	; VALUE INTO [VALUE]
	LDA	ARG1+LO	; GET VARIABLE ID
	JMP	VARPUT	; AND CHANGE THE VARIABLE


; ------
; INTBL?
; ------

ZINTBL:	LDA	ARG2+LO	; PICK UP TBL ADDR
	STA	MPCL	
	LDA	ARG2+HI	
	STA	MPCM	
	LDA	#0	
	STA	MPCH	; ONLY A WORD ADDR, SO IN 1ST 64K
	JSR	VLDMPC	
INTLP:	JSR	GETBYT	; GET A WORD ENTRY FROM TBL
	STA	I+HI	
	JSR	GETBYT	
	CMP	ARG1+LO	; DOES IT = THE VALUE LOOKING FOR?
	BNE	INTNXT	; NO
	LDA	I+HI	
	CMP	ARG1+HI	
	BEQ	INTFND	; YES, FOUND IT
INTNXT:	DEC	ARG3+LO	
	BNE	INTLP	
	LDA	ARG3+HI	
	BEQ	INTNF	; NOT FOUND
	DEC	ARG3+HI	
	JMP	INTLP	; GO DO SOME MORE
INTFND:	SEC		
	LDA	MPCL	
	SBC	#2	
	STA	MPCL	
	BCS	INTEX	
	DEC	MPCM		; ONCE AGAIN, RETURNING WORD SO 64K LIMIT
INTEX:	STA	VALUE+LO	; AND SET TO RETURN THE VALUE
	LDA	MPCM	
	STA	VALUE+HI	
	JSR	PUTVAL	; SEND IT BACK
	JMP	PREDS	; AND SCREEM SUCCESS
INTNF:	LDA	#0	; 0 = NOT FOUND
	STA	VALUE+LO	
	STA	VALUE+HI	
	JSR	PUTVAL	
	JMP	PREDF	; FAILED!

	END