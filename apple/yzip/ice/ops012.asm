	TITLE	"Apple ][ YZIP (c)Infocom","0-OPS"


; -----
; RTRUE
; -----
; SIMULATE A "RETURN 1"

ZRTRUE:	LDX	#1	
ZRT0:	LDA	#0	
ZRT1:	STX	ARG1+LO	; GIVE TO
	STA	ARG1+HI	; [ARG1]
	JMP	ZRET	; AND DO THE RETURN


; ------
; RFALSE
; ------
; SIMULATE A "RETURN 0"

ZRFALS:	LDX	#0	
	BEQ	ZRT0	


; ------
; PRINTI
; ------
; PRINT Z-STRING FOLLOWING THE OPCODE

ZPRI:	LDX	#5	;MOVE ZPC INTO MPC
ZPRI1:			
	LDA	ZPC,X	
	STA	MPC,X	
	DEX		
	BPL	ZPRI1	;NO NEED TO VALIDATE AS ZPC WAS VALID ANYWAY
	JSR	PZSTR	; PRINT THE Z-STRING AT [MPC]
	LDX	#5	; COPY STATE OF [MPC]
ZPRI2:	LDA	MPC,X	; INTO [ZPC]
	STA	ZPC,X	
	DEX		
	BPL	ZPRI2	
	RTS		


; ------
; PRINTR
; ------
; DO A "PRINTI," FOLLOWED BY "CRLF" AND "RTRUE"

ZPRR:	JSR	ZPRI	
	JSR	ZZCRLF	
	JMP	ZRTRUE	


; ------
; RSTACK
; ------
; "RETURN" WITH VALUE ON STACK

ZRSTAK:	JSR	POPVAL	; GET VALUE INTO [X/A]
	JMP	ZRT1	; AND GIVE IT TO "RETURN"


; -----
; CATCH
; -----

ZCATCH:	LDX	OLDZSP+HI	; RETURN ZSTACK POINTER AS
	LDA	OLDZSP+LO	; ZRET WILL NEED IT
	JMP	PUTBYT


; ---------
; ORIGINAL?
; ---------

; COPY PROTECTION DEVICE, RETURNS TRUE FOR NOW

ZORIG:	JMP	PREDS


	TITLE	"Apple ][ YZIP (c)Infocom","--- 1-OPS ---"
	PAGE	


; -----
; ZERO?
; -----
; [ARG1] = 0?

ZZERO:	LDA	ARG1+LO	
	ORA	ARG1+HI	
	BEQ	PFINE	
PYUCK:	JMP	PREDF	


; -----
; NEXT?
; -----
; RETURN "NEXT" POINTER IN OBJECT [ARG1] ;
; FAIL IF LAST AND RETURN ZERO

ZNEXT:
	LDA	ARG1+LO	
	LDX	ARG1+HI	; (EZIP)
	JSR	OBJLOC	; GET OBJECT ADDR INTO [FPC]
	lda	#8	; POINT TO "NEXT" SLOT (EZIP)
	jsr	ADDFPC	; and point to it
	jmp	FIRST1	; JMP to get it

; ------
; FIRST?
; ------
; RETURN "FIRST" POINTER IN OBJECT [ARG1] ;
; FAIL IF LAST AND RETURN ZERO
; (EZIP ALTERATIONS)

ZFIRST:
	lda	ARG1+LO	
	ldx	ARG1+HI	
	jsr	OBJLOC	; GET OBJECT ADDR INTO [FPC]
	lda	#10	; point to "First" slot
	jsr	ADDFPC	; and add it
FIRST1:
	jsr	FETCHB	; and get it
	pha		; save it
	jsr	NEXTFPC
	jsr	FETCHB	; and next one
	tay		; now dance around a little; save LSB
	pla		; get MSB back
	tax		; and put to x
	tya		; and put LSB back into [A]
	jsr	PUTBYT	; PASS IT TO VARIABLE
	lda	VALUE+LO	; EXAMINE THE VALUE JUST "PUT"
	bne	PFINE	
	lda	VALUE+HI	
	beq	PYUCK	; FAIL IF IT WAS ZERO
PFINE:
	jmp	PREDS	; ELSE REJOICE

; ---
; LOC
; ---
; RETURN THE OBJECT CONTAINING OBJECT [ARG1] ;
; RETURN ZERO IF NONE
; (EZIP ALTERED)

ZLOC:
	lda	ARG1+LO	
	ldx	ARG1+HI	
	jsr	OBJLOC	; GET ADDR OF OBJECT INTO [FPC]
	lda	#6	; POINT TO "LOC" SLOT
	jsr	ADDFPC	; and make FPC point there
	jsr	FETCHB	; go get byte
	pha		; save this please
	jsr	NEXTFPC	; and point to next one
	jsr	FETCHB	; and get it in [A]
	tay		; save LSB
	pla		; get MSB back
	tax		; and put to [X]
	tya		; and put LSB into a
	jmp	PUTBYT	; AND SHIP IT OUT

; ------
; PTSIZE
; ------
; RETURN LENGTH OF PROP TABLE [ARG1] IN BYTES

ZPTSIZ:
	lda	ARG1+HI		; MOVE ABS ADDR OF
	jsr	SETPC		; get bank/page
	sta	FPCH		; save page
	sty	FPCBNK		; and bank
	lda	ARG1+LO
	sta	FPCL		; and save lo part
	jsr	PREVFPC		; Decrement address while moving LSB
PTZ0:
	jsr	FETCHB		; get length of property
	bmi	PTZ2		; BIT 7 = 1, LENGTH > 2 BYTES
	and	#%01000000
	beq	PTZ1		; BIT 6 = 0, LENGTH = 1
	lda	#2		; BIT 6 = 1, LENGTH = 2
	bne	PTZ3		; JMP
PTZ1:
	lda	#1
	bne	PTZ3		; JMP
PTZ2:
	jsr	FETCHB		; and get it
	and	#%00111111	; ELSE PICK UP SIZE > 2
PTZ3:
	ldx	#0		; CLEAR FOR PUTBYT
	jmp	PUTBYT
; ---
; INC
; ---
; INCREMENT VARIABLE [ARG1]

ZINC:	LDA	ARG1+LO	
	JSR	VARGET	; FETCH VARIABLE INTO [VALUE]
	INC	VALUE+LO
	BNE	ZINC1
	INC	VALUE+HI
ZINC1:	JMP	ZD0


; ---
; DEC
; ---
; DECREMENT VARIABLE [ARG1]

ZDEC:	LDA	ARG1+LO	
	JSR	VARGET	; FETCH VAR INTO [VALUE]
	LDA	VALUE+LO	
	SEC		
	SBC	#1	
	STA	VALUE+LO	
	LDA	VALUE+HI	
	SBC	#0	
	STA	VALUE+HI	
ZD0:	LDA	ARG1+LO	; PUT RESULT BACK
	JMP	VARPUT	; INTO THE SAME VARIABLE


; ------
; PRINTB
; ------
; PRINT Z-STRING AT [ARG1]

ZPRB:
	lda	ARG1+LO		; move arg1 to I
	sta	I+LO		; lo part
	lda	ARG1+HI		; and now for arg1 hi
	sta	I+HI		; okay
	jsr	SETWRD		; make [MPC] point to it
	jmp	PZSTR		; and print it

; ------
; REMOVE
; ------
; MOVE OBJECT [ARG1] INTO PSEUDO-OBJECT #0
; (EZIP CHANGES - 1) OBJLOC NEEDS HI & LO
; 2) MOVES AND COMPARES 2 BYTES)

SAVEFPC: ds	3	; spot to save the FPC

ZREMOV:
	lda	ARG1+LO	; GET SOURCE OBJECT ADDR
	ldx	ARG1+HI	
	jsr	OBJLOC	; INTO [FPC]
;
; now save this objects address in I,J
;
	lda	#6	; point to the LOC slot
	jsr	ADDFPC	; thank you
	lda	FPCL	; COPY THE SOURCE ADDR
	sta	SAVEFPC+LO	; INTO [J]
	lda	FPCH	; FOR LATER REFERENCE
	sta	SAVEFPC+HI
	lda	FPCBNK
	sta	SAVEFPC+ABANK
;
; now go get object this one is in
;		
	jsr	FETCHB	; get the data
	sta	K	; HOLD IT
	jsr	NEXTFPC	; get lo part
	jsr	FETCHB	; now get part
	tay		; save it
	ora	K	; COMPARE BYTES
	bne	REMVj	; check for object
	jmp	REMVEX	; SCRAM IF NO OBJECT
REMVj:
	tya		; get lo part
	ldx	K	; and hi part
	jsr	OBJLOC	; ELSE GET ADDR OF OBJECT [A] INTO [FPC]
	lda	#11	; POINT TO "FIRST" SLOT, LSB
	jsr	ADDFPC	; and point to it
	jsr	FETCHB	; and get it
	pha		; save LSB
	jsr	PREVFPC	; point to MSB
	jsr	FETCHB	; A=LO, X=HI
	tax		; put into x
	pla		; get LSB back
	cmp	ARG1+LO	; IS THIS THE FIRST?
	bne	REMVC1	; NO, KEEP SEARCHING
	cpx	ARG1+HI	; HM?
	beq	DO_REMOVE
REMVC1:
	jsr	OBJLOC	; get object
	lda	#9	; GET "NEXT" slot address
	jsr	ADDFPC	; into FPC
	jsr	FETCHB	; get the byte
	pha		; save LSB for later
	jsr	PREVFPC	; and this is the hi part
	jsr	FETCHB
	tax		; and put into x for (possible) check
	pla		; get lo part back
	cmp	ARG1+LO	; FOUND IT?
	bne	REMVC1	; NO, KEEP TRYING
	cpx	ARG1+HI	
	bne	REMVC1	
DO_REMOVE:
;
; now, copy from FPC to SPC for stashing away
;
	jsr	FP2SP	; copied
;
; now get removed one's address for fetching
;
	lda	SAVEFPC+ABANK	; this is where bank is
	sta	FPCBNK	; so show it
	lda	SAVEFPC+HI	; and here's the hi/lo part
	sta	FPCH
	lda	SAVEFPC+LO
	sta	FPCL
	lda	#2	; so get to source's "NEXT" slot
	jsr	ADDFPC	; point to next slot
	jsr	FETCHB	; get the hi byte
	jsr	STASHB	; and save it
	jsr	NEXTSPC	; point to next one
	jsr	NEXTFPC	; and from the source too
	jsr	FETCHB	; get lo byte
	jsr	STASHB	; and save it away
;
; now zero out SOURCE's loc and next spots
;
	lda	SAVEFPC+ABANK	; bank of source
	sta	SPCBNK
	lda	SAVEFPC+HI	; and addr of source LOC slot
	sta	SPCH
	lda	SAVEFPC+LO
	sta	SPCL	
;
; zero out NEXT and LOC of source
;
	lda	#0	; zero out 4 locations (LOC MSB)
	jsr	STASHB
	jsr	NEXTSPC
	
	lda	#0	; zero out 4 locations (LOC LSB)
	jsr	STASHB
	jsr	NEXTSPC
	
	lda	#0	; zero out 4 locations (NEXT MSB)
	jsr	STASHB
	jsr	NEXTSPC
	
	lda	#0	; zero out 4 locations (NEXT LSB)
	jsr	STASHB
	jsr	NEXTSPC
REMVEX:
	RTS		


; ------
; PRINTD
; ------
; PRINT SHORT DESCRIPTION OF OBJECT [ARG1]

ZPRD:
	lda	ARG1+LO	
	ldx	ARG1+HI	; (EZIP)
	jsr	OBJLOC	; GET ADDR OF OBJECT INTO [FPC]
	lda	#13	; GET PROP TABLE POINTER (EZIP)
	jsr	ADDFPC	; and point to LSB
	jsr	FETCHB	; fetch LSB
	pha		; SAVE IT HERE
	jsr	PREVFPC	; and now fetch MSB
	jsr	FETCHB	; thank you
	sta	I+HI	; AND MSB
	pla		; get LSB back
	sta	I+LO	; STORE LSB
	inc	I+LO	; POINT PAST THE
	bne	PDC0	; LENGTH BYTE
	inc	I+HI	
PDC0:	jsr	SETWRD	; CALC Z-STRING ADDR
	jmp	PZSTR	; AND PRINT IT


; ------
; RETURN
; ------
; RETURN FROM "CALL" WITH VALUE [ARG1]

ZRET:
	LDA	OLDZSP+LO	; RE-SYNC THE
	STA	ZSP+LO		; Z-STACK POINTER
	LDA	OLDZSP+HI
	STA	ZSP+HI
	JSR	POPVAL		; POP # LOCALS INTO [X/A]
	STX	I+HI		; SAVE HERE
	JSR	POPVAL		; now we need number of args
	STA	ASSVLU		; for ASSIGNED?

	LDX	I+HI		; see how many locals
	BEQ	RET2		; SKIP IF NOT

	; RESTORE PUSHED LOCALS

	DEX			; ZERO-ALIGN
	TXA			; AND
	ASL	A		; WORD-ALIGN # LOCALS
	STA	I+LO		; FOR USE AS A STORAGE INDEX
RET1:	JSR	POPVAL		; POP A LOCAL INTO [X/A]
	LDY	I+LO		; RETRIEVE STORAGE INDEX
	STA	LOCALS+HI,Y	; STORE MSB OF LOCAL
	TXA			; MOVE LSB
	STA	LOCALS+LO,Y	; AND STORE THAT TOO
	DEC	I+LO
	DEC	I+LO		; UPDATE STORAGE INDEX
	DEC	I+HI		; AND LOCALS COUNT
	BNE	RET1		; POP TILL NO MORE LOCALS

	; RESTORE OTHER VARIABLES

RET2:
	JSR	POPVAL		; POP [ZPCH] AND [ZPCM]
	STX	ZPCM
	STA	ZPCH
	JSR	POPVAL		; POP AND RESTORE
	STX	IRET
	STA	ZPCL
	JSR	POPVAL
	STX	OLDZSP+LO
	STA	OLDZSP+HI

	LDA	ZPCL		; check for zero zpc
	BNE	RETj		; which means we are returning to
	LDA	ZPCM		; an internal call
	BNE	RETj		; rather than just a normal
	LDA	ZPCH		; return
	BNE	RETj		; but so far it isn't

	JSR	A12VAL		; MOVE [ARG1] TO [VALUE]
	JMP	ZIRET		; so then do internal return
RETj:
	JSR	VLDZPC		; MAKE VALID
	LDA	IRET		; CHECK IF SHOULD RETURN A VALUE
	BEQ	RETYES		; (0 = RET, 1  = NO RETURN)
	RTS			; NO, SO JUST GET OUT OF HERE
RETYES:
	JSR	A12VAL		; MOVE [ARG1] TO [VALUE]
	JMP	PUTVAL		; AND RETURN IT


; ----
; JUMP
; ----
; JUMP  TO Z-LOCATION IN [ARG1]

ZJUMP:	JSR	A12VAL	; MOVE [ARG1] TO [VALUE]
	JMP	PREDB3	; A BRANCH THAT ALWAYS SUCCEEDS


; -----
; PRINT
; -----
; PRINT Z-STRING AT WORD (QUAD) POINTER [ARG1]

ZPRINT:
	LDA	ARG1+LO	
	STA	I+LO	
	LDA	ARG1+HI	
	STA	I+HI	
	JSR	SETSTR	; CALC STRING ADDRESS
	JMP	PZSTR	; AND PRINT IT


; -----
; VALUE
; -----
; RETURN VALUE OF VARIABLE [ARG1]

ZVALUE:
	LDA	ARG1+LO	
	JSR	VARGET	; GET THE VALUE
	JMP	PUTVAL	; EASY ENOUGH



	TITLE	"Apple ][ YZIP (c)Infocom","--- 2-OPS ---"
	PAGE	


; -----
; LESS?
; -----
; [ARG1] < [ARG2]?

ZLESS:
	JSR	A12VAL	; MOVE [ARG1] TO [VALUE]
	JMP	DLS0	; MOVE [ARG2] TO [I] & COMPARE


; ------
; DLESS?
; ------
; DECREMENT [ARG1] ; SUCCEED IF < [ARG2]

ZDLESS:
	JSR	ZDEC	; MOVES ([ARG1]-1) TO [VALUE]
DLS0:	LDA	ARG2+LO	; MOVE [ARG2] TO [I]
	STA	I+LO	
	LDA	ARG2+HI	
	STA	I+HI	
	JMP	COMPAR	; COMPARE & RETURN


; -----
; GRTR?
; -----
; [ARG1] < [ARG2]?

ZGRTR:	LDA	ARG1+LO	; MOVE [ARG1] TO [I]
	STA	I+LO	
	LDA	ARG1+HI	
	STA	I+HI	
	JMP	A2VAL	; MOVE [ARG2] TO [VALUE] & COMPARE


; ------
; IGRTR?
; ------
; INCREMENT [ARG1] ; SUCCEED IF GREATER THAN [ARG2]

ZIGRTR:	JSR	ZINC	; GET ([ARG1]+1) INTO [VALUE]
	LDA	VALUE+LO	; MOVE [VALUE] TO [I]
	STA	I+LO	
	LDA	VALUE+HI	
	STA	I+HI	
A2VAL:	LDA	ARG2+LO	; MOVE [ARG2] TO [VALUE]
	STA	VALUE+LO	
	LDA	ARG2+HI	
	STA	VALUE+HI	


; -----------------
; SIGNED COMPARISON
; -----------------
; ENTRY: VALUES IN [VALUE] AND [I]
;
;  IS [VALUE] > [I]
;
COMPAR:
	LDA	I+HI
	EOR	VALUE+HI
	BPL	SCMP
	LDA	I+HI
	CMP	VALUE+HI
	BCC	PGOOD
	JMP	PREDF

SCMP:	LDA	VALUE+HI
	CMP	I+HI
	BNE	SCEX
	LDA	VALUE+LO
	CMP	I+LO
SCEX:	BCC	PGOOD
	JMP	PREDF


; ---
; IN?
; ---
; IS OBJECT [ARG1] CONTAINED IN OBJECT [ARG2]?
ZIN:
	LDA	ARG1+LO	
	LDX	ARG1+HI	
	JSR	OBJLOC	; GET ADDR OF TARGET OBJECT INTO [FPC]
	LDA	#6	; POINT TO "LOC" SLOT
	jsr	ADDFPC	; and point to it
	jsr	FETCHB	; well?
	CMP	ARG2+HI	; IS IT THERE?
	BNE	PBAD	; NO
	jsr	NEXTFPC	; point to lo part
	jsr	FETCHB	; and get it
	CMP	ARG2+LO	
	BEQ	PGOOD	; YES, SUCCEED
PBAD:	JMP	PREDF	; TOO BAD, CHUM ...


; ----
; BTST
; ----
; IS EVERY "ON" BIT IN [ARG1]
; ALSO "ON" IN [ARG2]?

ZBTST:	LDA	ARG2+LO	; FIRST CHECK LSBS
	AND	ARG1+LO	
	CMP	ARG2+LO	; LSBS MATCH?
	BNE	PBAD	; NO, EXIT NOW
	LDA	ARG2+HI	; ELSE CHECK MSBS
	AND	ARG1+HI	
	CMP	ARG2+HI	; MATCHED?
	BNE	PBAD	; SORRY ...
PGOOD:
	JMP	PREDS	


; ---
; BOR
; ---
; RETURN [ARG1] "OR" [ARG2]

ZBOR:	LDA	ARG1+LO	
	ORA	ARG2+LO	
	TAX		
	LDA	ARG1+HI	
	ORA	ARG2+HI	

	; FALL THROUGH ...


; ---------------------
; RETURN VALUE IN [X/A]
; ---------------------

VEXIT:	STX	VALUE+LO
	STA	VALUE+HI
	JMP	PUTVAL


; ----
; BAND
; ----
; RETURN [ARG1] "AND" [ARG2]

ZBAND:	LDA	ARG1+LO	
	AND	ARG2+LO	
	TAX		
	LDA	ARG1+HI	
	AND	ARG2+HI	
	JMP	VEXIT	


; -----
; FSET?
; -----
; IS FLAG [ARG2] SET IN OBJECT [ARG1]?

ZFSETP:	JSR	FLAGSU	; GET BITS INTO [K] AND [J]
	LDA	K+HI	; DO MSBS
	AND	J+HI	
	STA	K+HI	
	LDA	K+LO	; DO LSBS
	AND	J+LO	
	ORA	K+HI	; ANY BITS ON?
	BNE	PGOOD	; TARGET BIT MUST BE ON
	JMP	PREDF	


; ----
; FSET
; ----
; SET FLAG [ARG2] IN OBJECT [ARG1]

ZFSET:
	JSR	FLAGSU	; GET BITS INTO [K] & [J], ADDR IN [FPC]
;
; now, copy from FPC to SPC for stashing away
;
	jsr	FP2SP	; okay, done it
	lda	K+HI	; FIRST DO MSBS
	ora	J+HI	
	jsr	STASHB
	jsr	NEXTSPC
	lda	K+LO	; THEN LSBS
	ora	J+LO	
	jsr	STASHB
	rts


; ------
; FCLEAR
; ------
; CLEAR FLAG [ARG2] IN OBJECT [ARG1]

ZFCLR:
	jsr	FLAGSU	; GETS BITS INTO [J] & [K], ADDR IN [FPC]
;
; now, copy from FPC to SPC for stashing away
;
	jsr	FP2SP	; okey dokey

	lda	J+HI	; FETCH MSB
	eor	#$FF	; COMPLEMENT IT
	and	K+HI	; RUB OUT FLAG
	jsr	STASHB	; and save it
	jsr	NEXTSPC	; point to lo part
	lda	J+LO	; SAME FOR LSB
	eor	#$FF	
	and	K+LO	
	jsr	STASHB	; and show it to the world
	rts


; ---
; SET
; ---
; SET VARIABLE [ARG1] EQUAL TO [ARG2]

ZSET:	LDA	ARG2+LO	; MOVE THE VALUE
	STA	VALUE+LO	; INTO [VALUE]
	LDA	ARG2+HI	
	STA	VALUE+HI	
	LDA	ARG1+LO	; GET VARIABLE ID
	JMP	VARPUT	; AND CHANGE THE VARIABLE


; ----
; MOVE
; ----
; MOVE OBJECT [ARG1] INTO OBJECT [ARG2]
; (EZIP - EXPANDED FROM BYTE OBJECTS TO WORD OBJECTS)

ZMOVE:
	jsr	ZREMOV		; REMOVE FIRST - CUT ARG1 OUT OF WHERE IT IS
;
; Make [ARG1] be first in [ARG2]'s chain.
;
	lda	ARG2+LO	
	ldx	ARG2+HI		; Get parent's address
	jsr	OBJLOC		; into [FPC]
	lda	#10		; point to FIRST slot
	jsr	ADDFPC		; okay
	jsr	FETCHB		; get old first one (MSB)
	sta	K+HI		; save it
	jsr	NEXTFPC		; point to next part
	jsr	FETCHB		; and get it
	sta	K+LO		; okay, saved
	jsr	FP2SP		; make FPC == SPC
;
; now make object [ARG1] be first in object [ARG2]
; SPC is currently pointing to LSB of [ARG2]'s FIRST slot
;
	lda	ARG1+LO		; stash away low part first
	jsr	STASHB		; stashed
	jsr	PREVSPC		; point to high part
	lda	ARG1+HI		; get hi part of source
	jsr	STASHB		; save it
;
; now point to object [ARG1] to update its slots
;
	ldx	ARG1+HI		; put hi part here
	lda	ARG1+LO		; and lo part here for OBJLOC
	jsr	OBJLOC		; get me the SOURCE of all confusion
	lda	#6		; point to LOC slot
	jsr	ADDFPC		; and now FPC points there
	jsr	FP2SP		; now make it the STASH pointer
;
; update [ARG1]s LOC pointer to be [ARG2]
; SPC points to LOC slot
;
	lda	ARG2+HI		; get PARENT number
	jsr	STASHB		; and save it in LOC
	jsr	NEXTSPC		; and point to LSB of LOC
	lda	ARG2+LO		; got parent's lowness
	jsr	STASHB		; and saved it in ARG1
;
; Move old FIRST object and make it [ARG1]s NEXT
; now SPC points to [ARG1]s NEXT slot
;
	jsr	NEXTSPC		; point to MSB of NEXT
	lda	K+HI		; get old FIRST one
	jsr	STASHB	; and save it
	jsr	NEXTSPC	; and point to LSB now
	lda	K+LO	; get LSB of this
	jsr	STASHB	; saved
	rts

; ---
; GET
; ---
; RETURN ITEM [ARG2] IN WORD-TABLE [ARG1]

ZGET:	JSR	WCALC		; CALC ADDRESS
	JSR	GETBYT		; GET 1ST BYTE (MSB)
DOGET:	STA	VALUE+HI	; SAVE MSB
	JSR	GETBYT		; GET LSB
	STA	VALUE+LO	; SAVE AND
	JMP	PUTVAL		; HAND IT OVER

; ----
; GETB
; ----
; RETURN ITEM [ARG2] IN BYTE-TABLE AT [ARG1]

ZGETB:
	JSR	BCALC	
	LDA	#0	
	BEQ	DOGET		; [A] = 0, SO CLEAR MSB OF [VALUE]


; --------------------
; CALC TABLE ADDRESSES
; --------------------
; WORD-ALIGNED ENTRY

WCALC:
	ASL	ARG2+LO	; WORD-ALIGN FOR
	ROL	ARG2+HI	; WORD ACCESS
;
; BYTE-ALIGNED ENTRY
;
BCALC:
	LDA	ARG2+LO	; ADD BASE ADDR OF TABLE
	CLC		; TO ITEM
	ADC	ARG1+LO	; INDEX
	STA	MPCL	
	LDA	ARG2+HI	; SAME FOR MSBS
	ADC	ARG1+HI	
	STA	MPCM	
	LDA	#0	
	ADC	#0	; PICK UP CARRY FROM MPCM
	STA	MPCH	; TO GET TOP BIT
	JMP	VLDMPC	


; ----
; GETP
; ----
; RETURN PROPERTY [ARG2] OF OBJECT [ARG1] ;
; IF NO PROP [ARG2], RETURN [ARG2]'TH ELEMENT OF OBJECT #0

ZGETP:
	jsr	PROPB		; set up FPC
GETP1:
	jsr	PROPN		; GET ID OF PROP TBL
	cmp	ARG2+LO		; Compare PROP ID
	beq	GETP3		; FOUND IT
	bcc	GETP2		; NOT THERE
	jsr	PROPNX		; GET NEXT PROP, ALIGN [FPC] TO IT
	jmp	GETP1		; TRY AGAIN WITH NEXT PROP
;
; PROPERTY NOT THERE, GET DEFAULT
;
GETP2:
	lda	ARG2+LO		; GET PROPERTY #
	sec			; ZERO-ALIGN IT
	sbc	#1	
	asl	A		; WORD-ALIGN IT
	clc			; add in table start
	adc	ZBEGIN+ZOBJEC+1	; add lo part
	sta	FPCL		; save for fetch
	lda	ZBEGIN+ZOBJEC	; now get hi part
	adc	#0		; pick up carry, if any
	jsr	SETPC		; get memory spot for this page
	sta	FPCH		; page and
	sty	FPCBNK		; bank
	jsr	FETCHB		; GET MSB OF PROPERTY
	sta	VALUE+HI	
	jsr	NEXTFPC
	jsr	FETCHB		; DO SAME WITH LSB
	sta	VALUE+LO	
	jmp	PUTVAL		; RETURN DEFAULT IN [VALUE]
GETP3:
	jsr	PROPL		; GET LENGTH OF PROP INTO [A]
	tax			; save [A]
	jsr	NEXTFPC		; MAKE [FPC] POINT TO 1ST BYTE OF PROP
	cpx	#1		; IF LENGTH =1
	beq	GETPB		; GET A BYTE PROPERTY
	cpx	#2		; IF LENGTH = 2
	beq	GETPW		; GET A WORD PROPERTY

	; *** ERROR #7: PROPERTY LENGTH ***

	LDA	#7	
	JMP	ZERROR	
;
; GET A 1-BYTE PROPERTY
;
GETPB:
	jsr	FETCHB		; GET LSB INTO [A]
	sta	VALUE+LO	; STORE LSB
	lda	#0		; CLEAR MSB
	sta	VALUE+HI	; and zero it
	beq	ETPEX	
;
; GET A 2-BYTE PROPERTY
;
GETPW:
	jsr	FETCHB		; GET MSB
	sta	VALUE+HI	; store MSB
	jsr	NEXTFPC		; POINT TO LSB
	jsr	FETCHB		; GET IT INTO [A]
	sta	VALUE+LO	; AND MSB
ETPEX:
	jmp	PUTVAL	

; -----
; GETPT
; -----
; RETURN POINTER TO PROP TABLE [ARG2]
; IN OBJECT [ARG1]

ZGETPT:
	lda	ARG1+LO
	ldx	ARG1+HI		; (EZIP)
	jsr	OBJLOC		; put location into [FPC]
	lda	#12		; get offset table pointer
	jsr	ADDFPC		; and point to it
	jsr	FETCHB		; get MSB of table pointer
	pha			; save page
	jsr	NEXTFPC		; point to LSB
	jsr	FETCHB		; and get it
	sta	FPCL		; and save it
	pla			; get page address back
	jsr	SETPC		; and absolutize it
	sta	FPCH		; and save page
	sty	FPCBNK		; and bank
	jsr	FETCHB		; GET LENGTH OF SHORT DESC
	asl	A		; WORD-ALIGN IT
	tay			; now point to just past it
	iny			; thank you
	tya			; for adding
	jsr	ADDFPC		; and POINT JUST PAST THE DESCRIPTION
GETPT1:
	jsr	PROPN		; get prop ID
	cmp	ARG2+LO		; CHECK ID
	beq	GETPT2
	bcs	DDD		; .GE. so check some more
	jmp	DORET		; BEYOND IT, SO NOT THERE
DDD:
	jsr	PROPNX		; point to next one
	jmp	GETPT1		; next please
GETPT2:
	jsr	PROPL		; get the length of this property
	jsr	NEXTFPC		; INC TO POINT AT PROPERTY VALUE (EZIP)
;
; now subtract absolute address to get relative address for returning
; what a pain in the neck
;
	lda	FPCBNK		; check which bank we are in
	beq	GETPB0		; in main bank, it's easier
;
; if in AUX bank, then we must subtract beginning of AUX mem, then
; add in size of MAIN bank
;
	lda	FPCH		; get page
	cmp	# HIGH Z3BEGIN	; in part 2 of aux?
	bcs	GETPB3		; ayyup
	sec			; and subtract
	sbc	# HIGH Z2BEGIN	; first page in aux
	clc			; now add in how many pages in main
	adc	#P2PAGE		; now we have relative start
	bne	GETP15		; JUMP to putval
GETPB3:
	sec			; and subtract
	sbc	# HIGH Z3BEGIN	; first page in aux, part 2
	clc			; now add in how many pages in main
	adc	#P3PAGE		; now we have relative start
	bne	GETP15		; jump to putval	
GETPB0:
	lda	FPCH		; get page in main mem
	sec			; and then subtract
	sbc	# HIGH ZBEGIN	; start of main mem
GETP15:
	sta	VALUE+HI	; save hi part for returning
	lda	FPCL		; and just save lo part
	sta	VALUE+LO	; okay?
	jmp	PUTVAL		; AND RETURN
DORET:	
	jmp	RET0		; ELSE RETURN A ZERO


; -----
; NEXTP
; -----
; RETURN INDEX # OF PROP FOLLOWING PROP [ARG2] IN OBJECT [AR
; RETURN ZERO IF LAST ; RETURN FIRST IF [ARG2]=0; ERROR IF NO

ZNEXTP:
	JSR	PROPB	; ALIGN [FPC] AT PROPERTY TBL'S 1ST ENTRY
	LDA	ARG2+LO	; IF [ARG2]=0
	BEQ	NXTP3	; RETURN "FIRST" SLOT
NXTP1:
	JSR	PROPN	; FETCH PROPERTY #
	CMP	ARG2+LO	; COMPARE TO TARGET #
	BEQ	NXTP2	; FOUND IT!
	BCC	DORET	; LAST PROP, SO RETURN ZERO
	JSR	PROPNX	; ELSE TRY NEXT PROPERTY (EZIP)
	JMP	NXTP1	
NXTP2:	JSR	PROPNX	; POINT TO FOLLOWING PROPERTY
NXTP3:	JSR	PROPN	; GET THE PROPERTY #
	LDX	#0	; FOR PUTBYT (EZIP)
	JMP	PUTBYT	; AND RETURN IT


; ---
; ADD
; ---
; RETURN [ARG1] + [ARG2]

ZADD:	LDA	ARG1+LO	; ADD LSBS
	CLC		
	ADC	ARG2+LO	
	TAX		; SAVE LSB HERE
	LDA	ARG1+HI	; ADD MSBS
	ADC	ARG2+HI	
	JMP	VEXIT	


; ---
; SUB
; ---
; RETURN [ARG1] - [ARG2]

ZSUB:	LDA	ARG1+LO	; SUBTRACT LSBS
	SEC		
	SBC	ARG2+LO	
	TAX		; SAVE LSB HERE
	LDA	ARG1+HI	; SUBTRACT MSBS
	SBC	ARG2+HI	
	JMP	VEXIT	; EXIT WITH [X]=LSB, [A]=MSB


; ---
; MUL
; ---
; RETURN [ARG1] * [ARG2]

ZMUL:	JSR	MINIT	; INIT THINGS
ZMLOOP:	ROR	MTEMP+HI	
	ROR	MTEMP+LO	
	ROR	ARG2+HI	
	ROR	ARG2+LO	
	BCC	ZMNEXT	
	LDA	ARG1+LO	
	CLC		
	ADC	MTEMP+LO	
	STA	MTEMP+LO	
	LDA	ARG1+HI	
	ADC	MTEMP+HI	
	STA	MTEMP+HI	
ZMNEXT:	DEX		
	BPL	ZMLOOP	
	LDX	ARG2+LO	; PUT LSB OF PRODUCT
	LDA	ARG2+HI	; AND MSB
	JMP	VEXIT	; WHERE "VEXIT" EXPECTS THEM


; ---
; DIV
; ---
; RETURN QUOTIENT OF [ARG1] / [ARG2]

ZDIV:	JSR	DIVIDE	
	LDX	QUOT+LO	
	LDA	QUOT+HI	
	JMP	VEXIT	


; ---
; MOD
; ---
; RETURN REMAINDER OF [ARG1] / [ARG2]

ZMOD:	JSR	DIVIDE	
	LDX	REMAIN+LO	; FETCH THE REMAINDER
	LDA	REMAIN+HI	; IN [REMAIN]
	JMP	VEXIT	; AND RETURN IT


; ---------------
; SIGNED DIVISION
; ---------------
; ENTRY: DIVIDEND IN [ARG1], DIVISOR IN [ARG2]
; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]

DIVIDE:
	LDA	ARG1+HI	; SIGN OF REMAINDER
	STA	RSIGN	; IS THE SIGN OF THE DIVIDEND
	EOR	ARG2+HI	; SIGN OF QUOTIENT IS POSITIVE
	STA	QSIGN	; IF SIGNS OF TERMS ARE THE SAME
	LDA	ARG1+LO	; MOVE [ARG1] TO [QUOT]
	STA	QUOT+LO	
	LDA	ARG1+HI	
	STA	QUOT+HI	; IF DIVIDEND IS POSITIVE
	BPL	ABSDIV	; MOVE DIVISOR
	JSR	ABQUOT	; ELSE CALC ABS(DIVIDEND) FIRST
ABSDIV:	LDA	ARG2+LO	
	STA	REMAIN+LO	
	LDA	ARG2+HI	
	STA	REMAIN+HI	; IF REMAINDER IS POSITIVE
	BPL	GODIV	; WE'RE READY TO DIVIDE
	JSR	ABREM	; ELSE CALC ABS(DIVISOR)
GODIV:	JSR	UDIV	; DO UNSIGNED DIVIDE
	LDA	QSIGN	; SHOULD QUOTIENT BE FLIPPED?
	BPL	RFLIP	; NO, TEST REMAINDER
	JSR	ABQUOT	; ELSE GET ABSOLUTE VALUE
RFLIP:	LDA	RSIGN	; SHOULD EMAINDER BE FLIPPED?
	BPL	DIVEX	; NO, WE'RE DONE

	; ELSE FALL THROUGH ...


; ----------------
; CALC ABS(REMAIN)
; ----------------

ABREM:	LDA	#0	
	SEC		
	SBC	REMAIN+LO	
	STA	REMAIN+LO	
	LDA	#0	
	SBC	REMAIN+HI	
	STA	REMAIN+HI	
DIVEX:	RTS		


; --------------
; CALC ABS(QUOT)
; --------------

ABQUOT:	LDA	#0	
	SEC		
	SBC	QUOT+LO	
	STA	QUOT+LO	
	LDA	#0	
	SBC	QUOT+HI	
	STA	QUOT+HI	
	RTS		


; -----------------
; UNSIGNED DIVISION
; -----------------
; ENTRY: DIVIDEND IN [QUOT], DIVISOR IN [REMAIN]
; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]

UDIV:
	LDA	REMAIN+LO	; CHECK [REMAIN]
	ORA	REMAIN+HI	; BEFORE PROCEEDING
	BEQ	DIVERR	; CAN'T DIVIDE BY ZERO!
	JSR	MINIT	; SET IT ALL UP
UDLOOP:	ROL	QUOT+LO	
	ROL	QUOT+HI	
	ROL	MTEMP+LO	
	ROL	MTEMP+HI	
	LDA	MTEMP+LO	
	SEC		
	SBC	REMAIN+LO	
	TAY		; SAVE HERE
	LDA	MTEMP+HI	
	SBC	REMAIN+HI	
	BCC	UDNEXT	
	STY	MTEMP+LO	
	STA	MTEMP+HI	
UDNEXT:	DEX		
	BNE	UDLOOP	
	ROL	QUOT+LO	; SHIFT LAST CARRY FOR QUOTIENT
	ROL	QUOT+HI	
	LDA	MTEMP+LO	; MOVE REMAINDER
	STA	REMAIN+LO	; INTO [REMAIN]
	LDA	MTEMP+HI	
	STA	REMAIN+HI	
	RTS		

	; *** ERROR #8: DIVISION BY ZERO ***

DIVERR:	LDA	#8	
	JMP	ZERROR	


; ---------
; MATH INIT
; ---------

MINIT:	LDX	#16		; INIT LOOPING INDEX
	LDA	#0
	STA	MTEMP+LO	; CLEAR TEMP
	STA	MTEMP+HI	; REGISTER
	CLC			; AND CARRY
	RTS


; -----
; THROW
; -----

ZTHROW:	LDA	ARG2+LO		; SET ZSTACK POINTER
	STA	OLDZSP+LO	; UP FOR ZRET
	LDA	ARG2+HI
	STA	OLDZSP+HI
	JMP	ZRET

	END
