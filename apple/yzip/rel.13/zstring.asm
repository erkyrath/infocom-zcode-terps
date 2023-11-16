	PAGE	
	STTL "--- Z-STRING HANDLERS ---"

; -----------------------
; POINT MPC TO ZSTRING IN [I], using SOFF
; -----------------------

SETSTR:
	lda	I+LO
	sta	MPCL		; save in lo part	
	lda	I+HI
	sta	MPCM		; middle part
	lda	#0		; clear hi part
	sta	MPCH		; okay, we did
	asl	MPCL		; *2
	rol	MPCM
	rol	MPCH
	asl	MPCL		; *4
	rol	MPCM
	rol	MPCH
;
; now add the offset
;
	lda	MPCL		; carry clear from above
	adc	SOFFL		; add lo part of offset
	sta	MPCL		; save
	lda	MPCM
	adc	SOFFM
	sta	MPCM
	lda	MPCH
	adc	SOFFH
	sta	MPCH
	jmp	VLDMPC		; make MPCPNT to it


ZSTEX:	RTS		

; -----------------------
; PRINT Z-STRING AT [MPC]
; -----------------------
PZSTR:
        ldx	#0	
	stx	PSET	        ; ASSUME PERMANENT CHARSET
	stx	ZFLAG	        ; CLEAR BYTE FLAG
	dex		        ; = $FF
	stx	TSET	        ; NO TEMPSET ACTIVE
PZTOP:
        jsr	GETZCH	        ; GET A Z-CHAR
	bcs	ZSTEX	        ; END OF STRING IF CARRY IS SET
	sta	ZCHAR	        ; ELSE SAVE CHAR HERE
	tax		        ; SET FLAGS
	beq	BLANK	        ; PRINT SPACE IF CHAR = 0
	cmp	#4	        ; IS THIS AN F-WORD?
	bcc	DOFREQ	        ; APPARENTLY SO
	cmp	#6	        ; PERHAPS A SHIFT CODE?
	bcc	NEWSET	        ; YES, CHANGE CHARSETS
	jsr	GETSET	        ; ELSE GET CHARSET
	tax		        ; SET FLAGS
	bne	SET1	        ; SKIP IF NOT CHARSET #0
;
; PRINT A LOWER-CASE CHAR (CHARSET #0)
;
        lda     #$FA            ; what to add to get offset into char table
TOASC:
        sta     TOASCM+1        ; modify code
        lda     ZCHAR           ; use char as offset
        clc                     ; make char be an index
TOASCM: adc     #6              ; we just did
        tax                     ; now use as index
        lda     CHARSET,X       ; go get that char in charset zero
SHOVE:
        jsr	COUT	        ; SHOW THE CHAR
	jmp	PZTOP	        ; AND GRAB NEXT CHAR
;
; PRINT AN UPPER-CASE CHAR (CHARSET #1)
;
SET1:
        cmp	#1	        ; make sure it's set #1
	bne	SET2	        ; else must be set #2
	lda	#20             ; skip into Charset 1 part of table
	bne	TOASC	        ; fix just like the others
;
; PRINT FROM CHARSET #2
;
SET2:
        lda	ZCHAR	        ; retrieve the z-char
        cmp     #6              ; is it a special ascii char?
        beq     DIRECT          ; yes, so do it special
        lda     #46             ; nothing special, just get offset
        bne     TOASC           ; and jump
;
; DECODE A "DIRECT" ASCII CHAR
;
DIRECT:
        jsr	GETZCH	        ; FETCH NEXT Z-CHAR
	asl	A		
	asl	A		
	asl	A		
	asl	A		
	asl	A	        ; SHIFT INTO POSITION
	sta	ZCHAR	        ; AND SAVE HERE
	jsr	GETZCH	        ; GRAB YET ANOTHER Z-CHAR
	ora	ZCHAR	        ; SUPERIMPOSE THE 2ND BYTE
	bne	SHOVE	        ; AND PRINT THE RESULT
;
; PRINT A SPACE
;
BLANK:
        lda	#SPACE	        ; ASCII SPACE CHAR
	bne	SHOVE	

	; CHANGE CHARSET

NEWSET:	SEC		; CONVERT THE SHIFT CODE
	SBC	#3	; TO 1 OR 2
	TAY		
	JSR	GETSET	; IS MODE TEMPORARY?
	BNE	TOPERM	; YES, DO A PERMSHIFT
	STY	TSET	; ELSE JUST A TEMPSHIFT
	JMP	PZTOP	; AND CONTINUE
TOPERM:	STY	PSET	; SET PERM CHARSET
	CMP	PSET	; SAME AS BEFORE?
	BEQ	PZTOP	; YES, CONTINUE
	LDA	#0	
	STA	PSET	; ELSE RESET CHARSET
	BEQ	PZTOP	; BEFORE LOOPING BACK

	; PRINT AN F-WORD

DOFREQ:	SEC		
	SBC	#1	; ZERO-ALIGN THE CODE
	ASL	A	; AND MULTIPLY TIMES 64
	ASL	A	; TO OBTAIN THE SEGMENT OFFSET
	ASL	A	; INTO THE F-WORDS TABLE
	ASL	A		
	ASL	A		
	ASL	A		
	STA	OFFSET	; SAVE OFFSET FOR LATER
	JSR	GETZCH	; NOW GET THE F-WORD POINTER
	ASL	A	; WORD-ALIGN IT
	CLC		; AND
	ADC	OFFSET	; ADD THE SEGMENT OFFSET
;
; set up FPC to point to FWORDS table
;
	ldx	FWORDS+ABANK
	stx	FPCBNK
	ldx	FWORDS+HI
	stx	FPCH
	ldx	FWORDS+LO
	stx	FPCL
	jsr	ADDFPC		; add offset of the F-word
	jsr	FETCHB		; and get MSB of F-word
	sta	I+HI		; and save it
	jsr	NEXTFPC		; and point to LSB
	jsr	FETCHB		; and get it
	sta	I+LO		; and save it
;
; SAVE THE STATE OF CURRENT Z-STRING
;
	LDA	MPCH	
	PHA		
	LDA	MPCM	
	PHA		
	LDA	MPCL	
	PHA		
	LDA	PSET	
	PHA		
	LDA	ZFLAG	
	PHA		
	LDA	ZWORD+HI	
	PHA		
	LDA	ZWORD+LO	
	PHA		
	JSR	SETFWD	; PRINT THE Z-STRING
	JSR	PZSTR	; IN [I]
;
; RESTORE OLD Z-STRING
;
	PLA		
	STA	ZWORD+LO	
	PLA		
	STA	ZWORD+HI	
	PLA		
	STA	ZFLAG	
	PLA		
	STA	PSET	
	PLA		
	STA	MPCL	
	PLA		
	STA	MPCM	
	PLA		
	STA	MPCH	
	LDX	#$FF	
	STX	TSET	; DISABLE TEMP CHARSET
	JSR	VLDMPC	
	JMP	PZTOP	; CONTINUE INNOCENTLY


; ----------------------
; RETURN CURRENT CHARSET
; ----------------------

GETSET:	LDA	TSET	
	BPL	GS	
	LDA	PSET	
	RTS		
GS:	LDY	#$FF	
	STY	TSET	
	RTS		


; -------------------------
; POINT [I] AT FWORD STRING
; -------------------------

SETFWD:	LDA	I+LO	; WORD-ALIGN THE ADDRESS
	ASL	A		
	STA	MPCL	
	LDA	I+HI	
	ROL	A		
	STA	MPCM	
	LDA	#0	
	ROL	A		
	STA	MPCH	
	JMP	VLDMPC	


; -----------------
; FETCH NEXT Z-CHAR
; -----------------

GETZCH:	LDA	ZFLAG	; WHICH BYTE IS THIS?
	BPL	GTZ0	; $FF = LAST
	SEC		; SET CARRY TO INDICATE
	RTS		; NO MORE CHARS
GTZ0:	BNE	GETZ1	; NOT FIRST CHAR, EITHER

	; GET A Z-WORD INTO [ZWORD], RETURN 1ST CHAR IN TRIPLET

	INC	ZFLAG	; UPDATE CHAR COUNT
	JSR	GETBYT	; GET TRIPLET AT [MPC]
	STA	ZWORD+HI	; INTO [ZWORD]
	JSR	GETBYT	
	STA	ZWORD+LO	
	LDA	ZWORD+HI	
	LSR	A		
	LSR	A	; SHIFT 1ST CHAR INTO PLACE
	JMP	GTEXIT	; AND RETURN IT
GETZ1:	SEC		
	SBC	#1	
	BNE	GETZ2	; LAST CHAR IN TRIPLET IF ZERO
	LDA	#2	; ELSE
	STA	ZFLAG	; RESET CHAR INDEX
	LDA	ZWORD+LO	; GET BOTTOM HALF OF TRIPLET
	STA	I+LO	; MOVE HERE FOR SHIFTING
	LDA	ZWORD+HI	; GET TOP HALF
	ASL	I+LO	; SHIFT THE TOP 3 BITS OF LOWER HALF
	ROL	A	; INTO THE BOTTOM OF THE TOP HALF
	ASL	I+LO	
	ROL	A		
	ASL	I+LO	
	ROL	A		
	JMP	GTEXIT	
GETZ2:	LDA	#0	; SET FLAG TO INDICATE
	STA	ZFLAG	; END OF TRIPLET
	LDA	ZWORD+HI	; TEST TOP HALF OF TRIPLET
	BPL	GETZ3	; CONTINUE IF NOT END OF STRING
	LDA	#$FF	; ELSE
	STA	ZFLAG	; INDICATE LAST TRIPLET IN STRING
GETZ3:	LDA	ZWORD+LO	; GET BOTTOM HALF OF TRIPLET
GTEXIT:	AND	#%00011111	; MASK OUT GARBAGE BITS
	CLC		
	RTS		


; ---------------------------------
; CONVERT [IN] TO Z-STRING IN [OUT]
; ---------------------------------
CONZST:
        lda	#5	        ; FILL OUTPUT BUFFER
	ldx	#8	        ; WITH PAD CHARS ($05)
CZSL:
        sta	OUT,X	
	dex		
	bpl	CZSL	

	lda	#9	        ; INIT
	sta	CONCNT	        ; CHAR COUNT
	lda	#0	        ; CLEAR
	sta	CONIN	        ; SOURCE AND
	sta	CONOUT	        ; OUTPUT INDEXES
CONTOP:
        ldx	CONIN	        ; fetch source index
	inc	CONIN	        ; and update
	lda	IN,X	        ; grab an ascii char
	sta	ZCHAR	        ; save it here
	bne	NEXTZ	        ; continue if char was nz
	lda	#5	        ; else ship out
	bne	CSHIP1	        ; a pad char
NEXTZ:
        jsr     FINDCHAR        ; find out where it tis
        beq     CSHIP           ; no shift for charset zero
	clc		        ; else do a temp-shift
	adc	#3	        ; 4 = charset 1, 5 = charset 2
        jsr     CSTASH          ; save the char in outbuf
        cmp     #5              ; charset 2?
        bne     CSHIP           ; nope
        cpx     #6              ; ascii escape?
        bne     CSHIP           ; nope
;
; Handle special Ascii escape sequence
;
        txa                     ; get ASCII alert char (#6)        
        jsr     CSTASH          ; shove it away
;
; do 1st half of "direct"
;
	lda	ZCHAR           ; re-fetch char        
	lsr	A               ; get upper 2 bits in lower 2 bits
	lsr	A
	lsr	A
	lsr	A
	lsr	A
        jsr     CSTASH
;
; SEND 2ND HALF OF "DIRECT"
;
	lda	ZCHAR	        ; get char yet again
	and	#%00011111	; and get lower 5 bits
        tax                     ; this is where it is expected
;
; SHIP Z-CHAR TO OUTPUT BUFFER
;
CSHIP:
        txa                     ; get char
CSHIP1:
        jsr     CSTASH          ; put char away
        bne     CONTOP          ; do again
;
; FINDCHAR - look through the charset table for the character.  If found,
;       figger out which shift it is.  If not found, return charset 2, 
;       character #6.
;  Enter:
;       [A] = Char we are looking for
;  Returns:
;       [A] = Charset (0-2)
;       [X] = Character # (6-31)
;
FINDCHAR:
        ldx     #0              ; start at first char, first set
        ldy     #78             ; there are 78 characters
FNDCL:
        cmp     CHARSET,X       ; found it?
        beq     FNDCLX          ; yes
        inx                     ; next char
        dey                     ; count char
        bne     FNDCL           ; go check next char
;
; not found in table, use ASCII escape sequence
;
        lda     #2              ; escape sequence is char set 2
        ldx     #6              ; character 6
        rts                     ; and done
FNDCLX:
        txa                     ; put here for compares/action
        ldy     #0              ; this is char set 0
        ldx     #$FA            ; what to "subtract" to get +6
        cmp     #26             ; how we doin'?
        bcc     FNDCHX          ; all done
        iny                     ; char set 1
        ldx     #20             ; char set 1 offset
        cmp     #52             ; well?
        bcc    FNDCHX           ; must be char set 1
        ldx     #46             ; for char set 2 setting up
        iny                     ; must be char set 2 then
FNDCHX:
        stx     FNDCHM+1        ; what to subtract to get offset
        sec                     ; doing subtract
FNDCHM: sbc     #6              ; make good offset
        tax                     ; put here for return
        tya                     ; and here to set flag
        rts
;
; CSTASH - put the char in OUT.  If we run out of room don't return, just
;       jump to ZCRUSH as final destination
;   [A] - char to be put away
;
CSTASH:
	ldy	CONOUT	        ; fetch output index
	sta	OUT,Y	        ; send the shift char
	inc	CONOUT	        ; update index
	dec	CONCNT	        ; and char count
	bne	CSTX            ; plenty more room
        pla                     ; get rid of return spot
        pla                     ; fine
	jmp	ZCRUSH	        ; if out of room, crush 'em!
CSTX:
        rts
; ----------------------
; CRUSH Z-CHARS IN [OUT], mushing them into small 5 bit pieces
; ----------------------
ZCRUSH:
        LDA	OUT+1	        ; GET 2ND Z-CHAR
	ASL	A	        ; SHIFT BITS INTO POSITION
	ASL	A		
	ASL	A		
	ASL	A		
	ROL	OUT	        ; ALONG WITH 1ST Z-CHAR
	ASL	A		
	ROL	OUT	
	ORA	OUT+2	        ; SUPERIMPOSE 3RD Z-CHAR
	STA	OUT+1	
	LDA	OUT+4	        ; GET 5TH Z-CHAR
	ASL	A	        ; SHIFT BITS
	ASL	A		
	ASL	A		
	ASL	A		
	ROL	OUT+3	        ; ALONG WITH 4TH Z-CHAR
	ASL	A		
	ROL	OUT+3	
	ORA	OUT+5	        ; SUPERIMPOSE 6TH Z-CHAR
	TAX		        ; SAVE HERE
	LDA	OUT+3	        ; GRAB 4TH Z-CHAR
	STA	OUT+2	        ; MOVE CRUSHED Z-WORD
	STX	OUT+3	        ; INTO PLACE
	LDA	OUT+7	        ; GET 8TH Z-CHAR (EZIP)
	ASL	A	        ; SHIFT BITS
	ASL	A		
	ASL	A		
	ASL	A		
	ROL	OUT+6	        ; ALONG WITH 7TH Z-CHAR
	ASL	A		
	ROL	OUT+6	
	ORA	OUT+8	        ; SUPERIMPOSE 9TH Z-CHAR
	STA	OUT+5	        ; SAVE HERE
	LDA	OUT+6	        ; GRAB 7TH Z-CHAR
	ORA	#%10000000	; SET HIGH BIT
	STA	OUT+4	        ; MOVE CRUSHED Z-WORD INTO PLACE
	RTS		

	END

