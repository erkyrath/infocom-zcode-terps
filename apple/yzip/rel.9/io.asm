	STTL "--- GAME I/O: APPLE II ---"
	PAGE	

	; --------------
	; INTERNAL ERROR
	; --------------
	; ENTRY: ERROR CODE IN [A]
	; EXIT: HA!

ERRM:	DB	EOL,"Internal error "
ERRML	EQU	$-ERRM

ZERROR:
	pha			; save err number
        jsr     SWAP2INFOW      ; go to the info window (window 0)`
	DLINE	ERRM		; print message
	pla			; get error number
	jsr	HEXNUM		; print error number
	jmp	ZQUIT1		; and die


	; ----
	; QUIT
	; ----

ZQUIT:	JSR	ZCRLF		; FLUSH BUFFER
ZQUIT1:
	DLINE	ENDM		; print ending message
	jsr	GETRET		; wait for <CR>
        lda     INFODOS         ; in dinky-dos?
        beq     ZQUIT2          ; nope
        lda     #0              ; clear power up byte
        sta     $3F4            ; make it do cold boot        
        lda     RESET_VECTOR+HI ; now, set up where to die to
        sta     ZQUITR1+2
        lda     RESET_VECTOR+LO
        sta     ZQUITR1+1
        lda     RDROM           ; get ROM back
ZQUITR1:
        jmp     RESET_VECTOR    ; just reset things
;
; re-enable /RAM
;
ZQUIT2:
	ldx	DEVCNT		; put device in at end
	inx			; point to one past end
	stx	DEVCNT		; show new count
	lda	#$BF		; /RAM ID
	sta	DEVNUM,X	; save it
	lda	OLDVEC+HI	; restore old vector
	sta	RAMVEC+HI
	lda	OLDVEC+LO
	sta	RAMVEC+LO
;
; now format /RAM
;
	lda	#3		; stash into FORMAT parm block
	sta	$42		; it goes here, oddly enough
	lda	#$B0		; device id for /RAM
	sta	$43		; and here it goes
	lda	#>GAME1FIO	; IO buffer
	sta	$45		; this is where it goes
	lda	#<GAME1FIO	; lsb
	sta	$44
	lda	RDBNK2		; set up card for driver
	jsr	RAMDRV		; format /RAM
	lda	RDROM		; get ROM back
	
	sta	TEXTSW+ON	; turn on text
	QUIT	QUIT_PB		
QUIT_PB:
	db	4		; 4 parms
	db	0,0,0,0,0,0	; just zeros
RAMDRV:
	jmp	(RAMVEC)	; goto RAM driver

ENDM:	DB	EOL,"End of session."
ENDML	EQU	$-ENDM

; -------
; RESTART
; -------
BOOT_RD:
	db 	4
	db	0
	dw	BORG		; put into where it wants
	dw	$FFFF		; read lots
BOOTNAME:
	db	BNAMEL		; length will go here
        db      "INFOCOM.SYSTEM"
BNAMEL  EQU     $-BOOTNAME-1
ZSTART:
	jsr	CLOSE_GAME	; make sure everything is closed

	lda	#$FF		; just do a clear -1
	sta	ARG1+LO		; done
	jsr	ZCLR		; to clear screen and set up window 0
	lda	#<BOOTNAME	; now, set open file name
	sta	OPEN_FILE+OP_PATHNAME+LO
	lda	#>BOOTNAME
	sta	OPEN_FILE+OP_PATHNAME+HI
	lda	#'1'		; set disk name to disk #1
        sta     SAVENUM         ; show open_gamef
        jsr     OPEN_GAMEF      ; okay

	lda	OPEN_FILE+OP_REFNUM ; get ref num
	sta	BOOT_RD+RD_REFNUM ; save ref num
	sta	CLOSE_PB+CL_REFNUM
	READ	BOOT_RD		; read in boot system
	bcc	ZSTRTX		; okay, everything is fine
	jmp	DISK_FATAL	; die otherwise
ZSTRTX:
	CLOSE	CLOSE_PB	; and close it up
	jmp	BORG		; and go to it
	
	; --------------------------------
	; RETURN RANDOM BYTES IN [A] & [X]
	; --------------------------------
RANDOM:
        inc	RAND+HI	
	dec	RAND+LO	
	lda	RAND+LO         ; get LSB
        adc     RAND+HI         ; add MSB
        and     #$7F            ; don't let it go into $C0 page
        sta     RAND+HI         ; new MSB        
        tay                     ; get random offset
        lda     (RAND),Y        ; get random number
        tax                     ; save in X
        adc     RAND+LO         ; and go to another spot
        and     #$7F            ; don't go above $80
        sta     RAND+HI         ; okay
        tay                     ; more randomness
        lda     (RAND),Y        ; and get it again
        ora     RAND+HI         ; set more bits
	rts		


	; -------------------
	; Z-PRINT A CHARACTER
	; -------------------
	; ENTRY: ASCII CHAR IN [A]
	; COMMENT: SCRIPTING IS HANDLED IN UNBUFR AND FLUSH,
	; SO CAN OUTPUT TO PRINTER AS A LINE.  TABLE AND SCREEN
	; OUTPUT IS SET UP HERE, HANDLED A BYTE AT A TIME
	; (DIROUT CHANGES 6/24/85)

COUT:
	sta	IOCHAR		; HOLD IT A SEC
	ldx	TABLEF		; OUTPUT TO TABLE?
	beq	COUT1		; NO
	ldx	FMTTBL		; formatted table?
	bne	COUT5		; yes, so just do it normal
	jmp	TBLOUT		; just put into table
COUT1:
	ldx	SCREENF		; OUTPUT TO SCREEN?
	bne	COUT5		; YES
	ldx	SCRIPTF		; OUTPUT TO PRINTER?
	bne	COUT5		; YES
	rts			; NO, SO DONE
COUT5:
	lda	IOCHAR		; RETRIEVE CHAR
	bmi	COUT2		; highlight chars have no width
;
; this is an entry point for DLINE, so it doesn't check any of the above
; things.  Enter with character in [A]
;
DIRECT_OUT: 
	cmp	#EOL		; IF ASCII EOL, just handle it special
	bne	COUT0	

	jmp	ZCRLF		; DO IT
COUT0:
	bcs	COUT02          ; not control character
        cmp     #EOS            ; control-k (end of sentence)?
        beq     COUT01          ; yes, so put out 2 spaces
        cmp     #TAB            ; tab char?
        bne     CEX             ; ignore all other control chars
;
; this means do a couple of spaces
;
        lda     #SPACE          ; do 3 spaces for start of line
        jsr     COUT            ; a little recursion never hurt!
        ldx     CHRCNT          ; back to beginning of line?
        bne     COUT01          ; nope
COUT010:
        rts                     ; don't do any to start line
COUT01:
        lda     #SPACE          ; 2 more spaces
        jsr     COUT
        ldx     CHRCNT          ; back to zero?
        beq     COUT010         ; yes, so don't add to start of line

        lda     #SPACE          ; last one
        jmp     COUT            ; finally
COUT02:
	tax			; use char as index
	lda	FONTFLG		; get which font we be using
	beq	COUTF1		; must be variable width
	lda	#MONOFONT_W	; get width then of mono font
	bne	COUTF2		; okay, now do add
COUTF1:
	lda	CHWID,X		; get width of char
COUTF2:
	clc 			; get ready for add
	adc	LENGTH+LO	; ADD LINE LENGTH COUNTER
	sta	LENGTH+LO	; update length
	bcc	COUT0C		; no wrap
	inc	LENGTH+HI	; okay, wrap then
COUT0C:
	lda	LENGTH+HI	; get MSB
	cmp	XSIZE+HI	; check MSB first
	bcc	COUT2		; no need to check lsb
	bne	COUT00		; XSIZE < LENGTH?
	
	lda	LENGTH+LO	; check LSB
	cmp	XSIZE+LO	; END OF SCREEN LINE?
	bcc	COUT2		; haven't reached the end if XSIZE > LENGTH
COUT00:
	ldy	WRPFLG		; are we wrapping
	beq	CEX		; no, so truncate
	ldx	CHRCNT		; get character count
	lda	IOCHAR		; get the character
	sta	LBUFF,X		; save current char in buffer
	jmp	FLUSH		; YES, FLUSH THE LINE
COUT2:
	ldx	CHRCNT		; GET LINE POINTER
	lda	IOCHAR		; get char back
	sta	LBUFF,X		; ADD CHAR TO BUFFER
	inc	CHRCNT		; and point to next CHRCNT
CEX:
	rts
	
	; ---------------
	; OUTPUT TO TABLE
	; ---------------
;
; this one just puts a char into the table
;
TBLOUT:
	tax			; HOLD CHAR A SEC.
;
; figger out length of line in there
;
	lda	FONTFLG		; get which font we be using
	beq	TBLOUT1		; must be variable width
	lda	#MONOFONT_W	; get width then of mono font
	bne	TBLOUT2		; okay, now do add
TBLOUT1:
	lda	CHWID,X		; get width of char
TBLOUT2:
	clc			; add width
	adc	ZBEGIN+ZTWIDTH+1	; to current line width
	sta	ZBEGIN+ZTWIDTH+1	; save current width
	bcc	TBLOUT3		; no wrap
	inc	ZBEGIN+ZTWIDTH+0	; wrap, then dammit
TBLOUT3:
;
; PUT BYTE IN TABLE AT CURRENT OFFSET
;
	lda	DIRITM+LO	; ADD IN OFFSET
	clc		
	adc	DIRTBL+LO	
	sta	SPCL		; and make it lo part
	lda	DIRITM+HI	
	adc	DIRTBL+HI	
	jsr	SETPC		; set the PC
	sta	SPCH		; and this is high part
	sty	SPCBNK		; and the bank
	txa			; PICK UP ASCII CHAR
	jsr	STASHB		; and save it
;
; SET ITM OFFSET TO NEXT POSITION, INCREMENT COUNTER
;
	inc	DIRITM+LO	; INC OFFSET TO NEXT BYTE
	bne	TBLRTS	
	inc	DIRITM+HI	
TBLRTS:
	rts
;
; PUT LBUFF IN TABLE AT CURRENT OFFSET
;
TBLRTN:
	lda	DIRITM+LO	; get where we are
	clc
	adc	DIRTBL+LO	; ADD IN OFFSET
	sta	SPCL		; and make it lo part
	sta	FPCL		; save for later usage
	lda	DIRITM+HI	; get hi part
	adc	DIRTBL+HI	
	jsr	SETPC		; set the PC
	sta	SPCH		; and this is high part
	sta	FPCH		; saving
	sty	SPCBNK		; and the bank
	sty	FPCBNK		; and here too
	lda	#0		; start counter off at zero
	sta	J		; use J
	sta	K+HI		; this will be line length
	sta	K+LO		; both parts, please
	jsr	NEXTSPC		; point past the counter
TBLOOP:
	ldy	J		; get offset
	lda	LBUFF,Y		; get char
	tax			; save char
	bmi	TBLP1		; don't count hi light chars
	cmp	#SPACE		; see if less than a space
	bcc	TBLP1		; no width if <$20
	tax			; use char as index

	lda	FONTFLG		; get which font we be using
	beq	TBLF1		; must be variable width
	lda	#MONOFONT_W	; get width then of mono font
	bne	TBLF2		; okay, now do add
TBLF1:
	lda	CHWID,X		; get width of char
TBLF2:
	clc			; add width
	adc	K+LO		; to current line width
	sta	K+LO		; save current width
	bcc	TBLP1		; no wrap
	inc	K+HI		; wrap, then dammit
TBLP1:
	txa			; get char back
	jsr	STASHB		; and save it
	jsr	NEXTSPC		; next table entry
	inc	J		; point to next char
	dec	CHRCNT		; decrement counter
	bne	TBLOOP		; get next one
;
; now fill with necessary spaces
;
	lda	FONTFLG		; first, set up width to be added
	beq	TBLSPF1		; must be variable width font
	lda	#MONOFONT_W	; get mono width	
	bne	TBLSPF2		; okay
TBLSPF1:
	ldx	#SPACE		; get space index
	lda	CHWID,X		; okay
TBLSPF2:
	sta	ARG8		; use temporarily
TBLSP:
	lda	K+LO		; get how big line is
	clc			; add in space
	adc	ARG8		; pick up space width
	sta	K+LO		; saved
	bcc	TBLSP1		; no wrap?
	inc	K+HI		; yes, wrapping
TBLSP1:
	lda	XSIZE+HI	; check against end
	cmp 	K+HI		; compare against max width
	bcc	TBLSPX		; all done then
	bne	TBLSP2		; no need to check if <>
	lda	XSIZE+LO	; check LSB
	cmp	K+LO		; well?
	bcc	TBLSPX		; all done then
TBLSP2:
	lda	#SPACE		; get it
	jsr	STASHB		; save it
	jsr	NEXTSPC		; point to next one
	inc	J		; count it
	bne	TBLSP		; and continue
TBLSPX:
	lda	#0		; show end of table
	sta	LENGTH+LO	; clear out line length too
	sta	LENGTH+HI	; and hi part too
	jsr	STASHB		; marked with a zero
	jsr	FP2SP		; have SPC point to beginning
	lda	J		; get how many chars are there	
	jsr	STASHB		; save at beginning of line
	inc	J		; count counter in offset
	lda	J		; get J back
	clc			; add in where we were
	adc	DIRITM+LO	; okay with lo part
	sta	DIRITM+LO	; save it
	lda	DIRITM+HI	; and now the hi part
	adc	#0		; pick up carry, maybe
	sta	DIRITM+HI	; and save it
	rts

	; -------------------
	; FLUSH OUTPUT BUFFER
	; -------------------
	; ENTRY: LENGTH OF BUFFER IN [X]

FLUSH:
	lda	#SPACE		; SPACE CHAR
	stx	OLDEND		; SAVE CURRENT END OF LINE
FL0:
	cmp	LBUFF,X		; FIND LAST SPACE CHAR
	beq	FL1		; IN THE LINE
	dex
	bne	FL0		; IF NONE FOUND,
	ldx	CHRCNT		; FLUSH ENTIRE LINE
	dex			; minus last one
FL1:
	stx	OLDLEN		; SAVE OLD LINE POS HERE
	stx	CHRCNT		; MAKE IT THE NEW LINE LENGTH
	
	lda	TABLEF		; are we doing table I/O?
	beq	FL11		; nope
	jsr	TBLRTN		; YES, DO IT 
	jmp	FL12		; so refill line
FL11:
	jsr	ZCRLF		; PRINT LINE UP TO LAST SPACE
;
; START NEW LINE WITH REMAINDER OF OLD
;
FL12:
	ldx	OLDLEN		; GET OLD LINE POS
	ldy	#0		; START NEW LINE AT BEGINNING
FL2:
	inx		
	cpx	OLDEND		; CONTINUE IF
	bcc	FL3		; INSIDE of end of line
	beq	FL3		; or at end of line
	sty	CHRCNT	
	rts		
FL3:
	lda	LBUFF,X		; GET CHAR FROM OLD LINE
        cmp     #SPACE          ; don't move start spaces in either
        beq     FL4             ; fine, I won't        
	sta	LBUFF,Y		; MOVE TO START OF NEW LINE
	bmi	FL4		; don't count hi light chars
	cmp	#SPACE		; don't count control chars
	bcc	FL4		; okay
	txa			; save current x
	pha			; saved
	lda	FONTFLG		; get which font we be using
	beq	FLSF1		; must be variable width
	lda	#MONOFONT_W	; get width then of mono font
	bne	FLSF2		; okay, now do add
FLSF1:
	lda	LBUFF,X		; get char back
	tax			; make index
	lda	CHWID,X		; get width of char
FLSF2:
	clc			; get ready for add
	adc	LENGTH+LO	; okay, now add char width
	sta	LENGTH+LO	; save new length
	pla			; get old X back
	tax			; okay
	bcc	FL4		; no wrap
	inc	LENGTH+HI
FL4:
	iny			; UPDATE # OF chars in NEW LINE
	bne	FL2		; (ALWAYS)


	; ---------------
	; CARRIAGE RETURN
	; ---------------

ZZCRLF:
	ldx	TABLEF		; OUTPUT TO TABLE?
	beq	ZCRLF		; NO
;
; if putting a <CR> into table, just add to line buffer and flush
;
	lda	#EOL		; get carriage return char
	ldy	CHRCNT		; add to lbuff
	sta	LBUFF,Y		; got it
	inc	CHRCNT		; now flush it
	jsr	TBLRTN		; YES, DO IT (TBL ONLY)
	jmp	ZCRLFX		; git out
ZCRLF:
	ldx	CHRCNT		; how far have we gone
	lda	#EOL		; get carriage return char
	sta	LBUFF,X		; END OF CURRENT LINE
	inc	CHRCNT		; UPDATE LINE LENGTH
	jsr	LINOUT		; DISPLAY LINE

;
; now check about line count
;
	lda	#0
	sta	CHRCNT		; AND RESET LINE COUNT
	sta	LENGTH+LO	; okay
	sta	LENGTH+HI	; okay

	jsr	CHKFNC		; CHECK FOR CR FUNCTION (XZIP)

	lda	SCREENF		; CHECK IF DISPLAYING TO SCREEN
	beq	ZCRLFX		; NO, GO HANDLE IF PRINTING
	lda	SCRLFLG		; is it a scrolling window?
	beq	ZCRLFX		; nope, so no more
	inc	LINCNT		; NEW LINE GOING OUT

	ldx	LINCNT		; IS IT TIME TO
	cpx	MAXLINES	; PRINT "MORE" YET?
	bcc	ZCRLFX		; NO, CONTINUE

	lda	#1		; leave one line from last screen, and input line
	sta	LINCNT		; RESET LINE COUNTER
	lda	LEFTMRG		; SET LEFT MARGIN
	sta	SCRCX
	lda	#1		; show cursor changed (maybe)
	sta	CURSFLG		; okay
;
; print [MORE] out myself, to avoid re-entrancy problems
;
        lda     SCRIPTF         ; make sure we don't script
        pha
        ldx     #0              ; start with first char
        stx     SCRIPTF         ; clear script flag
        stx     SCLLINES        ; use this as a counter
MORELOOP:
        lda     MORE,X          ; get char
        sta     IOCHAR          ; save it
        jsr     DIRECT_OUT      ; pump it out there
        inc     SCLLINES        ; count char
        ldx     SCLLINES        ; how many have we done        
        cpx     #MOREL          ; done?
        bne     MORELOOP        ; nope

        jsr     CLRBUF          ; make sure it shows
WAIT:
	jsr	GETKEY		; wait for key please
	lda	#MOREL-1	; how many backspaces
	sta	LENGTH		; just use this as counter
WM11:
	ldx	LENGTH		; which char?
	lda	MORE,X		; get char
	tax			; put here for erase
	lda	#BACKSPACE	; erase char
	jsr	CHAR
	dec	LENGTH		; count it
	bpl	WM11
        pla                     ; get script flag back
        sta     SCRIPTF         ; fine
	lda	LEFTMRG		; move back to left margin
	sta	SCRCX		; okay
	lda	#1		; show cursor changed (maybe)
	sta	CURSFLG		; okay
ZCRLFX:
	lda	#0
	sta	CHRCNT		; AND RESET LINE COUNT
	sta	LENGTH+LO	; okay
	sta	LENGTH+HI
	rts
LINOUT:
	ldy	CHRCNT		; IF BUFFER EMPTY,
	beq	LINEX		; DON'T PRINT ANYTHING
	sty	PRLEN		; SAVE LENGTH HERE FOR "PPRINT"
	lda	SCREENF		; DISPLAY TO SCREEN?
	beq	LOUT1		; NO, GO CHECK IF PRINT
	ldx	#0		; SEND CONTENTS OF [LBUFF]
LOUT:
	lda	LBUFF,X		; TO SCREEN
	jsr	CHAR
	inx
	dey
	bne	LOUT
	jsr	DISP_LINE	; make sure line gets printed
LOUT1:	JSR	PPRINT		; PRINT [LBUFF] IF ENABLED
LINEX:
	rts			; AND RETURN
;
; CHECK IF THERE IS A PENDING FUNCTION CALL ASSOCIATED WITH <CR>'S
;
CHKFNC:
	ldy	#WINCRC+HI		; get function offset
	lda	(WINDOW),Y		; IF NULL IGNORE
 	dey				; point to lo part
	ora	(WINDOW),Y
	beq	CHKOUT

        lda     CRLF_CHECK              ; do we really want to do this?
        beq     CHKOUT                  ; nope

	lda	(WINDOW),Y		; DECR COUNTER
	sec
	sbc	#1
	sta	(WINDOW),Y
	iny
	lda	(WINDOW),Y		; work on hi part
	sbc	#0			; pick up carry
	sta	(WINDOW),Y		; save it
	dey		
	ora	(WINDOW),Y		; if NULL NOW, CALL FCN
	bne	CHKOUT

	ldy	#WINCRF+LO		; pointing to routine
	lda	(WINDOW),Y		; get lo part	
	sta	J+LO			; and save in J
	iny				; point to hi part
	lda	(WINDOW),Y		; got it
	sta	J+HI
	jsr	INTCLL			; DO FUNCTION CALL
CHKOUT:
	rts


	; ----------------------
	; UPDATE THE STATUS LINE
	; ----------------------
	; NOT APPLICABLE IN EZIP.

ZUSL:	RTS		

; ------
; BUFOUT
; ------
; ENTER: ARG1 = BUFFERED (1) OR NONBUFFERED (0) OUTPUT CHOICE
; EXIT: FLAG (BUFFLG) IS SET TO TELL COUT WHICH TO DO
;
; shouldn't need this in YZIP ?!
;
ZBUFOUT:
;
; CLEAR OUTPUT BUFFER BEFORE DOING ANYTHING FANCY
;
CLRBUF:
	jsr	LINOUT		; CLEAR BUFFER (DON'T RESET LINE COUNT)
	ldx	#0
	stx	CHRCNT
	rts

	; ------
	; DIROUT
	; ------
	; ARG1 CONTAINS VALUE OF WHICH DEVICE TO SELECT
	; OR DESELECT, ARG2 = THE TABLE ADDR FOR TABLE OUTPUT
	; MULTIPLE DEVICE USAGE IS POSSIBLE.

SAVEXSIZE: db	0,0		; for formatted table saving

ZDIRT:
        jsr     CLRBUF          ; send out anything there now!

	ldx	ARG1+LO	
	bmi	DIRRES		; NEGATIVE VALUE, DESELECTING
	dex		
	beq	DIR1		; 1 = SET OUTPUT TO SCREEN
	dex		
	beq	DIR2		; 2 = SCRIPTING
	dex		
	beq	DIR3		; 3 = TABLE
	rts			; INVALID VALUE
DIRRES:
	inx		
	beq	DRES1		; -1 = RESET TO SCREEN
	inx		
	beq	DRES2	
	inx		
	beq	DRES3	
	rts			; INVALID VALUE, JUST LEAVE
DIR1:
	jsr	CLRBUF
	lda 	#1		; turn screen on
	sta	SCREENF	
	rts		
DRES1:
	jsr	CLRBUF
	lda	#0
	sta	SCREENF		; 0, TURN SCREEN OFF
	rts
DIR2:
	inx		
	stx	SCRIPTF		; SET SCRIPT FLAG ON
	lda	ZBEGIN+ZFLAGS+1	; SET GAME FLAG ALSO
	ora	#%00000001	
	sta	ZBEGIN+ZFLAGS+1	
	lda	PSTAT		; CHECK IF PRINTER ALREADY INIT'D
	bne	DIR2A	
	jsr	PCHK		; NO, GO DO IT
DIR2A:
	rts			; YES, READY TO LEAVE
DRES2:
	stx	SCRIPTF		; TURN PRINTER OFF
	lda	ZBEGIN+ZFLAGS+1	; AND TURN OFF GAME FLAG TOO
	and	#%11111110	
	sta	ZBEGIN+ZFLAGS+1	
	rts		
DIR3:
        stx     SCREENF         ; turn off screen
	inx		
	stx	TABLEF		; TURN TABLE OUTPUT FLAG ON
	jmp	START_TABLE	; and set things up
DRES3:
	lda	TABLEF		; IF OFF ALREADY
	beq	OUT3		; LEAVE AS IS

	stx	TABLEF		; TURN TBL OUTPUT OFF
        inx                     ; turn screen back on
        stx     SCREENF         ; screen back on

        lda     #3              ; add three to make it wrap
        clc                     ; ready for add
        adc     ZBEGIN+ZTWIDTH+1 ; do it
        sta     ZBEGIN+ZTWIDTH+1
        bcc     DRESCC          ; no wrap into MSB
        inc     ZBEGIN+ZTWIDTH
DRESCC:
	lsr	ZBEGIN+ZTWIDTH+0 ; /4 to get # of pixels
	ror	ZBEGIN+ZTWIDTH+1
	lsr	ZBEGIN+ZTWIDTH+0
	ror	ZBEGIN+ZTWIDTH+1


	lda	FMTTBL		; did we do a formatted table?
	bne	DRESFT		; yes, so fix things
;
; now put count into table
;
	lda	DIRTBL+LO	; and put count into second byte of table
	sta	SPCL		; points to low par
	lda	DIRTBL+HI	; get page 
	jsr	SETPC		; okay, page and bank
	sta	SPCH
	sty	SPCBNK
	lda	DIRITM+LO	; (2 LESS THAN [DIRITM])
	sec		
	sbc	#2	
	sta	DIRITM+LO	; save this new count
	bcs	RESET0	
	dec	DIRITM+HI	; wrapped downwards
RESET0:
	lda	DIRITM+HI	; get hi part
	jsr	STASHB		; saved that count
	jsr	NEXTSPC		; and point to lo part
	lda	DIRITM+LO	; and get lo part
	jsr	STASHB		; and save it
	rts			; done
DRESFT:
	lda	CHRCNT		; anything in buffer?
	beq	DRES31		; nope
	jsr	TBLRTN		; then fill out last line
DRES31:
	lda	SAVEXSIZE+LO	; get old xsize back
	sta	XSIZE+LO	; restored
	lda	SAVEXSIZE+HI
	sta	XSIZE+HI
	lda	#0		; reset this flag too
	sta	FMTTBL		; cleared
OUT3:
	rts		
;
; set things up for doing table output
;
START_TABLE:

	lda	ARG2+HI		; SET UP TBL
	sta	DIRTBL+HI	
	lda	ARG2+LO		; TO STORE CHARS IN
	sta	DIRTBL+LO	
	lda	#2
	sta	DIRITM+LO	
	lda	#0	
	sta	DIRITM+HI	
	sta	ZBEGIN+ZTWIDTH+1	; clear width count
	sta	ZBEGIN+ZTWIDTH+0
;
; check to see if we have formatted table
;
	lda	NARGS		; get # of arguments
	cmp	#3		; if == 3, then we wrap it
	beq	DIR30		; nope
	rts			; all done other wise
DIR30:
	stx	FMTTBL		; set formatted table flag
	lda	XSIZE+LO	; save the current XSIZE
	sta	SAVEXSIZE+LO	; okay, did it
	lda	XSIZE+HI	; and MSB
	sta	SAVEXSIZE+HI	; okay
	lda	ARG3+LO		; this is the actual amount
	ldx	ARG3+HI		; get interesting arg
	bmi	DIRFT1		; check for negative width
	jsr	SETWJ		; get the window offset
	ldy	#WINXSZ		; get its XSIZE
	lda	(J),Y		; got it
	sta	XSIZE+LO	; aren't we nice
	bne	DIR31		; jump to end
DIRFT1:
	eor	#$FF		; turn to a positive number
	sta	XSIZE+LO	; save it
	inc	XSIZE+LO	; to make it right
DIR31:
	lda	#0		; items starts at zero
	sta	DIRITM+LO	; okay
	sta	XSIZE+HI	; clear out highness
	asl	XSIZE+LO	; *2
	rol	XSIZE+HI	
	asl	XSIZE+LO	; *4
	rol	XSIZE+HI
	rts		

	; ------
	; CURSET
	; ------
	; SET CURSOR AT LINE (ARG1) AS OFFSET FROM TOP OF WINDOW
	; AND AT COLUMN (ARG2) in (optional) window ARG3

ZCURST:
	jsr	CLRBUF		; CLEAR OUT ANY NON DISPLAYED TEXT 1ST
        lda     ARG1+LO         ; see if < 0
        cmp     #$FF            ; -1 == turn off cursor
        bne     ZCURS00         ; nope
        lda     #1              ; show cursor off
        bne     ZCURSTXX        ; go do it and return
ZCURS00:
        cmp     #$FE            ; -2 == turn cursor on
        bne     ZCURS0          ; nope, just deal with normally
        lda     #0              ; show cursor not off
ZCURSTXX:
        sta     CURSOR_OFF      ; show current cursor state
        rts
ZCURS0:
	dec	ARG1+LO		; zero base both args
	dec	ARG2+LO		; thanx
	
	lda	NARGS		; check for id
	cmp	#3		; if 3 args, then window ID specified
	beq	ZCURS1		; not 3, so use current window

	lda	CURWIN		; show current window being done
	sta	ARG3+LO		; thanx
ZCURS1:
	lda	ARG3+LO		; get window ID
	jsr	SETWJ		; put address into J
	lda	ARG1+LO		; GET LINE
	ldy	#WINHGHT	; check against height
	cmp	(J),Y		; too far?
	bcc	ZCURS3		; no, so use it
	lda	(J),Y		; got max out
	tay			; but one too far
	dey			; all better now
	tya			; back to A
ZCURS3:
	clc			; get ready for add
	ldy	#WINTOP		; do y pos first
	adc	(J),Y		; add in top to get absolute value
	ldy	#WINY		; get y pos offset
	sta	(J),Y		; save here
	lda	ARG2+LO		; GET COLUMN
	ldy	#WINWID		; check against width
	cmp	(J),Y		; is it to for right (like Reagan?)
	bcc	ZCURS4		; nope, must be dukakis
	lda	(J),Y		; get max
	tay			; for going down one
	dey			; done it
	tya			; back to A for usefullness
ZCURS4:
	ldy	#WINLEFT	; get left edge to absolute it
	adc	(J),Y		; carry clear from above
	ldy	#WINX		; this is the x pos
	sta	(J),Y		; into the structure
;
; now check for current window again, move cursor anyway
;	
	jmp	GET_CURSOR	; get cursor pos from WINDOW

	; ------
	; CURGET
	; ------

ZCURGT:
	jsr	CLRBUF		; flush the buffer

	jsr	SAVE_CURSOR	; save the current position

	dec	NARGS		; see how many arguments
	beq	ZCURG1		; use main window
	lda	ARG2+LO		; get window id
	bpl	ZCURG2		; and figger it out
ZCURG1:
	lda	CURWIN		; use current window
ZCURG2:
	jsr	SETWJ		; make J point to right place

	lda	ARG1+HI		; get table address
	jsr	SETPC		; get real address in memory
	sty	SPCBNK		; set bank
	sta	SPCH		; and page
	lda	ARG1+LO		; get table offset
	sta	SPCL		; and save it

	lda	#0		; zero hi part of Y pos
	jsr	STASHB		; and save it
	jsr	NEXTSPC		; and point to next one
	jsr	FETCHCY		; Fetch Y pos
	jsr	STASHB		; and save it
	jsr	NEXTSPC
;
; now for the X pos
	lda	#0		; zero top byte of x pos
	jsr	STASHB
	jsr	NEXTSPC
	jsr	FETCHCX		; and now the X pos, and return
	jmp	STASHB		; and stash it away

;
; FETCHCY - get the relativized Y pos into [A]
;	J points to window structure
FETCHCY:	
	ldy	#WINY		; get Y pos of window's cursor
	lda	(J),Y		; got it
	ldy	#WINTOP		; need to de-absolute it
	sec			; get ready for sub
	sbc	(J),Y		; get relative pos
	tax			; for 1 basing it
	inx			; 1 align it
	txa			; and put for save
	rts
;
; FETCHCX - get relativized X pos into [A]
;	J points to window structure
FETCHCX:
	ldy	#WINX		; get X pos of cursor
	lda	(J),Y		; got it
	ldy	#WINLEFT	; relativize it
	sec			; get ready for sub
	sbc	(J),Y		; subtract left edge
	tax			; for incing
	inx			; 1 ALIGN IT
	txa
	rts

	; -----
	; DIRIN
	; -----
	; NOT YET IMPLEMENTED, BUT RESERVED

ZDIRIN:	RTS		

;
; SETWJ - set up J to point to window structure for window ID in [A]
;
SETWJ:
	tay			; check for -3 (current window)
	bpl	SWJ1		; nope, window ID here
	lda	CURWIN		; -3 means current window
SWJ1:
	asl	A		; shift to make word index
	tay			; and now make index
	lda	WINTABLE,Y	; get lo part of window struct address
	sta	J+LO		; and save it
	lda	WINTABLE+1,Y	; here's the hi part
	sta	J+HI		; prove it
	rts

	; -----
	; ERASE
	; -----

ZERASE:
	lda	ARG1+LO	
	cmp	#1	
	bne	ZEROUT		; not clreol

	jsr	CLRBUF
	jmp	CLREOL		; CLEAR TO END OF LINE
ZEROUT:
        sta     CLSWIDTH        ; this many pixels wide
        lda     #FONT_H         ; pixels of font_height
        sta     CLSHEIGHT
        lda     SCRCX           ; start at current cursor pos
        sta     CLSLEFT
        lda     SCRCY
        sta     CLSTOP
        jmp     CLS             ; and do it

; -----
; CLEAR the current window
; -----
SAVEXY:	dw	0

ZCLR:
	jsr	CLRBUF		; flush the current buffer
	jsr	SAVE_CURSOR	; save the cursor pos

	lda	ARG1+LO		; CHECK WHAT TO DO
	bpl	CLRW		; a particular window
 	cmp	#$FD		; -3 means current window	
	beq	CLRW		; so just handle it regular
	cmp	#$FE		; clear with nothing social
	beq	CLRCLS		; just clear the screen
;
; UNSPLIT SCREEN & CLEAR IT
;
	lda	#0		; just use SPLIT 0
	sta	ARG1+LO		; as if called normally
	jsr	ZSPLIT		; do the split
;
; just clear the entire screen
;
CLRCLS:
	lda	#0		; clear the entire screen
	sta	CLSLEFT		; from top left
	sta	CLSTOP
	lda	#MAXWIDTH	; to bottom right
	sta	CLSWIDTH
	lda	#MAXHEIGHT
	sta	CLSHEIGHT

	jsr	CLS		; do the clear screen (no cursor movement)
	lda 	ARG1+LO		; check for -2
	bmi	ZCLRX 		; it is, don't move cursor
	bpl	ZCLEARX		; move cursor
CLRW:
	jsr	SETWJ		; get me the window pointer
	ldy	#WINTOP		; step thro to get data
	lda	(J),Y		; TOP
	sta	CLSTOP		; save for clear
	iny			; point to left
	lda	(J),Y		; get it
	sta	CLSLEFT		; tell CLS
	iny			; now at WINHGHT
	lda	(J),Y		; got it
	sta	CLSHEIGHT	; save for clear
	iny			; now at WINWID
	lda	(J),Y		; get height
	sta	CLSWIDTH	; saved
	jsr	CLS		; screen cleared
;
; now move the cursor to 1,1, if not == -2
;
ZCLEARX:
	lda	ARG1+LO		; check arg
	jsr	SETWJ		; make sure J still points to window
	ldy	#WINTOP		; put at top left of window
	lda	(J),Y		; get top
	ldy	#WINY		; and make it the y pos
	sta	(J),Y		; of the cursor
	ldy	#WINLEFT	; and the left is
	lda 	(J),Y		; the
	clc			; add in the left margin
	ldy	#WINLM		; here's the left margin
	adc	(J),Y		; added it
	ldy	#WINX		; x pos of the
	sta	(J),Y		; cursor
	lda	#0		; and clear out line count
	ldy	#WINLCNT	; line count
	sta	(J),Y		; okay
	ldy	#WINLLEN	; and length of current line
	sta	(J),Y		; okay
	iny			; do hi
	sta	(J),Y		; okay
ZCLRX:
	jmp	GET_CURSOR	; restore the cursor pos for the current window

; ------
; PRINTT
; ------
; PRINT A TABLE TO SCREEN, ARG1 = # OF BYTES
; ARG2 = WIDTH, ARG3 (DEF = 1) = HEIGHT

OLDCHZ	EQU	I+LO		; EASIER TO READ
OLDEHZ	EQU	I+HI
OLDCVT	EQU	L+LO

TBL_LEN DW	0		; save for length

ZPRNTT:
;
; make sure the buffer is empty, thanks
;
	jsr	CLRBUF

	lda	ARG1+LO		; USE GETBYT AS TBL COULD
	sta	MPCL		; BE ANYWHERE
	lda	ARG1+HI
	sta	MPCM
	lda	#0
	sta	MPCH
	jsr	VLDMPC

	lda	ARG2+LO		; ONLY A BYTE AS MAX
	beq	PTTDUN		; QUIT NOW IF NULL

	sta	TBLWIDTH	; width of the table
	sta	TBLCNT		; start counter off at width
	dec	NARGS		; count down to 1 (maybe)
	lda	NARGS
	cmp	#1
	beq	NOHIGHT		; DEFAULT HEIGHT IS 1
	lda	ARG3+LO		; get passed height
NOHIGHT:
	sta	TBLHEIGHT	; height of the table (at least 1)
	lda	SCRCX
	sta	OLDEHZ
	sec			; subtract left margin to get how long line is
	sbc	LEFTMRG		; okay we did that
	sta	TBL_LEN+LO	; this is how big line is too
	lda	#0		; clear MSB
	asl	TBL_LEN+LO	; *2
	rol	A
	asl	TBL_LEN+LO	; *4
	rol	A
	sta	TBL_LEN+HI	; save
PTTLP:
	jsr	GETBYT		; GET A BYTE
	jsr	COUT		; and send it out

	dec	TBLCNT		; one more byte done
	bne	PTTLP
	dec	TBLHEIGHT	; IF DONE ALL LINES
	beq	PTTDUN		; LEAVE

	jsr	CLRBUF		; so send out stuff that's there

	lda	OLDEHZ		; get old x pos
	sta	SCRCX		; and restore it
	lda	#1		; show cursor changed
	sta	CURSFLG		; okay
	lda	SCRCY		; point to next line
	clc			; by adding in the font height
	adc	#FONT_H		; okay, goody
	sta	SCRCY		; and here we are

	lda	TBL_LEN+HI	; reset length
	sta	LENGTH+HI	; hi part
	lda	TBL_LEN+LO	; lo part
	sta	LENGTH+LO	; just a lo

	lda	TBLWIDTH	; RESET COUNT
	sta	TBLCNT		; thanx
	bne	PTTLP		; GO DO NEXT LINE
PTTDUN:
	jsr	CLRBUF		; send out last bit
	rts
;--------------
; ZPRINTF
;--------------
; ZPRINTF - print a formatted table
;
ZPRINTF:
	lda	ARG1+LO		; USE GETBYT AS TBL COULD
	sta	MPCL		; BE ANYWHERE
	lda	ARG1+HI
	sta	MPCM
	lda	#0
	sta	MPCH
	jsr	VLDMPC

	lda	SCRCX		; save the X pos
	sta	OLDEHZ		; saved here
ZPFL1:
	jsr	GETBYT		; get the length byte
	beq	ZPFX		; no more if == 0
	sta	TBLWIDTH	; keep track of it
ZPFL2:
	jsr	GETBYT		; now get the char byte
	jsr	CHAR		; and send it yt
	dec	TBLWIDTH	; any more?
	bne	ZPFL2		; ayyup

 	jsr	DISP_LINE	; make sure line goes out
	lda	OLDEHZ		; reset a few things
	sta	SCRCX		; including x pos
	lda	#1		; show cursor changed
	sta	CURSFLG		; okay
	lda	SCRCY		; point to next line
	clc			; by adding in the font height
	adc	#FONT_H		; okay, goody
	sta	SCRCY		; and here we are
	bne	ZPFL1		; and do it again
ZPFX:
	rts
; ------------
; SET NEW FONT
; ------------

; Font 4 is mono-spaced font.

ZFONT:
        jsr     CLRBUF          ; clear the buffer first

	dec	NARGS		; did we get passed a window id?
	bne	ZFNTWID		; yup
	lda	#$FD		; -3 means current window
	sta	ARG2+LO		; make it the arg2 then
ZFNTWID:
	lda	ARG2+LO		; get window we are interested in
	jsr	SETWJ		; make [J] point to window struct

	lda	ARG1+LO		; check for 
	cmp	#1		; font 1
	beq	ZFNT1		;  or
	cmp	#4		; font 4 (monospace)
	bne	ZFBAD		; nope, so die
;
; font 4 is the monospaced font
;
	ldy	#WINFSIZE	; point to width
	lda	#MFONT_W	; get game width
	sta 	(J),Y		; store the width
	lda	#3		; set font ID
	bne	ZFNTEX		; all done
ZFNT1:
	ldy	#WINFSIZE	; point to width
	lda	#FONT_W		; get game width
	sta 	(J),Y		; store the width
	lda	#0		; set font ID
ZFNTEX:
	ldy	#WINFONT	; set font
	pha			; save font id
	lda	(J),Y		; get old font id
	tax			; save old font id
	pla			; get new one back
	sta	(J),Y		; save in structure
	lda	(WINDOW),Y	; get current window font
	beq	ZFNT0X		; set flag with zero	
	lda	#MONOFONT_W	; get width of mono spaced font
ZFNT0X:
	sta	FONTFLG		; and set flag too
	inx			; make it say correct font ID for game
	txa			; put in A for put val
	ldx	#0		; clear X
	jmp	PUTVAL		; and return it
ZFBAD:
	jmp	RET0		; TELL IT DOESN'T WORK
; ------------------------------
; FETCH A LINE OF INPUT FOR READ
; ------------------------------
; ENTRY: Relative READ table address in RDTBL1
; EXIT: # CHARS READ IN [A]
SV_SPC:	ds	3		; save SPC here for later usage

INPUT:
	jsr	CLRBUF		; FLUSH [LBUFF]
	ldy	#0		; RESET LINE COUNT
	sty	I+HI		; clear local variables
	sty	I+LO	
	sty	J+HI	
	sty	J+LO	
	sty	BRKCHR		; init break char
	sty	LINCNT		; RESET LINE COUNT
        sty     PTR_COUNT       ; we start by looking at pointer        
        sty     CURSFLG         ; make sure we don't change cursor
;
; check for new TCHARS table
;
	lda	ZBEGIN+ZTCHAR+1	; so check lower byte for different tchars
	cmp	SVTCHAR+LO	; is it different?
	bne	IO_DOT		; ayyup, get new one
	lda	ZBEGIN+ZTCHAR	; get current TCHARS hi part
	cmp	SVTCHAR+HI	; is it the same as saved one?

	beq	IOj		; yes, so don't muck with it
IO_DOT:
	jsr	DO_TCHARS	; new table, so copy it over
;
; now set up FPC
;
IOj:
	lda	RDTBL1+HI	; get page of table
	jsr	SETPC		; and get me the memory page and bank
	sta	FPCH		; save page
	sty	FPCBNK		; and bank
	lda	RDTBL1+LO	; and get offset
	sta	FPCL		; set up FPC/SPC
	jsr	FETCHB		; get length of line
	tay			; for dec'ing
	dey			; don't count the offset byte
	dey			; and leave room for terminator
	sty	CHRMAX		; and save it
	jsr	NEXTFPC		; point to next byte
	jsr	FETCHB		; get current offset into buffer
	sta	CHARCNT		; save how many chars out there
	jsr	NEXTFPC 	; point to beginning of data buffer
;
; now finish setting up saved SPC
;
	ldx	#2
INSVCL:
	lda	FPC,X		; get byte to saved for use later on
	sta	SV_SPC,X	; it is saved here
	dex
	bpl	INSVCL		; next!
;
; now copy current buffer into local buffer
;
	lda	CHARCNT		; make sure we have some
	beq	CPIOLX		; nope, none in there now
	ldx	#0		; start at first char
CPIOL:
	jsr	FETCHB		; get a char
	sta	LBUFF,X		; save char
	jsr	NEXTFPC		; point to next one
	inx			; next char
	cpx	CHARCNT		; got them all yet?
	bne	CPIOL		; nope
CPIOLX:
	jsr	CHKTME		; START  TIME LIMIT
;
; this is the get-a-key-and-whack-on-it loop
;
INLOOP:
	lda	I+HI		; is there a time?
	beq	INPL1		; nope
	jsr	TIMIN		; do timed input
	bcc	INPL2		; got a char, process it
	jmp	LEXBAD		; timed out with nothing there!
INPL1:
	jsr	GETKEY		; let apple do the walking
INPL2:
	jsr	ISTCHR		; CHECK IF IT'S AN ACCEPTABLE TERMINATOR KEY
	bcs	NOTYET

	sta	BRKCHR
	cmp	#EOL		; IF EOL PUT TO SCREEN
	beq	ENDLINx
	jmp	ENDIN		; ELSE JUST END
ENDLINx:
        jmp     ENDLIN          ; can't reach with a branch
NOTYET:
        ldy     #$FF            ; restart pointer device counter
        sty     PTR_COUNT       ; and start again
	tay			; check for functions keys that aren't
	bmi	CBAD		; terminators and feep if it is
	cmp	#EOL		; EOL?
	beq	ENDLIN		; LINE DONE IF SO
	cmp	ESCAPE		; don't allow escape char's thru
	beq	CBAD		; okay, I won't

	cmp	#BACKSPACE	; BACKSPACE?
	beq	BACKUP		; SPECIAL HANDLING
        cmp     #ESCAPE         ; ESCAPE char?
        beq     CBAD            ; don't like it

	ldy	CHARCNT		; where do we put char?
	cpy	CHRMAX		; are we filled up?
	bcs	CBAD		; boy, am i full

	pha			; save it
	tax			; use as index
	lda	LENGTH+LO	; GET LINE LENGTH COUNTER
	clc 			; get ready for add
	adc	CHWID,X		; add width
        tay                     ; save LSB in Y
        ldx     LENGTH+HI       ; get high byte        
	bcc	INP0C		; no wrap
	inx             	; okay, wrap then
INP0C:
	pla			; get char back
	cpx	XSIZE+HI	; check MSB first
	bcc	NOIOWRAP	; no need to check lsb
	cpy	XSIZE+LO	; end of screen line?
	bcs     CBAD            ; reached end, so just beep
NOIOWRAP:
        stx     LENGTH+HI       ; save MSB
        sty     LENGTH+LO       ; and LSB
	ldx	CHARCNT		; for putting in line buffer
	sta	LBUFF,X		; and save it in case we goto printer
	jsr	CHAR		; SEND TO SCREEN
	jsr	DISP_LINE	; show char
	inc	CHARCNT		; NEXT POSITION IN LINE
	jmp	INLOOP		; NO, GET ANOTHER CHAR
;
; HANDLE BACKSPACE
;
BACKUP:
	ldx	CHARCNT		; if == 2 then empty
	beq	CBAD		; JMP to beeping
	dex			; get rid of char
	stx	CHARCNT		; saved
	lda	LBUFF,X		; get char we are interested in
	tax			; put in x

	lda	LENGTH+LO	; GET LINE LENGTH COUNTER
	sec 			; get ready for sub
	sbc	CHWID,X		; sub width
	sta	LENGTH+LO	; update length
	bcs	BCKP1		; no wrap
	dec	LENGTH+HI	; okay, wrap then
BCKP1:
	lda	#BACKSPACE	; so do erasing backspace
	jsr	CHAR
	jmp	INLOOP
CBAD:
	jsr	BEEP		; ELSE SCREAM WITH PAIN
	jmp	INLOOP		; AND WAIT FOR SOMETHING BETTER
;
; HANDLE END OF LINE KEY
;
ENDLIN:
	sta	BRKCHR
	lda	#EOL		; get EOL char
	jsr	CHAR		; SEND EOL TO SCREEN
	lda	#0		; and show 0 length of line out there
	sta	LENGTH+LO	; thanx
	sta	LENGTH+HI	; and msb
	inc	LINCNT		; take into account the <CR> at EOL
ENDIN:
;
; now save lbuff in table for posterity
;
	lda	SV_SPC+ABANK	; get bank first
	sta	SPCBNK		; saved
	lda	SV_SPC+HI
	sta	SPCH
	lda	SV_SPC+LO
	sta	SPCL
	ldx	#0		; start with first char in buffer
ENDLOOP:
	cpx	CHARCNT		; done yet?
	beq	INP111
	lda	LBUFF,X		; get char
	cmp	#'A'		; IF CHAR IS UPPERCASE ALPHA,
	bcc	LEX		; CONVERT TO LOWER CASE
	cmp	#'Z'+1	
	bcs	LEX
	adc	#$20		; converting away
LEX:
	jsr	STASHB		; okay, stashing away
	jsr	NEXTSPC		; and point to next char in table
	inx			; point to next char
	bne	ENDLOOP		; so get next one
INP111:
	lda	#00		; to show end of line
	jsr	STASHB		; so put it in buffer
;
; now put in new line length
;
	lda	RDTBL1+HI	; get page of table
	jsr	SETPC		; and get me the memory page and bank
	sta	SPCH		; save page
	sty	SPCBNK		; and bank
	lda	RDTBL1+LO	; and get offset
	sta	SPCL
	jsr	NEXTSPC		; and point to char count
	lda	CHARCNT		; get character count
	jsr	STASHB		; and shove it away  
LEXBAD:
        ldy     #0              ; clear out
        sty     PTR_COUNT       ; pointer flag
	rts			; Length is in [A]
;
; Copy over a new TCHARS table
;
DO_TCHARS:
;
; now do somethin' with the TCHAR table (maybe, if <> 0)
;
	lda	ZBEGIN+ZTCHAR	; DO SAME FOR TCHARS TABLE
	sta	SVTCHAR+HI	; save it for checking
	jsr	SETPC		; and now make absolute
	sta	FPCH		; Save in FPC
	sty	FPCBNK
	lda	ZBEGIN+ZTCHAR+1	; NO CHANGE FOR LSB
	sta	SVTCHAR+LO	; and save for later check
	sta	FPCL		; now move pointer to fetch spot

	lda	#0		; and set index
	sta	I		; thank you
	sta	ALLFLG		; turn it off
TCHLP:
	jsr	FETCHB		; get the byte in [a]
	jsr	NEXTFPC		; point to next one
	ldx	I		; get offset
	inc	I		; and point to next one
	sta	TCHARTBL,X	; save in lower memory, thank you
	cmp	#0		; are we done yet?
	beq	TCHj		; NULL TERMINATED STRING
	cmp	#$FF		; $ff means all >128 chars are terminators
	bne	TCHLNEXT	; nope
	lda	#1		; yes,
	sta	ALLFLG		; so set flag to say so
TCHLNEXT:
	bne	TCHLP		; and go get it
TCHj:
	rts

	; ------------------------
	; IS IT A TERMINATOR CHAR?
	; ------------------------

ISTCHR:
	ldx	TCHARTBL	; check for chars in TCHAR table
	beq	ISNOT		; nope

	ldx	ALLFLG		; ARE ALL FCN KEYS (<127) TERMINATORS
	beq	ISCHK		; 0 = NO, GO CHECK LIST
        tax                     ; check for minus
	bmi	ISFND		; YUP
	bpl	ISNOT
ISCHK:
	ldy	#0
ISLP:
        cmp	TCHARTBL,Y	; IS CHAR WE HAVE A TCHAR?
	beq	ISFND		; YES
	ldx	TCHARTBL,Y	; NULL = END OF STRING
	beq	ISNOT
	iny
	bne	ISLP		; SHOULD ALWAYS LOOP
ISNOT:
	sec
	rts			; NOT FOUND
ISFND:	
	clc
	rts

	; -----
	; INPUT 1 char, no printing
	; -----

ZINPUT:
	JSR	CLRBUF

	ldy	#0	
	sty	LINCNT	
	sty	CHRCNT	
	sty	I+HI		; init locals too
	sty	I+LO
	sty	J+HI
	sty	J+LO
        sty     PTR_COUNT       ; always check the pointer
 
	DEC	NARGS	
	BEQ	ZINP3		; NO TIME LIMIT
	LDA	ARG2+LO		; GET DELAY WANTED
	STA	I+HI	
	DEC	NARGS	
	BEQ	ZINP4		; NO FCN
	LDA	ARG3+LO	
	STA	J+LO	
	LDA	ARG3+HI	
	STA	J+HI	
ZINP4:
	JSR	TIMIN		; CALL timed input rotine
	BCC	ZINPRK		; send char on home
	JMP	RET0		; time out without character
ZINP3:
	jsr	GETKEY		; ok, find which char was pressed
        cmp     #ESCAPE         ; don't use escape char
        bne     ZINPRK          ; fine
        jsr     BEEP            ; complain
        jmp     ZINP3           ; do again
ZINPRK:
	LDX	#0
	JMP	PUTBYT		; RETURN CHAR


INTCLL:
	LDA	I+HI		; SAVE VALUES FOR CALLING RTN
	PHA		
	LDA	I+LO
	PHA
	LDA	J+HI	
	STA	ARG1+HI		; pretend it's arg1
	PHA		
	LDA	J+LO	
	STA	ARG1+LO		; and this is the other half
	PHA		

	LDX	#1
	STX	NARGS		; 0 args for internal call
	DEX
	STX	IRET		; make sure it returns here!
	
	LDA	ZPCL		; a fake one to say we
	PHA			; are an internal call
	LDA	ZPCM		; so save real one
	PHA
	LDA	ZPCH	
	PHA

	LDA	#0		; return addr of zero is 
	STA	ZPCH		; internal call!
	STA	ZPCM
	STA	ZPCL

	JSR	DOCALL

	JMP	MLOOP		; GO DO FCN
;
; RETURN FROM FCN WILL COME HERE
;
ZIRET:
 	PLA			; GET RID OF RTS FROM ZRET
	PLA		

	PLA			; get old zpc back
	STA	ZPCH
	PLA
	STA	ZPCM
	PLA
	STA	ZPCL
	JSR	VLDZPC		; and validate it

	PLA			; RESTORE FOR CALLING RTN
	STA	J+LO	
	PLA		
	STA	J+HI	
	PLA
	STA	I+LO
	PLA		
	STA	I+HI	
	RTS			; GO BACK TO CALLER

; INPUT: routine locals!
CHARCNT:	DB	0	; where into buffer goes char
SVCHAR:		DB	0	; where to save the char before printing

IRET:	DB	0		; FLAG TELLS IF RETURNLESS CALL

MTEMP:	DB	00,00		; temp spot for math routines
TYPE:	DB	0		; PARTIAL OR NORMAL (WHOLE) SAVE/RESTORE
ASSVLU:	DB	0		; how many args to this subroutine
BRKCHR:	DB	0		; READ BREAK CHAR
RDFLAG:	DB	0		; 0 - only read 1 - do lex on it
MORE:	DB	"[MORE]"	
MOREL	EQU	$-MORE
	END


