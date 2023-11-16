	TITLE	"Apple ][ YZIP (c)Infocom","--- ZIP SAVE AND RESTORE ROUTINES ---"
; -----------------------------
; SET UP SAVE & RESTORE SCREENS
; -----------------------------
SAVRES:
	jsr	ZCRLF	; CLEAR THE LINE BUFFER
	lda	#0	
	sta	SCRIPT	; DISABLE SCRIPTING
	rts

; -----------------------------
; SAVE & RESTORE STRINGS
; -----------------------------
YES:	DB	"YES"
	DB	EOL	
YESL	EQU	$-YES	
NO:	DB	"NO"	
	DB	EOL	
NOL	EQU	$-NO	

NAMEQ:	db	EOL
	db	"Insert save disk and enter "
	db	"full pathame of save file: "
	db	EOL
	db	"Hit '?' key to get a list of online volumes."
	db	EOL
	db	"Current pathname is:"
	db	EOL
NAMEQL	EQU	$-NAMEQ
SNDATA:		 	; show start of name and length
SNAMEL:	db	0	; place to save length of name
SAVENAME: ds	64	; save plenty of room for max name

DELQ:	db	EOL,"File exists, delete it (Yes/No)? "
DELQL	EQU	$-DELQ+1	; include this following EOL
RETQ:	db	EOL,"Please hit [RETURN]",EOL
RETQL	EQU	$-RETQ
; -----------------------------
; SAVE/RESTORE Parameter Blocks
; -----------------------------
CREATE_PB:
	db	7		; 7 parameters
	dw	SNDATA		; pointer to name
	db	$C3		; full access to file
	db	$06		; BIN file type
	dw	0		; no aux data
	db	$01		; standard file
	dw	0		; create date
	dw	0
	     ; creation time
SETEOF_PB:
	db 	2		; 1 parameter
        db      0               ; refnum
        db      0,0,0           ; set to zero spot to clear it out
OPEN_SV:
	db	3		; 3 parameters
	dw	SNDATA		; name
	dw	GAME1FIO	; file buffer
	db	0		; ref num
CLOSE_PB:
	db	1		; only one parm
	db	0		; the refnum
WRITE_SV:
	db	4		; parm count
	db	0		; refnum
	dw	IOBUFF		; data is always here
	dw	512             ; 1 page worth
	dw	0		; how many actually went

; get the save file name.  If user hits the ESC key, then abort the
; save by return with the carry set.
;
GET_SNAME:
	jsr	CLOSE_GAME	; close the game files
	jsr	SWAP2INFOW	; goto information window
GTSN0:
	DLINE	NAMEQ,
	lda	SNAMEL		; is there a name yet?
	beq	GTSN00		; nope
	DLINE	SAVENAME,SNAMEL	; show current name of file
GTSN00:
	lda	#0		; clear line count	
	sta	CLOSE_PB+CL_REFNUM ; clear this too
	ldx	SNAMEL		; get length
	stx	CHRCNT		; okay
	ldy	SNAMEL		; point to copy
	dey			; one less
GCOPY:
	lda	SNAMEL,X	; get char
	sta	LBUFF,Y		; save it
	dex			; point to previous one
	dey			; previous pointer
	bpl	GCOPY		; copy until length byte
GNAME:
	jsr	GETKEY		; WAIT FOR A KEY
	cmp	#EOL		; IF [RETURN],
	beq	GOTNAME		; got the name
	cmp	#ESCAPE		; hit escape key?
	sec			; just in case it does exit
	beq	GNX		; so then die
	cmp	#BACKSPACE	; erasing things?
	bne	GNM1		; nope

	ldx	CHRCNT		; make sure there are chars there
	bne	GNMBP		; ayyup, do delete
GNMBAD:
	jsr	BEEP		; no room for delete
	jmp	GNAME		; okay
GNMBP:
	dex			; point down one
	stx	CHRCNT		; count one down
	lda	LBUFF,X		; get char to delete
	tax			; show in [X]
	lda	#BACKSPACE	; and doing a backspace 
	bne	GNMSHOW		; okay, delete char on screen
GNM1:
	cmp	#'/'		; slash is the only good non-numeric char
	beq	GNMGOOD		; fine, use it
	cmp	#'.'		; well, maybe a . too
	beq	GNMGOOD 	; fine, here it is
	cmp	#VOLCHAR	; does user want list of volumes?
	bne	GNM1x		; nope

	lda	#0		; clear out current name
	sta	CHRCNT		; okay, we did
	jsr	LISTVOLS	; show them
	jmp	GTSN0		; start over, kind of
GNM1x:
	cmp	#'0'		; is it a number
	bcc	GNMBAD		; nope
	cmp	#'9'+1		; well?
	bcc	GNMGOOD		; yup
	cmp	#'z'+1		; make sure it is alpha numeric
	bcs	GNMBAD		; nope
	cmp	#'A'		; well?
	bcc	GNMBAD		; nope
	cmp	#'a'		; little char?
	bcs	GNMGOOD		; yup
	cmp	#'Z'+1		; big char
	bcs	GNMBAD		; nope
GNMGOOD:
	ldx	CHRCNT		; get name index
	inc	CHRCNT		; point to next char
	sta	LBUFF,X		; save name char
GNMSHOW:
	jsr	CHAR		; show character
	jsr	DISP_LINE	; make sure it is there
	jmp	GNAME		; go get next char
;
; got the name, so copy it to the SAVENAME buffer
;
GOTNAME:
	ldx	CHRCNT		; get how many characters
	stx	SNAMEL		; save in length byte
	dex			; points one too far
GNL:
	lda	LBUFF,X		; get the char
	sta	SAVENAME,X	; save the char
	dex			; point to previous one
	bpl	GNL		; and go get it
	clc			; show did just fine
GNX:
	php			; save status
	lda	#EOL		; print EOL
	jsr	CHAR		; okay
	lda	#0		; and clear CHRCNT
	sta	CHRCNT		; okay
	jsr	SWAPBACK	; change back to old window
	plp			; get status back
	rts			; all done
;
; open up a save file, by first trying to create it.  If it already exists
; then make sure the player wants to delete the file, then get rid of it.
; Finally open the file.  Return with carry set if user aborts the save.
; Store the ref number into the write parm block.
;
OPEN_SAVE:
	CREATE	CREATE_PB	; first try to create the file
	bcc	OPSV_OPEN	; created just fine, so open it
;
; can't create the file, check out why
;
	cmp	#$47		; this means file already there	
	beq	OPSV1		; nope, not that
	jmp	DISK_ERR	; show badness
OPSV1:
	DLINE	DELQ,
	jsr	GETYN		; get me the yes or no
	bcc	OPSV_OPEN	; so then delete it if yes
	rts			; nope, so just quit
OPSV_OPEN:
	OPEN	OPEN_SV		; open the save file
	bcc	OPSV_OP1	; okey, things worked just fine
	jmp	DISK_ERR		; complain about error
OPSV_OP1:
	lda	OPEN_SV+OP_REFNUM	; get the ref number
	sta	WRITE_SV+WR_REFNUM	; save the ref number
	sta	CLOSE_PB+CL_REFNUM	; to close parm too
        sta     SETEOF_PB+SE_REFNUM     ; for cleansing file
        SET_EOF SETEOF_PB       ; clear out file
        bcc     OPSVEX          ; no problems
        jsr     DISK_ERR        ; complain
OPSVEX:
	rts			; file has been opened, return
;
; OPEN_RES - open the save file
;
OPEN_RES:
	OPEN	OPEN_SV		; open it up
	bcc	OPR1		; okay, it worked
	rts			; okay, it didn't
OPR1:
	lda	OPEN_SV+OP_REFNUM ; get reference number
	sta	READ_PB+RD_REFNUM ; save for read
	sta	CLOSE_PB+CL_REFNUM ; and for close
	rts
;
; CLOSE_SAVE: close up the save file if it is open
;
CLOSE_SAVE:
	lda	CLOSE_PB+CL_REFNUM ; check if it opened
	beq	CLSVX		; okay, nothing
	CLOSE	CLOSE_PB	; close the save file
CLSVX:
	lda	#1		; open up GAME2 file, just for kicks
	sta	SAVEDISK	; show we have a save disk in there
	jsr	FETCH_FILE	; this does it
	lda	D2SEG+HI	; set DSEGS to point to #2
	sta	DSEGS+HI
	lda	D2SEG+LO
	sta	DSEGS+LO
	ldx	GAME2NML	; get length of current name
	lda	GAME2NM,X	; get the number of the file
	sta	SAVENUM 	; we need this to look for prefix
	jsr	OPEN_GAME2	; open up GAME2 file
	inc	SCRIPT		; allow scripting again
	lda	#0		; open up GAME2 file, just for kicks
	sta	SAVEDISK	; show we have a save disk in there
	sta	CLOSE_PB+CL_REFNUM	; clear close
	rts			; DONE
;
; CLOSE_GAME - close the current game file(s)
;	and set DSEGS to point to preload so it will reopen them
;
CLOSE_GAME:
	lda	#0			; show no files are open
	sta	CLOSE_PB+CL_REFNUM	; 0 closes all files
	sta	GAME1REF		; zero out two game files too
	sta	GAME2REF		; and here is number 2
	CLOSE	CLOSE_PB		; now all are closed
	rts
;
; Get answer to Yes/No question.  Return with C==0 for yes, and C==1
; for a no.  RETURN == Yes, ESCAPE == NO
;
GETYN:
	jsr	GETKEY		; get the key strok
	cmp	#'y'		; IF REPLY IS "Y"
	beq	ALLSET		; ACCEPT RESPONSES
	cmp	#'Y'		; get both y's
	beq	ALLSET	
	cmp	#EOL		; EOL IS ALSO ACCEPTABLE
	beq	ALLSET	
	cmp	#'n'		; IF REPLY IS "N"
	beq	NOTSAT		; return with carry set
	cmp	#'N'		; check both n's
	beq	NOTSAT	
	cmp	#ESCAPE		; check for ESC key too
	beq	NOTSAT		; which means no
	jsr	BEEP		; ELSE BEEP
	jmp	GETYN		; INSIST ON Y OR N
NOTSAT:
	DLINE	NO,
	sec			; set the carry
	rts			; and show it
ALLSET:
	DLINE	YES,
	clc			; clear the carry
	rts
GETRET:
	DLINE	RETQ,
GETRETL:
	jsr	GETKEY		; get a key
	cmp	#EOL		; return key?
	bne	GETRETL		; nope
	jsr	CHAR		; show the LOW CRHIGH 
	rts
		
; ---------
; SAVE GAME
; ---------
ZSAVE:
	lda	#'N'
	ldx	NARGS
	beq	OLDSAV		; NORMAL, COMPLETE SAVE
	lda	#'P'
OLDSAV:
	sta	TYPE
	jsr	SAVRES		; set up screen
	jsr	GET_SNAME	; get the name of the save file
	bcs	ZSEXIT		; don't wanna after all
	jsr	OPEN_SAVE	; open the file
	bcs	ZSEXIT		; don't really care to
;
; SAVE GAME PARAMETERS IN [BUFSAV]
;		
	lda	ZBEGIN+ZID	; MOVE GAME ID
	sta	BUFSAV+0	; INTO 1ST 2 BYTES
	lda	ZBEGIN+ZID+1	; OF THE A
	sta	BUFSAV+1	
	lda	ZSP+LO		; MOVE [ZSP]
	sta	BUFSAV+2	
	lda	ZSP+HI	
	sta	BUFSAV+3	
	lda	OLDZSP+LO	
	sta	BUFSAV+4	
	lda	OLDZSP+HI	; MOVE [OLDZSP]
	sta	BUFSAV+5	
	ldx	#2		; MOVE CONTENTS OF [ZPC]
ZSL1:	lda	ZPC,X		; TO BYTES 7-9
	sta	BUFSAV+6,X	; OF [BUFSAV]
	dex		
	bpl	ZSL1	
	lda	TYPE
	sta	BUFSAV+9	; NORMAL OR PARTIAL
	cmp	#'P'
	bne	ZSNONM		; NORMAL SAVE SO NO name TO SAVE

	lda	ARG3+LO		; set up FPC to get save name
	sta	FPCL		; lo part is okay
	lda	ARG3+HI		; get page
	jsr	SETPC		; get memory addr
	sta	FPCH		; page number
	sty	FPCBNK		; and bank
	jsr	FETCHB		; get count
	sta	I		; and save it
	jsr	NEXTFPC		; point to next byte
	lda	#0		; set up data offset
	sta	J		; did it
ZSL3:
	jsr	FETCHB		; get data byte
	ldy	J		; get offset
	sta	BUFSAV+10,Y	; save into buffer
	jsr	NEXTFPC		; point to next byte
	inc	J		; next byte
	dec	I		; count it
	bne	ZSL3		; loop again
ZSNONM:
;
; WRITE [LOCALS]/[BUFSAV] PAGE TO DISK
;
	lda	#MAIN		; in the main bank
	sta	DSKBNK		; thank you
	lda	#HIGH LOCALS	; start at locals
	sta	DBUFF+HI	; POINT TO THE PAGE
	jsr	PUTDSK		; AND WRITE IT OUT
	bcc	ZSOK		; IF SUCCEEDED, WRITE STACK
ZSBAD:
	jsr	DISK_ERR	; print error message
ZSEXIT:
	jsr	CLOSE_SAVE	  ; else get game file back
	jmp	RET0		; AND FAIL
;
; IF A PARTIAL SAVE WRITE FROM ARG1 FOR ARG2 BYTES TO DISK
; (ROUNDED TO PGS) SKIPPING ZSTACK WRITE
;
ZSOK:
	lda	TYPE
	cmp	#'P'
	bne	ZSALL
	lda	ARG1+HI		; find where to start & how far to go
	jsr	SETPC		; get page in memory
	sta	DBUFF+HI	; this is page
	sty	DSKBNK		; which bank
	ldx	ARG2+HI		; get MSB of count
	lda	ARG1+LO		; get lo offset
	clc			; add
	adc	ARG2+LO		; lo count
	bcc	ZSPINC		; no extra page
	inx			; wrapped extra page
ZSPINC:
        bne     SAVE2DISK       ; go copy it now
;
; WRITE CONTENTS OF Z-STACK TO DISK
;
ZSALL:
	lda	#HIGH ZSTKBL	; point to 1st page
	sta	DBUFF+HI        
	jsr	PUTDSK		; write them, first one
	bcs	ZSBAD
	jsr	PUTDSK		; write them, second one
	bcs	ZSBAD
;
; WRITE ENTIRE GAME PRELOAD TO DISK
;
	lda	#HIGH ZBEGIN	; POINT TO 1ST PAGE
	sta	DBUFF+HI	; OF PRELOAD
	ldx	ZBEGIN+ZPURBT	; GET # IMPURE PAGES
SAVE2DISK:
	inx			; use for counting
	stx	I+LO
        lsr     I+LO            ; /2 for 512byte pages
        bcc     ZSL2            ; no wrapping
        inc     I+LO            ; wrapped once
ZSL2:
	jsr	PUTDSK          ; this does the write
	bcs	ZSBAD
	dec	I+LO            ; count one page
	bne	ZSL2            ; not done yet

	jsr	CLOSE_SAVE	; PROMPT FOR GAME FILE
	lda	#1		; SET TO MARK
	ldx	#0
	jmp	PUTBYT		; SUCCESS

; ------------
; RESTORE GAME
; ------------

ZREST:
	lda	#'N'
	ldx	NARGS
	beq	OLDRES		; NORMAL, COMPLETE RESTORE
	lda	#'P'		; partial restore
OLDRES:
	sta	TYPE		; save which kind of restore
	
	jsr	GET_SNAME	; get the name of the file
	bcs	ZRBAD		; okay, don't do it
	jsr	OPEN_RES	; open the restore file
	bcs	ZRBAD		; can't do it

	lda	TYPE		; PARTIAL SAVE DIFFERS STARTING HERE
	cmp	#'P'
	bne	ZRNRML
	jmp	ZPARTR		; just a partial restore

	; SAVE LOCALS IN CASE OF ERROR

ZRNRML:
	ldx	#31
LOCSAV:	lda	LOCALS,X	; COPY ALL LOCALS
	sta	LOCAL_SV,X	; to a save spot
	dex
	bpl	LOCSAV

	lda	#MAIN
	sta	DSKBNK		; SET TO WRITE TO MAIN BANK
	lda	#HIGH LOCALS
	sta	DBUFF+HI
	lda	#2              ; must read in two pages
	sta	L+LO
	jsr	GETRES		; RETRIEVE 1ST BLOCK OF PRELOAD
	bcs	ZRBAD

	lda	BUFSAV+0	; DOES 1ST BYTE OF SAVED GAME ID
	cmp	ZBEGIN+ZID	; MATCH THE CURRENT ID?
	bne	ZRQUIT		; WRONG DISK IF NOT

	lda	BUFSAV+1	; WHAT ABOUT THE 2ND BYTE?
	cmp	ZBEGIN+ZID+1
	beq	ZROK		; CONTINUE IF BOTH BYTES MATCH
 	bne	ZRQUIT		; skip disk error message
;
; HANDLE RESTORE ERROR
;
ZRBAD:
	jsr	DISK_ERR	; print error message
ZRQUIT:
	ldx	#31		; RESTORE ALL SAVED LOCALS
ZRL2:	lda	LOCAL_SV,X
	sta	LOCALS,X
	dex
	bpl	ZRL2
BADRES:
	jsr	CLOSE_SAVE	  ; PROMPT FOR GAME DISK
	jmp	RET0		; PREDICATE FAILS
;
; CONTINUE RESTORE
;
ZROK:
	lda	ZBEGIN+ZFLAGS	; SAVE BOTH FLAG BYTES
	sta	I+LO
	lda	ZBEGIN+ZFLAGS+1
	sta	I+HI

	lda	#HIGH ZSTKBL	; RETRIEVE OLD CONTENTS OF
	sta	DBUFF+HI	; z-stack
	lda	#4		; do 4 pages
	sta	L+LO		; tell GETRES how many pages
	jsr	GETRES		; get 4 pages of z-stack
	bcc	ZROKL1
	jmp	DISK_FATAL	; if here, mix of good & bad so die
ZROKL1:
	lda	#HIGH ZBEGIN	; get where we are
	sta	DBUFF+HI
	lda	ZBEGIN+ZPURBT	; GET # PAGES TO LOAD
	sta	I+LO
LREST0:
	lda	I+LO		; how many pages left
	sec			; doing subtract
	sbc	#4		; doing it 4 blocks at a time
	beq	LRESTj		; all done
	bcc	LREST1		; LOW 4 blocks left so deal with it special
	sta	I+LO		; save remenants
LREST:
	lda	#4		; assume at least 4 pages
	sta	L+LO		; this tells GETRES how many to read in
	jsr	GETRES		; fetch the remainder
	bcc	LREST0
	jmp	DISK_FATAL
LREST1:
	lda	I+LO		; get how many left
	sta	L+LO		; and show it to GETRES
	jsr	GETRES		; and finish it up
;
; RESTORE THE STATE OF THE SAVED GAME
;
LRESTj:
	lda	I+LO		; RESTORE THE STATE
	sta	ZBEGIN+ZFLAGS	; OF THE FLAG WORD
	lda	I+HI
	sta	ZBEGIN+ZFLAGS+1

	lda	BUFSAV+2	; RESTORE THE [ZSP]
	sta	ZSP+LO
	lda	BUFSAV+3
	sta	ZSP+HI
	lda	BUFSAV+4
	sta	OLDZSP+LO
	lda	BUFSAV+5	; AND THE [OLDZSP]
	sta	OLDZSP+HI

	ldx	#2		; RESTORE THE [ZPC]
ZRL4:	lda	BUFSAV+6,X
	sta	ZPC,X
	dex
	bpl	ZRL4

ZROUT:	jsr	CLOSE_SAVE	  ; PROMPT FOR GAME DISK
	jsr	VLDZPC		; MAKE VALID (MUST DO AFTER GET DISK)

	lda	#2		; SET TO
	ldx	#0
	jmp	PUTBYT		; SUCCESS


	; DO PARTIAL RESTORE GETTING 1ST PAGE 
	; AND LAST PAGE BYTE ALIGNMENT CORRECT

ZPARTR:	; WRITE LOCALS TO IOBUFF JUST TO LOOK AT NAME

	lda	#MAIN
	sta	DSKBNK
	lda	#HIGH IOBUFF	; DON'T READ TO LOCALS YET (X)
	sta	DBUFF+HI
	lda	#1		; just one block please
	sta	L+LO
	jsr	GETRES		; RETRIEVE 1ST BLOCK OF PRELOAD
	bcs	ZPBAD		; BAD DISK READ IF CARRY CLEAR

	lda	ARG3+LO		; set up FPC to get save name
	sta	FPCL		; lo part is okay
	lda	ARG3+HI		; get page
	jsr	SETPC		; get memory addr
	sta	FPCH		; page number
	sty	FPCBNK		; and bank
	jsr	FETCHB		; get count
	sta	I		; and save it
	jsr	NEXTFPC		; point to next byte
	lda	#LOW BUFSAV	; get bufsav offset
	clc			; and add
	adc	#10		; name offset
	sta	J		; did it
ZRN3:
	jsr	FETCHB		; get data byte
	ldy	J		; get offset
	cmp	IOBUFF,Y	; save into buffer
	bne	ZPBAD		; okay, then it's not it
	jsr	NEXTFPC		; point to next byte
	inc	J		; next byte
	dec	I		; count it
	bne	ZRN3		; loop again

	lda	ARG1+HI		; FIND WHERE TO START & HOW FAR TO GO
	jsr	SETPC		; get page in memory
	sta	SPCH		; this is page
	sty	SPCBNK		; which bank
	lda	ARG1+LO		; START BYTE FIRST PAGE
	sta	SPCL

	ldx	ARG2+HI
	stx	J+HI
	ldx	ARG2+LO
	stx	J+LO

	jsr	DECJ		; CORRECT ALIGNMENT FOR THIS USAGE
POK:
	lda	#HIGH IOBUFF	; GET 1ST PAGE
	sta	DBUFF+HI	; GETRES SHOULD KEEP IN IOBUFF
	lda	#1		; just do one block
	sta	L+LO
	jsr	GETRES
	bcc	ZPART0
	jmp	DISK_FATAL	; ALL MESSED UP, JUST QUIT
ZPART0:
	ldy	ARG1+LO		; START BYTE FIRST PAGE
	lda	IOBUFF,Y
	jsr	STASHB
	jsr	NEXTSPC
	jsr	DECJ
	bcs	ZPART1		; CARRY CLEAR IF $FFFF RESULT
	jmp	ZROUT
ZPART1:
	inc	ARG1+LO
	bne	ZPART0
	beq	POK		; read in a new block

ZPBAD:	JMP	BADRES		; NAMES DON'T MATCH, DIE
;
; THE OLD SAVE & RESTORE STILL HAVE OPCODES
; SO JUST PUT IN A PLACE FOR THEM HERE FOR NOW
;
OSAVE:
OREST:	RTS

ZISAVE:
ZIREST:	JMP	RET0	; NOT IMPLEMENTED ON APPLE
	END

