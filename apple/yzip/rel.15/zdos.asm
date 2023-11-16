	STTL "--- ZDOS (SEEKING, READING, WRITING) ---"
	PAGE	
;
; some ProDOS parameter blocks here
;
READ_PB:			; for READING files
	db	4		; 4 parameters
GMREF:	db	0		; refnum
	dw	IOBUFF		; read into io buffer
	dw	PAGELEN		; 512 byte buffer
	dw	0		; length actually read
OPEN_FILE:			; opening up the pure file
	db	3		; 3 parameters
	dw	GAME1NM		; name of pure file
	dw	GAME1FIO	; game file buffer
	db	0		; where refnum goes
PSEEK:	
	db	2	; 2 pararmeters
	db	0	; refnum
	db 	0,0,0	; 3 byte new file pos
SET_PB:
	db	1	; just one parameter
	dw	GAMEPL	; where to go to
;
; some messages
;
SWAPSAVE: db    EOL, "Take out save disk and insert ", EOL
SWAPSAVEL EQU   $-SWAPSAVE
NOSWAP: db      "game disk."
NOSWAPL EQU     $-NOSWAP
INS_MSG: db     EOL,"Insert "
INS_MSGL EQU    $-INS_MSG
SIDEMSG: db	", Side "
SIDEMSGL EQU	$-SIDEMSG
DRIVE_MSG: db ", in Disk Drive "
DRIVE_MSGL EQU  $-DRIVE_MSG

RESEAT_MSG1: db EOL, "Having problems reading side #"
RESEAT_MSG1L EQU   $-RESEAT_MSG1
RESEAT_MSG2: db ".",EOL,"Please re-insert it.",EOL,EOL
RESEAT_MSG2L EQU $-RESEAT_MSG2
	IF CHECKSUM
RETRYING: db	"RETRYING . . ."
RETRYINGL EQU $-RETRYING
	ENDIF
;
; place to stash prefixes and names
;
GPRE_PB:		; game prefix spot
	db	1		; 1 parm
	dw	GAMEPL		; where to put prefix
GAMEL:	db	0
GAME:	ds	15	; longest name

GAME1NM:
GAME1NML:
	db	0	; not sure how long
GAME1NAME:
	BLKB	15,0	; room for name
GAME1REF: db	0	; save refnum here
GAME1NUM: db	0	; number (0-3) of side

GAME2NM:
GAME2NML:
	db	0	; not sure how long
GAME2NAME:
	BLKB	15,0	; room for name
GAME2REF: db	0	; refnum for game file 2
GAME2NUM: db	0	; number (0-3) of side
GAMEPL: db	0		; name of prefix for game file
GAMEP:	ds	64-15		; max len
; --------------------
; READ A VIRTUAL PAGE
; --------------------
; ENTRY: V-BLOCK TO READ IN [DBLOCK]
; BUFFER ADDRESS IN [DBUFF]
; DSKBNK SAYS WHERE TO PUT PAGE (AUX OR MAIN)
; EXIT: DATA AT [DBUFF]
;	
RFLAG: ds 1
RCHKS: ds 1
GETDSK:
        lda     #RETRY_COUNT    ; set retry
        sta     RETRIES         ; just use this for now

	IF	DEBUG
	lda	#0
	sta	RFLAG		; show no retries
	sta	RCHKS		; and checksum for it
	ENDIF

	jsr	FINDSEG		; find the segment and point to it
GETDSKL:
	SET_MARK PSEEK		; move to the block
	bcs	GDBAD		; just die then
	jsr	READ_DOS	; do the read, thank you
	bcc     GDEX            ; okay, just fine
	inc	RFLAG
        jsr     RETRY           ; try again
        bcc     GETDSKL         ; try all over
GDEX:
	lda	READ_PB+RD_LENGTH+HI ; Get how much read in
	cmp	READ_PB+RD_BUFFLEN+HI ; same as we wanted?
	beq	GDEX1		; yes
	lda	#23
	jmp	ZERROR
GDEX1:
	IF	DEBUG
	lda	RFLAG		; see if we retried
	beq	GDEX3		; nope
	lda	RCHKS		; did retry checksum?
	bne	GDEX2
	inc	RCHKS		; now we have
	ldy	#0		; save block
	sty	$B00
	clc
GDEXL:
	lda	IOBUFF,Y
	sta	$A00,Y
	adc	$B00
	sta	$B00
	iny
	bne	GDEXL
	beq	GETDSKL
GDEX2:
	ldy	#0
	clc
	sty	RFLAG
GDEX2L:
	lda	IOBUFF,Y
	adc	RFLAG
	sta	RFLAG
	iny
	bne	GDEX2L
	lda	RFLAG
	cmp	$B00
	beq	GDEX3
	lda	#24
	jmp	ZERROR
	ENDIF
GDEX3:
	lda	#>IOBUFF	; first   256 byte page
	jsr	COPY_DATA	; and copy from IOBUFF to DBUFF
	inc	DBUFF+HI	; point to next one
	lda	#>IOBUFF+1	; next 256 byte page
	jmp	COPY_DATA	; and copy it over
GDBAD:
	jmp	DISK_FATAL	; just die then
RETRY:
	IF	CHECKSUM
	DLINE	RETRYING	; show me the way to go  home
	ENDIF

        dec     RETRIES         ; count this try
        bmi     GDBAD           ; it's dead jim
	beq	RETRY0		; ask for reset
	cmp	#1		; down to last time?
        bne     RETRYX          ; just try again
	lda	PSEEK+SM_FPOS+0
	pha
	lda	PSEEK+SM_FPOS+1
	pha
	lda	PSEEK+SM_FPOS+2
	pha
	lda	#0		; seek to zero
	sta	PSEEK+SM_FPOS+0 ; save here for now
	sta	PSEEK+SM_FPOS+1 ; save here for now
	sta	PSEEK+SM_FPOS+2 ; save here for now
	SET_MARK PSEEK		; move to the block
	pla
	sta	PSEEK+SM_FPOS+2
	pla
	sta	PSEEK+SM_FPOS+1
	pla
	sta	PSEEK+SM_FPOS+0
	bcc	RETRYX		; all done
RETRY0:
        pha                     ; save disk error
        jsr     SWAP2INFOW      ; go to window zero
        DLINE   RESEAT_MSG1     ; ask to move the disk around
        lda     GMREF           ; get ref num
        cmp     GAME2REF        ; game2?
        bne     RETRY1          ; nope
        ldy     GAME2NML        ; get length
        lda     GAME2NM,Y       ; and side #
        bne     RETRY2          ; and continue
RETRY1:
        ldy     GAME1NML        ; get length
        lda     GAME1NM,Y       ; and side #
RETRY2:
        jsr     CHAR            ; send char out there
        DLINE   RESEAT_MSG2     ; and finish statement
        pla                     ; get disk error back
        jsr     DISK_ERR        ; tell the error
        jsr     SWAPBACK        ; back to current window
RETRYX:
        clc                     ; show goodness
        rts                     ; and done
;
; this is the fatal error spot
;
DISK_FATAL:
	jsr	DISK_ERR	; print out problem
DRIVE_ERR:
	lda	#14
	jmp	ZERROR		; DRIVE ACCESS ERROR
;
; this routine prints out the string associated with the error and 
; returns with the carry set, like most routines do when there is
; an error.  The error should be in [A].
;
UNK_DISK:	db	"ProDOS error $"
UNK_DISKL	EQU	$-UNK_DISK

DISK_ERR:
	pha			; save [A]
	jsr	SWAP2INFOW	; point to information window
	pla			; get it back

	ldx	#ELISTL		; scan the error table
DSKE:
	cmp	ELIST,X		;  is it this one
	beq	DSKE1		; ayyup
	dex			; now skip the address
	dex			; of the description
	dex			; point to next error number
	bpl	DSKE		; jump to start
;
; if we are here print out unknown error and it's value
;
	pha			; save [A]
	DLINE	UNK_DISK	; show error
	pla			; get [A]
	jsr	HEXNUM		; print [A]
	jmp	DSK_EXIT	; done
DSKE1:
	inx			; point to string address
	lda	ELIST,X		; get lo part
	sta	L+LO		; save it
	inx			; point to hi part
	lda	ELIST,X		; get hi part
	tax			; save here
	sta	L+HI		; and here
	ldy	#0		; for offset
	lda	(L),Y		; get length of string
	tay			; save in y
	inc	L+LO		; point past length
	lda	L+LO		; get it
	bne	DSKPR		; for printing
	inx			; wrapped
DSKPR:
	txa			; hi part in a
	ldx	L+LO		; lo part in X
	jsr	DLINE		; print out message
DSK_EXIT:	
	jsr	GETRET		; wait for RETURN
	jsr	SWAPBACK	; all done
	sec			; show badness again
	rts			; and be done with it
;
; HEXNUM - print out the HEX value of [A] at the current cursor
; 	location
;
HEXNUM:
	pha
	lsr	A
	lsr	A
	lsr	A
	lsr	A
	jsr	NIB1
	pla
NIB1:
	and	#%00001111
	tay
	lda	HCHARS,Y
	jsr	CHAR			; print it out
	rts
HCHARS:	DB	"0123456789ABCDEF"

; ----------------------
; WRITE [DBUFF] TO DISK
; ----------------------
; ENTRY: File already open and positioned, ready to be written to
;	 from page in (DBUFF).  Writes out 512bytes, starting @ DBUFF.
; EXIT: CARRY CLEAR IF OKAY, SET IF FAILED

PUTDSK:
	ldx	DSKBNK		; get bank
	ldy	#0		; clear Y
        lda     #>IOBUFF        ; first buffer
	jsr	SAVE_DATA	; and copy from DBUFF to IOBUFF

	IF	CHECKSUM == 1
	lda	#>IOBUFF
	jsr	CKS_BLOCK
	ENDIF

	jsr	NEXT_DBUFF	; point to next buffer
        ldx     DSKBNK          ; go get new DSKBNK
        ldy     #0              ; clear Y again
        lda     #>IOBUFF+1      ; top part
        jsr     SAVE_DATA       ; copy to IOBUFF

	IF	CHECKSUM == 1
	lda	#>IOBUFF+1
	jsr	CKS_BLOCK
	ENDIF

        jsr     NEXT_DBUFF      ; count buffer
	jmp	WRITE_DOS	; and now write it
; ---------------------
; READ DBLOCK FROM DISK
; ---------------------
; CALLED BY RESTORE
; ENTER: with file already set up as it DBUFF
;	L+LO == # of 256k blocks to read
;
GETRES:
	lda	L+LO		; get # of blocks
	sta	READ_PB+RD_BUFFLEN+HI
	jsr	READ_DOS	
	ldx	#>PAGELEN	; get starting number of pages
	stx	READ_PB+RD_BUFFLEN+HI
	bcc	GTROK		; everything is fine
	rts			; error if c == set
GTROK:
	lda	#>IOBUFF	; start at first block
	sta	L+HI		; we can use L+HI
GTRLOOP:
	lda 	L+HI		; doing this block

        IF CHECKSUM == 1
        jsr     CKS_BLOCK       ; check check sum        
        ENDIF

	jsr	COPY_DATA	; and copy from IOBUFF to DBUFF
	jsr	NEXT_DBUFF	; check for wrap
	inc	L+HI		; count this block
	dec	L+LO		; count this block
	bne	GTRLOOP		; do next one
	rts			; all finished
;
; NEXT_DBUFF
;       increment DBUFF to count the 2 pages done
;
NEXT_DBUFF:
	inc	DBUFF+HI	; point to next page
	lda	DBUFF+HI	; see where we are
	cmp	#>PRGLBL	; wrapping?
	bne	GTREX		; okay then
	lda	DSKBNK		; which bank we be in
	bne	GTR1		; aux, so go to page 3
	inc	DSKBNK		; point to aux
	lda	#Z2PAGE		; start of page 2
        bne     GTREX           ; so tell me which page
GTR1:
	lda	#P3BANK		; show page 3 bank
	sta	DSKBNK		; okay
	lda	#Z3PAGE		; page 3 of things
GTREX:
	sta	DBUFF+HI	; saved
	rts			; all done		
;
WRITE_DOS:
	WRITE	WRITE_SV	; write out save buffer
	rts			; done
READ_DOS:
	READ	READ_PB		; read it
	rts			; go home
;
; COPY_DATA - 
; now move the data from iobuff to dbuff (in some bank)
;	which part of IOBUFF is in [a] ($08 - $0B)
;
COPY_DATA:
	sta	SDLP3+2		; self-modify code to get from IOBUFF
	sta	RDBNK+MAIN	; read from main
	ldx	DSKBNK		; get which bank it's going to
	bmi	CPD1		; oh oh, third bank
	sta	WRTBNK,X	; and select that bank for writing
	bpl	CPD2		; okay, bank selected
CPD1:
	lda	DBUFF+HI	; get from this zero page
	sta	ALTZP+AUX	; talk about aux mem
	sta	DBUFF+HI	; and save this in aux mem ZP
CPD2:
	ldy	#0		; start at beginning
	sty	DBUFF+LO	; just to be sure
SDLP3:
	lda	IOBUFF,Y	; this becomes modified!
	sta	(DBUFF),Y	; to [DBUFF]
	iny		
	bne	SDLP3	
	sta	ALTZP+MAIN	; talk about main page again
	sta	WRTBNK+MAIN	; end up back at main
	rts
;
; Segment table handling routines
;

;
; FINDSEG - find the Virtual page in DBLOCK by searching through
;	the SEGTBL.
;
DISKPAGE: ds	2		; spot to put starting disk page
BIGPAGE:  ds	2		; DBLOCK/2 for 512 pages
FINDSEG:
	lda	DBLOCK+HI	; work with MSB first
	lsr	A		; /2
	sta	BIGPAGE+HI
	lda	DBLOCK+LO	; now LSB
	ror	A		; /2
	sta	BIGPAGE+LO
	lda	GAME1NUM	; get number for disk 1
	jsr	SCANSEG		; scan through the current disk table
	lda	GAME1REF	; save refnum
	bcc	FNDSG1		; found it, figger it out
	lda	GAME2NUM	; get number for disk 2
	jsr	SCANSEG		; see if it is here
	lda	GAME2REF	; save refnum
	bcc	FNDSG1		; ayyup
	jsr	SEGQUEST	; get correct disk/file with segment
FNDSG1:
	sta	PSEEK+SM_REFNUM	; for seeking
	sta	GMREF		; and for reading
	lda	(DSEGS),Y	; pick up MSB of disk page
	sta	DISKPAGE+HI	; save it
	iny			; point to LSB
	lda	(DSEGS),Y	; get it
	sta	DISKPAGE+LO	; save it
; now point to Starting page again
	dey
	dey
	dey
	dey
	lda	BIGPAGE+LO	; LSB of desired page
	sec			; doing subtract
	sbc	(DSEGS),Y	; get rid of LSB of starting page
	sta	PSEEK+SM_FPOS+1 ; save here for now
	dey			; point to MSB of starting page
	lda	BIGPAGE+HI	; get DBLOCK MSB
	sbc	(DSEGS),Y	; get offset
	sta	PSEEK+SM_FPOS+2 ; save here temporarily
	clc			; now add offset to starting disk page
	lda	DISKPAGE+LO	; get starting disk page
	adc	PSEEK+SM_FPOS+1	; add inter-segment offset
	sta	PSEEK+SM_FPOS+1	; save LSB
	lda	DISKPAGE+HI	; get MSB of segment disk page
	adc	PSEEK+SM_FPOS+2	; add inter-segment offset
	sta	PSEEK+SM_FPOS+2	; save MSB of disk page
	asl	PSEEK+SM_FPOS+1	; *2 for 256Kb pages
	rol	PSEEK+SM_FPOS+2	; okay, we did
	lda	DBLOCK+LO	; check to see which page in 512 we are
	and	#$01		; if odd, then add one more
	beq	FNDEX		; all done then
	inc	PSEEK+SM_FPOS+1	; one more page
	bne	FNDEX		; all done if no wrap
	inc	PSEEK+SM_FPOS+2	; nother page
FNDEX:
	rts			; done
;
; SCANSEG - look through the current segment table for the desired
;	address found in BIGPAGE.  Return with Y pointing to disk page
;	and carry cleared if found.  Otherwise, return carry set.
;	[A] = which side number we are checking (0-n)
;
SCANSEG:
	pha			; save which disk
	asl	A		; make word index
	tax			; make it an index
	lda	DSKSEG+LO,X	; get LSB
	sta	DSEGS+LO
	lda	DSKSEG+HI,X
	sta	DSEGS+HI
	ldy	#SGTNSEG+1	; point to segment count
	lda	(DSEGS),Y	; get it
	tax			; use x as counter
	pla			; get which side 
	tay			; is it side zero
	bne	SCANSG1		; nope
	ldy	#SGTSEG+6	; skip first entry, cuz they're preload
	bne	SCANNING	; okay ready to go
SCANSG1:
	ldy	#SGTSEG		; begin at beginning
SCANNING:
	lda	(DSEGS),Y	; get MSB of start
	cmp	BIGPAGE+HI	; check against block we want
	beq	SCAN1		; might be okay
	bcc	SCAN2		; PAGE > start seg, check end seg
        iny                     ; LSB of start
SCAN0:
        iny                     ; MSB of end
        bcs     NEXTSEG         ; not this one
SCAN1:
	iny			; point to LSB of start
	lda	(DSEGS),Y	; get LSB
	cmp	BIGPAGE+LO	; check against desired LSB
	beq	GOTSEG		; we found it
	bcs	SCAN0		; DBLOCK LSB < then start LSB, not found
	dey			; point back to MSB of start
SCAN2:
	iny			; LSB of start
 	iny			; MSB of end
	lda	(DSEGS),Y	; get MSB of end
	cmp	BIGPAGE+HI	; check against DBLOCK MSB
	bcc	NEXTSEG 	; end < DBLOCK, check next segment
	bne	GOTSEG1		; end > DBLOCK, must be in this segment
	iny			; point to end LSB
	lda	(DSEGS),Y	; get LSB
	cmp	BIGPAGE+LO	; how does it compare to desired LSB
	bcs	GOTSEG2		; it's <= end, so it is here
	dey			; point back to MSB
NEXTSEG:
	iny			; point to LSB of end
	iny			; MSB of disk page
	iny			; LSB of disk page
	iny			; MSB of next start page
	dex			; count this segment
	bne	SCANNING	; check this segment

	sec			; show not on this disk
	rts			; and done
GOTSEG:
	iny			; MSB of end page
GOTSEG1:
	iny			; LSB of end page
GOTSEG2:
	iny			; MSB of disk offset
	clc			; show we found it
	rts			; all done
;
; SEGQUEST - find the correct disk/file with the desired page on
;	it.  Returns SCANSEG's stuff.
;
DISKCNTR:	ds	1	; disk count down
PAGENF:
	db	"Page not found in segment table: "
PAGENFL	EQU	$-PAGENF
SEGQUEST:
	lda	#1		; start at first disk
	sta	DISKCNTR	; init counter
SEGQL:
	lda	DISKCNTR	; get disk
	cmp	SEGTBL+SGTDISKS+1	; looked at all the disks?
	bne	SEGQL1		; nope
;
; as a last resort, check disk 1, the boot disk
;
	lda	#0		; set up DISKCNTR
	sta	DISKCNTR	; we did
	jsr	SCANSEG		; see if it is there
	bcc	SEGQ1		; we found it

	DLINE	PAGENF		; page not found
	lda	BIGPAGE+HI	; show MSB
	jsr	HEXNUM		; printed
	lda	BIGPAGE+LO	; and LSB
	jsr	HEXNUM		; we did

	lda	#17		; bad page 
	jmp	ZERROR		; die a horrible death
SEGQL1:
	lda	DISKCNTR	; get which disk we be working on
	jsr	SCANSEG		; find out if it is here
	bcc	SEGQ1		; it is, so open up file
	inc	DISKCNTR	; not in this disk
	bne	SEGQL		; and try again
;
; we have found the disk it is in, so ask for it if necessary
;
SEGQ1:
	tya			; save the y pointer
	pha			; it is saved
	lda	DISKCNTR	; get which disk we found it under
	jsr	FETCH_FILE	; go get the disk desired
	pla			; get the Y reg back
	tay			; and show it
	lda	GMREF		; get reference number again
	rts			; all done
;
; FETCH_FILE: check to see if we can open GAMEFILE# (where # is in [A])
; 	if not, ask for DISK#.
;
SAVENUM: db	0	; spot to save ascii number  
FETCH_FILE:
	clc			; adding to make
	adc	#$30		; it an ascii number
	sta	SAVENUM		; just save a minute
	inc	SAVENUM		; make name be one based
	ldx	GAME1NML	; get length of name
	lda	SAVENUM		; get name back
	cmp	#'3'		; are we looking for disk 1 or 2?
	bcs	FEFI1		; disks 3 and up handled special
FEFI00:
	cmp	GAME1NM,X	; is it the current open one?
	bne	FEFI0		; nope, so we need to open it
	ldy	GAME1REF	; get game 1 refnum
	bne	FEFIX		; all set, so point to it
FEFI0:
	jsr	DO_GAME1	; handle it special
        lda     GAME1REF        ; fetch refnum
	jsr	GET_PDATA	; now get the picture data
        ldy     GAME1REF        ; refetch refnum        
	bne	FEFIX		; so all done
FEFI1:
        ldx     GAME2NML        ; get length of second name        
	cmp	GAME2NM,X	; how bout second open file?
	bne	FEFI2		; nope, so we need to open it
	ldy	GAME2REF	; it is second one, so show me
	bne	FEFIX		; it is open too
FEFI2:
	lda	GAME2REF	; get its refnum
	beq	FEFI20		; nothing to close, thank you
	sta	CLOSE_PB+CL_REFNUM ; show close who to close
	CLOSE	CLOSE_PB	; close it up tight
FEFI20:
	jsr	OPEN_GAME2	; open up GAME2 file
        lda     GAME2REF        ; get refnum for this file        
	jsr	GET_PDATA	; now get the picture data
	ldy	GAME2REF	; get ref back, please
FEFIX:
	sty	PSEEK+SM_REFNUM	; for seeking
	sty	GMREF		; and for reading
	rts			; did it
;
; OPEN_GAME2 - open up the file that GAME2NM is pointing to,
;	returning the REFNUM in [A], after storing in GAME2REF,
;	and the 2 picture structures
;
OPEN_GAME2:
        ldx     GAME2NML        ; get end of name
        lda     SAVENUM         ; get new name
	sta	GAME2NM,X	; using second file
	lda	#>GAME2NM	; get lo byte
	sta	OPEN_FILE+OP_PATHNAME+HI
	lda	#<GAME2NM	; set address in open block
	sta	OPEN_FILE+OP_PATHNAME+LO
	lda	#>GAME2FIO	; now set up file i/o buffer
	sta	OPEN_FILE+OP_FILEBUFF+HI
	lda	#<GAME2FIO	; now set up file i/o buffer
	sta	OPEN_FILE+OP_FILEBUFF+LO
	jsr	OPEN_GAMEF	; find and open game file
	lda	OPEN_FILE+OP_REFNUM	; get refnum
	sta	GAME2REF	; save refnum here
	ldx	GAME2NML	; get me number of this side
	lda	GAME2NM,X	; get ascii number
	eor	#'0'		; make it binary
	sta	GAME2NUM	; save num
	dec	GAME2NUM	; make it zero based
	rts			; finis
;
; SET_GAMEPRE: check if game prefix is in, and if not go ask for it
;
SAVEDISK: db    0               ; swap out save disk?
SWAPPED:  db    0               ; <>0 if multi disk game

SET_GAMEPRE:
	jsr	SWAP2INFOW	; point to info window
        lda     SWAPPED         ; are we swapping disks?
        beq     SETGP           ; nope, don't change game prefix
	lda	SAVENUM		; get which disk to put it
        ldy     GAMEPL          ; get length of prefix
        dey                     ; subtract 2 to point to prefix
        dey                     ;   number
	sta	GAMEP,Y		; set number for prefix
SETGP:
	lda	#>GAMEPL	; set up parm block
	sta	SET_PB+SP_PATHNAME+HI
	lda	#<GAMEPL
	sta	SET_PB+SP_PATHNAME+LO
SETGP0:
	SET_PREFIX SET_PB	; set to game file prefix
	bcc	SETGP1		; okay, it is
	lda	SAVEDISK	; is it a save disk in there?
	beq	SEGTP00		; nope
;
; special stuff for  save disk swapping
;
	DLINE	SWAPSAVE	; tell them to take out save disk
        lda     SWAPPED         ; should we ask for specific disk?
        bne     SEGTP01         ; yes, so ask for it
        DLINE   NOSWAP          ; no, so just get game disk
        jmp     SEGTP03         ; and continue
SEGTP00:
	DLINE	INS_MSG		; ask for game disk
SEGTP01:
	DLINE	GAME, GAMEL	; this is the name of the game
	DLINE	SIDEMSG		; part 2 of message
	lda	SAVENUM		; get which disk to put it
	jsr	CHAR		; print it out
        DLINE   DRIVE_MSG       ; tell about drive
        lda     #'2'            ; assume it's drive 2
        ldx     SAVENUM         ; get which disk we want
        cpx     #'3'            ; >= side 3, must be disk drive 2
        bcs     SEGTP02         ; fine, send that message out
        lda     #'1'            ; ask for drive 1 then
SEGTP02:
        jsr     CHAR            ; save which drive
        lda     #'.'            ; finish sentence
        jsr     CHAR            ; okay
SEGTP03:
	jsr	GETRET		; wait for <CR>
	jmp	SETGP0		; try again
SETGP1:
	jmp	SWAPBACK	; pointing to disk
;
; DO_GAME1 - open up the special game 1 file and point to it
;
DO_GAME1:
	ldy	GAME1REF	; get the current game 1 ref num
	beq	DOG1		; not currently open
	pha			; save Name
	sty	CLOSE_PB+CL_REFNUM	; show close
	CLOSE	CLOSE_PB	; and close it
	pla			; get name back
DOG1:
        ldx     GAME1NML        ; get length of name
        lda     SAVENUM         ; get new number        
	sta	GAME1NM,X	; save name
	pha			; save it for later use
	lda	#>GAME1NM	; get lo byte
	sta	OPEN_FILE+OP_PATHNAME+HI
	lda	#<GAME1NM	; set address in open block
	sta	OPEN_FILE+OP_PATHNAME+LO
	lda	#>GAME1FIO	; now set up file i/o buffer
	sta	OPEN_FILE+OP_FILEBUFF+HI
	lda	#<GAME1FIO	; now set up file i/o buffer
	sta	OPEN_FILE+OP_FILEBUFF+LO
	jsr	OPEN_GAMEF	; so find and open game file
	pla			; get number back
	eor	#'0'		; make binary
	sta	GAME1NUM	; save number
	dec	GAME1NUM	; decrement for usage
	ldy	OPEN_FILE+OP_REFNUM	; get refnum
	sty	GAME1REF	; save refnum here
	rts			; did it
;
;OPEN_GAMEF: open the currently pointed to game file, getting the
; disk if necessary (ascii number of disk/file found in SAVENUM)
;
OPEN_GAMEF:
	lda	#0		; reset volume flag
	sta	CHPTR+HI	;okay, we be ready
OPGM1:
	OPEN	OPEN_FILE	; open the file file
	bcc	OPGMX		; okay
	cmp	#$46		; file not found error?
	beq	OPGMV		; get the volume
	cmp	#$45		; volume not found error?
	beq	OPGMV		; then ask for it
	jmp	DISK_ERR	; die with ZERROR
OPGMV:
	jsr	CHECKVOLS	; try another volume
	bcc	OPGM1		; ==0 if no more volumes to try
        lda     SAVEDISK        ; doing save?
        bne     OPGMV1          ; yes, so don't set swap flag
        lda     #1              ; set swap flag
        sta     SWAPPED         ; we have to swap disks
OPGMV1:
	jsr	SET_GAMEPRE	; okay, set up prefix
	bcc	OPEN_GAMEF	; try again
OPGMX:
	rts
;
; LISTVOLS - list all the online volumes for saving to
;
ONLINE_PB:
	db	2	; 2 parms for ONLINE call
	db	0	; unit == 0 to get all current ones
	dw	IOBUFF	; use IOBUFFER to store names
LSTVM:	db	EOL,"Volumes: ",EOL
LSTVML	EQU	$-LSTVM

LISTVOLS:
	DLINE	LSTVM		; print out intro message

	ONLINE	ONLINE_PB
	bcc	LSTV1		; worked fine
	jsr	DISK_ERR	; complain if we had an error
	bcs	LSTVEX		; all done
LSTV1:
	lda	#0		; start at byte zero
	pha			; saved on stack
LSTVL:
	pla			; get index
	tax			; make it an index
	lda	IOBUFF,X	; get drive/length byte
	beq	LSTVEX		; all done if == 0
	and	#$0F		; just care about length
	tay			; save in [Y] for now
	txa			; into [A] for addition
	clc			; doing add
	adc	#16		; point to next entry
	pha			; save for later reference
	tya			; check for zero length
	beq	LSTVL		; nothing here but an error
	lda	#'/'		; start with / to be better
	jsr	CHAR		; and it is out there
LSTVCL:
	inx			; point to next char
	lda	IOBUFF,X	; get char
	jsr	CHAR		; print it out
	dey			; count char
	bne	LSTVCL		; go get next char
	lda	#'/'		; end with '/' to show volume status
	jsr	CHAR		; and awaaaaay we go
	lda	#EOL		; do a <CR>
	jsr	CHAR		; printed
	dex			; count back one, as loop starts with inx
	bne	LSTVL		; go do next one
LSTVEX:
	rts			; toots finis
;
; CHECKVOLS - set prefix to particular volume that is currently online
;
CHECKVOLS:
	lda	CHPTR+HI	; is it zero?
	bne	CHV1		; if not, then get next volume
	ONLINE	ONLINE_PB	; get online volumes
	bcc	CHV0		; okey dokey
CHVBX:
	sec			; show badness
	rts			; all done
CHV0:
	lda	#<(IOBUFF-16)	; get LSB (-16 cuz we start with add)
	sta	CHPTR+LO
	lda	#>IOBUFF	; and mSB
	sta	CHPTR+HI
	lda	#>SCRBUFF		; this is where we will work on it
	sta	SET_PB+SP_PATHNAME+HI
	lda	#<SCRBUFF
	sta	SET_PB+SP_PATHNAME+LO
CHV1:
	lda	CHPTR+LO	; point to next entry
	clc			; getting ready for add
	adc	#16		; this points to next one
	sta	CHPTR+LO	; can't be any wrapping
	ldy	#0		; to use indirect addressing
	lda	(CHPTR),Y	; get length byte
	beq	CHVBX		; all done if == 0
	and 	#$0F		; get rid of upper junk
	beq	CHV1		; nothing in this one, do it again
	sta	SCRBUFF		; save length
	inc	SCRBUFF		; count the add '/'
	tay			; get for countdown
	lda	#'/'		; need to start with a slash
	sta	SCRBUFF+1	; okay, we are
CHVL:
	lda	(CHPTR),Y	; get char in path name
	sta	SCRBUFF+1,Y	; save name
	dey			; count char
	bne	CHVL		; go get next one

	SET_PREFIX SET_PB	; so point to it
CHVX:
	rts			; carry==0, we worked okay
;
; GET_SPECIAL - if we are in infodos, then we preload the first 12Kb of
;   stuff into the area in the main bank, pages $D0-$FF, Language Card 2.
;  Assume .D2 file has just been opened and go get it.
GET_SPECIAL:
        lda     #RETRY_COUNT
        sta     RETRIES         ; how many retries
GTS_RDL:
        lda     #0              ; make sure we are at the start
        sta     PSEEK+SM_FPOS	; set pos to zero
        sta     PSEEK+SM_FPOS+1
        sta     PSEEK+SM_FPOS+2
        sta     K+LO
        sta     L+LO            ; make sure these are zero too
        SET_MARK PSEEK		; go back to the beginning
        
        lda     #2              ; one page at a time        
        sta     READ_PB+RD_BUFFLEN+HI   ; show how much is special
        lda     #SP_SIZE        ; how many pages in special part?
        sta     J+LO            ; use as counter
        lda     #>SP_START      ; where does it go?
        sta     L+HI            ; L is pointer to there
GTS_RD:
        lda     #MAIN           ; make sure we are in main
        sta     WRTBNK+MAIN
        READ    READ_PB         ; go get 1Kb
        bcc     GTS_RD1         ; just fine
        jsr     RETRY           ; try again
        bcc     GTS_RDL         ; and do it again
GTS_RD1:
        ldx     #SP_BANK        ; get bank that special is in
        sta     WRTBNK,X
        lda     #>IOBUFF        ; get MSB of start
        sta     K+HI            ; K is source
        ldx     #2              ; 2 blocks of 256 bytes each
GTS_CPL0:
        ldy     #0              ; indexer
GTS_CPL:
        lda     (K),y           ; get it
        sta     (L),y           ; store it
        iny                     ; next
        bne     GTS_CPL         ; gwon then
        inc     K+HI            ; point to next block
        inc     L+HI            ; for destination too
        dex                     ; count block
        bne     GTS_CPL0        ; next block
        dec     J+LO            ; count this 1Kb
        bne     GTS_RD          ; go get next one
        
        sta     WRTBNK+MAIN     ; make sure we are in main
;
; now figger out last page
;
        ldy     #SGTSEG+1       ; get start segment LSB
        lda     (INFODOS),Y     ; got it
        clc                     ; doing add
        adc     #<SP_SIZE       ; add in size LSB
        sta     INFODOS_END+LO  ; save end
        dey                     ; point to MSB
        lda     (INFODOS),Y     ; got it
        adc     #>SP_SIZE       ; add in size MSB
        sta     INFODOS_END+HI  ; and save it
        lda     INFODOS_END+LO  ; now, subtract one to make
        bne     GTS_END         ; it inclusive
        dec     INFODOS_END+HI  ; wrapped, so dec MSB
GTS_END:
        dec     INFODOS_END+LO  ; make pointers inclusive

        rts                     ; done

	IF	CHECKSUM == 1
CKS_COUNT: db 0
CKSB:	db	0
CKS_BLOCK:
        pha
        sta     CKSMOD+2
        ldy     #0
	lda	CKSB
CKSLP:
        clc
CKSMOD: adc     IOBUFF,Y
        iny
        bne     CKSLP

	sta	CKSB
	inc	CKS_COUNT
	pla
	rts

	IF 	0
        jsr     HEXNUM
        lda     CKS_COUNT
        cmp     #$F
        bne     CKSCNT
        lda     #0
        sta     CKS_COUNT
        lda     #EOL
        bne     CKSSEND
CKSCNT:
        inc     CKS_COUNT
        lda     #SPACE
CKSSEND:
        jsr     CHAR
        jsr     DISP_LINE
        pla
        rts
	ENDIF

        ENDIF

	END
