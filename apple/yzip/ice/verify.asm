	TITLE	"Apple ][ YZIP (c)Infocom","VERIFY CODE"

	; ------
	; VERIFY
	; ------
	; VERIFY GAME CODE ON DISK
VERBAD:	DB	EOL,"The data segment of file is BAD!",EOL
VERBADL	EQU	$-VERBAD
VERPBAD: DB	EOL,"The picture data of file is BAD!",EOL
VERPBADL EQU	$-VERPBAD
ZVFLAG:	db	0		; set to indicate ugliness
STARTPOS EQU	I+LO		; this is where to start block
ZVER:
	lda	DSEGS+HI		; save current DSEG
	pha
	lda	DSEGS+LO
	pha
	lda	GMREF			; to go back to this one
	pha
	jsr	CLOSE_GAME	; make sure the game files are closed
	ldy	GAME2NML	; get length of name
	lda	GAME2NM,Y	; get last char
	eor	#$30		; make normal number
	tay			; -1 to make ready for FETCH_FILE
	dey			; F_F incs first
	tya			; to push onto stack
	pha			; and save for restoring later
	lda	#0		; clear a few counters
	sta 	ZVFLAG		; ==0 - verify worked; !=0 - verify broke
	lda	SEGTBL+SGTDISKS+1	; get how many disks are here
	sta 	DISKCNTR	; this shows which disk we are working on
	dec	DISKCNTR	; start down by one
VERIFY_LOOP:
	jsr	VERIFY_DATA	; check data in this file
	jsr	VERIFY_PICD	; check (possible) picture data in this file
	lda	GMREF		; get reference number
	sta	CLOSE_PB+CL_REFNUM ; and show CLOSE
	CLOSE	CLOSE_PB	; and shut it up
	dec	DISKCNTR	; next please
	bpl	VERIFY_LOOP	; and check the next file
ZVERX:
	lda	# HIGH PAGELEN	; reset read buffer length
	sta	READ_PB+RD_BUFFLEN+HI	; to be $100
	lda	# LOW PAGELEN	; this is for LSB
	sta	READ_PB+RD_BUFFLEN+LO	; to be $100
	jsr	CLOSE_GAME	; close up the 2 files, thank you
	lda	#1		; open game file #1 (*.D2)
	jsr	FETCH_FILE	; this opens it up
	pla			; get file number back (*.D3/4)
	jsr	FETCH_FILE	; and open it back up
;
; now point to the one we used to pointing to
;
	pla				; get old ref num back
	sta	GMREF			; and for reading
	sta	PSEEK+SM_REFNUM 	; this sets it up for set marking
	pla				; get old DSEG back
	sta	DSEGS+LO
	pla
	sta	DSEGS+HI

	lda	ZVFLAG		; see if we were successful
	beq	ZVERGOOD	; it worked fine
	jmp	PREDF		; no it didn't
ZVERGOOD:
	jmp	PREDS		; all done
;
; VERIFY_DATA - check the data part of the picture.  This routine sets
; up DSEGS and open the file.  It will move to the data section if this
; is the preload file, set up J to be the number of 256 byte blocks, and
; print out the verdict.  The disk we are working on (0-3) is in DISKCNTR.
;
VERIFY_DATA:
	lda	DISKCNTR	; get disk we are interested in
	asl	A		; make word pointer
	tax			; create index
	lda	DSKSEG+HI,X	; get MSB of seg table address
	sta	DSEGS+HI	; save in pointer
	lda	DSKSEG+LO,X	; get LSB of seg table address
	sta	DSEGS+LO	; now pointer is all set
	lda	DISKCNTR	; get disk again
	jsr	FETCH_FILE	; so we can open up correct file
	lda	#0		; make sure we start at beginning
	sta	PSEEK+SM_FPOS
	sta	PSEEK+SM_FPOS+1
	sta	PSEEK+SM_FPOS+2
	sta	STARTPOS	; start at first byte, prob'ly
	lda	DISKCNTR	; now check if this is preload
	bne	VERD1		; if <> 0, then it isn't
	jsr	SETUP_DISK0	; move around to point to start of data
VERD1:
	SET_MARK PSEEK		; and move
	ldy	#SGTPICOF	; find where picture data starts
	lda	(DSEGS),Y	; MSB
	sta	J+HI		; J is the page counter
	iny			; point to LSB
	ora	(DSEGS),Y	; any picture file?
	bne	VERD01		; yes, so mark end of data
	lda	#$FF		; set J to be a real big number
	sta	J+LO		; okay
	sta	J+HI		; and this one
	bne	VERD11		; all done
VERD01:
	lda	(DSEGS),Y	; and here it is
	asl	A		; *2 to make 512 pages be 256 pages
	sta	J+LO		; this is where it ends up
	rol	J+HI		; move in carry to MSB
VERD11:
	jsr	VERIFY_FILE	; now, actually do the work
	bcc	VERDX		; worked just fine
	DLINE	VERBAD,		; puke, gag, argh
;
; This prints out which file is garfed
;
VER_FMSG:
	DLINE	GAMEP,GAMEPL
	lda	DISKCNTR	; which file did we do?
	cmp	#2		; 0,1 are in one place
	bcs	VERDB1		; nope it isn't it
	DLINE	GAME1NAME,GAME1NML
	jmp 	VERDB2		
VERDB1:
	DLINE	GAME2NAME,GAME2NML ; 2,3 are in another
VERDB2:
	inc	ZVFLAG		; show bad file
	jsr	GETRET		; just wait for <CR>
VERDX:
	rts			; all done
;
; VERIFY_PICD - verify the picture data in a file.  First check to see if
; there is any.  If so, seek to it, set J==0 to show VERIFY_FILE to read
; til EOF, and print out bad picture data message if necessary.
;
VERIFY_PICD:
	lda	#$FF		; gonna zero bunch of stuff
	sta	J+HI		; and the counter
	sta	J+LO		; which means now goto EOF
	ldy	#SGTPICOF	; fetch the picture data offset
	lda	(DSEGS),Y	; get MSB
	sta	PSEEK+SM_FPOS+2	; we are doing pages
	iny			; pointing to LSB
	ora	(DSEGS),Y	; first, check for all zeros
	bne	VERP1		; nope, some picture data is there
	rts			; just gwon back if nothing here
VERP1:
	lda	(DSEGS),Y	; go get LSB
	asl	A		; *2 to get 256 byte pages
	sta	PSEEK+SM_FPOS+1	; put away here
	rol	PSEEK+SM_FPOS+2	; pick up carry
	lda	#PHSIZE		; skip over header of file
	sta	STARTPOS	; show offset in first block
VERP11:
	SET_MARK PSEEK		; and move to picture data spot
	jsr	VERIFY_FILE	; read in the data
	SET_MARK PSEEK		; get back to beginning of pic data
	READ	READ_PB		; read in a block worth
	lda	IOBUFF+PHCHKS	; get MSB of picture checksum
	cmp	L+HI		; same as what we got?
	bne 	VERPB		; nope
	lda 	IOBUFF+PHCHKS+1	; get LSB of picture checksum
	cmp	L+LO		; same as mine?
	beq	VERPX		; yup, checked out fine
VERPB:
	DLINE	VERPBAD,	; picture data bad
	jmp	VER_FMSG	; print out file name
VERPX:
	rts			; tootis finis
;
; VERIFY_FILE - Files is already open and pointing to start of checksummed
; data.  Works along til J == 0 or EOF, which ever comes first.  Starts by
; dec'ing J, so if J starts as 0, effectively means goto EOF.
;
VERIFY_FILE:
	lda	#0		; clear out checksum counter
	sta	L+HI		; MSB
	sta	L+LO		; LSB
	sta	READ_PB+RD_BUFFLEN+LO
	lda	#4		; make read read $400 (1Kb)
	sta	READ_PB+RD_BUFFLEN+HI
VFLOOP:	
	lda	# LOW IOBUFF	; reset K to point to beginning of
	sta	K+LO		; IOBUFF we are
	lda	# HIGH IOBUFF	; reading all the data
	sta	K+HI		; into, using as pointer
	READ	READ_PB		; read in 1Kb of data
	bcc	VERF0		; just fine read
	cmp	#$4C		; EOF error?
	beq	VFCHECK		; yes, so wee bee done
	jmp	DISK_ERR	; complain about read
VERF0:
	ldy	STARTPOS	; and begin
VERF1:
	lda	(K),Y		; get byte
	clc			; doing add
	adc	L+LO		; add it in
	sta	L+LO		; save it
	bcc	VERF2		; no wrap
	inc	L+HI		; yes ther is
VERF2:
	iny			; next byte
	bne	VERF1		; back to start of inner tloop
	dec	J+LO		; count block
	bne	VERF3		; no wrap
	lda	J+LO		; get LSB
	ora	J+HI		; or with MSB to check for zero
	beq	VFCHECK		; all done then
	dec	J+HI		; then wrap
VERF3:
	lda	#0		; start at first byte
	sta	STARTPOS	; okay
	inc	K+HI		; point to next block
	dec	READ_PB+RD_LENGTH+HI	; count this one
	beq	VFLOOP		; go read some more
	bne	VERF1		; go do next 256 byte block
VFCHECK:
	ldy	#SGTCHKS	; get check sum
	lda	L+HI		; start with MSB
	cmp	(DSEGS),Y	; well . . . ?
	bne	VFBAD		; nope, it is wrong
	iny			; first byte is okay
	lda	L+LO		; so check LSB
	cmp	(DSEGS),Y	; well . . . ?
	bne	VFBAD		; die a slow ugly death
	clc			; clear carry to show niceness
	rts
VFBAD:
	sec			; carry is set if bad
	rts			; and return to your fate
;
; SETUP_DISK0 - this routine does some special processing to get the file
; pointer to the beginning of data for the preload file.  It skips over
; segment table and first 64 bytes of lo core.
SETUP_DISK0:
	READ	READ_PB		; read in first block
	lda	IOBUFF		; MSB of segment table size (in words)
	sta	PSEEK+SM_FPOS+1	; middle part of offset
	lda	IOBUFF+1	; LSB of size
	asl	A		; *2 to get # bytes
	sta	STARTPOS	; LSB of offset
	rol	PSEEK+SM_FPOS+1	; rotate in carry
	rol	PSEEK+SM_FPOS+2	; and once more
	lda	#66		; add 2 to skip word count and 64 to
	adc	STARTPOS	; skip lo core in preload
	sta	STARTPOS	; save it
	bcc	STD0X		; all done, no wrapping
	inc	PSEEK+SM_FPOS+1	; count wrap
	bne	STD0X		; again, no wrapping
	inc	PSEEK+SM_FPOS+2	; gipper
STD0X:
	rts			; all done

	END
