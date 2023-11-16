;
; this does receiving and writing
;
doingmsg: db	EOL,EOL,"Waiting for transfer . . . .",EOL,EOL
	db	CYAN,"Blocks Received: "
doinglen equ	$-doingmsg

donemsg: db	EOL,EOL,RVSON,DYELLOW,"Finished with transfer.",EOL,EOL
	db	EOL,"   Hit any key to continue  . . . ",
donelen	equ	$-donemsg

doit:
	lda	#'0'		; init ascii counter
	sta	BLNUM
	sta	BLNUM+1
	sta	BLNUM+2

	jsr	copen		; open comm port
	jsr	dopen		; open disk drive
	MSG	doing
	
	sec			; now get where cursor is for
	jsr	PLOT		; blocks count display
	stx	XSAVE		; and save it
	sty	YSAVE
xloop:
;	jsr	chkkey		; see if user wants to abort
;	bcs	done		; then quit now
	jsr	frame		; get transmit frame
	bcc	writeit
	jmp	done		; cs if EOF
writeit:
	jsr	dwrite		; write out block
	jsr	nxtts		; get next track and sector
	jsr	doack		; send back ack
	jsr	blocks		; inc and display blocks done
	jmp	xloop		; and continue
;
; all done with transfer, so show it and leave
;
done:
	jsr	doack		; finished with this transfer
	jsr	CLRCHN		; clear things up

	MSG	done		; and show world we are done
donel:
	jsr	GETIN
	tay
	beq	donel		; wait for user to be happy

	jmp	begin		; and start all over

blocks:
	ldx	XSAVE		; get where to put it
	ldy	YSAVE
	clc
	jsr	PLOT		; and put it there

	ldx	#2		; inc chars counting
countl:
	inc	BLNUM,x		; inc it
	lda	BLNUM,x		; get it for check
	cmp	#'9'+1		; if <= '9' all set
	bcc	printit		; so print all chars
	lda	#'0'		; reset this one to zero
	sta	BLNUM,x		; and then go check next one
	dex
	bpl	countl		; check next one
printit:
	lda	BLNUM		; now put it out there
	cmp	#'0'		; don't print leading zero
	beq	next1		; so don't do it
	jsr	CHROUT
next1:
	lda	BLNUM+1		; one by one, by hand for speed
	cmp	#'0'		; don't print leading zeros
	bne	pj		; okay, so print it
	ldx	BLNUM		; gotta check first one too,
	cpx	#'0'		; and if it is zero then don't print
	beq	pj1		; any of these
pj:
	jsr	CHROUT		; not leading zero, so print
pj1:
	lda	BLNUM+2		; and now the last one
	jsr	CHROUT

	rts

;
; see if user has hit abort (ESCAPE KEY) to get out
;
chkkey:
	jsr	CLALL
	jsr	GETIN
	cmp	#ESCAPE		; is it the escape key?
	bne	notdone

	sec			; set to show found
	rts
notdone:
	clc			; clear to show not found
	rts

