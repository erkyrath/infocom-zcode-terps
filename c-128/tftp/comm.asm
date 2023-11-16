;
; communications stuff
;
baud:	db	$0A,$10
baudlen	equ	$-baud

copen:				; open comm channel
	lda	#3		; file number
	tay			; nothing special
	ldx	#2		; device number
	jsr	SETLFS		; set up file

	ldx	#<baud		; set up baud command
	ldy	#>baud
	lda	#baudlen	; and length
	jsr	SETNAM		; so send it

	lda	RIDBS		; where does it now start
	sta	INDEX		; where do i start

	jsr	OPEN		; and so open it

	jmp	CLRCHN		; done
;
; get a byte from the comm buffer
;
getbyt:
	tya
	pha			; save y
	txa
	pha			; and save x
waiting:
	ldx	#3		; set channel
	jsr	CHKIN		; for input
	ldy	RIDBS		; get where we are
	cpy	RIDBE		; get where it ends
	beq	waiting		; nothing yet

	lda	RIBUFF,Y	; get char
	inc	RIDBS		; point to first one
	sta	INCHAR		; save it

	pla			; get a back
	tax			; and make it x
	pla			; and then just
	tay			; get y back
	lda	INCHAR		; put char in a
	rts			; and give it
;
; acknowledge a good block
;
doack:
	lda	#ACK		; get ack char
; fall through to send
putcom:
	sta	INCHAR		; save it
	ldx	#3
	jsr	CHKOUT		; get ready for output
	lda	#CMND		; sending back command
	jsr	CHROUT		; and bye
	lda	INCHAR		; get command going
	jsr	CHROUT		; and bye
	jmp	CLRCHN		; clear and leave
;
; get a tftp data frame
;
frame:
getcom:
; i'm just going to ignore this here first byte and just look for
; either a data or eof command
;	jsr	getbyt		; get byte from comm
;	cmp	#CMND		; getting command?
;	bne	getcom		; better, or i'll ignore it
	jsr	getbyt		; what kind of command?
	cmp	#DATACMND	; is this some more data?
	beq	dodata		; yup, so start getting it
	cmp	#EOF		; end of file?
	bne	getcom		; if not, just ignore it
	sec			; carry set 
	rts			; means EOF
;
; frame better start with: $01, $00, $FE, $FF
;
dodata:
	jsr	getbyt		; see what's up
	cmp	#$01		; first 	
	bne	rerror		; receive error
	jsr	getbyt
	bne	rerror		; must be $00
	jsr	getbyt
	cmp	#$FE		; looking for an FE
	bne	rerror
	jsr	getbyt
	cmp	#$FF		; and now an FF
	bne	rerror
;
; okay, now we can get the 256 byte data block and
; do the checksumming
;
	ldx	#0
	stx	CHECKSUM+LO	; reset the checksum
	stx	CHECKSUM+HI
comloop:
	jsr	getbyt		; get a byte
	sta	IOBUFF,X	; save it
	clc			; get ready for add
	adc	CHECKSUM+LO	; low byte
	sta	CHECKSUM+LO	; and save it
	bcc	cl0
	inc	CHECKSUM+HI	; went to next one
cl0:
	inx			; point to next char
	bne	comloop		; and get next one
;
; check check sum
;
	jsr	getbyt		; get MSB of check sum
	cmp	CHECKSUM+HI	; and compare with mine
	bne	rerror		; nope
	jsr	getbyt		; get LSB of check sum
	cmp	CHECKSUM+LO	; and compare with min
	bne	rerror		; nope
	clc			; carry clear
	rts			; show's its good
;
; nak a bad block here
;
rerror:
	lda	#NAK		; get nak cmnd
	jsr	putcom		; and send it out
	jmp	frame		; and start anew

