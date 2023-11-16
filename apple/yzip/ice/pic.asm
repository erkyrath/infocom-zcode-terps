	TITLE	"Apple ][ YZIP (c)Infocom","Picture Op Codes"
PFILE_RD:			; for READING files
	db	4		; 4 parameters
	db	0		; refnum
	dw	IOBUFF		; read into io buffer
	dw	$400		; 1Kb buffer
	dw	0		; length actually read
PFSEEK:
	db	2	; 2 pararmeters
	db	0	; refnum
	db 	0,0,0	; 3 byte new file pos
NUMPIC:	dw	0	; how many entries
PICCNT: dw	0	; counter of the entries
P_IDX:	db 	0	
P_BCNT:	db	0
P_LOFF:	db	0
PICINFO:
	ds	PLDSIZE ; get room for picture data

TRANSCLR: DB	0	; transparent color
UNKPIC:
	db	EOL,"Unkown Picture #"
UNKPICL	EQU	$-UNKPIC
ZDISPL:
	jsr	CLRBUF			; put out any and all text
	jsr	SAVE_CURSOR		; save the cursor pos

	jsr	GET_PICINF		; get the necessary data
	bcc	ZDSP1			; everything just fine
	jmp	GET_CURSOR		; not able to, restore cursor and die
ZDSP1:
	lda	#0			; show no cursor change
	sta	CURSFLG			; start with none, anyway

	dec	NARGS			; x,y pos passed?
	beq	ZDSP10			; nope, just use as is

	lda	ARG2+LO			; get passed y pos
	beq	ZDSP01			; don't change
	sta	SCRCY			; change, then
	dec	SCRCY			; zero base it
	ldy	#WINTOP			; get top edge of window
	lda	(WINDOW),Y		; got it
	clc				; get ready for add
	adc	SCRCY			; add the offset
	sta	SCRCY			; and make it Y pos
	lda	#1			; show cursor change anyway
	sta	CURSFLG			; okay, we did
ZDSP01:
	dec	NARGS			; y pos passed?
	beq	ZDSP10			; nope

	lda	ARG3+LO			; get x pos
	beq	ZDSP10			; if == 0, don't change
	sta	SCRCX			; set as passed
	dec	SCRCX			; zero base it
	ldy	#WINLEFT		; get left hand edge of window
	lda	(WINDOW),Y		; got it
	clc				; get ready for add
	adc	SCRCX			; add the offset
	sta	SCRCX			; and make it X pos
	lda	#1			; show cursor change anyway
	sta	CURSFLG			; okay, we did
ZDSP10:
	lda	CURSFLG			; see if cursor changed?
	bne	ZDSP101			; it did, so restore back to here
	lda	SCRCX			; get current X
	pha				; save it
	clc				; adding
	adc	PICINFO+PLDWID		; add in pic width
	sta	SCRCX			; save here
	jsr	SAVE_CURSOR		; resave the cursor
	pla				; get start X pos back
	sta	SCRCX			; restored
ZDSP101:
	lda 	PICINFO+PLDFLG		; get flag byte
	and	#1			; is there a transparent color?
	bne	ZDSP11			; ayyup
	lda	#$FF			; make TRANSCLR be $FF
	bne	ZDSP12			; okay
ZDSP11:
	lda	PICINFO+PLDFLG		; get hi byte of flag word
	lsr	A			; put in lower byte
	lsr	A			; put in lower byte
	lsr	A			; put in lower byte
	lsr	A			; put in lower byte
ZDSP12:
	sta	TRANSCLR		; save transparent color

	lda	PICINFO+PLDPTR		; MSB of offset
	sta	PFSEEK+SM_FPOS+2	; MSB of seek
	lda	PICINFO+PLDPTR+1	; Middle
        and     #$FE                    ; seek only to 512byte boundary
	sta	PFSEEK+SM_FPOS+1
;	lda	PICINFO+PLDPTR+2	; LSB
;	sta	PFSEEK+SM_FPOS

	SET_MARK PFSEEK			; go to pic data
	lda	# HIGH PIC1BUF		; set up pointers to decode buffers
	sta	J+HI
	lda	# LOW PIC1BUF
	sta	J+LO
	lda	# HIGH PIC2BUF
	sta	K+HI
	lda	# LOW PIC2BUF
	sta	K+LO
	lda	#0			; start line at zero
	sta	P_LOFF			; done
	ldy	#MAXWIDTH-1		; clear out width buffer
ZDLI:
	sta	(J),Y			; init 'previous line' buffer to zero
	dey				; down one
	bne	ZDLI
	sta	(J),Y			; get last one

	lda	#3			; 3 bytes of width data start it
        clc                             ; doing add
	adc	PICINFO+PLDPTR+2	; pick up LSB pointer
	sta	P_IDX			; start index
	lda	# HIGH IOBUFF		; now get data buffer address
	sta	L+HI
	lda	#4			; 4 pages read in at once
	sta	P_BCNT			; okay
	lda	PICINFO+PLDPTR+1        ; tells which block it is in               
        and     #$01                    ; pick up which 512 block
        beq     ZDLP0                   ; start at first one
        inc     L+HI                    ; start at second one
        dec     P_BCNT                  ; one less block
ZDLP0:
	lda	# LOW IOBUFF
	sta	L+LO
	READ	PFILE_RD		; read in 1kb worth of data
	bcc	ZDECLP			; everything went just fine
	jsr	DISK_ERR		; print out an error
	rts				; and die
ZDECLP:
	ldy	P_IDX			; get data index
	lda	(L),Y			; get data byte
	sta	ARG8			; save here
	iny				; point to next one
	bne	ZDCLP0			; okay, use offset
	jsr	NEXT_PICB		; read in next block
	bcc	ZDCLP0			; everything went fine
	rts				; return if not
ZDCLP0:
	lda	(L),Y			; is this count or data?
	cmp	#16			; if <= 15, previous one was data 
	bcs	ZDCL2			; nope, must be compressed
	lda	#15			; show 1 bytes
	bne	ZDCL3			; and count this one
ZDCL2:
	iny				; point to next byte
	bne	ZDCL3			; okay, no wrap
	pha				; save A
	jsr	NEXT_PICB		; check about nother block
	pla				; get A back
	bcc	ZDCL3			; everything came out okay
	rts				; problems, return
ZDCL3:
	sty	P_IDX			; save index
	sec				; get ready for sub
	sbc	#14			; make good counter
	tax				; put count into x
ZDCLPC:
	ldy	P_LOFF			; get line offset
	lda	ARG8			; get data byte
	eor	(J),Y			; XOR with previous line
	sta	(K),Y			; and save away
	iny				; next offset
	sty	P_LOFF			; save Y
	cpy	PICINFO+PLDWID		; end of line?
	bne	ZDCLPC1			; nope
	jsr	COPY_PIC		; copy line into screen
	lda	SCRCY			; past bottom?
	cmp	#MAXHEIGHT-1		; well?
	beq	ZDCLPX			; truncate at bottom then
	inc	SCRCY			; down one line
	dec	PICINFO+PLDHGHT		; count line
	beq	ZDCLPX			; all done then
ZDCLPC1:
	dex				; count down counter
	bne	ZDCLPC			; do this byte again
	beq	ZDECLP			; check next byte
ZDCLPX:
	jmp	GET_CURSOR		; restore the cursor

COPY_PIC:
	txa				; save x
	pha 
	lda	SCRCX			; get start X pos
	pha				; okay, saved
	lda	#0			; start offset at zero
	sta	CH_OFFSET		; and into data buffer
CPICLOOP:
	jsr	SETPOS			; point to correct line
	ldx	BITOFF			; get starting offset
	stx	BITTER			; bit offset for COPY_SCRL to use
	lda	BIT_BITS,X		; get starting bit counter
	tax				; okay, we did
	lda	#MAXWIDTH		; don't let screen wrap
	sec				; doing sub
	sbc	SCRCX			; get current X pos
	sta	DATALOOP		; only use this many pixels
	ldy	#0			; start with first byte
	sty	SCR_DATA		; data starts with zero
	dey				; inclusive count
	sty	LINEOFF			; reset line offset
	ldy	CH_OFFSET		; start Y out
	jsr	PIC2SCR			; move line to screen

	lda	CH_OFFSET		; current offset
	cmp	PICINFO+PLDWID		; did we finish whole line?
	bcs	COPYPIC1		; yup, so wrap things up
	adc	SCRCX			; point to next start
	sta	SCRCX			; save it
	bne	CPICLOOP		; and continue with line
COPYPIC1:
	lda	J+HI			; get previous line
 	pha				; save addr
	lda	K+HI			; make current line be previous
	sta	J+HI
	pla				; get old previous line back
	sta	K+HI			; make it be new current line
	lda	J+LO			; get previous line
	pha				; and save it
	lda	K+LO
	sta	J+LO			; okay, it is
	pla	
	sta	K+LO			; and save new one
	lda	#0			; pic offset is zero
	sta	P_LOFF			; done
	pla				; get start X back
	sta	SCRCX			; restored
	pla				; get X reg back
	tax
	rts	

PIC2SCR:
	lda	#4			; 4 bits per pixel
	sta	ARG7			; use zero page
	lda	(K),Y			; get data byte
	cmp	TRANSCLR		; is it transparent?
	bne	CPYPL1			; nope
P2LOOP:
	iny				; next pixel
	cpy	PICINFO+PLDWID		; done with line?
	bcs	P2LP1			; yup
	lda	(K),Y			; get it
	cmp	TRANSCLR		; still transparent?
	beq	P2LOOP			; ayyup
P2LP1:
	sty	CH_OFFSET		; show where we ended up
      	rts
P2SLOOP:
	lda	(K),Y			; get data byte
	cmp	TRANSCLR		; is it transparent?
	beq	CPYSL1X			; yes, so done with this
	ldy	#4			; 4 bits per pixel
	sty	ARG7			; use zero page
CPYPL1:
	asl	A			; shift to upper nybble
	asl	A			; shift to upper nybble
	asl	A			; shift to upper nybble
	asl	A			; shift to upper nybble
CPYSL:
	asl	A			; shift into carry
	ror	SCR_DATA		; pick up carry
	dex				; count it
	bne	CPYSL1			; shift again, please
CPYSLST:
	pha				; save byte
	lda	SCR_DATA		; get screen  byte
	lsr	A			; move down to last 7 bits
	inc	LINEOFF			; point to interesting byte
	ldy	LINEOFF			; get offset
	sta	SCR_LINE,Y		; save into mem
	ldx	#7			; and start bitoff at 7
	lda	#0			; clear a few locals
	sta	SCR_DATA		; screen data
	pla				; get byte back
CPYSL1:
	dec	ARG7			; count this bit
	bmi	CPYSLX			; all done
	bne	CPYSL			; not done with 'pixel' yet
	inc	CH_OFFSET		; point to next one
	ldy	CH_OFFSET		; next pixel
	cpy	PICINFO+PLDWID		; finished with line yet?
	beq 	CPYSL1X			; yes, so move to screen
	dec	DATALOOP		; reached right edge of screen?
	bne	P2SLOOP			; do next pixel
CPYSL1X:
	lda	BIT_BITS,X		; get me number of significant bits in this byte
	sta	ENDBIT			; show end bit
	cpx	#7			; is there a last byte to use?
	beq	CPYSLX			; no last byte
	lda	SCR_DATA		; get screen data
CPYLSH:
	lsr	A			; put down low, please
	dex				; one more
	bne	CPYLSH			; and do it again
	sta	SCR_DATA		; save for later use
	beq	CPYSLST			; now save this char
CPYSLX:
	jmp	COPY_SCRL1		; copy to screen

NEXT_PICB:
	pha				; save a please

	dec	P_BCNT			; done all four pages?
	bne	NXTPX1			; nope

	lda	# HIGH IOBUFF		; reset data buffer address
	sta	L+HI
	lda	# LOW IOBUFF
	sta	L+LO
	lda	#4			; 4 pages read in at once
	sta	P_BCNT			; okay
	READ	PFILE_RD		; read in 1kb worth of data
	bcc	NXTPX			; everything went just fine
	jsr	DISK_ERR		; print out an error
	bcs	NXTPXR			; and die
NXTPX1:
	inc	L+HI			; point to next page
NXTPX:
	clc				; make sure carry is clear
NXTPXR:
	ldy	#0			; start reading at beginnig of block
	pla				; get [A] back
	rts				; done

GET_PDATA:
	ldy	#SGTPICOF		; point to picture offset
	lda	(DSEGS),Y		; get MSB
	sta	PFSEEK+SM_FPOS+2	; Byte 2
	iny				; point to LSB
	ora	(DSEGS),Y		; is there any pic data?
	bne	GTPD00			; yes
	rts
GTPD00:
	lda	(DSEGS),Y		; get it for shifting
	asl	A			; *2
	sta	PFSEEK+SM_FPOS+1	; stash away
	rol	PFSEEK+SM_FPOS+2	; pick up carry
	lda	#0			; clear out MSB
	sta	PFILE_RD+RD_BUFFLEN+HI
	sta	PFSEEK+SM_FPOS		; and LSB of seeking

	SET_MARK PFSEEK			; and go to beginning

	lda	#4			; read in 4 256 blocks
	sta	PFILE_RD+RD_BUFFLEN+HI	
	READ	PFILE_RD		; read in first block
	bcc	GTPD1			; everything is okey
	jmp	DISK_ERR		; print out problem if not
GTPD1:
	lda	IOBUFF+PHNLD		; get number of pictures
	sta	NUMPIC+HI		; save MSB
	sta	PICCNT+HI		; save for count down too
	lda	IOBUFF+PHNLD+1		; and here is LSB
	sta	NUMPIC+LO		; okay, we got it
	sta	PICCNT+LO		; saving for counting
		
	sta	ALTZP+AUX		; use aux mem for most of this
        lda     #0                      ; clear a few things
	sta	J+LO			; J = source
	sta	K+LO			; K = destination
	lda	# HIGH PIC_DIR		; start destination off here
	sta	K+HI			; MSB of destination
	lda	#4			; 4 * (4*256) bytes max
	sta	PFILE_RD+RD_BUFFLEN+HI	; show the read too
	sta	P_IDX			; save here for counting
GTPDL:
        ldy     #0                      ; start y
	lda	# HIGH IOBUFF		; get where source is
	sta	J+HI			; MSB of source
	lda	#4			; 4 blocks worth of 256
	sta	P_BCNT			; used as counter
	lda	BNK1SET			; we be using bank 1 @$D000
	lda	BNK1SET			; we be using bank 1 @$D000
GTPDL1:
	lda	(J),Y			; get the data byte
	sta	(K),Y			; store in upper aux
	iny				; point to the next one
	bne	GTPDL1			; and go get it
	
	inc	J+HI			; next block please
	inc	K+HI			; for destination too
	lda	PICCNT+LO		; how many entries?
	sec				; doing sbc
	sbc	#(256/PLDSIZE)		; how many entries in 256 bytes?
	sta	PICCNT+LO		; save result
	bcs	GTPDL2			; no wrapping
	dec	PICCNT+HI		; wrap
	bmi	GTPDLX			; all done if we go negative
GTPDL2:
	ora	PICCNT+HI		; make sure both are not zero
	beq	GTPDLX			; if they are, then we are done
	dec	P_BCNT			; count this block
	bne	GTPDL1			; go fetch next 256 bytes
	sta	ALTZP+MAIN		; swap back to main for read
	READ	PFILE_RD		; go get the next 4Kb worth
	sta	ALTZP+AUX		; and back to aux for working
	dec	P_IDX			; max 4Kb worth of reading
	bne	GTPDL			; and read in more
GTPDLX:
	sta	ALTZP+MAIN		; back to main bank of mem
	lda	BNK2SET			; back to bank 2
	lda	BNK2SET			; yes please
	rts				; all done
GTPFLAG: db 0				; if 1 when opening next file, then
					; this is the next file, so unable to
					; find picture number
GET_PICINF:
	lda	#0			; set flag for double check
	sta	GTPFLAG			; start at zero
GTPLOOP:
	lda	NUMPIC+HI		; get # of entries
	sta	PICCNT+HI		; MSB
	lda	NUMPIC+LO		; and the
	sta	PICCNT+LO		; LSB
	lda	ARG1+LO			; check argument for zero
	ora	ARG1+HI			; is it zero?
	bne	GTPINDX			; jump to look at first block
	clc				; found some pictures
	rts				; if zero, just give count
GTPINDX:
	ldy	ARG1+LO 		; get arg, as we are swapping
	ldx	ARG1+HI 		; to aux mem
	sta	ALTZP+AUX		; look at AUX mem
	lda	BNK1SET 		; bank 1
	lda	BNK1SET 		; bank 1
	sty	ARG1+LO 		; use aux mem zero page
	stx	ARG1+HI 		; for new arg1
	lda	# HIGH PIC_DIR		; start at beginning of block
	sta	MPNTH			; hi part
	lda	# LOW PIC_DIR		; don't forget to start at beginning
	sta	MPNTL			; we are
	lda	#PHSIZE                 ; skip over header
	sta	P_IDX			; this is the counter
GTP3:
	ldy	P_IDX			; get offset
	lda	ARG1+HI			; check against desired one
	cmp	(MPCPNT),Y		; get ID hi byte
	bcc	GTPNOT			; arg1 < than entry, can't be here
	bne	GTP4			; not it, so check next one
	iny				; now to LSB
	lda	(MPCPNT),Y		; get it
	cmp	ARG1+LO			; is it what we want?
	beq	GTPFOUND		; found the picture
	bcs	GTPNOT			; arg1 < entry, cannot be here
GTP4:
	lda	PICCNT+LO		; get LSB
	bne	GTP4D			; no wrap down
	dec	PICCNT+HI		; count down MSB
	bpl	GTP4D			; everything still okay
GTPNOT:
	sta	ALTZP+MAIN		; back to main mem
	lda	BNK2SET			; back to bank 1
	lda	BNK2SET			; yes please
	lda	GTPFLAG			; have we already tried to open another file?
	beq	GTPNOT1			; nope
	DLINE	UNKPIC,			; unknown picture number error
	lda	ARG1+HI			; get MSB
	jsr	HEXNUM			; print it
	lda	ARG1+LO			; and LSB
	jsr 	HEXNUM
	lda	#$14			; bad picture number error
	jmp	ZERROR			; and croak
GTPNOT1:
	inc	GTPFLAG			; show trying next file
	jsr	NEXTPICF		; open other picture file
	jmp	GTPLOOP			; and try again
GTP4D:	
	dec	PICCNT+LO		; count this entry
	lda	P_IDX			; get index
	clc				; ready to add
	adc	#PLDSIZE		; point to next entry
	sta	P_IDX			; save lo part
	bcc	GTP3			; no wrap
	inc	MPNTH			; next block
	bne	GTP3			; do next block
GTPFOUND:
	ldx	#0			; for copying data
	dey				; point to beginning of data
GTPFL:
	lda	(MPCPNT),Y		; get data
	sta	PICINFO,X		; save data
	iny				; point to new entry stuff
	inx				; next x
	cpx	#PLDSIZE		; got it all yet (not ID, tho)?
	bne	GTPFL			; get data
	sta	ALTZP+MAIN		; back to main mem
	lda	BNK2SET			; back to bank 1
	lda	BNK2SET			; yes please
	clc				; show got data
	rts

ZPICNF:
	jsr	GET_PICINF		; find the info
	bcs	ZPICNFX			; not found
;
; being here means we found the picture
;
	lda	ARG2+LO			; lsb is okay
	sta	SPCL			; right
	lda	ARG2+HI			; get page
	jsr	SETPC			; get where it is
	sta	SPCH			; hi
	sty	SPCBNK			; bnak
	lda	ARG1+HI			; get argumnet
	ora	ARG1+LO			; check for zero arg
	bne	ZPIC1			; okay, get picture data
;
; arg1 == 0, just return number of entries
;
	lda	PICCNT+HI		; get MSB
	jsr	STASHB			; and save it
	jsr	NEXTSPC			; point to LSB in table
	lda	PICCNT+LO		; get LSB
	jsr	STASHB			; and save it
	jmp	PREDS			; and show success		
ZPIC1:
	lda	#0			; hieght MSB
	jsr	STASHB			; save it too
	jsr	NEXTSPC			; next byte
	lda	PICINFO+PLDHGHT		; height LSB
	jsr	STASHB			; save it too
	jsr	NEXTSPC			; next byte
	lda	#0			; width first
	jsr	STASHB			; put it there
	jsr	NEXTSPC			; continue
	lda	PICINFO+PLDWID		; LSB of width
	jsr	STASHB			; save it too
	jmp	PREDS
ZPICNFX:
	JMP	PREDF	; SET AS BAD
ZDCLR:
ZPICSET:
	rts				; NOT IMPLEMENTED YET
;
; NEXTPICF - open other picture file
;
NEXTPICF:
	lda	DSEGS+HI		; save current DSEG
	pha
	lda	DSEGS+LO
	pha
	lda	GMREF			; to go back to this one
	pha
	ldx	GAME2NML		; get length of name
	lda	GAME2NM,X		; get last number
	cmp	#'3'			; if 3, then open #4
	bne	NXTPF1			; nope
	lda	D4SEG+LO		; point to segment for part 4
	sta	DSEGS+LO
	lda	D4SEG+HI
	sta	DSEGS+HI
	lda	#3			; this opens file #4
	bne	NXTPF2			; and continue on
NXTPF1:
	lda	D3SEG+LO		; point to segment for part 3
	sta	DSEGS+LO
	lda	D3SEG+HI
	sta	DSEGS+HI
	lda	#2			; and this if for #3
NXTPF2:
	jsr	FETCH_FILE		; and do it
	pla				; get old ref num back
	sta	GMREF			; and for reading
	sta	PSEEK+SM_REFNUM 	; this sets it up for set marking
	pla				; get old DSEG back
	sta	DSEGS+LO
	pla
	sta	DSEGS+HI
	rts

	END
