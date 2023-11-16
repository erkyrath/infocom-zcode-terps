	STTL "--- Picture Op Codes ---"
	PAGE
PFILE_RD:			; for READING files
	db	4		; 4 parameters
	db	0		; refnum
	dw	IOBUFF		; read into io buffer
	dw	$400		; 1Kb buffer
	dw	0		; length actually read
PFSEEK:
	db	2	        ; 2 pararmeters
	db	0	        ; refnum
	db 	0,0,0	        ; 3 byte new file pos
PICCNT: dw	0	        ; counter of the entries
PF_REFNUM: db     0             ; place to store refnum
PF_FID: db      $FF             ; File ID of local directory data
P_IDX:	db 	0	
P_BCNT:	db	0
P_LOFF:	db	0
GBL_FLAG: db    0               ; found global directory?
PIC_FLAG: db    0               ; found pictures?
PF_NAME: db     0                       ; name we are looking at
        db      0                       ; name we found

PICINFO:
	ds	PLDSIZE ; get room for picture data

TRANSCLR: DB	0	; transparent color
UNKPIC:

	db	EOL,"Unknown Picture #"
UNKPICL	EQU	$-UNKPIC
ZDISPL:
	jsr	CLRBUF			; put out any and all text

	jsr	GET_PICINF		; get the necessary data
	bcc	ZDSP1			; everything just fine

	DLINE	UNKPIC			; unknown picture number error
	lda	ARG1+HI			; get MSB
	jsr	HEXNUM			; print it
	lda	ARG1+LO			; and LSB
	jsr 	HEXNUM
	lda	#$14			; bad picture number error
	jmp	ZERROR			; and croak
ZDSP1:
	jsr	SAVE_CURSOR		; save the cursor

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
	dec	NARGS			; x pos passed?
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
	lda	SCRCX			; get current X
	pha				; save it
	clc				; adding
	adc	PICINFO+PLDWID		; add in pic width
	sta	SCRCX			; save here
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

	SET_MARK PFSEEK			; go to pic data
	lda	#>PIC1BUF		; set up pointers to decode buffers
	sta	J+HI
	lda	#<PIC1BUF
	sta	J+LO
	lda	#>PIC2BUF
	sta	K+HI
	lda	#<PIC2BUF
	sta	K+LO
	lda	#0			; start line at zero
	sta	P_LOFF			; done
	ldy	#MAXWIDTH-1		; clear out width buffer
ZDLI:
	sta	(J),Y			; init 'previous line' buffer to zero
	dey				; down one
	bne	ZDLI
	sta	(J),Y			; get last one

	lda	#>IOBUFF		; now get data buffer address
	sta	L+HI
	lda	#4			; 4 pages read in at once
	sta	P_BCNT			; okay
	lda	#3			; 3 bytes of width data start it
        clc                             ; doing add
	adc	PICINFO+PLDPTR+2	; pick up LSB pointer
	sta	P_IDX			; start index
        bcc     ZDLPCC                  ; no wrap
        inc     L+HI                    ; start at second one
        dec     P_BCNT                  ; one less block
ZDLPCC:
	lda	PICINFO+PLDPTR+1        ; tells which block it is in               
        and     #$01                    ; pick up which 256 block
        beq     ZDLP0                   ; start at first one
        inc     L+HI                    ; start at next one
        dec     P_BCNT                  ; one less block
ZDLP0:
	lda	#<IOBUFF
	sta	L+LO
        lda     #RETRY_COUNT    ; set retry
        sta     NY_DATA         ; just use this for now
ZDLPRD:
	READ	PFILE_RD		; read in 1kb worth of data
	bcc	ZDECLP			; everything went just fine
        jsr     RETRY                   ; must we?
        bcc     ZDLPRD                  ; yes
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
	sta	NY_DATA 		; and into data buffer
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
	sty	CH_OFFSET		; reset line offset
	ldy	NY_DATA 		; start Y out
	jsr	PIC2SCR			; move line to screen

	ldy	NY_DATA 		; current offset
	cpy	PICINFO+PLDWID		; did we finish whole line?
	bcs	COPYPIC1		; yup, so wrap things up
	bcc	CPICLOOP		; and continue with line
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
        inc     SCRCX                   ; point to next pixel
	iny				; next pixel
	cpy	PICINFO+PLDWID		; done with line?
	bcs	P2LP1			; yup
	lda	(K),Y			; get it
	cmp	TRANSCLR		; still transparent?
	beq	P2LOOP			; ayyup
P2LP1:
	sty	NY_DATA 		; show where we ended up
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
	inc	CH_OFFSET			; point to interesting byte
	ldy	CH_OFFSET			; get offset
	sta	SCR_LINE,Y		; save into mem
	ldx	#7			; and start bitoff at 7
	lda	#0			; clear a few locals
	sta	SCR_DATA		; screen data
	pla				; get byte back
CPYSL1:
	dec	ARG7			; count this bit
	bmi	CPYSLX			; all done
	bne	CPYSL			; not done with 'pixel' yet
	inc	NY_DATA 		; point to next one
        inc     SCRCX                   ; point to next pixel
	ldy	NY_DATA 		; next pixel
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

        lda     #RETRY_COUNT            ; set retry
        sta     RETRIES                 ; just use this for now

	lda	#>IOBUFF		; reset data buffer address
	sta	L+HI
	lda	#<IOBUFF
	sta	L+LO
	lda	#4			; 4 pages read in at once
	sta	P_BCNT			; okay
NXTPRD:
	READ	PFILE_RD		; read in 1kb worth of data
	bcc	NXTPX			; everything went just fine
        jsr     RETRY                   ; try again?
	bcc     NXTPRD                  ; okay, do it again
NXTPX1:
	inc	L+HI			; point to next page
NXTPX:
	clc				; make sure carry is clear
NXTPXR:
	ldy	#0			; start reading at beginnig of block
	pla				; get [A] back
	rts				; done
;
; GET_PDATA - go out and check the just opened file to see what kind,
;       if any, of picture data it has.  
;  [A] = REFNUM of the freshly opened file
;

GET_PDATA:
        sta     PF_REFNUM               ; save refnum for later usage
        lda     DSEGS+HI                ; save old one
        pha
        lda     DSEGS+LO
        pha
        ldy     SAVENUM                 ; which file are we talking about
        dey                             ; zero base it
        tya
        and     #$0F                    ; make sure DSEG is set
        asl     A                       ; *2 for words
        tay                             ; make it  index again
        lda     DSKSEG+HI,Y             ; get MSB
        sta     DSEGS+HI
        lda     DSKSEG+LO,Y             ; and LSB
        sta     DSEGS+LO
        jsr     GO_GET_PDATA            ; actually do the work
        pla                             ; get old DSEG back
        sta     DSEGS+LO     
        pla
        sta     DSEGS+HI
GETPX:
        rts                             ; bye
GO_GET_PDATA:
        lda     GBL_FLAG                ; is there already a global dir?
        bne     GTPG1                   ; yes, so don't check again
        ldy     #SGTGPOF                ; first, check for global directory
	lda	#>GBL_DIR		; start destination off here
        jsr     READ_IN_PDATA           ; go get global directory, if any
        sta     GBL_FLAG                ; indicate whether there is a global
GTPG1:
        lda     SAVENUM                 ; get which file is being opened
        and     #$0F                    ; just make it binary
        cmp     PF_FID                  ; same as the file we already have?
        beq     GETPGX                  ; yes, so don't bother
	ldy	#SGTPICOF		; point to picture offset
	lda	#>PIC_DIR		; start destination off here
        jsr     READ_IN_PDATA           ; go get the data for local pictures
        sta     PIC_FLAG                ; any picture data
GETPGX:
        rts
;
; READ_IN_PDATA - check for desired data being in file, and skip to it if
;       it is.  Then, read it in in 1Kb chunks, stashing it away into the
;       upper bank of the aux mem language card.
; Enter with:
;   [A] = Destination page
;   [Y] = Offset into Segment table 
;
READ_IN_PDATA:
        pha                             ; save destination page
	lda	(DSEGS),Y		; get MSB
	sta	PFSEEK+SM_FPOS+2	; Byte 2
	iny				; point to LSB
	ora	(DSEGS),Y		; is there any pic data?
	bne	GTPD00			; yes
        pla                             ; get page back
        lda     #0                      ; show not here
        rts                             ; nope
GTPD00:
	lda	(DSEGS),Y		; get it for shifting
	asl	A			; *2
	sta	PFSEEK+SM_FPOS+1	; stash away
	rol	PFSEEK+SM_FPOS+2	; pick up carry
	lda	#0			; clear out MSB
	sta	PFILE_RD+RD_BUFFLEN+HI
	sta	PFSEEK+SM_FPOS		; and LSB of seeking

        lda     PF_REFNUM               ; get file ref number
	sta	PFILE_RD+RD_REFNUM      ; and save it for reading and
	sta	PFSEEK+SM_REFNUM	; for seeking

        lda     #RETRY_COUNT    ; set retry
        sta     NY_DATA         ; just use this for now
GTPDRD0:
	SET_MARK PFSEEK			; and go to beginning

	lda	#4			; read in 4 256 blocks
	sta	PFILE_RD+RD_BUFFLEN+HI	
	READ	PFILE_RD		; go get the next 4Kb worth
        bcc     GTPD1                   ; fine
        jsr     RETRY                   ; well, do it again?
        bcc     GTPDRD0                 ; yes
GTPD1:
        pla                             ; get destination page back
        cmp     #>PIC_DIR               ; if doing local directory, set up
        bne     GTPD2                   ;  picture count
	ldx	IOBUFF+PHNLD		; get number of pictures
	stx	PICCNT+HI		; save for count down too
	ldx	IOBUFF+PHNLD+1		; and here is LSB
	stx	PICCNT+LO		; saving for counting
        ldx     IOBUFF+PHFID            ; get file ID
        stx     PF_FID                  ; and save for later
        ldx     #(256/PLDSIZE)          ; how many entries in 256 bytes
        bne     GTPD3                   ; done
GTPD2:
        ldx     IOBUFF                  ; get MSB of how many
        stx     PICCNT+HI               ; show me
        ldx     IOBUFF+1                ; get LSB of count
        stx     PICCNT+LO
        ldx     #(256/2)                ; how many entries / 256 bytes
GTPD3:
        stx     GTPMOD+1                ; set up how big entry is
	sta	ALTZP+AUX		; use aux mem for most of this
	sta	K+HI			; MSB of destination
        lda     #0                      ; clear a few things
	sta	J+LO			; J = source
	sta	K+LO			; K = destination
	lda	#4			; 4 * (4*256) bytes max
	sta	PFILE_RD+RD_BUFFLEN+HI	; show the read too
	sta	P_IDX			; save here for counting
GTPDL:
        ldy     #0                      ; start y
	lda	#>IOBUFF		; get where source is
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
GTPMOD:	sbc	#(256/PLDSIZE)		; how many entries in 256 bytes?
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

        lda     #RETRY_COUNT    ; set retry
        sta     NY_DATA         ; just use this for now
GTPDRD:
	READ	PFILE_RD		; go get the next 4Kb worth
        bcc     GTPDRDG                 ; fine
        jsr     RETRY                   ; well, do it again?
        bcc     GTPDRD                  ; yes
GTPDRDG:
	sta	ALTZP+AUX		; and back to aux for working
	dec	P_IDX			; max 4Kb worth of reading
	bne	GTPDL			; and read in more
GTPDLX:
	sta	ALTZP+MAIN		; back to main bank of mem
	lda	BNK2SET			; back to bank 2
	lda	BNK2SET			; yes please
        lda     #1                      ; show we found it
	rts				; all done

GTPFLAG: db 0				; if 1 when opening next file, then
					; this is the next file, so unable to
					; find picture number
GET_PICINF:
	lda	#0			; set flag for double check
	sta	GTPFLAG			; start at zero
GTPLOOP:
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
        lda     PIC_DIR+PHNLD           ; get # of entries
        sta     PICCNT+HI
        lda     PIC_DIR+PHNLD+1         ; it's in reverse order
        sta     PICCNT+LO
	sty	ARG1+LO 		; use aux mem zero page
	stx	ARG1+HI 		; for new arg1
	lda	#>PIC_DIR		; start at beginning of block
	sta	MPNTH			; hi part
	lda	#<PIC_DIR		; don't forget to start at beginning
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
        jsr     FIND_GBL                ; find picture in global dir
        bcc     GTPLOOP                 ; go try again
        rts                             ; just show no goodness
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
	lda	BNK2SET			; back to bank 2
	lda	BNK2SET			; yes please
	clc				; show got data
	rts
;
; FIND_GBL - check the global directory for the picture in question.  If there
;       isn't a global directory, just open the other file.  Return with carry 
;       set if either not in global directory, or the other file has already
;       been opened.
;
FIND_GBL:
	lda	GTPFLAG			; have we already tried to open another file?
	inc	GTPFLAG			; show trying next file
        tay                             ; check flag
	bne     GTPNOT1                 ; we already should have found it
        lda     GBL_FLAG                ; do we have global dir?
        bne     FG1                     ; yes, so look for picture there
;
; otherwise, just check the other file
;
	jsr	NEXTPICF		; open other picture file
        clc
        rts
GTPNOT1:
        sec                             ; show badness
        rts
;
; go find the picture in the global directory file
;
FG1:
	ldy	ARG1+LO 		; get arg, as we are swapping
	ldx	ARG1+HI 		; to aux mem
	sta	ALTZP+AUX		; look at AUX mem
	lda	BNK1SET 		; bank 1
	lda	BNK1SET 		; bank 1 is where the directory is
        lda     GBL_DIR                 ; MSB of count
        sta     PICCNT+HI
        lda     GBL_DIR+1               ; LSB of count
        sta     PICCNT+LO               
        sty     ARG1+LO                 ; restore args
        stx     ARG1+HI         
	lda	#>GBL_DIR		; start at beginning of block
	sta	MPNTH			; hi part
	lda	#<GBL_DIR		; don't forget to start at beginning
	sta	MPNTL			; we are
        lda     #2                      ; start at 2nd word, skipping
        sta     P_IDX                   ; the count byte
FGLOOP:
        inc     P_IDX                   ; point to LSB
	ldy	P_IDX			; get offset
        lda     (MPCPNT),Y              ; get global lo byte
        cmp     ARG1+LO                 ; same as desired one?
        bne     FGNOT                   ; nope
        dey                             ; point to ID MSB
        lda     (MPCPNT),Y              ; get global MSB
        and     #$03                    ; lower two bits are upper ID part
        cmp     ARG1+HI                 ; same as desired one?
        bne     FGNOT                   ; nope
        lda     (MPCPNT),Y              ; get disk map
        tay                             ; save in Y
        clc                             ; ready for return
        bcc     FTNX                    ; and done
FGNOT:
        lda     PICCNT+LO               ; wrap?
        bne     FTN1                    ; nope
        lda     PICCNT+HI               ; already zero?
        beq     FTNOTFOUND              ; sorry, it ain't anywhere
        dec     PICCNT+HI               ; count down the wrap
FTN1:
        dec     PICCNT+LO               ; count down towards the end
        inc     P_IDX                   ; point to next entry
        bne     FGLOOP                  ; no wrap to next page
        inc     MPNTH                   ; point to next page
        bne     FGLOOP                  ; and continue
FTNOTFOUND:
        sec                             ; show not found
FTNX:
	sta	ALTZP+MAIN		; back to main mem
	lda	BNK2SET			; back to bank 2
	lda	BNK2SET			; yes please
        bcc     FTNXX                   ; go find it then
        rts                             ; done
FTNXX:
;
; FIND_PICDISK - we have figgered out that the pictures exists somewhere,
;       so parse the passed disk number and go get that disk.
;       First, check to make sure the disk isn't just the other one that
;       is in there.  If it isn't, just use the last one that it is on.
;
;  [Y] = bit 7-2 is the 6 bit disk map
;
FIND_PICDISK:
        tya                             ; get into more useful register        
        lsr     A                       ; bring down to lower part
        lsr     A                       ; all set up
        sta     P_IDX                   ; save for later looking
        ldx     #'1'                    ; begin at file 1
	stx	PF_NAME+1		; both places
	inx				; start counter off at 2
        stx     PF_NAME                 ; and save it
        ldx     #6                      ; 6 possible disks
FNDPL:
        ror     P_IDX                   ; put bit into carry
        bcc     FNDPL1                  ; not here
        lda     PF_NAME                 ; get name we are looking at
        sta     PF_NAME+1               ; save found name
        ldy     GAME1NML                ; get name length
        cmp     GAME1NM,Y               ; compare to last char
        beq     FNDP1                   ; found it
        ldy     GAME2NML                ; how 'bout in disk 2?
        cmp     GAME2NM,Y               ; well?
        beq     FNDP1                   ; yes, it's already in there
FNDPL1:
        inc     PF_NAME                 ; next name
        dex                             ; count down
        bne     FNDPL                   ; still more to look at
FNDP1:
        lda     PF_NAME+1               ; get file name
        and     #$07                    ; make it non-ascii
        tay                             ; put in Y for routine
        dey                             ; and make it zero relative
        jmp     OPEN_NEW_PF             ; go get it
;
; ZPICNF - YZIP call to get just the picture info
;
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
	sta	ALTZP+AUX		; look at AUX mem
	lda	BNK1SET 		; bank 1
	lda	BNK1SET 		; bank 1
        lda     PIC_DIR+PHFVERS		; get version number
	sta	P_IDX			; just save here
        lda     PIC_DIR+PHFVERS+1	; now for the LSB
	sta	P_BCNT			; and here
	ldy	PIC_DIR+PHCHKS		; MSB of checksum
	lda	PIC_DIR+PHCHKS+1	; and LSB
	sta	ALTZP+MAIN		; back to main mem
	ldx	BNK2SET			; back to bank 2
	ldx	BNK2SET			; yes please
        pha                             ; save LSB
        tya                             ; put MSB into [A]        
	jsr	STASHB			; and save it
	jsr	NEXTSPC			; point to LSB in table
        pla                             ; get LSB back
	jsr	STASHB			; and save it
	jsr	NEXTSPC			; point to Version number
	lda	P_BCNT			; now for version
	jsr	STASHB			; give it to game
	jsr	NEXTSPC			; now for LSB
	lda	P_IDX			; this is where it is
	jsr	STASHB			; and away it goes
	jmp	PREDS			; and show success		
ZPIC1:
	lda	#0			; height MSB
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
; NEXTPICF - Just toggle between files 3/4 for those slim and trim games
;       that have pictures only on those 2 disks.
;
NEXTPICF:
        ldy     #3                      ; pretend we want file #4
	ldx	GAME2NML		; get length of name
	lda	GAME2NM,X		; get last number
	cmp	#'3'			; if 3, then open #4
	beq	NXTPF1			; yes
        dey                             ; no, so point to 3 then
NXTPF1:
        jmp     OPEN_NEW_PF             ; and open the new pic file
;
; OPEN_NEW_PF - open the file so we can pick up new picture data
;
;   [Y] - # of file to be pointed to (0-n)
;
OPEN_NEW_PF:
	lda	DSEGS+HI		; save current DSEG
	pha
	lda	DSEGS+LO
	pha
	lda	GMREF			; to go back to this one
	pha
;
; now, set up DSEGS to point to correct segment table
;
        tya                             ; get to muck
	asl	A		        ; make word index
	tax			        ; make it an index
	lda	DSKSEG+LO,X	        ; get LSB
	sta	DSEGS+LO
	lda	DSKSEG+HI,X             ; and MSB
	sta	DSEGS+HI
        tya                             ; point to the new file
	jsr	FETCH_FILE		; and do it
        sty     PF_REFNUM               ; save refnum, please
        jsr     GO_GET_PDATA            ; just make sure pic data is gotten    
OPENPF1:
	pla				; get old ref num back
	sta	GMREF			; and for reading
	sta	PSEEK+SM_REFNUM 	; this sets it up for set marking
	pla				; get old DSEG back
	sta	DSEGS+LO
	pla
	sta	DSEGS+HI
        clc                             ; show it worked
	rts

	END
