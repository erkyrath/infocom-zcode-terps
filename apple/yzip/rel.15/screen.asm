	STTL	"--- APPLE ][ SCREEN STUFF ---"
	PAGE

; -----------------
; PRINT CHAR IN [A] AT CV,CH
; -----------------
;
; uses memory register [L]
;
SAVECY	DB	0	; spot to save current screen y pos
SAVECX	DB	0	; spot for x pos

CHAR:
	sta	IOCHAR	; SAVE HERE
	txa		; SAVE [X] AND [Y]
	pha		
	tya		
	pha
	
	lda	IOCHAR		; get it back
	cmp	#BACKSPACE	; is it backspace with erase?
	bne	CHCX		; nope

	jmp	DO_BSPC		; so handle backspace
CHCX:
	cmp	#EOL		; is it <CR>?
	bne	CHCONT		; just let dline handle it
	
	jmp	DO_EOL		; handle <CR>
;
; so save some stuff and do all the necessary figgering
;
CHCONT:
	ldx	SCRCNT		; just stash character
	sta	SCRBUFF,X	; save it
	inc	SCRCNT		; and count it
CH_EXIT:
	pla			; RESTORE [X] AND [Y]
	tay		
	pla		
	tax		
	rts
;
; PUT_NYBBLE - put 4 bits to the screen at SCRCX, SCRCY (the lower 4 bits
;	in [A]).
;
PUT_NYBBLE:
	sta	NY_DATA		; save [A] for mucking with
	jsr	SETPOS		; set up offsets using SCRCX

	lda	#$8		; start at first bit, left to right
	sta	DATALOOP	; and init the counter with it
PNY0:
	ldx	FPCBNK		; which bank
	lda	PAGE2SW,X	; select it
	ldy	#0		; start with zero
	lda	(FPC),Y		; get it
	sta	SCR_DATA	; and save it
PNY1:	
	ldx	BITOFF		; get which interesting bit we want
	lda	GT_BITS,X	; get bit 
	pha			; save bit pictures
	lda	DATALOOP 	; see if we have done all 4 bits in this 'pixel'
	beq	PNY_EXIT 	; toots finis
	lsr	DATALOOP 	; point to next bit
	bit	NY_DATA		; is this bit set?
	bne	PNY_SET		; yes it is, so we will handle different
;
; Bit is clear, so clear appropriate one in the SCR_DATA byte
;
	pla			; get bit picture back
	eor	#$FF		; turn bits all around
	and 	SCR_DATA	; turn off bit in screen data
	jmp	PNY_NEXT	; and continue
PNY_SET:
	pla			; get bit mask back
	ora	SCR_DATA	; turn on bit
PNY_NEXT:
	sta	SCR_DATA	; save result
	inc	BITOFF		; we're looking at next bit
	lda	BITOFF		; now check how we're doing
	cmp	#7		; only doing bits 0-6
	bne	PNY1		; check next bit
;
; we've finished this byte in screen memory, point to next one
;
	lda	#0		; start at zero'th bit
	sta	BITOFF		; thank you
	lda	SCR_DATA	; now stash this one in the screen
	sta	(FPC),Y		; y is still zero; still pointing to bank
	jsr	NEXT_SCR	; make SPC/FPC point to next spot
	jmp	PNY0	; and continue on please
;
; all done, so make sure we stash anything out there
;
PNY_EXIT:
	pla			; get bit mask back to fix stack
	lda	SCR_DATA	; put away the current data byte
	sta	(FPC),Y		; y is still zero; still pointing to bank
	lda	PAGE2SW+MAIN	; back to MAIN page 2
	rts
;
; NEXT_SCR - make FPC/SPC point to next screen byte
;
NEXT_SCR:
;
; if in main bank, when we go to aux bank, we need to look at the next
; 	byte in memory
;
	lda	FPCBNK		; get what bank we're talking to currently
	bne	NXS0		; 0 = main bank, 1 = aux bank
	inc	SPCL	; point to next byte
	inc	FPCL	; and for fetching
	lda	#AUX	; and point to aux bank
	bne	NXS1	; (BRANCH) and continue on
;
; if in aux bank, all we need to do is switch banks
;
NXS0:
	lda	#MAIN	; just make it main bank
NXS1:
	sta	SPCBNK	; for stashing and
	sta	FPCBNK	; fetching
	rts
;
; GET_NYBBLE - get the 4 bits which represent the screen at position
;	SCRCX, SCRCY; return nybble in low 4 bits of [A]
;
GT_BITS: db 1,2,4,8,$10,$20,$40
GET_NYBBLE:
	jsr	SETPOS	; set up offsets using SCRCX

	lda	#0	; clear out my data spot
	sta	NY_DATA	; for mucking with

	lda	#$10	; setting bits 0-3, starting with shift
	sta	DATALOOP ; so init the setter
GNY0:
	ldx	FPCBNK		; which bank
	lda	PAGE2SW,X	; save which bank
	ldy	#0		; zero y
	lda	(FPC),Y		; get byte
	sta	SCR_DATA	; and save it
GNY1:	
	lsr	DATALOOP ; see if we have done all 4 bits in this 'pixel'
	beq	GNY_EXIT ; all done!

	ldx	BITOFF	; get bit offset
	lda	GT_BITS,X	; get bit pattern
;
; now check appropriate bit in Screen Data
;
	and	SCR_DATA ; see if it is set
	beq	GNY3	; no bit set here
;
; bit is set, so set it in my data nybble
;
	lda	DATALOOP ; get bit setter
	ora	NY_DATA	; and put it into my data byte
	sta	NY_DATA	; and save it
GNY3:
	inc	BITOFF	; we're looking at next bit
	lda	BITOFF	; now check how we're doing
	cmp	#7	; only doing bits 0-6
	bne	GNY1	; check next bit
;
; we've finished this byte in screen memory, point to next one
;
	lda	#0	; start at zero'th bit
	sta	BITOFF	; thank you
	jsr	NEXT_SCR	; point to next byte
	jmp	GNY0	; and continue on please
GNY_EXIT:
	lda	PAGE2SW+MAIN	; back to main page 2
	lda	NY_DATA	; get the nybble desired
	rts
;
; SETPOS - get the byte offset and the bit offset from the table using
;		SCRCX
;
;	USES: SCRCX, SCRCY
;	SETS: FPC - pointer to correct screen memory location
;	      BITOFF - bit offset to get to start of byte (N.B. this is left to
;			right count, while in memory it is right to left!)
SETPOS:
	lda	SCRCX		; get the x pos
	tax			; put where we can use it
	lda	XPOSTBL,X	; get byte #
	sta	FPCL		; okay, saved
	lda	XBITTBL,X	; get bitoffset
	sta	BITOFF		; where it wants it

	lda	FPCL		; check for correct bank
	and	#$01		; if odd, then must be main bank
	bne	STP11		; okay, it be odd
	lda	#AUX		; aux bank
	bne	STP22		; jump
STP11:
	lda	#MAIN		; main bank
STP22:
	sta	FPCBNK		; save it
	lda	FPCL		; get for fetch
	lsr	A		; /2 to get correct byte offset
	ldx	SCRCY		; get vertical pos
	clc			; get ready for adding
	adc	BASEL,X	 	; add low part
	sta	FPCL		; save low part
	lda	BASEH,X		; get high part
	sta	FPCH		; save high part
	rts			; done !?
;
; DO_EOL - if this is end of line, check if we are at end of window
; 	and if we are, and it is a scrollable window, scroll.  Then move
;	the cursor to the left margin of said window
;
DO_EOL:
	jsr	DISP_LINE	; make sure line gets out there
	lda	SCRCY		; get current vertical pos
	clc			; add font height
	adc	#FONT_H		; thank you
	pha			; save this position
	adc	#FONT_H		; make sure we have room for characters here
	cmp	SCRBTM		; so check against the bottom
	beq	SAVECV		; no scroll yet
	bcs	SCROLLCITY	; nope, can't use it
SAVECV:
	pla			; get new CV
	sta	SCRCY		; and save it
	bne	DEL1		; JUMP
SCROLLCITY:
	pla			; get bad y pos
	lda	SCRLFLG		; is this a scrolling window?
	beq	DEL1		; nope, just move to left edge
	jsr	SCROLL_UP	; and scroll window
DEL1:
;
; move cursor back to left margin
;
	lda	LEFTMRG		; get left margin
	sta	SCRCX		; and set itpositions
	lda	#1		; show cursor changed (maybe)
	sta	CURSFLG		; okay
	lda	#0		; clear length too
	sta	LENGTH+LO	; no more on line
	sta	LENGTH+HI	; no more on line
	jmp	CH_EXIT		; now finish it up
;
; DO_BSPC - do a backspace, by erasing char just before cursor to
;	background color, and moving cursor back one space
; 	[X] == character to delete
;
SAVECHX: DW 	0

DO_BSPC:
	lda	CHR_X+LO	; figger new CHR_X
	sec			; subtract width
	ldy	FONTFLG		; get which font we be using
	beq	DOBSP0		; must be variable width
	sbc	#MONOFONT_W	; get width then of mono font
	jmp	DOBSP1		; okay, now do add
DOBSP0:
	sbc	CHWID,X		; get width of char to be erased
DOBSP1:
	sta	CHR_X+LO	; show new one
	sta	SAVECHX+LO	; save to restore later
	lda	CHR_X+HI	; and pick up carry
	sbc	#0		; okay, did it
	sta	CHR_X+HI	; save it
	sta	SAVECHX+HI	; okay
	ldy	#SPACE		; get SPACE offset
        lda     FONTFLG         ; monospaced font?
        beq     DOBSP2          ; nope
        lda     #MONOFONT_W     ; make monospaced wide
        bne     DOBSP3          ; fine
DOBSP2:
	lda	CHWID,X		; get width of char
DOBSP3:
	sta	CHWID,Y		; set space to be this wide	
	sty	SCRBUFF		; make space for erasing
	lda	#1		; show one char
	sta	SCRCNT		; assume it used to be zero
	jsr	DISP_LINE	; put that space out
	lda	SAVECHX+LO	; restore old/new x pos
	sta	CHR_X+LO
	lda	SAVECHX+HI
	sta	CHR_X+HI
	jsr	GET_SCRCX	; update SCRCX please

	lda	#SPACE_WIDTH	; restore width of space char
	ldy	#SPACE		; get offset
	sta	CHWID,Y		; okay

	jmp	CH_EXIT		; done
;
; CLS - clear the window, using CLS parameters
;
CLS:
	lda	SCRCX		; save the current X,Y
	sta	SAVECX		; x
	lda	SCRCY
	sta	SAVECY		; saved
	lda	CLSTOP		; get top line
	sta	SCRCY		; and set up a y pos

	jsr	SETMASK		; set up masks and ending point

	lda	CPY_COUNT	; get count
	sta	SH_LOOP		; save count
	lda	CLSLEFT		; get left edge
	sta	SCRCX		; set up as X pos
	ldy	#WINBGND	; get background offset
	lda	(WINDOW),Y	; get background color
	sta	ARG8		; save it here
	lda	CLSWIDTH	; how wide we be?
	bne	CLSOKAY		; not a zero width
	lda	#25		; zero can't do zero width
	jmp	ZERROR		; so die
CLSOKAY:
	cmp	#3		; if skinny, handle special
	bcs	L1L0		; nope
	sta	ARG7		; make this be the counter
	lda	CLSHEIGHT	; just use PUT/GET NYBBLE if only one wide
	bne	CLSOKAY1	; not a zero height either
	lda	#25		; bad error
	jmp	ZERROR
CLSOKAY1:	
	sta	J		; save here
CLS1W:
	lda	ARG8		; get color
	jsr	PUT_NYBBLE	; put it
	inc	SCRCX		; next x
	dec	ARG7		; counter
	bne	CLS1W		; do next X
	lda	CLSLEFT		; get left edge again
	sta	SCRCX		; point to it
	inc	SCRCY		; next line
	lda	CLSWIDTH	; restore width count
	sta	ARG7		; thanks
	dec	J		; count
	bne	CLS1W		; do it
	beq	CLSEXIT		; all done then
L1L0:
	sta	J		; use j as counter
L1L:
	lda	ARG8		; a color please
	jsr	PUT_NYBBLE	; do first line easy
	inc	SCRCX		; next nybble
	dec	J		; done yet?
	bne	L1L		; nope

	lda	CLSLEFT		; where is left column
	sta	SCRCX		; here!
	jsr	SETPOS		; get the address
	jsr	FP2SP		; SPC is source PC, now!
	lda	CLSHEIGHT	; how many lines are we clearing
	bne	CLSOKAY2	; not a zero height either
	lda	#25		; bad error
	jmp	ZERROR
CLSOKAY2:	
	sta	J		; j is counter again
 	dec	J		; already cleared one line
	beq	CLSEXIT		; only one line to do!
CLSLOOP:
        lda     SPCH            ; now, modify COPY_LINE
        sta     CPY_MOD1_SRC+HI
        sta     CPY_MOD2_SRC+HI
        lda     SPCL
        sta     CPY_MOD1_SRC+LO
        sta     CPY_MOD2_SRC+LO     
	inc	SCRCY		; clear this line
	jsr	SETPOS		; where am i?
        lda     FPCH            ; now, modify COPY_LINE
        sta     CPY_MOD1_DST+HI
        sta     CPY_MOD2_DST+HI
        lda     FPCL
        sta     CPY_MOD1_DST+LO
        sta     CPY_MOD2_DST+LO     
	lda	SH_LOOP		; get count
	sta	CPY_COUNT	; and save it
	jsr	COPY_LINE	; did the copy (i hope)
	dec	J		; count it
	bne	CLSLOOP		; next one please
CLSEXIT:
 	lda	SAVECX		; get X,Y back
	sta	SCRCX		; got it
	lda	SAVECY		; now for y
	sta	SCRCY		; restored!

	rts
;
; SETMASK - figger out the start and end masks, as well as setting
;	stuff to point to stopping address.  Use CLSLEFT and CLSWIDTH
;
ST_BITS: db 0,1,3,7,$f,$1f,$3f
SETMASK:
	lda	CLSLEFT		; munge with SCRX
	sta	SCRCX		; okay
	jsr	SETPOS		; get me the bit offset of start

	ldx	BITOFF		; now set up mask
	lda	ST_BITS,X	; get the bit pattern
	sta	STMASK		; save it

	lda	CLSLEFT		; get left edge again
	clc			; add width
	adc	CLSWIDTH	; to get where to stop
	tax			; make index
	stx	SCRCX		; set cur pos
	lda	XPOSTBL,X	; get how many bytes
	ldx	CLSLEFT		; subtract from byte offset of start
	sec			; subbing
	sbc	XPOSTBL,X
	sta	CPY_COUNT	; and save it
	inc	CPY_COUNT	; and make it inclusive count

	jsr	SETPOS		; so get its address

	ldx	BITOFF		; get bit offset
	lda	ST_BITS,X	; get pattern
	sta	ENDMASK		; okay!
	rts
;
; SCROLL_UP - roll the current window by FONT_H lines
;
SCROLL_UP:
	lda 	#FONT_H		; show one line
	sta	SCLLINES	; done
	sta	SCLDIR		; show scroll up with positive number
	ldy	#WINTOP		; get top of screen
	lda	(WINDOW),Y	; got it
	sta	CLSTOP		; saved it
	iny			; point to left edge
	lda	(WINDOW),Y	; get left edge
	sta	CLSLEFT		; save
	iny			; this is now the height
	lda	(WINDOW),Y	; get window height
	sta	CLSHEIGHT	; show me
	iny			; and now for the width
	lda	(WINDOW),Y	; get width
  	sta	CLSWIDTH	; saved it
				; so fall thru
				; and do the scrolling
;	FALLING THRU
;	   |
;	  \ /
;
;
; DO_SCROLL - scroll SCLLINES withing the window as defined by
;	CLSLEFT,TOP,WIDTH, and HEIGHT
;
DO_SCROLL:
	lda	SCRCX		; save the current X,Y
	sta	SAVECX		; x
	lda	SCRCY
	sta	SAVECY		; saved
	lda	SCLLINES	; how many lines?

	ldx	SCLDIR		; which direction?
	bpl	SC1		; >0 means scroll up
;
; scroll down, so we are going to start at the bottom and copy downwards
; until we reach the top
;
; now modify code so we do adds to get destination line and subtracts
; to get new src line
;
	ldy	#$C6		; 'dec' opcode
	bne	SC2		; jump to modify
SC1:
;
; scroll up - start at top and copy upwards until we reach the bottom
;
	ldy	#$E6		; 'inc' opcode
	ldx	SCLLINES	; get # of scrolling lines
SC2:
	sty	SCMOD1		; make inc/dec
	sty	SCMOD2		; either inc or dec
	stx	SCOFF		; save the offset between SRC and DST

	lda	SCLDIR		; which direction?
	bpl	SC3		; must be normal scroll
;
; if scrolling downwards, we need to start source at bottom-offset and
; end when the destination is at top+offset
;
	lda	CLSHEIGHT	; get # of lines in window
	sec			; subbing
	sbc	SCOFF		; subtract offset to get source
	clc			; adding
	adc	CLSTOP		; and get absolute position
	tax			; put in X
	dex			; for 0 offset it
	lda	CLSTOP		; now figger last line to copy
	clc			; adding
	adc	CLSHEIGHT	; get me last line
	tay			; for later storage
	dey			; make 0 based
	bne	SC4		; always jumps
SC3:
;
; if scrolling upwards, start src at top+offset and end when destination
; is at bottom-offset
;
	lda	CLSTOP		; get top
	clc			; adding
	adc	SCOFF		; add in offset
	tax			; for later on
	ldy	CLSTOP		; first destination is top of window
SC4:
	stx	SCRCY		; Starting Source
	sty	SCROLLY		; Starting Destination

	jsr	SETMASK		; set the masks and horizontal endpoint
	lda	CPY_COUNT	; save for future generations
	sta	SH_LOOP		; save it
	lda	CLSHEIGHT	; how high
	sta	DATALOOP	; save as counter
	sec			; for subbing
	sbc	SCOFF		; subtract scrolling amount
	sta	CLSHEIGHT	; this is how many lines to copy
	lda	SCRCY		; save Start Source
	pha
	lda	CLSTOP		; start at Y pos
	sta	SCRCY
	lda	CLSLEFT		; and x pos
	sta	SCRCX		; thank you
	lda	PAGE2SW+MAIN	; make sure main memory page is in there
SCRL1:
	jsr	SETPOS		; set up line pointer
	ldy	SCRCY		; get top
	cpy	#120		; 120 bytes per list
	bcc	SCRLY		; all set
	tya			; for adding
	clc
	adc	#8
	tay
SCRLY:
	lda	FPCH
	sta	SV_PCH,Y
	lda	FPCL
	sta	SV_PCL,Y
	inc	SCRCY		; next line please
	dec	DATALOOP
	bne	SCRL1

	pla			; get StartSource back
	sta	SCRCY
	lda	FPCBNK		; init SPC bank too
	sta	SPCBNK		; fine, we did
SCROLLING:
	ldy	SCRCY		; get source Y for index
	cpy	#120		; jump over junk
	bcc	SCLYY
	tya
	clc
	adc	#8
	tay
SCLYY:
	lda	SV_PCH,Y
	sta	SPCH
        sta     CPY_MOD1_SRC+HI
        sta     CPY_MOD2_SRC+HI
	lda	SV_PCL,Y
        sta     CPY_MOD1_SRC+LO
        sta     CPY_MOD2_SRC+LO     
	sta	SPCL

	ldy	SCROLLY		; get destination Y pos
	cpy	#120		; jump over junk
	bcc	SCLYY1
	tya
	clc
	adc	#8
	tay
SCLYY1:
	lda	SV_PCH,Y
	sta	FPCH
        sta     CPY_MOD1_DST+HI
        sta     CPY_MOD2_DST+HI
	lda	SV_PCL,Y
        sta     CPY_MOD1_DST+LO
        sta     CPY_MOD2_DST+LO     
	sta	FPCL

	lda	SH_LOOP		; get byte counter
	sta	CPY_COUNT	; show copy line
	jsr	COPY_LINE	; and do the copy
SCMOD1:	inc	SCRCY		; inc or dec to get next one
SCMOD2:	inc	SCROLLY		; inc/dec for next destination Y
	dec	CLSHEIGHT	; count down
	bne	SCROLLING	; nope

	ldx	SCLDIR		; scroll up or down?
	bpl	SCLCLR1		; clear a little different
;
; if scrolling down, clear from current Y+1
;	
	lda	SCRCY		; get current source
	sta	CLSTOP		; save in top spot
	inc	CLSTOP		; get inside window
	jmp	SCLCLR2
SCLCLR1:
	lda	SCROLLY		; get last destination
	sta	CLSTOP		; and save it
SCLCLR2:
	lda	SCOFF		; and both clear this many lines
	sta	CLSHEIGHT	; okay?

	lda	SAVECX		; now restore X and Y
	sta	SCRCX		; x
	lda	SAVECY		; and
	sta	SCRCY		; y

	jsr	CLS		; WIDTH and LEFT should be okay still

	rts
;
; CLREOL - clear from current cursor position to end of line by
;	using CLS after changing the window size
;	
CLREOL:
	lda	SCRCX		; and make left be where cursor is
	sta	CLSLEFT
	lda	SCRCY		; and top be where Y is
	sta	CLSTOP
	lda	#FONT_H		; and bottom be bottom of char
	sta	CLSHEIGHT
	ldy	#WINWID		; get width
	lda	(WINDOW),Y	; got it
	clc			; now adding 
	ldy	#WINLEFT	; the left edge
	adc	(WINDOW),Y	; to get the right edge
	sec			; subbing
	sbc	CLSLEFT		; and subtract left to get width
        ldy     #WINRM          ; remove the size of the right margin
        sbc     (WINDOW),Y      ; that too
	sta	CLSWIDTH 	; okay?
	jmp	CLS		; now clear the screen
;
; COPY_SCRL - copy from SCR_LINE onto the screen.  CH_OFFSET says how
;	many bytes in SCR_LINE to copy.  FPC is assumed to be pointing
;	to start of screen bytes.  BITTER has the original BITOFF from
;	SETPOS.  ENDBIT shows how many significant bits in last byte.
;
COPY_SCRL:
	ldx	SHOW_CURSOR	; are we showing the cursor (ugh)
	beq	COPYL2		; nope
	jmp	DO_CURSOR	; then XOR cursor in
COPYL2:
	ldy	#WINBGND	; get background offset
	lda	(WINDOW),Y	; get current background color
	beq	COPY_SCRL1	; if black, ignore this
	jsr	FIX_COLOR	; or in the color, please
;
; Alternate entry point for COPY_SCRL, used by picture drawing routine
; to avoid using background color and checking for cursor
;
COPY_SCRL1:
	ldx	FPCBNK		; set up bank
	lda	PAGE2SW,X	; set first bank 
	lda	ENDBIT		; did we end evenly?
	bne	COPYL22		; nope
	inc	CH_OFFSET		; make CH_OFFSET be a counter
COPYL22:
	ldy	#0		; zero y

	lda	CH_OFFSET		; if only one, then handle real special
	bne	COPYL21		; nope

	ldx	BITTER		; get starting bit offset
	lda	ST_BITS,X	; get starting bit mask
	sta	IOCHAR		; save it
	ldx	ENDBIT		; get ending bit offset
	lda	ST_BITS,X	; combine with ending mask bits
	eor	#$FF		; set for good ones
	ora	IOCHAR		; set up mask for bits
	and	(FPC),Y		; get me interesting bits
	sta	(FPC),Y		; save
	lda	IOCHAR		; get flag for data
	eor	#$FF		; flip to get good data
CPYSM1:	and	SCR_LINE	; pick up my bits
	ora	(FPC),Y		; and or in screen data
	sta	(FPC),Y		; and save it 
	jmp	COPYLEX		; go away now
COPYL21:
	lda	CH_OFFSET		; get # of chars
	lsr	A		; /2 get number in this bank
	adc	#0		; pick up carry for first one
	sta	DATALOOP	; and save it to work on

	ldx	BITTER		; do we have a weird start?
	beq	DLL2		; nope
;
; need to do the first one special
;
	lda	(FPC),Y		; get byte
	and	ST_BITS,X	; get masking bits
	sta	IOCHAR		; save for later
	lda	ST_BITS,X	; get masking bits again
	eor	#$FF		; get good bits
CPYSM2:	and	SCR_LINE	; get first data byte
	ora	IOCHAR		; pick up screen bits
	sta	(FPC),Y		; pointing
	iny			; next byte
	ldx	#2		; start at next one for this bank
	dec 	DATALOOP	; kount as done
COPYLOOP:
	beq	CPYLP2		; all done with this part of line
DLL2:
CPYSM3:	lda	SCR_LINE,X	; get data
	sta	(FPC),Y		; pointing
	iny			; next byte
	inx			; next one
	inx			; next one
	dec	DATALOOP	; count as used
	bpl	COPYLOOP	; start loop again
CPYLP2:
	ldy	#0		; reset Y
	lda	FPCBNK		; which bank were we at
	eor	#1		; swap to other one
	tax			; make it in x
	lda	PAGE2SW,X	; swap to write bank
	txa			; check which one
	beq	CPYLP23		; correct offset
	iny			; fix offset if going from main to aux
CPYLP23:
	ldx	#1		; start at second byte in screen line
	lda	CH_OFFSET		; get starting number
	lsr	A		; /2 to get how many for this bank
	sta	DATALOOP	; and show it
COPYLOOP2:
	beq	LINE_DONE	; all done with this part of line

CPYSM4:	lda	SCR_LINE,X	; get data
	sta	(FPC),Y		; pointing
	iny			; next byte
	inx			; next one
	inx			; next one
	dec	DATALOOP	; count as used
	bpl	COPYLOOP2	; start loop again
LINE_DONE:
;
; do the last byte special
;
	stx	DATALOOP	; save pointer
	lda	ENDBIT		; get ending bit offset
	beq	COPYLEX		; all done if nothing funky
	lsr	CH_OFFSET	; get line offset
	pha			; save [A]
	bcs	LINED1		; if odd, point at right things
	dec	DATALOOP	;  one less then
	ldx	FPCBNK		; get original bank
	lda	PAGE2SW,X	; switch to it
	txa			; which bank did we do?
	bne	LINED1		; no need to step back one
	dey			; point one less here too
LINED1:
	pla			; get ENDBIT back
	tax			; make end bits be index
	lda	ST_BITS,X	; get end mask
	sta	IOCHAR		; save mask bits
	eor	#$FF		; toggle every bit
	and	(FPC),Y		; get the interesting bits in byte
	sta	(FPC),Y		; save it
	ldx	DATALOOP	; get data pointer
CPYSM5:	lda	SCR_LINE,X	; turn on my bits
	and	IOCHAR		; pick up my good bits
	ora	(FPC),Y		; bring everything together
	sta	(FPC),Y		; save the byte
COPYLEX:
	lda	PAGE2SW+MAIN	; back to main, thanks
	rts

FIX_COLOR:
        inc     CH_OFFSET       ; just to make checking easier for now

	asl	A		; *2
	asl	A		; *4 to get correct offset
	adc	#<COLORS	; add start of table
	sta	COLORP+LO	; save for pointer
	lda	#>COLORS	; get hi part
	adc	#0		; pick up maybe carry
	sta	COLORP+HI	; save pointer
        ldx     #$11            ; ORA (),Y opcode
        ldy     #WINFORE        ; check foreground color
        lda     (WINDOW),Y      ; check what color
        bne     FIXC1           ; just leave white if not black
        ldx     #0              ; start at beginning
FIXMUNGEL:
        lda     SCR_LINE,X      ; get byte
        eor     #$FF            ; swap all the bits
        sta     SCR_LINE,X      ; store back
        inx                     ; count it
        cpx     CH_OFFSET       ; done?
        bcc     FIXMUNGEL       ; nope
        ldx     #$31            ; AND (),Y opcode
FIXC1:
        stx     FCLM            ; self mod my code (again . . . sigh)
	lda	BITTER		; get starting offset
	and	#3		; pick up wrap
	tay			; make index
	ldx	#0		; start at zero line
FIXLOOP:
FCM1:	lda	SCR_LINE,X	; get screen byte
FCLM:	ora	(COLORP),Y	; or with color byte
FCM2:	sta	SCR_LINE,X	; save it
	iny			; next color byte
	cpy	#4		; wrapped?
	bne	FIXLP1		; nope
	ldy	#0		; restart
FIXLP1:
	inx			; next screen byte
	cpx	CH_OFFSET	; done yet?
        bcc     FIXLOOP         ; nope
        
        dec     CH_OFFSET       ; fix counter
	rts			; now we are done
;
; MSCURS - Erase the cursor from the old spot and write it in the
;  new spot
;
MOLDX:	db	MAXWIDTH	; spot to save x,y of mouse
MOLDY:	db	MAXHEIGHT
MSCCNT:	ds	1
SVCURS:	ds	CURSW*CURSH	; save room
MSWCNT:	db	1
MSSTATE: db	0		; 0 == off, 1 == on
MSCOLOR: db	$f,7,7,7,7,7,7,7
;
MSCURS:
	lda	#0		; clear moved flag
	sta	MSMOVEF		; we did
	jsr	MSCOFF		; turn cursor off at current pos
	jmp	MSCON		; and turn it on in new spot

;
; MSCOFF - mouse cursor off
;	Copy info from saved spot back onto screen @ MOLDX, MOLDY
MSCOFF:
        lda     MSSTATE         ; already off?
        bne     MSCOFF1         ; no
        rts                     ; done then
MSCOFF1:
	lda	SCRCY		; save X,Y
	sta	SAVECY
	lda	SCRCX
	sta	SAVECX
	lda	MOLDX		; point to old X,Y
	sta	SCRCX
	lda	MOLDY
	sta	SCRCY
	lda	#0		; start at beginning of saved area
	sta	MSCCNT		; save counter
	lda	#CURSW		; restore width
	sta	MSWCNT		; and counter
RENEWL:
	lda	SCRCY		; check bounds
	cmp	#MAXHEIGHT
	bcs	MSC1		; all done
RENEWL0:
	lda	SCRCX
	cmp	#MAXWIDTH
	bcs	RENEWL1		; might not be done
        jsr     GET_NYBBLE      ; get screen nybble
	ldx	MSCCNT		; get counter
;        eor     MSCOLOR,X       ; xor to turn off
        eor     #$0F            ; turn white/black
	jsr	PUT_NYBBLE	; save nybble
	inc	MSCCNT		; point to next one
	lda	MSCCNT		; get counter
	cmp	#(CURSW*CURSH)	; done yet?
	beq	MSC1		; yup
	dec	MSWCNT		; count down one width
	beq	RENEWL1		; it is
	inc	SCRCX		; next x
	bne	RENEWL0		; try again
RENEWL1:
	lda	MOLDX		; reset X
	sta	SCRCX
	lda	#CURSW		; restore width
	sta	MSWCNT		; and counter
	inc	SCRCY		; next y
	bne	RENEWL		; okay try some more
MSC1:
	jmp 	MSCON4		; done turning it off
;
; MSCON - turn mouse cursor on
;	Copy white pixels into area specified by MSX, MSY
;
MSCON:
        lda     MSSTATE         ; already on?
        beq     MSCON0          ; no
        rts                     ; don't bother
MSCON0:
	lda	SCRCY		; save X,Y
	sta	SAVECY
	lda	SCRCX
	sta	SAVECX

	lda	MSX		; get mouse x
	sta	SCRCX		; set mouse X
	sta	MOLDX		; save mouse x
	lda	MSY		; get mouse y
	sta	SCRCY		; set mouse Y
	sta	MOLDY		; and mouse Y
	lda	#0		; start at beginning of saved area
	sta	MSCCNT		; save counter
	lda	#CURSW		; restore width
	sta	MSWCNT		; and counter
MSCON1:
	lda	SCRCY		; check bounds
	cmp	#MAXHEIGHT
	bcs	MSCON4		; all done
MSCON2:
	lda	SCRCX
	cmp	#MAXWIDTH
	bcs	MSCON3		; might not be done
	jsr	GET_NYBBLE	; save nybble
 	ldx	MSCCNT		; get counter
;	eor	MSCOLOR,X	; get cursor color
        eor     #$0F            ; turn white/black
	jsr	PUT_NYBBLE	; put nybble
	inc	MSCCNT		; point to next one
	lda	MSCCNT		; check ending
	cmp	#(CURSW*CURSH)	; done yet?
	beq	MSCON4		; yup
	dec	MSWCNT		; count down one width
	beq	MSCON3		; finished for line
	inc	SCRCX		; next x
	bne	MSCON2		; try again
MSCON3:
	lda	MSX		; reset X
	sta	SCRCX
	lda	#CURSW		; restore width
	sta	MSWCNT		; and counter
	inc	SCRCY		; next y
	bne	MSCON1		; okay try some more
MSCON4:
        lda     MSSTATE         ; get current state
        eor     #$01            ; toggle it
        sta     MSSTATE         ; bang
	lda	SAVECX		; restore X,Y
	sta	SCRCX
	lda	SAVECY
	sta	SCRCY
	rts	
	

	END
