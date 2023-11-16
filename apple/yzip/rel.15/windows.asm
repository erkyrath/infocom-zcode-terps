
	STTL	"--- WINDOW OPERATIONS ---"
	PAGE
;
; these are the data structures for all 8 WINDOW.  They are identical and
; are just seperated cuz I use the addresses in the table that follows.
;	All numbers are inclusive, absolute, and zero-based.
;
WINDSTRUCT	EQU	0
WINTOP 		EQU	WINDSTRUCT	; first line of the window
WINLEFT 	EQU	WINTOP+1	; left edge of the window
WINHGHT		EQU	WINLEFT+1	; height of the window
WINWID		EQU	WINHGHT+1	; width of the window, in pixels
WINY		EQU	WINWID+1	; y pos of cursor (pos, not relative)
WINX		EQU	WINY+1		; x pos of cursor (remember, ABSOLUTE)
WINLM		EQU	WINX+1		; left margin
WINRM		EQU	WINLM+1		; right margin
WINCRF		EQU	WINRM+1		; (WORD) <CR> function
WINCRC		EQU	WINCRF+2	; (WORD) <CR> counter
WINHIGHL	EQU	WINCRC+2	; highlight mode
WINFORE		EQU	WINHIGHL+1	; foreground color (0=black-7=white)
WINBGND		EQU	WINFORE+1	; background color (0=black-7=white)
WINFONT		EQU	WINBGND+1	; window font (0=normal/4=monospaced)
WINFSIZE	EQU	WINFONT+1	; (WORD) font Height/Width
WINATR		EQU	WINFSIZE+2	; Window Attributes
WINLCNT		EQU	WINATR+1	; current number of lines
;
; these are my interesting things
;
WINXSZ		EQU	WINLCNT+1	; width of window, in pixels, using margin
WINLLEN		EQU	WINXSZ+1	; length of current line
WINLINES	EQU	WINLLEN+2	; max # of lines for window

WINDOW0:
	DB	0		; WINTOP - first line of the window
	DB	0		; WINLEFT - left edge of the window
	DB	MAXHEIGHT	; WINHGHT - height of window
	DB	MAXWIDTH	; WINWID - width of window
	DB	0		; WINY - y pos of cursor (pos, not relative)
	DB	0		; WINX - x pos of cursor (remember, ABSOLUTE)
	DB	0		; WINLM - left margin
	DB	0		; WINRM - right margin
	DW	0		; WINCRF - <CR> function
	DW	0		; WINCRC - <CR> counter
	DB	0		; WINHIGHL - Highlights
	DB	$f		; WINFORE - foreground color default of white
	DB	0		; WINBGND - background color
	DB	0		; WINFONT - window font (0=normal/1=alternate)
	DB	2,FONT_H	; WINFSIZE - Width/Height
	DB	$0f		; WINATR - all attributes on for window 0
	DB	0		; WINLCNT - current number of lines
	DB	MAXWIDTH	; WINXSZ - width of window, in pixels, using margin
	DB	0,0		; WINLLEN - length of current line
	DB	(MAXHEIGHT/FONT_H)-1 ; WINLINES - max # of lines for window
;
; same start as window 0, but with 0 length
;
WINDOW1:
	DB	0,0,0,MAXWIDTH,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,MAXWIDTH,0,0,0
;
; the rest have no width/height/attributes (except buffering)
;
WINDOW2:
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,0,0,0,0
WINDOW3:
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,0,0,0,0
WINDOW4:
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,0,0,0,0
WINDOW5:
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,0,0,0,0
WINDOW6:
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,0,0,0,0
WINDOW7:
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	$F,0,0,2,FONT_H,$08,0,0,0,0,0

WINTABLE:
	DW	WINDOW0,WINDOW1,WINDOW2,WINDOW3
	DW	WINDOW4,WINDOW5,WINDOW6,WINDOW7
	PAGE

; ------
; SCREEN
; ------
; Move to the screen specified, by updating CURWIN and the cursor
; X,Y pos (SCRX,SCRY).  Also put address of that window's structure
; in WINDOW. Save a bunch of the old stuff in old window's structure then
; update the same bunch with the new window's stuff.
;
;	ARG1 - new screen id: 0-7
;
ZSCRN:
	jsr	CLRBUF		; EMPTY OUTPUT BUFFER BEFORE MOVING
;
; now, save a few things from the common variables
;
	ldy	#WINLLEN	; current line length
	lda	LENGTH+LO	; get current line length
	sta	(WINDOW),Y	; save for later referencing
	iny			; point to msb
	lda	LENGTH+HI	; get MSB
	sta	(WINDOW),Y	; saved
	jsr	SAVE_CURSOR	; save the x,y pos of cursor
	lda	INVFLG		; get inverse flag
	beq	ZSCA1		; not set
	lda	#1		; set 1 bit
	bne	ZSCA2		; set in window
ZSCA1:
	lda	UNDFLG		; how about underlining
	beq	ZSCA2		; nope
	lda	#4		; 4 == underlining
ZSCA2:
	ldy	#WINHIGHL	; set new highlight
	sta	(WINDOW),Y	; save current attributes
;
; now it is time to update for new screen
;
	lda	ARG1+LO		; get which window
	sta	CURWIN		; save window number
	asl	A		; shift to make word indes
	tax			; swap to indexing reg
	lda	WINTABLE,X	; get the address of the new WINDOW
	sta	WINDOW+LO	; lo part comes first
	lda	WINTABLE+1,X	; so go get hi part
	sta	WINDOW+HI	; save the hi part of the address

	jsr	GET_CURSOR	; restore the cursor pos

	ldy	#WINXSZ		; get line length
	lda	(WINDOW),Y	; update zero page variable
	sta	XSIZE+LO	; this is for quicky comparing
	lda	#0		; clear MSB
	sta	XSIZE+HI
	asl	XSIZE+LO	; *2
	rol	XSIZE+HI
	asl	XSIZE+LO	; *4
	rol	XSIZE+HI
	ldy	#WINLLEN	; get current line length
	lda	(WINDOW),Y	; from before
	sta	LENGTH+LO	; save for later checking
	iny
	lda	(WINDOW),Y
	sta	LENGTH+HI
	ldy	#WINLCNT	; how many lines already out there
	lda	(WINDOW),Y	; has been already saved
	sta	LINCNT		; and save 'ere too
	ldy	#WINLINES	; how many lines in this
	lda	(WINDOW),Y	; window, anyway?
	sta	MAXLINES	; show for everyone to see
	ldy	#WINLEFT	; show left margin
	lda	(WINDOW),Y	; get left edge
	ldy	#WINLM		; left margin
	clc			; adding
	adc	(WINDOW),Y	; to find new left margin
	sta	LEFTMRG		; set up left margin for ease of use
	ldy	#WINTOP		; get top of window
	lda	(WINDOW),Y	; got it
	clc			; adding
	ldy	#WINHGHT	; add height
	adc	(WINDOW),Y	; did it
	sta	SCRBTM		; this is first line outside window
	ldy	#WINHIGHL	; get highlighting modes
	lda	(WINDOW),Y	; first for inverse video
	and	#1		; check for inverse video
	beq	SCRINV		; nope
	lda	#$80		; means inverse video
SCRINV:
	sta	INVFLG		; there it be
	lda	(WINDOW),Y	; point to underline flag
	and	#4		; check for underlining
	beq	SCRUND		; nope
	lda	#$80		; means underlining
SCRUND:
	sta	UNDFLG		; save for everyone

	ldy	#WINATR		; get the current attributes
	lda	(WINDOW),Y	; gottem
	jsr	SET_ATTRIBUTES	; set the flags, thank you
	ldy	#WINFONT	; get the font
	lda	(WINDOW),Y	; thank you
	beq	SCRFNT0		; is it font zero?
	lda	#MONOFONT_W	; must be mono font, set width
SCRFNT0:
	sta	FONTFLG		; mark normal font
ZSCREX:
	rts
;--------------
; ZWINPOS
;--------------
;
; change the window ARG1's top left corner to the new position
;
; ARG1 - window id from 0-7
; ARG2 - new top y pos
; ARG3 - new top x pos
;
ZWINPOS:
	dec	ARG2+LO		; make pos be zero based
	dec	ARG3+LO		; now they are!
; 
; if moving current window, save current cursor pos
;
ZWPOS0:
	jsr	SAVE_CURSOR	; saving
;
; let's set up [J] to point to window we are talking about
;
ZWPOS1:
	lda	ARG1+LO		; get window ID 
	jsr	SETWJ		; get J to point to it

        lda     ARG2+LO         ; first, check the top
        cmp     #MAXHEIGHT      ; < max height?
        bcc     CKWA1           ; fine
        lda     #0              ; make it zero then        
        sta     ARG2+LO         ; it is now
CKWA1:
        lda     ARG3+LO         ; now check left edge
        cmp     #MAXWIDTH       ; howzit compare
        bcc     CKWA2           ; just fine
        lda     #0              ; this too
        sta     ARG3+LO         ; it is now
CKWA2:
;
; make the cursor pos be relative, so we can change back to
; absolute using new window pos
;
	ldy	#WINY		; let's do y pos first
	lda	(J),Y		; get the old y pos
	ldy	#WINTOP		; and subtract the top to make
	sec			; (getting ready)
	sbc	(J),Y		; the pos relative
	clc			; now add in new top
	adc	ARG2+LO		; this will be new top
	ldy	#WINY		; get y offset again
	sta	(J),Y		; and save new absolute y pos
  	ldy	#WINX		; now we be talking about x pos
	lda	(J),Y		; get old x pos
	sec			; getting ready for sub
	ldy	#WINLEFT	; get rid of left ness
	sbc	(J),Y		; now it's relative
	clc			; get ready to add in new left ness
	adc	ARG3+LO		; done
	ldy	#WINX		; get x offset again
	sta	(J),Y		; save in structure
;
; now we can change the top and left of the window
;
	lda	ARG2+LO		; this is top of window
	ldy	#WINTOP		; TOP offset
	sta	(J),Y		; save the new top
	lda	ARG3+LO		; here is the left edge
	ldy	#WINLEFT	; offset into struct
	sta	(J),Y		; saved !
;
; we might have moved current window so update screen cursor and left margin
;
	jsr	GET_CURSOR	; restore cursor
	ldy	#WINLEFT	; get left edge
	lda	(WINDOW),Y	; okay
	ldy	#WINLM		; add in left margin
	clc			; adding
	adc	(WINDOW),Y	; to find new left edge
	sta	LEFTMRG		; store for ease of use
	ldy	#WINXSZ		; get xsize
	lda	(J),Y		; okay
	sta	XSIZE+LO	; this is for quicky comparing
	lda	#0		; clear MSB
	sta	XSIZE+HI
	asl	XSIZE+LO	; *2
	rol	XSIZE+HI
	asl	XSIZE+LO	; *4
	rol	XSIZE+HI
ZWPOSX:
	rts
;------------
; ZWINSIZE
;------------
;
; Change the size of window ARG1.  If cursor is outside of window,
; move to it.
;
;	ARG1 - window ID
; 	ARG2 - height
;	ARG3 - width
;
; Uses [J].
;
ZWINSIZE:
;
; first put SCRCX,CY into window structure, just in case
;
	jsr	SAVE_CURSOR	; saved
;
; now do everything
;
	lda	ARG1+LO		; get the window ID
	jsr	SETWJ		; and get window pointer

        lda     ARG2+LO         ; first, check the top
        cmp     #MAXHEIGHT      ; < max height?
        bcc     CKWA11          ; fine
        lda     #MAXHEIGHT      ; make better
        sta     ARG2+LO         ; it is now
CKWA11:
        lda     ARG3+LO         ; now check left edge
        cmp     #MAXWIDTH       ; howzit compare
        bcc     CKWA21          ; just fine
        lda     #MAXWIDTH       ; make it the max
        sta     ARG3+LO         ; it is now
CKWA21:
	lda	ARG2+LO		; get new height
	ldy	#WINHGHT	; offset
	sta	(J),Y		; save new height
	lda	ARG3+LO		; get width
	ldy	#WINWID		; store width
	sta	(J),Y		; okay
;
; now we need to figger out new XSIZE, MAXLINES
;
	ldy	#WINWID		; store width
	lda	(J),Y		; get width
	sec			; have width, subtract margins
	ldy	#WINRM		; first right margin
	sbc	(J),Y		; subtracted right margin
	ldy	#WINLM		; and now for the left margin
	sbc	(J),Y		; now we have new XSIZE
	ldy	#WINXSZ		; get offset
	sta	(J),Y		; save for later ref
	lda	ARG2+LO		; get new height
	ldx	#$FF		; this is the counter
	sec			; get ready for subs
ZWSZ1:
	inx			; count this line
	sbc	#FONT_H		; subtract off font height	
	bcs	ZWSZ1		; still some lines	
	dex			; to keep the input line on screen
	txa			; get line count for storage
	ldy	#WINLINES	; this is how many lines are allowed
	sta	(J),Y		; saved
;
; check to make sure the cursor is still in the window
;
	ldy	#WINLM		; get left margin
	clc			; for adding
	adc	(J),Y		; for figgering out right edge
	ldy	#WINLEFT	; add in left edge
	adc	(J),Y		; get right column by adding in left one
	ldy	#WINX		; check X pos
	cmp	(J),Y		; see if X is still inside?
	beq	ZWSZ2		; must move to top left
	bcc	ZWSZ2		; ditto if X >= margin
	ldy	#WINTOP		; get top to figger last line
	lda	(J),Y		; from the structure
	ldy	#WINHGHT	; and add in the height
	clc			; getting ready for add
	adc	(J),Y		; to find first line outside of range
	ldy	#WINY		; now check y
	cmp	(J),Y		; now check y then
	beq	ZWSZ2		; outside, move to top left
	bcs	ZWSZ3		; inside so quit
;
; move the cursor to top left if outside of resized window
;
ZWSZ2:
	ldy	#WINTOP		; top line is here
	lda	(J),Y		; so get me it
	ldy	#WINY		; now we be doing y
	sta	(J),Y		; change Y
	ldy	#WINLEFT	; move X to left margin
	lda	(J),Y		; first get left edge
	ldy	#WINLM		; and add in left margin
	clc			; (adding)
	adc	(J),Y		; to get left spot of cursor
	ldy	#WINX		; this is x offset
	sta	(J),Y		; so move X there
;
; now check to see if we mucked with current window
;
ZWSZ3:
	lda	ARG1+HI		; this is the id
	bmi	ZWSZ4		; must be current window
	cmp	CURWIN		; is it current window?
	bne	ZWSZX		; nope, so done
ZWSZ4:
	ldy	#WINLINES	; get how many lines
	lda	(J),Y		; get number of lines
	sta	MAXLINES	; set global
	ldy	#WINXSZ		; get new XSIZE too
	lda	(J),Y		; get it
	sta	XSIZE+LO	; this is for quicky comparing
	lda	#0		; clear MSB
	sta	XSIZE+HI
	asl	XSIZE+LO	; *2
	rol	XSIZE+HI
	asl	XSIZE+LO	; *4
	rol	XSIZE+HI
	jsr	GET_CURSOR	; fix the cursor pos
ZWSZX:
	rts
;
;CHECK_WIN_ARGS
; check args to make sure they be okay
;
;  ARG2 = top/height
;  ARG3 = right/width
;
CHECK_WIN_ARGS:
        rts

	; ------
	; HLIGHT
	; ------

ZLIGHT:
	lda	ARG1+LO		; get argument
	cmp	#$10		; must be <= 8
	bcs	ZLIEX		; nope it aint
 	ora	#$80		; turn on hi bit
	jsr	COUT		; send it out then
ZLIEX:
	rts			; done
;
; actually set the highlight flags according to [A]
;	
HLIGHT:
;
; TURN OFF ALL HIGHLIGHTS
;	
	and	#$7F		; turn off high bit
	bne	ZL1		; nope, must be something
	sta	UNDFLG		; and turn of underlining flag
	sta	INVFLG		; and inverse
	rts			; all done
ZL1:
	cmp	#4		; underlining
	bne	ZL_REV		; maybe its INVERSE?

	lda	#$80		; turn on UNDFLG
	sta	UNDFLG		; with an FF
	rts			; done
ZL_REV:	
	cmp	#1		; INVERSE?
	bne	ZL_MONO		; maybe monospaced then
	lda	#$80		; must be inverse video
	sta	INVFLG		; and put it in invflg
	rts
ZL_MONO:
	rts			; fuck it for now!	
;
; ZWINATTER - change the scrolling/buffering/scripting/wrapping attributes
; of the window.
;
; ARG1 - window id
; ARG2 - interesting bits
;	| buffering | scripting | scrolling | wrapping |
; ARG3 - operation - 	0 MOVE
;		  	1 SET
;			2 CLEAR
;			3 COMPLEMENT
;
ZWINATTR:
	lda	NARGS		; see how many args
	cmp	#3		; check for operation args
	beq	ZWAT1		; yes, already there
	lda	#0		; zero means MOVE
	sta	ARG3+LO		; so show it as default
ZWAT1:
	lda	ARG1+LO		; get window ID
	jsr	SETWJ		; put window address into [J]

	ldy	#WINATR		; get attribute offset
	lda	(J),Y		; get current attribute
	clc			; so we can just branch

	dec	ARG3+LO		; decrement to find out what to do
	bmi	ZWMOVE		; do a move
	beq	ZWSET		; do a set
	dec	ARG3+LO		; check once more
	beq	ZWCLEAR		; clear set ones
;
; this is for the COMPLEMENT operations
;
	eor	ARG2+LO		; complement bits
	bcc	ZWAT2		; done
ZWMOVE:
;
; move into the flag word
;
	lda	ARG2+LO		; get new flags
	bcc	ZWAT2		; done
ZWSET:
;
; set turned on ones
;
	ora	ARG2+LO		; set'em up
	bcc	ZWAT2		; done
ZWCLEAR:
;
; clear just the ones that are on
;
        lda     ARG2+LO         ; get argument
        eor     #$FF            ; turn on all the off ones
	and	(J),Y		; keep all the other ones
ZWAT2:
	sta	(J),Y		; and save it back
;
; now, if current window, set necessary flags
;
	ldx 	ARG1+LO		; get window ID
        bmi     SET_ATTRIBUTES  ; assume negative is current window
	cpx	CURWIN		; is this the current one?
	bne	ZWATX		; nope, so leave
SET_ATTRIBUTES:
;
; current window, so set flags accordingly
;
	ldx	#0		; to turn off flag
	ldy	#1		; to turn on flag
	ror	A		; get wrapping flag into carry
	bcc	ZWAT3		; clear it
	sty	WRPFLG		; set wrapping on
	bcs	ZWAT33		; okay
ZWAT3:
	stx	WRPFLG		; clear wrapping flag
ZWAT33:
	ror	A		; now check thes crolling flag
	bcc	ZWAT4		; not set
	sty	SCRLFLG		; turn on scrolling flag
	bcs	ZWAT44		; okay, next please
ZWAT4:
	stx	SCRLFLG		; turn off scrolling
ZWAT44:
	ror	A		; checking the scripting flag
	bcc	ZWAT5		; not set
	sty	SCRIPTFLG	; allow scripting? yes
	bcs	ZWAT55		; next flag
ZWAT5:	
	stx	SCRIPTFLG	; allow scripting? no
ZWAT55:
	ror	A		; this checks the buffering flag
	bcc	ZWAT6		; not set
	sty	BUFFLG		; turn on buffering
	bcs	ZWATX		; scram
ZWAT6:
	stx	BUFFLG		; turn off buffering
ZWATX:
	rts
;
; ZWINGET - put the window stuff into a table
;
;  ARG1 - Window id
;  ARG2 - offset
;
ZWINGET:
	jsr	SAVE_CURSOR	; save x,y into structure

	lda	ARG1+LO		; get window ID
	jsr	SETWJ		; set up window address

	lda	ARG2+LO		; get where to start getting
	asl	A		; make word index
	tax			; make index
	lda	ZWGTBL+LO,X	; get lo part of address
	sta	K+LO		; save for jumping
	lda	ZWGTBL+HI,X	; get hi part
	sta	K+HI		; saved
	jsr	ZWGVEC		; do the vectoring
	stx	VALUE+HI	; save hi part
	sta	VALUE+LO	; and the lo part
	jmp	PUTVAL		; and return it
ZWGVEC:
	jmp	(K)		; and goto it boys
ZWG0:
	ldx	#0		; zero out high part
	ldy	#WINTOP		; get window y pos
	lda	(J),Y		; got it
	tay			; need to inc it
	iny			; to make it a one-based number
	tya			; back to a
	rts
ZWG1:
	ldx	#0		; zero out high part
	ldy	#WINLEFT	; get window x pos
	lda	(J),Y		; got it
	tay			; need to inc it
	iny			; to make it a one-based number
	tya			; back to a
	rts
ZWG2:
	ldx	#0		; zero out high part
	ldy	#WINHGHT	; get window height
	lda	(J),Y		; got it
	rts
ZWG3:
	ldx	#0		; zero out high part
	ldy	#WINWID		; window width
	lda	(J),Y		; got it
	rts
ZWG4:
	jsr	FETCHCY		; get the current Y pos
	ldx	#0		; zero out high part
	rts
ZWG5:
	jsr	FETCHCX		; fetch the current X pos
	ldx	#0		; zero out high part
	rts
ZWG6:
	ldx	#0		; zero out high part
	ldy	#WINLM		; left margin
	lda	(J),Y		; got it
	rts
ZWG7:
	ldx	#0		; zero out high part
	ldy	#WINRM		; right margin
	lda	(J),Y		; got it
	rts
ZWG8:
	ldy	#WINCRF+HI	; <CR> function
	lda	(J),Y		; got it, lo part
	tax			; save hi part
	dey			; point to lo part
	lda	(J),Y		; got lo part
	rts
ZWG9:
	ldy	#WINCRC+HI	; <CR> count
	lda	(J),Y		; got it, hi part
	tax			; save it
	dey			; now to lo part
	lda 	(J),Y		; got it
	rts
ZWG10:
	ldy	#WINHIGHL	; get high light mode
	lda	(J),Y		; got it
	ldx	#0		; zero out high part
	rts
ZWG11:
	ldy	#WINBGND	; and background
	lda	(J),Y		; got it
        tay                     ; make index
        lda     APLCOLOR,Y      ; get apl->zip color
        tax                     ; save for return
	ldy	#WINFORE	; get foreground
	lda	(J),Y		; got it
	tay			; save it
        lda     APLCOLOR,Y      ; get apl->zip color
	rts
ZWG12:
	ldy	#WINFONT	; now for font id
	lda	(J),Y		; got it
	ldx	#0		; zero out high part
	rts
ZWG13:
	ldy	#WINFSIZE+1	; font size (width)
	lda	(J),Y		; got it
	tax			; save it
	dey			; font size (height)
	lda	(J),Y		; got it
	rts
ZWG14:
	ldx	#0		; zero out high part
	ldy	#WINATR		; attributes
	lda	(J),Y		; got it
	rts			; got them!
ZWG15:
	ldx	#0		; only 1 byte worth
	ldy	#WINLCNT	; offset
	lda	(J),Y		; get more counter
	rts
;
; jump table for figgering out where to start filling
;
ZWGTBL:
	DW	ZWG0,ZWG1,ZWG2,ZWG3,ZWG4,ZWG5,ZWG6,ZWG7
	DW	ZWG8,ZWG9,ZWG10,ZWG11,ZWG12,ZWG13,ZWG14,ZWG15
;	
; ZSCROLL - scroll the specified window up or down
;
;  ARG1 - window ID
;  ARG2 - lines to scroll; <0 scrolls down
;
ZSCROLL:
	jsr	SAVE_CURSOR	; save where cursor is

        lda     #0              ; don't script scroll
        sta     SCRIPT          ; fine, we won't now

	lda	ARG1+LO		; get which window
	jsr	SETWJ		; set up pointer
	dec	NARGS		; see what we have for lines
	bne	ZSCR1		; a passed arg
	lda	#1		; 1 line is default
	sta	ARG2+LO		; so say so
ZSCR1:
	ldy	#WINTOP		; get me window limits
	lda	(J),Y		; first top
	sta	CLSTOP		; save for usage
	iny			; now left
	lda	(J),Y		; okay, get for this window
	sta	CLSLEFT		; save left
	iny			; now it's width
	lda	(J),Y		; got the window height
	sta	CLSHEIGHT	; this is it
	iny			; this is height
	lda	(J),Y		; get window's height
	sta	CLSWIDTH	; save
	lda	ARG2+HI		; is it negative?
	sta	SCLDIR		; show direction of scroll
	bpl	ZSCRL1		; nope
	ldx	ARG2+LO		; get how many lines
	eor	#$FF		; make pos
	tax			; put in X for inc
	inx			; and make right
	bne	ZSCRLEX		; done
ZSCRL1:
	ldx	ARG2+LO		; get how many lines
ZSCRLEX:
	stx	SCLLINES	; save for routine
	jsr	DO_SCROLL	; and do the scroll
ZSCRLEX1:
        lda     #1              ; allow scripting
        sta     SCRIPT          ; we do
	jmp	GET_CURSOR	; restore the cursor, thank you
;
; ZWINPUT - put some information into window structure.  Just CRF/CRC
;	is legit now.
;
;  ARG1 - window ID
;  ARG2 - Word to be saved
;  ARG3 - offset of value
;
ZWINPUT:
        jsr     SAVE_CURSOR     ; save the cursor pos
	lda	ARG1+LO		; get window ID
	jsr	SETWJ		; have J point to structure

	lda	ARG2+LO		; get first value in table
	cmp	#8		; 8 = <CR> function
	beq	ZWIPCRF		; so save that
	cmp	#9		; this is counter
	beq	ZWIPCRC		; so set counter
        cmp     #15             ; LINCNT?
        beq     ZIPLCT          ; then set it too
	rts			; just die then
ZIPLCT:
        ldy     #WINLCNT        ; point to line count
        bne     ZWINPUT2        ; and stash it away
ZWIPCRF:
        ldy     #WINCRF+1       ; point to window's CR function
        bne     ZWINPUT1        ; and put it        
ZWIPCRC:
	ldy	#WINCRC+1	; point to window's CR counter
ZWINPUT1:
	lda	ARG3+HI		; now get hi part
	sta	(J),Y		; saved!
        dey                     ; point to lo part
ZWINPUT2:
	lda	ARG3+LO		; start with lo part
	sta	(J),Y		; save lo part
ZWIPLCT:
        jmp     GET_CURSOR      ; restore cursor/lincnt        
	
	END
