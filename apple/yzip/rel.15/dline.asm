	STTL	"--- Display line Routine ---"
	PAGE
;
; some aux mem zero page stuff
;
DLVARS:
CHR_X	EQU	DLVARS		; (WORD) X pos for char (0-559)
ENDBIT	EQU	CHR_X+2 	; (BYTE) ending bit offset
WRPFLG	EQU	ENDBIT+1	; wrapping flag
SCRLFLG	EQU	WRPFLG+1	; scrolling flag
BUFFLG	EQU	SCRLFLG+1	; buffering flag
SAVEX	EQU	BUFFLG+1	; (BYTE) save original X
CHR_Y	EQU	SAVEX+1		; (BYTE) Working cursor Y pos
LINECHAR EQU	CHR_Y+1		; (BYTE) working char counter
BITTER	EQU	LINECHAR+1	; (BYTE) bit twiddler counter
INCFLG	EQU	BITTER+1	; (BYTE) ==1 if NEXT_SCR needed after setpos
SHOW_CURSOR EQU	INCFLG+1	; (BYTE) flag as to whether we are doing cursor
SCRCNT	EQU	SHOW_CURSOR+1	; (BYTE) screen buffer offset
SV_UND	EQU	SCRCNT+1	; (BYTE) spot to save underline flag
SV_INV	EQU	SV_UND+1	; (BYTE) save starting INVFLG
UNDERF  EQU     SV_INV+1        ; (BYTE) underline flag used internally
LINE_HEIGHT EQU	UNDERF+1	; (BYTE) counter for height
DLVARSL	EQU	LINE_HEIGHT-CHR_X+1

	BLKB	DLVARSL,0

BIT_BITS: DB	7,6,5,4,3,2,1,0	; number significant bits in last byte

DISP_LINE:
	lda	SCRCNT		; make sure there are characters
	bne	DPL1		; ayyup
	rts			; no chars, don't do anything
DPL1:
	lda	#0		; start at zero
	sta	CC_OFF		; which byte to use
	sta	INCFLG		; and clear out inc'er flag
	
	lda	#FONT_H-1	; do for height of char
	sta	LINE_HEIGHT	; height counter

	ldy	SCRCY		; and starting y
	sty	CHR_Y		; saved
	ldy	SCRCX		; save x to
	sty	SAVEX		; save it
	lda	INVFLG		; get current state
	sta	SV_INV		; and save it
        lda     UNDFLG          ; get current underline state
        sta     SV_UND          ; and save it too

	lda	CURSFLG		; changed x pos?
 	beq	DPL2		; no new figgering
	ldx	#0		
	stx	CHR_X+HI	; clear out msb

	lda	SCRCX		; get new cursor x pos
	asl	A		; *2
	rol	CHR_X+HI
	asl	A		; *4
	sta	CHR_X+LO
	rol	CHR_X+HI
DPL2:
	lda	CHR_X+LO
	sta	SCRCX
	and	#3		; pick up 2 bits rolling out
	sta	BITTER		; what to add
	lda	CHR_X+HI
	lsr	A		; /2
	ror	SCRCX
	lsr	A		; /4
	ror	SCRCX
	jsr	SETPOS		; set up pointers

	lda	BITTER		; get extra
	clc			; add in
	adc	BITOFF		; normal bit offset
	sta	BITTER		; and make it new one
	cmp	#7		; but can't be greater than 6
	bcc	DO_LINE		; okay, it's not
	sta	INCFLG		; show needing NEXT_SCR call
	sbc	#7		; so shrink it
	sta	BITTER		; save it
DO_LINE:
	jsr	SETPOS		; set up pointers
	lda	INCFLG		; do we need to go one more?
	beq	DOL1		; nope
	jsr	NEXT_SCR	; and point to next screen byte
DOL1:
 	ldx	BITTER		; get bit off set
	stx	BITOFF		; and save here
	lda	#0		; init a few things
	sta	SCR_DATA	; init screen data byte
	sta	LINECHAR	; which character on the line 
	sta	CH_OFFSET		; which byte in line on screen
	sta	LINE_X+LO	; X pos of line
	sta	LINE_X+HI	; part 2
        sta     UNDERF          ; make sure this be zero

	lda	SV_INV		; get starting INV state
	sta	INVFLG		; okay!
        lda     SV_UND          ; get starting UNDERLINE state
        sta     UNDFLG          ; and restart it

	dec	CH_OFFSET		; inclusive count
	ldx	BITOFF		; make bitoff be a count
	lda	BIT_BITS,X	; get count
	tax			; x is bit counter
CHARL:
	ldy	LINECHAR	; get the char
	cpy	SCRCNT		; done with line?
	bne	CHARL1		; nope
	jmp	COPYL		; okay, move onto screen
CHARL1:
	inc	LINECHAR	; point to next one
	lda	SCRBUFF,Y	; got it
	bpl	CHARL3		; not a highlight char

	jsr	HLIGHT		; so check it out
	jmp	CHARL		; done
CHARL3:
	cmp	#SPACE		; is it a highlight char?
	bcc	CHARL		; ignore this one then
CHARL2:
	tay			; use as index
	lda	CHADR_H,Y	; get high part of address
	sta	CHPTR+HI	; point to it
	lda	CHADR_L,Y	; get lo part
	sta	CHPTR+LO	; save it
	lda	CHWID,Y		; get how wide the char is
	sta	CW		; save it
	lda	CC_OFF		; to get data
	ldy	CW		; see how wide the char is
	cpy	#10		; if <= 8, only one byte of info
	bcc	CHRLCW		; it is, so skip shift
 	asl	A		; *2 for bigger chars
CHRLCW:
	tay			; get the index
	lda	(CHPTR),Y	; get first byte
	sta	CHAR_D1		; part one
	iny			; point to next part
	lda	(CHPTR),Y	; get second byte
	sta	CHAR_D2		; and save it
	lda	CW		; get width back
	cmp	#10		; if <= 8 zero byte 2 of char data
	bcs	CHRLCW1		; nope
	lda	#0		; clear out byte 2
	sta	CHAR_D2		; okay
CHRLCW1:
	lda	FONTFLG		; is this monospacing?
	beq	CHRLFX		; nope
;
; if monospacing, see if it char is < mono width, and try to split
; the difference if so
;
	cmp	CW		; check against current width
	bcs	CHRLF0		; CW < monowidth, so center
;
; too big, so take a little off front end too
;
	lda	CW		; get current width
	sec			; get ready for subbing
	sbc	#MONOFONT_W	; take away mono width
	lsr	A		; /2 to get front half
	tay			; use as counter
	beq	CHRLF1		; nothing to do
CHRLFX1:
	asl	CHAR_D2		; step to the left
	rol	CHAR_D1		; pick up char
	dey			; count one
	bne	CHRLFX1		; do next one
	beq	CHRLF1		; all done
CHRLF0:
	sec			; do sub
	sbc	CW		; subtract CW
	lsr	A		; /2 to get front part
	tay			; put in counter
	beq	CHRLF1		; no need to do anything
CHRLFL:
	lsr	CHAR_D1		; shift to right
	ror	CHAR_D2		; this part too
	dey			; count shift
	bne	CHRLFL		; not done shifting yet
CHRLF1:
	lda	#MONOFONT_W	; get mono font width
	sta	CW		; chop at mono width
CHRLFX:
	lda	LINE_HEIGHT	; if == 0, then pick up underlining
	bne	CHRL3		; nope

	lda	UNDFLG		; get underlining flag
	sta	UNDERF		; and mark for later
CHRL3:
	lda	SCR_DATA	; get starting data
DATAL:
	rol	CHAR_D2		; it's a step to the left
	rol	CHAR_D1		; nother one
	ror	A		; and put into screen byte
	eor	INVFLG		; pick up inversing
	ora	UNDERF		; pick up underline flag
	inc	LINE_X+LO	; next X pos too
	bne	DATAL0		; no wrap
	inc	LINE_X+HI	; wrapped
DATAL0:
	dex			; next bit pos
	bne	DATAL1		; nope
;
; save the byte and point to next byte
;
STCH:
	lsr	A		; put in lower 7 bits
	inc	CH_OFFSET		; point to next one
	ldy	CH_OFFSET		; get current offset
	sta	SCR_LINE,Y	; save in line
	ldx	#7		; and start bitoff at 7
	lda	#0		; clear a few locals
DATAL1:
	dec	CW		; well?
	bmi	COPYL1		; all done, start copying
	bne	DATAL		; nope
	sta	SCR_DATA	; save current screen byte stuff here
	jmp	CHARL		; do next char
COPYL:
	lda	LINE_X+LO	; see if we have any at all
	ora	LINE_X+HI	; well, did we go anywhere?
	beq	LINE_EXIT	; then quit
	lda	BIT_BITS,X	; get number of significant bits
	sta	ENDBIT		; save this one
	cpx	#7		; has it been renewed?
	beq	COPYL1		; no last byte
	lda	SCR_DATA	; get screen data
COPYSH:
	lsr	A		; put down low, please
	dex			; one more
	bne	COPYSH		; and do it again
	beq	STCH		; now save this char
COPYL1:
	jsr	COPY_SCRL	; copy SCR_LINE to screen
LED2:
	inc	SCRCY		; point to next line	
	dec	LINE_HEIGHT	; count this line
	bmi	LINE_EXIT	; all done!

	inc	CC_OFF		; point to next byte
	jmp	DO_LINE		; and do next line
LINE_EXIT:
	ldx	SAVEX		; get x back
	stx	SCRCX		; restore it

	lda	SHOW_CURSOR	; are we just doing cursor?
	bne	DLXX		; yes, don't change char pos

	clc			; add in length of this line
	lda	LINE_X+LO	; get length of line
	adc	CHR_X+LO	; add to start pos
	sta	CHR_X+LO	; save it
	lda	LINE_X+HI	; and the MSB
	adc	CHR_X+HI	; okay
	sta	CHR_X+HI	; save for a rainy day
	jsr	GET_SCRCX	; set SCRCX back up
DLXX:
	lda	CHR_Y		; restore the 
	sta	SCRCY		; y pos
	lda	#0		; clear out counter
	sta	SCRCNT		; okay
	sta	CURSFLG		; reset this flag too
	rts

GET_SCRCX:
	lda	CHR_X+LO	; pick this back up
        sta     SCRCX
	lda	CHR_X+HI	; /2
	lsr	A       	; pick up remainder
	ror	SCRCX   	; /4
	lsr	A		; pick up remainder
	ror	SCRCX		; got it
	lda	CHR_X+LO	; any remainder?
	and	#3		; well?
	beq	DLXX		; nope
	inc	SCRCX		; point to next pos
	rts

;
; DO_CURSOR - read in the screen byte, XOR cursor data, store it
;
DO_CURSOR:
	ldy	#0		; and zero y
	sty	DATALOOP	; init this counter
DOCLOOP:

	ldy	#0		; must use Y
	ldx	FPCBNK		; what bank
	lda	PAGE2SW,X	; select it
	lda	(FPC),Y		; okay, get byte
	ldx	DATALOOP	; get offset
	eor	SCR_LINE,X	; get data
	sta	(FPC),Y		; save data
	jsr	NEXT_SCR	; point to next screen byte
	inc	DATALOOP	; point to next char
	dec	CH_OFFSET	; count one char
	bpl	DOCLOOP		; all done with this line?
	lda	PAGE2SW+MAIN	; make sure we are point to MAIN
	rts

	END

