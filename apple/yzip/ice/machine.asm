	TITLE	"Apple ][ YZIP (c)Infocom","MACHINE-DEPENDENT I/O: APPLE II"
; -----------------------
; DIRECT PRINT LINE [X/A]
; -----------------------
; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)
; STRING LENGTH IN [Y]
DLCNT:	db 	0
DLOFF:	db	0
DISPLAY_LINE:
	stx	STRING+LO	; DROP STRING ADDRESS
	sta	STRING+HI	; INTO DUMMY BYTES
	sty	DLCNT		; COUNTER
	ldx	#0		; INIT CHAR-FETCH INDEX
	stx	SCRIPT		; don't script any of my internal
DOUT:	DB	$BD		; 6502 "LDA nnnn,X" OPCODE
STRING:	DW	$0000		; DUMMY OPERAND BYTES
	stx	DLOFF		; save X
DOUT1:	jsr	COUT
	ldx	DLOFF		; get x back
	inx
	dec	DLCNT		; LOOP TILL
	bne	DOUT		; OUT OF CHARS
	jsr	CLRBUF		; and force the chars out
	lda	#1		; allow scripting again
	sta	SCRIPT		; okay, we did
	rts			; done

; SWAP2INFOW - save current state and swap to the information window,
;	which is currently window zero
SWAPA1:	ds	1		; save for current ARG1
SWAPCW:	ds	1		; save for current window
SWAPFLG: db	0		; flag == 1 if we have already swapped
SWAP2INFOW:
	lda	SWAPFLG		; already swapped?
	bne	SWP2		; ayyup
	jsr	CLRBUF		; clear out the buffer
	jsr	SAVE_CURSOR	; save current postion of cursor
	lda	ARG1+LO		; get current arg1
	sta	SWAPA1		; save it
	lda	CURWIN		; get current window
	sta	SWAPCW		; save it
	lda	#0		; swap to window zero for messages
	sta 	ARG1+LO		; okay
	jsr	ZSCRN		; and swap to it
SWP2:
	inc	SWAPFLG		; show we are in swap window
SWAPEX:
	rts			; all done
;
; SWAPBACK - swap out of info window, and back to old window
;
SWAPBACK:
	dec	SWAPFLG		; are we finally leaving for good?
	bne	SWAPEX		; nope
	lda	SWAPCW		; and now for current window
	sta	ARG1+LO		; swap to it
	jsr	ZSCRN		; we did
	lda	SWAPA1		; and get arg1 back
	sta 	ARG1+LO		; we did
	jmp	GET_CURSOR	; and get cursor back

; -----------------------
; SEND [LBUFF] TO PRINTER
; -----------------------
; ENTRY: LENTH OF LINE IN [PRLEN]

PLEAV:	RTS		

PPRINT:
	lda	SCRIPT		; SCRIPTING INTERNALLY ENABLED?
	and	SCRIPTF		; SCRIPTING ON?
	and	SCRIPTFLG	; Window allow scripting?
	beq	PLEAV		; NO, EXIT
	lda	CSW+LO		; SAVE NORMAL OUTPUT HOOK
	pha		
	lda	CSW+HI	
	pha		
	lda	ALTCSW+LO	; LOAD SCRIPTING HOOK
	sta	CSW+LO	
	lda	ALTCSW+HI	
	sta	CSW+HI	
        lda     RDROM           ; put system ROM in for printer out
	ldy	#0	
PP5:
	lda	LBUFF,Y		;GET A CHAR TO SEND OUT
	jsr	MCOUT	
	iny		
	dec	PRLEN		;LINE COUNT
	bne	PP5		;PRINT WHOLE LINE
;
; ALL DONE, RESET TO NORMAL AND LEAVE
;
        lda     BNK2SET         ; write RAM, bank 1
        lda     BNK2SET
	pla		
	sta	CSW+HI	
	pla		
	sta	CSW+LO	
	rts		

PSTAT:	DB	0	;SET TO CLEAR WHEN BOOT,
			;I PUT IT HERE SO RESTART WON'T ALTER
ALTCSW:	DB	0,0	;(WORD) PRINTER COUT

	; FIRST TIME USING PRINTER, INITIALIZE IT
SLOTM:	DB	EOL	
	DB	"Printer Slot 1-7: "
SLOTML	EQU	$-SLOTM

PCHK:	
	jsr	SWAP2INFOW	; point to info window
PCHK1:
	DLINE	SLOTM
	jsr	GETKEY	
	cmp	#'8'		;1-7
	bcs	PCHK1		;OOPS
	cmp	#'1'		; less than '1'?
	bcc	PCHK1		; ayyup
PC2:
	eor	#$F0		; make it a slot address	
	sta	ALTCSW+HI	
	lda	#EOL
	jsr	CHAR		;SEND >CR< TO SCREEN FOR NEATNESS
	inc	PSTAT		;SET TO ON
	lda	CSW+LO		;SAVE NORMAL OUTPUT HOOK
	pha		
	lda	CSW+HI	
	pha		
	lda	ALTCSW+LO	;LOAD SCRIPTING HOOK
	sta	CSW+LO	
	lda	ALTCSW+HI	
	sta	CSW+HI	
	jsr	INITPRT		; now, init it 

        lda     RDROM           ; bring in system ROM
	lda	#$89		; OUTPUT PRINTER SETUP SEQUENCE
	jsr	MCOUT		; START WITH COMMAND CHAR >CTRL-I<
	lda	#$B8		; 8 (80 COL WIDE)
	jsr	MCOUT	
	lda	#$B0		; 0
	jsr	MCOUT	
	lda	#$CE		; N (LF AFTER CR)
	jsr	MCOUT	
	lda	CSW+LO		; SAVE REAL PRINTER OUTPUT
	sta	ALTCSW+LO	; LOC. FOR NEXT TIME
	lda	CSW+HI	
	sta	ALTCSW+HI	
	pla			; RESET NORMAL OUTPUT
	sta	CSW+HI	
	pla		
	sta	CSW+LO	
        lda     BNK2SET         ; and bring back top RAM
        lda     BNK2SET         ; okay
	jmp	SWAPBACK	; and back to the old window
INITPRT:
	lda	#0		; jump to $Cn00
	sta	CSW+LO		; make LSB == 0
	jmp	(CSW)		; and goto it


SAVE_CURSOR:
	lda	SCRCY		; save the cursor pos
	ldy	#WINY		; get offset
	sta	(WINDOW),Y	; first y pos
	iny			; now x pos
	lda	SCRCX		; got it
	sta	(WINDOW),Y	; saved it
	ldy	#WINLCNT	; reset line count too
	lda	LINCNT		; okay
	sta	(WINDOW),Y	; get it
	rts

GET_CURSOR:
	ldy	#WINY		; get cursor pos back
	lda	(WINDOW),Y	; got y pos
	sta	SCRCY		; saved
	iny			; point to x pos
	lda	(WINDOW),Y	; got it
	sta	SCRCX		; make it better
	sec			; subtract left margin to get how long line is
	sbc	LEFTMRG		; okay we did that
	sta	LENGTH+LO	; this is how big line is too
	lda	#0		; clear MSB
	sta	LENGTH+HI
	asl	LENGTH+LO	; *2
	rol	LENGTH+HI
	asl	LENGTH+LO	; *4
	rol	LENGTH+HI
	
	ldy	#WINLCNT	; reset line count too
	lda	(WINDOW),Y	; get it
	sta	LINCNT		; okay
	lda	#1		; show cursor changed (maybe)
	sta	CURSFLG		; okay
	rts

; ------------
; SPLIT SCREEN
; ------------
;
; SPLIT SCREEN AT LINE [ARG1] putting screen 1 at top and screen
; 0 at bottom.
; DISABLE SPLIT IF [ARG1] = 0
;
ZSPLIT:
	jsr	SAVE_CURSOR	; save the cursor pos
;
; first muck with window 1
;
	lda	ARG1+LO		; get split arg back
	sta	WINDOW1+WINHGHT	; this becomes the height of window 1
	lda	#0		; put top of 1 to top of screen
	sta	WINDOW1+WINTOP	; okay, we did it
	lda	ARG1+LO		; get new height
	ldx	#$FF		; this is the counter
	sec			; get ready for subs
ZSPLIT0:
	inx			; count this line
	sbc	#FONT_H		; subtract off font height	
	bcs	ZSPLIT0		; still some lines	
	dex			; save input line
	stx	WINDOW1+WINLINES ; saved

	lda	WINDOW1+WINY	; check cursor pos
	cmp	ARG1+LO		; against height
	bcc	ZSPL1		; inside window, so save it
ZSPL0:
	lda	#0		; reset to top left
	sta	WINDOW1+WINY	; y pos at top
	sta	WINDOW1+WINX	; x pos at left
	sta	WINDOW1+WINLLEN+LO	; line length
	sta	WINDOW1+WINLLEN+HI	; line length
	sta	WINDOW1+WINLCNT	; line counter
;
; now muck with window 0
;
ZSPL1:
	lda	WINDOW0+WINTOP	; top of window 0
	sec			; doing sub
	sbc	ARG1+LO		; subtract out the new top
	clc			; adding
	adc	WINDOW0+WINHGHT	; to get the new height
;	bcc	ZSPLIT1		; okay, positive height
;	lda	#0		; make height 0
ZSPLIT1:
	sta	WINDOW0+WINHGHT	; and save new height
	
	ldx	#$FF		; this is the counter
	sec			; get ready for subs
ZSPLIT2:
	inx			; count this line
	sbc	#FONT_H		; subtract off font height	
	bcs	ZSPLIT2		; still some lines	
	dex			; to save input line
	stx	WINDOW0+WINLINES ; saved

	lda	ARG1+LO		; this is the new top
	sta	WINDOW0+WINTOP	; set in structure
	cmp	WINDOW0+WINY	; make sure cursor is still in window
	beq	ZSPL5		; nope, at the top
	bcc	ZSPL5		; or under it
	sta	WINDOW0+WINY	; put cursor at top
	lda	#0		; left of new
	sta	WINDOW0+WINX	; window 0
	sta	WINDOW0+WINLLEN+LO	; linelength
	sta	WINDOW0+WINLLEN+HI	; linelength
	sta	WINDOW0+WINLCNT	; line counter
ZSPL5:
	jsr	GET_CURSOR	; get the cursor pos back

	lda	#0		; now switch to window zero
	sta	ARG1+LO		; good bye
	jmp	ZSCRN		; making window 0 be current

; ------
; MARGIN
; ------
;
; Set the margins for the window
;
;  ARG1 - left margin
;  ARG2 - right margin
;  ARG3 - window ID (optional)
;
ZMARG:
	jsr	CLRBUF		; CLEAR LBUFF BEFORE RESETTING LINE MARGINS
	jsr	SAVE_CURSOR	; save current cursor pos
	
	lda	NARGS		; see if window ID was passed
	cmp	#3		; if ==3, then it's there
	beq	ZMRG1		; okay, so use it
	lda	CURWIN		; get the current window
	sta	ARG3+LO		; and use as the default
ZMRG1:
	lda	ARG3+LO		; check what window
	jsr	SETWJ		; get window offset into J
	lda	ARG1+LO		; do left margin first
	ldy	#WINLM		; get offset
	sta	(J),Y		; save for window
	iny			; point to right margin
	lda	ARG2+LO		; get right margin
	sta	(J),Y		; save right margin
	ldy	#WINWID		; get full width of window
	lda	(J),Y		; got it
	sec			; subtract off the 2 margins
	ldy	#WINLM		; first the left margin
	sbc	(J),Y		; okay, gone
	iny			; point to right margin
	sbc	(J),Y		; and take it off	
	ldy	#WINXSZ		; set width of usable window
	sta	(J),Y		; see, here it is
;
; move cursor to left margin
;
	ldy	#WINLEFT	; get left edge
	lda	(J),Y		; got it
	ldy	#WINLM		; and add left margin
	clc			; adding
	adc	(J),Y		; to get minimum X
	ldy	#WINX		; check to make sure X pos is okay
	sta	(J),Y		; then reset it
;
; now see if we changed the current window
;
ZMRGXP:
	ldx	ARG3+LO		; get the window
	bmi	ZMRG3		; -3 means current window
	cpx	CURWIN		; check against the current window
	bne	ZMRGX		; nope, so we be done
ZMRG3:
	sta	LEFTMRG		; [A] already has left margin
	ldy	#WINXSZ		; get xsize to set
	lda	(WINDOW),Y	; got it
	sta	XSIZE+LO	; this is for quicky comparing
	lda	#0		; clear MSB
	sta	XSIZE+HI
	asl	XSIZE+LO	; *2
	rol	XSIZE+HI
	asl	XSIZE+LO	; *4
	rol	XSIZE+HI
	jsr	GET_CURSOR	; restore the cursor pos
ZMRGX:
	rts
;
; SOUND
; -----
; ARG1 = BOOP (2) BEEP (1) ALL OTHERS INVALID
; (EZIP)

ZSOUND:
	ldx	ARG1+LO	; GET SOUND WANTED
	dex		
	beq	BEEP	
	dex		
	bne	ZSOEX	; INVALID
	ldy	#$FF	; DURATION ($C0 = .1 SEC)
BOOP:
	lda	#$10	; TONE ($0C = 1 KHZ)
	jsr	WAIT10
	lda	SPKR	; TOGGLE SPEAKER
	dey
	bne	BOOP	
ZSOEX:
	rts		

BEEP:
        lda     RDROM
	jsr	MBELL	; just use system beep
        lda     BNK2SET ; back to bank 2
        lda     BNK2SET ; back to bank 2
        rts
;
; just do the background color - foreground is always white/black
;
;
; ZIPCOLOR - maps ZIP colors to screen colors
;
ZIPCOLOR: db	0,$B,6,7,$C,1,$E,$F
ZCOLOR:
	jsr	CLRBUF		; print out what we have

	lda 	NARGS		; check if window was passed
	cmp	#3		; was it?
	beq	ZCLR0		; ayyup
	lda	CURWIN		; make it current window
	sta	ARG3+LO		; it is now
ZCLR0:
	lda	ARG3+LO		; get window ID
	jsr	SETWJ		; and put pointer into J

	ldx 	ARG2+LO		; get background color
	beq	ZCLR3		; check fore ground color
	bpl	ZCLR01		; not -1
	jsr	GET_NYBBLE	; get me the color nybble here
	jmp	ZCLR11		; and use as background color
ZCLR01:
	dex			; check for default
	bne	ZCLR1		; nope, find the color
	ldx	#1		; use black as default back color
ZCLR1:
	dex			; zero base the color
	lda	ZIPCOLOR,X	; get my color from the zip color
ZCLR11:
	ldy	#WINBGND	; get background offset
	sta	(J),Y		; saved color
	ldx 	ARG1+LO		; get foreground color
	beq	ZCLREX		; no change
	dex			; check for default
	bne	ZCLR3		; nope, find the color
	ldx	#8		; use white as default fore color
ZCLR3:
	dex			; zero base the color
	ldy	#WINFORE	; foreground color offset
	txa			; get into A for fun
	sta	(J),Y		; save in structure
ZCLREX:
	rts

;
; CHKTME RTN - CALLED BY INPUT & ZINPUT
;
CHKTME:	
	LDA	NARGS		; CHECK IF TIME LIMIT
	CMP	#2
	BEQ	CHKT1		; NO
	LDA	ARG3+LO		; GET DELAY WANTED
	STA	I+HI
	LDA	NARGS		; IS THERE A FCN?
	CMP	#4
	BNE	CHKT1		; NO
	LDA	ARG4+LO		; YES, SET IT
	STA	J+LO
	LDA	ARG4+HI
	STA	J+HI
CHKT1:
	RTS			; just set things up, please

WAIT10:
	ldy 	#OUTER_DELAY	; make a delay loop
	lda	#INNER_DELAY
	ldx	MID		; on a GS?
	cpx	#IIgsID		; well?
	bne	WAIT0		; no changes
	ldy	#GS_OUTER_DELAY	; this is special GS loops
	lda	#GS_INNER_DELAY	; inner loop delay for GS
WAIT0:	
	sty	DELAY_COUNTER	; show outer delay loop
WAIT11:
	tay			; pick up inner delay loop
WAIT2:	
	ldx	#0		; inner loop
WAIT3:
	dex
	bne	WAIT3
	dey
	bne	WAIT2
	pha			; save inner delay loop
	lda	MOUSEF		; move mouse cursor?
	beq	WAIT1		; nope
	jsr	MOVE_MC		; move cursor, if necessary
	dec	DELAY_COUNTER	; for all the overhead
WAIT1:
	pla			; get inner delay
	dec 	DELAY_COUNTER
	bpl	WAIT11
	rts
;
; tick the ol timer
;
TIMEK:
	lda	#BLINK_RATE	; how often to blink
	sta	CURCOUNT	; okay!
TIMEST:
	lda	I+LO		; don't reset if not zero
	bne	TIMELOOP	; so keep goin' then
	lda	I+HI
	sta	I+LO
	jsr	STCUR		; start the cursor
TIMELOOP:
	jsr	WAIT10		; wait .10 secs

	jsr	FKEYX		; Check for Keystroke
	bmi	TIME2		; OK, HE'S THERE, CONTINUE

	dec	CURCOUNT	; count down to toggle
	bne	TMCNT		; okay, no blink
	jsr	STCUR		; blink cursor
	lda	#BLINK_RATE	; once per second
	sta	CURCOUNT	; okay!
TMCNT:
	dec	I+LO		; 10TH'S OF SECONDS TO WAIT
	beq	TIMEOUT		; SOME TIME LEFT

	bne	TIMELOOP	; so gwon back and try again! (JMP)
;
; THERE IS A TIME OUT, CHECK FOR A FCN
;
TIMEOUT:
	jsr	ERCUR		; after erasing cursor
	lda	J+HI		; IS THERE A FCN
	beq	TIMEBAD		; NO FCN, LEAVE WITH NOTHING
TIME3:
	jsr	INTCLL		; INTERNALLY CALL THE FCN
	lda	VALUE+LO	; CHECK RESULTS
	beq	TIMEST		; ELSE TRY AGAIN
	bne	TIMEBAD		; else die a horrible death!
TIME2:
	jsr	ERCUR		; after erasing cursor
	clc			; GOT A KEY
	rts
TIMEBAD:
	sec
	rts
;
; display the cursor in the current spot
;
CURSTATE:	db	$80	; blinking cursor state
CURCOUNT:	db	0	; toggle counter

STCUR:
	pha			; save a
	lda	INVFLG		; get current INVFLG
	pha			; save it
	lda	#$80		; make it all be ones
	sta	INVFLG		; and blink

	lda	#SPACE		; space for cursor
	sta	SHOW_CURSOR	; show that we are doing cursor
	jsr	CHAR		; and print it out
	jsr	DISP_LINE	; send it out

	lda	CURSTATE	; get current state
	eor	#$80		; toggle it
	sta	CURSTATE	; save it
	pla			; get invflg
	sta	INVFLG		; restored
	lda	#0		; clear cursor flag
	sta	SHOW_CURSOR	; okay
	
	pla
	rts
;
; just erase the cusor char, but leave cursor in its old place
;
ERCUR:
	pha			; save a
	lda	CURSTATE	; get current state
	bne	ERCURX		; not on, leave alone
	jsr	STCUR		; 'start' it out
ERCURX:
	pla			; retrieve [A]
	rts
;
; timed key input loop
;
; carry set if timed out
; char in [A], if there is one!
TIMIN:
	lda	MOUSEF		; is there a mouse/joystick?
	beq	TIMIN1		; nope
	jsr	MSCON		; turn on mouse cursor
TIMIN1:
	jsr	TIMEK		; check for keystroke
	bcs	TMBAD		; ELSE ABORT
TM1:
	jsr	PARSEKEY	; GET ASCII INTO [A] AND [IOCHAR]
	bcs	TIMIN1		; c==1 means no good char
TMBAD:
	ldx	MOUSEF		; is there a mouse/joystick?
	beq	TIMIN2		; nope
	php			; save status
	pha			; save (possible) char
	jsr	MSCOFF		; turn off mouse cursor
	pla			; get char back
	plp			; get return status
TIMIN2:
	rts			; and away we go
;
; FKEYX - move the mouse cursor if any, then check for keystroke
;
FKEYX:
	lda	MOUSEF		; is there a mouse cursor?
	beq	KEYX		; nope
	bmi	KEYMOUSE	; handle mouse
	jsr	DO_STICK	; handle joystick
	bpl	KEYX		; go look for key still
	bmi	KEYBTN		; do button handling
KEYMOUSE:
	jsr	DO_MOUSE	; handle mouse stuff
	bpl	KEYX		; go look for key still
;
; button hit, so show pos in MSTBL
;
KEYBTN:
	pha			; save status
	lda	MSTBL+LO
	sta	SPCL
	lda	MSTBL+HI
	sta 	SPCH
	lda	MSTBL+ABANK
	sta	SPCBNK
	lda	#ZMSLOCX+1	; point to LSB of mouse x
	jsr	ADDSPC		; add to point
	lda	MSX		; get X
	jsr	STASHB		; and stuff it away
	lda	#2		; 2 more to get to mouse y
	jsr	ADDSPC		; okay
	lda	MSY		; now show the world the y pos
	jsr	STASHB		; and it is away
	pla			; get status back
	tay			; and set it again
	rts			; otherwise, done
KEYX:
	lda	KBD		; check keyboard strobe
	bpl	KEYXX		; nothing
	sta	ANYKEY		; reset strobe
KEYXX:
	rts
;
; MOVE_MC - move the mouse/joystick cursor, if necessary
;
MOVE_MC:
	lda	MOUSEF		; check which kind to move
	bpl	MOVE_MC1	; move joystick cursor
	jsr	CHK_MOUSE	; move the mouse cursor
	jmp 	MOVE_MC2	; and print it
MOVE_MC1:
	jsr	MOVE_STICK	; okay, checked it out
MOVE_MC2
	lda	MSMOVEF		; did it move?
	beq	MOVE_MCX	; nope
	jmp	MSCURS		; print it
MOVE_MCX:
	rts			; done
;
; CHK_MOUSE - check the mouse position and button state, and flag any change
;
CHK_MOUSE:
	sta	PAGE2SW		; make sure we are pointing to main bank
	ldx	#READM		; get me current cursor pos
	jsr	MOUSER		; turn off interrupts and set current pos
MSFIX0	lda	MOUSEST		; get status byte
	tay			; save for a sec
	and	#$20		; moved since last time?
	beq	CHKM1		; nope
	sta	MSMOVEF		; show movement
MSFIX1	lda	MOUSEXL		; get mouse X pos
	sta	MSX		; save new one
MSFIX2	lda	MOUSEYL		; and the y pos
	sta	MSY		; save for me
CHKM1:
	tya			; get status back
	and	#$40		; button still down?
	bne	CHKMX		; yes
	tya			; get status again
	and	#$80		; is the button now down?
	ora	MSBTNF		; so say so
	sta	MSBTNF		; turn it on if necessary
CHKMX:
;	cli			; reenable interrupts now
	rts			; done
;
; there is a mouse, so check it		 
;
DO_MOUSE:
	jsr	CHK_MOUSE	; check the mouse please
; FALL THROUGH TO BUTTON CLICKING HANDLER
;	jmp	DO_BUTTON	; handle button clicking
;
; DO_BUTTON - handle button clicking, working with the timer to
; 	check for double clicking or not
;
DO_BUTTON:
	lda	MSMOVEF		; check moved flag
	beq	DOM01		; nope
	jsr	MSCURS		; move mouse cursor
DOM01:
	lda	#0		; show no char
	ldy	MSBTNF		; check button flag
	beq	DOBX		; none
;
; button strike, check for double click
;
	ldy	CLKCTR		; have we started click counter?
	bpl	DOB02		; nope
	ldy	#1		; this resets counter
	sty	CLKCTR		; reset it
	lda	#DBL_CLK	; show double click char
	bne	DOBEXIT		; and finis
DOB02:
	ldy	#CLK_CNT	; set to double click timeout
	sty	CLKCTR		; okay
	bmi	DOBEXIT		;done
DOBX:
	ldy	CLKCTR		; click counter counting?
	bpl	DOBEXIT		; nope
	inc	CLKCTR		; count it then
	bne	DOBEXIT		; all done
	ldy	#1		; reset counter
	sty	CLKCTR		; okay
	lda	#SGL_CLK	; set as char
DOBEXIT:
	ldy	#0		; clear out flags
	sty	MSBTNF		; button flag
	sty	MSMOVEF		; moved flag
	tay			; set flag to show any char
	rts			; return char	
;
; DO_STICK - handle the joystick 'interrupt'
;
DO_STICK:
	jsr	MOVE_STICK	; first move it
	lda	APKEY1		; get apple/button flag
	bmi	DOST5		; button click
	lda	#0		; show no button click
DOST5:
	sta	MSBTNF		; show the click
	jmp	DO_BUTTON	; now handle it
MOVE_STICK:
	ldx	#0		; get horizontal change
	jsr	READ_STICK	; puts value in Y
	cpy	#80		; if < 80, then jump to the left
	bcs	DOST1		; it is not
	lda	MSX		; make sure X is > 0
	beq	DOST2		; it is == 0, can't get smaller
	sta	MSMOVEF		; show movement
	dec	MSX		; move one to the left
	dec	MSX		; move two to the left
	bne	DOST2		; now check vertical
DOST1:
	cpy	#180		; if > 160, then move right
	bcc	DOST2		; nope
	lda	MSX		; make sure X is in bounds
	cmp	#MAXWIDTH-4	; don't go too far
	bcs	DOST2		; already maxxed out
	sty	MSMOVEF		; show movement
	inc	MSX		; one step to the left
	inc	MSX		; and another one
DOST2:
	ldx	#1		; now check vertical
	jsr	READ_STICK	; ask the monitor
	cpy	#80		; if < 80, move up
	bcs	DOST3		; nope
	lda	MSY		; don't go negative
	beq	DOST4		; already minned out
	sta	MSMOVEF		; show movement
	dec	MSY		; count down
	dec	MSY		; twice
	bne	DOST4		; all done
DOST3:
	cpy	#180		; check for downward motion
	bcc	DOST4		; none
	lda	MSY		; check for maximum
	cmp	#MAXHEIGHT-3	; don't go below water
	bcs	DOST4		; gone, thanx
	sty	MSMOVEF		; show movement
	inc	MSY		; go further down
	inc	MSY		; twice as far for joystick
DOST4:
	rts			; done
READ_STICK:
	txa			; save which stick we want
	ldy	#6		; another delay loop
READST1:
	ldx	#0
READST2:
	dex
	bne	READST2
	dey
	bne	READST1
	dec	DELAY_COUNTER	; show outside loop what we be doing
	dec	DELAY_COUNTER	; show outside loop what we be doing
	tax			; get x back
	jmp	MPREAD		; NOW do the read
; ----------------------------
; FETCH ASCII KEYCODE INTO [A]
; ----------------------------
; EXIT: ASCII IN [A] & [IOCHAR]

GETKEY:
	lda	#BLINK_RATE	; flag as wait for good key
	sta	CURCOUNT	; clear blinker
	lda	MOUSEF		; is there a mouse/joystick?
	beq	GTK0		; nope
	jsr	MSCON		; turn on mouse cursor
GTK0:
	lda	#0		; clear line counter
	sta	LINCNT		; okay, we did
	txa			; SAVE [X] & [Y]
	pha		
	tya		
	pha		
GKEY0:
	inc	RNUM1		; just whack on random number
	dec	RNUM2		; and more
	jsr	FKEYX		; is there a key?
	bmi	GKEY01		; got the key

	jsr	WAIT10		; wait .1 seconds, moving mouse cursor

	dec	CURCOUNT	; down one
	bne	GKEY0		; no toggle
	jsr	STCUR		; okay, toggle
	lda	#BLINK_RATE	; 1 per second
	sta	CURCOUNT	; okay
	bne	GKEY0		; check for key
GKEY01:
	pha			; save char
	lda	MOUSEF		; any mouse cursor?
	beq	GTK1		; nope
	jsr	MSCOFF		; turn mouse cursor off
GTK1:
	pla			; get char back
	jsr	ERCUR		; so erase cursor
CHKKEY:
	jsr	PARSEKEY	; how was the key?
	bcs	GKEY0		;TRY AGAIN
	sta	IOCHAR		;HOLD ON TO IT
	adc	RNUM1		;FUTZ WITH RANDOM
	sta	RNUM1	
	eor	RNUM2	
	sta	RNUM2	
	pla			; RESTORE
	tay			; EVERYTHING
	pla		
	tax		
	lda	IOCHAR		; GET CHAR INTO [A]
	rts			; AND RETURN IT

;
; CHECK TO MAKE SURE KEY IS VALID, ONLY ACCEPT IT IF IT IS
;
PARSEKEY:
	and	#$7F	;SCREEN OUT SHIFTS
;
;CHECK FOR "ARROWS" & FUNCTION KEYS (X),  CONVERT FOR USE (EZIP)
;ALSO : CHANGE <_>)@%^&*( TO ,-.0256789 - and 'mouse' clicks
; and other kinds of special chars
;
GK0:
	ldx	#ENDKEY ; GET LENGTH OF LIST
GK2:
	cmp	HAVE,X	; CHECK AGAINST LIST OF UNWANTED KEYS
	beq	GK3	; FOUND IT
	dex
	bpl	GK2	; CHECK THEM ALL
	bmi	GK4	; NOT FOUND, CONTINUE OTHER CHECKS
GK3:
	lda	WANT,X	; GET KEY TO USE INSTEAD
	clc		; show niceness
	rts		; done
GK4:
	cmp	#SPACE	; NO CTRL CHARS ACCEPTABLE
	bcc	BADKEY	; IF < SPACE, BAD

	cmp	#'<'	; pick up numbers and most punctuation
	bcc	OK	; we did

	cmp	#'z'+1	;PICK OUT LETTERS NOW
	bcs	BADKEY	;IF > BAD
	cmp	#'a'
	bcs	OK	;IF > OK
	cmp	#'A'
	bcc	BADKEY
	CMP	#'Z'+1	
	BCC	OK	;IF < OK
BADKEY:
	jsr	BEEP	;BAD KEY, GIVE WARNING NOISE, gwon back
	sec		; show badness
	rts		; and done
OK:
	cmp	#'0'	; check for number keys
	bcc	OKj	; nope, < 0
	cmp	#'9'+1	; more than a nine?
	bcs	OKj	; ayyup
;
; here we check for the closed apple key being down too
;
	ldx	APKEY2	; how about the closed apple key
	bpl	OKj	; not pressed,	so use as number key
;
; transform number key into a function key
;
	CLC		; get ready for add
	ADC	#84	; transforms '1'-'9' to 133-141
	CMP	#132	; but '0' wants to be a 142!
	BNE 	OKj	; but it's not it
	CLC		; again, don't want carry
	ADC	#10	; voila!
OKj:
	clc		; show a wicked good character is about to arrive
	rts		; toots finis

HAVE:	DB	$0B,$0A,$08,$15,$7f,ESCAPE,$3C,$7C,$3F
	DB	$3C,$5F,$3E,$40,$25,$5E,$26,$01,$02, EOL
WANT:	DB	129,130,131,132,08,ESCAPE,$3C,$7C,$3F
	DB	$2C,$2D,$2E,$32,$35,$36,$37,254,253, EOL
ENDKEY	EQU	$-WANT-1

	END


