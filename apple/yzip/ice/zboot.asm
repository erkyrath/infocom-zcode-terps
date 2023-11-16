	TITLE	"Apple ][ YZIP (c)Infocom","ZIP CODE BOOT PART"
ZBOOT:
;
; clear out ZERO page stuff
;
	ldy	MID		; save machine id!
	ldx	#LASTWV-1	; start at end of first part
	lda	#0		; and clear to  zero
ZEROL:
	sta	0,X		; clear out zero page loc
	dex			; next loc
	bpl	ZEROL		; and do next one
	ldx	#ZEROPG		; now do other part
ZEROL1:
	sta	0,X		; zero this one
	inx			; point to next one
	cpx	#LASTZP		; find the last
	bne	ZEROL1		; okay, so do it

	sty	MID		; restore machine ID
;
; get game code from data and set up absolute pointers
;
	lda	ZBEGIN+ZVERS	; IS GAME A YZIP?
	cmp	#6		; (X)
	beq	YESEZ		; YES, CONTINUE

; *** ERROR #15 -- NOT AN EZIP GAME ***
	lda	#15	
	ldy	#0
	jmp	ZERROR 	
YESEZ:
	lda	#%00111011	; ENABLE SOUND, underline, MONOSPACING (X)
	sta	ZBEGIN+ZMODE

	lda	MID		; get machine id!
	sta	ZBEGIN+ZINTWD	
	lda	#0		; CLEAR HIGH BYTE
	sta	MOUSEF		; init mouse flag to no mouse
	sta	ZBEGIN+ZHWRD
	sta	ZBEGIN+ZVWRD
	lda	#MAXWIDTH	; SET SCREEN PARAMETERS
	sta	ZBEGIN+ZHWRD+1
	lda	#MAXWIDTH/3 	; number of mono spaces on a line
	sta	ZBEGIN+ZSCRWD+1	
	lda	#MAXHEIGHT
	sta	ZBEGIN+ZVWRD+1
	lda	#MAXHEIGHT/FONT_H
	sta	ZBEGIN+ZSCRWD	 ; number of lines of chars
	lda	#FONT_H		; height of font
	sta	ZBEGIN+ZFWRD
	lda	#3		; width of font (2 spaces == 8 pixels)
	sta	ZBEGIN+ZFWRD+1
	lda	#9		; the color white is the foreground color
	sta	ZBEGIN+ZCLRWD+1	; show Z game too
	lda	#2		; black is the background color
	sta	ZBEGIN+ZCLRWD	; tell game about it
;
; just move global address to zero page for quick working
;
	lda	ZBEGIN+ZGLOBAL	; get page
	sta	GLOBAL+HI	; save which page
	lda	ZBEGIN+ZGLOBAL+1 ; LSB NEEDN'T CHANGE
	sta	GLOBAL+LO	; so just store it away
;
; figger absolute address for the fword table
;
	lda	ZBEGIN+ZFWORD	; get fword page number
	jsr	SETPC		; and get absolute address
	sta	FWORDS+HI	; show high addr of table
	sty	FWORDS+ABANK	; show which bank
	lda	ZBEGIN+ZFWORD+1	; LSB NEEDN'T CHANGE
	sta	FWORDS+LO	; so just save it
;
; figger absolute address for the vocab table
;
	lda	ZBEGIN+ZVOCAB	; get fword page number
	jsr	SETPC		; and get absolute address
	sta	VOCAB+HI	; show high addr of table
	sty	VOCAB+ABANK	; show which bank
	lda	#0		; this is always zero (get lo part from MPCL)
	sta	VOCAB+LO	; so just save it
;
; now do somethin' with the TCHAR table (maybe, if <> 0)
;
	lda	ZBEGIN+ZTCHAR	; DO SAME FOR TCHARS TABLE
	ora	ZBEGIN+ZTCHAR+1	; is it zero though?
	bne	TCH1		; no, so copy it to somewhere useful
	sta	TCHARTBL	; not there, so zero first byte in table
	beq	TCHj		; jmp
TCH1:
	lda	ZBEGIN+ZTCHAR	; DO SAME FOR TCHARS TABLE
	jsr	SETPC		; and now make absolute
	sta	FPCH		; Save in FPC
	sty	FPCBNK
	lda	ZBEGIN+ZTCHAR+1	; NO CHANGE FOR LSB
	sta	FPCL		; now move pointer to fetch spot

	lda	#0		; and set index
	sta	I		; thank you
TCHLP:
	jsr	FETCHB		; get the byte in [a]
	ldx	I		; get offset
	inc	I		; and point to next one
	sta	TCHARTBL,X	; save in lower memory, thank you
	cmp	#0		; are we done yet?
	beq	TCHj		; NULL TERMINATED STRING
	cmp	#$FF		; $ff means all >128 chars are terminators
	bne	TCHLNEXT	; nope
	lda	#1		; yes,
	sta	ALLFLG		; so set flag to say so
TCHLNEXT:
	jsr	NEXTFPC		; point to next one
	jmp	TCHLP		; and go get it
TCHj:
	jsr	DO_DSEGS	; set up disk segment pointers
;
; use mouse/joystick, see if either is connected
;
	lda	ZBEGIN+ZFLAGS+1	; get game flags
	and	#FMOUS		; uses a mouse?
	beq	ZBEND		; nope, so don't ask
	jsr	MHOME		; clear & home
MSLOOP:
	DLINE	MMSG, 		; ask about Mouse/Joystick/Keyboard
	jsr	MRDKEY		; get a key please
	jsr	MCOUT		; put key out there
	and	#$7F		; turn off hi bit
	cmp	#'M'		; mouse?
	beq	DO_MOUSE	; yup
	cmp	#'m'
	beq	DO_MOUSE
	cmp	#'J'		; Joystick?
	beq	DO_STICK	; yup
	cmp	#'j'
	beq	DO_STICK
	cmp	#'N'		; neither
	beq	DO_NEITHER	; yup
	cmp	#'n'
	beq	DO_NEITHER
MSLOOP1:
	jsr	MBELL		; complain
	jmp	MSLOOP		; and do again
DO_MOUSE:
	jsr	INST_MOUSE	; install mouse handlers
	bcs	MSLOOP1		; problems, try again
	bcc	ZBEND		; continue
DO_STICK:
	jsr	INST_STICK	; install joystick handler
	bcs	MSLOOP1		; try again
	bcc	ZBEND		; continue, please
DO_NEITHER:
ZBEND:
;
;
; Multiply ZFOFF & ZSOFF * 8 to get actual offset
;
	lda	ZBEGIN+ZFOFF	; hi part
	sta	FOFFM		; this'll be the middle part
	lda	ZBEGIN+ZFOFF+1	; lo part
	asl	A		; *2
	rol	FOFFM
	rol	FOFFH
	asl	A		; *4
	rol	FOFFM
	rol	FOFFH
	asl	A		; *8
	rol	FOFFM
	rol	FOFFH
	sta	FOFFL		; lo part here too
;
; now do string offset
;
	lda	ZBEGIN+ZSOFF	; hi part
	sta	SOFFM		; this'll be the middle part
	lda	ZBEGIN+ZSOFF+1	; lo part
	asl	A		; *2
	rol	SOFFM
	rol	SOFFH
	asl	A		; *4
	rol	SOFFM
	rol	SOFFH
	asl	A		; *8
	rol	SOFFM
	rol	SOFFH
	sta	SOFFL		; lo part here too

	lda	ZBEGIN+ZGO	; GET START ADDRESS OF Z-CODE
	sta	ZPCM		; MSB
	lda	ZBEGIN+ZGO+1	; AND LSB
	asl	A		; *2
	rol	ZPCM	
	rol	ZPCH	
	asl	A		; *4
	rol	ZPCM
	rol	ZPCH	
;
; now add offset
;
	clc			; doing adding
	adc	FOFFL		; add in lo part
	sta	ZPCL
	lda	ZPCM
	adc	FOFFM
	sta	ZPCM
	lda	ZPCH
	adc	FOFFH
	sta	ZPCH
;
; now, set up TBLPUR, FUNPRE, and FUNPUR
;
	lda	SEGTBL+SGTTBLE+1	; get last page of table preload
	asl	A		; make 256K page
	sta	TBLPUR		; show last pre table
	inc	TBLPUR		; but we want first pure table address
	inc	TBLPUR		; so point to it
	lda	SEGTBL+SGTFUNS+1	; get first page of function preload
	asl	A		; make 256K page
	sta	FUNPRE		; show me
	lda	SEGTBL+SGTFUNE+1	; last page of function preload
	asl	A		; make 256K page
	sta	FUNPUR		; show last pre function
	inc	FUNPUR		; but we want first pure function
	inc	FUNPUR		; now we point to it
	
	lda	TBLPUR		; now figger out negative number to
	sec			; add to function preload addres to
	sbc	FUNPRE		; get page offset in memory
	sta	FUNPGE		; set up offset
;
; and now to set up extension table pointer
;
	lda 	ZBEGIN+ZEXTAB+1	; LSB of table
	sta	MSTBL+LO	; stays the same
	lda	ZBEGIN+ZEXTAB	; MSB of table
	jsr	SETPC		; get me the memory addres
	sty	MSTBL+ABANK	; save bank
	sta	MSTBL+HI	; and page
	
	jmp	INITPAG		; set up paging system

	END
