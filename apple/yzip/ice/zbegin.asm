	TITLE	"Apple ][ YZIP (c)Infocom","--- START OF APPLE ][ INTERPRETER ---"

	ORG	INTR_BEGIN
;
; Set up for Double HiRes full screen graphics
;	
	sta	PAGE2SW+OFF	; turn off page 2
	sta	TEXTSW+OFF	; turn off text - turn on graphics
	sta	HIRESSW+ON	; turn on high resolution
	sta	MIXEDSW+OFF	; full screen graphics
	sta	COL80SW+ON	; turn on 80 column
	sta	IOUDIS+OFF	; turn ON (it too is backwards) Dhires switch
	sta	DHIRESW+OFF	; turn ON (it's backwards!) Dhires

	sta	STORE80+ON	; turn on aux page display ability
        lda     BNK2SET         ; read/write RAM, bank 2
        lda     BNK2SET
;
; copy in progame from boot code
;
	ldx	#0		; start at first letter
MVPRE:
	lda	BORG+3,X	; get letter
	beq	MVPREX		; all done
	sta	GAME1NAME,X	; save letter
	sta	GAME2NAME,X	; save letter
	sta	GAME,X		; save for asking about later
	inx			; next letter
	bne	MVPRE		; do gen
MVPREX:
	stx	GAMEL		; save length of game name
	lda	#'.'		; get extension for names
	sta	GAME1NAME,X	; save letter
	sta	GAME2NAME,X	; save letter
	inx			; next letter
	lda	#'D'		; for data segments
	sta	GAME1NAME,X	; save letter
	sta	GAME2NAME,X	; save letter
	inx			; points to number
	inx			; inclusive count for length
	stx	GAME1NML	; save length of name
	stx	GAME2NML	; save length of name

	lda	#VERSID		; put in version number
	sta	ZBEGIN+ZINTWD+1	

	inc	ZSP+LO		; INIT Z-STACK POINTERS
	inc	OLDZSP+LO	; TO "1"
	inc	SCREENF		; TURN DISPLAY ON
	inc	SCRIPT		; enable scripting

	lda	WINTABLE+LO	; set WINDOW to point to window 0
	sta	WINDOW+LO
	lda	WINTABLE+HI
	sta	WINDOW+HI	; okay, it does

	lda	ARG1+LO		; using mouse?
	beq	ZBEGIN1		; nope
	ora	#$78		; point to correct screen holes
	sta	MSFIX0+1
	sta	MSFIX1+1	; and modify code to point to correct spot
	sta	MSFIX2+1
	lda	ARG1+LO
	ora	#$F8		; and one more
	sta	MSFIX2+1
ZBEGIN1:
	lda	#$FF		; do a clear -1 to start off
	sta	ARG1+LO		; so arg 1 is this
	jsr	ZCLR		; doing it

	GET_PREFIX GPRE_PB	; get where we are to start

	lda	D3SEG+HI	; start DSEGS at .D2
	sta	DSEGS+HI
	lda	D3SEG+LO
	sta	DSEGS+LO

	lda	#1		; open game file .D2 please
	jsr	FETCH_FILE	; we did that
        lda     INFODOS         ; are we on little dos?
        beq     ZBEGIN2         ; nope
        jsr     GET_SPECIAL     ; do special preloading if so
ZBEGIN2:
	lda	#2		; and just for giggles, do the
	jsr	FETCH_FILE	; same for game file .D3

	jsr	VLDZPC		; MAKE ZPC VALID
	jsr	NEXTPC		; skip over # of locals

	; ... AND FALL INTO MAIN LOOP

	END


