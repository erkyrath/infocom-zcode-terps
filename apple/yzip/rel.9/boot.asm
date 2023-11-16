	TITLE	"APPLE YZIP (c)Infocom, Inc.. --- BOOT.SYSTEM"

	INCLUDE 	ZIP.EQU
	INCLUDE		ZERO.EQU
	INCLUDE		PRODOS.EQU
	INCLUDE 	APPLE.EQU
	INCLUDE		MACROS.ASM

	STTL	"--- APPLE ProDOS BOOT CODE ---"
	PAGE

CHZ	EQU	$24	; CURSOR HORIZONTAL
EHZ	EQU	$57B	; CURSOR HORIZONTAL
CVT	EQU	$25	; CURSOR VERTICAL

;
; my own little error checking macro
;
ERRVAL	.VAR	0
;
; this macros checks for carry being set (ProDOS error) and if it is
; loads an error code in x and jumps to the error handler
ERRCHK .MACRO
ERRVAL	.VAR	ERRVAL+1
	bcc	BOOT|<ERRVAL>
	ldy	#ERRVAL	
	jmp	BOOT_ERROR
BOOT|<ERRVAL>:
	.MACEND

	ORG	BORG	
	jmp	BOOT_BEGIN	; just jump to beginning
PROGNAME:
	BLKB	9,0		; 8 bytes for name + 1 for ending zero
BOOT_BEGIN:
        lda     RDROM           ; swap in rom
	sta	TEXTSW+ON	; turn on text
	sta	COL80SW+OFF	; turn on 80 column

;
; make sure we are on a ][c, or ][e+
;
	jsr	MACHINE		; check it out, returns if good
	lda	ARG2+LO		; check machine
	cmp	#IIgsID		; 2gs?
	bne	ZBOOTGS		; nope
        lda     IIGSVID         ; get current video setting
        and     #$1F            ; turn off upper 3 bits
        sta     IIGSVID         ; make sure double hires works
        lda     IIGSSHD         ; get shadowing reg
        and     #$A0            ; turn on shadowing everywhere
        sta     IIGSSHD         ; okay
ZBOOTGS:
	jsr	FIXRAM		; check and clear RAM disk
        jsr     DO_PREFIX       ; go set my current prefix

	jsr	MHOME		; clear and home
	lda	#9		; CENTER DISPLAY
	sta	CVT	
	lda	#8
	sta	CHZ
        sta     EHZ
	jsr	MBASCAL		; move cursor there!	
	DLINE	STRYM
;
; move down to lower left and print out version number
;
        lda     #21
        sta     CVT
        lda     #1
        sta     CHZ             ; move to 1,20
        sta     EHZ
	jsr	MBASCAL		; move cursor there!	
        DLINE   VERMSG          ; show me the version
        lda     #VERSID/10      ; get version
        ora     #$B0            ; make normal char for ][
        jsr     MCOUT
        lda     #VERSID.MOD.10
        ora     #$B0            ; make normal char for ][
        jsr     MCOUT

	lda	#$FF		; init invflg to normal
	sta	INVFLG		; okay

	lda	#0		; zero out bit map
	ldx	#$17		; 17 bytes in ProDOS system bitmap
CLRPRD:
	sta	PR_BITMAP,X	; and zero out the memory
	dex
	bpl	CLRPRD		; and zero entire bitmap

	lda	#$C0		; start out the bitmap with c0 at start
	sta	PR_BITMAP
	lda	#$01		; and a 1 at the end
	sta	PR_BITMAP+$17	; thankx
;
; now we need to load in the interpreter
;
	OPEN	INTR_OPEN	; open up interpreter file
	ERRCHK

	lda	INTR_OPEN+OP_REFNUM 	; get refnum for intr file
	sta	READ_BLOCK+RD_REFNUM 	; and save for read
	sta	CLOSE_BLOCK+CL_REFNUM 	; close up interpreter file

        lda     #RETRY_COUNT    ; set retry
        sta     RETRIES         ; just use this for now
RDINTR:
	READ	READ_BLOCK	; read in interpreter
        bcc     RDINTR1         ; it worked fine
        dec     RETRIES         ; try again
        bpl     RDINTR          ; fine
        ERRCHK                  ; then just die
RDINTR1:
	CLOSE	CLOSE_BLOCK	; done with interpreter
;
; now read in first part of preload
;
	ldx	#0		; start at first letter
MVPRE:
	lda	PROGNAME,X	; get letter
	beq	MVPREX		; all done
	sta	PRE_NAME,X	; save letter
	inx			; next letter
	bne	MVPRE		; do gen
MVPREX:
;
; now add on ".d1" suffix for the preload
;
	lda	#'.'		; get '.'
	sta	PRE_NAME,X	; save it
	inx			; next char
	lda	#'D'		; 'D' char
	sta	PRE_NAME,X	; save it
	inx			; next char
	lda	#'1'		; preload is .1
	sta	PRE_NAME,X	; saved
	inx			; count the .1
	stx	PRELOAD_NAME	; save length of name

	OPEN	PRELOAD_OPEN	; so now open preload file
	ERRCHK

	lda	PRELOAD_OPEN+OP_REFNUM	; get refnum
	sta	READ_BLOCK+RD_REFNUM 	; save refnum for read
	sta	PSEEK+SM_REFNUM		; and for moving around
;
; first, get the segment table
;
	lda	#>IOBUFF	; just read in size
	sta	READ_BLOCK+RD_BUFFER+HI ; and where to begin next read
	lda	#4              ; read in first 1 Kb
	sta	READ_BLOCK+RD_BUFFLEN+HI ; to show how much to read
	lda	#0		; get size in words
	sta	READ_BLOCK+RD_BUFFLEN+LO ; to show how much to read
		
	READ	READ_BLOCK	; and read first block
	ERRCHK
;
; now copy segtable to low mem
;
        lda     #>IOBUFF
        sta     J+HI
        lda     #<IOBUFF
        sta     J+LO
        lda     #>(SEGTBL-2)
        sta     K+HI
        lda     #<(SEGTBL-2)
        sta     K+LO

        lda     IOBUFF+1        ; get LSB
        asl     A               ; *2, to pick up the carry
        lda     IOBUFF          ; get how many words
        rol     A               ; *2 for number of bytes
	sta	PSEEK+SM_FPOS+HI	; set page number
        tax                     ; make it a counter
        ldy     #2              ; start at second byte, after length
CPSEGT:
        lda     (J),y           ; get byte
        sta     (K),y           ; save byte
        iny                     ; next byte
        bne     CPSEGT          ; go do it
        inc     J+HI
        inc     K+HI
        ldy     #0              ; restart at beginning of block
        dex                     ; count page
        bpl     CPSEGT          ; next page
;
; now, seek to page following seg table
;
	inc	PSEEK+SM_FPOS+HI	; next page
	lda	PSEEK+SM_FPOS+HI	; make sure it is an even 512 page
	and	#$01			; is it odd?
	beq	BTSK0			; nope, so we be okay
	inc	PSEEK+SM_FPOS+HI	; next page
        lda     #RETRY_COUNT    ; set retry
        sta     RETRIES         ; just use this for now
BTSK0:
	SET_MARK PSEEK		; and point to beginning of game data

	lda	#>ZBEGIN	; begining of game code
	sta	READ_BLOCK+RD_BUFFER+HI ; and where to begin next read
	lda	#(Z1SIZE&$FE)	; get size
	sta	READ_BLOCK+RD_BUFFLEN+HI ; to show how much to read
	lda	#0		; get size in words
	sta	READ_BLOCK+RD_BUFFLEN+LO ; to show how much to read
	READ	READ_BLOCK	; and read in part 1 of preload
        bcc     BTSK00          ; just fine
        dec     RETRIES         ; try again
        bpl     BTSK0           ; fine
        ERRCHK                  ; then just die
BTSK00:
	lda	#4			; 4 pages per read
	sta	READ_BLOCK+RD_BUFFLEN+HI ; and lots
	lda	#>IOBUFF		; read into my io buffer
	sta	READ_BLOCK+RD_BUFFER+HI ; hi part of address

        IF      Z1SIZE & 1      ; if odd, do something special
        READ    READ_BLOCK
        ldy     #0              ; copy second page
C2LOOP:
        lda     IOBUFF,Y
        sta     ZBEGIN+(Z1SIZE*$100)-$100,Y
        iny
        bne     C2LOOP
        lda     #(>Z2BEGIN-1)     ; don't need first block
        sta     MODOP+2
        bne     RD10            ; skip that first read
        ENDIF
AUXRDL:
        lda     #RETRY_COUNT    ; set retry
        sta     RETRIES         ; just use this for now
AUXRDL0:
	READ	READ_BLOCK	; and do the read
	bcc	RD10
	cmp	#$4C		; this just means EOF already
	beq	BT12		; otherwise, blech
        dec     RETRIES         ; try again
        bpl     AUXRDL0         ; fine
        ERRCHK                  ; then just die
RD10:
	lda	#>IOBUFF	; read into my io buffer
	sta	CLOOP+2		; start at IOBUFF
	lda	#4		; 4 copies
	sta	L		; okay
RD11:
	ldy	#0		; start at zero, of course
	lda	MODOP+2		; check where we are reading too
	beq	BT12		; wrapped, so we had better be done
	cmp	#>PRGLBL	; done with aux mem, part 1?
	bcc	LOOPST		; nope
	bne	RD12		; no need to change
	lda	#Z3PAGE		; restart at top of memory
	sta	MODOP+2		; and do some self modifying code
RD12:
	sta	ALTZP+AUX	; turn on alt ZP
	lda	BNK2SET		; write to upper mem
	lda	BNK2SET		; write to upper mem
LOOPST:
	sta	WRTBNK+AUX	; always aux mem
CLOOP:
	lda	IOBUFF,Y	; get char
MODOP:	sta	Z2BEGIN,Y	; save
	iny			; point to next one
	bne	CLOOP		; get next one

	lda	RDROM		; swap back rom, just in case
	lda	RDROM		; swap back rom, just in case
	sta	ALTZP+MAIN	; and back to main page
	sta	WRTBNK+MAIN	; read into main bank

	inc	MODOP+2		; point to next page
	inc	CLOOP+2		; point to next page
	dec	L		; next page
	bne	RD11		; and get it

	jmp	AUXRDL		; get next 1k
BT12:
	lda	READ_BLOCK+RD_REFNUM	; get refnum for
	sta	CLOSE_BLOCK+CL_REFNUM	; closing the file
	CLOSE	CLOSE_BLOCK		; okay, closed
;
; relocate the Zero Page routines 
;
	ldx	#RLCLEN			; length of routine
RLOC:
	lda	FETCHBx,X		; get byte
	sta	ZERO_FB,X		; save byte
	dex				; get next one
	bpl	RLOC			; thank you
;
; and now for the copying screen line sub
;
	ldx	#CPYLEN
CPYCPLP:
	lda	CPYx,X
	sta	COPY_LINE,X
	dex
	bpl	CPYCPLP
;
; now relocate page 3 stuff
;
	ldx	#PAGE3RELL+1	; get length of page 3 stuff
MSRCL:
	lda	PAGE3REL-1,X	; get byte
	sta	PAGE3STUFF-1,X	; save byte
	dex			; count byte
	bne	MSRCL		; get next?

	jsr	ZBOOT			; set up ZIP stuff

	JMP	INTR_BEGIN		; jump to the interpreter

	INCLUDE	ZBOOT.ASM		; ZBOOT subroutine
	INCLUDE	BREL.ASM		; relocated subroutines
	INCLUDE	BSUBS.ASM		; subs for e to use

	PAGE
	STTL	"--- BOOT DATA STRUCTURES ---"
INTR_OPEN:
	DB	3		; 3 parameters
	DW	INTR_NAME	; name of file
	DW	GAME1FIO	; file data buffer
	DS	1		; refnum goes here
CLOSE_BLOCK:
	DB	1		; 1 parameter
	DS	2		; room for ref num
PSEEK:	
	db	2	; 2 pararmeters
	db	0	; refnum
	db 	0,0,0	; 3 byte new file pos
PRELOAD_OPEN:
	DB	3
	DW	PRELOAD_NAME
	DW	GAME1FIO	; close interpreter first!
	DS	1		; refnum here
;
; start with interpreter data
;
READ_BLOCK:
	DB	4		; 4 parms
	DB	0		; refnum
	DW	INTR_BEGIN	; where to start the interpreter
	DW	INTR_SIZE	; and how big could it be
	DW	0		; actual length of interpreter
INTR_NAME:	
	DB	INAME_LEN	; length of name
I_NAME:
	DB	"INFOCOM"	; interpreter name
INAME_LEN EQU	$-I_NAME

PRELOAD_NAME:
	DB	0		; length of name
PRE_NAME:
	DS	20		; spot for preload name
PRELEN	EQU	$-PRE_NAME
	
DH_SAVE:			; to actual 'write' the data
	DB	4		; 4 parms for write
	DB	0		; refnum goes here
	DW	0		; just 'copy' junk
	DW	RAMSAVE		; to preserve this much mem in AUX
	DW	0		; how much is done
		
STRYM:	DB	"The story is loading ..."
STRYML	EQU	$-STRYM
MMSG:	DB	EOL,"Enter the letter of the pointing",EOL
	DB	"device you will be using:",EOL
	DB	"J)oystick  M)ouse  N)either",EOL,EOL
MMSGL	EQU	$-MMSG
MSLT:	DB	EOL,"Enter the slot number in which the",EOL
	db	"mouse card is installed >"
MSLTL	EQU	$-MSLT
MSLTBAD: DB	EOL,"Slot number must be between",EOL
	db	"ONE and SEVEN!  Try again!",EOL
MSLTBADL EQU	$-MSLTBAD
VERMSG: db      EOL,"Version: "
VERMSGL equ     $-VERMSG
	END

