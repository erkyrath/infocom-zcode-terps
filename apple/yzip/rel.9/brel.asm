	STTL	"--- RELOCATED SUBROUTINES ---"
	PAGE

;
; FETCHB - use FPC to get a byte in either aux or main mem
;
FETCHBx:
	sta	RDBNK,Y		; set bank, lower
	ldy	#0		; gotta use Y
	lda	(FPC),Y		; so go get it
	sta	RDBNK		; go back to main bank
	rts			; and return
;
; ZFETCH - fetch byte at (ZPCPNT),Y - offset from (ZPCPNT)
;
ZFETCHx:
	sta	RDBNK,X		; set lower bank
	lda	(ZPCPNT),Y	; so go get it
	sta	RDBNK		; go back to main bank
	rts			; bye
;
; MFETCH - fetch byte at (MPCPNT),Y - offset from (MPCPNT)
;
MFETCHx:
	sta	RDBNK,X		; set bank
	lda	(MPCPNT),Y	; so go get it
	sta	RDBNK		; go back to main bank
	rts			; bye
;
; SAVE_DATA - copy data from [DBUFF] to IOBUFF
;	[Y] must == 0;  [X] == DSKBNK; [A] == IOBUFF ($8 or $9)
;
	sta	RDBNK,X		; and select that bank for reading
        sta     SAVE_DATA+9     ; show which part of iobuff
SDAT1:
	lda	(DBUFF),Y	; get me that byte
SDAT2:
	sta	IOBUFF,Y	; MODIFIED ABOVE
	iny		
	bne	SDAT1
	sta	RDBNK+MAIN	; and back to main
	rts
RLCLEN	EQU	$-FETCHBx-1	; mark length
;
; COPY_LINE:
;	SPC - source pointer
;	FPC - destination pointer
;	SPCBNK - bank the above two are talking about
;       CPY_COUNT - end-start bytes to move
;	STMASK - mask to use on first byte (0 if none)
;	ENDMASK - mask to use for end byte (0 if none)
;
; MODIFIED CODE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;  The definitions for CPY_MODs are HARD CODED in apple.equ - they MUST
;    match the definitions below for similar xCPY_MODs!!!!!!!!
;               DON'T CHANGE WITHOUT FIXING THOSE OFFSETS IN APPLE.EQU!!!!!
;
CPYx	EQU	*
	ldx	SPCBNK		; set up banks for first byte
	lda	PAGE2SW,X	; r/w which bank in display mem
	ldy	#0		; use Y reg
	lda	STMASK		; get start mask
	beq	CPLOOP		; no start mask, start in middle of loop
	and	(FPC),Y		; turn off changing bits, keep good bits
	sta	(FPC),Y		; save it
	lda	STMASK		; get changing bits
	eor	#$FF		; turn on changing bits
	and	(SPC),Y		; get SPC byte
	ora	(FPC),Y		; turn them on in destination too
	sta	(FPC),Y		; and destiny
CPLOOP:
	lda	CPY_COUNT	; get count
	lsr	A		; /2 for number in this bank
	tax			; put in X for indexing
	beq	CPLP2		; then done!
        dex                     ; zero base the number
	lda	STMASK		; special start?
	beq	CPL1		; nope
	inc     CPY_MOD1_SRC	; first one already done
        inc     CPY_MOD1_DST    ; point to next one
        dex                     ; and count first one as done
CPL1:
        lda     SCREEN,X	; get byte
	sta	SCREEN,X	; store byte
	dex			; count down
	bpl	CPL1		; and do it all again
CPLP2:
	ldy	#0		; reset y
	lda	SPCBNK		; get current bank
	eor	#1		; toggle to point to the other one
	tax
	lda	PAGE2SW,X
	txa			; which bank?
	beq	CPLP21		; we are now in main, things are fine
        inc     CPY_MOD2_SRC    ; start one more if now in AUX
        inc     CPY_MOD2_DST
        dex                     ; and count as done
CPLP21:
	lda	CPY_COUNT	; get count again
	lsr	A		; /2 for bytes in this bank
	tax			; put in X for counter
	beq	CPDONE		; nothing here
        dex                     ; zero base the number
CPL2:
        lda	SCREEN,X	; get byte
	sta	SCREEN,X	; store byte
	dex			; count byte
	bpl	CPL2		; and swing by again	
CPDONE:
	lda	ENDMASK		; do same mucking as when starting
	beq	CPEXIT		; no mucking
	pha			; save endmask
	lda	CPY_COUNT	; get how many bytes being copied
        lsr     A               ; /2
	bcs	CPDONE1		; if odd, then we are pointing to correct
	pha			; save a
	ldx	SPCBNK		; get original bank
	lda	PAGE2SW,X	; and point to it
	pla			; get it back
CPDONE1:
        tay                     ; get offset
	pla			; get end mask back
	eor	#$FF		; turn on all off bits
	and	(FPC),Y		; turn off changing bits
	sta	(FPC),Y		; save it
	lda	(SPC),Y		; get source byte
	and	ENDMASK		; turn on changing bits
	ora	(FPC),Y		; turn them on in destination too
	sta	(FPC),Y		; and destiny
CPEXIT:
	lda	PAGE2SW		; set back to main mem
	rts
CPYLEN	EQU 	$-CPYx-1
xCPY_MOD1 equ    (CPL1-CPYx+1)
xCPY_MOD1_DST equ xCPY_MOD1+3
xCPY_MOD2 equ    (CPL2-CPYx+1)
xCPY_MOD2_DST equ xCPY_MOD2+3
PAGE3REL:
;
; MOUSER - this vectors the call to the correct place
;
MOUSERx:
	pha			; save A for a sec
MSX1:	lda	MTABLE,X	; get lo part of address
	sta	MSVECTOR+LO	; save lo part of address
	pla			; get A back
	nop
MSX2:	ldx	#$C4		; might be changed, depending on slot
MSX3:	ldy	#$40		; hopefully, this doesn't
	jmp	(MSVECTOR)	; and away we go
MSRxLEN	EQU	$-MOUSERx-1	; show how long this is
MSVECTORx:
	ds	1		; where lo part of vector goes
MSX4:	db	$C4		; slot where mouse is
MSRLEN	EQU	$-MOUSERx-1	; length or routine
;
; colors for screen lines
;
	RADIX	H
COLORSx:
	db	00,00,00,00	; black
	db	08,11,22,44	; magenta
	db	44,08,11,22	; brown
	db	4C,19,33,66	; orange
	db	22,44,08,11	; dark green
	db	2A,55,2A,55	; gray 1
	db	66,4C,19,33	; green
	db	6E,5D,3B,77	; yellow
	db	11,22,44,08	; dark blue
	db	19,33,66,4C	; purple
	db	55,2A,55,2A	; gray 2
	db	5D,3B,77,6E	; pink
	db	33,66,4C,19	; medium blue
	db	3B,77,6E,5D	; light blue
	db	77,6E,5D,3B	; aqua 
	db	7F,7F,7F,7F	; white
XPOSTBLx:
	DB	0
	DB	0,1,1,2,2,3,4
	DB	4,5,5,6,6,7,8
	DB	8,9,9,0a,0a,0b,0c
	DB	0c,0d,0d,0e,0e,0f,10
	DB	10,11,11,12,12,13,14
	DB	14,15,15,16,16,17,18
	DB	18,19,19,1a,1a,1b,1c
	DB	1c,1d,1d,1e,1e,1f,20
	DB	20,21,21,22,22,23,24
	DB	24,25,25,26,26,27,28
	DB	28,29,29,2a,2a,2b,2c
	DB	2c,2d,2d,2e,2e,2f,30
	DB	30,31,31,32,32,33,34
	DB	34,35,35,36,36,37,38
	DB	38,39,39,3a,3a,3b,3c
	DB	3c,3d,3d,3e,3e,3f,40
	DB	40,41,41,42,42,43,44
	DB	44,45,45,46,46,47,48
	DB	48,49,49,4a,4a,4b,4c
	DB	4c,4d,4d,4e,4e,4f,50
	RADIX	D
PAGE3RELL EQU	$-PAGE3REL-1
	END


