 	TITLE	DIP/8086
	PAGE	58,132
	.LIST

CSEG	SEGMENT	PARA	PUBLIC
	ASSUME	CS:CSEG,DS:CSEG,ES:CSEG,SS:CSEG

COLORSET	DB	0	;0 for RGB, 1 for COMPOSITE
grmtbl		dw	256 dup (0)	;mask table, b/w only

EXTRN	GRXTBL:WORD
EXTRN	GRETBL:WORD
EXTRN	IX1:BYTE	; # blocks across being dumped (for edge effect)
EXTRN	IXSKIP:BYTE

	public grxperm
grxperm	proc	near
	push	cx
	push	di
	mov	di, offset grmtbl
	mov	si, offset grxtbl
	mov	cx, 256		;# wds to copy
	rep	movsw
	pop	di
	pop	cx
	mov	si, offset grxtbl
    	mov	dx, 1
	call	grxmod			; change 00->00, 11->01 (odd bytes)
	mov	si, offset gretbl
	mov	dx, 2
	call	grxmod			; change 00->00, 11->10 (even bytes)
	ret

grxmod:
	mov	cx,256		; byte lookup table
qloop:	mov	ax,[si]
	rol	ax,1
	rol	ax,1		;offset 'phase shift'
	push	cx
	mov	cx,8		; change 8 pairs of bits
	mov	bx,0		; new pattern into b
qloop1:	
	shl	bx,1
	shl	bx,1
	shl	ax,1		; old pattern in a
	jnc	qlbit0		; jump if bit is 0
	or	bx,dx		; 11->01 or ->10

	test	cl,1	;+++hack to alternate colors in x
	jz	qlevnx
	xor	bx,3		; change colors
qlevnx:

	; following code finds 11{11}+ sequences and puts out white 11+
qlb11:	shl	ax,1		; flush second 1 of 11 pair
	loop	qlb1		; check for end of word
	jmp	short qljoin	; out of bits, rejoin outer loop
qlb1:	shl	ax,1		; check for 1111->white(1111)
	jnc	qlb01
	or	bx,3		; change (last) 01 or 10 to 11
	shl	bx,1
	shl	bx,1
	or	bx,3		; and another 11
	jmp	qlb11		; and try for another 1

qlb01:	shl	bx,1		; (11.00)
	shl	bx,1
	; end of white sequence finder
qlbit0:	
	shl	ax,1		; flush 2nd of 00 or 11	
	loop	qloop1
qljoin:	pop	cx
	ror	bx,1
	ror	bx,1
	mov	[si],bx
	inc	si
	inc	si
	loop	qloop
	ret

grxperm	endp

	PAGE
	; --------------------
	; DUMP BLOCK TO SCREEN
	; --------------------

	; ENTRY: BLOCK TO PUT ON SCREEN IN [BLOCK] (TOP BYTE IN [BLOCK + 0])
	;        X-COORDINATE (0-39) IN [AH], Y-COORDINATE (0-23) IN [AL]

VID_SG	EQU	0B800H

EXTRN	YAXTBL:WORD
EXTRN	BLOCK:BYTE
EXTRN	MBLOCK:BYTE
EXTRN	VDPADR:WORD

	PUBLIC	DUMP
DUMP	PROC
	PUSH	DI
	CMP	AH,39			; (B4) SHIFT FIX
	JNE	DMP1
	JMP	DMPFIX

DMP1:	MOV	BX,VID_SG		;POINT AT THE VIDEO BUFFER
	MOV	ES,BX

	;  CALCULATE OFFSET FROM VIDEO_RAM BASE ADDRESS TO STORE THE FIRST BYTE
	SUB	BX,BX			;ZERO MSB
	XCHG	BL,AL			;BX <= ROW ADDRESS (0-23)
					; AL IS NOW "0"
	SHL	BX,1			;MAKE BX A WORD INDEX

	XCHG	AH,AL			;GET COLUMN CONTRIBUTION IN AL
	SHL	AX,1			; x2 SINCE IBM HAS 80 COLUMNS

	ADD	AX,WORD PTR YAXTBL[BX]	;ADD ROW CONTRIBUTION
					; TO COLUMN CONTRIBUTION
	MOV	DI,AX			; (B4) USE REG
	MOV	SI,OFFSET BLOCK		; (B4) USE REG

	MOV	CX,4			;INITIALIZE LOOP COUNTER
	SUB	DI,1FAFH		; (B4) FIRST PASS FUDGE FACTOR
	JMP	DLOOP1			; (B4) SKIP ACTUAL FIX OFFSET
	;   TRANSLATE BYTE IN [AL] INTO A DOUBLED BIT WORD IN [AX]

DLOOP:	SUB	DI,1FB2H		; (B4) FIX OFFSET
DLOOP1:	XOR	AH,AH			; (B4) ZERO TOP HALF
	LODSB				; (B4) GET BLOCKSET BYTE
	SHL	AX,1			; (B4) BYTE TO WORD
	XCHG	BP,AX			; (B4) INTO AN INDEX REG
	MOV	AX,GRETBL[BP]		; (B4) GET SHIFTED WORD FROM TBL
		;note:gretbl

	;PHASE SHIFT
	;MOVE THE WORD INTO VIDEO RAM

	STOSB				; (B4) STORE MIDDLE BYTE
	MOV	AL,AH			;SWAP
	AND	AX,3FC0H		;MASK
	MOV	DH,ES:[DI-2]		; (B4) GET IMAGE ALREADY ON SCREEN
	MOV	DL,ES:[DI]		; (B4)
	AND	DX,0C03FH		;MASK IT
	OR	AX,DX			;COMBINE SOURCE AND DEST TO
	call	edgere
	MOV	ES:[DI-2],AH		; (B4) ACHIEVE PHASE SHIFT ON LEFT
	STOSB				; (B4) AND ON RIGHT

	; ODD BYTE IN BLOCK

	ADD	DI,1FFEH		; FIX OFFSET
	;   TRANSLATE BYTE IN [AL] INTO A DOUBLED BIT WORD IN [AX]
	XOR	AH,AH			; (B4) ZERO TOP HALF
	LODSB				; (B4) GET BLOCKSET BYTE
	SHL	AX,1			; (B4) BYTE TO WORD
	XCHG	BP,AX			; (B4) INTO AN BASE REG
	MOV	AX,GRXTBL[BP]		; (B4) GET SHIFTED WORD FROM TBL

	;PHASE SHIFT
	;MOVE THE WORD INTO VIDEO RAM

	STOSB				; (B4) STORE MIDDLE BYTE
	MOV	AL,AH			;SWAP
	AND	AX,3FC0H		;MASK
	MOV	DH,ES:[DI-2]		; (B4) GET IMAGE ALREADY ON SCREEN
	MOV	DL,ES:[DI]		; (B4)
	AND	DX,0C03FH		;MASK IT
	OR	AX,DX			;COMBINE SOURCE AND DEST TO
	call	edgero
	MOV	ES:[DI-2],AH		; (B4) ACHIEVE PHASE SHIFT ON LEFT
	STOSB				; (B4) AND ON RIGHT

	LOOP	DLOOP

	POP	DI
	RET
DUMP	ENDP
;Following routine handles edge effects between blocks. Note that a block
;can affect the next block (bc.cb->bw.wb), so when a different block
;is written over the one that caused the mutation to white, the
;color must be put back: bb.wb->bb.cb.
;In forcing to white, we only need to look at the outer cells: c.c->w.w
;The return to color requires that we look one cell beyond, since the
;white cell at the boundary may be white because the cell next to it
;on the other side is white.
edgere:
; test to eliminate edge effects--two colors (or whites) make a white
	test	ah,0c0h	; lhs black?
	jz	hiblk3	;   jmp if so, leave alone (j if xb.x)
	test	ah,030h	; rhs black?
	jnz	hiblk4	;   jmp if not, force xc.c or xw.c to xw.w
	test	byte ptr es:[di-3],3	;look at color to left x in xc.b
	jnz	hiblk3		;if ~b, then must have wwb, leave alone
	and	ah,07fh	; force bwb (or bcb) to bcb (7fh or bfh dep on color)
	jmp	hiblk3
hiblk4:
	or	ah,0f0h	; both were color or white, make both white
hiblk3:
	test	al,030h	; rhs black?
	jz	loblk3	;   jmp if so, leave alone
	test	al,0c0h	; lhs black?
	jnz	loblk4	;   jmp if so, leave alone
	test	al,0ch	; look at adjacent color to right
	jnz	loblk3	; if ~b, then must have bww, leave alone
	and	al,0efh	; force bwb (or bcb) to bcb (efh or dfh dep on color)
	jmp	loblk3
loblk4:
	or	al,0f0h	; both were color or white, make both white
loblk3:	;end of edge code
	ret
edgero:
; test to eliminate edge effects--two colors (or whites) make a white
	test	ah,0c0h	; lhs black?
	jz	hiblk1	;   jmp if so, leave alone (jmp if xb.x)
	test	ah,030h	; rhs black?
	jnz	hiblk2	;   jmp if not, force cc or wc to ww (j if xc.c)
	test	byte ptr es:[di-3],3	;look at color to left (x in xc.b)
	jnz	hiblk1		;if ~b, then must have ww.b, leave alone
	and	ah,0bfh	; force bwb (or bcb) to bcb (7fh or bfh dep on color)
	jmp	hiblk1
hiblk2:
	or	ah,0f0h	; both were color or white, make both white
hiblk1:
	test	al,030h	; rhs black?
	jz	loblk3	;   jmp if so, leave alone (jmp if cb.x)
	test	al,0c0h	; lhs black?
	jnz	loblk4	;   jmp if not, force cc or cw to ww (j if c.cx)
	test	al,0ch	; look at color to right (x in b.cx)
	jnz	loblk3	; if ~b, then must have b.ww, leave alone
	and	al,0dfh	; force bwb (or bcb) to bcb (efh or dfh dep on color)
	jmp	loblk1
loblk2:
	or	al,0f0h	; both were color or white, make both white
loblk1:	;end of edge code
	ret

DMPFIX:	MOV	BX,VID_SG		;POINT AT THE VIDEO BUFFER
	MOV	ES,BX
	SUB	BX,BX			;ZERO MSB
	XCHG	BL,AL			;BX <= ROW ADDRESS (0-23)
	SHL	BX,1			;MAKE BX A WORD INDEX
	XCHG	AH,AL			;GET COLUMN CONTRIBUTION IN AL
	SHL	AX,1			; x2 SINCE IBM HAS 80 COLUMNS
	ADD	AX,WORD PTR YAXTBL[BX]	;ADD ROW CONTRIBUTION
	MOV	DI,AX			; (B4) USE REG
	MOV	SI,OFFSET BLOCK		; (B4) USE REG
	MOV	CX,4			;INITIALIZE LOOP COUNTER
	SUB	DI,1FAFH		; (B4) FIRST PASS FUDGE FACTOR
	JMP	DFLP1			; (B4) SKIP ACTUAL FIX OFFSET
	SUB	DI,1FAFH		; (B4) FIRST PASS FUDGE FACTOR
	JMP	DFLP1			; (B4) SKIP ACTUAL FIX OFFSET
DFLP:	SUB	DI,1FB1H		; (B4) FIX OFFSET
DFLP1:	XOR	AH,AH			; (B4) ZERO TOP HALF
	LODSB				; (B4) GET BLOCKSET BYTE
	SHL	AX,1			; (B4) BYTE TO WORD
	XCHG	BP,AX			; (B4) INTO AN INDEX REG
	MOV	AX,GRETBL[BP]		; (B4) GET SHIFTED WORD FROM TBL
	STOSB				; (B4) STORE MIDDLE BYTE
	MOV	AL,AH			;SWAP
	AND	AX,3FC0H		;MASK
	MOV	DH,ES:[DI-2]		; (B4) GET IMAGE ALREADY ON SCREEN
	MOV	DL,ES:[DI]		; (B4)
	AND	DX,0C03FH		; (B4) MASK IT
	OR	AX,DX			;COMBINE SOURCE AND DEST TO
	MOV	ES:[DI-2],AH		; (B4) ACHIEVE PHASE SHIFT ON LEFT
	ADD	DI,1FFFH		; FIX OFFSET
	XOR	AH,AH			; (B4) ZERO TOP HALF
	LODSB				; (B4) GET BLOCKSET BYTE
	SHL	AX,1			; (B4) BYTE TO WORD
	XCHG	BP,AX			; (B4) INTO AN BASE REG
	MOV	AX,GRXTBL[BP]		; (B4) GET SHIFTED WORD FROM TBL
	STOSB				; (B4) STORE MIDDLE BYTE
	MOV	AL,AH			;SWAP
	AND	AX,3FC0H		;MASK
	MOV	DH,ES:[DI-2]		; (B4) GET IMAGE ALREADY ON SCREEN
	MOV	DL,ES:[DI]		; (B4)
	AND	DX,0C03FH		;MASK IT
	OR	AX,DX			;COMBINE SOURCE AND DEST TO
	MOV	ES:[DI-2],AH		; (B4) ACHIEVE PHASE SHIFT ON LEFT
	LOOP	DFLP
	POP	DI
	RET

	PAGE
	; ------------------------------
	; DUMP BLOCK TO SCREEN WITH MASK
	; ------------------------------

	; ENTRY: BLOCK TO PUT ON SCREEN IN [BLOCK] (TOP BYTE IN [BLOCK + 0])
	;        X-COORDINATE (0-39) IN [AH], Y-COORDINATE (0-23) IN [AL]
	;
	; IF THE MASK BIT IS A "1" THEN THE SCREEN SHOWS THROUGH
	; IF THE MASK BIT IS A "0" THEN THE ICON IS DISPLAYED
	;
	PUBLIC	DMPMSK
DMPMSK	PROC
	PUSH	BX			;SAVE AFFECTED REGISTER(S)
	PUSH	CX
	PUSH	DX
	PUSH	ES

	MOV	BX,VID_SG		;POINT AT THE VIDEO BUFFER
	MOV	ES,BX

	;  CALCULATE OFFSET FROM VIDEO_RAM BASE ADDRESS TO STORE THE FIRST BYTE
	SUB	BX,BX			;ZERO MSB
	XCHG	BL,AL			;BX <= ROW ADDRESS (0-23)
					; AL IS NOW "0"
	SHL	BX,1			;MAKE BX A WORD INDEX

	XCHG	AH,AL			;GET COLUMN CONTRIBUTION IN AL
	SHL	AX,1			; x2 SINCE IBM HAS 80 COLUMNS

	ADD	AX,WORD PTR YAXTBL[BX]	;ADD ROW CONTRIBUTION
					; TO COLUMN CONTRIBUTION
	MOV	VDPADR,AX		;SAVE STARTING MEMORY OFFSET

	SUB	SI,SI			;INITIALIZE INDEX REGISTER
	MOV	CX,4			;INITIALIZE LOOP COUNTER

DMPMLP:	MOV	BL,BLOCK[SI]		;SELECT ICON BYTE
	MOV	BH,MBLOCK[SI]		;SELECT MASK BYTE
	SUB	VDPADR,1FB0H		;FIX OFFSET
	CALL	M1DISP			;TRANSLATE, MASK AND MOVE IT TO VRAM
	INC	SI			; NEXT BYTES
	MOV	BL,BLOCK[SI]		;SELECT ICON BYTE
	MOV	BH,MBLOCK[SI]		;SELECT MASK BYTE
	ADD	VDPADR,2000H		;FIX OFFSET
	CALL	M1DISP			;TRANSLATE, MASK AND MOVE IT TO VRAM
	INC	SI			; NEXT BYTES
	LOOP	DMPMLP

	POP	ES			;RESTORE AFFECTED REGISTER(S)
	POP	DX
	POP	CX
	POP	BX
	RET
DMPMSK	ENDP

	PUBLIC	M1DISP
M1DISP	PROC
	PUSH	CX
	PUSH	BX			;SAVE MASK BYTE [IN BH]

	;   TRANSLATE BLOCK BYTE IN [BL] INTO A DOUBLED BIT WORD IN [AX]
	SUB	BH,BH
	SHL	BX,1
	MOV	DX,GRMTBL[BX]		;BLOCK WORD
	POP	BX			;RECOVER MASK BYTE
	XCHG	BH,BL			;PUT IT IN BL
	SUB	BH,BH
	SHL	BX,1
	MOV	CX,GRMTBL[BX]		;MASK WORD

	PUSH	DX			;SAVE BLOCK WORD

	; GET THE VIDEO WORD FROM VIDEO RAM
	MOV	BX,VDPADR		;GET BASE ADDRESS
	MOV	AX,ES:[BX]
	XCHG	AL,AH
	AND	AX,3FFFH
	MOV	DL,ES:[BX+2]
	AND	DL,0C0H
	OR	AH,DL

	POP	DX			;DX <= BLOCK WORD
					;CX <= MASK WORD
					;AX <= VIDEO WORD

	;BLOCK <= ((VIDEO XOR BLOCK) AND MASK) XOR BLOCK
	XOR	AX,DX
	AND	AX,CX
	XOR	AX,DX

	;   STORE THE WORD IN VIDEO RAM
	MOV	BX,VDPADR		;GET BASE ADDRESS

	;PHASE SHIFT
	;MOVE THE WORD INTO VIDEO RAM
	MOV	ES:[BX+1],AL
	MOV	AL,AH
	AND	AX,3FC0H
	MOV	DH,ES:[BX]
	MOV	DL,ES:[BX+2]
	AND	DX,0C03FH
	OR	DX,AX
; test to eliminate edge effects--two colors (or whites) make a white
;	test	dh,0c0h	; lhs black?
;	jz	hiblk	;   jmp if so, leave alone
;	test	dh,030h	; rhs black?
;	jz	hiblk	;   jmp if so, leave alone
;	or	dh,0f0h	; both were color or white, make both white
;hiblk:
;	test	dl,0c0h	; lhs black?
;	jz	loblk	;   jmp if so, leave alone
;	test	dl,030h	; rhs black?
;	jz	loblk	;   jmp if so, leave alone
;	or	dl,0f0h	; both were color or white, make both white
;loblk:	;end of edge code
	MOV	ES:[BX],DH
	MOV	ES:[BX+2],DL
	POP	CX
	RET
M1DISP	ENDP

%OUT DMPMSK DONE


	; ----------------------
	; CONSOLE INITIALIZATION
	; ----------------------

EXTRN	MPRNT:NEAR
EXTRN	GERROR:NEAR

EXTRN	WAITSTR:BYTE
EXTRN	FSIZE:BYTE
EXTRN	ARG1:WORD

	PUBLIC	CSETUP
CSETUP	PROC
	;   INITIALIZE VIDEO
	;     SELECT MEDIUM RESOLUTION GRAPHICS
	MOV	AH,0
	MOV	AL,4
	INT	10H
	MOV	AH,0FH			; SEE IF IT WORKED
	INT	10H			; BY ASKING THE VIDEO
	CMP	AL,4			; IT SHOULD BE AS SET
	JE	CSET$
	MOV	AL,10H			; NEW ERROR EXIT
	JMP	GERROR

;     SET BACKGROUND COLOR
CSET$:	MOV	AH,11			;REQUEST COLOR
	MOV	BL,0			;SELECT BACKGROUND
	MOV	BH,0			;  0=BLACK 
	INT	10H			;CALL BIOS

;     SELECT COLOR PALETTE
	MOV	AH,11			;REQUEST COLOR
	MOV	BL,COLORSET		;SELECT PALETTE #1
	MOV	BH,1			;0=BLACK, 1=CYAN, 2=MAGENTA, 3=WHITE
	INT	10H			;CALL BIOS

;	CLEAR THE SCREEN TO BLACK AND RESET THE ACTIVE ICON TABLE
	MOV	ARG1,0			;LOAD ARG1 WITH A POSITIVE VALUE
	CALL	GCLEAR			;DO IT
	CMP	FSIZE,0			; (B4) GAME FIT?
	JNE	CSET$$
	RET
CSET$$:	MOV	AH,2			; (B4) NOW PRINT A LOADING MESSAGE
	MOV	DX,800H			; (B4) SOMEWHERE ON THE SCREEN
	MOV	BH,0
	INT	10H
	MOV	AX,OFFSET WAITSTR	; (B4) GAME IS LOADING...
	CALL	MPRNT
	MOV	AH,2			; (B4) AND RESET THE CURSOR TO TOP
	MOV	DX,100H
	MOV	BH,0
	INT	10H
	RET
CSETUP	ENDP


	PAGE
	; -----
	; CLEAR
	; -----
	;
	; CLEAR SCREEN TO WHITE IF [ARG1] IS NEGATIVE
	; CLEAR SCREEN TO BLACK IF [ARG1] IS POSITIVE
	;

	PUBLIC	GCLEAR,GCLR1,GCLR2,GCLR3
GCLEAR	PROC
	PUSH	DI		;SAVE AFFECTED REGISTER(S)
	PUSH	CX
	PUSH	ES

	MOV	AX,VID_SG	;POINT AT THE VIDEO BUFFER
	MOV	ES,AX
	MOV	AX,ARG1		;GET THE ARGUMENT

	CMP	AX,0		; (B5) DO BLACK DIFFERENTLY
	JE	GCLR4
	MOV	BP,AX		; GET WORD 
	AND	BP,3FH		; MASK OFF HI NIBBLE FOR BIT SHIFT PROBLEM
	MOV	DI,0		;SET UP INDEX
	MOV	BX,1E00H	;SET UP LOOP COUNTER
	; PAINT THE SCREEN WITH AX
GCLR1:	XCHG	AX,BP		; GET MASK BYTE
	STOSB	
	XCHG	AX,BP
	MOV	CX,79		; DO A LINE - 1 AT A TIME
	REPZ	STOSB
	SUB	BX,80		; SUBTRACT FROM TOTAL
	JNZ	GCLR1	
	MOV	DI,2000H
	MOV	BX,1E00H
GCLR2:	XCHG	AX,BP
	STOSB
	XCHG	AX,BP
	MOV	CX,79		; (B4) DO A LINE
	REPZ	STOSB
	SUB	BX,80
	JNZ	GCLR2

	;CLEANUP WHEN COMPLETE
GCLR3:	POP	ES		;RESTORE THOSE REGISTERS
	POP	CX
	POP	DI
	RET

GCLR4:	MOV	DI,0
	MOV	CX,0F00H	; (B5) DO WHOLE SCREEN
	REPZ	STOSW	
	MOV	DI,2000H	; (B5) ODD BYTES
	MOV	CX,0F00H	; (B5) COUNT
	REPZ	STOSW
	JMP	GCLR3

GCLEAR	ENDP

	;paging space starts here

	PUBLIC	MEMBUF
MEMBUF	LABEL	BYTE

CSEG	ENDS

	END
