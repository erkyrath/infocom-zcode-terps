	xlist		; Turn off listing while setting up printing parameters
	page	,80	; Use default page length, set page width to 80 columns
;
	title	"HMA6502 test file",'Alphabetical listing of 65C02 mnemonics'

	ifndef	pass2	; Set up local pass counter
pass2	=	0
	else
pass2	=	1	; pass2 is true on pass 2
	endif

	list
;
;
; Note:	Instructions marked with a "[C]" are for CMOS devices only
;
;
n	EQU	0020h
w	EQU	0584h
z	EQU	0005h
;
	CSEG
;
first_label:
	adc	#%100000	; Add memory to accumulator with carry
	ADC	z			; (Default to zero page addressing)
	ADC	<z,x			; (Force zero page addressing)
	ADC	w			; (Default to extended addressing)
	ADC	>w,X			; (Force extended addressing)
	ADC	w,y
	ADC	(z)		; [C]
	ADC	(z,X)
	ADC	(z),Y

	and	#$20		; "AND" memory with accumulator
	AND	z
	AND	z,X
	AND	w
	AND	w,X
	AND	w,Y
	AND	(z)		; [C]
	AND	(z,X)
	AND	(z),Y

	asl	a		; Shift left one bit
	ASL	z
	ASL	z,X
	ASL	w
	ASL	w,X

	bbr	0,z,first_label	; Branch on bit reset  [C]
	BBR	1,z,first_label	; [C]
	BBR	2,z,$		; [C]
	BBR	3,z,*		; [C]
	BBR	4,z,$+3		; [C]
	BBR	5,z,*-3		; [C]
	BBR	6,z,destination	; [C]
	BBR	7,z,destination	; [C]

	bbs	0,z,destination	; Branch on bit set  [C]
	BBS	1,z,destination	; [C]
	BBS	2,z,*		; [C]
	BBS	3,z,$		; [C]
	BBS	4,z,$-3		; [C]
	BBS	5,z,*+3		; [C]
	BBS	6,z,first_label	; [C]
	BBS	7,z,first_label	; [C]

	bcc	first_label	; Branch on carry clear

	bcs	first_label	; Branch on carry set

	beq	$		; Branch on result zero

	bit	#20h		; Test bits in memory with accumulator [C]
	BIT	z
	BIT	z,X		; [C]
	BIT	w
	BIT	w,X		; [C]

	bmi	*		; Branch on result minus

	bne	$+2		; Branch on result not zero

	bpl	*-2		; Branch on result plus

	bra	2+*		; Branch always  [C]

	brk			; Force break

	bvc	destination	; Branch on overflow clear

	bvs	destination	; Branch on overflow set

destination:

	clc			; Clear carry flag

	cld			; Clear decimal mode

	cli			; Clear interrupt disable bit

	clv			; Clear overflow flag

	cmp	#100000b	; Compare memory and accumulator
	CMP	z
	CMP	z,X
	CMP	w
	CMP	w,X
	CMP	w,Y
	CMP	(z)		; [C]
	CMP	(z,X)
	CMP	(z),Y

	cpx	#n		; Compare memory and index X
	CPX	z
	CPX	w

	cpy	#n		; Compare memory and index Y
	CPY	z
	CPY	w

	dec	A		; Decrement memory by one  [C]
	DEC	z
	DEC	z,X
	DEC	w
	DEC	w,X

	dex			; Decrement index X by one

	dey			; Decrement index Y by one

	eor	#n		; "Exclusive-Or" memory with accumulator
	EOR	z
	EOR	z,X
	EOR	w
	EOR	w,X
	EOR	w,Y
	EOR	(z)		; [C]
	EOR	(z,X)
	EOR	(z),Y

	inc	A		; Increment memory by one  [C]
	INC	z
	INC	z,X
	INC	w
	INC	w,X

	inx			; Increment index X by one

	iny			; Increment index Y by one

	jmp	w		; Jump to new location
	JMP	(w)
	JMP	(w,X)		; [C]

	jsr	w		; Jump to new location saving return address

	lda	#n		; Load accumulator with memory
	LDA	z
	LDA	z,X
	LDA	w
	LDA	w,X
	LDA	w,Y
	LDA	(z)		; [C]
	LDA	(z,X)
	LDA	(z),Y
	xlist
	printx	'Halfway through pass ',pass2 + 1
	list

	ldx	#n		; Load index X with memory
	LDX	z
	LDX	z,Y
	LDX	w
	LDX	w,Y

	ldy	#n		; Load index Y with memory
	LDY	z
	LDY	z,X
	LDY	w
	LDY	w,X

	lsr	A		; Shift right one bit
	LSR	z
	LSR	z,X
	LSR	w
	LSR	w,X

	NOP			; No operation

	ora	#n		; "OR" memory with accumulator
	ORA	z
	ORA	z,X
	ORA	w
	ORA	w,X
	ORA	w,Y
	ORA	(z)		; [C]
	ORA	(z,X)
	ORA	(z),Y

	pha			; Push accumulator on stack

	php			; Push processor status on stack

	phx			; Push index X on stack  [C]

	phy			; Push index Y on stack  [C]

	pla			; Pull accumulator from stack

	plp			; Pull processor status from stack

	plx			; Pull index X from stack  [C]

	ply			; Pull index Y from stack  [C]

	rmb	0,z		; Reset memory bit  [C]
	RMB	1,z		; [C]
	RMB	2,z		; [C]
	RMB	3,z		; [C]
	RMB	4,z		; [C]
	RMB	5,z		; [C]
	RMB	6,z		; [C]
	RMB	7,z		; [C]

	rol	A		; Rotate one bit left
	ROL	z
	ROL	z,X
	ROL	w
	ROL	w,X

	ror	A		; Rotate one bit right
	ROR	z
	ROR	z,X
	ROR	w
	ROR	w,X

	rti			; Return from interrupt

	rts			; Return from subroutine

	sbc	#n		; Subtract memory from accumulator with borrow
	SBC	z
	SBC	z,X
	SBC	w
	SBC	w,X
	SBC	w,Y
	SBC	(z)		; [C]
	SBC	(z,X)
	SBC	(z),Y

	sec			; Set carry flag

	sed			; Set decimal mode

	sei			; Set interrupt disable status

	smb	0,z		; Set memory bit  [C]
	SMB	1,z		; [C]
	SMB	2,z		; [C]
	SMB	3,z		; [C]
	SMB	4,z		; [C]
	SMB	5,z		; [C]
	SMB	6,z		; [C]
	SMB	7,z		; [C]

	sta	z		; Store accumulator in memory
	STA	z,X
	STA	w
	STA	w,X
	STA	w,Y
	STA	(z)		; [C]
	STA	(z,X)
	STA	(z),Y

	stx	z		; Store index X in memory
	STX	z,Y
	STX	w

	sty	z		; Store index Y in memory
	STY	z,X
	STY	w

	stz	z		; Store zero  [C]
	STZ	z,X		; [C]
	STZ	w		; [C]
	STZ	w,X		; [C]

	tax			; Transfer accumulator to index X

	tay			; Transfer accumulator to index Y

	trb	z		; Test and reset bits  [C]
	TRB	w		; [C]

	tsb	z		; Test and set bits  [C]
	TSB	w		; [C]

	tsx			; Transfer stack pointer to index X

	txa			; Transfer index X to accumulator

	txs			; Transfer index X to stack pointer

	tya			; Transfer index Y to accumulator

last_label:
	END	first_label

	xlist
	title	,"Symbol Table"
	list