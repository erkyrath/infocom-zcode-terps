;
; print out prompt message and set up start track and sector for each one
;
gc128msg: db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put C128 Xzip Interpreter onto Side 1",EOL
gc128len equ	$-gc128msg

getc128:			; get c128 xzip interpreter
	MSG	gc128

	lda	#C128T		; start track
	sta	TRACK
	lda	#C128S		; start sector
	sta	SECTOR
	jmp	ready		; now see if user is ready

gc64msg: db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put C64 Xzip Interpreter onto Side 1"
gc64len equ	$-gc64msg
	
getc64:				; go get c64 xzip interpreter
	MSG	gc64
	lda	#C64T		; c64 interpreter start track
	sta	TRACK
	lda	#C64S		; and start sector
	sta	SECTOR
	jmp	ready		; now do it

gs1lmsg: db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put LZIP story preload onto side 1 of disk",EOL
	db	EOL,LRED,"REMEMBER:",EOL
	db	"    Combined C64/C128 split @"
	db	YELLOW,"44800",EOL
	db	LRED,"    Commodore C128 split @"
	db	DCYAN,"88064",EOL
gs1llen	equ	$-gs1lmsg

gets1l:				; get side 1 of the story
	MSG	gs1l

	lda	#LSIDE1T	; start story at track
	sta	TRACK
	lda	#LSIDE1S	; sector
	sta	SECTOR

	jmp	ready		; now do it

gs1msg:	db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put story preload onto side 1 of disk",EOL
	db	EOL,LRED,"REMEMBER:",EOL
	db	"    Combined C64/C128 split @"
	db	YELLOW,"44800",EOL
	db	LRED,"    Commodore C128 split @"
	db	DCYAN,"88064",EOL
gs1len	equ	$-gs1msg

gets1:				; get side 1 of the story
	MSG	gs1

	lda	#SIDE1T		; start story at track
	sta	TRACK
	lda	#SIDE1S		; sector
	sta	SECTOR

	jmp	ready		; now do it

gs2msg:	db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put story PURLOAD onto side 2 of disk",EOL
	db	EOL,LRED,"REMEMBER:",EOL
	db	"    Combined C64/C128 split @"
	db	YELLOW,"44800",EOL
	db	LRED,"    Commodore C128 split @"
	db	DCYAN,"88064",EOL
gs2len	equ	$-gs2msg

gets2:				; get side 2 of story
	MSG	gs2
	
	lda	#SIDE2T		; side 2 starts at track
	sta	TRACK
	lda	#SIDE2S		; sector
	sta	SECTOR

	jmp	ready		; now get ready

gf1msg:	db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put C128 Font 1 onto side 1"
gf1len	equ	$-gf1msg

getf1:				; get font 1
	MSG	gf1

	lda	#FONT1T		; font track 
	sta	TRACK
	lda	#FONT1S		; and sector
	sta	SECTOR

	jmp	ready		; so do it already

gf2msg:	db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put C128 Font 2 onto side 1"
gf2len	equ	$-gf2msg

getf2:				; get font 2
	MSG	gf2

	lda	#FONT2T		; font 2 track
	sta	TRACK
	lda	#FONT2S		; font 2 sector
	sta	SECTOR

	jmp	ready		; and wing it

gfcmsg:	db	CLS,EOL,EOL,"               ",RVSON
	db	YELLOW,"Put Commodore 64 FASTCODE onto side 2"
gfclen	equ	$-gfcmsg

getfc:				; get the c64 fastcode
	MSG	gfc

	lda	#FASTT		; fast code start track
	sta	TRACK
	lda	#FASTS		; and sector
	sta	SECTOR

	jmp 	ready		; and do it	

rdymsg:	db	EOL,EOL,LGREEN,"Insert Disk to be copied onto and then"
	db	" hit any key to begin transfer or the",EOL
	db	RED,"<ESCAPE> ", LGREEN, "key to escape and start all over!"
rdylen	equ	$-rdymsg

ready:
	MSG	rdy

rloop:
	jsr	GETIN
	tay
	beq	rloop		; wait for user

	cmp	#ESCAPE		; check of X-scape
	beq	rexit

	jsr	doit		; do the transfer
rexit:
	jmp	begin		; begin again

; -----------------------
; DIRECT PRINT LINE [X/A]
; -----------------------
; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)
; STRING LENGTH IN [Y]

DLINE:
	STX	STRING+LO	; DROP STRING ADDRESS
	STA	STRING+HI	; INTO DUMMY BYTES
	STY	TEMP		; COUNTER

	LDX	#0		; INIT CHAR-FETCH INDEX
DOUT:	DB	$BD		; 6502 "LDA nnnn,X" OPCODE
STRING:	DW	$0000		; DUMMY OPERAND BYTES
	JSR	CHAR
	INX
	DEC	TEMP		; LOOP TILL
	BNE	DOUT		; OUT OF CHARS
	RTS		

CHAR:	CMP	#'a'		; LOWER-CASE?
	BCC	LET0		; NO, CONTINUE
	CMP	#'z'+1
	BCS	LETEX		; CTRL CHARS
	AND	#%01011111	; ELSE MASK FOR LOWER-CASE
	BNE	LETEX
LET0:
	CMP	#'A'		; UPPER-CASE?
	BCC	LETEX
	CMP	#'Z'+1
	BCS	LETEX
	ORA	#%00100000	; MAKE UPPER
LETEX:
	JSR	CHROUT	; goodbye, cruel char
	RTS
	; ----------------
	; DIVIDE [A] BY 10
	; ----------------

	; EXIT: QUOTIENT IN [X], REMAINDER IN [A]

DIV10:	LDX	#0		; START WITH ZERO QUOTIENT

D10L:	CMP	#10		; IF DIVISOR < 10,
	BCC	D10EX		; WE'RE DONE
	SBC	#10		; ELSE SUBTRACT ANOTHER 10
	INX			; UPDATE QUOTIENT
	BNE	D10L		; BRANCH ALWAYS

D10EX:	RTS
	end


