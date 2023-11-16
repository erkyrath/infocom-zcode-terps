;	title - boss
;	include file to add boss key to ZIP

;	call boss with character in AL to display file boss.scr
;	call herald to display file herald.scr

;	Written by E. H. Black - May-19-1986
;
;
BOSSKEY	equ	2	;ctrl B	-- function keys not available through DOS?

OPENF	equ	3DH	;AH/3DH, DS:DX->ASCIZ path name, AL/open mode
			;return AX/open mode if carry clear, else error bits
RACC	equ	0	;access for read
WACC	equ	1	;access for write
RWACC	equ	2	;access for read/write

CLOSEF	equ	3EH	;AH/3EH, BX/File Handle from OPENF
			;return AX/- if carry clear, else error bits
BUFSIZE	equ	256
BUFFER	db	256 dup (?)

STDOUT	equ	1	;file handle for screen
ESC	equ	27	;ascii escape

INCHR	equ	08H	;DOS function: console input without echo
PRTSTR	equ	09H	;"	       print string to screen
CLRKEY	equ	0CH	;"	       clear buffer & invoke kb function

DISPLAY	MACRO	nm
	mov	dx,offset nm
	call	showit
	ENDM

filehan	dw	0			;file handle for input file
bossfn	db	'LEATHER.SCR',0
	db	53 DUP (0)		;room for 64 chrs in fn
bossext db	'SCR'
hrldext	db	'HRL'
cntfind	db	'File not Found',13,10,'$'
clrstr	db	ESC,'[0m',ESC,'[2J','$'	;attrs off, erase screen and home
;
boss	proc	
	push	AX
	push	BX
	push	CX
	push	DX
	push	SI
	cmp	al,BOSSKEY	;hit the magic key?
	jnz	bossret		;carry is clear
	CALL	SAVECUR		;save cursor
	CALL	clearit			;clear screen
	MOV	DX,offset bossfn	;file name
	CALL	openit
	jnc	reader
	DISPLAY	cntfind
	jmp	waiter
reader:	CALL	readit
	CALL	closit

waiter:	CALL	waitkey

	CALL	restor
	CALL	RESTCUR		;restore cursor
	stc			;alert caller that key is to be deleted
bossret:
	POP	SI
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET
;herald puts up intro screen, waits for any key
herald:	push	AX
	push	BX
	push	CX
	push	DX
	push	SI
	MOV	DX,offset bossfn	;file name string
	MOV	BX,offset hrldext	;extension for herald
	CALL	makefn			;make <game>.ext in bossfn
	CALL	openit
	jc	herret
	CALL	SAVECUR			;save cursor
	CALL	clearit			;clear screen
	CALL	readit
	CALL	closit
	CALL	waitkey
	CALL	restor
	CALL	RESTCUR		;restore cursor
herret:
	MOV	DX,offset bossfn	;file name string
	MOV	BX,offset bossext	;extension
	CALL	makefn			;make <game>.ext in bossfn
					;   so boss just opens
	POP	SI
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET
;openit takes file name ptr in DX, returns handle in AX
openit:
	MOV	AL,RACC	;read only	
	MOV	AH,3Dh
	INT	21h
	mov	filehan,ax	;store file handle for read & close
	RET	                                   
;
closit:	MOV	BX,filehan
	MOV	AH,3Eh		;close handle in BX
	INT	21h
	RET	                                   
;read the file and transfer it to the screen
readit: 
readlp:	CALL	readbl		;get a block
	jc	readxx		;error occurred?
	CALL	showbl		;if not, show the block (or amt read)
	CMP	AX,BUFSIZE	;full block read?
	JZ	readlp
readxx:	RET
;read a block of text or less from the file
readbl:	MOV	BX,filehan	;handle
	MOV	CX,BUFSIZE	;byte count
	MOV	DX,offset BUFFER
	MOV	AH,3Fh		;read block
	INT	21h
	RET	   		;AX/number of bytes read, if cc
;output the buffer to the screen (AX assumed to contain # bytes)
showbl:	MOV	CX,AX		;#bytes to transfer
	MOV	DX,offset buffer
	MOV	BX,STDOUT	;screen
	MOV	AH,40H		;write
	INT	21h
	RET
;showit called with DS:DX->character string, terminated by $
showit:	MOV	AH,PRTSTR	;print string
	INT	21h
	RET
;
restor:	CALL	clearit
	RET		;nice if we could switch screens?

;clears the screen
clearit:DISPLAY clrstr	
	RET
;
waitkey:MOV	AH,CLRKEY	;clear keyboard buffer and invoke kb function
	MOV	AL,INCHR	;function (Console Input without echo)
	INT	21H
	RET
;makefn takes an output string pointer in DX, an extension in BX
; and copies the game file name from gamfile into the output string,
; appending the extension and a 0

makefn:	push	ax
	push	di
	push	si
	push	es
	push	ds			;point ds,es at same seg
	pop	es
	mov	si,offset gamfile	;input is the name of the game
	mov	di,dx			;output to string passed in DX
makef1:	lodsb				;get char from name
	or	al,al			;end of string?
	jz	makef2
	cmp	al,'.'			;extension reached?
	jz	makef2
	stosb				;unspecial char, just store
	jmp	makef1
makef2:	mov	al,'.'			;whether . or 0 reached, output .
	stosb
	mov	si,bx			;input is extension string
	movsb
	movsb
	movsb				;copy 3 chrs
	sub	al,al
	stosb				;and write a 0
	pop	es
	pop	si
	pop	di
	pop	ax
	ret

boss	endp
