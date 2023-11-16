;
; format a disk please
;
fmtmsg:
	DB	CLS,EOL,EOL,"               ",RVSON
	DB	YELLOW,"Format Commodore game disk",RVSOFF
	DB	EOL,EOL,LRED,"Please select which side you wish to format:",EOL
	DB	"   1 - format side 1 (NAME: 'INFOCOM-S1')",EOL
	DB	"   2 - format side 2 (NAME: 'INFOCOM-S2')",EOL
	DB	"   X - Done Formatting",EOL,LGREEN
fmtlen	EQU	$-fmtmsg

fcmd	DB	"N0:INFOCOM-S"	; formatting command
fside	DB	'*'		; store actual side here
	DB	",XX"
fcmdl	equ	$-fcmd

insmsg:	db	EOL,EOL,"Give me the disk to be MUNGED, please!"
	db	EOL, "Hit any key to CHOMP it, or ",RED,"<ESCAPE>",LGREEN,
	db	" to begin anew"
inslen	equ	$-insmsg

waitmsg: db	DYELLOW,EOL,EOL,"Now hit any key to continue . . ."
waitlen	equ	$-waitmsg

workmsg: db	CYAN,EOL,EOL,"Give me a moment while I format the disk . . ."
worklen	equ	$-workmsg

formatter:		
	MSG	fmt		; print out prompt
floop:
	jsr	GETIN
	tay	
	beq	floop		; wait for command

	cmp	#'1'		; do side 1
	bne	fk1

	sta	fside		; save in string
	jmp 	dofmt
fk1:
	cmp	#'2'		; do side 2?
	bne	fk2

	sta	fside		; yes, so show it
	jmp	dofmt
fk2:
	cmp	#'X'		; done?
	bne	floop		; unknown command, so try again
	jmp	begin		; go back to start
;
; do the actual formatting of disk, please
;
dofmt:
	MSG	ins		; ask for disk
dfloop:
	jsr	GETIN
	tay
	beq	dfloop		; no key yet
	cmp	#ESCAPE		; check for get me outta here
	bne	dfl1		; nope
	jmp	begin		; begin again
dfl1:
	MSG	work		; show we are working
	
	lda	#$0f		; logical number 15
	ldx	#$08		; drive 8
	ldy	#$0f		; secondary address 15
	jsr	SETLFS		; and setit
	bcs	error		; problems with drive

	lda	#fcmdl		; length of command
	ldx	#<fcmd		; get LSB
	ldy	#>fcmd		; and MSG of command address
	jsr	SETNAM		; and send it out
	bcs	error		; problems with drive
	
	jsr	OPEN		; open things up
	bcs	error		; problems with drive
	
	lda	#$0f		; and now close it
	jsr	CLOSE		; thank you
	bcs	error		; problems with drive
wait:
	MSG	wait		; now wait for key
wloop:
	jsr	GETIN
	tay
	beq	wloop

	jmp	formatter

;
; just complain about error
;
errmsg: db	"Problems with disk and/or drive",EOL
	db	"Please check things out and try again!!"
errlen	equ	$-errmsg

error:
	MSG	err		; complain about error

	jmp	wait
