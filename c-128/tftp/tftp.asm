	PAGE
	STTL "--- MACHINE COLDSTART: CBM64 ---"

	INCLUDE EQ.ASM
	INCLUDE HARDEQ.ASM

	ORG	$4000
TFTP:
	LDA	#%00001110	; SET UP CONFIGURATION
	STA	CR

	LDA	#%00001001	; 80 Column display
				; Fast disk i/o
				; 8502 processor
	STA	MCR		; and save it

	LDA	#$01		; set fast mode!
	STA	FASTER		; 

	LDA	#$0E
	JSR	CHROUT		; USE UPPER/LOWER CHARS

	LDA	#0
	JSR	SETMSG		; DISABLE KERNAL MESSAGES

	CLD
	LDX	#$FF
	TXS

;	JSR	CLALL		; CLOSE EVERYTHING

	JMP	begin

	; ---------------
	; WARMSTART ENTRY
	; ---------------

tftp1msg:
	DB	RVSON,YELLOW
	DB	CLS,"Commodore C128 TFTP Rev 1.0 == 2400 BAUD ==",EOL,EOL,LRED
	DB	"Please select TFTP option from list below:",EOL
	DB	"   F - Format Disk",EOL,EOL
	DB	"   8 - C128 XZIP Interpreter",EOL
	DB	"   4 - C64 XZIP Interpreter",EOL,EOL
tftp1len equ 	$-tftp1msg
tftp2msg:
	DB	"   1 - Story Side 1",EOL
	DB	"   2 - Story Side 2", EOL,EOL
	DB	"   L - LZIP Story Side 1",EOL,EOL
	DB	"   A - C128 Font 1 (on Side 1)",EOL
	DB	"   B - C128 Font 2 (on Side 1)",EOL,EOL
	DB	"   C - C64 FastCode (on Side 2)",EOL,EOL
	DB	"   X - Exit and return to BASIC",EOL
tftp2len EQU	$-tftp2msg

;
; this is where all the commands return to when all done
;
begin:
	MSG	tftp1
	MSG	tftp2
GLOOP:
	JSR	GETIN
	TAY
	BEQ	GLOOP		; wait for command key

	CMP	#'F'		; format disk
	bne	ck1
	jmp	formatter
ck1:
	cmp	#'8'		; get c128 xzip interpreter
	bne	ck2
	jmp	getc128
ck2:
	cmp	#'4'		; get c64 xzip interpreter
	bne	ck3
	jmp	getc64
ck3:
	cmp	#'1'		; get story, side 1
	bne	ck4
	jmp	gets1
ck4:
	cmp	#'2'		; get story, side 2
	bne	ck5
	jmp	gets2
ck5:
	cmp 	#'A'		; get c128 font 1
	bne	ck6
	jmp	getf1
ck6:
	cmp	#'B'		; get c128 font 2
	bne	ck7
	jmp	getf2
ck7:
	cmp	#'C'		; get c64 fast code
	bne	ck8
	jmp	getfc
ck8:
	cmp	#'X'		; all done
	bne 	ck9	
	jmp 	(RESET)		; so quit then
ck9:
	cmp	#'L'		; put on lzip side one
	bne	ck10
	jmp 	gets1l
ck10:
	jmp	GLOOP		; just wait some more

	INCLUDE FORMAT.ASM
	INCLUDE SUBS.ASM
	INCLUDE	DISK.ASM
	INCLUDE	COMM.ASM
	INCLUDE MAIN.ASM
	
	END

