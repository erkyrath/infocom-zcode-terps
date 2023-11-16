
	TITLE "EZIP/6502-C INFOCOM, INC. --- EQUATES"

	; --------------------------
	; XZIP/6502 VERSION A
	; Z-CODE INTERPRETER PROGRAM
	; FOR COMMODORE 128
	; --------------------------

	; INFOCOM, INC.
	; 125 CAMBRIDGEPARK DRIVE
	; CAMBRIDGE, MA 02140

	; COMPANY PRIVATE -- NOT FOR DISTRIBUTION

	LLCHAR	?
	PL	58		; PAGE LENGTH OF PRINTER

ZEROPG	EQU	$03		; START OF FREE Z-PAGE RAM
ZPGTOP	EQU	$8F		; END OF FREE Z-PAGE RAM

DEBUG	EQU	0		; ASSEMBLY FLAG FOR DEBUGGER (1=YES,0=NO)

	; -----------
	; ERROR CODES
	; -----------

	; 00 -- INSUFFICIENT RAM
	; 01 -- ILLEGAL X-OP
	; 02 -- ILLEGAL 0-OP
	; 03 -- ILLEGAL 1-OP
	; 04 -- ILLEGAL 2-OP
	; 05 -- Z-STACK UNDERFLOW
	; 06 -- Z-STACK OVERFLOW
	; 07 -- ILLEGAL PROPERTY LENGTH (GETP)
	; 08 -- DIVISION BY ZERO
	; 09 -- ILLEGAL ARGUMENT COUNT (EQUAL?)
	; 10 -- ILLEGAL PROPERTY ID (PUTP)
	; 11 -- ILLEGAL PROPERTY LENGTH (PUTP)
	; 12 -- DISK ADDRESS OUT OF RANGE
	; 13 -- PRELOAD/IMPURE TOO LARGE
	; 14 -- DRIVE ACCESS
	; 15 -- Z-STACK DESTROYED
	; 16 -- NOT AN EZIP GAME
	; 17 -- BAD FONT FOUND

	; NOTE: DISK ACCESS IS SET SO THAT SIDE 1 STARTS ON TRACK 3
	; SIDE 2 ON TRACK 1 AND SAVES STARTING ON TRACK 1. THIS IS IN 
	; THE CODE IN ZDOS.ASM (APPEARS NOWHERE ELSE 3/18/86 LE)

VERSID	EQU	'K'	; VERSION OF INTERPRETER
;
; A - Initial Released Version
;
; B - Combined c64/c128 version
;	Mostly, just remember to skip Tr20, Sect0 because of FASTCODE
;
; C - jdarnold 10/5/87
;	Fix some scrolling problems in CRLF
;
; D - jdarnold 10/7/87
;	Don't allow function keys unless they are terminators
;
; E - jdarnold 10/14/87
;	Don't clear the screen before asking for SIDE X of disk
;
; F - jdarnold 11/17/87
;	Make SCR width == HWRD (i.e. both 79)
;	Don't do wrapping in Screen 1
;	Save X Pos as LENGTH in CurSet
;	If reverse video and a <CR> goes out, then turn it back on
;		(as the C128 turns it off after a <CR>)
;	Don't update LENGTH counter if in Window 1
;
; G - jdarnold 12/1/87
;	Make for maximum of 9 saves, thank you
;
; H - jdarnold 12/14/87
;	Don't clear buff if in window 0 in PRNTT
;	Call COUT instead of unbuffered output in PRNTT
;	Don't clear buff when turning off screen
;
; I - jdarnold 2/17/88
;	Make sure we have at least 3 expansion RAM banks in MEMCHK
;
; J - jdarnold 2/24/88
;	Don't let XEXIST get greater than 8
;
; K - jdarnold 3/1/88
;	Make SRHOLD be a word, not a byte
;
	INCLUDE EQ.ASM

	TITLE "XZIP/6502-C128 INFOCOM, INC. --- MACHINE DEPENDENT INIT"
	INCLUDE HARDEQ.ASM
	INCLUDE MEMORY.ASM
	INCLUDE COLD.ASM

	TITLE "XZIP/6502-C128 INFOCOM, INC. --- INIT & MAINLINE"
	INCLUDE WARM.ASM
	INCLUDE MAIN.ASM
	INCLUDE SUBS.ASM
	INCLUDE DISPATCH.ASM

	TITLE "XZIP/6502-C128 INFOCOM, INC. --- OPCODE EXECUTORS"
	INCLUDE OPS012.ASM
	INCLUDE OPSX.ASM
	INCLUDE READ.ASM

	TITLE "XZIP/6502-C128 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT"
	INCLUDE XPAGING.ASM
	INCLUDE ZSTRING.ASM
	INCLUDE OBJECTS.ASM

	TITLE "XZIP/6502-C128 INFOCOM, INC. --- MACHINE DEPENDENT I/O"

	INCLUDE IO.ASM
	INCLUDE MACHINE.ASM
	INCLUDE ZDOS.ASM
	INCLUDE DISK.ASM

	IF	DEBUG
	INCLUDE BUGGER.ASM
	ENDIF

	STTL "END ADDR OF INTERPRETER"

	END

