

	TITLE	"EZIP/6502 INFOCOM, INC. --- EQUATES"

	; --------------------------
	; EZIP/6502 2.0
	; Z-CODE INTERPRETER PROGRAM
	; FOR APPLE IIE/IIC
	; --------------------------

	; INFOCOM, INC.
	; 125 CAMBRIDGEPARK DRIVE
	; CAMBRIDGE, MA 02140

	; COMPANY PRIVATE -- NOT FOR DISTRIBUTION
	PL	58		; PAGE LENGTH OF PRINTER

DEBUG	EQU	0	; ASSEMBLY FLAG FOR DEBUGGER (1 = YES)

VERSID	EQU	'I'	; VERSION OF INTERPRETER
;
; A - INITIAL RELEASE VERSION  9/15/87
;
; B - jdarnold 10/5/87
;	Fix some scrolling bugs (inc LINCNT twice in CRLF)
;	Move INC LINCNT in INPUT: to after the <CR> is hit, so
;		the FLUSHRD routine had easier check
;
; C - jdarnold 10/8/87
;	Feep on non-terminating function key inputs
;
; D - jdarnold 11/19/87
;	Don't increment LENGTH when in Window 1
;	Get side 1 size from ZENDLOD
;	Don't ask for side 2 if there is nothing more there
;
; E - jdarnold 12/1/87
;	Flush buffer before HIGHLIGHT change
;
; F - jdarnold 12/14/87
;	Wrap PRNTT in screen 0, please by using COUT
;
; G - jdarnold 2/3/88
;	Fix SCRIPTING of input line - let CLEARP put in <CR>
;
;
; I - jdarnold November 10, 1988
;	Kludge in check of lo-core SCRIPT flag in main loop
;
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
	; 13 -- IMPURE CODE TOO LARGE (BM 1/20/86)
	; 14 -- DRIVE ACCESS
	; 15 -- NOT AN EZIP GAME
	; 16 -- ILLEGAL EXTENDED RANGE X-OP

	INCLUDE 	EQ.ASM


	TITLE 	"ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT SHELL"
	INCLUDE 	HARDEQ.ASM
	INCLUDE		RWTS.ASM
	INCLUDE		ZDOS.ASM
	INCLUDE		MACHINE.ASM
	INCLUDE 	COLD.ASM

	TITLE 	"ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE"
	INCLUDE 	WARM.ASM
	INCLUDE 	MAIN.ASM
	INCLUDE 	SUBS.ASM
	INCLUDE 	DISPATCH.ASM

	TITLE 	"ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS"
	INCLUDE 	OPS012.ASM
	INCLUDE 	OPSX.ASM
	INCLUDE 	READ.ASM

	TITLE 	"ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT"

	INCLUDE 	XPAGING.ASM
	INCLUDE 	ZSTRING.ASM
	INCLUDE 	OBJECTS.ASM

	TITLE 	"ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT SHELL"

	INCLUDE	IO.ASM			; (EZIP SO COLD IS ON 1ST TRACK)

	IF	DEBUG
	INCLUDE	BUGGER.ASM
	ENDIF

	END

