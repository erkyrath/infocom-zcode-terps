	PAGE
	SBTTL "--- MEMORY ORGANIZATION ---"

TRUE	EQU	$FF
FALSE	EQU	0
LO	EQU	0
HI	EQU	1

	; SEE "HARDEQ.ASM" FOR APPLE II MEMORY MAP

	; ---------------------
	; Z-CODE HEADER OFFSETS
	; ---------------------

ZVERS	EQU	0		; VERSION BYTE
ZMODE	EQU	1		; MODE SELECT BYTE
ZID	EQU	2		; GAME ID WORD
ZENDLD	EQU	4		; START OF NON-PRELOADED Z-CODE
ZGO	EQU	6		; EXECUTION ADDRESS
ZVOCAB	EQU	8		; START OF VOCABULARY TABLE
ZOBJEC	EQU	10		; START OF OBJECT TABLE
ZGLOBA	EQU	12		; START OF GLOBAL VARIABLE TABLE
ZPURBT	EQU	14		; START OF "PURE" Z-CODE
ZSCRIP	EQU	16		; FLAG WORD
ZSERIA	EQU	18		; 3-WORD ASCII SERIAL NUMBER
ZFWORD	EQU	24		; START OF FWORDS TABLE
ZLENTH	EQU	26		; LENGTH OF Z-PROGRAM IN WORDS
ZCHKSM	EQU	28		; Z-CODE CHECKSUM WORD

	PAGE
	SBTTL "--- ZIP Z-PAGE VARIABLES ---"

ZEROPG	EQU	$80		; FIRST FREE Z-PAGE LOCATION

OPCODE	EQU	ZEROPG		; (BYTE) CURRENT OPCODE
NARGS	EQU	OPCODE+1	; (BYTE) # ARGUMENTS
ARG1	EQU	OPCODE+2	; (WORD) ARGUMENT #1
ARG2	EQU	OPCODE+4	; (WORD) ARGUMENT #2
ARG3	EQU	OPCODE+6	; (WORD) ARGUMENT #3
ARG4	EQU	OPCODE+8	; (WORD) ARGUMENT #4
ABYTE	EQU	OPCODE+10	; (BYTE) X-OP ARGUMENT BYTE
ADEX	EQU	OPCODE+11	; (BYTE) X-OP ARGUMENT INDEX

VALUE	EQU	OPCODE+12	; (WORD) VALUE RETURN REGISTER
I	EQU	VALUE+2		; (WORD) GEN-PURPOSE REGISTER #1
J	EQU	VALUE+4		; (WORD) GEN-PURPOSE REGISTER #2
K	EQU	VALUE+6		; (WORD) GEN-PURPOSE REGISTER #3

ZSP	EQU	VALUE+8		; (BYTE) Z-STACK POINTER
OLDZSP	EQU	ZSP+1		; (BYTE) OLD Z-STACK POINTER

ZPC	EQU	ZSP+2		; (3 BYTES) ZIP PROGRAM COUNTER
ZPCL	EQU	ZPC		; (BYTE) LOW 8 BITS OF [ZPC]
ZPCM	EQU	ZPC+1		; (BYTE) MIDDLE 8 BITS OF [ZPC]
ZPCH	EQU	ZPC+2		; (BYTE) HIGH BIT OF [ZPC]
ZPCFLG	EQU	ZPC+3		; (BYTE) FLAG: "TRUE" IF [ZPCPNT] VALID
ZPCPNT	EQU	ZPC+4		; (3 BYTES) ABS POINTER TO CURRENT Z-PAGE
ZPNTL	EQU	ZPCPNT
ZPNTM	EQU	ZPCPNT+1
ZPNTH	EQU	ZPCPNT+2

MPC	EQU	ZPC+7		; (3 BYTES) MEMORY PROGRAM COUNTER
MPCL	EQU	MPC		; (BYTE) LOW 8 BITS OF [MPC]
MPCM	EQU	MPC+1		; (BYTE) MIDDLE 8 BITS OF [MPC]
MPCH	EQU	MPC+2		; (BYTE) HIGH BIT OF [MPC]
MPCFLG	EQU	MPC+3		; (BYTE) FLAG: "TRUE" IF [MPCPNT] VALID
MPCPNT	EQU	MPC+4		; (3 BYTES) ABS POINTER TO CURRENT M-PAGE
MPNTL	EQU	MPCPNT
MPNTM	EQU	MPCPNT+1
MPNTH	EQU	MPCPNT+2

GLOBAL	EQU	MPC+7		;LRU+16		; (WORD) GLOBAL VARIABLE POINTER
VOCAB	EQU	GLOBAL+2	; (WORD) VOCAB TABLE POINTER
FWORDS	EQU	GLOBAL+4	; (WORD) F-WORDS TABLE POINTER
OBJTAB	EQU	GLOBAL+6	; (WORD) OBJECT TABLE POINTER

	; Z-STRING MANIPULATION VARIABLES

IN	EQU	GLOBAL+8	; (6 BYTES) INPUT BUFFER
OUT	EQU	IN+6		; (6 BYTES) OUTPUT BUFFER

SOURCE	EQU	OUT+6		; (BYTE) SOURCE BUFFER POINTER
RESULT	EQU	SOURCE+1	; (BYTE) RESULT TABLE POINTER
LINLEN	EQU	SOURCE+2	; (BYTE) LENGTH OF CURRENT LINE
WRDLEN	EQU	SOURCE+3	; (BYTE) LENGTH OF CURRENT WORD
ENTRY	EQU	SOURCE+4	; (WORD) ADDR OF CURRENT RESULT ENTRY
NENTS	EQU	SOURCE+6	; (WORD) # ENTRIES IN VOCAB TABLE
ESIZE	EQU	SOURCE+8	; (BYTE) SIZE OF VOCAB TABLE ENTRIES
PSET	EQU	SOURCE+9	; (BYTE) PERMANENT CHARSET
TSET	EQU	SOURCE+10	; (BYTE) TEMPORARY CHARSET
ZCHAR	EQU	SOURCE+11	; (BYTE) CURRENT Z-CHAR
OFFSET	EQU	SOURCE+12	; (BYTE) F-WORD TABLE OFFSET
ZFLAG	EQU	SOURCE+13	; (BYTE) Z-WORD ACCESS FLAG
ZWORD	EQU	SOURCE+14	; (WORD) CURRENT Z-WORD
CONCNT	EQU	SOURCE+16	; (BYTE) Z-STRING SOURCE COUNTER
CONIN	EQU	SOURCE+17	; (BYTE) CONVERSION SOURCE INDEX
CONOUT	EQU	SOURCE+18	; (BYTE) CONVERSION DEST INDEX

QUOT	EQU	SOURCE+19	; (WORD) QUOTIENT FOR DIVISION
REMAIN	EQU	QUOT+2		; (WORD) REMAINDER FOR DIVISION
MTEMP	EQU	QUOT+4		; (WORD) MATH TEMPORARY REGISTER
QSIGN	EQU	QUOT+6		; (BYTE) SIGN OF QUOTIENT
RSIGN	EQU	QUOT+7		; (BYTE) SIGN OF REMAINDER
DIGITS	EQU	QUOT+8		; (BYTE) DIGIT COUNT FOR "PRINTN"

TIMEFL	EQU	QUOT+9		; (BYTE) "TRUE" IF TIME MODE
LENGTH	EQU	TIMEFL+1	; (BYTE) LENGTH OF LINE IN [LINBUF]
OLDLEN	EQU	TIMEFL+2	; (BYTE) OLD LINE LENGTH
SCRIPT	EQU	TIMEFL+3	; (BYTE) SCRIPT ENABLE FLAG
LINCNT	EQU	TIMEFL+4	; (BYTE) LINE COUNTER
LMAX	EQU	TIMEFL+5	; (BYTE) MAX # LINES/SCREEN

IOCHAR	EQU	TIMEFL+6	; (BYTE) CHARACTER BUFFER
SLINE	EQU	IOCHAR+1	; (BYTE) BORDERLINE FOR SPLIT
SPSTAT	EQU	IOCHAR+2	; (BYTE) SPLIT SCREEN STATUS FLAG
LFROM	EQU	IOCHAR+3	; (WORD) "FROM" LINE ADDRESS
LTO	EQU	IOCHAR+5	; (WORD) "TO" LINE ADDRESS
;PSTAT	EQU	IOCHAR+7	; (BYTE) PRINTER STATUS FLAG
PRLEN	EQU	IOCHAR+7	; (BYTE) SCRIPT LINE LENGTH

DBLOCK	EQU	IOCHAR+8	; (WORD) Z-BLOCK TO READ
DBUFF	EQU	DBLOCK+2	; (WORD) RAM PAGE TO ACCESS (LSB = 0)
SECTOR	EQU	DBLOCK+4	; (WORD) TARGET SECTOR
GPOSIT	EQU	DBLOCK+6	; (BYTE) DEFAULT SAVE POSITION
GDRIVE	EQU	DBLOCK+7	; (BYTE) DEFAULT SAVE DRIVE
;GSLOT	EQU	DBLOCK+8	; (BYTE) DEFAULT SAVE SLOT
TPOSIT	EQU	DBLOCK+8	; (BYTE) TEMP SAVE POSITION
TDRIVE	EQU	DBLOCK+9 	; (BYTE) TEMP SAVE DRIVE
TSLOT	EQU	DBLOCK+10	; (BYTE) TEMP SAVE SLOT
DRIVE	EQU	DBLOCK+11	; (BYTE) CURRENT DRIVE

;BLINK	EQU	DBLOCK+12	; (WORD) CURSOR BLINK TIMER
;CSHAPE	EQU	BLINK+2		; (BYTE) CURRENT CURSOR SHAPE
XSIZE	EQU	DBLOCK+12	; (BYTE) SCREEN WIDTH FOR TESTS
RAND1	EQU	XSIZE+1		; (BYTE) 
RAND2	EQU	XSIZE+2		; (BYTE) RANDOM #
SPLITF	EQU	XSIZE+3		; (BYTE) WHICH WINDOW TO WRITE IN
ZPURE	EQU	XSIZE+4		; (WORD) 1ST VIRTUAL PAGE OF "PURE" Z-CODE
HOLD	EQU	XSIZE+6		; (WORD) USED BY GETBUF (PAGING)

PR1	EQU	HOLD+2		; (WORD) PAGING REGISTERS
PR2	EQU	PR1+2		; (WORD)
PR3	EQU	PR1+4		; (WORD)

	; PLACE SOME VARIABLES IN PAGE 2, NOT ENOUGH ROOM ON PG0

LRU	EQU	$280		;MPC+6		; (WORD) EARLIEST TIMESTAMP
ZCODE	EQU	LRU+2		; (WORD) 1ST ABSOLUTE PAGE OF PRELOAD
PAGE0	EQU	LRU+4		; (WORD) 1ST PAGE OF ACTUAL SWAPPING SPACE
PMAX	EQU	LRU+6		; (WORD) MAXIMUM # OF SWAPPING PAGES
ZPAGE	EQU	LRU+8		; (WORD) CURRENT SWAPPING PAGE
TARGET	EQU	LRU+10		; (WORD) TARGET PAGE FOR SWAPPING
STAMP	EQU	LRU+12		; (WORD) CURRENT TIMESTAMP
SWAP	EQU	LRU+14		; (WORD) EARLIEST BUFFER
TSTVAL	EQU	LRU+16		; (BYTE) VALUE TO TEST FOR MEMORY EXISTENCE
				; CAN'T USE PG0 CAUSE SWAP TO AUX MEMORY.
PG2SIZ	EQU	17		; SIZE OF VARIABLE MEMORY IN PG 2 (ABOVE)

	END

