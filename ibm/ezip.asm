  	TITLE	EZIP	Z-LANGUAGE INTERPRETER   IBM/MS-DOS 2.0 VERSION

	PAGE	58,132
	.LIST

	; *********************************************
	;         EEEEEE  ZZZZZZ  IIIIII  PPPPP
	;         EE	      ZZ    II	  PP  PP
	;         EEEE	    ZZZ	    II	  PPPPP
	;         EE	   ZZ	    II	  PP
	;         EEEEEE  ZZZZZZ  IIIIII  PP
	; *********************************************
	;

ZVERSN	EQU	"E"		;ZIP VERSION NUMBER
ZEDIT0	EQU	"0"		;ZIP MINOR EDIT FIRST DIGIT
ZEDIT1	EQU	"0"
; EZIP Edit history
	; VERSION <major-letter>[minor-digit]
	;
	; A -- 30-Apr-85 PHG
	;	0 -- Initial version.  Broken up into includes.
	;	1 -- Fix scripting of status line (screen 1)
	;	2 -- Reduce clear on split by one line
	;	3 -- Fix cancel highlight on color and fix TI-PRO
	;	4 -- fixed paging bug in gtblks and idstr (no MS)
	;	5 -- changed color italic to red
	;	6 -- fix nxtbyt if pushing word at FFFF (crossing seg)
	;	7 -- fix verify to return t/f and work as well
	;	8 -- add spec change to reset morlin on opinput
	;	Changes by Rick Lay
	;	9 -- Reworked line input section of {READIO}
	;	     Added code to eliminate italics if it's not an IBM
	;	     implemented the Status Line Refresh Request Bit in
	;		SAVE, RESTORE, and VERIFY
	;	10 -- Major rework to implement change in {DIROUT} spec.
	;		Changed "[MORE]" prompt. Modified {VERIFY} to use
	;		two digit minor edit numbers.
	;	11 -- Fixed bug in scripting that appeared after A10
	;	12 -- Really fixed that scripting bug. Also repaired Status
	;	13 -- implemented new {INTBL?} opcode.  Repaired Buffered
	;		Output {PUTCHR}.
	;	Changes by PHG
	;	14 -- fixed scripting of crlfs
	;	15 -- made command line switches for color set ibmpc flag
	; 	      to false to avoid underline when chosing mono on color
	;	      monitor.
	;	16 -- speed up
	;	17 -- fixed script on start-up, color on request for bw,
	; 	      and added refresh after every printed line.
	;	18 -- stabbed at script on restart
	;	19 -- script + refresh bugs on DOS errors
	;	20 -- fixed verify for less than 256K
	; 
	; B -- 30-Apr-85 EHB
	;	0  -- Trinity -related bugs: dirout flushing, opsplit ADDING
	;		to TOPLIN, opsplt clearing the screen, opclear(-1)
	;		falling through after unsplit/clear. Verify if fits=1
	; C -- 7-Nov-86 LSD
	;	0 -- Bureaucracy - random just was not working, rewrote so
	; 		does (LD1)
	;	1 -- Paging bugs in FINDPG (LD2)
	; D -- May 87 TAA
	;	0 -- Bureaucracy - DELAY doesn't work when no 1/100's clock,
	;		so notice that case
	;	1 -- Bureaucracy - problems with output in reverse video and
	;		latest ansi.sys, change to NOT set foreground to white
	;		before all escape sequences from highlight.
	; E -- July 87 TAA
	;	0 -- Remove somewhat bogus notion of forcing status line
	;		refresh after every carriage return when scripting

;ZIP Edit History


	; Modification History -
	;---------------------------------------------------
	;
	; 1) REWORKED FOR NEW SETUP FILE USAGE
	; 2) FIXED NON-WORKING STACK SAVE AND RESTORE FILE
	; 3) FIX RANDOM BUG - 6/25/84
	; 4) FIXED STATUS LINE (LONG ROOM DESCRIPTION) - 07/24/84
	; 5) Fixed status line bug on restart 22-Oct-84 - PHG "G"
	; 6) REWRITE OF ZIP TO USE ALL MEMORY 1-NOV-84 - PHG  "H"
	; 7) Combination of IBM and MSZIP with new copy protection
	;    for all IBM compatible machines. 11-Mar-85 - PHG
	;    Minor versions:	I -- original to test
	;			J -- fixed status line and more
	;			k -- enhanced printer timeout for IBM
	;			l -- mcrlf added to save, commenting
	;			m -- fixed status line for TI-PRO
	;			n -- added insert game disk on save
	;			     and paging problems.  Also reworked
	;			     scripting.
	;			o -- fixed script on restart, no script on
	;			     more, and minor save problems
	;			q -- fixed disk switch for verify
	;
	;			r -- fixed status line bugs and refixed
	;			     disk switch on restart.
	;			s -- restart bug on Tandy (their incompati-
	;			     bility and more on restart
	;			t -- removed extra CR from opening screen bet
	;			     ween last line of text and prompt.
	;			u -- Fixed mchri not to echo character on
	;			     more or insert game disks.
	;			v -- fixed broken restore or failed save on
	;			     low memory configuration, error in fit
	;			     calculation, /k was fixed to work any
	;			     where on the cmdline.
	;			w -- added /P for PCjr support, fixed still
	;			     broken end of memory calculation caused
	;			     by forgetting to count prelod blocks as
	;			     used memory blocks.  Also fixed opread
	; 			     so that it would flush words read great-
	;			     er than 59.
	;			x -- fix script checks from cmp to test.
	;			     to fix wishbringer. RELEASE AS H
	;		J	z -- stab at verify/paging bug, found in
	;			     newzpc when setting curtab to zero since
	;			     page is preloaded, didn't set zpcseg to
	;			     0 as well. RELEASE AS J
	;		J	1 -- fixed script bug on failed restore
	;		K	1 -- fixed save/restore bug.  Removed srblks
	;			2 -- fixed fix above which was still broken.
	;			3 -- j1 was broken by k2 edit. fixed here
	;			4 -- above
	;			5 -- fixed restart to set segs to 0
	;			6 -- stab at fixing screwy dos disk flush on
	;			     create.
	; 	
IF1 
%OUT PASS ONE
ENDIF
IF2 
%OUT PASS TWO
ENDIF
%OUT EQUATES
;******* I N C L U D E ********
INCLUDE EQUATES.EZP

; ALL SEGS ORIGINALLY POINT TO CSEG.  THE ORG 100H IS STANDARD FOR 
; PRODUCING A .COM FILE.  THE ES SEGMENT IS ADJUSTED DYNAMICALLY IN THE
; SYSINI ROUTINE TO ALLOW GAME SEGMENTS TO START AT OFFSET ZERO.
; SEE COMMENTS IN SYSINI FOR GAME SEGMENT ALLOCATION AND CALCULATION.

CSEG	SEGMENT	PARA PUBLIC
	ASSUME	CS:CSEG,DS:CSEG,ES:CSEG,SS:CSEG
	ORG	100H
;
	PUBLIC	EZIP
EZIP	PROC
	JMP	START			; SKIP OVER ZIP DATA
;
;******* I N C L U D E ********
%OUT DATA
INCLUDE DATA.EZP
%OUT MACROS
INCLUDE MACROS.EZP

	SUBTTL	SYSTEM INITIALIZATION
	PAGE	+

	PUBLIC 	START
START:	MOV	SP,OFFSET STK_TOP
	MOV	DI,OFFSET ZSTK_TP
	JMP	ZIPBGN		;JUMP TO BEGINNING OF ZIP CODE
;
;******* I N C L U D E ********
%OUT SCRNOPS
INCLUDE SCRNOPS.EZP
%OUT GAMEOPS
INCLUDE GAMEOPS.EZP
%OUT MATHOPS
INCLUDE MATHOPS.EZP
%OUT RANDOM
INCLUDE RANDOM.EZP
%OUT LOGICOPS
INCLUDE LOGICOPS.EZP
%OUT OBJOPS
INCLUDE OBJOPS.EZP
%OUT TABLEOPS
INCLUDE TABLEOPS.EZP
%OUT VAROPS
INCLUDE VAROPS.EZP
%OUT READIO
INCLUDE READIO.EZP
%OUT PRINTOPS
INCLUDE PRINTOPS.EZP
%OUT BRANCHES
INCLUDE BRANCHES.EZP
%OUT GETBYT
INCLUDE GETBYT.EZP
%OUT ZSTRINGS
INCLUDE ZSTRINGS.EZP
%OUT SETUP
INCLUDE SETUP.EZP
%OUT ZIPBEGIN
INCLUDE ZIPBEGIN.EZP
%OUT MAINLOOP
INCLUDE MAINLOOP.EZP
%OUT PAGING
INCLUDE PAGING.EZP
%OUT SYSINI
INCLUDE SYSINI.EZP
%OUT PAGEIO
INCLUDE PAGEIO.EZP
%OUT SYSIO
INCLUDE SYSIO.EZP

%OUT ENDEZIP
FINISH	PROC
	CALL	MCRLF
	MOV	BX,OFFSET STRESET
	CALL	MSPRT
	MOV	AH,4CH
	INT	21H
FINISH	ENDP

EVEN
;**********************************************************************
;        DO NOT PLACE ANY VARIABLES OR CODE BEYOND THIS POINT!  THE
;	 ZIP DYNAMICALLY ALLOCATES A PAGE TABLE AND PAGING SPACE BEYOND
;	 THIS LABEL.
;**********************************************************************
	PUBLIC	PAGTAB
PAGTAB	LABEL	BYTE
EZIP	ENDP
CSEG	ENDS

	END	EZIP
