	PAGE	
	STTL "--- MEMORY ORGANIZATION ---"

TRUE	EQU	$FF	
FALSE	EQU	0	
LO	EQU	0	
HI	EQU	1	

	; ---------
	; CONSTANTS
	; ---------

EOL	EQU	$0D		; EOL CHAR
SPACE	EQU	$20		; SPACE CHAR
BACKSP	EQU	$14		; BACKSPACE
LF	EQU	$0A		; LINE FEED
CLS	EQU	$93		; CLEAR SCREEN, HOME CURSOR
ESCAPE	EQU	$1B		; ESCAPE KEY

;
; tftp constants
;
CMND	EQU	$7E		; command for tftp
DATACMND EQU	$00		; data following
EOF	EQU	$04		; end of file
ACK	EQU	$02		; acknowledge good block
NAK	EQU	$05		; bad block here

	; FONT 1 & 2 OFFSETS

FONT1T	EQU	32		; TRACK & SECTOR TO FIND
FONT1S	EQU	0		; FONT FILES ON
FONT2T	EQU	33
FONT2S	EQU	0

C128T	EQU	1		; C128 interpreter start track 
C128S	EQU	0		; and sector
C64T	EQU	20		; C64 interpreter start track
C64S	EQU	0		; and sector
LSIDE1T	EQU	3		; side 1 start track for LZIP games
LSIDE1S	EQU	0		; and sector
SIDE1T	EQU	5		; side 1 start track
SIDE1S	EQU	0		; and sector
SIDE2T	EQU	1		; side 2 start track
SIDE2S	EQU	0		; and sector
FASTT	EQU	20		; fastcode, side 2, start track
FASTS	EQU	0		; and sector

	END
