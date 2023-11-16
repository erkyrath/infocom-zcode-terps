	TITLE	"Apple ][ YZIP (c)Infocom","BOOT CODE SUBROUTINES"

ERRM:	DB	EOL,EOL
	DB	"Internal error "
ENUMB:	DB	"00"
	DB	"00.  "
ERRML	EQU	$-ERRM
HCHARS:	DB	"0123456789ABCDEF"
;
; ZERROR: print out error code in HexWord: [A][Y].
;
ZERROR:
	pha			; save [A]
	tya			; and put y to a for figgering
	ldy	#3		; CONVERT ERROR BYTE IN [A]
ZERR0:
	pha
	and	#%00001111
	tax
	lda	HCHARS,X
	sta	ENUMB,Y	
	pla
	LSR	A
	LSR	A
	lsr	A
	lsr	A
	tax
	lda	HCHARS,X
	dey
	sta	ENUMB,Y	
	beq	ZR1		; done
	
	pla			; done with first number
	dey			; and point one more down
	jmp	ZERR0		; so work on second number
ZR1:
	ldx	# LOW ERRM
	lda	# HIGH ERRM
	ldy	#ERRML
	jsr	DISPLAY_LINE	; PRINT ERROR MESSAGE
	jsr	GETRET		; wait for <CR>
	jmp	ZQUIT

RETQ:	db	EOL,"Please insert new disk to boot."
	db	EOL, "And hit RETURN when ready.",EOL
RETQL	EQU	$-RETQ
GETRET:
	DLINE	RETQ, 		; ask for return
GETRETL:
	lda	KBD		; get a key
	bpl	GETRETL 	; no key
	and	#$7f		; make a good key
	cmp	#EOL		; return key?
	bne	GETRETL		; nope
	jsr	MCOUT		; show the <CR>
	rts
		

	; ----
	; QUIT
	; ----

ZQUIT:
;
; re-enable /RAM
;
	ldx	DEVCNT		; put device in at end
	inx			; point to one past end
	stx	DEVCNT		; show new count
	lda	#$BF		; /RAM ID
	sta	DEVNUM,X	; save it
	lda	OLDVEC+HI	; restore old vector
	sta	RAMVEC+HI
	lda	OLDVEC+LO
	sta	RAMVEC+LO
;
; now format /RAM
;
	lda	#3		; stash into FORMAT parm block
	sta	$42		; it goes here, oddly enough
	lda	#$B0		; device id for /RAM
	sta	$43		; and here it goes
	lda	# HIGH GAME1FIO	; IO buffer
	sta	$45		; this is where it goes
	lda	# LOW GAME1FIO	; lsb
	sta	$44
	sta	RDBNK2		; set up card for driver
	jsr	RAMDRV		; format /RAM
	sta	RDROM		; get ROM back
	sta	RDROM		; get ROM back
	
	sta	TEXTSW+ON	; turn on text
	QUIT	QUIT_PB		
QUIT_PB:
	db	4		; 4 parms
	db	0,0,0,0,0,0	; just zeros
RAMDRV:
	jmp	(RAMVEC)	; goto RAM driver

;
; Jump here when ProDOS has a problem
;
;  [A] - ProDOS error
;  [Y] - my error code
;
BOOT_ERROR:
	JMP	ZERROR		; just use error routine
;
; Make sure we are on a good machine, like a ][c or ][e+/][gs
;		
MACHINE:
	lda	MACHID1		; Machine ID part 1
	cmp	#6		; must be a 6 for me to be interested
	bne	BADMACH		; nope
	lda	MACHID2		; Machine ID part 2
	bne	MACH1		; must be a ][e or ][gs
	lda	#IIcID		; Apple ][c thank you
	jmp	MACH2		; and save it
MACH1:
	sec			; get ready to check machine
	jsr	MACHCHK		; check for 'new' machine
	bcs	OLDMACH		; must just be a ][e
	lda	#IIgsID		; this is a ][gs
	bne	MACH2		; done
OLDMACH:
	lda	#IIeID		; this is IIe
MACH2:
	sta	MID		; save machine id	
	rts
BADMACH:
	lda	#30		; bad machine error
	ldy	#$FF		; nothing here
	jmp	ZERROR		; and end it all

; -----------------------
; DIRECT PRINT LINE [X/A]
; -----------------------
; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)
; STRING LENGTH IN [Y]

DISPLAY_LINE:	STX 	STRING+LO	; DROP STRING ADDRESS
	STA	STRING+HI	; INTO DUMMY BYTES
	STY	J		; COUNTER
	LDX	#0		; INIT CHAR-FETCH INDEX
DOUT:	DB	$BD		; 6502 "LDA nnnn,X" OPCODE
STRING:	DW	$0000		; DUMMY OPERAND BYTES
	ORA	#%10000000	; ELSE MAKE IT NORMAL
DOUT1:	JSR	MCOUT
	INX
	DEC	J		; LOOP TILL
	BNE	DOUT		; OUT OF CHARS
	RTS		
;
; set [A](page), [Y](bank) to point to memory page where page in [A] is
;
SETPC:			
	sta	MEMPAGE		; save it for later addition
	cmp	#P2PAGE 	; IS IT A PAGE IN MAIN
	bcs	VF2		; No, it might be in aux mem

	lda	# HIGH ZBEGIN 	; ADD OFFSET TO GET RAM PAGE
	ldy	#MAIN		; in the main bank
	beq	VFEXI		; BRA to fetch
VF2:
	cmp	#PGBEGIN	; is it paged?
	bcs	VFERR		; yes it be paged, so can't deal with it
	cmp	#P3PAGE		; is it in Aux Mem, Part 2?
	bcs	VF3		; yes, so subtract different amount
;
; this is in lower aux 
;				
	lda	#(Z2PAGE-Z1SIZE) ; subtract size from offset
	ldy	#AUX		; show aux mem
	bne	VFEXI		; jump to end
VF3:
	lda	#(Z3PAGE-(Z1SIZE+Z2SIZE)) ; subtract out first 2 sides
	ldy	#P3BANK		; show page 3 bank
VFEXI:
	clc			; get ready for addition
	adc	MEMPAGE		; now get actual offset
	rts	
VFERR:
;
; out of range
;
	lda	#$22
	jmp	ZERROR

;
; FETCHB - fetch a byte from either main memory, aux memory, or upper
;	Aux memory
;
FETCHB:
	ldy	FPCBNK		; get the bank
	bmi	FB1		; must be in upper RAM
	jmp	ZERO_FB		; go to low end fetch
;
; this is in aux mem, >$E000
;
FB1:
	lda	FPCH		; get which page
	sta	FBMOD+2		; and show in operand
	ldy	FPCL		; get which byte
	sta	ALTZP+AUX	; talk about aux mem
	lda	BNK1SET		; set up read/write ram in upper mem
	lda	BNK1SET		; set up read/write ram in upper mem
FBMOD:	lda	Z3BEGIN,Y	; get the sucker
	sta	ALTZP+MAIN	; go back to main mem
	ldx	RDROM		; now use ROM please
	ldx	RDROM		; now use ROM please
	rts
;
; NEXTFPC - inc DPCL and check for wrapping round to next bank
;
NEXTFPC:
	inc	FPCL		; next lo byte
	bne	NXFP_EXIT	; no change then
	inc	FPCH		; next page
	lda	FPCH		; and get it for checking
	cmp	# HIGH PRGLBL	; have we reached end of line?
	bne	NXFP_EXIT	; we be okay
	lda	FPCBNK		; get bank
	bne	NXFP1		; must skip over stuff in middle
	inc	FPCBNK		; so point to aux bank
	lda	#Z2PAGE		; first page in aux
	sta	FPCH		; and point to it
	rts			; toots finis
NXFP1:
	lda	#Z3PAGE		; start of part 3
	sta	FPCH		; so show me
	lda	#P3BANK		; and point to this bank
	sta	FPCBNK		; okey
NXFP_EXIT:
	rts
;
; INST_MOUSE - install mouse handler IRQ
;
MFIX1	EQU	(MSX1-MOUSERx)+PAGE3STUFF+2
MFIX2	EQU	(MSX2-MOUSERx)+PAGE3STUFF+1
MFIX3	EQU	(MSX4-MOUSERx)+PAGE3STUFF
MFIX4	EQU	(MSX3-MOUSERx)+PAGE3STUFF+1
INST_MOUSE:
	jsr	FINDMOUSE	; find which slot the mouse is in
	bcc	INMS1		; everything is fine
	rts			; can't find the mouse
INMS1:
	sta	MFIX1
	sta	MFIX2
	sta	MFIX3		; okay, everyone knows now
	and	#$0f		; pick up low byte
	sta	ARG1+LO		; save for program to pick up
	asl	A		; shift slot number to upper nybble
	asl	A
	asl	A
	asl	A
	sta	MFIX4		; and save here
;
; now init mouse, please
;
	ldx	#INITM		; init first
	jsr	MOUSER		; so do it
	ldx	#SETM		; setup mouse
	lda	#$01		; transparent mode
	jsr	MOUSER		; we do that
	ldx	#CLEARM		; and clear mouse pos
	jsr	MOUSER		; we did
;
; now set min and max for clamping.
; 0 <= xpos <= 139
; 0 <= ypos <= 191
;
	lda 	#0		; zero out a few of the things
	sta	CLMPMINH	; hi part of min clamp
	sta	CLMPMINL	; lo part of min clamp
	lda	# LOW 137		; max X
	sta	CLMPMAXL	; stash in lo part
	lda	# HIGH 137		; max X
	sta	CLMPMAXH	; hi part of clamp
	lda	#0		; show setting X clamp
	ldx	#CLAMPM		; do clamping
	jsr	MOUSER		; okay, we did
	lda	# LOW 189		; now for Y
	sta	CLMPMAXL	; saved in lo part
	lda	# HIGH 189		; now for Y
	sta	CLMPMAXH	; saved in lo part
	lda	#1		; set y boundries
	ldx	#CLAMPM		; okay, show which routine
	jsr	MOUSER		; and do it

;
; initialize the mouse position
;
	lda	#0		; clear MSB
	sta	MOUSEXH
	sta	MOUSEYH
	lda	#MAXHEIGHT
	sta	MOUSEYL
	lda	#MAXWIDTH
	sta	MOUSEXL

	ldx	#POSM	
	jsr	MOUSER		; and move it

;	cli			; enable interrupts now
	lda	#$FF		; < 0 show mouse
	sta	MOUSEF		; so show it
	clc			; show everything is fine
	rts			; done
;
; FINDMOUSE - scan the slots for a mouse
;
FINDMOUSE:
	lda	$C1FB		; check for mouse ID == D6
	cmp	#$D6		; mouse ID
	bne 	FNDM1		; not it
	lda	FINDMOUSE+2	; get slot
	clc			; show okay-ness
	rts			; and return
FNDM1:
	inc	FINDMOUSE+2	; point to next slot
	lda	FINDMOUSE+2	; reached last one yet?
	cmp	#$C8		; no slot eight
	bcc	FINDMOUSE	; go look again
	sec			; show no mouse
	rts
;
; INST_STICK - install the joystick handler
INST_STICK:
	lda	#$1		; > 0 show joystick
	sta	MOUSEF		; okay, we do
	clc			; show it worked
	rts
RAMINFO:
	db	10		; 10 parms
	dw	RAMNAME	; name
	ds	15		; room for stuff
RAMNAME:
	db	5		; length of name
	db	"/RAM/" 	; name
RAMQ:	db	EOL,"This program disables /RAM and it",EOL
	db	"is not empty.  Do you want to"
	db	EOL,"continue (Y destroys /RAM)?"
RAMQL	EQU	$-RAMQ
FIXRAM:
	GET_FILE_INFO RAMINFO	; get the volumn size
	bcs	FIXR1		; problem, so we'll just disable it

	lda	RAMINFO+9	; get amount used
	bne	ASKUSER		; some files there complain
	lda	RAMINFO+8	; get LSB of used
	cmp	#9		; if >= 9, must be being used
	bcc	FIXR1		; no problem, disable /RAM
ASKUSER:
	jsr	MHOME		; clear and home
	DLINE	RAMQ, 		; ask about continuing
	jsr	MRDKEY		; get answer
	and	#$7F		; turn off hi bit
	cmp	#'y'		; check for yes
	beq	FIXR1		; then continue
	cmp	#'Y'		; also, captital Y
	beq	FIXR1		; okay again
	jmp	ZQUIT		; so die
FIXR1:
	lda	RAMVEC+LO	; get current /RAM vector
	sta	OLDVEC+LO	; save it for reinstalling
	lda	RAMVEC+HI
	sta	OLDVEC+HI
	lda	UNSVEC+LO	; point /RAM vector to uninstalled device
	sta	RAMVEC+LO
	lda	UNSVEC+HI
	sta	RAMVEC+HI
	ldx	DEVCNT		; get how many devices
DEVLP:
	lda	DEVNUM,X	; try to find /RAM device
	and	#$70		; isolate slot
	cmp	#$30		; look for slot 3
	beq	GOTSLT		; got it
	dex			; missed it
	bpl	DEVLP		; try again
	rts			; didn't find it (?)
GOTSLT:
	lda	DEVNUM+1,X	; now move all down one
	sta	DEVNUM,X	; down one
	cpx	DEVCNT		; done yet
	beq	FIXREX		; ayyup
	inx			; point to next slot
	bne	GOTSLT		; continue on
FIXREX:
	dec	DEVCNT		; one less device, thank you
	rts			; all done
;
; DO_DSEGS - scan through the disk segment table and point to the
;	beginning of the segment list for each of the disks in the
;	file.
DO_DSEGS:
	lda	SEGTBL+SGTDISKS+1	; get # number disks
	sta	J		; save in counter
	dec	J		; we hand hold #1
	lda	# HIGH (SEGTBL+SGTCHKS1)	; start off at beginning
	sta	D1SEG+HI	; here is first one
	lda	# LOW (SEGTBL+SGTCHKS1); and here is LSB
	sta	D1SEG+LO	; save it
	ldy	SEGTBL+SGTSEG1+1	; get number of segs in first disk
	ldx	#2		; start at second one
DODSL:
	lda	DSKSEG-2+LO,X	; get LSB of previous start
	clc			; doing some adding
	adc	#SGTSEG 	; point to start of segment list
	sta	DSKSEG+LO,X	; save in working segment pointer
	lda	DSKSEG-2+HI,X	; now work on MSB
	adc	#0		; pick up possible carry
	sta	DSKSEG+HI,X	; and save in new pointer
ADDLOOP:
	clc			; doing add
	lda	#6		; size of segment entry
	adc	DSKSEG+LO,X	; counting
	sta	DSKSEG+LO,X	; counting
	bcc	ADDL1		; no wrap
	inc	DSKSEG+HI,X	; add one to hi
ADDL1:
	dey			; count segment
	bne	ADDLOOP		; okay, not done yet
	lda	DSKSEG+LO,X	; get pointer to #segs in this one
	sta	DSEGS+LO	; save ptr
	lda	DSKSEG+HI,X	; get MSB
	sta	DSEGS+HI	; save ptr
	ldy	#SGTNSEG+1	; just need LSB
	lda	(DSEGS),Y	; get #segs in this segment
	tay			; save in counter Y
	inx
	inx			; point to new pointer loc
	dec	J		; count this disk
	bne	DODSL		; and do it again
;
; now, init DSEGS to point to beginnning again
;
	lda	D1SEG+HI	; here is first one
	sta	DSEGS+HI
	lda	D1SEG+LO	; save it
	sta	DSEGS+LO
;
; check to see if we are using infoDOS, and set pointer to second
; disk segment table if we are.
;
        lda     KVERSION        ; get "version" number
        cmp     #INFODOS_ID     ; is in our DOS?
        bne     DODX            ; nope, it ain't
        lda     D2SEG+HI        ; then have infoDOS flag to  point
        sta     INFODOS+HI      ; to the D2SEG, cuz that's what
        lda     D2SEG+LO        ; we use to check all the ZPC's
        sta     INFODOS+LO
        rts
DODX:
        lda     #0              ; regular ProDOS
        sta     INFODOS         ; show nothing there!
	rts	

;
; INITPAG - init the paging buffers, thank you
;
INITPAG:
	LDX	#NUMBUFS-1	;***
	STX	CURRENT	  	; make last one first one to be used
	dec	CURRENT		; using only even pages
	LDA	#$FF	
INILP:			
	STA	VPAGEH,X	
	DEX		
	BPL	INILP	
	LDX	#0	
	LDY	#2		;***
INILP2:			
	TYA		
	STA	PREVPNT,X	
	INX	  
	inx			;***
	INY
	iny			;***
	CPX	#NUMBUFS	
	BCC	INILP2	
	LDA	#00	
	DEX		
	dex			;***
	STA	PREVPNT,X	
	LDX	#0		;***
	LDY	#$FE		;***
	LDA	#NUMBUFS-2	;***
INILP3:			
	STA	NEXTPNT,X	
	INX		
	inx			;***
	iny			;***
	INY		
	TYA		
	CPX	#NUMBUFS	
	BCC	INILP3	
	rts

	END


