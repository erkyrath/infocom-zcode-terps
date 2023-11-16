
    xdef    _pump_sound		;defined below, called from C
    xdef    _stop_sound
;;; xdef    check_sound		;(called from 68K)

    xref    _init_sound		;defined in C
    xref    _zsound

; lowmem globals

conterm	    equ	$484		;(byte) console attribute bits
_hz_200     equ $4BA		;(long) raw 200 Hz system timer

; macro defs (should be integrated into STX3 ...)

SAVEREGS    MACRO
	MOVEM.L	D1-D2/A1-A2,-(SP)	* used by ST bios/xbios (gemdos?)
    ENDM
	
RESTREGS    MACRO
	MOVEM.L	(SP)+,D1-D2/A1-A2
    ENDM

FLUSH       MACRO
	ADD.W	#\1,SP		* assemble into ADDQ or ADDA
    ENDM
	
* ----------------------
* INITSND
* ----------------------

* called conditionally; setup for special (sampled) sounds

INITSND	SAVEREGS
	jsr	_init_sound		* allocate buffer, etc
	RESTREGS
	rts

* ----------------------
* DOSOUND
* ----------------------

DOSOUND	CMPI.W	#1,D0		* FIRST CHECK FOR SIMPLE CASES
	BEQ	BEEP
	CMPI.W	#2,D0
	BEQ	BOOP

* ADJUST VOLUME: 0..64, -1=USE MIDI VOL
	TST.W	D2
	BLT.S	DOSDX1
	ASL.W	#3,D2		* MAP 0-8 TO 0-64

* ADJUST COUNT: 0=INFINITE, 1..254=FINITE, -1=USE MIDI COUNT
DOSDX1	TST.W	D3
	BGT.S	DOSDX2		* IF 1-254, JUST PASS ALONG
	ADDQ.W	#1,D3		* MAP -1 TO 0
	BEQ.S	DOSDX2
	MOVEQ	#-1,D3		* MAP 0 TO -1

DOSDX2	SAVEREGS
	MOVE.L	D3,-(SP)	* COUNT
	MOVE.L	D2,-(SP)	* VOLUME
	MOVE.L	D1,-(SP)	* ACTION
	MOVE.L	D0,-(SP)	* ID
	JSR	_zsound
	FLUSH	16
	RESTREGS
	RTS

* ----------------------
* BEEP, BOOP
* ----------------------

BEEP	lea	dbeep,a0
	bra.s	bxxp
BOOP	lea	dboop,a0

bxxp	SAVEREGS
	move.l	a0,a1			;save arg here
	moveq	#1,d0			;must be Super to read lowmem
	bsr	super

; check the "bell" global to avoid colliding with an ongoing pump_sound

	move.b	conterm,d0
	btst.b	#2,d0			;sound disabled globally?
	beq.s	bxpx2			;yes, us too

	move.l	a1,-(sp)		;sound "command list"
	move.w	#32,-(sp)		;call sound daemon
	trap	#XBIOS
	addq.w	#6,sp			;flush stack  [a1 now trashed]

	moveq	#2,d0			;wait 1/10 sec
	bsr	delay

	pea	dstop
	move.w	#32,-(sp)		;halt sound
	trap	#XBIOS
	addq.w	#6,sp			;flush stack

bxpx2	moveq	#0,d0
	bsr	super
	RESTREGS
	rts

    data
dbeep
	dc.b	0,$90			;tone period, fine, channel A 
	dc.b	1,0			;  coarse
	dc.b	7,$FE			;enable tone, chan A only
	dc.b	8,$0F			;max volume, chan A
	dc.b	255,0
dboop
	dc.b	0,$40			;(4x beep = 2 octaves under beep)
	dc.b	1,$02
	dc.b	7,$FE			;enable tone, chan A only
	dc.b	8,$0F			;max volume, chan A
	dc.b	255,0
dstop
	dc.b	7,$FF			;stop all sound
	dc.b	255,0
    text

*-----------------------
* delay
*-----------------------

; d0.w = delay in 20ths.  Our caller must be in Super mode.

delay	mulu	#10,d0			;convert to 200ths (--> d0.l)
	add.l	_hz_200,d0
dlyx1	cmp.l	_hz_200,d0		;wait for sysclk to catch up
	bgt.s	dlyx1			;not yet
	rts

* ----------------------
* GAMINT, GAMINT1
* ----------------------

* PERIODICALLY CHECK FOR GAME INTERRUPTS

* NOTE: BECAUSE THE INTERPRETER AS A WHOLE IS /NOT/ RE-ENTRANT, THIS 
* ROUTINE CHOULD BE CALLED ONLY FROM THE TOP OF THE MAIN LOOP (NXTINS), AND
* (PERHAPS) FROM OPREAD/OPINPUT (SO SOUNDS CAN CHAIN BETWEEN MOVES).  
* BUT IT SHOULD /NOT/ BE CALLED FROM ITTYIN, SINCE THAT IS ALSO CALLED 
* FROM [MORE], AND THE INTERPRETER STATE IS UNDEFINED.

SKPCNT	EQU	50	* 2500 ops/sec (max) ==> 50 ints/sec (max)

* ENTRY POINT FROM MAIN LOOP
* To avoid cutting into interpreter performance too much, we keep a
* simple counter, and only occasionally perform the (somewhat costlier) 
* check for a sound interrupt.

GAMINT	SUBQ.W	#1,SCOUNT(A6)	* TIME FOR A SOUND CHECK?
	BLE.S	GAMIX1
	RTS			* NO, QUICK EXIT
GAMIX1	MOVE.W	#SKPCNT,SCOUNT(A6)    * YES, RESET COUNTER AND FALL THRU

* ENTRY POINT FROM INPUT-WAIT LOOP
* Probably safe to call an interrupt function as long as it doesn't itself 
* call OPREAD/OPINPUT.

GAMINT1
***	SAVEREGS		; not necessary
	BSR	check_sound	* END-OF-SOUND CHECK (68K)
	TST.W	D0
	BEQ.S	GAMIX3

* (could check first for valid SFUNC(A6), skip end-check if none,
* but then end event would hang around unreported ...)

	MOVE.W	SFUNC(A6),D0	* SOUND-INTERRUPT FUNCTION ("START NEXT")
	BEQ.S	GAMIX3		* NONE
	BSR	INCALL		* CALL IT (INTERNALLY)
***	TST.W	D0		* (NO RETURN VAL)
GAMIX3
***	JSR	_int_key	* TYPE-AHEAD CHECK (here or from real int?)
***	RESTREGS
	RTS

; -----------------------------------------------------------------------------
; Sound driver for ST XZIP
;   adapted from Glyn H. Anderson's PUMPSND
; -----------------------------------------------------------------------------

; register conventions: in interrupt code, must preserve everything, 
; elsewhere, preserve all except a0/d0

CLKFRQ	    equ 8000000		;cpu clock freq (8 MHz)
CLKFRQ2	    equ 2457600		;68901 clock freq (observed value)
GEMDOS	    equ 1		;trap number
XBIOS	    equ 14

; memory-mapped hardware

GISelect    equ $FFFF8800	;GI sound chip register select
GIRead	    equ GISelect	;GI sound chip register read
GIWrite	    equ GISelect+2	;GI sound chip register write

mfp	    equ $FFFFFA01	;68901 multifunction peripheral chip
isra	    equ 14		;interrupt in-service register A

*-----------------------
* _pump_sound
*-----------------------

; setup a sound & activate our sound interrupt driver
; VOID pump_sound (buffer, reps)  char *buffer; WORD reps;

_pump_sound
; logically, should first check/stop any ongoing sound
	bsr	_stop_sound		;(for ST, probably doesn't matter)

	move.w	8+2(a7),d0		;get caller's repeat count
***	clr.w	d0
***	move.b	2(a0),d0		;get data's repeat count (byte)
	move.w	d0,i_reps		;store

	move.l	4(a7),a0		;get pointer to data buffer
	move.l	a0,i_samp		;store
	addq.l	#8,a0			;point to data length
	moveq	#0,d0
	move.w  (a0)+,d0		;get it
	move.l	a0,i_begin		;store pointer to start of data
	move.l	a0,i_p			;init the roving pointer
	adda.l	d0,a0
	move.l	a0,i_end		;store pointer to end of data (+1)
	clr.w	i_done			;reset status flag

; enter Super mode, disable keyclick & bell (anything else?)

	moveq	#1,d0
	bsr	super

	move.b	conterm,d0
	move.b	d0,oldct		;save current settings
	andi.b	#$FA,d0
	move.b	d0,conterm		;disable os sounds

; disable interrupts and atomically adjust Mixer control bits 
; (don't disturb I/O control bits)

	move	sr,d0			;get status reg
	move.w	d0,oldSR
	or.w	#$0700,d0		;disable all interrupts
	move	d0,sr

	move.l	#GISelect,a0
	move.b	#$07,(a0)		;request mixer controls
	move.b	(a0),d0			;get controls
	ori.b	#$3F,d0			;disable all noise and tones
	move.b	d0,2(a0)		;set controls

	move	oldSR,sr		;re-enable interrupts

; calculate interrupt rate, and install our handler

	tst.l	t_clock			;inited?
	bne.s	pmpsx2			;yes
	bsr	calibrate		;first time thru, init it now
pmpsx2
	move.l	t_clock,d0		;timer clock freq
***	move.l	#CLKFRQ2,d0
	move.l	i_samp,a0
	divu	4(a0),d0		;over sample freq -> ticks per sample
	lsr.w	#2,d0			;will be using timer /4 mode

	cmpi.w	#$FF,d0			;too long?
	ble.s	pmpsx3
	move.w	#$FF,d0			;yes, clip to max
pmpsx3	lea	pump_one,a0		;handler
	bsr	set_timer

	clr.w	d0			;return to user mode
	bsr	super			;(always 'bsr', see note)
	rts

*-----------------------
* stop_sound
*-----------------------

; un-install our interrupt & restore OS sounds

_stop_sound
	tst.l	i_samp
	beq.s	stsnx1			;zero means already been here

	moveq	#1,d0			;jump into Super mode 
	bsr	super

	moveq	#-1,d0
	move.l	d0,a0
	moveq	#0,d0			;control mode = STOP
	bsr	set_timer

	move.b	oldct,conterm		;restore keyclick and bell settings
	clr.w	d0
	bsr	super			;(always 'bsr', see note)

	clr.l	i_samp			;mark it as stopped
stsnx1	rts

*-----------------------
* calibrate
*-----------------------

; determine timer's actual clock rate (different than cpu).
; Not brought out by OS, but we can measure it using the 200 Hz "system timer"
; global in low memory.  Our caller must be in Super mode.

calibrate
	move.w	#$80,d0			;128 x 4 ticks per interrupt
	lea	count_one,a0		;handler
	bsr	set_timer

	move.l	_hz_200,d0
calix1	cmp.l	_hz_200,d0		;wait for a transition
	beq.s	calix1			;not yet

	clr.l	t_temp			;ok, start counting
	add.l	#26,d0			;wait for 1/8 sec exactly (25/200)

calix2	cmp.l	_hz_200,d0		;wait for ending transition
	bgt.s	calix2			;not yet

	move.l	t_temp,d0		;grab timer result (~600 at CLKFRQ2)
	lsl.l	#7,d0			;x128 count
	lsl.l	#5,d0			;x4 ticks, x8 sec
	move.l	d0,t_clock		;save ticks/sec

	moveq	#-1,d0
	move.l	d0,a0
	moveq	#0,d0			;control mode = STOP
	bsr	set_timer
	rts

*-----------------------
* super
*-----------------------

; this routine must always be called (1) in matched pairs, with (2) the SP
; at the /same/ place both times.  Otherwise the second call "restores" the
; SP incorrectly and crashes.
;
; if d0 is nonzero, enter supervisor mode, else exit it

super	SAVEREGS			;preserve regs across trap
	tst.w	d0
	beq.s	sprx1

	clr.l	-(sp)			;set system sp to current sp
	move.w	#$20,-(sp)		;Super is function $20
	trap	#GEMDOS
	addq.l	#6,sp			;flush (system) stack
	move.l	d0,oldSSP		;save old system sp
	bra.s	sprx2

sprx1	move.l	oldSSP,-(sp)		;restore old system sp
	move.w	#$20,-(sp)
	trap	#GEMDOS
	addq.l	#6,sp			;flush our stack

sprx2	RESTREGS
	rts

*-----------------------
* set_timer
*-----------------------

; We use 68901 timer A in its fastest mode (divide-by-4), which maximizes 
; the playback accuracy at higher sample rates (within 2 percent at 18 KHz, 
; assuming CLKFRQ2).  It also limits the lowest rate we can playback
; to 2.5 KHz, which should cover all cases.  Our caller must be in Super mode.  

; given d0.w = count, 0 means disable timer
;   a0 -> interrupt handler, -1 means no change

set_timer
	SAVEREGS
	move.l	a0,-(sp)		;vector
	move.w	d0,-(sp)		;count
	beq.s	stmx1			;disable timer?
	moveq	#$01,d0			;no, divide-by-4
stmx1	move.w	d0,-(sp)		;control mode
	clr.w	-(sp)			;use timer A

; we now disable interrupts during the timer installation call -- it appeared
; to be causing occasional system crashes (id=24, "spurious interrupt")

	move	sr,d0			;get status reg
	move.w	d0,oldSR
	or.w	#$0700,d0		;disable all interrupts
	move	d0,sr

	move.w	#31,-(sp)		;call xbtimer
	trap	#XBIOS			;  <-- our ints normally begin/end here
	adda.w	#12,sp			;flush stack

	move	oldSR,sr		;re-enable interrupts  <-- now here
	RESTREGS
	rts

*-----------------------
* pump_one
*-----------------------

; interrupt entry point -- output the next sample

; Each pass through the critical section uses 298 ticks, plus interrupt 
; overhead of 44 ticks, so an 18k sample rate (one every 434 ticks at 8 MHz)
; consumes, in theory, 79 percent of total cpu time.

; In practice (running alongside other system interrupts), a 15.4k sample
; rate uses about 75 percent of available cpu time, which causes a "slowdown
; factor" of 4x in the foreground program.  This proves to be about the
; maximum permissible (in terms of performance).  An 18.4k rate uses about
; 90 percent, for a 10x slowdown.  At that rate, the mechanics of drawing to
; the screen are visibly affected, and scrolling occurs in "waves."

pump_one
	move.l	a0,-(sp)	;[14]	;save registers used
	move.l	a1,-(sp)	;[14]
	move.w	d0,-(sp)	; [8]

	lea	volmap-8,a0	;[12]	;address of our interrupt vars
;;;	lea	volmap-8(pc),a0	; [8]	;  (vars in code space)
	move.l	(a0)+,a1	;[12] 	;get pointer to next sample
	cmpa.l	(a0)+,a1	;[14]	;end of cycle?
	beq.s	pslx3		; [8]	;yes  [10 taken]		;[[ 82]]

	clr.w	d0		; [4]	;treat sample as unsigned
	move.b	(a1)+,d0	; [8]	;get it
	move.l	a1,-8(a0)	;[16]	;update pointer
	adda.w	d0,a0		; [8]
	add.w	d0,d0		; [4]
	adda.w	d0,a0		; [8]	;index into table (x3)		;[[ 48]]

; update volume on each channel (as simultaneously as possible)

	move.l	#GISelect,a1	;[12]
;;;	move.w	#GISelect,a1	; [8]
	move.b	#8,(a1)		;[12]
	move.b	(a0)+,2(a1)	;[16] 	;write A vol
	move.b	#9,(a1)		;[12]
	move.b	(a0)+,2(a1)	;[16] 	;write B vol
	move.b	#10,(a1)	;[12]
	move.b	(a0)+,2(a1)	;[16] 	;write C vol			;[[ 96]]

pslx2	move.b	#$df,mfp+isra	;[20]	;reset timer A interrupt bit
;;;	move.b	#$df,xxxx(a1)	;[16]
	move.w	(sp)+,d0	; [8]	;restore regs
	move.l	(sp)+,a1	;[12]
	move.l	(sp)+,a0	;[12]
	rte			;[20]	;return from exception		;[[ 72]]

; handle end of cycle.  The longest path through this section shouldn't be
; allowed to exceed the length of the critical path above by very much, since
; the timing may be tight.

pslx3	tst.w	i_done			;already finished?
	bne.s	pslx2			;just waiting for user to disable ints

	move.w	i_reps,d0		;any more reps?
	beq.s	pslx4			;infinite if initially zero
	subq.w	#1,d0
	beq.s	pslx5			;no
	move.w	d0,i_reps		;yes, update count

pslx4	move.l	i_begin,i_p		;reset the roving pointer
	bra.s	pslx2			;but wait until next int to restart

pslx5	move.w	#1,i_done		;finished, raise flag
	bra.s	pslx2

*-----------------------
* count_one
*-----------------------

; another interrupt, for timer calibration

count_one
	addq.l	#1,t_temp		;bump counter
	move.b	#$df,mfp+isra		;reset timer A interrupt bit
	rte				;return from exception

*-----------------------
* check_sound
*-----------------------

; return d0 = status of current sound, nonzero if end detected

check_sound
	move.w	i_done,d0		;end detected?
	beq.s	cksdx1			;no

	bsr	_stop_sound		;yes, un-install the interrupt now
	clr.w	i_done			;THEN reset this flag
	moveq	#1,d0			;return flag
cksdx1	rts

*-----------------------
* sound globals
*-----------------------

    bss

oldct	ds.b	1	;saved keyclick/bell settings
oldSSP	ds.l	1	;saved supervisor stack pointer
oldSR	ds.w	1	;saved status register

    data

i_reps	dc.w	0	;repeat count
i_samp	dc.l	0	;address of sound data buffer
i_done	dc.w	0	;end-of-sound flag

t_clock	dc.l	0	;timer (68901) clock rate (measured)
t_temp	dc.l	0	;calibration temp

; don't change the positions of the following vars -- they're located directly
; under the volume table to allow quicker access.

i_begin	dc.l	0	; (-12) pointer to first sample
i_p	dc.l	0	; (-8) pointer to next sample
i_end	dc.l	0	; (-4) pointer to last sample (+1)

; This table maps signed bytes (-128..127) into amplitude inputs for the
; sound chip.  Each entry is the 4-bit amplitude for one channel.
; In the Atari ST the three channels are not weighted equally; the values 
; in the table were determined empirically.

volmap
 	dc.b	$0A,$9,$B,$09,$B,$A,$0B,$9,$A,$0B,$B,$2   ;4 sets per line
	dc.b	$0B,$A,$9,$09,$C,$4,$09,$C,$5,$0A,$4,$C
 	dc.b	$0A,$5,$C,$0B,$8,$B,$0C,$1,$A,$0C,$2,$A
	dc.b	$0C,$3,$A,$0C,$4,$A,$0C,$5,$A,$0C,$8,$9
 	dc.b	$0C,$9,$8,$0C,$6,$A,$0C,$A,$1,$0C,$A,$3
	dc.b	$0C,$A,$5,$0C,$9,$9,$0C,$A,$6,$0C,$1,$B
 	dc.b	$0C,$A,$7,$0D,$2,$5,$0D,$3,$5,$0D,$5,$3
	dc.b	$0D,$5,$4,$0D,$5,$5,$0D,$6,$3,$0D,$6,$4
 	dc.b	$0D,$6,$5,$0D,$7,$2,$0D,$6,$6,$0D,$7,$4
	dc.b	$0D,$7,$5,$0D,$7,$6,$0D,$8,$0,$0D,$7,$7
 	dc.b	$0D,$6,$8,$0D,$8,$5,$0D,$8,$6,$0D,$8,$7
	dc.b	$0D,$9,$0,$0D,$9,$2,$0D,$9,$4,$0D,$9,$5
 	dc.b	$0D,$9,$6,$0D,$3,$A,$0D,$4,$A,$0D,$5,$A
	dc.b	$0D,$6,$A,$0B,$3,$D,$0B,$4,$D,$0B,$5,$D
 	dc.b	$0D,$A,$1,$0D,$A,$3,$0D,$A,$5,$0D,$A,$6
	dc.b	$0D,$A,$7,$0D,$4,$B,$0D,$5,$B,$0D,$A,$8
 	dc.b	$0D,$6,$B,$0D,$7,$B,$0D,$A,$9,$0D,$8,$B
	dc.b	$0D,$B,$3,$0D,$B,$5,$0D,$B,$6,$0D,$B,$7
 	dc.b	$0D,$9,$B,$0D,$B,$8,$0B,$A,$D,$0C,$0,$D
	dc.b	$0C,$2,$D,$0C,$4,$D,$0C,$5,$D,$0D,$0,$C
 	dc.b	$0D,$2,$C,$0D,$3,$C,$0D,$4,$C,$0D,$5,$C
	dc.b	$0D,$6,$C,$0D,$B,$A,$0D,$7,$C,$0D,$8,$C
 	dc.b	$0C,$9,$D,$0D,$9,$C,$0D,$B,$B,$0C,$D,$3
	dc.b	$0C,$D,$6,$0C,$D,$7,$0D,$C,$7,$0C,$D,$8
 	dc.b	$0D,$C,$8,$0A,$D,$C,$0A,$1,$E,$0A,$3,$E
	dc.b	$0A,$5,$E,$0A,$6,$E,$0A,$7,$E,$0C,$D,$A
 	dc.b	$06,$D,$D,$07,$D,$D,$08,$E,$6,$0A,$9,$E
	dc.b	$08,$D,$D,$0C,$D,$B,$0E,$7,$8,$09,$D,$D
 	dc.b	$0A,$7,$E,$0C,$D,$A,$0D,$C,$A,$0B,$C,$D
	dc.b	$0C,$B,$D,$05,$D,$D,$07,$E,$8,$0B,$D,$C
 	dc.b	$0D,$5,$D,$0E,$4,$8,$0D,$6,$D,$0E,$7,$6
	dc.b	$0D,$7,$D,$0D,$C,$B,$0D,$8,$D,$09,$D,$D

; the high half of the table precedes the low half so the index can be
; treated as unsigned (saves an instruction)

	dc.b	$01,$1,$1,$01,$1,$2,$01,$1,$3,$00,$3,$3
	dc.b	$01,$1,$4,$01,$2,$4,$01,$4,$2,$01,$4,$3
 	dc.b	$01,$5,$2,$03,$3,$4,$04,$3,$3,$05,$3,$2
	dc.b	$05,$3,$3,$05,$4,$2,$05,$3,$4,$03,$6,$2
 	dc.b	$03,$6,$3,$03,$4,$6,$03,$6,$4,$03,$5,$6
	dc.b	$04,$6,$4,$05,$6,$3,$04,$6,$5,$04,$7,$3
 	dc.b	$05,$7,$2,$05,$7,$3,$04,$7,$5,$05,$7,$4
	dc.b	$05,$6,$6,$05,$7,$5,$04,$7,$6,$03,$7,$7
 	dc.b	$05,$7,$6,$05,$8,$3,$04,$8,$5,$03,$8,$6
	dc.b	$07,$6,$6,$07,$7,$5,$07,$8,$0,$05,$9,$0
 	dc.b	$05,$9,$1,$05,$9,$2,$05,$9,$3,$04,$9,$5
	dc.b	$06,$9,$1,$06,$9,$2,$06,$9,$3,$07,$9,$0
 	dc.b	$08,$8,$2,$08,$8,$3,$09,$6,$5,$09,$0,$8
	dc.b	$09,$7,$4,$09,$6,$6,$09,$7,$5,$09,$8,$0
 	dc.b	$08,$9,$1,$08,$9,$2,$08,$9,$3,$08,$8,$7
	dc.b	$07,$9,$7,$08,$9,$5,$06,$A,$3,$09,$6,$8
 	dc.b	$09,$4,$9,$08,$7,$9,$09,$5,$9,$09,$9,$3
	dc.b	$09,$9,$4,$0A,$6,$6,$0A,$7,$5,$0A,$3,$8
 	dc.b	$08,$A,$1,$08,$A,$2,$04,$B,$2,$04,$B,$3
	dc.b	$03,$B,$5,$03,$A,$9,$02,$B,$6,$03,$B,$6
 	dc.b	$06,$B,$2,$06,$B,$3,$06,$B,$4,$07,$B,$1
	dc.b	$07,$B,$2,$07,$B,$3,$07,$B,$4,$07,$B,$5
 	dc.b	$07,$A,$9,$02,$A,$A,$03,$A,$A,$09,$2,$B
	dc.b	$09,$3,$B,$08,$9,$A,$08,$7,$B,$05,$9,$B
 	dc.b	$03,$3,$C,$03,$4,$C,$05,$2,$C,$05,$3,$C
	dc.b	$06,$1,$C,$07,$9,$B,$06,$3,$C,$04,$6,$C
 	dc.b	$06,$4,$C,$07,$2,$C,$07,$3,$C,$07,$4,$C
	dc.b	$07,$5,$C,$05,$B,$A,$08,$B,$9,$0A,$6,$B
 	dc.b	$09,$B,$8,$09,$A,$A,$09,$9,$B,$08,$5,$C
	dc.b	$06,$C,$5,$07,$C,$3,$05,$C,$7,$03,$C,$8
 	dc.b	$03,$B,$B,$08,$7,$C,$04,$B,$B,$05,$C,$8
	dc.b	$05,$B,$B,$06,$9,$C,$06,$C,$8,$06,$B,$B

    text
